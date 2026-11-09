//! Serve Claude Code stream-json from an in-process peer so provider tests launch no process.
//!
//! `PeerLauncher` implements the production `ClaudeLauncher` boundary with duplex streams. Each
//! launch builds a fresh `PeerHandler` that receives every frame Forge writes and returns the
//! frames the CLI would print. Control requests the handler declines receive the generic
//! success the CLI gives for configuration controls.

use super::process::{ClaudeLauncher, LaunchSpec};
use super::transport::ProcessIo;
use async_trait::async_trait;
use forge_provider_api::ProviderError;
use serde_json::{Value, json};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

/// Answer one inbound frame with the frames the CLI prints, or `None` to use the default answer.
pub(super) type PeerHandler = Box<dyn FnMut(&Value) -> Option<Vec<Value>> + Send>;

/// Record every launch and inbound frame for assertions.
#[derive(Default)]
pub(super) struct PeerRecord {
    pub launches: Vec<Vec<String>>,
    pub frames: Vec<Value>,
}

/// Launch in-memory peers in place of `claude`.
pub(super) struct PeerLauncher {
    handler_factory: Box<dyn Fn() -> PeerHandler + Send + Sync>,
    pub record: Arc<Mutex<PeerRecord>>,
}

impl PeerLauncher {
    pub(super) fn new(
        handler_factory: impl Fn() -> PeerHandler + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            handler_factory: Box::new(handler_factory),
            record: Arc::default(),
        })
    }

    /// Get every inbound frame of one `type`, in order.
    pub(super) fn frames(&self, kind: &str) -> Vec<Value> {
        self.record
            .lock()
            .unwrap()
            .frames
            .iter()
            .filter(|frame| frame["type"] == kind)
            .cloned()
            .collect()
    }
}

#[async_trait]
impl ClaudeLauncher for PeerLauncher {
    fn available(&self) -> bool {
        true
    }

    fn executable(&self) -> PathBuf {
        PathBuf::from("claude")
    }

    async fn version(
        &self,
        _: &forge_provider_api::OperationContext,
    ) -> Result<String, ProviderError> {
        Ok("2.1.317".into())
    }

    fn launch(&self, spec: &LaunchSpec) -> Result<ProcessIo, ProviderError> {
        self.record.lock().unwrap().launches.push(spec.arguments());
        let (client_stdin, peer_stdin) = tokio::io::duplex(1 << 20);
        let (mut peer_stdout, client_stdout) = tokio::io::duplex(1 << 20);
        let mut handler = (self.handler_factory)();
        let record = self.record.clone();
        tokio::spawn(async move {
            let mut lines = BufReader::new(peer_stdin).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                let frame: Value = serde_json::from_str(&line).expect("Forge writes JSON lines");
                record.lock().unwrap().frames.push(frame.clone());
                let replies = handler(&frame).unwrap_or_else(|| default_reply(&frame));
                for reply in replies {
                    let mut line = serde_json::to_vec(&reply).unwrap();
                    line.push(b'\n');
                    if peer_stdout.write_all(&line).await.is_err() {
                        return;
                    }
                }
            }
        });
        Ok(ProcessIo {
            stdout: Box::new(client_stdout),
            stdin: Box::new(client_stdin),
            process: None,
        })
    }
}

/// Answer configuration controls with success and the initialize request with a model catalog.
pub(super) fn default_reply(frame: &Value) -> Vec<Value> {
    if frame["type"] != "control_request" {
        return Vec::new();
    }
    let request_id = frame["request_id"].clone();
    let response = match frame["request"]["subtype"].as_str() {
        Some("initialize") => json!({
            "models": [
                {"value": "default", "displayName": "Default", "supportedEffortLevels": ["low", "high"]},
                {"value": "haiku", "displayName": "Haiku", "supportedEffortLevels": ["low", "high"]},
            ],
            "commands": [],
            "claude_code_version": "2.1.317",
        }),
        Some("get_context_usage") => json!({"totalTokens": 1200, "maxTokens": 200000}),
        _ => json!({}),
    };
    vec![success(request_id, response)]
}

/// Build a successful control response.
pub(super) fn success(request_id: Value, response: Value) -> Value {
    json!({"type": "control_response", "response": {
        "subtype": "success", "request_id": request_id, "response": response,
    }})
}

/// Build a `command_lifecycle` frame for one input uuid.
pub(super) fn lifecycle(uuid: &Value, state: &str) -> Value {
    json!({"type": "command_lifecycle", "command_uuid": uuid, "state": state, "uuid": uuid::Uuid::new_v4().to_string()})
}

/// Build the streamed frames of one complete text answer from one API message.
pub(super) fn text_message(message_id: &str, chunks: &[&str]) -> Vec<Value> {
    let event = |event: Value| {
        json!({"type": "stream_event", "event": event, "parent_tool_use_id": null,
            "uuid": uuid::Uuid::new_v4().to_string(), "api_message_id": message_id})
    };
    let mut frames = vec![
        event(
            json!({"type": "message_start", "message": {"id": message_id, "model": "claude-haiku-5-5",
            "type": "message", "role": "assistant", "content": [],
            "usage": {"input_tokens": 1, "output_tokens": 0}}}),
        ),
        event(
            json!({"type": "content_block_start", "index": 0, "content_block": {"type": "text", "text": ""}}),
        ),
    ];
    for chunk in chunks {
        frames.push(event(json!({"type": "content_block_delta", "index": 0,
            "delta": {"type": "text_delta", "text": chunk}})));
    }
    frames.push(json!({"type": "assistant", "message": {"id": message_id, "model": "claude-haiku-5-5",
        "type": "message", "role": "assistant", "content": [{"type": "text", "text": chunks.concat()}],
        "usage": {"input_tokens": 1, "output_tokens": 2}},
        "parent_tool_use_id": null, "uuid": uuid::Uuid::new_v4().to_string()}));
    frames.push(event(json!({"type": "content_block_stop", "index": 0})));
    frames.push(event(
        json!({"type": "message_delta", "delta": {"stop_reason": "end_turn"},
        "usage": {"input_tokens": 1, "output_tokens": 2}}),
    ));
    frames.push(event(json!({"type": "message_stop"})));
    frames
}

/// Build the successful end of one CLI turn.
pub(super) fn result_success() -> Value {
    json!({"type": "result", "subtype": "success", "is_error": false, "duration_ms": 10,
        "num_turns": 1, "result": "", "stop_reason": "end_turn",
        "usage": {"input_tokens": 1, "output_tokens": 2},
        "uuid": uuid::Uuid::new_v4().to_string()})
}

/// Build the end of a CLI turn that the client interrupted.
pub(super) fn result_interrupted() -> Value {
    json!({"type": "result", "subtype": "error_during_execution", "is_error": true,
        "duration_ms": 10, "num_turns": 1, "terminal_reason": "aborted_streaming",
        "uuid": uuid::Uuid::new_v4().to_string()})
}
