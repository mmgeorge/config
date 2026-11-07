use super::super::{CopilotSessions, PreparedSession, SessionReuse, SessionSelection};
use super::{Host, reply_frame};
use crate::copilot::handle::{CopilotClient, CopilotSession};
use crate::copilot::session::SessionOptions;
use serde_json::Value;
use std::sync::Arc;
use tokio::io::{AsyncReadExt, AsyncWriteExt};

/// Own one real SDK transport and record every native request until transport loss.
pub(super) struct RecoveryPeer {
    pub(super) stop: tokio::sync::oneshot::Sender<()>,
    pub(super) methods: Arc<std::sync::Mutex<Vec<String>>>,
    pub(super) task: tokio::task::JoinHandle<()>,
}

impl RecoveryPeer {
    /// Attach a fresh peer to the same retained runtime without launching an external CLI.
    pub(super) async fn attach(
        runtime: &crate::copilot::runtime::CopilotRuntime,
        version: &'static str,
    ) -> Self {
        Self::attach_with_turns(runtime, version, false).await
    }

    /// Attach a peer that publishes incomplete-ancestry turns before each send acknowledgment.
    pub(super) async fn attach_with_turns(
        runtime: &crate::copilot::runtime::CopilotRuntime,
        version: &'static str,
        emit_turns: bool,
    ) -> Self {
        let (client_write, mut peer_read) = tokio::io::duplex(1 << 20);
        let (mut peer_write, client_read) = tokio::io::duplex(1 << 20);
        runtime
            .attach_streams(client_read, client_write, std::env::temp_dir())
            .await
            .unwrap();
        let (stop, mut stopped) = tokio::sync::oneshot::channel();
        let methods = Arc::new(std::sync::Mutex::new(Vec::new()));
        let observed = methods.clone();
        let task = tokio::spawn(async move {
            let mut send_count = 0usize;
            loop {
                let request = tokio::select! {
                    _ = &mut stopped => break,
                    request = recovery_frame(&mut peer_read) => match request { Some(request) => request, None => break },
                };
                let method = request["method"].as_str().unwrap();
                observed.lock().unwrap().push(method.to_owned());
                let result = match method {
                    "status.get" => serde_json::json!({"version":version}),
                    "mcp.discover" => serde_json::json!({"servers":[]}),
                    "session.create" => {
                        serde_json::json!({"sessionId":request["params"]["sessionId"],"workspacePath":std::env::temp_dir()})
                    }
                    "session.options.update" | "session.detach" => {
                        serde_json::json!({"success":true})
                    }
                    "session.plan.readSqlTodosWithDependencies" => {
                        serde_json::json!({"rows":[],"dependencies":[]} )
                    }
                    "session.eventLog.tail" => {
                        serde_json::json!({"cursor":format!("before-{send_count}")})
                    }
                    "session.eventLog.read" => {
                        // Journal admission captures only the latest durable anchor, never output recovery.
                        assert_eq!(request["params"]["direction"], "backward");
                        assert_eq!(request["params"]["max"], 1);
                        assert_eq!(request["params"]["includeEphemeral"], false);
                        assert_eq!(request["params"]["agentScope"], "primary");
                        assert_eq!(request["params"]["waitMs"], 0);
                        assert!(request["params"].get("cursor").is_none());
                        let events = if send_count == 0 {
                            Vec::new()
                        } else {
                            let ordinal = send_count - 1;
                            vec![serde_json::json!({
                                "id":format!("end-{ordinal}"), "parentId":format!("answer-{ordinal}"),
                                "type":"assistant.turn_end", "timestamp":"2026-11-02T00:00:00Z",
                                "data":{"turnId":"0"}
                            })]
                        };
                        serde_json::json!({"cursor":format!("anchor-{send_count}"),"cursorStatus":"ok","hasMore":send_count > 0,"events":events})
                    }
                    "session.send" => {
                        let ordinal = send_count;
                        send_count += 1;
                        let message_id = format!("accepted-{ordinal}");
                        if emit_turns {
                            for (event_id, parent_id, event_type, data) in [
                                (
                                    format!("user-{ordinal}"),
                                    None,
                                    "user.message",
                                    serde_json::json!({
                                        "messageId":message_id,"parentAgentTaskId":"root-task","turnId":"0"
                                    }),
                                ),
                                (
                                    format!("start-{ordinal}"),
                                    Some(format!("user-{ordinal}")),
                                    "assistant.turn_start",
                                    serde_json::json!({"turnId":"0"}),
                                ),
                                (
                                    format!("answer-{ordinal}"),
                                    Some("omitted-usage-record".to_owned()),
                                    "assistant.message",
                                    serde_json::json!({
                                        "messageId":format!("answer-{ordinal}"),"content":format!("full answer {ordinal}"),"phase":"final_answer","turnId":"0"
                                    }),
                                ),
                                (
                                    format!("end-{ordinal}"),
                                    Some(format!("answer-{ordinal}")),
                                    "assistant.turn_end",
                                    serde_json::json!({"turnId":"0"}),
                                ),
                                (
                                    format!("idle-{ordinal}"),
                                    Some("ephemeral-idle-parent".to_owned()),
                                    "session.idle",
                                    serde_json::json!({"mode":"interactive"}),
                                ),
                            ] {
                                recovery_notification(
                                    &mut peer_write,
                                    &request["params"]["sessionId"],
                                    &event_id,
                                    parent_id.as_deref(),
                                    event_type,
                                    data,
                                )
                                .await;
                            }
                        }
                        serde_json::json!({"messageId":message_id})
                    }
                    "session.tasks.refresh" | "session.mcp.listConfigured" => continue,
                    other => panic!("unexpected recovery fixture RPC: {other}"),
                };
                reply_frame(&mut peer_write, &request, result).await;
            }
        });
        Self {
            stop,
            methods,
            task,
        }
    }
}

/// Distinguish actual transport EOF from a malformed frame during recovery.
pub(super) async fn recovery_frame(reader: &mut tokio::io::DuplexStream) -> Option<Value> {
    let mut header = Vec::new();
    loop {
        match reader.read_u8().await {
            Ok(byte) => header.push(byte),
            Err(error)
                if header.is_empty() && error.kind() == std::io::ErrorKind::UnexpectedEof =>
            {
                return None;
            }
            Err(error) => panic!("recovery frame failed: {error}"),
        }
        if header.ends_with(b"\r\n\r\n") {
            break;
        }
        assert!(header.len() < 1024);
    }
    let size: usize = std::str::from_utf8(&header)
        .unwrap()
        .trim()
        .strip_prefix("Content-Length: ")
        .unwrap()
        .parse()
        .unwrap();
    assert!(size < 1 << 20);
    let mut body = vec![0; size];
    reader.read_exact(&mut body).await.unwrap();
    Some(serde_json::from_slice(&body).unwrap())
}

/// Prepare a real native session while retaining a disposable resource profile.
pub(super) async fn recovery_session(
    sessions: &Arc<CopilotSessions<tempfile::TempDir>>,
    client: CopilotClient,
    owner: &forge_provider_api::ThreadId,
) -> Arc<CopilotSession> {
    sessions
        .acquire(
            async move { Ok((client, None)) },
            SessionSelection {
                execution_owner: owner.clone(),

                correlation_id: owner.to_string(),
                session_id: None,
                model: None,
                context_tier: None,
                tools: None,
                reuse: SessionReuse::Compatible,
            },
            Arc::new(Host),
            Arc::new(Host),
            || async {
                Ok(PreparedSession {
                    options: SessionOptions {
                        isolated_text: false,
                        session_id: None,
                        model: None,
                        reasoning_effort: None,
                        context_tier: None,
                        workspace: std::env::temp_dir(),
                        system_message: String::new(),
                        resources: crate::copilot::session::SessionResources::default(),
                        excluded_tools: Vec::new(),
                        tools: Vec::new(),
                        native_callbacks: None,
                    },
                    skills: None,
                    mcp: None,
                    profile: Some(tempfile::tempdir().unwrap()),
                })
            },
            |_| Ok(()),
        )
        .await
        .unwrap()
}

/// Publish one SDK notification with the durable bookkeeping parent deliberately omitted.
async fn recovery_notification(
    writer: &mut tokio::io::DuplexStream,
    session_id: &Value,
    event_id: &str,
    parent_id: Option<&str>,
    event_type: &str,
    data: Value,
) {
    let mut event = serde_json::json!({
        "id":event_id,
        "type":event_type,
        "timestamp":"2026-11-02T00:00:00Z",
        "data":data,
    });
    if let Some(parent_id) = parent_id {
        event["parentId"] = serde_json::Value::String(parent_id.to_owned());
    }
    let frame = serde_json::json!({
        "jsonrpc":"2.0",
        "method":"session.event",
        "params":{"sessionId":session_id,"event":event},
    });
    let bytes = serde_json::to_vec(&frame).unwrap();
    writer
        .write_all(format!("Content-Length: {}\r\n\r\n", bytes.len()).as_bytes())
        .await
        .unwrap();
    writer.write_all(&bytes).await.unwrap();
    writer.flush().await.unwrap();
}
