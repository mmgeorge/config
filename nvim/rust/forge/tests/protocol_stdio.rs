use std::process::{ExitStatus, Stdio};

use serde_json::{Value, json};
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};

struct HostProcess {
    process: tokio::process::Child,
    input: Option<tokio::process::ChildStdin>,
    output: BufReader<tokio::process::ChildStdout>,
    bytes: usize,
    frames: usize,
}

impl HostProcess {
    async fn start() -> Self {
        let mut process = tokio::process::Command::new(env!("CARGO_BIN_EXE_forge"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .unwrap();
        let input = process.stdin.take();
        let output = BufReader::new(process.stdout.take().unwrap());
        let mut host = Self {
            process,
            input,
            output,
            bytes: 0,
            frames: 0,
        };
        host.send(json!({"id":1,"method":"host.initialize","params":{"protocol_version":forge_protocol::WIRE_VERSION}})).await;
        assert!(host.response(1).await.get("error").is_none());
        host
    }

    async fn send(&mut self, request: Value) {
        let mut frame = serde_json::to_vec(&request).unwrap();
        frame.push(b'\n');
        self.input
            .as_mut()
            .unwrap()
            .write_all(&frame)
            .await
            .unwrap();
    }

    async fn read(&mut self, return_credit: bool) -> Value {
        let mut frame = Vec::new();
        self.output.read_until(b'\n', &mut frame).await.unwrap();
        assert!(!frame.is_empty());
        assert!(frame.len() <= forge_protocol::MAX_FRAME_BYTES);
        self.bytes += frame.len();
        self.frames += 1;
        let message: Value = serde_json::from_slice(&frame).unwrap();
        if return_credit && self.input.is_some() {
            self.send(json!({"id":0,"method":"host.transport.consumed","params":{"bytes":self.bytes,"frames":self.frames}})).await;
        }
        message
    }

    async fn response(&mut self, id: u64) -> Value {
        loop {
            let message = self.read(true).await;
            if message["id"].as_u64() == Some(id) {
                return message;
            }
        }
    }

    /// Leave request 3 in flight on a hold that no prompt ever reaches. Prompt submission now
    /// answers at owner acceptance, so a mock hold wait is the request that stays active until
    /// the connection cancels it.
    async fn start_blocked_request(&mut self, workspace: &std::path::Path, data: &std::path::Path) {
        self.send(json!({"id":2,"method":"session.initialize","params":{
            "workspace":workspace,"data_root":data,"client_id":"eof-test",
            "provider":{"kind":"mock","command":["mock"]}
        }}))
        .await;
        let initialized = self.response(2).await;
        assert!(initialized.get("error").is_none(), "{initialized}");
        let session_id = initialized["result"]["session"]["id"].as_str().unwrap();
        self.send(json!({"id":4,"session_id":session_id,"method":"session.provider.mock_response","params":{"operation":"hold"}})).await;
        let hold = self.response(4).await;
        let hold_id = hold["result"]["id"]
            .as_str()
            .unwrap_or_else(|| panic!("mock hold was not armed: {hold}"))
            .to_owned();
        self.send(json!({"id":3,"session_id":session_id,"method":"session.provider.mock_response","params":{"operation":"wait","id":hold_id}})).await;
        self.send(json!({"id":5,"session_id":session_id,"method":"session.state.get","params":{}}))
            .await;
        loop {
            let message = self.read(true).await;
            assert_ne!(
                message["id"].as_u64(),
                Some(3),
                "blocked request ended before EOF: {message}"
            );
            if message["id"].as_u64() == Some(5) {
                break;
            }
        }
    }

    async fn finish(mut self) -> (Vec<Value>, ExitStatus, String) {
        self.input.take();
        let mut messages = Vec::new();
        loop {
            let mut frame = Vec::new();
            if self.output.read_until(b'\n', &mut frame).await.unwrap() == 0 {
                break;
            }
            assert!(frame.len() <= forge_protocol::MAX_FRAME_BYTES);
            assert!(messages.len() < forge_protocol::MAX_PENDING_FRAMES * 2);
            messages.push(serde_json::from_slice(&frame).unwrap());
        }
        let status = self.process.wait().await.unwrap();
        let mut error = String::new();
        self.process
            .stderr
            .take()
            .unwrap()
            .read_to_string(&mut error)
            .await
            .unwrap();
        (messages, status, error)
    }
}

#[tokio::test]
async fn eof_cancels_active_producers_before_waiting_for_output_drainage() {
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut host = HostProcess::start().await;
    host.start_blocked_request(workspace.path(), data.path())
        .await;
    let (messages, status, error) = host.finish().await;
    assert!(status.success(), "{error}");
    let cancelled = messages
        .iter()
        .find(|message| message["id"].as_u64() == Some(3))
        .unwrap();
    assert_eq!(
        cancelled.pointer("/error/code").and_then(Value::as_str),
        Some("request_failed"),
        "{cancelled}"
    );
}
