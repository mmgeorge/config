use std::{
    process::{ExitStatus, Stdio},
    time::Duration,
};

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
        host.send(json!({"id":1,"method":"initialize","params":{"protocol_version":forge_protocol::WIRE_VERSION}})).await;
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
        tokio::time::timeout(
            Duration::from_secs(5),
            self.output.read_until(b'\n', &mut frame),
        )
        .await
        .unwrap()
        .unwrap();
        assert!(!frame.is_empty());
        assert!(frame.len() <= forge_protocol::MAX_FRAME_BYTES);
        self.bytes += frame.len();
        self.frames += 1;
        let message: Value = serde_json::from_slice(&frame).unwrap();
        if return_credit && self.input.is_some() {
            self.send(json!({"id":0,"method":"transport.consumed","params":{"bytes":self.bytes,"frames":self.frames}})).await;
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

    async fn start_turn(&mut self, workspace: &std::path::Path, data: &std::path::Path) {
        self.send(json!({"id":2,"method":"harness.initialize","params":{
            "workspace":workspace,"data_root":data,"client_id":"eof-test",
            "backend":{"kind":"mock","command":["visible-blocking"]}
        }}))
        .await;
        let initialized = self.response(2).await;
        assert!(initialized.get("error").is_none(), "{initialized}");
        self.send(json!({"id":3,"method":"prompt.submit","params":{"text":"remain active until connection closes"}})).await;
        loop {
            let message = self.read(true).await;
            assert_ne!(
                message["id"].as_u64(),
                Some(3),
                "turn ended before EOF: {message}"
            );
            if message.pointer("/payload/kind").and_then(Value::as_str)
                == Some("timeline_node_updated")
            {
                break;
            }
        }
    }

    async fn finish(mut self) -> (Vec<Value>, ExitStatus, String) {
        self.input.take();
        tokio::time::timeout(Duration::from_secs(5), async {
            let mut messages = Vec::new();
            let mut received_bytes = 0;
            loop {
                let mut frame = Vec::new();
                if self.output.read_until(b'\n', &mut frame).await.unwrap() == 0 {
                    break;
                }
                received_bytes += frame.len();
                assert!(frame.len() <= forge_protocol::MAX_FRAME_BYTES);
                assert!(received_bytes <= forge_protocol::MAX_QUEUED_BYTES);
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
        })
        .await
        .expect("Forge did not finish connection shutdown within five seconds")
    }
}

#[tokio::test]
async fn eof_cancels_active_producers_before_waiting_for_output_drainage() {
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut host = HostProcess::start().await;
    host.start_turn(workspace.path(), data.path()).await;
    let (messages, status, error) = host.finish().await;
    assert!(status.success(), "{error}");
    let cancelled = messages
        .iter()
        .find(|message| message["id"].as_u64() == Some(3))
        .unwrap();
    assert_eq!(
        cancelled.pointer("/error/code").and_then(Value::as_str),
        Some("turn_cancelled")
    );
}

#[tokio::test]
async fn exhausted_receive_credit_cannot_hold_an_eof_connection_open() {
    let mut host = HostProcess::start().await;
    for id in 2..2 + forge_protocol::MAX_PENDING_FRAMES {
        host.send(json!({"id":id,"method":"state.get","params":{}}))
            .await;
        assert_eq!(host.read(false).await["id"].as_u64(), Some(id as u64));
    }
    host.send(json!({"id":1000,"method":"state.get","params":{}}))
        .await;
    let (messages, status, error) = host.finish().await;
    assert!(!status.success());
    assert!(
        error.contains("connection shutdown deadline expired"),
        "{error}"
    );
    assert!(
        messages.is_empty(),
        "host published output without consumption credit"
    );
}

#[tokio::test]
async fn duplicate_running_request_ids_close_the_connection_and_cancel_the_owner() {
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut host = HostProcess::start().await;
    host.start_turn(workspace.path(), data.path()).await;
    host.send(json!({"id":3,"method":"state.get","params":{}}))
        .await;
    let (_, status, error) = host.finish().await;
    assert!(!status.success());
    assert!(
        error.contains("duplicate in-flight Forge request id: 3"),
        "{error}"
    );
}

#[tokio::test]
async fn invalid_consumption_credit_cancels_and_joins_active_requests() {
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut host = HostProcess::start().await;
    host.start_turn(workspace.path(), data.path()).await;
    host.send(
        json!({"id":0,"method":"transport.consumed","params":{"bytes":u64::MAX,"frames":u64::MAX}}),
    )
    .await;
    let (messages, status, error) = host.finish().await;
    assert!(!status.success());
    assert!(
        error.contains("consumer credit does not match published prefix"),
        "{error}"
    );
    let cancelled = messages
        .iter()
        .find(|message| message["id"].as_u64() == Some(3))
        .unwrap();
    assert_eq!(
        cancelled.pointer("/error/code").and_then(Value::as_str),
        Some("turn_cancelled")
    );
}
