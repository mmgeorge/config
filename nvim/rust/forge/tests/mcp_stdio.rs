use std::{process::Stdio, time::Duration};

use serde_json::{Value, json};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

fn spawn() -> tokio::process::Child {
    tokio::process::Command::new(env!("CARGO_BIN_EXE_forge"))
        .arg("mcp")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .unwrap()
}

#[tokio::test]
async fn fragmented_requests_preserve_order_and_notification_silence() {
    let mut child = spawn();
    let mut input = child.stdin.take().unwrap();
    let mut output = BufReader::new(child.stdout.take().unwrap());
    let requests = [
        json!({"jsonrpc":"2.0", "id":1, "method":"initialize"}),
        json!({"jsonrpc":"2.0", "method":"notifications/initialized"}),
        json!({"jsonrpc":"2.0", "id":2, "method":"tools/list"}),
        json!({"jsonrpc":"2.0", "id":3, "method":"tools/call", "params":{"name":"harness_goal_status", "arguments":{"status":"🦀"}}}),
    ];
    for request in requests {
        let mut encoded = serde_json::to_vec(&request).unwrap();
        encoded.push(b'\n');
        for fragment in encoded.chunks(3) {
            input.write_all(fragment).await.unwrap();
        }
    }
    drop(input);
    for request_id in 1..=3 {
        let mut line = Vec::new();
        tokio::time::timeout(Duration::from_secs(5), output.read_until(b'\n', &mut line))
            .await
            .unwrap()
            .unwrap();
        assert!(line.len() <= forge_protocol::MAX_FRAME_BYTES);
        let response: Value = serde_json::from_slice(&line).unwrap();
        assert_eq!(response["id"], request_id);
        if request_id == 2 {
            assert!(response["result"]["tools"].as_array().unwrap().len() > 1);
        }
        if request_id == 3 {
            assert_eq!(
                response["result"]["structuredContent"]["arguments"]["status"],
                "🦀"
            );
        }
    }
    let mut extra = Vec::new();
    assert_eq!(
        tokio::time::timeout(Duration::from_secs(5), output.read_until(b'\n', &mut extra))
            .await
            .unwrap()
            .unwrap(),
        0
    );
    assert!(
        tokio::time::timeout(Duration::from_secs(5), child.wait())
            .await
            .unwrap()
            .unwrap()
            .success()
    );
}

#[tokio::test]
async fn oversized_unterminated_input_exits_while_stdin_remains_open() {
    let mut child = spawn();
    let mut input = child.stdin.take().unwrap();
    tokio::time::timeout(
        Duration::from_secs(5),
        input.write_all(&vec![b'x'; forge_protocol::MAX_FRAME_BYTES + 1]),
    )
    .await
    .unwrap()
    .unwrap();
    let output = tokio::time::timeout(Duration::from_secs(5), child.wait_with_output())
        .await
        .unwrap()
        .unwrap();
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("frame exceeds its byte limit"));
    drop(input);
}

#[tokio::test]
async fn truncated_json_fails_without_a_partial_response() {
    let mut child = spawn();
    let mut input = child.stdin.take().unwrap();
    input
        .write_all(b"{\"jsonrpc\":\"2.0\",\"id\":")
        .await
        .unwrap();
    drop(input);
    let output = tokio::time::timeout(Duration::from_secs(5), child.wait_with_output())
        .await
        .unwrap()
        .unwrap();
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("decode Harness control MCP request"));
}

#[tokio::test]
async fn oversized_result_returns_one_bounded_error_and_keeps_request_order() {
    let mut child = spawn();
    let mut input = child.stdin.take().unwrap();
    let mut output = BufReader::new(child.stdout.take().unwrap());
    let request = json!({"jsonrpc":"2.0", "id":11, "method":"tools/call", "params":{"name":"harness_goal_status", "arguments":{"status":"x".repeat(300 * 1024)}}});
    let mut encoded = serde_json::to_vec(&request).unwrap();
    encoded.push(b'\n');
    assert!(encoded.len() < forge_protocol::MAX_FRAME_BYTES);
    tokio::time::timeout(Duration::from_secs(5), input.write_all(&encoded))
        .await
        .unwrap()
        .unwrap();
    input
        .write_all(b"{\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"initialize\"}\n")
        .await
        .unwrap();
    drop(input);
    for request_id in [11, 12] {
        let mut line = Vec::new();
        tokio::time::timeout(Duration::from_secs(5), output.read_until(b'\n', &mut line))
            .await
            .unwrap()
            .unwrap();
        assert!(line.len() <= forge_protocol::MAX_FRAME_BYTES);
        let response: Value = serde_json::from_slice(&line).unwrap();
        assert_eq!(response["id"], request_id);
        if request_id == 11 {
            assert_eq!(response["error"]["code"], -32000);
        } else {
            assert!(response.get("result").is_some());
        }
    }
    assert!(
        tokio::time::timeout(Duration::from_secs(5), child.wait())
            .await
            .unwrap()
            .unwrap()
            .success()
    );
}
