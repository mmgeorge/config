//! Drive adapter regressions through actual SDK framing without starting a native executable.

use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use tokio::io::{AsyncReadExt, AsyncWriteExt, DuplexStream};

/// Retain the real peer and every request until client collection closes its transport.
pub(super) struct SdkPeer {
    requests: Arc<Mutex<Vec<Value>>>,
    task: tokio::task::JoinHandle<()>,
    _writer: Arc<tokio::sync::Mutex<DuplexStream>>,
}

impl SdkPeer {
    /// Join peer exit and inspect the complete native request sequence.
    pub(super) async fn collected(self) -> Vec<Value> {
        self.task
            .await
            .expect("SDK peer task completed without panic");
        self.requests
            .lock()
            .expect("fixture request lock is not poisoned")
            .clone()
    }
}

/// Connect a real SDK client to a deterministic handler for native protocol requests.
pub(super) fn client(
    handler: impl FnMut(&Value) -> Vec<Value> + Send + 'static,
) -> (github_copilot_sdk::Client, SdkPeer) {
    let (reader, writer, peer) = streams(handler);
    (
        github_copilot_sdk::Client::from_streams(reader, writer, std::env::temp_dir())
            .expect("fixture streams form a client"),
        peer,
    )
}

/// Supply the same framed transport to the production retained runtime fixture entry point.
pub(super) fn streams(
    mut handler: impl FnMut(&Value) -> Vec<Value> + Send + 'static,
) -> (DuplexStream, DuplexStream, SdkPeer) {
    let (client_writer, mut peer_reader) = tokio::io::duplex(64 * 1024);
    let (peer_writer, client_reader) = tokio::io::duplex(64 * 1024);
    let writer = Arc::new(tokio::sync::Mutex::new(peer_writer));
    let peer_writer = writer.clone();
    let requests = Arc::new(Mutex::new(Vec::new()));
    let captured = requests.clone();
    let task = tokio::spawn(async move {
        while let Some(request) = frame(&mut peer_reader).await {
            captured
                .lock()
                .expect("fixture request lock is not poisoned")
                .push(request.clone());
            for response in handler(&request) {
                write_frame(&mut *peer_writer.lock().await, &response).await;
            }
        }
    });
    (
        client_reader,
        client_writer,
        SdkPeer {
            requests,
            task,
            _writer: writer,
        },
    )
}

/// Write one complete native frame atomically across scripted replies and delayed notifications.
async fn write_frame(writer: &mut DuplexStream, value: &Value) {
    let body = serde_json::to_vec(value).expect("fixture native value serializes");
    writer
        .write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
        .await
        .expect("fixture peer writes header");
    writer
        .write_all(&body)
        .await
        .expect("fixture peer writes value");
    writer.flush().await.expect("fixture peer flushes value");
}

/// Preserve the real SDK request identity when acknowledging its native operation.
pub(super) fn response(request: &Value, result: Value) -> Value {
    json!({"jsonrpc":"2.0", "id":request["id"], "result":result})
}

/// Read one framed SDK request and distinguish collection from a truncated frame.
async fn frame(reader: &mut DuplexStream) -> Option<Value> {
    let mut header = Vec::new();
    loop {
        match reader.read_u8().await {
            Ok(byte) => header.push(byte),
            Err(error)
                if header.is_empty() && error.kind() == std::io::ErrorKind::UnexpectedEof =>
            {
                return None;
            }
            Err(error) => panic!("fixture SDK frame header failed: {error}"),
        }
        if header.ends_with(b"\r\n\r\n") {
            break;
        }
        assert!(header.len() < 1024, "fixture SDK frame header is unbounded");
    }
    let count = std::str::from_utf8(&header)
        .unwrap()
        .trim()
        .strip_prefix("Content-Length: ")
        .unwrap()
        .parse::<usize>()
        .unwrap();
    assert!(count < 64 * 1024);
    let mut body = vec![0; count];
    reader
        .read_exact(&mut body)
        .await
        .expect("fixture peer reads complete request");
    Some(serde_json::from_slice(&body).expect("fixture SDK request is JSON"))
}
