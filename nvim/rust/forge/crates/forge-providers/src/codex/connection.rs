use futures_util::{SinkExt, StreamExt};
use serde_json::Value;
use std::sync::Arc;
use tokio::net::TcpStream;
use tokio::sync::{mpsc, oneshot, watch};
use tokio::task::JoinHandle;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{MaybeTlsStream, WebSocketStream, connect_async};

/// Distinguish native transport failure from malformed data and bounded observation loss.
#[derive(Clone, Debug, thiserror::Error)]
pub enum CodexConnectionError {
    /// A WebSocket operation failed.
    #[error("{operation}: {source}")]
    Transport {
        operation: &'static str,
        #[source]
        source: Arc<tokio_tungstenite::tungstenite::Error>,
    },
    /// The provider closed its connection before returning a result.
    #[error("{0}")]
    Closed(String),
    /// A binary frame did not contain UTF-8 JSON text.
    #[error("decode shared Codex app-server binary frame: {0}")]
    Encoding(#[source] Arc<std::string::FromUtf8Error>),
    /// Required incoming frames could not fit in the retained session queue.
    #[error("Codex session observation queue overflowed")]
    Overflow,
}

/// Retain transport size alongside decoded JSON or its precise parse failure.
pub struct ReceivedMessage {
    #[cfg(test)]
    pub encoded_bytes: usize,
    pub value: Result<Value, serde_json::Error>,
}

/// Keep the socket reader alive independently of any RPC or foreground consumer.
pub struct CodexConnection {
    commands: mpsc::Sender<Command>,
    received: tokio::sync::Mutex<mpsc::Receiver<ReceivedMessage>>,
    failure: watch::Receiver<Option<CodexConnectionError>>,
    task: tokio::sync::Mutex<Option<JoinHandle<()>>>,
}

enum Command {
    Write {
        encoded: String,
        reply: oneshot::Sender<Result<(), CodexConnectionError>>,
    },
    Close {
        reply: oneshot::Sender<Result<(), CodexConnectionError>>,
    },
}

impl CodexConnection {
    /// Connect once and immediately start the owner of the native socket.
    pub async fn connect(endpoint: &str) -> Result<Self, CodexConnectionError> {
        let (socket, _) =
            connect_async(endpoint)
                .await
                .map_err(|source| CodexConnectionError::Transport {
                    operation: "connect shared Codex app-server",
                    source: Arc::new(source),
                })?;
        let (commands, receive_commands) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (publish, received) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (failure, failures) = watch::channel(None);
        let task = tokio::spawn(async move {
            if let Err(error) = run_socket(socket, receive_commands, publish).await {
                failure.send_replace(Some(error));
            }
        });
        Ok(Self {
            commands,
            received: tokio::sync::Mutex::new(received),
            failure: failures,
            task: tokio::sync::Mutex::new(Some(task)),
        })
    }

    /// Await only the socket write; cancelling the waiter does not retract a queued write.
    pub async fn send(&self, encoded: String) -> Result<(), CodexConnectionError> {
        if let Some(error) = self.failure.borrow().clone() {
            return Err(error);
        }
        let (reply, response) = oneshot::channel();
        self.commands
            .try_send(Command::Write { encoded, reply })
            .map_err(|error| match error {
                mpsc::error::TrySendError::Full(_) => CodexConnectionError::Overflow,
                mpsc::error::TrySendError::Closed(_) => self.stopped(),
            })?;
        response.await.map_err(|_| self.stopped())?
    }

    /// Drain retained frames in order before exposing sticky observation failure.
    pub async fn receive(&self) -> Result<ReceivedMessage, CodexConnectionError> {
        self.received
            .lock()
            .await
            .recv()
            .await
            .ok_or_else(|| self.stopped())
    }

    /// Close and collect the socket owner after the adapter has drained required native work.
    pub async fn close(&self) -> Result<(), CodexConnectionError> {
        let mut task = self.task.lock().await;
        let Some(running) = task.as_mut() else {
            return Ok(());
        };
        let (reply, response) = oneshot::channel();
        let result = if self.commands.send(Command::Close { reply }).await.is_ok() {
            response.await.unwrap_or_else(|_| {
                Err(CodexConnectionError::Closed(
                    "Codex socket owner closed during shutdown".into(),
                ))
            })
        } else {
            Ok(())
        };
        let collected = running.await.map_err(|error| {
            CodexConnectionError::Closed(format!("Codex socket owner failed: {error}"))
        });
        task.take();
        collected?;
        result
    }

    /// Get the sticky loss reason even when the ordinary data queue has no spare capacity.
    fn stopped(&self) -> CodexConnectionError {
        self.failure
            .borrow()
            .clone()
            .unwrap_or_else(|| CodexConnectionError::Closed("Codex socket owner closed".into()))
    }
}

impl Drop for CodexConnection {
    /// Prevent a cancelled initialization from leaving an unowned socket task behind.
    fn drop(&mut self) {
        if let Some(task) = self.task.get_mut().as_ref() {
            task.abort();
        }
    }
}

/// Read and write protocol frames without awaiting application callbacks or event consumers.
async fn run_socket(
    mut socket: WebSocketStream<MaybeTlsStream<TcpStream>>,
    mut commands: mpsc::Receiver<Command>,
    publish: mpsc::Sender<ReceivedMessage>,
) -> Result<(), CodexConnectionError> {
    loop {
        tokio::select! {
            command = commands.recv() => match command {
                Some(Command::Write { encoded, reply }) => {
                    let result = socket.send(Message::Text(encoded.into())).await.map_err(|source| CodexConnectionError::Transport {
                        operation: "write shared Codex app-server JSON-RPC request", source: Arc::new(source),
                    });
                    let failure = result.as_ref().err().cloned();
                    let _ = reply.send(result);
                    if let Some(error) = failure { return Err(error); }
                },
                Some(Command::Close { reply }) => {
                    let result = socket.close(None).await.map_err(|source| CodexConnectionError::Transport {
                        operation: "close shared Codex app-server", source: Arc::new(source),
                    });
                    let _ = reply.send(result);
                    return Ok(());
                },
                None => return Ok(()),
            },
            frame = socket.next() => {
                let frame = frame.ok_or_else(|| CodexConnectionError::Closed("shared Codex app-server closed before completing the request".into()))?
                    .map_err(|source| CodexConnectionError::Transport { operation: "read Codex response", source: Arc::new(source) })?;
                let encoded = match frame {
                    Message::Text(text) => text.to_string(),
                    Message::Binary(bytes) => String::from_utf8(bytes.to_vec()).map_err(|error| CodexConnectionError::Encoding(Arc::new(error)))?,
                    Message::Ping(payload) => {
                        socket.send(Message::Pong(payload)).await.map_err(|source| CodexConnectionError::Transport {
                            operation: "answer Codex ping", source: Arc::new(source),
                        })?;
                        continue;
                    },
                    Message::Pong(_) | Message::Frame(_) => continue,
                    Message::Close(close) => return Err(CodexConnectionError::Closed(format!("shared Codex app-server closed the session channel: {close:?}"))),
                };
                let message = ReceivedMessage { #[cfg(test)] encoded_bytes: encoded.len(), value: serde_json::from_str(&encoded) };
                match publish.try_send(message) {
                    Ok(()) => {},
                    Err(mpsc::error::TrySendError::Full(_)) => return Err(CodexConnectionError::Overflow),
                    Err(mpsc::error::TrySendError::Closed(_)) => return Ok(()),
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use tokio::net::TcpListener;

    #[tokio::test]
    async fn binary_and_ping_frames_preserve_json() {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            socket.send(Message::Ping(vec![7].into())).await.unwrap();
            socket
                .send(Message::Binary(
                    br#"{"id":1,"result":"accepted"}"#.to_vec().into(),
                ))
                .await
                .unwrap();
            assert_eq!(
                socket.next().await.unwrap().unwrap(),
                Message::Pong(vec![7].into())
            );
        });
        let connection = CodexConnection::connect(&format!("ws://{address}"))
            .await
            .unwrap();
        let message = connection.receive().await.unwrap();
        assert_eq!(message.encoded_bytes, 28);
        assert_eq!(message.value.unwrap(), json!({"id":1,"result":"accepted"}));
        server.await.unwrap();
    }

    #[tokio::test]
    async fn malformed_json_and_close_remain_distinct() {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            socket.send(Message::Text("{".into())).await.unwrap();
            socket.send(Message::Close(None)).await.unwrap();
        });
        let connection = CodexConnection::connect(&format!("ws://{address}"))
            .await
            .unwrap();
        let malformed = connection.receive().await.unwrap();
        assert_eq!(malformed.encoded_bytes, 1);
        assert!(malformed.value.is_err());
        assert!(matches!(
            connection.receive().await,
            Err(CodexConnectionError::Closed(_))
        ));
        server.await.unwrap();
    }
    /// The socket answers control frames even while no RPC consumer polls receive.
    #[tokio::test]
    async fn session_reader_runs_before_the_first_consumer_poll() {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            socket
                .send(Message::Text(r#"{"early":true}"#.into()))
                .await
                .unwrap();
            socket.send(Message::Ping(vec![9].into())).await.unwrap();
            assert_eq!(
                socket.next().await.unwrap().unwrap(),
                Message::Pong(vec![9].into())
            );
        });
        let connection = CodexConnection::connect(&format!("ws://{address}"))
            .await
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), server)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(
            connection.receive().await.unwrap().value.unwrap(),
            json!({"early":true})
        );
    }

    /// A full data queue cannot hide the loss signal, and retained frames still drain in order.
    #[tokio::test]
    async fn overflow_is_sticky_after_retained_frames_are_drained() {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            for sequence in 0..=forge_protocol::MAX_PENDING_FRAMES {
                if socket
                    .send(Message::Text(
                        json!({"sequence":sequence}).to_string().into(),
                    ))
                    .await
                    .is_err()
                {
                    break;
                }
            }
            let _ = socket.next().await;
        });
        let mut connection = CodexConnection::connect(&format!("ws://{address}"))
            .await
            .unwrap();
        tokio::time::timeout(
            std::time::Duration::from_secs(2),
            connection.failure.wait_for(|failure| failure.is_some()),
        )
        .await
        .unwrap()
        .unwrap();
        for sequence in 0..forge_protocol::MAX_PENDING_FRAMES {
            assert_eq!(
                connection.receive().await.unwrap().value.unwrap(),
                json!({"sequence":sequence})
            );
        }
        assert!(matches!(
            connection.receive().await,
            Err(CodexConnectionError::Overflow)
        ));
        assert!(matches!(
            connection.send("{}".into()).await,
            Err(CodexConnectionError::Overflow)
        ));
        server.await.unwrap();
    }
}
