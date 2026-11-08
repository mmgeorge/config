use futures_util::{SinkExt, StreamExt};
use serde_json::Value;
use std::sync::Arc;
use tokio::net::TcpStream;
use tokio::sync::{mpsc, oneshot, watch};
use tokio::task::JoinHandle;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{MaybeTlsStream, WebSocketStream, connect_async_with_config};

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
    pub value: Result<Value, serde_json::Error>,
}

/// Keep the socket reader alive independently of any RPC or foreground consumer.
pub struct CodexConnection {
    commands: mpsc::Sender<Command>,
    received: tokio::sync::Mutex<mpsc::UnboundedReceiver<ReceivedMessage>>,
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
        // Native snapshots may contain a complete large tool result in a single frame.
        let config = tokio_tungstenite::tungstenite::protocol::WebSocketConfig::default()
            .max_frame_size(None)
            .max_message_size(None);
        let (socket, _) = connect_async_with_config(endpoint, Some(config), false)
            .await
            .map_err(|source| CodexConnectionError::Transport {
                operation: "connect shared Codex app-server",
                source: Arc::new(source),
            })?;
        let (commands, receive_commands) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (publish, received) = mpsc::unbounded_channel();
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
    publish: mpsc::UnboundedSender<ReceivedMessage>,
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
                let message = ReceivedMessage { value: serde_json::from_str(&encoded) };
                if publish.send(message).is_err() { return Ok(()); }
            },
        }
    }
}
