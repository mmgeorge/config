use futures_util::{SinkExt, StreamExt};
use serde_json::Value;
use tokio::net::TcpStream;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{MaybeTlsStream, WebSocketStream, connect_async};

/// Distinguishes native connection failures from malformed provider data.
#[derive(Debug, thiserror::Error)]
pub enum CodexConnectionError {
    /// A WebSocket operation failed.
    #[error("{operation}: {source}")]
    Transport {
        operation: &'static str,
        #[source]
        source: tokio_tungstenite::tungstenite::Error,
    },
    /// The provider closed its connection before returning a result.
    #[error("{0}")]
    Closed(String),
    /// A binary frame did not contain UTF-8 JSON text.
    #[error("decode shared Codex app-server binary frame: {0}")]
    Encoding(#[source] std::string::FromUtf8Error),
}

/// Retains transport size alongside decoded JSON or its precise parse failure.
pub struct ReceivedMessage {
    pub encoded_bytes: usize,
    pub value: Result<Value, serde_json::Error>,
}

/// Owns one native Codex WebSocket without host state or tool execution.
pub struct CodexConnection {
    socket: WebSocketStream<MaybeTlsStream<TcpStream>>,
}

impl CodexConnection {
    /// Connect to the resolved native app-server endpoint.
    pub async fn connect(endpoint: &str) -> Result<Self, CodexConnectionError> {
        let (socket, _) =
            connect_async(endpoint)
                .await
                .map_err(|source| CodexConnectionError::Transport {
                    operation: "connect shared Codex app-server",
                    source,
                })?;
        Ok(Self { socket })
    }

    /// Send one already-serialized JSON-RPC message without re-encoding it.
    pub async fn send(&mut self, encoded: String) -> Result<(), CodexConnectionError> {
        self.socket
            .send(Message::Text(encoded.into()))
            .await
            .map_err(|source| CodexConnectionError::Transport {
                operation: "write shared Codex app-server JSON-RPC request",
                source,
            })
    }

    /// Receive one JSON message while answering control frames in order.
    pub async fn receive(&mut self) -> Result<ReceivedMessage, CodexConnectionError> {
        let encoded = loop {
            let frame = self
                .socket
                .next()
                .await
                .ok_or_else(|| {
                    CodexConnectionError::Closed(
                        "shared Codex app-server closed before completing the request".into(),
                    )
                })?
                .map_err(|source| CodexConnectionError::Transport {
                    operation: "read Codex response",
                    source,
                })?;
            match frame {
                Message::Text(text) => break text.to_string(),
                Message::Binary(bytes) => {
                    break String::from_utf8(bytes.to_vec())
                        .map_err(CodexConnectionError::Encoding)?;
                }
                Message::Ping(payload) => {
                    self.socket
                        .send(Message::Pong(payload))
                        .await
                        .map_err(|source| CodexConnectionError::Transport {
                            operation: "answer Codex ping",
                            source,
                        })?;
                }
                Message::Pong(_) | Message::Frame(_) => {}
                Message::Close(close) => {
                    return Err(CodexConnectionError::Closed(format!(
                        "shared Codex app-server closed the session channel: {close:?}",
                    )));
                }
            }
        };
        Ok(ReceivedMessage {
            encoded_bytes: encoded.len(),
            value: serde_json::from_str(&encoded),
        })
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
        let mut connection = CodexConnection::connect(&format!("ws://{address}"))
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
        let mut connection = CodexConnection::connect(&format!("ws://{address}"))
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
}
