use super::{CodexConnection, ReceivedMessage};
use forge_provider_api::{
    ProviderError, ProviderFailure,
    error::{ProviderFailureKind, ProviderOperation},
};
use serde_json::{Value, json};
use std::{
    collections::HashMap,
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
};
use tokio::sync::{Mutex, mpsc, oneshot};
use tokio::task::JoinHandle;

/// Share native RPC methods without serializing their independent send and receive paths.
#[derive(Clone)]
pub(super) struct RpcHandle(Arc<tokio::sync::RwLock<Arc<NativeRpc>>>);

pub(super) struct RpcGuard(Arc<NativeRpc>);

impl RpcHandle {
    pub(super) fn new(rpc: NativeRpc) -> Self {
        Self(Arc::new(tokio::sync::RwLock::new(Arc::new(rpc))))
    }
    pub(super) async fn lock(&self) -> RpcGuard {
        RpcGuard(self.0.read().await.clone())
    }
    pub(super) async fn replace(&self, rpc: NativeRpc) -> Arc<NativeRpc> {
        std::mem::replace(&mut *self.0.write().await, Arc::new(rpc))
    }
}

impl std::ops::Deref for RpcGuard {
    type Target = NativeRpc;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

type RpcWaiters = Arc<Mutex<HashMap<u64, oneshot::Sender<Result<Value, ProviderError>>>>>;

/// Own request correlation and continuously dispatch replies away from session notifications.
pub(super) struct NativeRpc {
    connection: Arc<CodexConnection>,
    next_id: AtomicU64,
    waiters: RpcWaiters,
    responses: Mutex<HashMap<u64, oneshot::Receiver<Result<Value, ProviderError>>>>,
    methods: Mutex<HashMap<u64, String>>,
    notifications: Mutex<mpsc::Receiver<Result<Value, ProviderError>>>,
    reader: Mutex<Option<JoinHandle<()>>>,
}

impl NativeRpc {
    /// Create a response dispatcher that keeps native request waiters independent of its reader.
    pub(super) fn new(connection: CodexConnection) -> Self {
        let connection = Arc::new(connection);
        let waiters = Arc::new(Mutex::new(HashMap::<
            u64,
            oneshot::Sender<Result<Value, ProviderError>>,
        >::new()));
        let (notification_tx, notifications) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let reader_connection = connection.clone();
        let reader_waiters = waiters.clone();
        let reader = tokio::spawn(async move {
            loop {
                let received = match reader_connection.receive().await {
                    Ok(received) => received,
                    Err(error) => {
                        let error = ProviderError::from(error);
                        let pending = std::mem::take(&mut *reader_waiters.lock().await);
                        for (_, waiter) in pending {
                            let _ = waiter.send(Err(error.clone()));
                        }
                        let _ = notification_tx.send(Err(error)).await;
                        break;
                    }
                };
                let value = match decode_frame(received) {
                    Ok(value) => value,
                    Err(error) => {
                        let pending = std::mem::take(&mut *reader_waiters.lock().await);
                        for (_, waiter) in pending {
                            let _ = waiter.send(Err(error.clone()));
                        }
                        let _ = notification_tx.send(Err(error)).await;
                        break;
                    }
                };
                if let Some(id) = response_id(&value) {
                    if let Some(waiter) = reader_waiters.lock().await.remove(&id) {
                        let _ = waiter.send(Ok(value));
                    }
                    continue;
                }
                match notification_tx.try_send(Ok(value)) {
                    Ok(()) => {}
                    Err(mpsc::error::TrySendError::Full(_)) => {
                        let error = ProviderError::Observation(
                            forge_provider_api::lifecycle::ObservationFailure::Overflow,
                        );
                        let pending = std::mem::take(&mut *reader_waiters.lock().await);
                        for (_, waiter) in pending {
                            let _ = waiter.send(Err(error.clone()));
                        }
                        let _ = notification_tx.send(Err(error)).await;
                        break;
                    }
                    Err(mpsc::error::TrySendError::Closed(_)) => break,
                }
            }
        });
        Self {
            connection,
            next_id: AtomicU64::new(0),
            waiters,
            responses: Mutex::new(HashMap::new()),
            methods: Mutex::new(HashMap::new()),
            notifications: Mutex::new(notifications),
            reader: Mutex::new(Some(reader)),
        }
    }

    /// Send a correlated request while the dispatcher routes its response independently.
    pub(super) async fn request(
        &self,
        method: &str,
        params: Value,
        operation: ProviderOperation,
    ) -> Result<Value, ProviderError> {
        let id = self.send_request(method, params).await?;
        self.wait_response(id, operation).await
    }

    /// Await one request response without owning the native session reader lock.
    pub(super) async fn wait_response(
        &self,
        id: u64,
        operation: ProviderOperation,
    ) -> Result<Value, ProviderError> {
        let receiver = self
            .responses
            .lock()
            .await
            .remove(&id)
            .ok_or_else(|| failure(operation, ProviderFailureKind::Protocol))?;
        let method = self.methods.lock().await.remove(&id).unwrap_or_default();
        let response = tokio::time::timeout(std::time::Duration::from_secs(30), receiver).await;
        self.waiters.lock().await.remove(&id);
        let result = response
            .map_err(|_| failure(operation, ProviderFailureKind::Timeout))?
            .map_err(|_| failure(operation, ProviderFailureKind::Transport))??;
        decode_result(&result, operation, &method)
    }

    /// Write one native notification without adding a JSON-RPC request identity.
    pub(super) async fn notify(&self, method: &str, params: Value) -> Result<(), ProviderError> {
        let encoded =
            serde_json::to_string(&json!({"method":method,"params":params})).map_err(|_| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::Protocol,
                )
            })?;
        self.connection.send(encoded).await.map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Transport,
            )
        })
    }

    /// Send one server request and retain its private response waiter before writing.
    pub(super) async fn send_request(
        &self,
        method: &str,
        params: Value,
    ) -> Result<u64, ProviderError> {
        let id = self
            .next_id
            .fetch_add(1, Ordering::Relaxed)
            .checked_add(1)
            .ok_or_else(|| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::Unknown,
                )
            })?;
        let encoded = serde_json::to_string(&json!({"id":id,"method":method,"params":params}))
            .map_err(|_| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let (sender, receiver) = oneshot::channel();
        self.waiters.lock().await.insert(id, sender);
        self.responses.lock().await.insert(id, receiver);
        self.methods.lock().await.insert(id, method.to_owned());
        if self.connection.send(encoded).await.is_err() {
            self.waiters.lock().await.remove(&id);
            self.responses.lock().await.remove(&id);
            self.methods.lock().await.remove(&id);
            return Err(failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Transport,
            ));
        }
        Ok(id)
    }

    /// Receive the next unsolicited notification or server request in wire order.
    pub(super) async fn receive(&self) -> Result<Value, ProviderError> {
        self.notifications
            .lock()
            .await
            .recv()
            .await
            .ok_or_else(|| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::Transport,
                )
            })?
    }

    /// Respond to a native server request without exposing its identity to Harness.
    pub(super) async fn respond(&self, id: &Value, result: Value) -> Result<(), ProviderError> {
        let encoded = serde_json::to_string(&json!({"id":id,"result":result})).map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            )
        })?;
        self.connection.send(encoded).await.map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Transport,
            )
        })
    }

    /// Reject a native server request when no foreground invocation owns its callbacks.
    pub(super) async fn reject_request(&self, id: &Value) -> Result<(), ProviderError> {
        let encoded = serde_json::to_string(&json!({
            "id":id,"error":{"code":-32001,"message":"No active provider invocation"}
        }))
        .map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            )
        })?;
        self.connection.send(encoded).await.map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Transport,
            )
        })
    }

    /// Collect the socket owner and dispatcher after native session work has settled.
    pub(super) async fn close(&self) -> Result<(), ProviderError> {
        let connection = self.connection.close().await.map_err(|_| {
            failure(
                ProviderOperation::CollectNativeRuntime,
                ProviderFailureKind::Transport,
            )
        });
        let mut reader = self.reader.lock().await;
        let collection = if let Some(running) = reader.as_mut() {
            running.abort();
            let result = match running.await {
                Ok(()) => Ok(()),
                Err(error) if error.is_cancelled() => Ok(()),
                Err(_) => Err(failure(
                    ProviderOperation::CollectEventReader,
                    ProviderFailureKind::Runtime,
                )),
            };
            reader.take();
            result
        } else {
            Ok(())
        };
        match (connection, collection) {
            (Ok(()), result) | (result, Ok(())) => result,
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
        }
    }
}

fn decode_frame(received: ReceivedMessage) -> Result<Value, ProviderError> {
    received.value.map_err(|_| {
        failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Protocol,
        )
    })
}

fn response_id(message: &Value) -> Option<u64> {
    if message.get("method").is_some() {
        return None;
    }
    message.get("id").and_then(Value::as_u64)
}

fn decode_result(
    message: &Value,
    operation: ProviderOperation,
    method: &str,
) -> Result<Value, ProviderError> {
    if message.get("result").is_some() == message.get("error").is_some() {
        return Err(failure(operation, ProviderFailureKind::Protocol));
    }
    if message.get("error").is_some() {
        let error = &message["error"];
        if method == "thread/resume"
            && error.get("code").and_then(Value::as_i64) == Some(-32600)
            && error
                .get("message")
                .and_then(Value::as_str)
                .is_some_and(|message| {
                    message.starts_with("no rollout found for thread id ")
                        || message.starts_with("thread not found: ")
                })
        {
            return Err(failure(operation, ProviderFailureKind::SessionMissing));
        }
        if method == "turn/start"
            && error
                .get("code")
                .and_then(Value::as_i64)
                .is_some_and(|code| (-32602..=-32600).contains(&code))
        {
            return Err(failure(operation, ProviderFailureKind::Rejected));
        }
        let kind = match error.get("code").and_then(Value::as_i64) {
            Some(-32600 | -32602) => ProviderFailureKind::Rejected,
            Some(-32700 | -32601 | -32603) => ProviderFailureKind::Protocol,
            _ => ProviderFailureKind::Unknown,
        };
        return Err(failure(operation, kind));
    }
    Ok(message["result"].clone())
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_util::{SinkExt, StreamExt};
    use serde_json::json;
    use tokio::net::TcpListener;
    use tokio_tungstenite::{accept_async, tungstenite::Message};

    /// Create a fully collected transport for isolated dispatcher-collection tests.
    async fn closed_rpc() -> NativeRpc {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let peer = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = accept_async(stream).await.unwrap();
            while let Some(Ok(message)) = socket.next().await {
                if message.is_close() {
                    break;
                }
            }
        });
        let rpc = NativeRpc::new(
            CodexConnection::connect(&format!("ws://{address}"))
                .await
                .unwrap(),
        );
        rpc.close().await.unwrap();
        peer.await.unwrap();
        rpc
    }

    /// Keep the original dispatcher owned when a cleanup waiter times out.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn interrupted_dispatcher_collection_retains_join_ownership() {
        let rpc = closed_rpc().await;
        let (entered, started) = oneshot::channel();
        let (release, wait) = std::sync::mpsc::channel();
        let task = tokio::spawn(async move {
            tokio::task::block_in_place(|| {
                let _ = entered.send(());
                let _ = wait.recv();
            });
        });
        *rpc.reader.lock().await = Some(task);
        started.await.unwrap();
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(5), rpc.close())
                .await
                .is_err()
        );
        assert!(rpc.reader.lock().await.is_some());
        release.send(()).unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), rpc.close())
            .await
            .unwrap()
            .unwrap();
        assert!(rpc.reader.lock().await.is_none());
    }

    /// Report a dispatcher panic without exposing its private payload.
    #[tokio::test]
    async fn dispatcher_panic_is_a_normalized_collection_failure() {
        let rpc = closed_rpc().await;
        let task = tokio::spawn(async { panic!("private dispatcher panic marker") });
        while !task.is_finished() {
            tokio::task::yield_now().await;
        }
        *rpc.reader.lock().await = Some(task);
        let error = rpc.close().await.unwrap_err();
        assert!(matches!(&error, ProviderError::Failure(failure)
            if failure.operation() == ProviderOperation::CollectEventReader
            && failure.kind() == ProviderFailureKind::Runtime));
        assert!(!format!("{error:?} {error}").contains("private dispatcher panic marker"));
        let source = std::error::Error::source(&error).unwrap();
        assert!(source.is::<ProviderFailure>());
        assert!(source.source().is_none());
        assert!(rpc.reader.lock().await.is_none());
    }

    /// Keep method-specific missing-session recognition inside the native response decoder.
    #[test]
    fn response_errors_are_typed_and_never_retain_native_text() {
        for (method, code, message, expected) in [
            (
                "thread/resume",
                -32600,
                "thread not found: private-native-id",
                ProviderFailureKind::SessionMissing,
            ),
            (
                "thread/resume",
                -32600,
                "private-native-error",
                ProviderFailureKind::Rejected,
            ),
            (
                "thread/goal/clear",
                -32600,
                "thread not found: private-native-id",
                ProviderFailureKind::Rejected,
            ),
            (
                "turn/start",
                -32602,
                "private-native-error",
                ProviderFailureKind::Rejected,
            ),
            (
                "thread/read",
                -32603,
                "private-native-error",
                ProviderFailureKind::Protocol,
            ),
            (
                "thread/read",
                -32099,
                "private-native-error",
                ProviderFailureKind::Unknown,
            ),
        ] {
            let error = decode_result(&json!({"id":17,"error":{"code":code,"message":message,"data":"private-native-payload"}}),
                ProviderOperation::AdmitInput, method).unwrap_err();
            assert!(
                matches!(&error, ProviderError::Failure(failure) if failure.kind() == expected)
            );
            assert!(!format!("{error:?} {error}").contains("private-native"));
            let source =
                std::error::Error::source(&error).expect("normalized classification is retained");
            assert!(source.is::<ProviderFailure>());
            assert!(source.source().is_none());
        }
        assert_eq!(
            response_id(&json!({"id":17,"method":"item/tool/call"})),
            None
        );
        assert_eq!(
            decode_result(
                &json!({"id":17,"result":null}),
                ProviderOperation::AdmitInput,
                "turn/start"
            )
            .unwrap(),
            Value::Null
        );
        for malformed in [json!({"id":17}), json!({"id":17,"result":null,"error":{}})] {
            assert!(
                matches!(decode_result(&malformed, ProviderOperation::AdmitInput, "turn/start"),
                Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Protocol)
            );
        }
    }

    /// Release in-flight requests immediately when a native frame cannot be decoded.
    #[tokio::test]
    async fn malformed_frame_releases_waiting_rpc_without_native_payload() {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = accept_async(stream).await.unwrap();
            let _request = socket.next().await;
            socket
                .send(Message::Text("private-native-invalid-json".into()))
                .await
                .unwrap();
            let _closed = socket.next().await;
        });
        let rpc = NativeRpc::new(
            CodexConnection::connect(&format!("ws://{address}"))
                .await
                .unwrap(),
        );
        let error = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            rpc.request("turn/start", json!({}), ProviderOperation::AdmitInput),
        )
        .await
        .expect("malformed frame must release the request")
        .unwrap_err();
        assert!(
            matches!(&error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Protocol)
        );
        assert!(!format!("{error:?} {error}").contains("private-native"));
        let _ = rpc.close().await;
        server.await.unwrap();
    }

    #[tokio::test]
    async fn response_dispatch_keeps_unrelated_notifications_in_order() {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = accept_async(stream).await.unwrap();
            let request: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            socket
                .send(Message::Text(
                    json!({
                        "method":"turn/started","params":{"threadId":"thread","turnId":"turn"}
                    })
                    .to_string()
                    .into(),
                ))
                .await
                .unwrap();
            socket
                .send(Message::Text(
                    json!({
                        "id":request["id"],"result":{"turnId":"turn"}
                    })
                    .to_string()
                    .into(),
                ))
                .await
                .unwrap();
        });
        let connection = CodexConnection::connect(&format!("ws://{address}"))
            .await
            .unwrap();
        let rpc = NativeRpc::new(connection);
        let result = rpc
            .request("turn/start", json!({}), ProviderOperation::AdmitInput)
            .await
            .unwrap();
        assert_eq!(result["turnId"], "turn");
        let event = rpc.receive().await.unwrap();
        assert_eq!(event["method"], "turn/started");
        rpc.close().await.unwrap();
        server.await.unwrap();
    }

    #[tokio::test]
    async fn connection_loss_releases_every_waiting_rpc() {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = accept_async(stream).await.unwrap();
            let _ = socket.next().await;
            socket.close(None).await.unwrap();
        });
        let connection = CodexConnection::connect(&format!("ws://{address}"))
            .await
            .unwrap();
        let rpc = NativeRpc::new(connection);
        let error = rpc
            .request("turn/start", json!({}), ProviderOperation::AdmitInput)
            .await
            .unwrap_err();
        assert!(
            matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Transport)
        );
        let _ = rpc.close().await;
        server.await.unwrap();
    }
}
