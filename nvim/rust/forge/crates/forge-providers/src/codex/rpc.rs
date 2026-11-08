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

/// Distinguish a runtime that lacks a request method from every answered reply.
pub(super) enum MethodReply {
    /// The runtime answered the request; the value is its decoded `result`.
    Answered(Value),
    /// The runtime predates the method and rejected its name before reading the request.
    UnknownMethod,
}

/// Share native RPC methods without serializing their independent send and receive paths.
#[derive(Clone)]
pub(super) struct RpcHandle {
    current: Arc<tokio::sync::RwLock<Arc<NativeRpc>>>,
    replacements: tokio::sync::watch::Sender<()>,
}

pub(super) struct RpcGuard(Arc<NativeRpc>);

impl RpcHandle {
    #[cfg(test)]
    pub(super) fn new(rpc: NativeRpc) -> Self {
        Self::from_shared(Arc::new(rpc))
    }
    /// Retain an already shared native dispatcher through binding replacement.
    pub(super) fn from_shared(rpc: Arc<NativeRpc>) -> Self {
        Self {
            current: Arc::new(tokio::sync::RwLock::new(rpc)),
            replacements: tokio::sync::watch::channel(()).0,
        }
    }
    pub(super) async fn lock(&self) -> RpcGuard {
        RpcGuard(self.current.read().await.clone())
    }
    pub(super) async fn replace_shared(&self, rpc: Arc<NativeRpc>) -> Arc<NativeRpc> {
        let previous = std::mem::replace(&mut *self.current.write().await, rpc);
        self.replacements.send_replace(());
        previous
    }
    /// Observe dispatcher replacements without querying its current identity repeatedly.
    pub(super) fn replacements(&self) -> tokio::sync::watch::Receiver<()> {
        self.replacements.subscribe()
    }
}

impl RpcGuard {
    /// Clone the dispatcher without exposing the replacement lock.
    pub(super) fn shared(&self) -> Arc<NativeRpc> {
        self.0.clone()
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
    notifications: Mutex<mpsc::UnboundedReceiver<Result<ReceivedMessage, ProviderError>>>,
    reader: Mutex<Option<JoinHandle<()>>>,
    terminal_changes: tokio::sync::watch::Sender<()>,
    closed_threads: tokio::sync::watch::Sender<
        std::collections::HashMap<forge_provider_api::ProviderSessionId, uuid::Uuid>,
    >,
}

impl NativeRpc {
    /// Create a response dispatcher that keeps native request waiters independent of its reader.
    pub(super) fn new(connection: CodexConnection) -> Self {
        let connection = Arc::new(connection);
        let waiters = Arc::new(Mutex::new(HashMap::<
            u64,
            oneshot::Sender<Result<Value, ProviderError>>,
        >::new()));
        let (notification_tx, notifications) = mpsc::unbounded_channel();
        let reader_connection = connection.clone();
        let reader_waiters = waiters.clone();
        let (terminal_changes, _) = tokio::sync::watch::channel(());
        let terminal_updates = terminal_changes.clone();
        let (closed_threads, _) = tokio::sync::watch::channel(std::collections::HashMap::new());
        let thread_updates = closed_threads.clone();
        let reader = tokio::spawn(async move {
            loop {
                let received = match reader_connection.receive().await {
                    Ok(received) => received,
                    Err(error) => {
                        let error = crate::error::codex(error, ProviderOperation::TurnStart);
                        let pending = std::mem::take(&mut *reader_waiters.lock().await);
                        for (_, waiter) in pending {
                            let _ = waiter.send(Err(error.clone()));
                        }
                        let _ = notification_tx.send(Err(error));
                        break;
                    }
                };
                let value = match &received.value {
                    Ok(value) => value,
                    Err(_) => {
                        let error =
                            failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol);
                        let pending = std::mem::take(&mut *reader_waiters.lock().await);
                        for (_, waiter) in pending {
                            let _ = waiter.send(Err(error.clone()));
                        }
                        let _ = notification_tx.send(Err(error));
                        break;
                    }
                };
                if let Some(id) = response_id(value) {
                    if let Some(waiter) = reader_waiters.lock().await.remove(&id) {
                        let _ = waiter.send(decode_frame(received));
                    }
                    continue;
                }
                if uncorrelated_error(value) {
                    // Codex rejects an unparseable request, such as an unknown method, before it
                    // reads the request id. Only a sole waiter can own that rejection; with
                    // several in flight the owner is unknowable, so every waiter fails.
                    let mut waiters = reader_waiters.lock().await;
                    if waiters.len() == 1 {
                        let (_, waiter) = waiters.drain().next().expect("one waiter is pending");
                        let _ = waiter.send(decode_frame(received));
                    } else {
                        let error =
                            failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol);
                        for (_, waiter) in waiters.drain() {
                            let _ = waiter.send(Err(error.clone()));
                        }
                    }
                    continue;
                }
                if terminal_inventory_changed(value) {
                    terminal_updates.send_replace(());
                }
                if value.get("method").and_then(Value::as_str) == Some("thread/closed")
                    && let Some(id) = value.pointer("/params/threadId").and_then(Value::as_str)
                    && let Ok(id) = forge_provider_api::ProviderSessionId::try_from(id.to_owned())
                {
                    thread_updates.send_modify(|closed| {
                        closed.insert(id, uuid::Uuid::new_v4());
                    });
                }
                // RPC responses stay dispatchable while notification consumption is paused.
                if notification_tx.send(Ok(received)).is_err() {
                    break;
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
            terminal_changes,
            closed_threads,
        }
    }

    /// Subscribe to native events that can change independently owned terminal inventory.
    pub(super) fn terminal_changes(&self) -> tokio::sync::watch::Receiver<()> {
        self.terminal_changes.subscribe()
    }

    /// Subscribe before unsubscribe so an immediate native close cannot be missed.
    pub(super) fn closed_threads(
        &self,
    ) -> tokio::sync::watch::Receiver<
        std::collections::HashMap<forge_provider_api::ProviderSessionId, uuid::Uuid>,
    > {
        self.closed_threads.subscribe()
    }

    /// Send a correlated request while the dispatcher routes its response independently.
    pub(super) async fn request(
        &self,
        method: &str,
        params: Value,
        operation: ProviderOperation,
    ) -> Result<Value, ProviderError> {
        match self.method_request(method, params, operation).await? {
            MethodReply::Answered(result) => Ok(result),
            MethodReply::UnknownMethod => Err(failure(operation, ProviderFailureKind::Rejected)),
        }
    }

    /// Send a request whose method an older runtime may not define, reporting that absence apart
    /// from every other rejection so the caller can fall back without guessing at error text.
    pub(super) async fn method_request(
        &self,
        method: &str,
        params: Value,
        operation: ProviderOperation,
    ) -> Result<MethodReply, ProviderError> {
        let id = self.send_request(method, params).await?;
        let response = self.wait_reply(id, operation);
        let context = super::operations::current();
        if let Some(context) = context {
            // These identity-addressed mutations retain late acknowledgement so an expired
            // observer cannot authorize blind creation of another fork or terminal action.
            if matches!(
                method,
                "thread/fork" | "thread/backgroundTerminals/terminate"
            ) {
                return response.await;
            }
            tokio::select! {
                result = response => result,
                reason = context.context.interrupted() => {
                    self.abandon_response(id).await;
                    Err(reason.failure(context.operation, forge_provider_api::ProviderPhase::Observation, if context.changed() { forge_provider_api::FailureDisposition::Uncertain } else { forge_provider_api::FailureDisposition::Unchanged }))
                }
            }
        } else {
            response.await
        }
    }

    /// Await one request response without owning the native session reader lock.
    pub(super) async fn wait_response(
        &self,
        id: u64,
        operation: ProviderOperation,
    ) -> Result<Value, ProviderError> {
        match self.wait_reply(id, operation).await? {
            MethodReply::Answered(result) => Ok(result),
            MethodReply::UnknownMethod => Err(failure(operation, ProviderFailureKind::Rejected)),
        }
    }

    /// Await one request response, separating an unknown-method rejection from decoded results.
    async fn wait_reply(
        &self,
        id: u64,
        operation: ProviderOperation,
    ) -> Result<MethodReply, ProviderError> {
        let receiver = self
            .responses
            .lock()
            .await
            .remove(&id)
            .ok_or_else(|| failure(operation, ProviderFailureKind::Protocol))?;
        let method = self.methods.lock().await.remove(&id).unwrap_or_default();
        // The retained operation owns any observation deadline. Native acknowledgements
        // may arrive at turn end, and cleanup must retain correlation until collection.
        let response = receiver.await;
        self.waiters.lock().await.remove(&id);
        let result = response.map_err(|_| failure(operation, ProviderFailureKind::Transport))??;
        if unknown_method(&result, &method) {
            return Ok(MethodReply::UnknownMethod);
        }
        decode_result(&result, operation, &method).map(MethodReply::Answered)
    }

    /// Release obsolete acknowledgement bookkeeping after positive native lifecycle admission.
    /// Later replies remain correlated to this retired ID and cannot affect a successor request.
    pub(super) async fn abandon_response(&self, id: u64) {
        self.waiters.lock().await.remove(&id);
        self.responses.lock().await.remove(&id);
        self.methods.lock().await.remove(&id);
    }

    /// Write one native notification without adding a JSON-RPC request identity.
    pub(super) async fn notify(&self, method: &str, params: Value) -> Result<(), ProviderError> {
        let encoded = serde_json::to_string(&json!({"method":method,"params":params}))
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        self.connection
            .send(encoded)
            .await
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport))
    }

    /// Send one server request and retain its private response waiter before writing.
    pub(super) async fn send_request(
        &self,
        method: &str,
        params: Value,
    ) -> Result<u64, ProviderError> {
        super::operations::check()?;
        let mutates = !matches!(
            method,
            "initialize"
                | "config/read"
                | "model/list"
                | "skills/list"
                | "getAuthStatus"
                | "account/read"
                | "thread/read"
                | "thread/loaded/list"
                | "thread/items/list"
                | "mcpServerStatus/list"
                | "thread/backgroundTerminals/list"
        );
        let id = self
            .next_id
            .fetch_add(1, Ordering::Relaxed)
            .checked_add(1)
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Unknown))?;
        let encoded = serde_json::to_string(&json!({"id":id,"method":method,"params":params}))
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        let (sender, receiver) = oneshot::channel();
        self.waiters.lock().await.insert(id, sender);
        self.responses.lock().await.insert(id, receiver);
        self.methods.lock().await.insert(id, method.to_owned());
        if let Err(error) = super::operations::check() {
            self.abandon_response(id).await;
            return Err(error);
        }
        if mutates {
            super::operations::mark_changed();
        }
        if self.connection.send(encoded).await.is_err() {
            self.waiters.lock().await.remove(&id);
            self.responses.lock().await.remove(&id);
            self.methods.lock().await.remove(&id);
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Transport,
            ));
        }
        Ok(id)
    }

    /// Receive the next unsolicited notification or server request in wire order.
    pub(super) async fn receive(&self) -> Result<Value, ProviderError> {
        let receive = async {
            self.notifications
                .lock()
                .await
                .recv()
                .await
                .ok_or_else(|| {
                    failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport)
                })?
                .and_then(decode_frame)
        };
        match super::operations::current() {
            Some(context) => tokio::select! {
                result = receive => result,
                reason = context.context.interrupted() => Err(reason.failure(context.operation, forge_provider_api::ProviderPhase::Observation, if context.changed() { forge_provider_api::FailureDisposition::Uncertain } else { forge_provider_api::FailureDisposition::Unchanged })),
            },
            None => receive.await,
        }
    }

    /// Respond to a native server request without exposing its identity to Harness.
    pub(super) async fn respond(&self, id: &Value, result: Value) -> Result<(), ProviderError> {
        let encoded = serde_json::to_string(&json!({"id":id,"result":result}))
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        self.connection
            .send(encoded)
            .await
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport))
    }

    /// Reject a native server request when no foreground invocation owns its callbacks.
    pub(super) async fn reject_request(&self, id: &Value) -> Result<(), ProviderError> {
        let encoded = serde_json::to_string(&json!({
            "id":id,"error":{"code":-32001,"message":"No active provider invocation"}
        }))
        .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        self.connection
            .send(encoded)
            .await
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport))
    }

    /// Collect the socket owner and dispatcher after native session work has settled.
    pub(super) async fn close(&self) -> Result<(), ProviderError> {
        // Observation owners must wake before the dispatcher is aborted, including explicit close.
        let pending = std::mem::take(&mut *self.waiters.lock().await);
        for (_, waiter) in pending {
            let _ = waiter.send(Err(failure(
                ProviderOperation::ThreadClose,
                ProviderFailureKind::BindingClosed,
            )));
        }
        let connection = self
            .connection
            .close()
            .await
            .map_err(|_| failure(ProviderOperation::Shutdown, ProviderFailureKind::Transport));
        let mut reader = self.reader.lock().await;
        let collection = if let Some(running) = reader.as_mut() {
            running.abort();
            let result = match running.await {
                Ok(()) => Ok(()),
                Err(error) if error.is_cancelled() => Ok(()),
                Err(_) => Err(failure(
                    ProviderOperation::TurnStart,
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

/// Select native lifecycle notifications without copying streamed output into another queue.
fn terminal_inventory_changed(message: &Value) -> bool {
    matches!(
        message.get("method").and_then(Value::as_str),
        Some(
            "item/started"
                | "item/completed"
                | "item/commandExecution/terminalInteraction"
                | "process/exited"
                | "thread/status/changed"
                | "thread/closed"
        )
    )
}

fn decode_frame(received: ReceivedMessage) -> Result<Value, ProviderError> {
    received
        .value
        .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))
}

/// Recognize an error response whose request id the native runtime could not recover.
fn uncorrelated_error(message: &Value) -> bool {
    message.get("method").is_none()
        && message.get("error").is_some()
        && message.get("id").is_none_or(Value::is_null)
}

/// Recognize the serde rejection Codex sends when its request enum has no variant for `method`.
fn unknown_method(message: &Value, method: &str) -> bool {
    !method.is_empty()
        && message.pointer("/error/code").and_then(Value::as_i64) == Some(-32600)
        && message
            .pointer("/error/message")
            .and_then(Value::as_str)
            .is_some_and(|text| text.contains(&format!("unknown variant `{method}`")))
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
        if matches!(method, "thread/resume" | "thread/fork")
            && error.get("code").and_then(Value::as_i64) == Some(-32600)
            && error
                .get("message")
                .and_then(Value::as_str)
                .is_some_and(|message| {
                    message.starts_with("thread ")
                        && message.ends_with(" already has an active writer")
                })
        {
            return Err(failure(operation, ProviderFailureKind::ResourceConflict));
        }
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
    let disposition = if matches!(
        kind,
        ProviderFailureKind::Rejected | ProviderFailureKind::ResourceConflict
    ) {
        forge_provider_api::error::FailureDisposition::Unchanged
    } else {
        forge_provider_api::error::FailureDisposition::Uncertain
    };
    ProviderError::Failure(ProviderFailure::new(operation, kind).at_phase(
        forge_provider_api::error::ProviderPhase::Observation,
        disposition,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_util::{SinkExt, StreamExt};
    use serde_json::json;
    use tokio::net::TcpListener;
    use tokio_tungstenite::{accept_async, tungstenite::Message};

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
                "thread/unsubscribe",
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
                ProviderOperation::TurnStart, method).unwrap_err();
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
                ProviderOperation::TurnStart,
                "turn/start"
            )
            .unwrap(),
            Value::Null
        );
        for malformed in [json!({"id":17}), json!({"id":17,"result":null,"error":{}})] {
            assert!(
                matches!(decode_result(&malformed, ProviderOperation::TurnStart, "turn/start"),
                Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Protocol)
            );
        }
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
            .request("turn/start", json!({}), ProviderOperation::TurnStart)
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
            .request("turn/start", json!({}), ProviderOperation::TurnStart)
            .await
            .unwrap_err();
        assert!(
            matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Transport)
        );
        let _ = rpc.close().await;
        server.await.unwrap();
    }
}
