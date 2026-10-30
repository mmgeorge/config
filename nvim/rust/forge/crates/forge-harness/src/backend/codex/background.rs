use super::CodexConnection;
use crate::backend::{BackendEventSink, ExecutionOwner};
use anyhow::Result;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{Mutex, oneshot};
use tokio::task::JoinHandle;

/// Preserve session delivery while a native command outlives its foreground invocation.
#[derive(Default)]
pub(super) struct BackgroundDelivery {
    sink: Mutex<HashMap<String, BackendEventSink>>,
    reader: Mutex<HashMap<String, ReaderSlot>>,
}

#[derive(Default)]
struct ReaderSlot {
    generation: Arc<()>,
    active: Option<IdleReader>,
}

struct IdleReader {
    stop: oneshot::Sender<()>,
    task: JoinHandle<Result<()>>,
}

impl IdleReader {
    /// Revoke the idle wait and join any complete frame publication already in flight.
    async fn stop(self) -> Result<()> {
        let _ = self.stop.send(());
        self.task.await?
    }
}

impl BackgroundDelivery {
    /// Register the durable observer without borrowing an invocation's short-lived stream.
    pub(super) async fn subscribe(
        &self,
        owner: ExecutionOwner,
    ) -> crate::backend::events::BackendEventStream {
        let key = owner.runtime_key();
        let (sink, stream) =
            crate::backend::events::channel(crate::backend::EventScope::SessionOperation {
                owner,
                operation_id: uuid::Uuid::new_v4().to_string(),
            });
        self.sink.lock().await.insert(key, sink);
        stream
    }

    /// Get the registered same-owner session delivery channel.
    pub(super) async fn sink(&self, key: &str) -> Option<BackendEventSink> {
        self.sink.lock().await.get(key).cloned()
    }

    /// Release the sole socket reader before a new command acquires the connection.
    pub(super) async fn stop(&self, key: &str) -> Result<()> {
        let reader = {
            let mut reader = self.reader.lock().await;
            let slot = reader.entry(key.to_owned()).or_default();
            slot.generation = Arc::new(());
            slot.active.take()
        };
        if let Some(reader) = reader {
            reader.stop().await?;
        }
        Ok(())
    }

    /// Drain background evidence while idle, yielding only between complete publications.
    pub(super) async fn start(
        &self,
        key: String,
        connection: Arc<Mutex<CodexConnection>>,
    ) -> Result<()> {
        let (generation, previous) = {
            let mut reader = self.reader.lock().await;
            let slot = reader.entry(key.clone()).or_default();
            slot.generation = Arc::new(());
            (slot.generation.clone(), slot.active.take())
        };
        if let Some(previous) = previous {
            previous.stop().await?;
        }
        if self.sink(&key).await.is_none() {
            return Ok(());
        }
        let (stop, mut stopped) = oneshot::channel();
        let task = tokio::spawn(async move {
            let mut connection = tokio::select! {
                biased;
                _ = &mut stopped => return Ok(()),
                connection = connection.lock() => connection,
            };
            if !connection
                .process
                .normalization
                .has_pending_background_tools()
            {
                return Ok(());
            }
            loop {
                let received = tokio::select! {
                    biased;
                    _ = &mut stopped => return Ok(()),
                    received = connection.process.receive_message() => received?,
                };
                connection.process.publish_idle_received(received).await?;
                if !connection
                    .process
                    .normalization
                    .has_pending_background_tools()
                {
                    return Ok(());
                }
            }
        });
        let reader = IdleReader { stop, task };
        let displaced = {
            let mut slots = self.reader.lock().await;
            let slot = slots.entry(key).or_default();
            if Arc::ptr_eq(&slot.generation, &generation) {
                slot.active.replace(reader)
            } else {
                Some(reader)
            }
        };
        if let Some(displaced) = displaced {
            displaced.stop().await?;
        }
        Ok(())
    }

    /// Join every idle reader before shutting down its provider sockets.
    pub(super) async fn stop_readers(&self) -> Result<()> {
        let reader = std::mem::take(&mut *self.reader.lock().await);
        let mut failure = None;
        for (_, slot) in reader {
            if let Some(reader) = slot.active
                && let Err(error) = reader.stop().await
            {
                failure.get_or_insert(error);
            }
        }
        match failure {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    /// Release observer channels only after shutdown has published native settlement.
    pub(super) async fn close(&self) {
        self.sink.lock().await.clear();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::backend::approval::PermissionCoordinator;
    use crate::backend::{BackendEvent, BackendOutput, InvocationScope, ToolOutcome, ToolUpdate};
    use crate::session::ApprovalMode;
    use crate::trace::TraceStore;
    use futures_util::{SinkExt, StreamExt};
    use serde_json::json;

    #[derive(Clone, Copy)]
    enum CompletionReader {
        Idle,
        Rebound,
        Reinitialized,
        StoppedDuringStart,
    }

    #[derive(Clone, Copy)]
    enum ShutdownEvidence {
        Absent,
        Completed,
        CompletedBeforeInventory,
        Rejected,
    }

    /// Shutdown proves exact-command absence before settling its original owner.
    #[tokio::test]
    async fn shutdown_settles_verified_background_command_before_stream_closes() -> Result<()> {
        assert_background_shutdown(ShutdownEvidence::Absent).await
    }

    /// A natural completion during shutdown retains success instead of becoming interrupted.
    #[tokio::test]
    async fn shutdown_preserves_background_completion_racing_inventory() -> Result<()> {
        assert_background_shutdown(ShutdownEvidence::Completed).await
    }

    /// An initially empty inventory still drains and verifies a raced natural completion.
    #[tokio::test]
    async fn shutdown_verifies_initially_empty_background_inventory() -> Result<()> {
        assert_background_shutdown(ShutdownEvidence::CompletedBeforeInventory).await
    }

    /// Failed native cleanup closes delivery without inventing terminal success or interruption.
    #[tokio::test]
    async fn shutdown_reports_background_cleanup_failure_without_false_settlement() -> Result<()> {
        assert_background_shutdown(ShutdownEvidence::Rejected).await
    }

    /// Exercise provider cleanup, original-owner delivery, and EOF through the backend lifecycle.
    async fn assert_background_shutdown(evidence: ShutdownEvidence) -> Result<()> {
        use crate::backend::Backend;
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (socket, _) = listener.accept().await?;
            let mut socket = tokio_tungstenite::accept_async(socket).await?;
            let mut request_count = 0;
            while let Some(frame) = socket.next().await {
                let frame = frame?;
                let Ok(text) = frame.to_text() else { continue };
                let request: serde_json::Value = serde_json::from_str(text)?;
                request_count += 1;
                let result = match request["method"].as_str() {
                    Some("thread/backgroundTerminals/list") if request_count == 1 => {
                        assert_eq!(request["params"]["threadId"], "thread");
                        let output = json!({"method":"item/commandExecution/outputDelta","params":{
                            "threadId":"thread","turnId":"turn","itemId":"command","delta":"partial"}});
                        socket
                            .send(tokio_tungstenite::tungstenite::Message::Text(
                                output.to_string().into(),
                            ))
                            .await?;
                        if matches!(evidence, ShutdownEvidence::CompletedBeforeInventory) {
                            json!({"data":[],"nextCursor":null})
                        } else {
                            json!({"data":[{"itemId":"foreign","processId":"foreign-process"},{"itemId":"command","processId":"owned-process"}],"nextCursor":null})
                        }
                    }
                    Some("thread/backgroundTerminals/terminate") => {
                        assert_eq!(request["params"]["processId"], "owned-process");
                        if matches!(evidence, ShutdownEvidence::Rejected) {
                            socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({
                                "id":request["id"],"error":{"code":-32000,"message":"cleanup refused"}
                            }).to_string().into())).await?;
                            break;
                        }
                        json!({"terminated":true})
                    }
                    Some("thread/backgroundTerminals/list") => {
                        if matches!(
                            evidence,
                            ShutdownEvidence::Completed
                                | ShutdownEvidence::CompletedBeforeInventory
                        ) {
                            let completed = json!({"method":"item/completed","params":{"threadId":"thread","turnId":"turn",
                                "item":{"type":"commandExecution","id":"command","command":"slow","status":"completed","aggregatedOutput":"partial end","exitCode":0}}});
                            socket
                                .send(tokio_tungstenite::tungstenite::Message::Text(
                                    completed.to_string().into(),
                                ))
                                .await?;
                        }
                        json!({"data":[],"nextCursor":null})
                    }
                    method => anyhow::bail!("unexpected shutdown request: {method:?}"),
                };
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({
                            "id":request["id"],"result":result
                        })
                        .to_string()
                        .into(),
                    ))
                    .await?;
                if request_count == 3
                    || (request_count == 2
                        && matches!(evidence, ShutdownEvidence::CompletedBeforeInventory))
                {
                    break;
                }
            }
            Ok::<(), anyhow::Error>(())
        });
        let owner = ExecutionOwner::root("session");
        let invocation = InvocationScope::new(owner.clone(), "old-exchange");
        let (active_sink, _active_stream) = crate::backend::events::channel(invocation.clone());
        let backend = super::super::CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()],
            PermissionCoordinator::transient(&workspace)?,
            Arc::new(TraceStore::open(fixture.path())?),
            None,
        )?;
        let mut stream = backend.background.subscribe(owner).await;
        let mut process = super::super::json_rpc::CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Allow,
            PermissionCoordinator::transient(&workspace)?,
            Some(active_sink),
            Arc::new(TraceStore::open(fixture.path())?),
            "session".into(),
        )
        .await?;
        process.set_background_sink(backend.background.sink("session").await);
        let mut output = BackendOutput::default();
        process.publish_message(json!({"method":"item/started","params":{"threadId":"thread","turnId":"turn",
            "item":{"type":"commandExecution","id":"command","command":"slow","status":"inProgress"}}}), &mut output).await?;
        process.publish_message(json!({"method":"turn/completed","params":{"threadId":"thread","turn":{"id":"turn","status":"completed"}}}), &mut output).await?;
        let connection = Arc::new(Mutex::new(CodexConnection {
            process,
            timing: Vec::new(),
            thread: Some(super::super::CodexThread::Submitted("thread".into())),
        }));
        backend
            .connection_by_session
            .lock()
            .await
            .insert("session".into(), connection.clone());
        backend
            .background
            .start("session".into(), connection)
            .await?;
        let shutdown =
            tokio::time::timeout(std::time::Duration::from_secs(2), backend.shutdown()).await?;
        assert_eq!(
            shutdown.is_err(),
            matches!(evidence, ShutdownEvidence::Rejected)
        );
        let mut terminal = Vec::new();
        let mut text = String::new();
        while let Some(event) =
            tokio::time::timeout(std::time::Duration::from_secs(2), stream.recv()).await??
        {
            assert_eq!(event.scope().invocation(), Some(&invocation));
            match event {
                BackendEvent::Tool {
                    update:
                        ToolUpdate::Output {
                            update: crate::backend::TextUpdate::Append(chunk),
                        },
                    ..
                } => text.push_str(&chunk),
                BackendEvent::Tool {
                    update:
                        ToolUpdate::Output {
                            update: crate::backend::TextUpdate::Replace(snapshot),
                        },
                    ..
                } => text = snapshot,
                BackendEvent::Tool {
                    update: ToolUpdate::Finished { outcome },
                    ..
                } => terminal.push(outcome),
                event => anyhow::bail!("unexpected shutdown event: {event:?}"),
            }
        }
        assert!(text.starts_with("partial"));
        match evidence {
            ShutdownEvidence::Absent => assert_eq!(terminal, vec![ToolOutcome::Interrupted]),
            ShutdownEvidence::Completed | ShutdownEvidence::CompletedBeforeInventory => {
                assert_eq!(terminal, vec![ToolOutcome::Succeeded]);
                assert_eq!(text, "partial end");
            }
            ShutdownEvidence::Rejected => assert!(terminal.is_empty()),
        }
        server.await??;
        Ok(())
    }

    /// Deliver idle completion without requiring another foreground prompt.
    #[tokio::test]
    async fn idle_reader_releases_after_native_background_completion() -> Result<()> {
        assert_background_delivery(CompletionReader::Idle).await
    }

    /// Preserve original ownership when completion arrives during the next invocation.
    #[tokio::test]
    async fn idle_reader_delivers_background_completion_and_releases_socket() -> Result<()> {
        assert_background_delivery(CompletionReader::Rebound).await
    }

    /// Reusing an initialized thread restarts idle delivery before returning.
    #[tokio::test]
    async fn initialization_reuse_resumes_background_reader() -> Result<()> {
        assert_background_delivery(CompletionReader::Reinitialized).await
    }

    /// A stop invalidates a reader that is still waiting to acquire its socket.
    #[tokio::test]
    async fn idle_reader_start_invalidated_during_connection_wait() -> Result<()> {
        assert_background_delivery(CompletionReader::StoppedDuringStart).await
    }

    /// Exercise the real socket reader and the session-scoped event channel together.
    async fn assert_background_delivery(completion_reader: CompletionReader) -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let (send, receive) = oneshot::channel();
        let (finish, finishing) = oneshot::channel();
        let (release, released) = oneshot::channel();
        let server = tokio::spawn(async move {
            let (socket, _) = listener.accept().await?;
            let mut socket = tokio_tungstenite::accept_async(socket).await?;
            receive.await?;
            for message in [
                json!({"method":"item/commandExecution/outputDelta","params":{"threadId":"foreign","turnId":"turn","itemId":"command","delta":"foreign"}}),
                json!({"method":"item/commandExecution/outputDelta","params":{"threadId":"thread","turnId":"turn","itemId":"command","delta":"first"}}),
            ] {
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        message.to_string().into(),
                    ))
                    .await?;
            }
            finishing.await?;
            let completed = json!({"method":"item/completed","params":{"threadId":"thread","turnId":"turn",
                "item":{"type":"commandExecution","id":"command","command":"slow","status":"completed","aggregatedOutput":"first end","exitCode":0}}});
            socket
                .send(tokio_tungstenite::tungstenite::Message::Text(
                    completed.to_string().into(),
                ))
                .await?;
            let _ = released.await;
            Ok::<(), anyhow::Error>(())
        });
        let owner = ExecutionOwner::root("session");
        let invocation = InvocationScope::new(owner.clone(), "old-exchange");
        let (active_sink, _active_stream) = crate::backend::events::channel(invocation.clone());
        let backend = super::super::CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()],
            PermissionCoordinator::transient(&workspace)?,
            Arc::new(TraceStore::open(fixture.path())?),
            None,
        )?;
        let delivery = &backend.background;
        let mut stream = delivery.subscribe(owner).await;
        let mut process = super::super::json_rpc::CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Allow,
            PermissionCoordinator::transient(&workspace)?,
            Some(active_sink),
            Arc::new(TraceStore::open(fixture.path())?),
            "session".into(),
        )
        .await?;
        process.set_background_sink(delivery.sink("session").await);
        let mut output = BackendOutput::default();
        process.publish_message(json!({"method":"item/started","params":{"threadId":"thread","turnId":"turn",
            "item":{"type":"commandExecution","id":"command","command":"slow","status":"inProgress"}}}), &mut output).await?;
        process.publish_message(json!({"method":"turn/completed","params":{"threadId":"thread","turn":{"id":"turn","status":"completed"}}}), &mut output).await?;
        let connection = Arc::new(Mutex::new(CodexConnection {
            process,
            timing: Vec::new(),
            thread: Some(super::super::CodexThread::Submitted("thread".into())),
        }));
        backend
            .connection_by_session
            .lock()
            .await
            .insert("session".into(), connection.clone());
        if matches!(completion_reader, CompletionReader::StoppedDuringStart) {
            let blocked = connection.lock().await;
            let starting = delivery.start("session".into(), connection.clone());
            let stopping = async {
                tokio::time::timeout(std::time::Duration::from_secs(2), async {
                    loop {
                        if delivery.reader.lock().await.contains_key("session") {
                            break;
                        }
                        tokio::task::yield_now().await;
                    }
                })
                .await?;
                delivery.stop("session").await?;
                Ok::<(), anyhow::Error>(())
            };
            let (started, stopped) =
                tokio::time::timeout(std::time::Duration::from_secs(2), async {
                    tokio::join!(starting, stopping)
                })
                .await?;
            started?;
            stopped?;
            drop(blocked);
            assert!(delivery.reader.lock().await["session"].active.is_none());
            assert!(connection.try_lock().is_ok());
            send.send(())
                .map_err(|_| anyhow::anyhow!("test provider disappeared"))?;
            finish
                .send(())
                .map_err(|_| anyhow::anyhow!("test provider disappeared"))?;
            let _ = release.send(());
            server.await??;
            return Ok(());
        }
        delivery.start("session".into(), connection.clone()).await?;
        send.send(())
            .map_err(|_| anyhow::anyhow!("test provider disappeared"))?;
        let idle = tokio::time::timeout(std::time::Duration::from_secs(2), stream.recv())
            .await??
            .ok_or_else(|| anyhow::anyhow!("session stream closed"))?;
        assert_eq!(idle.scope().invocation(), Some(&invocation));
        assert!(
            matches!(idle, BackendEvent::Tool { update: ToolUpdate::Output { update: crate::backend::TextUpdate::Append(ref text) }, .. } if text == "first")
        );
        let mut next_stream = None;
        match completion_reader {
            CompletionReader::StoppedDuringStart => {
                unreachable!("start cancellation returned before delivery")
            }
            CompletionReader::Idle => {
                finish
                    .send(())
                    .map_err(|_| anyhow::anyhow!("test provider disappeared"))?;
            }
            CompletionReader::Reinitialized => {
                use crate::backend::Backend;
                let request = super::super::CodexBackend::catalog_backend_request(
                    &crate::backend::BackendCatalogRequest {
                        execution_owner: None,
                        harness_session_id: "session".into(),
                        workspace: workspace.clone(),
                        access: Default::default(),
                        approval_mode: ApprovalMode::Allow,
                        backend_session_id: Some("thread".into()),
                    },
                );
                assert_eq!(
                    backend.initialize_session(request).await?,
                    Some("thread".into())
                );
                finish
                    .send(())
                    .map_err(|_| anyhow::anyhow!("test provider disappeared"))?;
            }
            CompletionReader::Rebound => {
                tokio::time::timeout(std::time::Duration::from_secs(2), delivery.stop("session"))
                    .await??;
                assert!(connection.try_lock().is_ok());
                let next = InvocationScope::new(invocation.owner.clone(), "next-exchange");
                let (next_sink, stream) = crate::backend::events::channel(next);
                next_stream = Some(stream);
                let mut connection = connection.lock().await;
                connection
                    .process
                    .set_request_context(&workspace, ApprovalMode::Allow, Some(next_sink))
                    .await?;
                finish
                    .send(())
                    .map_err(|_| anyhow::anyhow!("test provider disappeared"))?;
                connection
                    .process
                    .read_message(&mut BackendOutput::default())
                    .await?;
                assert!(!connection.process.normalization.replay_sensitive);
                assert!(
                    !connection
                        .process
                        .normalization
                        .has_pending_background_tools()
                );
            }
        }
        loop {
            let event = tokio::time::timeout(std::time::Duration::from_secs(2), stream.recv())
                .await??
                .ok_or_else(|| anyhow::anyhow!("session stream closed"))?;
            assert_eq!(event.scope().invocation(), Some(&invocation));
            match event {
                BackendEvent::Tool {
                    update: ToolUpdate::Output { .. },
                    ..
                } => {}
                BackendEvent::Tool {
                    update:
                        ToolUpdate::Finished {
                            outcome: ToolOutcome::Succeeded,
                        },
                    ..
                } => break,
                event => anyhow::bail!("unexpected retained event: {event:?}"),
            }
        }
        if let Some(stream) = &mut next_stream {
            assert!(stream.try_recv().is_err());
        }
        tokio::time::timeout(std::time::Duration::from_secs(2), delivery.stop("session")).await??;
        assert!(connection.try_lock().is_ok());
        let _ = release.send(());
        server.await??;
        Ok(())
    }
}
