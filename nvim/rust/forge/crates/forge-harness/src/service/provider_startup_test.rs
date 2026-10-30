use super::*;
use crate::backend::{
    Backend, BackendCapability, BackendDescriptor, BackendEventSink, BackendKind, BackendRequest,
};
use std::sync::atomic::AtomicUsize;

struct StartupBackend {
    entered: Notify,
    release: tokio::sync::Semaphore,
    calls: AtomicUsize,
    fail: bool,
}

#[async_trait::async_trait]
impl Backend for StartupBackend {
    async fn initialize_session(&self, request: BackendRequest) -> Result<Option<String>> {
        assert!(request.input.text().is_empty());
        self.calls.fetch_add(1, Ordering::AcqRel);
        self.entered.notify_one();
        self.release.acquire().await.unwrap().forget();
        if self.fail {
            anyhow::bail!("fixture startup failure");
        }
        Ok(Some("prepared-provider-thread".into()))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
    async fn cleanup_execution(&self, _: &str) -> Result<()> {
        Ok(())
    }
    async fn cancel_session(&self, _: &str) -> Result<()> {
        Ok(())
    }
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }
    async fn active_session_id_for(&self, _: &str) -> Option<String> {
        None
    }
    fn descriptor(&self) -> BackendDescriptor {
        BackendDescriptor {
            kind: BackendKind::Mock,
            label: "startup fixture".into(),
            capability: BackendCapability::default(),
        }
    }
    async fn prompt_stream(
        &self,
        _: BackendRequest,
        _: BackendEventSink,
    ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
        panic!("initialization must not submit a model turn")
    }
}

#[tokio::test]
async fn provider_startup_is_nonblocking_persists_identity_and_reports_failure() -> Result<()> {
    for (fail, stale) in [(false, false), (true, false), (false, true)] {
        let fixture = tempfile::tempdir()?;
        let initialize = serde_json::from_value(json!({
            "workspace":fixture.path(), "data_root":fixture.path().join("data"),
            "client_id":"startup-fixture", "backend":{"kind":"mock","command":[]},
        }))?;
        let broker = HarnessBroker::initialize(initialize)?;
        let mut controller = SessionController::new(broker)?;
        let backend = Arc::new(StartupBackend {
            entered: Notify::new(),
            release: tokio::sync::Semaphore::new(0),
            calls: AtomicUsize::new(0),
            fail,
        });
        Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
        let (sink, mut output) = forge_protocol::outbound::channel();
        controller.start_provider(&sink).await;
        backend.entered.notified().await;
        controller.start_provider(&sink).await;
        assert_eq!(backend.calls.load(Ordering::Acquire), 1);
        let before = controller.observer.snapshot()?;
        assert!(before.session.backend_session_id.is_none());
        assert!(
            SqliteStore::read_only(&controller.data_root)?
                .list_exchange(&before.session.id)?
                .is_empty()
        );
        assert!(
            controller.broker.try_lock().is_ok(),
            "startup held the broker during network work"
        );
        if stale {
            controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
        }
        backend.release.add_permits(1);
        let task = controller.provider_startup.lock().await.take().unwrap();
        task.await?;
        if stale {
            assert!(
                controller
                    .observer
                    .snapshot()?
                    .session
                    .backend_session_id
                    .is_none()
            );
            continue;
        }
        let frame = output.recv().await?.unwrap();
        let event: Value = serde_json::from_slice(frame.bytes())?;
        if fail {
            assert_eq!(event["event"], "backend_event");
            assert!(
                event["payload"]["text"]
                    .as_str()
                    .unwrap()
                    .contains("fixture startup failure")
            );
            assert!(controller.provider_startup_error.lock().unwrap().is_some());
        } else {
            assert_eq!(event["event"], "session_configured");
            assert_eq!(
                controller
                    .observer
                    .snapshot()?
                    .session
                    .backend_session_id
                    .as_deref(),
                Some("prepared-provider-thread")
            );
            assert!(
                SqliteStore::read_only(&controller.data_root)?
                    .list_exchange(&before.session.id)?
                    .is_empty()
            );
            assert_eq!(
                controller
                    .catalog_request
                    .read()
                    .await
                    .backend_session_id
                    .as_deref(),
                Some("prepared-provider-thread")
            );
        }
    }
    Ok(())
}
