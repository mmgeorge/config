use super::*;
use crate::backend::{
    Backend, BackendCapability, BackendDescriptor, BackendEventSink, BackendKind, BackendRequest,
};
use std::sync::atomic::AtomicUsize;

struct StartupBackend {
    entered: Notify,
    release: tokio::sync::Semaphore,
    calls: AtomicUsize,
    request: std::sync::Mutex<Vec<BackendRequest>>,
    fail: bool,
}

#[async_trait::async_trait]
impl Backend for StartupBackend {
    async fn initialize_session(&self, request: BackendRequest) -> Result<Option<String>> {
        assert!(request.input.text().is_empty());
        self.calls.fetch_add(1, Ordering::AcqRel);
        self.request.lock().unwrap().push(request.clone());
        self.entered.notify_one();
        self.release.acquire().await.unwrap().forget();
        if request.backend_session_id.as_deref() == Some("missing-provider-thread") {
            return Err(crate::backend::BackendError::SessionMissing {
                message: "fixture missing provider thread".into(),
            }
            .into());
        }
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
async fn provider_startup_is_nonblocking_persists_ready_identity_and_reports_failure() -> Result<()>
{
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
            request: Default::default(),
            fail,
        });
        Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
        let (sink, mut output) = forge_protocol::outbound::channel();
        controller.start_provider(&sink).await;
        backend.entered.notified().await;
        assert!(!controller.provider_ready().await);
        assert!(controller.background_terminals().await?.starting);
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
        controller
            .provider_startup
            .lock()
            .await
            .as_mut()
            .unwrap()
            .await?;
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

/// Replace an unused thread before startup but preserve one with a recorded checkpoint.
#[tokio::test]
async fn provider_startup_recovers_only_missing_empty_sessions() -> Result<()> {
    for (checkpoint, has_exchange) in [(None, false), (Some("durable-turn"), false), (None, true)] {
        let fixture = tempfile::tempdir()?;
        let parameters = json!({
            "workspace":fixture.path(), "data_root":fixture.path().join("data"),
            "client_id":"resume-fixture", "backend":{"kind":"mock","command":[]},
        });
        let broker = HarnessBroker::initialize(serde_json::from_value(parameters.clone())?)?;
        let mut session = broker.snapshot()?.session;
        session.backend_session_id = Some("missing-provider-thread".into());
        session.provider_checkpoint_id = checkpoint.map(str::to_owned);
        drop(broker);
        let mut store = SqliteStore::open(&fixture.path().join("data"))?;
        store.save_session(&session)?;
        store.acquire_session_lease(&session.id, "resume-fixture")?;
        store.rename_owned_session(
            &session.id,
            "resume-fixture",
            "Renamed before the first exchange",
            None,
            session.updated_at_ms + 1,
        )?;
        if has_exchange {
            let exchange: crate::exchange::Exchange = serde_json::from_value(json!({
                "id":"failed-exchange","session_id":session.id,"agent_id":session.primary_agent_id,
                "ordinal":1,"prompt":"Keep this request","kind":"chat","state":"failed",
                "disposition":"current","turn":[],"node_list":[],"created_at_ms":1,
                "awaiting_input":false,"duration_ms":0,"metrics":{},"comment":[],
                "attributed_matches_checkpoint":false
            }))?;
            store.save_exchange(&exchange)?;
        }
        drop(store);
        let broker = HarnessBroker::initialize(serde_json::from_value(parameters)?)?;
        let mut controller = SessionController::new(broker)?;
        let backend = Arc::new(StartupBackend {
            entered: Notify::new(),
            release: tokio::sync::Semaphore::new(2),
            calls: AtomicUsize::new(0),
            request: Default::default(),
            fail: false,
        });
        Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
        let (sink, _output) = forge_protocol::outbound::channel();
        controller.start_provider(&sink).await;
        controller
            .provider_startup
            .lock()
            .await
            .as_mut()
            .unwrap()
            .await?;
        let persisted = SqliteStore::read_only(&controller.data_root)?
            .load_session(&session.id)?
            .unwrap();
        if checkpoint.is_none() && !has_exchange {
            assert_eq!(backend.calls.load(Ordering::Acquire), 1);
            assert!(controller.provider_ready().await);
            assert!(
                backend.request.lock().unwrap()[0]
                    .backend_session_id
                    .is_none()
            );
            assert_eq!(
                persisted.backend_session_id.as_deref(),
                Some("prepared-provider-thread")
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
        } else {
            assert_eq!(backend.calls.load(Ordering::Acquire), 1);
            assert!(!controller.provider_ready().await);
            assert_eq!(
                backend.request.lock().unwrap()[0].backend_session_id,
                session.backend_session_id
            );
            assert_eq!(persisted.backend_session_id, session.backend_session_id);
            assert!(
                controller
                    .provider_startup_error
                    .lock()
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .contains("retained conversation was preserved")
            );
        }
        assert_eq!(persisted.name, "Renamed before the first exchange");
    }
    Ok(())
}

/// Preserve local renames before and during startup while keeping an empty session replaceable.
#[tokio::test]
async fn rename_without_exchanges_is_local_and_survives_provider_replacement() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let parameters = json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"rename-fixture", "backend":{"kind":"mock","command":[]},
    });
    let broker = HarnessBroker::initialize(serde_json::from_value(parameters.clone())?)?;
    let mut controller = SessionController::new(broker)?;
    let backend = Arc::new(StartupBackend {
        entered: Notify::new(),
        release: tokio::sync::Semaphore::new(0),
        calls: AtomicUsize::new(0),
        request: Default::default(),
        fail: false,
    });
    Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
    let session_id = controller.observer.session_id().to_owned();
    controller.observer.query(
        RuntimeMethod::SessionRename,
        json!({
            "session_id":session_id,"name":"Before provider startup"
        }),
    )?;
    assert_eq!(backend.calls.load(Ordering::Acquire), 0);
    let (sink, _output) = forge_protocol::outbound::channel();
    controller.start_provider(&sink).await;
    backend.entered.notified().await;
    controller.observer.query(
        RuntimeMethod::SessionRename,
        json!({
            "session_id":session_id,"name":"While provider startup is pending"
        }),
    )?;
    backend.release.add_permits(1);
    while !controller.provider_ready().await {
        tokio::task::yield_now().await;
    }
    assert_eq!(
        controller.observer.snapshot()?.session.name,
        "While provider startup is pending"
    );
    controller.observer.query(
        RuntimeMethod::SessionRename,
        json!({
            "session_id":session_id,"name":"After provider startup"
        }),
    )?;
    assert_eq!(backend.calls.load(Ordering::Acquire), 1);
    assert!(
        SqliteStore::read_only(&controller.data_root)?
            .list_exchange(&session_id)?
            .is_empty()
    );
    drop(controller);
    let reopened = HarnessBroker::initialize(serde_json::from_value(parameters)?)?;
    let snapshot = reopened.snapshot()?;
    assert_eq!(snapshot.session.id, session_id);
    assert_eq!(snapshot.session.name, "After provider startup");
    assert!(snapshot.session.backend_session_id.is_none());
    Ok(())
}
