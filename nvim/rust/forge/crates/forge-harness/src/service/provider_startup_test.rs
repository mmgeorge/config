use super::*;
use crate::provider::{
    ProviderCapability, ProviderDescriptor, ProviderEventSink, ProviderKind, ProviderRequest,
};
use std::sync::atomic::AtomicUsize;

struct StartupProvider {
    fixture_events: std::sync::Arc<crate::provider::test_support::SessionEvents>,
    entered: Notify,
    release: tokio::sync::Semaphore,
    calls: AtomicUsize,
    request: std::sync::Mutex<Vec<ProviderRequest>>,
    fail: bool,
}

#[async_trait::async_trait]
impl crate::provider::ProviderAdapter for StartupProvider {
    type Host = crate::provider::HarnessProviderHost;
    async fn initialize_session(
        &self,
        request: ProviderRequest,
    ) -> std::result::Result<
        Option<forge_provider_api::ProviderSessionId>,
        crate::provider::ProviderError,
    > {
        let result: anyhow::Result<Option<forge_provider_api::ProviderSessionId>> = async move {
            assert!(request.input.text().is_empty());
            self.calls.fetch_add(1, Ordering::AcqRel);
            self.request.lock().unwrap().push(request.clone());
            self.entered.notify_one();
            self.release.acquire().await.unwrap().forget();
            if request.provider_session_id.as_ref().map(|id| id.as_str())
                == Some("missing-provider-thread")
            {
                return Err(crate::provider::ProviderError::SessionMissing {
                    message: "fixture missing provider thread".into(),
                }
                .into());
            }
            if self.fail {
                anyhow::bail!("fixture startup failure");
            }
            Ok(Some("prepared-provider-thread".to_owned().try_into()?))
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::InitializeSession,
                source,
            )
        })
    }

    async fn shutdown(&self) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move { Ok(()) }.await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::Shutdown,
                source,
            )
        })
    }
    async fn cleanup_execution(
        &self,
        _: &str,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move { Ok(()) }.await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CleanupExecution,
                source,
            )
        })
    }
    async fn cancel_session(
        &self,
        _: &str,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move { Ok(()) }.await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CancelSession,
                source,
            )
        })
    }
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }
    async fn active_session_id_for(
        &self,
        _: &str,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>> {
        Ok(None)
    }
    fn descriptor(&self) -> ProviderDescriptor {
        ProviderDescriptor {
            kind: ProviderKind::Mock,
            label: "startup fixture".into(),
            capability: ProviderCapability::default(),
        }
    }

    async fn session_events(
        &self,
        owner: crate::provider::ExecutionOwner,
    ) -> std::result::Result<
        crate::provider::events::ProviderEventStream,
        crate::provider::ProviderError,
    > {
        let stream = self.fixture_events.subscribe(owner.clone())?;
        Ok(stream)
    }

    async fn session_prompt(
        self: std::sync::Arc<Self>,
        request: crate::provider::ProviderRequest,
        scope: crate::provider::InvocationScope,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let events = self.fixture_events.clone();
        events
            .submit(
                request,
                scope,
                crate::provider::TerminalBoundary::LocalFixture,
                move |request, sink| async move { self.run_native(request, sink).await },
            )
            .await
    }
}

impl StartupProvider {
    async fn run_native(
        &self,
        _: ProviderRequest,
        _: ProviderEventSink,
    ) -> Result<crate::provider::ProviderCompletion, crate::provider::ProviderError> {
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
            "client_id":"startup-fixture", "provider":{"kind":"mock","command":[]},
        }))?;
        let broker = HarnessBroker::initialize(initialize)?;
        let controller = SessionController::new(broker)?;
        let provider = Arc::new(StartupProvider {
            fixture_events: Default::default(),
            entered: Notify::new(),
            release: tokio::sync::Semaphore::new(0),
            calls: AtomicUsize::new(0),
            request: Default::default(),
            fail,
        });
        *controller.provider.write().unwrap() = provider.clone();
        let (sink, mut output) = forge_protocol::outbound::channel();
        controller.start_provider(&sink).await;
        provider.entered.notified().await;
        assert!(!controller.provider_ready().await);
        assert!(controller.background_terminals().await?.starting);
        controller.start_provider(&sink).await;
        assert_eq!(provider.calls.load(Ordering::Acquire), 1);
        let before = controller.observer.snapshot()?;
        assert!(before.session.provider_session_id.is_none());
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
        provider.release.add_permits(1);
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
                    .provider_session_id
                    .is_none()
            );
            continue;
        }
        let frame = output.recv().await?.unwrap();
        let event: Value = serde_json::from_slice(frame.bytes())?;
        if fail {
            assert_eq!(event["event"], "provider_event");
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
                    .provider_session_id
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
                    .provider_session_id
                    .as_ref()
                    .map(|id| id.as_str()),
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
            "client_id":"resume-fixture", "provider":{"kind":"mock","command":[]},
        });
        let broker = HarnessBroker::initialize(serde_json::from_value(parameters.clone())?)?;
        let mut session = broker.snapshot()?.session;
        session.provider_session_id = Some("missing-provider-thread".into());
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
        let controller = SessionController::new(broker)?;
        let provider = Arc::new(StartupProvider {
            fixture_events: Default::default(),
            entered: Notify::new(),
            release: tokio::sync::Semaphore::new(2),
            calls: AtomicUsize::new(0),
            request: Default::default(),
            fail: false,
        });
        *controller.provider.write().unwrap() = provider.clone();
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
            assert_eq!(provider.calls.load(Ordering::Acquire), 1);
            assert!(controller.provider_ready().await);
            assert!(
                provider.request.lock().unwrap()[0]
                    .provider_session_id
                    .is_none()
            );
            assert_eq!(
                persisted.provider_session_id.as_deref(),
                Some("prepared-provider-thread")
            );
            assert_eq!(
                controller
                    .catalog_request
                    .read()
                    .await
                    .provider_session_id
                    .as_ref()
                    .map(|id| id.as_str()),
                Some("prepared-provider-thread")
            );
        } else {
            assert_eq!(provider.calls.load(Ordering::Acquire), 1);
            assert!(!controller.provider_ready().await);
            assert_eq!(
                provider.request.lock().unwrap()[0]
                    .provider_session_id
                    .clone()
                    .map(String::from),
                session.provider_session_id
            );
            assert_eq!(persisted.provider_session_id, session.provider_session_id);
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
        "client_id":"rename-fixture", "provider":{"kind":"mock","command":[]},
    });
    let broker = HarnessBroker::initialize(serde_json::from_value(parameters.clone())?)?;
    let controller = SessionController::new(broker)?;
    let provider = Arc::new(StartupProvider {
        fixture_events: Default::default(),
        entered: Notify::new(),
        release: tokio::sync::Semaphore::new(0),
        calls: AtomicUsize::new(0),
        request: Default::default(),
        fail: false,
    });
    *controller.provider.write().unwrap() = provider.clone();
    let session_id = controller.observer.session_id().to_owned();
    controller.observer.query(
        RuntimeMethod::SessionRename,
        json!({
            "session_id":session_id,"name":"Before provider startup"
        }),
    )?;
    assert_eq!(provider.calls.load(Ordering::Acquire), 0);
    let (sink, _output) = forge_protocol::outbound::channel();
    controller.start_provider(&sink).await;
    provider.entered.notified().await;
    controller.observer.query(
        RuntimeMethod::SessionRename,
        json!({
            "session_id":session_id,"name":"While provider startup is pending"
        }),
    )?;
    provider.release.add_permits(1);
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
    assert_eq!(provider.calls.load(Ordering::Acquire), 1);
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
    assert!(snapshot.session.provider_session_id.is_none());
    Ok(())
}
