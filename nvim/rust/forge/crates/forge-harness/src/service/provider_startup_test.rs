use super::*;
use crate::provider::test_support::ProviderTestSession;
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    events::{self, ProviderEventSink, ProviderEventStream},
    provider::Provider,
    session::{
        BindingId, ContinuationOutcome, InputAcceptance, PromptRequest, RecoveryToken,
        SessionBinding, SessionRequest, SteeringRequest,
    },
    terminal::TerminalSnapshot,
};
use std::{
    collections::HashMap,
    sync::{
        Arc, Mutex as StandardMutex,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};
use tokio::sync::Mutex;

struct StartupProvider {
    entered: Notify,
    release: tokio::sync::Semaphore,
    calls: AtomicUsize,
    request: StandardMutex<Vec<SessionRequest>>,
    fail: bool,
    starting: AtomicBool,
    streams: Mutex<HashMap<BindingId, ProviderEventStream>>,
    sinks: Mutex<HashMap<BindingId, ProviderEventSink>>,
}

#[async_trait::async_trait]
impl Provider for StartupProvider {
    async fn initialize_session(
        &self,
        request: SessionRequest,
    ) -> std::result::Result<SessionBinding, ProviderError> {
        self.calls.fetch_add(1, Ordering::AcqRel);
        self.request.lock().unwrap().push(request.clone());
        self.starting.store(true, Ordering::Release);
        self.entered.notify_one();
        self.release
            .acquire()
            .await
            .map_err(|_| ProviderError::Cancelled)?
            .forget();
        if request
            .provider_session_id
            .as_ref()
            .is_some_and(|id| id.as_str() == "missing-provider-thread")
        {
            self.starting.store(false, Ordering::Release);
            return Err(startup_failure(
                ProviderOperation::RecoverSession,
                ProviderFailureKind::SessionMissing,
            ));
        }
        if self.fail {
            self.starting.store(false, Ordering::Release);
            return Err(startup_failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Initialization,
            ));
        }
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            startup_failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Unknown,
            )
        })?;
        let scope = forge_provider_api::EventScope::SessionOperation {
            owner: request.owner.clone(),
            operation_id: binding_id.as_str().to_owned(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), scope);
        self.streams.lock().await.insert(binding_id.clone(), stream);
        self.sinks.lock().await.insert(binding_id.clone(), sink);
        self.starting.store(false, Ordering::Release);
        Ok(SessionBinding {
            id: binding_id,
            provider_session_id: Some(request.provider_session_id.unwrap_or_else(|| {
                "prepared-provider-thread"
                    .to_owned()
                    .try_into()
                    .expect("fixture provider ID is valid")
            })),
        })
    }

    async fn take_session_events(
        &self,
        binding: &BindingId,
    ) -> std::result::Result<ProviderEventStream, ProviderError> {
        self.streams.lock().await.remove(binding).ok_or_else(|| {
            startup_failure(
                ProviderOperation::CollectEventReader,
                ProviderFailureKind::SessionMissing,
            )
        })
    }
    async fn session_prompt(
        &self,
        _: &BindingId,
        _: PromptRequest,
    ) -> std::result::Result<InputAcceptance, ProviderError> {
        Err(startup_failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn continue_session(
        &self,
        _: &BindingId,
        _: forge_provider_api::ContinuationRequest,
    ) -> std::result::Result<ContinuationOutcome, ProviderError> {
        Err(startup_failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn session_steer(
        &self,
        _: &BindingId,
        _: SteeringRequest,
    ) -> std::result::Result<forge_provider_api::SteeringAcceptance, ProviderError> {
        Err(startup_failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn retire_session(&self, binding: &BindingId) -> std::result::Result<(), ProviderError> {
        self.streams.lock().await.remove(binding);
        self.sinks.lock().await.remove(binding);
        Ok(())
    }
    async fn cancel_session(&self, binding: &BindingId) -> std::result::Result<(), ProviderError> {
        self.retire_session(binding).await
    }
    async fn cleanup_execution(&self, _: &BindingId) -> std::result::Result<(), ProviderError> {
        Ok(())
    }
    async fn fence_session(&self, binding: &BindingId) -> std::result::Result<(), ProviderError> {
        if let Some(sink) = self.sinks.lock().await.get(binding) {
            sink.fail(forge_provider_api::events::EventDeliveryError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Disconnected,
            ));
        }
        Ok(())
    }
    async fn recover_session(
        &self,
        request: SessionRequest,
        _: Option<RecoveryToken>,
    ) -> std::result::Result<SessionBinding, ProviderError> {
        self.initialize_session(request).await
    }
    async fn shutdown(&self) -> std::result::Result<(), ProviderError> {
        Ok(())
    }
    async fn active_session_id_for(
        &self,
        _: &forge_provider_api::ExecutionOwner,
    ) -> std::result::Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
        Ok(None)
    }
    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("registry fixture identity"),
        )
        .expect("registry fixture descriptor")
    }
    async fn has_active_turn(&self, _: &forge_provider_api::ExecutionOwner) -> bool {
        false
    }
    async fn background_terminals(
        &self,
        _: forge_provider_api::ProviderCatalogRequest,
    ) -> std::result::Result<TerminalSnapshot, ProviderError> {
        Ok(TerminalSnapshot {
            supported: true,
            starting: self.starting.load(Ordering::Acquire),
            terminal: Vec::new(),
        })
    }
}

impl StartupProvider {
    fn new(fail: bool, permits: usize) -> Self {
        Self {
            entered: Notify::new(),
            release: tokio::sync::Semaphore::new(permits),
            calls: AtomicUsize::new(0),
            request: StandardMutex::new(Vec::new()),
            fail,
            starting: AtomicBool::new(false),
            streams: Mutex::new(HashMap::new()),
            sinks: Mutex::new(HashMap::new()),
        }
    }
}

fn startup_failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
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
        let provider = Arc::new(StartupProvider::new(fail, 0));
        let provider_fixture = ProviderTestSession::from_native(provider.clone())?;
        *controller.provider.write().unwrap() = provider_fixture.provider_handle();
        controller
            .broker
            .lock()
            .await
            .set_provider_for_test(provider_fixture.provider_handle());
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
            assert!(event["payload"]["text"].as_str().is_some());
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
        let provider = Arc::new(StartupProvider::new(false, 2));
        let provider_fixture = ProviderTestSession::from_native(provider.clone())?;
        *controller.provider.write().unwrap() = provider_fixture.provider_handle();
        controller
            .broker
            .lock()
            .await
            .set_provider_for_test(provider_fixture.provider_handle());
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
    let provider = Arc::new(StartupProvider::new(false, 0));
    let provider_fixture = ProviderTestSession::from_native(provider.clone())?;
    *controller.provider.write().unwrap() = provider_fixture.provider_handle();
    controller
        .broker
        .lock()
        .await
        .set_provider_for_test(provider_fixture.provider_handle());
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
