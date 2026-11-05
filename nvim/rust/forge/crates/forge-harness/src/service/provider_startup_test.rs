use super::*;
use crate::provider::test_support::ProviderTestSession;
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    events::{self, ProviderEventSink},
    provider::Provider,
    session::{BindingId, InputAcceptance, PromptRequest, SessionBinding, SessionRequest},
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
    sinks: Mutex<HashMap<BindingId, ProviderEventSink>>,
}

#[async_trait::async_trait]
impl Provider for StartupProvider {
    async fn model_list(
        &self,
        context: forge_provider_api::OperationContext,
        _: forge_provider_api::QueryContext,
    ) -> std::result::Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        context.check_admission(ProviderOperation::ModelList)?;
        Ok(vec![forge_provider_api::ProviderModel {
            id: "fixture-model".into(),
            service_tiers: vec![forge_provider_api::ServiceTier::Ultrafast],
            reasoning: vec!["high".into()],
            default_reasoning: Some("high".into()),
            selected_reasoning: None,
            context_window: Vec::new(),
            default_context_window: None,
            selected_context_window: None,
            vision: false,
            description: None,
            is_default: true,
        }])
    }

    async fn session_open(
        &self,
        context: forge_provider_api::OperationContext,
        request: SessionRequest,
    ) -> std::result::Result<forge_provider_api::OpenedSession, ProviderError> {
        context.check_admission(ProviderOperation::SessionOpen)?;
        self.calls.fetch_add(1, Ordering::AcqRel);
        self.request.lock().unwrap().push(request.clone());
        self.starting.store(true, Ordering::Release);
        self.entered.notify_one();
        tokio::select! {
            permit = self.release.acquire() => permit.map_err(|_| ProviderError::Cancelled)?.forget(),
            interruption = context.interrupted() => {
                self.starting.store(false, Ordering::Release);
                return Err(interruption.failure(ProviderOperation::SessionOpen, forge_provider_api::ProviderPhase::Preparation, forge_provider_api::FailureDisposition::Unchanged));
            }
        }
        if let Err(error) = context.check_admission(ProviderOperation::SessionOpen) {
            self.starting.store(false, Ordering::Release);
            return Err(error);
        }
        if request
            .history
            .resume_id()
            .is_some_and(|id| id.as_str() == "missing-provider-thread")
        {
            self.starting.store(false, Ordering::Release);
            return Err(startup_failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::SessionMissing,
            ));
        }
        if self.fail {
            self.starting.store(false, Ordering::Release);
            return Err(startup_failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Initialization,
            ));
        }
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            startup_failure(ProviderOperation::SessionOpen, ProviderFailureKind::Unknown)
        })?;
        let scope = forge_provider_api::EventScope::Binding {
            owner: request.owner.clone(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), scope);
        self.sinks.lock().await.insert(binding_id.clone(), sink);
        self.starting.store(false, Ordering::Release);
        Ok(forge_provider_api::OpenedSession {
            capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
            events: stream,
            binding: SessionBinding {
                id: binding_id,
                provider_session_id: Some(request.history.resume_id().cloned().unwrap_or_else(
                    || {
                        "prepared-provider-thread"
                            .to_owned()
                            .try_into()
                            .expect("fixture provider ID is valid")
                    },
                )),
            },
        })
    }

    async fn input_submit(
        &self,
        context: forge_provider_api::OperationContext,
        _: &BindingId,
        _: PromptRequest,
    ) -> std::result::Result<InputAcceptance, ProviderError> {
        context.check_admission(ProviderOperation::InputSubmit)?;
        Err(startup_failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn session_configure(
        &self,
        context: forge_provider_api::OperationContext,
        _: &BindingId,
        _: forge_provider_api::SessionConfiguration,
    ) -> std::result::Result<forge_provider_api::ConfiguredSession, ProviderError> {
        context.check_admission(ProviderOperation::SessionConfigure)?;
        Err(startup_failure(
            ProviderOperation::SessionConfigure,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn session_close(
        &self,
        context: forge_provider_api::OperationContext,
        binding: &BindingId,
    ) -> std::result::Result<(), ProviderError> {
        context.check_admission(ProviderOperation::SessionClose)?;
        self.sinks.lock().await.remove(binding);
        Ok(())
    }
    async fn cancel(
        &self,
        context: forge_provider_api::OperationContext,
        _: &BindingId,
        _: forge_provider_api::CancelRequest,
    ) -> std::result::Result<forge_provider_api::InvocationSettlement, ProviderError> {
        context.check_admission(ProviderOperation::Cancel)?;
        Err(startup_failure(
            ProviderOperation::Cancel,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn inspect(
        &self,
        context: forge_provider_api::OperationContext,
        _: forge_provider_api::catalog::WorkspaceContext,
    ) -> std::result::Result<forge_provider_api::ProviderSetup, ProviderError> {
        context.check_admission(ProviderOperation::Inspect)?;
        Err(startup_failure(
            ProviderOperation::Inspect,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn shutdown(
        &self,
        context: forge_provider_api::OperationContext,
    ) -> std::result::Result<(), ProviderError> {
        context.check_admission(ProviderOperation::Shutdown)?;
        self.sinks.lock().await.clear();
        Ok(())
    }
    async fn terminal_list(
        &self,
        context: forge_provider_api::OperationContext,
        _: &BindingId,
    ) -> std::result::Result<TerminalSnapshot, ProviderError> {
        context.check_admission(ProviderOperation::TerminalList)?;
        Ok(TerminalSnapshot {
            supported: true,
            starting: self.starting.load(Ordering::Acquire),
            terminal: Vec::new(),
        })
    }
}

impl StartupProvider {
    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("registry fixture identity"),
        )
        .expect("registry fixture descriptor")
    }

    fn new(fail: bool, permits: usize) -> Self {
        Self {
            entered: Notify::new(),
            release: tokio::sync::Semaphore::new(permits),
            calls: AtomicUsize::new(0),
            request: StandardMutex::new(Vec::new()),
            fail,
            starting: AtomicBool::new(false),
            sinks: Mutex::new(HashMap::new()),
        }
    }
}

fn startup_failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Discard an inherited context override before any native session or prompt can consume it.
#[tokio::test]
async fn saved_context_is_resolved_before_native_startup() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let initialize: InitializeRequest = serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"saved-context", "provider":{"kind":"mock","command":[]},
    }))?;
    let broker = HarnessBroker::initialize(initialize.clone())?;
    let workspace = broker.snapshot()?.session.workspace;
    drop(broker);
    let mut store = SqliteStore::open(&fixture.path().join("data"))?;
    let mut preference = store.load_preference(&workspace, "mock")?.unwrap();
    preference.model = "fixture-model".into();
    preference.effort = "high".into();
    preference.service_tier = forge_provider_api::ServiceTier::Ultrafast;
    preference.model_setting.insert(
        "fixture-model".into(),
        crate::session::ModelSetting {
            reasoning: Some("high".into()),
            context_window: Some("default".into()),
        },
    );
    store.save_preference(&workspace, "mock", &preference)?;
    drop(store);
    let mut initialize = initialize;
    initialize.new_session_name = Some("fresh".into());
    let mut broker = HarnessBroker::initialize(initialize)?;
    assert_eq!(
        broker.snapshot()?.session.context_window.as_deref(),
        Some("default")
    );
    let provider = Arc::new(StartupProvider::new(false, 1));
    let native =
        ProviderTestSession::from_registered(forge_providers::registry::RegisteredProvider::new(
            provider.descriptor(),
            provider.clone(),
            None,
        ))?;
    broker.set_provider_for_test(native.provider_handle());
    let controller = SessionController::new(broker).await?;
    let session = controller.observer.snapshot()?.session;
    assert!(session.context_window.is_none());
    assert_eq!(session.effort, "high");
    assert_eq!(
        session.service_tier,
        forge_provider_api::ServiceTier::Ultrafast
    );
    assert_eq!(provider.calls.load(Ordering::Acquire), 0);
    let request = controller
        .broker
        .lock()
        .await
        .initialization_request()?
        .unwrap();
    assert!(request.context_window.is_none());
    let (sink, _output) = forge_protocol::outbound::channel();
    controller.start_provider(&sink).await;
    controller
        .provider_startup
        .lock()
        .await
        .take()
        .unwrap()
        .await?;
    {
        let opened = provider.request.lock().unwrap();
        assert_eq!(opened.len(), 1);
        assert!(opened[0].configuration.model.context_window.is_none());
    }
    controller.finish_exchange_owner().await?;
    Ok(())
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
        let controller = SessionController::new(broker).await?;
        let provider = Arc::new(StartupProvider::new(fail, 0));
        let provider_fixture = ProviderTestSession::from_registered(
            forge_providers::registry::RegisteredProvider::new(
                provider.descriptor(),
                provider.clone(),
                None,
            ),
        )?;
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
        let expected_event = if fail {
            "provider_event"
        } else {
            "session_configured"
        };
        let mut observed = Vec::new();
        let event = tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                let frame = output.recv().await?.context("startup output closed")?;
                let event: Value = serde_json::from_slice(frame.bytes())?;
                observed.push(event.clone());
                if event["event"] == expected_event {
                    return Ok::<_, anyhow::Error>(event);
                }
            }
        })
        .await
        .with_context(|| {
            format!(
                "startup event was not observed: {observed:?}; failure={:?}",
                controller.provider_startup_error.lock().unwrap()
            )
        })??;
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
        let controller = SessionController::new(broker).await?;
        let provider = Arc::new(StartupProvider::new(false, 2));
        let provider_fixture = ProviderTestSession::from_registered(
            forge_providers::registry::RegisteredProvider::new(
                provider.descriptor(),
                provider.clone(),
                None,
            ),
        )?;
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
                    .history
                    .resume_id()
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
                    .history
                    .resume_id()
                    .cloned()
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
    let controller = SessionController::new(broker).await?;
    let provider = Arc::new(StartupProvider::new(false, 0));
    let provider_fixture =
        ProviderTestSession::from_registered(forge_providers::registry::RegisteredProvider::new(
            provider.descriptor(),
            provider.clone(),
            None,
        ))?;
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
    controller.finish_exchange_owner().await?;
    drop(controller);
    let reopened = HarnessBroker::initialize(serde_json::from_value(parameters)?)?;
    let snapshot = reopened.snapshot()?;
    assert_eq!(snapshot.session.id, session_id);
    assert_eq!(snapshot.session.name, "After provider startup");
    assert!(snapshot.session.provider_session_id.is_none());
    Ok(())
}
