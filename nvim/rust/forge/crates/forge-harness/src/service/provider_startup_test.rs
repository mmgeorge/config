use super::*;
use crate::provider::ScopeOwner;
use crate::provider::test_support::ProviderTestSession;
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    events::{self, ProviderEventSink},
    provider::Provider,
    session::{BindingId, InputAcceptance, PromptRequest, SessionBinding, SessionRequest},
    terminal::TerminalInventory,
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
            label: None,
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

    async fn thread_open(
        &self,
        context: forge_provider_api::OperationContext,
        request: SessionRequest,
    ) -> std::result::Result<forge_provider_api::OpenedSession, ProviderError> {
        context.check_admission(ProviderOperation::ThreadOpen)?;
        self.calls.fetch_add(1, Ordering::AcqRel);
        self.request.lock().unwrap().push(request.clone());
        self.starting.store(true, Ordering::Release);
        self.entered.notify_one();
        tokio::select! {
            permit = self.release.acquire() => permit.map_err(|_| ProviderError::Cancelled)?.forget(),
            interruption = context.interrupted() => {
                self.starting.store(false, Ordering::Release);
                return Err(interruption.failure(ProviderOperation::ThreadOpen, forge_provider_api::ProviderPhase::Preparation, forge_provider_api::FailureDisposition::Unchanged));
            }
        }
        if let Err(error) = context.check_admission(ProviderOperation::ThreadOpen) {
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
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::SessionMissing,
            ));
        }
        if self.fail {
            self.starting.store(false, Ordering::Release);
            return Err(startup_failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Initialization,
            ));
        }
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            startup_failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Unknown)
        })?;
        let scope = forge_provider_api::EventScope::Binding {
            thread_id: forge_provider_api::ThreadId::from(request.owner().clone()),
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
                thread_id: request.thread_id.clone(),
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

    async fn turn_start(
        &self,
        context: forge_provider_api::OperationContext,
        _: &BindingId,
        _: PromptRequest,
    ) -> std::result::Result<InputAcceptance, ProviderError> {
        context.check_admission(ProviderOperation::TurnStart)?;
        Err(startup_failure(
            ProviderOperation::TurnStart,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn thread_configure(
        &self,
        context: forge_provider_api::OperationContext,
        _: &BindingId,
        _: forge_provider_api::SessionConfiguration,
    ) -> std::result::Result<forge_provider_api::ConfiguredSession, ProviderError> {
        context.check_admission(ProviderOperation::ThreadConfigure)?;
        Err(startup_failure(
            ProviderOperation::ThreadConfigure,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn thread_close(
        &self,
        context: forge_provider_api::OperationContext,
        binding: &BindingId,
    ) -> std::result::Result<(), ProviderError> {
        context.check_admission(ProviderOperation::ThreadClose)?;
        self.sinks.lock().await.remove(binding);
        Ok(())
    }
    async fn turn_interrupt(
        &self,
        context: forge_provider_api::OperationContext,
        _: &BindingId,
        _: forge_provider_api::CancelRequest,
    ) -> std::result::Result<forge_provider_api::TurnInterruptResult, ProviderError> {
        context.check_admission(ProviderOperation::TurnInterrupt)?;
        Err(startup_failure(
            ProviderOperation::TurnInterrupt,
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
    ) -> std::result::Result<TerminalInventory, ProviderError> {
        context.check_admission(ProviderOperation::TerminalList)?;
        Ok(TerminalInventory {
            terminal: Vec::new(),
            complete: !(self.starting.load(Ordering::Acquire)),
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

/// Publish a fixture provider into the controller and its broker before startup runs.
async fn install_startup_provider(
    controller: &Arc<SessionController>,
    provider: &Arc<StartupProvider>,
) -> Result<ProviderTestSession> {
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
    Ok(provider_fixture)
}

/// Run interactive startup to completion and return the failure it published, if any.
async fn finish_startup(controller: &Arc<SessionController>) -> Result<Option<String>> {
    let (sink, _output) = forge_protocol::outbound::channel();
    controller.start_provider(&sink).await;
    controller
        .provider_startup
        .lock()
        .await
        .as_mut()
        .unwrap()
        .await?;
    Ok(controller.provider_startup_error.lock().unwrap().clone())
}

#[tokio::test]
async fn provider_startup_retains_saved_history_until_first_use_with_exchange() -> Result<()> {
    provider_startup_retains_saved_history_case(None, true).await
}

/// Run one saved-identity variant, each in its own test so it reports independently and stays
/// within the per-test time limit.
async fn provider_startup_retains_saved_history_case(
    checkpoint: Option<&str>,
    has_exchange: bool,
) -> Result<()> {
    {
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
        let _provider_fixture = install_startup_provider(&controller, &provider).await?;
        // Missing native history surfaces when the first input resumes it, not at startup.
        assert_eq!(finish_startup(&controller).await?, None);
        assert!(controller.provider_ready().await);
        assert_eq!(provider.calls.load(Ordering::Acquire), 0);
        let persisted = SqliteStore::read_only(&controller.data_root)?
            .load_session(&session.id)?
            .unwrap();
        let expected = (checkpoint.is_some() || has_exchange).then_some("missing-provider-thread");
        assert_eq!(persisted.provider_session_id.as_deref(), expected);
        assert_eq!(
            controller
                .catalog_request
                .read()
                .await
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str()),
            expected
        );
        assert_eq!(persisted.name, "Renamed before the first exchange");
        controller.finish_exchange_owner().await?;
    }
    Ok(())
}

/// Retain the history identity that a lazy first open reports only in its result.
///
/// Codex, Copilot, and ACP report a new conversation's identity in the open result rather than
/// as `Thread.IdentityResolved`, so a restart must still resume the conversation the first
/// input created.
#[tokio::test]
async fn first_input_retains_history_reported_only_by_open_result() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    crate::checks::configure_fixture(fixture.path());
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
    )
    .expect("mock fixture descriptor");
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        |request, _, start, _| async move {
            start.observed(1).await?;
            Ok(crate::provider::test_support::native_result(
                &request,
                forge_provider_api::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
            ))
        },
    );
    native.with_models(vec![serde_json::from_value(json!({
        "id":"mock-model", "reasoning":["low","medium","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?]);
    let provider_fixture = ProviderTestSession::from_native(native)?;
    let parameters = json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"lazy-identity-fixture", "provider":{"kind":"mock","command":[]},
    });
    let mut broker = HarnessBroker::initialize(serde_json::from_value(parameters)?)?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    async {
        while !controller.provider_ready().await {
            tokio::task::yield_now().await;
        }
    }
    .await;
    assert_eq!(
        controller.observer.snapshot()?.session.provider_session_id,
        None
    );
    controller.attach_input(&sink).await?;
    super::prompt::apply(
        &controller,
        forge_protocol::message::Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"first-input","text":"Open the conversation"}),
        },
        &sink,
    )
    .await?;
    let session = controller.observer.session_id().to_owned();
    let retained = async {
        loop {
            if let Some(history) = SqliteStore::read_only(&controller.data_root)?
                .load_session(&session)?
                .and_then(|session| session.provider_session_id)
            {
                return Ok::<_, anyhow::Error>(history);
            }
            tokio::task::yield_now().await;
        }
    }
    .await?;
    assert_eq!(
        controller
            .observer
            .snapshot()?
            .session
            .provider_session_id
            .as_deref(),
        Some(retained.as_str())
    );
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    controller.finish_exchange_owner().await?;
    provider_fixture.provider_handle().shutdown().await?;
    controller.finish_session_events().await?;
    reader.abort();
    Ok(())
}
