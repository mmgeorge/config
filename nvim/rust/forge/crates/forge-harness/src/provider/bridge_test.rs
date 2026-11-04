use super::*;

use forge_provider_api::{
    BindingId, ExecutionOwner, ProviderError, ProviderFailure, ProviderFailureKind,
    ProviderOperation, ProviderSessionId,
    events::{self, ProviderEventSink, ProviderEventStream},
    provider::Provider as NativeProvider,
    session::{
        ContinuationOutcome, InputAcceptance, PromptRequest, RecoveryToken, SessionBinding,
        SessionRequest, SteeringAcceptance, SteeringRequest,
    },
    turn_control::TurnControl as _,
};
use std::{
    collections::HashMap,
    sync::{
        Arc, Mutex,
        atomic::{AtomicUsize, Ordering},
    },
};

struct BridgeFixtureProvider {
    streams: tokio::sync::Mutex<HashMap<BindingId, ProviderEventStream>>,
    sinks: Mutex<HashMap<BindingId, ProviderEventSink>>,
    fail_fence: bool,
    fence_calls: AtomicUsize,
    retire_calls: AtomicUsize,
    cleanup_calls: AtomicUsize,
    reconcile_calls: AtomicUsize,
    terminal_calls: AtomicUsize,
    terminal_started: tokio::sync::Notify,
    terminal_release: tokio::sync::Notify,
}

impl BridgeFixtureProvider {
    fn new(fail_fence: bool) -> Arc<Self> {
        Arc::new(Self {
            streams: tokio::sync::Mutex::new(HashMap::new()),
            sinks: Mutex::new(HashMap::new()),
            fail_fence,
            fence_calls: AtomicUsize::new(0),
            retire_calls: AtomicUsize::new(0),
            cleanup_calls: AtomicUsize::new(0),
            reconcile_calls: AtomicUsize::new(0),
            terminal_calls: AtomicUsize::new(0),
            terminal_started: tokio::sync::Notify::new(),
            terminal_release: tokio::sync::Notify::new(),
        })
    }

    fn close_native_stream(&self, binding: &BindingId) {
        self.sinks
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .remove(binding);
    }
}

#[async_trait::async_trait]
impl NativeProvider for BridgeFixtureProvider {
    /// Record configuration writes against an initialized native session.
    async fn reconcile_settings(
        &self,
        _: forge_provider_api::catalog::ProviderCatalogRequest,
        _: forge_provider_api::SessionSettings,
        _: forge_provider_api::ResourceSelection,
    ) -> Result<(), ProviderError> {
        self.reconcile_calls.fetch_add(1, Ordering::AcqRel);
        Ok(())
    }

    async fn initialize_session(
        &self,
        request: SessionRequest,
    ) -> Result<SessionBinding, ProviderError> {
        let binding = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            provider_failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Unknown,
            )
        })?;
        let scope = forge_provider_api::EventScope::SessionOperation {
            owner: request.owner,
            operation_id: binding.as_str().to_owned(),
        };
        let (sink, stream) = events::channel(binding.clone(), scope);
        self.streams.lock().await.insert(binding.clone(), stream);
        self.sinks
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(binding.clone(), sink);
        Ok(SessionBinding {
            id: binding,
            provider_session_id: None,
        })
    }

    async fn take_session_events(
        &self,
        binding: &BindingId,
    ) -> Result<ProviderEventStream, ProviderError> {
        self.streams.lock().await.remove(binding).ok_or_else(|| {
            provider_failure(
                ProviderOperation::CollectEventReader,
                ProviderFailureKind::SessionMissing,
            )
        })
    }

    async fn session_prompt(
        &self,
        _: &BindingId,
        _: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        Err(provider_failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ))
    }

    async fn continue_session(
        &self,
        _: &BindingId,
        _: forge_provider_api::ContinuationRequest,
    ) -> Result<ContinuationOutcome, ProviderError> {
        Err(provider_failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ))
    }

    async fn session_steer(
        &self,
        _: &BindingId,
        _: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        Err(provider_failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ))
    }

    async fn retire_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        self.retire_calls.fetch_add(1, Ordering::AcqRel);
        self.close_native_stream(binding);
        Ok(())
    }

    async fn fence_session(&self, _: &BindingId) -> Result<(), ProviderError> {
        self.fence_calls.fetch_add(1, Ordering::AcqRel);
        if self.fail_fence {
            Err(provider_failure(
                ProviderOperation::FenceSession,
                ProviderFailureKind::Runtime,
            ))
        } else {
            Ok(())
        }
    }

    async fn recover_session(
        &self,
        request: SessionRequest,
        _: Option<RecoveryToken>,
    ) -> Result<SessionBinding, ProviderError> {
        self.initialize_session(request).await
    }

    async fn shutdown(&self) -> Result<(), ProviderError> {
        Ok(())
    }

    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        forge_provider_api::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id().expect("fixture provider identity"),
            label: "bridge fixture".into(),
            capability: Default::default(),
        }
    }

    async fn has_active_turn(&self, _: &ExecutionOwner) -> bool {
        false
    }

    async fn cancel_session(&self, _: &BindingId) -> Result<(), ProviderError> {
        Ok(())
    }

    async fn cleanup_execution(&self, _: &BindingId) -> Result<(), ProviderError> {
        self.cleanup_calls.fetch_add(1, Ordering::AcqRel);
        Ok(())
    }

    async fn background_terminals(
        &self,
        _: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
        self.terminal_calls.fetch_add(1, Ordering::AcqRel);
        self.terminal_started.notify_one();
        self.terminal_release.notified().await;
        Ok(forge_provider_api::terminal::TerminalSnapshot {
            supported: true,
            starting: false,
            terminal: Vec::new(),
        })
    }

    async fn active_session_id_for(
        &self,
        _: &ExecutionOwner,
    ) -> Result<Option<ProviderSessionId>, ProviderError> {
        Ok(None)
    }
}

fn provider_failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

fn provider_request(root: &std::path::Path) -> ProviderRequest {
    ProviderRequest {
        dispatch: forge_provider_api::submission::DispatchTracker::default(),
        execution_owner: None,
        harness_session_id: "bridge-test-session".into(),
        workspace: root.to_string_lossy().into_owned(),
        input: super::super::ProviderInput::from_text(""),
        mode: super::super::PromptMode::Chat,
        model: "default".into(),
        effort: "default".into(),
        context_window: None,
        service_tier: Default::default(),
        access: Default::default(),
        approval_mode: Default::default(),
        provider_session_id: None,
        submission: None,
        recovered_input: Vec::new(),
        control_context: None,
    }
}

async fn bridge(
    root: &std::path::Path,
    native: Arc<BridgeFixtureProvider>,
) -> Result<ProviderBridge, anyhow::Error> {
    Ok(ProviderBridge::new(
        native,
        PermissionCoordinator::transient(root)?,
        Arc::new(crate::trace::TraceStore::open(root)?),
        root.to_owned(),
    ))
}

/// Caller cancellation does not release observation ownership before the actual native response.
#[tokio::test]
async fn terminal_observation_holds_retirement_until_response_after_waiter_is_dropped()
-> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.initialize_session(request.clone()).await?;
    let querying = bridge.clone();
    let query = tokio::spawn(async move {
        querying
            .background_terminals(catalog_request(&request))
            .await
    });
    native.terminal_started.notified().await;
    query.abort();
    let _ = query.await;
    let retiring = bridge.retire_owner(&owner);
    tokio::pin!(retiring);
    assert!(futures_util::poll!(&mut retiring).is_pending());
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 0);
    native.terminal_release.notify_one();
    retiring.await?;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    assert!(
        bridge
            .background_terminals(catalog_request(&provider_request(root.path())))
            .await
            .is_err()
    );
    assert_eq!(native.terminal_calls.load(Ordering::Acquire), 1);
    Ok(())
}

/// Steering keeps its target invocation's immutable preferences and rejects a mismatched exchange.
#[tokio::test]
async fn steering_seals_original_invocation_configuration_and_validates_full_scope()
-> anyhow::Result<()> {
    use crate::provider::submission::{SubmissionOrigin, SubmissionState, SubmissionStore};
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native).await?;
    let store = SubmissionStore::open(
        root.path(),
        Arc::new(crate::trace::TraceStore::open(root.path())?),
    )?;
    let provider = forge_providers::registry::nonselectable_id()
        .expect("registry supplies a fixture identity");
    let mut first = provider_request(root.path());
    first.model = "original-invocation-model".into();
    first.effort = "high".into();
    first.submission = Some(store.prepare(&first, provider.clone())?);
    let owner = first.owner()?;
    let scope = forge_provider_api::InvocationScope::new(owner.clone(), "original-exchange");
    let _original = bridge
        .prepare_dispatch(first.clone(), scope.clone())
        .await?;

    let mut later = provider_request(root.path());
    later.model = "later-invocation-model".into();
    later.submission = Some(store.prepare(&later, provider.clone())?);
    let _later = bridge
        .prepare_dispatch(
            later.clone(),
            forge_provider_api::InvocationScope::new(owner.clone(), "later-exchange"),
        )
        .await?;
    later.input = super::super::ProviderInput::from_text("steering text");
    let receipt = store.prepare_input_request(
        &later,
        provider,
        SubmissionOrigin::QueuedInput(crate::exchange::command::InputId::new("queued-steering")?),
    )?;
    let input = super::super::SteeringInput::User {
        text: "steering text".into(),
        attachments: Vec::new(),
        submission: Some(receipt.clone()),
    };
    let (_, prepared) = bridge
        .prepare_steering(&owner, scope.clone(), input.clone())
        .await?;
    assert_eq!(prepared.scope, scope);
    assert_eq!(receipt.record()?.request.model, "original-invocation-model");
    assert_eq!(receipt.record()?.request.effort, "high");
    assert_eq!(receipt.record()?.request.input.text(), "steering text");
    assert_eq!(receipt.record()?.delivery, SubmissionState::Prepared);

    let mut mismatched = scope;
    mismatched.exchange_id = "wrong-exchange".into();
    assert!(
        bridge
            .prepare_steering(&owner, mismatched, input)
            .await
            .is_err()
    );
    assert_eq!(receipt.record()?.delivery, SubmissionState::Prepared);
    bridge.shutdown().await?;
    Ok(())
}

/// Defer cold configuration until initialization without poisoning later native writes.
#[tokio::test]
async fn cold_configuration_defers_native_write_until_initialization() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let selection = crate::settings::PortableSettings::default();

    bridge
        .reconcile_settings(catalog_request(&request), &selection)
        .await?;
    let _events = bridge.session_events(request.owner()?).await?;
    bridge
        .reconcile_settings(catalog_request(&request), &selection)
        .await?;
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 0);

    bridge.initialize_session(request.clone()).await?;
    bridge
        .reconcile_settings(catalog_request(&request), &selection)
        .await?;
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 1);

    bridge.shutdown().await?;
    assert!(matches!(
        bridge
            .reconcile_settings(catalog_request(&request), &selection)
            .await,
        Err(ProviderError::Failure(failure))
            if failure.operation() == ProviderOperation::ReconcileSettings
                && failure.kind() == ProviderFailureKind::Rejected
    ));
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 1);
    Ok(())
}

#[tokio::test]
async fn unexpected_native_eof_attempts_retirement_after_fence_failure() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(true);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner).await?;
    bridge.initialize_session(request).await?;

    let session = bridge
        .sessions
        .lock()
        .await
        .values()
        .next()
        .cloned()
        .unwrap();
    let binding = session.initialized.get().unwrap().binding.id.clone();
    native.close_native_stream(&binding);

    let error = tokio::time::timeout(std::time::Duration::from_secs(2), events.recv())
        .await?
        .expect_err("unexpected native EOF was not reported to the host stream");
    assert!(matches!(error, EventDeliveryError::Closed));
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        while native.retire_calls.load(Ordering::Acquire) == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert_eq!(native.fence_calls.load(Ordering::Acquire), 1);
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    let _ = bridge.shutdown().await;
    Ok(())
}

#[tokio::test]
async fn foreground_cleanup_does_not_cancel_a_settled_invocation_observer() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.initialize_session(request).await?;
    let host = bridge.session(owner.clone()).await;
    let bound = host.initialized.get().unwrap();
    let scope = forge_provider_api::InvocationScope::new(owner.clone(), "settled-test-turn");
    let (sink, _control_stream) = events::channel(bound.binding.id.clone(), scope.clone());
    let endpoint = Arc::new(crate::tools::control_session::ControlSession::new(
        Vec::new(),
    ));
    let context = crate::tools::ControlTurnContext::inactive(super::super::PromptMode::Chat);
    let permissions = Arc::new(
        crate::permissions::provider::ProviderPermissionContext::new(bridge.permissions.clone()),
    );
    permissions.configure(
        owner.clone(),
        crate::session::ApprovalMode::Ask,
        root.path().to_string_lossy().into_owned(),
        Some(sink.clone()),
    )?;
    endpoint.begin(context.clone(), sink).await;
    let (_event_sink, event_stream) = events::channel(bound.binding.id.clone(), scope.clone());
    let server = ControlServer::start(endpoint.clone()).await?;
    let endpoint_lease = Arc::new(EndpointLease { _server: server });
    let relay = crate::provider::host_invocation::HostEventRelay::start(
        bound.binding.id.clone(),
        event_stream,
        host.sender.clone(),
    );
    let invocation = Arc::new(crate::provider::host_invocation::HostInvocation::new(
        bound.binding.id.clone(),
        scope.clone(),
        &provider_request(root.path()),
        context,
        &[],
        crate::provider::host_invocation::HostInvocationResources {
            permissions: permissions.clone(),
            directories: None,
            endpoint,
            endpoint_lease,
            relay,
            skills: None,
        },
    ));
    host.invocations
        .lock()
        .await
        .insert(scope.invocation_id.clone(), invocation.clone());
    forge_provider_api::session::ProviderHostEffects::settle_foreground(
        invocation.as_ref(),
        &bound.binding.id,
        &scope,
    )
    .await?;
    assert!(!invocation.foreground_pending());
    assert!(!invocation.status(&scope).await?.cancelled);

    bridge.cleanup_execution(&owner.runtime_key()).await?;

    assert_eq!(native.cleanup_calls.load(Ordering::Acquire), 1);
    assert!(!invocation.status(&scope).await?.cancelled);
    assert!(
        host.invocations
            .lock()
            .await
            .contains_key(&scope.invocation_id)
    );
    assert!(
        forge_provider_api::permissions::ProviderPermissions::before_tool(
            permissions.as_ref(),
            &scope
        )
        .await
        .is_ok()
    );
    bridge.cancel_session(&owner.runtime_key()).await?;
    assert!(matches!(
        forge_provider_api::permissions::ProviderPermissions::before_tool(
            permissions.as_ref(),
            &scope
        )
        .await,
        Err(forge_provider_api::ProviderError::Cancelled)
    ));
    let _ = bridge.shutdown().await;
    Ok(())
}

#[tokio::test]
async fn mcp_mutation_reply_waits_for_live_commit_and_replays_same_call() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.initialize_session(request).await?;
    let host = bridge.session(owner.clone()).await;
    let bound = host.initialized.get().unwrap();
    let scope = forge_provider_api::InvocationScope::new(owner.clone(), "settled-test-turn");
    let (sink, _control_stream) = events::channel(bound.binding.id.clone(), scope.clone());
    let endpoint = Arc::new(crate::tools::control_session::ControlSession::new(
        crate::tools::ControlToolRegistry.definition_list(),
    ));
    let mut context = crate::tools::ControlTurnContext::inactive(super::super::PromptMode::Chat);
    let (_task_root, _task_store, task_controller) =
        crate::task::SessionTaskController::test_goal();
    context.bind_test_task_controller(task_controller);
    let (live, mut live_receiver) = crate::provider::LiveHostEffectSender::channel();
    context.live_host_effects = Some(live);
    let permissions = Arc::new(
        crate::permissions::provider::ProviderPermissionContext::new(bridge.permissions.clone()),
    );
    permissions.configure(
        owner.clone(),
        crate::session::ApprovalMode::Ask,
        root.path().to_string_lossy().into_owned(),
        Some(sink.clone()),
    )?;
    endpoint.begin(context.clone(), sink).await;
    let (_event_sink, event_stream) = events::channel(bound.binding.id.clone(), scope.clone());
    let server = ControlServer::start(endpoint.clone()).await?;
    let endpoint_lease = Arc::new(EndpointLease { _server: server });
    let relay = crate::provider::host_invocation::HostEventRelay::start(
        bound.binding.id.clone(),
        event_stream,
        host.sender.clone(),
    );
    let invocation = Arc::new(crate::provider::host_invocation::HostInvocation::new(
        bound.binding.id.clone(),
        scope.clone(),
        &provider_request(root.path()),
        context,
        &crate::tools::ControlToolRegistry.definition_list(),
        crate::provider::host_invocation::HostInvocationResources {
            permissions: permissions.clone(),
            directories: None,
            endpoint: endpoint.clone(),
            endpoint_lease,
            relay,
            skills: None,
        },
    ));
    let delegated: Arc<dyn forge_provider_api::control::ProviderTools> = invocation.clone();
    endpoint.bind_host(scope.clone(), Arc::downgrade(&delegated));
    let call = forge_provider_api::control::ToolCall {
        id: forge_provider_api::control::ToolCallId::new("mcp-live-goal")?,
        name: "goal_complete".into(),
        arguments: serde_json::json!({"summary":"completed goal"}),
    };
    let mut pending = Box::pin(crate::tools::control_session::ControlEndpoint::call(
        endpoint.as_ref(),
        call.clone(),
    ));
    assert!(futures_util::poll!(pending.as_mut()).is_pending());
    let effect = tokio::select! {
        effect = live_receiver.recv() => effect.expect("mutation requests owner commit"),
        result = pending.as_mut() => panic!("mutation replied before owner commit: {result:?}"),
    };
    assert_eq!(effect.call_id, call.id);
    assert!(effect.batch.completion.goal_control.is_some());
    assert!(futures_util::poll!(pending.as_mut()).is_pending());
    let mut concurrent = Box::pin(crate::tools::control_session::ControlEndpoint::call(
        endpoint.as_ref(),
        call.clone(),
    ));
    assert!(futures_util::poll!(concurrent.as_mut()).is_pending());
    assert!(
        live_receiver.try_recv().is_err(),
        "same-call retry must share the original commit"
    );
    effect
        .reply
        .send(Ok(crate::provider::LiveHostCommit { task_permit: None }))
        .map_err(|_| anyhow::anyhow!("callback lost its commit waiter"))?;
    let reply = pending.await?;
    assert_eq!(concurrent.await?.message, reply.message);
    invocation.cancel();
    let replay =
        crate::tools::control_session::ControlEndpoint::call(endpoint.as_ref(), call).await?;
    assert_eq!(reply.message, replay.message);
    assert!(live_receiver.try_recv().is_err());
    invocation.cancel();
    let _ = bridge.shutdown().await;
    Ok(())
}
