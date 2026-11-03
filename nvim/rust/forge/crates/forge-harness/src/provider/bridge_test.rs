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
