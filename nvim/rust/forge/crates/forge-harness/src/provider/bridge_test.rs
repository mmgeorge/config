use super::*;

use forge_provider_api::{
    BindingId, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    events::{self, ProviderEventSink},
    provider::Provider as NativeProvider,
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
    sinks: Mutex<HashMap<BindingId, ProviderEventSink>>,
    fail_fence: bool,
    close_failed: std::sync::atomic::AtomicBool,
    retire_calls: AtomicUsize,
    reconcile_calls: AtomicUsize,
    cancel_calls: AtomicUsize,
    terminal_calls: AtomicUsize,
    terminal_started: tokio::sync::Notify,
    terminal_release: tokio::sync::Notify,
    open_blocked: std::sync::atomic::AtomicBool,
    open_started: tokio::sync::Notify,
    input_failure: Mutex<Option<ProviderError>>,
    open_identity: Mutex<Option<forge_provider_api::ProviderSessionId>>,
    configured_identity: Mutex<Option<forge_provider_api::ProviderSessionId>>,
}

impl BridgeFixtureProvider {
    fn new(fail_fence: bool) -> Arc<Self> {
        Arc::new(Self {
            sinks: Mutex::new(HashMap::new()),
            fail_fence,
            close_failed: std::sync::atomic::AtomicBool::new(false),
            retire_calls: AtomicUsize::new(0),
            reconcile_calls: AtomicUsize::new(0),
            cancel_calls: AtomicUsize::new(0),
            terminal_calls: AtomicUsize::new(0),
            terminal_started: tokio::sync::Notify::new(),
            terminal_release: tokio::sync::Notify::new(),
            open_blocked: std::sync::atomic::AtomicBool::new(false),
            open_started: tokio::sync::Notify::new(),
            input_failure: Mutex::new(None),
            open_identity: Mutex::new(None),
            configured_identity: Mutex::new(None),
        })
    }

    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        forge_provider_api::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id().expect("fixture provider identity"),
            label: "bridge fixture".into(),
            capability: Default::default(),
        }
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
    async fn session_open(
        &self,
        operation: forge_provider_api::OperationContext,
        request: forge_provider_api::SessionRequest,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        operation.check_admission(ProviderOperation::SessionOpen)?;
        if self.open_blocked.load(Ordering::Acquire) {
            self.open_started.notify_one();
            return Err(operation.interrupted().await.failure(
                ProviderOperation::SessionOpen,
                forge_provider_api::ProviderPhase::Preparation,
                forge_provider_api::FailureDisposition::Unchanged,
            ));
        }
        let binding = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            provider_failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Protocol,
            )
        })?;
        let (sink, events) = events::channel(
            binding.clone(),
            forge_provider_api::EventScope::Binding {
                owner: request.owner,
            },
        );
        self.sinks
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(binding.clone(), sink);
        Ok(forge_provider_api::OpenedSession {
            binding: forge_provider_api::SessionBinding {
                id: binding,
                provider_session_id: self.open_identity.lock().unwrap().clone(),
            },
            capabilities: forge_provider_api::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
            events,
        })
    }

    async fn session_configure(
        &self,
        operation: forge_provider_api::OperationContext,
        binding: &BindingId,
        configuration: forge_provider_api::SessionConfiguration,
    ) -> Result<forge_provider_api::ConfiguredSession, ProviderError> {
        operation.check_admission(ProviderOperation::SessionConfigure)?;
        self.reconcile_calls.fetch_add(1, Ordering::AcqRel);
        let identity = self.configured_identity.lock().unwrap().clone();
        let sink = self
            .sinks
            .lock()
            .unwrap()
            .get(binding)
            .cloned()
            .ok_or_else(|| {
                provider_failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        if let Some(provider_session_id) = identity {
            sink.send_wait(ProviderEvent::SessionIdentity {
                scope: sink.scope().clone(),
                provider_session_id,
            })
            .await?;
        }
        Ok(forge_provider_api::ConfiguredSession {
            configuration,
            capabilities: forge_provider_api::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
            watermark: sink.watermark(),
        })
    }

    async fn session_close(
        &self,
        operation: forge_provider_api::OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        operation.check_admission(ProviderOperation::SessionClose)?;
        self.retire_calls.fetch_add(1, Ordering::AcqRel);
        if self.fail_fence && !self.close_failed.swap(true, Ordering::AcqRel) {
            return Err(provider_failure(
                ProviderOperation::SessionClose,
                ProviderFailureKind::Runtime,
            ));
        }
        self.close_native_stream(binding);
        Ok(())
    }

    async fn input_submit(
        &self,
        operation: forge_provider_api::OperationContext,
        _: &BindingId,
        _: forge_provider_api::PromptRequest,
    ) -> Result<forge_provider_api::InputAcceptance, ProviderError> {
        operation.check_admission(ProviderOperation::InputSubmit)?;
        if let Some(error) = self.input_failure.lock().unwrap().clone() {
            return Err(error);
        }
        Err(provider_failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Rejected,
        ))
    }

    async fn cancel(
        &self,
        operation: forge_provider_api::OperationContext,
        _: &BindingId,
        _: forge_provider_api::CancelRequest,
    ) -> Result<forge_provider_api::InvocationSettlement, ProviderError> {
        operation.check_admission(ProviderOperation::Cancel)?;
        self.cancel_calls.fetch_add(1, Ordering::AcqRel);
        Err(provider_failure(
            ProviderOperation::Cancel,
            ProviderFailureKind::WrongInvocation,
        ))
    }

    async fn inspect(
        &self,
        operation: forge_provider_api::OperationContext,
        _: forge_provider_api::WorkspaceContext,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        operation.check_admission(ProviderOperation::Inspect)?;
        Err(provider_failure(
            ProviderOperation::Inspect,
            ProviderFailureKind::Runtime,
        ))
    }

    async fn shutdown(
        &self,
        operation: forge_provider_api::OperationContext,
    ) -> Result<(), ProviderError> {
        operation.check_admission(ProviderOperation::Shutdown)
    }

    async fn terminal_list(
        &self,
        operation: forge_provider_api::OperationContext,
        _: &BindingId,
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
        operation.check_admission(ProviderOperation::TerminalList)?;
        self.terminal_calls.fetch_add(1, Ordering::AcqRel);
        self.terminal_started.notify_one();
        self.terminal_release.notified().await;
        Ok(forge_provider_api::terminal::TerminalSnapshot {
            supported: true,
            starting: false,
            terminal: Vec::new(),
        })
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
        forge_providers::registry::RegisteredProvider::new(native.descriptor(), native, None),
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
    bridge.session_open(request.clone()).await?;
    let querying = bridge.clone();
    let query =
        tokio::spawn(async move { querying.terminal_list(catalog_request(&request)).await });
    native.terminal_started.notified().await;
    query.abort();
    let _ = query.await;
    let retiring = bridge.retire_owner(&owner);
    tokio::pin!(retiring);
    assert!(futures_util::poll!(&mut retiring).is_pending());
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    native.terminal_release.notify_one();
    retiring.await?;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    assert!(
        bridge
            .terminal_list(catalog_request(&provider_request(root.path())))
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
/// Collect positively prevented host preparation without inventing a native cancellation target.
#[tokio::test]
async fn prevented_host_preparation_skips_native_cancel_and_keeps_binding_reusable()
-> anyhow::Result<()> {
    use crate::provider::host_invocation::NativeInputAdmission;
    use crate::provider::submission::{SubmissionRetirement, SubmissionStore};
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let store = SubmissionStore::open(
        root.path(),
        Arc::new(crate::trace::TraceStore::open(root.path())?),
    )?;
    let provider = forge_providers::registry::nonselectable_id().unwrap();
    let mut expected_cancels = 0;
    for admission in [
        NativeInputAdmission::Preparing,
        NativeInputAdmission::Requested,
    ] {
        let mut request = provider_request(root.path());
        let receipt = store.prepare(&request, provider.clone())?;
        request.submission = Some(receipt.clone());
        let scope = forge_provider_api::InvocationScope::new(request.owner()?, "prepared-exchange");
        let prepared = bridge.prepare_dispatch(request, scope.clone()).await?;
        let binding = prepared.binding.clone();
        let effects = prepared.effects.clone();
        if matches!(admission, NativeInputAdmission::Requested) {
            effects.request_native_input()?;
            expected_cancels += 1;
        }
        let retirement = receipt.retire_prepared_attempt(&receipt.record()?.attempt_id)?;
        assert_eq!(retirement, SubmissionRetirement::Prevented);
        match admission {
            NativeInputAdmission::Preparing => {
                assert!(
                    matches!(
                        prepared.admit(receipt.clone()),
                        Err(ProviderError::PreparationFailed(_))
                    ),
                    "journal retirement before native admission must remain positive non-consumption"
                );
            }
            NativeInputAdmission::Requested => drop(prepared),
        }
        bridge
            .cancel_prepared_invocation(&scope, retirement)
            .await?;
        assert_eq!(
            native.cancel_calls.load(Ordering::Acquire),
            expected_cancels
        );
        assert!(
            !effects.foreground_pending(),
            "host-only preparation resources were not collected"
        );
        assert!(
            effects.request_native_input().is_err(),
            "collected preparation regained native admission"
        );
        assert!(
            native.sinks.lock().unwrap().contains_key(&binding),
            "preparation cancellation retired its reusable binding"
        );
    }
    bridge.shutdown().await?;
    Ok(())
}

/// Collect positively rejected native admission while retaining every uncertain or invalidated cause.
#[tokio::test]
async fn native_input_rejection_preserves_delivery_proof_and_exact_collection() -> anyhow::Result<()>
{
    use forge_provider_api::{FailureDisposition as Disposition, ProviderPhase as Phase};
    for (phase, disposition, unconsumed) in [
        (Phase::Admission, Disposition::Unchanged, true),
        (Phase::Preparation, Disposition::Unchanged, true),
        (Phase::Observation, Disposition::Uncertain, false),
        (Phase::Collection, Disposition::Invalidated, false),
        (Phase::Admission, Disposition::Closed, false),
        (Phase::Observation, Disposition::Unchanged, false),
    ] {
        let root = tempfile::tempdir()?;
        let native = BridgeFixtureProvider::new(false);
        *native.input_failure.lock().unwrap() = Some(
            ProviderFailure::new(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::WrongInvocation,
            )
            .at_phase(phase, disposition)
            .into(),
        );
        let bridge = bridge(root.path(), native.clone()).await?;
        let store = super::super::submission::SubmissionStore::open(
            root.path(),
            Arc::new(crate::trace::TraceStore::open(root.path())?),
        )?;
        let mut request = provider_request(root.path());
        let owner = request.owner()?;
        request.submission = Some(store.prepare(
            &request,
            forge_providers::registry::nonselectable_id().unwrap(),
        )?);
        let scope = forge_provider_api::InvocationScope::new(owner.clone(), "native-rejection");
        let error = bridge
            .input_submit(request, scope.clone())
            .await
            .unwrap_err();
        let cause = match error {
            ProviderError::PreparationFailed(cause) if unconsumed => *cause,
            error if !unconsumed => error,
            error => panic!("positive native admission proof was lost: {error:?}"),
        };
        assert!(matches!(cause, ProviderError::Failure(failure)
            if failure.operation() == ProviderOperation::InputSubmit
                && failure.kind() == ProviderFailureKind::WrongInvocation
                && failure.phase() == phase && failure.disposition() == disposition));
        assert_eq!(
            native.cancel_calls.load(Ordering::Acquire),
            usize::from(unconsumed)
        );
        let host = bridge.session(owner).await;
        let effects = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned()
            .unwrap();
        assert_eq!(effects.foreground_pending(), !unconsumed);
        assert_eq!(
            native.sinks.lock().unwrap().len(),
            1,
            "rejection retired the stable binding"
        );
        bridge.shutdown().await?;
    }
    Ok(())
}

#[tokio::test]
async fn cold_configuration_defers_native_write_until_initialization() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let selection = crate::settings::PortableSettings::default();

    let receipt = bridge
        .session_configure(request.clone(), &selection)
        .await?;
    assert!(matches!(receipt, ConfigureReceipt::Deferred));
    let _events = bridge.session_events(request.owner()?).await?;
    let receipt = bridge
        .session_configure(request.clone(), &selection)
        .await?;
    assert!(matches!(receipt, ConfigureReceipt::Deferred));
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 0);

    bridge.session_open(request.clone()).await?;
    bridge
        .session_configure(request.clone(), &selection)
        .await?;
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 1);

    bridge.shutdown().await?;
    assert!(matches!(
        bridge
            .session_configure(request.clone(), &selection)
            .await,
        Err(ProviderError::Failure(failure))
            if failure.operation() == ProviderOperation::SessionConfigure
                && failure.kind() == ProviderFailureKind::BindingClosed
    ));
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 1);
    Ok(())
}

/// Bind configuration history to its consumed native watermark without retaining closed resources.
#[tokio::test]
async fn configuration_receipt_rejects_new_operation_identity_and_retired_generation()
-> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let root = tempfile::tempdir()?;
        let native = BridgeFixtureProvider::new(false);
        *native.open_identity.lock().unwrap() = Some("before".try_into()?);
        *native.configured_identity.lock().unwrap() = Some("after-first".try_into()?);
        let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
        let request = provider_request(root.path());
        let owner = request.owner()?;
        let mut events = bridge.session_events(owner.clone()).await?;
        bridge.session_open(request.clone()).await?;

        // A configured result cannot certify identity before the owner consumes its exact publication.
        let applying = bridge.clone();
        let desired = request.clone();
        let configured = tokio::spawn(async move {
            applying
                .session_configure(desired, &crate::settings::PortableSettings::default())
                .await
        });
        let (binding, watermark) = identity_boundary(&mut events, "after-first").await?;
        assert!(
            !configured.is_finished(),
            "configuration bypassed its identity watermark"
        );
        bridge.event_consumed(&binding, watermark).await?;
        let ConfigureReceipt::Applied(first) = configured.await?? else {
            anyhow::bail!("open binding configuration was deferred");
        };
        assert_eq!(
            first
                .before_history()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("before")
        );
        assert_eq!(
            first
                .after_history()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-first")
        );
        first.validate(&owner)?;
        assert!(
            first
                .validate(&ExecutionOwner::root("foreign-owner"))
                .is_err()
        );
        assert!(
            first
                .observed_history(&ExecutionOwner::root("foreign-owner"))
                .is_err()
        );

        // Merely beginning a newer operation revokes the old proof, even before its identity is consumed.
        *native.configured_identity.lock().unwrap() = Some("after-second".try_into()?);
        let applying = bridge.clone();
        let desired = request.clone();
        let configured = tokio::spawn(async move {
            applying
                .session_configure(desired, &crate::settings::PortableSettings::default())
                .await
        });
        let (binding, watermark) = identity_boundary(&mut events, "after-second").await?;
        assert!(!configured.is_finished());
        assert!(
            first.validate(&owner).is_err(),
            "new configuration retained old commit authority"
        );
        assert_eq!(
            first
                .observed_history(&owner)?
                .as_ref()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-first")
        );
        bridge.event_consumed(&binding, watermark).await?;
        let ConfigureReceipt::Applied(second) = configured.await?? else {
            anyhow::bail!("second open binding configuration was deferred");
        };
        assert_eq!(
            second
                .before_history()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-first")
        );
        assert_eq!(
            second
                .after_history()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-second")
        );
        second.validate(&owner)?;
        assert_eq!(
            first
                .observed_history(&owner)?
                .as_ref()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-second")
        );

        // A later consumed history change and binding retirement independently invalidate the receipt.
        {
            let sink = native.sinks.lock().unwrap().get(&binding).cloned().unwrap();
            sink.send_wait(ProviderEvent::SessionIdentity {
                scope: sink.scope().clone(),
                provider_session_id: "later-history".try_into()?,
            })
            .await?;
        }
        let (binding, watermark) = identity_boundary(&mut events, "later-history").await?;
        bridge.event_consumed(&binding, watermark).await?;
        assert!(
            second.validate(&owner).is_err(),
            "later history reused a configuration receipt"
        );
        assert_eq!(
            second
                .observed_history(&owner)?
                .as_ref()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("later-history")
        );
        let retained_host = second.host.clone();
        bridge.session_close_owner(&owner).await?;
        assert!(second.validate(&owner).is_err());
        assert!(second.observed_history(&owner).is_err());
        assert!(
            retained_host.upgrade().is_none(),
            "configuration proof retained host resources"
        );
        bridge.shutdown().await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

/// Capture a native identity boundary while leaving its consumption acknowledgment to the test owner.
async fn identity_boundary(
    events: &mut HostEventStream,
    expected: &str,
) -> anyhow::Result<(BindingId, forge_provider_api::EventSequence)> {
    loop {
        let event = events
            .recv()
            .await?
            .ok_or_else(|| anyhow::anyhow!("identity observation closed"))?;
        if let ProviderEvent::SessionIdentity {
            provider_session_id,
            ..
        } = event.event
        {
            assert_eq!(provider_session_id.as_str(), expected);
            return event
                .native
                .ok_or_else(|| anyhow::anyhow!("native identity lost its binding watermark"));
        }
        assert!(
            event.native.is_none(),
            "unconsumed native event preceded identity boundary"
        );
    }
}

/// Publish a new binding identity only after metadata used by the broker's stale-event filter advances.
#[tokio::test]
async fn native_identity_metadata_precedes_queued_owner_publication() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let root = tempfile::tempdir()?;
        let native = BridgeFixtureProvider::new(false);
        *native.open_identity.lock().unwrap() = Some("opened".try_into()?);
        *native.configured_identity.lock().unwrap() = Some("configured".try_into()?);
        let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
        let request = provider_request(root.path());
        let owner = request.owner()?;
        let inbox =
            Arc::new(crate::provider::session_execution::SessionObservationInbox::default());
        let registry = crate::provider::session_execution::SessionExecutionRegistry::with_observer(
            bridge.clone(),
            owner.clone(),
            inbox.clone(),
            Arc::new(crate::trace::TraceStore::open(root.path())?),
        );
        let engine = registry.owner(owner.clone()).await?;
        bridge.session_open(request.clone()).await?;
        let ConfigureReceipt::Applied(receipt) = bridge
            .session_configure(request, &crate::settings::PortableSettings::default())
            .await?
        else {
            anyhow::bail!("initialized configuration was deferred");
        };

        // Drain a FIFO marker before gating consumption, excluding bootstrap publications from the assertion.
        let host = bridge.session(owner.clone()).await;
        host.sender
            .send(Ok(HostEvent::host(ProviderEvent::Diagnostic {
                scope: forge_provider_api::EventScope::Binding {
                    owner: owner.clone(),
                },
                diagnostic: forge_provider_api::ProviderDiagnostic {
                    kind: ProviderFailureKind::Unknown,
                    message: forge_provider_api::SanitizedMessage::new(
                        "identity-publication-marker",
                    )?,
                    fatal: false,
                },
            })))?;
        loop {
            if inbox.drain()?.iter().any(|event| {
                matches!(event, ProviderEvent::Diagnostic { diagnostic, .. }
                if diagnostic.message.as_str() == "identity-publication-marker")
            }) {
                break;
            }
            inbox.notified().await?;
        }
        assert_eq!(engine.presentation_queued_snapshot(), 0);
        let sessions = bridge.sessions.lock().await;
        let consuming = bridge.consumption_entered.notified();
        tokio::pin!(consuming);
        consuming.as_mut().enable();
        {
            let binding = &host.initialized.get().unwrap().binding.id;
            let sink = native.sinks.lock().unwrap().get(binding).cloned().unwrap();
            sink.send_wait(ProviderEvent::SessionIdentity {
                scope: sink.scope().clone(),
                provider_session_id: "advanced".try_into()?,
            })
            .await?;
        }
        consuming.await;

        // The current-thread worker either retains an enqueued frame or pushes its inbox without another await.
        assert_eq!(
            engine.presentation_queued_snapshot(),
            0,
            "identity was queued before metadata consumption"
        );
        assert!(
            inbox.drain()?.is_empty(),
            "identity was published before metadata consumption"
        );
        assert_eq!(
            receipt
                .observed_history(&owner)?
                .as_ref()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("configured")
        );
        drop(sessions);
        loop {
            let events = inbox.drain()?;
            if events.iter().any(|event| {
                matches!(event, ProviderEvent::SessionIdentity { provider_session_id, .. }
                if provider_session_id.as_str() == "advanced")
            }) {
                assert_eq!(
                    receipt
                        .observed_history(&owner)?
                        .as_ref()
                        .map(forge_provider_api::ProviderSessionId::as_str),
                    Some("advanced")
                );
                break;
            }
            inbox.notified().await?;
        }
        assert!(
            receipt.validate(&owner).is_err(),
            "later identity retained configuration commit authority"
        );
        registry.close().await?;
        drop(host);
        bridge.shutdown().await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

#[tokio::test]
async fn unexpected_native_eof_retains_failed_close_for_retry() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(true);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner).await?;
    bridge.session_open(request).await?;
    assert!(matches!(
        events.recv().await?.map(|event| event.event),
        Some(ProviderEvent::CapabilitiesChanged { .. })
    ));

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
    bridge.session_open(request).await?;
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
            provider_closed: bridge.closed.clone(),
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

    let mut wrong_scope = scope.clone();
    wrong_scope.exchange_id = "foreign-exchange".into();
    assert!(bridge.cancel_invocation(wrong_scope).await.is_err());
    assert!(!invocation.status(&scope).await?.cancelled);
    bridge.collect_host_effects(&scope).await?;

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
    bridge.session_close_owner(&owner).await?;
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
    bridge.session_open(request).await?;
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
            provider_closed: bridge.closed.clone(),
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

/// A returned lifecycle result cannot advance before its event is reduced by the owner.
#[tokio::test]
async fn operation_watermark_waits_for_owner_consumption() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner.clone()).await?;
    bridge.session_open(request.clone()).await?;
    let host = bridge.session(owner.clone()).await;
    let bound = host.initialized.get().expect("the session is opened");
    let sink = native
        .sinks
        .lock()
        .unwrap()
        .get(&bound.binding.id)
        .cloned()
        .unwrap();
    let mut capabilities =
        forge_provider_api::CapabilityManifest::from_descriptor(native.descriptor());
    capabilities.effective_revision = 7;
    sink.send_wait(ProviderEvent::CapabilitiesChanged {
        scope: sink.scope().clone(),
        capabilities,
    })
    .await?;
    let observation = loop {
        let observation = events
            .recv()
            .await?
            .expect("capability update is published");
        if observation.native.is_some() {
            break observation;
        }
    };
    let (binding, sequence) = observation
        .native
        .expect("native events carry their sequence");
    assert_ne!(
        bridge
            .capabilities(catalog_request(&request))
            .await?
            .effective_revision,
        7
    );
    let barrier = bridge.wait_watermark(&owner, sequence);
    tokio::pin!(barrier);
    assert!(futures_util::poll!(barrier.as_mut()).is_pending());
    bridge.event_consumed(&binding, sequence).await?;
    barrier.await?;
    assert_eq!(
        bridge
            .capabilities(catalog_request(&request))
            .await?
            .effective_revision,
        7
    );
    drop(sink);
    bridge.shutdown().await?;
    Ok(())
}

/// Closed observation cannot certify an operation whose published prefix was never consumed.
#[tokio::test]
async fn operation_watermark_rejects_early_stream_closure() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner.clone()).await?;
    bridge.session_open(request).await?;
    let host = bridge.session(owner.clone()).await;
    let binding = host
        .initialized
        .get()
        .expect("the session is opened")
        .binding
        .id
        .clone();
    assert!(matches!(
        events.recv().await?.map(|event| event.event),
        Some(ProviderEvent::CapabilitiesChanged { .. })
    ));
    native.close_native_stream(&binding);
    assert!(
        tokio::time::timeout(std::time::Duration::from_secs(2), events.recv())
            .await?
            .is_err()
    );
    let error = tokio::time::timeout(
        std::time::Duration::from_secs(2),
        bridge.wait_watermark(&owner, forge_provider_api::EventSequence::new(1)),
    )
    .await?
    .unwrap_err();
    assert!(matches!(error, ProviderError::Observation(_)));
    let _ = bridge.shutdown().await;
    Ok(())
}

/// Intentional close keeps a published final prefix routable until the owner reduces it.
#[tokio::test]
async fn close_waits_for_published_prefix_without_fabricating_observation_loss()
-> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner.clone()).await?;
    bridge.session_open(request).await?;
    assert!(events.recv().await?.is_some());
    let host = bridge.session(owner.clone()).await;
    let binding = host
        .initialized
        .get()
        .expect("opened session has a binding")
        .binding
        .id
        .clone();
    let sink = native.sinks.lock().unwrap().get(&binding).cloned().unwrap();
    sink.send_wait(ProviderEvent::SessionIdentity {
        scope: sink.scope().clone(),
        provider_session_id: "final-history".to_owned().try_into()?,
    })
    .await?;
    let observed = events
        .recv()
        .await?
        .expect("final native event is published");
    let (_, sequence) = observed
        .native
        .expect("native identity event has a sequence");
    drop(sink);
    let closing = bridge.session_close_owner(&owner);
    tokio::pin!(closing);
    assert!(futures_util::poll!(closing.as_mut()).is_pending());
    assert!(bridge.sessions.lock().await.contains_key(&owner));
    bridge.event_consumed(&binding, sequence).await?;
    tokio::time::timeout(std::time::Duration::from_secs(2), closing).await??;
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    Ok(())
}

/// Shutdown interrupts a pending open before awaiting its shared admission guard.
#[tokio::test]
async fn shutdown_revokes_pending_open_before_waiting_for_admission() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    native.open_blocked.store(true, Ordering::Release);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let request = provider_request(root.path());
    let opening = bridge.clone();
    let worker = tokio::spawn(async move { opening.session_open(request).await });
    native.open_started.notified().await;
    tokio::time::timeout(std::time::Duration::from_secs(2), bridge.shutdown()).await??;
    let error = worker
        .await?
        .expect_err("shutdown must interrupt the held open");
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Cancelled)
    );
    let error = bridge
        .session_open(provider_request(root.path()))
        .await
        .expect_err("closed admission must reject reopening");
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::BindingClosed)
    );
    Ok(())
}

/// A failed native close retains its live reader without waiting for a success-only EOF barrier.
#[tokio::test]
async fn failed_native_close_retains_reader_for_prompt_exact_binding_retry() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(true);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.session_open(request.clone()).await?;
    let querying = bridge.clone();
    let query =
        tokio::spawn(async move { querying.terminal_list(catalog_request(&request)).await });
    native.terminal_started.notified().await;
    let error = tokio::time::timeout(
        std::time::Duration::from_secs(1),
        bridge.session_close_owner(&owner),
    )
    .await?
    .unwrap_err();
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionClose && failure.kind() == ProviderFailureKind::Runtime)
    );
    let host = bridge
        .sessions
        .lock()
        .await
        .get(&owner)
        .cloned()
        .expect("failed close retains host ownership");
    let bound = host
        .initialized
        .get()
        .expect("the retained native binding is initialized");
    let binding = bound.binding.id.clone();
    assert!(native.sinks.lock().unwrap().contains_key(&binding));
    assert!(
        bound
            .relay
            .lock()
            .await
            .as_ref()
            .is_some_and(|reader| !reader.is_finished())
    );
    assert_eq!(
        *host.observers.lock().unwrap(),
        1,
        "failed collection retains the pending query lease"
    );
    native.terminal_release.notify_one();
    query.await??;
    tokio::time::timeout(
        std::time::Duration::from_secs(1),
        bridge.session_close_owner(&owner),
    )
    .await??;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 2);
    assert!(!native.sinks.lock().unwrap().contains_key(&binding));
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    Ok(())
}

/// A pending host query is collection failure even after native resources are positively closed.
#[tokio::test(start_paused = true)]
async fn host_collection_timeout_preserves_closed_native_binding_proof() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.session_open(request.clone()).await?;
    let querying = bridge.clone();
    let query =
        tokio::spawn(async move { querying.terminal_list(catalog_request(&request)).await });
    native.terminal_started.notified().await;
    let closing = bridge.session_close_owner(&owner);
    tokio::pin!(closing);
    assert!(futures_util::poll!(closing.as_mut()).is_pending());
    tokio::time::advance(std::time::Duration::from_secs(16)).await;
    let error = closing.await.unwrap_err();
    assert!(matches!(error, ProviderError::Failure(failure)
        if failure.kind() == ProviderFailureKind::Timeout
        && failure.phase() == forge_provider_api::ProviderPhase::Collection
        && failure.disposition() == forge_provider_api::FailureDisposition::Invalidated));
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    native.terminal_release.notify_one();
    query.await??;
    bridge.session_close_owner(&owner).await?;
    assert_eq!(
        native.retire_calls.load(Ordering::Acquire),
        1,
        "host retry must preserve the completed native close"
    );
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    Ok(())
}
