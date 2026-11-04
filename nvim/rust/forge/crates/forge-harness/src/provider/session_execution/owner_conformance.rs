//! Exercise the production engine with independently controlled adapter acknowledgments.

use super::*;
use crate::provider::submission::SubmissionStore;
use crate::provider::{PromptMode, ProviderInput, SteeringInput};
use anyhow::Context;
use forge_provider_api::retained_operation::{IdentityRegistry, Interruption, Store};
use forge_provider_api::{
    CancelRequest, ConfiguredSession, FailureDisposition, InvocationSettlement, OpenedSession,
    OperationContext, PromptRequestIdentity, ProviderError, ProviderFailure, ProviderFailureKind,
    ProviderOperation, ProviderPhase, ProviderSetup, RuntimeAvailability, RuntimeOrigin,
    RuntimeVersions, SessionConfiguration, SessionRequestIdentity, SteeringRequestIdentity,
    WorkspaceContext,
    events::{self, ProviderEventSink},
    lifecycle::{ExecutionObservation, ExecutionResult},
    provider::Provider as NativeProvider,
    session::{
        BindingId, InputAcceptance, SessionBinding, SessionRequest, SteeringAcceptance,
        SteeringRequest,
    },
};

enum Dispatch {
    Prompt {
        scope: InvocationScope,
        input: SubmissionId,
    },
    Steer {
        scope: InvocationScope,
        input: SubmissionId,
        acknowledgment: oneshot::Sender<()>,
    },
}

/// Retain complete context and typed request identity for independently controlled operations.
struct ControlledOperations {
    open: Store<SessionRequestIdentity, OpenedSession>,
    configure: Store<(BindingId, SessionConfiguration), ConfiguredSession>,
    close: Store<BindingId, ()>,
    cancel: Store<(BindingId, CancelRequest), InvocationSettlement>,
    submit: Store<(BindingId, PromptRequestIdentity), InputAcceptance>,
    steer: Store<(BindingId, SteeringRequestIdentity), SteeringAcceptance>,
    inspect: Store<WorkspaceContext, ProviderSetup>,
    shutdown: Store<(), ()>,
}

impl Default for ControlledOperations {
    /// Create typed records sharing one provider-wide operation identity namespace.
    fn default() -> Self {
        let identities = Arc::new(IdentityRegistry::new());
        Self {
            open: Store::registered(identities.clone()),
            configure: Store::registered(identities.clone()),
            close: Store::registered(identities.clone()),
            cancel: Store::registered(identities.clone()),
            submit: Store::registered(identities.clone()),
            steer: Store::registered(identities.clone()),
            inspect: Store::registered(identities.clone()),
            shutdown: Store::registered(identities),
        }
    }
}

/// Hold only adapter acknowledgments; lifecycle observations still use the real intake.
struct ControlledProvider {
    myself: std::sync::Weak<Self>,
    operations: ControlledOperations,
    sessions: Mutex<HashMap<BindingId, Arc<ControlledSession>>>,
    owners: Mutex<HashMap<ExecutionOwner, BindingId>>,
    streams: Mutex<HashMap<ExecutionOwner, ProviderEventSink>>,
    invocations: Mutex<HashMap<InvocationScope, ProviderEventSink>>,
    dispatch: mpsc::Sender<Dispatch>,
    subscriptions: Mutex<HashMap<ExecutionOwner, usize>>,
    cleanup: Mutex<Vec<String>>,
    settlements: Mutex<HashMap<InvocationScope, InvocationSettlement>>,
    inputs: Mutex<HashMap<InvocationScope, std::collections::HashSet<SubmissionId>>>,
    covered: Mutex<HashMap<InvocationScope, std::collections::HashSet<SubmissionId>>>,
    closed: Mutex<std::collections::HashSet<BindingId>>,
    steering_failure: Mutex<Option<ProviderFailure>>,
    panic_prompt: bool,
}

struct ControlledSession {
    owner: ExecutionOwner,
    sink: ProviderEventSink,
    closed: tokio::sync::watch::Sender<bool>,
}

impl ControlledProvider {
    /// Describe the normalized steering fixture without borrowing another adapter's capabilities.
    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        forge_provider_api::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies fixture identity"),
            label: "Controlled owner fixture".into(),
            capability: forge_provider_api::ProviderCapability {
                native_steer: true,
                ..Default::default()
            },
        }
    }

    /// Bind explicit synthetic metadata to this independently controlled native fixture.
    fn registration(self: &Arc<Self>) -> forge_providers::registry::RegisteredProvider {
        forge_providers::registry::RegisteredProvider::new(self.descriptor(), self.clone(), None)
    }

    fn new(dispatch: mpsc::Sender<Dispatch>, panic_prompt: bool) -> Arc<Self> {
        Arc::new_cyclic(|myself| Self {
            myself: myself.clone(),
            operations: ControlledOperations::default(),
            sessions: Mutex::new(HashMap::new()),
            owners: Mutex::new(HashMap::new()),
            streams: Mutex::new(HashMap::new()),
            invocations: Mutex::new(HashMap::new()),
            dispatch,
            subscriptions: Mutex::new(HashMap::new()),
            cleanup: Mutex::new(Vec::new()),
            settlements: Mutex::new(HashMap::new()),
            inputs: Mutex::new(HashMap::new()),
            covered: Mutex::new(HashMap::new()),
            closed: Mutex::new(Default::default()),
            steering_failure: Mutex::new(None),
            panic_prompt,
        })
    }

    /// Publish normalized lifecycle evidence through its original owner's stream.
    async fn publish(
        &self,
        scope: &InvocationScope,
        update: ExecutionUpdate,
    ) -> anyhow::Result<()> {
        let sink = self
            .invocations
            .lock()
            .unwrap()
            .get(scope)
            .cloned()
            .context("owner was not subscribed")?;
        let terminal = match &update {
            ExecutionUpdate::Finished {
                result,
                covered_submissions,
                ..
            }
            | ExecutionUpdate::ResolvedWithoutExecution {
                result,
                covered_submissions,
                ..
            }
            | ExecutionUpdate::PublicationFailed {
                result,
                covered_submissions,
                ..
            }
            | ExecutionUpdate::ResolutionPublicationFailed {
                result,
                covered_submissions,
                ..
            } => {
                let mut covered = self.covered.lock().unwrap();
                let covered = covered.entry(scope.clone()).or_default();
                covered.extend(covered_submissions.iter().cloned());
                self.inputs
                    .lock()
                    .unwrap()
                    .get(scope)
                    .filter(|inputs| inputs.is_subset(covered))
                    .map(|_| result.clone())
            }
            ExecutionUpdate::Started { .. } | ExecutionUpdate::ObservationLost { .. } => None,
        };
        sink.send_wait(ProviderEvent::Execution {
            observation: ExecutionObservation::new(scope.clone(), update)?,
        })
        .await?;
        if let Some(result) = terminal {
            let settlement = InvocationSettlement {
                binding_id: sink.binding_id().clone(),
                scope: scope.clone(),
                result,
                watermark: sink.watermark(),
            };
            let settlement = sink.send_settlement(settlement).await?;
            self.settlements
                .lock()
                .unwrap()
                .insert(scope.clone(), settlement);
        }
        Ok(())
    }

    async fn accept(
        receipt: Arc<dyn forge_provider_api::submission::InputReceipt>,
    ) -> Result<SubmissionId, ProviderError> {
        let id = receipt.id().clone();
        let token =
            forge_provider_api::RecoveryToken::new(format!("fixture:{}", id.as_str()).into_bytes())
                .map_err(|_| {
                    failure(ProviderOperation::InputSubmit, ProviderFailureKind::Unknown)
                })?;
        receipt.clone().record_sending(Some(token.clone())).await?;
        receipt.record_accepted(Some(token)).await?;
        Ok(id)
    }

    async fn session(&self, binding: &BindingId) -> Result<Arc<ControlledSession>, ProviderError> {
        self.sessions
            .lock()
            .unwrap()
            .get(binding)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::SessionMissing,
                )
            })
    }
}

impl ControlledProvider {
    async fn open_inner(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError> {
        context.check_admission(ProviderOperation::SessionOpen)?;
        let mut owners = self.owners.lock().unwrap();
        if owners.contains_key(&request.owner) {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let id = BindingId::new(uuid::Uuid::new_v4().to_string())
            .map_err(|_| failure(ProviderOperation::SessionOpen, ProviderFailureKind::Unknown))?;
        let scope = forge_provider_api::EventScope::Binding {
            owner: request.owner.clone(),
        };
        let (sink, stream) = events::channel(id.clone(), scope);
        let binding = SessionBinding {
            id: id.clone(),
            provider_session_id: request.history.resume_id().cloned(),
        };
        let session = Arc::new(ControlledSession {
            owner: request.owner.clone(),
            sink: sink.clone(),
            closed: tokio::sync::watch::channel(false).0,
        });
        self.streams
            .lock()
            .unwrap()
            .insert(request.owner.clone(), sink);
        owners.insert(request.owner, id.clone());
        self.sessions.lock().unwrap().insert(id, session.clone());
        *self
            .subscriptions
            .lock()
            .unwrap()
            .entry(session.owner.clone())
            .or_default() += 1;
        Ok(OpenedSession {
            binding,
            capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
            events: stream,
        })
    }

    /// Acknowledge a prompt without publishing terminal evidence.
    async fn submit_inner(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        context.check_admission(ProviderOperation::InputSubmit)?;
        let session = self.session(binding).await?;
        if request.scope.owner != session.owner {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            ));
        }
        let input = Self::accept(request.receipt.clone()).await?;
        let scope = request.scope.clone();
        self.inputs
            .lock()
            .unwrap()
            .entry(scope.clone())
            .or_default()
            .insert(input.clone());
        let sink = session.sink.invocation_sink(&scope)?;
        self.invocations.lock().unwrap().insert(scope.clone(), sink);
        let mut closed = session.closed.subscribe();
        if *closed.borrow() {
            return Err(ProviderError::Cancelled);
        }
        tokio::select! {
            result = self.dispatch.send(Dispatch::Prompt { scope, input: input.clone() }) => {
                result.map_err(|_| failure(ProviderOperation::InputSubmit, ProviderFailureKind::Transport))?;
            }
            reason = context.interrupted() => return Err(reason.failure(ProviderOperation::InputSubmit, ProviderPhase::Observation, FailureDisposition::Uncertain)),
            changed = closed.changed() => { let _ = changed; return Err(ProviderError::Cancelled); }
        }
        request.dispatch.confirm();
        Ok(InputAcceptance {
            submission_id: input,
        })
    }

    /// Allow terminal evidence to precede an independently held steering acknowledgment.
    async fn steer_inner(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        context.check_admission(ProviderOperation::InputSteer)?;
        let session = self.session(binding).await?;
        if request.scope.owner != session.owner {
            return Err(failure(
                ProviderOperation::InputSteer,
                ProviderFailureKind::Rejected,
            ));
        }
        let scope = request.scope.clone();
        let Some(receipt) = request.delivery.receipt().cloned() else {
            return Ok(SteeringAcceptance::Context);
        };
        let rejection = *self.steering_failure.lock().unwrap();
        let input = if let Some(rejection) = rejection {
            if rejection.disposition() != FailureDisposition::Unchanged {
                receipt.clone().record_sending(None).await?;
            }
            receipt.id().clone()
        } else {
            let input = Self::accept(receipt.clone()).await?;
            self.inputs
                .lock()
                .unwrap()
                .entry(scope.clone())
                .or_default()
                .insert(input.clone());
            let sink = session.sink.invocation_sink(&scope)?;
            self.invocations.lock().unwrap().insert(scope.clone(), sink);
            input
        };
        let (acknowledgment, received) = oneshot::channel();
        let mut closed = session.closed.subscribe();
        if *closed.borrow() {
            return Err(ProviderError::Cancelled);
        }
        tokio::select! {
            result = self.dispatch.send(Dispatch::Steer { scope, input: input.clone(), acknowledgment }) => {
                result.map_err(|_| failure(ProviderOperation::InputSteer, ProviderFailureKind::Transport))?;
            }
            reason = context.interrupted() => return Err(reason.failure(ProviderOperation::InputSteer, ProviderPhase::Observation, FailureDisposition::Uncertain)),
            changed = closed.changed() => { let _ = changed; return Err(ProviderError::Cancelled); }
        }
        if *closed.borrow() {
            return Err(ProviderError::Cancelled);
        }
        tokio::select! {
            received = received => received.map_err(|_| failure(ProviderOperation::InputSteer, ProviderFailureKind::Transport))?,
            reason = context.interrupted() => return Err(reason.failure(ProviderOperation::InputSteer, ProviderPhase::Observation, FailureDisposition::Uncertain)),
            changed = closed.changed() => { let _ = changed; return Err(ProviderError::Cancelled); }
        }
        if let Some(rejection) = rejection {
            let error = ProviderError::Failure(rejection);
            if rejection.disposition() == FailureDisposition::Unchanged {
                receipt.record_rejected(error.clone()).await?;
            } else {
                receipt.record_failure(error.clone()).await?;
            }
            return Err(error);
        }
        Ok(SteeringAcceptance::Journaled {
            submission_id: input,
        })
    }

    /// Finish the fixture only after the registry has explicitly closed its engines.
    async fn shutdown_inner(
        &self,
        operation: &forge_provider_api::OperationId,
    ) -> Result<(), ProviderError> {
        self.operations.open.close_admission();
        self.operations.configure.close_admission();
        self.operations.submit.close_admission();
        self.operations.steer.close_admission();
        self.operations.inspect.close_admission();
        let sessions: Vec<_> = self.sessions.lock().unwrap().values().cloned().collect();
        for session in sessions {
            session.closed.send_replace(true);
        }
        self.invocations.lock().unwrap().clear();
        self.streams.lock().unwrap().clear();
        self.sessions.lock().unwrap().clear();
        self.owners.lock().unwrap().clear();
        self.operations.open.join_all().await?;
        self.operations.configure.join_all().await?;
        self.operations.close.join_all().await?;
        self.operations.cancel.join_all().await?;
        self.operations.submit.join_all().await?;
        self.operations.steer.join_all().await?;
        self.operations.inspect.join_all().await?;
        self.operations.shutdown.join_all_except(operation).await
    }

    async fn close_inner(&self, binding: &BindingId) -> Result<(), ProviderError> {
        if self.closed.lock().unwrap().contains(binding) {
            return Ok(());
        }
        let session = self.session(binding).await?;
        session.closed.send_replace(true);
        self.cleanup
            .lock()
            .unwrap()
            .push(format!("close:{}", session.owner.runtime_key()));
        self.sessions.lock().unwrap().remove(binding);
        self.owners.lock().unwrap().remove(&session.owner);
        self.streams.lock().unwrap().remove(&session.owner);
        self.invocations
            .lock()
            .unwrap()
            .retain(|_, sink| sink.binding_id() != binding);
        self.closed.lock().unwrap().insert(binding.clone());
        Ok(())
    }

    /// Apply complete defaults only when the synthetic native work has settled.
    async fn configure_inner(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: SessionConfiguration,
    ) -> Result<ConfiguredSession, ProviderError> {
        context.check_admission(ProviderOperation::SessionConfigure)?;
        let session = self.session(binding).await?;
        let active = {
            let invocations = self.invocations.lock().unwrap();
            let settlements = self.settlements.lock().unwrap();
            invocations.iter().any(|(scope, sink)| {
                sink.binding_id() == binding && !settlements.contains_key(scope)
            })
        };
        if active {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        Ok(ConfiguredSession {
            configuration,
            capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
            watermark: session.sink.watermark(),
        })
    }

    /// Preserve exact settlement identity without selecting an active successor.
    async fn cancel_inner(
        &self,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<InvocationSettlement, ProviderError> {
        self.session(binding).await?;
        self.settlements
            .lock()
            .unwrap()
            .get(&request.scope)
            .filter(|settlement| &settlement.binding_id == binding)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::Cancel,
                    ProviderFailureKind::WrongInvocation,
                )
            })
    }

    /// Inspect the generic fixture without any native history or model work.
    async fn inspect_inner(
        &self,
        context: OperationContext,
        _workspace: WorkspaceContext,
    ) -> Result<ProviderSetup, ProviderError> {
        context.check_admission(ProviderOperation::Inspect)?;
        Ok(ProviderSetup {
            runtime: RuntimeAvailability::Available,
            origin: RuntimeOrigin::Configured,
            versions: RuntimeVersions::default(),
            authentication: Vec::new(),
            capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
        })
    }
}

#[async_trait::async_trait]
impl NativeProvider for ControlledProvider {
    /// Open one owned stream and retain its complete operation identity before mutation.
    async fn session_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("controlled fixture has a live Arc owner");
        let cleanup = owner.clone();
        let requested_owner = request.owner.clone();
        let work_context = context.clone();
        self.operations
            .open
            .take(
                context,
                ProviderOperation::SessionOpen,
                request.identity(),
                async move { owner.open_inner(work_context, request).await },
                move |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::SessionOpen,
                            ProviderPhase::Preparation,
                            FailureDisposition::Uncertain,
                        ),
                        async move {
                            let binding = cleanup
                                .owners
                                .lock()
                                .unwrap()
                                .get(&requested_owner)
                                .cloned();
                            match binding {
                                Some(binding) => cleanup.close_inner(&binding).await,
                                None => Ok(()),
                            }
                        },
                    )
                },
            )
            .await
    }

    /// Apply exact defaults once while preserving replay of the retained result after expiry.
    async fn session_configure(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: SessionConfiguration,
    ) -> Result<ConfiguredSession, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("controlled fixture has a live Arc owner");
        let payload = (binding.clone(), configuration.clone());
        let binding = binding.clone();
        let work_context = context.clone();
        self.operations
            .configure
            .observe(
                context,
                ProviderOperation::SessionConfigure,
                payload,
                async move {
                    owner
                        .configure_inner(work_context, &binding, configuration)
                        .await
                },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::SessionConfigure,
                            ProviderPhase::Preparation,
                            FailureDisposition::Uncertain,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    /// Close the exact fixture binding through retained cleanup after observer interruption.
    async fn session_close(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("controlled fixture has a live Arc owner");
        let cleanup = owner.clone();
        let cleanup_binding = binding.clone();
        let payload = binding.clone();
        let binding = binding.clone();
        let work_context = context.clone();
        self.operations
            .close
            .observe(
                context,
                ProviderOperation::SessionClose,
                payload,
                async move {
                    work_context.check_admission(ProviderOperation::SessionClose)?;
                    owner.close_inner(&binding).await
                },
                move |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::SessionClose,
                            ProviderPhase::Close,
                            FailureDisposition::Invalidated,
                        ),
                        async move { cleanup.close_inner(&cleanup_binding).await },
                    )
                },
            )
            .await
    }

    /// Observe only the targeted retained invocation settlement without selecting a successor.
    async fn cancel(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<InvocationSettlement, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("controlled fixture has a live Arc owner");
        let payload = (binding.clone(), request.clone());
        let binding = binding.clone();
        let work_context = context.clone();
        self.operations
            .cancel
            .observe(
                context,
                ProviderOperation::Cancel,
                payload,
                async move {
                    work_context.check_admission(ProviderOperation::Cancel)?;
                    owner.cancel_inner(&binding, request).await
                },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::Cancel,
                            ProviderPhase::Collection,
                            FailureDisposition::Uncertain,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    /// Keep input admission independently owned when the host drops its acknowledgement waiter.
    async fn input_submit(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        if self.panic_prompt {
            panic!("controlled native submission task panic");
        }
        let owner = self
            .myself
            .upgrade()
            .expect("controlled fixture has a live Arc owner");
        let cleanup = owner.clone();
        let cleanup_binding = binding.clone();
        let payload = (binding.clone(), request.identity());
        let binding = binding.clone();
        let work_context = context.clone();
        self.operations
            .submit
            .observe(
                context,
                ProviderOperation::InputSubmit,
                payload,
                async move { owner.submit_inner(work_context, &binding, request).await },
                move |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::InputSubmit,
                            ProviderPhase::Observation,
                            FailureDisposition::Uncertain,
                        ),
                        async move { cleanup.close_inner(&cleanup_binding).await },
                    )
                },
            )
            .await
    }

    /// Retain the independently held acknowledgement and collect it through binding revocation.
    async fn input_steer(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("controlled fixture has a live Arc owner");
        let cleanup = owner.clone();
        let cleanup_binding = binding.clone();
        let payload = (binding.clone(), request.identity());
        let binding = binding.clone();
        let work_context = context.clone();
        self.operations
            .steer
            .observe(
                context,
                ProviderOperation::InputSteer,
                payload,
                async move { owner.steer_inner(work_context, &binding, request).await },
                move |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::InputSteer,
                            ProviderPhase::Observation,
                            FailureDisposition::Uncertain,
                        ),
                        async move { cleanup.close_inner(&cleanup_binding).await },
                    )
                },
            )
            .await
    }

    /// Inspect complete workspace context without mutating native history.
    async fn inspect(
        &self,
        context: OperationContext,
        workspace: WorkspaceContext,
    ) -> Result<ProviderSetup, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("controlled fixture has a live Arc owner");
        let payload = workspace.clone();
        let work_context = context.clone();
        self.operations
            .inspect
            .observe(
                context,
                ProviderOperation::Inspect,
                payload,
                async move { owner.inspect_inner(work_context, workspace).await },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::Inspect,
                            ProviderPhase::Observation,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    /// Revoke all fixture resources and join every other retained operation worker.
    async fn shutdown(&self, context: OperationContext) -> Result<(), ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("controlled fixture has a live Arc owner");
        let cleanup = owner.clone();
        let operation = context.id().clone();
        let cleanup_operation = operation.clone();
        let work_context = context.clone();
        self.operations
            .shutdown
            .observe(
                context,
                ProviderOperation::Shutdown,
                (),
                async move {
                    work_context.check_admission(ProviderOperation::Shutdown)?;
                    owner.shutdown_inner(&operation).await
                },
                move |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::Shutdown,
                            ProviderPhase::Close,
                            FailureDisposition::Invalidated,
                        ),
                        async move { cleanup.shutdown_inner(&cleanup_operation).await },
                    )
                },
            )
            .await
    }
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Match production identities: managed children share a session, Background has its own.
fn owners() -> [ExecutionOwner; 3] {
    [
        ExecutionOwner::root("conversation"),
        ExecutionOwner {
            session_id: "conversation".into(),
            agent_id: "managed-child".into(),
        },
        ExecutionOwner::root("background-session"),
    ]
}

/// Prepare a request with an explicit owner and the same persisted journal as production.
fn request(owner: &ExecutionOwner, directory: &std::path::Path) -> ProviderRequest {
    ProviderRequest {
        dispatch: forge_provider_api::submission::DispatchTracker::default(),
        execution_owner: Some(owner.clone()),
        harness_session_id: owner.session_id.to_string(),
        workspace: directory.to_string_lossy().into_owned(),
        input: ProviderInput::from_text("inspect"),
        mode: if owner.session_id == forge_provider_api::SessionId::from("background-session") {
            PromptMode::Background
        } else {
            PromptMode::Chat
        },
        model: "mock".into(),
        effort: "low".into(),
        context_window: None,
        service_tier: Default::default(),
        access: Default::default(),
        approval_mode: crate::session::ApprovalMode::Ask,
        provider_session_id: None,
        submission: None,
        recovered_input: Vec::new(),
        control_context: None,
    }
}

/// Construct a native address without conflating its owner, exchange, and execution anchor.
fn address(scope: &InvocationScope, turn: &str) -> crate::provider::TurnAddress {
    crate::provider::TurnAddress {
        scope: scope.clone(),
        thread_id: "controlled-thread".into(),
        turn_id: turn.into(),
    }
}

/// Publish native terminal evidence for exactly the supplied admitted journal inputs.
fn finished(scope: &InvocationScope, turn: &str, covered: Vec<SubmissionId>) -> ExecutionUpdate {
    ExecutionUpdate::Finished {
        address: address(scope, turn),
        covered_submissions: covered,
        completed_at_ms: 1,
        result: ExecutionResult::Completed {
            completion: forge_provider_api::completion::ProviderCompletion::default().into(),
        },
    }
}

/// Replay the complete original operation before expired admission and reject context reuse across payloads or domains.
#[tokio::test(start_paused = true)]
async fn controlled_operation_replays_after_expiry_and_rejects_changed_identity()
-> anyhow::Result<()> {
    let (dispatch, _calls) = mpsc::channel(1);
    let peer = ControlledProvider::new(dispatch, false);
    let (source, cancellation) = forge_provider_api::OperationCancellation::channel();
    let context = OperationContext::new(
        forge_provider_api::OperationId::new("controlled-inspection")?,
        std::time::Duration::from_millis(1),
        cancellation,
    )?;
    let workspace = WorkspaceContext {
        workspace: "original-workspace".into(),
    };
    let first = peer.inspect(context.clone(), workspace.clone()).await?;
    tokio::time::advance(std::time::Duration::from_millis(2)).await;
    source.cancel();
    let replay = peer.inspect(context.clone(), workspace).await?;
    assert_eq!(first.capabilities.provider, replay.capabilities.provider);
    assert_eq!(
        first.capabilities.effective_revision,
        replay.capabilities.effective_revision
    );
    let error = peer
        .inspect(
            context.clone(),
            WorkspaceContext {
                workspace: "changed-workspace".into(),
            },
        )
        .await
        .unwrap_err();
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::OperationConflict)
    );
    let error = peer
        .session_close(context, &BindingId::new("unrelated-binding")?)
        .await
        .unwrap_err();
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::OperationConflict)
    );
    peer.shutdown(
        crate::provider::operation::OperationControl::fresh(ProviderOperation::Shutdown)?.context(),
    )
    .await?;
    Ok(())
}

/// Cancel owner-reserved preparation before Execute exists and keep an old resume from clearing a successor's fence.
#[tokio::test]
async fn reserved_preparation_cancel_releases_receipt_and_stale_resume_preserves_successor_fence()
-> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let (dispatch, mut calls) = mpsc::channel(4);
        let peer = ControlledProvider::new(dispatch, false);
        let provider = crate::provider::test_support::ProviderTestSession::from_registered(peer.registration())?;
        let registry = SessionExecutionRegistry::new(provider.provider_handle(), trace);
        let owner = ExecutionOwner::root("reserved-before-execute");
        let engine = registry.owner(owner.clone()).await?;
        let first_scope = InvocationScope::new(owner.clone(), "exchange");
        let first = store.prepare(&request(&owner, directory.path()), engine.provider_id())?;
        let released = Arc::downgrade(&first);
        engine.reserve_invocation(first_scope.clone(), first.clone())?;
        let first_collection = engine.collect_exchange(first_scope.clone()).await?;
        assert_eq!(first.record()?.delivery, crate::provider::submission::SubmissionState::Cancelled);
        drop(first);
        assert!(released.upgrade().is_none(), "collected prevention retained live receipt authority");
        engine.resume_exchange(first_collection.clone()).await?;
        engine.cancel_exchange(first_scope.clone()).await?;
        assert!(matches!(engine.resume_exchange(first_collection.clone()).await,
            Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation));
        let current = engine.collect_exchange(first_scope.clone()).await?;
        engine.resume_exchange(current).await?;

        let second_scope = InvocationScope::new(owner.clone(), "exchange");
        let mut second_request = request(&owner, directory.path());
        second_request.input = ProviderInput::from_text("second prepared instruction");
        let second = store.prepare(&second_request, engine.provider_id())?;
        engine.reserve_invocation(second_scope.clone(), second.clone())?;
        engine.cancel_exchange(second_scope).await?;
        assert_eq!(second.record()?.delivery, crate::provider::submission::SubmissionState::Cancelled);
        let stale = engine.resume_exchange(first_collection).await;
        assert!(matches!(stale, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation));

        let third_scope = InvocationScope::new(owner.clone(), "exchange");
        let (sink, _events) = crate::provider::events::channel(third_scope);
        let mut third_request = request(&owner, directory.path());
        third_request.input = ProviderInput::from_text("must remain fenced");
        assert!(matches!(engine.run_until_ready(third_request, sink, &store).await, Err(ProviderError::Cancelled)));
        assert!(calls.try_recv().is_err(), "stale resume admitted native work through the successor fence");
        registry.close().await?;
        peer.shutdown(crate::provider::operation::OperationControl::fresh(ProviderOperation::Shutdown)?.context()).await?;
        Ok::<_, anyhow::Error>(())
    }).await?
}

/// Reopen an explicit settled replacement while rejecting retired scopes and isolating stale cancellation.
#[tokio::test]
async fn ordinary_successor_has_fresh_scope_and_stale_cancel_cannot_stop_it() -> anyhow::Result<()>
{
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let (dispatch, mut calls) = mpsc::channel(4);
        let peer = ControlledProvider::new(dispatch, false);
        let provider = crate::provider::test_support::ProviderTestSession::from_registered(peer.registration())?;
        let registry = SessionExecutionRegistry::new(provider.provider_handle(), trace);
        let owner = ExecutionOwner::root("ordinary-successor");
        let engine = registry.owner(owner.clone()).await?;
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let (sink, _events) = crate::provider::events::channel(scope.clone());
        let running = engine.clone();
        let journal = store.clone();
        let mut first_request = request(&owner, directory.path());
        first_request.submission = Some(store.prepare(&first_request, engine.provider_id())?);
        let retired_request = first_request.clone();
        let first_sink = sink.clone();
        let first = tokio::spawn(async move { running.run_until_ready(first_request, first_sink, &journal).await });
        let Some(Dispatch::Prompt { scope: first_scope, input: first_input }) = calls.recv().await else { anyhow::bail!("missing first input"); };
        peer.publish(&first_scope, ExecutionUpdate::Started { address: address(&first_scope, "first") }).await?;
        peer.publish(&first_scope, finished(&first_scope, "first", vec![first_input.clone()])).await?;
        let ready = first.await??;
        let first_collection = engine.collect_exchange(first_scope.clone()).await?;
        engine.resume_exchange(first_collection.clone()).await?;
        let retired = engine.run_until_ready(retired_request, sink.clone(), &store).await;
        assert!(matches!(retired, Err(ProviderError::Failure(failure))
            if failure.operation() == ProviderOperation::InputSubmit && failure.kind() == ProviderFailureKind::WrongInvocation));
        assert!(calls.try_recv().is_err(), "replacement authorization replayed a retired invocation");
        let running = engine.clone();
        let journal = store.clone();
        let mut next_request = request(&owner, directory.path());
        next_request.input = ProviderInput::from_text("new host instruction");
        let second = tokio::spawn(async move { running.continue_attempt(&ready.ticket, next_request, sink, &journal).await });
        let Some(Dispatch::Prompt { scope: second_scope, input: second_input }) = calls.recv().await else { anyhow::bail!("missing successor input"); };
        assert_ne!(first_scope.invocation_id, second_scope.invocation_id);
        assert_eq!(first_scope.exchange_id, second_scope.exchange_id);
        assert_ne!(first_input, second_input);
        peer.publish(&second_scope, ExecutionUpdate::Started { address: address(&second_scope, "second") }).await?;
        engine.cancel(first_scope.clone()).await?;
        assert!(!second.is_finished(), "old cancellation released its successor");
        peer.publish(&second_scope, finished(&second_scope, "second", vec![second_input])).await?;
        let ContinuationAttempt::Ready(ready) = second.await?? else { anyhow::bail!("successor rejected current host ticket"); };
        ready.outcome?;
        ready.ticket.commit().await?;
        engine.cancel_exchange(second_scope).await?;
        assert!(matches!(engine.resume_exchange(first_collection).await,
            Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation));
        let third_scope = InvocationScope::new(owner.clone(), "exchange");
        let (third_sink, _events) = crate::provider::events::channel(third_scope);
        let mut third_request = request(&owner, directory.path());
        third_request.input = ProviderInput::from_text("cancelled successor must stay fenced");
        assert!(matches!(engine.run_until_ready(third_request, third_sink, &store).await, Err(ProviderError::Cancelled)));
        assert!(calls.try_recv().is_err());
        registry.close().await?;
        peer.shutdown(crate::provider::operation::OperationControl::fresh(ProviderOperation::Shutdown)?.context()).await?;
        Ok::<_, anyhow::Error>(())
    }).await?
}

/// Collect an adapter's admitted predispatch supervisor even though its receipt proves no model input was sent.
#[tokio::test]
async fn prepared_cancellation_collects_native_admission_without_dispatch() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let started = Arc::new(tokio::sync::Semaphore::new(0));
        let writes = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let observed = writes.clone();
        let native = crate::provider::test_support::EventFixtureProvider::new(
            forge_provider_api::ProviderDescriptor {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies fixture identity"),
                label: "Prepared cancellation fixture".into(),
                capability: Default::default(),
            },
            move |_, _, _| {
                let observed = observed.clone();
                async move {
                    observed.fetch_add(1, Ordering::SeqCst);
                    Err(ProviderError::Cancelled)
                }
            },
        );
        let entered = started.clone();
        native.with_pre_dispatch(move |_| {
            let entered = entered.clone();
            async move {
                entered.add_permits(1);
                std::future::pending::<Result<(), ProviderError>>().await
            }
        });
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let registry = SessionExecutionRegistry::new(provider.provider_handle(), trace);
        let owner = ExecutionOwner::root("prepared-cancel");
        let engine = registry.owner(owner.clone()).await?;
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let mut input = request(&owner, directory.path());
        let receipt = store.prepare(&input, engine.provider_id())?;
        input.submission = Some(receipt.clone());
        let (sink, _events) = crate::provider::events::channel(scope.clone());
        let running = engine.clone();
        let journal = store.clone();
        let result =
            tokio::spawn(async move { running.run_until_ready(input, sink, &journal).await });
        crate::provider::test_support::wait_gate(&started).await?;
        engine.cancel(scope).await?;
        assert!(matches!(result.await?, Err(ProviderError::Cancelled)));
        assert_eq!(writes.load(Ordering::SeqCst), 0);
        assert_eq!(
            receipt.record()?.delivery,
            crate::provider::submission::SubmissionState::Cancelled
        );
        registry.close().await?;
        provider.as_provider().shutdown().await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

/// Drain an allocated predispatch rejection before releasing its scope or admitting another input.
#[tokio::test]
async fn predispatch_rejection_collects_native_scope_before_owner_reuse() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let preparing = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let writes = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let observed = writes.clone();
        let native = crate::provider::test_support::EventFixtureProvider::new(
            forge_provider_api::ProviderDescriptor {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies fixture identity"),
                label: "Rejected preparation collection fixture".into(),
                capability: Default::default(),
            },
            move |request, sink, _cancelled| {
                let observed = observed.clone();
                async move {
                    observed.fetch_add(1, Ordering::SeqCst);
                    sink.send_wait(ProviderEvent::Execution {
                        observation: ExecutionObservation::new(
                            request.scope.clone(),
                            ExecutionUpdate::Started {
                                address: address(&request.scope, "accepted"),
                            },
                        )?,
                    })
                    .await?;
                    sink.send_wait(ProviderEvent::Execution {
                        observation: ExecutionObservation::new(
                            request.scope.clone(),
                            finished(&request.scope, "accepted", vec![request.submission_id]),
                        )?,
                    })
                    .await?;
                    Ok(())
                }
            },
        );
        native.with_pre_dispatch(move |_| {
            let preparing = preparing.clone();
            async move {
                if preparing.fetch_add(1, Ordering::SeqCst) == 0 {
                    return Err(ProviderError::Failure(ProviderFailure::new(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Initialization,
                    )));
                }
                Ok(())
            }
        });
        let collection_started = Arc::new(tokio::sync::Semaphore::new(0));
        let collection_release = Arc::new(tokio::sync::Semaphore::new(0));
        let collecting = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let started = collection_started.clone();
        let release = collection_release.clone();
        native.with_cleanup(move |_| {
            let collecting = collecting.clone();
            let started = started.clone();
            let release = release.clone();
            async move {
                if collecting.fetch_add(1, Ordering::SeqCst) == 0 {
                    started.add_permits(1);
                    crate::provider::test_support::wait_gate(&release).await?;
                }
                Ok(())
            }
        });

        // Keep the exact failed scope authoritative until its allocated native supervisor collects.
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let registry = SessionExecutionRegistry::new(provider.provider_handle(), trace);
        let owner = ExecutionOwner::root("rejected-preparation");
        let engine = registry.owner(owner.clone()).await?;
        let rejected_scope = InvocationScope::new(owner.clone(), "rejected");
        let mut first = request(&owner, directory.path());
        let receipt = store.prepare(&first, engine.provider_id())?;
        first.submission = Some(receipt.clone());
        let (sink, _events) = crate::provider::events::channel(rejected_scope.clone());
        let running = engine.clone();
        let journal = store.clone();
        let rejected = tokio::spawn(async move { running.run_once(first, sink, &journal).await });
        crate::provider::test_support::wait_gate(&collection_started).await?;
        assert!(
            !rejected.is_finished(),
            "input rejection escaped before native collection"
        );
        assert_eq!(writes.load(Ordering::SeqCst), 0);
        collection_release.add_permits(1);
        assert!(
            matches!(rejected.await?, Err(ProviderError::PreparationFailed(error))
            if matches!(error.as_ref(), ProviderError::Failure(failure)
                if failure.kind() == ProviderFailureKind::Initialization))
        );
        assert!(matches!(
            receipt.record()?.delivery,
            crate::provider::submission::SubmissionState::Prepared
        ));
        assert!(engine.is_healthy().await);

        // Reuse the owner through an explicitly admitted fresh invocation after both collections.
        let successor = InvocationScope::new(owner.clone(), "successor");
        let (sink, _events) = crate::provider::events::channel(successor.clone());
        engine
            .run_once(request(&owner, directory.path()), sink, &store)
            .await?;
        assert_ne!(successor.invocation_id, rejected_scope.invocation_id);
        assert_eq!(writes.load(Ordering::SeqCst), 1);
        assert!(engine.is_healthy().await);
        registry.close().await?;
        provider.as_provider().shutdown().await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

/// Backpressure mixed foreground output without blocking exact cancellation or configuration reuse.
#[tokio::test]
async fn foreground_backpressure_preserves_burst_cancel_and_successor_order() -> anyhow::Result<()>
{
    tokio::time::timeout(std::time::Duration::from_secs(15), async {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let owner = ExecutionOwner::root("foreground-backpressure");
        let scope = InvocationScope::new(owner.clone(), "configuration-exchange");
        let original = scope.clone();
        let calls = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let observed = calls.clone();
        let native = crate::provider::test_support::EventFixtureProvider::new(
            forge_provider_api::ProviderDescriptor {
                kind: forge_providers::registry::nonselectable_id().expect("registry supplies fixture identity"),
                label: "Slow foreground fixture".into(), capability: Default::default(),
            }, move |request, sink, mut cancelled| {
                let original = original.clone();
                let observed = observed.clone();
                async move {
                    observed.fetch_add(1, Ordering::SeqCst);
                    let turn = address(&request.scope, "native");
                    sink.send_wait(ProviderEvent::Execution {
                        observation: ExecutionObservation::new(request.scope.clone(), ExecutionUpdate::Started { address: turn.clone() })?,
                    }).await?;
                    let result = if request.scope == original {
                        let message = forge_provider_api::MessageAddress { turn: turn.clone(), message_id: forge_provider_api::MessageId::Native("response".into()) };
                        let tool = forge_provider_api::ToolAddress { turn: turn.clone(), tool_id: "tool".into() };
                        sink.send_wait(ProviderEvent::Message { address: message.clone(), update: forge_provider_api::MessageUpdate::Started {
                            classification: forge_provider_api::MessageClassification::Known(forge_provider_api::MessageRole::Response), text: String::new(),
                        }}).await?;
                        sink.send_wait(ProviderEvent::Tool { address: tool.clone(), update: forge_provider_api::ToolUpdate::Started }).await?;
                        let mut complete = String::new();
                        for index in 0..forge_protocol::MAX_PENDING_FRAMES * 8 {
                            let text = format!("{index};");
                            complete.push_str(&text);
                            sink.send_wait(ProviderEvent::Message { address: message.clone(), update: forge_provider_api::MessageUpdate::Delta { text: text.clone() } }).await?;
                            sink.send_wait(ProviderEvent::Tool { address: tool.clone(), update: forge_provider_api::ToolUpdate::Output { update: forge_provider_api::TextUpdate::Append(text) } }).await?;
                        }
                        loop {
                            if *cancelled.borrow_and_update() { break; }
                            cancelled.changed().await.map_err(|_| ProviderError::DeliveryUncertain)?;
                        }
                        sink.send_wait(ProviderEvent::Tool { address: tool, update: forge_provider_api::ToolUpdate::Finished { outcome: forge_provider_api::ToolOutcome::Cancelled } }).await?;
                        sink.send_wait(ProviderEvent::Message { address: message, update: forge_provider_api::MessageUpdate::Completed { text: complete } }).await?;
                        ExecutionResult::Cancelled { completion: forge_provider_api::completion::ProviderCompletion::default().into() }
                    } else {
                        ExecutionResult::Completed { completion: forge_provider_api::completion::ProviderCompletion::default().into() }
                    };
                    sink.send_wait(ProviderEvent::Execution {
                        observation: ExecutionObservation::new(request.scope, ExecutionUpdate::Finished {
                            address: turn, covered_submissions: vec![request.submission_id], completed_at_ms: crate::provider::retry::now_ms(), result,
                        })?,
                    }).await?;
                    Ok(())
                }
            },
        );
        let cancellation_started = Arc::new(tokio::sync::Semaphore::new(0));
        let notified = cancellation_started.clone();
        native.with_cleanup_request(move |_| {
            let notified = notified.clone();
            async move { notified.add_permits(1); Ok(()) }
        });
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let registry = SessionExecutionRegistry::new(provider.provider_handle(), trace);
        let engine = registry.owner(owner.clone()).await?;
        let (sink, mut events) = crate::provider::events::channel(scope.clone());
        let input = request(&owner, directory.path());
        let running = engine.clone();
        let journal = store.clone();
        let result = tokio::spawn(async move { running.run_until_ready(input, sink, &journal).await });

        // Hold presentation until the real owner has retained a blocked frame, then issue control.
        let mut pressure = engine.delivery_pressure.clone();
        pressure.wait_for(|blocked| *blocked).await?;
        assert!(engine.is_healthy().await);
        let collecting = engine.clone();
        let selected = scope.clone();
        let collection = tokio::spawn(async move { collecting.collect_exchange(selected).await });
        crate::provider::test_support::wait_gate(&cancellation_started).await?;
        assert!(!collection.is_finished(), "collection bypassed the undelivered native prefix");
        assert!(!result.is_finished(), "readiness overtook blocked presentation");

        // Drain slowly and check every interleaved text/tool frame before native readiness.
        let mut messages = 0;
        let mut tools = 0;
        loop {
            let event = events.recv().await?.context("foreground burst ended before terminal evidence")?;
            assert_eq!(event.scope().invocation(), Some(&scope));
            match event {
                ProviderEvent::Message { update: forge_provider_api::MessageUpdate::Delta { text }, .. } => {
                    assert_eq!(messages, tools);
                    assert_eq!(text, format!("{messages};"));
                    messages += 1;
                },
                ProviderEvent::Tool { update: forge_provider_api::ToolUpdate::Output { update: forge_provider_api::TextUpdate::Append(text) }, .. } => {
                    assert_eq!(messages, tools + 1);
                    assert_eq!(text, format!("{tools};"));
                    tools += 1;
                },
                ProviderEvent::Execution { observation } if matches!(observation.update(), ExecutionUpdate::Finished { .. }) => break,
                _ => {},
            }
            tokio::task::yield_now().await;
        }
        assert_eq!(messages, forge_protocol::MAX_PENDING_FRAMES * 8);
        assert_eq!(tools, messages);
        let ready = result.await??;
        assert!(matches!(ready.outcome, Err(ProviderError::Cancelled)));
        let collection = collection.await??;
        ready.ticket.commit().await?;
        assert_eq!(calls.load(Ordering::SeqCst), 1);
        assert!(engine.is_healthy().await);

        // Reopen only with exact collection proof and keep the successor on its fresh event stream.
        engine.resume_exchange(collection).await?;
        let successor = InvocationScope::new(owner.clone(), scope.exchange_id.clone());
        let (sink, mut successor_events) = crate::provider::events::channel(successor.clone());
        engine.run_once(request(&owner, directory.path()), sink, &store).await?;
        while let Ok(event) = successor_events.try_recv() {
            assert_eq!(event.scope().invocation(), Some(&successor));
        }
        assert!(matches!(events.try_recv(), Err(crate::provider::events::EventDeliveryError::Empty)));
        assert_eq!(calls.load(Ordering::SeqCst), 2);
        registry.close().await?;
        provider.as_provider().shutdown().await?;
        Ok::<_, anyhow::Error>(())
    }).await?
}

/// Preserve old A's live boundary and fail both A and newer B without replaying either input.
#[tokio::test]
async fn committed_old_owner_execution_conflicts_with_new_active_exchange_without_losing_attribution()
-> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(20), async {
        for _ in 0..10 {
            let directory = tempfile::tempdir()?;
            let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
            let store = SubmissionStore::open(directory.path(),trace.clone())?;
            let (dispatch, mut calls) = mpsc::channel(16);
            let peer = ControlledProvider::new(dispatch, false);
            let provider_session = crate::provider::test_support::ProviderTestSession::from_registered(peer.registration())?;
            let registry = SessionExecutionRegistry::new(provider_session.provider_handle(), trace);
            for owner in owners() {
                let engine = registry.owner(owner.clone()).await?;
                let observer = registry.observer(owner.clone())?;
                let a = InvocationScope::new(owner.clone(),"committed-a");
                let mut first_request = request(&owner,directory.path());
                let receipt_a = store.prepare(&first_request,forge_providers::registry::nonselectable_id().expect("registry supplies a nonselectable fixture identity"))?;
                first_request.submission = Some(receipt_a.clone());
                let (sink_a, mut events_a) = crate::provider::events::channel(a.clone());
                let running = engine.clone(); let journal = store.clone();
                let first = tokio::spawn(async move { running.run_until_ready(first_request,sink_a,&journal).await });
                let Some(Dispatch::Prompt { scope, input }) = calls.recv().await else { anyhow::bail!("missing A prompt"); };
                assert_eq!(scope,a);
                peer.publish(&a,ExecutionUpdate::Started { address:address(&a,"a-original") }).await?;
                peer.publish(&a,finished(&a,"a-original",vec![input])).await?;
                let ready = first.await??;
                ready.ticket.commit().await?;
                let b = InvocationScope::new(owner.clone(),"active-b");
                let mut next_request = request(&owner,directory.path());
                let receipt_b = store.prepare(&next_request,forge_providers::registry::nonselectable_id().expect("registry supplies a nonselectable fixture identity"))?;
                next_request.submission = Some(receipt_b.clone());
                let (sink_b, mut events_b) = crate::provider::events::channel(b.clone());
                let running = engine.clone(); let journal = store.clone();
                let second = tokio::spawn(async move { running.run_until_ready(next_request,sink_b,&journal).await });
                let Some(Dispatch::Prompt { scope, .. }) = calls.recv().await else { anyhow::bail!("missing B prompt"); };
                assert_eq!(scope,b);
                peer.publish(&b,ExecutionUpdate::Started { address:address(&b,"b-active") }).await?;
                loop {
                    if let Some(ProviderEvent::Execution { observation }) = events_b.recv().await?
                        && matches!(observation.update(),ExecutionUpdate::Started {..}) { break; }
                }
                peer.publish(&a,ExecutionUpdate::Started { address:address(&a,"a-live-late") }).await?;
                let error = match second.await? { Err(error) => error, Ok(_) => panic!("ownership conflict produced a complete B result") };
                assert!(matches!(error,ProviderError::Observation(ObservationFailure::OwnershipConflict)));
                engine.close().await?;
                for (scope, events) in [(&a,&mut events_a),(&b,&mut events_b)] {
                    let mut lost = false;
                    while let Some(event) = events.recv().await? {
                        if let ProviderEvent::Execution { observation } = event
                            && matches!(observation.update(),ExecutionUpdate::ObservationLost { failure:ObservationFailure::OwnershipConflict,.. }) {
                            assert_eq!(observation.scope(),scope); lost=true; break;
                        }
                    }
                    assert!(lost,"conflict did not preserve typed loss for {scope:?}");
                }
                let prefix = observer.drain()?;
                assert!(prefix.iter().any(|event| matches!(event,ProviderEvent::Execution {observation}
                    if observation.scope()==&a && matches!(observation.update(),ExecutionUpdate::Started {address} if address.turn_id==forge_provider_api::ProviderTurnId::from("a-live-late")))));
                assert!(matches!(observer.end(),Some(super::super::ObservationEnd::Lost(ObservationFailure::OwnershipConflict))));
                for receipt in [receipt_a,receipt_b] {
                    assert!(matches!(receipt.record()?.delivery,crate::provider::submission::SubmissionState::Accepted {..}));
                }
                assert!(!engine.is_healthy().await);
                assert!(calls.try_recv().is_err(),"ownership conflict replayed an accepted native input");
            }
            registry.close().await?;
            peer.shutdown(crate::provider::operation::OperationControl::fresh(ProviderOperation::Shutdown)?.context()).await?;
        }
        Ok::<_,anyhow::Error>(())
    }).await?
}

/// Reject nominally successful cleanup that has no terminal coverage for accepted input.
#[tokio::test]
async fn intentional_retirement_rejects_accepted_input_without_terminal_proof() -> anyhow::Result<()>
{
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let (dispatch, mut calls) = mpsc::channel(4);
        let peer = ControlledProvider::new(dispatch, false);
        let provider_session = crate::provider::test_support::ProviderTestSession::from_registered(
            peer.registration(),
        )?;
        let registry = SessionExecutionRegistry::new(provider_session.provider_handle(), trace);
        let owner = ExecutionOwner::root("missing-terminal");
        let engine = registry.owner(owner.clone()).await?;
        let observer = registry.observer(owner.clone())?;
        let scope = InvocationScope::new(owner.clone(), "accepted-without-start");
        let mut input = request(&owner, directory.path());
        let receipt = store.prepare(
            &input,
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
        )?;
        input.submission = Some(receipt.clone());
        let (sink, _events) = crate::provider::events::channel(scope);
        let running = engine.clone();
        let journal = store.clone();
        let result =
            tokio::spawn(async move { running.run_until_ready(input, sink, &journal).await });
        assert!(matches!(calls.recv().await, Some(Dispatch::Prompt { .. })));
        let closed = engine.close_with_reason(OwnerCloseReason::Cancelled).await;
        assert!(
            matches!(
                closed,
                Err(ProviderError::Observation(
                    ObservationFailure::LifecycleUnavailable
                ))
            ),
            "retirement without terminal proof: {closed:?}"
        );
        assert!(matches!(
            result.await?,
            Err(ProviderError::Observation(
                ObservationFailure::LifecycleUnavailable
            ))
        ));
        assert!(matches!(
            observer.end(),
            Some(super::super::ObservationEnd::Lost(
                ObservationFailure::LifecycleUnavailable
            ))
        ));
        assert!(matches!(
            receipt.record()?.delivery,
            crate::provider::submission::SubmissionState::Accepted { .. }
        ));
        assert!(calls.try_recv().is_err());
        registry.close().await?;
        peer.shutdown(
            crate::provider::operation::OperationControl::fresh(ProviderOperation::Shutdown)?
                .context(),
        )
        .await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

/// Observe that the production engine reduced a native boundary before checking its waiter.
async fn wait_finished(
    events: &mut crate::provider::events::ProviderEventStream,
    turn: &str,
) -> anyhow::Result<()> {
    loop {
        if let Some(ProviderEvent::Execution { observation }) = events.recv().await?
            && matches!(observation.update(), ExecutionUpdate::Finished { address, .. } if address.turn_id == forge_provider_api::ProviderTurnId::from(turn))
        {
            return Ok(());
        }
    }
}

/// Create a journaled steering input instead of inventing a second submission identity.
fn steering(
    store: &Arc<SubmissionStore>,
    owner: &ExecutionOwner,
    directory: &std::path::Path,
) -> anyhow::Result<SteeringInput> {
    let catalog = crate::provider::ProviderCatalogRequest {
        execution_owner: Some(owner.clone()),
        harness_session_id: owner.session_id.to_string(),
        workspace: directory.to_string_lossy().into_owned(),
        provider_session_id: None,
        access: Default::default(),
        approval_mode: crate::session::ApprovalMode::Ask,
    };
    Ok(SteeringInput::User {
        text: "continue".into(),
        attachments: Vec::new(),
        submission: Some(
            store.prepare_steering(
                &catalog,
                ProviderInput::from_text("continue"),
                forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
            )?,
        ),
    })
}

/// Preserve successful terminal evidence when a racing steer is positively rejected before dispatch.
#[tokio::test]
async fn completion_racing_steering_preserves_proven_rejection_and_delivery_uncertainty()
-> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        for rejection in [
            ProviderFailure::new(ProviderOperation::InputSteer, ProviderFailureKind::WrongInvocation)
                .at_phase(ProviderPhase::Admission, FailureDisposition::Unchanged),
            ProviderFailure::new(ProviderOperation::InputSteer, ProviderFailureKind::Rejected)
                .at_phase(ProviderPhase::Preparation, FailureDisposition::Unchanged),
            ProviderFailure::new(ProviderOperation::InputSteer, ProviderFailureKind::Rejected)
                .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain),
        ] {
            let directory = tempfile::tempdir()?;
            let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
            let store = SubmissionStore::open(directory.path(), trace.clone())?;
            let (dispatch, mut calls) = mpsc::channel(4);
            let peer = ControlledProvider::new(dispatch, false);
            *peer.steering_failure.lock().unwrap() = Some(rejection);
            let provider = crate::provider::test_support::ProviderTestSession::from_registered(peer.registration())?;
            let registry = SessionExecutionRegistry::new(provider.provider_handle(), trace);
            let owner = ExecutionOwner::root("completion-racing-steering");
            let engine = registry.owner(owner.clone()).await?;
            let observer = registry.observer(owner.clone())?;
            let scope = InvocationScope::new(owner.clone(), "original");
            let (sink, mut events) = crate::provider::events::channel(scope.clone());
            let original = request(&owner, directory.path());
            let running = engine.clone();
            let journal = store.clone();
            let result = tokio::spawn(async move { running.run_until_ready(original, sink, &journal).await });
            let Some(Dispatch::Prompt { scope: dispatched, input }) = calls.recv().await else {
                anyhow::bail!("original invocation was not dispatched");
            };
            assert_eq!(dispatched, scope);
            peer.publish(&scope, ExecutionUpdate::Started { address: address(&scope, "original") }).await?;

            // Hold preflight independently of a native terminal and its collected stream watermark.
            let steer_input = steering(&store, &owner, directory.path())?;
            let receipt = steer_input.submission().context("steering receipt missing")?;
            let running = engine.clone();
            let selected = scope.clone();
            let steering = tokio::spawn(async move { running.steer(selected, steer_input).await });
            let Some(Dispatch::Steer { scope: dispatched, input: rejected_input, acknowledgment }) = calls.recv().await else {
                anyhow::bail!("steering preflight was not entered");
            };
            assert_eq!(dispatched, scope);
            assert_eq!(&rejected_input, receipt.receipt_id());
            peer.publish(&scope, finished(&scope, "original", vec![input])).await?;
            wait_finished(&mut events, "original").await?;
            let settlement = peer.settlements.lock().unwrap().get(&scope).cloned()
                .context("original native settlement was not retained")?;
            assert!(matches!(settlement.result, ExecutionResult::Completed { .. }));
            provider.as_provider().wait_watermark(&owner, settlement.watermark).await?;
            assert!(!result.is_finished(), "native settlement bypassed the held steering result");
            acknowledgment.send(()).map_err(|_| anyhow::anyhow!("steering preflight acknowledgment lost"))?;
            let error = steering.await?.expect_err("scripted steering rejection succeeded");

            if rejection.disposition() == FailureDisposition::Unchanged {
                assert!(matches!(error, ProviderError::PreparationFailed(cause)
                    if matches!(cause.as_ref(), ProviderError::Failure(failure) if *failure == rejection)),
                    "positive preflight rejection lost its non-consumption classification");
                assert!(matches!(receipt.record()?.delivery, crate::provider::submission::SubmissionState::Prepared));
                let ready = result.await??;
                assert!(ready.outcome.is_ok(), "rejected steering rewrote successful native completion");
                assert_eq!(ready.ticket.scope, scope);
                ready.ticket.commit().await?;
                assert!(engine.is_healthy().await);
                assert!(observer.end().is_none());

                // Explicitly admit the still-prepared queued input using a fresh invocation identity.
                let successor = InvocationScope::new(owner.clone(), "queued-successor");
                let (sink, mut successor_events) = crate::provider::events::channel(successor.clone());
                let mut queued = receipt.record()?.request;
                queued.dispatch = Default::default();
                queued.submission = Some(receipt.clone());
                let running = engine.clone();
                let journal = store.clone();
                let next = tokio::spawn(async move { running.run_until_ready(queued, sink, &journal).await });
                let Some(Dispatch::Prompt { scope: dispatched, input }) = calls.recv().await else {
                    anyhow::bail!("normal completion fenced its queued successor");
                };
                assert_eq!(dispatched, successor);
                assert_eq!(input, rejected_input);
                peer.publish(&successor, ExecutionUpdate::Started { address: address(&successor, "successor") }).await?;
                peer.publish(&successor, finished(&successor, "successor", vec![input])).await?;
                wait_finished(&mut successor_events, "successor").await?;
                let next = next.await??;
                assert!(next.outcome.is_ok());
                next.ticket.commit().await?;
                assert!(engine.is_healthy().await);
                assert!(matches!(events.try_recv(), Err(crate::provider::events::EventDeliveryError::Empty)));
            } else {
                assert!(matches!(error, ProviderError::Failure(failure) if failure == rejection));
                assert!(matches!(receipt.record()?.delivery, crate::provider::submission::SubmissionState::Sending { .. }));
                assert!(matches!(result.await?, Err(ProviderError::DeliveryUncertain)),
                    "ambiguous steering was treated as safely rejected input");
                assert!(!engine.is_healthy().await);
                assert!(calls.try_recv().is_err(), "uncertain steering was replayed");
            }
            registry.close().await?;
            peer.shutdown(crate::provider::operation::OperationControl::fresh(ProviderOperation::Shutdown)?.context()).await?;
        }
        Ok::<_, anyhow::Error>(())
    }).await?
}

/// Repeat terminal-before-ack ordering for every owner and retain intake after host return.
#[tokio::test]
async fn every_owner_retains_pending_steering_and_idle_observation() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(20), async {
        for iteration in 0..10 {
            let directory = tempfile::tempdir()?;
            let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
            let store = SubmissionStore::open(directory.path(), trace.clone())?;
            let (dispatch, mut calls) = mpsc::channel(16);
            let peer = ControlledProvider::new(dispatch, false);
            let provider_session =
                crate::provider::test_support::ProviderTestSession::from_registered(
                    peer.registration(),
                )?;
            let registry =
                SessionExecutionRegistry::new(provider_session.provider_handle(), trace.clone());
            for owner in owners() {
                // Admit through the production registry and wait for an actual native dispatch.
                let engine = registry.owner(owner.clone()).await?;
                let _idle = engine.session_events().await?;
                let scope = InvocationScope::new(owner.clone(), format!("exchange-{iteration}"));
                let (sink, mut events) = crate::provider::events::channel(scope.clone());
                let running = engine.clone();
                let journal = store.clone();
                let input_request = request(&owner, directory.path());
                let result =
                    tokio::spawn(
                        async move { running.run_once(input_request, sink, &journal).await },
                    );
                let Dispatch::Prompt {
                    scope: observed,
                    input,
                } = calls.recv().await.context("missing initial dispatch")?
                else {
                    anyhow::bail!("expected prompt")
                };
                assert_eq!(observed, scope);
                peer.publish(
                    &scope,
                    ExecutionUpdate::Started {
                        address: address(&scope, "initial"),
                    },
                )
                .await?;
                assert!(!result.is_finished(), "acceptance settled {owner:?}");

                // Reduce both native terminal boundaries while the steering acknowledgment is held.
                let running = engine.clone();
                let steer_scope = scope.clone();
                let steer_input = steering(&store, &owner, directory.path())?;
                let steer =
                    tokio::spawn(async move { running.steer(steer_scope, steer_input).await });
                let Dispatch::Steer {
                    scope: observed,
                    input: steered,
                    acknowledgment,
                } = calls.recv().await.context("missing steering dispatch")?
                else {
                    anyhow::bail!("expected steer")
                };
                assert_eq!(observed, scope);
                peer.publish(&scope, finished(&scope, "initial", vec![input]))
                    .await?;
                wait_finished(&mut events, "initial").await?;
                assert!(
                    !result.is_finished(),
                    "earlier terminal covered pending input"
                );
                peer.publish(
                    &scope,
                    ExecutionUpdate::Started {
                        address: address(&scope, "steered"),
                    },
                )
                .await?;
                peer.publish(&scope, finished(&scope, "steered", vec![steered]))
                    .await?;
                wait_finished(&mut events, "steered").await?;
                assert!(
                    !result.is_finished(),
                    "terminal bypassed pending acknowledgment"
                );
                acknowledgment
                    .send(())
                    .map_err(|_| anyhow::anyhow!("steering acknowledgment lost"))?;
                steer.await??;
                result.await??;

                let steer_input = steering(&store, &owner, directory.path())?;
                assert!(
                    engine.steer(scope.clone(), steer_input).await.is_err(),
                    "late steering reopened settled work"
                );
                assert_eq!(peer.subscriptions.lock().unwrap().get(&owner), Some(&1));
                assert!(
                    calls.try_recv().is_err(),
                    "provider silently replayed input"
                );
            }
            registry.close().await?;
            for slot in registry.owners.lock().unwrap().values() {
                assert!(
                    !slot.engine.get().unwrap().healthy.load(Ordering::Acquire)
                        || slot.engine.get().unwrap().commands.is_closed()
                );
            }
            peer.shutdown(
                crate::provider::operation::OperationControl::fresh(ProviderOperation::Shutdown)?
                    .context(),
            )
            .await?;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

/// Fence pending controls without cancelling peer owners or replaying accepted input.
#[tokio::test]
async fn every_owner_isolates_pending_control_cancellation_and_loss() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(30), async {
        for iteration in 0..10 {
            for target in 0..3 {
                for lose_observation in [false, true] {
                    let directory = tempfile::tempdir()?;
                    let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
                    let store = SubmissionStore::open(directory.path(), trace.clone())?;
                    let (dispatch, mut calls) = mpsc::channel(16);
                    let peer = ControlledProvider::new(dispatch, false);
                    let provider_session = crate::provider::test_support::ProviderTestSession::from_registered(peer.registration())?;
                    let registry = SessionExecutionRegistry::new(provider_session.provider_handle(), trace.clone());
                    let owners = owners();
                    let mut active = Vec::new();

                    // Keep all three independent owners active before disrupting one of them.
                    for owner in &owners {
                        let engine = registry.owner(owner.clone()).await?;
                        let scope = InvocationScope::new(owner.clone(), format!("isolated-{iteration}"));
                        let (sink, events) = crate::provider::events::channel(scope.clone());
                        let running = engine.clone();
                        let journal = store.clone();
                        let input_request = request(owner, directory.path());
                        let result = tokio::spawn(async move { running.run_once(input_request, sink, &journal).await });
                        let Dispatch::Prompt { scope: observed, input } = calls.recv().await.context("missing concurrent prompt")? else { anyhow::bail!("unexpected control before prompt") };
                        assert_eq!(observed, scope);
                        peer.publish(&scope, ExecutionUpdate::Started { address: address(&scope, "active") }).await?;
                        active.push((engine, scope, input, result, events));
                    }
                    let (engine, scope, _, _, _) = &active[target];
                    let steer_input = steering(&store, &owners[target], directory.path())?;
                    let receipt = steer_input.submission().context("missing held steering journal")?;
                    let running = engine.clone();
                    let steer_scope = scope.clone();
                    let steer = tokio::spawn(async move { running.steer(steer_scope, steer_input).await });
                    let Dispatch::Steer { scope: observed, acknowledgment, .. } = calls.recv().await.context("missing held control")? else { anyhow::bail!("expected pending control") };
                    assert_eq!(&observed, scope);

                    // Interrupt only the selected owner while its accepted acknowledgment is pending.
                    if lose_observation {
                        let sink = peer.streams.lock().unwrap().get(&owners[target]).cloned().unwrap();
                        sink.fail(crate::provider::events::EventDeliveryError::Observation(ObservationFailure::Overflow));
                    } else {
                        registry.fence(&owners[target]).await?;
                    }
                    let selected = active.remove(target);
                    let failure = selected.3.await?.expect_err("interrupted work returned successful completion");
                    assert!(matches!(&failure,
                        ProviderError::Observation(ObservationFailure::Overflow) if lose_observation)
                        || matches!(&failure,
                        ProviderError::Observation(ObservationFailure::Disconnected) if !lose_observation),
                        "owner interruption lost its typed cause: {failure}");
                    assert!(steer.await?.is_err(), "pending control outlived its fenced owner");
                    drop(acknowledgment);
                    assert!(matches!(receipt.record()?.delivery, crate::provider::submission::SubmissionState::Accepted { .. }), "cancellation discarded native acceptance");
                    assert!(!selected.0.is_healthy().await);
                    assert!(!peer.cleanup.lock().unwrap().is_empty());
                    assert!(peer.cleanup.lock().unwrap().iter().all(|entry| entry.ends_with(&owners[target].runtime_key())), "cleanup targeted an unrelated owner");

                    // Peer results remain pending and then finish through their unchanged listeners.
                    for (engine, scope, input, result, mut events) in active {
                        assert!(!result.is_finished(), "another owner's failure released a peer result");
                        assert!(engine.is_healthy().await);
                        peer.publish(&scope, finished(&scope, "active", vec![input])).await?;
                        wait_finished(&mut events, "active").await?;
                        result.await??;
                        assert_eq!(peer.subscriptions.lock().unwrap().get(&scope.owner), Some(&1));
                    }
                    assert!(calls.try_recv().is_err(), "cleanup replayed a native input");
                    registry.close().await?;
                    peer.shutdown(crate::provider::operation::OperationControl::fresh(ProviderOperation::Shutdown)?.context()).await?;
                }
            }
        }
        Ok::<_, anyhow::Error>(())
    }).await?
}

/// Normalize a panicking submission without exposing its payload or losing owner fencing.
#[tokio::test]
async fn submission_task_failure_is_normalized_and_fences_owner() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let (dispatch, _calls) = mpsc::channel(4);
        let peer = ControlledProvider::new(dispatch, true);
        let provider_session = crate::provider::test_support::ProviderTestSession::from_registered(
            peer.registration(),
        )?;
        let registry = SessionExecutionRegistry::new(provider_session.provider_handle(), trace);
        let owner = ExecutionOwner::root("submission-panic");
        let engine = registry.owner(owner.clone()).await?;
        let (sink, _events) =
            crate::provider::events::channel(InvocationScope::new(owner.clone(), "exchange"));
        let outcome = engine
            .run_until_ready(request(&owner, directory.path()), sink, &store)
            .await;
        let error = match outcome {
            Err(error) => error,
            Ok(_) => panic!("panicking submission completed"),
        };
        assert!(matches!(&error, ProviderError::Failure(failure)
            if failure.kind() == ProviderFailureKind::Runtime));
        assert!(!format!("{error:?} {error}").contains("controlled native submission task panic"));
        let mut source: Option<&(dyn std::error::Error + 'static)> = Some(&error);
        let mut observed = false;
        while let Some(error) = source {
            observed |= error.downcast_ref::<tokio::task::JoinError>().is_some();
            source = error.source();
        }
        assert!(!observed, "engine exposed an unnormalized worker failure");
        assert!(!engine.is_healthy().await);
        registry.close().await?;
        peer.shutdown(
            crate::provider::operation::OperationControl::fresh(ProviderOperation::Shutdown)?
                .context(),
        )
        .await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}
