use std::collections::HashMap;
use std::sync::{Arc, Weak};

use forge_provider_api::{
    BindingId, ExecutionOwner, ProviderError, ProviderEvent,
    error::{ProviderFailure, ProviderFailureKind, ProviderOperation},
    events::{EventDeliveryError, ProviderEventStream},
    provider::Provider as NativeProvider,
    session::{HostToolEndpoint, ResourceLease, SessionBinding, SessionMode, SessionRequest},
};
use tokio::sync::{Mutex, OnceCell, mpsc};

use crate::tools::control_server::ControlServer;
use crate::tools::control_session::ControlSession;

use super::{ProviderRequest, approval::PermissionCoordinator, operation::OperationControl};

#[cfg(test)]
#[path = "bridge_test.rs"]
mod tests;

/// Prepare host resources and consume normalized provider observations for each owner.
pub struct ProviderBridge {
    native: Arc<dyn NativeProvider>,
    descriptor: forge_provider_api::ProviderDescriptor,
    test_control: Option<Arc<dyn forge_provider_api::testing::ProviderTestControl>>,
    utility: super::text_generation::TextGenerationJobs,
    retained_operations:
        std::sync::Mutex<HashMap<forge_provider_api::OperationId, OperationControl>>,
    permissions: Arc<PermissionCoordinator>,
    trace: Arc<crate::trace::TraceStore>,
    root: std::path::PathBuf,
    sessions: Mutex<HashMap<ExecutionOwner, Arc<HostSession>>>,
    admission: tokio::sync::RwLock<()>,
    closed: Arc<std::sync::atomic::AtomicBool>,
    #[cfg(test)]
    consumption_entered: tokio::sync::Notify,
}

struct HostSession {
    initialized: OnceCell<BoundSession>,
    open_operation: OperationControl,
    preparation: Mutex<()>,
    closing: std::sync::atomic::AtomicBool,
    sender: mpsc::UnboundedSender<Result<HostEvent, EventDeliveryError>>,
    observation: std::sync::Mutex<ObservationState>,
    changed: tokio::sync::Notify,
    observers: std::sync::Mutex<usize>,
    receiver: Mutex<Option<HostEventStream>>,
    invocations: Mutex<
        HashMap<forge_provider_api::InvocationId, Arc<super::host_invocation::HostInvocation>>,
    >,
    request: Mutex<Option<ProviderRequest>>,
}

struct BoundSession {
    binding: SessionBinding,
    relay: Mutex<Option<tokio::task::JoinHandle<Result<(), ProviderError>>>>,
    retiring: Arc<std::sync::atomic::AtomicBool>,
    retired: Arc<Mutex<bool>>,
}

/// Retain an admitted host query across a dropped caller while allowing close to revoke native work.
struct HostObservation {
    host: Arc<HostSession>,
}

impl Drop for HostObservation {
    /// Release only this query's collection obligation and wake retained closure.
    fn drop(&mut self) {
        let mut pending = self
            .host
            .observers
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *pending = pending
            .checked_sub(1)
            .expect("each admitted observation releases exactly one lease");
        drop(pending);
        self.host.changed.notify_waiters();
    }
}

pub(super) struct EndpointLease {
    _server: ControlServer,
}

impl EndpointLease {
    /// Stop the exact invocation endpoint and collect all accepted HTTP calls.
    pub(super) async fn close(&self) -> Result<(), forge_provider_api::HostEffectFailure> {
        match tokio::time::timeout(std::time::Duration::from_secs(15), self._server.close()).await {
            Ok(Ok(())) => Ok(()),
            Ok(Err(_)) | Err(_) => Err(forge_provider_api::HostEffectFailure::Uncertain),
        }
    }
}

struct PreparedOperation {
    binding: BindingId,
    request: Box<forge_provider_api::PromptRequest>,
    effects: Arc<super::host_invocation::HostInvocation>,
}

impl PreparedOperation {
    /// Admit the original journal attempt before handing any request to the native adapter.
    fn admit(
        self,
        submission: Arc<super::submission::Submission>,
    ) -> Result<NativeOperation, ProviderError> {
        let prepare = |error| ProviderError::PreparationFailed(Box::new(error));
        if self.request.submission_id != *submission.receipt_id() {
            return Err(prepare(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::WrongInvocation,
            )));
        }
        let flight = submission.begin_prompt().map_err(|error| {
            prepare(super::lifecycle_error::classify(
                ProviderOperation::InputSubmit,
                error,
            ))
        })?;
        let operation = OperationControl::fresh(ProviderOperation::InputSubmit).map_err(prepare)?;
        self.effects.request_native_input()?;
        Ok(NativeOperation {
            binding: self.binding,
            request: self.request,
            context: operation.context(),
            flight,
        })
    }
}

/// Own a successfully admitted journal flight and its immutable native operation context.
struct NativeOperation {
    binding: BindingId,
    request: Box<forge_provider_api::PromptRequest>,
    context: forge_provider_api::OperationContext,
    flight: super::submission::SubmissionFlight,
}

/// Distinguish unopened policy from a configuration applied to one retained binding generation.
#[derive(Clone, Debug)]
pub(crate) enum ConfigureReceipt {
    /// Native initialization will read the owner's committed policy later.
    Deferred,
    /// Native configuration and its ordered identity observation completed for this exact binding.
    Applied(ConfiguredBindingReceipt),
}

/// Prove ordered configuration without retaining native resources or extending a binding's lifetime.
#[derive(Clone, Debug)]
pub(crate) struct ConfiguredBindingReceipt {
    operation: forge_provider_api::OperationId,
    owner: ExecutionOwner,
    binding: BindingId,
    before_history: Option<forge_provider_api::ProviderSessionId>,
    after_history: Option<forge_provider_api::ProviderSessionId>,
    watermark: forge_provider_api::EventSequence,
    host: Weak<HostSession>,
    provider_closed: Weak<std::sync::atomic::AtomicBool>,
}

impl ConfiguredBindingReceipt {
    /// Get owner-consumed native history before this configuration operation.
    pub(crate) fn before_history(&self) -> Option<&forge_provider_api::ProviderSessionId> {
        self.before_history.as_ref()
    }

    /// Get native history consumed through this operation's configuration watermark.
    pub(crate) fn after_history(&self) -> Option<&forge_provider_api::ProviderSessionId> {
        self.after_history.as_ref()
    }

    /// Get the latest consumed history of this live binding without granting configuration commit authority.
    pub(crate) fn observed_history(
        &self,
        owner: &ExecutionOwner,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
        let host = self.live_host(owner)?;
        let bound = host
            .initialized
            .get()
            .expect("validated binding initialization cannot be removed");
        let state = host.observation.lock().map_err(|_| {
            failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::Runtime,
            )
        })?;
        if state.closed {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        Ok(state
            .identity
            .clone()
            .or_else(|| bound.binding.provider_session_id.clone()))
    }

    /// Reject changed owners, retired generations, newer configuration, or superseding native identity.
    pub(crate) fn validate(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        let host = self.live_host(owner)?;
        let bound = host
            .initialized
            .get()
            .expect("validated binding initialization cannot be removed");
        let state = host.observation.lock().map_err(|_| {
            failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::Runtime,
            )
        })?;
        if state.closed {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if state.last_configuration.as_ref() != Some(&self.operation)
            || state.consumed < self.watermark
            || state
                .identity
                .as_ref()
                .or(bound.binding.provider_session_id.as_ref())
                != self.after_history.as_ref()
        {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::HistoryConflict,
            ));
        }
        Ok(())
    }

    /// Resolve only the original live owner and public binding generation through weak metadata.
    fn live_host(&self, owner: &ExecutionOwner) -> Result<Arc<HostSession>, ProviderError> {
        if owner != &self.owner {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let closed = || {
            failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::BindingClosed,
            )
        };
        let provider_closed = self.provider_closed.upgrade().ok_or_else(closed)?;
        let host = self.host.upgrade().ok_or_else(closed)?;
        let bound = host.initialized.get().ok_or_else(closed)?;
        if provider_closed.load(std::sync::atomic::Ordering::Acquire)
            || host.closing.load(std::sync::atomic::Ordering::Acquire)
            || bound.retiring.load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(closed());
        }
        if bound.binding.id != self.binding {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        Ok(host)
    }
}

/// Carry native sequence evidence separately from host-generated publications.
#[derive(Debug)]
pub(crate) struct HostEvent {
    pub(crate) event: ProviderEvent,
    pub(crate) native: Option<(BindingId, forge_provider_api::EventSequence)>,
}

impl HostEvent {
    /// Wrap a host publication without assigning a native stream position.
    pub(crate) fn host(event: ProviderEvent) -> Self {
        Self {
            event,
            native: None,
        }
    }
}

#[derive(Default)]
struct ObservationState {
    consumed: forge_provider_api::EventSequence,
    published: forge_provider_api::EventSequence,
    closed: bool,
    capabilities: Option<forge_provider_api::CapabilityManifest>,
    identity: Option<forge_provider_api::ProviderSessionId>,
    last_configuration: Option<forge_provider_api::OperationId>,
    pending: std::collections::BTreeMap<forge_provider_api::EventSequence, ProviderEvent>,
    settlements:
        HashMap<forge_provider_api::InvocationId, forge_provider_api::InvocationSettlement>,
}

impl ResourceLease for EndpointLease {}

/// Keep host observation available before native initialization without inventing events.
pub(crate) struct HostEventStream {
    receiver: mpsc::UnboundedReceiver<Result<HostEvent, EventDeliveryError>>,
}

impl HostEventStream {
    /// Receive the next already-normalized event or its typed delivery failure.
    pub(crate) async fn recv(&mut self) -> Result<Option<HostEvent>, EventDeliveryError> {
        self.receiver.recv().await.transpose()
    }

    /// Drain one pending event without waiting for native work.
    pub(crate) fn try_recv(&mut self) -> Result<HostEvent, EventDeliveryError> {
        match self.receiver.try_recv() {
            Ok(event) => event,
            Err(mpsc::error::TryRecvError::Empty) => Err(EventDeliveryError::Empty),
            Err(mpsc::error::TryRecvError::Disconnected) => Err(EventDeliveryError::Closed),
        }
    }
}

impl ProviderBridge {
    /// Retain host policy independently of native construction and session lifetime.
    pub fn new(
        registration: forge_providers::registry::RegisteredProvider,
        permissions: Arc<PermissionCoordinator>,
        trace: Arc<crate::trace::TraceStore>,
        root: std::path::PathBuf,
    ) -> Self {
        Self {
            native: registration.adapter().clone(),
            descriptor: registration.descriptor().clone(),
            test_control: registration.test_control().cloned(),
            utility: Default::default(),
            retained_operations: Default::default(),
            permissions,
            trace,
            root,
            sessions: Mutex::new(HashMap::new()),
            admission: tokio::sync::RwLock::new(()),
            closed: Arc::new(std::sync::atomic::AtomicBool::new(false)),
            #[cfg(test)]
            consumption_entered: tokio::sync::Notify::new(),
        }
    }

    /// Take the one host consumer before native setup or input dispatch begins.
    pub(crate) async fn session_events(
        &self,
        owner: ExecutionOwner,
    ) -> Result<HostEventStream, ProviderError> {
        let _admission = self.admission.read().await;
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionClose,
                ProviderFailureKind::Rejected,
            ));
        }
        let session = self.session(owner).await;
        let result = session.receiver.lock().await.take();
        result.ok_or_else(|| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::SessionClose,
                ProviderFailureKind::Rejected,
            ))
        })
    }

    /// Initialize native history using admitted host resources and a normalized callback endpoint.
    async fn binding_for(
        &self,
        request: &ProviderRequest,
    ) -> Result<SessionBinding, ProviderError> {
        let _admission = self.admission.read().await;
        self.binding_under_admission(request).await
    }

    /// Initialize while the caller retains admission against closure and shutdown.
    async fn binding_under_admission(
        &self,
        request: &ProviderRequest,
    ) -> Result<SessionBinding, ProviderError> {
        self.ensure_admission(ProviderOperation::SessionOpen)?;
        let owner = request.owner().map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Rejected,
            ))
        })?;
        let session = self.session(owner.clone()).await;
        if session.closing.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let initialized = session
            .initialized
            .get_or_try_init(|| async {
                if self.closed.load(std::sync::atomic::Ordering::Acquire)
                    || session.closing.load(std::sync::atomic::Ordering::Acquire)
                {
                    session.open_operation.cancel();
                }
                let descriptor = self.descriptor();
                let catalog = super::ProviderCatalogRequest {
                    execution_owner: Some(owner.clone()),
                    harness_session_id: request.harness_session_id.clone(),
                    workspace: request.workspace.clone(),
                    access: request.access.clone(),
                    approval_mode: request.approval_mode,
                    provider_session_id: request.provider_session_id.clone(),
                };
                let resources = match descriptor.capability.integration.resource_target.as_deref() {
                    Some(target) => crate::settings::SessionResources::load(
                        &self.root,
                        &catalog,
                        descriptor.kind.as_str(),
                        target,
                    )
                    .map_err(|_| {
                        ProviderError::Failure(ProviderFailure::new(
                            ProviderOperation::SessionOpen,
                            ProviderFailureKind::Rejected,
                        ))
                    })?
                    .selection(),
                    None => Default::default(),
                };
                let tools = crate::tools::ControlToolRegistry
                    .registration_definitions(&owner, request.mode);
                let operation = session.open_operation.context();
                let opened = super::operation::observe(
                    self.trace.clone(),
                    ProviderOperation::SessionOpen,
                    &operation,
                    forge_provider_api::EventScope::Binding {
                        owner: owner.clone(),
                    },
                    None,
                    self.native.session_open(
                        operation.clone(),
                        SessionRequest {
                            owner: owner.clone(),
                            mode: if request.mode == super::PromptMode::Background {
                                SessionMode::Background
                            } else if owner.is_root() {
                                SessionMode::Interactive
                            } else {
                                SessionMode::Child
                            },
                            workspace: request.workspace.clone(),
                            history: forge_provider_api::SessionHistory::Durable {
                                resume: request.provider_session_id.clone(),
                            },
                            configuration: forge_provider_api::SessionConfiguration {
                                model: model_selection(request),
                                access: request.access.clone(),
                                approval: request.approval_mode,
                                resources,
                            },
                            resource_lease: None,
                            tools,
                            tool_endpoint: None,
                            system_instructions: request.system_message(),
                            trace: self.trace.clone(),
                        },
                    ),
                )
                .await?;
                let forge_provider_api::OpenedSession {
                    binding,
                    capabilities,
                    events,
                } = opened;
                {
                    let mut observed = session
                        .observation
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    observed.capabilities = Some(capabilities.clone());
                    observed.identity = binding.provider_session_id.clone();
                }
                session
                    .sender
                    .send(Ok(HostEvent::host(ProviderEvent::CapabilitiesChanged {
                        scope: forge_provider_api::EventScope::Binding {
                            owner: owner.clone(),
                        },
                        capabilities,
                    })))
                    .map_err(|_| {
                        failure(
                            ProviderOperation::SessionOpen,
                            ProviderFailureKind::Transport,
                        )
                    })?;
                let retiring = Arc::new(std::sync::atomic::AtomicBool::new(
                    session.closing.load(std::sync::atomic::Ordering::Acquire),
                ));
                let retired = Arc::new(Mutex::new(false));
                let relay = tokio::spawn(relay_events(
                    self.native.clone(),
                    binding.id.clone(),
                    events,
                    session.clone(),
                    retiring.clone(),
                    retired.clone(),
                ));
                Ok::<BoundSession, ProviderError>(BoundSession {
                    binding,
                    relay: Mutex::new(Some(relay)),
                    retiring,
                    retired,
                })
            })
            .await?;
        if initialized
            .retiring
            .load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        Ok(initialized.binding.clone())
    }

    /// Submit the host's admitted input through the normalized provider interface.
    pub(crate) async fn input_submit(
        &self,
        request: ProviderRequest,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        let _dispatch_observation = request.dispatch.observe_worker();
        let _admission = self.admission.read().await;
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(ProviderError::PreparationFailed(Box::new(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            ))));
        }
        let owner = request.owner().map_err(|_| {
            ProviderError::PreparationFailed(Box::new(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            )))
        })?;
        let host = self.session(owner).await;
        let _preparation = host.preparation.lock().await;
        let submission = request.submission.clone();
        let prepared = self
            .prepare_dispatch(request, scope)
            .await
            .map_err(|error| ProviderError::PreparationFailed(Box::new(error)))?;
        let submission = submission.ok_or_else(|| {
            ProviderError::PreparationFailed(Box::new(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            )))
        })?;
        let NativeOperation {
            binding,
            request,
            context,
            flight: _flight,
        } = prepared.admit(submission)?;
        let expected = request.submission_id.clone();
        drop(_preparation);
        drop(_admission);
        let native_scope = request.scope.clone();
        let result = super::operation::observe(
            self.trace.clone(),
            ProviderOperation::InputSubmit,
            &context,
            native_scope.clone().into(),
            Some(binding.clone()),
            self.native
                .input_submit(context.clone(), &binding, *request),
        )
        .await
        .map_err(|error| input_rejection(ProviderOperation::InputSubmit, error));
        let accepted = match result {
            Ok(accepted) => accepted,
            Err(primary @ (ProviderError::NotConsumed | ProviderError::PreparationFailed(_))) => {
                drop(_flight);
                // Negative delivery evidence does not prove an allocated native scope has collected.
                return match self.collect_unconsumed_invocation(&native_scope).await {
                    Ok(()) => Err(primary),
                    Err(cleanup) => Err(ProviderError::Combined {
                        primary: Box::new(primary),
                        secondary: vec![cleanup],
                    }),
                };
            }
            Err(error) => return Err(error),
        };
        if accepted.submission_id != expected {
            return self.invalid_acceptance(&binding).await;
        }
        Ok(())
    }

    /// Complete all host preparation before a native operation can consume input.
    async fn prepare_dispatch(
        &self,
        request: ProviderRequest,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<PreparedOperation, ProviderError> {
        let owner = request.owner().map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            ))
        })?;
        if owner != scope.owner {
            return Err(forge_provider_api::lifecycle::LifecycleError::OwnershipMismatch.into());
        }
        let submission = request.submission.as_ref().ok_or_else(|| {
            failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            )
        })?;
        let invocation = request.invocation_snapshot();
        submission
            .persist(move |receipt| receipt.seal_request(invocation).map_err(Into::into))
            .await
            .map_err(super::submission::journal_failure)?;
        let prepared_attachments =
            prepare_attachments(request.input.attachments().to_vec()).await?;
        let input = request.input.normalized();
        let instructions = format!(
            "{}\n{}",
            request.system_message(),
            request.input.additional_instructions()
        );
        let mut recovered_inputs = Vec::with_capacity(request.recovered_input.len());
        for recovered in &request.recovered_input {
            recovered_inputs.push(forge_provider_api::RecoveredInput {
                input: recovered.normalized(),
                attachments: prepare_attachments(recovered.attachments().to_vec()).await?,
                instructions: recovered.additional_instructions().to_owned(),
            });
        }
        let binding = self.binding_under_admission(&request).await?;
        let host = self.session(owner.clone()).await;
        host.initialized.get().ok_or_else(|| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::SessionMissing,
            ))
        })?;

        let previous = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned();
        if let Some(previous) = previous {
            if previous.foreground_pending() {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Rejected,
                ));
            }
            // A settled attempt cannot authorize callbacks against its replacement's policy.
            previous.cancel();
        }
        let (sink, events) = forge_provider_api::events::channel(binding.id.clone(), scope.clone());
        let skills = if self
            .descriptor()
            .capability
            .integration
            .resource_target
            .is_some()
        {
            Some(
                self.permissions
                    .skills(Some(&self.root), &request, &self.descriptor)
                    .map_err(|_| {
                        failure(
                            ProviderOperation::InputSubmit,
                            ProviderFailureKind::Rejected,
                        )
                    })?,
            )
        } else {
            None
        };
        let sink = skills
            .as_ref()
            .and_then(|skills| skills.sink(Some(sink.clone())))
            .unwrap_or(sink);
        let mut context = request
            .control_context
            .clone()
            .unwrap_or_else(|| crate::tools::ControlTurnContext::inactive(request.mode));
        context.question_invocation = Some(scope.clone());
        context.bind_task_authority_progress();
        let directories = self
            .permissions
            .directories(Some(&self.root), &request, &scope)
            .map_err(|error| {
                super::lifecycle_error::classify(ProviderOperation::InputSubmit, error)
            })?;
        if let Some(lsp) = &context.lsp {
            let protected_path = self
                .permissions
                .store()
                .read()
                .map_err(|_| {
                    failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Rejected,
                    )
                })?
                .open()
                .0
                .into();
            lsp.bind_mutation_policy(directories.policy(), protected_path)
                .map_err(|error| {
                    super::lifecycle_error::classify(ProviderOperation::InputSubmit, error)
                })?;
        }
        let tools = crate::tools::ControlToolRegistry
            .definition_list_for_context(Some(&context), scope.owner.is_root());
        let permissions = Arc::new(
            crate::permissions::provider::ProviderPermissionContext::new(self.permissions.clone()),
        );
        permissions
            .configure(
                owner,
                request.approval_mode,
                request.workspace.clone(),
                Some(sink.clone()),
            )
            .map_err(|_| {
                ProviderError::Failure(ProviderFailure::new(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Rejected,
                ))
            })?;
        permissions.bind_context(Some(&context)).map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            ))
        })?;
        let endpoint = Arc::new(ControlSession::new(tools.clone()));
        endpoint.begin(context.clone(), sink).await;
        let server = ControlServer::start(endpoint.clone())
            .await
            .map_err(|_| failure(ProviderOperation::InputSubmit, ProviderFailureKind::Runtime))?;
        let tool_endpoint = HostToolEndpoint {
            url: server.url.clone(),
            headers: Default::default(),
        };
        let lease = Arc::new(EndpointLease { _server: server });
        let relay = super::host_invocation::HostEventRelay::start(
            binding.id.clone(),
            events,
            host.sender.clone(),
        );
        let effects = Arc::new(super::host_invocation::HostInvocation::new(
            binding.id.clone(),
            scope.clone(),
            &request,
            context.clone(),
            &tools,
            super::host_invocation::HostInvocationResources {
                provider_closed: self.closed.clone(),
                permissions: permissions.clone(),
                directories: Some(directories),
                endpoint: endpoint.clone(),
                endpoint_lease: lease.clone(),
                relay,
                skills,
            },
        ));
        let delegated: Arc<dyn forge_provider_api::control::ProviderTools> = effects.clone();
        endpoint.bind_host(scope.clone(), Arc::downgrade(&delegated));
        *host.request.lock().await = Some(request.clone());
        host.invocations
            .lock()
            .await
            .insert(scope.invocation_id.clone(), effects.clone());
        let configuration = forge_provider_api::InvocationConfiguration {
            model: model_selection(&request),
            access: request.access.clone(),
            approval: request.approval_mode,
        };
        let receipt = request.submission.ok_or_else(|| {
            failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            )
        })?;
        let native_receipt = receipt.receipt_for_attempt()?;
        let submission_id = native_receipt.id().clone();
        let attempt_id = native_receipt.attempt_id()?;
        let dispatch =
            receipt.prepare_dispatch(&request.dispatch, binding.id.clone(), scope.clone())?;
        Ok(PreparedOperation {
            binding: binding.id,
            effects: effects.clone(),
            request: Box::new(forge_provider_api::PromptRequest {
                configuration,
                tools,
                tool_endpoint: Some(tool_endpoint),
                resource_lease: Some(lease),
                scope,
                mode: request.mode,
                task: context.task_start.clone(),
                attempt_id,
                submission_id,
                dispatch,
                input,
                recovered_inputs,
                instructions,
                attachments: prepared_attachments,
                receipt: native_receipt,
                permissions,
                provider_tools: effects.clone(),
                turn_control: effects.clone(),
                host_effects: effects,
            }),
        })
    }

    /// Describe the selected implementation using only registry-owned normalized metadata.
    pub fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        self.descriptor.clone()
    }

    /// Adapt deterministic provider controls without exposing any native protocol.
    pub(crate) fn mock_response_control(
        &self,
    ) -> Option<super::mock_response::MockResponseControl> {
        self.test_control
            .clone()
            .map(super::mock_response::MockResponseControl::new)
    }

    /// Deliver independently journaled input to its original owner and invocation.
    pub(crate) async fn input_steer(
        &self,
        owner: &ExecutionOwner,
        scope: forge_provider_api::InvocationScope,
        input: super::SteeringInput,
    ) -> Result<(), ProviderError> {
        let _admission = self.admission.read().await;
        let (binding, request) =
            self.prepare_steering(owner, scope, input)
                .await
                .map_err(|error| {
                    ProviderError::PreparationFailed(Box::new(
                        error.for_operation(ProviderOperation::InputSteer),
                    ))
                })?;
        let expected = request.delivery.submission_id().cloned();
        let operation = OperationControl::fresh(ProviderOperation::InputSteer)?;
        let context = operation.context();
        let accepted = super::operation::observe(
            self.trace.clone(),
            ProviderOperation::InputSteer,
            &context,
            request.scope.clone().into(),
            Some(binding.clone()),
            self.native.input_steer(context.clone(), &binding, request),
        )
        .await
        .map_err(|error| input_rejection(ProviderOperation::InputSteer, error))?;
        let actual = match accepted {
            forge_provider_api::SteeringAcceptance::Journaled { submission_id } => {
                Some(submission_id)
            }
            forge_provider_api::SteeringAcceptance::Context => None,
        };
        if actual != expected {
            return self.invalid_acceptance(&binding).await;
        }
        Ok(())
    }

    /// Prepare steering without changing the original invocation or inventing receipt identities.
    async fn prepare_steering(
        &self,
        owner: &ExecutionOwner,
        scope: forge_provider_api::InvocationScope,
        input: super::SteeringInput,
    ) -> Result<(BindingId, forge_provider_api::SteeringRequest), ProviderError> {
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::InputSteer,
                ProviderFailureKind::Rejected,
            ));
        }
        if owner != &scope.owner {
            return Err(forge_provider_api::lifecycle::LifecycleError::OwnershipMismatch.into());
        }
        let session = self
            .sessions
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::InputSteer,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let native = session.initialized.get().ok_or_else(|| {
            failure(
                ProviderOperation::InputSteer,
                ProviderFailureKind::SessionMissing,
            )
        })?;
        let invocation = session
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned()
            .ok_or_else(|| failure(ProviderOperation::InputSteer, ProviderFailureKind::Rejected))?;
        let request = invocation.request_snapshot(&scope)?;
        let (delivery, normalized, attachments) = match input.submission() {
            Some(receipt) => {
                receipt
                    .persist(move |receipt| receipt.seal_request(request).map_err(Into::into))
                    .await
                    .map_err(super::submission::journal_failure)?;
                let record = receipt.record().map_err(|_| {
                    failure(
                        ProviderOperation::InputSteer,
                        ProviderFailureKind::Persistence,
                    )
                })?;
                let normalized = record.request.input.normalized();
                let attachments =
                    prepare_attachments(record.request.input.attachments().to_vec()).await?;
                (
                    forge_provider_api::SteeringDelivery::Journaled {
                        receipt: receipt.receipt_for_attempt()?,
                    },
                    normalized,
                    attachments,
                )
            }
            None if matches!(input, super::SteeringInput::AgentResult(_)) => (
                forge_provider_api::SteeringDelivery::Context,
                forge_provider_api::PromptInput::Text(input.text().to_owned()),
                Vec::new(),
            ),
            None => {
                return Err(failure(
                    ProviderOperation::InputSteer,
                    ProviderFailureKind::Rejected,
                ));
            }
        };
        Ok((
            native.binding.id.clone(),
            forge_provider_api::SteeringRequest {
                scope,
                delivery,
                input: normalized,
                attachments,
                task: input.task_start().cloned(),
            },
        ))
    }

    /// Fence a provider that acknowledges a different normalized input than was admitted.
    async fn invalid_acceptance(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let primary = ProviderError::Observation(
            forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
        );
        match self
            .native
            .session_close(
                OperationControl::fresh(ProviderOperation::SessionClose)?.context(),
                binding,
            )
            .await
        {
            Ok(()) => Err(primary),
            Err(cleanup) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![cleanup],
            }),
        }
    }

    /// Open or resume native history without creating an exchange.
    pub async fn session_open(
        &self,
        request: ProviderRequest,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
        Ok(self.binding_for(&request).await?.provider_session_id)
    }

    /// Inspect runtime discovery without changing a live binding.
    pub async fn inspect(
        &self,
        context: forge_provider_api::WorkspaceContext,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        self.ensure_admission(ProviderOperation::Inspect)?;
        self.native
            .inspect(
                OperationControl::fresh(ProviderOperation::Inspect)?.context(),
                context,
            )
            .await
    }

    /// Authenticate only through an explicitly advertised method.
    pub async fn authenticate(
        &self,
        context: forge_provider_api::WorkspaceContext,
        method: String,
    ) -> Result<(), ProviderError> {
        self.ensure_admission(ProviderOperation::Authenticate)?;
        let method = forge_provider_api::AuthenticationMethodId::new(method).map_err(|_| {
            failure(
                ProviderOperation::Authenticate,
                ProviderFailureKind::Rejected,
            )
        })?;
        self.native
            .authenticate(
                OperationControl::fresh(ProviderOperation::Authenticate)?.context(),
                context,
                method,
            )
            .await
    }

    /// Read negotiated owner state, using registry baseline before a binding exists.
    pub async fn capabilities(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::CapabilityManifest, ProviderError> {
        let owner = request
            .owner()
            .map_err(|_| failure(ProviderOperation::Inspect, ProviderFailureKind::Rejected))?;
        let session = self.sessions.lock().await.get(&owner).cloned();
        Ok(session
            .and_then(|host| {
                host.observation
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .capabilities
                    .clone()
            })
            .unwrap_or_else(|| {
                forge_provider_api::CapabilityManifest::from_descriptor(self.descriptor())
            }))
    }

    /// Select discovery context without opening or reconfiguring a session.
    pub async fn query_context(
        &self,
        request: &super::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::QueryContext, ProviderError> {
        self.ensure_admission(ProviderOperation::Inspect)?;
        let owner = request
            .owner()
            .map_err(|_| failure(ProviderOperation::Inspect, ProviderFailureKind::Rejected))?;
        let sessions = self.sessions.lock().await;
        Ok(
            match sessions.get(&owner).and_then(|host| host.initialized.get()) {
                Some(bound) if !bound.retiring.load(std::sync::atomic::Ordering::Acquire) => {
                    forge_provider_api::QueryContext::Binding(bound.binding.id.clone())
                }
                Some(_) => {
                    return Err(failure(
                        ProviderOperation::Inspect,
                        ProviderFailureKind::BindingClosed,
                    ));
                }
                None => forge_provider_api::QueryContext::Workspace(
                    forge_provider_api::WorkspaceContext {
                        workspace: request.workspace.clone(),
                    },
                ),
            },
        )
    }

    /// Apply host-selected resources through the normalized configuration boundary.
    pub(crate) async fn session_configure(
        &self,
        desired: ProviderRequest,
        selection: &crate::settings::PortableSettings,
    ) -> Result<ConfigureReceipt, ProviderError> {
        self.ensure_admission(ProviderOperation::SessionConfigure)?;
        let request = catalog_request(&desired);
        let _admission = self.admission.write().await;
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::Rejected,
            ));
        }
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::Rejected,
            )
        })?;
        let host = self.sessions.lock().await.get(&owner).cloned();
        let Some(host) = host else {
            return Ok(ConfigureReceipt::Deferred);
        };
        if host.closing.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        // Initialization reads committed settings; no native configuration exists to mutate yet.
        let Some(bound) = host.initialized.get() else {
            return Ok(ConfigureReceipt::Deferred);
        };
        if bound.retiring.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let operation = OperationControl::fresh(ProviderOperation::SessionConfigure)?;
        let context = operation.context();
        let before_history = {
            let mut state = host.observation.lock().map_err(|_| {
                failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::Runtime,
                )
            })?;
            if state.closed {
                return Err(failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::BindingClosed,
                ));
            }
            state.last_configuration = Some(context.id().clone());
            state
                .identity
                .clone()
                .or_else(|| bound.binding.provider_session_id.clone())
        };
        let descriptor = self.descriptor();
        let resources = match descriptor.capability.integration.resource_target.as_deref() {
            Some(target) => crate::settings::SessionResources::load_with_selection(
                &self.root,
                &request,
                descriptor.kind.as_str(),
                target,
                Some(selection),
            )
            .map_err(|_| {
                failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::Rejected,
                )
            })?
            .selection(),
            None => Default::default(),
        };
        let configuration = forge_provider_api::SessionConfiguration {
            model: model_selection(&desired),
            access: desired.access.clone(),
            approval: request.approval_mode,
            resources,
        };
        let configured = super::operation::observe(
            self.trace.clone(),
            ProviderOperation::SessionConfigure,
            &context,
            forge_provider_api::EventScope::Binding {
                owner: owner.clone(),
            },
            Some(bound.binding.id.clone()),
            self.native
                .session_configure(context.clone(), &bound.binding.id, configuration),
        )
        .await?;
        self.wait_watermark(&owner, configured.watermark).await?;
        let after_history = {
            let mut state = host
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if state.capabilities.as_ref().is_none_or(|current| {
                current.effective_revision <= configured.capabilities.effective_revision
            }) {
                state.capabilities = Some(configured.capabilities.clone());
            }
            state
                .identity
                .clone()
                .or_else(|| bound.binding.provider_session_id.clone())
        };
        host.sender
            .send(Ok(HostEvent::host(ProviderEvent::CapabilitiesChanged {
                scope: forge_provider_api::EventScope::Binding {
                    owner: owner.clone(),
                },
                capabilities: configured.capabilities,
            })))
            .map_err(|_| {
                failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::Transport,
                )
            })?;
        *host.request.lock().await = Some(desired);
        let receipt = ConfiguredBindingReceipt {
            operation: context.id().clone(),
            owner: owner.clone(),
            binding: bound.binding.id.clone(),
            before_history,
            after_history,
            watermark: configured.watermark,
            host: Arc::downgrade(&host),
            provider_closed: Arc::downgrade(&self.closed),
        };
        receipt.validate(&owner)?;
        Ok(ConfigureReceipt::Applied(receipt))
    }

    /// Read model choices through a read-only workspace or exact-binding context.
    pub async fn model_list(
        &self,
        context: forge_provider_api::QueryContext,
    ) -> Result<Vec<super::ProviderModel>, ProviderError> {
        self.ensure_admission(ProviderOperation::ModelList)?;
        self.native
            .model_list(
                OperationControl::fresh(ProviderOperation::ModelList)?.context(),
                context,
            )
            .await
    }

    /// Read skill choices without changing enablement.
    pub async fn skill_list(
        &self,
        context: forge_provider_api::QueryContext,
    ) -> Result<Vec<super::SkillDefinition>, ProviderError> {
        self.ensure_admission(ProviderOperation::SkillList)?;
        self.native
            .skill_list(
                OperationControl::fresh(ProviderOperation::SkillList)?.context(),
                context,
            )
            .await
    }

    /// Read configured definitions or observed readiness without hidden configuration writes.
    pub async fn mcp_list(
        &self,
        query: forge_provider_api::McpQuery,
    ) -> Result<Vec<super::McpDefinition>, ProviderError> {
        self.ensure_admission(ProviderOperation::McpList)?;
        self.native
            .mcp_list(
                OperationControl::fresh(ProviderOperation::McpList)?.context(),
                query,
            )
            .await
    }

    /// Retain a read operation without delaying immediate native admission revocation.
    async fn observe_binding(
        &self,
        operation: ProviderOperation,
        owner: &ExecutionOwner,
    ) -> Result<(BindingId, HostObservation), ProviderError> {
        let host = self
            .sessions
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| failure(operation, ProviderFailureKind::SessionMissing))?;
        let bound = host
            .initialized
            .get()
            .ok_or_else(|| failure(operation, ProviderFailureKind::SessionMissing))?;
        let binding = bound.binding.id.clone();
        {
            let mut pending = host
                .observers
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if host.closing.load(std::sync::atomic::Ordering::Acquire)
                || bound.retiring.load(std::sync::atomic::Ordering::Acquire)
            {
                return Err(failure(operation, ProviderFailureKind::BindingClosed));
            }
            *pending = pending
                .checked_add(1)
                .ok_or_else(|| failure(operation, ProviderFailureKind::Runtime))?;
        }
        Ok((binding, HostObservation { host }))
    }

    /// Read terminals from an existing exact binding without opening a missing session.
    pub async fn terminal_list(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<super::terminal::TerminalSnapshot, ProviderError> {
        self.ensure_admission(ProviderOperation::TerminalList)?;
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::TerminalList,
                ProviderFailureKind::Rejected,
            )
        })?;
        let (binding, permit) = self
            .observe_binding(ProviderOperation::TerminalList, &owner)
            .await?;
        let native = self.native.clone();
        tokio::spawn(async move {
            let _permit = permit;
            native
                .terminal_list(
                    OperationControl::fresh(ProviderOperation::TerminalList)?.context(),
                    &binding,
                )
                .await
        })
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::TerminalList,
                ProviderFailureKind::Runtime,
            )
        })?
    }

    /// Stop an opaque terminal only in the explicitly selected live binding.
    pub async fn terminal_terminate(
        &self,
        request: super::ProviderCatalogRequest,
        terminal: &forge_provider_api::terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        self.ensure_admission(ProviderOperation::TerminalTerminate)?;
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::TerminalTerminate,
                ProviderFailureKind::Rejected,
            )
        })?;
        let (binding, permit) = self
            .observe_binding(ProviderOperation::TerminalTerminate, &owner)
            .await?;
        let native = self.native.clone();
        let terminal = terminal.clone();
        tokio::spawn(async move {
            let _permit = permit;
            native
                .terminal_terminate(
                    OperationControl::fresh(ProviderOperation::TerminalTerminate)?.context(),
                    &binding,
                    &terminal,
                )
                .await
        })
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::TerminalTerminate,
                ProviderFailureKind::Runtime,
            )
        })?
    }

    /// Fork native history without exposing native request or response fields.
    pub async fn session_fork(
        &self,
        request: super::ProviderForkRequest,
    ) -> Result<super::ProviderForkResult, ProviderError> {
        self.ensure_admission(ProviderOperation::SessionFork)?;
        let operation_id = forge_provider_api::OperationId::new(format!(
            "fork/{}",
            request.target_harness_session_id
        ))?;
        let owner = ExecutionOwner::root(request.target_harness_session_id.clone());
        let source = catalog_request(&request.source);
        let target_catalog = super::ProviderCatalogRequest {
            execution_owner: Some(owner.clone()),
            harness_session_id: request.target_harness_session_id,
            provider_session_id: None,
            ..source.clone()
        };
        let descriptor = self.descriptor();
        let resources = match descriptor.capability.integration.resource_target.as_deref() {
            Some(target) => crate::settings::SessionResources::load(
                &self.root,
                &target_catalog,
                descriptor.kind.as_str(),
                target,
            )
            .map_err(|_| {
                failure(
                    ProviderOperation::SessionFork,
                    ProviderFailureKind::Rejected,
                )
            })?
            .selection(),
            None => Default::default(),
        };
        let target = forge_provider_api::ForkTarget {
            owner,
            workspace: request.source.workspace.clone(),
            configuration: forge_provider_api::SessionConfiguration {
                model: model_selection(&request.source),
                access: request.source.access.clone(),
                approval: request.source.approval_mode,
                resources,
            },
            system_instructions: request.source.system_message(),
        };
        let checkpoint = request
            .checkpoint_id
            .map(forge_provider_api::CheckpointId::new)
            .transpose()
            .map_err(|_| {
                ProviderError::Failure(ProviderFailure::new(
                    ProviderOperation::SessionFork,
                    ProviderFailureKind::Rejected,
                ))
            })?;
        let source_owner = request.source.owner().map_err(|_| {
            failure(
                ProviderOperation::SessionFork,
                ProviderFailureKind::Rejected,
            )
        })?;
        let source = match checkpoint {
            Some(checkpoint) => forge_provider_api::ForkSource::Checkpoint {
                provider_session_id: request.source.provider_session_id.ok_or_else(|| {
                    failure(
                        ProviderOperation::SessionFork,
                        ProviderFailureKind::SessionMissing,
                    )
                })?,
                checkpoint,
            },
            None => {
                let host = self
                    .sessions
                    .lock()
                    .await
                    .get(&source_owner)
                    .cloned()
                    .ok_or_else(|| {
                        failure(
                            ProviderOperation::SessionFork,
                            ProviderFailureKind::SessionMissing,
                        )
                    })?;
                let bound = host.initialized.get().ok_or_else(|| {
                    failure(
                        ProviderOperation::SessionFork,
                        ProviderFailureKind::SessionMissing,
                    )
                })?;
                forge_provider_api::ForkSource::Binding(bound.binding.id.clone())
            }
        };
        let context = self.retained_context(ProviderOperation::SessionFork, operation_id)?;
        super::operation::observe(
            self.trace.clone(),
            ProviderOperation::SessionFork,
            &context,
            forge_provider_api::EventScope::SessionOperation {
                owner: target.owner.clone(),
                operation_id: context.id().clone(),
            },
            None,
            self.native.session_fork(
                context.clone(),
                forge_provider_api::SessionForkRequest { source, target },
            ),
        )
        .await
    }

    /// Run bounded isolated generation through the shared retained utility lifecycle.
    pub async fn generate_text(
        &self,
        request: super::ProviderCatalogRequest,
        purpose: super::TextGeneration,
        model: &str,
        reasoning: Option<&str>,
        history: &str,
    ) -> Result<String, ProviderError> {
        self.ensure_admission(ProviderOperation::SessionOpen)?;
        self.utility
            .generate(
                self.native.clone(),
                self.trace.clone(),
                forge_provider_api::TextGenerationRequest {
                    workspace: request.workspace,
                    model: model.to_owned(),
                    reasoning: reasoning
                        .map_or(forge_provider_api::ReasoningSelection::Default, |value| {
                            forge_provider_api::ReasoningSelection::Level(value.to_owned())
                        }),
                    instructions: purpose.instructions().to_owned(),
                    input: purpose.input(history),
                    validator: Arc::new(purpose),
                },
            )
            .await
    }

    /// Read durable identity already published by open or an ordered binding observation.
    pub async fn retained_session_id(
        &self,
        owner: &ExecutionOwner,
    ) -> Option<forge_provider_api::ProviderSessionId> {
        self.sessions.lock().await.get(owner).and_then(|host| {
            host.observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .identity
                .clone()
        })
    }

    /// Retract the exact checked cancelled submission without selecting current history.
    pub async fn input_retract(
        &self,
        context: forge_provider_api::OperationContext,
        owner: &ExecutionOwner,
        request: forge_provider_api::InputRetractRequest,
    ) -> Result<forge_provider_api::InputRetractOutcome, ProviderError> {
        let host = self
            .sessions
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::InputRetract,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let bound = host.initialized.get().ok_or_else(|| {
            failure(
                ProviderOperation::InputRetract,
                ProviderFailureKind::SessionMissing,
            )
        })?;
        super::operation::observe(
            self.trace.clone(),
            ProviderOperation::InputRetract,
            &context,
            request.scope.clone().into(),
            Some(bound.binding.id.clone()),
            self.native
                .input_retract(context.clone(), &bound.binding.id, request),
        )
        .await
    }

    /// Return normalized compaction evidence without reconstructing native notifications.
    pub async fn session_compact(
        &self,
        request: super::ProviderCompactionRequest,
    ) -> Result<forge_provider_api::Compaction, ProviderError> {
        let _admission = self.admission.read().await;
        let binding = self.binding_under_admission(&request.request).await?;
        let owner = request.request.owner().map_err(|_| {
            failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            )
        })?;
        let scope = forge_provider_api::EventScope::SessionOperation {
            owner,
            operation_id: forge_provider_api::OperationId::new(request.id.clone())?,
        };
        let context = self.retained_context(
            ProviderOperation::SessionCompact,
            forge_provider_api::OperationId::new(request.id)?,
        )?;
        let result = super::operation::observe(
            self.trace.clone(),
            ProviderOperation::SessionCompact,
            &context,
            scope.clone(),
            Some(binding.id.clone()),
            self.native.session_compact(
                context.clone(),
                &binding.id,
                forge_provider_api::CompactionRequest {
                    scope: scope.clone(),
                },
            ),
        )
        .await?;
        self.wait_watermark(&scope.owner().clone(), result.watermark)
            .await?;
        Ok(result.compaction)
    }

    /// Cancel only the identified invocation and collect its host-owned effects separately.
    pub async fn cancel_invocation(
        &self,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<forge_provider_api::InvocationSettlement, ProviderError> {
        let host = self
            .sessions
            .lock()
            .await
            .get(&scope.owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::Cancel,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let bound = host.initialized.get().ok_or_else(|| {
            failure(
                ProviderOperation::Cancel,
                ProviderFailureKind::SessionMissing,
            )
        })?;
        let invocation = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned();
        if let Some(invocation) = &invocation {
            invocation.request_snapshot(&scope)?;
            if invocation.foreground_pending() {
                invocation.cancel();
            }
        }
        let context = OperationControl::fresh(ProviderOperation::Cancel)?.context();
        let native = super::operation::observe(
            self.trace.clone(),
            ProviderOperation::Cancel,
            &context,
            scope.clone().into(),
            Some(bound.binding.id.clone()),
            self.native.cancel(
                context.clone(),
                &bound.binding.id,
                forge_provider_api::CancelRequest {
                    scope: scope.clone(),
                },
            ),
        )
        .await;
        let host_collection = collect_selected_invocations(invocation.into_iter().collect()).await;
        match (native, host_collection) {
            (Ok(settlement), Ok(())) => {
                self.wait_watermark(&scope.owner, settlement.watermark)
                    .await?;
                Ok(settlement)
            }
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
            (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
        }
    }

    /// Collect adapter preparation after the journal positively prevented this exact input's send.
    /// Ignore only an exact WrongInvocation rejection: it proves no retained native invocation
    /// was installed. Other failures retain uncertainty and cannot authorize a replacement.
    pub(crate) async fn cancel_prepared_invocation(
        &self,
        scope: &forge_provider_api::InvocationScope,
        retirement: super::submission::SubmissionRetirement,
    ) -> Result<(), ProviderError> {
        match retirement {
            super::submission::SubmissionRetirement::Prevented
            | super::submission::SubmissionRetirement::AlreadyCancelled => {}
            super::submission::SubmissionRetirement::Sending
            | super::submission::SubmissionRetirement::Accepted => {
                return Err(ProviderError::DeliveryUncertain);
            }
        }
        self.collect_unconsumed_invocation(scope).await
    }

    /// Join an exact native rejection or positively prevented input before releasing its host scope.
    /// Call only after positive non-consumption proof; an uncertain send requires reconciliation.
    async fn collect_unconsumed_invocation(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        let host = self.sessions.lock().await.get(&scope.owner).cloned();
        let Some(host) = host else {
            return Ok(());
        };
        let invocation = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned();
        let native_input = if let Some(invocation) = invocation {
            invocation.request_snapshot(scope)?;
            invocation.cancel()
        } else {
            super::host_invocation::NativeInputAdmission::Preparing
        };
        let result = if let (super::host_invocation::NativeInputAdmission::Requested, Some(bound)) =
            (native_input, host.initialized.get())
        {
            match self
                .native
                .cancel(
                    OperationControl::fresh(ProviderOperation::Cancel)?.context(),
                    &bound.binding.id,
                    forge_provider_api::CancelRequest {
                        scope: scope.clone(),
                    },
                )
                .await
            {
                Ok(settlement)
                    if settlement.binding_id == bound.binding.id && settlement.scope == *scope =>
                {
                    self.wait_watermark(&scope.owner, settlement.watermark)
                        .await
                }
                Ok(_) => Err(ProviderError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                )),
                Err(ProviderError::Failure(failure))
                    if failure.operation() == ProviderOperation::Cancel
                        && failure.kind() == ProviderFailureKind::WrongInvocation =>
                {
                    Ok(())
                }
                Err(error) => Err(error),
            }
        } else {
            // Cancellation atomically excluded native admission; only host preparation can remain.
            Ok(())
        };
        let collection = self.collect_host_effects(scope).await;
        self.trace.record(scope.owner.session_id.as_str(), "provider.preparation.collected", serde_json::json!({
            "scope":scope,
            "native_requested":matches!(native_input, super::host_invocation::NativeInputAdmission::Requested),
            "native_collected":result.is_ok(), "host_collected":collection.is_ok(),
        }));
        match (result, collection) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
            (Err(error), Ok(())) | (Ok(()), Err(error)) => Err(error),
        }
    }

    /// Collect only host-owned jobs after provider settlement; never issue a native cleanup request.
    pub async fn collect_host_effects(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        let Some(host) = self.sessions.lock().await.get(&scope.owner).cloned() else {
            return Ok(());
        };
        let invocation = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned();
        if let Some(invocation) = &invocation {
            invocation.request_snapshot(scope)?;
        }
        collect_selected_invocations(invocation.into_iter().collect()).await
    }

    /// Close the exact retained owner without exposing a separate fence operation.
    pub async fn session_close_owner(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        if let Some(host) = self.sessions.lock().await.get(owner).cloned() {
            host.observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .last_configuration = None;
            host.closing
                .store(true, std::sync::atomic::Ordering::Release);
            host.open_operation.cancel();
            if let Some(bound) = host.initialized.get() {
                bound
                    .retiring
                    .store(true, std::sync::atomic::Ordering::Release);
            }
            for invocation in host.invocations.lock().await.values() {
                invocation.cancel();
            }
        }
        let _admission = self.admission.write().await;
        self.retire_owner(owner).await
    }

    /// Collect all bindings and preserve every distinct cleanup failure.
    pub async fn shutdown(&self) -> Result<(), ProviderError> {
        self.closed
            .store(true, std::sync::atomic::Ordering::Release);
        // Revoke pending opens before waiting for their shared admission guards.
        let pending = self
            .sessions
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        for session in pending {
            session
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .last_configuration = None;
            session
                .closing
                .store(true, std::sync::atomic::Ordering::Release);
            session.open_operation.cancel();
            if let Some(bound) = session.initialized.get() {
                bound
                    .retiring
                    .store(true, std::sync::atomic::Ordering::Release);
            }
        }
        let _admission = self.admission.write().await;
        let owners = self
            .sessions
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        let mut errors = Vec::new();
        for owner in owners {
            if let Err(error) = self.retire_owner(&owner).await {
                errors.push(error);
            }
        }
        if let Err(error) = self.utility.shutdown().await {
            errors.push(error);
        }
        if let Err(error) = self
            .native
            .shutdown(OperationControl::fresh(ProviderOperation::Shutdown)?.context())
            .await
        {
            errors.push(error);
        }
        combined(errors)
    }

    /// Collect the exact retained reader before allowing another channel for this owner.
    async fn retire_owner(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        let session = self.sessions.lock().await.get(owner).cloned();
        let Some(session) = session else {
            return Ok(());
        };
        session
            .closing
            .store(true, std::sync::atomic::Ordering::Release);
        session
            .observation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .last_configuration = None;
        for invocation in session.invocations.lock().await.values() {
            invocation.cancel();
        }
        let mut errors = Vec::new();
        if let Some(native) = session.initialized.get() {
            native
                .retiring
                .store(true, std::sync::atomic::Ordering::Release);
            {
                let mut retired = native.retired.lock().await;
                if !*retired {
                    let context =
                        OperationControl::fresh(ProviderOperation::SessionClose)?.context();
                    match super::operation::observe(
                        self.trace.clone(),
                        ProviderOperation::SessionClose,
                        &context,
                        forge_provider_api::EventScope::Binding {
                            owner: owner.clone(),
                        },
                        Some(native.binding.id.clone()),
                        self.native
                            .session_close(context.clone(), &native.binding.id),
                    )
                    .await
                    {
                        Ok(()) => *retired = true,
                        Err(error) => errors.push(error),
                    }
                }
            }
            let native_collected = *native.retired.lock().await;
            if let Err(error) = collect_invocations(&session).await {
                errors.push(error);
            }
            if native_collected
                && tokio::time::timeout(std::time::Duration::from_secs(15), async {
                    loop {
                        let changed = session.changed.notified();
                        tokio::pin!(changed);
                        changed.as_mut().enable();
                        if *session
                            .observers
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            == 0
                        {
                            break;
                        }
                        changed.await;
                    }
                })
                .await
                .is_err()
            {
                errors.push(
                    ProviderFailure::new(
                        ProviderOperation::SessionClose,
                        ProviderFailureKind::Timeout,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Collection,
                        forge_provider_api::FailureDisposition::Invalidated,
                    )
                    .into(),
                );
            }
            let mut relay = native.relay.lock().await;
            if let Some(task) = relay.as_mut()
                && (native_collected || task.is_finished())
            {
                match tokio::time::timeout(std::time::Duration::from_secs(15), task).await {
                    Ok(result) => {
                        match result {
                            Ok(Ok(())) => {}
                            Ok(Err(error)) => errors.push(error),
                            Err(_) => errors.push(
                                ProviderFailure::new(
                                    ProviderOperation::SessionClose,
                                    ProviderFailureKind::Runtime,
                                )
                                .at_phase(
                                    forge_provider_api::ProviderPhase::Collection,
                                    forge_provider_api::FailureDisposition::Invalidated,
                                )
                                .into(),
                            ),
                        }
                        relay.take();
                    }
                    Err(_) => errors.push(failure(
                        ProviderOperation::SessionClose,
                        ProviderFailureKind::Transport,
                    )),
                }
            }
        }
        // Failed native collection still owns its producer and reader for the next close attempt.
        combined(errors)?;
        let mut errors = Vec::new();
        // Closing the native reader may leave a valid published prefix queued for the owner.
        // Keep the binding routable until that exact prefix has been consumed.
        let watermark = session
            .observation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .published;
        match tokio::time::timeout(
            std::time::Duration::from_secs(15),
            self.wait_watermark(owner, watermark),
        )
        .await
        {
            Ok(Ok(())) => {}
            Ok(Err(error)) => errors.push(error),
            Err(_) => errors.push(
                ProviderFailure::new(
                    ProviderOperation::SessionClose,
                    ProviderFailureKind::Timeout,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Collection,
                    forge_provider_api::FailureDisposition::Invalidated,
                )
                .into(),
            ),
        }
        combined(errors)?;
        session.invocations.lock().await.clear();
        self.sessions.lock().await.remove(owner);
        Ok(())
    }

    /// Observe durable delivery evidence without admitting a new input or mutating its journal.
    pub(crate) async fn input_reconcile(
        &self,
        request: forge_provider_api::InputReconcileRequest,
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        self.ensure_admission(ProviderOperation::InputReconcile)?;
        self.native
            .input_reconcile(
                OperationControl::fresh(ProviderOperation::InputReconcile)?.context(),
                request,
            )
            .await
    }

    /// Commit owner-consumed stream progress and apply ordered binding metadata.
    pub(crate) async fn event_consumed(
        &self,
        binding: &BindingId,
        sequence: forge_provider_api::EventSequence,
    ) -> Result<(), ProviderError> {
        #[cfg(test)]
        self.consumption_entered.notify_waiters();
        let sessions = self.sessions.lock().await;
        let host = sessions
            .values()
            .find(|host| {
                host.initialized
                    .get()
                    .is_some_and(|bound| &bound.binding.id == binding)
            })
            .ok_or(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
            ))?;
        let mut state = host
            .observation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if sequence > state.published {
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Protocol,
            ));
        }
        if sequence <= state.consumed {
            return Ok(());
        }
        while state
            .pending
            .first_key_value()
            .is_some_and(|(position, _)| *position <= sequence)
        {
            let (_, event) = state
                .pending
                .pop_first()
                .expect("the checked pending prefix contains an event");
            match event {
                ProviderEvent::CapabilitiesChanged { capabilities, .. } => {
                    state.capabilities = Some(capabilities)
                }
                ProviderEvent::SessionIdentity {
                    provider_session_id,
                    ..
                } => state.identity = Some(provider_session_id),
                ProviderEvent::Settled { settlement } => {
                    state
                        .settlements
                        .insert(settlement.scope.invocation_id.clone(), *settlement);
                }
                _ => {}
            }
        }
        state.consumed = sequence;
        drop(state);
        host.changed.notify_waiters();
        Ok(())
    }

    /// Wait for authoritative event consumption through an operation's publication boundary.
    pub(crate) async fn wait_watermark(
        &self,
        owner: &ExecutionOwner,
        watermark: forge_provider_api::EventSequence,
    ) -> Result<(), ProviderError> {
        let host = self
            .sessions
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::SessionClose,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        loop {
            let notified = host.changed.notified();
            tokio::pin!(notified);
            notified.as_mut().enable();
            {
                let state = host
                    .observation
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                if state.consumed >= watermark {
                    return Ok(());
                }
                if state.closed && state.published < watermark {
                    return Err(ProviderError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                    ));
                }
            }
            notified.await;
        }
    }

    /// Reject new native work as soon as host shutdown closes its global admission gate.
    fn ensure_admission(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(
                ProviderFailure::new(operation, ProviderFailureKind::BindingClosed)
                    .at_phase(
                        forge_provider_api::ProviderPhase::Admission,
                        forge_provider_api::FailureDisposition::Invalidated,
                    )
                    .into(),
            );
        }
        Ok(())
    }

    /// Reuse an operation's original deadline and cancellation authority across retained retries.
    fn retained_context(
        &self,
        operation: ProviderOperation,
        id: forge_provider_api::OperationId,
    ) -> Result<forge_provider_api::OperationContext, ProviderError> {
        use std::collections::hash_map::Entry;
        let mut operations = self
            .retained_operations
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let control = match operations.entry(id.clone()) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(OperationControl::new(operation, id)?),
        };
        if control.operation() != operation {
            return Err(failure(operation, ProviderFailureKind::OperationConflict));
        }
        Ok(control.context())
    }

    /// Allocate only host delivery state; native initialization remains explicit.
    async fn session(&self, owner: ExecutionOwner) -> Arc<HostSession> {
        self.sessions
            .lock()
            .await
            .entry(owner)
            .or_insert_with(|| {
                let (sender, receiver) = mpsc::unbounded_channel();
                Arc::new(HostSession {
                    initialized: OnceCell::new(),
                    open_operation: OperationControl::fresh(ProviderOperation::SessionOpen)
                        .expect("fixed host operation deadline fits the runtime clock"),
                    sender,
                    preparation: Mutex::new(()),
                    closing: std::sync::atomic::AtomicBool::new(false),
                    receiver: Mutex::new(Some(HostEventStream { receiver })),
                    invocations: Mutex::new(HashMap::new()),
                    request: Mutex::new(None),
                    observation: Default::default(),
                    changed: Default::default(),
                    observers: Default::default(),
                })
            })
            .clone()
    }
}

/// Convert only host-owned catalog context into its normalized request.
fn catalog_request(request: &ProviderRequest) -> super::ProviderCatalogRequest {
    super::ProviderCatalogRequest {
        execution_owner: request.execution_owner.clone(),
        harness_session_id: request.harness_session_id.clone(),
        workspace: request.workspace.clone(),
        access: request.access.clone(),
        approval_mode: request.approval_mode,
        provider_session_id: request.provider_session_id.clone(),
    }
}

/// Keep open catalog identifiers together with typed reasoning and service choices.
fn model_selection(request: &ProviderRequest) -> forge_provider_api::ModelSelection {
    forge_provider_api::ModelSelection {
        model: (request.model != "default").then(|| request.model.clone()),
        reasoning: if request.effort == "default" {
            forge_provider_api::ReasoningSelection::Default
        } else {
            forge_provider_api::ReasoningSelection::Level(request.effort.clone())
        },
        context_window: request.context_window.clone(),
        service_tier: request.service_tier,
    }
}

/// Resolve admitted attachments on the blocking pool before any native dispatch.
async fn prepare_attachments(
    attachments: Vec<crate::attachment::InputAttachment>,
) -> Result<Vec<forge_provider_api::FileAttachment>, ProviderError> {
    tokio::task::spawn_blocking(move || {
        attachments
            .iter()
            .map(crate::attachment::InputAttachment::prepare)
            .collect::<Result<Vec<_>, crate::attachment::AttachmentPreparationError>>()
    })
    .await
    .map_err(|_| failure(ProviderOperation::InputSubmit, ProviderFailureKind::Runtime))?
    .map_err(|_| {
        failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Rejected,
        )
    })
}

/// Keep host failures typed and independent of native diagnostics.
fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Preserve positive native admission evidence in the owner's input-delivery result vocabulary.
fn input_rejection(operation: ProviderOperation, error: ProviderError) -> ProviderError {
    if matches!(&error, ProviderError::Failure(failure)
        if failure.operation() == operation
            && matches!(failure.phase(), forge_provider_api::ProviderPhase::Admission | forge_provider_api::ProviderPhase::Preparation)
            && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
    {
        return ProviderError::PreparationFailed(Box::new(error));
    }
    error
}

/// Check binding identity before forwarding an event without reinterpreting its lifecycle.
async fn relay_events(
    native: Arc<dyn NativeProvider>,
    binding: BindingId,
    mut events: ProviderEventStream,
    host: Arc<HostSession>,
    retiring: Arc<std::sync::atomic::AtomicBool>,
    retired: Arc<Mutex<bool>>,
) -> Result<(), ProviderError> {
    struct ClosedObservation(Arc<HostSession>);
    impl Drop for ClosedObservation {
        fn drop(&mut self) {
            let mut state = self
                .0
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            state.closed = true;
            state.last_configuration = None;
            drop(state);
            self.0.changed.notify_waiters();
        }
    }
    let _closed = ClosedObservation(host.clone());
    let sender = &host.sender;
    loop {
        match events.recv().await {
            Ok(Some(observation)) if observation.binding_id == binding => {
                {
                    let mut state = host
                        .observation
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    if observation.sequence <= state.published {
                        let error = EventDeliveryError::InvalidTransportData;
                        let _ = sender.send(Err(error.clone()));
                        return Err(error.into());
                    }
                    state.published = observation.sequence;
                    if matches!(
                        &observation.event,
                        ProviderEvent::CapabilitiesChanged { .. }
                            | ProviderEvent::SessionIdentity { .. }
                            | ProviderEvent::Settled { .. }
                    ) {
                        state
                            .pending
                            .insert(observation.sequence, observation.event.clone());
                    }
                }
                let invocation = if let Some(scope) = observation.event.scope().invocation() {
                    host.invocations
                        .lock()
                        .await
                        .get(&scope.invocation_id)
                        .cloned()
                } else {
                    None
                };
                let additional = match invocation {
                    Some(invocation) => invocation.observe(&observation.event),
                    None => Ok(Vec::new()),
                };
                let additional = match additional {
                    Ok(events) => events,
                    Err(error) => {
                        let _ = sender.send(Err(error.clone()));
                        return retire_failed_observer(
                            &native,
                            &binding,
                            &retired,
                            &retiring,
                            error.into(),
                        )
                        .await;
                    }
                };
                if sender
                    .send(Ok(HostEvent {
                        event: observation.event,
                        native: Some((binding.clone(), observation.sequence)),
                    }))
                    .is_err()
                {
                    return retire_failed_observer(
                        &native,
                        &binding,
                        &retired,
                        &retiring,
                        EventDeliveryError::Closed.into(),
                    )
                    .await;
                }
                for event in additional {
                    if sender.send(Ok(HostEvent::host(event))).is_err() {
                        return retire_failed_observer(
                            &native,
                            &binding,
                            &retired,
                            &retiring,
                            EventDeliveryError::Closed.into(),
                        )
                        .await;
                    }
                }
            }
            Ok(Some(_)) => {
                let failure = forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict;
                let error = EventDeliveryError::Observation(failure);
                let _ = sender.send(Err(error.clone()));
                return retire_failed_observer(
                    &native,
                    &binding,
                    &retired,
                    &retiring,
                    error.into(),
                )
                .await;
            }
            Ok(None) if retiring.load(std::sync::atomic::Ordering::Acquire) => return Ok(()),
            Ok(None) => {
                let error = EventDeliveryError::Closed;
                let _ = sender.send(Err(error.clone()));
                return retire_failed_observer(
                    &native,
                    &binding,
                    &retired,
                    &retiring,
                    error.into(),
                )
                .await;
            }
            Err(EventDeliveryError::Closed)
                if retiring.load(std::sync::atomic::Ordering::Acquire) =>
            {
                return Ok(());
            }
            Err(error) => {
                let _ = sender.send(Err(error.clone()));
                return retire_failed_observer(
                    &native,
                    &binding,
                    &retired,
                    &retiring,
                    error.into(),
                )
                .await;
            }
        }
    }
}

/// Always attempt collection after fencing, retaining the original observation failure.
async fn retire_failed_observer(
    native: &Arc<dyn NativeProvider>,
    binding: &BindingId,
    retired: &Mutex<bool>,
    retiring: &std::sync::atomic::AtomicBool,
    primary: ProviderError,
) -> Result<(), ProviderError> {
    retiring.store(true, std::sync::atomic::Ordering::Release);
    let mut errors = Vec::new();
    let mut retired = retired.lock().await;
    if !*retired {
        match native
            .session_close(
                OperationControl::fresh(ProviderOperation::SessionClose)?.context(),
                binding,
            )
            .await
        {
            Ok(()) => *retired = true,
            Err(error) => errors.push(error),
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: errors,
        })
    }
}

/// Join retained host event producers before forgetting their invocation authority.
async fn collect_invocations(session: &HostSession) -> Result<(), ProviderError> {
    let invocations = session
        .invocations
        .lock()
        .await
        .values()
        .cloned()
        .collect::<Vec<_>>();
    collect_selected_invocations(invocations).await
}

/// Bound collection to the invocations whose foreground lifetime is being retired.
async fn collect_selected_invocations(
    invocations: Vec<Arc<super::host_invocation::HostInvocation>>,
) -> Result<(), ProviderError> {
    let mut errors = Vec::new();
    for invocation in invocations {
        match tokio::time::timeout(std::time::Duration::from_secs(15), invocation.collect()).await {
            Ok(Ok(())) => {}
            Ok(Err(_)) => errors.push(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Unknown,
            )),
            Err(_) => errors.push(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Timeout,
            )),
        }
    }
    combined(errors)
}

/// Preserve the primary failure and every independent cleanup failure.
fn combined(errors: Vec<ProviderError>) -> Result<(), ProviderError> {
    let mut errors = errors.into_iter();
    match errors.next() {
        None => Ok(()),
        Some(primary) => {
            let secondary = errors.collect::<Vec<_>>();
            if secondary.is_empty() {
                Err(primary)
            } else {
                Err(ProviderError::Combined {
                    primary: Box::new(primary),
                    secondary,
                })
            }
        }
    }
}
