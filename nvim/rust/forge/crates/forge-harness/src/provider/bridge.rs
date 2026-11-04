use std::collections::HashMap;
use std::sync::Arc;

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

use super::{ProviderRequest, approval::PermissionCoordinator};

#[cfg(test)]
#[path = "bridge_test.rs"]
mod tests;

/// Prepare host resources and consume normalized provider observations for each owner.
pub struct ProviderBridge {
    native: Arc<dyn NativeProvider>,
    permissions: Arc<PermissionCoordinator>,
    trace: Arc<crate::trace::TraceStore>,
    root: std::path::PathBuf,
    sessions: Mutex<HashMap<ExecutionOwner, Arc<HostSession>>>,
    admission: tokio::sync::RwLock<()>,
    closed: std::sync::atomic::AtomicBool,
}

struct HostSession {
    initialized: OnceCell<BoundSession>,
    preparation: Mutex<()>,
    sender: mpsc::UnboundedSender<Result<ProviderEvent, EventDeliveryError>>,
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

#[derive(serde::Serialize)]
#[serde(rename_all = "snake_case")]
enum CollectionPhase {
    RetiringCallbacks,
    CallbacksRetired,
    NativeCollectionStarted,
    NativeCollectionFinished,
    PublicationsCollected,
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

enum PreparedOperation {
    Prompt {
        binding: BindingId,
        request: Box<forge_provider_api::PromptRequest>,
    },
    Continue {
        binding: BindingId,
        request: Box<forge_provider_api::ContinuationRequest>,
    },
}

impl ResourceLease for EndpointLease {}

/// Keep host observation available before native initialization without inventing events.
pub(crate) struct HostEventStream {
    receiver: mpsc::UnboundedReceiver<Result<ProviderEvent, EventDeliveryError>>,
}

impl HostEventStream {
    /// Receive the next already-normalized event or its typed delivery failure.
    pub(crate) async fn recv(&mut self) -> Result<Option<ProviderEvent>, EventDeliveryError> {
        self.receiver.recv().await.transpose()
    }

    /// Drain one pending event without waiting for native work.
    pub(crate) fn try_recv(&mut self) -> Result<ProviderEvent, EventDeliveryError> {
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
        native: Arc<dyn NativeProvider>,
        permissions: Arc<PermissionCoordinator>,
        trace: Arc<crate::trace::TraceStore>,
        root: std::path::PathBuf,
    ) -> Self {
        Self {
            native,
            permissions,
            trace,
            root,
            sessions: Mutex::new(HashMap::new()),
            admission: tokio::sync::RwLock::new(()),
            closed: std::sync::atomic::AtomicBool::new(false),
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
                ProviderOperation::CollectEventReader,
                ProviderFailureKind::Rejected,
            ));
        }
        let session = self.session(owner).await;
        let result = session.receiver.lock().await.take();
        result.ok_or_else(|| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::CollectEventReader,
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

    /// Initialize while the caller retains admission against reset and shutdown.
    async fn binding_under_admission(
        &self,
        request: &ProviderRequest,
    ) -> Result<SessionBinding, ProviderError> {
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            )));
        }
        let owner = request.owner().map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            ))
        })?;
        let session = self.session(owner.clone()).await;
        let initialized = session
            .initialized
            .get_or_try_init(|| async {
                let descriptor = self.native.descriptor();
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
                            ProviderOperation::InitializeSession,
                            ProviderFailureKind::Rejected,
                        ))
                    })?
                    .selection(),
                    None => Default::default(),
                };
                let tools = crate::tools::ControlToolRegistry
                    .registration_definitions(&owner, request.mode);
                let binding = self
                    .native
                    .initialize_session(SessionRequest {
                        owner: owner.clone(),
                        mode: if request.mode == super::PromptMode::Background {
                            SessionMode::Background
                        } else if owner.is_root() {
                            SessionMode::Interactive
                        } else {
                            SessionMode::Child
                        },
                        workspace: request.workspace.clone(),
                        provider_session_id: request.provider_session_id.clone(),
                        model: model_selection(request),
                        access: request.access.clone(),
                        approval: request.approval_mode,
                        resources,
                        resource_lease: None,
                        tools,
                        tool_endpoint: None,
                        system_instructions: request.system_message(),
                        trace: self.trace.clone(),
                    })
                    .await?;
                let events = match self.native.take_session_events(&binding.id).await {
                    Ok(events) => events,
                    Err(primary) => {
                        return Err(match self.native.retire_session(&binding.id).await {
                            Ok(()) => primary,
                            Err(cleanup) => ProviderError::Combined {
                                primary: Box::new(primary),
                                secondary: vec![cleanup],
                            },
                        });
                    }
                };
                let retiring = Arc::new(std::sync::atomic::AtomicBool::new(false));
                let retired = Arc::new(Mutex::new(false));
                let relay = tokio::spawn(relay_events(
                    self.native.clone(),
                    binding.id.clone(),
                    events,
                    session.clone(),
                    retiring.clone(),
                    retired.clone(),
                ));
                Ok(BoundSession {
                    binding,
                    relay: Mutex::new(Some(relay)),
                    retiring,
                    retired,
                })
            })
            .await?;
        Ok(initialized.binding.clone())
    }

    /// Submit the host's admitted input through the normalized provider interface.
    pub(crate) async fn session_prompt(
        &self,
        request: ProviderRequest,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        let _dispatch_observation = request.dispatch.observe_worker();
        let _admission = self.admission.read().await;
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(ProviderError::PreparationFailed(Box::new(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ))));
        }
        let owner = request.owner().map_err(|_| {
            ProviderError::PreparationFailed(Box::new(failure(
                ProviderOperation::AdmitInput,
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
        match prepared {
            PreparedOperation::Prompt { binding, request } => {
                let submission = submission.ok_or_else(|| {
                    failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected)
                })?;
                let _flight = submission.begin_prompt().map_err(|error| {
                    super::lifecycle_error::classify(ProviderOperation::AdmitInput, error)
                })?;
                let expected = request.submission_id.clone();
                let accepted = self.native.session_prompt(&binding, *request).await?;
                if accepted.submission_id != expected {
                    return self.invalid_acceptance(&binding).await;
                }
            }
            PreparedOperation::Continue { binding, request } => {
                if !matches!(
                    self.native.continue_session(&binding, *request).await?,
                    forge_provider_api::ContinuationOutcome::ExecutionResumed
                ) {
                    return self.invalid_acceptance(&binding).await;
                }
            }
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
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ))
        })?;
        if owner != scope.owner {
            return Err(forge_provider_api::lifecycle::LifecycleError::OwnershipMismatch.into());
        }
        let continuation = request
            .control_context
            .as_ref()
            .and_then(|context| context.provider_continuation.clone());
        if continuation.is_none() && self.native.has_active_turn(&owner).await {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        if continuation.is_none() && request.submission.is_none() {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        if continuation.is_none() {
            let submission = request.submission.as_ref().ok_or_else(|| {
                failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected)
            })?;
            let invocation = request.invocation_snapshot();
            submission
                .persist(move |receipt| receipt.seal_request(invocation).map_err(Into::into))
                .await
                .map_err(super::submission::journal_failure)?;
        }
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
                ProviderOperation::InitializeSession,
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
                    ProviderOperation::AdmitInput,
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
                    .skills(Some(&self.root), &request, self.descriptor().kind)
                    .map_err(|_| {
                        failure(
                            ProviderOperation::HostEffectPublication,
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
                super::lifecycle_error::classify(ProviderOperation::AdmitInput, error)
            })?;
        if let Some(lsp) = &context.lsp {
            let protected_path = self
                .permissions
                .store()
                .read()
                .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))?
                .open()
                .0
                .into();
            lsp.bind_mutation_policy(directories.policy(), protected_path)
                .map_err(|error| {
                    super::lifecycle_error::classify(ProviderOperation::AdmitInput, error)
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
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ))
            })?;
        permissions.bind_context(Some(&context)).map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ))
        })?;
        let endpoint = Arc::new(ControlSession::new(tools.clone()));
        endpoint.begin(context.clone(), sink).await;
        let server = ControlServer::start(endpoint.clone())
            .await
            .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Runtime))?;
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
        if let Some(authorization) = continuation {
            let dispatch = request
                .dispatch
                .track_continuation(binding.id.clone(), scope.clone(), authorization.clone())
                .map_err(|_| {
                    failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol)
                })?;
            return Ok(PreparedOperation::Continue {
                binding: binding.id,
                request: Box::new(
                    forge_provider_api::ContinuationRequest::RetryableExecution {
                        scope,
                        authorization,
                        dispatch: Box::new(dispatch),
                        callbacks: Box::new(forge_provider_api::InvocationCallbacks {
                            tool_endpoint: Some(tool_endpoint),
                            resource_lease: Some(lease),
                            permissions,
                            provider_tools: effects.clone(),
                            turn_control: effects.clone(),
                            host_effects: effects,
                        }),
                    },
                ),
            });
        }
        let configuration = forge_provider_api::InvocationConfiguration {
            model: model_selection(&request),
            access: request.access.clone(),
            approval: request.approval_mode,
        };
        let receipt = request
            .submission
            .ok_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))?;
        let submission_id =
            forge_provider_api::submission::InputReceipt::id(receipt.as_ref()).clone();
        let dispatch =
            receipt.prepare_dispatch(&request.dispatch, binding.id.clone(), scope.clone())?;
        Ok(PreparedOperation::Prompt {
            binding: binding.id,
            request: Box::new(forge_provider_api::PromptRequest {
                configuration,
                tools,
                tool_endpoint: Some(tool_endpoint),
                resource_lease: Some(lease),
                scope,
                mode: request.mode,
                task: context.task_start.clone(),
                goal: context.native_goal.clone(),
                submission_id,
                dispatch,
                input,
                recovered_inputs,
                instructions,
                attachments: prepared_attachments,
                receipt,
                permissions,
                provider_tools: effects.clone(),
                turn_control: effects.clone(),
                host_effects: effects,
            }),
        })
    }

    /// Describe the selected implementation using only registry-owned normalized metadata.
    pub fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        self.native.descriptor()
    }

    /// Adapt deterministic provider controls without exposing any native protocol.
    pub(crate) fn mock_response_control(
        &self,
    ) -> Option<super::mock_response::MockResponseControl> {
        self.native
            .test_control()
            .map(super::mock_response::MockResponseControl::new)
    }

    /// Deliver independently journaled input to its original owner and invocation.
    pub(crate) async fn session_steer(
        &self,
        owner: &ExecutionOwner,
        scope: forge_provider_api::InvocationScope,
        input: super::SteeringInput,
    ) -> Result<(), ProviderError> {
        let _admission = self.admission.read().await;
        let (binding, request) = self
            .prepare_steering(owner, scope, input)
            .await
            .map_err(|error| ProviderError::PreparationFailed(Box::new(error)))?;
        let expected = request.delivery.submission_id().cloned();
        let accepted = self.native.session_steer(&binding, request).await?;
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
                ProviderOperation::AdmitInput,
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
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let native = session.initialized.get().ok_or_else(|| {
            failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::SessionMissing,
            )
        })?;
        let invocation = session
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned()
            .ok_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))?;
        let request = invocation.request_snapshot(&scope)?;
        let (delivery, normalized, attachments) = match input.submission() {
            Some(receipt) => {
                receipt
                    .persist(move |receipt| receipt.seal_request(request).map_err(Into::into))
                    .await
                    .map_err(super::submission::journal_failure)?;
                let record = receipt.record().map_err(|_| {
                    failure(
                        ProviderOperation::RecordInput,
                        ProviderFailureKind::Persistence,
                    )
                })?;
                let normalized = record.request.input.normalized();
                let attachments =
                    prepare_attachments(record.request.input.attachments().to_vec()).await?;
                (
                    forge_provider_api::SteeringDelivery::Journaled { receipt },
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
                    ProviderOperation::AdmitInput,
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
                goal: input.goal().cloned(),
            },
        ))
    }

    /// Fence a provider that acknowledges a different normalized input than was admitted.
    async fn invalid_acceptance(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let primary = ProviderError::Observation(
            forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
        );
        match self.native.fence_session(binding).await {
            Ok(()) => Err(primary),
            Err(cleanup) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![cleanup],
            }),
        }
    }

    /// Open or resume native history without creating an exchange.
    pub async fn initialize_session(
        &self,
        request: ProviderRequest,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
        Ok(self.binding_for(&request).await?.provider_session_id)
    }

    /// Inspect setup through the normalized discovery contract.
    pub async fn setup(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        self.native.setup(request).await
    }

    /// Select an advertised authentication method without inspecting native handshakes.
    pub async fn authenticate(
        &self,
        request: super::ProviderCatalogRequest,
        method: String,
    ) -> Result<(), ProviderError> {
        let method = forge_provider_api::AuthenticationMethodId::new(method).map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::Authenticate,
                ProviderFailureKind::Rejected,
            ))
        })?;
        self.native.authenticate(request, method).await
    }

    /// Query normalized capabilities independently of session initialization.
    pub async fn capabilities(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::capability::CapabilityManifest, ProviderError> {
        self.native.capabilities(request).await
    }

    /// Apply host-selected resources through the normalized configuration boundary.
    pub async fn reconcile_settings(
        &self,
        request: super::ProviderCatalogRequest,
        selection: &crate::settings::PortableSettings,
    ) -> Result<(), ProviderError> {
        let _admission = self.admission.write().await;
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            )
        })?;
        let initialized = self
            .sessions
            .lock()
            .await
            .get(&owner)
            .is_some_and(|session| session.initialized.get().is_some());
        // Initialization reads committed settings; no native configuration exists to mutate yet.
        if !initialized {
            return Ok(());
        }
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
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::Rejected,
                )
            })?
            .selection(),
            None => Default::default(),
        };
        let settings = forge_provider_api::SessionSettings {
            access: selection.access.clone(),
            approval: Some(request.approval_mode),
            model: None,
        };
        self.native
            .reconcile_settings(request, settings, resources)
            .await
    }

    /// List normalized models for the host's existing catalog request shape.
    pub async fn model_list(
        &self,
        request: ProviderRequest,
    ) -> Result<Vec<super::ProviderModel>, ProviderError> {
        self.native.model_list(catalog_request(&request)).await
    }

    /// List native skills after provider-owned decoding and ordering.
    pub async fn skill_list(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<Vec<super::SkillDefinition>, ProviderError> {
        self.native.skill_list(request).await
    }

    /// Apply one normalized skill mutation.
    pub async fn set_skill_enabled(
        &self,
        request: super::ProviderCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<super::CatalogMutation, ProviderError> {
        self.native.set_skill_enabled(request, name, enabled).await
    }

    /// List configured resources without exposing native launch formats.
    pub async fn mcp_configuration(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<Vec<super::McpDefinition>, ProviderError> {
        let descriptor = self.descriptor();
        if let Some(target) = descriptor.capability.integration.resource_target.as_deref() {
            return crate::settings::SessionResources::load(
                &self.root,
                &request,
                descriptor.kind.as_str(),
                target,
            )
            .map(|resources| resources.mcp)
            .map_err(|error| {
                super::lifecycle_error::classify(ProviderOperation::ReconcileSettings, error)
            });
        }
        self.native.mcp_configuration(request).await
    }

    /// Query resource readiness through its normalized catalog.
    pub async fn mcp_list(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<Vec<super::McpDefinition>, ProviderError> {
        if self
            .descriptor()
            .capability
            .integration
            .resource_target
            .is_none()
        {
            return self.native.mcp_list(request).await;
        }
        let configured = self.mcp_configuration(request.clone()).await?;
        if configured
            .iter()
            .all(|row| !row.enabled || row.status == super::McpStatus::Unavailable)
        {
            return Ok(configured);
        }
        let observed = self.native.mcp_list(request).await?;
        Ok(super::mcp::merge_configured(configured, observed))
    }

    /// Apply one normalized resource mutation.
    pub async fn set_mcp_enabled(
        &self,
        request: super::ProviderCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<super::CatalogMutation, ProviderError> {
        self.native.set_mcp_enabled(request, name, enabled).await
    }

    /// Admit observation atomically with native retirement and retain its exclusion until completion.
    async fn observe_binding(
        &self,
        owner: &ExecutionOwner,
    ) -> Result<tokio::sync::OwnedMutexGuard<bool>, ProviderError> {
        let session = self
            .sessions
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let binding = session.initialized.get().ok_or_else(|| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::SessionMissing,
            )
        })?;
        let permit = binding.retired.clone().lock_owned().await;
        if *permit || binding.retiring.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::SessionMissing,
            ));
        }
        Ok(permit)
    }

    /// Query normalized terminal state without parsing process or native item fields.
    pub async fn background_terminals(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<super::terminal::TerminalSnapshot, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            )
        })?;
        let permit = self.observe_binding(&owner).await?;
        let native = self.native.clone();
        tokio::spawn(async move {
            let _permit = permit;
            native.background_terminals(request).await
        })
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Runtime,
            )
        })?
    }

    /// Terminate the selected opaque terminal identity.
    pub async fn terminate_terminal(
        &self,
        request: super::ProviderCatalogRequest,
        terminal: &str,
    ) -> Result<(), ProviderError> {
        self.native.terminate_terminal(request, terminal).await
    }

    /// Preserve host task steering policy while delegating native support to its owner.
    pub fn supports_task_steering(&self, kind: forge_provider_api::input::TaskKind) -> bool {
        self.native.supports_task_steering(kind)
    }

    /// Fork native history without exposing native request or response fields.
    pub async fn fork(
        &self,
        request: super::ProviderForkRequest,
    ) -> Result<super::ProviderForkResult, ProviderError> {
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
                    ProviderOperation::InitializeSession,
                    ProviderFailureKind::Rejected,
                )
            })?
            .selection(),
            None => Default::default(),
        };
        let target = forge_provider_api::ForkTarget {
            owner,
            workspace: request.source.workspace.clone(),
            configuration: forge_provider_api::InvocationConfiguration {
                model: model_selection(&request.source),
                access: request.source.access.clone(),
                approval: request.source.approval_mode,
            },
            resources,
            system_instructions: request.source.system_message(),
        };
        let checkpoint = request
            .checkpoint_id
            .map(forge_provider_api::CheckpointId::new)
            .transpose()
            .map_err(|_| {
                ProviderError::Failure(ProviderFailure::new(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ))
            })?;
        self.native
            .fork(forge_provider_api::ConversationForkRequest {
                source,
                target,
                checkpoint,
            })
            .await
    }

    /// Render the host's isolated text contract without sharing persistent Background state.
    pub async fn generate_text(
        &self,
        request: super::ProviderCatalogRequest,
        purpose: super::TextGeneration,
        model: &str,
        reasoning: Option<&str>,
        history: &str,
    ) -> Result<String, ProviderError> {
        self.native
            .generate_text(forge_provider_api::TextGenerationRequest {
                workspace: request.workspace,
                model: model.to_owned(),
                reasoning: reasoning
                    .map_or(forge_provider_api::ReasoningSelection::Default, |value| {
                        forge_provider_api::ReasoningSelection::Level(value.to_owned())
                    }),
                instructions: purpose.instructions().to_owned(),
                input: purpose.input(history),
                validator: Arc::new(purpose),
            })
            .await
    }

    /// Resolve a host-owned runtime key without interpreting any native identity.
    async fn owner_for_key(&self, key: &str) -> Option<ExecutionOwner> {
        self.sessions
            .lock()
            .await
            .keys()
            .find(|owner| owner.runtime_key() == key)
            .cloned()
    }

    /// Check the native owner's admitted work for a known host session.
    pub async fn has_active_turn(&self, key: &str) -> bool {
        match self.owner_for_key(key).await {
            Some(owner) => self.native.has_active_turn(&owner).await,
            None => false,
        }
    }

    /// Prepare an existing owner for another invocation.
    pub async fn prepare_turn(&self, key: &str) -> Result<(), ProviderError> {
        if let Some(owner) = self.owner_for_key(key).await {
            self.native.prepare_turn(&owner).await?;
        }
        Ok(())
    }

    /// Request a normalized pause or clear of an existing host owner's goal.
    pub async fn stop_goal_session(&self, key: &str, clear: bool) -> Result<(), ProviderError> {
        if let Some(owner) = self.owner_for_key(key).await {
            self.native
                .stop_goal_session(
                    &owner,
                    if clear {
                        forge_provider_api::GoalStopMode::ClearConversation
                    } else {
                        forge_provider_api::GoalStopMode::RetainConversation
                    },
                )
                .await?;
        }
        Ok(())
    }

    /// Publish canonical goal state without constructing native goal configuration.
    pub async fn goal_status(
        &self,
        request: ProviderRequest,
        update: forge_provider_api::NativeGoalUpdate,
    ) -> Result<(), ProviderError> {
        let owner = request.owner().map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ))
        })?;
        self.native.goal_status(&owner, update).await
    }

    /// Read the normalized persisted identity for an existing host session.
    pub async fn active_session_id_for(
        &self,
        key: &str,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
        match self.owner_for_key(key).await {
            Some(owner) => self.native.active_session_id_for(&owner).await,
            None => Ok(None),
        }
    }

    /// Roll back only through the provider that owns the opaque conversation.
    pub async fn rollback_cancelled_turn_for(&self, key: &str) -> Result<bool, ProviderError> {
        let Some(owner) = self.owner_for_key(key).await else {
            return Ok(false);
        };
        let session = self.sessions.lock().await.get(&owner).cloned();
        match session
            .as_ref()
            .and_then(|session| session.initialized.get())
        {
            Some(native) => {
                self.native
                    .rollback_cancelled_turn(&native.binding.id)
                    .await
            }
            None => Ok(false),
        }
    }

    /// Return normalized compaction evidence without reconstructing native notifications.
    pub async fn compact(
        &self,
        request: super::ProviderCompactionRequest,
    ) -> Result<forge_provider_api::Compaction, ProviderError> {
        let _admission = self.admission.read().await;
        let binding = self.binding_under_admission(&request.request).await?;
        let owner = request
            .request
            .owner()
            .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))?;
        let scope = forge_provider_api::EventScope::SessionOperation {
            owner,
            operation_id: request.id.clone(),
        };
        self.native
            .compact(
                &binding.id,
                forge_provider_api::CompactionRequest {
                    operation_id: request.id,
                    scope: scope.clone(),
                },
            )
            .await
    }

    /// Cancel host callbacks before asking the native owner to collect its foreground work.
    pub async fn cancel_session(&self, key: &str) -> Result<(), ProviderError> {
        let Some(owner) = self.owner_for_key(key).await else {
            return Ok(());
        };
        let session = self.session(owner).await;
        for invocation in session.invocations.lock().await.values() {
            invocation.cancel();
        }
        let mut errors = Vec::new();
        if let Some(native) = session.initialized.get() {
            let retired = native.retired.lock().await;
            if !*retired && let Err(error) = self.native.cancel_session(&native.binding.id).await {
                errors.push(error);
            }
        }
        if let Err(error) = collect_invocations(&session).await {
            errors.push(error);
        }
        combined(errors)
    }

    /// Collect the current invocation through the normalized native cleanup operation.
    pub async fn cleanup_execution(&self, key: &str) -> Result<(), ProviderError> {
        let Some(owner) = self.owner_for_key(key).await else {
            return Ok(());
        };
        let session_id = owner.session_id.as_str().to_owned();
        let session = self.session(owner).await;
        self.trace.record(
            &session_id,
            "execution.cleanup",
            serde_json::json!({"phase":CollectionPhase::RetiringCallbacks}),
        );
        let pending = session
            .invocations
            .lock()
            .await
            .values()
            .filter(|invocation| invocation.foreground_pending())
            .cloned()
            .collect::<Vec<_>>();
        for invocation in &pending {
            invocation.cancel();
        }
        self.trace.record(
            &session_id,
            "execution.cleanup",
            serde_json::json!({"phase":CollectionPhase::CallbacksRetired}),
        );
        let mut errors = Vec::new();
        if let Some(native) = session.initialized.get() {
            let retired = native.retired.lock().await;
            self.trace.record(
                &session_id,
                "execution.cleanup",
                serde_json::json!({"phase":CollectionPhase::NativeCollectionStarted}),
            );
            if !*retired && let Err(error) = self.native.cleanup_execution(&native.binding.id).await
            {
                errors.push(error);
            }
            self.trace.record(
                &session_id,
                "execution.cleanup",
                serde_json::json!({"phase":CollectionPhase::NativeCollectionFinished}),
            );
        }
        if let Err(error) = collect_selected_invocations(pending).await {
            errors.push(error);
        }
        self.trace.record(
            &session_id,
            "execution.cleanup",
            serde_json::json!({"phase":CollectionPhase::PublicationsCollected}),
        );
        combined(errors)
    }

    /// Revoke callback authority without admitting a replacement native binding.
    pub async fn fence_session(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        let session = self.sessions.lock().await.get(owner).cloned();
        if let Some(session) = session {
            for invocation in session.invocations.lock().await.values() {
                invocation.cancel();
            }
            if let Some(native) = session.initialized.get() {
                let retired = native.retired.lock().await;
                if !*retired {
                    self.native.fence_session(&native.binding.id).await?;
                }
            }
        }
        Ok(())
    }

    /// Remove an old host observation channel only after its native binding is collected.
    pub async fn reset_session(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        let _admission = self.admission.write().await;
        self.retire_owner(owner).await
    }

    /// Collect all bindings and preserve every distinct cleanup failure.
    pub async fn shutdown(&self) -> Result<(), ProviderError> {
        let _admission = self.admission.write().await;
        self.closed
            .store(true, std::sync::atomic::Ordering::Release);
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
        if let Err(error) = self.native.shutdown().await {
            errors.push(error);
        }
        let mut errors = errors.into_iter();
        match errors.next() {
            None => Ok(()),
            Some(primary) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: errors.collect(),
            }),
        }
    }

    /// Collect the exact retained reader before allowing another channel for this owner.
    async fn retire_owner(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        let session = self.sessions.lock().await.get(owner).cloned();
        let Some(session) = session else {
            return Ok(());
        };
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
                    match self.native.retire_session(&native.binding.id).await {
                        Ok(()) => *retired = true,
                        Err(error) => errors.push(error),
                    }
                }
            }
            if let Err(error) = collect_invocations(&session).await {
                errors.push(error);
            }
            let mut relay = native.relay.lock().await;
            if let Some(task) = relay.as_mut() {
                match tokio::time::timeout(std::time::Duration::from_secs(15), task).await {
                    Ok(result) => {
                        match result {
                            Ok(Ok(())) => {}
                            Ok(Err(error)) => errors.push(error),
                            Err(_) => errors.push(failure(
                                ProviderOperation::CollectEventReader,
                                ProviderFailureKind::Runtime,
                            )),
                        }
                        relay.take();
                    }
                    Err(_) => errors.push(failure(
                        ProviderOperation::CollectEventReader,
                        ProviderFailureKind::Transport,
                    )),
                }
            }
        }
        combined(errors)?;
        session.invocations.lock().await.clear();
        self.sessions.lock().await.remove(owner);
        Ok(())
    }

    /// Reconcile journal evidence without interpreting the provider's opaque recovery bytes.
    pub(crate) async fn reconcile_submission(
        &self,
        submission_id: &forge_provider_api::SubmissionId,
        token: Option<&forge_provider_api::RecoveryToken>,
        accepted_receipts: &[forge_provider_api::RecoveryToken],
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        self.native
            .reconcile_submission(submission_id, token, accepted_receipts)
            .await
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
                    sender,
                    preparation: Mutex::new(()),
                    receiver: Mutex::new(Some(HostEventStream { receiver })),
                    invocations: Mutex::new(HashMap::new()),
                    request: Mutex::new(None),
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
    .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Runtime))?
    .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))
}

/// Keep host failures typed and independent of native diagnostics.
fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
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
    let sender = &host.sender;
    loop {
        match events.recv().await {
            Ok(Some(observation)) if observation.binding_id == binding => {
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
                        return retire_failed_observer(&native, &binding, &retired, error.into())
                            .await;
                    }
                };
                if sender.send(Ok(observation.event)).is_err() {
                    return retire_failed_observer(
                        &native,
                        &binding,
                        &retired,
                        EventDeliveryError::Closed.into(),
                    )
                    .await;
                }
                for event in additional {
                    if sender.send(Ok(event)).is_err() {
                        return retire_failed_observer(
                            &native,
                            &binding,
                            &retired,
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
                return retire_failed_observer(&native, &binding, &retired, error.into()).await;
            }
            Ok(None) if retiring.load(std::sync::atomic::Ordering::Acquire) => return Ok(()),
            Ok(None) => {
                let error = EventDeliveryError::Closed;
                let _ = sender.send(Err(error.clone()));
                return retire_failed_observer(&native, &binding, &retired, error.into()).await;
            }
            Err(EventDeliveryError::Closed)
                if retiring.load(std::sync::atomic::Ordering::Acquire) =>
            {
                return Ok(());
            }
            Err(error) => {
                let _ = sender.send(Err(error.clone()));
                return retire_failed_observer(&native, &binding, &retired, error.into()).await;
            }
        }
    }
}

/// Always attempt collection after fencing, retaining the original observation failure.
async fn retire_failed_observer(
    native: &Arc<dyn NativeProvider>,
    binding: &BindingId,
    retired: &Mutex<bool>,
    primary: ProviderError,
) -> Result<(), ProviderError> {
    let mut errors = Vec::new();
    let mut retired = retired.lock().await;
    if !*retired {
        if let Err(error) = native.fence_session(binding).await {
            errors.push(error);
        }
        match native.retire_session(binding).await {
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
                ProviderOperation::HostEffectPublication,
                ProviderFailureKind::Unknown,
            )),
            Err(_) => errors.push(failure(
                ProviderOperation::HostEffectPublication,
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
