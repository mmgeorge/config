//! Reusable normalized provider fixture for Harness lifecycle tests.

use std::{collections::HashMap, future::Future, sync::Arc};

use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    completion::ProviderCompletion,
    events::{self, ProviderEventSink, ProviderEventStream},
    provider::Provider,
    session::{
        BindingId, ContinuationOutcome, InputAcceptance, PromptRequest, RecoveryToken,
        SessionBinding, SessionRequest, SteeringRequest, SubmissionEvidence,
    },
    *,
};
use futures_util::future::BoxFuture;
use tokio::sync::{Mutex, watch};

type PromptHandler = dyn Fn(
        PromptRequest,
        ProviderEventSink,
        watch::Receiver<bool>,
    ) -> BoxFuture<'static, Result<(), ProviderError>>
    + Send
    + Sync;
type ContinuationHandler = dyn Fn(
        InvocationScope,
        session::InvocationCallbacks,
        ProviderEventSink,
        watch::Receiver<bool>,
    ) -> BoxFuture<'static, Result<(), ProviderError>>
    + Send
    + Sync;
type SteeringHandler =
    dyn Fn(SteeringRequest) -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type AdmissionHandler =
    dyn Fn(&PromptRequest) -> Result<Option<RecoveryToken>, ProviderError> + Send + Sync;
type ReconcileHandler = dyn Fn(&SubmissionId, Option<&RecoveryToken>, &[RecoveryToken]) -> SubmissionEvidence
    + Send
    + Sync;
type GoalStatusHandler = dyn Fn(ExecutionOwner, NativeGoalUpdate) -> BoxFuture<'static, Result<(), ProviderError>>
    + Send
    + Sync;
type PreDispatchHandler =
    dyn Fn(PromptRequest) -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type ActiveSessionHandler = dyn Fn(ExecutionOwner) -> BoxFuture<'static, Result<Option<ProviderSessionId>, ProviderError>>
    + Send
    + Sync;
type CleanupHandler =
    dyn Fn(BindingId) -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type CleanupRequestHandler =
    dyn Fn(BindingId) -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type ShutdownHandler = dyn Fn() -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type McpCatalogHandler = dyn Fn(
        catalog::ProviderCatalogRequest,
    ) -> BoxFuture<'static, Result<Vec<catalog::McpDefinition>, ProviderError>>
    + Send
    + Sync;
type TextGenerationHandler = dyn Fn(generation::TextGenerationRequest) -> BoxFuture<'static, Result<String, ProviderError>>
    + Send
    + Sync;

struct FixtureSession {
    binding: SessionBinding,
    owner: ExecutionOwner,
    sink: ProviderEventSink,
    stream: Mutex<Option<ProviderEventStream>>,
    admission: Mutex<()>,
    active: Mutex<Option<tokio::task::JoinHandle<Result<(), ProviderError>>>>,
    cancelled: Mutex<Option<watch::Sender<bool>>>,
    last_prompt: Mutex<Option<PromptRequest>>,
    continuations: Mutex<HashMap<Vec<u8>, InvocationScope>>,
    finished: tokio::sync::Notify,
    context_inputs: Mutex<Vec<session::RecoveredInput>>,
}

/// Execute a normalized event script supplied by one broker test.
pub struct EventFixtureProvider {
    descriptor: ProviderDescriptor,
    handler: Arc<PromptHandler>,
    continuation: std::sync::RwLock<Option<Arc<ContinuationHandler>>>,
    steering: std::sync::RwLock<Option<Arc<SteeringHandler>>>,
    models: std::sync::RwLock<Option<Vec<ProviderModel>>>,
    admission: std::sync::RwLock<Option<Arc<AdmissionHandler>>>,
    reconcile: std::sync::RwLock<Option<Arc<ReconcileHandler>>>,
    goal_status: std::sync::RwLock<Option<Arc<GoalStatusHandler>>>,
    pre_dispatch: std::sync::RwLock<Option<Arc<PreDispatchHandler>>>,
    active_session_id: std::sync::RwLock<Option<Arc<ActiveSessionHandler>>>,
    cleanup: std::sync::RwLock<Option<Arc<CleanupHandler>>>,
    cleanup_request: std::sync::RwLock<Option<Arc<CleanupRequestHandler>>>,
    shutdown: std::sync::RwLock<Option<Arc<ShutdownHandler>>>,
    mcp_configuration: std::sync::RwLock<Option<Arc<McpCatalogHandler>>>,
    mcp_list: std::sync::RwLock<Option<Arc<McpCatalogHandler>>>,
    text_generation: std::sync::RwLock<Option<Arc<TextGenerationHandler>>>,
    sessions: Mutex<HashMap<BindingId, Arc<FixtureSession>>>,
    owners: Mutex<HashMap<ExecutionOwner, BindingId>>,
}

impl EventFixtureProvider {
    /// Create a provider fake that emits only normalized provider events.
    pub fn new<F, Fut>(descriptor: ProviderDescriptor, handler: F) -> Arc<Self>
    where
        F: Fn(PromptRequest, ProviderEventSink, watch::Receiver<bool>) -> Fut
            + Send
            + Sync
            + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        Arc::new(Self {
            descriptor,
            handler: Arc::new(move |request, sink, cancelled| {
                Box::pin(handler(request, sink, cancelled))
            }),
            continuation: std::sync::RwLock::new(None),
            steering: std::sync::RwLock::new(None),
            models: std::sync::RwLock::new(None),
            admission: std::sync::RwLock::new(None),
            reconcile: std::sync::RwLock::new(None),
            goal_status: std::sync::RwLock::new(None),
            pre_dispatch: std::sync::RwLock::new(None),
            active_session_id: std::sync::RwLock::new(None),
            cleanup: std::sync::RwLock::new(None),
            cleanup_request: std::sync::RwLock::new(None),
            shutdown: std::sync::RwLock::new(None),
            mcp_configuration: std::sync::RwLock::new(None),
            mcp_list: std::sync::RwLock::new(None),
            text_generation: std::sync::RwLock::new(None),
            sessions: Mutex::new(HashMap::new()),
            owners: Mutex::new(HashMap::new()),
        })
    }

    async fn session(&self, binding: &BindingId) -> Result<Arc<FixtureSession>, ProviderError> {
        self.sessions
            .lock()
            .await
            .get(binding)
            .cloned()
            .ok_or_else(|| {
                fixture_error(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::SessionMissing,
                )
            })
    }

    /// Add normalized continuation behavior for retry lifecycle tests.
    pub fn with_continuation<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(
                InvocationScope,
                session::InvocationCallbacks,
                ProviderEventSink,
                watch::Receiver<bool>,
            ) -> Fut
            + Send
            + Sync
            + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .continuation
            .write()
            .expect("fixture continuation lock is not poisoned") =
            Some(Arc::new(move |scope, callbacks, sink, cancelled| {
                Box::pin(handler(scope, callbacks, sink, cancelled))
            }));
        self.clone()
    }

    /// Add normalized steering behavior for admission and ordering tests.
    pub fn with_steering<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(SteeringRequest) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .steering
            .write()
            .expect("fixture steering lock is not poisoned") =
            Some(Arc::new(move |request| Box::pin(handler(request))));
        self.clone()
    }

    /// Advertise a normalized model catalog for model-selection tests.
    pub fn with_models(self: &Arc<Self>, models: Vec<ProviderModel>) -> Arc<Self> {
        *self
            .models
            .write()
            .expect("fixture model catalog lock is not poisoned") = Some(models);
        self.clone()
    }

    /// Script the normalized evidence returned by submission reconciliation.
    pub fn with_reconcile<F>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(&SubmissionId, Option<&RecoveryToken>, &[RecoveryToken]) -> SubmissionEvidence
            + Send
            + Sync
            + 'static,
    {
        *self
            .reconcile
            .write()
            .expect("fixture reconciliation lock is not poisoned") = Some(Arc::new(handler));
        self.clone()
    }

    /// Inject a deterministic result between the sending and accepted journal transitions.
    pub fn with_admission<F>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(&PromptRequest) -> Result<Option<RecoveryToken>, ProviderError>
            + Send
            + Sync
            + 'static,
    {
        *self
            .admission
            .write()
            .expect("fixture admission lock is not poisoned") = Some(Arc::new(handler));
        self.clone()
    }

    /// Control normalized goal status calls in workflow lifecycle tests.
    pub fn with_goal_status<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(ExecutionOwner, NativeGoalUpdate) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .goal_status
            .write()
            .expect("fixture goal status lock is not poisoned") =
            Some(Arc::new(move |owner, update| {
                Box::pin(handler(owner, update))
            }));
        self.clone()
    }

    /// Pause or reject before any durable input transition or native dispatch.
    pub fn with_pre_dispatch<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(PromptRequest) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .pre_dispatch
            .write()
            .expect("fixture pre-dispatch lock is not poisoned") =
            Some(Arc::new(move |request| Box::pin(handler(request))));
        self.clone()
    }

    /// Override provider-session identity lookup for lifecycle barrier tests.
    pub fn with_active_session_id<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(ExecutionOwner) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Option<ProviderSessionId>, ProviderError>> + Send + 'static,
    {
        *self
            .active_session_id
            .write()
            .expect("fixture session identity lock is not poisoned") =
            Some(Arc::new(move |owner| Box::pin(handler(owner))));
        self.clone()
    }

    /// Run a host-controlled cleanup barrier after cancellation and task join.
    pub fn with_cleanup<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(BindingId) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .cleanup
            .write()
            .expect("fixture cleanup lock is not poisoned") =
            Some(Arc::new(move |binding| Box::pin(handler(binding))));
        self.clone()
    }

    /// Request cleanup before joining an active handler, for retryable cleanup tests.
    pub fn with_cleanup_request<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(BindingId) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .cleanup_request
            .write()
            .expect("fixture cleanup request lock is not poisoned") =
            Some(Arc::new(move |binding| Box::pin(handler(binding))));
        self.clone()
    }

    /// Run a final normalized lifecycle hook before fixture sessions are retired.
    pub fn with_shutdown<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn() -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .shutdown
            .write()
            .expect("fixture shutdown lock is not poisoned") =
            Some(Arc::new(move || Box::pin(handler())));
        self.clone()
    }

    /// Script normalized provider MCP configuration discovery.
    pub fn with_mcp_configuration<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(catalog::ProviderCatalogRequest) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Vec<catalog::McpDefinition>, ProviderError>> + Send + 'static,
    {
        *self
            .mcp_configuration
            .write()
            .expect("fixture MCP configuration lock is not poisoned") =
            Some(Arc::new(move |request| Box::pin(handler(request))));
        self.clone()
    }

    /// Script normalized provider MCP readiness and tool discovery.
    pub fn with_mcp_list<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(catalog::ProviderCatalogRequest) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Vec<catalog::McpDefinition>, ProviderError>> + Send + 'static,
    {
        *self
            .mcp_list
            .write()
            .expect("fixture MCP list lock is not poisoned") =
            Some(Arc::new(move |request| Box::pin(handler(request))));
        self.clone()
    }

    /// Script normalized isolated text-generation requests.
    pub fn with_text_generation<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(generation::TextGenerationRequest) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<String, ProviderError>> + Send + 'static,
    {
        *self
            .text_generation
            .write()
            .expect("fixture text generation lock is not poisoned") =
            Some(Arc::new(move |request| Box::pin(handler(request))));
        self.clone()
    }

    /// Clone the session stream sink for normalized cross-turn event fixtures.
    pub async fn event_sink(
        &self,
        owner: &ExecutionOwner,
    ) -> Result<ProviderEventSink, ProviderError> {
        let binding = self
            .owners
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                fixture_error(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        Ok(self.session(&binding).await?.sink.clone())
    }

    /// Bind the retained session stream to one invocation scope for event injection.
    pub async fn invocation_sink(
        &self,
        scope: &InvocationScope,
    ) -> Result<ProviderEventSink, ProviderError> {
        let binding = self
            .owners
            .lock()
            .await
            .get(&scope.owner)
            .cloned()
            .ok_or_else(|| {
                fixture_error(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        self.session(&binding)
            .await?
            .sink
            .invocation_sink(scope)
            .map_err(Into::into)
    }
}

impl EventFixtureProvider {
    async fn submit_prompt(
        &self,
        binding: &BindingId,
        mut request: PromptRequest,
        sending_recorded: bool,
    ) -> Result<InputAcceptance, ProviderError> {
        let session = self.session(binding).await?;
        let _admission = session.admission.lock().await;
        self.collect_finished(&session).await?;
        if session.active.lock().await.is_some() {
            return Err(fixture_error(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        if request.scope.owner != session.owner || request.receipt.id() != &request.submission_id {
            return Err(fixture_error(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        let context = std::mem::take(&mut *session.context_inputs.lock().await);
        request.recovered_inputs.splice(0..0, context);
        let handler = self
            .pre_dispatch
            .read()
            .expect("fixture pre-dispatch lock is not poisoned")
            .clone();
        if let Some(handler) = handler
            && let Err(error) = handler(request.clone()).await
        {
            return Err(match error {
                ProviderError::PreparationFailed(_) | ProviderError::PreparationCleanup { .. } => {
                    error
                }
                error => ProviderError::PreparationFailed(Box::new(error)),
            });
        }
        let sink = session.sink.invocation_sink(&request.scope)?;
        if !sending_recorded {
            let token = RecoveryToken::new(
                format!("fixture-sending:{}", request.submission_id.as_str()).into_bytes(),
            )
            .map_err(|_| {
                fixture_error(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown)
            })?;
            request
                .receipt
                .clone()
                .record_sending(Some(token))
                .await
                .map_err(|error| ProviderError::PreparationFailed(Box::new(error)))?;
        }
        let admission_handler = self
            .admission
            .read()
            .expect("fixture admission lock is not poisoned")
            .clone();
        let accepted_receipt = if let Some(handler) = admission_handler {
            match handler(&request) {
                Ok(receipt) => receipt,
                Err(ProviderError::DeliveryUncertain) => {
                    request
                        .receipt
                        .clone()
                        .record_failure(ProviderError::DeliveryUncertain)
                        .await?;
                    return Err(ProviderError::DeliveryUncertain);
                }
                Err(error) => {
                    return match request.receipt.clone().record_rejected(error.clone()).await {
                        Ok(()) => Err(ProviderError::PreparationFailed(Box::new(error))),
                        Err(cleanup) => Err(ProviderError::PreparationCleanup {
                            cause: Box::new(error),
                            cleanup: Box::new(cleanup),
                        }),
                    };
                }
            }
        } else {
            None
        };
        request
            .receipt
            .clone()
            .record_accepted(accepted_receipt)
            .await
            .map_err(|error| ProviderError::PreparationFailed(Box::new(error)))?;
        *session.last_prompt.lock().await = Some(request.clone());
        let acceptance = InputAcceptance {
            submission_id: request.submission_id.clone(),
        };
        let handler = self.handler.clone();
        let descriptor = self.descriptor.clone();
        let task_session = session.clone();
        let task_request = request.clone();
        let (cancel_sender, cancel_receiver) = watch::channel(false);
        *session.cancelled.lock().await = Some(cancel_sender);
        let task = tokio::spawn(async move {
            let result = if let Err(primary) =
                handler(task_request.clone(), sink.clone(), cancel_receiver).await
            {
                let failure = if matches!(primary, ProviderError::DeliveryUncertain) {
                    lifecycle::ObservationFailure::DeliveryUncertain
                } else {
                    lifecycle::ObservationFailure::Protocol
                };
                let completion = ProviderCompletion {
                    provider_session_id: task_session.binding.provider_session_id.clone(),
                    capability: descriptor.capability,
                    runtime: ProviderRuntime {
                        provider: descriptor.label,
                        model: task_request.configuration.model.model.clone(),
                    },
                    ..Default::default()
                };
                let observation = lifecycle::ExecutionObservation::new(
                    task_request.scope.clone(),
                    lifecycle::ExecutionUpdate::ObservationLost {
                        failure,
                        completion: Some(completion.into()),
                    },
                )?;
                match sink
                    .send_wait(ProviderEvent::Execution { observation })
                    .await
                {
                    Ok(()) => Err(primary),
                    Err(cleanup) => Err(ProviderError::Combined {
                        primary: Box::new(primary),
                        secondary: vec![cleanup.into()],
                    }),
                }
            } else {
                Ok(())
            };
            task_session.finished.notify_one();
            result
        });
        *session.active.lock().await = Some(task);
        request.dispatch.confirm();
        Ok(acceptance)
    }
}

#[async_trait::async_trait]
impl Provider for EventFixtureProvider {
    async fn initialize_session(
        &self,
        request: SessionRequest,
    ) -> Result<SessionBinding, ProviderError> {
        if let Some(previous) = self.owners.lock().await.get(&request.owner).cloned() {
            self.retire_session(&previous).await?;
        }
        let binding = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            fixture_error(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Unknown,
            )
        })?;
        let provider_session_id = request.provider_session_id.or_else(|| {
            Some(
                uuid::Uuid::new_v4()
                    .to_string()
                    .try_into()
                    .expect("UUID is nonempty"),
            )
        });
        let session_binding = SessionBinding {
            id: binding.clone(),
            provider_session_id,
        };
        let scope = EventScope::SessionOperation {
            owner: request.owner.clone(),
            operation_id: binding.as_str().to_owned(),
        };
        let (sink, stream) = events::channel(binding.clone(), scope);
        let session = Arc::new(FixtureSession {
            binding: session_binding.clone(),
            owner: request.owner.clone(),
            sink,
            stream: Mutex::new(Some(stream)),
            admission: Mutex::new(()),
            active: Mutex::new(None),
            cancelled: Mutex::new(None),
            last_prompt: Mutex::new(None),
            continuations: Mutex::new(HashMap::new()),
            finished: tokio::sync::Notify::new(),
            context_inputs: Mutex::new(Vec::new()),
        });
        self.sessions.lock().await.insert(binding.clone(), session);
        self.owners.lock().await.insert(request.owner, binding);
        Ok(session_binding)
    }

    async fn take_session_events(
        &self,
        binding: &BindingId,
    ) -> Result<ProviderEventStream, ProviderError> {
        self.session(binding)
            .await?
            .stream
            .lock()
            .await
            .take()
            .ok_or_else(|| {
                fixture_error(
                    ProviderOperation::CollectEventReader,
                    ProviderFailureKind::Rejected,
                )
            })
    }

    async fn session_prompt(
        &self,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        self.submit_prompt(binding, request, false).await
    }

    async fn continue_session(
        &self,
        binding: &BindingId,
        request: session::ContinuationRequest,
    ) -> Result<ContinuationOutcome, ProviderError> {
        match request {
            session::ContinuationRequest::NotConsumed {
                request,
                authorization,
            } => {
                let session = self.session(binding).await?;
                {
                    let mut grants = session.continuations.lock().await;
                    if grants.get(authorization.as_bytes()) != Some(&request.scope) {
                        return Err(fixture_error(
                            ProviderOperation::AdmitInput,
                            ProviderFailureKind::Rejected,
                        ));
                    }
                    grants.remove(authorization.as_bytes());
                }
                Ok(ContinuationOutcome::InputAccepted(
                    self.submit_prompt(binding, *request, true).await?,
                ))
            }
            session::ContinuationRequest::RetryableExecution {
                scope,
                callbacks,
                authorization,
                dispatch,
            } => {
                if !dispatch.matches_continuation(binding, &scope, &authorization) {
                    return Err(fixture_error(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Protocol,
                    ));
                }
                let callbacks = *callbacks;
                let session = self.session(binding).await?;
                let _admission = session.admission.lock().await;
                self.collect_finished(&session).await?;
                if session.active.lock().await.is_some() || scope.owner != session.owner {
                    return Err(fixture_error(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                }
                let handler = self
                    .continuation
                    .read()
                    .expect("fixture continuation lock is not poisoned")
                    .clone()
                    .ok_or_else(|| {
                        fixture_error(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected)
                    })?;
                let sink = session.sink.invocation_sink(&scope)?;
                let (cancel_sender, cancel_receiver) = watch::channel(false);
                *session.cancelled.lock().await = Some(cancel_sender);
                *session.active.lock().await = Some(tokio::spawn(async move {
                    handler(scope, callbacks, sink, cancel_receiver).await
                }));
                dispatch.confirm();
                Ok(ContinuationOutcome::ExecutionResumed)
            }
        }
    }

    async fn session_steer(
        &self,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<session::SteeringAcceptance, ProviderError> {
        let session = self.session(binding).await?;
        if request.scope.owner != session.owner {
            return Err(fixture_error(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        let submission_id = request.delivery.submission_id().cloned();
        let receipt = request.delivery.receipt().cloned();
        if let (Some(id), Some(receipt)) = (&submission_id, receipt.as_ref()) {
            if receipt.id() != id {
                return Err(fixture_error(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ));
            }
            let token =
                RecoveryToken::new(format!("fixture-steering:{}", id.as_str()).into_bytes())
                    .map_err(|_| {
                        fixture_error(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown)
                    })?;
            receipt
                .clone()
                .record_sending(Some(token))
                .await
                .map_err(|error| ProviderError::PreparationFailed(Box::new(error)))?;
        }
        let steering = self
            .steering
            .read()
            .expect("fixture steering lock is not poisoned")
            .clone();
        let result = if let Some(handler) = steering {
            handler(request.clone()).await
        } else {
            Ok(())
        };
        match result {
            Err(ProviderError::NotConsumed { continuation }) if submission_id.is_some() => {
                let (Some(submission_id), Some(receipt)) = (submission_id.clone(), receipt) else {
                    return Err(fixture_error(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                };
                let mut prompt = session.last_prompt.lock().await.clone().ok_or_else(|| {
                    fixture_error(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected)
                })?;
                prompt.scope = request.scope.clone();
                prompt.submission_id = submission_id.clone();
                prompt.receipt = receipt;
                prompt.input = request.input.clone();
                prompt.attachments = request.attachments.clone();
                prompt.task = request.task.clone();
                prompt.goal = request.goal.clone();
                session
                    .continuations
                    .lock()
                    .await
                    .insert(continuation.as_bytes().to_vec(), request.scope.clone());
                self.wait_for_finished(&session).await?;
                match self
                    .continue_session(
                        binding,
                        session::ContinuationRequest::NotConsumed {
                            request: Box::new(prompt),
                            authorization: continuation,
                        },
                    )
                    .await?
                {
                    session::ContinuationOutcome::InputAccepted(_) => {
                        return Ok(session::SteeringAcceptance::Journaled { submission_id });
                    }
                    session::ContinuationOutcome::ExecutionResumed => {
                        unreachable!("not-consumed continuation admits input")
                    }
                }
            }
            Err(ProviderError::NotConsumed { .. }) if submission_id.is_none() => {
                session
                    .context_inputs
                    .lock()
                    .await
                    .push(session::RecoveredInput {
                        input: request.input.clone(),
                        attachments: request.attachments.clone(),
                        instructions: request
                            .task
                            .as_ref()
                            .map_or(String::new(), |task| task.heading()),
                    });
                return Ok(session::SteeringAcceptance::Context);
            }
            Err(error) => return Err(error),
            Ok(()) => {}
        }
        if let Some(receipt) = receipt {
            receipt.clone().record_accepted(None).await?;
        }
        match submission_id {
            Some(submission_id) => Ok(session::SteeringAcceptance::Journaled { submission_id }),
            None => {
                session
                    .context_inputs
                    .lock()
                    .await
                    .push(session::RecoveredInput {
                        input: request.input,
                        attachments: request.attachments,
                        instructions: request.task.map_or(String::new(), |task| task.heading()),
                    });
                Ok(session::SteeringAcceptance::Context)
            }
        }
    }

    async fn retire_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.session(binding).await?;
        self.cancel_session(binding).await?;
        self.sessions.lock().await.remove(binding);
        self.owners.lock().await.remove(&session.owner);
        Ok(())
    }

    async fn fence_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.session(binding).await?;
        if let Some(cancelled) = session.cancelled.lock().await.as_ref() {
            cancelled.send_replace(true);
        }
        session.sink.fail(events::EventDeliveryError::Observation(
            lifecycle::ObservationFailure::Disconnected,
        ));
        Ok(())
    }

    async fn recover_session(
        &self,
        request: SessionRequest,
        _token: Option<RecoveryToken>,
    ) -> Result<SessionBinding, ProviderError> {
        self.initialize_session(request).await
    }

    async fn shutdown(&self) -> Result<(), ProviderError> {
        let handler = self
            .shutdown
            .read()
            .expect("fixture shutdown lock is not poisoned")
            .clone();
        if let Some(handler) = handler {
            handler().await?;
        }
        let bindings = self
            .sessions
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for binding in bindings {
            self.retire_session(&binding).await?;
        }
        Ok(())
    }

    fn descriptor(&self) -> ProviderDescriptor {
        self.descriptor.clone()
    }

    async fn has_active_turn(&self, owner: &ExecutionOwner) -> bool {
        let Some(binding) = self.owners.lock().await.get(owner).cloned() else {
            return false;
        };
        self.session(&binding).await.ok().is_some_and(|session| {
            session
                .active
                .try_lock()
                .ok()
                .and_then(|active| active.as_ref().map(|task| !task.is_finished()))
                .unwrap_or(false)
        })
    }

    async fn cancel_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.session(binding).await?;
        if let Some(cancelled) = session.cancelled.lock().await.as_ref() {
            cancelled.send_replace(true);
        }
        self.cleanup_execution(binding).await
    }

    async fn cleanup_execution(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.session(binding).await?;
        if let Some(cancelled) = session.cancelled.lock().await.as_ref() {
            cancelled.send_replace(true);
        }
        let handler = self
            .cleanup_request
            .read()
            .expect("fixture cleanup request lock is not poisoned")
            .clone();
        if let Some(handler) = handler {
            handler(binding.clone()).await?;
        }
        let task_result = {
            let mut active = session.active.lock().await;
            let result = match active.as_mut() {
                Some(task) => match task.await {
                    Ok(result) => result,
                    Err(_) => Err(fixture_error(
                        ProviderOperation::CleanupExecution,
                        ProviderFailureKind::Unknown,
                    )),
                },
                None => Ok(()),
            };
            active.take();
            result
        };
        let cleanup_handler = self
            .cleanup
            .read()
            .expect("fixture cleanup lock is not poisoned")
            .clone();
        let hook_result = if let Some(handler) = cleanup_handler {
            handler(binding.clone()).await
        } else {
            Ok(())
        };
        match (task_result, hook_result) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(primary), Ok(())) => Err(primary),
            (Ok(()), Err(cleanup)) => Err(cleanup),
            (Err(primary), Err(cleanup)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![cleanup],
            }),
        }
    }

    async fn active_session_id_for(
        &self,
        owner: &ExecutionOwner,
    ) -> Result<Option<ProviderSessionId>, ProviderError> {
        let handler = self
            .active_session_id
            .read()
            .expect("fixture session identity lock is not poisoned")
            .clone();
        if let Some(handler) = handler {
            return handler(owner.clone()).await;
        }
        let Some(binding) = self.owners.lock().await.get(owner).cloned() else {
            return Ok(None);
        };
        Ok(self
            .session(&binding)
            .await?
            .binding
            .provider_session_id
            .clone())
    }

    async fn model_list(
        &self,
        _request: catalog::ProviderCatalogRequest,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        self.models
            .read()
            .expect("fixture model catalog lock is not poisoned")
            .clone()
            .ok_or_else(|| {
                capability::UnsupportedCapability(capability::ProviderFeature::ModelSelection)
                    .into()
            })
    }

    async fn mcp_configuration(
        &self,
        request: catalog::ProviderCatalogRequest,
    ) -> Result<Vec<catalog::McpDefinition>, ProviderError> {
        let handler = self
            .mcp_configuration
            .read()
            .expect("fixture MCP configuration lock is not poisoned")
            .clone()
            .ok_or_else(|| {
                capability::UnsupportedCapability(capability::ProviderFeature::McpCatalog)
            })?;
        handler(request).await
    }

    async fn mcp_list(
        &self,
        request: catalog::ProviderCatalogRequest,
    ) -> Result<Vec<catalog::McpDefinition>, ProviderError> {
        let handler = self
            .mcp_list
            .read()
            .expect("fixture MCP list lock is not poisoned")
            .clone()
            .ok_or_else(|| {
                capability::UnsupportedCapability(capability::ProviderFeature::McpCatalog)
            })?;
        handler(request).await
    }

    async fn generate_text(
        &self,
        request: generation::TextGenerationRequest,
    ) -> Result<String, ProviderError> {
        let handler = self
            .text_generation
            .read()
            .expect("fixture text generation lock is not poisoned")
            .clone()
            .ok_or_else(|| {
                capability::UnsupportedCapability(capability::ProviderFeature::TextGeneration)
            })?;
        handler(request).await
    }

    async fn goal_status(
        &self,
        owner: &ExecutionOwner,
        update: NativeGoalUpdate,
    ) -> Result<(), ProviderError> {
        let handler = self
            .goal_status
            .read()
            .expect("fixture goal status lock is not poisoned")
            .clone()
            .ok_or_else(|| {
                capability::UnsupportedCapability(capability::ProviderFeature::NativeGoal)
            })?;
        handler(owner.clone(), update).await
    }

    async fn reconcile_submission(
        &self,
        _submission_id: &SubmissionId,
        _token: Option<&RecoveryToken>,
        _accepted_receipts: &[RecoveryToken],
    ) -> Result<SubmissionEvidence, ProviderError> {
        let handler = self
            .reconcile
            .read()
            .expect("fixture reconciliation lock is not poisoned")
            .clone();
        Ok(handler.map_or(SubmissionEvidence::Unknown, |handler| {
            handler(_submission_id, _token, _accepted_receipts)
        }))
    }
}

impl EventFixtureProvider {
    async fn wait_for_finished(&self, session: &Arc<FixtureSession>) -> Result<(), ProviderError> {
        loop {
            let notified = session.finished.notified();
            if session
                .active
                .lock()
                .await
                .as_ref()
                .is_none_or(tokio::task::JoinHandle::is_finished)
            {
                return Ok(());
            }
            notified.await;
        }
    }

    async fn collect_finished(&self, session: &Arc<FixtureSession>) -> Result<(), ProviderError> {
        let task = {
            let mut active = session.active.lock().await;
            if active
                .as_ref()
                .is_some_and(tokio::task::JoinHandle::is_finished)
            {
                active.take()
            } else {
                None
            }
        };
        if let Some(task) = task {
            task.await.map_err(|_| {
                fixture_error(
                    ProviderOperation::CleanupExecution,
                    ProviderFailureKind::Unknown,
                )
            })?
        } else {
            Ok(())
        }
    }
}

fn fixture_error(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Invoke one host-owned test tool through the normalized per-invocation callback.
pub async fn call_tool(
    request: &PromptRequest,
    name: &str,
    arguments: serde_json::Value,
) -> Result<serde_json::Value, ProviderError> {
    if !request.tools.iter().any(|tool| tool.name == name) {
        return Err(fixture_error(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Authorization,
        ));
    }
    let output = request
        .provider_tools
        .call(
            &request.scope,
            forge_provider_api::control::ToolCall {
                id: forge_provider_api::control::ToolCallId::new(uuid::Uuid::new_v4().to_string())
                    .expect("fixture call identity is nonempty"),
                name: name.to_owned(),
                arguments,
            },
        )
        .await?;
    serde_json::from_str(&output)
        .map_err(|_| fixture_error(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol))
}
