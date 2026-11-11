//! Reusable normalized provider fixture with retained invocation collection.
use crate::provider::{ScopeOwner, ThreadOwner};

use forge_provider_api::retained_operation::{IdentityRegistry, Interruption, Store};
use forge_provider_api::{
    capability::{CapabilityManifest, ProviderFeature, UnsupportedCapability},
    catalog::{McpDefinition, McpQuery, QueryContext},
    events::{self, ProviderEventSink},
    provider::Provider,
    *,
};
use futures_util::future::BoxFuture;
use std::{
    collections::{HashMap, HashSet},
    future::Future,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};
use tokio::sync::{Mutex, watch};

type PromptHandler = dyn Fn(
        PromptRequest,
        AdapterActivityWriter,
        NativeTurnStart,
        watch::Receiver<bool>,
    ) -> BoxFuture<'static, Result<TurnResult, ProviderError>>
    + Send
    + Sync;
type SteeringHandler =
    dyn Fn(SteeringRequest) -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type AdmissionHandler =
    dyn Fn(&PromptRequest) -> Result<Option<RecoveryToken>, ProviderError> + Send + Sync;
type ReconcileHandler = dyn Fn(&SubmissionId, Option<&RecoveryToken>, &[RecoveryToken]) -> SubmissionEvidence
    + Send
    + Sync;
type PreDispatchHandler =
    dyn Fn(PromptRequest) -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type SendingHandler = dyn Fn(PromptRequest, watch::Receiver<bool>) -> BoxFuture<'static, Result<(), ProviderError>>
    + Send
    + Sync;
type OpenHandler =
    dyn Fn(SessionRequest) -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type ConfigureHandler = dyn Fn(BindingId, SessionConfiguration) -> BoxFuture<'static, Result<(), ProviderError>>
    + Send
    + Sync;
type IdentityHandler = dyn Fn(ThreadOwner) -> BoxFuture<'static, Result<Option<ProviderSessionId>, ProviderError>>
    + Send
    + Sync;
type CleanupHandler =
    dyn Fn(BindingId) -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type ShutdownHandler = dyn Fn() -> BoxFuture<'static, Result<(), ProviderError>> + Send + Sync;
type McpHandler =
    dyn Fn(McpQuery) -> BoxFuture<'static, Result<Vec<McpDefinition>, ProviderError>> + Send + Sync;

struct FixtureOperations {
    identities: Arc<IdentityRegistry>,
    open: Store<SessionRequestIdentity, OpenedSession>,
    configure: Store<(BindingId, SessionConfiguration), ConfiguredSession>,
    fork: Store<SessionForkRequest, ProviderForkResult>,
    compact: Store<(BindingId, provider::CompactionRequest), operations::CompactionResult>,
    close: Store<BindingId, ()>,
    cancel: Store<(BindingId, CancelRequest), TurnInterruptResult>,
    submit: Store<(BindingId, PromptRequestIdentity), InputAcceptance>,
    steer: Store<(BindingId, SteeringRequestIdentity), SteeringAcceptance>,
    reconcile: Store<InputReconcileRequest, SubmissionEvidence>,
    retract: Store<(BindingId, InputRetractRequest), InputRetractOutcome>,
    shutdown: Store<(), ()>,
    models: Store<QueryContext, Vec<ProviderModel>>,
    skills: Store<QueryContext, Vec<catalog::SkillDefinition>>,
    mcp: Store<McpQuery, Vec<McpDefinition>>,
    terminals: Store<BindingId, terminal::TerminalInventory>,
    terminal_terminate: Store<(BindingId, terminal::TerminalId), ()>,
}

impl Default for FixtureOperations {
    /// Create separate typed retained records for each semantic operation.
    fn default() -> Self {
        let identities = Arc::new(IdentityRegistry::new());
        Self {
            identities: identities.clone(),
            open: Store::registered(identities.clone()),
            configure: Store::registered(identities.clone()),
            fork: Store::registered(identities.clone()),
            compact: Store::registered(identities.clone()),
            close: Store::registered(identities.clone()),
            cancel: Store::registered(identities.clone()),
            submit: Store::registered(identities.clone()),
            steer: Store::registered(identities.clone()),
            reconcile: Store::registered(identities.clone()),
            retract: Store::registered(identities.clone()),
            shutdown: Store::registered(identities.clone()),
            models: Store::registered(identities.clone()),
            skills: Store::registered(identities.clone()),
            mcp: Store::registered(identities.clone()),
            terminals: Store::registered(identities.clone()),
            terminal_terminate: Store::registered(identities.clone()),
        }
    }
}

impl FixtureOperations {
    /// Stop new fixture operations while preserving exact recorded replays.
    fn close_admission(&self) {
        self.identities.close_admission();
        self.open.close_admission();
        self.configure.close_admission();
        self.fork.close_admission();
        self.compact.close_admission();
        self.submit.close_admission();
        self.steer.close_admission();
        self.reconcile.close_admission();
        self.retract.close_admission();
        self.models.close_admission();
        self.skills.close_admission();
        self.mcp.close_admission();
        self.terminals.close_admission();
        self.terminal_terminate.close_admission();
    }

    /// Join retained work without waiting for the shutdown operation which performs this collection.
    async fn join_all(&self, shutdown: &OperationId) -> Result<(), ProviderError> {
        let results = [
            self.open.join_all().await,
            self.configure.join_all().await,
            self.fork.join_all().await,
            self.compact.join_all().await,
            self.close.join_all().await,
            self.cancel.join_all().await,
            self.submit.join_all().await,
            self.steer.join_all().await,
            self.reconcile.join_all().await,
            self.retract.join_all().await,
            self.models.join_all().await,
            self.skills.join_all().await,
            self.mcp.join_all().await,
            self.terminals.join_all().await,
            self.terminal_terminate.join_all().await,
            self.shutdown.join_all_except(shutdown).await,
        ];
        let mut errors = results.into_iter().filter_map(Result::err);
        match errors.next() {
            None => Ok(()),
            Some(primary) => {
                let secondary: Vec<_> = errors.collect();
                if secondary.is_empty() {
                    return Err(primary);
                }
                Err(ProviderError::Combined {
                    primary: Box::new(primary),
                    secondary,
                })
            }
        }
    }
}

struct FixtureSession {
    binding: SessionBinding,
    owner: ThreadOwner,
    sink: ProviderEventSink,
    admission: Mutex<()>,
    configuration: Mutex<SessionConfiguration>,
    state: Mutex<SessionState>,
    closed: AtomicBool,
    invalidated: AtomicBool,
    closing: Mutex<()>,
}

#[derive(Default)]
struct SessionState {
    active: Option<InvocationId>,
    invocations: HashMap<InvocationId, Arc<FixtureInvocation>>,
    history_boundary: Option<(
        InvocationScope,
        SubmissionId,
        DeliveryAttemptId,
        CheckpointId,
    )>,
}

struct FixtureInvocation {
    scope: InvocationScope,
    submission_id: SubmissionId,
    attempt_id: DeliveryAttemptId,
    cancelled: watch::Sender<bool>,
    settlement: watch::Sender<Option<Result<CompletedTurn, ProviderError>>>,
    task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    cleanup_request: Mutex<bool>,
    collected: AtomicBool,
    started: Arc<AtomicBool>,
}

struct FixtureStartObserver(Arc<AtomicBool>);
impl forge_provider_api::events::EventObserver for FixtureStartObserver {
    /// Retain positive native start so pre-start cancellation never invalidates a reusable fixture.
    fn before_delivery(
        &self,
        event: &forge_provider_api::ProviderEvent,
    ) -> Result<(), forge_provider_api::events::EventDeliveryError> {
        if matches!(
            event,
            forge_provider_api::ProviderEvent::Turn {
                event: forge_provider_api::TurnEvent::Started(_),
                ..
            }
        ) {
            self.0.store(true, Ordering::Release);
        }
        Ok(())
    }
}

struct FixtureDriver {
    request: PromptRequest,
    session: Arc<FixtureSession>,
    invocation: Arc<FixtureInvocation>,
    handler: Arc<PromptHandler>,
    pre_dispatch: Option<Arc<PreDispatchHandler>>,
    sending: Option<Arc<SendingHandler>>,
    admission: Option<Arc<AdmissionHandler>>,
    cleanup: Option<Arc<CleanupHandler>>,
    identity: Option<Arc<IdentityHandler>>,
    context: OperationContext,
    accepted: std::sync::Mutex<
        Option<tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>>,
    >,
}
#[async_trait::async_trait]
impl forge_providers::testing::NativeTurnDriver for FixtureDriver {
    /// Apply exact journal admission before the fixture's actual native observations.
    async fn run(&self, start: NativeTurnStart) -> Result<TurnResult, ProviderError> {
        let request = self.request.clone();
        let mut cancelled = self.invocation.cancelled.subscribe();
        let result = async {
            if *cancelled.borrow() || self.session.closed.load(Ordering::Acquire) {
                return Err(ProviderError::NotConsumed);
            }
            if let Some(hook) = &self.pre_dispatch {
                tokio::select! {
                    result = hook(request.clone()) => result.map_err(|cause| ProviderError::PreparationFailed(Box::new(cause)))?,
                    changed = cancelled.changed() => { let _ = changed; return Err(ProviderError::NotConsumed); }
                    reason = self.context.interrupted() => return Err(reason.failure(ProviderOperation::TurnStart, ProviderPhase::Preparation, FailureDisposition::Unchanged)),
                }
            }
            if *cancelled.borrow() || self.session.closed.load(Ordering::Acquire) { return Err(ProviderError::NotConsumed); }
            self.context.check_admission(ProviderOperation::TurnStart)?;
            let token = RecoveryToken::new(format!("fixture-sending:{}:{}", request.submission_id.as_str(), request.attempt_id.as_str()).into_bytes()).expect("fixture sending evidence is nonempty");
            request.receipt.clone().record_sending(Some(token)).await?;
            if *cancelled.borrow() || self.session.closed.load(Ordering::Acquire) {
                request.receipt.clone().record_rejected(ProviderError::NotConsumed).await?;
                return Err(ProviderError::NotConsumed);
            }
            // A send in progress cannot observe cancellation: the provider may already hold the input.
            if let Some(hook) = &self.sending
                && hook(request.clone(), cancelled.clone()).await.is_err()
            {
                request.receipt.clone().record_failure(ProviderError::DeliveryUncertain).await?;
                return Err(ProviderError::DeliveryUncertain);
            }
            request.dispatch.confirm();
            let receipt = match self.admission.as_ref().map_or(Ok(None), |hook| hook(&request)) {
                Ok(receipt) => receipt,
                Err(error) => {
                    if matches!(error, ProviderError::DeliveryUncertain) { request.receipt.clone().record_failure(error.clone()).await?; }
                    else { request.receipt.clone().record_rejected(error.clone()).await?; }
                    return Err(error);
                }
            };
            request.receipt.clone().record_accepted(receipt).await?;
            if let Some(accepted) = self.accepted.lock().expect("fixture acceptance lock is not poisoned").take() { let _ = accepted.send(Ok(InputAcceptance { submission_id: request.submission_id.clone() })); }
            let writer = self.session.sink.activity_writer().invocation_writer(&request.scope)?;
            (self.handler)(request, writer, start, cancelled).await
        }.await;
        if let Some(accepted) = self
            .accepted
            .lock()
            .expect("fixture acceptance lock is not poisoned")
            .take()
        {
            let _ = accepted.send(Err(result.clone().err().unwrap_or_else(|| {
                fixture_error(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
            })));
        }
        result
    }
    /// Notify the original native callback without replacing its terminal classification.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        self.invocation.cancelled.send_replace(true);
        Ok(())
    }
    /// Collect original native resources before the real runner publishes completion.
    async fn collect(&self) -> Result<(), ProviderError> {
        if let Some(hook) = &self.cleanup {
            hook(self.session.binding.id.clone()).await?;
        }
        if let Some(hook) = &self.identity
            && let Some(provider_session_id) = hook(self.session.owner.clone()).await?
        {
            self.session
                .sink
                .activity_writer()
                .send_wait(AdapterActivity::Thread {
                    thread_id: self.session.owner.thread_id(),
                    event: ThreadEvent::IdentityResolved(provider_session_id),
                })
                .await?;
        }
        self.invocation.collected.store(true, Ordering::Release);
        Ok(())
    }
}

/// Execute deterministic normalized scripts while the adapter owns collection and exact targets.
pub struct EventFixtureProvider {
    myself: std::sync::Weak<Self>,
    operations: FixtureOperations,
    opened_operations: Mutex<HashMap<OperationId, BindingId>>,
    descriptor: ProviderDescriptor,
    handler: Arc<PromptHandler>,
    steering: std::sync::RwLock<Option<Arc<SteeringHandler>>>,
    models: std::sync::RwLock<Option<Vec<ProviderModel>>>,
    admission: std::sync::RwLock<Option<Arc<AdmissionHandler>>>,
    reconcile: std::sync::RwLock<Option<Arc<ReconcileHandler>>>,
    pre_dispatch: std::sync::RwLock<Option<Arc<PreDispatchHandler>>>,
    sending: std::sync::RwLock<Option<Arc<SendingHandler>>>,
    thread_open: std::sync::RwLock<Option<Arc<OpenHandler>>>,
    thread_configure: std::sync::RwLock<Option<Arc<ConfigureHandler>>>,
    configure_guard: std::sync::RwLock<Option<Arc<ConfigureHandler>>>,
    session_identity: std::sync::RwLock<Option<Arc<IdentityHandler>>>,
    cleanup: std::sync::RwLock<Option<Arc<CleanupHandler>>>,
    cleanup_request: std::sync::RwLock<Option<Arc<CleanupHandler>>>,
    shutdown: std::sync::RwLock<Option<Arc<ShutdownHandler>>>,
    thread_close: std::sync::RwLock<Option<Arc<CleanupHandler>>>,
    mcp: std::sync::RwLock<Option<Arc<McpHandler>>>,
    sessions: Mutex<HashMap<BindingId, Arc<FixtureSession>>>,
    owners: Mutex<HashMap<ThreadOwner, BindingId>>,
    closed_bindings: Mutex<HashSet<BindingId>>,
    closed: AtomicBool,
}

impl EventFixtureProvider {
    /// Create a fixture whose ordinary input handler supplies native observations.
    pub fn new<F, Fut>(descriptor: ProviderDescriptor, handler: F) -> Arc<Self>
    where
        F: Fn(PromptRequest, AdapterActivityWriter, NativeTurnStart, watch::Receiver<bool>) -> Fut
            + Send
            + Sync
            + 'static,
        Fut: Future<Output = Result<TurnResult, ProviderError>> + Send + 'static,
    {
        Arc::new_cyclic(|myself| Self {
            myself: myself.clone(),
            operations: Default::default(),
            opened_operations: Default::default(),
            descriptor,
            handler: Arc::new(move |request, sink, start, cancelled| {
                Box::pin(handler(request, sink, start, cancelled))
            }),
            steering: Default::default(),
            models: Default::default(),
            admission: Default::default(),
            reconcile: Default::default(),
            pre_dispatch: Default::default(),
            sending: Default::default(),
            thread_open: Default::default(),
            thread_configure: Default::default(),
            configure_guard: Default::default(),
            session_identity: Default::default(),
            cleanup: Default::default(),
            cleanup_request: Default::default(),
            shutdown: Default::default(),
            thread_close: Default::default(),
            mcp: Default::default(),
            sessions: Default::default(),
            owners: Default::default(),
            closed_bindings: Default::default(),
            closed: AtomicBool::new(false),
        })
    }

    /// Create the cohesive production registration while preserving fixture capability overrides.
    pub fn registration(self: &Arc<Self>) -> forge_providers::registry::RegisteredProvider {
        forge_providers::registry::RegisteredProvider::new(
            self.descriptor.clone(),
            self.clone(),
            None,
        )
    }

    /// Inspect or reject open policy before native resources are allocated.
    pub fn with_session_open<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(SessionRequest) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .thread_open
            .write()
            .expect("fixture open hook lock is not poisoned") =
            Some(Arc::new(move |request| Box::pin(handler(request))));
        self.clone()
    }

    /// Observe confirmed configuration before capturing its result watermark and capability snapshot.
    pub fn with_session_configure<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(BindingId, SessionConfiguration) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .thread_configure
            .write()
            .expect("fixture configure hook lock is not poisoned") =
            Some(Arc::new(move |binding, configuration| {
                Box::pin(handler(binding, configuration))
            }));
        self.clone()
    }

    /// Script delivery into the exact active invocation, without fallback submission.
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

    /// Advertise a normalized catalog without provider identity dispatch.
    pub fn with_models(self: &Arc<Self>, models: Vec<ProviderModel>) -> Arc<Self> {
        *self
            .models
            .write()
            .expect("fixture model lock is not poisoned") = Some(models);
        self.clone()
    }

    /// Supply deterministic native delivery evidence without resending input.
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

    /// Inject native acceptance, rejection, or uncertainty after sending evidence is persisted.
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

    /// Pause or reject while cancellation ownership is retained but dispatch has not occurred.
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

    /// Pause or fail after sending evidence is persisted and before the adapter confirms dispatch.
    /// The handler receives the invocation's interruption signal so a test can detect a stop
    /// requested while the send is in flight; the send itself cannot honor it.
    pub fn with_sending<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(PromptRequest, watch::Receiver<bool>) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .sending
            .write()
            .expect("fixture sending lock is not poisoned") =
            Some(Arc::new(move |request, interrupted| {
                Box::pin(handler(request, interrupted))
            }));
        self.clone()
    }

    /// Publish late durable identity through normalized binding observations during collection.
    pub fn with_session_identity<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(ThreadOwner) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Option<ProviderSessionId>, ProviderError>> + Send + 'static,
    {
        *self
            .session_identity
            .write()
            .expect("fixture identity lock is not poisoned") =
            Some(Arc::new(move |owner| Box::pin(handler(owner))));
        self.clone()
    }

    /// Gate automatic foreground collection after the native handler has returned.
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

    /// Request native interruption before joining the retained handler, with retry after failure.
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

    /// Gate provider-wide collection independently of individual binding closure.
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

    /// Gate full binding release after foreground settlement independently of invocation cleanup.
    pub fn with_session_close<F, Fut>(self: &Arc<Self>, handler: F) -> Arc<Self>
    where
        F: Fn(BindingId) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        *self
            .thread_close
            .write()
            .expect("fixture close hook lock is not poisoned") =
            Some(Arc::new(move |binding| Box::pin(handler(binding))));
        self.clone()
    }

    /// Clone a binding-scoped sink for capability, identity, and background observations.
    pub async fn event_sink(
        &self,
        owner: &ThreadOwner,
    ) -> Result<AdapterActivityWriter, ProviderError> {
        let binding = self
            .owners
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                fixture_error(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        Ok(self
            .session(&binding, ProviderOperation::TurnStart)
            .await?
            .sink
            .activity_writer())
    }

    /// Bind the original retained stream to an exact invocation for event injection.
    pub async fn invocation_sink(
        &self,
        scope: &InvocationScope,
    ) -> Result<AdapterActivityWriter, ProviderError> {
        self.event_sink(&scope.owner())
            .await?
            .invocation_writer(scope)
            .map_err(Into::into)
    }

    /// Resolve the exact live generation without an owner-key fallback.
    async fn session(
        &self,
        binding: &BindingId,
        operation: ProviderOperation,
    ) -> Result<Arc<FixtureSession>, ProviderError> {
        self.sessions
            .lock()
            .await
            .get(binding)
            .cloned()
            .ok_or_else(|| fixture_error(operation, ProviderFailureKind::SessionMissing))
    }

    /// Request interruption once after successful acknowledgement, retaining failure ownership for retry.
    async fn request_collection(
        &self,
        session: &FixtureSession,
        invocation: &FixtureInvocation,
    ) -> Result<(), ProviderError> {
        invocation.cancelled.send_replace(true);
        let mut requested = invocation.cleanup_request.lock().await;
        if *requested {
            return Ok(());
        }
        let handler = self
            .cleanup_request
            .read()
            .expect("fixture cleanup request lock is not poisoned")
            .clone();
        if let Some(handler) = handler {
            handler(session.binding.id.clone()).await?;
        }
        *requested = true;
        Ok(())
    }
}

impl FixtureInvocation {
    /// Wait for retained collection evidence independently of any stream consumer.
    async fn wait(&self) -> Result<CompletedTurn, ProviderError> {
        let mut receiver = self.settlement.subscribe();
        loop {
            if let Some(result) = receiver.borrow_and_update().clone() {
                return result;
            }
            if receiver.changed().await.is_err() {
                return Err(fixture_error(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::Runtime,
                ));
            }
        }
    }
}

impl EventFixtureProvider {
    async fn open_inner(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError> {
        context.check_admission(ProviderOperation::ThreadOpen)?;
        if self.closed.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let hook = self
            .thread_open
            .read()
            .expect("fixture open hook lock is not poisoned")
            .clone();
        if let Some(hook) = hook {
            tokio::select! {
                result = hook(request.clone()) => result?,
                reason = context.interrupted() => return Err(reason.failure(ProviderOperation::ThreadOpen, ProviderPhase::Preparation, FailureDisposition::Unchanged)),
            }
        }
        if matches!(request.history, SessionHistory::IsolatedText(_))
            && (!request.tools.is_empty()
                || request.tool_endpoint.is_some()
                || !request.configuration.resources.skills.is_empty()
                || !request.configuration.resources.mcp.is_empty())
        {
            return Err(fixture_error(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Policy,
            ));
        }
        let mut owners = self.owners.lock().await;
        context.check_admission(ProviderOperation::ThreadOpen)?;
        if self.closed.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let request_owner = request.owner();
        if owners.contains_key(&request_owner) {
            return Err(fixture_error(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let id = BindingId::new(uuid::Uuid::new_v4().to_string())
            .expect("UUID binding identity is nonempty");
        let history = request.history.resume_id().cloned().unwrap_or_else(|| {
            uuid::Uuid::new_v4()
                .to_string()
                .try_into()
                .expect("UUID history identity is nonempty")
        });
        let binding = SessionBinding {
            thread_id: request.thread_id.clone(),
            id: id.clone(),
            provider_session_id: Some(history),
        };
        let (sink, events) = events::channel(
            id.clone(),
            EventScope::Binding {
                thread_id: forge_provider_api::ThreadId::from(request.owner().clone()),
            },
        );
        let session = Arc::new(FixtureSession {
            binding: binding.clone(),
            owner: request.owner().clone(),
            sink,
            admission: Mutex::new(()),
            configuration: Mutex::new(request.configuration),
            state: Mutex::new(SessionState::default()),
            closed: AtomicBool::new(false),
            invalidated: AtomicBool::new(false),
            closing: Mutex::new(()),
        });
        let mut sessions = self.sessions.lock().await;
        let mut opened = self.opened_operations.lock().await;
        context.check_admission(ProviderOperation::ThreadOpen)?;
        if self.closed.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        opened.insert(context.id().clone(), id.clone());
        sessions.insert(id.clone(), session);
        owners.insert(request_owner, id);
        Ok(OpenedSession {
            binding,
            capabilities: CapabilityManifest::from_descriptor(self.descriptor.clone()),
            events,
        })
    }

    async fn configure_inner(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: SessionConfiguration,
    ) -> Result<ConfiguredSession, ProviderError> {
        context.check_admission(ProviderOperation::ThreadConfigure)?;
        let session = self
            .session(binding, ProviderOperation::ThreadConfigure)
            .await?;
        let _admission = session.admission.lock().await;
        if session.closed.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if session.invalidated.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::BindingInvalidated,
            ));
        }
        if session.state.lock().await.active.is_some() {
            return Err(fixture_error(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        context.check_admission(ProviderOperation::ThreadConfigure)?;
        let guard = self
            .configure_guard
            .read()
            .expect("fixture configure guard lock is not poisoned")
            .clone();
        if let Some(guard) = guard {
            guard(binding.clone(), configuration.clone()).await?;
        }
        context.check_admission(ProviderOperation::ThreadConfigure)?;
        *session.configuration.lock().await = configuration.clone();
        let hook = self
            .thread_configure
            .read()
            .expect("fixture configure hook lock is not poisoned")
            .clone();
        if let Some(hook) = hook {
            hook(binding.clone(), configuration.clone()).await?;
        }
        Ok(ConfiguredSession {
            configuration,
            capabilities: CapabilityManifest::from_descriptor(self.descriptor.clone()),
            watermark: session.sink.watermark(),
        })
    }

    async fn close_inner(
        &self,
        _context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        if self.closed_bindings.lock().await.contains(binding) {
            return Ok(());
        }
        let session = self
            .session(binding, ProviderOperation::ThreadClose)
            .await?;
        // Revoke authority before waiting for either admission or native cleanup.
        session.closed.store(true, Ordering::Release);
        let _closing = session.closing.lock().await;
        if self.closed_bindings.lock().await.contains(binding) {
            return Ok(());
        }
        let invocations: Vec<_> = session
            .state
            .lock()
            .await
            .invocations
            .values()
            .cloned()
            .collect();
        let mut failure = None;
        for invocation in invocations {
            let collection_pending = invocation.settlement.borrow().is_none();
            if collection_pending
                && let Err(error) = self.request_collection(&session, &invocation).await
            {
                failure.get_or_insert(error);
                continue;
            }
            // Await the retained native barrier; its historical failure stays in its own operation record.
            let _ = invocation.wait().await;
            let task = invocation.task.lock().await.take();
            if let Some(task) = task
                && task.await.is_err()
            {
                failure.get_or_insert_with(|| {
                    fixture_error(ProviderOperation::ThreadClose, ProviderFailureKind::Runtime)
                });
            }
            if !invocation.collected.load(Ordering::Acquire) {
                let cleanup = self
                    .cleanup
                    .read()
                    .expect("fixture cleanup lock is not poisoned")
                    .clone();
                let collected = if let Some(cleanup) = cleanup {
                    cleanup(binding.clone()).await
                } else {
                    Ok(())
                };
                match collected {
                    Ok(()) => invocation.collected.store(true, Ordering::Release),
                    Err(error) => {
                        failure.get_or_insert(error);
                    }
                }
            }
        }
        if let Some(error) = failure {
            return Err(error);
        }
        // Release the binding only when every retained collection phase has completed.
        let close = self
            .thread_close
            .read()
            .expect("fixture close hook lock is not poisoned")
            .clone();
        if let Some(close) = close {
            close(binding.clone()).await?;
        }
        self.sessions.lock().await.remove(binding);
        let mut owners = self.owners.lock().await;
        if owners.get(&session.owner) == Some(binding) {
            owners.remove(&session.owner);
        }
        self.closed_bindings.lock().await.insert(binding.clone());
        Ok(())
    }

    async fn cancel_inner(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<TurnInterruptResult, ProviderError> {
        let session = self
            .session(binding, ProviderOperation::TurnInterrupt)
            .await?;
        let invocation = session
            .state
            .lock()
            .await
            .invocations
            .get(&request.scope.invocation_id)
            .cloned()
            .filter(|invocation| invocation.scope == request.scope)
            .ok_or_else(|| {
                fixture_error(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        if invocation.settlement.borrow().is_none() {
            self.request_collection(&session, &invocation).await?;
        }
        match invocation.wait().await {
            Ok(_) | Err(ProviderError::NotConsumed) => {}
            Err(ProviderError::Cancelled)
                if !invocation.started.load(Ordering::Acquire)
                    && invocation.collected.load(Ordering::Acquire) => {}
            Err(_) if invocation.collected.load(Ordering::Acquire) => {}
            Err(error) => return Err(error),
        }
        Ok(TurnInterruptResult {
            operation_id: context.id().clone(),
            thread_id: request.scope.owner().thread_id(),
            turn_id: request.scope.turn_id().map_err(|_| {
                fixture_error(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::WrongInvocation,
                )
            })?,
            watermark: session.sink.watermark(),
        })
    }

    async fn submit_inner(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        context.check_admission(ProviderOperation::TurnStart)?;
        // Reserve the exact invocation before any cancellable preparation or transport work.
        let session = self.session(binding, ProviderOperation::TurnStart).await?;
        let _admission = session.admission.lock().await;
        context.check_admission(ProviderOperation::TurnStart)?;
        if self.closed.load(Ordering::Acquire) || session.closed.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::TurnStart,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if session.invalidated.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::TurnStart,
                ProviderFailureKind::BindingInvalidated,
            ));
        }
        if request.scope.owner() != session.owner
            || request.receipt.id() != &request.submission_id
            || request.receipt.attempt_id()? != request.attempt_id
            || !request.dispatch.matches(
                binding,
                &request.scope,
                &request.submission_id,
                &request.attempt_id,
            )
        {
            return Err(fixture_error(
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if session.state.lock().await.active.is_some() {
            return Err(fixture_error(
                ProviderOperation::TurnStart,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let (cancelled, _cancel_receiver) = watch::channel(false);
        let invocation = Arc::new(FixtureInvocation {
            scope: request.scope.clone(),
            submission_id: request.submission_id.clone(),
            attempt_id: request.attempt_id.clone(),
            cancelled,
            settlement: watch::channel(None).0,
            task: Mutex::new(None),
            cleanup_request: Mutex::new(false),
            collected: AtomicBool::new(false),
            started: Arc::new(AtomicBool::new(false)),
        });
        {
            let mut state = session.state.lock().await;
            if state.invocations.contains_key(&request.scope.invocation_id) {
                return Err(fixture_error(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::WrongInvocation,
                ));
            }
            context.check_admission(ProviderOperation::TurnStart)?;
            state.history_boundary = None;
            state.active = Some(request.scope.invocation_id.clone());
            state
                .invocations
                .insert(request.scope.invocation_id.clone(), invocation.clone());
        }
        let acceptance = InputAcceptance {
            submission_id: request.submission_id.clone(),
        };
        let (accepted, acceptance_receiver) = tokio::sync::oneshot::channel();
        let driver = Arc::new(FixtureDriver {
            request: request.clone(),
            session: session.clone(),
            invocation: invocation.clone(),
            handler: self.handler.clone(),
            pre_dispatch: self
                .pre_dispatch
                .read()
                .expect("fixture hook lock is not poisoned")
                .clone(),
            sending: self
                .sending
                .read()
                .expect("fixture hook lock is not poisoned")
                .clone(),
            admission: self
                .admission
                .read()
                .expect("fixture hook lock is not poisoned")
                .clone(),
            cleanup: self
                .cleanup
                .read()
                .expect("fixture hook lock is not poisoned")
                .clone(),
            identity: self
                .session_identity
                .read()
                .expect("fixture hook lock is not poisoned")
                .clone(),
            context: context.clone(),
            accepted: std::sync::Mutex::new(Some(accepted)),
        });
        let runner = Arc::new(forge_providers::testing::RetainedTurn::spawn(
            driver,
            session
                .sink
                .clone()
                .with_observer(Arc::new(FixtureStartObserver(invocation.started.clone()))),
            TurnAddress {
                scope: request.scope.clone(),
                thread_id: request.thread_id.clone(),
                turn_id: request.turn_id.clone(),
            },
        )?);
        let worker_runner = runner.clone();
        let task_session = session.clone();
        let task_invocation = invocation.clone();
        let task = tokio::spawn(async move {
            let result = worker_runner.join().await;
            let unconsumed = !task_invocation.started.load(Ordering::Acquire)
                && task_invocation.collected.load(Ordering::Acquire)
                && match &result {
                    Err(
                        ProviderError::NotConsumed
                        | ProviderError::Cancelled
                        | ProviderError::PreparationFailed(_),
                    ) => true,
                    Err(ProviderError::Failure(failure)) => {
                        failure.disposition() == FailureDisposition::Unchanged
                    }
                    Ok(_)
                    | Err(
                        ProviderError::RuntimeVersion(_)
                        | ProviderError::RuntimeLaunch { .. }
                        | ProviderError::Unsupported(_)
                        | ProviderError::Delivery(_)
                        | ProviderError::Contract(_)
                        | ProviderError::Observation(_)
                        | ProviderError::Cleanup(_)
                        | ProviderError::DeliveryUncertain
                        | ProviderError::Retryable(_)
                        | ProviderError::Combined { .. }
                        | ProviderError::PreparationCleanup { .. },
                    ) => false,
                };
            if result.is_ok() || unconsumed {
                let mut state = task_session.state.lock().await;
                if state.active.as_ref() == Some(&request.scope.invocation_id) {
                    state.active = None;
                }
                if let Ok(completed) = &result {
                    state.history_boundary =
                        completed.result().checkpoint_id().cloned().map(|boundary| {
                            (
                                request.scope.clone(),
                                request.submission_id.clone(),
                                request.attempt_id.clone(),
                                boundary,
                            )
                        });
                }
            } else {
                task_session.invalidated.store(true, Ordering::Release);
            }
            task_invocation.settlement.send_replace(Some(result));
        });
        *invocation.task.lock().await = Some(task);
        drop(_admission);
        // A native panic may bypass acceptance publication; retain its actual result instead of
        // leaving the submission operation waiting on a sender owned by the completed driver.
        tokio::select! {
            biased;
            accepted = acceptance_receiver => accepted.unwrap_or_else(|_| Err(fixture_error(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Runtime,
            ))),
            completed = runner.join() => completed.map(|_| acceptance.clone()),
        }?;
        runner.started().await?;
        Ok(acceptance)
    }

    async fn steer_inner(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        context.check_admission(ProviderOperation::TurnSteer)?;
        if !self.descriptor.capability.native_steer {
            return Err(UnsupportedCapability(ProviderFeature::NativeSteer).into());
        }
        let session = self.session(binding, ProviderOperation::TurnSteer).await?;
        let _admission = session.admission.lock().await;
        if session.closed.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::TurnSteer,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if session.invalidated.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::TurnSteer,
                ProviderFailureKind::BindingInvalidated,
            ));
        }
        let state = session.state.lock().await;
        if state.active.as_ref() != Some(&request.scope.invocation_id)
            || request.scope.owner() != session.owner
        {
            return Err(ProviderError::NotConsumed);
        }
        let invocation = state
            .invocations
            .get(&request.scope.invocation_id)
            .cloned()
            .filter(|invocation| invocation.scope == request.scope)
            .ok_or_else(|| {
                fixture_error(
                    ProviderOperation::TurnSteer,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        if invocation.settlement.borrow().is_some() {
            return Err(ProviderError::NotConsumed);
        }
        drop(state);
        let receipt = request.delivery.receipt().cloned();
        if let Some(receipt) = &receipt {
            let token = RecoveryToken::new(
                format!(
                    "fixture-steer:{}:{}",
                    receipt.id().as_str(),
                    receipt.attempt_id()?.as_str()
                )
                .into_bytes(),
            )
            .expect("fixture steering token is nonempty");
            receipt.clone().record_sending(Some(token)).await?;
        }
        let hook = self
            .steering
            .read()
            .expect("fixture steering lock is not poisoned")
            .clone();
        let delivered = if let Some(hook) = hook {
            tokio::select! {
                result = hook(request) => result,
                reason = context.interrupted() => Err(reason.failure(
                    ProviderOperation::TurnSteer,
                    ProviderPhase::Observation,
                    FailureDisposition::Uncertain,
                )),
            }
        } else {
            Ok(())
        };
        if let Err(error) = delivered {
            let definitely_not_delivered = matches!(
                &error,
                ProviderError::NotConsumed | ProviderError::PreparationFailed(_)
            ) || matches!(&error, ProviderError::Failure(failure)
                if failure.operation() == ProviderOperation::TurnSteer
                    && matches!(failure.phase(), ProviderPhase::Admission | ProviderPhase::Preparation)
                    && failure.disposition() == FailureDisposition::Unchanged);
            if let Some(receipt) = &receipt {
                let recorded = if definitely_not_delivered {
                    receipt.clone().record_rejected(error.clone()).await
                } else {
                    receipt.clone().record_failure(error.clone()).await
                };
                if let Err(persistence) = recorded {
                    return Err(ProviderError::Combined {
                        primary: Box::new(error),
                        secondary: vec![persistence],
                    });
                }
            }
            return Err(error);
        }
        if let Some(receipt) = receipt {
            let id = receipt.id().clone();
            receipt.record_accepted(None).await?;
            return Ok(SteeringAcceptance::Journaled { submission_id: id });
        }
        Ok(SteeringAcceptance::Context)
    }

    async fn reconcile_inner(
        &self,
        context: OperationContext,
        request: InputReconcileRequest,
    ) -> Result<SubmissionEvidence, ProviderError> {
        context.check_admission(ProviderOperation::InputReconcile)?;
        let hook = self
            .reconcile
            .read()
            .expect("fixture reconciliation lock is not poisoned")
            .clone();
        Ok(hook.map_or(SubmissionEvidence::Unknown, |hook| {
            hook(
                &request.submission_id,
                request.token.as_ref(),
                &request.accepted_receipts,
            )
        }))
    }

    /// Retract only the exact cancelled input proven by the latest retained native boundary.
    async fn retract_inner(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: InputRetractRequest,
    ) -> Result<InputRetractOutcome, ProviderError> {
        context.check_admission(ProviderOperation::InputRetract)?;
        if !self.descriptor.capability.native_turn_rollback {
            return Ok(InputRetractOutcome::Unavailable(
                InputRetractUnavailable::Unsupported,
            ));
        }
        let session = self
            .session(binding, ProviderOperation::InputRetract)
            .await?;
        let _admission = session.admission.lock().await;
        if session.closed.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::InputRetract,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if session.invalidated.load(Ordering::Acquire) {
            return Err(fixture_error(
                ProviderOperation::InputRetract,
                ProviderFailureKind::BindingInvalidated,
            ));
        }
        let mut state = session.state.lock().await;
        if state.active.is_some() {
            return Err(fixture_error(
                ProviderOperation::InputRetract,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let invocation = state.invocations.get(&request.scope.invocation_id).cloned();
        let Some(invocation) = invocation.filter(|invocation| {
            invocation.scope == request.scope
                && invocation.submission_id == request.submission_id
                && invocation.attempt_id == request.attempt_id
        }) else {
            return Ok(InputRetractOutcome::Unavailable(
                InputRetractUnavailable::WrongInvocation,
            ));
        };
        let settlement = invocation.settlement.borrow().clone();
        let Some(Ok(completed)) = settlement else {
            return Ok(InputRetractOutcome::Unavailable(
                InputRetractUnavailable::NotCancelled,
            ));
        };
        if completed.result().outcome() != turn::TurnOutcome::Interrupted {
            return Ok(InputRetractOutcome::Unavailable(
                InputRetractUnavailable::NotCancelled,
            ));
        }
        if completed.result().checkpoint_id() != Some(&request.boundary)
            || state.history_boundary.as_ref()
                != Some(&(
                    request.scope.clone(),
                    request.submission_id.clone(),
                    request.attempt_id.clone(),
                    request.boundary.clone(),
                ))
        {
            return Ok(InputRetractOutcome::Unavailable(
                InputRetractUnavailable::HistoryBoundaryChanged,
            ));
        }
        match request.authorization {
            RetractionAuthorization::OutputFreeCancellation => {}
        }
        context.check_admission(ProviderOperation::InputRetract)?;
        state.history_boundary = None;
        Ok(InputRetractOutcome::Retracted {
            boundary: request.boundary,
        })
    }

    async fn shutdown_inner(&self, context: OperationContext) -> Result<(), ProviderError> {
        self.closed.store(true, Ordering::Release);
        self.operations.close_admission();
        let hook = self
            .shutdown
            .read()
            .expect("fixture shutdown lock is not poisoned")
            .clone();
        let mut failure = if let Some(hook) = hook {
            hook().await.err()
        } else {
            None
        };
        let bindings: Vec<_> = self.sessions.lock().await.keys().cloned().collect();
        for binding in bindings {
            if let Err(error) = self.close_inner(context.clone(), &binding).await {
                failure.get_or_insert(error);
            }
        }
        if let Err(error) = self.operations.join_all(context.id()).await {
            failure.get_or_insert(error);
        }
        match failure {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    async fn models_inner(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        context.check_admission(ProviderOperation::ModelList)?;
        if let QueryContext::Binding(binding) = query {
            self.session(&binding, ProviderOperation::ModelList).await?;
        }
        self.models
            .read()
            .expect("fixture model lock is not poisoned")
            .clone()
            .ok_or_else(|| UnsupportedCapability(ProviderFeature::ModelSelection).into())
    }

    async fn mcp_inner(
        &self,
        context: OperationContext,
        query: McpQuery,
    ) -> Result<Vec<McpDefinition>, ProviderError> {
        context.check_admission(ProviderOperation::McpList)?;
        let context = match &query {
            McpQuery::Configured(context) | McpQuery::Observed(context) => context,
        };
        if let QueryContext::Binding(binding) = context {
            self.session(binding, ProviderOperation::McpList).await?;
        }
        let hook = self
            .mcp
            .read()
            .expect("fixture MCP lock is not poisoned")
            .clone()
            .ok_or(UnsupportedCapability(ProviderFeature::McpCatalog))?;
        hook(query).await
    }
}

#[async_trait::async_trait]
impl Provider for EventFixtureProvider {
    async fn thread_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let cleanup_owner = owner.clone();
        let cleanup_context = context.clone();
        let work_context = context.clone();
        let work_request = request.clone();
        self.operations
            .open
            .take(
                context,
                ProviderOperation::ThreadOpen,
                request.identity(),
                async move { owner.open_inner(work_context, work_request).await },
                move |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::ThreadOpen,
                            ProviderPhase::Preparation,
                            FailureDisposition::Uncertain,
                        ),
                        async move {
                            let binding = cleanup_owner
                                .opened_operations
                                .lock()
                                .await
                                .get(cleanup_context.id())
                                .cloned();
                            match binding {
                                Some(binding) => {
                                    cleanup_owner.close_inner(cleanup_context, &binding).await
                                }
                                None => Ok(()),
                            }
                        },
                    )
                },
            )
            .await
    }

    async fn thread_configure(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: SessionConfiguration,
    ) -> Result<ConfiguredSession, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let payload = (binding.clone(), configuration.clone());
        let work_context = context.clone();
        let binding = binding.clone();
        self.operations
            .configure
            .observe(
                context,
                ProviderOperation::ThreadConfigure,
                payload,
                async move {
                    owner
                        .configure_inner(work_context, &binding, configuration)
                        .await
                },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::ThreadConfigure,
                            ProviderPhase::Preparation,
                            FailureDisposition::Uncertain,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn thread_fork(
        &self,
        context: OperationContext,
        request: SessionForkRequest,
    ) -> Result<ProviderForkResult, ProviderError> {
        self.operations
            .fork
            .observe(
                context,
                ProviderOperation::ThreadFork,
                request,
                async { Err(UnsupportedCapability(ProviderFeature::NativeFork).into()) },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::ThreadFork,
                            ProviderPhase::Admission,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn thread_compact(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: provider::CompactionRequest,
    ) -> Result<operations::CompactionResult, ProviderError> {
        request.validate_context(&context)?;
        self.operations
            .compact
            .observe(
                context,
                ProviderOperation::ThreadCompact,
                (binding.clone(), request),
                async { Err(UnsupportedCapability(ProviderFeature::NativeCompact).into()) },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::ThreadCompact,
                            ProviderPhase::Admission,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn thread_close(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let work_context = context.clone();
        let payload = binding.clone();
        let binding = binding.clone();
        self.operations
            .close
            .observe(
                context,
                ProviderOperation::ThreadClose,
                payload,
                async move { owner.close_inner(work_context, &binding).await },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::ThreadClose,
                            ProviderPhase::Close,
                            FailureDisposition::Invalidated,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn turn_interrupt(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<TurnInterruptResult, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let payload = (binding.clone(), request.clone());
        let work_context = context.clone();
        let binding = binding.clone();
        self.operations
            .cancel
            .observe(
                context,
                ProviderOperation::TurnInterrupt,
                payload,
                async move { owner.cancel_inner(work_context, &binding, request).await },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::TurnInterrupt,
                            ProviderPhase::Collection,
                            FailureDisposition::Uncertain,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn turn_start(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let cleanup_owner = owner.clone();
        let cleanup_context = context.clone();
        let cleanup_binding = binding.clone();
        let cleanup_scope = request.scope.clone();
        let payload = (binding.clone(), request.identity());
        let work_context = context.clone();
        let binding = binding.clone();
        self.operations
            .submit
            .observe(
                context,
                ProviderOperation::TurnStart,
                payload,
                async move { owner.submit_inner(work_context, &binding, request).await },
                move |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::TurnStart,
                            ProviderPhase::Observation,
                            FailureDisposition::Uncertain,
                        ),
                        async move {
                            let session = cleanup_owner
                                .session(&cleanup_binding, ProviderOperation::TurnStart)
                                .await?;
                            let admitted = session
                                .state
                                .lock()
                                .await
                                .invocations
                                .get(&cleanup_scope.invocation_id)
                                .is_some_and(|invocation| invocation.scope == cleanup_scope);
                            if !admitted {
                                return Ok(());
                            }
                            cleanup_owner
                                .cancel_inner(
                                    cleanup_context,
                                    &cleanup_binding,
                                    CancelRequest {
                                        scope: cleanup_scope,
                                    },
                                )
                                .await
                                .map(|_| ())
                        },
                    )
                },
            )
            .await
    }

    async fn turn_steer(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let payload = (binding.clone(), request.identity());
        let work_context = context.clone();
        let binding = binding.clone();
        self.operations
            .steer
            .observe(
                context,
                ProviderOperation::TurnSteer,
                payload,
                async move { owner.steer_inner(work_context, &binding, request).await },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::TurnSteer,
                            ProviderPhase::Observation,
                            FailureDisposition::Uncertain,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn input_reconcile(
        &self,
        context: OperationContext,
        request: InputReconcileRequest,
    ) -> Result<SubmissionEvidence, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let work_context = context.clone();
        let payload = request.clone();
        self.operations
            .reconcile
            .observe(
                context,
                ProviderOperation::InputReconcile,
                payload,
                async move { owner.reconcile_inner(work_context, request).await },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::InputReconcile,
                            ProviderPhase::Observation,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn input_retract(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: InputRetractRequest,
    ) -> Result<InputRetractOutcome, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let payload = (binding.clone(), request.clone());
        let work_context = context.clone();
        let binding = binding.clone();
        self.operations
            .retract
            .observe(
                context,
                ProviderOperation::InputRetract,
                payload,
                async move { owner.retract_inner(work_context, &binding, request).await },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::InputRetract,
                            ProviderPhase::Observation,
                            FailureDisposition::Uncertain,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn shutdown(&self, context: OperationContext) -> Result<(), ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let work_context = context.clone();
        self.operations
            .shutdown
            .observe(
                context,
                ProviderOperation::Shutdown,
                (),
                async move { owner.shutdown_inner(work_context).await },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::Shutdown,
                            ProviderPhase::Close,
                            FailureDisposition::Invalidated,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn model_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let payload = query.clone();
        let work_context = context.clone();
        self.operations
            .models
            .observe(
                context,
                ProviderOperation::ModelList,
                payload,
                async move { owner.models_inner(work_context, query).await },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::ModelList,
                            ProviderPhase::Observation,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn skill_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<catalog::SkillDefinition>, ProviderError> {
        self.operations
            .skills
            .observe(
                context,
                ProviderOperation::SkillList,
                query,
                async { Err(UnsupportedCapability(ProviderFeature::SkillCatalog).into()) },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::SkillList,
                            ProviderPhase::Admission,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn mcp_list(
        &self,
        context: OperationContext,
        query: McpQuery,
    ) -> Result<Vec<McpDefinition>, ProviderError> {
        let owner = self
            .myself
            .upgrade()
            .expect("fixture operation starts through its live Arc owner");
        let payload = query.clone();
        let work_context = context.clone();
        self.operations
            .mcp
            .observe(
                context,
                ProviderOperation::McpList,
                payload,
                async move { owner.mcp_inner(work_context, query).await },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::McpList,
                            ProviderPhase::Observation,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn terminal_list(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<terminal::TerminalInventory, ProviderError> {
        self.operations
            .terminals
            .observe(
                context,
                ProviderOperation::TerminalList,
                binding.clone(),
                async { Err(UnsupportedCapability(ProviderFeature::BackgroundTerminals).into()) },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::TerminalList,
                            ProviderPhase::Admission,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn terminal_terminate(
        &self,
        context: OperationContext,
        binding: &BindingId,
        terminal: &terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        self.operations
            .terminal_terminate
            .observe(
                context,
                ProviderOperation::TerminalTerminate,
                (binding.clone(), terminal.clone()),
                async { Err(UnsupportedCapability(ProviderFeature::BackgroundTermination).into()) },
                |reason| {
                    Interruption::new(
                        reason.failure(
                            ProviderOperation::TerminalTerminate,
                            ProviderPhase::Admission,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }
}

/// Construct a normalized failure at the operation which actually rejected fixture work.
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
            ProviderOperation::TurnStart,
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
    Ok(serde_json::from_str(&output).unwrap_or_else(|_| serde_json::Value::String(output)))
}
