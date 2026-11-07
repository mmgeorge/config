use super::{
    AcpKind,
    conversation::{AcpConversation, ConversationOptions},
    runtime::AcpRuntime,
    session::AcpSessionState,
};
use crate::pending_input::{PendingInput, PendingInputs};
use async_trait::async_trait;
use forge_provider_api::ThreadId;
use forge_provider_api::events::{self, ProviderEventSink};
use forge_provider_api::retained_operation::{IdentityRegistry, Interruption, Store};
use forge_provider_api::session::{
    BindingId, InputAcceptance, PromptRequest, SessionBinding, SessionRequest, SteeringRequest,
};
use forge_provider_api::{FailureDisposition, OperationContext, OperationId, ProviderPhase};
use forge_provider_api::{ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation};
use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};
use tokio::sync::{Mutex, Notify, RwLock};

struct AcpBinding {
    binding: SessionBinding,
    owner: ThreadId,
    conversation: Arc<AcpConversation>,
    state: Arc<AcpSessionState>,
    sink: ProviderEventSink,
    cleanup: super::connection::AcpCleanup,
    profiles: Mutex<Vec<Arc<super::connection::AcpProfile>>>,
    selection: Mutex<forge_provider_api::session::ResourceSelection>,
    native_model: Mutex<forge_provider_api::ModelSelection>,
    initial_model: forge_provider_api::ModelSelection,
    operation: Mutex<()>,
    steering: RwLock<()>,
    covered_inputs: std::sync::Mutex<
        HashMap<forge_provider_api::InvocationScope, Vec<forge_provider_api::CoveredInput>>,
    >,
    active: std::sync::Mutex<Option<ActiveExecution>>,
    settlements: std::sync::Mutex<
        HashMap<
            forge_provider_api::InvocationScope,
            Arc<OperationRecord<forge_provider_api::CompletedTurn>>,
        >,
    >,
    collection_watermarks: std::sync::Mutex<
        HashMap<forge_provider_api::InvocationScope, forge_provider_api::EventSequence>,
    >,
    close_task: Mutex<Option<Arc<crate::retained_task::RetainedTask>>>,
    history_released: AtomicBool,
    rejected_inputs: Mutex<
        Vec<(
            forge_provider_api::InvocationScope,
            forge_provider_api::SubmissionId,
            forge_provider_api::DeliveryAttemptId,
        )>,
    >,
}

impl AcpBinding {
    /// Add accepted steering coverage only to its original retained prompt.
    fn cover_input(
        &self,
        scope: &forge_provider_api::InvocationScope,
        input: forge_provider_api::CoveredInput,
    ) -> Result<(), ProviderError> {
        let mut inputs = self
            .covered_inputs
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let covered = inputs
            .get_mut(scope)
            .ok_or_else(|| failure(ProviderOperation::TurnSteer, ProviderFailureKind::Protocol))?;
        if let Some(retained) = covered
            .iter()
            .find(|retained| retained.submission_id() == input.submission_id())
        {
            if retained != &input {
                return Err(failure(
                    ProviderOperation::TurnSteer,
                    ProviderFailureKind::Protocol,
                ));
            }
        } else {
            covered.push(input);
        }
        Ok(())
    }

    /// Check whether native input preparation or a prompt worker is still running.
    fn has_active_prompt(&self) -> bool {
        self.active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .is_some()
    }

    /// Derive current binding support from the native handshake and retained catalog.
    async fn capabilities(&self) -> forge_provider_api::capability::CapabilityManifest {
        if let Some(manifest) = self.state.effective_capabilities().await {
            return manifest;
        }
        let mut descriptor =
            crate::registry::descriptor(&self.state.kind_id()).expect("ACP kind is registered");
        descriptor.capability = self.conversation.capabilities(descriptor.capability).await;
        let mut manifest =
            forge_provider_api::capability::CapabilityManifest::from_descriptor(descriptor.clone());
        manifest.provider_version = self.conversation.version();
        manifest
    }

    /// Revoke new work before asynchronous transport collection begins.
    async fn invalidate(&self, cause: ProviderError) {
        self.state.invalidate_binding(cause).await;
        self.state.revoke_callbacks().await;
    }

    /// Fence post-write results after the final await against interruption and pending collection.
    fn ensure_current_result(
        &self,
        context: &OperationContext,
        operation: ProviderOperation,
    ) -> Result<(), ProviderError> {
        if let Err(error) = context.check_admission(operation) {
            self.state.close_admission();
            return Err(operation_failure(
                operation,
                &error,
                ProviderPhase::Observation,
                FailureDisposition::Invalidated,
            ));
        }
        match self.state.ensure_admission(operation) {
            Ok(()) => Ok(()),
            Err(ProviderError::Failure(failure)) => {
                Err(ProviderFailure::new(operation, failure.kind())
                    .at_phase(ProviderPhase::Observation, failure.disposition())
                    .into())
            }
            Err(error) => Err(error),
        }
    }

    /// Wait for exactly one retained invocation's collector without selecting current work.
    async fn settlement(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<forge_provider_api::CompletedTurn, ProviderError> {
        let record = self
            .settlements
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(scope)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        record.wait().await
    }
}

/// Retain one operation's immutable completion across dropped and repeated awaits.
struct OperationRecord<T> {
    result: Mutex<Option<Result<T, ProviderError>>>,
    ready: Notify,
}

impl<T: Clone> OperationRecord<T> {
    /// Create pending result storage before native mutation can begin.
    fn new() -> Self {
        Self {
            result: Mutex::new(None),
            ready: Notify::new(),
        }
    }
    /// Publish one final result before waking every retrying observer.
    async fn complete(&self, result: Result<T, ProviderError>) {
        *self.result.lock().await = Some(result);
        self.ready.notify_waiters();
    }
    /// Observe the retained operation without becoming its execution owner.
    async fn wait(&self) -> Result<T, ProviderError> {
        loop {
            let ready = self.ready.notified();
            if let Some(result) = self.result.lock().await.clone() {
                return result;
            }
            ready.await;
        }
    }
}

/// Retain the provider's complete typed operation namespace independently of observers.
struct AcpOperations {
    identities: Arc<IdentityRegistry>,
    open: Store<forge_provider_api::SessionRequestIdentity, forge_provider_api::OpenedSession>,
    configure: Store<
        (BindingId, forge_provider_api::SessionConfiguration),
        forge_provider_api::ConfiguredSession,
    >,
    fork: Store<forge_provider_api::SessionForkRequest, forge_provider_api::ProviderForkResult>,
    compact: Store<
        (BindingId, forge_provider_api::CompactionRequest),
        forge_provider_api::CompactionResult,
    >,
    close: Store<BindingId, ()>,
    cancel: Store<
        (BindingId, forge_provider_api::CancelRequest),
        forge_provider_api::TurnInterruptResult,
    >,
    submit: Store<(BindingId, forge_provider_api::PromptRequestIdentity), InputAcceptance>,
    steer: Store<
        (BindingId, forge_provider_api::SteeringRequestIdentity),
        forge_provider_api::SteeringAcceptance,
    >,
    reconcile:
        Store<forge_provider_api::InputReconcileRequest, forge_provider_api::SubmissionEvidence>,
    retract: Store<
        (BindingId, forge_provider_api::InputRetractRequest),
        forge_provider_api::InputRetractOutcome,
    >,
    inspect:
        Store<forge_provider_api::catalog::WorkspaceContext, forge_provider_api::ProviderSetup>,
    authenticate: Store<
        (
            forge_provider_api::catalog::WorkspaceContext,
            forge_provider_api::provider::AuthenticationMethodId,
        ),
        (),
    >,
    shutdown: Store<(), ()>,
    models:
        Store<forge_provider_api::catalog::QueryContext, Vec<forge_provider_api::ProviderModel>>,
    skills: Store<
        forge_provider_api::catalog::QueryContext,
        Vec<forge_provider_api::catalog::SkillDefinition>,
    >,
    mcp: Store<
        forge_provider_api::catalog::McpQuery,
        Vec<forge_provider_api::catalog::McpDefinition>,
    >,
    terminals: Store<BindingId, forge_provider_api::terminal::TerminalInventory>,
    terminate: Store<(BindingId, forge_provider_api::terminal::TerminalId), ()>,
}

impl AcpOperations {
    /// Create typed stores sharing one immutable operation identity ledger.
    fn new() -> Self {
        let identities = Arc::new(IdentityRegistry::new());
        Self {
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
            inspect: Store::registered(identities.clone()),
            authenticate: Store::registered(identities.clone()),
            shutdown: Store::registered(identities.clone()),
            models: Store::registered(identities.clone()),
            skills: Store::registered(identities.clone()),
            mcp: Store::registered(identities.clone()),
            terminals: Store::registered(identities.clone()),
            terminate: Store::registered(identities.clone()),
            identities,
        }
    }

    /// Close provider-wide operation admission while preserving retained exact replays.
    fn close_admission(&self) {
        self.identities.close_admission();
    }

    /// Join operation workers after native producers stop, excluding the owning shutdown operation.
    async fn join_all_except(&self, id: &OperationId) -> Result<(), ProviderError> {
        let mut failures = Vec::new();
        if let Err(error) = self.open.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.configure.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.fork.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.compact.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.close.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.cancel.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.submit.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.steer.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.reconcile.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.retract.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.inspect.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.authenticate.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.models.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.skills.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.mcp.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.terminals.join_all_except(id).await {
            failures.push(error);
        }
        if let Err(error) = self.terminate.join_all_except(id).await {
            failures.push(error);
        }
        if failures.is_empty() {
            Ok(())
        } else {
            Err(combine_errors(failures))
        }
    }
}

/// Preserve known durable-history ownership while an open has not produced a public binding.
#[derive(Clone)]
struct HistoryReservation {
    operation_id: OperationId,
    owner: ThreadId,
    cleanup: super::connection::AcpCleanup,
}

/// Own one original native prompt and its callback collection under the shared runner.
struct AcpTurnDriver {
    session: Arc<AcpBinding>,
    scope: forge_provider_api::InvocationScope,
    invocation: Mutex<Option<PromptInvocation>>,
}

#[async_trait]
impl crate::turn_runner::NativeTurnDriver for AcpTurnDriver {
    /// Consume original prompt admission once while preserving the separate receipt result.
    async fn run(
        &self,
        start: forge_provider_api::NativeTurnStart,
    ) -> Result<forge_provider_api::TurnResult, ProviderError> {
        let PromptInvocation {
            request,
            accepted,
            admission,
        } = self.invocation.lock().await.take().ok_or_else(|| {
            failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            )
        })?;
        let _ = admission.await;
        run_prompt(self.session.clone(), request, accepted, start).await
    }

    /// Interrupt only this retained native prompt when its original scope is still active.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        let mut active = self
            .session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some(active) = active.as_mut().filter(|active| active.scope == self.scope) {
            active.cancelled = true;
            active.cancellation.cancel();
        }
        Ok(())
    }

    /// Join original invocation callbacks and the existing native collection lock before publication.
    async fn collect(&self) -> Result<(), ProviderError> {
        let _steering = self.session.steering.write().await;
        self.session
            .state
            .revoke_invocation_callbacks(&self.scope)
            .await;
        let _operation = self.session.operation.lock().await;
        if let Some(error) = self.session.state.binding_failure().await {
            return Err(error);
        }
        Ok(())
    }
}

/// Retain immutable prompt admission and its independent acceptance response until native run starts.
struct PromptInvocation {
    request: PromptRequest,
    accepted: tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>,
    admission: tokio::sync::oneshot::Receiver<()>,
}

struct ActiveExecution {
    scope: forge_provider_api::InvocationScope,
    cancelled: bool,
    cancellation: tokio_util::sync::CancellationToken,
    task: Option<Arc<crate::turn_runner::RetainedTurn>>,
}

/// Own ACP processes, retained conversations, native event decoding, and completion.
#[derive(Clone)]
pub struct AcpProvider {
    kind: AcpKind,
    runtime: Arc<AcpRuntime>,
    cleanup: super::connection::AcpCleanup,
    runtime_root: PathBuf,
    bindings: Arc<std::sync::Mutex<HashMap<BindingId, Arc<AcpBinding>>>>,
    owners: Arc<Mutex<HashMap<ThreadId, BindingId>>>,
    preparation: Arc<Mutex<HashMap<ThreadId, Arc<Mutex<()>>>>>,
    history_admission: Arc<Mutex<()>>,
    history_reservations:
        Arc<Mutex<HashMap<forge_provider_api::ProviderSessionId, Vec<HistoryReservation>>>>,
    admission: Arc<AtomicBool>,
    shutdown_task: Arc<Mutex<Option<Arc<crate::retained_task::RetainedTask>>>>,
    operations: Arc<AcpOperations>,
    pending_inputs: Arc<PendingInputs>,
}

impl AcpProvider {
    /// Create a native ACP owner from the registry identity and configured launch command.
    pub fn new(
        id: forge_provider_api::ProviderId,
        command: Vec<String>,
        runtime_root: PathBuf,
    ) -> Result<Self, ProviderError> {
        let kind = match id.as_str() {
            "claude" => AcpKind::Claude,
            "antigravity" => AcpKind::Antigravity,
            _ => {
                return Err(failure(
                    ProviderOperation::ThreadOpen,
                    ProviderFailureKind::Rejected,
                ));
            }
        };
        if command.is_empty() {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Rejected,
            ));
        }
        Ok(Self::from_runtime(
            AcpRuntime::new(kind, command, &runtime_root),
            runtime_root,
        ))
    }

    /// Create an ACP owner from its selected adapter and native launch dependencies.
    pub(super) fn from_runtime(runtime: AcpRuntime, runtime_root: PathBuf) -> Self {
        Self {
            kind: runtime.kind(),
            runtime: Arc::new(runtime),
            cleanup: super::connection::AcpCleanup::default(),
            runtime_root,
            bindings: Arc::new(std::sync::Mutex::new(HashMap::new())),
            owners: Arc::new(Mutex::new(HashMap::new())),
            preparation: Arc::new(Mutex::new(HashMap::new())),
            history_admission: Arc::new(Mutex::new(())),
            history_reservations: Arc::new(Mutex::new(HashMap::new())),
            admission: Arc::new(AtomicBool::new(true)),
            shutdown_task: Arc::new(Mutex::new(None)),
            operations: Arc::new(AcpOperations::new()),
            pending_inputs: Arc::new(PendingInputs::new()),
        }
    }

    /// Revoke an exact binding before publishing interruption, then retain all close phases.
    fn interrupt_binding(
        self,
        binding: BindingId,
        operation: ProviderOperation,
        interruption: forge_provider_api::OperationInterruption,
        phase: ProviderPhase,
    ) -> Interruption<impl std::future::Future<Output = Result<(), ProviderError>> + Send + 'static>
    {
        let provider = self.clone();
        let observation = interruption.failure(operation, phase, FailureDisposition::Invalidated);
        let cause = observation.clone();
        if let Ok(session) = self.binding(&binding, operation) {
            session.state.close_admission();
        }
        Interruption::new(observation, async move {
            let session = provider.binding(&binding, operation)?;
            session.invalidate(cause).await;
            provider.close_session(&binding).await
        })
    }

    /// Revoke only a fork's live source before publishing uncertain native target disposition.
    fn interrupt_fork(
        self,
        source: forge_provider_api::ForkSource,
        interruption: forge_provider_api::OperationInterruption,
    ) -> Interruption<impl std::future::Future<Output = Result<(), ProviderError>> + Send + 'static>
    {
        let provider = self.clone();
        let binding = match source {
            forge_provider_api::ForkSource::Binding(binding) => Some(binding),
            forge_provider_api::ForkSource::Checkpoint { .. } => None,
        };
        let disposition = if binding.is_some() {
            FailureDisposition::Invalidated
        } else {
            FailureDisposition::Unchanged
        };
        let observation = interruption.failure(
            ProviderOperation::ThreadFork,
            ProviderPhase::Observation,
            disposition,
        );
        let cause = observation.clone();
        if let Some(binding) = &binding
            && let Ok(session) = self.binding(binding, ProviderOperation::ThreadFork)
        {
            session.state.close_admission();
        }
        Interruption::new(observation, async move {
            if let Some(binding) = binding {
                let session = provider.binding(&binding, ProviderOperation::ThreadFork)?;
                session.invalidate(cause).await;
                provider.close_session(&binding).await
            } else {
                Ok(())
            }
        })
    }

    /// Retain exact invocation collection after cancellation observation ends without closing its binding.
    fn interrupt_cancel(
        self,
        binding: BindingId,
        scope: forge_provider_api::InvocationScope,
        interruption: forge_provider_api::OperationInterruption,
    ) -> Interruption<impl std::future::Future<Output = Result<(), ProviderError>> + Send + 'static>
    {
        let observation = interruption.failure(
            ProviderOperation::TurnInterrupt,
            ProviderPhase::Collection,
            FailureDisposition::Uncertain,
        );
        Interruption::new(observation, async move {
            // The active invocation and its collection lock fence reuse until actual settlement.
            // An expired observer does not authorize collecting independently owned terminals.
            match self
                .cancel_invocation(&binding, forge_provider_api::CancelRequest { scope })
                .await
            {
                Ok(_) => Ok(()),
                Err(ProviderError::Failure(failure))
                    if failure.kind() == ProviderFailureKind::WrongInvocation =>
                {
                    Ok(())
                }
                Err(error) => Err(error),
            }
        })
    }

    /// Collect interrupted delivery without replacing its invocation or shortening its lifetime.
    async fn collect_interrupted_input(
        &self,
        binding: &BindingId,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        // Collection remains attached to this exact invocation after its observer expires.
        // A shorter timer must not tear down independently owned terminal resources.
        match self
            .cancel_invocation(binding, forge_provider_api::CancelRequest { scope })
            .await
        {
            Ok(_) => Ok(()),
            Err(ProviderError::Failure(failure))
                if failure.kind() == ProviderFailureKind::WrongInvocation =>
            {
                Ok(())
            }
            Err(error) => Err(error),
        }
    }

    /// Get immutable registry metadata without native work.
    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        crate::registry::descriptor(&self.kind.id()).expect("ACP kind is registered")
    }

    /// Get the exact retained binding generation without opening a replacement.
    fn binding(
        &self,
        id: &BindingId,
        operation: ProviderOperation,
    ) -> Result<Arc<AcpBinding>, ProviderError> {
        self.bindings
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(id)
            .cloned()
            .ok_or_else(|| failure(operation, ProviderFailureKind::SessionMissing))
    }

    /// Install exact invocation collection ownership before submitting native input.
    async fn submit(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: PromptRequest,
        pending: PendingInput,
    ) -> Result<InputAcceptance, ProviderError> {
        context.check_admission(ProviderOperation::TurnStart)?;
        let session = self.binding(binding, ProviderOperation::TurnStart)?;
        session
            .state
            .ensure_admission(ProviderOperation::TurnStart)?;
        pending.check_admission(ProviderOperation::TurnStart)?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let admission = tokio::select! {
            biased;
            _ = pending.cancelled() => return Err(pending.check_admission(ProviderOperation::TurnStart).expect_err("sticky pending prevention rejects admission")),
            admission = session.operation.lock() => admission,
        };
        session.sink.check()?;
        pending.check_admission(ProviderOperation::TurnStart)?;
        context.check_admission(ProviderOperation::TurnStart)?;
        session
            .state
            .ensure_admission(ProviderOperation::TurnStart)?;
        if request.scope.thread_id != session.owner
            || request.receipt.id() != &request.submission_id
            || request.receipt.attempt_id()? != request.attempt_id
            || !request.dispatch.matches(
                binding,
                &request.scope,
                &request.submission_id,
                &request.attempt_id,
            )
        {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        validate_configuration(&request.configuration.model, &request.configuration.access)?;
        let (acceptance, scope, task, started) = {
            let mut active = session
                .active
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            pending.check_admission(ProviderOperation::TurnStart)?;
            context.check_admission(ProviderOperation::TurnStart)?;
            if active.is_some() {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::ExecutionActive,
                ));
            }
            {
                let mut settlements = session
                    .settlements
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                if settlements.contains_key(&request.scope) {
                    return Err(failure(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::WrongInvocation,
                    ));
                }
                settlements.insert(request.scope.clone(), Arc::new(OperationRecord::new()));
            }
            session
                .covered_inputs
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .insert(request.scope.clone(), request.input_coverage());
            *active = Some(ActiveExecution {
                scope: request.scope.clone(),
                cancelled: false,
                cancellation: tokio_util::sync::CancellationToken::new(),
                task: None,
            });
            let (started, start) = tokio::sync::oneshot::channel();
            let (accepted, acceptance) = tokio::sync::oneshot::channel();
            let scope = request.scope.clone();
            let driver = Arc::new(AcpTurnDriver {
                session: session.clone(),
                scope: scope.clone(),
                invocation: Mutex::new(Some(PromptInvocation {
                    request: request.clone(),
                    accepted,
                    admission: start,
                })),
            });
            let address = forge_provider_api::TurnAddress {
                scope: scope.clone(),
                thread_id: request.thread_id.clone(),
                turn_id: request.turn_id.clone(),
            };
            let task = Arc::new(crate::turn_runner::RetainedTurn::spawn(
                driver,
                session.sink.invocation_sink(&scope)?,
                address,
            )?);
            active.as_mut().expect("active execution inserted").task = Some(task.clone());
            (acceptance, scope, task, started)
        };
        let collected_session = session.clone();
        let started_turn = task.clone();
        tokio::spawn(async move {
            collect_invocation(collected_session, scope, task).await;
        });
        pending.registered();
        let _ = started.send(());
        drop(admission);
        let accepted = match acceptance.await {
            Ok(result) => result?,
            Err(_) => {
                started_turn.started().await?;
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Transport,
                ));
            }
        };
        started_turn.started().await?;
        Ok(accepted)
    }

    /// Open native history under retained admission and publish one owned reader.
    async fn open_session(
        &self,
        context: OperationContext,
        mut request: SessionRequest,
        cleanup: super::connection::AcpCleanup,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        context.check_admission(ProviderOperation::ThreadOpen)?;
        if !self.admission.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if matches!(
            request.history,
            forge_provider_api::SessionHistory::IsolatedText(_)
        ) {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::TextGeneration,
            )
            .into());
        }
        validate_configuration(&request.configuration.model, &request.configuration.access)?;
        let mcp_servers = super::resources::mcp_launches(
            ProviderOperation::ThreadOpen,
            &request.configuration.resources.mcp,
        )?;
        let runtime = self
            .runtime
            .resolve()
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Runtime))?;
        if self.kind == AcpKind::Claude {
            runtime.validate_native_claude(&context).await?;
        }
        context.check_admission(ProviderOperation::ThreadOpen)?;
        let _history_admission = self.history_admission.lock().await;
        context.check_admission(ProviderOperation::ThreadOpen)?;
        if !self.admission.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if let Some(resume) = request.history.resume_id() {
            let reservations = self
                .history_reservations
                .lock()
                .await
                .get(resume)
                .cloned()
                .unwrap_or_default();
            if reservations.iter().any(|reservation| {
                reservation.owner != request.thread_id && !reservation.cleanup.is_collected()
            }) {
                return Err(failure(
                    ProviderOperation::ThreadOpen,
                    ProviderFailureKind::HistoryConflict,
                ));
            }
            let bindings = self
                .bindings
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if bindings.values().any(|binding| {
                binding.owner != request.thread_id
                    && binding.binding.provider_session_id.as_ref() == Some(resume)
                    && !binding.history_released.load(Ordering::Acquire)
            }) {
                return Err(failure(
                    ProviderOperation::ThreadOpen,
                    ProviderFailureKind::HistoryConflict,
                ));
            }
        }
        let preparation = self
            .preparation
            .lock()
            .await
            .entry(request.thread_id.clone())
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone();
        let _preparation = preparation.lock().await;
        let existing = self.owners.lock().await.get(&request.thread_id).cloned();
        if let Some(existing) = existing {
            self.close_session(&existing).await?;
        }
        if let Some(resume) = request.history.resume_id() {
            let reservations = self
                .history_reservations
                .lock()
                .await
                .get(resume)
                .cloned()
                .unwrap_or_default();
            for reservation in reservations {
                if !reservation.cleanup.is_collected() {
                    reservation.cleanup.shutdown().await?;
                }
            }
            self.history_reservations
                .lock()
                .await
                .entry(resume.clone())
                .or_default()
                .push(HistoryReservation {
                    operation_id: context.id().clone(),
                    owner: request.thread_id.clone(),
                    cleanup: cleanup.clone(),
                });
        }
        context.check_admission(ProviderOperation::ThreadOpen)?;
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string())
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Unknown))?;
        let scope = forge_provider_api::EventScope::Binding {
            thread_id: request.thread_id.clone(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), scope);
        let profile = cleanup.retain_profile(super::profile::create(
            self.kind,
            &self.runtime_root,
            &request.configuration.resources.skills,
        )?);
        cleanup.retain_lease(request.resource_lease.take());
        let selection = request.configuration.resources.clone();
        let state = Arc::new(AcpSessionState::new_binding(
            self.kind,
            self.admission.clone(),
        ));
        let conversation = AcpConversation::open(
            super::AcpLaunch {
                command: &runtime.command,
                environment: &runtime.environment,
                workspace: &request.workspace,
                profile: (self.kind == AcpKind::Antigravity).then_some(profile.path()),
            },
            state.clone(),
            ConversationOptions {
                kind: self.kind,
                workspace: request.workspace.clone(),
                session_id: request.history.resume_id().map(ToString::to_string),
                control_endpoint: request.tool_endpoint.clone(),
                mcp_servers,
                resources: mcp_definitions(&selection),
                profile: profile.path().to_owned(),
                skills: request
                    .configuration
                    .resources
                    .skills
                    .iter()
                    .filter(|skill| skill.enabled)
                    .map(|skill| skill.name.clone())
                    .collect(),
                system_instructions: request.system_instructions.clone(),
            },
            &cleanup,
        )
        .await?;
        let session_id: forge_provider_api::ProviderSessionId =
            conversation.id().to_owned().try_into().map_err(|_| {
                failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol)
            })?;
        let conflict = {
            let mut reservations = self.history_reservations.lock().await;
            let entries = reservations.entry(session_id.clone()).or_default();
            let conflict = entries.iter().any(|reservation| {
                reservation.operation_id != *context.id()
                    && reservation.owner != request.thread_id
                    && !reservation.cleanup.is_collected()
            });
            if !entries
                .iter()
                .any(|reservation| reservation.operation_id == *context.id())
            {
                entries.push(HistoryReservation {
                    operation_id: context.id().clone(),
                    owner: request.thread_id.clone(),
                    cleanup: cleanup.clone(),
                });
            }
            conflict
        };
        if conflict {
            let cause = failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::HistoryConflict,
            );
            state.invalidate_binding(cause.clone()).await;
            let collected = cleanup.shutdown().await;
            return Err(match collected {
                Ok(()) => cause,
                Err(cleanup) => with_cleanup(cause, vec![cleanup]),
            });
        }
        let binding = SessionBinding {
            thread_id: request.thread_id.clone(),
            id: binding_id.clone(),
            provider_session_id: Some(session_id),
        };
        let initial_model = conversation.initial_model().await;
        let requested_model = request.configuration.model.clone();
        let retained = Arc::new(AcpBinding {
            binding: binding.clone(),
            owner: request.thread_id.clone(),
            conversation: Arc::new(conversation),
            state,
            sink,
            cleanup: cleanup.clone(),
            profiles: Mutex::new(vec![profile]),
            selection: Mutex::new(selection),
            native_model: Mutex::new(forge_provider_api::ModelSelection::default()),
            initial_model,
            operation: Mutex::new(()),
            steering: RwLock::new(()),
            covered_inputs: std::sync::Mutex::new(HashMap::new()),
            active: std::sync::Mutex::new(None),
            settlements: std::sync::Mutex::new(HashMap::new()),
            collection_watermarks: Default::default(),
            close_task: Mutex::new(None),
            history_released: AtomicBool::new(false),
            rejected_inputs: Mutex::new(Vec::new()),
        });
        self.bindings
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(binding_id.clone(), retained.clone());
        self.owners
            .lock()
            .await
            .insert(request.thread_id.clone(), binding_id.clone());
        let configured = match context.check_admission(ProviderOperation::ThreadOpen) {
            Ok(()) => {
                configure_model(
                    &retained,
                    &requested_model,
                    &forge_provider_api::ModelSelection::default(),
                    forge_provider_api::ExecutionMode::Default,
                )
                .await
            }
            Err(error) => Err(error),
        };
        if let Err(error) = configured {
            let error = with_operation(ProviderOperation::ThreadOpen, error);
            retained.state.retain_failure(error.clone()).await;
            let cleanup = self.close_session(&binding_id).await;
            return Err(match cleanup {
                Ok(()) => error,
                Err(cleanup) => with_cleanup(error, vec![cleanup]),
            });
        }
        if let Err(error) = context.check_admission(ProviderOperation::ThreadOpen) {
            let error = operation_failure(
                ProviderOperation::ThreadOpen,
                &error,
                ProviderPhase::Observation,
                FailureDisposition::Invalidated,
            );
            retained.state.retain_failure(error.clone()).await;
            let cleanup = self.close_session(&binding_id).await;
            return Err(match cleanup {
                Ok(()) => error,
                Err(cleanup) => with_cleanup(error, vec![cleanup]),
            });
        }
        *retained.native_model.lock().await = requested_model;
        let capabilities = retained.capabilities().await;
        retained
            .state
            .install_capabilities(
                retained.conversation.id(),
                retained.sink.clone(),
                capabilities.clone(),
            )
            .await;
        if let Err(cause) = retained.ensure_current_result(&context, ProviderOperation::ThreadOpen)
        {
            retained.state.retain_failure(cause.clone()).await;
            let cleanup = self.close_session(&binding_id).await;
            return Err(match cleanup {
                Ok(()) => cause,
                Err(cleanup) => with_cleanup(cause, vec![cleanup]),
            });
        }
        Ok(forge_provider_api::OpenedSession {
            binding,
            capabilities,
            events: stream,
        })
    }

    /// Apply complete defaults to settled history while preserving its public binding.
    async fn configure_session(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: forge_provider_api::SessionConfiguration,
    ) -> Result<forge_provider_api::ConfiguredSession, ProviderError> {
        context.check_admission(ProviderOperation::ThreadConfigure)?;
        let session = self.binding(binding, ProviderOperation::ThreadConfigure)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadConfigure)?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let _operation = session.operation.lock().await;
        session.sink.check()?;
        context.check_admission(ProviderOperation::ThreadConfigure)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadConfigure)?;
        if session.has_active_prompt() || session.state.has_active_work().await {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        validate_configuration(&configuration.model, &configuration.access)?;
        let servers = super::resources::mcp_launches(
            ProviderOperation::ThreadConfigure,
            &configuration.resources.mcp,
        )?;
        let resources_changed =
            !same_resources(&*session.selection.lock().await, &configuration.resources);
        if resources_changed && self.kind == AcpKind::Antigravity {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ResourceSelection,
            )
            .into());
        }
        let prepared_resources = if resources_changed {
            let profile = session.cleanup.retain_profile(super::profile::create(
                self.kind,
                &self.runtime_root,
                &configuration.resources.skills,
            )?);
            session.profiles.lock().await.push(profile.clone());
            let skills = configuration
                .resources
                .skills
                .iter()
                .filter(|skill| skill.enabled)
                .map(|skill| skill.name.clone())
                .collect::<Vec<_>>();
            Some((profile, servers, skills))
        } else {
            None
        };
        let stored_model = session.native_model.lock().await.clone();
        if let Err(error) = configure_model(
            &session,
            &configuration.model,
            &stored_model,
            forge_provider_api::ExecutionMode::Default,
        )
        .await
        {
            if matches!(&error, ProviderError::Unsupported(_))
                || matches!(&error, ProviderError::Failure(failure) if failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
            {
                return Err(error);
            }
            let error = operation_failure(
                ProviderOperation::ThreadConfigure,
                &error,
                forge_provider_api::ProviderPhase::Preparation,
                forge_provider_api::FailureDisposition::Invalidated,
            );
            session.invalidate(error.clone()).await;
            return Err(error);
        }
        if let Some((profile, servers, skills)) = prepared_resources {
            if let Err(error) = session
                .conversation
                .configure_resources(servers, &skills, profile.path())
                .await
            {
                let error = operation_failure(
                    ProviderOperation::ThreadConfigure,
                    &error,
                    forge_provider_api::ProviderPhase::Preparation,
                    forge_provider_api::FailureDisposition::Invalidated,
                );
                session.invalidate(error.clone()).await;
                return Err(error);
            }
            let profiles = session.profiles.lock().await.clone();
            for previous in profiles {
                if !Arc::ptr_eq(&previous, &profile)
                    && let Err(error) = previous.close(ProviderOperation::ThreadConfigure).await
                {
                    let cause = operation_failure(
                        ProviderOperation::ThreadConfigure,
                        &error,
                        ProviderPhase::Collection,
                        FailureDisposition::Invalidated,
                    );
                    session.invalidate(cause.clone()).await;
                    return Err(cause);
                }
            }
            *session.selection.lock().await = configuration.resources.clone();
        }
        *session.native_model.lock().await = configuration.model.clone();
        if let Err(cause) =
            session.ensure_current_result(&context, ProviderOperation::ThreadConfigure)
        {
            session.invalidate(cause.clone()).await;
            return Err(cause);
        }
        let capabilities = session.capabilities().await;
        if let Err(cause) =
            session.ensure_current_result(&context, ProviderOperation::ThreadConfigure)
        {
            session.invalidate(cause.clone()).await;
            return Err(cause);
        }
        Ok(forge_provider_api::ConfiguredSession {
            configuration,
            capabilities,
            watermark: session.sink.watermark(),
        })
    }

    /// Create separate durable history only from an exact settled native source.
    async fn fork_session(
        &self,
        context: OperationContext,
        request: forge_provider_api::session::SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        context.check_admission(ProviderOperation::ThreadFork)?;
        if self.kind != AcpKind::Claude {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::NativeFork,
            )
            .into());
        }
        let binding = match &request.source {
            forge_provider_api::ForkSource::Binding(binding) => binding.clone(),
            forge_provider_api::ForkSource::Checkpoint { .. } => {
                return Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::CheckpointFork,
                )
                .into());
            }
        };
        let session = self.binding(&binding, ProviderOperation::ThreadFork)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadFork)?;
        if !session.capabilities().await.capability.iter().any(|entry| {
            entry.id == forge_provider_api::capability::CapabilityId::NativeFork
                && entry.support == forge_provider_api::capability::Support::Supported
        }) {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::NativeFork,
            )
            .into());
        }
        let source_owner = &session.owner;
        if &request.target.thread_id == source_owner {
            return Err(failure(
                ProviderOperation::ThreadFork,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        validate_configuration(
            &request.target.configuration.model,
            &request.target.configuration.access,
        )?;
        let mcp = super::resources::mcp_launches(
            ProviderOperation::ThreadFork,
            &request.target.configuration.resources.mcp,
        )?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::ThreadFork,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let _operation = session.operation.lock().await;
        session.sink.check()?;
        context.check_admission(ProviderOperation::ThreadFork)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadFork)?;
        if session.has_active_prompt() || session.state.has_active_work().await {
            return Err(failure(
                ProviderOperation::ThreadFork,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let source_model = session.native_model.lock().await.clone();
        let target_model =
            resolve_native_model(&session, &request.target.configuration.model, &source_model)?;
        let models =
            session.conversation.models().await.map_err(|_| {
                failure(ProviderOperation::ThreadFork, ProviderFailureKind::Protocol)
            })?;
        if let Some(model) = target_model.model.as_deref()
            && !models.iter().any(|candidate| candidate.id == model)
        {
            return Err(failure(
                ProviderOperation::ThreadFork,
                ProviderFailureKind::Rejected,
            ));
        }
        let selected_model = target_model.model.as_deref().or_else(|| {
            models
                .iter()
                .find(|model| model.is_default)
                .map(|model| model.id.as_str())
        });
        let effort = match &target_model.reasoning {
            forge_provider_api::generation::ReasoningSelection::Default => None,
            forge_provider_api::generation::ReasoningSelection::Level(level) => {
                if !selected_model
                    .and_then(|id| models.iter().find(|model| model.id == id))
                    .is_some_and(|model| model.reasoning.contains(level))
                {
                    return Err(forge_provider_api::capability::UnsupportedCapability(
                        forge_provider_api::capability::ProviderFeature::EffortSelection,
                    )
                    .into());
                }
                Some(level.as_str())
            }
            forge_provider_api::generation::ReasoningSelection::Lowest => Some(
                selected_model
                    .and_then(|id| models.iter().find(|model| model.id == id))
                    .and_then(|model| model.reasoning.first())
                    .ok_or({
                        forge_provider_api::capability::UnsupportedCapability(
                            forge_provider_api::capability::ProviderFeature::EffortSelection,
                        )
                    })?
                    .as_str(),
            ),
        };
        session
            .conversation
            .validate_fork_instructions(&request.target.system_instructions)?;
        let skills = request
            .target
            .configuration
            .resources
            .skills
            .iter()
            .filter(|skill| skill.enabled)
            .map(|skill| skill.name.clone())
            .collect::<Vec<_>>();
        let profile = session.cleanup.retain_profile(super::profile::create(
            self.kind,
            &self.runtime_root,
            &request.target.configuration.resources.skills,
        )?);
        let id = session
            .conversation
            .fork(
                context.id(),
                super::conversation::ForkConfiguration {
                    workspace: &request.target.workspace,
                    mcp_servers: mcp,
                    skills: &skills,
                    profile: profile.path(),
                    model: target_model.model.as_deref(),
                    effort,
                    system_instructions: &request.target.system_instructions,
                },
            )
            .await
            .map_err(|error| {
                operation_failure(
                    ProviderOperation::ThreadFork,
                    &error,
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Uncertain,
                )
            })?;
        let id = id
            .try_into()
            .map_err(|_| failure(ProviderOperation::ThreadFork, ProviderFailureKind::Protocol))?;
        Ok(forge_provider_api::ProviderForkResult::unprofiled(id))
    }

    /// Require positive native compaction completion within the requested operation.
    async fn compact_session(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        context.check_admission(ProviderOperation::ThreadCompact)?;
        request.validate_context(&context)?;
        if self.kind != AcpKind::Claude {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::Rejected,
            ));
        }
        let session = self.binding(binding, ProviderOperation::ThreadCompact)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadCompact)?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let _operation = session.operation.lock().await;
        session.sink.check()?;
        context.check_admission(ProviderOperation::ThreadCompact)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadCompact)?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        if request.scope.thread_id() != &session.owner {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let sink = session.sink.bind_scope(request.scope.clone())?;
        session
            .state
            .begin_compaction(session.conversation.id(), sink)
            .await?;
        let result = session.conversation.request("session/prompt", serde_json::json!({
            "sessionId":session.conversation.id(), "prompt":[{"type":"text","text":"/compact"}]
        })).await;
        session.state.finish_compaction().await;
        let response = result?;
        super::validate_prompt_response(response.clone()).map_err(|_| {
            failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::Protocol,
            )
        })?;
        if response["stopReason"] == "cancelled" {
            return Err(ProviderError::Cancelled);
        }
        let compaction = match settled_compaction(session.state.latest_compaction().await) {
            Some(compaction) => compaction,
            None => {
                let cause = ProviderError::Failure(
                    ProviderFailure::new(
                        ProviderOperation::ThreadCompact,
                        ProviderFailureKind::Protocol,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Observation,
                        forge_provider_api::FailureDisposition::Invalidated,
                    ),
                );
                session.invalidate(cause.clone()).await;
                return Err(cause);
            }
        };
        Ok(forge_provider_api::CompactionResult {
            compaction,
            watermark: session.sink.watermark(),
        })
    }

    /// Deliver only negotiated steering into the exact invocation or report its disposition.
    async fn steer_session(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<forge_provider_api::session::SteeringAcceptance, ProviderError> {
        context.check_admission(ProviderOperation::TurnSteer)?;
        let session = self.binding(binding, ProviderOperation::TurnSteer)?;
        let _steering = session.steering.read().await;
        session
            .state
            .ensure_admission(ProviderOperation::TurnSteer)?;
        if request.scope.thread_id != session.owner {
            return Err(failure(
                ProviderOperation::TurnSteer,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .as_ref()
            .is_none_or(|active| active.scope != request.scope)
        {
            return Err(failure(
                ProviderOperation::TurnSteer,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if self.kind != AcpKind::Claude {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::NativeSteer,
            )
            .into());
        }
        let capabilities = session.capabilities().await;
        if capabilities.support(forge_provider_api::capability::CapabilityId::NativeSteer)
            != Some(&forge_provider_api::capability::Support::Supported)
        {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::NativeSteer,
            )
            .into());
        }
        let input = prompt_input(&request.input);
        let prompt = super::attachments::input(
            input,
            &request.attachments,
            session.conversation.supports_images(),
        )?;
        let receipt = request.delivery.receipt().cloned();
        let submission_id = request.delivery.submission_id().cloned();
        let covered_input = request
            .delivery
            .receipt()
            .map(|receipt| {
                receipt.attempt_id().map(|attempt| {
                    forge_provider_api::CoveredInput::new(receipt.id().clone(), attempt)
                })
            })
            .transpose()?;
        session.sink.check()?;
        let response = match session
            .conversation
            .steer(
                serde_json::json!({"sessionId":session.conversation.id(),"prompt":prompt}),
                receipt,
            )
            .await
        {
            Ok(response) => response,
            Err(error) => return Err(error),
        };
        if response == super::submission::SteeringAcceptance::UnobservableExecution {
            session.invalidate(ProviderError::DeliveryUncertain).await;
            let _ = session.conversation.shutdown().await;
            session.sink.fail(events::EventDeliveryError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Disconnected,
            ));
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Disconnected,
            ));
        }
        if let Some(input) = covered_input
            && let Err(error) = session.cover_input(&request.scope, input)
        {
            session.invalidate(error.clone()).await;
            return Err(error);
        }
        Ok(match submission_id {
            Some(submission_id) => {
                forge_provider_api::session::SteeringAcceptance::Journaled { submission_id }
            }
            None => forge_provider_api::session::SteeringAcceptance::Context,
        })
    }

    /// Inspect installation and authentication methods through a disposable native handshake.
    async fn inspect_runtime(
        &self,
        context: OperationContext,
        request: forge_provider_api::catalog::WorkspaceContext,
        cleanup: super::connection::AcpCleanup,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        let status = self.runtime.status();
        let provider_version = match status.provider_version(&context).await {
            Ok(Some(version)) => forge_provider_api::VersionObservation::Known { version },
            Ok(None) => forge_provider_api::VersionObservation::NotReported,
            Err(ProviderError::Failure(error)) => forge_provider_api::VersionObservation::Failed {
                failure: error.kind(),
            },
            Err(_) => forge_provider_api::VersionObservation::Failed {
                failure: ProviderFailureKind::Unknown,
            },
        };
        let origin = if status.managed {
            forge_provider_api::RuntimeOrigin::Managed
        } else {
            forge_provider_api::RuntimeOrigin::Configured
        };
        let mut versions = forge_provider_api::RuntimeVersions {
            provider: provider_version,
            adapter: status.adapter_version.clone().map_or(
                forge_provider_api::VersionObservation::NotReported,
                |version| forge_provider_api::VersionObservation::Known { version },
            ),
        };
        if !status.available {
            return Ok(forge_provider_api::ProviderSetup {
                runtime: forge_provider_api::RuntimeAvailability::Missing,
                origin,
                versions,
                authentication: Vec::new(),
                capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                    self.descriptor(),
                ),
            });
        }
        let initialized = match self
            .initialize_handshake(&status, &request.workspace, &cleanup)
            .await
        {
            Ok(initialized) => initialized,
            Err(ProviderError::RuntimeVersion(error)) => {
                versions.adapter = match error {
                    forge_provider_api::version::RuntimeVersionError::Unsupported {
                        detected,
                        ..
                    } => forge_provider_api::VersionObservation::Known {
                        version: detected.to_string(),
                    },
                    forge_provider_api::version::RuntimeVersionError::Missing { .. } => {
                        forge_provider_api::VersionObservation::NotReported
                    }
                    forge_provider_api::version::RuntimeVersionError::Malformed { .. } => {
                        forge_provider_api::VersionObservation::Failed {
                            failure: ProviderFailureKind::Protocol,
                        }
                    }
                    forge_provider_api::version::RuntimeVersionError::Probe { .. } => {
                        forge_provider_api::VersionObservation::Failed {
                            failure: ProviderFailureKind::Initialization,
                        }
                    }
                };
                return Ok(forge_provider_api::ProviderSetup {
                    runtime: forge_provider_api::RuntimeAvailability::Incompatible,
                    origin,
                    versions,
                    authentication: Vec::new(),
                    capabilities:
                        forge_provider_api::capability::CapabilityManifest::from_descriptor(
                            self.descriptor(),
                        ),
                });
            }
            Err(_) => {
                return Ok(forge_provider_api::ProviderSetup {
                    runtime: forge_provider_api::RuntimeAvailability::Unavailable,
                    origin,
                    versions,
                    authentication: Vec::new(),
                    capabilities:
                        forge_provider_api::capability::CapabilityManifest::from_descriptor(
                            self.descriptor(),
                        ),
                });
            }
        };
        let authentication = initialized["authMethods"]
            .as_array()
            .into_iter()
            .flatten()
            .filter_map(|method| {
                let id = forge_provider_api::provider::AuthenticationMethodId::new(
                    method["id"].as_str()?,
                )
                .ok()?;
                let terminal = method["type"] == "terminal";
                let interaction = if terminal {
                    let program = status.command.first().map(PathBuf::from)?;
                    let arguments = status
                        .command
                        .iter()
                        .skip(1)
                        .cloned()
                        .chain(
                            method["args"]
                                .as_array()
                                .into_iter()
                                .flatten()
                                .filter_map(serde_json::Value::as_str)
                                .map(str::to_owned),
                        )
                        .collect();
                    forge_provider_api::provider::AuthenticationInteraction::Terminal {
                        launch: forge_provider_api::provider::TerminalLaunch {
                            program,
                            arguments,
                            environment: status.environment.clone(),
                            working_directory: Some(PathBuf::from(&request.workspace)),
                        },
                    }
                } else {
                    forge_provider_api::provider::AuthenticationInteraction::ProviderManaged
                };
                Some(forge_provider_api::provider::AuthenticationMethod {
                    id,
                    label: method["name"].as_str().unwrap_or("Sign in").to_owned(),
                    description: method["description"]
                        .as_str()
                        .unwrap_or_default()
                        .to_owned(),
                    interaction,
                })
            })
            .collect();
        if let Some(version) = initialized
            .pointer("/agentInfo/version")
            .and_then(serde_json::Value::as_str)
        {
            versions.adapter = forge_provider_api::VersionObservation::Known {
                version: version.to_owned(),
            };
        }
        Ok(forge_provider_api::ProviderSetup {
            runtime: forge_provider_api::RuntimeAvailability::Available,
            origin,
            versions,
            authentication,
            capabilities: forge_provider_api::capability::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
        })
    }

    /// Authenticate through an explicitly advertised provider-managed method.
    async fn authenticate_runtime(
        &self,
        request: forge_provider_api::catalog::WorkspaceContext,
        method: forge_provider_api::provider::AuthenticationMethodId,
        cleanup: super::connection::AcpCleanup,
    ) -> Result<(), ProviderError> {
        let status = self.runtime.status();
        if !status.available {
            return Err(failure(
                ProviderOperation::Authenticate,
                ProviderFailureKind::Runtime,
            ));
        }
        let profile =
            cleanup.retain_profile(super::profile::create(self.kind, &self.runtime_root, &[])?);
        let state = Arc::new(AcpSessionState::new_binding(
            self.kind,
            self.admission.clone(),
        ));
        let connection = super::AcpConnection::launch_retained(
            super::AcpLaunch {
                command: &status.command,
                environment: &status.environment,
                workspace: &request.workspace,
                profile: (self.kind == AcpKind::Antigravity).then_some(profile.path()),
            },
            state,
            &cleanup,
        )
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::Authenticate,
                ProviderFailureKind::Transport,
            )
        })?;
        let result = async {
            let initialized = connection
                .request("initialize", initialize_params())
                .await
                .map_err(|_| {
                    failure(
                        ProviderOperation::Authenticate,
                        ProviderFailureKind::Protocol,
                    )
                })?;
            let selected = initialized["authMethods"]
                .as_array()
                .into_iter()
                .flatten()
                .find(|candidate| candidate["id"].as_str() == Some(method.as_str()))
                .ok_or_else(|| {
                    failure(
                        ProviderOperation::Authenticate,
                        ProviderFailureKind::Rejected,
                    )
                })?;
            if selected["type"] == "terminal" {
                // Terminal authentication is owned by the normalized setup interaction;
                // this provider operation must not launch or impersonate that action.
                return Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::TerminalAuthentication,
                )
                .into());
            }
            connection
                .request(
                    "authenticate",
                    serde_json::json!({"methodId":method.as_str()}),
                )
                .await
                .map(|_| ())
                .map_err(|_| {
                    failure(
                        ProviderOperation::Authenticate,
                        ProviderFailureKind::Transport,
                    )
                })
        }
        .await;
        let collected = cleanup
            .shutdown()
            .await
            .map_err(|error| with_operation(ProviderOperation::Authenticate, error));
        match (result, collected) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(cause), Ok(())) | (Ok(()), Err(cause)) => Err(cause),
            (Err(cause), Err(cleanup)) => Err(ProviderError::PreparationCleanup {
                cause: Box::new(cause),
                cleanup: Box::new(cleanup),
            }),
        }
    }

    /// Close every producer before joining retained native operations and pending opens.
    async fn shutdown_provider(&self, operation_id: OperationId) -> Result<(), ProviderError> {
        self.admission.store(false, Ordering::Release);
        self.operations.close_admission();
        let mut errors = Vec::new();
        let bindings = self
            .bindings
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for binding in bindings {
            if let Err(error) = self.close_session(&binding).await {
                errors.push(error);
            }
        }
        if let Err(error) = self.cleanup.shutdown().await {
            errors.push(error);
        }
        if let Err(error) = self.operations.join_all_except(&operation_id).await {
            errors.push(error);
        }
        let bindings = self
            .bindings
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for binding in bindings {
            if let Err(error) = self.close_session(&binding).await {
                errors.push(error);
            }
        }
        if let Err(error) = self.cleanup.shutdown().await {
            errors.push(error);
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(ProviderError::Combined {
                primary: Box::new(errors.remove(0)),
                secondary: errors,
            })
        }
    }

    async fn close_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.binding(binding, ProviderOperation::ThreadClose)?;
        session.state.close_admission();
        let pending = self.pending_inputs.fence_binding(binding);
        session.state.revoke_authority().await;
        pending.await;
        let mut retained = session.close_task.lock().await;
        if retained.as_ref().is_some_and(|task| task.is_finished()) {
            if let Some(task) = retained.as_ref()
                && task.join().await.is_ok()
            {
                return Ok(());
            }
            retained.take();
        }
        let task = retained
            .get_or_insert_with(|| {
                let session = session.clone();
                Arc::new(crate::retained_task::RetainedTask::new(
                    ProviderOperation::ThreadClose,
                    tokio::spawn(async move { close_binding(session).await }),
                ))
            })
            .clone();
        drop(retained);
        let result = task.join().await;
        if result.is_ok() {
            let mut owners = self.owners.lock().await;
            if owners.get(&session.owner) == Some(binding) {
                owners.remove(&session.owner);
            }
        }
        result
    }

    async fn cancel_invocation(
        &self,
        binding: &BindingId,
        request: forge_provider_api::CancelRequest,
    ) -> Result<(), ProviderError> {
        self.pending_inputs.cancel(binding, &request.scope).await;
        let session = self.binding(binding, ProviderOperation::TurnInterrupt)?;
        let record = session
            .settlements
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(&request.scope)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        if let Some(result) = record.result.lock().await.clone() {
            return match result {
                Ok(_) | Err(ProviderError::Cancelled) => Ok(()),
                Err(error) => Err(error),
            };
        }
        let task = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .as_ref()
            .filter(|active| active.scope == request.scope)
            .and_then(|active| active.task.clone());
        if let Some(task) = task {
            task.interrupt().await?;
        }
        session
            .state
            .revoke_invocation_callbacks(&request.scope)
            .await;
        match session.settlement(&request.scope).await {
            Ok(_) | Err(ProviderError::Cancelled) => Ok(()),
            Err(error) => Err(error),
        }
    }

    async fn reconcile_input(
        &self,
        request: forge_provider_api::InputReconcileRequest,
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        let token = request.token.as_ref();
        let accepted_receipts = &request.accepted_receipts;
        let Some(token) = token else {
            return Ok(forge_provider_api::SubmissionEvidence::Unknown);
        };
        let checkpoint = parse_checkpoint(token)?;
        if checkpoint.submission_id != request.submission_id.as_str()
            || checkpoint.attempt_id != request.attempt_id.as_str()
            || request
                .provider_session_id
                .as_ref()
                .is_some_and(|session| session.as_str() != checkpoint.session_id)
        {
            return Err(failure(
                ProviderOperation::InputReconcile,
                ProviderFailureKind::HistoryConflict,
            ));
        }
        if checkpoint.accepted
            && accepted_receipts
                .iter()
                .any(|receipt| receipt.as_bytes() == token.as_bytes())
        {
            let provider_session_id =
                Some(checkpoint.session_id.to_owned().try_into().map_err(|_| {
                    failure(
                        ProviderOperation::InputReconcile,
                        ProviderFailureKind::Protocol,
                    )
                })?);
            return Ok(forge_provider_api::SubmissionEvidence::Accepted {
                receipt: Some(token.clone()),
                provider_session_id,
            });
        }
        // ACP v1 does not expose a client-assigned input identity or exact history evidence.
        Ok(forge_provider_api::SubmissionEvidence::Unknown)
    }

    async fn retract_input(
        &self,
        binding: &BindingId,
        request: forge_provider_api::InputRetractRequest,
    ) -> Result<forge_provider_api::InputRetractOutcome, ProviderError> {
        let session = self.binding(binding, ProviderOperation::InputRetract)?;
        session
            .state
            .ensure_admission(ProviderOperation::InputRetract)?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::InputRetract,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        if request.scope.thread_id != session.owner {
            return Err(failure(
                ProviderOperation::InputRetract,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let record = session
            .settlements
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(&request.scope)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::InputRetract,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        let settlement = record.result.lock().await.clone();
        if settlement.is_some()
            && session
                .rejected_inputs
                .lock()
                .await
                .iter()
                .any(|(scope, submission, attempt)| {
                    scope == &request.scope
                        && submission == &request.submission_id
                        && attempt == &request.attempt_id
                })
        {
            return Ok(forge_provider_api::InputRetractOutcome::NeverConsumed);
        }
        if !settlement.as_ref().is_some_and(|result| {
            result.as_ref().is_ok_and(|completed| {
                completed.result().outcome() == forge_provider_api::turn::TurnOutcome::Interrupted
            })
        }) {
            return Ok(forge_provider_api::InputRetractOutcome::Unavailable(
                forge_provider_api::InputRetractUnavailable::NotCancelled,
            ));
        }
        Ok(forge_provider_api::InputRetractOutcome::Unavailable(
            forge_provider_api::InputRetractUnavailable::Unsupported,
        ))
    }

    async fn list_models(
        &self,
        request: forge_provider_api::catalog::QueryContext,
    ) -> Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        let binding = match request {
            forge_provider_api::catalog::QueryContext::Binding(binding) => binding,
            forge_provider_api::catalog::QueryContext::Workspace(_) => {
                return Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::ModelSelection,
                )
                .into());
            }
        };
        let session = self.binding(&binding, ProviderOperation::ModelList)?;
        session
            .state
            .ensure_admission(ProviderOperation::ModelList)?;
        session
            .conversation
            .models()
            .await
            .map_err(|_| failure(ProviderOperation::ModelList, ProviderFailureKind::Protocol))
    }

    async fn list_skills(
        &self,
        request: forge_provider_api::catalog::QueryContext,
    ) -> Result<Vec<forge_provider_api::catalog::SkillDefinition>, ProviderError> {
        let binding = match request {
            forge_provider_api::catalog::QueryContext::Binding(binding) => binding,
            forge_provider_api::catalog::QueryContext::Workspace(_) => {
                return Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::SkillCatalog,
                )
                .into());
            }
        };
        let session = self.binding(&binding, ProviderOperation::SkillList)?;
        session
            .state
            .ensure_admission(ProviderOperation::SkillList)?;
        let skills = session.selection.lock().await.skills.clone();
        Ok(skills)
    }

    async fn list_mcp(
        &self,
        query: forge_provider_api::catalog::McpQuery,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let (request, observed) = match query {
            forge_provider_api::catalog::McpQuery::Configured(context) => (context, false),
            forge_provider_api::catalog::McpQuery::Observed(context) => (context, true),
        };
        let binding = match request {
            forge_provider_api::catalog::QueryContext::Binding(binding) => binding,
            forge_provider_api::catalog::QueryContext::Workspace(_) => {
                return Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::McpCatalog,
                )
                .into());
            }
        };
        let session = self.binding(&binding, ProviderOperation::McpList)?;
        session.state.ensure_admission(ProviderOperation::McpList)?;
        let mut definitions = mcp_definitions(&*session.selection.lock().await);
        if !observed {
            return Ok(definitions);
        }
        let catalog = session.state.catalog().await;
        if let Some(resources) = catalog["mcpResources"].as_array() {
            for definition in &mut definitions {
                if let Some(native) = resources
                    .iter()
                    .find(|resource| resource["name"] == definition.name)
                {
                    match native["state"].as_str() {
                        Some("requested") => {
                            definition.status = forge_provider_api::catalog::McpStatus::Loading
                        }
                        Some("disabled") => {
                            definition.status = forge_provider_api::catalog::McpStatus::Disabled
                        }
                        Some("unavailable") => {
                            definition.status = forge_provider_api::catalog::McpStatus::Unavailable;
                            definition.status_detail =
                                Some(normalized_mcp_reason(native["reason"].as_str()));
                        }
                        _ => {}
                    }
                }
            }
        }
        Ok(definitions)
    }

    async fn shutdown_sessions(&self, operation_id: OperationId) -> Result<(), ProviderError> {
        self.admission.store(false, Ordering::Release);
        let mut retained = self.shutdown_task.lock().await;
        if retained.as_ref().is_some_and(|task| task.is_finished()) {
            if let Some(task) = retained.as_ref()
                && task.join().await.is_ok()
            {
                return Ok(());
            }
            retained.take();
        }
        let task = retained
            .get_or_insert_with(|| {
                let provider = self.clone();
                Arc::new(crate::retained_task::RetainedTask::new(
                    ProviderOperation::Shutdown,
                    tokio::spawn(async move { provider.shutdown_provider(operation_id).await }),
                ))
            })
            .clone();
        drop(retained);
        task.join().await
    }

    /// Inspect native initialization capabilities and always collect its temporary process.
    async fn initialize_handshake(
        &self,
        runtime: &super::runtime::RuntimeStatus,
        workspace: &str,
        cleanup: &super::connection::AcpCleanup,
    ) -> Result<serde_json::Value, ProviderError> {
        let profile =
            cleanup.retain_profile(super::profile::create(self.kind, &self.runtime_root, &[])?);
        let connection = super::AcpConnection::launch_retained(
            super::AcpLaunch {
                command: &runtime.command,
                environment: &runtime.environment,
                workspace,
                profile: (self.kind == AcpKind::Antigravity).then_some(profile.path()),
            },
            Arc::new(AcpSessionState::new_binding(
                self.kind,
                self.admission.clone(),
            )),
            cleanup,
        )
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Transport,
            )
        })?;
        let requirement = super::conversation::runtime_requirement(
            self.kind,
            runtime
                .command
                .first()
                .map(PathBuf::from)
                .unwrap_or_else(|| "ACP".into()),
        )?;
        let result = async {
            let initialized = connection
                .request("initialize", initialize_params())
                .await
                .map_err(|_| {
                    failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol)
                })?;
            super::conversation::validate_initialization(&requirement, &initialized)?;
            Ok(initialized)
        }
        .await;
        let collected = cleanup
            .shutdown()
            .await
            .map_err(|error| with_operation(ProviderOperation::Inspect, error));
        match (result, collected) {
            (Ok(initialized), Ok(())) => Ok(initialized),
            (Err(cause), Ok(())) => Err(cause),
            (Ok(_), Err(cleanup)) => Err(cleanup),
            (Err(cause), Err(cleanup)) => Err(ProviderError::PreparationCleanup {
                cause: Box::new(cause),
                cleanup: Box::new(cleanup),
            }),
        }
    }
}

#[async_trait]
impl forge_provider_api::provider::Provider for AcpProvider {
    async fn thread_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        request.validate_thread_owner()?;
        let provider = self.clone();
        let worker_context = context.clone();
        let payload = request.identity();
        let cleanup = self.cleanup.scope();
        let interrupt_cleanup = cleanup.clone();
        self.operations
            .open
            .take(
                context,
                ProviderOperation::ThreadOpen,
                payload,
                async move {
                    provider
                        .open_session(worker_context, request, cleanup)
                        .await
                },
                move |interruption| {
                    interrupt_cleanup.close_admission();
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::ThreadOpen,
                            ProviderPhase::Preparation,
                            FailureDisposition::Uncertain,
                        ),
                        async move { interrupt_cleanup.shutdown().await },
                    )
                },
            )
            .await
    }

    async fn thread_configure(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: forge_provider_api::SessionConfiguration,
    ) -> Result<forge_provider_api::ConfiguredSession, ProviderError> {
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let binding = binding.clone();
        let cleanup_binding = binding.clone();
        let payload = (binding.clone(), configuration.clone());
        let worker_context = context.clone();
        self.operations
            .configure
            .observe(
                context,
                ProviderOperation::ThreadConfigure,
                payload,
                async move {
                    provider
                        .configure_session(worker_context, &binding, configuration)
                        .await
                },
                move |interruption| {
                    cleanup_provider.interrupt_binding(
                        cleanup_binding,
                        ProviderOperation::ThreadConfigure,
                        interruption,
                        ProviderPhase::Preparation,
                    )
                },
            )
            .await
    }

    async fn thread_fork(
        &self,
        context: OperationContext,
        request: forge_provider_api::SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let source = request.source.clone();
        let payload = request.clone();
        let worker_context = context.clone();
        self.operations
            .fork
            .observe(
                context,
                ProviderOperation::ThreadFork,
                payload,
                async move { provider.fork_session(worker_context, request).await },
                move |interruption| cleanup_provider.interrupt_fork(source, interruption),
            )
            .await
    }

    async fn thread_compact(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let binding = binding.clone();
        let cleanup_binding = binding.clone();
        let payload = (binding.clone(), request.clone());
        let worker_context = context.clone();
        self.operations
            .compact
            .observe(
                context,
                ProviderOperation::ThreadCompact,
                payload,
                async move {
                    provider
                        .compact_session(worker_context, &binding, request)
                        .await
                },
                move |interruption| {
                    cleanup_provider.interrupt_binding(
                        cleanup_binding,
                        ProviderOperation::ThreadCompact,
                        interruption,
                        ProviderPhase::Observation,
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
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let binding = binding.clone();
        let cleanup_binding = binding.clone();
        self.operations
            .close
            .observe(
                context,
                ProviderOperation::ThreadClose,
                binding.clone(),
                async move { provider.close_session(&binding).await },
                move |interruption| {
                    cleanup_provider.interrupt_binding(
                        cleanup_binding,
                        ProviderOperation::ThreadClose,
                        interruption,
                        ProviderPhase::Close,
                    )
                },
            )
            .await
    }

    async fn turn_interrupt(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CancelRequest,
    ) -> Result<forge_provider_api::TurnInterruptResult, ProviderError> {
        let operation_id = context.id().clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let binding = binding.clone();
        let cleanup_binding = binding.clone();
        let payload = (binding.clone(), request.clone());
        let scope = request.scope.clone();
        self.operations
            .cancel
            .observe(
                context,
                ProviderOperation::TurnInterrupt,
                payload,
                async move {
                    provider
                        .cancel_invocation(&binding, request.clone())
                        .await?;
                    let session = provider.binding(&binding, ProviderOperation::TurnInterrupt)?;
                    Ok(forge_provider_api::TurnInterruptResult {
                        operation_id,
                        thread_id: request.scope.thread_id.clone(),
                        turn_id: request.scope.turn_id().map_err(|_| {
                            failure(
                                ProviderOperation::TurnInterrupt,
                                ProviderFailureKind::Protocol,
                            )
                        })?,
                        watermark: session
                            .collection_watermarks
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .get(&request.scope)
                            .copied()
                            .ok_or_else(|| {
                                failure(
                                    ProviderOperation::TurnInterrupt,
                                    ProviderFailureKind::Protocol,
                                )
                            })?,
                    })
                },
                move |interruption| {
                    cleanup_provider.interrupt_cancel(cleanup_binding, scope, interruption)
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
        request.validate_turn_identity()?;
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let binding = binding.clone();
        let cleanup_binding = binding.clone();
        let scope = request.scope.clone();
        let payload = (binding.clone(), request.identity());
        let worker_context = context.clone();
        let pending = self.pending_inputs.register(&binding, &scope, context.id());
        self.operations
            .submit
            .observe(
                context,
                ProviderOperation::TurnStart,
                payload,
                async move {
                    provider
                        .submit(worker_context, &binding, request, pending)
                        .await
                },
                move |interruption| {
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::TurnStart,
                            ProviderPhase::Dispatch,
                            FailureDisposition::Uncertain,
                        ),
                        async move {
                            cleanup_provider
                                .collect_interrupted_input(&cleanup_binding, scope)
                                .await
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
    ) -> Result<forge_provider_api::SteeringAcceptance, ProviderError> {
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let binding = binding.clone();
        let cleanup_binding = binding.clone();
        let payload = (binding.clone(), request.identity());
        let worker_context = context.clone();
        self.operations
            .steer
            .observe(
                context,
                ProviderOperation::TurnSteer,
                payload,
                async move {
                    provider
                        .steer_session(worker_context, &binding, request)
                        .await
                },
                move |interruption| {
                    cleanup_provider.interrupt_binding(
                        cleanup_binding,
                        ProviderOperation::TurnSteer,
                        interruption,
                        ProviderPhase::Observation,
                    )
                },
            )
            .await
    }

    async fn input_reconcile(
        &self,
        context: OperationContext,
        request: forge_provider_api::InputReconcileRequest,
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        let provider = self.clone();
        let payload = request.clone();
        self.operations
            .reconcile
            .observe(
                context,
                ProviderOperation::InputReconcile,
                payload,
                async move { provider.reconcile_input(request).await },
                move |interruption| {
                    Interruption::new(
                        interruption.failure(
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
        request: forge_provider_api::InputRetractRequest,
    ) -> Result<forge_provider_api::InputRetractOutcome, ProviderError> {
        let provider = self.clone();
        let binding = binding.clone();
        let payload = (binding.clone(), request.clone());
        self.operations
            .retract
            .observe(
                context,
                ProviderOperation::InputRetract,
                payload,
                async move { provider.retract_input(&binding, request).await },
                move |interruption| {
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::InputRetract,
                            ProviderPhase::Observation,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }

    async fn inspect(
        &self,
        context: OperationContext,
        workspace: forge_provider_api::catalog::WorkspaceContext,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        let provider = self.clone();
        let worker_context = context.clone();
        let payload = workspace.clone();
        let cleanup = self.cleanup.scope();
        let interrupt_cleanup = cleanup.clone();
        self.operations
            .inspect
            .observe(
                context,
                ProviderOperation::Inspect,
                payload,
                async move {
                    provider
                        .inspect_runtime(worker_context, workspace, cleanup)
                        .await
                },
                move |interruption| {
                    interrupt_cleanup.close_admission();
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::Inspect,
                            ProviderPhase::Observation,
                            FailureDisposition::Unchanged,
                        ),
                        async move { interrupt_cleanup.shutdown().await },
                    )
                },
            )
            .await
    }

    async fn authenticate(
        &self,
        context: OperationContext,
        workspace: forge_provider_api::catalog::WorkspaceContext,
        method: forge_provider_api::provider::AuthenticationMethodId,
    ) -> Result<(), ProviderError> {
        let provider = self.clone();
        let payload = (workspace.clone(), method.clone());
        let cleanup = self.cleanup.scope();
        let interrupt_cleanup = cleanup.clone();
        self.operations
            .authenticate
            .observe(
                context,
                ProviderOperation::Authenticate,
                payload,
                async move {
                    provider
                        .authenticate_runtime(workspace, method, cleanup)
                        .await
                },
                move |interruption| {
                    interrupt_cleanup.close_admission();
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::Authenticate,
                            ProviderPhase::Observation,
                            FailureDisposition::Uncertain,
                        ),
                        async move { interrupt_cleanup.shutdown().await },
                    )
                },
            )
            .await
    }

    async fn shutdown(&self, context: OperationContext) -> Result<(), ProviderError> {
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let operation = context.id().clone();
        let cleanup_operation = operation.clone();
        self.operations
            .shutdown
            .observe(
                context,
                ProviderOperation::Shutdown,
                (),
                async move { provider.shutdown_sessions(operation).await },
                move |interruption| {
                    cleanup_provider.admission.store(false, Ordering::Release);
                    cleanup_provider.operations.close_admission();
                    cleanup_provider.cleanup.close_admission();
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::Shutdown,
                            ProviderPhase::Close,
                            FailureDisposition::Invalidated,
                        ),
                        async move { cleanup_provider.shutdown_sessions(cleanup_operation).await },
                    )
                },
            )
            .await
    }

    async fn model_list(
        &self,
        context: OperationContext,
        query: forge_provider_api::catalog::QueryContext,
    ) -> Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        let provider = self.clone();
        let payload = query.clone();
        self.operations
            .models
            .observe(
                context,
                ProviderOperation::ModelList,
                payload,
                async move { provider.list_models(query).await },
                move |interruption| {
                    Interruption::new(
                        interruption.failure(
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
        query: forge_provider_api::catalog::QueryContext,
    ) -> Result<Vec<forge_provider_api::catalog::SkillDefinition>, ProviderError> {
        let provider = self.clone();
        let payload = query.clone();
        self.operations
            .skills
            .observe(
                context,
                ProviderOperation::SkillList,
                payload,
                async move { provider.list_skills(query).await },
                move |interruption| {
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::SkillList,
                            ProviderPhase::Observation,
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
        query: forge_provider_api::catalog::McpQuery,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let provider = self.clone();
        let payload = query.clone();
        self.operations
            .mcp
            .observe(
                context,
                ProviderOperation::McpList,
                payload,
                async move { provider.list_mcp(query).await },
                move |interruption| {
                    Interruption::new(
                        interruption.failure(
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
    ) -> Result<forge_provider_api::terminal::TerminalInventory, ProviderError> {
        self.operations
            .terminals
            .observe(
                context,
                ProviderOperation::TerminalList,
                binding.clone(),
                async {
                    Err(forge_provider_api::capability::UnsupportedCapability(
                        forge_provider_api::capability::ProviderFeature::BackgroundTerminals,
                    )
                    .into())
                },
                move |interruption| {
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::TerminalList,
                            ProviderPhase::Observation,
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
        terminal: &forge_provider_api::terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        self.operations
            .terminate
            .observe(
                context,
                ProviderOperation::TerminalTerminate,
                (binding.clone(), terminal.clone()),
                async {
                    Err(forge_provider_api::capability::UnsupportedCapability(
                        forge_provider_api::capability::ProviderFeature::BackgroundTermination,
                    )
                    .into())
                },
                move |interruption| {
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::TerminalTerminate,
                            ProviderPhase::Observation,
                            FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
    }
}

/// Advertise only the ACP features implemented by this native adapter.
fn initialize_params() -> serde_json::Value {
    serde_json::json!({
        "protocolVersion":1,
        "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
        "clientCapabilities":{"auth":{"terminal":true},"session":{"compaction":{},"configOptions":{"boolean":{}},"notices":{}},"subagents":{}}
    })
}

/// Get all journaled input identities accepted under the exact retained invocation.
fn covered_inputs(
    session: &AcpBinding,
    scope: &forge_provider_api::InvocationScope,
) -> Result<Vec<forge_provider_api::CoveredInput>, ProviderError> {
    session
        .covered_inputs
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(scope)
        .cloned()
        .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))
}

/// Retain the shared runner's collected result before releasing prompt admission.
async fn collect_invocation(
    session: Arc<AcpBinding>,
    scope: forge_provider_api::InvocationScope,
    task: Arc<crate::turn_runner::RetainedTurn>,
) {
    let result = task.join().await;
    let record = session
        .settlements
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(&scope)
        .cloned()
        .expect("admitted turn retains its collection result");
    session
        .collection_watermarks
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(scope.clone(), session.sink.watermark());
    {
        let mut active = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if active.as_ref().is_some_and(|active| active.scope == scope) {
            active.take();
        }
    }
    record.complete(result).await;
}

/// Attempt every binding cleanup phase while preserving the first failure for retry.
async fn close_binding(session: Arc<AcpBinding>) -> Result<(), ProviderError> {
    let mut errors = Vec::new();
    session.state.revoke_callbacks().await;
    let scope = {
        let mut active = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        active.as_mut().map(|active| {
            active.cancelled = true;
            active.cancellation.cancel();
            active.scope.clone()
        })
    };
    if scope.is_some()
        && let Err(error) = session.conversation.cancel()
    {
        errors.push(error);
    }
    // Explicit binding close owns the entire transport, including unresponsive prompts.
    // Collect it before joining settlement instead of adding a second cancellation timer.
    let collected = match session.cleanup.shutdown().await {
        Ok(()) => true,
        Err(error) => {
            errors.push(with_operation(ProviderOperation::ThreadClose, error));
            false
        }
    };
    if let Some(scope) = scope {
        let result = session.settlement(&scope).await;
        if let Err(error) = result {
            errors.push(error);
        }
    }
    let _operation = session.operation.lock().await;
    // A pending configuration may have registered more resources while native interruption ran.
    if collected && let Err(error) = session.cleanup.shutdown().await {
        errors.push(with_operation(ProviderOperation::ThreadClose, error));
    }
    if errors.is_empty() && session.cleanup.is_collected() {
        session.state.mark_collected();
        session.history_released.store(true, Ordering::Release);
        return Ok(());
    }
    if errors.is_empty() {
        errors.push(failure(
            ProviderOperation::ThreadClose,
            ProviderFailureKind::ResourceConflict,
        ));
    }
    let error = combine_errors(errors);
    session.state.retain_failure(error.clone()).await;
    Err(error)
}

async fn run_prompt(
    session: Arc<AcpBinding>,
    request: PromptRequest,
    accepted: tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>,
    start: forge_provider_api::NativeTurnStart,
) -> Result<forge_provider_api::TurnResult, ProviderError> {
    let mut accepted = Some(accepted);
    let result = async {
        let cancellation = session.active.lock().unwrap_or_else(std::sync::PoisonError::into_inner).as_ref().filter(|active| active.scope == request.scope).map(|active| active.cancellation.clone()).ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::WrongInvocation))?;
        let preparation = tokio::select! {
            biased;
            _ = cancellation.cancelled() => Err(ProviderError::Cancelled),
            result = request.turn_control.before_send(&request.scope) => result,
        };
        if let Err(error) = preparation {
            session.rejected_inputs.lock().await.push((request.scope.clone(), request.submission_id.clone(), request.attempt_id.clone()));
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        if !session.state.is_admitting() || session.active.lock().unwrap_or_else(std::sync::PoisonError::into_inner).as_ref().is_some_and(|active| active.cancelled) {
            session.rejected_inputs.lock().await.push((request.scope.clone(), request.submission_id.clone(), request.attempt_id.clone()));
            return Err(reject_before_dispatch(&request.receipt, ProviderError::Cancelled).await);
        }
        let _operation = session.operation.lock().await;
        session.sink.check()?;
        if let Err(error) = session
            .conversation
            .bind_tool_endpoint(request.tool_endpoint.as_ref())
            .await
        {
            let error = with_operation(ProviderOperation::TurnStart, error);
            if matches!(&error, ProviderError::Failure(failure) if matches!(failure.disposition(), FailureDisposition::Uncertain | FailureDisposition::Invalidated)) {
                session.invalidate(error.clone()).await;
            }
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        let event_sink = session.sink.invocation_sink(&request.scope)?;
        if let Err(error) = session
            .state
            .begin_native(
                session.conversation.id(),
                event_sink.clone(),
                request.permissions.clone(),
            )
            .await
        {
            let error = if cancellation.is_cancelled() { ProviderError::Cancelled } else { error };
            if error.is_clean_cancellation() { session.rejected_inputs.lock().await.push((request.scope.clone(), request.submission_id.clone(), request.attempt_id.clone())); }
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        session.state.install_native_start(start).await;
        let _address = session
            .state
            .address(session.conversation.id())
            .await
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        let current_model = session.native_model.lock().await.clone();
        let prompt = match prompt_content(&request, session.conversation.supports_images(), session.conversation.prompt_instructions()) {
            Ok(prompt) => prompt,
            Err(error) => return Err(reject_before_dispatch(&request.receipt, error).await),
        };
        if let Err(error) = configure_model(
            &session,
            &request.configuration.model,
            &current_model,
            request.execution,
        )
        .await
        {
            let error = with_operation(ProviderOperation::TurnStart, error);
            if matches!(&error, ProviderError::Failure(failure) if failure.disposition() == forge_provider_api::FailureDisposition::Invalidated) { session.invalidate(error.clone()).await; }
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        *session.native_model.lock().await = request.configuration.model.clone();
        let cancellation = session.active.lock().unwrap_or_else(std::sync::PoisonError::into_inner).as_ref().filter(|active| active.scope == request.scope).map(|active| active.cancellation.clone()).ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::WrongInvocation))?;
        if cancellation.is_cancelled() || !session.state.is_admitting() {
            session.rejected_inputs.lock().await.push((request.scope.clone(), request.submission_id.clone(), request.attempt_id.clone()));
            return Err(reject_before_dispatch(&request.receipt, ProviderError::Cancelled).await);
        }
        session.sink.check()?;
        let response = session
            .conversation
            .submit(
                super::submission::AcpSubmission::Prompt,
                serde_json::json!({"sessionId":session.conversation.id(),"prompt":prompt}),
                Some(request.receipt.clone()),
                &request.dispatch,
                &cancellation,
            )
            .await;
        let _response = match response {
            Ok(response) => response,
            Err(error) => {
                let started = session.state.execution_started().await;
                let proven_unconsumed = match &error {
                    ProviderError::NotConsumed => true,
                    ProviderError::Failure(native) => {
                        native.kind() == ProviderFailureKind::Rejected && native.disposition() == forge_provider_api::FailureDisposition::Unchanged
                    }
                    _ => false,
                };
                let uncertain_delivery = !proven_unconsumed;
                if uncertain_delivery { session.invalidate(error.clone()).await; }
                let mut cleanup = Vec::new();
                if started || uncertain_delivery {
                    if let Err(cleanup_error) = session
                        .state
                        .finish(forge_provider_api::turn::TurnOutcome::Failed { error: forge_provider_api::ProviderFailure::new(forge_provider_api::ProviderOperation::TurnStart, forge_provider_api::ProviderFailureKind::Unknown).at_phase(forge_provider_api::ProviderPhase::Observation, forge_provider_api::FailureDisposition::Uncertain), retry: forge_provider_api::turn::RetryDecision::NotAllowed })
                        .await
                    {
                        cleanup.push(cleanup_error);
                    }
                } else {
                    session.state.revoke_callbacks().await;
                    if let Err(cleanup_error) = session
                        .state
                        .finish(forge_provider_api::turn::TurnOutcome::Failed { error: forge_provider_api::ProviderFailure::new(forge_provider_api::ProviderOperation::TurnStart, forge_provider_api::ProviderFailureKind::Unknown).at_phase(forge_provider_api::ProviderPhase::Observation, forge_provider_api::FailureDisposition::Uncertain), retry: forge_provider_api::turn::RetryDecision::NotAllowed })
                        .await
                    {
                        cleanup.push(cleanup_error);
                    }
                }
                return Err(with_cleanup(error, cleanup));
            }
        };
        let native_end = session.conversation.prompt_end(&forge_provider_api::CoveredInput::new(
            request.submission_id.clone(), request.attempt_id.clone()))
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        let completed_at_ms = native_end.completed_at_ms();
        if let Some(sender) = accepted.take() {
            let _ = sender.send(Ok(InputAcceptance {
                submission_id: request.submission_id.clone(),
            }));
        }
        let _steering_boundary = session.steering.write().await;
        let native_outcome = match native_end.outcome() {
            super::PromptOutcome::Completed(reason) => forge_provider_api::turn::TurnOutcome::Completed { stop_reason: reason },
            super::PromptOutcome::Cancelled => forge_provider_api::turn::TurnOutcome::Interrupted,
        };
        if native_outcome == forge_provider_api::turn::TurnOutcome::Interrupted
            && !session.state.execution_started().await {
            session.state.finish(native_outcome).await?;
            return Err(ProviderError::Cancelled);
        }
        session.state.observe_native_start(completed_at_ms).await?;
        let outcome = native_outcome;
        let mut terminal_errors = Vec::new();
        if let Err(error) = session.state.finish(outcome).await {
            terminal_errors.push(error);
        }
        let completion = session.state.take_completion().await;
        let checkpoint = completion.provider_checkpoint_id.clone().map(forge_provider_api::CheckpointId::new)
            .transpose().map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        if !terminal_errors.is_empty() {
            return Err(combine_errors(terminal_errors));
        }
        forge_provider_api::TurnResult::new(outcome, completed_at_ms, None,
            covered_inputs(&session, &request.scope)?, Vec::new(), checkpoint)
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))

    }
    .await;
    match result {
        Ok(result) => Ok(result),
        Err(error) => {
            let mut error = error;
            if accepted.is_some() && session.state.has_active_work().await {
                session.state.revoke_callbacks().await;
                if let Err(cleanup) = session
                    .state
                    .finish(forge_provider_api::turn::TurnOutcome::Failed {
                        error: forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Unknown,
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Observation,
                            forge_provider_api::FailureDisposition::Uncertain,
                        ),
                        retry: forge_provider_api::turn::RetryDecision::NotAllowed,
                    })
                    .await
                {
                    error = with_cleanup(error, vec![cleanup]);
                }
            }
            if let Some(sender) = accepted.take() {
                let _ = sender.send(Err(error.clone()));
                return Err(error);
            }
            Err(error)
        }
    }
}

/// Resolve provider-default resets from the native snapshot captured before task overrides.
fn resolve_native_model(
    session: &AcpBinding,
    requested: &forge_provider_api::ModelSelection,
    retained: &forge_provider_api::ModelSelection,
) -> Result<forge_provider_api::ModelSelection, ProviderError> {
    use forge_provider_api::generation::ReasoningSelection;
    let mut resolved = requested.clone();
    if requested.model.is_none() && retained.model.is_some() {
        resolved.model = Some(session.initial_model.model.clone().ok_or(
            forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ModelDefaultReset,
            ),
        )?);
    }
    if requested.reasoning == ReasoningSelection::Default
        && retained.reasoning != ReasoningSelection::Default
    {
        if session.initial_model.reasoning == ReasoningSelection::Default {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ReasoningDefaultReset,
            )
            .into());
        }
        resolved.reasoning = session.initial_model.reasoning.clone();
    }
    Ok(resolved)
}

/// Reject unsupported native controls before allocating profiles or changing history ownership.
fn validate_configuration(
    model: &forge_provider_api::session::ModelSelection,
    access: &forge_provider_api::access::AccessPolicy,
) -> Result<(), ProviderError> {
    use forge_provider_api::capability::{ProviderFeature, UnsupportedCapability};
    if access.sandbox {
        return Err(UnsupportedCapability(ProviderFeature::OsSandbox).into());
    }
    if access.isolation_mode.is_some() {
        return Err(UnsupportedCapability(ProviderFeature::IsolationMode).into());
    }
    validate_model_selection(model)
}

/// Reject model shapes that ACP cannot encode without contacting the peer.
fn validate_model_selection(
    requested: &forge_provider_api::session::ModelSelection,
) -> Result<(), ProviderError> {
    use forge_provider_api::ServiceTier;
    if requested.service_tier != ServiceTier::Standard {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ServiceTier,
        )
        .into());
    }
    if requested.context_window.is_some() {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ContextWindowSelection,
        )
        .into());
    }
    Ok(())
}

/// Apply one invocation's supported ACP model and reasoning choices before prompt dispatch.
async fn configure_model(
    session: &AcpBinding,
    requested: &forge_provider_api::session::ModelSelection,
    retained: &forge_provider_api::session::ModelSelection,
    mode: forge_provider_api::ExecutionMode,
) -> Result<(), ProviderError> {
    use forge_provider_api::generation::ReasoningSelection;

    validate_model_selection(requested)?;
    let resolved = resolve_native_model(session, requested, retained)?;
    let requested = &resolved;

    let models = session.conversation.models().await.map_err(|_| {
        failure(
            ProviderOperation::ThreadConfigure,
            ProviderFailureKind::Protocol,
        )
    })?;
    let selected_model = match requested.model.as_deref() {
        Some(model) => {
            if !models.iter().any(|candidate| candidate.id == model) {
                return Err(failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::Rejected,
                ));
            }
            Some(model)
        }
        None => models
            .iter()
            .find(|model| model.is_default)
            .map(|model| model.id.as_str()),
    };
    if let ReasoningSelection::Level(level) = &requested.reasoning
        && !selected_model
            .and_then(|id| models.iter().find(|model| model.id == id))
            .is_some_and(|model| model.reasoning.contains(level))
    {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::EffortSelection,
        )
        .into());
    }
    let effort = match &requested.reasoning {
        ReasoningSelection::Default => "default",
        ReasoningSelection::Level(level) => level.as_str(),
        ReasoningSelection::Lowest => {
            let reasoning = selected_model
                .and_then(|id| models.iter().find(|model| model.id == id))
                .and_then(|model| model.reasoning.first())
                .ok_or({
                    forge_provider_api::capability::UnsupportedCapability(
                        forge_provider_api::capability::ProviderFeature::EffortSelection,
                    )
                })?;
            reasoning.as_str()
        }
    };
    session
        .conversation
        .configure(
            requested.model.as_deref().unwrap_or("default"),
            effort,
            mode,
        )
        .await
        .map_err(|_| {
            ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::Protocol,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Preparation,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
            )
        })
}

/// Preserve independent cleanup failures without recursively nesting combined reports.
fn with_cleanup(primary: ProviderError, cleanup: Vec<ProviderError>) -> ProviderError {
    let mut failures = Vec::new();
    flatten_failures(primary, &mut failures);
    for failure in cleanup {
        flatten_failures(failure, &mut failures);
    }
    let primary = failures.remove(0);
    if failures.is_empty() {
        primary
    } else {
        ProviderError::Combined {
            primary: Box::new(primary),
            secondary: failures,
        }
    }
}

/// Preserve the first failure when several independent lifecycle phases failed.
fn combine_errors(mut failures: Vec<ProviderError>) -> ProviderError {
    let primary = failures.remove(0);
    with_cleanup(primary, failures)
}

/// Flatten only aggregate reports while retaining each typed leaf cause and its order.
fn flatten_failures(error: ProviderError, failures: &mut Vec<ProviderError>) {
    match error {
        ProviderError::Combined { primary, secondary } => {
            flatten_failures(*primary, failures);
            for failure in secondary {
                flatten_failures(failure, failures);
            }
        }
        error => failures.push(error),
    }
}

/// Record positive local no-send evidence while retaining an independent journal failure.
async fn reject_before_dispatch(
    receipt: &Arc<dyn forge_provider_api::submission::InputReceipt>,
    cause: ProviderError,
) -> ProviderError {
    match receipt.clone().record_rejected(cause.clone()).await {
        Ok(()) => ProviderError::PreparationFailed(Box::new(cause)),
        Err(cleanup) => ProviderError::PreparationCleanup {
            cause: Box::new(cause),
            cleanup: Box::new(cleanup),
        },
    }
}

struct AcpCheckpoint {
    accepted: bool,
    session_id: String,
    submission_id: String,
    attempt_id: String,
}

/// Decode only exact-attempt receipts issued under the current host session contract.
fn parse_checkpoint(
    token: &forge_provider_api::RecoveryToken,
) -> Result<AcpCheckpoint, ProviderError> {
    let value: serde_json::Value = serde_json::from_slice(token.as_bytes()).map_err(|_| {
        failure(
            ProviderOperation::InputReconcile,
            ProviderFailureKind::Protocol,
        )
    })?;
    if value["version"] != 2
        || !matches!(
            value["method"].as_str(),
            Some("session/prompt" | "_session/steering")
        )
    {
        return Err(failure(
            ProviderOperation::InputReconcile,
            ProviderFailureKind::Protocol,
        ));
    }
    let field = |name: &str| {
        value[name]
            .as_str()
            .filter(|value| !value.trim().is_empty())
            .map(str::to_owned)
            .ok_or_else(|| {
                failure(
                    ProviderOperation::InputReconcile,
                    ProviderFailureKind::Protocol,
                )
            })
    };
    Ok(AcpCheckpoint {
        accepted: value["accepted"].as_bool().ok_or_else(|| {
            failure(
                ProviderOperation::InputReconcile,
                ProviderFailureKind::Protocol,
            )
        })?,
        session_id: field("session")?,
        submission_id: field("submission")?,
        attempt_id: field("attempt")?,
    })
}

pub(super) fn normalized_mcp_reason(
    reason: Option<&str>,
) -> forge_provider_api::diagnostic::SanitizedMessage {
    let message = match reason {
        Some("authentication_required") | Some("authenticationRequired") => {
            "MCP resource requires authentication."
        }
        Some("permission_denied") | Some("permissionDenied") => "MCP resource access was denied.",
        Some("connection_failed") | Some("connectionFailed") => "MCP resource connection failed.",
        Some("initialization_failed") | Some("initializationFailed") => {
            "MCP resource could not initialize."
        }
        _ => "MCP resource is unavailable.",
    };
    forge_provider_api::diagnostic::SanitizedMessage::new(message)
        .expect("static normalized MCP messages are nonempty")
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

fn prompt_input(input: &forge_provider_api::PromptInput) -> String {
    match input {
        forge_provider_api::PromptInput::Text(text) => text.clone(),
        forge_provider_api::PromptInput::Skill { name, arguments } => {
            format!("/{name} {arguments}")
        }
        forge_provider_api::PromptInput::QuestionReply(reply) => {
            let response = match &reply.response {
                forge_provider_api::QuestionReplyResponse::Answer {
                    answer,
                    required_questions_remain,
                } => {
                    format!(
                        "Answer: {answer}\nRequired questions remain: {required_questions_remain}"
                    )
                }
                forge_provider_api::QuestionReplyResponse::Clarification {
                    options,
                    text,
                    reply_to,
                } => {
                    let options = options
                        .iter()
                        .map(|option| {
                            format!(
                                "{} [{}]: {}{}",
                                option.label,
                                option.id,
                                option.description,
                                if option.recommended {
                                    " (recommended)"
                                } else {
                                    ""
                                }
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("\n");
                    format!(
                        "Clarification: {text}\nOptions:\n{options}\nReply to: {}",
                        reply_to.as_deref().unwrap_or("none")
                    )
                }
                forge_provider_api::QuestionReplyResponse::Correction {
                    answer,
                    required_questions_remain,
                } => {
                    format!(
                        "Correction: {answer}\nRequired questions remain: {required_questions_remain}"
                    )
                }
            };
            format!(
                "Question response for {} (revision {}, message {}):\n{}\n{}",
                reply.question_id, reply.revision, reply.message_id, reply.question, response
            )
        }
    }
}

/// Encode current and recovered typed inputs without dropping their instructions or files.
fn prompt_content(
    request: &PromptRequest,
    supports_images: bool,
    base_instructions: Option<&str>,
) -> Result<Vec<serde_json::Value>, ProviderError> {
    let compact = request.recovered_inputs.is_empty()
        && matches!(&request.input, forge_provider_api::PromptInput::Text(text) if text == "/compact");
    if compact {
        return Ok(vec![serde_json::json!({"type":"text","text":"/compact"})]);
    }

    let mut content = Vec::new();
    if let Some(instructions) = base_instructions {
        content.push(serde_json::json!({"type":"text","text":instructions}));
    }
    for (index, recovered) in request.recovered_inputs.iter().enumerate() {
        content.push(serde_json::json!({
            "type":"text",
            "text":format!("Earlier user input {} was saved locally but not accepted:", index + 1),
        }));
        if !recovered.instructions.is_empty() {
            content.push(serde_json::json!({"type":"text","text":recovered.instructions}));
        }
        let mut attachments =
            super::attachments::input(String::new(), &recovered.attachments, supports_images)?;
        attachments.pop();
        content.extend(attachments);
        content.push(serde_json::json!({"type":"text","text":prompt_input(&recovered.input)}));
    }
    if !request.recovered_inputs.is_empty() {
        content.push(serde_json::json!({"type":"text","text":"Newest user input:"}));
    }
    if !request.instructions.is_empty() {
        content.push(serde_json::json!({"type":"text","text":request.instructions}));
    }
    let mut attachments =
        super::attachments::input(String::new(), &request.attachments, supports_images)?;
    attachments.pop();
    content.extend(attachments);
    content.push(serde_json::json!({"type":"text","text":prompt_input(&request.input)}));
    Ok(content)
}

fn mcp_definitions(
    selection: &forge_provider_api::session::ResourceSelection,
) -> Vec<forge_provider_api::catalog::McpDefinition> {
    selection
        .mcp
        .iter()
        .map(|launch| forge_provider_api::catalog::McpDefinition {
            resource_id: None,
            scope: None,
            source_path: None,
            name: launch.name.clone(),
            transport: match &launch.transport {
                forge_provider_api::session::McpTransport::Stdio { .. } => {
                    forge_provider_api::catalog::McpTransportKind::Stdio
                }
                forge_provider_api::session::McpTransport::Http { .. } => {
                    forge_provider_api::catalog::McpTransportKind::Http
                }
                forge_provider_api::session::McpTransport::Sse { .. } => {
                    forge_provider_api::catalog::McpTransportKind::Sse
                }
            },
            enabled: true,
            status: forge_provider_api::catalog::McpStatus::Loading,
            status_detail: None,
            token_count: None,
            token_estimated: false,
            tools: Vec::new(),
            tool_error: None,
        })
        .collect()
}

fn same_resources(
    current: &forge_provider_api::session::ResourceSelection,
    desired: &forge_provider_api::session::ResourceSelection,
) -> bool {
    current.skills == desired.skills
        && current.mcp.len() == desired.mcp.len()
        && current
            .mcp
            .iter()
            .zip(&desired.mcp)
            .all(|(current, desired)| {
                if current.name != desired.name
                    || current.timeout != desired.timeout
                    || current.working_directory != desired.working_directory
                    || current.tools != desired.tools
                {
                    return false;
                }
                match (&current.transport, &desired.transport) {
                    (
                        forge_provider_api::session::McpTransport::Stdio {
                            command: left_command,
                            arguments: left_arguments,
                            environment: left_environment,
                        },
                        forge_provider_api::session::McpTransport::Stdio {
                            command: right_command,
                            arguments: right_arguments,
                            environment: right_environment,
                        },
                    ) => {
                        left_command == right_command
                            && left_arguments == right_arguments
                            && left_environment == right_environment
                    }
                    (
                        forge_provider_api::session::McpTransport::Http {
                            url: left_url,
                            headers: left_headers,
                        },
                        forge_provider_api::session::McpTransport::Http {
                            url: right_url,
                            headers: right_headers,
                        },
                    ) => left_url == right_url && left_headers == right_headers,
                    (
                        forge_provider_api::session::McpTransport::Sse {
                            url: left_url,
                            headers: left_headers,
                        },
                        forge_provider_api::session::McpTransport::Sse {
                            url: right_url,
                            headers: right_headers,
                        },
                    ) => left_url == right_url && left_headers == right_headers,
                    _ => false,
                }
            })
}

/// Preserve a typed native cause while classifying the requested operation and authority effect.
fn operation_failure(
    operation: ProviderOperation,
    error: &ProviderError,
    phase: forge_provider_api::ProviderPhase,
    disposition: forge_provider_api::FailureDisposition,
) -> ProviderError {
    let kind = match error {
        ProviderError::Failure(failure) => failure.kind(),
        ProviderError::Cancelled => ProviderFailureKind::Cancelled,
        _ => ProviderFailureKind::Unknown,
    };
    ProviderError::Failure(ProviderFailure::new(operation, kind).at_phase(phase, disposition))
}

/// Reclassify a normalized typed cause at the public operation that requested it.
fn with_operation(operation: ProviderOperation, error: ProviderError) -> ProviderError {
    match error {
        ProviderError::Failure(failure) => ProviderError::Failure(
            ProviderFailure::new(operation, failure.kind())
                .at_phase(failure.phase(), failure.disposition()),
        ),
        error => error,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::provider::Provider;

    /// Create a fresh bounded host operation whose dropped cancellation source stays inert.
    fn operation_context() -> forge_provider_api::OperationContext {
        let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
        forge_provider_api::OperationContext::new(
            forge_provider_api::OperationId::new(uuid::Uuid::new_v4().to_string()).unwrap(),
            std::time::Duration::from_secs(10),
            cancellation,
        )
        .unwrap()
    }

    /// Create a transport-free adapter for durable recovery-evidence decisions.
    fn provider() -> AcpProvider {
        AcpProvider::new(
            forge_provider_api::ProviderId::new("claude").unwrap(),
            vec!["must-not-launch".into()],
            PathBuf::from("unused-acp-fixture"),
        )
        .unwrap()
    }

    /// Prepare a deterministic native peer whose open or initial configuration can fail.
    fn failed_open_fixture(
        missing_identity: bool,
    ) -> (tempfile::TempDir, AcpProvider, SessionRequest) {
        let directory = tempfile::tempdir().unwrap();
        let script = directory.path().join("failed-open.cjs");
        std::fs::write(&script, r#"
const readline=require('node:readline');
if(process.argv.includes('--version')){console.log('2.1.302');process.exit(0);}
readline.createInterface({input:process.stdin}).on('line',line=>{
 const request=JSON.parse(line);if(request.id===undefined)return;let result={};
 if(request.method==='initialize')result={protocolVersion:1,agentInfo:{version:'99.0.0'},agentCapabilities:{loadSession:true}};
 if(request.method==='session/load'&&process.argv[2]==='missing'){process.stdout.write(JSON.stringify({jsonrpc:'2.0',id:request.id,error:{code:-32603,message:'injected load failure'}})+'\n');return;}
 if(request.method==='session/new'||request.method==='session/load')result=process.argv[2]==='missing'?{}:{sessionId:'failed-open-history'};
 process.stdout.write(JSON.stringify({jsonrpc:'2.0',id:request.id,result})+'\n');
});
"#).unwrap();
        let node = forge_shell::process::executable("node")
            .expect("Node.js is required by the ACP failure fixture");
        let provider = AcpProvider::new(
            forge_provider_api::ProviderId::new("antigravity").unwrap(),
            vec![
                node.to_string_lossy().into_owned(),
                script.to_string_lossy().into_owned(),
                if missing_identity {
                    "missing".into()
                } else {
                    "valid".into()
                },
            ],
            directory.path().join("runtime"),
        )
        .unwrap();
        let request = SessionRequest {
            thread_id: (forge_provider_api::testing::root_thread("failed-open")).clone(),
            mode: forge_provider_api::SessionMode::Interactive,
            workspace: directory.path().to_string_lossy().into_owned(),
            history: forge_provider_api::SessionHistory::Durable { resume: None },
            configuration: forge_provider_api::SessionConfiguration {
                model: Default::default(),
                access: forge_provider_api::access::AccessPolicy {
                    sandbox: false,
                    ..Default::default()
                },
                approval: forge_provider_api::approval::ApprovalMode::Ask,
                resources: Default::default(),
            },
            resource_lease: None,
            tools: Vec::new(),
            tool_endpoint: None,
            system_instructions: String::new(),
            trace: Arc::new(IgnoreTrace),
        };
        (directory, provider, request)
    }

    /// Exact cancel must wait for queued native ownership to prove no future registration.
    #[tokio::test]
    async fn cancel_waits_for_queued_input_before_reporting_wrong_invocation() {
        let (_directory, provider, request) = failed_open_fixture(false);
        let opened = provider
            .thread_open(operation_context(), request)
            .await
            .unwrap();
        let binding = opened.binding.id;
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::testing::root_thread("failed-open"),
            "queued-input",
        );
        let pending = provider
            .pending_inputs
            .register(&binding, &scope, operation_context().id());
        let successor_scope =
            forge_provider_api::InvocationScope::new(scope.thread_id.clone(), "successor");
        let successor =
            provider
                .pending_inputs
                .register(&binding, &successor_scope, operation_context().id());
        let (cancel_seen, seen) = tokio::sync::oneshot::channel();
        let (release, released) = tokio::sync::oneshot::channel();
        let worker = tokio::spawn(async move {
            pending.cancelled().await;
            let _ = cancel_seen.send(());
            let _ = released.await;
            drop(pending);
        });
        let cancelling_provider = provider.clone();
        let cancelling_binding = binding.clone();
        let cancelling_scope = scope.clone();
        let cancel = tokio::spawn(async move {
            cancelling_provider
                .turn_interrupt(
                    operation_context(),
                    &cancelling_binding,
                    forge_provider_api::CancelRequest {
                        scope: cancelling_scope,
                    },
                )
                .await
        });
        seen.await.unwrap();
        assert!(!cancel.is_finished());
        assert!(!successor.is_cancelled());
        release.send(()).unwrap();
        worker.await.unwrap();
        assert!(
            matches!(cancel.await.unwrap(), Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation)
        );
        let late = provider
            .pending_inputs
            .register(&binding, &scope, operation_context().id());
        assert!(late.is_cancelled());
        drop(late);
        drop(successor);
        provider
            .thread_close(operation_context(), &binding)
            .await
            .unwrap();
        provider.shutdown(operation_context()).await.unwrap();
    }

    /// An exact cancel which wins the wrapper race fences that scope before future registration.
    #[tokio::test]
    async fn absent_invocation_cancel_fences_late_input_without_touching_successor() {
        let (_directory, provider, request) = failed_open_fixture(false);
        let opened = provider
            .thread_open(operation_context(), request)
            .await
            .unwrap();
        let binding = opened.binding.id;
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::testing::root_thread("failed-open"),
            "not-yet-polled",
        );
        assert!(
            matches!(provider.turn_interrupt(operation_context(), &binding, forge_provider_api::CancelRequest { scope: scope.clone() }).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation)
        );
        let late = provider
            .pending_inputs
            .register(&binding, &scope, operation_context().id());
        assert!(late.is_cancelled());
        let successor_scope =
            forge_provider_api::InvocationScope::new(scope.thread_id, "successor");
        let successor =
            provider
                .pending_inputs
                .register(&binding, &successor_scope, operation_context().id());
        assert!(!successor.is_cancelled());
        drop(late);
        drop(successor);
        provider
            .thread_close(operation_context(), &binding)
            .await
            .unwrap();
        provider.shutdown(operation_context()).await.unwrap();
    }

    /// Build adapter-issued opaque evidence for a single stored delivery attempt.
    fn checkpoint(
        accepted: bool,
        submission: &str,
        attempt: &str,
    ) -> forge_provider_api::RecoveryToken {
        forge_provider_api::RecoveryToken::new(serde_json::to_vec(&serde_json::json!({"version":2,"accepted":accepted,"method":"session/prompt","session":"history","submission":submission,"attempt":attempt})).unwrap()).unwrap()
    }

    /// Build a recovery query that never opens or writes native history.
    fn reconciliation(
        token: forge_provider_api::RecoveryToken,
    ) -> forge_provider_api::InputReconcileRequest {
        forge_provider_api::InputReconcileRequest {
            submission_id: "input".try_into().unwrap(),
            attempt_id: forge_provider_api::DeliveryAttemptId::new("attempt").unwrap(),
            provider_session_id: Some("history".to_owned().try_into().unwrap()),
            token: Some(token),
            accepted_receipts: Vec::new(),
        }
    }

    /// An inconclusive read cannot authorize replay after a delayed native acceptance.
    #[tokio::test]
    async fn reconciliation_remains_unknown_without_an_exact_accepted_receipt() {
        let provider = provider();
        for accepted in [false, true] {
            let request = reconciliation(checkpoint(accepted, "input", "attempt"));
            assert!(matches!(
                provider
                    .input_reconcile(operation_context(), request)
                    .await
                    .unwrap(),
                forge_provider_api::SubmissionEvidence::Unknown
            ));
        }
    }

    struct IgnoreTrace;
    impl forge_provider_api::trace::ProviderTrace for IgnoreTrace {
        fn record(&self, _: forge_provider_api::trace::ProviderTraceRecord) {}
    }

    /// Unsupported native isolation must fail before an invalid launch command can run.
    #[tokio::test]
    async fn isolated_text_is_rejected_before_native_runtime_dispatch() {
        let provider = provider();
        let result = provider
            .thread_open(
                operation_context(),
                forge_provider_api::SessionRequest {
                    thread_id: (forge_provider_api::testing::root_thread("isolated")).clone(),
                    mode: forge_provider_api::SessionMode::Background,
                    workspace: "must-not-inspect".into(),
                    history: forge_provider_api::SessionHistory::IsolatedText(
                        forge_provider_api::IsolatedTextPolicy::new(),
                    ),
                    configuration: forge_provider_api::SessionConfiguration {
                        model: Default::default(),
                        access: forge_provider_api::access::AccessPolicy {
                            sandbox: false,
                            ..Default::default()
                        },
                        approval: forge_provider_api::approval::ApprovalMode::Ask,
                        resources: Default::default(),
                    },
                    resource_lease: None,
                    tools: Vec::new(),
                    tool_endpoint: None,
                    system_instructions: "supplied only".into(),
                    trace: Arc::new(IgnoreTrace),
                },
            )
            .await;
        assert!(matches!(
            result,
            Err(ProviderError::Unsupported(
                forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::TextGeneration
                )
            ))
        ));
        provider.shutdown(operation_context()).await.unwrap();
    }
}

/// Return the compaction Claude reported as completed or failed after `/compact`.
///
/// A failed compaction leaves the conversation unchanged, so the binding stays usable. When
/// Claude reports neither outcome, the conversation state is unknown and the binding is invalidated.
fn settled_compaction(
    latest: Option<forge_provider_api::Compaction>,
) -> Option<forge_provider_api::Compaction> {
    latest.filter(|compaction| {
        matches!(
            compaction.state,
            forge_provider_api::CompactionState::Completed
                | forge_provider_api::CompactionState::Failed
        )
    })
}

#[cfg(test)]
mod settled_compaction_tests {}
