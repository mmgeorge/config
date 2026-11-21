use super::{
    AcpKind,
    conversation::{AcpConversation, ConversationOptions},
    runtime::AcpRuntime,
    session::AcpSessionState,
    submission::reject_before_dispatch,
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

/// Leave time within the host drain for process, reader, and profile collection.
const NATIVE_CLOSE_GRACE: std::time::Duration = std::time::Duration::from_millis(500);

struct AcpBinding {
    binding: SessionBinding,
    owner: ThreadId,
    conversation: Arc<AcpConversation>,
    state: Arc<AcpSessionState>,
    sink: ProviderEventSink,
    cleanup: crate::resource_cleanup::ResourceCleanup,
    selection: Mutex<forge_provider_api::session::ResourceSelection>,
    native_model: Mutex<forge_provider_api::ModelSelection>,
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
    /// Check whether native input preparation or a prompt worker is still running.
    fn has_active_prompt(&self) -> bool {
        self.active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .is_some()
    }

    /// Fence prompt publication and prevent any successor from acquiring this binding.
    fn close_prompt_admission(&self) {
        let active = self
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some(active) = active.as_ref() {
            active.cancellation.stop();
        }
        // Root admission takes this same lock before installing the next publication gate.
        self.state.close_admission();
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
        self.settlement_record(scope)?.wait().await
    }

    /// Get the retained collector for exactly one admitted invocation.
    fn settlement_record(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<Arc<OperationRecord<forge_provider_api::CompletedTurn>>, ProviderError> {
        self.settlements
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(scope)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::WrongInvocation,
                )
            })
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
    terminate_binding: Store<BindingId, ()>,
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
            terminate_binding: Store::registered(identities.clone()),
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
        if let Err(error) = self.terminate_binding.join_all_except(id).await {
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
    cleanup: crate::resource_cleanup::ResourceCleanup,
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
        let active = self
            .session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some(active) = active.as_ref().filter(|active| active.scope == self.scope) {
            active.cancellation.stop();
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
    steering_end: Option<super::response::NativePromptEnd>,
    cancellation: Arc<super::connection::PromptCancellation>,
    task: Option<Arc<crate::turn_runner::RetainedTurn>>,
}

/// Own ACP processes, retained conversations, native event decoding, and completion.
#[derive(Clone)]
pub struct AcpProvider {
    kind: AcpKind,
    runtime: Arc<AcpRuntime>,
    cleanup: crate::resource_cleanup::ResourceCleanup,
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
            AcpRuntime::new(kind, command),
            runtime_root,
        ))
    }

    /// Create an ACP owner from its selected adapter and native launch dependencies.
    pub(super) fn from_runtime(runtime: AcpRuntime, runtime_root: PathBuf) -> Self {
        Self {
            kind: runtime.kind(),
            runtime: Arc::new(runtime),
            cleanup: crate::resource_cleanup::ResourceCleanup::default(),
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
            session.close_prompt_admission();
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
            session.close_prompt_admission();
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
                .cancel_invocation(
                    &binding,
                    forge_provider_api::CancelRequest {
                        scope,
                        input: forge_provider_api::operations::InterruptInput::Retain,
                    },
                )
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
            .cancel_invocation(
                binding,
                forge_provider_api::CancelRequest {
                    scope,
                    input: forge_provider_api::operations::InterruptInput::Retain,
                },
            )
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
            session.state.ensure_admission(ProviderOperation::TurnStart)?;
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
                steering_end: None,
                cancellation: Arc::default(),
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
        cleanup: crate::resource_cleanup::ResourceCleanup,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        context.check_admission(ProviderOperation::ThreadOpen)?;
        if !self.admission.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        validate_configuration(&request.configuration.model, &request.configuration.access)?;
        let mcp_servers = super::resources::mcp_launches(
            ProviderOperation::ThreadOpen,
            &request.configuration.resources.mcp,
        )?;
        let runtime = self
            .runtime
            .resolve()
            .map_err(|error| runtime_unavailable(ProviderOperation::ThreadOpen, error))?;
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
            let previous = self.binding(&existing, ProviderOperation::ThreadOpen)?;
            if let Err(error) = self.close_session(&existing).await {
                // Only release of this exact binding permits the replacement to write history.
                if !previous.history_released.load(Ordering::Acquire) {
                    return Err(error);
                }
                let failure = match &error {
                    ProviderError::Failure(failure) => Some(*failure),
                    _ => None,
                };
                tracing::warn!(target: "forge::provider", event_name = "provider.acp.replaced_after_close_failure",
                    provider = "antigravity", operation = ?ProviderOperation::ThreadClose,
                    kind = ?failure.map(ProviderFailure::kind), phase = ?failure.map(ProviderFailure::phase),
                    disposition = ?failure.map(ProviderFailure::disposition), error = %error);
            }
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
            &request.history,
        )?);
        let process_owner = request
            .resource_lease
            .as_ref()
            .and_then(|lease| lease.process_owner());
        cleanup.retain_lease(request.resource_lease.take());
        let selection = request.configuration.resources.clone();
        let state = Arc::new(AcpSessionState::new_binding(
            self.kind,
            self.admission.clone(),
            request.workspace.clone(),
        ));
        let conversation = match AcpConversation::open(
            super::AcpLaunch {
                process_owner,
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
                system_instructions: request.system_instructions.clone(),
            },
            &cleanup,
        )
        .await
        {
            Ok(conversation) => conversation,
            Err(cause) => {
                // A failed open has no public binding whose close can release its profile and lease.
                return Err(match cleanup.shutdown().await {
                    Ok(()) => cause,
                    Err(cleanup) => ProviderError::PreparationCleanup {
                        cause: Box::new(cause),
                        cleanup: Box::new(cleanup),
                    },
                });
            }
        };
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
        let requested_model = request.configuration.model.clone();
        let retained = Arc::new(AcpBinding {
            binding: binding.clone(),
            owner: request.thread_id.clone(),
            conversation: Arc::new(conversation),
            state,
            sink,
            cleanup: cleanup.clone(),
            selection: Mutex::new(selection),
            native_model: Mutex::new(forge_provider_api::ModelSelection::default()),
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
                    None,
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
        let resources_changed =
            !same_resources(&*session.selection.lock().await, &configuration.resources);
        if resources_changed {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ResourceSelection,
            )
            .into());
        }
        let stored_model = session.native_model.lock().await.clone();
        if let Err(error) = configure_model(
            &session,
            &configuration.model,
            &stored_model,
            forge_provider_api::ExecutionMode::Default,
            None,
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
        _request: forge_provider_api::session::SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        context.check_admission(ProviderOperation::ThreadFork)?;
        Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::NativeFork,
        )
        .into())
    }

    /// Require positive native compaction completion within the requested operation.
    async fn compact_session(
        &self,
        context: OperationContext,
        _binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        context.check_admission(ProviderOperation::ThreadCompact)?;
        request.validate_context(&context)?;
        Err(failure(
            ProviderOperation::ThreadCompact,
            ProviderFailureKind::Rejected,
        ))
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
        if !session.state.execution_started().await {
            return Err(ProviderError::NotConsumed);
        }
        let cancellation = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .as_ref()
            .filter(|active| active.scope == request.scope)
            .map(|active| active.cancellation.clone())
            .ok_or(ProviderError::NotConsumed)?;
        let mut rendered = forge_provider_api::forge::input_block(&request.input);
        rendered.push_str(&user_text(&request.input));
        let receipt = request.delivery.receipt().cloned();
        let prompt = match super::attachments::input(
            rendered,
            &request.attachments,
            session.conversation.supports_images(),
        ) {
            Ok(prompt) => prompt,
            Err(error) => {
                let error = error.for_operation(ProviderOperation::TurnSteer);
                return Err(match &receipt {
                    Some(receipt) => reject_before_dispatch(receipt, error).await,
                    None => ProviderError::PreparationFailed(Box::new(error)),
                });
            }
        };
        let covered = receipt
            .as_ref()
            .map(|receipt| {
                Ok::<_, ProviderError>(forge_provider_api::CoveredInput::new(
                    receipt.id().clone(),
                    receipt.attempt_id()?,
                ))
            })
            .transpose()?;
        if let Some(dispatch) = &request.dispatch
            && !covered.as_ref().is_some_and(|covered| {
                dispatch.matches(
                    binding,
                    &request.scope,
                    covered.submission_id(),
                    covered.attempt_id(),
                )
            })
        {
            return Err(failure(ProviderOperation::TurnSteer, ProviderFailureKind::WrongInvocation));
        }
        let response = session
            .conversation
            .submit(
                super::submission::AcpSubmission::Steer,
                serde_json::json!({"sessionId":session.conversation.id(),"prompt":prompt}),
                receipt,
                request.dispatch.as_ref(),
                Some(&cancellation),
            )
            .await;
        let native_end = match response {
            Ok(native_end) => native_end,
            Err(error) => {
                if !matches!(error, ProviderError::NotConsumed) {
                    cancellation.seal();
                    session.state.retain_failure(error.clone()).await;
                }
                return Err(error);
            }
        };
        let mut active = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let active = active
            .as_mut()
            .filter(|active| active.scope == request.scope)
            .ok_or_else(|| failure(ProviderOperation::TurnSteer, ProviderFailureKind::WrongInvocation))?;
        active.steering_end = Some(
            active
                .steering_end
                .map_or(native_end, |end| end.latest(native_end)),
        );
        if let Some(covered) = covered {
            let submission_id = covered.submission_id().clone();
            session
                .covered_inputs
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .get_mut(&request.scope)
                .ok_or_else(|| failure(ProviderOperation::TurnSteer, ProviderFailureKind::Protocol))?
                .push(covered);
            Ok(forge_provider_api::SteeringAcceptance::Journaled { submission_id })
        } else {
            Ok(forge_provider_api::SteeringAcceptance::Context)
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
        session.close_prompt_admission();
        let pending = self.pending_inputs.fence_binding(binding);
        session.state.revoke_authority().await;
        pending.await;
        let mut retained = session.close_task.lock().await;
        if let Some(task) = retained.as_ref()
            && task.is_finished()
            && task.join().await.is_err()
        {
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
        if session.history_released.load(Ordering::Acquire) {
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
            return crate::turn_runner::interrupt_outcome(result);
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
        crate::turn_runner::interrupt_outcome(session.settlement(&request.scope).await)
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
        cleanup: crate::resource_cleanup::ResourceCleanup,
    ) -> Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        let binding = match request {
            forge_provider_api::catalog::QueryContext::Binding(binding) => binding,
            forge_provider_api::catalog::QueryContext::Workspace(workspace) => {
                return self.workspace_models(&workspace.workspace, &cleanup).await;
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

    /// Read the launch definitions for the exact binding without claiming live readiness.
    async fn list_mcp(
        &self,
        query: forge_provider_api::catalog::McpQuery,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let request = match query {
            forge_provider_api::catalog::McpQuery::Configured(context)
            | forge_provider_api::catalog::McpQuery::Observed(context) => context,
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
        let definitions = mcp_definitions(&*session.selection.lock().await);
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

    /// Read the model catalog for a workspace that has no live binding.
    ///
    /// ACP publishes models only in a session response. Discovery opens a disposable session
    /// without configuring MCP servers or supplying instructions, and collects its private profile.
    async fn workspace_models(
        &self,
        workspace: &str,
        cleanup: &crate::resource_cleanup::ResourceCleanup,
    ) -> Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        let runtime = self.runtime.resolve()
            .map_err(|error| runtime_unavailable(ProviderOperation::ModelList, error))?;
        let profile =
            cleanup.retain_profile(super::profile::discovery(&self.runtime_root)?);
        let requirement = super::conversation::runtime_requirement(self.kind, &runtime.command);
        let connection = super::AcpConnection::launch_retained(
            super::AcpLaunch {
                process_owner: None,
                command: &runtime.command,
                environment: &runtime.environment,
                workspace,
                profile: (self.kind == AcpKind::Antigravity).then_some(profile.path()),
            },
            Arc::new(AcpSessionState::new_binding(
                self.kind,
                self.admission.clone(),
                workspace.to_owned(),
            )),
            cleanup,
        )
        .await
        .map_err(|error| {
            super::conversation::launch_failure(&requirement, ProviderOperation::ModelList, error)
        })?;
        let result = async {
            let initialized = super::conversation::initialize(
                &connection,
                &requirement,
                ProviderOperation::ModelList,
            ).await?;
            let parameter = serde_json::json!({"cwd":workspace,"mcpServers":[]});
            let session = connection
                .request("session/new", parameter)
                .await
                .map_err(|error| crate::error::acp(error, ProviderOperation::ModelList))?;
            let vision = initialized
                .pointer("/agentCapabilities/promptCapabilities/image")
                .and_then(serde_json::Value::as_bool)
                == Some(true);
            Ok(super::conversation::decode_model_catalog(&session, vision))
        }
        .await;
        let collected = cleanup
            .shutdown()
            .await
            .map_err(|error| with_operation(ProviderOperation::ModelList, error));
        match (result, collected) {
            (Ok(models), Ok(())) => Ok(models),
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

    /// Reap the binding's dedicated process tree without waiting for a protocol reply.
    async fn thread_terminate(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        let provider = self.clone();
        let binding = binding.clone();
        let worker_context = context.clone();
        self.operations
            .terminate_binding
            .observe(
                context,
                ProviderOperation::ThreadClose,
                binding.clone(),
                async move {
                    worker_context.check_admission(ProviderOperation::ThreadClose)?;
                    let Ok(session) = provider.binding(&binding, ProviderOperation::ThreadClose) else {
                        return Ok(());
                    };
                    session.close_prompt_admission();
                    drop(provider.pending_inputs.fence_binding(&binding));
                    session.cleanup.terminate().await?;
                    session.state.mark_collected();
                    session.history_released.store(true, Ordering::Release);
                    let mut owners = provider.owners.lock().await;
                    if owners.get(&session.owner) == Some(&binding) {
                        owners.remove(&session.owner);
                    }
                    Ok(())
                },
                |interruption| {
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::ThreadClose,
                            ProviderPhase::Close,
                            FailureDisposition::Uncertain,
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
                        withdrawn: None,
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
        // Workspace discovery owns a disposable native process; interruption must collect it.
        let cleanup = self.cleanup.scope();
        let interrupt_cleanup = cleanup.clone();
        self.operations
            .models
            .observe(
                context,
                ProviderOperation::ModelList,
                payload,
                async move { provider.list_models(query, cleanup).await },
                move |interruption| {
                    interrupt_cleanup.close_admission();
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::ModelList,
                            ProviderPhase::Observation,
                            FailureDisposition::Unchanged,
                        ),
                        async move { interrupt_cleanup.shutdown().await },
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
        let active = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        active
            .as_ref()
            .map(|active| (active.scope.clone(), active.cancellation.clone()))
    };
    if let Some((_, cancellation)) = &scope
        && let Err(error) = session.conversation.cancel(cancellation)
    {
        errors.push(error);
    }
    if scope.is_none()
        && !session.cleanup.is_collected()
        && session.state.binding_failure().await.is_none()
    {
        // Configuration can hold the operation lock until transport cleanup unblocks its RPC.
        // Bound the entire graceful attempt so this close always reaches that cleanup.
        let graceful = tokio::time::timeout(NATIVE_CLOSE_GRACE, async {
            let _operation = session.operation.lock().await;
            if session.state.binding_failure().await.is_none() && !session.cleanup.is_collected() {
                session.conversation.close_native().await?;
            }
            Ok(())
        });
        match graceful.await {
            Ok(Ok(())) => {}
            Ok(Err(error)) => errors.push(error),
            Err(_) => errors.push(failure(ProviderOperation::ThreadClose, ProviderFailureKind::Timeout)),
        }
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
    let mut settled = true;
    if let Some((scope, _)) = scope {
        match session.settlement_record(&scope) {
            Ok(record) => {
                // The retained outcome still reports delivery uncertainty to invocation callers.
                // Force collection can cause that outcome while proving the writer was reaped.
                let _ = record.wait().await;
            }
            Err(error) => {
                settled = false;
                errors.push(error.for_operation(ProviderOperation::ThreadClose));
            }
        }
    }
    let _operation = session.operation.lock().await;
    // A pending configuration may have registered more resources while native interruption ran.
    if collected && let Err(error) = session.cleanup.shutdown().await {
        errors.push(with_operation(ProviderOperation::ThreadClose, error));
    }
    if settled && session.cleanup.is_collected() {
        // Collection proves the previous writer stopped even when native close reported a failure.
        session.state.mark_collected();
        session.history_released.store(true, Ordering::Release);
        if errors.is_empty() {
            return Ok(());
        }
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
            _ = cancellation.token.cancelled() => Err(ProviderError::Cancelled),
            result = request.turn_control.before_send(&request.scope) => result,
        };
        if let Err(error) = preparation {
            session.rejected_inputs.lock().await.push((request.scope.clone(), request.submission_id.clone(), request.attempt_id.clone()));
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        if !session.state.is_admitting() || cancellation.token.is_cancelled() {
            session.rejected_inputs.lock().await.push((request.scope.clone(), request.submission_id.clone(), request.attempt_id.clone()));
            return Err(reject_before_dispatch(&request.receipt, ProviderError::Cancelled).await);
        }
        let _operation = session.operation.lock().await;
        session.sink.check()?;
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
            let error = if cancellation.token.is_cancelled() { ProviderError::Cancelled } else { error };
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
        let prelude = session.conversation.prompt_instructions();
        let prompt = match prompt_content(&request, session.conversation.supports_images(), prelude) {
            Ok(prompt) => prompt,
            Err(error) => return Err(reject_before_dispatch(&request.receipt, error).await),
        };
        if let Err(error) = configure_model(
            &session,
            &request.configuration.model,
            &current_model,
            request.execution,
            Some(&cancellation),
        )
        .await
        {
            let error = with_operation(ProviderOperation::TurnStart, error);
            if error.is_clean_cancellation() {
                session.rejected_inputs.lock().await.push((request.scope.clone(), request.submission_id.clone(), request.attempt_id.clone()));
            }
            if matches!(&error, ProviderError::Failure(failure) if failure.disposition() == forge_provider_api::FailureDisposition::Invalidated) { session.invalidate(error.clone()).await; }
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        *session.native_model.lock().await = request.configuration.model.clone();
        let transcript = session.conversation.transcript_cursor().await;
        let cancellation = session.active.lock().unwrap_or_else(std::sync::PoisonError::into_inner).as_ref().filter(|active| active.scope == request.scope).map(|active| active.cancellation.clone()).ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::WrongInvocation))?;
        if cancellation.token.is_cancelled() || !session.state.is_admitting() {
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
                Some(&request.dispatch),
                Some(&cancellation),
            )
            .await;
        let _response = match response {
            Ok(response) => response,
            Err(error) if error.is_clean_cancellation()
                || matches!(&error, ProviderError::PreparationCleanup { cause, .. } if cause.is_clean_cancellation()) =>
            {
                session.rejected_inputs.lock().await.push((request.scope.clone(), request.submission_id.clone(), request.attempt_id.clone()));
                return Err(error);
            }
            Err(error) => {
                cancellation.seal();
                cancellation.wait_for_replies().await;
                let _steering = session.steering.write().await;
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
        if let Some(sender) = accepted.take() {
            let _ = sender.send(Ok(InputAcceptance {
                submission_id: request.submission_id.clone(),
            }));
        }
        cancellation.wait_for_replies().await;
        let _steering_boundary = session.steering.write().await;
        if let Some(error) = session.state.binding_failure().await {
            session.invalidate(error.clone()).await;
            return Err(error);
        }
        let steering_end = session.active.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
            .as_ref().filter(|active| active.scope == request.scope)
            .map(|active| active.steering_end)
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::WrongInvocation))?;
        let native_end = steering_end.map_or(native_end, |end| native_end.latest(end));
        let completed_at_ms = native_end.completed_at_ms();
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
        let transcript = transcript.filter(|_| steering_end.is_some());
        let mut terminal_errors = Vec::new();
        if let Err(error) = session.state.finish_with_transcript(outcome, transcript).await {
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
            if session.state.has_active_work().await {
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
    initial: &forge_provider_api::ModelSelection,
    requested: &forge_provider_api::ModelSelection,
    retained: &forge_provider_api::ModelSelection,
    models: &[forge_provider_api::ProviderModel],
) -> Result<forge_provider_api::ModelSelection, ProviderError> {
    use forge_provider_api::capability::{ProviderFeature, UnsupportedCapability};
    use forge_provider_api::generation::ReasoningSelection;
    let mut resolved = requested.clone();
    let resetting_effort = requested.reasoning == ReasoningSelection::Default
        && retained.reasoning != ReasoningSelection::Default;
    if requested.model.is_none() && (retained.model.is_some() || resetting_effort) {
        let feature = if retained.model.is_some() {
            ProviderFeature::ModelDefaultReset
        } else {
            ProviderFeature::ReasoningDefaultReset
        };
        resolved.model = Some(initial.model.clone().ok_or(UnsupportedCapability(feature))?);
    }
    if resetting_effort
        && let Some(model) = models
            .iter()
            .find(|model| resolved.model.as_deref() == Some(model.id.as_str()))
        && model.default_reasoning.is_none()
        && !model.reasoning.is_empty()
    {
        // For older peers with a separate effort control, the opening effort belongs to
        // the opening model. Concrete variants already restore their own effort on selection.
        if initial.model.as_deref() != Some(model.id.as_str())
            || !matches!(&initial.reasoning, ReasoningSelection::Level(level) if model.reasoning.contains(level))
        {
            return Err(UnsupportedCapability(ProviderFeature::ReasoningDefaultReset).into());
        }
        resolved.reasoning = initial.reasoning.clone();
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
    cancellation: Option<&super::connection::PromptCancellation>,
) -> Result<(), ProviderError> {
    use forge_provider_api::generation::ReasoningSelection;

    validate_model_selection(requested)?;
    let models = session.conversation.models().await.map_err(|_| {
        failure(
            ProviderOperation::ThreadConfigure,
            ProviderFailureKind::Protocol,
        )
    })?;
    let resolved = resolve_native_model(
        session.conversation.initial_model(),
        requested,
        retained,
        &models,
    )?;
    let selected_model = match resolved.model.as_deref() {
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
    if let ReasoningSelection::Level(level) = &resolved.reasoning
        && !selected_model
            .and_then(|id| models.iter().find(|model| model.id == id))
            .is_some_and(|model| model.reasoning.contains(level))
    {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::EffortSelection,
        )
        .into());
    }
    let effort = match &resolved.reasoning {
        ReasoningSelection::Default => None,
        ReasoningSelection::Level(level) => Some(level.as_str()),
        ReasoningSelection::Lowest => {
            let reasoning = selected_model
                .and_then(|id| models.iter().find(|model| model.id == id))
                .and_then(|model| model.reasoning.first())
                .ok_or({
                    forge_provider_api::capability::UnsupportedCapability(
                        forge_provider_api::capability::ProviderFeature::EffortSelection,
                    )
                })?;
            Some(reasoning.as_str())
        }
    };
    let outcome = session
        .conversation
        .configure(resolved.model.as_deref(), effort, mode, cancellation)
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
        })?;
    match outcome {
        super::conversation::ConfigurationOutcome::Configured => Ok(()),
        super::conversation::ConfigurationOutcome::Cancelled => Err(ProviderError::Cancelled),
        super::conversation::ConfigurationOutcome::ModelOnly => {
            // Record the confirmed model so a successor can restore its own defaults.
            session.native_model.lock().await.model = requested.model.clone();
            Err(ProviderError::Cancelled)
        }
    }
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

/// Explain how to resolve an unavailable runtime without exposing launch arguments.
fn runtime_unavailable(operation: ProviderOperation, error: anyhow::Error) -> ProviderError {
    let cause = failure(operation, ProviderFailureKind::Runtime);
    match forge_provider_api::diagnostic::SanitizedMessage::new(error.to_string()) {
        Ok(explanation) => ProviderError::Explained { cause: Box::new(cause), explanation },
        Err(_) => cause,
    }
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
    }
}

/// Render the user's own words of one turn input, or nothing for a Harness-started turn.
fn user_text(input: &forge_provider_api::forge::TurnInput) -> String {
    input.user_input().map(prompt_input).unwrap_or_default()
}

/// Encode current and recovered typed inputs without dropping their elements or files.
fn prompt_content(
    request: &PromptRequest,
    supports_images: bool,
    base_instructions: Option<&str>,
) -> Result<Vec<serde_json::Value>, ProviderError> {
    let compact = request.recovered_inputs.is_empty()
        && matches!(
            request.input.kind(),
            forge_provider_api::forge::TurnKind::UserMessage {
                input: forge_provider_api::PromptInput::Text(text)
            } if text == "/compact"
        );
    if compact {
        return Ok(vec![serde_json::json!({"type":"text","text":"/compact"})]);
    }

    let mut content = Vec::new();
    let push_text = |content: &mut Vec<serde_json::Value>, text: String| {
        if !text.is_empty() {
            content.push(serde_json::json!({"type":"text","text":text}));
        }
    };
    if let Some(instructions) = base_instructions {
        push_text(
            &mut content,
            forge_provider_api::forge::system_instructions_block(instructions),
        );
    }
    // ACP has no mid-session developer channel, so the turn's standing sections and elements
    // travel as one `<forge>` block that the open-time instructions explain.
    push_text(&mut content, request.user_message_block());
    for (index, recovered) in request.recovered_inputs.iter().enumerate() {
        push_text(&mut content, recovered.block(index + 1));
        let mut attachments =
            super::attachments::input(String::new(), &recovered.attachments, supports_images)?;
        attachments.pop();
        content.extend(attachments);
        push_text(&mut content, user_text(&recovered.input));
    }
    let mut attachments =
        super::attachments::input(String::new(), &request.attachments, supports_images)?;
    attachments.pop();
    content.extend(attachments);
    push_text(&mut content, user_text(&request.input));
    Ok(content)
}

/// Describe the resources sent on open without implying that the native agent connected.
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
            status: forge_provider_api::catalog::McpStatus::Unavailable,
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

    /// Describe submitted MCP launches without reporting a readiness observation.
    #[test]
    fn acp_mcp_launch_catalog_has_no_readiness_observation() {
        use forge_provider_api::{
            catalog::{McpStatus, McpTransportKind},
            session::{McpLaunch, McpTransport, ResourceSelection},
        };

        let selection = ResourceSelection {
            skills: Vec::new(),
            mcp: vec![McpLaunch {
                name: "selected".into(),
                transport: McpTransport::Http {
                    url: "http://localhost/mcp".into(),
                    headers: Default::default(),
                },
                timeout: None,
                working_directory: None,
                tools: None,
            }],
        };
        let rows = mcp_definitions(&selection);
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].name, "selected");
        assert_eq!(rows[0].transport, McpTransportKind::Http);
        assert!(rows[0].enabled);
        assert_eq!(rows[0].status, McpStatus::Unavailable);
        assert!(rows[0].tools.is_empty());
    }

    /// Restore the native default without transferring effort from another model.
    #[test]
    fn bundled_effort_default_uses_the_selected_variant() {
        use forge_provider_api::{ModelSelection, generation::ReasoningSelection};
        use serde_json::json;

        let initial = ModelSelection {
            model: Some("neon-xhigh".to_owned()),
            reasoning: ReasoningSelection::Level("xhigh".to_owned()),
            ..Default::default()
        };
        let models = super::super::conversation::decode_model_catalog(
            &json!({
                "models": {"availableModels": [
                    {"modelId": "neon-xhigh"}, {"modelId": "flash-low"},
                    {"modelId": "flash-high"}, {"modelId": "orchestrator"}
                ]},
                "configOptions": [
                    {"id": "model", "currentValue": "neon", "options": [
                        {"value": "neon"}, {"value": "flash"}, {"value": "orchestrator"}
                    ]},
                    {"id": "thought_level", "currentValue": "xhigh", "options": [
                        {"value": "xhigh"}
                    ]}
                ]
            }),
            false,
        );
        for model in ["flash-low", "orchestrator"] {
            let requested = ModelSelection {
                model: Some(model.to_owned()),
                ..Default::default()
            };
            assert_eq!(
                resolve_native_model(&initial, &requested, &initial, &models).unwrap(),
                requested
            );
        }
        let reset = resolve_native_model(&initial, &ModelSelection::default(), &initial, &models)
            .unwrap();
        assert_eq!(reset.model, initial.model);
        assert_eq!(reset.reasoning, ReasoningSelection::Default);
    }

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
            forge_provider_api::ProviderId::new("antigravity").unwrap(),
            vec!["must-not-launch".into()],
            PathBuf::from("unused-acp-fixture"),
        )
        .unwrap()
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

    /// Fence a published invocation's next send before close waits for queued preparation.
    #[tokio::test]
    async fn close_stops_active_prompt_publication_before_waiting_for_queued_preparation() {
        let (_fixture, provider, opened) =
            super::super::wire_fixture_test::provider_fixture("ok", "").await;
        let binding_id = &opened.binding.id;
        let binding = provider
            .binding(binding_id, ProviderOperation::ThreadClose)
            .unwrap();
        let scope = forge_provider_api::InvocationScope::new(binding.owner.clone(), "active");
        let cancellation = Arc::new(super::super::connection::PromptCancellation::default());
        let settlement = Arc::new(OperationRecord::new());
        binding
            .settlements
            .lock()
            .unwrap()
            .insert(scope.clone(), settlement.clone());
        *binding.active.lock().unwrap() = Some(ActiveExecution {
            scope: scope.clone(),
            steering_end: None,
            cancellation: cancellation.clone(),
            task: None,
        });
        let queued_scope = forge_provider_api::InvocationScope::new(binding.owner.clone(), "queued");
        let pending = provider.pending_inputs.register(
            binding_id,
            &queued_scope,
            operation_context().id(),
        );

        let mut close = Box::pin(provider.close_session(binding_id));
        assert!(futures_util::poll!(&mut close).is_pending());
        assert!(pending.is_cancelled());
        assert!(cancellation.token.is_cancelled());
        assert!(!binding.state.is_admitting());

        // A force-collected transport can still leave the active prompt's delivery uncertain.
        settlement.complete(Err(ProviderError::DeliveryUncertain)).await;
        drop(pending);
        close.await.unwrap();
        assert!(binding.history_released.load(Ordering::Acquire));
        assert!(matches!(
            binding.settlement(&scope).await,
            Err(ProviderError::DeliveryUncertain)
        ));
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
}
