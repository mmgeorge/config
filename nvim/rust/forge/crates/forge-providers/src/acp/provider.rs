use super::{
    AcpKind,
    conversation::{AcpConversation, ConversationOptions},
    runtime::AcpRuntime,
    session::AcpSessionState,
};
use crate::pending_input::{PendingInput, PendingInputs};
use async_trait::async_trait;
use forge_provider_api::events::{self, ProviderEventSink};
use forge_provider_api::retained_operation::{IdentityRegistry, Interruption, Store};
use forge_provider_api::session::{
    BindingId, InputAcceptance, PromptRequest, SessionBinding, SessionRequest, SteeringRequest,
};
use forge_provider_api::{ExecutionOwner, ProviderEvent};
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
    owner: ExecutionOwner,
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
    covered_submissions: std::sync::Mutex<
        HashMap<forge_provider_api::InvocationScope, Vec<forge_provider_api::SubmissionId>>,
    >,
    active: std::sync::Mutex<Option<ActiveExecution>>,
    terminal_results:
        Mutex<HashMap<forge_provider_api::InvocationScope, forge_provider_api::ExecutionResult>>,
    settlements: std::sync::Mutex<
        HashMap<
            forge_provider_api::InvocationScope,
            Arc<OperationRecord<forge_provider_api::InvocationSettlement>>,
        >,
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
        manifest.task_steering = if descriptor.capability.native_steer {
            vec![
                forge_provider_api::TaskKind::Plan,
                forge_provider_api::TaskKind::Execute,
            ]
        } else {
            Vec::new()
        };
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
    ) -> Result<forge_provider_api::InvocationSettlement, ProviderError> {
        let record = self
            .settlements
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(scope)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::Cancel,
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
        forge_provider_api::InvocationSettlement,
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
    terminals: Store<BindingId, forge_provider_api::terminal::TerminalSnapshot>,
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
    owner: ExecutionOwner,
    cleanup: super::connection::AcpCleanup,
}

struct ActiveExecution {
    scope: forge_provider_api::InvocationScope,
    cancelled: bool,
    cancellation: tokio_util::sync::CancellationToken,
    task: Option<Arc<crate::retained_task::RetainedTask>>,
}

/// Own ACP processes, retained conversations, native event decoding, and completion.
#[derive(Clone)]
pub struct AcpProvider {
    kind: AcpKind,
    runtime: Arc<AcpRuntime>,
    cleanup: super::connection::AcpCleanup,
    runtime_root: PathBuf,
    bindings: Arc<std::sync::Mutex<HashMap<BindingId, Arc<AcpBinding>>>>,
    owners: Arc<Mutex<HashMap<ExecutionOwner, BindingId>>>,
    preparation: Arc<Mutex<HashMap<ExecutionOwner, Arc<Mutex<()>>>>>,
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
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::Rejected,
                ));
            }
        };
        if command.is_empty() {
            return Err(failure(
                ProviderOperation::SessionOpen,
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
            ProviderOperation::SessionFork,
            ProviderPhase::Observation,
            disposition,
        );
        let cause = observation.clone();
        if let Some(binding) = &binding
            && let Ok(session) = self.binding(binding, ProviderOperation::SessionFork)
        {
            session.state.close_admission();
        }
        Interruption::new(observation, async move {
            if let Some(binding) = binding {
                let session = provider.binding(&binding, ProviderOperation::SessionFork)?;
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
            ProviderOperation::Cancel,
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
        context.check_admission(ProviderOperation::InputSubmit)?;
        let session = self.binding(binding, ProviderOperation::InputSubmit)?;
        session
            .state
            .ensure_admission(ProviderOperation::InputSubmit)?;
        pending.check_admission(ProviderOperation::InputSubmit)?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let admission = tokio::select! {
            biased;
            _ = pending.cancelled() => return Err(pending.check_admission(ProviderOperation::InputSubmit).expect_err("sticky pending prevention rejects admission")),
            admission = session.operation.lock() => admission,
        };
        pending.check_admission(ProviderOperation::InputSubmit)?;
        context.check_admission(ProviderOperation::InputSubmit)?;
        session
            .state
            .ensure_admission(ProviderOperation::InputSubmit)?;
        if request.scope.owner != session.owner
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
                ProviderOperation::InputSubmit,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        validate_configuration(&request.configuration.model, &request.configuration.access)?;
        let (acceptance, scope, task, started) = {
            let mut active = session
                .active
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            pending.check_admission(ProviderOperation::InputSubmit)?;
            context.check_admission(ProviderOperation::InputSubmit)?;
            if active.is_some() {
                return Err(failure(
                    ProviderOperation::InputSubmit,
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
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::WrongInvocation,
                    ));
                }
                settlements.insert(request.scope.clone(), Arc::new(OperationRecord::new()));
            }
            session
                .covered_submissions
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .insert(request.scope.clone(), vec![request.submission_id.clone()]);
            *active = Some(ActiveExecution {
                scope: request.scope.clone(),
                cancelled: false,
                cancellation: tokio_util::sync::CancellationToken::new(),
                task: None,
            });
            let (started, start) = tokio::sync::oneshot::channel();
            let (accepted, acceptance) = tokio::sync::oneshot::channel();
            let scope = request.scope.clone();
            let session_for_task = session.clone();
            let task = tokio::spawn(async move {
                let _ = start.await;
                run_prompt(session_for_task.clone(), request, accepted).await
            });
            let task = Arc::new(crate::retained_task::RetainedTask::new(
                ProviderOperation::InputSubmit,
                task,
            ));
            active.as_mut().expect("active execution inserted").task = Some(task.clone());
            (acceptance, scope, task, started)
        };
        let collected_session = session.clone();
        tokio::spawn(async move {
            collect_invocation(collected_session, scope, task).await;
        });
        pending.registered();
        let _ = started.send(());
        drop(admission);
        acceptance.await.map_err(|_| {
            failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Transport,
            )
        })?
    }

    /// Open native history under retained admission and publish one owned reader.
    async fn open_session(
        &self,
        context: OperationContext,
        mut request: SessionRequest,
        cleanup: super::connection::AcpCleanup,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        context.check_admission(ProviderOperation::SessionOpen)?;
        if !self.admission.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionOpen,
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
            ProviderOperation::SessionOpen,
            &request.configuration.resources.mcp,
        )?;
        let runtime = self
            .runtime
            .resolve()
            .map_err(|_| failure(ProviderOperation::SessionOpen, ProviderFailureKind::Runtime))?;
        if self.kind == AcpKind::Claude {
            runtime.validate_native_claude(&context).await?;
        }
        context.check_admission(ProviderOperation::SessionOpen)?;
        let _history_admission = self.history_admission.lock().await;
        context.check_admission(ProviderOperation::SessionOpen)?;
        if !self.admission.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionOpen,
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
                reservation.owner != request.owner && !reservation.cleanup.is_collected()
            }) {
                return Err(failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::HistoryConflict,
                ));
            }
            let bindings = self
                .bindings
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if bindings.values().any(|binding| {
                binding.owner != request.owner
                    && binding.binding.provider_session_id.as_ref() == Some(resume)
                    && !binding.history_released.load(Ordering::Acquire)
            }) {
                return Err(failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::HistoryConflict,
                ));
            }
        }
        let preparation = self
            .preparation
            .lock()
            .await
            .entry(request.owner.clone())
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone();
        let _preparation = preparation.lock().await;
        let existing = self.owners.lock().await.get(&request.owner).cloned();
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
                    owner: request.owner.clone(),
                    cleanup: cleanup.clone(),
                });
        }
        context.check_admission(ProviderOperation::SessionOpen)?;
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string())
            .map_err(|_| failure(ProviderOperation::SessionOpen, ProviderFailureKind::Unknown))?;
        let scope = forge_provider_api::EventScope::Binding {
            owner: request.owner.clone(),
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
                failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let conflict = {
            let mut reservations = self.history_reservations.lock().await;
            let entries = reservations.entry(session_id.clone()).or_default();
            let conflict = entries.iter().any(|reservation| {
                reservation.operation_id != *context.id()
                    && reservation.owner != request.owner
                    && !reservation.cleanup.is_collected()
            });
            if !entries
                .iter()
                .any(|reservation| reservation.operation_id == *context.id())
            {
                entries.push(HistoryReservation {
                    operation_id: context.id().clone(),
                    owner: request.owner.clone(),
                    cleanup: cleanup.clone(),
                });
            }
            conflict
        };
        if conflict {
            let cause = failure(
                ProviderOperation::SessionOpen,
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
            id: binding_id.clone(),
            provider_session_id: Some(session_id),
        };
        let initial_model = conversation.initial_model().await;
        let requested_model = request.configuration.model.clone();
        let retained = Arc::new(AcpBinding {
            binding: binding.clone(),
            owner: request.owner.clone(),
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
            covered_submissions: std::sync::Mutex::new(HashMap::new()),
            active: std::sync::Mutex::new(None),
            terminal_results: Mutex::new(HashMap::new()),
            settlements: std::sync::Mutex::new(HashMap::new()),
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
            .insert(request.owner.clone(), binding_id.clone());
        let configured = match context.check_admission(ProviderOperation::SessionOpen) {
            Ok(()) => {
                configure_model(
                    &retained,
                    &requested_model,
                    &forge_provider_api::ModelSelection::default(),
                    forge_provider_api::PromptMode::Chat,
                )
                .await
            }
            Err(error) => Err(error),
        };
        if let Err(error) = configured {
            let error = with_operation(ProviderOperation::SessionOpen, error);
            retained.state.retain_failure(error.clone()).await;
            let cleanup = self.close_session(&binding_id).await;
            return Err(match cleanup {
                Ok(()) => error,
                Err(cleanup) => with_cleanup(error, vec![cleanup]),
            });
        }
        if let Err(error) = context.check_admission(ProviderOperation::SessionOpen) {
            let error = operation_failure(
                ProviderOperation::SessionOpen,
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
        if let Err(cause) = retained.ensure_current_result(&context, ProviderOperation::SessionOpen)
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
        context.check_admission(ProviderOperation::SessionConfigure)?;
        let session = self.binding(binding, ProviderOperation::SessionConfigure)?;
        session
            .state
            .ensure_admission(ProviderOperation::SessionConfigure)?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let _operation = session.operation.lock().await;
        context.check_admission(ProviderOperation::SessionConfigure)?;
        session
            .state
            .ensure_admission(ProviderOperation::SessionConfigure)?;
        if session.has_active_prompt() || session.state.has_active_work().await {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        validate_configuration(&configuration.model, &configuration.access)?;
        let servers = super::resources::mcp_launches(
            ProviderOperation::SessionConfigure,
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
            forge_provider_api::PromptMode::Chat,
        )
        .await
        {
            if matches!(&error, ProviderError::Unsupported(_))
                || matches!(&error, ProviderError::Failure(failure) if failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
            {
                return Err(error);
            }
            let error = operation_failure(
                ProviderOperation::SessionConfigure,
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
                    ProviderOperation::SessionConfigure,
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
                    && let Err(error) = previous.close(ProviderOperation::SessionConfigure).await
                {
                    let cause = operation_failure(
                        ProviderOperation::SessionConfigure,
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
            session.ensure_current_result(&context, ProviderOperation::SessionConfigure)
        {
            session.invalidate(cause.clone()).await;
            return Err(cause);
        }
        let capabilities = session.capabilities().await;
        if let Err(cause) =
            session.ensure_current_result(&context, ProviderOperation::SessionConfigure)
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
        context.check_admission(ProviderOperation::SessionFork)?;
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
        let session = self.binding(&binding, ProviderOperation::SessionFork)?;
        session
            .state
            .ensure_admission(ProviderOperation::SessionFork)?;
        let source_owner = &session.owner;
        if &request.target.owner == source_owner {
            return Err(failure(
                ProviderOperation::SessionFork,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        validate_configuration(
            &request.target.configuration.model,
            &request.target.configuration.access,
        )?;
        let mcp = super::resources::mcp_launches(
            ProviderOperation::SessionFork,
            &request.target.configuration.resources.mcp,
        )?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::SessionFork,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let _operation = session.operation.lock().await;
        context.check_admission(ProviderOperation::SessionFork)?;
        session
            .state
            .ensure_admission(ProviderOperation::SessionFork)?;
        if session.has_active_prompt() || session.state.has_active_work().await {
            return Err(failure(
                ProviderOperation::SessionFork,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let source_model = session.native_model.lock().await.clone();
        let target_model =
            resolve_native_model(&session, &request.target.configuration.model, &source_model)?;
        let models = session.conversation.models().await.map_err(|_| {
            failure(
                ProviderOperation::SessionFork,
                ProviderFailureKind::Protocol,
            )
        })?;
        if let Some(model) = target_model.model.as_deref()
            && !models.iter().any(|candidate| candidate.id == model)
        {
            return Err(failure(
                ProviderOperation::SessionFork,
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
                    ProviderOperation::SessionFork,
                    &error,
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Uncertain,
                )
            })?;
        let id = id.try_into().map_err(|_| {
            failure(
                ProviderOperation::SessionFork,
                ProviderFailureKind::Protocol,
            )
        })?;
        Ok(forge_provider_api::ProviderForkResult::unprofiled(id))
    }

    /// Require positive native compaction completion within the requested operation.
    async fn compact_session(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        context.check_admission(ProviderOperation::SessionCompact)?;
        request.validate_context(&context)?;
        if self.kind != AcpKind::Claude {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::Rejected,
            ));
        }
        let session = self.binding(binding, ProviderOperation::SessionCompact)?;
        session
            .state
            .ensure_admission(ProviderOperation::SessionCompact)?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let _operation = session.operation.lock().await;
        context.check_admission(ProviderOperation::SessionCompact)?;
        session
            .state
            .ensure_admission(ProviderOperation::SessionCompact)?;
        if session.has_active_prompt() {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        if request.scope.owner() != &session.owner {
            return Err(failure(
                ProviderOperation::SessionCompact,
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
                ProviderOperation::SessionCompact,
                ProviderFailureKind::Protocol,
            )
        })?;
        if response["stopReason"] == "cancelled" {
            return Err(ProviderError::Cancelled);
        }
        let compaction = match session.state.latest_compaction().await {
            Some(compaction)
                if compaction.state == forge_provider_api::CompactionState::Completed =>
            {
                compaction
            }
            _ => {
                let cause = ProviderError::Failure(
                    ProviderFailure::new(
                        ProviderOperation::SessionCompact,
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
        context.check_admission(ProviderOperation::InputSteer)?;
        let session = self.binding(binding, ProviderOperation::InputSteer)?;
        let _steering = session.steering.read().await;
        session
            .state
            .ensure_admission(ProviderOperation::InputSteer)?;
        if request.scope.owner != session.owner {
            return Err(failure(
                ProviderOperation::InputSteer,
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
                ProviderOperation::InputSteer,
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
        if !capabilities.capability.iter().any(|capability| {
            capability.id == forge_provider_api::capability::CapabilityId::NativeSteer
                && capability.support == forge_provider_api::capability::Support::Supported
        }) || request
            .task
            .as_ref()
            .is_some_and(|task| !capabilities.task_steering.contains(&task.kind))
        {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::NativeSteer,
            )
            .into());
        }
        let input = prompt_input(&request.input);
        let text = request.task.as_ref().map_or(input.clone(), |task| {
            format!("{}\n\n{input}", task.heading())
        });
        let prompt = super::attachments::input(
            text,
            &request.attachments,
            session.conversation.supports_images(),
        )?;
        let receipt = request.delivery.receipt().cloned();
        let submission_id = request.delivery.submission_id().cloned();
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
        if let Some(submission_id) = &submission_id
            && let Some(covered) = session
                .covered_submissions
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .get_mut(&request.scope)
            && !covered.contains(submission_id)
        {
            covered.push(submission_id.clone());
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
        let session = self.binding(binding, ProviderOperation::SessionClose)?;
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
                    ProviderOperation::SessionClose,
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
    ) -> Result<forge_provider_api::InvocationSettlement, ProviderError> {
        self.pending_inputs.cancel(binding, &request.scope).await;
        let session = self.binding(binding, ProviderOperation::Cancel)?;
        let record = session
            .settlements
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(&request.scope)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::Cancel,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        if let Some(result) = record.result.lock().await.clone() {
            return result;
        }
        {
            let mut active = session
                .active
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if let Some(active) = active
                .as_mut()
                .filter(|active| active.scope == request.scope)
            {
                active.cancelled = true;
                active.cancellation.cancel();
            }
        }
        session
            .state
            .revoke_invocation_callbacks(&request.scope)
            .await;
        session.settlement(&request.scope).await
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
        if request.scope.owner != session.owner {
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
        if !matches!(
            settlement,
            Some(Ok(forge_provider_api::InvocationSettlement {
                result: forge_provider_api::ExecutionResult::Cancelled { .. },
                ..
            }))
        ) {
            return Ok(forge_provider_api::InputRetractOutcome::Unavailable(
                forge_provider_api::InputRetractUnavailable::NotCancelled,
            ));
        }
        if session
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
                ProviderOperation::SessionOpen,
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
                    failure(
                        ProviderOperation::SessionOpen,
                        ProviderFailureKind::Protocol,
                    )
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
    async fn session_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        let provider = self.clone();
        let worker_context = context.clone();
        let payload = request.identity();
        let cleanup = self.cleanup.scope();
        let interrupt_cleanup = cleanup.clone();
        self.operations
            .open
            .take(
                context,
                ProviderOperation::SessionOpen,
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
                            ProviderOperation::SessionOpen,
                            ProviderPhase::Preparation,
                            FailureDisposition::Uncertain,
                        ),
                        async move { interrupt_cleanup.shutdown().await },
                    )
                },
            )
            .await
    }

    async fn session_configure(
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
                ProviderOperation::SessionConfigure,
                payload,
                async move {
                    provider
                        .configure_session(worker_context, &binding, configuration)
                        .await
                },
                move |interruption| {
                    cleanup_provider.interrupt_binding(
                        cleanup_binding,
                        ProviderOperation::SessionConfigure,
                        interruption,
                        ProviderPhase::Preparation,
                    )
                },
            )
            .await
    }

    async fn session_fork(
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
                ProviderOperation::SessionFork,
                payload,
                async move { provider.fork_session(worker_context, request).await },
                move |interruption| cleanup_provider.interrupt_fork(source, interruption),
            )
            .await
    }

    async fn session_compact(
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
                ProviderOperation::SessionCompact,
                payload,
                async move {
                    provider
                        .compact_session(worker_context, &binding, request)
                        .await
                },
                move |interruption| {
                    cleanup_provider.interrupt_binding(
                        cleanup_binding,
                        ProviderOperation::SessionCompact,
                        interruption,
                        ProviderPhase::Observation,
                    )
                },
            )
            .await
    }

    async fn session_close(
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
                ProviderOperation::SessionClose,
                binding.clone(),
                async move { provider.close_session(&binding).await },
                move |interruption| {
                    cleanup_provider.interrupt_binding(
                        cleanup_binding,
                        ProviderOperation::SessionClose,
                        interruption,
                        ProviderPhase::Close,
                    )
                },
            )
            .await
    }

    async fn cancel(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CancelRequest,
    ) -> Result<forge_provider_api::InvocationSettlement, ProviderError> {
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
                ProviderOperation::Cancel,
                payload,
                async move { provider.cancel_invocation(&binding, request).await },
                move |interruption| {
                    cleanup_provider.interrupt_cancel(cleanup_binding, scope, interruption)
                },
            )
            .await
    }

    async fn input_submit(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
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
                ProviderOperation::InputSubmit,
                payload,
                async move {
                    provider
                        .submit(worker_context, &binding, request, pending)
                        .await
                },
                move |interruption| {
                    Interruption::new(
                        interruption.failure(
                            ProviderOperation::InputSubmit,
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

    async fn input_steer(
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
                ProviderOperation::InputSteer,
                payload,
                async move {
                    provider
                        .steer_session(worker_context, &binding, request)
                        .await
                },
                move |interruption| {
                    cleanup_provider.interrupt_binding(
                        cleanup_binding,
                        ProviderOperation::InputSteer,
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
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
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
        "clientCapabilities":{"auth":{"terminal":true},"session":{"compaction":{},"configOptions":{"boolean":{}}},"subagents":{}}
    })
}

/// Get all journaled input identities accepted under the exact retained invocation.
fn covered_submissions(
    session: &AcpBinding,
    scope: &forge_provider_api::InvocationScope,
) -> Vec<forge_provider_api::SubmissionId> {
    session
        .covered_submissions
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(scope)
        .cloned()
        .unwrap_or_default()
}

/// Join the native worker before publishing one retained foreground settlement.
async fn collect_invocation(
    session: Arc<AcpBinding>,
    scope: forge_provider_api::InvocationScope,
    task: Arc<crate::retained_task::RetainedTask>,
) {
    let joined = task.join().await;
    let _steering_boundary = session.steering.write().await;
    session.state.revoke_callbacks().await;
    let _collection_boundary = session.operation.lock().await;
    {
        let mut active = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if active.as_ref().is_some_and(|active| active.scope == scope) {
            active.take();
        }
    }
    let terminal = session.terminal_results.lock().await.remove(&scope);
    let invalidated = session.state.is_invalidated().await;
    let result = match (joined, terminal) {
        (Ok(()), Some(result)) if !invalidated => {
            let settlement = forge_provider_api::InvocationSettlement {
                binding_id: session.binding.id.clone(),
                scope: scope.clone(),
                result,
                watermark: session.sink.watermark(),
            };
            match session.sink.invocation_sink(&scope) {
                Ok(sink) => match sink.send_settlement(settlement).await {
                    Ok(settlement) => Ok(settlement),
                    Err(error) => Err(error.into()),
                },
                Err(error) => Err(error.into()),
            }
        }
        (Ok(()), Some(_)) => Err(failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::BindingInvalidated,
        )),
        (Err(error), _) => Err(error),
        (Ok(()), None) => Err(failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Unknown,
        )),
    };
    if let Err(error) = &result {
        session.invalidate(error.clone()).await;
        let failure = ProviderFailure::new(
            ProviderOperation::InputSubmit,
            match error {
                ProviderError::Failure(failure) => failure.kind(),
                _ => ProviderFailureKind::Unknown,
            },
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Collection,
            forge_provider_api::FailureDisposition::Invalidated,
        );
        if let Ok(sink) = session.sink.invocation_sink(&scope) {
            let _ = sink
                .send_wait(ProviderEvent::SettlementFailed {
                    scope: scope.clone(),
                    failure,
                })
                .await;
        }
    }
    let record = session
        .settlements
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(&scope)
        .cloned()
        .expect("admitted invocation retains its settlement record");
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
            errors.push(with_operation(ProviderOperation::SessionClose, error));
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
        errors.push(with_operation(ProviderOperation::SessionClose, error));
    }
    if errors.is_empty() && session.cleanup.is_collected() {
        session.state.mark_collected();
        session.history_released.store(true, Ordering::Release);
        return Ok(());
    }
    if errors.is_empty() {
        errors.push(failure(
            ProviderOperation::SessionClose,
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
) -> Result<(), ProviderError> {
    let mut accepted = Some(accepted);
    let result = async {
        let cancellation = session.active.lock().unwrap_or_else(std::sync::PoisonError::into_inner).as_ref().filter(|active| active.scope == request.scope).map(|active| active.cancellation.clone()).ok_or_else(|| failure(ProviderOperation::InputSubmit, ProviderFailureKind::WrongInvocation))?;
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
        if let Err(error) = session
            .conversation
            .bind_tool_endpoint(request.tool_endpoint.as_ref())
            .await
        {
            let error = with_operation(ProviderOperation::InputSubmit, error);
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
        let address = session
            .state
            .address(session.conversation.id())
            .await
            .ok_or_else(|| failure(ProviderOperation::InputSubmit, ProviderFailureKind::Protocol))?;
        let current_model = session.native_model.lock().await.clone();
        let prompt = match prompt_content(&request, session.conversation.supports_images(), session.conversation.prompt_instructions()) {
            Ok(prompt) => prompt,
            Err(error) => return Err(reject_before_dispatch(&request.receipt, error).await),
        };
        if let Err(error) = configure_model(
            &session,
            &request.configuration.model,
            &current_model,
            request.mode,
        )
        .await
        {
            let error = with_operation(ProviderOperation::InputSubmit, error);
            if matches!(&error, ProviderError::Failure(failure) if failure.disposition() == forge_provider_api::FailureDisposition::Invalidated) { session.invalidate(error.clone()).await; }
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        *session.native_model.lock().await = request.configuration.model.clone();
        let cancellation = session.active.lock().unwrap_or_else(std::sync::PoisonError::into_inner).as_ref().filter(|active| active.scope == request.scope).map(|active| active.cancellation.clone()).ok_or_else(|| failure(ProviderOperation::InputSubmit, ProviderFailureKind::WrongInvocation))?;
        if cancellation.is_cancelled() || !session.state.is_admitting() {
            session.rejected_inputs.lock().await.push((request.scope.clone(), request.submission_id.clone(), request.attempt_id.clone()));
            return Err(reject_before_dispatch(&request.receipt, ProviderError::Cancelled).await);
        }
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
        let response = match response {
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
                    let native_failure = match &error {
                        ProviderError::DeliveryUncertain => {
                            forge_provider_api::lifecycle::ObservationFailure::DeliveryUncertain
                        }
                        ProviderError::Failure(native)
                            if native.kind() == ProviderFailureKind::Protocol =>
                        {
                            forge_provider_api::lifecycle::ObservationFailure::Protocol
                        }
                        _ => forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                    };
                    if let Err(cleanup_error) = session
                        .state
                        .finish(forge_provider_api::turn::TurnOutcome::Failed)
                        .await
                    {
                        cleanup.push(cleanup_error);
                    }
                    let event = execution_observation(
                        &request.scope,
                        forge_provider_api::lifecycle::ExecutionUpdate::ObservationLost {
                            failure: native_failure,
                            completion: if started {
                                Some(session.state.take_completion().await.into())
                            } else {
                                None
                            },
                        },
                    );
                    match event {
                        Ok(event) => {
                            if let Err(cleanup_error) = event_sink
                                .send_wait(ProviderEvent::Execution { observation: event })
                                .await
                            {
                                cleanup.push(cleanup_error.into());
                            }
                        }
                        Err(cleanup_error) => cleanup.push(cleanup_error),
                    }
                } else {
                    session.state.revoke_callbacks().await;
                    if let Err(cleanup_error) = session
                        .state
                        .finish(forge_provider_api::turn::TurnOutcome::Failed)
                        .await
                    {
                        cleanup.push(cleanup_error);
                    }
                }
                return Err(with_cleanup(error, cleanup));
            }
        };
        if let Some(sender) = accepted.take() {
            let _ = sender.send(Ok(InputAcceptance {
                submission_id: request.submission_id.clone(),
            }));
        }
        let _steering_boundary = session.steering.write().await;
        let native_outcome = match super::prompt_outcome(&response) {
            Ok(super::PromptOutcome::Completed) => forge_provider_api::turn::TurnOutcome::Completed,
            Ok(super::PromptOutcome::Cancelled) => forge_provider_api::turn::TurnOutcome::Cancelled,
            Err(_) => {
                return Err(ProviderFailure::new(ProviderOperation::InputSubmit, ProviderFailureKind::Protocol)
                    .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain).into());
            }
        };
        let outcome = native_outcome;
        let mut terminal_errors = Vec::new();
        if let Err(error) = session.state.finish(outcome).await {
            terminal_errors.push(error);
        }
        let completion = session.state.take_completion().await.into();
        let result = match outcome {
            forge_provider_api::turn::TurnOutcome::Completed => {
                forge_provider_api::lifecycle::ExecutionResult::Completed { completion }
            }
            forge_provider_api::turn::TurnOutcome::Cancelled => {
                forge_provider_api::lifecycle::ExecutionResult::Cancelled { completion }
            }
            forge_provider_api::turn::TurnOutcome::Failed
            | forge_provider_api::turn::TurnOutcome::Interrupted => {
                forge_provider_api::lifecycle::ExecutionResult::Failed {
                    completion,
                    failure: forge_provider_api::lifecycle::ExecutionFailure::Terminal {
                        kind: ProviderFailureKind::Unknown,
                    },
                }
            }
        };
        session.terminal_results.lock().await.insert(request.scope.clone(), result.clone());
        let update = if session.state.execution_started().await {
            forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                address,
                covered_submissions: covered_submissions(&session, &request.scope),
                completed_at_ms: super::children::timestamp(),
                result,
            }
        } else {
            forge_provider_api::lifecycle::ExecutionUpdate::ResolvedWithoutExecution {
                covered_submissions: covered_submissions(&session, &request.scope),
                completed_at_ms: super::children::timestamp(),
                result,
            }
        };
        match execution_observation(&request.scope, update) {
            Ok(event) => {
                if let Err(error) = event_sink
                    .send_wait(ProviderEvent::Execution { observation: event })
                    .await
                {
                    terminal_errors.push(error.into());
                }
            }
            Err(error) => terminal_errors.push(error),
        }
        if terminal_errors.is_empty() {
            Ok(())
        } else {
            Err(combine_errors(terminal_errors))
        }
    }
    .await;
    match result {
        Ok(()) => Ok(()),
        Err(error) => {
            let mut error = error;
            if accepted.is_some() && session.state.has_active_work().await {
                session.state.revoke_callbacks().await;
                if let Err(cleanup) = session
                    .state
                    .finish(forge_provider_api::turn::TurnOutcome::Failed)
                    .await
                {
                    error = with_cleanup(error, vec![cleanup]);
                }
            }
            if accepted.is_some()
                && !session
                    .terminal_results
                    .lock()
                    .await
                    .contains_key(&request.scope)
            {
                let completion = session.state.take_completion().await.into();
                let result = if error.is_clean_cancellation() {
                    forge_provider_api::ExecutionResult::Cancelled { completion }
                } else {
                    forge_provider_api::ExecutionResult::Failed {
                        completion,
                        failure: forge_provider_api::lifecycle::ExecutionFailure::Terminal {
                            kind: ProviderFailureKind::Rejected,
                        },
                    }
                };
                session
                    .terminal_results
                    .lock()
                    .await
                    .insert(request.scope.clone(), result);
            }
            if let Some(sender) = accepted.take() {
                let unchanged = matches!(&error, ProviderError::PreparationFailed(_))
                    && !session.state.is_invalidated().await;
                let _ = sender.send(Err(error.clone()));
                return if unchanged { Ok(()) } else { Err(error) };
            }
            // Accepted work must never disappear without a terminal observation or explicit loss.
            session
                .sink
                .fail(events::EventDeliveryError::Observation(match &error {
                    ProviderError::Failure(failure)
                        if failure.kind() == ProviderFailureKind::Protocol =>
                    {
                        forge_provider_api::lifecycle::ObservationFailure::Protocol
                    }
                    _ => forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                }));
            Err(error)
        }
    }
}

/// Validate invocation ownership before publishing normalized execution evidence.
fn execution_observation(
    scope: &forge_provider_api::InvocationScope,
    update: forge_provider_api::lifecycle::ExecutionUpdate,
) -> Result<forge_provider_api::lifecycle::ExecutionObservation, ProviderError> {
    forge_provider_api::lifecycle::ExecutionObservation::new(scope.clone(), update)
        .map_err(Into::into)
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
    mode: forge_provider_api::PromptMode,
) -> Result<(), ProviderError> {
    use forge_provider_api::generation::ReasoningSelection;

    validate_model_selection(requested)?;
    let resolved = resolve_native_model(session, requested, retained)?;
    let requested = &resolved;

    let models = session.conversation.models().await.map_err(|_| {
        failure(
            ProviderOperation::SessionConfigure,
            ProviderFailureKind::Protocol,
        )
    })?;
    let selected_model = match requested.model.as_deref() {
        Some(model) => {
            if !models.iter().any(|candidate| candidate.id == model) {
                return Err(failure(
                    ProviderOperation::SessionConfigure,
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
                    ProviderOperation::SessionConfigure,
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
    if let Some(task) = &request.task {
        content.push(serde_json::json!({"type":"text","text":task.heading()}));
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
            owner: ExecutionOwner::root("failed-open"),
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
            .session_open(operation_context(), request)
            .await
            .unwrap();
        let binding = opened.binding.id;
        let scope = forge_provider_api::InvocationScope::new(
            ExecutionOwner::root("failed-open"),
            "queued-input",
        );
        let pending = provider
            .pending_inputs
            .register(&binding, &scope, operation_context().id());
        let successor_scope =
            forge_provider_api::InvocationScope::new(scope.owner.clone(), "successor");
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
                .cancel(
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
            .session_close(operation_context(), &binding)
            .await
            .unwrap();
        provider.shutdown(operation_context()).await.unwrap();
    }

    /// An exact cancel which wins the wrapper race fences that scope before future registration.
    #[tokio::test]
    async fn absent_invocation_cancel_fences_late_input_without_touching_successor() {
        let (_directory, provider, request) = failed_open_fixture(false);
        let opened = provider
            .session_open(operation_context(), request)
            .await
            .unwrap();
        let binding = opened.binding.id;
        let scope = forge_provider_api::InvocationScope::new(
            ExecutionOwner::root("failed-open"),
            "not-yet-polled",
        );
        assert!(
            matches!(provider.cancel(operation_context(), &binding, forge_provider_api::CancelRequest { scope: scope.clone() }).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation)
        );
        let late = provider
            .pending_inputs
            .register(&binding, &scope, operation_context().id());
        assert!(late.is_cancelled());
        let successor_scope = forge_provider_api::InvocationScope::new(scope.owner, "successor");
        let successor =
            provider
                .pending_inputs
                .register(&binding, &successor_scope, operation_context().id());
        assert!(!successor.is_cancelled());
        drop(late);
        drop(successor);
        provider
            .session_close(operation_context(), &binding)
            .await
            .unwrap();
        provider.shutdown(operation_context()).await.unwrap();
    }

    /// A post-write profile failure retains its cleanup phase and invalidates the binding.
    #[tokio::test]
    async fn configuration_profile_cleanup_failure_preserves_postwrite_disposition() {
        let (directory, unused_provider, request) = failed_open_fixture(false);
        let command = unused_provider.runtime.status().command;
        let runtime_root = directory.path().join("claude-runtime");
        let mut runtime = AcpRuntime::new(AcpKind::Claude, command.clone(), &runtime_root);
        runtime.select_native_command(command);
        let provider = AcpProvider::from_runtime(runtime, runtime_root);
        let opened = provider
            .session_open(operation_context(), request.clone())
            .await
            .unwrap();
        let session = provider
            .binding(&opened.binding.id, ProviderOperation::SessionConfigure)
            .unwrap();
        session.profiles.lock().await[0].fail_next_close(1);
        let mut desired = request.configuration;
        desired.resources.mcp.push(forge_provider_api::McpLaunch {
            name: "changed-resource".into(),
            transport: forge_provider_api::McpTransport::Http {
                url: "http://resource.invalid".into(),
                headers: Default::default(),
            },
            timeout: None,
            working_directory: None,
            tools: None,
        });
        let context = operation_context();
        for _ in 0..2 {
            let error = provider
                .session_configure(context.clone(), &opened.binding.id, desired.clone())
                .await
                .unwrap_err();
            assert!(
                matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionConfigure && failure.kind() == ProviderFailureKind::Runtime && failure.phase() == ProviderPhase::Collection && failure.disposition() == FailureDisposition::Invalidated)
            );
        }
        assert!(!session.history_released.load(Ordering::Acquire));
        assert!(
            matches!(session.ensure_current_result(&operation_context(), ProviderOperation::SessionConfigure), Err(ProviderError::Failure(failure)) if failure.phase() == ProviderPhase::Observation && failure.disposition() == FailureDisposition::Invalidated)
        );
        provider
            .session_close(operation_context(), &opened.binding.id)
            .await
            .unwrap();
        assert!(
            matches!(session.ensure_current_result(&operation_context(), ProviderOperation::SessionConfigure), Err(ProviderError::Failure(failure)) if failure.phase() == ProviderPhase::Observation && failure.disposition() == FailureDisposition::Closed)
        );
        provider.shutdown(operation_context()).await.unwrap();
    }

    /// Late context cancellation must prevent a post-write success even before cleanup closes state.
    #[tokio::test]
    async fn postwrite_result_fence_revokes_interrupted_binding_before_success() {
        let (_directory, provider, request) = failed_open_fixture(false);
        let opened = provider
            .session_open(operation_context(), request)
            .await
            .unwrap();
        let session = provider
            .binding(&opened.binding.id, ProviderOperation::SessionConfigure)
            .unwrap();
        let (cancellation, observer) = forge_provider_api::OperationCancellation::channel();
        let context = OperationContext::new(
            OperationId::new("postwrite-cancel").unwrap(),
            std::time::Duration::from_secs(10),
            observer,
        )
        .unwrap();
        cancellation.cancel();
        assert!(
            matches!(session.ensure_current_result(&context, ProviderOperation::SessionConfigure), Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Cancelled && failure.phase() == ProviderPhase::Observation && failure.disposition() == FailureDisposition::Invalidated)
        );
        assert!(!session.state.is_admitting());
        assert!(!session.history_released.load(Ordering::Acquire));
        provider
            .session_close(operation_context(), &opened.binding.id)
            .await
            .unwrap();
        assert!(session.history_released.load(Ordering::Acquire));
        provider.shutdown(operation_context()).await.unwrap();
    }

    /// A failed initial configuration cannot drop its privately registered binding after cleanup failure.
    #[tokio::test]
    async fn failed_initial_configuration_retains_binding_until_shutdown_collects_it() {
        let (_directory, provider, mut request) = failed_open_fixture(false);
        provider.cleanup.fail_next_collection(1);
        request.configuration.model.reasoning =
            forge_provider_api::generation::ReasoningSelection::Level("unsupported-effort".into());
        assert!(
            matches!(provider.session_open(operation_context(), request).await, Err(ProviderError::Combined { primary, .. }) if matches!(*primary, ProviderError::Unsupported(_)))
        );
        let bindings = provider
            .bindings
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .values()
            .cloned()
            .collect::<Vec<_>>();
        assert_eq!(bindings.len(), 1);
        assert!(!bindings[0].history_released.load(Ordering::Acquire));
        assert_eq!(provider.cleanup.pending_count(), 1);
        provider.shutdown(operation_context()).await.unwrap();
        assert!(bindings[0].history_released.load(Ordering::Acquire));
        assert_eq!(provider.cleanup.pending_count(), 0);
    }

    /// A failed native session open retains raw transport cleanup even before a binding exists.
    #[tokio::test]
    async fn failed_native_open_retains_transport_until_shutdown_retries_collection() {
        let (_directory, provider, request) = failed_open_fixture(true);
        provider.cleanup.fail_next_collection(1);
        assert!(
            matches!(provider.session_open(operation_context(), request).await, Err(ProviderError::PreparationCleanup { cause, .. }) if matches!(*cause, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Initialization))
        );
        assert!(
            provider
                .bindings
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .is_empty()
        );
        assert_eq!(provider.cleanup.pending_count(), 1);
        provider.shutdown(operation_context()).await.unwrap();
        assert_eq!(provider.cleanup.pending_count(), 0);
    }

    /// Failed recovery before binding creation still excludes another writable owner of that known history.
    #[tokio::test]
    async fn failed_resume_retains_history_reservation_until_transport_collection() {
        let (_directory, provider, mut request) = failed_open_fixture(true);
        request.history = forge_provider_api::SessionHistory::Durable {
            resume: Some("failed-open-history".to_owned().try_into().unwrap()),
        };
        let mut competing = request.clone();
        competing.owner = ExecutionOwner::root("competing-owner");
        provider.cleanup.fail_next_collection(1);
        assert!(
            provider
                .session_open(operation_context(), request)
                .await
                .is_err()
        );
        assert!(
            provider
                .bindings
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .is_empty()
        );
        assert!(
            matches!(provider.session_open(operation_context(), competing).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::HistoryConflict)
        );
        assert_eq!(provider.cleanup.pending_count(), 1);
        provider.shutdown(operation_context()).await.unwrap();
        assert_eq!(provider.cleanup.pending_count(), 0);
    }

    /// Build a complete retained fork request without launching a native runtime.
    fn fork_request() -> forge_provider_api::SessionForkRequest {
        forge_provider_api::SessionForkRequest {
            source: forge_provider_api::ForkSource::Binding(
                BindingId::new("source-binding").unwrap(),
            ),
            target: forge_provider_api::ForkTarget {
                owner: ExecutionOwner::root("target-owner"),
                workspace: "target-workspace".into(),
                system_instructions: "target instructions".into(),
                configuration: forge_provider_api::SessionConfiguration {
                    model: Default::default(),
                    access: forge_provider_api::access::AccessPolicy {
                        sandbox: false,
                        ..Default::default()
                    },
                    approval: forge_provider_api::approval::ApprovalMode::Ask,
                    resources: forge_provider_api::ResourceSelection {
                        skills: Vec::new(),
                        mcp: vec![forge_provider_api::McpLaunch {
                            name: "resource".into(),
                            transport: forge_provider_api::McpTransport::Http {
                                url: "http://resource.invalid".into(),
                                headers: std::collections::BTreeMap::from([(
                                    "authorization".into(),
                                    "first-secret".into(),
                                )]),
                            },
                            timeout: Some(std::time::Duration::from_secs(1)),
                            working_directory: None,
                            tools: None,
                        }],
                    },
                },
            },
        }
    }

    /// Reusing a fork operation cannot return cached evidence for a different destructive request.
    #[tokio::test]
    async fn fork_retries_validate_source_target_and_complete_configuration() {
        let provider = provider();
        let request = fork_request();
        let context = operation_context();
        assert!(
            matches!(provider.session_fork(context.clone(), request.clone()).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::SessionMissing)
        );
        assert!(
            matches!(provider.session_fork(context.clone(), request.clone()).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::SessionMissing)
        );
        let mut changed = Vec::new();
        let mut next = request.clone();
        next.source =
            forge_provider_api::ForkSource::Binding(BindingId::new("other-source").unwrap());
        changed.push(next);
        let mut next = request.clone();
        next.source = forge_provider_api::ForkSource::Checkpoint {
            provider_session_id: "history".to_owned().try_into().unwrap(),
            checkpoint: forge_provider_api::CheckpointId::new("checkpoint").unwrap(),
        };
        changed.push(next);
        let mut next = request.clone();
        next.target.owner = ExecutionOwner::root("other-target");
        changed.push(next);
        let mut next = request.clone();
        next.target.workspace = "other-workspace".into();
        changed.push(next);
        let mut next = request.clone();
        next.target.system_instructions = "other instructions".into();
        changed.push(next);
        let mut next = request.clone();
        next.target.configuration.model.model = Some("other-model".into());
        changed.push(next);
        let mut next = request.clone();
        next.target.configuration.access.sandbox = !next.target.configuration.access.sandbox;
        changed.push(next);
        let mut next = request.clone();
        next.target.configuration.approval = forge_provider_api::approval::ApprovalMode::Deny;
        changed.push(next);
        let mut next = request.clone();
        next.target.configuration.resources.mcp[0].timeout =
            Some(std::time::Duration::from_secs(2));
        changed.push(next);
        let mut next = request.clone();
        next.target.configuration.resources.mcp[0].transport =
            forge_provider_api::McpTransport::Http {
                url: "http://resource.invalid".into(),
                headers: std::collections::BTreeMap::from([(
                    "authorization".into(),
                    "different-secret".into(),
                )]),
            };
        changed.push(next);
        let mut next = request.clone();
        next.target.configuration.resources.mcp.clear();
        changed.push(next);
        for request in changed {
            assert!(
                matches!(provider.session_fork(context.clone(), request).await, Err(ProviderError::Failure(failure)) if failure.operation() == ProviderOperation::SessionFork && failure.kind() == ProviderFailureKind::OperationConflict)
            );
        }
        provider.shutdown(operation_context()).await.unwrap();
    }

    /// Compaction retries cannot reuse a retained result from another generation or operation scope.
    #[tokio::test]
    async fn compaction_retries_validate_binding_and_operation_scope() {
        let provider = provider();
        let binding = BindingId::new("source-binding").unwrap();
        let context = operation_context();
        let operation_id = context.id().clone();
        let request = forge_provider_api::CompactionRequest {
            scope: forge_provider_api::EventScope::SessionOperation {
                owner: ExecutionOwner::root("source-owner"),
                operation_id: operation_id.clone(),
            },
        };
        assert!(
            matches!(provider.session_compact(context.clone(), &binding, request.clone()).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::SessionMissing)
        );
        assert!(
            matches!(provider.session_compact(context.clone(), &BindingId::new("other-binding").unwrap(), request.clone()).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::OperationConflict)
        );
        let mut changed = request;
        changed.scope = forge_provider_api::EventScope::SessionOperation {
            owner: ExecutionOwner::root("other-owner"),
            operation_id,
        };
        assert!(
            matches!(provider.session_compact(context.clone(), &binding, changed).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::OperationConflict)
        );
        provider.shutdown(operation_context()).await.unwrap();
    }

    /// Workspace-only discovery cannot claim an empty native catalog when no live binding can negotiate it.
    #[tokio::test]
    async fn workspace_catalog_queries_report_unsupported_without_native_effects() {
        use forge_provider_api::capability::{ProviderFeature, UnsupportedCapability};
        use forge_provider_api::catalog::{McpQuery, QueryContext, WorkspaceContext};
        let provider = provider();
        let query = QueryContext::Workspace(WorkspaceContext {
            workspace: "must-not-inspect".into(),
        });
        assert!(matches!(
            provider
                .model_list(operation_context(), query.clone())
                .await,
            Err(ProviderError::Unsupported(UnsupportedCapability(
                ProviderFeature::ModelSelection
            )))
        ));
        assert!(matches!(
            provider
                .skill_list(operation_context(), query.clone())
                .await,
            Err(ProviderError::Unsupported(UnsupportedCapability(
                ProviderFeature::SkillCatalog
            )))
        ));
        for query in [
            McpQuery::Configured(query.clone()),
            McpQuery::Observed(query),
        ] {
            assert!(matches!(
                provider.mcp_list(operation_context(), query).await,
                Err(ProviderError::Unsupported(UnsupportedCapability(
                    ProviderFeature::McpCatalog
                )))
            ));
        }
        assert!(
            provider
                .bindings
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .is_empty()
        );
        assert_eq!(provider.cleanup.pending_count(), 0);
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

    /// Cached native dispositions win expiry, while changed contexts and payloads cannot reuse them.
    #[tokio::test(start_paused = true)]
    async fn retained_fork_context_replays_after_expiry_and_rejects_changed_tuple() {
        let provider = provider();
        let request = fork_request();
        let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
        let context = OperationContext::new(
            OperationId::new("retained-fork").unwrap(),
            std::time::Duration::from_millis(10),
            cancellation,
        )
        .unwrap();
        assert!(
            matches!(provider.session_fork(context.clone(), request.clone()).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::SessionMissing)
        );
        tokio::time::advance(std::time::Duration::from_millis(10)).await;
        assert!(
            context
                .deadline()
                .is_some_and(|deadline| tokio::time::Instant::now() >= deadline)
        );
        assert!(
            matches!(provider.session_fork(context.clone(), request.clone()).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::SessionMissing)
        );
        let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
        let changed_context = OperationContext::new(
            context.id().clone(),
            std::time::Duration::from_secs(10),
            cancellation,
        )
        .unwrap();
        assert!(
            matches!(provider.session_fork(changed_context, request).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::OperationConflict)
        );
        provider.shutdown(operation_context()).await.unwrap();
    }

    struct LeaseProbe(Arc<std::sync::atomic::AtomicUsize>);
    impl forge_provider_api::ResourceLease for LeaseProbe {}
    impl Drop for LeaseProbe {
        fn drop(&mut self) {
            self.0.fetch_add(1, Ordering::AcqRel);
        }
    }

    /// Replay identities cannot retain native callback/profile leases after successful close.
    #[tokio::test]
    async fn retained_open_identity_does_not_keep_resource_lease_alive_after_close() {
        let (_directory, provider, mut request) = failed_open_fixture(false);
        let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let lease = Arc::new(LeaseProbe(drops.clone()));
        let weak = Arc::downgrade(&lease);
        request.resource_lease = Some(lease);
        let opened = provider
            .session_open(operation_context(), request)
            .await
            .unwrap();
        provider
            .session_close(operation_context(), &opened.binding.id)
            .await
            .unwrap();
        assert!(weak.upgrade().is_none());
        assert_eq!(drops.load(Ordering::Acquire), 1);
        provider.shutdown(operation_context()).await.unwrap();
    }

    /// Identical text or history does not let another delivery attempt claim an accepted input.
    #[tokio::test]
    async fn reconciliation_rejects_receipts_from_another_input_or_attempt() {
        let provider = provider();
        for (input, attempt) in [("other-input", "attempt"), ("input", "other-attempt")] {
            let token = checkpoint(true, input, attempt);
            let mut request = reconciliation(token.clone());
            request.accepted_receipts.push(token);
            assert!(
                matches!(provider.input_reconcile(operation_context(), request).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::HistoryConflict)
            );
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
            .session_open(
                operation_context(),
                forge_provider_api::SessionRequest {
                    owner: forge_provider_api::ExecutionOwner::root("isolated"),
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

    /// Persisted exact evidence resolves acceptance after losing the original live binding.
    #[tokio::test]
    async fn reconciliation_recovers_exact_acceptance_without_a_live_binding() {
        let provider = provider();
        let token = checkpoint(true, "input", "attempt");
        let mut request = reconciliation(token.clone());
        request.accepted_receipts.push(token);
        assert!(matches!(
            provider
                .input_reconcile(operation_context(), request)
                .await
                .unwrap(),
            forge_provider_api::SubmissionEvidence::Accepted { .. }
        ));
    }
}
