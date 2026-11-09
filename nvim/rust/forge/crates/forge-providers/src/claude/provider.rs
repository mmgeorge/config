//! The Claude Code `Provider`: retained operations, bindings, and turn execution.

use super::conversation::{ClaudeConversation, ConversationOptions, failure, wait_command};
use super::process::{ClaudeLauncher, ProcessLauncher, SessionStart};
use super::session::{ClaudeSessionState, timestamp};
use super::wire::stdin::UserInput;
use super::wire::stdout::CommandState;
use crate::pending_input::{PendingInput, PendingInputs};
use crate::resource_cleanup::{ResourceCleanup, RetainedProfile};
use async_trait::async_trait;
use forge_provider_api::ThreadId;
use forge_provider_api::events::{self, ProviderEventSink};
use forge_provider_api::retained_operation::{IdentityRegistry, Interruption, Store};
use forge_provider_api::session::{
    BindingId, InputAcceptance, PromptRequest, SessionBinding, SessionRequest, SteeringRequest,
};
use forge_provider_api::{FailureDisposition, OperationContext, OperationId, ProviderPhase};
use forge_provider_api::{ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation};
use serde_json::{Map, Value, json};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tokio::sync::{Mutex, Notify, RwLock};

/// How long an interrupted turn may take to end before its process is treated as wedged.
const INTERRUPT_GRACE: std::time::Duration = std::time::Duration::from_secs(10);

/// One completed root turn in the binding's native history.
#[derive(Clone, Debug)]
struct LedgerTurn {
    /// The client-stamped uuid of the turn's first input.
    prompt: String,
    /// The turn's last transcript chain entry.
    checkpoint: Option<String>,
}

struct ClaudeBinding {
    binding: SessionBinding,
    owner: ThreadId,
    conversation: Arc<ClaudeConversation>,
    state: Arc<ClaudeSessionState>,
    sink: ProviderEventSink,
    cleanup: ResourceCleanup,
    profiles: Mutex<Vec<Arc<RetainedProfile>>>,
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
    ledger: Mutex<Vec<LedgerTurn>>,
}

impl ClaudeBinding {
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

    /// Derive current binding support from the registry descriptor and the live process.
    async fn capabilities(&self) -> forge_provider_api::capability::CapabilityManifest {
        if let Some(manifest) = self.state.effective_capabilities().await {
            return manifest;
        }
        let descriptor = crate::registry::descriptor(&claude_id()).expect("claude is registered");
        let mut manifest =
            forge_provider_api::capability::CapabilityManifest::from_descriptor(descriptor);
        manifest.provider_version = self.conversation.version().await;
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

    /// Require no foreground turn, compaction, or other operation before a native mutation.
    async fn require_idle(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        if self.has_active_prompt() || self.state.has_active_work().await {
            return Err(failure(operation, ProviderFailureKind::ExecutionActive));
        }
        Ok(())
    }
}

/// Retain one operation's immutable completion across dropped and repeated awaits.
struct OperationRecord<T> {
    result: Mutex<Option<Result<T, ProviderError>>>,
    ready: Notify,
}

impl<T: Clone> OperationRecord<T> {
    fn new() -> Self {
        Self {
            result: Mutex::new(None),
            ready: Notify::new(),
        }
    }
    async fn complete(&self, result: Result<T, ProviderError>) {
        *self.result.lock().await = Some(result);
        self.ready.notify_waiters();
    }
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
struct Operations {
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
    rewind: Store<(BindingId, Option<forge_provider_api::CheckpointId>), ()>,
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

impl Operations {
    fn new() -> Self {
        let identities = Arc::new(IdentityRegistry::new());
        Self {
            open: Store::registered(identities.clone()),
            configure: Store::registered(identities.clone()),
            fork: Store::registered(identities.clone()),
            compact: Store::registered(identities.clone()),
            rewind: Store::registered(identities.clone()),
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
            terminate: Store::registered(identities.clone()),
            identities,
        }
    }

    fn close_admission(&self) {
        self.identities.close_admission();
    }

    /// Join operation workers after native producers stop, excluding the owning shutdown operation.
    async fn join_all_except(&self, id: &OperationId) -> Result<(), ProviderError> {
        let results = [
            self.open.join_all_except(id).await,
            self.configure.join_all_except(id).await,
            self.fork.join_all_except(id).await,
            self.compact.join_all_except(id).await,
            self.rewind.join_all_except(id).await,
            self.close.join_all_except(id).await,
            self.cancel.join_all_except(id).await,
            self.submit.join_all_except(id).await,
            self.steer.join_all_except(id).await,
            self.reconcile.join_all_except(id).await,
            self.retract.join_all_except(id).await,
            self.models.join_all_except(id).await,
            self.skills.join_all_except(id).await,
            self.mcp.join_all_except(id).await,
            self.terminals.join_all_except(id).await,
            self.terminate.join_all_except(id).await,
        ];
        let failures = results
            .into_iter()
            .filter_map(Result::err)
            .collect::<Vec<_>>();
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
    owner: ThreadId,
    cleanup: ResourceCleanup,
}

/// Own one original native prompt and its callback collection under the shared runner.
struct ClaudeTurnDriver {
    session: Arc<ClaudeBinding>,
    scope: forge_provider_api::InvocationScope,
    invocation: Mutex<Option<PromptInvocation>>,
}

#[async_trait]
impl crate::turn_runner::NativeTurnDriver for ClaudeTurnDriver {
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

/// Own Claude Code processes, retained conversations, native event decoding, and completion.
#[derive(Clone)]
pub struct ClaudeProvider {
    launcher: Arc<dyn ClaudeLauncher>,
    cleanup: ResourceCleanup,
    runtime_root: PathBuf,
    bindings: Arc<std::sync::Mutex<HashMap<BindingId, Arc<ClaudeBinding>>>>,
    owners: Arc<Mutex<HashMap<ThreadId, BindingId>>>,
    preparation: Arc<Mutex<HashMap<ThreadId, Arc<Mutex<()>>>>>,
    history_admission: Arc<Mutex<()>>,
    history_reservations:
        Arc<Mutex<HashMap<forge_provider_api::ProviderSessionId, Vec<HistoryReservation>>>>,
    admission: Arc<AtomicBool>,
    shutdown_task: Arc<Mutex<Option<Arc<crate::retained_task::RetainedTask>>>>,
    operations: Arc<Operations>,
    pending_inputs: Arc<PendingInputs>,
    validated_version: Arc<Mutex<Option<String>>>,
}

impl ClaudeProvider {
    /// Create a Claude Code owner from the configured launch command.
    pub fn new(command: Vec<String>, runtime_root: PathBuf) -> Result<Self, ProviderError> {
        if command.is_empty() {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Rejected,
            ));
        }
        Ok(Self::with_launcher(
            Arc::new(ProcessLauncher::new(command)),
            runtime_root,
        ))
    }

    /// Create an owner from its process launcher.
    pub(super) fn with_launcher(launcher: Arc<dyn ClaudeLauncher>, runtime_root: PathBuf) -> Self {
        Self {
            launcher,
            cleanup: ResourceCleanup::default(),
            runtime_root,
            bindings: Arc::new(std::sync::Mutex::new(HashMap::new())),
            owners: Arc::new(Mutex::new(HashMap::new())),
            preparation: Arc::new(Mutex::new(HashMap::new())),
            history_admission: Arc::new(Mutex::new(())),
            history_reservations: Arc::new(Mutex::new(HashMap::new())),
            admission: Arc::new(AtomicBool::new(true)),
            shutdown_task: Arc::new(Mutex::new(None)),
            operations: Arc::new(Operations::new()),
            pending_inputs: Arc::new(PendingInputs::new()),
            validated_version: Arc::new(Mutex::new(None)),
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
            self.collect_interrupted_input(&binding, scope).await
        })
    }

    /// Collect interrupted delivery without replacing its invocation or shortening its lifetime.
    async fn collect_interrupted_input(
        &self,
        binding: &BindingId,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
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

    /// Get the exact retained binding generation without opening a replacement.
    fn binding(
        &self,
        id: &BindingId,
        operation: ProviderOperation,
    ) -> Result<Arc<ClaudeBinding>, ProviderError> {
        self.bindings
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(id)
            .cloned()
            .ok_or_else(|| failure(operation, ProviderFailureKind::SessionMissing))
    }

    /// Validate the installed CLI against the verified version floor once per provider.
    async fn validate_runtime(&self, context: &OperationContext) -> Result<(), ProviderError> {
        let mut validated = self.validated_version.lock().await;
        if validated.is_some() {
            return Ok(());
        }
        let requirement = forge_provider_api::version::RuntimeRequirement::new(
            claude_id(),
            self.launcher.executable(),
            super::process::MINIMUM_VERSION,
        );
        if !self.launcher.available() {
            return Err(requirement
                .probe_failed(forge_provider_api::version::RuntimeProbeFailure::Unavailable)
                .into());
        }
        let version = self.launcher.version(context).await.map_err(|_| {
            ProviderError::from(
                requirement
                    .probe_failed(forge_provider_api::version::RuntimeProbeFailure::Unavailable),
            )
        })?;
        requirement.validate(Some(&version))?;
        *validated = Some(version);
        Ok(())
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
            let driver = Arc::new(ClaudeTurnDriver {
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
        cleanup: ResourceCleanup,
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
        let mcp_servers = mcp_servers(
            ProviderOperation::ThreadOpen,
            &request.configuration.resources.mcp,
        )?;
        self.validate_runtime(&context).await?;
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
        let (session_id, start) = match request.history.resume_id() {
            Some(resume) => (
                resume.as_str().to_owned(),
                SessionStart::Resume {
                    session_id: resume.as_str().to_owned(),
                    resume_at: None,
                    drops_turn: None,
                },
            ),
            None => {
                let session_id = uuid::Uuid::new_v4().to_string();
                (session_id.clone(), SessionStart::Fresh { session_id })
            }
        };
        let provider_session_id: forge_provider_api::ProviderSessionId = session_id
            .clone()
            .try_into()
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol))?;
        {
            let mut reservations = self.history_reservations.lock().await;
            let entries = reservations.entry(provider_session_id.clone()).or_default();
            for reservation in entries.iter() {
                if !reservation.cleanup.is_collected() {
                    reservation.cleanup.shutdown().await?;
                }
            }
            entries.push(HistoryReservation {
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
            &self.runtime_root,
            &request.configuration.resources.skills,
        )?);
        cleanup.retain_lease(request.resource_lease.take());
        let selection = request.configuration.resources.clone();
        let state = Arc::new(ClaudeSessionState::new(self.admission.clone()));
        state.replace_host_tools(request.tools.clone());
        let requested_model = request.configuration.model.clone();
        let options = ConversationOptions {
            workspace: request.workspace.clone(),
            system_instructions: request.system_instructions.clone(),
            mcp_servers,
            plugin_dir: Some(profile.path().to_owned()),
            model: requested_model.model.clone(),
            effort: effort_level(&requested_model.reasoning),
            planning: false,
        };
        let conversation = match ClaudeConversation::open(
            self.launcher.clone(),
            state.clone(),
            session_id,
            start,
            options,
            &cleanup,
        )
        .await
        {
            Ok(conversation) => conversation,
            Err(error) => {
                state.invalidate_binding(error.clone()).await;
                return Err(match cleanup.shutdown().await {
                    Ok(()) => error,
                    Err(collection) => ProviderError::PreparationCleanup {
                        cause: Box::new(error),
                        cleanup: Box::new(collection),
                    },
                });
            }
        };
        let binding = SessionBinding {
            thread_id: request.thread_id.clone(),
            id: binding_id.clone(),
            provider_session_id: Some(provider_session_id),
        };
        let retained = Arc::new(ClaudeBinding {
            binding: binding.clone(),
            owner: request.thread_id.clone(),
            conversation: Arc::new(conversation),
            state,
            sink,
            cleanup: cleanup.clone(),
            profiles: Mutex::new(vec![profile]),
            selection: Mutex::new(selection),
            native_model: Mutex::new(requested_model.clone()),
            operation: Mutex::new(()),
            steering: RwLock::new(()),
            covered_inputs: std::sync::Mutex::new(HashMap::new()),
            active: std::sync::Mutex::new(None),
            settlements: std::sync::Mutex::new(HashMap::new()),
            collection_watermarks: Default::default(),
            close_task: Mutex::new(None),
            history_released: AtomicBool::new(false),
            rejected_inputs: Mutex::new(Vec::new()),
            ledger: Mutex::new(Vec::new()),
        });
        self.bindings
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(binding_id.clone(), retained.clone());
        self.owners
            .lock()
            .await
            .insert(request.thread_id.clone(), binding_id.clone());
        let capabilities = retained.capabilities().await;
        retained
            .state
            .install_capabilities(retained.sink.clone(), capabilities.clone())
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
        session
            .require_idle(ProviderOperation::ThreadConfigure)
            .await?;
        let _operation = session.operation.lock().await;
        session.sink.check()?;
        context.check_admission(ProviderOperation::ThreadConfigure)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadConfigure)?;
        session
            .require_idle(ProviderOperation::ThreadConfigure)
            .await?;
        validate_configuration(&configuration.model, &configuration.access)?;
        let servers = mcp_servers(
            ProviderOperation::ThreadConfigure,
            &configuration.resources.mcp,
        )?;
        let resources_changed =
            !same_resources(&*session.selection.lock().await, &configuration.resources);
        if let Err(error) = configure_model(
            &session,
            &configuration.model,
            forge_provider_api::ExecutionMode::Default,
        )
        .await
        {
            if matches!(&error, ProviderError::Unsupported(_))
                || matches!(&error, ProviderError::Failure(failure) if failure.disposition() == FailureDisposition::Unchanged)
            {
                return Err(error);
            }
            let error = operation_failure(
                ProviderOperation::ThreadConfigure,
                &error,
                ProviderPhase::Preparation,
                FailureDisposition::Invalidated,
            );
            session.invalidate(error.clone()).await;
            return Err(error);
        }
        if resources_changed {
            let profile = session.cleanup.retain_profile(super::profile::create(
                &self.runtime_root,
                &configuration.resources.skills,
            )?);
            session.profiles.lock().await.push(profile.clone());
            if let Err(error) = session
                .conversation
                .reconfigure(servers, Some(profile.path().to_owned()), &session.cleanup)
                .await
            {
                let error = operation_failure(
                    ProviderOperation::ThreadConfigure,
                    &error,
                    ProviderPhase::Preparation,
                    FailureDisposition::Invalidated,
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
        Ok(forge_provider_api::ConfiguredSession {
            configuration,
            capabilities,
            watermark: session.sink.watermark(),
        })
    }

    /// Copy settled history into a separately owned session id.
    async fn fork_session(
        &self,
        context: OperationContext,
        request: forge_provider_api::session::SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        context.check_admission(ProviderOperation::ThreadFork)?;
        validate_configuration(
            &request.target.configuration.model,
            &request.target.configuration.access,
        )?;
        let mcp = mcp_servers(
            ProviderOperation::ThreadFork,
            &request.target.configuration.resources.mcp,
        )?;
        let cleanup = self.cleanup.scope();
        let profile = cleanup.retain_profile(super::profile::create(
            &self.runtime_root,
            &request.target.configuration.resources.skills,
        )?);
        let options = ConversationOptions {
            workspace: request.target.workspace.clone(),
            system_instructions: request.target.system_instructions.clone(),
            mcp_servers: mcp,
            plugin_dir: Some(profile.path().to_owned()),
            model: request.target.configuration.model.model.clone(),
            effort: effort_level(&request.target.configuration.model.reasoning),
            planning: false,
        };
        let target_id = uuid::Uuid::new_v4().to_string();
        let result = match &request.source {
            forge_provider_api::ForkSource::Binding(binding) => {
                let session = self.binding(binding, ProviderOperation::ThreadFork)?;
                session
                    .state
                    .ensure_admission(ProviderOperation::ThreadFork)?;
                if request.target.thread_id == session.owner {
                    return Err(failure(
                        ProviderOperation::ThreadFork,
                        ProviderFailureKind::WrongInvocation,
                    ));
                }
                session.require_idle(ProviderOperation::ThreadFork).await?;
                let _operation = session.operation.lock().await;
                session.sink.check()?;
                context.check_admission(ProviderOperation::ThreadFork)?;
                session.require_idle(ProviderOperation::ThreadFork).await?;
                session
                    .conversation
                    .fork(&target_id, None, options, &cleanup)
                    .await
            }
            forge_provider_api::ForkSource::Checkpoint {
                provider_session_id,
                checkpoint,
            } => {
                super::conversation::fork_session(
                    &*self.launcher,
                    provider_session_id.as_str(),
                    &target_id,
                    Some(checkpoint.as_str().to_owned()),
                    options,
                    &cleanup,
                )
                .await
            }
        };
        let collected = cleanup.shutdown().await;
        let result = match (result, collected) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(error), Ok(())) => Err(error),
            (Ok(()), Err(collection)) => Err(collection),
            (Err(error), Err(collection)) => Err(with_cleanup(error, vec![collection])),
        };
        result.map_err(|error| {
            operation_failure(
                ProviderOperation::ThreadFork,
                &error,
                ProviderPhase::Observation,
                FailureDisposition::Uncertain,
            )
        })?;
        let id = target_id
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
        let session = self.binding(binding, ProviderOperation::ThreadCompact)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadCompact)?;
        session
            .require_idle(ProviderOperation::ThreadCompact)
            .await?;
        let _operation = session.operation.lock().await;
        session.sink.check()?;
        context.check_admission(ProviderOperation::ThreadCompact)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadCompact)?;
        session
            .require_idle(ProviderOperation::ThreadCompact)
            .await?;
        if request.scope.thread_id() != &session.owner {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let sink = session.sink.bind_scope(request.scope.clone())?;
        let command_id = uuid::Uuid::new_v4().to_string();
        let ended = session
            .state
            .begin_compaction(sink, &session.owner, &command_id)
            .await?;
        let written = session
            .conversation
            .write(&UserInput::command(command_id, "/compact"))
            .await;
        let ended = match written {
            Ok(()) => ended.await.ok(),
            Err(error) => {
                session.state.finish_compaction().await;
                return Err(with_operation(ProviderOperation::ThreadCompact, error));
            }
        };
        session.state.finish_compaction().await;
        let compaction = ended
            .and_then(|ended| ended.compaction)
            .filter(|compaction| {
                matches!(
                    compaction.state,
                    forge_provider_api::CompactionState::Completed
                        | forge_provider_api::CompactionState::Failed
                )
            });
        let Some(compaction) = compaction else {
            let cause = ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::ThreadCompact,
                    ProviderFailureKind::Protocol,
                )
                .at_phase(ProviderPhase::Observation, FailureDisposition::Invalidated),
            );
            session.invalidate(cause.clone()).await;
            return Err(cause);
        };
        Ok(forge_provider_api::CompactionResult {
            compaction,
            watermark: session.sink.watermark(),
        })
    }

    /// Truncate native history after a checkpoint by restarting the idle process at it.
    async fn rewind_session(
        &self,
        context: OperationContext,
        binding: &BindingId,
        after: Option<forge_provider_api::CheckpointId>,
    ) -> Result<(), ProviderError> {
        context.check_admission(ProviderOperation::ThreadRewind)?;
        let session = self.binding(binding, ProviderOperation::ThreadRewind)?;
        session
            .state
            .ensure_admission(ProviderOperation::ThreadRewind)?;
        // The CLI resumes only at a transcript entry, so a rewind past every turn has no target.
        let Some(after) = after else {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::NativeConversationRewind,
            )
            .into());
        };
        session
            .require_idle(ProviderOperation::ThreadRewind)
            .await?;
        let _operation = session.operation.lock().await;
        session.sink.check()?;
        context.check_admission(ProviderOperation::ThreadRewind)?;
        session
            .require_idle(ProviderOperation::ThreadRewind)
            .await?;
        let mut ledger = session.ledger.lock().await;
        let kept = ledger
            .iter()
            .position(|turn| turn.checkpoint.as_deref() == Some(after.as_str()));
        if let Some(kept) = kept
            && kept + 1 == ledger.len()
        {
            return Ok(());
        }
        // The CLI validates exactly one dropped turn; a wider range runs unguarded.
        let drops_turn = kept
            .filter(|kept| kept + 2 == ledger.len())
            .map(|kept| ledger[kept + 1].prompt.clone());
        let session_id = session.conversation.id().to_owned();
        let rewound = session
            .conversation
            .restart(
                SessionStart::Resume {
                    session_id: session_id.clone(),
                    resume_at: Some(after.as_str().to_owned()),
                    drops_turn,
                },
                &session.cleanup,
            )
            .await;
        let persisted = match rewound {
            Ok(()) => persist_rewind(&session).await,
            Err(error) => Err(error),
        };
        if let Err(error) = persisted {
            // A refused or unpersisted rewind leaves the transcript unchanged; resume it plainly.
            let restored = session
                .conversation
                .restart(
                    SessionStart::Resume {
                        session_id,
                        resume_at: None,
                        drops_turn: None,
                    },
                    &session.cleanup,
                )
                .await;
            return Err(match restored {
                Ok(()) => ProviderError::Failure(
                    ProviderFailure::new(
                        ProviderOperation::ThreadRewind,
                        ProviderFailureKind::Rejected,
                    )
                    .at_phase(ProviderPhase::Observation, FailureDisposition::Unchanged),
                ),
                Err(restore) => {
                    let cause = with_cleanup(
                        operation_failure(
                            ProviderOperation::ThreadRewind,
                            &error,
                            ProviderPhase::Observation,
                            FailureDisposition::Invalidated,
                        ),
                        vec![restore],
                    );
                    session.invalidate(cause.clone()).await;
                    cause
                }
            });
        }
        match kept {
            Some(kept) => ledger.truncate(kept + 1),
            None => ledger.clear(),
        }
        Ok(())
    }

    /// Deliver steering into the exact running invocation.
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
        if request.scope.thread_id != session.owner
            || session
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
        let content = super::conversation::input_blocks(
            vec![prompt_input(&request.input)],
            &request.attachments,
        );
        let receipt = request.delivery.receipt().cloned();
        let submission_id = request.delivery.submission_id().cloned();
        let covered_input = receipt
            .as_ref()
            .map(|receipt| {
                receipt.attempt_id().map(|attempt| {
                    forge_provider_api::CoveredInput::new(receipt.id().clone(), attempt)
                })
            })
            .transpose()?;
        session.sink.check()?;
        let uuid = uuid::Uuid::new_v4().to_string();
        let mut observer = match session.state.track_command(&uuid, true).await {
            Ok(observer) => observer,
            Err(_) => {
                // The turn already ended; Harness delivers the input as a new turn instead.
                let cause = ProviderError::PreparationFailed(Box::new(
                    ProviderFailure::new(
                        ProviderOperation::TurnSteer,
                        ProviderFailureKind::Rejected,
                    )
                    .at_phase(ProviderPhase::Observation, FailureDisposition::Unchanged)
                    .into(),
                ));
                if let Some(receipt) = &receipt
                    && let Err(record) = receipt.clone().record_rejected(cause.clone()).await
                {
                    return Err(ProviderError::Combined {
                        primary: Box::new(cause),
                        secondary: vec![record],
                    });
                }
                return Err(cause);
            }
        };
        let session_id = session.conversation.id().to_owned();
        let delivered = deliver_input(
            &session,
            ProviderOperation::TurnSteer,
            UserInput::composed(uuid.clone(), content),
            &session_id,
            receipt.as_ref(),
            None,
            &mut observer,
        )
        .await;
        if let Err(error) = delivered {
            session.state.forget_command(&uuid).await;
            return Err(error);
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

    /// Close every producer before joining retained native operations and pending opens.
    async fn shutdown_provider(&self, operation_id: OperationId) -> Result<(), ProviderError> {
        self.admission.store(false, Ordering::Release);
        self.operations.close_admission();
        let mut errors = Vec::new();
        for _ in 0..2 {
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
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(combine_errors(errors))
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
        match session.settlement(&request.scope).await {
            Ok(_) | Err(ProviderError::Cancelled) => Ok(()),
            Err(error) => Err(error),
        }
    }

    async fn reconcile_input(
        &self,
        request: forge_provider_api::InputReconcileRequest,
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        let Some(token) = request.token.as_ref() else {
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
            && request
                .accepted_receipts
                .iter()
                .any(|receipt| receipt.as_bytes() == token.as_bytes())
        {
            let provider_session_id = Some(checkpoint.session_id.try_into().map_err(|_| {
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
        context: OperationContext,
        request: forge_provider_api::catalog::QueryContext,
        cleanup: ResourceCleanup,
    ) -> Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        match request {
            forge_provider_api::catalog::QueryContext::Binding(binding) => {
                let session = self.binding(&binding, ProviderOperation::ModelList)?;
                session
                    .state
                    .ensure_admission(ProviderOperation::ModelList)?;
                Ok(session.conversation.models().await)
            }
            forge_provider_api::catalog::QueryContext::Workspace(workspace) => {
                self.validate_runtime(&context)
                    .await
                    .map_err(|error| with_operation(ProviderOperation::ModelList, error))?;
                let result = super::conversation::workspace_models(
                    &*self.launcher,
                    &workspace.workspace,
                    &cleanup,
                )
                .await
                .map_err(|error| with_operation(ProviderOperation::ModelList, error));
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
        Ok(session.selection.lock().await.skills.clone())
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
        for definition in &mut definitions {
            use forge_provider_api::catalog::McpStatus;
            definition.status = match catalog.mcp_status.get(&definition.name).map(String::as_str) {
                Some("connected") => McpStatus::Connected,
                Some("disabled") => McpStatus::Disabled,
                Some("needs-auth") => McpStatus::NeedsAuthentication,
                Some("failed") => McpStatus::Failed,
                Some("pending") => McpStatus::Loading,
                _ => McpStatus::Unavailable,
            };
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
}

/// Make a rewind durable by appending a note, because the CLI persists a truncation only when
/// the next entry is appended.
async fn persist_rewind(session: &ClaudeBinding) -> Result<(), ProviderError> {
    let note = UserInput::note(
        uuid::Uuid::new_v4().to_string(),
        "[Forge: the conversation was rewound to an earlier point; later turns were discarded]",
    );
    let mut observer = session.state.track_command(&note.uuid, false).await?;
    session.conversation.write(&note).await?;
    wait_command(
        &mut observer,
        |state| state == CommandState::Completed,
        ProviderOperation::ThreadRewind,
    )
    .await
    .map(|_| ())
}

#[async_trait]
impl forge_provider_api::provider::Provider for ClaudeProvider {
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

    async fn thread_rewind(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::ThreadRewindRequest,
    ) -> Result<(), ProviderError> {
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let binding = binding.clone();
        let cleanup_binding = binding.clone();
        let payload = (binding.clone(), request.after.clone());
        let worker_context = context.clone();
        self.operations
            .rewind
            .observe(
                context,
                ProviderOperation::ThreadRewind,
                payload,
                async move {
                    provider
                        .rewind_session(worker_context, &binding, request.after)
                        .await
                },
                move |interruption| {
                    cleanup_provider.interrupt_binding(
                        cleanup_binding,
                        ProviderOperation::ThreadRewind,
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
        let worker_context = context.clone();
        // Workspace discovery owns a disposable native process; interruption must collect it.
        let cleanup = self.cleanup.scope();
        let interrupt_cleanup = cleanup.clone();
        self.operations
            .models
            .observe(
                context,
                ProviderOperation::ModelList,
                payload,
                async move { provider.list_models(worker_context, query, cleanup).await },
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
    session: &ClaudeBinding,
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
    session: Arc<ClaudeBinding>,
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
async fn close_binding(session: Arc<ClaudeBinding>) -> Result<(), ProviderError> {
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
    // Explicit binding close owns the entire transport, including unresponsive prompts.
    let collected = match session.cleanup.shutdown().await {
        Ok(()) => true,
        Err(error) => {
            errors.push(with_operation(ProviderOperation::ThreadClose, error));
            false
        }
    };
    if let Some(scope) = scope
        && let Err(error) = session.settlement(&scope).await
        && !matches!(error, ProviderError::Cancelled)
    {
        errors.push(error);
    }
    let _operation = session.operation.lock().await;
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

/// Write one user input and commit its delivery evidence through the host receipt.
///
/// The CLI reports `command_lifecycle queued` once it owns the input, which is the acceptance
/// evidence; the write alone proves nothing.
async fn deliver_input(
    session: &ClaudeBinding,
    operation: ProviderOperation,
    input: UserInput,
    session_id: &str,
    receipt: Option<&Arc<dyn forge_provider_api::submission::InputReceipt>>,
    dispatch: Option<&forge_provider_api::submission::DispatchReporter>,
    observer: &mut tokio::sync::watch::Receiver<CommandState>,
) -> Result<(), ProviderError> {
    let sending = receipt
        .map(|receipt| {
            checkpoint_token(false, session_id, &input.uuid, receipt.as_ref(), operation)
        })
        .transpose()?;
    let accepted = receipt
        .map(|receipt| checkpoint_token(true, session_id, &input.uuid, receipt.as_ref(), operation))
        .transpose()?;
    if let Some(receipt) = receipt {
        receipt
            .clone()
            .record_sending(sending)
            .await
            .map_err(|error| error.for_operation(operation))?;
    }
    let result = async {
        session.conversation.write(&input).await?;
        if let Some(dispatch) = dispatch {
            dispatch.confirm();
        }
        wait_command(observer, |state| state != CommandState::Other, operation).await?;
        if let Some(receipt) = receipt {
            receipt
                .clone()
                .record_accepted(accepted)
                .await
                .map_err(|error| match error {
                    ProviderError::Failure(failure) => ProviderError::Failure(
                        failure
                            .for_operation(operation)
                            .at_phase(ProviderPhase::Persistence, FailureDisposition::Uncertain),
                    ),
                    error => ProviderError::Combined {
                        primary: Box::new(
                            ProviderFailure::new(operation, ProviderFailureKind::Unknown)
                                .at_phase(ProviderPhase::Persistence, FailureDisposition::Uncertain)
                                .into(),
                        ),
                        secondary: vec![error],
                    },
                })?;
        }
        Ok::<(), ProviderError>(())
    }
    .await;
    if let (Err(error), Some(receipt)) = (&result, receipt)
        && let Err(record) = receipt
            .clone()
            .record_failure(error.clone().for_operation(operation))
            .await
    {
        return Err(ProviderError::Combined {
            primary: Box::new(error.clone()),
            secondary: vec![record],
        });
    }
    result
}

async fn run_prompt(
    session: Arc<ClaudeBinding>,
    request: PromptRequest,
    accepted: tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>,
    start: forge_provider_api::NativeTurnStart,
) -> Result<forge_provider_api::TurnResult, ProviderError> {
    let mut accepted = Some(accepted);
    let result = async {
        let cancellation = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .as_ref()
            .filter(|active| active.scope == request.scope)
            .map(|active| active.cancellation.clone())
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        let reject = |session: &Arc<ClaudeBinding>, request: &PromptRequest| {
            let session = session.clone();
            let entry = (
                request.scope.clone(),
                request.submission_id.clone(),
                request.attempt_id.clone(),
            );
            async move { session.rejected_inputs.lock().await.push(entry) }
        };
        let preparation = tokio::select! {
            biased;
            _ = cancellation.cancelled() => Err(ProviderError::Cancelled),
            result = request.turn_control.before_send(&request.scope) => result,
        };
        if let Err(error) = preparation {
            reject(&session, &request).await;
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        if !session.state.is_admitting() || cancellation.is_cancelled() {
            reject(&session, &request).await;
            return Err(reject_before_dispatch(&request.receipt, ProviderError::Cancelled).await);
        }
        let _operation = session.operation.lock().await;
        session.sink.check()?;
        if session.state.replace_host_tools(request.tools.clone())
            && let Err(error) = session.conversation.host_tools_changed().await
        {
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        if let Err(error) = configure_model(
            &session,
            &request.configuration.model,
            request.execution,
        )
        .await
        {
            let error = with_operation(ProviderOperation::TurnStart, error);
            if matches!(&error, ProviderError::Failure(failure) if failure.disposition() == FailureDisposition::Invalidated)
            {
                session.invalidate(error.clone()).await;
            }
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        *session.native_model.lock().await = request.configuration.model.clone();
        let event_sink = session.sink.invocation_sink(&request.scope)?;
        let address = forge_provider_api::TurnAddress {
            scope: request.scope.clone(),
            thread_id: request.thread_id.clone(),
            turn_id: request.turn_id.clone(),
        };
        let turn_end = match session
            .state
            .begin_turn(
                event_sink,
                address,
                request.permissions.clone(),
                request.provider_tools.clone(),
                start,
            )
            .await
        {
            Ok(turn_end) => turn_end,
            Err(error) => {
                let error = if cancellation.is_cancelled() {
                    ProviderError::Cancelled
                } else {
                    error
                };
                reject(&session, &request).await;
                return Err(reject_before_dispatch(&request.receipt, error).await);
            }
        };
        if cancellation.is_cancelled() || !session.state.is_admitting() {
            session.state.finish(forge_provider_api::turn::TurnOutcome::Interrupted).await?;
            reject(&session, &request).await;
            return Err(reject_before_dispatch(&request.receipt, ProviderError::Cancelled).await);
        }
        let prompt_uuid = uuid::Uuid::new_v4().to_string();
        let content = super::conversation::input_blocks(prompt_texts(&request), &request.attachments);
        let input = if is_compact_command(&request) {
            UserInput::command(prompt_uuid.clone(), "/compact")
        } else {
            UserInput::composed(prompt_uuid.clone(), content)
        };
        let mut observer = session.state.track_command(&prompt_uuid, true).await?;
        let session_id = session.conversation.id().to_owned();
        let receipt = request.receipt.clone();
        if let Err(error) = deliver_input(
            &session,
            ProviderOperation::TurnStart,
            input,
            &session_id,
            Some(&receipt),
            Some(&request.dispatch),
            &mut observer,
        )
        .await
        {
            session.invalidate(error.clone()).await;
            let finished = session
                .state
                .finish(forge_provider_api::turn::TurnOutcome::Interrupted)
                .await;
            return Err(match finished {
                Ok(()) => error,
                Err(cleanup) => with_cleanup(error, vec![cleanup]),
            });
        }
        if let Some(sender) = accepted.take() {
            let _ = sender.send(Ok(InputAcceptance {
                submission_id: request.submission_id.clone(),
            }));
        }
        tokio::pin!(turn_end);
        let end = tokio::select! {
            biased;
            end = &mut turn_end => end,
            _ = cancellation.cancelled() => {
                session.state.mark_interrupting(&request.scope).await;
                let interrupted = async {
                    if let Err(error) = session.conversation.interrupt().await {
                        session.invalidate(error).await;
                    }
                    (&mut turn_end).await
                };
                match tokio::time::timeout(INTERRUPT_GRACE, interrupted).await {
                    Ok(end) => end,
                    Err(_) => {
                        // A CLI that neither answers the interrupt nor ends its turn is wedged;
                        // invalidating the binding ends the turn so Harness can restart it.
                        session
                            .invalidate(
                                ProviderFailure::new(
                                    ProviderOperation::TurnInterrupt,
                                    ProviderFailureKind::Transport,
                                )
                                .at_phase(ProviderPhase::Observation, FailureDisposition::Invalidated)
                                .into(),
                            )
                            .await;
                        turn_end.await
                    }
                }
            }
        };
        let end = match end {
            Ok(Ok(end)) => end,
            Ok(Err(error)) => return Err(error),
            Err(_) => {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Transport,
                ));
            }
        };
        let _steering_boundary = session.steering.write().await;
        if end.outcome == forge_provider_api::turn::TurnOutcome::Interrupted
            && !session.state.execution_started().await
        {
            session.state.finish(end.outcome).await?;
            return Err(ProviderError::Cancelled);
        }
        session.state.observe_native_start(end.completed_at_ms).await?;
        if !session.conversation.is_closed().await
            && let Some(usage) = session.conversation.context_usage().await
        {
            session.state.publish_context_usage(usage).await?;
        }
        session.state.finish(end.outcome).await?;
        let _ = session.state.take_completion().await;
        if let Some(prompt) = end.commands.first() {
            session.ledger.lock().await.push(LedgerTurn {
                prompt: prompt.clone(),
                checkpoint: end.checkpoint.clone(),
            });
        }
        let checkpoint = end
            .checkpoint
            .clone()
            .map(forge_provider_api::CheckpointId::new)
            .transpose()
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        forge_provider_api::TurnResult::new(
            end.outcome,
            end.completed_at_ms,
            end.duration_ms,
            covered_inputs(&session, &request.scope)?,
            Vec::new(),
            checkpoint,
        )
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
                    .finish(forge_provider_api::turn::TurnOutcome::Interrupted)
                    .await
                {
                    error = with_cleanup(error, vec![cleanup]);
                }
            }
            if let Some(sender) = accepted.take() {
                let _ = sender.send(Err(error.clone()));
            }
            Err(error)
        }
    }
}

/// Reject native controls this adapter cannot apply before allocating profiles or history.
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

fn validate_model_selection(
    requested: &forge_provider_api::session::ModelSelection,
) -> Result<(), ProviderError> {
    if requested.service_tier != forge_provider_api::ServiceTier::Standard {
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

/// Get the CLI effort level for a reasoning selection.
fn effort_level(reasoning: &forge_provider_api::generation::ReasoningSelection) -> Option<String> {
    match reasoning {
        forge_provider_api::generation::ReasoningSelection::Default => None,
        forge_provider_api::generation::ReasoningSelection::Level(level) => Some(level.clone()),
        forge_provider_api::generation::ReasoningSelection::Lowest => Some("low".into()),
    }
}

/// Apply one invocation's model, effort, and permission mode before its prompt is written.
async fn configure_model(
    session: &ClaudeBinding,
    requested: &forge_provider_api::session::ModelSelection,
    mode: forge_provider_api::ExecutionMode,
) -> Result<(), ProviderError> {
    use forge_provider_api::generation::ReasoningSelection;
    validate_model_selection(requested)?;
    let models = session.conversation.models().await;
    let model = requested.model.as_deref().unwrap_or("default");
    let Some(selected) = models.iter().find(|candidate| candidate.id == model) else {
        return Err(failure(
            ProviderOperation::ThreadConfigure,
            ProviderFailureKind::Rejected,
        ));
    };
    let effort = match &requested.reasoning {
        ReasoningSelection::Default => None,
        ReasoningSelection::Level(level) => {
            if !selected.reasoning.contains(level) {
                return Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::EffortSelection,
                )
                .into());
            }
            Some(level.as_str())
        }
        ReasoningSelection::Lowest => Some(selected.reasoning.first().map(String::as_str).ok_or(
            forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::EffortSelection,
            ),
        )?),
    };
    session
        .conversation
        .configure(
            Some(model),
            effort,
            mode == forge_provider_api::ExecutionMode::Planning,
        )
        .await
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

fn combine_errors(mut failures: Vec<ProviderError>) -> ProviderError {
    let primary = failures.remove(0);
    with_cleanup(primary, failures)
}

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

/// Recovery evidence for one exact delivery attempt.
struct DeliveryCheckpoint {
    accepted: bool,
    session_id: String,
    submission_id: String,
    attempt_id: String,
}

/// Encode recovery evidence naming the input's session, uuid, and journal attempt.
fn checkpoint_token(
    accepted: bool,
    session_id: &str,
    uuid: &str,
    receipt: &dyn forge_provider_api::submission::InputReceipt,
    operation: ProviderOperation,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    let attempt = receipt.attempt_id()?;
    let bytes = serde_json::to_vec(&json!({
        "version": 1, "provider": "claude", "accepted": accepted, "session": session_id,
        "input": uuid, "submission": receipt.id().as_str(), "attempt": attempt.as_str(),
    }))
    .map_err(|_| failure(operation, ProviderFailureKind::Protocol))?;
    forge_provider_api::RecoveryToken::new(bytes).map_err(Into::into)
}

fn parse_checkpoint(
    token: &forge_provider_api::RecoveryToken,
) -> Result<DeliveryCheckpoint, ProviderError> {
    let protocol = || {
        failure(
            ProviderOperation::InputReconcile,
            ProviderFailureKind::Protocol,
        )
    };
    let value: Value = serde_json::from_slice(token.as_bytes()).map_err(|_| protocol())?;
    if value["version"] != 1 || value["provider"] != "claude" {
        return Err(protocol());
    }
    let field = |name: &str| {
        value[name]
            .as_str()
            .filter(|value| !value.trim().is_empty())
            .map(str::to_owned)
            .ok_or_else(protocol)
    };
    Ok(DeliveryCheckpoint {
        accepted: value["accepted"].as_bool().ok_or_else(protocol)?,
        session_id: field("session")?,
        submission_id: field("submission")?,
        attempt_id: field("attempt")?,
    })
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
                } => format!(
                    "Answer: {answer}\nRequired questions remain: {required_questions_remain}"
                ),
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
                } => format!(
                    "Correction: {answer}\nRequired questions remain: {required_questions_remain}"
                ),
            };
            format!(
                "Question response for {} (revision {}, message {}):\n{}\n{}",
                reply.question_id, reply.revision, reply.message_id, reply.question, response
            )
        }
    }
}

/// Report whether the input is exactly the native `/compact` command.
fn is_compact_command(request: &PromptRequest) -> bool {
    request.recovered_inputs.is_empty()
        && request.attachments.is_empty()
        && matches!(&request.input, forge_provider_api::PromptInput::Text(text) if text == "/compact")
}

/// Order the prompt's text parts: recovered inputs, task instructions, then the input itself.
fn prompt_texts(request: &PromptRequest) -> Vec<String> {
    let mut texts = Vec::new();
    for (index, recovered) in request.recovered_inputs.iter().enumerate() {
        texts.push(format!(
            "Earlier user input {} was saved locally but not accepted:",
            index + 1
        ));
        texts.push(recovered.instructions.clone());
        for attachment in &recovered.attachments {
            texts.push(attachment.context.clone());
        }
        texts.push(prompt_input(&recovered.input));
    }
    if !request.recovered_inputs.is_empty() {
        texts.push("Newest user input:".into());
    }
    texts.push(request.instructions.clone());
    texts.push(prompt_input(&request.input));
    texts
}

/// Encode admitted MCP launches in the CLI's `--mcp-config` format.
fn mcp_servers(
    operation: ProviderOperation,
    launches: &[forge_provider_api::session::McpLaunch],
) -> Result<Map<String, Value>, ProviderError> {
    use forge_provider_api::session::McpTransport;
    let mut servers = Map::new();
    for launch in launches {
        if launch.name == super::tool_targets::HOST_TOOL_SERVER
            || servers.contains_key(&launch.name)
        {
            return Err(failure(operation, ProviderFailureKind::Rejected));
        }
        if launch.timeout.is_some()
            || launch.working_directory.is_some()
            || matches!(
                launch.tools,
                Some(forge_provider_api::session::McpToolSelection::Only(_))
            )
        {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::McpLaunchRequirements,
            )
            .into());
        }
        let config = match &launch.transport {
            McpTransport::Stdio {
                command,
                arguments,
                environment,
            } => {
                json!({"type": "stdio", "command": command, "args": arguments, "env": environment})
            }
            McpTransport::Http { url, headers } => {
                json!({"type": "http", "url": url, "headers": headers})
            }
            McpTransport::Sse { url, headers } => {
                json!({"type": "sse", "url": url, "headers": headers})
            }
        };
        servers.insert(launch.name.clone(), config);
    }
    Ok(servers)
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
    current.skills == desired.skills && current.mcp == desired.mcp
}

/// Preserve a typed native cause while classifying the requested operation and authority effect.
fn operation_failure(
    operation: ProviderOperation,
    error: &ProviderError,
    phase: ProviderPhase,
    disposition: FailureDisposition,
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

fn claude_id() -> forge_provider_api::ProviderId {
    forge_provider_api::ProviderId::new("claude").expect("claude is a valid provider id")
}

#[allow(dead_code)]
fn now() -> i64 {
    timestamp()
}
