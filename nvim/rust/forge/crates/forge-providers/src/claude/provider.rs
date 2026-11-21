//! The Claude Code `Provider`: retained operations, bindings, and turn execution.

use super::conversation::{ClaudeConversation, ConversationOptions, failure, wait_command};
use super::process::{ClaudeLauncher, ProcessLauncher, SessionStart};
use super::recovery::{RepairIntent, RepairJournal, RepairStatus};
use super::session::{ClaudeSessionState, timestamp};
use super::wire::control::{OutboundControl, RewindRefusal, RewindResponse};
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

/// How long an interrupt waits for its turn to settle before it reports the stop failed.
///
/// The CLI gets `INTERRUPT_GRACE` to end the turn and each written input the same to be
/// acknowledged, so a healthy settlement fits well inside twice that. The bound stays under the
/// 30-second response deadline Harness gives a stop, so a wedged turn surfaces as a failed stop,
/// which restarts the provider, rather than as a stop that never ends.
const INTERRUPT_SETTLEMENT_GRACE: std::time::Duration = std::time::Duration::from_secs(20);

/// Report whether the live CLI could apply the requested cut.
enum LiveRewind {
    Applied,
    ResumeRequired,
}

/// One completed root turn in the binding's native history.
#[derive(Clone, Debug)]
struct LedgerTurn {
    /// The client-stamped uuid of the turn's first input.
    prompt: String,
    /// The client-stamped uuid of the last input the turn consumed.
    last_input: String,
    /// The turn's last transcript chain entry.
    checkpoint: Option<String>,
    /// The rewind note the adapter appended after the checkpoint, the newest chain entry while
    /// this turn is the last one kept.
    ///
    /// A later turn's prompt follows this note rather than the checkpoint, so a rewind that keeps
    /// this turn resumes at the note, and a rewind that drops this turn discards the note too.
    rewind_note: Option<String>,
    /// Whether the CLI absorbed steering into this turn. The CLI records each absorbed steer as
    /// queued content, which `--resume-drops-turn` refuses to discard, so dropping this turn
    /// resumes unguarded.
    absorbed_steering: bool,
    /// The chain entry the turn's prompt was written after, when this process knew it.
    follows: Option<String>,
    /// Whether an interrupt ended the turn while a request the CLI recorded had no answer: the
    /// prompt itself, or a steer that started a follow-up CLI turn after the turn's answer.
    ///
    /// The CLI then keeps the request as an unanswered user entry, after `checkpoint` when the
    /// turn answered earlier input, until `withdraw_unanswered_request` removes or withdraws it.
    unanswered: bool,
}

impl LedgerTurn {
    /// Name the chain entry a rewind that keeps this turn resumes at.
    ///
    /// Harness names the kept turn by its checkpoint, but the entries between the checkpoint and
    /// the next turn's prompt belong to neither turn. Resuming at the note keeps it, so the
    /// discarded range starts with the dropped turn's prompt as `--resume-drops-turn` requires.
    fn resume_point(&self) -> Option<&str> {
        self.rewind_note.as_deref().or(self.checkpoint.as_deref())
    }
}

struct ClaudeBinding {
    binding: SessionBinding,
    owner: ThreadId,
    conversation: Arc<ClaudeConversation>,
    repair: Option<RepairJournal>,
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
    /// The last entry of the history this binding keeps, while this process knows it.
    ///
    /// It is the newest turn's checkpoint, or the checkpoint a rewind kept. A rewind leaves the
    /// discarded turns in the CLI transcript beside the kept history, and a `--fork-session`
    /// without a position copied the discarded branch, so a fork copies up to this entry. A turn
    /// that reports no checkpoint and a compaction append entries the adapter cannot name, so they
    /// clear it, and a fork then takes the CLI's own choice.
    fork_point: std::sync::Mutex<Option<String>>,
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
    unstarted_sessions: super::conversation::UnstartedSessions,
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
            unstarted_sessions: Default::default(),
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

    /// Reconcile a checkpoint fork's source before the fork helper can read its history.
    async fn repair_fork_source(
        &self,
        source: &str,
        journal: &RepairJournal,
        options: &ConversationOptions,
    ) -> Result<(), ProviderError> {
        const OPERATION: ProviderOperation = ProviderOperation::ThreadFork;
        let live = self
            .bindings
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .values()
            .find(|binding| binding.conversation.id() == source)
            .cloned();
        if let Some(live) = live {
            let _operation = live.operation.lock().await;
            live.require_idle(OPERATION).await?;
            return reconcile_stopped_input(&live.conversation, journal, OPERATION).await;
        }
        let cleanup = self.cleanup.scope();
        let result = async {
            let conversation = ClaudeConversation::open(
                self.launcher.clone(),
                Arc::new(ClaudeSessionState::new(self.admission.clone())),
                source.to_owned(),
                SessionStart::Resume {
                    session_id: source.to_owned(),
                    resume_at: None,
                    drops_turn: None,
                },
                options.clone(),
                self.unstarted_sessions.clone(),
                &cleanup,
            )
            .await?;
            reconcile_stopped_input(&conversation, journal, OPERATION).await
        }
        .await;
        match (result, cleanup.shutdown().await) {
            (Ok(()), result) | (result, Ok(())) => result,
            (Err(error), Err(cleanup)) => Err(with_cleanup(error, vec![cleanup])),
        }
    }

    /// Resolve Claude's versioned repair record for a durable native conversation.
    fn repair_journal(&self, session: &str) -> Option<RepairJournal> {
        RepairJournal::new(
            &self.runtime_root,
            self.launcher.transcript_projects()?,
            session,
        )
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

    /// Validate the selected CLI before each catalog probe or replacement process.
    async fn validate_runtime(&self, context: &OperationContext) -> Result<(), ProviderError> {
        let requirement = super::process::runtime_requirement(self.launcher.executable());
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
        let isolated = matches!(
            request.history,
            forge_provider_api::SessionHistory::Temporary
        );
        if isolated
            && (!request.tools.is_empty()
                || request.tool_endpoint.is_some()
                || !request.configuration.resources.skills.is_empty()
                || !request.configuration.resources.mcp.is_empty())
        {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Policy,
            ));
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
            None if isolated => (uuid::Uuid::new_v4().to_string(), SessionStart::Ephemeral),
            None => {
                let session_id = uuid::Uuid::new_v4().to_string();
                self.unstarted_sessions.mint(&session_id);
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
        let process_owner = request
            .resource_lease
            .as_ref()
            .and_then(|lease| lease.process_owner());
        cleanup.retain_lease(request.resource_lease.take());
        let selection = request.configuration.resources.clone();
        let state = Arc::new(ClaudeSessionState::new(self.admission.clone()));
        state.replace_host_tools(request.tools.clone());
        let requested_model = request.configuration.model.clone();
        let options = ConversationOptions {
            process_owner,
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
            self.unstarted_sessions.clone(),
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
            provider_session_id: (!isolated).then_some(provider_session_id),
        };
        let retained = Arc::new(ClaudeBinding {
            binding: binding.clone(),
            owner: request.thread_id.clone(),
            repair: (!isolated)
                .then(|| self.repair_journal(conversation.id()))
                .flatten(),
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
            fork_point: std::sync::Mutex::new(None),
        });
        if let Some(journal) = retained.repair.as_ref()
            && let Err(error) = reconcile_stopped_input(
                &retained.conversation,
                journal,
                ProviderOperation::ThreadOpen,
            )
            .await
        {
            return Err(match cleanup.shutdown().await {
                Ok(()) => error,
                Err(collection) => with_cleanup(error, vec![collection]),
            });
        }
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
            process_owner: None,
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
                withdraw_unanswered_request(&session, ProviderOperation::ThreadFork).await?;
                let fork_point = session
                    .fork_point
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .clone();
                session
                    .conversation
                    .fork(&target_id, fork_point, options, &cleanup)
                    .await
            }
            forge_provider_api::ForkSource::Checkpoint {
                provider_session_id,
                checkpoint,
            } => {
                if let Some(journal) = self.repair_journal(provider_session_id.as_str())
                    && journal
                        .load()
                        .map_err(|error| {
                            super::recovery::failure(ProviderOperation::ThreadFork, error)
                        })?
                        .is_some()
                {
                    let _history = self.history_admission.lock().await;
                    self.repair_fork_source(provider_session_id.as_str(), &journal, &options)
                        .await?;
                }
                super::conversation::fork_session(
                    &*self.launcher,
                    &self.unstarted_sessions,
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
        withdraw_unanswered_request(&session, ProviderOperation::ThreadCompact).await?;
        let sink = session.sink.bind_scope(request.scope.clone())?;
        let command_id = uuid::Uuid::new_v4().to_string();
        let ended = session
            .state
            .begin_compaction(sink, &session.owner, context.id().as_str())
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
        // The compaction appended entries after the last known one, which a fork must copy.
        session
            .fork_point
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
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

    /// Truncate native history after a checkpoint and retain the idle process when possible.
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
        let resume_at = kept
            .and_then(|kept| ledger[kept].resume_point())
            .unwrap_or(after.as_str());
        let first_discarded = kept.and_then(|kept| ledger.get(kept + 1));
        // The control removes its target, while the launch flag keeps its target. Only use the
        // control when the first discarded prompt is known to follow the retained entry.
        let native = match (first_discarded, ledger.last()) {
            (Some(first), Some(last)) if first.follows.as_deref() == Some(resume_at) => {
                let last_seen = last.rewind_note.as_deref().unwrap_or(&last.last_input);
                rewind_live(&session, &first.prompt, last_seen).await?
            }
            _ => LiveRewind::ResumeRequired,
        };
        let rewind_note = match native {
            LiveRewind::Applied => None,
            LiveRewind::ResumeRequired => {
                // The launch guard checks that a single discarded turn owns its whole suffix.
                let guarded_turn = first_discarded
                    .filter(|turn| turn.rewind_note.is_none() && !turn.absorbed_steering)
                    .filter(|_| kept.is_some_and(|kept| kept + 2 == ledger.len()))
                    .map(|turn| turn.prompt.clone());
                Some(rewind_by_resume(&session, resume_at.to_owned(), guarded_turn).await?)
            }
        };
        match kept {
            Some(kept) => {
                ledger.truncate(kept + 1);
                if let Some(rewind_note) = rewind_note {
                    ledger[kept].rewind_note = Some(rewind_note);
                }
            }
            None => ledger.clear(),
        }
        *session
            .fork_point
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(after.as_str().to_owned());
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
        // The steered turn's interrupt bounds the steer's delivery as it bounds the prompt's.
        let cancellation = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .as_ref()
            .filter(|active| {
                request.scope.thread_id == session.owner && active.scope == request.scope
            })
            .map(|active| active.cancellation.clone())
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnSteer,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        // A steered input carries its own elements, such as a question response, and no
        // standing context.
        let content = super::conversation::input_blocks(
            [
                forge_provider_api::forge::input_block(&request.input),
                user_text(&request.input),
            ]
            .into_iter()
            .filter(|text| !text.is_empty())
            .collect(),
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
        // Context-only steering has no submission for Harness to match, so only journaled
        // steering waits for the CLI's `started` report.
        if let Some(submission_id) = &submission_id {
            session.state.record_steering(
                &uuid,
                submission_id.clone(),
                request.scope.clone(),
                user_text(&request.input),
            );
        }
        let session_id = session.conversation.id().to_owned();
        let delivered = deliver_input(
            &session,
            ProviderOperation::TurnSteer,
            UserInput::composed(uuid.clone(), content),
            &session_id,
            receipt.as_ref(),
            DeliveryTurn {
                dispatch: None,
                cancellation: &cancellation,
            },
            &mut observer,
        )
        .await;
        if let Err(error) = delivered {
            session.state.forget_command(&uuid).await;
            // A written steer the CLI never acknowledged leaves its consumption unknown.
            if matches!(&error, ProviderError::Failure(failure) if failure.disposition() == FailureDisposition::Invalidated)
            {
                session.invalidate(error.clone()).await;
            }
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
        match tokio::time::timeout(
            INTERRUPT_SETTLEMENT_GRACE,
            session.settlement(&request.scope),
        )
        .await
        {
            Ok(settlement) => crate::turn_runner::interrupt_outcome(settlement),
            Err(_) => {
                // Every wait the interrupt reaches is bounded, so a turn still unsettled here is
                // held by something the adapter cannot name. Invalidating the binding fails the
                // stop, and Harness restarts the provider instead of staying `Stopping`.
                tracing::error!(
                    grace_ms = INTERRUPT_SETTLEMENT_GRACE.as_millis() as u64,
                    "Claude turn did not settle after its interrupt"
                );
                session.invalidate(unanswered_interrupt()).await;
                Err(unanswered_interrupt())
            }
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

/// Recover a stopped input and require its saved active branch to prove the cut.
async fn reconcile_stopped_input(
    conversation: &ClaudeConversation,
    journal: &RepairJournal,
    operation: ProviderOperation,
) -> Result<(), ProviderError> {
    let report = |error| super::recovery::failure(operation, error);
    let Some(intent) = journal.load().map_err(report)? else {
        return Ok(());
    };
    let RepairStatus::Cut { target, last_input } = journal.status(&intent).map_err(report)? else {
        return journal.clear().map_err(report);
    };
    let result = conversation
        .request(
            operation,
            OutboundControl::RewindConversation {
                target_message_uuid: target.to_string(),
                last_seen_user_message_uuid: last_input.to_string(),
            },
        )
        .await?;
    let response: RewindResponse =
        serde_json::from_value(result).map_err(|error| report(error.into()))?;
    if !response.rewound
        || response.target_message_uuid.as_deref() != Some(target.to_string().as_str())
    {
        return Err(report(anyhow::anyhow!(
            "Claude refused stopped-input repair: {}",
            response.error.unwrap_or_default()
        )));
    }
    // Claude acknowledges an in-memory cut before its transcript writer flushes the branch.
    let persisted = tokio::time::timeout(std::time::Duration::from_secs(3), async {
        loop {
            match journal.status(&intent) {
                Ok(RepairStatus::Clean) => return journal.clear().map_err(report),
                Ok(RepairStatus::Cut {
                    target: pending, ..
                }) if pending == target => {}
                Ok(RepairStatus::Cut { .. }) => {
                    return Err(report(anyhow::anyhow!(
                        "Claude changed the pending repair boundary"
                    )));
                }
                Err(error) => return Err(report(error)),
            }
            tokio::time::sleep(std::time::Duration::from_millis(25)).await;
        }
    })
    .await;
    persisted.map_err(|_| report(anyhow::anyhow!("Claude did not save the repaired branch")))?
}

/// Apply an exclusive cut on the live CLI and require it to confirm the requested target.
async fn rewind_live(
    session: &ClaudeBinding,
    first_discarded: &str,
    last_seen: &str,
) -> Result<LiveRewind, ProviderError> {
    const OPERATION: ProviderOperation = ProviderOperation::ThreadRewind;
    let confirmed = session
        .conversation
        .request(
            OPERATION,
            OutboundControl::RewindConversation {
                target_message_uuid: first_discarded.to_owned(),
                last_seen_user_message_uuid: last_seen.to_owned(),
            },
        )
        .await;
    let response = confirmed.and_then(|response| {
        serde_json::from_value::<RewindResponse>(response).map_err(|_| {
            ProviderFailure::new(OPERATION, ProviderFailureKind::Protocol)
                .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain)
                .into()
        })
    });
    let cause = match response {
        Ok(response) if response.rewound => {
            if response.target_message_uuid.as_deref() == Some(first_discarded) {
                return Ok(LiveRewind::Applied);
            }
            ProviderFailure::new(OPERATION, ProviderFailureKind::Protocol)
                .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain)
                .into()
        }
        Ok(response) if matches!(response.reason, Some(RewindRefusal::TargetNotFound)) => {
            return Ok(LiveRewind::ResumeRequired);
        }
        Ok(response) => {
            let disposition = match response.reason {
                Some(RewindRefusal::PersistFailed | RewindRefusal::StateChanged) => {
                    FailureDisposition::Invalidated
                }
                _ => FailureDisposition::Unchanged,
            };
            let cause = ProviderError::Failure(
                ProviderFailure::new(OPERATION, ProviderFailureKind::Rejected)
                    .at_phase(ProviderPhase::Observation, disposition),
            );
            let cause = match response
                .error
                .and_then(|reason| forge_provider_api::SanitizedMessage::new(reason).ok())
            {
                Some(explanation) => ProviderError::Explained {
                    cause: Box::new(cause),
                    explanation,
                },
                None => cause,
            };
            if disposition == FailureDisposition::Invalidated {
                session.invalidate(cause.clone()).await;
            }
            return Err(cause);
        }
        Err(error) => error,
    };
    if matches!(&cause, ProviderError::Failure(failure) if failure.disposition() == FailureDisposition::Unchanged)
    {
        return Err(cause);
    }
    let cause = operation_failure(
        OPERATION,
        &cause,
        ProviderPhase::Observation,
        FailureDisposition::Invalidated,
    );
    session.invalidate(cause.clone()).await;
    Err(cause)
}

/// Keep a named transcript entry by resuming at it, then persist and select the new branch.
async fn rewind_by_resume(
    session: &ClaudeBinding,
    resume_at: String,
    drops_turn: Option<String>,
) -> Result<String, ProviderError> {
    const OPERATION: ProviderOperation = ProviderOperation::ThreadRewind;
    let session_id = session.conversation.id().to_owned();
    tracing::info!(target: "forge::provider", event_name = "provider.rewind.requested",
        provider = "claude", session_id = %session_id, resume_at = %resume_at,
        drops_turn = ?drops_turn);
    let rewound = session
        .conversation
        .restart(
            SessionStart::Resume {
                session_id: session_id.clone(),
                resume_at: Some(resume_at),
                drops_turn,
            },
            &session.cleanup,
        )
        .await;
    let persisted = match rewound {
        Ok(()) => append_note(
            session,
            "[Forge: the user rolled the conversation back to this point; later turns were discarded]",
            OPERATION,
        )
        .await,
        Err(error) => Err(error),
    };
    let note = match persisted {
        Ok(note) => note,
        Err(error) => {
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
                    ProviderFailure::new(OPERATION, ProviderFailureKind::Rejected)
                        .at_phase(ProviderPhase::Observation, FailureDisposition::Unchanged),
                ),
                Err(restore) => {
                    let cause = with_cleanup(
                        operation_failure(
                            OPERATION,
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
    };
    commit_truncation(session, OPERATION).await?;
    Ok(note)
}

/// Make a truncating restart the branch every later CLI process loads.
///
/// A CLI process that exits records its newest entry as the transcript's leaf checkpoint, and a
/// later resume or fork loads the chain of the newest checkpoint unless the transcript's newest
/// entry follows it. The process that a truncating restart replaces records the discarded branch
/// as it exits, and the restarted process records nothing until it exits in turn, so until then
/// the CLI loads the discarded branch: a fork at an answer given after the truncation is refused
/// because that answer is not in the loaded chain, and a plain resume brings the discarded
/// entries back. Ending the restarted process right after its note records the note, which every
/// later entry follows.
///
/// The caller holds the binding's operation lock while the CLI is idle. A failed restart leaves
/// the binding without a process, so it invalidates the binding.
async fn commit_truncation(
    session: &ClaudeBinding,
    operation: ProviderOperation,
) -> Result<(), ProviderError> {
    let restarted = session
        .conversation
        .restart(
            SessionStart::Resume {
                session_id: session.conversation.id().to_owned(),
                resume_at: None,
                drops_turn: None,
            },
            &session.cleanup,
        )
        .await;
    if let Err(error) = restarted {
        let cause = operation_failure(
            operation,
            &error,
            ProviderPhase::Observation,
            FailureDisposition::Invalidated,
        );
        session.invalidate(cause.clone()).await;
        return Err(cause);
    }
    Ok(())
}

/// Append a transcript entry that starts no model request, and return its chain entry uuid.
async fn append_note(
    session: &ClaudeBinding,
    text: &str,
    operation: ProviderOperation,
) -> Result<String, ProviderError> {
    let note = UserInput::note(uuid::Uuid::new_v4().to_string(), text);
    let mut observer = session.state.track_command(&note.uuid, false).await?;
    session.conversation.write(&note).await?;
    wait_command(
        &mut observer,
        |state| state == CommandState::Completed,
        operation,
    )
    .await
    .map(|_| note.uuid)
}

/// Name the chain entry the next input follows, while this process knows it.
///
/// `fork_point` is cleared whenever the CLI appended entries the adapter cannot name, such as a
/// compaction or a turn that reported no checkpoint; otherwise the newest ledger turn's resume
/// point is the newest entry of the kept history.
async fn history_tip(session: &ClaudeBinding) -> Option<String> {
    if session
        .fork_point
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .is_none()
    {
        return None;
    }
    session
        .ledger
        .lock()
        .await
        .last()
        .and_then(LedgerTurn::resume_point)
        .map(ToOwned::to_owned)
}

/// The note that makes the removal of an unanswered request durable.
const REMOVED_REQUEST_NOTE: &str =
    "[Forge: a request the user stopped before it was answered was removed here]";

/// The note that withdraws an unanswered request no resume can remove.
const WITHDRAWN_REQUEST_NOTE: &str = "[Forge: the user stopped the request above before it was \
     answered and withdrew it; respond only to the messages that follow]";

/// Remove an interrupted request the CLI never answered before the conversation continues.
///
/// The CLI keeps an interrupted prompt, and its `[Request interrupted by user]` marker, as user
/// entries, and the API joins consecutive user entries into one message. The next prompt then
/// reached the model in the same message as the unanswered request, and the model carried out
/// the cancelled request instead of the new one. A restart that resumes at the last entry before
/// the request drops it, and a note makes the truncation durable: the turn's own checkpoint when
/// the turn answered earlier input, as when a steer started a follow-up CLI turn after the
/// answer, or otherwise the entry the prompt followed, when the binding knows it. Otherwise, or
/// when the CLI refuses that resume, a note withdraws the request.
///
/// The removal also keeps the ledger's positions true: a later guarded rewind that drops the next
/// turn discards exactly that turn's entries, which the CLI refuses when the request and its
/// marker lie in the discarded range.
///
/// The caller holds the binding's operation lock while the CLI is idle.
async fn withdraw_unanswered_request(
    session: &ClaudeBinding,
    operation: ProviderOperation,
) -> Result<(), ProviderError> {
    if let Some(journal) = session.repair.as_ref() {
        reconcile_stopped_input(&session.conversation, journal, operation).await?;
    }
    let mut ledger = session.ledger.lock().await;
    let Some(unanswered) = ledger.last().filter(|turn| turn.unanswered) else {
        return Ok(());
    };
    // The interrupt marker is a user entry, which a guarded resume refuses to discard, so the
    // resume is unguarded; the range holds only the request and the marker, because the
    // interrupt cancelled every queued steer.
    let newest = ledger.len() - 1;
    let resume = match unanswered.checkpoint.clone() {
        Some(checkpoint) => Some((newest, checkpoint)),
        None => newest
            .checked_sub(1)
            .zip(unanswered.follows.clone())
            .filter(|(kept, follows)| ledger[*kept].resume_point() == Some(follows.as_str())),
    };
    let session_id = session.conversation.id().to_owned();
    if let Some((kept, resume_at)) = resume {
        let resumed = session
            .conversation
            .restart(
                SessionStart::Resume {
                    session_id: session_id.clone(),
                    resume_at: Some(resume_at),
                    drops_turn: None,
                },
                &session.cleanup,
            )
            .await;
        let noted = match resumed {
            Ok(()) => append_note(session, REMOVED_REQUEST_NOTE, operation).await,
            Err(error) => Err(error),
        };
        match noted {
            Ok(note) => {
                commit_truncation(session, operation).await?;
                ledger.truncate(kept + 1);
                ledger[kept].rewind_note = Some(note);
                ledger[kept].unanswered = false;
                ledger[kept].checkpoint.clone_into(
                    &mut session
                        .fork_point
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner),
                );
                return Ok(());
            }
            Err(error) => {
                // A refused or unpersisted resume leaves the transcript unchanged.
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
                if let Err(restore) = restored {
                    let cause = with_cleanup(
                        operation_failure(
                            operation,
                            &error,
                            ProviderPhase::Observation,
                            FailureDisposition::Invalidated,
                        ),
                        vec![restore],
                    );
                    session.invalidate(cause.clone()).await;
                    return Err(cause);
                }
            }
        }
    }
    let note = append_note(session, WITHDRAWN_REQUEST_NOTE, operation).await?;
    // The ledger lock is held throughout, so the unanswered turn is still the newest one.
    let Some(withdrawn) = ledger.last_mut() else {
        return Ok(());
    };
    withdrawn.rewind_note = Some(note);
    withdrawn.unanswered = false;
    Ok(())
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

    /// Kill the binding's CLI process tree, which serves this binding alone.
    ///
    /// Ending the process ends its output and every tool it could run, so the guarantee is a
    /// process-level one. The transcript keeps what the CLI had flushed before the kill.
    async fn thread_terminate(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        context.check_admission(ProviderOperation::ThreadClose)?;
        let Ok(session) = self.binding(binding, ProviderOperation::ThreadClose) else {
            return Ok(());
        };
        session.state.close_admission();
        drop(self.pending_inputs.fence_binding(binding));
        if let Some(active) = session
            .active
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .as_mut()
        {
            active.cancelled = true;
            active.cancellation.cancel();
        }
        session.cleanup.terminate().await?;
        session.state.mark_collected();
        session.history_released.store(true, Ordering::Release);
        let mut owners = self.owners.lock().await;
        if owners.get(&session.owner) == Some(binding) {
            owners.remove(&session.owner);
        }
        Ok(())
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

/// Describe a CLI that stayed silent through the interrupt grace period.
///
/// The binding cannot prove what the CLI consumed, so the failure invalidates it.
fn unanswered_interrupt() -> ProviderError {
    ProviderFailure::new(
        ProviderOperation::TurnInterrupt,
        ProviderFailureKind::Transport,
    )
    .at_phase(ProviderPhase::Observation, FailureDisposition::Invalidated)
    .into()
}

/// The turn one written input belongs to, as its delivery sees it.
struct DeliveryTurn<'a> {
    /// Reports that the turn's prompt was written; a steer joins a turn that already reported.
    dispatch: Option<&'a forge_provider_api::submission::DispatchReporter>,
    /// Fires when the turn is interrupted.
    cancellation: &'a tokio_util::sync::CancellationToken,
}

/// Write one user input and commit its delivery evidence through the host receipt.
///
/// The CLI reports `command_lifecycle queued` once it owns the input, which is the acceptance
/// evidence; the write alone proves nothing. When the turn's cancellation fires while the CLI has
/// not acknowledged the written input, `INTERRUPT_GRACE` bounds the rest of that wait. A steer
/// needs the bound as much as a prompt: the turn ends only after every steer's delivery settles,
/// so an unanswered steer would otherwise hold the interrupted turn open.
async fn deliver_input(
    session: &ClaudeBinding,
    operation: ProviderOperation,
    input: UserInput,
    session_id: &str,
    receipt: Option<&Arc<dyn forge_provider_api::submission::InputReceipt>>,
    turn: DeliveryTurn<'_>,
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
        if let Some(dispatch) = turn.dispatch {
            dispatch.confirm();
        }
        let acknowledged = wait_command(observer, |state| state != CommandState::Other, operation);
        tokio::pin!(acknowledged);
        tokio::select! {
            biased;
            state = &mut acknowledged => state?,
            // The input is written, so only the CLI's silence keeps this wait open. A CLI that
            // stays silent for the interrupt grace period is wedged; the caller invalidates the
            // binding on this failure so Harness can restart it.
            _ = turn.cancellation.cancelled() => {
                tokio::time::timeout(INTERRUPT_GRACE, acknowledged)
                    .await
                    .map_err(|_| unanswered_interrupt())??
            }
        };
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
        if let Err(error) =
            withdraw_unanswered_request(&session, ProviderOperation::TurnStart).await
        {
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
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
        if !session.state.instructions_initialized().await {
            let workspace = session.conversation.workspace().await;
            let paths = session
                .conversation
                .instruction_sources(&cancellation)
                .await;
            session.state.initialize_instructions(paths, &workspace).await;
        }
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
        let follows = history_tip(&session).await;
        let mut observer = session.state.track_command(&prompt_uuid, true).await?;
        let session_id = session.conversation.id().to_owned();
        let receipt = request.receipt.clone();
        if let Err(error) = deliver_input(
            &session,
            ProviderOperation::TurnStart,
            input,
            &session_id,
            Some(&receipt),
            DeliveryTurn {
                dispatch: Some(&request.dispatch),
                cancellation: &cancellation,
            },
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
                if let Some(journal) = session.repair.as_ref() {
                    let _steering = session.steering.write().await;
                    let commands = session.state.interrupt_commands(&request.scope).await;
                    if !commands.is_empty() {
                        let intent = RepairIntent {
                            scope: request.scope.clone(),
                            unanswered: false,
                            inputs: commands.iter().map(|command| uuid::Uuid::parse_str(command))
                                .collect::<Result<_, _>>().map_err(|error| super::recovery::failure(ProviderOperation::TurnInterrupt, error.into()))?,
                            retained: follows.as_deref().and_then(|id| uuid::Uuid::parse_str(id).ok()),
                        };
                        journal.save(&intent).map_err(|error| super::recovery::failure(ProviderOperation::TurnInterrupt, error))?;
                    }
                }
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
                        session.invalidate(unanswered_interrupt()).await;
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
        let repaired = if let Some(journal) = session.repair.as_ref()
            && let Some(mut intent) = journal.load().map_err(|error| super::recovery::failure(ProviderOperation::TurnInterrupt, error))?
        {
            if end.outcome == forge_provider_api::turn::TurnOutcome::Interrupted && end.unanswered {
                intent.unanswered = true;
                if let Some(checkpoint) = end.checkpoint.as_deref() {
                    intent.retained = uuid::Uuid::parse_str(checkpoint).ok();
                }
                journal.save(&intent).map_err(|error| super::recovery::failure(ProviderOperation::TurnInterrupt, error))?;
                if let Err(error) = reconcile_stopped_input(
                    &session.conversation, journal, ProviderOperation::TurnInterrupt,
                ).await {
                    session.invalidate(error.clone()).await;
                    return Err(error);
                }
                true
            } else {
                journal.clear().map_err(|error| super::recovery::failure(ProviderOperation::TurnInterrupt, error))?;
                false
            }
        } else {
            false
        };
        if end.outcome == forge_provider_api::turn::TurnOutcome::Interrupted
            && !session.state.execution_started().await
        {
            session.state.finish(end.outcome).await?;
            // The turn produced nothing, but a prompt the CLI started is a user entry in its
            // history, which the next prompt must not follow.
            if end.unanswered {
                session.conversation.observe_turn();
                if !repaired {
                    record_ledger_turn(&session, &end, follows, true).await;
                }
            }
            return Err(ProviderError::Cancelled);
        }
        session.conversation.observe_turn();
        session.state.observe_native_start(end.completed_at_ms).await?;
        // The `result` already ended the turn, so its reply is classified before the optional
        // context query, which an interrupt or its deadline abandons.
        session.state.finish(end.outcome).await?;
        if !session.conversation.is_closed().await
            && let Some(usage) = session.conversation.context_usage(&cancellation).await
        {
            session.state.publish_context_usage(usage).await?;
        }
        let _ = session.state.take_completion().await;
        let unanswered = end.outcome == forge_provider_api::turn::TurnOutcome::Interrupted
            && (end.unanswered || end.checkpoint.is_none());
        if !repaired || end.checkpoint.is_some() {
            record_ledger_turn(&session, &end, follows, unanswered && !repaired).await;
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

/// Record a finished turn's place in the binding's native history.
///
/// `unanswered` marks a turn that left a request the CLI recorded without an answer, which
/// `withdraw_unanswered_request` removes before the conversation continues.
async fn record_ledger_turn(
    session: &ClaudeBinding,
    end: &super::session::TurnEnd,
    follows: Option<String>,
    unanswered: bool,
) {
    let Some((prompt, last_input)) = end.commands.first().zip(end.commands.last()) else {
        return;
    };
    session.ledger.lock().await.push(LedgerTurn {
        prompt: prompt.clone(),
        last_input: last_input.clone(),
        checkpoint: end.checkpoint.clone(),
        rewind_note: None,
        absorbed_steering: end.commands.len() > 1,
        follows,
        unanswered,
    });
    end.checkpoint.clone_into(
        &mut session
            .fork_point
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner),
    );
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
    }
}

/// Render the user's own words of one turn input, or nothing for a Harness-started turn.
fn user_text(input: &forge_provider_api::forge::TurnInput) -> String {
    input.user_input().map(prompt_input).unwrap_or_default()
}

/// Report whether the input is exactly the native `/compact` command.
fn is_compact_command(request: &PromptRequest) -> bool {
    request.recovered_inputs.is_empty()
        && request.attachments.is_empty()
        && matches!(
            request.input.kind(),
            forge_provider_api::forge::TurnKind::UserMessage {
                input: forge_provider_api::PromptInput::Text(text)
            } if text == "/compact"
        )
}

/// Order the prompt's text parts: the turn's `<forge>` block, each earlier input after its own
/// marker block, then the user's own words.
///
/// Claude has no mid-conversation developer channel, so the block starts the user message, as
/// the open-time system prompt explains. Empty parts are left out.
pub(super) fn prompt_texts(request: &PromptRequest) -> Vec<String> {
    let mut texts = vec![request.user_message_block()];
    for (index, recovered) in request.recovered_inputs.iter().enumerate() {
        texts.push(recovered.block(index + 1));
        for attachment in &recovered.attachments {
            texts.push(attachment.context.clone());
        }
        texts.push(user_text(&recovered.input));
    }
    texts.push(user_text(&request.input));
    texts.retain(|text| !text.is_empty());
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
        // The CLI's own statement of a refusal is what the user acts on, so it survives the
        // reclassification.
        ProviderError::Explained { cause, explanation } => {
            return ProviderError::Explained {
                cause: Box::new(operation_failure(operation, cause, phase, disposition)),
                explanation: explanation.clone(),
            };
        }
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
        ProviderError::Explained { cause, explanation } => ProviderError::Explained {
            cause: Box::new(with_operation(operation, *cause)),
            explanation,
        },
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
