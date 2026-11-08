//! Advance the session's exchange owner through update steps: intake, admission, effects, and wakes.

use std::{
    collections::{HashMap, VecDeque},
    sync::Arc,
};

use anyhow::{Context, Result};
use forge_protocol::outbound::MessageSender;
use futures_util::{FutureExt, StreamExt};
use tokio::sync::{Mutex, mpsc, oneshot};
use tokio::task::JoinSet;

use super::active::{ActiveCommand, CommandEvent};
use super::command::InputControl;
use super::command::{
    Command, CommandFailure, CommandId, CommandOutcome, CommandRequest, ConfigurationRejection,
    ExecuteCommandError, InputRevision,
};
use super::failure::OperationFailure;
use super::foreground_publication::{forward_signal, run_foreground_job};
use super::input_command::{InputCommandFailure, InputCommandRun, PreparedInput, apply_input};
use super::input_delivery::{
    InputDeliveryEvent, InputDeliveryOffer, InputDeliveryProgress, InputDeliveryRun,
    InputForegroundSource, RetainedInput, SteeringDeliveryOffer, SteeringDispatchTarget,
};
use super::input_readiness::{InputOfferError, InputReadiness};
#[cfg(test)]
use super::input_receipts::InputReceiptError;
use super::input_receipts::{InputReceiptFact, InputReceiptWake, InputReceipts};
use super::journal::CommandJournalError;
use super::native_command::NativeCommandProgress;
use super::queue::{CommandAdmission, CommandQueue};
use super::rollback_run::{RollbackProgress, RollbackRecovery};
use super::services::OwnerServices;
use super::task_command::TaskCommandProgress;
use super::task_input::TaskInputProduction;
use super::transition::{
    RetainedTransition, TransitionFailure, TransitionOutcome, TransitionProgress, TurnStatus,
};
use crate::broker::{
    DispatchResult, ForegroundAction, ForegroundJob, ForegroundSignal, HarnessBroker,
    TurnTransaction,
};
use crate::trace::TraceStore;

mod admission;
mod check_admission;
mod execution_state;
mod handlers;
mod intake;
mod state;
mod step;
mod wake;
use super::owner::{OwnerEvent, QuestionUpdatePermit};
use execution_state::ExecutionState;
use state::{DriverEnv, DriverState, FailedWork, RunningTasks};
use step::StepGate;
use wake::{WakeEnd, WakeSelection};

const OWNER_INTAKE_BUDGET: usize = 32;
pub(super) const OWNER_QUEUE_CAPACITY: usize = forge_protocol::MAX_PENDING_FRAMES;

/// Carry the current command and steering gate for one owner intake decision.
struct OwnerIntake<'a> {
    active: Option<&'a mut ActiveCommand>,
    steering: SteeringAdmission,
}

/// Keep one owner update's services and diagnostic identity together across helpers.
#[derive(Clone, Copy)]
struct OwnerUpdateContext<'a> {
    services: &'a OwnerServices,
    trace: &'a TraceStore,
    session: &'a str,
    step: u64,
}

/// Hold the unique receiver and advance command and foreground state through update steps.
pub(crate) struct ExchangeDriver {
    receiver: mpsc::Receiver<OwnerEvent>,
    child_sender: Arc<std::sync::OnceLock<crate::agent::AgentCommandSender>>,
    shutdown_retry: Arc<tokio::sync::Notify>,
    commands: CommandQueue,
    execution_state: ExecutionState,
    configuration_attempt: Option<ConfigurationAttempt>,
    deferred_configuration: Option<DeferredConfiguration>,
    input_receipts: InputReceipts,
    input_delivery: Option<InputDeliveryRun>,
    question_inputs: super::question_input::QuestionInputs,
    question_waits: super::question_wait::QuestionWaits,
    question_tools: VecDeque<super::question_tool::QuestionTool>,
    pending_approval_timeout: Option<u64>,
    observation_recovery: super::observation_recovery::ObservationRecovery,
    steering: HashMap<super::command::InputId, InputDeliveryRun>,
    steering_order: Arc<tokio::sync::Semaphore>,
    task_check_requests: Vec<oneshot::Sender<Result<Option<InputReadiness>, InputOfferError>>>,
    reconnect_requests: VecDeque<PendingReconnect>,
}

/// Preserve the actual source boundary captured before an immutable configuration attempt.
struct ConfigurationAttempt {
    command: CommandId,
    availability: Option<crate::provider::session_execution::NativeAvailabilityReceipt>,
}

/// Retain an unchanged configuration intent until later actual availability can be revalidated.
struct DeferredConfiguration {
    patch: super::command::SessionConfigurationPatch,
    after: Option<crate::provider::session_execution::NativeAvailabilityReceipt>,
}

struct PendingReconnect {
    cursor: Option<super::publication::PublicationSequence>,
    limit: std::num::NonZeroU16,
    reply: oneshot::Sender<Result<super::ReconnectSnapshot, super::ReconnectError>>,
}

impl ExchangeDriver {
    /// Create the driver for an owner whose intake channel and command journal are already open.
    pub(super) fn new(
        receiver: mpsc::Receiver<OwnerEvent>,
        child_sender: Arc<std::sync::OnceLock<crate::agent::AgentCommandSender>>,
        shutdown_retry: Arc<tokio::sync::Notify>,
        commands: CommandQueue,
    ) -> Self {
        Self {
            receiver,
            child_sender,
            shutdown_retry,
            commands,
            execution_state: ExecutionState::Ready,
            configuration_attempt: None,
            deferred_configuration: None,
            input_receipts: InputReceipts::default(),
            input_delivery: None,
            question_inputs: super::question_input::QuestionInputs::default(),
            question_waits: super::question_wait::QuestionWaits::default(),
            question_tools: VecDeque::new(),
            pending_approval_timeout: None,
            observation_recovery: Default::default(),
            steering: HashMap::new(),
            steering_order: Arc::new(tokio::sync::Semaphore::new(1)),
            task_check_requests: Vec::new(),
            reconnect_requests: VecDeque::new(),
        }
    }

    /// Get the active turn's status as stops and commands see it.
    ///
    /// This is the only definition of whether turn work exists, so every caller agrees on when
    /// a stop may treat the owner as idle.
    fn turn_status(&self, state: &DriverState) -> TurnStatus {
        let scope = match state
            .foreground
            .as_ref()
            .map_or(Ok(None), |runtime| runtime.transaction.invocation_scope())
        {
            Ok(scope) => scope,
            Err(_) => return TurnStatus::Blocked,
        };
        let has_work = state.foreground.is_some()
            || !state.pending.is_empty()
            || !self.steering.is_empty()
            || !state.tasks.input_stops.is_empty()
            || !state.check_runs.is_empty()
            || state.task_verification.is_some();
        match self.execution_state {
            ExecutionState::Ready if has_work => {
                scope.map_or(TurnStatus::Local, TurnStatus::Running)
            }
            // A failed owner admits no new work, so once its last work ends nothing can run, and
            // shutdown must see it idle or its stop never finishes.
            ExecutionState::Ready | ExecutionState::Failed if !has_work => TurnStatus::Idle,
            ExecutionState::Ready
            | ExecutionState::Stopping
            | ExecutionState::Restarting
            | ExecutionState::Failed => TurnStatus::Blocked,
        }
    }

    /// Serialize snapshot leases after earlier output offers settle, without blocking the owner.
    async fn advance_reconnect(
        &mut self,
        broker: &Mutex<HarnessBroker>,
        context: OwnerUpdateContext<'_>,
    ) {
        let OwnerUpdateContext {
            services,
            trace,
            session,
            step,
        } = context;
        while let Some(request) = self.reconnect_requests.pop_front() {
            if request.reply.is_closed() {
                continue;
            }
            if self.receiver.is_closed() {
                let _ = request.reply.send(Err(super::ReconnectError::OwnerClosed));
                continue;
            }
            // A running turn may hold the broker for its whole duration. Reconnect then reads the
            // committed state with the last published runtime metadata instead of waiting.
            let result = super::reconnect::capture(
                || match broker.try_lock() {
                    Ok(broker) => broker.snapshot(),
                    Err(_) => services.observer.snapshot(),
                },
                services,
                request.cursor,
                request.limit,
                step,
                !self.commands.storage_unavailable()
                    && matches!(self.execution_state, ExecutionState::Ready),
            );
            if matches!(
                &result,
                Err(super::ReconnectError::Publication(
                    super::publication::PublicationError::DeliveryInProgress
                ))
            ) {
                self.reconnect_requests.push_front(request);
                return;
            }
            let cursor = request.cursor;
            let observed = request.reply.send(result).is_ok();
            trace.record(
                session,
                "exchange.reconnect.snapshot",
                serde_json::json!({
                    "update_step":step,"observed":observed,"after":cursor,
                }),
            );
            return;
        }
    }

    /// Drive command and foreground transitions for the complete session lifetime.
    pub(crate) async fn run(
        mut self,
        broker: Arc<Mutex<HarnessBroker>>,
        trace: Arc<TraceStore>,
        session_id: String,
        services: OwnerServices,
    ) {
        let env = DriverEnv {
            broker,
            trace,
            session_id,
            services,
        };
        let DriverEnv {
            trace,
            session_id,
            services,
            ..
        } = &env;
        self.child_sender.get_or_init(|| services.children.sender());
        self.commands
            .bind_publications(services.publications.clone());
        let (check_sender, check_events) = crate::checks::owner::CheckOwnerSender::channel();
        let mut state = DriverState::new(services, check_events);
        let mut closed = false;
        let mut step = 0_u64;
        let mut continuing_event_phase = false;
        let mut event_budget = OWNER_INTAKE_BUDGET;
        let mut command_at_step_entry = false;
        if let Err(error) = services.checks.install_owner(check_sender) {
            self.execution_fail(services);
            trace.record(
                session_id,
                "exchange.check.owner_unavailable",
                serde_json::json!({"reason":error.to_string()}),
            );
        }
        if let Err(error) = services.publications.recover_interrupted_offers() {
            self.execution_fail(services);
            trace.record(
                session_id,
                "exchange.publication.recovery_failed",
                serde_json::json!({"update_step":step,"reason":error.to_string()}),
            );
        }
        match self.commands.restore_task_delivery(services) {
            Ok(Some(update)) => {
                trace.record(
                    session_id,
                    "exchange.task_input.recovery_paused",
                    serde_json::json!({
                        "update_step":step,"command_id":update.operation_id,
                    }),
                );
                publish_input_observation(update, None, services, trace, session_id, step);
            }
            Ok(None) => {}
            Err(error) => {
                self.execution_fail(services);
                trace.record(
                    session_id,
                    "exchange.task_input.recovery_failed",
                    serde_json::json!({
                        "update_step":step,"reason":error.to_string(),
                    }),
                );
            }
        }
        match self.input_receipts.restore(services) {
            Ok(updates) => {
                for update in updates {
                    publish_input_observation(update, None, services, trace, session_id, step);
                }
            }
            Err(error) => {
                self.execution_fail(services);
                trace.record(
                    session_id,
                    "exchange.input_receipt.recovery_failed",
                    serde_json::json!({
                        "update_step":step,"reason":error.to_string(),
                    }),
                );
            }
        }
        loop {
            if !continuing_event_phase {
                step = step.checked_add(1).expect("exchange update step exhausted");
                event_budget = OWNER_INTAKE_BUDGET;
                command_at_step_entry = state.command.is_some();
            }
            self.step_begin(&env, &mut state, step, closed);
            let foreground_at_entry = state.foreground.is_some();
            self.events_receive(&env, &mut state, step, continuing_event_phase, &mut closed)
                .await;
            self.publications_flush(&env, &mut state, step).await;
            self.storage_failure_apply(&env, &mut state, step, closed)
                .await;
            self.deliveries_advance(&env, &mut state, step, closed)
                .await;
            let (ready_wake, event_status) =
                self.ready_wake_poll(&env, &mut state, closed, event_budget);
            let mut broker_deferred = false;
            continuing_event_phase = ready_wake.is_some();
            if continuing_event_phase {
                event_budget = event_budget.saturating_sub(1);
            }
            if ready_wake.is_none() && state.deferred_publications.updates.is_empty() {
                self.restart_advance(&env, &mut state, step, closed).await;
                self.command_start(
                    &env,
                    &mut state,
                    step,
                    closed,
                    command_at_step_entry,
                    &event_status,
                )
                .await;
                self.question_tools_admit(&env, &mut state, step, closed)
                    .await;
                self.checks_admit(&env, &mut state, step, closed, &mut broker_deferred)
                    .await;
                self.automatic_checks_advance(&env, &mut state, step, closed, &mut broker_deferred)
                    .await;
                self.deferred_configuration_retry(
                    &env,
                    &mut state,
                    step,
                    closed,
                    &mut broker_deferred,
                )
                .await;
                self.turns_advance(&env, &mut state, step, closed, foreground_at_entry)
                    .await;
            }
            match self.step_finish_check(&env, &mut state, step, closed, &ready_wake) {
                StepGate::Proceed => {}
                StepGate::Restart => continue,
                StepGate::Exit => return,
            }
            let (wake, foreground_status) = match self
                .wake_next(&env, &mut state, step, closed, broker_deferred, ready_wake)
                .await
            {
                WakeSelection::Apply {
                    wake,
                    foreground_status,
                } => (*wake, foreground_status),
                WakeSelection::Deferred => {
                    continuing_event_phase = false;
                    continue;
                }
            };
            let command_wake = matches!(&wake, DriverWake::Command(_));
            let wake_end = self
                .wake_apply(
                    &env,
                    &mut state,
                    step,
                    &foreground_status,
                    &mut closed,
                    wake,
                )
                .await;
            if matches!(wake_end, WakeEnd::RestartStep) {
                continue;
            }
            if command_wake {
                services.input.changed.notify_one();
            }
            tokio::task::yield_now().await;
        }
    }

    /// Bound tool callbacks independently of caller command admission and preserve every rejected response.
    fn stage_question_tool(&mut self, request: super::question_tool::QuestionTool) {
        if self.question_tools.len() >= OWNER_QUEUE_CAPACITY {
            request.reject();
        } else {
            self.question_tools.push_back(request);
        }
    }

    /// Restore a pending delivery to the same root or steering owner that produced its event.
    fn restore_delivery(&mut self, target: &InputDeliveryTarget, run: InputDeliveryRun) {
        match target {
            InputDeliveryTarget::Foreground => self.input_delivery = Some(run),
            InputDeliveryTarget::Steering(id) => {
                self.steering.insert(id.clone(), run);
            }
        }
    }

    /// Consume input evidence, commit domain state, then notify completion observers.
    fn finish_input(
        &mut self,
        run: InputCommandRun,
        prepared: Result<PreparedInput, super::command::CommandFailure>,
        messages: Option<&MessageSender>,
        context: OwnerUpdateContext<'_>,
    ) {
        let OwnerUpdateContext {
            services,
            trace,
            session,
            step,
        } = context;
        if self.commands.storage_unavailable() {
            let mut command = run.command;
            self.commands
                .lose_observation(&mut command, trace, session, step);
            trace.record(session, "exchange.command.uncommitted", serde_json::json!({
                "command_id":command.record.request.id.as_str(),"sequence":command.record.sequence,"update_step":step,
            }));
            return;
        }
        if let Err(error) = self.input_receipts.observe_acceptance(services) {
            services.execution.close();
            trace.record(session, "exchange.input_receipt.apply_failed", serde_json::json!({
                "command_id":run.command.record.request.id.as_str(),"update_step":step,"reason":error.to_string(),
            }));
            self.commands.uncommitted(
                run.command,
                CommandJournalError::InputCommit,
                trace,
                session,
                step,
            );
            return;
        }
        let update = match prepared {
            Ok(prepared) => apply_input(&run, prepared, services),
            Err(error) => Err(InputCommandFailure::Rejected(error)),
        };
        match update {
            Ok(update) => {
                if update.error.is_none()
                    && update
                        .upsert
                        .iter()
                        .any(|message| matches!(message.purpose, crate::input::InputPurpose::User))
                {
                    let cancelled = (|| -> Result<()> {
                        let mut store = crate::storage::SqliteStore::open(&services.root)?;
                        let now =
                            std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH)?;
                        services
                            .tasks
                            .cancel_continuation(&mut store, i64::try_from(now.as_millis())?)?;
                        Ok(())
                    })();
                    if let Err(error) = cancelled {
                        trace.record(session, "exchange.input.continuation_failed", serde_json::json!({"command_id":run.command.record.request.id.as_str(),"update_step":step,"reason":error.to_string()}));
                    }
                }
                publish_input_observation(update.clone(), messages, services, trace, session, step);
                services.input.changed.notify_one();
                match self.commands.committed(&run.command.record.request.id) {
                    Ok(Some(record)) if record.completion().is_some() => {
                        self.commands
                            .complete(run.command, record, trace, session, step)
                    }
                    Ok(Some(_)) | Ok(None) => {
                        let outcome =
                            super::journal::input_outcome(&run.command.record.request, &update);
                        self.commands
                            .finish(run.command, outcome, trace, session, step);
                    }
                    Err(error) => {
                        self.commands
                            .uncommitted(run.command, error, trace, session, step)
                    }
                }
            }
            Err(InputCommandFailure::Rejected(error)) => {
                self.commands
                    .finish(run.command, Err(error), trace, session, step)
            }
            Err(error) => {
                trace.record(session, "exchange.input.commit_failed", serde_json::json!({"command_id":run.command.record.request.id.as_str(),"update_step":step,"reason":error.to_string()}));
                self.commands.uncommitted(
                    run.command,
                    CommandJournalError::InputCommit,
                    trace,
                    session,
                    step,
                );
            }
        }
    }

    /// Select current input and acquire its capabilities only at the owner mutation boundary.
    async fn prepare_candidate_offer(
        &mut self,
        messages: &MessageSender,
        work_pending: bool,
        steering_admission: SteeringAdmission,
        broker: &Mutex<HarnessBroker>,
        context: OwnerUpdateContext<'_>,
    ) -> Result<CandidateAdmission, InputOfferError> {
        let OwnerUpdateContext {
            services,
            trace,
            session,
            step,
        } = context;
        use super::input_readiness::{InputQueuePolicy, PendingInput, select_pending_input};
        // Preserve caller command ordering before acquiring an internal delivery lane.
        if work_pending
            || matches!(steering_admission, SteeringAdmission::Closed)
            || self.receiver.is_closed()
            || self.commands.has_pending()
            || services.tasks.configuration_is_uncertain()
            || self.commands.storage_unavailable()
            || self
                .input_delivery
                .as_ref()
                .is_some_and(InputDeliveryRun::blocks_input_admission)
            || !matches!(self.execution_state, ExecutionState::Ready)
        {
            return Ok(CandidateAdmission::Waiting);
        }
        let Ok(control) = services.control.clone().try_acquire_owned() else {
            return Ok(CandidateAdmission::Waiting);
        };

        // Consume normalized observations before selecting from the current queue.
        let mut broker = broker.lock().await;
        let view = broker
            .owner_control_view()
            .map_err(InputOfferError::Queue)?;
        for event in view.events {
            if let Err(error) =
                retain_publication(event, Some(messages), services, trace, session, step)
            {
                trace.record(
                    session,
                    "exchange.input_readiness.publication_failed",
                    serde_json::json!({"update_step":step,"reason":error.to_string()}),
                );
            }
        }
        let ready = services
            .observer
            .input_readiness()
            .map_err(InputOfferError::Queue)?;
        let steerable = broker
            .parent_input
            .capture()
            .is_some_and(|parent| parent.is_open());
        let policy = if services.execution.available_permits() == 0 && steerable {
            InputQueuePolicy::AllowSteering
        } else {
            InputQueuePolicy::Ordered
        };
        let message = match select_pending_input(
            services.input.snapshot().map_err(InputOfferError::Queue)?,
            &services.questions,
            policy,
        )? {
            PendingInput::Ready(selection) => {
                let (message, command) = selection.into_parts();
                if command.is_some() {
                    return Ok(CandidateAdmission::Waiting);
                }
                message
            }
            PendingInput::Empty | PendingInput::Waiting => return Ok(CandidateAdmission::Waiting),
            PendingInput::WaitExecution => return Ok(CandidateAdmission::WaitExecution),
        };
        if ready.checks_running {
            // A running turn waiting on `check_run` keeps the owner busy, so the idle-path
            // interruption cannot observe this prompt; request it here and wait for collection.
            interrupt_checks_for_direct_input(&message, services, trace, session, step);
        }
        if !crate::input::input_purpose_is_current(
            &services.tasks,
            &services.questions,
            &message.purpose,
            &services.root,
            services.observer.session_id(),
        )
        .map_err(InputOfferError::Authority)?
            || ready.checks_running
            || ready.has_approval
            || message.intent == crate::input::InputIntent::Queue && ready.has_elicitation
        {
            return Ok(CandidateAdmission::Waiting);
        }
        let input = super::command::InputRevision {
            id: super::command::InputId::new(message.id.clone())
                .map_err(|error| InputOfferError::Queue(error.into()))?,
            revision: message.revision,
        };
        trace.record(session, "exchange.input_readiness.selected", serde_json::json!({"input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step}));

        // Install root or captured steering admission without lending permits to the readiness observer.
        match services.execution.clone().try_acquire_owned() {
            Ok(execution) => {
                return Ok(CandidateAdmission::Foreground(InputDeliveryOffer {
                    input,
                    execution,
                    control,
                }));
            }
            Err(tokio::sync::TryAcquireError::Closed) => return Ok(CandidateAdmission::Waiting),
            Err(tokio::sync::TryAcquireError::NoPermits) => {}
        }
        if !steerable || !steering_input_eligible(&message, services)? {
            return Ok(CandidateAdmission::WaitExecution);
        }
        let Ok(order) = self.steering_order.clone().try_acquire_owned() else {
            return Ok(CandidateAdmission::Waiting);
        };
        Ok(CandidateAdmission::Steering(SteeringDeliveryOffer {
            input,
            control,
            order,
        }))
    }
}

/// Hold owner-acquired capabilities until admission installs their retained delivery pipeline.
enum CandidateAdmission {
    Foreground(InputDeliveryOffer),
    Steering(SteeringDeliveryOffer),
    Waiting,
    WaitExecution,
}

/// Distinguish lost internal observation from a normalized provider fact's commit failure.
#[derive(Debug, thiserror::Error)]
pub(crate) enum ProviderObservationError {
    /// The owner closed before intake accepted this internal readiness notification.
    #[error("exchange owner closed before provider observation intake")]
    OwnerClosed,
    /// Work remains owned even though this forwarder lost its result notification.
    #[error("provider observation completion was not observed")]
    ObservationLost,
    /// An existing broker reducer could not commit the accepted observation prefix.
    #[error("provider observations could not be committed")]
    Commit(#[source] anyhow::Error),
}

impl ProviderObservationError {
    /// Get whether no owner remains to receive a final shutdown notification.
    pub(crate) fn owner_closed(&self) -> bool {
        matches!(self, Self::OwnerClosed)
    }

    /// Get the stable diagnostic code that traces record in place of free-form error text.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::OwnerClosed => "owner_closed",
            Self::ObservationLost => "observation_lost",
            Self::Commit(_) => "observation_commit_failed",
        }
    }
}

/// Retry only a native child guard which certifies that configuration performed no mutation.
fn configuration_waits_for_child(error: &crate::provider::ProviderError) -> bool {
    match error {
        crate::provider::ProviderError::Failure(failure) => {
            failure.operation() == forge_provider_api::ProviderOperation::ThreadConfigure
                && failure.kind() == forge_provider_api::ProviderFailureKind::ExecutionActive
                && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged
        }
        crate::provider::ProviderError::PreparationFailed(source) => {
            configuration_waits_for_child(source)
        }
        crate::provider::ProviderError::Combined { primary, secondary } => {
            configuration_waits_for_child(primary)
                && secondary
                    .iter()
                    .all(crate::broker::ConfigurationReconcileJob::unchanged_failure)
        }
        _ => false,
    }
}

pub(super) enum CommandIntent {
    #[cfg(test)]
    Fresh {
        id: CommandId,
        command: Command,
    },
    Identified(CommandRequest),
}

enum InputDeliveryTarget {
    Foreground,
    Steering(super::command::InputId),
}

/// Observe one steering stage without consuming any ready sibling's join handle.
async fn next_steering(
    runs: &mut HashMap<super::command::InputId, InputDeliveryRun>,
    foreground: TurnStatus,
) -> (super::command::InputId, InputDeliveryEvent) {
    let mut waiting = futures_util::stream::FuturesUnordered::new();
    for (id, run) in runs {
        let id = id.clone();
        let foreground = foreground.clone();
        waiting.push(async move { (id, run.next(foreground).await) });
    }
    match waiting.next().await {
        Some(observed) => observed,
        None => std::future::pending().await,
    }
}

type InternalCompletion = Option<
    Result<(uuid::Uuid, Result<(), super::publication::PublicationError>), tokio::task::JoinError>,
>;

enum DriverWake {
    PublicationStoreRetry,
    QuestionTool,
    PublicationRetry,
    ObservationRecovery(super::observation_recovery::RecoveryEvent),
    TaskCheck(Option<Result<crate::broker::TaskCheckEvent>>),
    TaskCheckUpdate(std::io::Result<Option<crate::broker::publication::SessionUpdate>>),
    ToolCheckUpdate(std::io::Result<Option<crate::broker::publication::SessionUpdate>>),
    ShutdownRetry,
    Shutdown(super::shutdown::ShutdownEvent),
    AutomaticCheck(Option<Result<crate::broker::OwnedCheckResult>>),
    AutomaticUpdate(std::io::Result<Option<crate::broker::publication::SessionUpdate>>),
    AutomaticRepair(Result<DispatchResult>),
    CheckEvent(Option<crate::checks::owner::CheckOwnerEvent>),
    CheckCollected(
        Option<Result<crate::checks::owner::CheckOwnerCompletion, tokio::task::JoinError>>,
    ),
    QuestionWait,
    BrokerAvailable,
    QuestionInput,
    Publication,
    InputDelivery(InputDeliveryTarget, InputDeliveryEvent),
    InputCollected(Option<Result<CollectedInput, tokio::task::JoinError>>),
    InputReceipt(InputReceiptWake),
    Event(Option<OwnerEvent>),
    Foreground(ForegroundInput),
    InternalCompleted(InternalCompletion),
    Dispatched(
        (
            uuid::Uuid,
            Result<
                forge_provider_api::submission::DispatchConfirmation,
                forge_provider_api::submission::DispatchObservationLost,
            >,
        ),
    ),
    Command(CommandEvent),
    StopFinished(Option<Result<CollectedCancellation, tokio::task::JoinError>>),
    RollbackCollected(Option<Result<(CommandId, RollbackRecovery), tokio::task::JoinError>>),
    FailedForeground(Option<Result<FailedForeground, tokio::task::JoinError>>),
}

/// Own the automatic-check stream through producer completion and final draining.
enum AutomaticPublication {
    Open {
        sink: crate::broker::publication::SessionUpdateSink,
        stream: crate::broker::publication::SessionUpdateStream,
    },
    Draining(crate::broker::publication::SessionUpdateStream),
}

impl AutomaticPublication {
    /// Get the receiver retained until its producer has closed and all output is consumed.
    fn stream(&mut self) -> &mut crate::broker::publication::SessionUpdateStream {
        match self {
            Self::Open { stream, .. } | Self::Draining(stream) => stream,
        }
    }

    /// Get a sender only while the check lifecycle can still request repair work.
    fn sink(&self) -> Option<&crate::broker::publication::SessionUpdateSink> {
        match self {
            Self::Open { sink, .. } => Some(sink),
            Self::Draining(_) => None,
        }
    }

    /// Release the producer while retaining its pending output for collection.
    fn close(self) -> Self {
        match self {
            Self::Open { stream, .. } | Self::Draining(stream) => Self::Draining(stream),
        }
    }
}

/// Hold task-check resource gates until all detached work and its final publication have settled.
struct TaskVerification {
    run: crate::broker::OwnedTaskCheckRun,
    _control: tokio::sync::OwnedSemaphorePermit,
    _execution: crate::command::ExecutionPermit,
}

/// Retain actual input cleanup work until its owner has applied collection evidence.
struct CollectedInput {
    work: RetainedInput,
    phase: InputCleanupPhase,
}
enum InputCleanupPhase {
    Native(Result<(), crate::provider::ProviderError>),
    HostDrained,
}

/// Identify one internal readiness request independently of caller commands and selected input.
pub(super) struct InputReadinessId(pub(super) uuid::Uuid);

/// Keep fixture capability injection off the production readiness path.
pub(super) enum InputOfferReply {
    Ready {
        id: InputReadinessId,
        reply: oneshot::Sender<Result<InputReadiness, InputOfferError>>,
    },
    #[cfg(test)]
    Offered(oneshot::Sender<Result<bool, InputOfferError>>),
}

/// Validate steering without allowing task execution preparation to be skipped.
fn steering_input_eligible(
    message: &crate::input::InputMessage,
    services: &OwnerServices,
) -> Result<bool, InputOfferError> {
    use crate::input::{InputDelivery, InputIntent, InputPurpose};
    if message.intent != InputIntent::Auto
        || message.config.is_some()
        || message.delivery() != InputDelivery::Queued
        || message.reservation().is_some()
        || message.text.trim().is_empty()
        || message.text == "/compact"
        || crate::task::command::SessionCommand::queued_input(message)
            .map_err(InputOfferError::Queue)?
            .is_some()
    {
        return Ok(false);
    }
    let task = services.tasks.current().map_err(InputOfferError::Queue)?;
    Ok(match &message.purpose {
        InputPurpose::User => true,
        InputPurpose::Clarification { .. } => false,
        InputPurpose::TaskEntry { .. } => {
            services
                .observer
                .native_steer_supported()
                .map_err(InputOfferError::Queue)?
                && task
                    .as_ref()
                    .and_then(|task| task.entry.as_ref())
                    .is_some_and(|entry| entry.execution.is_none())
        }
        InputPurpose::Question { task_id, .. } => {
            task.as_ref().map(|task| task.id.as_str()) == task_id.as_deref()
        }
        InputPurpose::QuestionContinuation { .. }
        | InputPurpose::TaskContinuation { .. }
        | InputPurpose::WorkflowContinuation { .. } => false,
    })
}

/// Notify a readiness observer without transferring any admitted delivery ownership.
fn notify_input_readiness(
    id: InputReadinessId,
    reply: oneshot::Sender<Result<InputReadiness, InputOfferError>>,
    outcome: Result<InputReadiness, InputOfferError>,
    trace: &TraceStore,
    session: &str,
    input: Option<&super::command::InputRevision>,
    step: u64,
) {
    let disposition = outcome.as_ref().ok().map(|outcome| match outcome {
        InputReadiness::Started => "started",
        InputReadiness::WaitExecution => "wait_execution",
        InputReadiness::Waiting => "waiting",
    });
    let code = outcome.as_ref().err().map(InputOfferError::code);
    let observed = reply.send(outcome).is_ok();
    trace.record(
        session,
        "exchange.input_readiness.response",
        serde_json::json!({
            "readiness_id":id.0.to_string(),"input_id":input.map(|input|input.id.as_str()),"input_revision":input.map(|input|input.revision),"update_step":step,
            "disposition":disposition,"error_code":code,"observed":observed,
        }),
    );
}

/// Notify readiness completion independently of whether its retained delivery still has a waiter.
fn notify_input_offer(
    reply: InputOfferReply,
    outcome: Result<bool, InputOfferError>,
    trace: &TraceStore,
    session: &str,
    input: &super::command::InputRevision,
    step: u64,
) {
    match reply {
        InputOfferReply::Ready { id, reply } => {
            notify_input_readiness(
                id,
                reply,
                outcome.map(|started| {
                    if started {
                        InputReadiness::Started
                    } else {
                        InputReadiness::Waiting
                    }
                }),
                trace,
                session,
                Some(input),
                step,
            );
        }
        #[cfg(test)]
        InputOfferReply::Offered(reply) => {
            let admitted = outcome.as_ref().ok().copied();
            let code = outcome.as_ref().err().map(InputOfferError::code);
            let observed = reply.send(outcome).is_ok();
            trace.record(
                session,
                "exchange.input_offer.response",
                serde_json::json!({
                    "input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,
                    "admitted":admitted,"error_code":code,"observed":observed,
                }),
            );
        }
    }
}

/// Publish task state without turning a lost notification into a failed task mutation or native effect.
fn publish_task_state(
    _messages: Option<&MessageSender>,
    services: &OwnerServices,
    trace: &TraceStore,
    session: &str,
    command: &CommandId,
    step: u64,
) {
    if let Err(error) = publish_session_snapshot(services, session) {
        trace.record(session, "exchange.command.publication_failed", serde_json::json!({"command_id":command.as_str(),"source":"task_commit","update_step":step,"reason":error.to_string()}));
    }
}

/// Publish task state that native input acceptance committed outside any caller command.
fn publish_task_snapshot(
    _messages: Option<&MessageSender>,
    services: &OwnerServices,
    trace: &TraceStore,
    session: &str,
    step: u64,
) {
    if let Err(error) = publish_session_snapshot(services, session) {
        trace.record(session, "exchange.input_receipt.publication_failed", serde_json::json!({"source":"task_acceptance","update_step":step,"reason":error.to_string()}));
    }
}

/// Publish the committed session snapshot with any timeline presentation patch it implies.
fn publish_session_snapshot(services: &OwnerServices, session: &str) -> Result<()> {
    let snapshot = services.observer.snapshot()?;
    let mut events = vec![forge_protocol::message::SessionEvent {
        session_id: session.into(),
        event: "session_changed".into(),
        payload: serde_json::json!({"snapshot":&snapshot}),
    }];
    let patch = services
        .presentation
        .lock()
        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
        .reconcile_with_status_thoughts(
            snapshot.timeline,
            services
                .observer
                .status_thoughts
                .load(std::sync::atomic::Ordering::Relaxed),
        )?;
    if !patch.is_empty() {
        events.push(forge_protocol::message::SessionEvent {
            session_id: session.into(),
            event: "document_changed".into(),
            payload: patch.notification(),
        });
    }
    services.publications.publish_batch(events)?;
    Ok(())
}

/// Publish committed internal input events without inventing a caller command identity.
fn publish_input_events(
    events: Vec<forge_protocol::message::SessionEvent>,
    _messages: Option<&MessageSender>,
    services: &OwnerServices,
    trace: &TraceStore,
    session: &str,
    input: &super::command::InputRevision,
    step: u64,
) {
    if let Err(error) = services.publications.publish_batch(events) {
        trace.record(session, "exchange.input.publication_failed", serde_json::json!({
            "input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,"reason":error.to_string(),
        }));
    }
}

/// Publish owner-committed receipt progress independently of the producer's response waiter.
/// Interrupt the active check run on behalf of a current direct prompt.
///
/// A direct prompt preempts checks whether the owner is idle or a running turn is waiting on a
/// `check_run` result; input readiness retries delivery once the cancelled run is collected.
/// Explicitly queued input and stale task input never interrupt checks. Returns whether a run
/// was active and cancellation was requested.
fn interrupt_checks_for_direct_input(
    message: &crate::input::InputMessage,
    services: &OwnerServices,
    trace: &TraceStore,
    session: &str,
    step: u64,
) -> bool {
    if message.intent != crate::input::InputIntent::Auto
        || !crate::input::input_purpose_is_current(
            &services.tasks,
            &services.questions,
            &message.purpose,
            &services.root,
            services.observer.session_id(),
        )
        .unwrap_or(false)
    {
        return false;
    }
    let Ok(Some(run_id)) = services.checks.active() else {
        return false;
    };
    if let Err(error) = services.checks.cancel_for_collection(&run_id) {
        trace.record(
            session,
            "exchange.check.input_collection_failed",
            serde_json::json!({
                "input_id":message.id,"input_revision":message.revision,"run_id":run_id,
                "reason":error.to_string(),"update_step":step,
            }),
        );
    }
    true
}

fn publish_input_observation(
    update: crate::input::InputUpdate,
    messages: Option<&MessageSender>,
    services: &OwnerServices,
    trace: &TraceStore,
    session: &str,
    step: u64,
) {
    let published = (|| -> Result<()> {
        let payload = serde_json::to_value(update)?;
        retain_publication(
            forge_protocol::message::SessionEvent {
                session_id: session.into(),
                event: "input_changed".into(),
                payload,
            },
            messages,
            services,
            trace,
            session,
            step,
        )?;
        Ok(())
    })();
    if let Err(error) = published {
        trace.record(
            session,
            "exchange.input_receipt.publication_failed",
            serde_json::json!({
                "update_step":step,"reason":error.to_string(),
            }),
        );
    }
}

/// Retain the owner's changed activity before making it visible to snapshot readers.
fn publish_activity(
    phase: super::ActivityPhase,
    messages: Option<&MessageSender>,
    services: &OwnerServices,
    trace: &TraceStore,
    session: &str,
    step: u64,
) -> Result<()> {
    let previous = *services
        .observer
        .activity
        .read()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(activity) = previous.changed(phase) else {
        return Ok(());
    };
    retain_publication(
        forge_protocol::message::SessionEvent {
            session_id: session.into(),
            event: "activity_changed".into(),
            payload: serde_json::json!({"runtime_epoch":services.observer.runtime_epoch(),"activity":activity}),
        },
        messages,
        services,
        trace,
        session,
        step,
    )?;
    *services
        .observer
        .activity
        .write()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = activity;
    Ok(())
}

/// Retain late cleanup facts and resources independently of the completed caller wait.
struct CollectedCancellation {
    command_id: CommandId,
    retained: RetainedTransition,
    phase: CancellationCleanupPhase,
}

/// Separate native collection from joining host work unblocked by the owner's cancellation signal.
enum CancellationCleanupPhase {
    Native(Result<(), crate::provider::ProviderError>),
    HostDrained { collected: bool },
}

/// Publish committed control observations without retaining the mutable broker guard.
fn publish_control(
    mut events: Vec<forge_protocol::message::SessionEvent>,
    input: Option<&crate::input::InputUpdate>,
    context: OwnerUpdateContext<'_>,
    command: &CommandId,
) {
    let OwnerUpdateContext {
        services,
        trace,
        session,
        step,
    } = context;
    let published = (|| -> Result<()> {
        if let Some(input) = input {
            events.push(forge_protocol::message::SessionEvent {
                session_id: session.into(),
                event: "input_changed".into(),
                payload: serde_json::to_value(input)?,
            });
        }
        services.publications.publish_batch(events)?;
        Ok(())
    })();
    if let Err(error) = published {
        trace.record(
            session,
            "exchange.command.publication_failed",
            serde_json::json!({
                "command_id":command.as_str(),"update_step":step,"reason":error.to_string(),
            }),
        );
    }
}

struct FailedForeground {
    runtime: ActiveTurn,
    original: anyhow::Error,
    cleanup: Result<(), crate::provider::ProviderError>,
    catalog: Option<Box<crate::provider::ProviderCatalogRequest>>,
    unpublished: Vec<crate::broker::publication::SessionUpdate>,
}

/// Retain committed update payloads until publication storage accepts their ordered prefix.
#[derive(Default)]
struct DeferredPublications {
    updates: VecDeque<DeferredUpdate>,
    retry_at: Option<tokio::time::Instant>,
}

/// Preserve the original publication identity together with its deterministic encoded source.
#[derive(Clone)]
struct DeferredUpdate {
    id: String,
    update: crate::broker::publication::SessionUpdate,
    metadata_applied: bool,
}

/// Apply committed metadata and retain failed publication work instead of discarding its payload.
async fn publish_updates(
    deferred: &mut DeferredPublications,
    commands: &mut CommandQueue,
    updates: Vec<crate::broker::publication::SessionUpdate>,
    messages: Option<&MessageSender>,
    context: OwnerUpdateContext<'_>,
) {
    let OwnerUpdateContext {
        services,
        trace,
        session,
        step,
    } = context;
    deferred
        .updates
        .extend(updates.into_iter().map(|update| DeferredUpdate {
            id: uuid::Uuid::new_v4().to_string(),
            update,
            metadata_applied: false,
        }));
    if deferred
        .retry_at
        .is_some_and(|deadline| tokio::time::Instant::now() < deadline)
    {
        return;
    }
    deferred.retry_at = None;
    for _ in 0..OWNER_INTAKE_BUDGET {
        let Some(DeferredUpdate {
            id,
            update,
            metadata_applied,
        }) = deferred.updates.front().cloned()
        else {
            break;
        };
        if !metadata_applied {
            if update.wakes_input() {
                services.input.changed.notify_one();
            }
            if let crate::broker::publication::SessionUpdate::Configuration { catalog, .. } =
                &update
            {
                services
                    .resource_revision
                    .fetch_add(1, std::sync::atomic::Ordering::AcqRel);
                *services.catalog.write().await = catalog.as_ref().clone();
            }
            deferred
                .updates
                .front_mut()
                .expect("the retained publication stays at the front until commit")
                .metadata_applied = true;
        }
        let publication = async {
            let event = crate::broker::publication::encode_session_event(session, &update)?;
            let publication = services.publications.publish_once(
                super::publication::PublicationOrigin::OwnerUpdate(id),
                event,
                messages,
            )?;
            trace.record(session, "exchange.publication.committed", serde_json::json!({
                "publication_sequence":publication.sequence().get(),"delivery":publication.state(),
                "event":publication.event().event,"update_step":step,
            }));
            Ok::<(), anyhow::Error>(())
        }
        .await;
        if let Err(error) = publication {
            commands.retention_failed(trace, session, step);
            services.execution.close();
            deferred.retry_at =
                Some(tokio::time::Instant::now() + std::time::Duration::from_millis(250));
            trace.record(
                session,
                "exchange.publication.failed",
                serde_json::json!({"update_step":step,"reason":error.to_string()}),
            );
            return;
        }
        deferred.updates.pop_front();
    }
    if !deferred.updates.is_empty() {
        deferred.retry_at = Some(tokio::time::Instant::now());
    }
}

/// Retain committed notification content before observing transient outbound admission.
fn retain_publication(
    event: forge_protocol::message::SessionEvent,
    messages: Option<&MessageSender>,
    services: &OwnerServices,
    trace: &TraceStore,
    session: &str,
    step: u64,
) -> Result<(), super::publication::PublicationError> {
    let publication = services.publications.publish(event, messages)?;
    trace.record(
        session,
        "exchange.publication.committed",
        serde_json::json!({
            "publication_sequence":publication.sequence().get(),"delivery":publication.state(),
            "event":publication.event().event,"update_step":step,
        }),
    );
    Ok(())
}

/// Collect external work without mutating lifecycle state or notifying its waiter.
async fn collect_failed_foreground(
    mut runtime: ActiveTurn,
    original: anyhow::Error,
    _provider: Arc<crate::provider::Provider>,
    permissions: Arc<crate::provider::approval::PermissionCoordinator>,
    session: String,
    trace: Arc<TraceStore>,
) -> FailedForeground {
    runtime
        .transaction
        .fail_live_host_observer(&trace, &session);
    runtime.reject_pending_live_effects();
    runtime.staged_effects.retain(ForegroundJob::is_collection);
    runtime.run_effects(ForegroundEffectGate::CollectionOnly);
    let approvals = std::panic::AssertUnwindSafe(async {
        permissions
            .cancel_owner(&crate::provider::ThreadOwner::root(&session), None)
            .await
            .map_err(|error| {
                crate::provider::lifecycle_error::classify(
                    forge_provider_api::ProviderOperation::TurnStart,
                    error,
                )
            })
    })
    .catch_unwind()
    .await
    .unwrap_or(Err(crate::provider::ProviderError::DeliveryUncertain));
    let invocation =
        runtime
            .transaction
            .invocation_scope()
            .map(|scope| match (&runtime.transaction, scope) {
                (
                    TurnTransaction::Running { transaction, .. }
                    | TurnTransaction::Finalizing { transaction, .. },
                    Some(scope),
                ) => Some((transaction.engine.clone(), scope)),
                _ => None,
            });
    let native = std::panic::AssertUnwindSafe(async {
        match invocation? {
            Some((engine, scope)) => engine.cancel_exchange(scope).await,
            None => Ok(()),
        }
    })
    .catch_unwind()
    .await
    .unwrap_or(Err(crate::provider::ProviderError::DeliveryUncertain));
    let cleanup = match (approvals, native) {
        (Ok(()), Ok(())) => Ok(()),
        (Err(error), Ok(())) | (Ok(()), Err(error)) => Err(error),
        (Err(primary), Err(secondary)) => Err(crate::provider::ProviderError::Combined {
            primary: Box::new(primary),
            secondary: vec![secondary],
        }),
    };
    let mut catalog = None;
    let mut unpublished = Vec::new();
    collect_failed_jobs(&mut runtime, &mut catalog, &mut unpublished).await;
    while let Some(publication) = runtime.publications.pop_front() {
        runtime.launch_publication(publication);
        collect_failed_jobs(&mut runtime, &mut catalog, &mut unpublished).await;
    }
    FailedForeground {
        runtime,
        original,
        cleanup,
        catalog,
        unpublished,
    }
}

/// Drain committed output while joining failed jobs so publication flushes cannot deadlock collection.
async fn collect_failed_jobs(
    runtime: &mut ActiveTurn,
    catalog: &mut Option<Box<crate::provider::ProviderCatalogRequest>>,
    unpublished: &mut Vec<crate::broker::publication::SessionUpdate>,
) {
    while !runtime.jobs.is_empty() || !runtime.publication_job.is_empty() {
        tokio::select! {
            result = runtime.jobs.join_next(), if !runtime.jobs.is_empty() => {
                if let Some(Ok(Ok(signal))) = result {
                    runtime.uncommitted_signals.push(signal);
                }
            }
            result = runtime.publication_job.join_next(), if !runtime.publication_job.is_empty() => {
                if let Some(Ok(Ok(signal))) = result {
                    runtime.uncommitted_signals.push(signal);
                }
            }
            update = async {
                match runtime.updates.as_mut() {
                    Some(updates) => updates.recv().await,
                    None => std::future::pending().await,
                }
            } => match update {
                Ok(Some(update)) => forward_failed_update(update, catalog, unpublished),
                Ok(None) | Err(_) => runtime.updates = None,
            }
        }
    }
    while let Some(updates) = runtime.updates.as_mut() {
        let update = updates.try_recv();
        match update {
            Ok(update) => forward_failed_update(update, catalog, unpublished),
            Err(_) => break,
        }
    }
}

/// Forward only committed update facts; return changed catalog evidence for the mutation owner.
fn forward_failed_update(
    update: crate::broker::publication::SessionUpdate,
    catalog: &mut Option<Box<crate::provider::ProviderCatalogRequest>>,
    unpublished: &mut Vec<crate::broker::publication::SessionUpdate>,
) {
    if let crate::broker::publication::SessionUpdate::Configuration { catalog: next, .. } = &update
    {
        *catalog = Some(next.clone());
    }
    unpublished.push(update);
}

/// Keep terminal result observation independent of who owns native work and committed output.
enum ForegroundCompletion {
    /// Commit the captured annotation even if its transport observer disconnects.
    Review {
        reply: oneshot::Sender<Result<DispatchResult>>,
        scope: Arc<crate::plan::review_answer::ReviewAnswerScope>,
        root: std::path::PathBuf,
    },
    /// Notify a compatibility caller; its result publications retain their existing transport path.
    Caller(oneshot::Sender<Result<DispatchResult>>),
    /// Retain terminal events for owner-admitted work without requiring an external forwarder.
    Retained {
        publications: Arc<super::publication::SessionPublications>,
        messages: Option<MessageSender>,
    },
    /// Preserve collected runtime resources after their one terminal notification has completed.
    Notified,
}

struct QueuedTurn {
    id: uuid::Uuid,
    transaction: TurnTransaction,
    messages: Option<MessageSender>,
    initial: Option<ForegroundJob>,
    completion: ForegroundCompletion,
    updates: Option<crate::broker::publication::SessionUpdateStream>,
    execution: Option<crate::command::ExecutionPermit>,
}

enum ForegroundProgress {
    Waiting,
    Finished,
}

/// Snapshot foreground steering authority before owner intake awaits unrelated work.
#[derive(Clone, Copy)]
enum SteeringAdmission {
    Open,
    Closed,
}

impl From<Option<&ActiveTurn>> for SteeringAdmission {
    /// Capture the exact foreground state without retaining its non-Sync runtime across await.
    fn from(foreground: Option<&ActiveTurn>) -> Self {
        if foreground.is_some_and(|runtime| !runtime.transaction.accepts_steering()) {
            Self::Closed
        } else {
            Self::Open
        }
    }
}

/// Authorize reducer commits and new effects only while owner storage remains certified.
enum ForegroundCommit {
    Certified,
    Unavailable,
}

/// Separate new invocation work from cleanup that remains authorized while admission is closed.
#[derive(Clone, Copy)]
enum ForegroundEffectGate {
    Open,
    CollectionOnly,
}

enum Publication {
    Effect(ForegroundJob),
    Forward(ForegroundSignal),
}

/// Keep handoff evidence separate from foreground lifecycle and native acceptance.
enum TurnDispatch {
    NotStarted,
    InFlight {
        scope: crate::provider::InvocationScope,
        observer: crate::provider::DispatchObserver,
    },
    Dispatched(Box<forge_provider_api::submission::DispatchConfirmation>),
    Lost,
}

/// Observe one live work item without retaining mutable foreground state across the wait.
struct PendingHandoff {
    work_id: uuid::Uuid,
    observer: crate::provider::DispatchObserver,
}

/// Distinguish lifecycle facts from committed output awaiting publication.
enum ForegroundInput {
    Signal(Box<ForegroundSignal>, bool),
    Update(Box<crate::broker::publication::SessionUpdate>),
    PublicationClosed,
}

struct ActiveTurn {
    id: uuid::Uuid,
    transaction: TurnTransaction,
    messages: Option<MessageSender>,
    publisher: Arc<super::publication::SessionPublications>,
    completion: ForegroundCompletion,
    jobs: JoinSet<Result<ForegroundSignal>>,
    publication_job: JoinSet<Result<ForegroundSignal>>,
    // Keep yielded mutation guards alive if storage fails before their owner commit.
    uncommitted_signals: Vec<ForegroundSignal>,
    pending_live_effects: super::live_effect_queue::LiveEffectQueue,
    publications: VecDeque<Publication>,
    collection_signals: VecDeque<ForegroundSignal>,
    staged_effects: VecDeque<ForegroundJob>,
    publication_drain_requested: bool,
    handoff: TurnDispatch,
    updates: Option<crate::broker::publication::SessionUpdateStream>,
    _execution: Option<crate::command::ExecutionPermit>,
}

impl ActiveTurn {
    /// Install all owned resources before polling another phase.
    fn new(prepared: QueuedTurn, publisher: Arc<super::publication::SessionPublications>) -> Self {
        let QueuedTurn {
            id,
            transaction,
            messages,
            initial,
            completion,
            updates,
            execution,
        } = prepared;
        let mut runtime = Self {
            publisher,
            id,
            transaction,
            messages,
            completion,
            jobs: JoinSet::new(),
            uncommitted_signals: Vec::new(),
            pending_live_effects: super::live_effect_queue::LiveEffectQueue::default(),
            publications: VecDeque::new(),
            collection_signals: VecDeque::new(),
            staged_effects: VecDeque::new(),
            publication_job: JoinSet::new(),
            publication_drain_requested: false,
            handoff: TurnDispatch::NotStarted,
            updates,
            _execution: execution,
        };
        if let Some(job) = initial {
            runtime.effect(job);
        }
        runtime
    }

    /// Retire admitted but undispatched work without starting its native preparation effect.
    fn stopping(
        mut prepared: QueuedTurn,
        publisher: Arc<super::publication::SessionPublications>,
    ) -> Self {
        prepared.initial.take();
        let mut runtime = Self::new(prepared, publisher);
        runtime.jobs.spawn(async {
            Ok(ForegroundSignal::EffectFailed(
                CommandFailure::OwnerStopping.into(),
            ))
        });
        runtime
    }

    /// Attach a handoff observer once, then clone only its result observation for cancellation-safe waiting.
    fn pending_handoff(&mut self) -> Option<PendingHandoff> {
        if matches!(self.handoff, TurnDispatch::NotStarted)
            && let Some((scope, observer)) = self.transaction.dispatch_observer()
        {
            self.handoff = TurnDispatch::InFlight { scope, observer };
        }
        match &self.handoff {
            TurnDispatch::InFlight { observer, .. } => Some(PendingHandoff {
                work_id: self.id,
                observer: observer.clone(),
            }),
            TurnDispatch::NotStarted | TurnDispatch::Dispatched(_) | TurnDispatch::Lost => None,
        }
    }

    /// Consume evidence already available before the foreground can retire its native request.
    fn read_handoff(&mut self, trace: &TraceStore, session: &str, step: u64) {
        let observed = match &self.handoff {
            TurnDispatch::InFlight { observer, .. } => match observer.current() {
                Ok(Some(confirmation)) => Some(Ok(confirmation)),
                Ok(None) => None,
                Err(error) => Some(Err(error)),
            },
            TurnDispatch::NotStarted | TurnDispatch::Dispatched(_) | TurnDispatch::Lost => None,
        };
        if let Some(observed) = observed {
            self.observe_handoff(observed, trace, session, step);
        }
    }

    /// Commit the scoped dispatch fact independently of native input acceptance or answer completion.
    fn observe_handoff(
        &mut self,
        observed: Result<
            forge_provider_api::submission::DispatchConfirmation,
            forge_provider_api::submission::DispatchObservationLost,
        >,
        trace: &TraceStore,
        session: &str,
        step: u64,
    ) {
        let TurnDispatch::InFlight { scope, .. } = &self.handoff else {
            return;
        };
        match observed {
            Ok(confirmation) if confirmation.scope() == scope => {
                let target = match confirmation.target() {
                    forge_provider_api::submission::DispatchTarget::Input(_) => "input",
                };
                trace.record(session, "exchange.dispatch.confirmed", serde_json::json!({
                    "work_id":self.id,"scope":scope,"binding_id":confirmation.binding().as_str(),
                    "target":target,"update_step":step,
                }));
                self.handoff = TurnDispatch::Dispatched(Box::new(confirmation));
            }
            Ok(_) => {
                trace.record(session, "exchange.dispatch.ignored", serde_json::json!({
                    "work_id":self.id,"scope":scope,"update_step":step,"reason":"scope_mismatch",
                }));
                self.handoff = TurnDispatch::Lost;
            }
            Err(_) => {
                trace.record(
                    session,
                    "exchange.dispatch.lost",
                    serde_json::json!({
                        "work_id":self.id,"scope":scope,"update_step":step,
                    }),
                );
                self.handoff = TurnDispatch::Lost;
            }
        }
    }

    /// Read one observation or owned job result without retaining a broker lock.
    async fn next(&mut self) -> ForegroundInput {
        if let Some(signal) = self.collection_signals.pop_front() {
            return ForegroundInput::Signal(Box::new(signal), false);
        }
        if self.publication_drain_requested
            && self.jobs.is_empty()
            && self.publication_job.is_empty()
            && self.publications.is_empty()
            && self.staged_effects.is_empty()
        {
            self.publication_drain_requested = false;
            return ForegroundInput::Signal(Box::new(ForegroundSignal::PublicationDrained), false);
        }
        tokio::select! {
            update = async {
                match self.updates.as_mut() { Some(updates) => updates.recv().await, None => std::future::pending().await }
            } => match update {
                Ok(Some(update)) => ForegroundInput::Update(Box::new(update)),
                Ok(None) => { self.updates = None; ForegroundInput::PublicationClosed }
                Err(error) => ForegroundInput::Signal(Box::new(ForegroundSignal::EffectFailed(error.into())), false),
            },
            signal = self.transaction.next() => ForegroundInput::Signal(Box::new(signal.unwrap_or_else(ForegroundSignal::EffectFailed)), false),
            result = self.publication_job.join_next(), if !self.publication_job.is_empty() => {
                let signal = match result {
                    Some(Ok(result)) => result.unwrap_or_else(ForegroundSignal::EffectFailed),
                    Some(Err(error)) => ForegroundSignal::EffectFailed(anyhow::anyhow!("foreground publication task failed: {error}")),
                    None => ForegroundSignal::EffectFailed(anyhow::anyhow!("foreground publication task disappeared")),
                };
                ForegroundInput::Signal(Box::new(signal), true)
            }
            result = self.jobs.join_next(), if !self.jobs.is_empty() => {
                match result {
                    Some(Ok(result)) => ForegroundInput::Signal(Box::new(result.unwrap_or_else(ForegroundSignal::EffectFailed)), false),
                    Some(Err(error)) => ForegroundInput::Signal(Box::new(ForegroundSignal::EffectFailed(anyhow::anyhow!("foreground effect task failed: {error}"))), false),
                    None => ForegroundInput::Signal(Box::new(ForegroundSignal::EffectFailed(anyhow::anyhow!("foreground effect task disappeared"))), false),
                }
            }
        }
    }

    /// Commit one reducer transition before launching its authorized effects.
    async fn update(
        &mut self,
        broker: &Mutex<HarnessBroker>,
        signal: ForegroundSignal,
        completed_publication: bool,
        commit: ForegroundCommit,
    ) -> Result<ForegroundProgress> {
        if let ForegroundSignal::LiveHost(request) = signal {
            self.pending_live_effects.stage(request);
            return Ok(ForegroundProgress::Waiting);
        }
        self.apply_update(broker, signal, completed_publication, commit)
            .await
    }

    async fn apply_update(
        &mut self,
        broker: &Mutex<HarnessBroker>,
        signal: ForegroundSignal,
        completed_publication: bool,
        commit: ForegroundCommit,
    ) -> Result<ForegroundProgress> {
        if matches!(commit, ForegroundCommit::Unavailable) {
            match signal {
                ForegroundSignal::LiveHost(request) => {
                    request.reject(crate::provider::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Runtime,
                        ),
                    ));
                }
                signal => self.uncommitted_signals.push(signal),
            }
            anyhow::bail!("foreground effects are unavailable after owner storage failure");
        }
        if needs_forward(&signal) {
            if completed_publication {
                self.launch_publication(Publication::Forward(signal));
            } else {
                self.publication(Publication::Forward(signal));
            }
            return Ok(ForegroundProgress::Waiting);
        }
        let collection_started = matches!(&signal, ForegroundSignal::CollectionStarted(_));
        if collection_started {
            self.reject_pending_live_effects();
        }
        let action = {
            let publication_sink = self.transaction.publication_sink()?;
            let mut broker = broker.lock().await;
            broker.with_publication_sink(&publication_sink, |broker| {
                match broker.step_foreground(&mut self.transaction, signal) {
                    Ok(action) => Ok(action),
                    Err(error) => broker.fail_foreground_dispatch(&mut self.transaction, error),
                }
            })?
        };
        if collection_started {
            let pending = self.staged_effects.len();
            self.staged_effects.retain(ForegroundJob::is_collection);
            if self.staged_effects.len() != pending {
                self.collection_signals
                    .push_back(ForegroundSignal::EffectFailed(
                        crate::broker::TurnCancelled.into(),
                    ));
            }
        }
        if completed_publication && let Some(publication) = self.publications.pop_front() {
            self.launch_publication(publication);
        }
        match action {
            ForegroundAction::Wait => {}
            ForegroundAction::WaitForPublicationDrain => self.publication_drain_requested = true,
            ForegroundAction::Effect(job) => self.effect(job),
            ForegroundAction::CancelJobsAndEffect(job) => {
                self.publication_drain_requested = false;
                self.effect(job);
            }
            ForegroundAction::Finish => {
                anyhow::ensure!(
                    self.jobs.is_empty()
                        && self.publication_job.is_empty()
                        && self.publications.is_empty()
                        && self.staged_effects.is_empty(),
                    "foreground settlement still has pending effects"
                );
                return Ok(ForegroundProgress::Finished);
            }
            ForegroundAction::FinishFailure(error) => {
                anyhow::ensure!(
                    self.jobs.is_empty()
                        && self.publication_job.is_empty()
                        && self.publications.is_empty()
                        && self.staged_effects.is_empty(),
                    "foreground failure still has pending effects"
                );
                return Err(error);
            }
        }
        Ok(ForegroundProgress::Waiting)
    }

    /// Retain ordinary work until the update's command transition has revalidated its admission.
    fn effect(&mut self, job: ForegroundJob) {
        if job.is_publication() {
            self.publication(Publication::Effect(job));
        } else {
            self.staged_effects.push_back(job);
        }
    }

    /// Dispatch retained effects only from the owner's post-command effect phase.
    async fn dispatch_live_effects(
        &mut self,
        broker: &Mutex<HarnessBroker>,
        gate: ForegroundEffectGate,
    ) {
        if !matches!(gate, ForegroundEffectGate::Open) {
            let _ = self
                .pending_live_effects
                .take_authorized(super::live_effect_queue::LiveEffectAdmission::Closed);
            return;
        }
        if matches!(&self.transaction, TurnTransaction::Running { transaction, .. }
            | TurnTransaction::Finalizing { transaction, .. } if transaction.live_host.is_some())
        {
            return;
        }
        if let Some(request) = self
            .pending_live_effects
            .take_authorized(super::live_effect_queue::LiveEffectAdmission::Open)
            && let Err(error) = self
                .apply_update(
                    broker,
                    ForegroundSignal::LiveHost(request),
                    false,
                    ForegroundCommit::Certified,
                )
                .await
        {
            self.collection_signals
                .push_back(ForegroundSignal::EffectFailed(error));
        }
    }

    fn reject_pending_live_effects(&mut self) {
        self.pending_live_effects.reject_pending();
    }

    /// Launch jobs already selected by the post-command admission decision.
    fn run_effects(&mut self, gate: ForegroundEffectGate) {
        if matches!(gate, ForegroundEffectGate::Open) {
            self.transaction.authorize_submission();
        }
        let pending = self.staged_effects.len();
        for _ in 0..pending {
            let job = self
                .staged_effects
                .pop_front()
                .expect("the captured staged prefix remains owned");
            if matches!(gate, ForegroundEffectGate::Open) || job.is_collection() {
                self.jobs.spawn(run_foreground_job(job));
            } else {
                self.staged_effects.push_back(job);
            }
        }
    }

    /// Preserve outbound order when multiple publication effects become ready.
    fn publication(&mut self, publication: Publication) {
        if !self.publication_job.is_empty() {
            self.publications.push_back(publication);
        } else {
            self.launch_publication(publication);
        }
    }

    /// Launch outbound work without blocking event intake on its waiter.
    fn launch_publication(&mut self, publication: Publication) {
        assert!(
            self.publication_job.is_empty(),
            "foreground retains one ordered publication worker"
        );
        match publication {
            Publication::Effect(job) => {
                self.publication_job.spawn(run_foreground_job(job));
            }
            Publication::Forward(signal) => {
                let sink = self.messages.clone();
                let publisher = self.publisher.clone();
                self.publication_job.spawn(async move {
                    Ok(forward_signal(signal, &publisher, sink.as_ref()).await)
                });
            }
        }
    }
}

/// Recognize outbound work that must run outside mutable transition ownership.
fn needs_forward(signal: &ForegroundSignal) -> bool {
    matches!(
        signal,
        ForegroundSignal::ObservationPublicationNeedsForward { .. }
            | ForegroundSignal::CommandEventsNeedForward { .. }
            | ForegroundSignal::CommandNeedsForward { .. }
            | ForegroundSignal::PermissionNeedsForward { .. }
            | ForegroundSignal::AdmissionEventsNeedForward { .. }
            | ForegroundSignal::FailureAdmissionEventsNeedForward { .. }
            | ForegroundSignal::FailurePublicationNeedsForward { .. }
            | ForegroundSignal::AutoCompactionNeedsForward { .. }
            | ForegroundSignal::CheckRepairNeedsForward { .. }
            | ForegroundSignal::ClarificationNeedsForward { .. }
            | ForegroundSignal::FinalResponseNeedsForward { .. }
            | ForegroundSignal::PlanTerminalNeedsForward { .. }
    )
}

/// Publish a completed review answer only while its original source annotation remains current.
fn retain_review_answer(
    scope: &crate::plan::review_answer::ReviewAnswerScope,
    root: &std::path::Path,
    result: &DispatchResult,
) -> Result<()> {
    if result.response.error().is_some() {
        return Ok(());
    }
    let id = result
        .response
        .result()
        .and_then(|value| value.pointer("/exchange/id"))
        .and_then(serde_json::Value::as_str)
        .context("review answer has no completed exchange")?;
    let exchange = crate::storage::SqliteStore::read_only(root)?
        .load_exchange(id)?
        .context("review answer exchange is missing")?;
    anyhow::ensure!(
        exchange.state() == crate::exchange::ExchangeState::Complete,
        "review answer stopped before completion"
    );
    let answer = exchange
        .turn
        .iter()
        .flat_map(|turn| turn.messages())
        .filter(|message| {
            message.kind() == crate::turn::MessageKind::Assistant
                && message.delivery() == crate::turn::MessageDelivery::Final
        })
        .map(|message| message.text())
        .collect::<Vec<_>>()
        .join("\n\n");
    scope.submit(answer)
}

/// Collect a stop's native work on a task, reporting a panic as unknown delivery.
///
/// A panic inside collection proves nothing about what the provider received, so it reports
/// `DeliveryUncertain` and the stop's finish handler restarts the provider.
fn stop_collect_spawn(
    running_stops: &mut JoinSet<CollectedCancellation>,
    command_id: CommandId,
    mut retained: RetainedTransition,
    services: &OwnerServices,
) {
    let services = services.clone();
    running_stops.spawn(async move {
        let outcome = std::panic::AssertUnwindSafe(retained.collect(&services))
            .catch_unwind()
            .await
            .unwrap_or(Err(crate::provider::ProviderError::DeliveryUncertain));
        CollectedCancellation {
            command_id,
            retained,
            phase: CancellationCleanupPhase::Native(outcome),
        }
    });
}

/// Fail every queued turn with one reason, because the owner can no longer start them.
fn queued_turns_reject(
    pending: &mut VecDeque<QueuedTurn>,
    reason: &'static str,
    context: OwnerUpdateContext<'_>,
) {
    for queued in pending.drain(..) {
        notify(
            queued.completion,
            queued.id,
            Err(anyhow::anyhow!(reason)),
            context.trace,
            context.session,
            context.step,
        );
    }
}

/// Move every stop decision a source yields into the active turn, before it reads native events.
///
/// Without an active turn the decisions have no reader, so the source is still drained and the
/// decisions are dropped.
fn collection_signals_move(
    mut active: Option<&mut ActiveTurn>,
    mut next: impl FnMut() -> Option<ForegroundSignal>,
) {
    while let Some(signal) = next() {
        if let Some(turn) = active.as_deref_mut() {
            turn.collection_signals.push_back(signal);
        }
    }
}

/// Resolve an observer without treating a dropped waiter as an execution failure.
fn notify(
    completion: ForegroundCompletion,
    id: uuid::Uuid,
    outcome: Result<DispatchResult>,
    trace: &TraceStore,
    session_id: &str,
    step: u64,
) {
    let outcome = match &completion {
        ForegroundCompletion::Review { scope, root, .. } => outcome.and_then(|result| {
            retain_review_answer(scope, root, &result)?;
            Ok(result)
        }),
        ForegroundCompletion::Caller(_)
        | ForegroundCompletion::Retained { .. }
        | ForegroundCompletion::Notified => outcome,
    };
    let settled = outcome.is_ok();
    let response_succeeded = outcome
        .as_ref()
        .is_ok_and(|result| result.response.error().is_none());
    let observed = match completion {
        ForegroundCompletion::Caller(reply) | ForegroundCompletion::Review { reply, .. } => {
            reply.send(outcome).is_ok()
        }
        ForegroundCompletion::Retained {
            publications,
            messages,
        } => {
            if let Ok(result) = outcome {
                for event in result.event {
                    if let Err(error) = publications.publish(event, messages.as_ref()) {
                        trace.record(
                            session_id,
                            "exchange.turn.terminal_publication_failed",
                            serde_json::json!({
                                "work_id":id,"update_step":step,"reason":error.to_string(),
                            }),
                        );
                    }
                }
            }
            true
        }
        ForegroundCompletion::Notified => false,
    };
    trace.record(
        session_id,
        "exchange.turn.response",
        serde_json::json!({
            "work_id":id,"update_step":step,"settled":settled,"response_succeeded":response_succeeded,"observed":observed,
        }),
    );
}

/// Retain required lifecycle evidence before publication or dependent owner transitions.
fn record_owner_core(
    queue: &mut super::queue::CommandQueue,
    record: &super::core_log::CoreRecord,
    trace: &crate::trace::TraceStore,
    session: &str,
    step: u64,
) -> Result<(), TransitionFailure> {
    if let Err(error) = queue.append_core(record) {
        queue.retention_failed(trace, session, step);
        return Err(TransitionFailure::Domain(error.into()));
    }
    Ok(())
}
