//! Own session commands, normalized observation transitions, and foreground work beyond response waiters.

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
    Command, CommandFailure, CommandId, CommandOutcome, CommandRequest, CommandResult,
    ConfigurationRejection, ExecuteCommandError, InputRevision,
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
use super::journal::{CommandJournalError, CommandRecord};
use super::native_command::NativeCommandProgress;
use super::queue::{CommandAdmission, CommandQueue, CommandReply};
use super::rollback_run::{RollbackProgress, RollbackRecovery};
use super::services::OwnerServices;
use super::task_command::TaskCommandProgress;
use super::task_input::TaskInputProduction;
use super::transition::{
    ForegroundStatus, RetainedTransition, TransitionFailure, TransitionOutcome, TransitionProgress,
};
use crate::broker::{
    DispatchResult, ForegroundAction, ForegroundDispatch, ForegroundJob, ForegroundSignal,
    HarnessBroker,
};
use crate::trace::TraceStore;

const OWNER_INTAKE_BUDGET: usize = 32;
const OWNER_QUEUE_CAPACITY: usize = forge_protocol::MAX_PENDING_FRAMES;

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

/// Submit caller intent and normalized facts to the session's persistent exchange owner.
/// A dropped response waiter never owns or cancels admitted work.
pub(crate) struct ExchangeOwner {
    sender: mpsc::Sender<OwnerEvent>,
    child_sender: Arc<std::sync::OnceLock<crate::agent::AgentCommandSender>>,
    shutdown_retry: Arc<tokio::sync::Notify>,
}

/// Submit scoped question facts without keeping the session owner alive through its own tool contexts.
#[derive(Clone)]
pub(crate) struct QuestionEvents {
    sender: mpsc::WeakSender<OwnerEvent>,
}

/// Authorize synchronous question-record writes within the receiving session's owner update.
/// This grants no native execution authority and cannot be constructed by production RPC handlers.
pub(crate) struct QuestionUpdatePermit<'step> {
    session: &'step str,
}

impl<'step> QuestionUpdatePermit<'step> {
    /// Get the session whose records this owner update may change.
    pub(crate) fn session(&self) -> &str {
        self.session
    }

    /// Exercise storage transactions without creating a second production mutation entry point.
    #[cfg(test)]
    pub(crate) fn for_test(session: &'step str) -> Self {
        Self { session }
    }
}

impl QuestionEvents {
    /// Commit a captured plan/workflow decision without entering a caller-command lane.
    pub(crate) async fn answer_plan(
        &self,
        answer: crate::plan::PlanQuestionAnswerRequest,
    ) -> Result<crate::plan::PlanQuestionAnswerCommit, super::QuestionEventError> {
        self.submit(|reply| OwnerEvent::PlanQuestionAnswer { answer, reply })
            .await
    }

    /// Commit a user decision or model mutation at the owner event boundary.
    pub(crate) async fn change(
        &self,
        change: super::QuestionChange,
    ) -> Result<crate::question::QuestionSnapshot, super::QuestionEventError> {
        self.submit(|reply| OwnerEvent::QuestionChange { change, reply })
            .await
    }

    /// Publish scoped model questions and return their committed identities.
    pub(crate) async fn ask(
        &self,
        creation: super::QuestionCreation,
    ) -> Result<Vec<crate::question::QuestionId>, super::QuestionEventError> {
        self.submit(|reply| OwnerEvent::QuestionCreate { creation, reply })
            .await
    }

    /// Register a waiting tool's destination before publishing its required questions.
    pub(crate) async fn begin_wait(
        &self,
        creation: super::QuestionCreation,
    ) -> Result<crate::question::OwnedQuestionWait, super::QuestionEventError> {
        self.submit(|reply| OwnerEvent::QuestionWait { creation, reply })
            .await
    }

    /// Retain an admitted event independently of a dropped waiter, with bounded intake and typed failure.
    async fn submit<T>(
        &self,
        event: impl FnOnce(oneshot::Sender<Result<T, super::QuestionEventError>>) -> OwnerEvent,
    ) -> Result<T, super::QuestionEventError> {
        let sender = self
            .sender
            .upgrade()
            .ok_or(super::QuestionEventError::OwnerClosed)?;
        let (reply, response) = oneshot::channel();
        sender.try_send(event(reply)).map_err(|error| match error {
            mpsc::error::TrySendError::Full(_) => super::QuestionEventError::QueueFull,
            mpsc::error::TrySendError::Closed(_) => super::QuestionEventError::OwnerClosed,
        })?;
        drop(sender);
        crate::admission::complete();
        response
            .await
            .map_err(|_| super::QuestionEventError::ObservationLost)?
    }
}

impl ExchangeOwner {
    /// Ask the owner to consider pending task verification without launching work in the caller.
    pub(crate) async fn task_checks_ready(
        &self,
        messages: MessageSender,
    ) -> Result<Option<InputReadiness>, InputOfferError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::TaskChecksReady { messages, reply })
            .await
            .map_err(|_| InputOfferError::OwnerClosed)?;
        response
            .await
            .map_err(|_| InputOfferError::ObservationLost)?
    }
    /// Settle initialization through the owner and attach output for either outcome.
    pub(crate) async fn initialize_provider(
        &self,
        initialized: Result<
            super::provider_lifecycle::ProviderInitialization,
            super::provider_lifecycle::ProviderLifecycleFailure,
        >,
        messages: Option<MessageSender>,
    ) -> Result<(), super::provider_lifecycle::ProviderLifecycleFailure> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::ProviderInitialized {
                initialized,
                messages,
                reply,
            })
            .await
            .map_err(|_| super::provider_lifecycle::ProviderLifecycleFailure::ControllerClosed)?;
        response
            .await
            .map_err(|_| super::provider_lifecycle::ProviderLifecycleFailure::ControllerClosed)?
    }
    /// Capture a coherent reconnect response while reserving its ordered output boundary.
    pub(crate) async fn reconnect_snapshot(
        &self,
        cursor: Option<u64>,
        limit: std::num::NonZeroU16,
    ) -> Result<super::ReconnectSnapshot, super::ReconnectError> {
        let cursor = cursor
            .map(super::publication::PublicationSequence::try_from)
            .transpose()
            .map_err(super::ReconnectError::Publication)?;
        let limit = std::num::NonZeroU16::new(limit.get().min(256))
            .expect("a positive reconnect page limit remains positive when capped");
        let (reply, response) = oneshot::channel();
        self.sender
            .try_send(OwnerEvent::ReconnectSnapshot {
                cursor,
                limit,
                reply,
            })
            .map_err(|error| match error {
                mpsc::error::TrySendError::Full(_) => super::ReconnectError::QueueFull,
                mpsc::error::TrySendError::Closed(_) => super::ReconnectError::OwnerClosed,
            })?;
        response
            .await
            .map_err(|_| super::ReconnectError::ObservationLost)?
    }

    /// Resume ordered output on the reconnecting transport only after its complete snapshot was admitted.
    pub(crate) async fn confirm_reconnect_to(
        &self,
        confirmation: super::ReconnectConfirmation,
        messages: Option<MessageSender>,
    ) -> Result<(), super::ReconnectError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .try_send(OwnerEvent::ReconnectConfirmed {
                confirmation,
                messages,
                reply,
            })
            .map_err(|error| match error {
                mpsc::error::TrySendError::Full(_) => super::ReconnectError::QueueFull,
                mpsc::error::TrySendError::Closed(_) => super::ReconnectError::OwnerClosed,
            })?;
        response
            .await
            .map_err(|_| super::ReconnectError::ObservationLost)?
    }

    /// Report permission-timeout feedback as an internal fact without acquiring execution ownership.
    pub(crate) async fn approval_timeout(&self, schedule_revision: u64) -> Result<()> {
        self.sender
            .send(OwnerEvent::ApprovalTimeout { schedule_revision })
            .await
            .map_err(|_| anyhow::anyhow!("exchange owner closed before timeout feedback"))
    }

    /// Get a non-owning event handle for scoped tool callbacks and user question decisions.
    pub(crate) fn question_events(&self) -> QuestionEvents {
        QuestionEvents {
            sender: self.sender.downgrade(),
        }
    }
    /// Create bounded owner intake before the session begins admitting work.
    pub(crate) fn channel(
        root: &std::path::Path,
        session: &str,
    ) -> Result<(Self, ExchangeDriver), CommandJournalError> {
        let commands = CommandQueue::open(root, session)?;
        let (sender, receiver) = mpsc::channel(OWNER_QUEUE_CAPACITY);
        let shutdown_retry = Arc::new(tokio::sync::Notify::new());
        let child_sender = Arc::new(std::sync::OnceLock::new());
        Ok((
            Self {
                sender,
                child_sender: child_sender.clone(),
                shutdown_retry: shutdown_retry.clone(),
            },
            ExchangeDriver {
                receiver,
                child_sender,
                shutdown_retry,
                commands,
                foreground_access: ForegroundAccess::Open,
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
            },
        ))
    }

    /// Execute caller intent through the owner FIFO and await its committed postcondition.
    /// Dropping this wait after admission never cancels the command or its owned effects.
    #[cfg(test)]
    pub(crate) async fn execute_command(
        &self,
        command: Command,
    ) -> Result<CommandResult, ExecuteCommandError> {
        let id = CommandId::new(uuid::Uuid::new_v4().to_string())
            .expect("generated command UUID is nonempty");
        self.submit_command(CommandIntent::Fresh { id, command }, None)
            .await
    }

    /// Observe automatic question admission in tests without adding a caller command or owning its work.
    #[cfg(test)]
    pub(crate) async fn reconcile_questions(
        &self,
        messages: Option<MessageSender>,
    ) -> Result<super::QuestionInputProgress, super::QuestionInputError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .try_send(OwnerEvent::QuestionsReady { messages, reply })
            .map_err(|error| match error {
                mpsc::error::TrySendError::Full(_) => super::QuestionInputError::QueueFull,
                mpsc::error::TrySendError::Closed(_) => super::QuestionInputError::OwnerClosed,
            })?;
        response
            .await
            .map_err(|_| super::QuestionInputError::ObservationLost)?
    }

    /// Preserve a legacy RPC's durable identity while using the same FIFO and resolvers.
    pub(crate) async fn execute_identified(
        &self,
        request: CommandRequest,
        messages: MessageSender,
    ) -> Result<CommandResult, ExecuteCommandError> {
        self.submit_command(CommandIntent::Identified(request), Some(messages))
            .await
    }

    /// Query retained results independently of the active command and its response waiter.
    pub(crate) async fn command_status(
        &self,
        id: CommandId,
    ) -> Result<Option<CommandRecord>, ExecuteCommandError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .try_send(OwnerEvent::Status { id, reply })
            .map_err(|error| match error {
                mpsc::error::TrySendError::Full(_) => ExecuteCommandError::QueueFull,
                mpsc::error::TrySendError::Closed(_) => ExecuteCommandError::OwnerClosed,
            })?;
        response
            .await
            .map_err(|_| ExecuteCommandError::ObservationLost)?
    }

    /// Submit bounded caller intake without exposing tickets to ordinary callers.
    async fn submit_command(
        &self,
        intent: CommandIntent,
        messages: Option<MessageSender>,
    ) -> Result<CommandResult, ExecuteCommandError> {
        let (id, command) = match &intent {
            #[cfg(test)]
            CommandIntent::Fresh { id, command } => (id, command),
            CommandIntent::Identified(request) => (&request.id, &request.command),
        };
        let task_admission = if matches!(command, Command::Agent(command) if command.creates_task())
            && self.command_status(id.clone()).await?.is_none()
        {
            let children = self
                .child_sender
                .get()
                .ok_or(ExecuteCommandError::OwnerClosed)?;
            tokio::select! {
                permit = children.acquire_task_admission(id.as_str()) => permit.map_err(|_| ExecuteCommandError::OwnerClosed)?,
                _ = self.sender.closed() => return Err(ExecuteCommandError::OwnerClosed),
            }
        } else {
            None
        };
        let (reply, response) = oneshot::channel();
        self.sender
            .try_send(OwnerEvent::Command {
                intent: Box::new(intent),
                task_admission,
                messages,
                reply,
            })
            .map_err(|error| match error {
                mpsc::error::TrySendError::Full(_) => ExecuteCommandError::QueueFull,
                mpsc::error::TrySendError::Closed(_) => ExecuteCommandError::OwnerClosed,
            })?;
        crate::admission::complete();
        response
            .await
            .map_err(|_| ExecuteCommandError::ObservationLost)?
    }

    /// Admit one dedicated review answer while retaining preparation and final annotation ownership.
    pub(crate) async fn review_question(
        &self,
        scope: Arc<crate::plan::review_answer::ReviewAnswerScope>,
        selection: crate::settings::AskModel,
        catalog: Option<crate::broker::ConfigurationCatalogResult>,
        sink: crate::broker::publication::SessionUpdateSink,
        messages: MessageSender,
        request_id: u64,
    ) -> Result<DispatchResult> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::ReviewQuestion {
                scope,
                selection,
                catalog,
                sink,
                messages,
                request_id,
                reply,
            })
            .await
            .map_err(|_| anyhow::anyhow!("review answer owner closed before admission"))?;
        crate::admission::complete();
        response
            .await
            .context("review answer owner closed before completion")?
    }

    /// Offer eligible new-turn input; the owner claims and executes it independently of this waiter.
    #[cfg(test)]
    pub(crate) async fn offer_input(
        &self,
        offer: InputDeliveryOffer,
        messages: MessageSender,
    ) -> Result<bool, InputOfferError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::InputOffer {
                offer,
                messages,
                reply: InputOfferReply::Offered(reply),
            })
            .await
            .map_err(|_| InputOfferError::OwnerClosed)?;
        response
            .await
            .map_err(|_| InputOfferError::ObservationLost)?
    }

    /// Offer steering without claiming it or owning its subsequent transport lifetime.
    #[cfg(test)]
    pub(crate) async fn offer_steering(
        &self,
        offer: SteeringDeliveryOffer,
        messages: MessageSender,
    ) -> Result<bool, InputOfferError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::SteeringOffer {
                offer,
                messages,
                reply: InputOfferReply::Offered(reply),
            })
            .await
            .map_err(|_| InputOfferError::OwnerClosed)?;
        response
            .await
            .map_err(|_| InputOfferError::ObservationLost)?
    }

    /// Apply local directive completion only to the exact queued revision that produced it.
    pub(crate) async fn settle_local_input(
        &self,
        input: super::command::InputRevision,
        outcome: Result<(), CommandFailure>,
        messages: MessageSender,
    ) -> Result<(), InputOfferError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::LocalInputSettled {
                input,
                outcome,
                messages,
                reply,
            })
            .await
            .map_err(|_| InputOfferError::OwnerClosed)?;
        response
            .await
            .map_err(|_| InputOfferError::ObservationLost)?
    }

    /// Reconcile committed task input through the owner without granting this observer mutation authority.
    pub(crate) async fn reconcile_task_input(
        &self,
        messages: MessageSender,
    ) -> Result<(), InputOfferError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::TaskInputReady {
                id: InputReadinessId(uuid::Uuid::new_v4()),
                messages,
                reply,
            })
            .await
            .map_err(|_| InputOfferError::OwnerClosed)?;
        response
            .await
            .map_err(|_| InputOfferError::ObservationLost)?
    }

    /// Wake input scheduling without selecting an input, route, or execution capability.
    /// The owner reads current queue state and retains every admitted effect independently.
    pub(crate) async fn input_ready(
        &self,
        messages: MessageSender,
    ) -> Result<InputReadiness, InputOfferError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::InputReady {
                id: InputReadinessId(uuid::Uuid::new_v4()),
                messages,
                reply,
            })
            .await
            .map_err(|_| InputOfferError::OwnerClosed)?;
        response
            .await
            .map_err(|_| InputOfferError::ObservationLost)?
    }

    /// Recover only the selected observation generation through owner-retained stages.
    pub(crate) async fn recover_observation(
        &self,
        inbox: Arc<crate::provider::session_execution::SessionObservationInbox>,
    ) -> Result<(), super::provider_lifecycle::ProviderLifecycleFailure> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::RecoverObservation { inbox, reply })
            .await
            .map_err(|_| super::provider_lifecycle::ProviderLifecycleFailure::ControllerClosed)?;
        response.await.map_err(|_| {
            super::provider_lifecycle::ProviderLifecycleFailure::ObservationUnavailable
        })?
    }

    /// Commit provider observations without requiring a live subscriber or output transport.
    pub(crate) async fn observe_provider_retained(
        &self,
        messages: Option<MessageSender>,
    ) -> Result<(), ProviderObservationError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::ProviderReady { messages, reply })
            .await
            .map_err(|_| ProviderObservationError::OwnerClosed)?;
        response
            .await
            .map_err(|_| ProviderObservationError::ObservationLost)?
    }

    /// Apply an internal input receipt fact and await its host commit acknowledgment.
    /// This is not a caller command and cannot recursively occupy the command FIFO.
    #[cfg(test)]
    pub(crate) async fn observe_input_receipt(
        &self,
        fact: InputReceiptFact,
        messages: Option<MessageSender>,
    ) -> Result<Option<crate::input::InputUpdate>, InputReceiptError> {
        let (reply, response) = oneshot::channel();
        self.sender
            .send(OwnerEvent::InputReceipt {
                fact,
                messages,
                reply,
            })
            .await
            .map_err(|_| InputReceiptError::OwnerClosed)?;
        response
            .await
            .map_err(|_| InputReceiptError::ObservationLost)?
    }

    /// Close intake and let admitted work settle before the driver ends.
    pub(crate) async fn close(&self) {
        self.shutdown_retry.notify_one();
        let _ = self.sender.send(OwnerEvent::Close).await;
    }
}

/// Hold the unique receiver and advance command and foreground state through update steps.
pub(crate) struct ExchangeDriver {
    receiver: mpsc::Receiver<OwnerEvent>,
    child_sender: Arc<std::sync::OnceLock<crate::agent::AgentCommandSender>>,
    shutdown_retry: Arc<tokio::sync::Notify>,
    commands: CommandQueue,
    foreground_access: ForegroundAccess,
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
    /// Select task delivery from owned work, rather than inferring it from semaphore availability.
    fn task_delivery_gate(
        &self,
        foreground: Option<&ForegroundRuntime>,
        pending: &VecDeque<PreparedForeground>,
    ) -> super::task_command::TaskDeliveryGate {
        if matches!(
            self.foreground_access,
            ForegroundAccess::Unavailable(_) | ForegroundAccess::CollectionLost
        ) {
            return super::task_command::TaskDeliveryGate::Unavailable;
        }
        if self.input_delivery.is_none()
            && self.steering.is_empty()
            && pending.is_empty()
            && foreground.is_none()
            && matches!(self.foreground_access, ForegroundAccess::Open)
        {
            super::task_command::TaskDeliveryGate::Open
        } else {
            super::task_command::TaskDeliveryGate::Waiting
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
            let result = super::reconnect::capture(
                &*broker.lock().await,
                services,
                request.cursor,
                request.limit,
                step,
                !self.commands.storage_unavailable()
                    && matches!(self.foreground_access, ForegroundAccess::Open),
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
        self.child_sender.get_or_init(|| services.children.sender());
        self.commands
            .bind_publications(services.publications.clone());
        let mut pending = VecDeque::new();
        let mut foreground: Option<ForegroundRuntime> = None;
        let mut command: Option<ActiveCommand> = None;
        let mut command_messages: Option<MessageSender> = None;
        let mut publication_delivery = super::publication::PublicationDelivery::default();
        let mut publication_recovery_at = None;
        let mut deferred_publications = DeferredPublications::default();
        let mut failed_foregrounds = JoinSet::new();
        let mut cancellation_cleanup = JoinSet::new();
        let mut rollback_cleanup = JoinSet::new();
        let mut internal_completions = JoinSet::new();
        let mut input_cleanup: JoinSet<CollectedInput> = JoinSet::new();
        let mut retained_inputs = Vec::new();
        let mut retained_cancellation: Option<RetainedTransition> = None;
        let mut retained_rollbacks: Vec<RollbackRecovery> = Vec::new();
        let mut steering_ready = None;
        let mut closed = false;
        let mut shutdown: Option<super::shutdown::OwnerShutdown> = None;
        let mut update_step = 0_u64;
        let mut continuing_event_phase = false;
        let mut event_budget = OWNER_INTAKE_BUDGET;
        let mut command_at_step_entry = false;
        let mut pending_wake = None;
        let mut question_changes = services.questions.changed.subscribe();
        let (check_sender, mut check_events) = crate::checks::owner::CheckOwnerSender::channel();
        let mut check_runs = crate::checks::owner::CheckOwnerRuns::new(services.checks.clone());
        let mut check_scope: Option<crate::provider::InvocationScope> = None;
        let mut pending_check_tools: VecDeque<crate::checks::owner::ToolCheckRequest> =
            VecDeque::new();
        // Tool-started runs outlive the provider turn that requested them, so their progress and
        // configuration errors publish through this owner-lifetime stream rather than the turn sink.
        let (tool_check_sink, mut tool_check_updates) = crate::broker::publication::channel();
        let mut pending_automatic_checks: VecDeque<crate::checks::owner::AutomaticCheckRequest> =
            VecDeque::new();
        let mut automatic_checks: Option<crate::broker::OwnedCheckCompletion> = None;
        let mut automatic_action: Option<crate::broker::OwnedCheckProgress> = None;
        let mut automatic_publication: Option<AutomaticPublication> = None;
        let mut automatic_repair: Option<oneshot::Receiver<Result<DispatchResult>>> = None;
        let mut task_verification: Option<TaskVerification> = None;
        let mut task_check_updates: Option<crate::broker::publication::SessionUpdateStream> = None;
        let mut task_check_completion: Option<(
            Vec<forge_protocol::message::SessionEvent>,
            Option<crate::checks::owner::CheckOwnerFailure>,
        )> = None;
        if let Err(error) = services.checks.install_owner(check_sender) {
            services.execution.close();
            self.foreground_access = ForegroundAccess::CollectionLost;
            trace.record(
                &session_id,
                "exchange.check.owner_unavailable",
                serde_json::json!({"reason":error.to_string()}),
            );
        }
        if let Err(error) = services.publications.recover_interrupted_offers() {
            services.execution.close();
            self.foreground_access = ForegroundAccess::CollectionLost;
            trace.record(
                &session_id,
                "exchange.publication.recovery_failed",
                serde_json::json!({"update_step":update_step,"reason":error.to_string()}),
            );
        }
        match self.commands.restore_task_delivery(&services) {
            Ok(Some(update)) => {
                trace.record(
                    &session_id,
                    "exchange.task_input.recovery_paused",
                    serde_json::json!({
                        "update_step":update_step,"command_id":update.operation_id,
                    }),
                );
                publish_input_observation(
                    update,
                    None,
                    &services,
                    &trace,
                    &session_id,
                    update_step,
                );
            }
            Ok(None) => {}
            Err(error) => {
                services.execution.close();
                self.foreground_access = ForegroundAccess::CollectionLost;
                trace.record(
                    &session_id,
                    "exchange.task_input.recovery_failed",
                    serde_json::json!({
                        "update_step":update_step,"reason":error.to_string(),
                    }),
                );
            }
        }
        match self.input_receipts.restore(&services) {
            Ok(updates) => {
                for update in updates {
                    publish_input_observation(
                        update,
                        None,
                        &services,
                        &trace,
                        &session_id,
                        update_step,
                    );
                }
            }
            Err(error) => {
                services.execution.close();
                self.foreground_access = ForegroundAccess::CollectionLost;
                trace.record(
                    &session_id,
                    "exchange.input_receipt.recovery_failed",
                    serde_json::json!({
                        "update_step":update_step,"reason":error.to_string(),
                    }),
                );
            }
        }
        loop {
            if !continuing_event_phase {
                update_step = update_step
                    .checked_add(1)
                    .expect("exchange update step exhausted");
                event_budget = OWNER_INTAKE_BUDGET;
                command_at_step_entry = command.is_some();
            }
            let activity = match self.foreground_access {
                ForegroundAccess::Unavailable(_) | ForegroundAccess::CollectionLost => {
                    super::ActivityPhase::Unavailable
                }
                ForegroundAccess::Collecting => super::ActivityPhase::Working,
                ForegroundAccess::Open
                    if foreground.is_some()
                        || command.is_some()
                        || !pending.is_empty()
                        || self.commands.has_pending()
                        || self.input_delivery.is_some()
                        || !self.steering.is_empty()
                        || !input_cleanup.is_empty()
                        || !failed_foregrounds.is_empty()
                        || !cancellation_cleanup.is_empty()
                        || !rollback_cleanup.is_empty()
                        || automatic_checks.is_some()
                        || automatic_repair.is_some()
                        || task_verification.is_some()
                        || !check_runs.is_empty() =>
                {
                    super::ActivityPhase::Working
                }
                ForegroundAccess::Open => super::ActivityPhase::Idle,
            };
            if let Err(error) = publish_activity(
                activity,
                command_messages.as_ref(),
                &services,
                &trace,
                &session_id,
                update_step,
            ) {
                self.commands
                    .retention_failed(&trace, &session_id, update_step);
                services.execution.close();
                trace.record(
                    &session_id,
                    "exchange.activity.publication_failed",
                    serde_json::json!({"reason":error.to_string(),"update_step":update_step}),
                );
            }
            if closed
                && command.is_none()
                && shutdown.is_none()
                && !self.observation_recovery.active()
            {
                shutdown = Some(super::shutdown::OwnerShutdown::start(&services));
            }
            if closed {
                self.question_waits.close();
                for reply in self.task_check_requests.drain(..) {
                    let _ = reply.send(Err(InputOfferError::OwnerClosed));
                }
                let _ = check_runs.cancel_all();
                if let Some(checks) = automatic_checks.as_mut() {
                    let _ = checks.cancel();
                }
                if let Some(checks) = task_verification.as_mut() {
                    let _ = checks.run.cancel();
                }
                automatic_action = None;
                for request in pending_automatic_checks.drain(..) {
                    let _ = request
                        .reply
                        .send(Err(crate::checks::owner::CheckOwnerFailure::Unavailable));
                }
            }
            if let Err(error) =
                self.question_waits
                    .update(&services, &trace, &session_id, update_step)
            {
                if error.error().requires_storage_fence() {
                    self.commands
                        .retention_failed(&trace, &session_id, update_step);
                    services.execution.close();
                }
                trace.record(&session_id, "exchange.question_wait.retirement_failed", serde_json::json!({
                    "wait_id":error.wait_id().to_string(),"error_code":error.error().code(),"reason":error.to_string(),"update_step":update_step,
                }));
            }
            let foreground_at_entry = foreground.is_some();
            let command_at_entry = command_at_step_entry;
            let intake = if continuing_event_phase || pending_wake.is_some() {
                0
            } else {
                OWNER_INTAKE_BUDGET.min(OWNER_QUEUE_CAPACITY - pending.len())
            };
            for _ in 0..intake {
                match self.receiver.try_recv() {
                    Ok(event) => {
                        self.receive(
                            event,
                            &mut pending,
                            OwnerIntake {
                                active: command.as_mut(),
                                steering: SteeringAdmission::from(foreground.as_ref()),
                            },
                            &mut command_messages,
                            &broker,
                            OwnerUpdateContext {
                                services: &services,
                                trace: &trace,
                                session: &session_id,
                                step: update_step,
                            },
                        )
                        .await
                    }
                    Err(mpsc::error::TryRecvError::Empty) => break,
                    Err(mpsc::error::TryRecvError::Disconnected) => {
                        closed = true;
                        break;
                    }
                }
            }
            if services.publications.storage_failed() {
                self.commands
                    .retention_failed(&trace, &session_id, update_step);
            }
            if services.publications.has_recovery()
                && publication_recovery_at
                    .is_none_or(|deadline| tokio::time::Instant::now() >= deadline)
            {
                if let Err(error) = services.publications.retry_recovery() {
                    trace.record(
                        &session_id,
                        "exchange.publication.recovery_failed",
                        serde_json::json!({"reason":error.to_string(),"update_step":update_step}),
                    );
                }
                publication_recovery_at = services
                    .publications
                    .has_recovery()
                    .then(|| tokio::time::Instant::now() + std::time::Duration::from_millis(250));
            }
            if !deferred_publications.updates.is_empty() {
                publish_updates(
                    &mut deferred_publications,
                    &mut self.commands,
                    Vec::new(),
                    command_messages.as_ref(),
                    OwnerUpdateContext {
                        services: &services,
                        trace: &trace,
                        session: &session_id,
                        step: update_step,
                    },
                )
                .await;
            }
            if self.commands.storage_unavailable()
                && let Some(command) = command.as_mut()
            {
                self.commands.lose_observation(
                    command.queued_mut(),
                    &trace,
                    &session_id,
                    update_step,
                );
            }
            if self.commands.storage_unavailable()
                && let Some(runtime) = foreground.take()
            {
                services.execution.close();
                self.foreground_access = ForegroundAccess::Collecting;
                let (provider, permissions) = {
                    let broker = broker.lock().await;
                    (broker.provider_handle(), broker.permission_coordinator())
                };
                failed_foregrounds.spawn(collect_failed_foreground(
                    runtime,
                    anyhow::anyhow!(
                        "foreground state cannot be committed after owner storage failure"
                    ),
                    provider,
                    permissions,
                    session_id.clone(),
                    trace.clone(),
                ));
            }
            if self.commands.storage_unavailable() {
                services.execution.close();
                if matches!(self.foreground_access, ForegroundAccess::Open) {
                    self.foreground_access = ForegroundAccess::CollectionLost;
                }
                for prepared in pending.drain(..) {
                    notify(
                        prepared.completion,
                        prepared.id,
                        Err(anyhow::anyhow!(
                            "foreground admission is unavailable after owner storage failure"
                        )),
                        &trace,
                        &session_id,
                        update_step,
                    );
                }
            }
            if (closed || self.commands.storage_unavailable()) && !self.question_tools.is_empty() {
                for _ in 0..OWNER_INTAKE_BUDGET {
                    let Some(request) = self.question_tools.pop_front() else {
                        break;
                    };
                    request.apply(
                        super::question_tool::QuestionToolAdmission::Closed,
                        &services,
                        &mut self.question_waits,
                        &trace,
                        &session_id,
                        update_step,
                    );
                }
            }
            if !closed && !self.commands.storage_unavailable() {
                match self
                    .question_inputs
                    .update(&services, command_messages.as_ref())
                {
                    Ok(Some(progress)) => {
                        trace.record(&session_id, "exchange.question_input.applied", serde_json::json!({
                        "question_revision":progress.revision,"admitted":progress.admitted,"remaining":progress.remaining,"update_step":update_step,
                    }));
                    }
                    Ok(None) => {}
                    Err(error) => {
                        trace.record(&session_id, "exchange.question_input.failed", serde_json::json!({
                        "error_code":error.code(),"reason":error.to_string(),"update_step":update_step,
                    }));
                    }
                }
            }
            let publication_transport = self
                .reconnect_requests
                .is_empty()
                .then_some(command_messages.as_ref())
                .flatten();
            match publication_delivery.update(&services.publications, publication_transport) {
                Ok(Some(progress)) => {
                    use super::publication::PublicationProgress;
                    let (sequence, delivery) = match progress {
                        PublicationProgress::Offering(sequence) => (sequence, "offering"),
                        PublicationProgress::Admitted(sequence) => (sequence, "admitted"),
                        PublicationProgress::Uncertain(sequence) => (sequence, "uncertain"),
                    };
                    trace.record(&session_id, "exchange.publication.delivery", serde_json::json!({
                        "publication_sequence":sequence.get(),"delivery":delivery,"update_step":update_step,
                    }));
                }
                Ok(None) => {}
                Err(error) => {
                    trace.record(
                        &session_id,
                        "exchange.publication.delivery_failed",
                        serde_json::json!({
                            "update_step":update_step,"reason":error.to_string(),
                        }),
                    );
                }
            }
            self.advance_reconnect(
                &broker,
                OwnerUpdateContext {
                    services: &services,
                    trace: &trace,
                    session: &session_id,
                    step: update_step,
                },
            )
            .await;
            if steering_ready.is_none()
                && !matches!(
                    &pending_wake,
                    Some(DriverWake::InputDelivery(
                        InputDeliveryTarget::Steering(_),
                        _
                    ))
                )
            {
                let status = foreground_status(
                    &self.foreground_access,
                    foreground
                        .as_ref()
                        .map_or(Ok(None), |runtime| runtime.transaction.invocation_scope()),
                    foreground.is_some() || !pending.is_empty() || !self.steering.is_empty(),
                );
                steering_ready = next_steering(&mut self.steering, status).now_or_never();
            }
            if closed {
                self.observation_recovery.close_pending();
            }
            // Apply a bounded ready-event prefix before admitting this update's command effects.
            if let Some(runtime) = foreground.as_mut() {
                if let Some(command) = command.as_mut() {
                    while let Some(signal) = command.take_collection_signal() {
                        runtime.collection_signals.push_back(signal);
                    }
                }
                if let Some(shutdown) = shutdown.as_mut() {
                    while let Some(signal) = shutdown.take_collection_signal() {
                        runtime.collection_signals.push_back(signal);
                    }
                }
            }
            let event_status = foreground_status(
                &self.foreground_access,
                foreground
                    .as_ref()
                    .map_or(Ok(None), |runtime| runtime.transaction.invocation_scope()),
                foreground.is_some()
                    || !pending.is_empty()
                    || !self.steering.is_empty()
                    || !input_cleanup.is_empty()
                    || !check_runs.is_empty()
                    || task_verification.is_some(),
            );
            let task_delivery = self.task_delivery_gate(foreground.as_ref(), &pending);
            // Apply the owner's collection barrier before a previously buffered job result.
            // Native cleanup can already have cancelled that job after the command committed.
            let mut ready_wake = foreground
                .as_mut()
                .and_then(|runtime| runtime.collection_signals.pop_front())
                .map(|signal| {
                    DriverWake::Foreground(ForegroundInput::Signal(Box::new(signal), false))
                })
                .or_else(|| pending_wake.take());
            if ready_wake.is_none() && event_budget > 0 {
                ready_wake = foreground
                    .as_mut()
                    .and_then(|runtime| runtime.next().now_or_never())
                    .map(DriverWake::Foreground);
                if ready_wake.is_none() {
                    ready_wake = command
                        .as_mut()
                        .and_then(|command| {
                            command
                                .next(event_status.clone(), task_delivery)
                                .now_or_never()
                        })
                        .map(DriverWake::Command);
                }
                if ready_wake.is_none() {
                    ready_wake = steering_ready.take().map(|(id, event)| {
                        DriverWake::InputDelivery(InputDeliveryTarget::Steering(id), event)
                    });
                }
                if ready_wake.is_none() {
                    ready_wake = self
                        .input_delivery
                        .as_mut()
                        .and_then(|run| run.next(event_status.clone()).now_or_never())
                        .map(|event| {
                            DriverWake::InputDelivery(InputDeliveryTarget::Foreground, event)
                        });
                }
                if ready_wake.is_none() {
                    ready_wake = self
                        .input_receipts
                        .next()
                        .now_or_never()
                        .map(DriverWake::InputReceipt);
                }
                if ready_wake.is_none() && !check_runs.is_empty() {
                    ready_wake = check_runs
                        .next()
                        .now_or_never()
                        .map(DriverWake::CheckCollected);
                }
                if ready_wake.is_none() && task_check_completion.is_none() {
                    ready_wake = task_verification
                        .as_mut()
                        .filter(|checks| checks.run.has_worker())
                        .and_then(|checks| checks.run.next().now_or_never())
                        .map(DriverWake::TaskCheck);
                }
                if ready_wake.is_none() {
                    ready_wake = automatic_checks
                        .as_mut()
                        .filter(|checks| checks.has_worker())
                        .and_then(|checks| checks.next().now_or_never())
                        .map(DriverWake::AutomaticCheck);
                }
                if ready_wake.is_none() {
                    ready_wake = shutdown
                        .as_mut()
                        .and_then(|shutdown| shutdown.next(event_status.clone()).now_or_never())
                        .map(DriverWake::Shutdown);
                }
                if ready_wake.is_none()
                    && let Some(updates) = automatic_publication
                        .as_mut()
                        .map(AutomaticPublication::stream)
                {
                    ready_wake = match updates.try_recv() {
                        Ok(update) => Some(DriverWake::AutomaticUpdate(Ok(Some(update)))),
                        Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => None,
                        Err(error) if error.kind() == std::io::ErrorKind::UnexpectedEof => {
                            Some(DriverWake::AutomaticUpdate(Ok(None)))
                        }
                        Err(error) => Some(DriverWake::AutomaticUpdate(Err(error))),
                    };
                }
                if ready_wake.is_none()
                    && let Some(updates) = task_check_updates.as_mut()
                {
                    ready_wake = match updates.try_recv() {
                        Ok(update) => Some(DriverWake::TaskCheckUpdate(Ok(Some(update)))),
                        Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => None,
                        Err(error) if error.kind() == std::io::ErrorKind::UnexpectedEof => {
                            Some(DriverWake::TaskCheckUpdate(Ok(None)))
                        }
                        Err(error) => Some(DriverWake::TaskCheckUpdate(Err(error))),
                    };
                }
                if ready_wake.is_none() && !failed_foregrounds.is_empty() {
                    ready_wake = failed_foregrounds
                        .join_next()
                        .now_or_never()
                        .map(DriverWake::FailedForeground);
                }
                if ready_wake.is_none() && !cancellation_cleanup.is_empty() {
                    ready_wake = cancellation_cleanup
                        .join_next()
                        .now_or_never()
                        .map(DriverWake::CancellationCollected);
                }
                if ready_wake.is_none() && !rollback_cleanup.is_empty() {
                    ready_wake = rollback_cleanup
                        .join_next()
                        .now_or_never()
                        .map(DriverWake::RollbackCollected);
                }
                if ready_wake.is_none() && !input_cleanup.is_empty() {
                    ready_wake = input_cleanup
                        .join_next()
                        .now_or_never()
                        .map(DriverWake::InputCollected);
                }
                if ready_wake.is_none() && !internal_completions.is_empty() {
                    ready_wake = internal_completions
                        .join_next()
                        .now_or_never()
                        .map(DriverWake::InternalCompleted);
                }
                if ready_wake.is_none() {
                    ready_wake = check_events
                        .try_recv()
                        .ok()
                        .map(|event| DriverWake::CheckEvent(Some(event)));
                }
            }
            let mut broker_deferred = false;
            continuing_event_phase = ready_wake.is_some();
            if continuing_event_phase {
                event_budget = event_budget.saturating_sub(1);
            }
            if ready_wake.is_none() && deferred_publications.updates.is_empty() {
                if self.observation_recovery.active() {
                    let quiescent = command.is_none()
                        && foreground.is_none()
                        && pending.is_empty()
                        && self.input_delivery.is_none()
                        && self.steering.is_empty()
                        && input_cleanup.is_empty()
                        && check_runs.is_empty()
                        && automatic_checks.is_none()
                        && automatic_repair.is_none()
                        && task_verification.is_none()
                        && task_check_completion.is_none()
                        && failed_foregrounds.is_empty()
                        && cancellation_cleanup.is_empty()
                        && rollback_cleanup.is_empty()
                        && internal_completions.is_empty();
                    let progress = if self.commands.storage_unavailable() {
                        self.observation_recovery.fail_storage();
                        self.foreground_access = ForegroundAccess::CollectionLost;
                        Ok(false)
                    } else if quiescent || self.observation_recovery.started() {
                        let mut state = broker.lock().await;
                        self.observation_recovery
                            .advance(quiescent, &mut state, &services)
                    } else {
                        Ok(false)
                    };
                    match progress {
                        Ok(true) => {}
                        Ok(false)
                            if self.observation_recovery.started()
                                && !self.commands.storage_unavailable() =>
                        {
                            self.foreground_access = ForegroundAccess::Collecting
                        }
                        Ok(false) => {}
                        Err(error) => {
                            trace.record(&session_id, "exchange.observation_recovery.failed", serde_json::json!({"reason":error.to_string(),"update_step":update_step}));
                            services.tasks.mark_configuration_uncertain();
                            self.observation_recovery.finish(Err(error));
                            self.foreground_access = ForegroundAccess::CollectionLost;
                        }
                    }
                }
                // Start only one command per update step, including immediate rejection.
                if !command_at_entry
                    && !self.observation_recovery.active()
                    && command.is_none()
                    && !self
                        .input_delivery
                        .as_ref()
                        .is_some_and(InputDeliveryRun::preparing)
                    && let Some(queued) = self.commands.start(&trace, &session_id, update_step)
                {
                    if !matches!(self.foreground_access, ForegroundAccess::Open)
                        && matches!(queued.record.request.command, Command::CancelExchange(_))
                    {
                        self.commands.finish(
                            queued,
                            Err(CommandFailure::CollectionUncertain),
                            &trace,
                            &session_id,
                            update_step,
                        );
                    } else {
                        let record = super::core_log::CoreRecord::OperationStarted {
                            operation: super::core_log::CoreOperation::Command {
                                command: queued.record.request.id.clone(),
                                kind: (&queued.record.request.command).into(),
                            },
                        };
                        if record_owner_core(
                            &mut self.commands,
                            &record,
                            &trace,
                            &session_id,
                            update_step,
                        )
                        .is_ok()
                        {
                            if matches!(
                                &queued.record.request.command,
                                Command::ConfigureSession(
                                    super::command::ConfigurationChange::Settings(_)
                                )
                            ) {
                                self.deferred_configuration = None;
                                self.configuration_attempt = Some(ConfigurationAttempt {
                                    command: queued.record.request.id.clone(),
                                    availability: services
                                        .observer
                                        .session_events()
                                        .configuration_availability(),
                                });
                            }
                            let mut active = ActiveCommand::start(queued, &services);
                            let pin = active
                                .pin_cancellation_target(
                                    &event_status,
                                    foreground.as_mut().map(|runtime| &mut runtime.transaction),
                                    &broker,
                                    &services,
                                )
                                .await;
                            match pin {
                                Ok((events, input)) => {
                                    let id = active.queued_mut().record.request.id.clone();
                                    publish_control(
                                        events,
                                        input.as_ref(),
                                        OwnerUpdateContext {
                                            services: &services,
                                            trace: &trace,
                                            session: &session_id,
                                            step: update_step,
                                        },
                                        &id,
                                    );
                                }
                                Err(_) => {
                                    services.execution.fence_collection_pending();
                                    self.foreground_access = ForegroundAccess::CollectionLost;
                                    self.commands.retention_failed(
                                        &trace,
                                        &session_id,
                                        update_step,
                                    );
                                }
                            }
                            command = Some(active);
                        } else {
                            let mut queued = queued;
                            self.commands.lose_observation(
                                &mut queued,
                                &trace,
                                &session_id,
                                update_step,
                            );
                        }
                    }
                }
                let question_gate_open = !closed
                    && !self.receiver.is_closed()
                    && !self.commands.storage_unavailable()
                    && !self.observation_recovery.active()
                    && matches!(self.foreground_access, ForegroundAccess::Open)
                    && !command
                        .as_ref()
                        .is_some_and(ActiveCommand::closes_tool_admission);
                if let Some(index) = self.question_tools.iter().position(|request| {
                    !question_gate_open || !request.waits_for_capacity(&self.question_waits)
                }) {
                    let request = self
                        .question_tools
                        .remove(index)
                        .expect("selected runnable question callback exists");
                    let gate_open = question_gate_open;
                    let live_scope =
                        foreground
                            .as_ref()
                            .and_then(|runtime| match &runtime.transaction {
                                ForegroundDispatch::Running { transaction, .. }
                                    if transaction.collection.is_none()
                                        && !transaction.failure_cleanup_active =>
                                {
                                    runtime
                                        .transaction
                                        .dispatch_observer()
                                        .map(|(scope, _)| scope)
                                }
                                ForegroundDispatch::Running { .. }
                                | ForegroundDispatch::Admitting(_)
                                | ForegroundDispatch::Finalizing { .. }
                                | ForegroundDispatch::Done(_)
                                | ForegroundDispatch::Transitioning => None,
                            });
                    let admission = if gate_open {
                        super::question_tool::QuestionToolAdmission::Open {
                            scope: live_scope.as_ref(),
                        }
                    } else {
                        super::question_tool::QuestionToolAdmission::Closed
                    };
                    if request.apply(
                        admission,
                        &services,
                        &mut self.question_waits,
                        &trace,
                        &session_id,
                        update_step,
                    ) {
                        self.question_inputs.request();
                        services.input.changed.notify_one();
                    }
                }
                // Let the next admitted immediate input collect checks through the owner before delivery is retried.
                // An in-flight input delivery does not exclude this: a delivery whose turn waits on
                // `check_run` stays open while the run is active, and the run's collection is what
                // lets that turn and the direct prompt proceed.
                if !closed
                    && command.is_none()
                    && !self.commands.has_pending()
                    && !self.commands.storage_unavailable()
                    && matches!(self.foreground_access, ForegroundAccess::Open)
                {
                    let interrupt = services
                        .input
                        .snapshot()
                        .map_err(InputOfferError::Queue)
                        .and_then(|snapshot| {
                            // A direct prompt interrupts checks even behind explicitly queued
                            // drafts, which wait for completion and must not hide it.
                            super::input_readiness::select_pending_input(
                                snapshot,
                                &services.questions,
                                super::input_readiness::InputQueuePolicy::AllowSteering,
                            )
                        });
                    if let Ok(super::input_readiness::PendingInput::Ready(selection)) = interrupt {
                        let (input, directive) = selection.into_parts();
                        if directive.is_none()
                            && interrupt_checks_for_direct_input(
                                &input,
                                &services,
                                &trace,
                                &session_id,
                                update_step,
                            )
                            && let Some(checks) = automatic_checks.as_mut()
                        {
                            let _ = checks.cancel();
                        }
                    }
                }
                if command
                    .as_ref()
                    .is_some_and(ActiveCommand::closes_tool_admission)
                    && let Some(checks) = task_verification.as_mut()
                {
                    let _ = checks.run.cancel();
                }
                // Admit task verification from durable task/input authority only after caller-command admission.
                if !self.task_check_requests.is_empty() {
                    let mut readiness = Some(InputReadiness::Waiting);
                    let relevant = services.tasks.current().ok().flatten().is_some_and(|task| {
                        task.next_action() == crate::task::TaskNextAction::Checks
                    });
                    if task_verification.is_none() && !relevant {
                        readiness = None;
                    } else if task_verification.is_none()
                        && !closed
                        && command.is_none()
                        && !self.commands.has_pending()
                        && foreground.is_none()
                        && pending.is_empty()
                        && self.input_delivery.is_none()
                        && self.steering.is_empty()
                        && automatic_checks.is_none()
                        && check_runs.is_empty()
                        && !self.commands.storage_unavailable()
                        && matches!(self.foreground_access, ForegroundAccess::Open)
                    {
                        let allowed = services
                            .input
                            .snapshot()
                            .map_err(InputOfferError::Queue)
                            .and_then(|input| {
                                if input.suspended
                                    || input.messages.iter().any(|message| {
                                        matches!(message.purpose, crate::input::InputPurpose::User)
                                            && !matches!(
                                                message.delivery(),
                                                crate::input::InputDelivery::Accepted
                                                    | crate::input::InputDelivery::Delivering
                                            )
                                    })
                                {
                                    return Ok(false);
                                }
                                Ok(!super::input_readiness::questions_block_task_work(
                                    &input,
                                    &services.questions,
                                )?)
                            });
                        match allowed {
                            Ok(true) => {
                                if let Ok(control) = services.control.clone().try_acquire_owned() {
                                    match services.execution.clone().try_acquire_owned() {
                                        Ok(execution) => match broker.try_lock() {
                                            Ok(mut broker) => {
                                                let (sink, updates) =
                                                    crate::broker::publication::channel();
                                                match crate::broker::OwnedTaskCheckRun::prepare(
                                                    &mut broker,
                                                    sink,
                                                ) {
                                                    Ok(Some(run)) => {
                                                        task_verification =
                                                            Some(TaskVerification {
                                                                run,
                                                                _control: control,
                                                                _execution: execution,
                                                            });
                                                        task_check_updates = Some(updates);
                                                        readiness = Some(InputReadiness::Started);
                                                    }
                                                    Ok(None) => readiness = None,
                                                    Err(error) => {
                                                        trace.record(&session_id, "exchange.task_check.admission_failed", serde_json::json!({"reason":error.to_string(),"update_step":update_step}));
                                                    }
                                                }
                                            }
                                            Err(_) => broker_deferred = true,
                                        },
                                        Err(_) => readiness = Some(InputReadiness::WaitExecution),
                                    }
                                }
                            }
                            Ok(false) => readiness = None,
                            Err(error) => {
                                trace.record(&session_id, "exchange.task_check.readiness_failed", serde_json::json!({"reason":error.to_string(),"update_step":update_step}));
                            }
                        }
                    }
                    for reply in self.task_check_requests.drain(..) {
                        let _ = reply.send(Ok(readiness));
                    }
                }
                if let Some((mut events, failure)) = task_check_completion.take() {
                    let mut drained = true;
                    if let Some(updates) = task_check_updates.as_mut() {
                        let mut retained = Vec::new();
                        for index in 0..OWNER_INTAKE_BUDGET {
                            match updates.try_recv() {
                                Ok(update) => {
                                    retained.push(update);
                                    if index + 1 == OWNER_INTAKE_BUDGET {
                                        drained = false;
                                    }
                                }
                                Err(_) => break,
                            }
                        }
                        publish_updates(
                            &mut deferred_publications,
                            &mut self.commands,
                            retained,
                            command_messages.as_ref(),
                            OwnerUpdateContext {
                                services: &services,
                                trace: &trace,
                                session: &session_id,
                                step: update_step,
                            },
                        )
                        .await;
                    }
                    if drained {
                        if let Some(error) = failure.as_ref() {
                            events.push(forge_protocol::message::SessionEvent { session_id: session_id.clone(), event: "provider_event".into(),
                            payload: serde_json::json!({"kind":"error","text":format!("Execution checks failed: {error}")}) });
                        }
                        if let Err(error) = services.publications.publish_batch(events.clone()) {
                            trace.record(&session_id, "exchange.task_check.publication_failed", serde_json::json!({"reason":error.to_string(),"update_step":update_step}));
                            services.execution.close();
                            self.foreground_access = ForegroundAccess::CollectionLost;
                        }
                        if let Some(error) = failure {
                            trace.record(&session_id, "exchange.task_check.failed", serde_json::json!({"reason":error.to_string(),"update_step":update_step}));
                        }
                        task_verification = None;
                        services.input.changed.notify_one();
                    } else {
                        task_check_completion = Some((events, failure));
                    }
                }
                // Revalidate tool candidates after this update's command has closed any collection gate.
                if let Some(request) = pending_check_tools.pop_front() {
                    let gate_open = !closed
                        && !self.commands.storage_unavailable()
                        && matches!(self.foreground_access, ForegroundAccess::Open)
                        && !command
                            .as_ref()
                            .is_some_and(ActiveCommand::closes_tool_admission);
                    if !gate_open {
                        let _ = request
                            .reply
                            .send(Err(crate::checks::owner::CheckOwnerFailure::Unavailable));
                    } else {
                        match broker.try_lock() {
                            Ok(mut broker) => match broker.owner_control_view() {
                                Ok(view) => {
                                    // The check request and the provider's turn observation travel on
                                    // separate channels, so a fast tool call can precede the observed
                                    // turn. The foreground transaction's dispatched scope remains
                                    // authoritative until that observation is drained. A finalizing
                                    // turn has already returned from the provider, so no live tool
                                    // call can still await this check; its scope is stale.
                                    let dispatched = foreground
                                        .as_ref()
                                        .filter(|runtime| {
                                            !matches!(
                                                runtime.transaction,
                                                ForegroundDispatch::Finalizing { .. }
                                            )
                                        })
                                        .and_then(|runtime| {
                                            runtime.transaction.invocation_scope().ok().flatten()
                                        });
                                    let current = dispatched.as_ref() == Some(&request.scope)
                                        || view.steering_scope.as_ref() == Some(&request.scope);
                                    let published = view.events.into_iter().try_for_each(|event| {
                                        retain_publication(
                                            event,
                                            command_messages.as_ref(),
                                            &services,
                                            &trace,
                                            &session_id,
                                            update_step,
                                        )
                                    });
                                    if published.is_err() {
                                        services.execution.close();
                                        self.foreground_access = ForegroundAccess::CollectionLost;
                                        let _ = request.reply.send(Err(
                                            crate::checks::owner::CheckOwnerFailure::Unavailable,
                                        ));
                                    } else if current {
                                        if check_scope.as_ref() != Some(&request.scope)
                                            && let Some(prior) =
                                                check_scope.replace(request.scope.clone())
                                        {
                                            let _ = check_runs.retire_scope(&prior);
                                        }
                                        if let Err(error) = check_runs.start(request) {
                                            trace.record(&session_id, "exchange.check.admission_failed", serde_json::json!({"reason":error.to_string(),"update_step":update_step}));
                                        }
                                    } else {
                                        let _ = request.reply.send(Err(
                                            crate::checks::owner::CheckOwnerFailure::StaleScope,
                                        ));
                                    }
                                }
                                Err(error) => {
                                    let _ = request.reply.send(Err(
                                        crate::checks::owner::CheckOwnerFailure::Worker(error),
                                    ));
                                }
                            },
                            Err(_) => {
                                pending_check_tools.push_front(request);
                                broker_deferred = true;
                            }
                        }
                    }
                }
                // Advance automatic work only after caller-command admission and under the same resource gates.
                if command
                    .as_ref()
                    .is_some_and(ActiveCommand::closes_tool_admission)
                    || !matches!(self.foreground_access, ForegroundAccess::Open)
                {
                    if let Some(checks) = automatic_checks.as_mut() {
                        let _ = checks.cancel();
                    }
                    automatic_action = None;
                }
                let automatic_available = !closed
                    && command.is_none()
                    && !self.commands.has_pending()
                    && foreground.is_none()
                    && pending.is_empty()
                    && self.input_delivery.is_none()
                    && self.steering.is_empty()
                    && task_verification.is_none()
                    && !self.commands.storage_unavailable()
                    && matches!(self.foreground_access, ForegroundAccess::Open);
                if automatic_available
                    && automatic_checks.is_none()
                    && automatic_publication.is_none()
                    && task_verification.is_none()
                    && automatic_repair.is_none()
                    && input_cleanup.is_empty()
                    && failed_foregrounds.is_empty()
                    && cancellation_cleanup.is_empty()
                    && rollback_cleanup.is_empty()
                    && check_runs.is_empty()
                    && pending_check_tools.is_empty()
                    && let Some(revision) = self.pending_approval_timeout.take()
                {
                    let staged = match broker.try_lock() {
                        Ok(mut broker) => match broker.approval_timeout_ready(revision) {
                            Ok(true) => {
                                let (sink, updates) = crate::broker::publication::channel();
                                Some(
                                    broker
                                        .with_publication_sink(&sink, |broker| {
                                            broker.begin_approval_timeout_dispatch(sink.clone())
                                        })
                                        .map(|(transaction, delivery)| {
                                            (transaction, delivery, updates)
                                        }),
                                )
                            }
                            Ok(false) => None,
                            Err(error) => Some(Err(error)),
                        },
                        Err(_) => {
                            self.pending_approval_timeout = Some(revision);
                            broker_deferred = true;
                            None
                        }
                    };
                    if let Some(staged) = staged {
                        match staged {
                            Ok((transaction, delivery, updates)) => {
                                publish_updates(
                                    &mut deferred_publications,
                                    &mut self.commands,
                                    delivery.into_updates(),
                                    command_messages.as_ref(),
                                    OwnerUpdateContext {
                                        services: &services,
                                        trace: &trace,
                                        session: &session_id,
                                        step: update_step,
                                    },
                                )
                                .await;
                                if let Some(transaction) = transaction {
                                    pending.push_back(PreparedForeground {
                                        id: uuid::Uuid::new_v4(),
                                        transaction,
                                        messages: command_messages.clone(),
                                        initial: None,
                                        completion: ForegroundCompletion::Retained {
                                            publications: services.publications.clone(),
                                            messages: command_messages.clone(),
                                        },
                                        updates: Some(updates),
                                        execution: None,
                                    });
                                }
                            }
                            Err(error) => {
                                trace.record(&session_id, "exchange.approval_timeout.failed",
                                serde_json::json!({"schedule_revision":revision,"reason":error.to_string(),"update_step":update_step}));
                            }
                        }
                    }
                }
                if automatic_checks.is_none()
                    && automatic_publication.is_none()
                    && automatic_available
                    && pending.is_empty()
                    && let Some(request) = pending_automatic_checks.pop_front()
                {
                    match broker.try_lock() {
                        Ok(mut broker) => {
                            let (sink, updates) = crate::broker::publication::channel();
                            match crate::broker::OwnedCheckCompletion::start(
                                &mut broker,
                                request,
                                sink.clone(),
                            ) {
                                Ok(checks) => {
                                    automatic_checks = Some(checks);
                                    automatic_publication = Some(AutomaticPublication::Open {
                                        sink,
                                        stream: updates,
                                    });
                                }
                                Err(error) => {
                                    trace.record(&session_id, "exchange.check.automatic_failed", serde_json::json!({"reason":error.to_string(),"update_step":update_step}));
                                }
                            }
                        }
                        Err(_) => {
                            pending_automatic_checks.push_front(request);
                            broker_deferred = true;
                        }
                    }
                }
                if automatic_action.is_none()
                    && automatic_repair.is_none()
                    && let Some(checks) = automatic_checks.as_mut()
                {
                    match checks.advance() {
                        Ok(crate::broker::OwnedCheckProgress::Pending) => {}
                        Ok(crate::broker::OwnedCheckProgress::Finished) => {
                            automatic_checks = None;
                            automatic_publication = automatic_publication
                                .take()
                                .map(AutomaticPublication::close);
                        }
                        Ok(action) => automatic_action = Some(action),
                        Err(error) => {
                            checks.fail(error);
                            let _ = checks.cancel();
                        }
                    }
                }
                if matches!(
                    automatic_action,
                    Some(crate::broker::OwnedCheckProgress::Ready(_))
                ) {
                    let Some(crate::broker::OwnedCheckProgress::Ready(mut outcome)) =
                        automatic_action.take()
                    else {
                        unreachable!("ready automatic outcome remains retained until publication")
                    };
                    let mut drained = true;
                    if let Some(updates) = automatic_publication
                        .as_mut()
                        .map(AutomaticPublication::stream)
                    {
                        let mut retained = Vec::new();
                        for index in 0..OWNER_INTAKE_BUDGET {
                            match updates.try_recv() {
                                Ok(update) => {
                                    retained.push(update);
                                    if index + 1 == OWNER_INTAKE_BUDGET {
                                        drained = false;
                                    }
                                }
                                Err(_) => break,
                            }
                        }
                        publish_updates(
                            &mut deferred_publications,
                            &mut self.commands,
                            retained,
                            command_messages.as_ref(),
                            OwnerUpdateContext {
                                services: &services,
                                trace: &trace,
                                session: &session_id,
                                step: update_step,
                            },
                        )
                        .await;
                    }
                    if drained {
                        let events = std::mem::take(&mut outcome.events);
                        let retained = services.publications.publish_batch(events);
                        if let Some(checks) = automatic_checks.as_mut() {
                            match retained {
                                Ok(()) => checks.complete(outcome),
                                Err(error) => checks.fail(error.into()),
                            }
                        }
                        automatic_checks = None;
                        automatic_publication = automatic_publication
                            .take()
                            .map(AutomaticPublication::close);
                    } else {
                        automatic_action = Some(crate::broker::OwnedCheckProgress::Ready(outcome));
                    }
                }
                if automatic_available && let Some(action) = automatic_action.take() {
                    match action {
                        crate::broker::OwnedCheckProgress::Revalidate => {
                            if let Some(checks) = automatic_checks.as_mut() {
                                match broker.try_lock() {
                                    Ok(mut broker) => {
                                        if let Err(error) = checks.revalidate(&mut broker) {
                                            checks.fail(error);
                                            let _ = checks.cancel();
                                        }
                                    }
                                    Err(_) => {
                                        automatic_action =
                                            Some(crate::broker::OwnedCheckProgress::Revalidate);
                                        broker_deferred = true;
                                    }
                                }
                            }
                        }
                        crate::broker::OwnedCheckProgress::Repair {
                            prompt,
                            change_owner,
                        } => match services.execution.clone().try_acquire_owned() {
                            Ok(execution) => match broker.try_lock() {
                                Ok(mut broker) => {
                                    let sink = automatic_publication
                                        .as_ref()
                                        .and_then(AutomaticPublication::sink)
                                        .expect("repair retains its open publication stream")
                                        .clone();
                                    match broker.begin_check_repair(0, prompt, change_owner, sink) {
                                        Ok(transaction) => {
                                            let (completion, response) = oneshot::channel();
                                            pending.push_back(PreparedForeground {
                                                id: uuid::Uuid::new_v4(),
                                                transaction,
                                                messages: command_messages.clone(),
                                                initial: None,
                                                completion: ForegroundCompletion::Caller(
                                                    completion,
                                                ),
                                                updates: None,
                                                execution: Some(execution),
                                            });
                                            automatic_repair = Some(response);
                                        }
                                        Err(error) => {
                                            if let Some(checks) = automatic_checks.as_mut() {
                                                checks.fail(error);
                                                let _ = checks.cancel();
                                            }
                                        }
                                    }
                                }
                                Err(_) => {
                                    automatic_action =
                                        Some(crate::broker::OwnedCheckProgress::Repair {
                                            prompt,
                                            change_owner,
                                        });
                                    broker_deferred = true;
                                }
                            },
                            Err(_) => {
                                automatic_action = Some(crate::broker::OwnedCheckProgress::Repair {
                                    prompt,
                                    change_owner,
                                })
                            }
                        },
                        action @ crate::broker::OwnedCheckProgress::Ready(_) => {
                            automatic_action = Some(action)
                        }
                        crate::broker::OwnedCheckProgress::Pending
                        | crate::broker::OwnedCheckProgress::Finished => {}
                    }
                }
                if !closed
                    && command.is_none()
                    && !self.commands.has_pending()
                    && self
                        .deferred_configuration
                        .as_ref()
                        .is_some_and(|deferred| {
                            services
                                .observer
                                .session_events()
                                .configuration_availability()
                                .is_some_and(|receipt| Some(receipt) != deferred.after)
                        })
                {
                    match broker.try_lock() {
                        Ok(broker) => {
                            let retry = broker.pending_configuration_retry(
                                &self
                                    .deferred_configuration
                                    .as_ref()
                                    .expect("deferred configuration selected above")
                                    .patch,
                            );
                            match retry {
                                Ok(crate::broker::PendingConfigurationRetry::Blocked) => {}
                                Ok(crate::broker::PendingConfigurationRetry::Obsolete) => {
                                    self.deferred_configuration = None;
                                }
                                Ok(crate::broker::PendingConfigurationRetry::Ready(patch)) => {
                                    self.deferred_configuration = None;
                                    let (reply, _response) = oneshot::channel();
                                    self.commands.admit(
                                        CommandAdmission {
                                            request: CommandRequest {
                                                id: CommandId::new(
                                                    uuid::Uuid::new_v4().to_string(),
                                                )
                                                .expect(
                                                    "generated retry command identity is nonempty",
                                                ),
                                                command: Command::ConfigureSession(
                                                    super::command::ConfigurationChange::Settings(
                                                        *patch,
                                                    ),
                                                ),
                                                input_control: InputControl::AtExecution,
                                            },
                                            reply,
                                            task_admission: None,
                                        },
                                        None,
                                        &trace,
                                        &session_id,
                                        update_step,
                                    );
                                }
                                Err(error) => {
                                    trace.record(&session_id, "exchange.configuration.retry_failed", serde_json::json!({"update_step":update_step,"reason":error.to_string()}));
                                    self.commands.retention_failed(
                                        &trace,
                                        &session_id,
                                        update_step,
                                    );
                                }
                            }
                        }
                        Err(_) => broker_deferred = true,
                    }
                }
                if !foreground_at_entry
                    && foreground.is_none()
                    && failed_foregrounds.is_empty()
                    && !self.commands.storage_unavailable()
                    && matches!(self.foreground_access, ForegroundAccess::Open)
                    && let Some(prepared) = pending.pop_front()
                {
                    trace.record(
                        &session_id,
                        "exchange.foreground.started",
                        serde_json::json!({"work_id":prepared.id,"update_step":update_step}),
                    );
                    foreground = Some(if closed {
                        ForegroundRuntime::stopping(prepared, services.publications.clone())
                    } else {
                        ForegroundRuntime::new(prepared, services.publications.clone())
                    });
                }
                if let Some(runtime) = foreground.as_mut() {
                    let gate = if !closed
                        && !self.commands.storage_unavailable()
                        && matches!(self.foreground_access, ForegroundAccess::Open)
                        && !command.as_ref().is_some_and(|command| {
                            command.closes_tool_admission()
                                && !command.authorizes_foreground(runtime.id)
                        }) {
                        ForegroundEffectGate::Open
                    } else {
                        ForegroundEffectGate::CollectionOnly
                    };
                    runtime.dispatch_live_effects(&broker, gate).await;
                    runtime.dispatch_effects(gate);
                }
            }
            // Collection and terminal evidence must progress even when an ordinary effect phase is deferred.
            if let Some(runtime) = foreground.as_mut() {
                runtime.dispatch_effects(ForegroundEffectGate::CollectionOnly);
            }
            if closed
                && ready_wake.is_none()
                && !self.observation_recovery.active()
                && shutdown
                    .as_ref()
                    .is_some_and(super::shutdown::OwnerShutdown::complete)
                && automatic_checks.is_none()
                && automatic_repair.is_none()
                && automatic_publication.is_none()
                && task_verification.is_none()
                && task_check_updates.is_none()
                && task_check_completion.is_none()
                && check_runs.is_empty()
                && pending_check_tools.is_empty()
                && foreground.is_none()
                && command.is_none()
                && pending.is_empty()
                && failed_foregrounds.is_empty()
                && cancellation_cleanup.is_empty()
                && internal_completions.is_empty()
                && rollback_cleanup.is_empty()
                && retained_rollbacks.is_empty()
                && self.input_delivery.is_none()
                && input_cleanup.is_empty()
                && self.question_waits.is_idle()
                && self.question_tools.is_empty()
                && self.steering.is_empty()
            {
                if let ForegroundAccess::Unavailable(runtime) = &self.foreground_access {
                    trace.record(
                        &session_id,
                        "exchange.unavailable.retained",
                        serde_json::json!({"work_id":runtime.id,"update_step":update_step}),
                    );
                }
                self.commands.close(&trace, &session_id, update_step);
                if !deferred_publications.updates.is_empty() {
                    trace.record(&session_id, "exchange.publication.shutdown_uncommitted", serde_json::json!({
                        "remaining_updates":deferred_publications.updates.len(),"update_step":update_step,
                    }));
                }
                drop(retained_cancellation);
                return;
            }
            if ready_wake.is_none()
                && foreground.is_none()
                && command.is_none()
                && self.commands.has_pending()
                && steering_ready.is_none()
                && !self
                    .input_delivery
                    .as_ref()
                    .is_some_and(InputDeliveryRun::preparing)
            {
                continue;
            }
            if ready_wake.is_none()
                && (matches!(
                    automatic_action,
                    Some(crate::broker::OwnedCheckProgress::Ready(_))
                ) || task_check_completion.is_some())
            {
                continue;
            }
            if let Some(runtime) = foreground.as_mut() {
                if let Some(command) = command.as_mut() {
                    while let Some(signal) = command.take_collection_signal() {
                        runtime.collection_signals.push_back(signal);
                    }
                }
                if let Some(shutdown) = shutdown.as_mut() {
                    while let Some(signal) = shutdown.take_collection_signal() {
                        runtime.collection_signals.push_back(signal);
                    }
                }
            }
            let task_delivery = self.task_delivery_gate(foreground.as_ref(), &pending);
            let foreground_status = foreground_status(
                &self.foreground_access,
                foreground
                    .as_ref()
                    .map_or(Ok(None), |runtime| runtime.transaction.invocation_scope()),
                foreground.is_some()
                    || !pending.is_empty()
                    || !self.steering.is_empty()
                    || !input_cleanup.is_empty()
                    || !check_runs.is_empty()
                    || task_verification.is_some(),
            );
            let handoff = foreground
                .as_mut()
                .and_then(ForegroundRuntime::pending_handoff);
            if ready_wake.is_none()
                && let Some(runtime) = foreground.as_ref()
            {
                trace.record(&session_id, "exchange.foreground.awaiting", serde_json::json!({
                    "work_id":runtime.id,"update_step":update_step,"running_jobs":(runtime.jobs.len() + runtime.publication_job.len()),
                    "staged_effects":runtime.staged_effects.iter().map(|job| (job.diagnostic_name(), job.is_collection())).collect::<Vec<_>>(),
                    "queued_publications":runtime.publications.len(),"publication_active":!runtime.publication_job.is_empty(),
                    "publication_drain_requested":runtime.publication_drain_requested,
                    "deferred_publications":deferred_publications.updates.len(),
                    "dispatch":runtime.transaction.diagnostic_name(),
                }));
            }
            let question_gate_open = !closed
                && !self.receiver.is_closed()
                && !self.commands.storage_unavailable()
                && !self.observation_recovery.active()
                && matches!(self.foreground_access, ForegroundAccess::Open)
                && !command
                    .as_ref()
                    .is_some_and(ActiveCommand::closes_tool_admission);
            let question_tool_ready = self.question_tools.iter().any(|request| {
                !question_gate_open || !request.waits_for_capacity(&self.question_waits)
            });
            let wake = if let Some(wake) = ready_wake {
                wake
            } else {
                let wake = tokio::select! {
                    _ = std::future::ready(()), if question_tool_ready => DriverWake::QuestionTool,
                    _ = self.question_waits.next() => DriverWake::QuestionWait,
                    _ = async {
                        match publication_recovery_at {
                            Some(deadline) => tokio::time::sleep_until(deadline).await,
                            None => std::future::pending().await,
                        }
                    } => DriverWake::PublicationStoreRetry,
                    _ = async {
                        match deferred_publications.retry_at {
                            Some(deadline) => tokio::time::sleep_until(deadline).await,
                            None => std::future::pending().await,
                        }
                    } => DriverWake::PublicationRetry,
                    _ = async { drop(broker.lock().await); }, if broker_deferred => DriverWake::BrokerAvailable,
                    _ = self.question_inputs.next(&mut question_changes), if !closed && !self.commands.storage_unavailable() => DriverWake::QuestionInput,
                    _ = async {
                        tokio::select! {
                            _ = publication_delivery.next() => {},
                            _ = services.publications.changed() => {},
                            _ = services.publications.reconciliation_expiry() => {},
                        }
                    } => DriverWake::Publication,
                    observed = async { match steering_ready.take() { Some(observed) => observed, None => next_steering(&mut self.steering, foreground_status.clone()).await } } => {
                        DriverWake::InputDelivery(InputDeliveryTarget::Steering(observed.0), observed.1)
                    },
                    event = self.observation_recovery.next(), if self.observation_recovery.started() => DriverWake::ObservationRecovery(event),
                    event = async { match self.input_delivery.as_mut() {
                        Some(run) => run.next(foreground_status.clone()).await,
                        None => std::future::pending().await,
                    }} => DriverWake::InputDelivery(InputDeliveryTarget::Foreground, event),
                    collected = input_cleanup.join_next(), if !input_cleanup.is_empty() => DriverWake::InputCollected(collected),
                    receipt = self.input_receipts.next() => DriverWake::InputReceipt(receipt),
                    event = check_events.recv(), if !closed => DriverWake::CheckEvent(event),
                    completed = check_runs.next(), if !check_runs.is_empty() => DriverWake::CheckCollected(completed),
                    event = async {
                        match task_verification.as_mut() {
                            Some(checks) => checks.run.next().await,
                            None => std::future::pending().await,
                        }
                    }, if task_check_completion.is_none() => DriverWake::TaskCheck(event),
                    update = async {
                        match task_check_updates.as_mut() {
                            Some(updates) => updates.recv().await,
                            None => std::future::pending().await,
                        }
                    } => DriverWake::TaskCheckUpdate(update),
                    update = tool_check_updates.recv() => DriverWake::ToolCheckUpdate(update),
                    event = async {
                        match automatic_checks.as_mut() {
                            Some(checks) if checks.has_worker() => checks.next().await,
                            Some(_) | None => std::future::pending().await,
                        }
                    } => DriverWake::AutomaticCheck(event),
                    update = async {
                        match automatic_publication.as_mut().map(AutomaticPublication::stream) {
                            Some(updates) => updates.recv().await,
                            None => std::future::pending().await,
                        }
                    } => DriverWake::AutomaticUpdate(update),
                    repaired = async {
                        match automatic_repair.as_mut() {
                            Some(response) => response.await.unwrap_or_else(|_| Err(anyhow::anyhow!("automatic repair observation was lost"))),
                            None => std::future::pending().await,
                        }
                    } => DriverWake::AutomaticRepair(repaired),
                    event = self.receiver.recv(), if !closed && pending.len() < OWNER_QUEUE_CAPACITY => DriverWake::Event(event),
                    signal = async {
                        match foreground.as_mut() {
                            Some(runtime) => runtime.next().await,
                            None => std::future::pending().await,
                        }
                    } => DriverWake::Foreground(signal),
                    observed = async {
                        match handoff {
                            Some(mut pending) => {
                                let outcome = pending.observer.wait().await;
                                (pending.work_id, outcome)
                            }
                            None => std::future::pending().await,
                        }
                    } => DriverWake::Dispatched(observed),
                    _ = self.shutdown_retry.notified(), if closed => DriverWake::ShutdownRetry,
                    event = async {
                        match shutdown.as_mut() {
                            Some(shutdown) => shutdown.next(foreground_status.clone()).await,
                            None => std::future::pending().await,
                        }
                    } => DriverWake::Shutdown(event),
                    prepared = async {
                        match command.as_mut() {
                            Some(command) => command.next(foreground_status.clone(), task_delivery).await,
                            None => std::future::pending().await,
                        }
                    } => DriverWake::Command(prepared),
                    collected = failed_foregrounds.join_next(), if !failed_foregrounds.is_empty() => DriverWake::FailedForeground(collected),
                    collected = cancellation_cleanup.join_next(), if !cancellation_cleanup.is_empty() => DriverWake::CancellationCollected(collected),
                    collected = rollback_cleanup.join_next(), if !rollback_cleanup.is_empty() => DriverWake::RollbackCollected(collected),
                    completed = internal_completions.join_next(), if !internal_completions.is_empty() => DriverWake::InternalCompleted(completed),
                };
                pending_wake = Some(wake);
                continuing_event_phase = false;
                continue;
            };
            let command_wake = matches!(&wake, DriverWake::Command(_));
            // A timer wake cannot invalidate handoff evidence already available when its command applies.
            if command_wake && let Some(runtime) = foreground.as_mut() {
                runtime.read_handoff(&trace, &session_id, update_step);
                if let ForegroundHandoff::Confirmed(confirmation) = &runtime.handoff {
                    if let Some(input) = self.input_delivery.as_mut() {
                        input.observe_dispatch(runtime.id);
                    }
                    if let Some(command) = command.as_mut() {
                        command.observe_dispatch(runtime.id, confirmation);
                    }
                }
            }
            match wake {
                DriverWake::ObservationRecovery(event) => {
                    if self.commands.storage_unavailable() {
                        self.observation_recovery.fail_storage();
                        self.foreground_access = ForegroundAccess::CollectionLost;
                    }
                    let result = {
                        let mut state = broker.lock().await;
                        self.observation_recovery
                            .apply(event, &mut state, &services)
                    };
                    match result {
                        Ok(true) => {
                            let catalog = { broker.lock().await.provider_catalog_request() };
                            match catalog {
                                Ok(catalog) => {
                                    *services.catalog.write().await = catalog;
                                    services
                                        .resource_revision
                                        .fetch_add(1, std::sync::atomic::Ordering::AcqRel);
                                    self.observation_recovery.finish(Ok(()));
                                    self.foreground_access = ForegroundAccess::Open;
                                }
                                Err(_) => {
                                    services.tasks.mark_configuration_uncertain();
                                    self.observation_recovery.finish(Err(super::provider_lifecycle::ProviderLifecycleFailure::StateUnavailable));
                                    self.foreground_access = ForegroundAccess::CollectionLost;
                                }
                            }
                        }
                        Ok(false) => {}
                        Err(error) => {
                            trace.record(&session_id, "exchange.observation_recovery.failed", serde_json::json!({"reason":error.to_string(),"update_step":update_step}));
                            services.tasks.mark_configuration_uncertain();
                            self.observation_recovery.finish(Err(error));
                            self.foreground_access = ForegroundAccess::CollectionLost;
                        }
                    }
                }

                DriverWake::ShutdownRetry => {
                    if let Some(shutdown) = shutdown.as_mut() {
                        shutdown.retry(&services);
                    }
                }
                DriverWake::Shutdown(event) => {
                    trace.record(
                        &session_id,
                        "exchange.shutdown.event",
                        serde_json::json!({"event":event.name(),"update_step":update_step}),
                    );
                    let progress = shutdown
                        .as_mut()
                        .expect("shutdown evidence retains its collection owner")
                        .update(
                            event,
                            foreground_status.clone(),
                            &mut *broker.lock().await,
                            &services,
                        );
                    if let Some(failure) = progress.failure {
                        trace.record(&session_id, "exchange.shutdown.collection_failed", serde_json::json!({"update_step":update_step,"reason":failure.to_string()}));
                    }
                    if let Some(input) = progress.input {
                        publish_input_observation(
                            input,
                            None,
                            &services,
                            &trace,
                            &session_id,
                            update_step,
                        );
                    }
                    for event in progress.events {
                        if let Err(error) = retain_publication(
                            event,
                            None,
                            &services,
                            &trace,
                            &session_id,
                            update_step,
                        ) {
                            trace.record(&session_id, "exchange.shutdown.publication_failed", serde_json::json!({"update_step":update_step,"reason":error.to_string()}));
                        }
                    }
                }
                DriverWake::Publication => {}
                DriverWake::QuestionInput => self.question_inputs.request(),
                DriverWake::QuestionWait => self.question_inputs.request(),
                DriverWake::QuestionTool => {}
                DriverWake::BrokerAvailable => {}
                DriverWake::InputDelivery(target, event) => {
                    let mut run = match &target {
                        InputDeliveryTarget::Foreground => self.input_delivery.take(),
                        InputDeliveryTarget::Steering(id) => self.steering.remove(id),
                    }
                    .expect("input event belongs to its retained delivery");
                    let input = run.input.clone();
                    let output = run.messages.clone();
                    trace.record(&session_id, "exchange.event.read", serde_json::json!({
                        "source":"input_delivery","input_id":input.id.as_str(),"input_revision":input.revision,
                        "event":event.name(),"update_step":update_step,
                    }));
                    let progress = run.update(
                        event,
                        foreground_status.clone(),
                        &mut *broker.lock().await,
                        &services,
                        &mut self.input_receipts,
                        !self.commands.storage_unavailable(),
                    );
                    match progress {
                        Ok(InputDeliveryProgress::Waiting {
                            events,
                            updates,
                            input: changed,
                        }) => {
                            publish_input_events(
                                events,
                                output.as_ref(),
                                &services,
                                &trace,
                                &session_id,
                                &input,
                                update_step,
                            );
                            publish_updates(
                                &mut deferred_publications,
                                &mut self.commands,
                                updates,
                                output.as_ref(),
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                            )
                            .await;
                            if let Some(changed) = changed {
                                publish_input_observation(
                                    changed,
                                    output.as_ref(),
                                    &services,
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                            self.restore_delivery(&target, run);
                        }
                        Ok(InputDeliveryProgress::Foreground(start)) => {
                            if let Some(ActiveCommand::Task(run)) = command.as_mut()
                                && let InputForegroundSource::Input(receipt) = &start.source
                                && run.input_foreground(&input, start.id, receipt.clone())
                            {
                                trace.record(&session_id, "exchange.command.effect_requested", serde_json::json!({
                                    "command_id":run.command.record.request.id.as_str(),"effect":"task_input_foreground",
                                    "input_id":input.id.as_str(),"input_revision":input.revision,
                                    "work_id":start.id,"submission_id":receipt.as_str(),"update_step":update_step,
                                }));
                            }
                            publish_updates(
                                &mut deferred_publications,
                                &mut self.commands,
                                start.committed,
                                output.as_ref(),
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                            )
                            .await;
                            if let Some(changed) = start.input {
                                publish_input_observation(
                                    changed,
                                    output.as_ref(),
                                    &services,
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                            pending.push_back(PreparedForeground {
                                id: start.id,
                                transaction: start.transaction,
                                messages: output,
                                initial: None,
                                completion: ForegroundCompletion::Caller(start.completion),
                                updates: Some(start.updates),
                                execution: Some(start.execution),
                            });
                            self.restore_delivery(&target, run);
                        }
                        Ok(InputDeliveryProgress::Finished {
                            events,
                            input: changed,
                            completion,
                        }) => {
                            if let Some(ActiveCommand::Task(task)) = command.as_mut() {
                                task.input_ended(&input);
                            }
                            drop(run);
                            if let Some(completion) = completion {
                                internal_completions.spawn(
                                    completion
                                        .forward(services.publications.clone(), output.clone()),
                                );
                            }
                            publish_input_events(
                                events,
                                output.as_ref(),
                                &services,
                                &trace,
                                &session_id,
                                &input,
                                update_step,
                            );
                            if let Some(changed) = changed {
                                publish_input_observation(
                                    changed,
                                    output.as_ref(),
                                    &services,
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                            trace.record(&session_id, "exchange.input.completed", serde_json::json!({
                                "input_id":input.id.as_str(),"input_revision":input.revision,"update_step":update_step,
                            }));
                            services.input.changed.notify_one();
                        }
                        Err(error) => {
                            if let Some(ActiveCommand::Task(task)) = command.as_mut() {
                                task.input_ended(&input);
                            }
                            let safe = matches!(
                                &error,
                                OperationFailure::Validation(_)
                                    | OperationFailure::Transition(TransitionFailure::Rejected(_))
                            );
                            trace.record(&session_id, "exchange.input.failed", serde_json::json!({
                                "input_id":input.id.as_str(),"input_revision":input.revision,"update_step":update_step,
                                "code":error.code(),"reason":error.diagnostic(),"collection_required":!safe,
                            }));
                            publish_input_events(
                                vec![forge_protocol::message::SessionEvent {
                                    session_id: session_id.clone(),
                                    event: "provider_event".into(),
                                    payload: serde_json::json!({"kind":"error","code":error.code(),"text":format!("Input delivery failed: {}", error.diagnostic())}),
                                }],
                                output.as_ref(),
                                &services,
                                &trace,
                                &session_id,
                                &input,
                                update_step,
                            );
                            if !safe {
                                services.execution.close();
                                if matches!(self.foreground_access, ForegroundAccess::Open) {
                                    self.foreground_access = ForegroundAccess::Collecting;
                                }
                            }
                            let changed = match self.input_receipts.apply(
                                InputReceiptFact::WorkerFailed {
                                    input: input.clone(),
                                },
                                &services,
                            ) {
                                Ok(changed) => changed,
                                Err(failure) => {
                                    services.execution.close();
                                    self.foreground_access = ForegroundAccess::CollectionLost;
                                    trace.record(&session_id, "exchange.input.failure_commit_failed", serde_json::json!({
                                        "input_id":input.id.as_str(),"update_step":update_step,"reason":failure.to_string(),
                                    }));
                                    None
                                }
                            };
                            if safe {
                                drop(run);
                                services.input.changed.notify_one();
                            } else {
                                let mut work = run.retain();
                                publish_input_events(
                                    std::mem::take(&mut work.terminal_events),
                                    work.messages.as_ref(),
                                    &services,
                                    &trace,
                                    &session_id,
                                    &work.input,
                                    update_step,
                                );
                                if let Some(completion) = work.completion.take() {
                                    internal_completions.spawn(completion.forward(
                                        services.publications.clone(),
                                        work.messages.clone(),
                                    ));
                                }
                                let recovery = services.clone();
                                input_cleanup.spawn(async move {
                                    let outcome = std::panic::AssertUnwindSafe(
                                        work.retained.collect(&recovery),
                                    )
                                    .catch_unwind()
                                    .await
                                    .unwrap_or(Err(
                                        crate::provider::ProviderError::DeliveryUncertain,
                                    ));
                                    CollectedInput {
                                        work,
                                        phase: InputCleanupPhase::Native(outcome),
                                    }
                                });
                            }
                            if let Some(changed) = changed {
                                publish_input_observation(
                                    changed,
                                    output.as_ref(),
                                    &services,
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                        }
                    }
                }
                DriverWake::InputCollected(Some(Ok(mut collected))) => {
                    let input = &collected.work.input;
                    match collected.phase {
                        InputCleanupPhase::Native(outcome) => {
                            let facts = collected
                                .work
                                .retained
                                .apply_compaction_facts(&mut *broker.lock().await);
                            let facts_committed = match facts {
                                Ok(updates) => {
                                    publish_updates(
                                        &mut deferred_publications,
                                        &mut self.commands,
                                        updates,
                                        collected.work.messages.as_ref(),
                                        OwnerUpdateContext {
                                            services: &services,
                                            trace: &trace,
                                            session: &session_id,
                                            step: update_step,
                                        },
                                    )
                                    .await;
                                    !self.commands.storage_unavailable()
                                }
                                Err(error) => {
                                    trace.record(&session_id, "exchange.input.collection_fact_commit_failed", serde_json::json!({
                                    "input_id":input.id.as_str(),"update_step":update_step,"reason":error.to_string(),
                                }));
                                    false
                                }
                            };
                            if outcome.is_ok()
                                && let Some(runtime) = foreground.as_mut()
                            {
                                runtime.collection_signals.push_back(
                                    collected.work.retained.collection_completed_signal(),
                                );
                            }
                            let committed = if outcome.is_ok() && facts_committed {
                                collected
                                    .work
                                    .retained
                                    .apply_collected(&mut *broker.lock().await)
                            } else {
                                Err(TransitionFailure::Uncertain)
                            };
                            if let Ok(events) = committed {
                                publish_input_events(
                                    events,
                                    collected.work.messages.as_ref(),
                                    &services,
                                    &trace,
                                    &session_id,
                                    input,
                                    update_step,
                                );
                            }
                            input_cleanup.spawn(async move {
                                collected.work.retained.drain_host().await;
                                CollectedInput {
                                    work: collected.work,
                                    phase: InputCleanupPhase::HostDrained,
                                }
                            });
                        }
                        InputCleanupPhase::HostDrained => {
                            collected.work.retained.release_legacy_control();
                            collected.work.execution.take();
                            retained_inputs.push(collected.work);
                            if matches!(self.foreground_access, ForegroundAccess::Collecting) {
                                self.foreground_access = ForegroundAccess::CollectionLost;
                            }
                        }
                    }
                }
                DriverWake::InputCollected(Some(Err(error))) => {
                    services.execution.close();
                    self.foreground_access = ForegroundAccess::CollectionLost;
                    trace.record(
                        &session_id,
                        "exchange.input.collection_observation_lost",
                        serde_json::json!({"update_step":update_step,"reason":error.to_string()}),
                    );
                }
                DriverWake::InputCollected(None) => {}
                DriverWake::InputReceipt(wake) => {
                    let submission = match wake {
                        InputReceiptWake::Evidence(id) => id,
                        InputReceiptWake::Unavailable(id) => {
                            services.execution.close();
                            self.input_receipts.suspend_observation(&id);
                            trace.record(
                                &session_id,
                                "exchange.input_receipt.observation_lost",
                                serde_json::json!({
                                    "submission_id":id.as_str(),"update_step":update_step,
                                }),
                            );
                            continue;
                        }
                    };
                    trace.record(&session_id, "exchange.event.read", serde_json::json!({
                        "source":"input_receipt_watch","submission_id":submission.as_str(),"update_step":update_step,
                    }));
                    match self.input_receipts.apply(
                        InputReceiptFact::Acceptance {
                            submission: submission.clone(),
                        },
                        &services,
                    ) {
                        Ok(update) => {
                            trace.record(&session_id, "exchange.event.applied", serde_json::json!({
                                "source":"input_receipt_watch","submission_id":submission.as_str(),"update_step":update_step,
                            }));
                            if let Some(update) = update {
                                publish_input_observation(
                                    update,
                                    command_messages.as_ref(),
                                    &services,
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                        }
                        Err(error) => {
                            self.input_receipts.suspend_observation(&submission);
                            if error.fences_execution() {
                                services.execution.close();
                            }
                            trace.record(&session_id, "exchange.input_receipt.apply_failed", serde_json::json!({
                                "submission_id":submission.as_str(),"update_step":update_step,"reason":error.to_string(),
                            }));
                        }
                    }
                }
                DriverWake::Event(Some(event)) => {
                    self.receive(
                        event,
                        &mut pending,
                        OwnerIntake {
                            active: command.as_mut(),
                            steering: SteeringAdmission::from(foreground.as_ref()),
                        },
                        &mut command_messages,
                        &broker,
                        OwnerUpdateContext {
                            services: &services,
                            trace: &trace,
                            session: &session_id,
                            step: update_step,
                        },
                    )
                    .await
                }
                DriverWake::Event(None) => closed = true,
                DriverWake::Dispatched((id, outcome)) => {
                    if let Some(runtime) = foreground.as_mut()
                        && runtime.id == id
                    {
                        runtime.observe_handoff(outcome, &trace, &session_id, update_step);
                        if matches!(runtime.handoff, ForegroundHandoff::Confirmed(_))
                            && let Some(input) = self.input_delivery.as_mut()
                        {
                            input.observe_dispatch(runtime.id);
                        }
                        if let Some(command) = command.as_mut() {
                            match &runtime.handoff {
                                ForegroundHandoff::Confirmed(confirmation) => {
                                    command.observe_dispatch(runtime.id, confirmation)
                                }
                                ForegroundHandoff::Unconfirmed => command.dispatch_lost(runtime.id),
                                ForegroundHandoff::Preparing
                                | ForegroundHandoff::Watching { .. } => {}
                            }
                        }
                    } else {
                        trace.record(&session_id, "exchange.dispatch.ignored", serde_json::json!({
                            "work_id":id,"update_step":update_step,"reason":"foreground_owner_changed",
                        }));
                    }
                }
                DriverWake::Command(CommandEvent::Input(prepared)) => {
                    let Some(ActiveCommand::Input(run)) = command.take() else {
                        unreachable!("input evidence belongs to the active input command")
                    };
                    self.finish_input(
                        run,
                        prepared,
                        command_messages.as_ref(),
                        OwnerUpdateContext {
                            services: &services,
                            trace: &trace,
                            session: &session_id,
                            step: update_step,
                        },
                    );
                }
                DriverWake::Command(CommandEvent::Configuration) => {
                    let Some(ActiveCommand::Configuration(queued)) = command.take() else {
                        unreachable!(
                            "configuration readiness belongs to the active configuration command"
                        )
                    };
                    if self.commands.storage_unavailable() {
                        let mut queued = queued;
                        self.commands.lose_observation(
                            &mut queued,
                            &trace,
                            &session_id,
                            update_step,
                        );
                    } else {
                        match super::configuration::apply_configuration(&queued, &services) {
                            Ok(outcome) => self.commands.finish(
                                queued,
                                Ok(CommandOutcome::Configured(outcome)),
                                &trace,
                                &session_id,
                                update_step,
                            ),
                            Err(super::configuration::ConfigurationCommitError::Rejected(
                                reason,
                            )) => {
                                self.commands.finish(
                                    queued,
                                    Err(CommandFailure::ConfigurationRejected(reason)),
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                            Err(error) => {
                                trace.record(&session_id, "exchange.configuration.commit_failed", serde_json::json!({
                                    "command_id":queued.record.request.id.as_str(),"update_step":update_step,"reason":error.to_string(),
                                }));
                                self.commands.uncommitted(
                                    queued,
                                    CommandJournalError::DomainCommit,
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                        }
                    }
                }
                DriverWake::PublicationStoreRetry => publication_recovery_at = None,
                DriverWake::PublicationRetry => deferred_publications.retry_at = None,
                DriverWake::TaskCheck(Some(result)) => {
                    let progress = match task_verification.as_mut() {
                        Some(checks) => {
                            if self.commands.storage_unavailable() {
                                let _ = checks.run.cancel();
                            }
                            match result {
                                Ok(event) => checks.run.update(&mut *broker.lock().await, event),
                                Err(error) => Err(error),
                            }
                        }
                        None => Err(anyhow::anyhow!("task verification owner disappeared")),
                    };
                    match progress {
                        Ok(crate::broker::TaskCheckProgress::Pending) => {}
                        Ok(crate::broker::TaskCheckProgress::Finished { events, failure }) => {
                            task_check_completion = Some((events, failure))
                        }
                        Err(error) => {
                            task_check_completion = Some((
                                Vec::new(),
                                Some(crate::checks::owner::CheckOwnerFailure::Worker(error)),
                            ))
                        }
                    }
                }
                DriverWake::TaskCheck(None) => {}
                DriverWake::TaskCheckUpdate(Ok(Some(update))) => {
                    publish_updates(
                        &mut deferred_publications,
                        &mut self.commands,
                        vec![update],
                        command_messages.as_ref(),
                        OwnerUpdateContext {
                            services: &services,
                            trace: &trace,
                            session: &session_id,
                            step: update_step,
                        },
                    )
                    .await;
                }
                DriverWake::ToolCheckUpdate(Ok(Some(update))) => {
                    publish_updates(
                        &mut deferred_publications,
                        &mut self.commands,
                        vec![update],
                        command_messages.as_ref(),
                        OwnerUpdateContext {
                            services: &services,
                            trace: &trace,
                            session: &session_id,
                            step: update_step,
                        },
                    )
                    .await;
                }
                // The owner retains a sender, so the stream closes only when the owner exits.
                DriverWake::ToolCheckUpdate(Ok(None)) => {}
                DriverWake::ToolCheckUpdate(Err(error)) => {
                    trace.record(
                        &session_id,
                        "exchange.tool_check.stream_failed",
                        serde_json::json!({"reason":error.to_string(),"update_step":update_step}),
                    );
                }
                DriverWake::TaskCheckUpdate(Ok(None)) => task_check_updates = None,
                DriverWake::TaskCheckUpdate(Err(error)) => {
                    task_check_updates = None;
                    if let Some(checks) = task_verification.as_mut() {
                        let _ = checks.run.cancel();
                    }
                    trace.record(
                        &session_id,
                        "exchange.task_check.stream_failed",
                        serde_json::json!({"reason":error.to_string(),"update_step":update_step}),
                    );
                }
                DriverWake::AutomaticCheck(Some(result)) => {
                    if let Some(checks) = automatic_checks.as_mut() {
                        let result = if self.commands.storage_unavailable() {
                            Err(anyhow::anyhow!(
                                "owner storage is unavailable for automatic check completion"
                            ))
                        } else {
                            match result {
                                Ok(result) => checks.apply(&mut *broker.lock().await, result),
                                Err(error) => Err(error),
                            }
                        };
                        if let Err(error) = result {
                            checks.fail(error);
                            let _ = checks.cancel();
                        }
                    }
                }
                DriverWake::AutomaticCheck(None) => {}
                DriverWake::AutomaticUpdate(Ok(Some(update))) => {
                    publish_updates(
                        &mut deferred_publications,
                        &mut self.commands,
                        vec![update],
                        command_messages.as_ref(),
                        OwnerUpdateContext {
                            services: &services,
                            trace: &trace,
                            session: &session_id,
                            step: update_step,
                        },
                    )
                    .await;
                }
                DriverWake::AutomaticUpdate(Ok(None)) => automatic_publication = None,
                DriverWake::AutomaticUpdate(Err(error)) => {
                    automatic_publication = None;
                    if let Some(checks) = automatic_checks.as_mut() {
                        checks.fail(error.into());
                        let _ = checks.cancel();
                    }
                }
                DriverWake::AutomaticRepair(result) => {
                    automatic_repair = None;
                    if let Some(checks) = automatic_checks.as_mut() {
                        let result = result.and_then(|result| {
                            if result.response.error().is_some() {
                                return Err(anyhow::anyhow!("automatic repair foreground failed"));
                            }
                            let value = result.response.result().cloned().ok_or_else(|| {
                                anyhow::anyhow!("automatic repair result is missing")
                            })?;
                            Ok((value, result.event))
                        });
                        let result = match result {
                            Ok(result) if !self.commands.storage_unavailable() => {
                                checks.repaired(&mut *broker.lock().await, result)
                            }
                            Ok(_) => Err(anyhow::anyhow!(
                                "owner storage is unavailable for automatic repair"
                            )),
                            Err(error) => Err(error),
                        };
                        if let Err(error) = result {
                            checks.fail(error);
                            let _ = checks.cancel();
                        }
                    }
                }
                DriverWake::CheckEvent(Some(crate::checks::owner::CheckOwnerEvent::StartTool(
                    request,
                ))) => {
                    if pending_check_tools.len() < OWNER_QUEUE_CAPACITY {
                        let mut request = *request;
                        if request.sink.is_some() {
                            request.sink = Some(tool_check_sink.clone());
                        }
                        pending_check_tools.push_back(request);
                    } else {
                        let _ = request
                            .reply
                            .send(Err(crate::checks::owner::CheckOwnerFailure::Unavailable));
                    }
                }
                DriverWake::CheckEvent(Some(
                    crate::checks::owner::CheckOwnerEvent::CancelTool {
                        scope,
                        run_id,
                        reply,
                    },
                )) => {
                    let _ = reply.send(check_runs.cancel(&scope, &run_id));
                }
                DriverWake::CheckEvent(Some(
                    crate::checks::owner::CheckOwnerEvent::CompleteAutomatic(request),
                )) => {
                    if pending_automatic_checks.len() < OWNER_QUEUE_CAPACITY {
                        pending_automatic_checks.push_back(request);
                    } else {
                        let _ = request
                            .reply
                            .send(Err(crate::checks::owner::CheckOwnerFailure::Unavailable));
                    }
                }
                DriverWake::CheckEvent(None) => {}
                DriverWake::CheckCollected(Some(Ok(completion))) => {
                    trace.record(&session_id, "exchange.check.collected", serde_json::json!({
                        "run_id":completion.run_id,"succeeded":completion.outcome.is_ok(),"update_step":update_step,
                    }));
                    check_runs.settle(&completion);
                    if check_scope.as_ref() != Some(&completion.scope) {
                        let _ = check_runs.retire_scope(&completion.scope);
                    }
                    services.input.changed.notify_one();
                }
                DriverWake::CheckCollected(Some(Err(error))) => {
                    services.execution.close();
                    self.foreground_access = ForegroundAccess::CollectionLost;
                    trace.record(
                        &session_id,
                        "exchange.check.collection_lost",
                        serde_json::json!({"reason":error.to_string(),"update_step":update_step}),
                    );
                }
                DriverWake::CheckCollected(None) => {}
                DriverWake::Command(CommandEvent::Agent(event)) => {
                    let Some(ActiveCommand::Agent(run)) = command.take() else {
                        unreachable!("child coordinator facts belong to the active agent command")
                    };
                    let id = run.command.record.request.id.clone();
                    match event {
                        super::agent_command::AgentCommandEvent::Completed(outcome) => {
                            self.commands.finish(
                                run.command,
                                outcome.map(CommandOutcome::Agent),
                                &trace,
                                &session_id,
                                update_step,
                            );
                        }
                        super::agent_command::AgentCommandEvent::ObservationDeadline => {
                            trace.record(
                                &session_id,
                                "exchange.command.observation_deadline",
                                serde_json::json!({
                                    "command_id":id.as_str(),"sequence":run.command.record.sequence,
                                    "source":"agent","update_step":update_step,
                                }),
                            );
                            command = Some(ActiveCommand::Agent(run));
                        }
                    }
                }
                DriverWake::Command(CommandEvent::Rollback(event)) => {
                    let Some(ActiveCommand::Rollback(mut run)) = command.take() else {
                        unreachable!("rollback facts belong to the retained rollback command")
                    };
                    let id = run.command.record.request.id.clone();
                    trace.record(
                        &session_id,
                        "exchange.command.event",
                        serde_json::json!({
                            "command_id":id.as_str(),"sequence":run.command.record.sequence,
                            "source":"rollback","event":event.name(),"update_step":update_step,
                        }),
                    );
                    let was_initializing = run.initializing();
                    let (progress, catalog) = {
                        let mut broker = broker.lock().await;
                        let progress = run.update(
                            event,
                            foreground_status.clone(),
                            &mut broker,
                            &services,
                            !self.commands.storage_unavailable(),
                        );
                        let catalog = if matches!(
                            &progress,
                            Ok(RollbackProgress::Finished { outcome: Ok(_), .. })
                        ) {
                            *services
                                .provider
                                .write()
                                .unwrap_or_else(std::sync::PoisonError::into_inner) =
                                broker.provider_handle();
                            Some(broker.provider_catalog_request())
                        } else {
                            None
                        };
                        (progress, catalog)
                    };
                    while let Some(signal) = run.take_collection_signal() {
                        if let Some(runtime) = foreground.as_mut() {
                            runtime.collection_signals.push_back(signal);
                        }
                    }
                    if let Some(catalog) = catalog {
                        match catalog {
                            Ok(catalog) => {
                                *services.catalog.write().await = catalog;
                                services
                                    .resource_revision
                                    .fetch_add(1, std::sync::atomic::Ordering::AcqRel);
                            }
                            Err(error) => {
                                services.execution.close();
                                self.foreground_access = ForegroundAccess::CollectionLost;
                                trace.record(&session_id, "exchange.rollback.catalog_failed", serde_json::json!({
                                    "command_id":id.as_str(),"reason":error.to_string(),"update_step":update_step,
                                }));
                            }
                        }
                    }
                    match progress {
                        Ok(RollbackProgress::Waiting { events, input }) => {
                            if run.initializing() {
                                self.foreground_access = ForegroundAccess::Collecting;
                            }
                            publish_control(
                                events,
                                input.as_deref(),
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                                &id,
                            );
                            command = Some(ActiveCommand::Rollback(run));
                        }
                        Ok(RollbackProgress::Finished { outcome, events }) => {
                            if was_initializing
                                && outcome.is_ok()
                                && matches!(self.foreground_access, ForegroundAccess::Collecting)
                            {
                                self.foreground_access = ForegroundAccess::Open;
                            }
                            if matches!(
                                &outcome,
                                Err(super::rollback::RollbackFailure::RecoveryRequired)
                            ) {
                                services.execution.fence_collection_pending();
                                self.foreground_access = ForegroundAccess::Collecting;
                                publish_control(
                                    events,
                                    None,
                                    OwnerUpdateContext {
                                        services: &services,
                                        trace: &trace,
                                        session: &session_id,
                                        step: update_step,
                                    },
                                    &id,
                                );
                                let (queued, recovery) = run.into_recovery();
                                self.commands.finish(
                                    queued,
                                    outcome
                                        .map(CommandOutcome::Rollback)
                                        .map_err(CommandFailure::Rollback),
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                                rollback_cleanup
                                    .spawn(async move { (id, recovery.collect().await) });
                                continue;
                            }
                            publish_control(
                                events,
                                None,
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                                &id,
                            );
                            self.commands.finish(
                                run.command,
                                outcome
                                    .map(CommandOutcome::Rollback)
                                    .map_err(CommandFailure::Rollback),
                                &trace,
                                &session_id,
                                update_step,
                            );
                        }
                        Err(error) => {
                            trace.record(&session_id, "exchange.command.effect_failed", serde_json::json!({
                                "command_id":id.as_str(),"code":error.code(),"reason":error.diagnostic(),"update_step":update_step,
                            }));
                            let (queued, recovery) = run.into_recovery();
                            services.execution.fence_collection_pending();
                            self.foreground_access = ForegroundAccess::Collecting;
                            match error {
                                OperationFailure::Domain(_)
                                | OperationFailure::StorageUncommitted
                                | OperationFailure::Transition(TransitionFailure::Domain(_)) => {
                                    self.commands.uncommitted(
                                        queued,
                                        CommandJournalError::DomainCommit,
                                        &trace,
                                        &session_id,
                                        update_step,
                                    )
                                }
                                OperationFailure::Transition(TransitionFailure::Rejected(
                                    failure,
                                )) => self.commands.finish(
                                    queued,
                                    Err(failure),
                                    &trace,
                                    &session_id,
                                    update_step,
                                ),
                                OperationFailure::JobDeadline
                                | OperationFailure::Transition(TransitionFailure::Overdue) => {
                                    self.commands.finish(
                                        queued,
                                        Err(CommandFailure::CollectionTimedOut),
                                        &trace,
                                        &session_id,
                                        update_step,
                                    )
                                }
                                OperationFailure::InputReconciliation(_)
                                | OperationFailure::Compaction { .. }
                                | OperationFailure::DispatchUnconfirmed
                                | OperationFailure::JobObservationLost(_)
                                | OperationFailure::Validation(_)
                                | OperationFailure::ExecutionUnavailable
                                | OperationFailure::Reconciliation(_)
                                | OperationFailure::Transition(TransitionFailure::Uncertain) => {
                                    self.commands.finish(
                                        queued,
                                        Err(CommandFailure::CollectionUncertain),
                                        &trace,
                                        &session_id,
                                        update_step,
                                    )
                                }
                            }
                            rollback_cleanup.spawn(async move { (id, recovery.collect().await) });
                        }
                    }
                }
                DriverWake::Command(CommandEvent::Task(event)) => {
                    let Some(ActiveCommand::Task(mut run)) = command.take() else {
                        unreachable!("task facts belong to the retained active task command")
                    };
                    let id = run.command.record.request.id.clone();
                    let source_plan = run.captured_plan();
                    let task = run.captured_task();
                    trace.record(&session_id, "exchange.event.read", serde_json::json!({
                        "source":"task_command","command_id":id.as_str(),"sequence":run.command.record.sequence,
                        "task_id":task.map(|task| task.id().as_str()),"task_revision":task.map(|task| task.revision().as_str()),
                        "source_plan_id":source_plan.as_ref().map(|source| source.id().as_str()),
                        "prepared_task_id":event.prepared_task().map(|task| task.id.as_str()),
                        "prepared_plan_id":event.prepared_task().and_then(|task| match &task.workflow {
                            crate::task::WorkflowSnapshot::Plan { plan } => Some(plan.artifact.id.as_str()),
                            crate::task::WorkflowSnapshot::Goal { .. } | crate::task::WorkflowSnapshot::Execute { .. } => None,
                        }),
                        "event":event.name(),"update_step":update_step,
                    }));
                    let was_preparing = run.preparing();
                    let progress = match self.input_receipts.observe_acceptance(&services) {
                        Ok(()) => run.update(
                            event,
                            foreground_status.clone(),
                            &mut *broker.lock().await,
                            &services,
                            !self.commands.storage_unavailable(),
                            self.task_delivery_gate(foreground.as_ref(), &pending)
                                == super::task_command::TaskDeliveryGate::Open,
                        ),
                        Err(error) => {
                            run.consume_completed_worker(&event);
                            Err(OperationFailure::Domain(error.into()))
                        }
                    };
                    if !was_preparing && run.preparing() {
                        trace.record(&session_id, "exchange.command.effect_requested", serde_json::json!({
                            "command_id":id.as_str(),"effect":"task_creation_preparation","update_step":update_step,
                        }));
                    }
                    while let Some(signal) = run.take_collection_signal() {
                        if let Some(runtime) = foreground.as_mut() {
                            runtime.collection_signals.push_back(signal);
                        }
                    }
                    match progress {
                        Ok(TaskCommandProgress::DeliverInput(start)) => {
                            let start = *start;
                            let output = command_messages.clone();
                            let input = InputRevision {
                                id: super::command::InputId::new(start.message.id.clone())
                                    .expect("task input identity was validated before its claim"),
                                revision: start.message.revision,
                            };
                            services.cancellation.arm();
                            self.input_delivery = Some(InputDeliveryRun::start(
                                start.message,
                                start.capabilities.execution,
                                start.capabilities.control,
                                output.clone(),
                                &services,
                            ));
                            // Retain native preparation before notifications can fail or observers disappear.
                            for update in start.updates {
                                publish_input_observation(
                                    update,
                                    output.as_ref(),
                                    &services,
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                            if let Ok(snapshot) = services.input.snapshot() {
                                publish_input_observation(
                                    crate::input::InputUpdate {
                                        revision: snapshot.revision,
                                        suspended: snapshot.suspended,
                                        control_revision: snapshot.control_revision,
                                        snapshot: Some(snapshot),
                                        operation_id: Some(id.as_str().into()),
                                        upsert: Vec::new(),
                                        removed: Vec::new(),
                                        error: None,
                                        error_code: None,
                                        prepend: false,
                                        retracted: None,
                                    },
                                    output.as_ref(),
                                    &services,
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                            trace.record(&session_id, "exchange.command.effect_requested", serde_json::json!({
                                "command_id":id.as_str(),"effect":"task_input_delivery","input_id":input.id.as_str(),
                                "input_revision":input.revision,"update_step":update_step,
                            }));
                            command = Some(ActiveCommand::Task(run));
                        }
                        Ok(TaskCommandProgress::Waiting { events, input }) => {
                            publish_control(
                                events,
                                input.as_ref(),
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                                &id,
                            );
                            command = Some(ActiveCommand::Task(run));
                        }
                        Ok(TaskCommandProgress::Finished { outcome, events }) => {
                            // A failed creation cannot become an implicit readiness retry after releasing its command slot.
                            if outcome.is_err()
                                && matches!(
                                    run.checkpoint(),
                                    Some(
                                        super::command::TaskCommandCheckpoint::InputPending { .. }
                                    )
                                )
                            {
                                match services.input.suspend(true, Some(id.as_str())) {
                                    Ok(update) => publish_input_observation(
                                        update,
                                        command_messages.as_ref(),
                                        &services,
                                        &trace,
                                        &session_id,
                                        update_step,
                                    ),
                                    Err(_) => {
                                        self.commands.uncommitted(
                                            run.command,
                                            CommandJournalError::DomainCommit,
                                            &trace,
                                            &session_id,
                                            update_step,
                                        );
                                        continue;
                                    }
                                }
                            }
                            publish_task_state(
                                command_messages.as_ref(),
                                &services,
                                &trace,
                                &session_id,
                                &id,
                                update_step,
                            );
                            publish_control(
                                events,
                                None,
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                                &id,
                            );
                            match self.commands.committed(&id) {
                                Ok(Some(record)) if record.completion().is_some() => {
                                    self.commands.complete(
                                        run.command,
                                        record,
                                        &trace,
                                        &session_id,
                                        update_step,
                                    )
                                }
                                Ok(Some(_)) | Ok(None) => self.commands.finish(
                                    run.command,
                                    outcome.map(CommandOutcome::Task),
                                    &trace,
                                    &session_id,
                                    update_step,
                                ),
                                Err(error) => self.commands.uncommitted(
                                    run.command,
                                    error,
                                    &trace,
                                    &session_id,
                                    update_step,
                                ),
                            }
                        }
                        Err(OperationFailure::Transition(TransitionFailure::Rejected(failure))) => {
                            let (queued, retained) = run.into_retained();
                            drop(retained);
                            self.commands.finish(
                                queued,
                                Err(failure),
                                &trace,
                                &session_id,
                                update_step,
                            );
                        }
                        Err(error) => {
                            trace.record(&session_id, "exchange.command.effect_failed", serde_json::json!({"command_id":id.as_str(),"code":error.code(),"reason":error.diagnostic(),"update_step":update_step}));
                            let checkpoint = run.checkpoint().cloned();
                            let preparation_overdue =
                                run.preparing() && matches!(&error, OperationFailure::JobDeadline);
                            let (queued, mut retained) = run.into_retained();
                            services.execution.fence_collection_pending();
                            if matches!(self.foreground_access, ForegroundAccess::Open) {
                                self.foreground_access = ForegroundAccess::Collecting;
                            }
                            if preparation_overdue {
                                self.commands.finish(
                                    queued,
                                    Err(CommandFailure::Task(
                                        super::command::TaskCommandFailure::PreparationTimedOut,
                                    )),
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            } else if let Some(committed) = checkpoint
                                .filter(|_| matches!(&error, OperationFailure::DispatchUnconfirmed))
                            {
                                self.commands.finish(queued, Err(CommandFailure::TaskSynchronization {
                                    committed, failure: super::command::TaskSynchronizationFailure::DispatchUnconfirmed,
                                }), &trace, &session_id, update_step);
                            } else {
                                match error {
                                    OperationFailure::Domain(_)
                                    | OperationFailure::StorageUncommitted
                                    | OperationFailure::Transition(TransitionFailure::Domain(_)) => {
                                        self.commands.uncommitted(
                                            queued,
                                            CommandJournalError::DomainCommit,
                                            &trace,
                                            &session_id,
                                            update_step,
                                        )
                                    }
                                    OperationFailure::JobDeadline
                                    | OperationFailure::Transition(TransitionFailure::Overdue) => {
                                        self.commands.finish(
                                            queued,
                                            Err(CommandFailure::CollectionTimedOut),
                                            &trace,
                                            &session_id,
                                            update_step,
                                        )
                                    }
                                    OperationFailure::JobObservationLost(_)
                                    | OperationFailure::ExecutionUnavailable
                                    | OperationFailure::DispatchUnconfirmed
                                    | OperationFailure::InputReconciliation(_)
                                    | OperationFailure::Reconciliation(_)
                                    | OperationFailure::Compaction { .. }
                                    | OperationFailure::Validation(_)
                                    | OperationFailure::Transition(TransitionFailure::Uncertain) => {
                                        self.commands.finish(
                                            queued,
                                            Err(CommandFailure::CollectionUncertain),
                                            &trace,
                                            &session_id,
                                            update_step,
                                        )
                                    }
                                    OperationFailure::Transition(TransitionFailure::Rejected(
                                        _,
                                    )) => {
                                        unreachable!("safe task rejection is handled separately")
                                    }
                                }
                            }
                            let recovery = services.clone();
                            cancellation_cleanup.spawn(async move {
                                let outcome =
                                    std::panic::AssertUnwindSafe(retained.collect(&recovery))
                                        .catch_unwind()
                                        .await
                                        .unwrap_or(Err(
                                            crate::provider::ProviderError::DeliveryUncertain,
                                        ));
                                CollectedCancellation {
                                    command_id: id,
                                    retained,
                                    phase: CancellationCleanupPhase::Native(outcome),
                                }
                            });
                        }
                    }
                    services.input.changed.notify_one();
                }
                DriverWake::Command(CommandEvent::NativeCommand(event)) => {
                    let Some(ActiveCommand::NativeCommand(mut run)) = command.take() else {
                        unreachable!("native policy evidence belongs to the active policy command")
                    };
                    let id = run.command.record.request.id.clone();
                    trace.record(&session_id, "exchange.command.event", serde_json::json!({
                        "command_id":id.as_str(),"sequence":run.command.record.sequence,"event":event.name(),"update_step":update_step,"compaction_id":run.compaction_id().map(|id| id.as_str()),
                    }));
                    let mut progress = match self.input_receipts.observe_acceptance(&services) {
                        Ok(()) => run.update(
                            event,
                            foreground_status.clone(),
                            &mut *broker.lock().await,
                            &services,
                            !self.commands.storage_unavailable(),
                        ),
                        Err(error) => {
                            match event {
                                super::native_command::NativeCommandEvent::Transition(event) => {
                                    run.retain_transition_event(event)
                                }
                                event => run.consume_completed_worker(&event),
                            }
                            Err(OperationFailure::Domain(error.into()))
                        }
                    };
                    if matches!(
                        &progress,
                        Ok(NativeCommandProgress::Finished(CommandOutcome::Configured(
                            _
                        )))
                    ) {
                        let record = super::core_log::CoreRecord::TransitionCommitted {
                            command: Some(id.clone()),
                            invocation: None,
                            kind: super::core_log::CoreTransitionKind::ConfigurationCommitted,
                            outcome: super::core_log::CoreOutcome::Succeeded,
                        };
                        if let Err(error) = record_owner_core(
                            &mut self.commands,
                            &record,
                            &trace,
                            &session_id,
                            update_step,
                        ) {
                            progress = Err(OperationFailure::Transition(error));
                        }
                    }
                    while let Some(signal) = run.take_collection_signal() {
                        if let Some(runtime) = foreground.as_mut() {
                            runtime.collection_signals.push_back(signal);
                        }
                    }
                    match progress {
                        Ok(NativeCommandProgress::Waiting { events, updates }) => {
                            publish_control(
                                events,
                                None,
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                                &id,
                            );
                            publish_updates(
                                &mut deferred_publications,
                                &mut self.commands,
                                updates,
                                command_messages.as_ref(),
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                            )
                            .await;
                            command = Some(ActiveCommand::NativeCommand(run));
                        }
                        Ok(NativeCommandProgress::Replacement(replacement)) => {
                            let messages = command_messages.clone();
                            let (completion, response) =
                                oneshot::channel::<Result<DispatchResult>>();
                            let id = replacement.id;
                            let output = messages.clone();
                            let publications = services.publications.clone();
                            internal_completions.spawn(async move {
                                let outcome = async {
                                    if let Ok(Ok(result)) = response.await {
                                        for event in result.event {
                                            publications.publish(event, output.as_ref())?;
                                        }
                                    }
                                    Ok::<(), super::publication::PublicationError>(())
                                }
                                .await;
                                (id, outcome)
                            });
                            pending.push_back(PreparedForeground {
                                id,
                                transaction: replacement.transaction,
                                messages,
                                initial: None,
                                completion: ForegroundCompletion::Caller(completion),
                                updates: Some(replacement.updates),
                                execution: Some(replacement.execution),
                            });
                            command = Some(ActiveCommand::NativeCommand(run));
                        }
                        Ok(NativeCommandProgress::Finished(outcome)) => {
                            let run = *run;
                            self.commands.finish(
                                run.command,
                                Ok(outcome),
                                &trace,
                                &session_id,
                                update_step,
                            );
                        }
                        Err(error) => {
                            trace.record(&session_id, "exchange.configuration.effect_failed", serde_json::json!({
                                "command_id":id.as_str(),"code":error.code(),"reason":error.diagnostic(),"update_step":update_step,
                            }));
                            if let OperationFailure::Validation(error) = error {
                                if error
                                    .downcast_ref::<crate::provider::ProviderError>()
                                    .is_some_and(configuration_waits_for_child)
                                    && let Command::ConfigureSession(
                                        super::command::ConfigurationChange::Settings(patch),
                                    ) = &run.command.record.request.command
                                    && let Some(attempt) = self
                                        .configuration_attempt
                                        .take()
                                        .filter(|attempt| attempt.command == id)
                                {
                                    self.deferred_configuration = Some(DeferredConfiguration {
                                        patch: patch.clone(),
                                        after: attempt.availability,
                                    });
                                }

                                let reason = error
                                    .downcast_ref::<ConfigurationRejection>()
                                    .copied()
                                    .or_else(|| {
                                        error.downcast_ref::<crate::provider::ProviderError>().map(|error| {
                                            ConfigurationRejection::Provider(
                                                crate::provider::lifecycle_error::diagnostic(
                                                    forge_provider_api::ProviderOperation::ThreadConfigure,
                                                    error,
                                                ).kind(),
                                            )
                                        })
                                    })
                                    .unwrap_or(ConfigurationRejection::Unavailable);
                                trace.record(&session_id, "exchange.configuration.rejected", serde_json::json!({
                                    "command_id":id.as_str(),"update_step":update_step,"reason":error.to_string(),
                                }));
                                self.commands.finish(
                                    run.command,
                                    Err(CommandFailure::ConfigurationRejected(reason)),
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            } else if let OperationFailure::Transition(
                                TransitionFailure::Rejected(failure),
                            ) = error
                            {
                                let (queued, _) = run.into_retained();
                                self.commands.finish(
                                    queued,
                                    Err(failure),
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            } else {
                                services.execution.fence_collection_pending();
                                if matches!(self.foreground_access, ForegroundAccess::Open) {
                                    self.foreground_access = ForegroundAccess::Collecting;
                                }
                                let (queued, mut retained) = run.into_retained();
                                let command_id = queued.record.request.id.clone();
                                match error {
                                    OperationFailure::Compaction { failure, cause } => {
                                        trace.record(&session_id, "exchange.compaction.failed", serde_json::json!({
                                            "command_id":command_id.as_str(),"update_step":update_step,"code":failure.code(),"reason":cause.to_string(),
                                        }));
                                        self.commands.finish(
                                            queued,
                                            Err(CommandFailure::Compaction(failure)),
                                            &trace,
                                            &session_id,
                                            update_step,
                                        );
                                    }
                                    OperationFailure::Reconciliation(error) => {
                                        let diagnostic = crate::provider::lifecycle_error::diagnostic(
                                            forge_provider_api::ProviderOperation::ThreadConfigure, &error,
                                        );
                                        trace.record(&session_id, "exchange.configuration.reconciliation_failed", serde_json::json!({
                                            "command_id":command_id.as_str(),"update_step":update_step,"kind":diagnostic.kind(),
                                        }));
                                        self.commands.finish(
                                            queued,
                                            Err(CommandFailure::ConfigurationReconcileFailed {
                                                kind: diagnostic.kind(),
                                            }),
                                            &trace,
                                            &session_id,
                                            update_step,
                                        );
                                    }
                                    OperationFailure::InputReconciliation(error) => {
                                        trace.record(&session_id, "exchange.input.reconciliation_failed", serde_json::json!({"command_id":command_id.as_str(),"update_step":update_step,"reason":error.to_string()}));
                                        self.commands.finish(
                                            queued,
                                            Err(CommandFailure::CollectionUncertain),
                                            &trace,
                                            &session_id,
                                            update_step,
                                        );
                                    }
                                    OperationFailure::ExecutionUnavailable => self.commands.finish(
                                        queued,
                                        Err(CommandFailure::OwnershipUnavailable),
                                        &trace,
                                        &session_id,
                                        update_step,
                                    ),
                                    OperationFailure::JobDeadline => self.commands.finish(
                                        queued,
                                        Err(CommandFailure::ConfigurationTimedOut),
                                        &trace,
                                        &session_id,
                                        update_step,
                                    ),
                                    OperationFailure::JobObservationLost(error) => {
                                        trace.record(&session_id, "exchange.configuration.worker_lost", serde_json::json!({
                                            "command_id":command_id.as_str(),"update_step":update_step,"reason":error.to_string(),
                                        }));
                                        self.commands.finish(
                                            queued,
                                            Err(CommandFailure::CollectionUncertain),
                                            &trace,
                                            &session_id,
                                            update_step,
                                        );
                                    }
                                    OperationFailure::Transition(TransitionFailure::Overdue) => {
                                        self.commands.finish(
                                            queued,
                                            Err(CommandFailure::CollectionTimedOut),
                                            &trace,
                                            &session_id,
                                            update_step,
                                        )
                                    }
                                    OperationFailure::Transition(TransitionFailure::Uncertain) => {
                                        self.commands.finish(
                                            queued,
                                            Err(CommandFailure::CollectionUncertain),
                                            &trace,
                                            &session_id,
                                            update_step,
                                        )
                                    }
                                    OperationFailure::DispatchUnconfirmed => self.commands.finish(
                                        queued,
                                        Err(CommandFailure::DispatchUnconfirmed),
                                        &trace,
                                        &session_id,
                                        update_step,
                                    ),
                                    OperationFailure::Domain(error)
                                    | OperationFailure::Transition(TransitionFailure::Domain(
                                        error,
                                    )) => {
                                        trace.record(&session_id, "exchange.configuration.commit_failed", serde_json::json!({
                                            "command_id":command_id.as_str(),"update_step":update_step,"reason":error.to_string(),
                                        }));
                                        self.commands.uncommitted(
                                            queued,
                                            CommandJournalError::DomainCommit,
                                            &trace,
                                            &session_id,
                                            update_step,
                                        );
                                    }
                                    OperationFailure::StorageUncommitted => {
                                        self.commands.uncommitted(
                                            queued,
                                            CommandJournalError::DomainCommit,
                                            &trace,
                                            &session_id,
                                            update_step,
                                        )
                                    }
                                    OperationFailure::Transition(TransitionFailure::Rejected(
                                        _,
                                    )) => unreachable!("safe rejection is handled separately"),
                                    OperationFailure::Validation(_) => unreachable!(
                                        "preflight rejection is handled without fencing execution"
                                    ),
                                }
                                let services = services.clone();
                                cancellation_cleanup.spawn(async move {
                                    let outcome =
                                        std::panic::AssertUnwindSafe(retained.collect(&services))
                                            .catch_unwind()
                                            .await
                                            .unwrap_or(Err(
                                                crate::provider::ProviderError::DeliveryUncertain,
                                            ));
                                    CollectedCancellation {
                                        command_id,
                                        retained,
                                        phase: CancellationCleanupPhase::Native(outcome),
                                    }
                                });
                            }
                        }
                    }
                }
                DriverWake::InternalCompleted(Some(Ok((id, result)))) => {
                    trace.record(
                        &session_id,
                        "exchange.foreground.completion_forwarded",
                        serde_json::json!({
                            "work_id":id,"update_step":update_step,"publication_succeeded":result.is_ok(),
                        }),
                    );
                }
                DriverWake::InternalCompleted(Some(Err(_))) => {
                    trace.record(
                        &session_id,
                        "exchange.foreground.internal_observation_lost",
                        serde_json::json!({"update_step":update_step}),
                    );
                }
                DriverWake::InternalCompleted(None) => {}
                DriverWake::Command(CommandEvent::Cancel(event)) => {
                    let Some(ActiveCommand::Cancel {
                        command: queued,
                        mut transition,
                    }) = command.take()
                    else {
                        unreachable!(
                            "cancellation evidence belongs to the active cancellation command"
                        )
                    };
                    let command_id = queued.record.request.id.clone();
                    trace.record(
                        &session_id,
                        "exchange.command.event",
                        serde_json::json!({
                            "command_id":command_id.as_str(),"sequence":queued.record.sequence,
                            "update_step":update_step,"event":event.name(),
                        }),
                    );
                    let core_transition = event.core_transition();
                    let mut progress = match self.input_receipts.observe_acceptance(&services) {
                        Ok(()) if !self.commands.storage_unavailable() => transition.update(
                            event,
                            foreground_status.clone(),
                            &mut *broker.lock().await,
                            &services,
                        ),
                        Ok(()) => {
                            transition.retain_uncommitted(event);
                            Err(TransitionFailure::Domain(anyhow::anyhow!(
                                "owner storage is unavailable"
                            )))
                        }
                        Err(error) => {
                            transition.retain_uncommitted(event);
                            Err(TransitionFailure::Domain(error.into()))
                        }
                    };
                    if let Some((kind, outcome)) = core_transition
                        && let Err(error) = record_owner_core(
                            &mut self.commands,
                            &transition.core_transition(kind, outcome),
                            &trace,
                            &session_id,
                            update_step,
                        )
                    {
                        progress = Err(error);
                    }
                    if let Ok(TransitionProgress::Finished { .. }) = &progress {
                        let record = transition.core_transition(
                            super::core_log::CoreTransitionKind::SettlementCompleted,
                            super::core_log::CoreOutcome::Succeeded,
                        );
                        if let Err(error) = record_owner_core(
                            &mut self.commands,
                            &record,
                            &trace,
                            &session_id,
                            update_step,
                        ) {
                            progress = Err(error);
                        }
                    }
                    while let Some(signal) = transition.take_collection_signal() {
                        if let Some(runtime) = foreground.as_mut() {
                            runtime.collection_signals.push_back(signal);
                        }
                    }
                    match progress {
                        Ok(TransitionProgress::Waiting { events, input }) => {
                            publish_control(
                                events,
                                input.as_deref(),
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                                &command_id,
                            );
                            command = Some(ActiveCommand::Cancel {
                                command: queued,
                                transition,
                            });
                        }
                        Ok(TransitionProgress::Finished {
                            outcome: TransitionOutcome::Cancelled(outcome),
                            events,
                        }) => {
                            publish_control(
                                events,
                                None,
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                                &command_id,
                            );
                            if self.commands.storage_unavailable() {
                                let mut queued = queued;
                                self.commands.lose_observation(
                                    &mut queued,
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            } else {
                                self.commands.finish(
                                    queued,
                                    Ok(CommandOutcome::Cancelled(outcome)),
                                    &trace,
                                    &session_id,
                                    update_step,
                                );
                            }
                        }
                        Ok(TransitionProgress::Finished {
                            outcome: TransitionOutcome::Reconfigured { .. },
                            ..
                        }) => {
                            unreachable!(
                                "explicit cancellation cannot finish as internal reconfiguration"
                            )
                        }
                        Err(TransitionFailure::Rejected(error)) => {
                            self.commands.finish(
                                queued,
                                Err(error),
                                &trace,
                                &session_id,
                                update_step,
                            );
                        }
                        Err(failure) => {
                            services.execution.fence_collection_pending();
                            if matches!(self.foreground_access, ForegroundAccess::Open) {
                                self.foreground_access = ForegroundAccess::Collecting;
                            }
                            let mut retained = transition.into_retained();
                            let command_id = queued.record.request.id.clone();
                            match failure {
                                TransitionFailure::Overdue => self.commands.finish(
                                    queued,
                                    Err(CommandFailure::CollectionTimedOut),
                                    &trace,
                                    &session_id,
                                    update_step,
                                ),
                                TransitionFailure::Uncertain => self.commands.finish(
                                    queued,
                                    Err(CommandFailure::CollectionUncertain),
                                    &trace,
                                    &session_id,
                                    update_step,
                                ),
                                TransitionFailure::Domain(error) => {
                                    trace.record(&session_id, "exchange.command.commit_failed", serde_json::json!({
                                        "command_id":command_id.as_str(),"update_step":update_step,"reason":error.to_string(),
                                    }));
                                    self.commands.uncommitted(
                                        queued,
                                        CommandJournalError::DomainCommit,
                                        &trace,
                                        &session_id,
                                        update_step,
                                    );
                                }
                                TransitionFailure::Rejected(_) => {
                                    unreachable!("safe command rejection is handled separately")
                                }
                            }
                            let services = services.clone();
                            cancellation_cleanup.spawn(async move {
                                let outcome =
                                    std::panic::AssertUnwindSafe(retained.collect(&services))
                                        .catch_unwind()
                                        .await
                                        .unwrap_or(Err(
                                            crate::provider::ProviderError::DeliveryUncertain,
                                        ));
                                CollectedCancellation {
                                    command_id,
                                    retained,
                                    phase: CancellationCleanupPhase::Native(outcome),
                                }
                            });
                        }
                    }
                }
                DriverWake::RollbackCollected(Some(Ok((id, mut recovery)))) => {
                    let (result, catalog) = {
                        let mut broker = broker.lock().await;
                        let result = if self.commands.storage_unavailable() {
                            Err(super::rollback::RollbackFailure::RecoveryRequired)
                        } else {
                            recovery.apply(&mut broker, &services)
                        };
                        *services
                            .provider
                            .write()
                            .unwrap_or_else(std::sync::PoisonError::into_inner) =
                            broker.provider_handle();
                        (result, broker.provider_catalog_request())
                    };
                    if let Ok(catalog) = catalog {
                        *services.catalog.write().await = catalog;
                        services
                            .resource_revision
                            .fetch_add(1, std::sync::atomic::Ordering::AcqRel);
                    }
                    match result {
                        Ok(super::rollback_run::RollbackRecoveryProgress::Waiting) => {
                            rollback_cleanup.spawn(async move { (id, recovery.collect().await) });
                            continue;
                        }
                        Ok(super::rollback_run::RollbackRecoveryProgress::Finished(events)) => {
                            publish_control(
                                events,
                                None,
                                OwnerUpdateContext {
                                    services: &services,
                                    trace: &trace,
                                    session: &session_id,
                                    step: update_step,
                                },
                                &id,
                            )
                        }
                        Err(error) => {
                            trace.record(&session_id, "exchange.rollback.recovery_failed", serde_json::json!({
                                "command_id":id.as_str(),"error_code":error.code(),"update_step":update_step,
                            }));
                            services.execution.close();
                            self.foreground_access = ForegroundAccess::CollectionLost;
                            retained_rollbacks.push(recovery);
                            continue;
                        }
                    }
                    let mut retained = recovery.into_transition();
                    let recovery_services = services.clone();
                    cancellation_cleanup.spawn(async move {
                        let outcome =
                            std::panic::AssertUnwindSafe(retained.collect(&recovery_services))
                                .catch_unwind()
                                .await
                                .unwrap_or(Err(crate::provider::ProviderError::DeliveryUncertain));
                        CollectedCancellation {
                            command_id: id,
                            retained,
                            phase: CancellationCleanupPhase::Native(outcome),
                        }
                    });
                }
                DriverWake::RollbackCollected(Some(Err(_))) => {
                    services.execution.close();
                    self.foreground_access = ForegroundAccess::CollectionLost;
                    trace.record(
                        &session_id,
                        "exchange.rollback.recovery_observation_lost",
                        serde_json::json!({"update_step":update_step}),
                    );
                }
                DriverWake::RollbackCollected(None) => {}
                DriverWake::CancellationCollected(Some(Ok(collected))) => {
                    let CollectedCancellation {
                        command_id,
                        mut retained,
                        phase,
                    } = collected;
                    match phase {
                        CancellationCleanupPhase::Native(outcome) => {
                            let record = super::core_log::CoreRecord::TransitionCommitted {
                                command: Some(command_id.clone()),
                                invocation: None,
                                kind: if outcome.is_ok() {
                                    super::core_log::CoreTransitionKind::CollectionCompleted
                                } else {
                                    super::core_log::CoreTransitionKind::CollectionFailed
                                },
                                outcome: if outcome.is_ok() {
                                    super::core_log::CoreOutcome::Succeeded
                                } else {
                                    super::core_log::CoreOutcome::ObservationLost
                                },
                            };
                            if record_owner_core(
                                &mut self.commands,
                                &record,
                                &trace,
                                &session_id,
                                update_step,
                            )
                            .is_err()
                            {
                                self.foreground_access = ForegroundAccess::CollectionLost;
                                retained_cancellation = Some(retained);
                                continue;
                            }
                            let facts = retained.apply_compaction_facts(&mut *broker.lock().await);
                            match facts {
                                Ok(updates) => {
                                    publish_updates(
                                        &mut deferred_publications,
                                        &mut self.commands,
                                        updates,
                                        command_messages.as_ref(),
                                        OwnerUpdateContext {
                                            services: &services,
                                            trace: &trace,
                                            session: &session_id,
                                            step: update_step,
                                        },
                                    )
                                    .await
                                }
                                Err(error) => {
                                    trace.record(&session_id, "exchange.cleanup.history_commit_failed", serde_json::json!({
                                    "command_id":command_id.as_str(),"update_step":update_step,"reason":error.to_string(),
                                }));
                                }
                            }
                            if outcome.is_ok()
                                && let Some(runtime) = foreground.as_mut()
                            {
                                runtime
                                    .collection_signals
                                    .push_back(retained.collection_completed_signal());
                            }
                            let outcome = match outcome {
                                Ok(()) => retained.apply_collected(&mut *broker.lock().await),
                                Err(_) => Err(TransitionFailure::Uncertain),
                            };
                            trace.record(&session_id, "exchange.cleanup.completed", serde_json::json!({
                                "command_id":command_id.as_str(),"update_step":update_step,"succeeded":outcome.is_ok(),
                            }));
                            let collected = outcome.is_ok();
                            if let Ok(events) = outcome {
                                publish_control(
                                    events,
                                    None,
                                    OwnerUpdateContext {
                                        services: &services,
                                        trace: &trace,
                                        session: &session_id,
                                        step: update_step,
                                    },
                                    &command_id,
                                );
                            }
                            cancellation_cleanup.spawn(async move {
                                retained.drain_host().await;
                                CollectedCancellation {
                                    command_id,
                                    retained,
                                    phase: CancellationCleanupPhase::HostDrained { collected },
                                }
                            });
                        }
                        CancellationCleanupPhase::HostDrained { collected } => {
                            retained.release_legacy_control();
                            retained_cancellation = Some(retained);
                            if matches!(self.foreground_access, ForegroundAccess::Collecting) {
                                if collected {
                                    services.execution.lift_collection_pending();
                                    services.input.changed.notify_one();
                                    self.foreground_access = ForegroundAccess::Open;
                                } else {
                                    services.execution.close();
                                    self.foreground_access = ForegroundAccess::CollectionLost;
                                }
                            }
                        }
                    }
                    for prepared in pending.drain(..) {
                        notify(
                            prepared.completion,
                            prepared.id,
                            Err(anyhow::anyhow!(
                                "execution is unavailable after cancellation collection"
                            )),
                            &trace,
                            &session_id,
                            update_step,
                        );
                    }
                }
                DriverWake::CancellationCollected(Some(Err(_))) => {
                    services.execution.close();
                    self.foreground_access = ForegroundAccess::CollectionLost;
                    trace.record(&session_id, "exchange.cleanup.failed", serde_json::json!({
                        "update_step":update_step,"reason":"cancellation collection observation lost",
                    }));
                    for prepared in pending.drain(..) {
                        notify(
                            prepared.completion,
                            prepared.id,
                            Err(anyhow::anyhow!(
                                "cancellation collection observation lost; execution remains unavailable"
                            )),
                            &trace,
                            &session_id,
                            update_step,
                        );
                    }
                }
                DriverWake::CancellationCollected(None) => {}
                DriverWake::Foreground(ForegroundInput::Update(update)) => {
                    let runtime = foreground
                        .as_ref()
                        .expect("publication belongs to the active foreground");
                    trace.record(&session_id, "exchange.event.read", serde_json::json!({
                        "work_id":runtime.id,"source":"committed_publication","update_step":update_step,
                    }));
                    publish_updates(
                        &mut deferred_publications,
                        &mut self.commands,
                        vec![*update],
                        runtime.messages.as_ref(),
                        OwnerUpdateContext {
                            services: &services,
                            trace: &trace,
                            session: &session_id,
                            step: update_step,
                        },
                    )
                    .await;
                }
                DriverWake::Foreground(ForegroundInput::PublicationClosed) => {}
                DriverWake::Foreground(ForegroundInput::Signal(signal, publication)) => {
                    let runtime = foreground
                        .as_mut()
                        .expect("foreground input belongs to the active runtime");
                    trace.record(&session_id, "exchange.event.read", serde_json::json!({
                        "work_id":runtime.id,"update_step":update_step,
                        "source":if publication { "publication_result" } else { "foreground_input" },
                        "signal":signal.diagnostic_name(),
                        "running_jobs":(runtime.jobs.len() + runtime.publication_job.len()),"staged_effects":runtime.staged_effects.iter().map(|job| job.diagnostic_name()).collect::<Vec<_>>(),
                    }));
                    runtime.read_handoff(&trace, &session_id, update_step);
                    if matches!(runtime.handoff, ForegroundHandoff::Confirmed(_))
                        && let Some(input) = self.input_delivery.as_mut()
                    {
                        input.observe_dispatch(runtime.id);
                    }
                    if let Some(command) = command.as_mut()
                        && let ForegroundHandoff::Confirmed(confirmation) = &runtime.handoff
                    {
                        command.observe_dispatch(runtime.id, confirmation);
                    }
                    let progress = match self.input_receipts.observe_acceptance(&services) {
                        Ok(()) => {
                            runtime
                                .update(
                                    &broker,
                                    *signal,
                                    publication,
                                    if self.commands.storage_unavailable() {
                                        ForegroundCommit::Unavailable
                                    } else {
                                        ForegroundCommit::Certified
                                    },
                                )
                                .await
                        }
                        Err(error) => {
                            services.execution.close();
                            self.commands
                                .retention_failed(&trace, &session_id, update_step);
                            match *signal {
                                ForegroundSignal::LiveHost(request) => {
                                    request
                                        .reject(crate::provider::ProviderError::DeliveryUncertain);
                                }
                                signal => runtime.uncommitted_signals.push(signal),
                            }
                            Err(error.into())
                        }
                    };
                    match progress {
                        Ok(ForegroundProgress::Waiting) => {}
                        Ok(ForegroundProgress::Finished) => {
                            let runtime = foreground
                                .take()
                                .expect("active runtime produced settlement");
                            if let Some(command) = command.as_mut() {
                                command.dispatch_lost(runtime.id);
                            }
                            let ForegroundRuntime {
                                transaction,
                                completion,
                                id,
                                _execution: execution,
                                ..
                            } = runtime;
                            let (outcome, catalog) = {
                                let mut broker = broker.lock().await;
                                (
                                    broker.finish_foreground(transaction),
                                    broker.provider_catalog_request(),
                                )
                            };
                            let outcome = match catalog {
                                Ok(catalog) => {
                                    *services.catalog.write().await = catalog;
                                    outcome
                                }
                                Err(error) => {
                                    services.execution.close();
                                    Err(error)
                                }
                            };
                            drop(execution);
                            notify(completion, id, outcome, &trace, &session_id, update_step);
                            // Readiness answers Waiting while foreground work is active, so input
                            // requeued during this turn needs a wake once the turn releases execution.
                            services.input.changed.notify_one();
                        }
                        Err(error) => {
                            let runtime = foreground
                                .take()
                                .expect("active runtime produced this failure");
                            if let Some(command) = command.as_mut() {
                                command.dispatch_lost(runtime.id);
                            }
                            {
                                // Fence replacement immediately, but keep reading owner inputs while
                                // the retained worker collects native work and already-started effects.
                                services.execution.fence_collection_pending();
                                self.foreground_access = ForegroundAccess::Collecting;
                                let (provider, permissions) = {
                                    let broker = broker.lock().await;
                                    (broker.provider_handle(), broker.permission_coordinator())
                                };
                                let session = session_id.clone();
                                failed_foregrounds.spawn(collect_failed_foreground(
                                    runtime,
                                    error,
                                    provider,
                                    permissions,
                                    session,
                                    trace.clone(),
                                ));
                            }
                        }
                    }
                }
                DriverWake::FailedForeground(Some(Ok(collected))) => {
                    let FailedForeground {
                        mut runtime,
                        original,
                        cleanup,
                        catalog,
                        unpublished,
                    } = collected;
                    publish_updates(
                        &mut deferred_publications,
                        &mut self.commands,
                        unpublished,
                        command_messages.as_ref(),
                        OwnerUpdateContext {
                            services: &services,
                            trace: &trace,
                            session: &session_id,
                            step: update_step,
                        },
                    )
                    .await;
                    if let Some(catalog) = catalog {
                        *services.catalog.write().await = *catalog;
                    }
                    let error = match cleanup {
                        Ok(()) => original,
                        Err(error) => original.context(error),
                    };
                    let completion =
                        std::mem::replace(&mut runtime.completion, ForegroundCompletion::Notified);
                    notify(
                        completion,
                        runtime.id,
                        Err(error),
                        &trace,
                        &session_id,
                        update_step,
                    );
                    self.foreground_access = ForegroundAccess::Unavailable(Box::new(runtime));
                    for prepared in pending.drain(..) {
                        notify(
                            prepared.completion,
                            prepared.id,
                            Err(anyhow::anyhow!(
                                "execution is unavailable after foreground collection"
                            )),
                            &trace,
                            &session_id,
                            update_step,
                        );
                    }
                }
                DriverWake::FailedForeground(Some(Err(error))) => {
                    services.execution.close();
                    self.foreground_access = ForegroundAccess::CollectionLost;
                    trace.record(
                        &session_id,
                        "exchange.cleanup.failed",
                        serde_json::json!({"update_step":update_step,"reason":error.to_string()}),
                    );
                    for prepared in pending.drain(..) {
                        notify(
                            prepared.completion,
                            prepared.id,
                            Err(anyhow::anyhow!(
                                "foreground collection observation was lost; execution remains unavailable"
                            )),
                            &trace,
                            &session_id,
                            update_step,
                        );
                    }
                }
                DriverWake::FailedForeground(None) => {}
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
            || !matches!(self.foreground_access, ForegroundAccess::Open)
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

    /// Admit caller intent and internal evidence without running nested lifecycle commands.
    async fn receive(
        &mut self,
        event: OwnerEvent,
        pending: &mut VecDeque<PreparedForeground>,
        admission: OwnerIntake<'_>,
        messages: &mut Option<MessageSender>,
        broker: &Mutex<HarnessBroker>,
        context: OwnerUpdateContext<'_>,
    ) {
        let OwnerIntake {
            active,
            steering: steering_admission,
        } = admission;
        let OwnerUpdateContext {
            services,
            trace,
            session,
            step,
        } = context;
        // Resolve readiness under the owner before consuming any capability-bearing delivery offer.
        let event = match event {
            OwnerEvent::InputReady {
                id,
                messages: output,
                reply,
            } => {
                trace.record(session, "exchange.event.read", serde_json::json!({"source":"input_readiness","readiness_id":id.0.to_string(),"update_step":step}));
                *messages = Some(output.clone());
                match self
                    .prepare_candidate_offer(
                        &output,
                        active.is_some() || !pending.is_empty(),
                        steering_admission,
                        broker,
                        OwnerUpdateContext {
                            services,
                            trace,
                            session,
                            step,
                        },
                    )
                    .await
                {
                    Ok(CandidateAdmission::Foreground(offer)) => OwnerEvent::InputOffer {
                        offer,
                        messages: output,
                        reply: InputOfferReply::Ready { id, reply },
                    },
                    Ok(CandidateAdmission::Steering(offer)) => OwnerEvent::SteeringOffer {
                        offer,
                        messages: output,
                        reply: InputOfferReply::Ready { id, reply },
                    },
                    Ok(CandidateAdmission::Waiting) => {
                        notify_input_readiness(
                            id,
                            reply,
                            Ok(InputReadiness::Waiting),
                            trace,
                            session,
                            None,
                            step,
                        );
                        return;
                    }
                    Ok(CandidateAdmission::WaitExecution) => {
                        notify_input_readiness(
                            id,
                            reply,
                            Ok(InputReadiness::WaitExecution),
                            trace,
                            session,
                            None,
                            step,
                        );
                        return;
                    }
                    Err(error) => {
                        notify_input_readiness(id, reply, Err(error), trace, session, None, step);
                        return;
                    }
                }
            }
            event => event,
        };
        match event {
            OwnerEvent::PlanQuestionAnswer { answer, reply } => {
                if self.commands.storage_unavailable() {
                    let _ = reply.send(Err(super::QuestionEventError::AdmissionClosed));
                    return;
                }
                let operation = answer.operation_id().to_owned();
                trace.record(session, "exchange.event.read", serde_json::json!({
                    "source":"plan_question_answer","operation_id":operation,
                    "question_id":answer.question_id(),"question_revision":answer.revision(),"update_step":step,
                }));
                let permit = QuestionUpdatePermit { session };
                let result = super::plan_question::commit(&permit, &answer, services);
                if result.is_ok() {
                    self.question_inputs.request();
                    services.input.changed.notify_one();
                }
                let code = result.as_ref().err().map(super::QuestionEventError::code);
                let reason = result.as_ref().err().map(|error| format!("{error:?}"));
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.question.response", serde_json::json!({
                    "operation_id":operation,"question_id":answer.question_id(),"observed":observed,
                    "error_code":code,"reason":reason,"update_step":step,
                }));
            }
            OwnerEvent::QuestionChange { change, reply } => {
                if self.commands.storage_unavailable() {
                    let _ = reply.send(Err(super::QuestionEventError::AdmissionClosed));
                    return;
                }
                if change.tool_call().is_some() {
                    self.stage_question_tool(super::question_tool::QuestionTool::Change {
                        change,
                        reply,
                    });
                    return;
                }
                if let Err(error) = self.question_waits.update(services, trace, session, step) {
                    if error.error().requires_storage_fence() {
                        self.commands.retention_failed(trace, session, step);
                        services.execution.close();
                    }
                    trace.record(session, "exchange.question_wait.retirement_failed", serde_json::json!({"wait_id":error.wait_id().to_string(),"error_code":error.error().code(),"reason":error.to_string(),"update_step":step}));
                }
                let operation = change.operation_id().to_owned();
                let question = change.question().as_str().to_owned();
                let result = change.apply(&services.questions, &services.tasks);
                if result.is_ok() {
                    self.question_inputs.request();
                    services.input.changed.notify_one();
                }
                let code = result.as_ref().err().map(super::QuestionEventError::code);
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.question.response", serde_json::json!({"operation_id":operation,"question_id":question,"observed":observed,"error_code":code,"update_step":step}));
            }
            OwnerEvent::QuestionCreate { creation, reply } => {
                self.stage_question_tool(super::question_tool::QuestionTool::Create {
                    creation,
                    reply,
                });
            }
            OwnerEvent::QuestionWait { creation, reply } => {
                self.stage_question_tool(super::question_tool::QuestionTool::Wait {
                    creation: creation.waiting(),
                    reply,
                });
            }
            #[cfg(test)]
            OwnerEvent::QuestionsReady {
                messages: output,
                reply,
            } => {
                if let Some(output) = output {
                    *messages = Some(output);
                }
                self.question_inputs.request();
                let result = self
                    .question_inputs
                    .update(services, messages.as_ref())
                    .and_then(|progress| {
                        progress.ok_or(super::QuestionInputError::ObservationLost)
                    });
                let _ = reply.send(result);
            }

            OwnerEvent::TaskChecksReady {
                messages: output,
                reply,
            } => {
                *messages = Some(output);
                self.task_check_requests.push(reply);
            }
            OwnerEvent::TaskInputReady {
                id,
                messages: output,
                reply,
            } => {
                trace.record(session, "exchange.event.read", serde_json::json!({"source":"task_input_readiness","readiness_id":id.0.to_string(),"update_step":step}));
                if messages.is_none() {
                    *messages = Some(output.clone());
                }
                let production = (|| {
                    if self.commands.storage_unavailable() {
                        return Err(InputOfferError::StateUnavailable);
                    }
                    if self.receiver.is_closed()
                        || active.is_some()
                        || self.commands.has_pending()
                        || services.tasks.configuration_is_uncertain()
                        || !matches!(self.foreground_access, ForegroundAccess::Open)
                    {
                        return Ok(None);
                    }
                    let Ok(_control) = services.control.clone().try_acquire_owned() else {
                        return Ok(None);
                    };
                    super::task_input::reconcile(services).map(Some)
                })();
                let disposition = production.as_ref().ok().map(|production| match production {
                    None => "blocked",
                    Some(TaskInputProduction::Idle) => "idle",
                    Some(TaskInputProduction::Updated(_)) => "updated",
                    Some(TaskInputProduction::ContinuationCancelled { .. }) => {
                        "continuation_cancelled"
                    }
                    Some(TaskInputProduction::Superseded) => "superseded",
                });
                let result = production.map(|production| match production {
                    Some(TaskInputProduction::Updated(updates)) => updates.into_iter().for_each(|update| publish_input_observation(update, Some(&output), services, trace, session, step)),
                    Some(TaskInputProduction::ContinuationCancelled { task, input }) => {
                        trace.record(session, "exchange.task_input.continuation_cancelled", serde_json::json!({"readiness_id":id.0.to_string(),"task_id":task.as_str(),"input_id":input.as_str(),"update_step":step}));
                        let publication = services.observer.snapshot().and_then(|snapshot| {
                            retain_publication(forge_protocol::message::SessionEvent {
                                session_id: session.into(), event: "session_changed".into(), payload: serde_json::json!({"snapshot":snapshot}),
                            }, Some(&output), services, trace, session, step)?;
                            Ok(())
                        });
                        if let Err(error) = publication {
                            trace.record(session, "exchange.task_input.publication_failed", serde_json::json!({"readiness_id":id.0.to_string(),"update_step":step,"reason":error.to_string()}));
                        }
                    }
                    None | Some(TaskInputProduction::Idle | TaskInputProduction::Superseded) => {}
                });
                let code = result.as_ref().err().map(InputOfferError::code);
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.task_input.response", serde_json::json!({"readiness_id":id.0.to_string(),"update_step":step,"disposition":disposition,"error_code":code,"observed":observed}));
            }
            OwnerEvent::LocalInputSettled {
                input,
                outcome,
                messages: output,
                reply,
            } => {
                trace.record(session, "exchange.event.read", serde_json::json!({"source":"local_input_settlement","input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,"failure":outcome.as_ref().err().map(CommandFailure::code)}));
                let result = if self.commands.storage_unavailable() {
                    Err(InputOfferError::StateUnavailable)
                } else {
                    (|| {
                        let snapshot = services.input.snapshot().map_err(InputOfferError::Queue)?;
                        let Some(source) = snapshot.messages.iter().find(|message| {
                            message.id == input.id.as_str() && message.revision == input.revision
                        }) else {
                            return Ok(None);
                        };
                        if crate::task::command::SessionCommand::queued_input(source)
                            .map_err(InputOfferError::Queue)?
                            .is_none()
                        {
                            return Err(InputOfferError::InvalidLocalResult);
                        }
                        let (delivery, error) = match outcome {
                            Ok(()) => (None, None),
                            Err(failure) => (
                                Some(crate::input::InputDelivery::Failed),
                                Some(failure.to_string()),
                            ),
                        };
                        services
                            .input
                            .delivery_if_current(
                                input.id.as_str(),
                                crate::input::InputDeliveryGuard::Queued(input.revision),
                                delivery,
                                None,
                                error,
                            )
                            .map_err(InputOfferError::Queue)
                    })()
                };
                let result = result.map(|update| {
                    if let Some(update) = update {
                        publish_input_observation(
                            update,
                            Some(&output),
                            services,
                            trace,
                            session,
                            step,
                        );
                    }
                });
                let succeeded = result.is_ok();
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.local_input.response", serde_json::json!({"input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,"succeeded":succeeded,"observed":observed}));
            }
            OwnerEvent::InputReady { .. } => {
                unreachable!("readiness is resolved before delivery admission")
            }
            OwnerEvent::SteeringOffer {
                offer,
                messages: output,
                reply,
            } => {
                let SteeringDeliveryOffer {
                    input,
                    control,
                    order,
                } = offer;
                *messages = Some(output.clone());
                trace.record(session, "exchange.event.read", serde_json::json!({"source":"input_steering_offer","input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step}));
                let unavailable = active.is_some()
                    || matches!(steering_admission, SteeringAdmission::Closed)
                    || self.receiver.is_closed()
                    || self.commands.has_pending()
                    || services.tasks.configuration_is_uncertain()
                    || self.commands.storage_unavailable()
                    || !pending.is_empty()
                    || self
                        .input_delivery
                        .as_ref()
                        .is_some_and(InputDeliveryRun::blocks_input_admission)
                    || self.steering.len() >= crate::provider::SteeringLane::CAPACITY
                    || self.steering.contains_key(&input.id)
                    || !matches!(self.foreground_access, ForegroundAccess::Open);
                let outcome = async {
                    if unavailable {
                        return Ok(false);
                    }
                    let snapshot = services.input.snapshot().map_err(InputOfferError::Queue)?;
                    if snapshot.messages.iter().any(|message| message.delivery() == crate::input::InputDelivery::Unknown) { return Ok(false); }
                    let Some(message) = snapshot.messages.iter().find(|message| message.id == input.id.as_str() && message.revision == input.revision).cloned() else { return Ok(false); };
                    if !steering_input_eligible(&message, services)?
                        || snapshot.suspended && !matches!(message.purpose, crate::input::InputPurpose::Question { .. } | crate::input::InputPurpose::Clarification { .. })
                        || *services.cancellation.subscribe().borrow() {
                        return Ok(false);
                    }
                    if !crate::input::input_purpose_is_current(&services.tasks, &services.questions, &message.purpose, &services.root, services.observer.session_id()).map_err(InputOfferError::Authority)? { return Ok(false); }
                    let ready = services.observer.input_readiness().map_err(InputOfferError::Queue)?;
                    if ready.has_approval || ready.checks_running { return Ok(false); }
                    let mut broker = broker.lock().await;
                    let view = broker.owner_control_view().map_err(InputOfferError::Queue)?;
                    publish_input_events(view.events, Some(&output), services, trace, session, &input, step);
                    let registry = broker.execution_registry();
                    let target = if let Some(parent) = broker.parent_input.capture().filter(crate::provider::SteeringTarget::is_open) {
                        SteeringDispatchTarget::parent(parent, registry, crate::provider::ThreadOwner::root(session))
                    } else { return Ok(false); };
                    let Some(message) = services.input.claim(input.id.as_str(), input.revision, crate::input::InputRoute::Steering).map_err(InputOfferError::Queue)? else { return Ok(false); };
                    let claimed_revision = message.revision;
                    self.steering.insert(input.id.clone(), InputDeliveryRun::start_steering(message, control, order, target, output.clone(), services));
                    let snapshot = services.input.snapshot().map_err(InputOfferError::Queue)?;
                    publish_input_observation(crate::input::InputUpdate { revision: snapshot.revision, suspended: snapshot.suspended,
                        control_revision: snapshot.control_revision, snapshot: Some(snapshot), operation_id: None,
                        upsert: Vec::new(), removed: Vec::new(), error: None, error_code: None,
                        prepend: false, retracted: None }, Some(&output), services, trace, session, step);
                    trace.record(session, "exchange.event.applied", serde_json::json!({"source":"input_steering_offer","input_id":input.id.as_str(),"input_revision":input.revision,"claimed_revision":claimed_revision,"update_step":step}));
                    Ok(true)
                }.await;
                notify_input_offer(reply, outcome, trace, session, &input, step);
            }
            OwnerEvent::InputOffer {
                offer,
                messages: output,
                reply,
            } => {
                let InputDeliveryOffer {
                    input,
                    execution,
                    control,
                } = offer;
                *messages = Some(output.clone());
                trace.record(session, "exchange.event.read", serde_json::json!({
                    "source":"input_delivery_offer","input_id":input.id.as_str(),"input_revision":input.revision,"update_step":step,
                }));
                if active.is_some()
                    || self.receiver.is_closed()
                    || self.commands.has_pending()
                    || services.tasks.configuration_is_uncertain()
                    || self.commands.storage_unavailable()
                    || self.input_delivery.is_some()
                    || !self.steering.is_empty()
                    || !pending.is_empty()
                    || !matches!(self.foreground_access, ForegroundAccess::Open)
                {
                    drop(control);
                    drop(execution);
                    notify_input_offer(reply, Ok(false), trace, session, &input, step);
                    return;
                }
                let selected =
                    (|| -> Result<Option<crate::input::InputMessage>, InputOfferError> {
                        let snapshot = services.input.snapshot().map_err(InputOfferError::Queue)?;
                        let Some(message) = snapshot.messages.iter().find(|message| {
                            message.id == input.id.as_str() && message.revision == input.revision
                        }) else {
                            return Ok(None);
                        };
                        if !crate::input::input_purpose_is_current(
                            &services.tasks,
                            &services.questions,
                            &message.purpose,
                            &services.root,
                            services.observer.session_id(),
                        )
                        .map_err(InputOfferError::Authority)?
                        {
                            return Ok(None);
                        }
                        let ready = services
                            .observer
                            .input_readiness()
                            .map_err(InputOfferError::Queue)?;
                        if ready.checks_running
                            || ready.has_approval
                            || message.intent == crate::input::InputIntent::Queue
                                && ready.has_elicitation
                        {
                            return Ok(None);
                        }
                        Ok(Some(message.clone()))
                    })();
                match selected {
                    Ok(Some(message)) => {
                        if message.intent == crate::input::InputIntent::Queue {
                            let view = broker.lock().await.owner_control_view();
                            match view {
                                Ok(view) => {
                                    publish_input_events(
                                        view.events,
                                        Some(&output),
                                        services,
                                        trace,
                                        session,
                                        &input,
                                        step,
                                    );
                                }
                                Err(error) => {
                                    drop(control);
                                    drop(execution);
                                    notify_input_offer(
                                        reply,
                                        Err(InputOfferError::Queue(error)),
                                        trace,
                                        session,
                                        &input,
                                        step,
                                    );
                                    return;
                                }
                            }
                        }
                        let message = match services.input.claim(
                            input.id.as_str(),
                            input.revision,
                            crate::input::InputRoute::Turn,
                        ) {
                            Ok(Some(message)) => message,
                            Ok(None) => {
                                drop(control);
                                drop(execution);
                                notify_input_offer(reply, Ok(false), trace, session, &input, step);
                                return;
                            }
                            Err(error) => {
                                drop(control);
                                drop(execution);
                                notify_input_offer(
                                    reply,
                                    Err(InputOfferError::Queue(error)),
                                    trace,
                                    session,
                                    &input,
                                    step,
                                );
                                return;
                            }
                        };
                        if let Ok(snapshot) = services.input.snapshot() {
                            publish_input_observation(
                                crate::input::InputUpdate {
                                    revision: snapshot.revision,
                                    suspended: snapshot.suspended,
                                    control_revision: snapshot.control_revision,
                                    snapshot: Some(snapshot),
                                    operation_id: None,
                                    upsert: Vec::new(),
                                    removed: Vec::new(),
                                    error: None,
                                    error_code: None,
                                    prepend: false,
                                    retracted: None,
                                },
                                Some(&output),
                                services,
                                trace,
                                session,
                                step,
                            );
                        }
                        let claimed_revision = message.revision;
                        services.cancellation.arm();
                        self.input_delivery = Some(InputDeliveryRun::start(
                            message,
                            execution,
                            control,
                            Some(output.clone()),
                            services,
                        ));
                        trace.record(session, "exchange.event.applied", serde_json::json!({
                            "source":"input_delivery_offer","input_id":input.id.as_str(),"input_revision":input.revision,"claimed_revision":claimed_revision,"update_step":step,
                        }));
                        notify_input_offer(reply, Ok(true), trace, session, &input, step);
                    }
                    Ok(None) => {
                        drop(control);
                        drop(execution);
                        notify_input_offer(reply, Ok(false), trace, session, &input, step);
                    }
                    Err(error) => {
                        drop(control);
                        drop(execution);
                        notify_input_offer(reply, Err(error), trace, session, &input, step);
                    }
                }
            }
            #[cfg(test)]
            OwnerEvent::InputReceipt {
                fact,
                messages: output,
                reply,
            } => {
                if let Some(output) = output {
                    *messages = Some(output);
                }
                let source = fact.name();
                let submission = fact.submission().cloned();
                let input = fact.input().cloned();
                trace.record(
                    session,
                    "exchange.event.read",
                    serde_json::json!({
                    "source":source,"submission_id":submission.as_ref().map(|id| id.as_str()),
                    "input_id":input.as_ref().map(|input| input.id.as_str()),
                    "input_revision":input.as_ref().map(|input| input.revision),"update_step":step,
                    }),
                );
                let result = self.input_receipts.apply(fact, services);
                if let Ok(Some(update)) = &result {
                    publish_input_observation(
                        update.clone(),
                        messages.as_ref(),
                        services,
                        trace,
                        session,
                        step,
                    );
                }
                if result
                    .as_ref()
                    .err()
                    .is_some_and(InputReceiptError::fences_execution)
                {
                    services.execution.close();
                    if let Some(submission) = &submission {
                        self.input_receipts.suspend_observation(submission);
                    }
                }
                trace.record(
                    session,
                    if result.is_ok() {
                        "exchange.event.applied"
                    } else {
                        "exchange.event.rejected"
                    },
                    serde_json::json!({
                    "source":source,"submission_id":submission.as_ref().map(|id| id.as_str()),"update_step":step,
                        "reason":result.as_ref().err().map(ToString::to_string),
                    }),
                );
                let observed = reply.send(result).is_ok();
                trace.record(session, "exchange.input_receipt.response", serde_json::json!({
                    "source":source,"submission_id":submission.as_ref().map(|id| id.as_str()),"update_step":step,"observed":observed,
                }));
            }
            OwnerEvent::ReviewQuestion {
                scope,
                selection,
                catalog,
                sink,
                messages: output,
                request_id,
                reply,
            } => {
                let available = active.is_none()
                    && pending.is_empty()
                    && !self.commands.has_pending()
                    && !self.receiver.is_closed()
                    && !self.commands.storage_unavailable()
                    && matches!(self.foreground_access, ForegroundAccess::Open);
                let prepared = async {
                    anyhow::ensure!(available, "review answer owner is busy");
                    let control = services
                        .control
                        .clone()
                        .try_acquire_owned()
                        .context("review answer control is busy")?;
                    let execution = services
                        .execution
                        .clone()
                        .try_acquire_owned()
                        .context("review answer execution is busy")?;
                    let mut broker = broker.lock().await;
                    let catalog = match catalog {
                        Some(catalog) => broker.commit_configuration_catalog(catalog)?,
                        None => Vec::new(),
                    };
                    let prompt = broker.prepare_review_question(
                        &scope,
                        &selection,
                        &catalog,
                        Some((&output, request_id)),
                    )?;
                    let transaction = broker.begin_review_question_dispatch(
                        prompt,
                        scope.question.body.clone(),
                        sink,
                    )?;
                    drop(broker);
                    drop(control);
                    Ok::<_, anyhow::Error>((transaction, execution))
                }
                .await;
                match prepared {
                    Ok((transaction, execution)) => pending.push_back(PreparedForeground {
                        id: uuid::Uuid::new_v4(),
                        transaction,
                        messages: Some(output),
                        initial: None,
                        completion: ForegroundCompletion::Review {
                            reply,
                            scope,
                            root: services.root.clone(),
                        },
                        updates: None,
                        execution: Some(execution),
                    }),
                    Err(error) => {
                        let _ = reply.send(Err(error));
                    }
                }
            }
            OwnerEvent::Command {
                intent,
                task_admission,
                messages: output,
                reply,
            } => {
                let (received_id, received_command) = match intent.as_ref() {
                    #[cfg(test)]
                    CommandIntent::Fresh { id, command } => (id, command),
                    CommandIntent::Identified(request) => (&request.id, &request.command),
                };
                trace.record(session, "exchange.command.received", serde_json::json!({
                    "command_id":received_id.as_str(),"command":received_command.name(),"update_step":step,
                }));
                if let Some(output) = output {
                    *messages = Some(output);
                }
                #[cfg(not(test))]
                let CommandIntent::Identified(request) = *intent;
                #[cfg(test)]
                let request = match *intent {
                    CommandIntent::Identified(request) => request,
                    #[cfg(test)]
                    CommandIntent::Fresh { id, command } => CommandRequest {
                        id,
                        command,
                        input_control: InputControl::AtExecution,
                    },
                };
                self.commands.admit(
                    CommandAdmission {
                        request,
                        reply,
                        task_admission,
                    },
                    active.map(ActiveCommand::queued_mut),
                    trace,
                    session,
                    step,
                );
            }
            OwnerEvent::ApprovalTimeout { schedule_revision } => {
                self.pending_approval_timeout = Some(schedule_revision);
            }
            OwnerEvent::RecoverObservation { inbox, reply } => {
                self.observation_recovery.request(
                    inbox,
                    &services.observer.session_events(),
                    reply,
                );
            }
            OwnerEvent::ProviderInitialized {
                initialized,
                messages: output,
                reply,
            } => {
                if let Some(output) = output.as_ref() {
                    *messages = Some(output.clone());
                }
                let mut result = if self.commands.storage_unavailable() {
                    Err(super::provider_lifecycle::ProviderLifecycleFailure::StateUnavailable)
                } else {
                    let broker = broker.lock().await;
                    initialized.and_then(|initialized| initialized.commit(&broker))
                };
                if result.is_ok() {
                    let catalog = { broker.lock().await.provider_catalog_request() };
                    match catalog {
                        Ok(catalog) => {
                            *services.catalog.write().await = catalog;
                            services
                                .resource_revision
                                .fetch_add(1, std::sync::atomic::Ordering::AcqRel);
                        }
                        Err(_) => result = Err(
                            super::provider_lifecycle::ProviderLifecycleFailure::StateUnavailable,
                        ),
                    }
                }
                if matches!(
                    &result,
                    Err(super::provider_lifecycle::ProviderLifecycleFailure::StateUnavailable)
                ) {
                    services.execution.close();
                    self.foreground_access = ForegroundAccess::CollectionLost;
                }
                let _ = reply.send(result);
            }
            OwnerEvent::ProviderReady {
                messages: output,
                reply,
            } => {
                trace.record(
                    session,
                    "exchange.event.read",
                    serde_json::json!({
                        "source":"provider_session_inbox","update_step":step,
                    }),
                );
                if messages.is_none() {
                    *messages = output.clone();
                }
                let (observed, retirement) = {
                    let mut broker = broker.lock().await;
                    let observed = self
                        .input_receipts
                        .observe_acceptance(services)
                        .map_err(anyhow::Error::from)
                        .and_then(|()| broker.drain_session_events());
                    let retired_binding = match services.observer.session_events().end() {
                        Some(
                            crate::provider::session_execution::ObservationEnd::Collected {
                                retired_binding,
                                ..
                            }
                            | crate::provider::session_execution::ObservationEnd::Lost {
                                retired_binding,
                                ..
                            },
                        ) => retired_binding,
                        None => None,
                    };
                    let retirement = retired_binding
                        .as_ref()
                        .map(|binding| broker.observed_binding_retired(binding))
                        .transpose();
                    (observed, retirement)
                };
                // Consume the actual closed generation even when observation loss is reported after its prefix.
                let retirement = match retirement {
                    Ok(Some(delivery)) => delivery.deliver().await,
                    Ok(None) => Ok(Vec::new()),
                    Err(error) => Err(error),
                };
                let observed =
                    match (observed, retirement) {
                        (Ok(mut events), Ok(retirement)) => {
                            events.extend(retirement);
                            Ok(events)
                        }
                        (Err(error), Ok(_)) | (Ok(_), Err(error)) => Err(error),
                        (Err(error), Err(retirement)) => Err(error
                            .context(format!("closed binding publication failed: {retirement:#}"))),
                    };
                let observed = observed
                    .and_then(|events| {
                        let event_count = events.len();
                        for event in events {
                            retain_publication(
                                event,
                                output.as_ref(),
                                services,
                                trace,
                                session,
                                step,
                            )?;
                        }
                        trace.record(session, "exchange.event.applied", serde_json::json!({
                        "source":"provider_session_inbox","update_step":step,"events":event_count,
                    }));
                        Ok(())
                    })
                    .map_err(ProviderObservationError::Commit);
                let _ = reply.send(observed);
            }
            OwnerEvent::ReconnectSnapshot {
                cursor,
                limit,
                reply,
            } => {
                if self.reconnect_requests.len() >= OWNER_QUEUE_CAPACITY {
                    let _ = reply.send(Err(super::ReconnectError::QueueFull));
                } else {
                    self.reconnect_requests.push_back(PendingReconnect {
                        cursor,
                        limit,
                        reply,
                    });
                }
            }
            OwnerEvent::ReconnectConfirmed {
                confirmation,
                messages: output,
                reply,
            } => {
                let result = confirmation.confirm(services);
                if result.is_ok() && output.is_some() {
                    *messages = output;
                }
                trace.record(
                    session,
                    "exchange.reconnect.confirmed",
                    serde_json::json!({
                        "update_step":step,"succeeded":result.is_ok(),
                    }),
                );
                let _ = reply.send(result);
            }
            OwnerEvent::Status { id, reply } => {
                let _ = reply.send(
                    self.commands
                        .committed(&id)
                        .map_err(|_| ExecuteCommandError::ObservationLost),
                );
            }
            OwnerEvent::Close => {
                for request in self.reconnect_requests.drain(..) {
                    let _ = request.reply.send(Err(super::ReconnectError::OwnerClosed));
                }
                self.question_waits.close();
                self.receiver.close();
                self.commands.close(trace, session, step);
            }
        }
    }
}

/// Hold owner-acquired capabilities until admission installs their retained delivery pipeline.
enum CandidateAdmission {
    Foreground(InputDeliveryOffer),
    Steering(SteeringDeliveryOffer),
    Waiting,
    WaitExecution,
}

enum OwnerEvent {
    RecoverObservation {
        inbox: Arc<crate::provider::session_execution::SessionObservationInbox>,
        reply: oneshot::Sender<Result<(), super::provider_lifecycle::ProviderLifecycleFailure>>,
    },
    ReviewQuestion {
        scope: Arc<crate::plan::review_answer::ReviewAnswerScope>,
        selection: crate::settings::AskModel,
        catalog: Option<crate::broker::ConfigurationCatalogResult>,
        sink: crate::broker::publication::SessionUpdateSink,
        messages: MessageSender,
        request_id: u64,
        reply: oneshot::Sender<Result<DispatchResult>>,
    },
    ApprovalTimeout {
        schedule_revision: u64,
    },
    TaskChecksReady {
        messages: MessageSender,
        reply: oneshot::Sender<Result<Option<InputReadiness>, InputOfferError>>,
    },
    ProviderInitialized {
        initialized: Result<
            super::provider_lifecycle::ProviderInitialization,
            super::provider_lifecycle::ProviderLifecycleFailure,
        >,
        messages: Option<MessageSender>,
        reply: oneshot::Sender<Result<(), super::provider_lifecycle::ProviderLifecycleFailure>>,
    },
    ReconnectSnapshot {
        cursor: Option<super::publication::PublicationSequence>,
        limit: std::num::NonZeroU16,
        reply: oneshot::Sender<Result<super::ReconnectSnapshot, super::ReconnectError>>,
    },
    ReconnectConfirmed {
        confirmation: super::ReconnectConfirmation,
        messages: Option<MessageSender>,
        reply: oneshot::Sender<Result<(), super::ReconnectError>>,
    },
    PlanQuestionAnswer {
        answer: crate::plan::PlanQuestionAnswerRequest,
        reply: oneshot::Sender<
            Result<crate::plan::PlanQuestionAnswerCommit, super::QuestionEventError>,
        >,
    },
    QuestionChange {
        change: super::QuestionChange,
        reply:
            oneshot::Sender<Result<crate::question::QuestionSnapshot, super::QuestionEventError>>,
    },
    QuestionCreate {
        creation: super::QuestionCreation,
        reply: oneshot::Sender<Result<Vec<crate::question::QuestionId>, super::QuestionEventError>>,
    },
    QuestionWait {
        creation: super::QuestionCreation,
        reply:
            oneshot::Sender<Result<crate::question::OwnedQuestionWait, super::QuestionEventError>>,
    },
    #[cfg(test)]
    QuestionsReady {
        messages: Option<MessageSender>,
        reply: oneshot::Sender<Result<super::QuestionInputProgress, super::QuestionInputError>>,
    },
    TaskInputReady {
        id: InputReadinessId,
        messages: MessageSender,
        reply: oneshot::Sender<Result<(), InputOfferError>>,
    },
    LocalInputSettled {
        input: super::command::InputRevision,
        outcome: Result<(), CommandFailure>,
        messages: MessageSender,
        reply: oneshot::Sender<Result<(), InputOfferError>>,
    },
    InputReady {
        id: InputReadinessId,
        messages: MessageSender,
        reply: oneshot::Sender<Result<InputReadiness, InputOfferError>>,
    },
    SteeringOffer {
        offer: SteeringDeliveryOffer,
        messages: MessageSender,
        reply: InputOfferReply,
    },
    InputOffer {
        offer: InputDeliveryOffer,
        messages: MessageSender,
        reply: InputOfferReply,
    },
    #[cfg(test)]
    InputReceipt {
        fact: InputReceiptFact,
        messages: Option<MessageSender>,
        reply: oneshot::Sender<Result<Option<crate::input::InputUpdate>, InputReceiptError>>,
    },
    Command {
        intent: Box<CommandIntent>,
        task_admission: Option<tokio::sync::OwnedSemaphorePermit>,
        messages: Option<MessageSender>,
        reply: CommandReply,
    },
    ProviderReady {
        messages: Option<MessageSender>,
        reply: oneshot::Sender<Result<(), ProviderObservationError>>,
    },
    Status {
        id: CommandId,
        reply: oneshot::Sender<Result<Option<CommandRecord>, ExecuteCommandError>>,
    },
    Close,
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

enum CommandIntent {
    #[cfg(test)]
    Fresh {
        id: CommandId,
        command: Command,
    },
    Identified(CommandRequest),
}

/// Keep replacement barred when collection or a prior transition cannot establish availability.
enum ForegroundAccess {
    Open,
    Collecting,
    Unavailable(Box<ForegroundRuntime>),
    CollectionLost,
}

enum InputDeliveryTarget {
    Foreground,
    Steering(super::command::InputId),
}

/// Observe one steering stage without consuming any ready sibling's join handle.
async fn next_steering(
    runs: &mut HashMap<super::command::InputId, InputDeliveryRun>,
    foreground: ForegroundStatus,
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
    CancellationCollected(Option<Result<CollectedCancellation, tokio::task::JoinError>>),
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
struct InputReadinessId(uuid::Uuid);

/// Keep fixture capability injection off the production readiness path.
enum InputOfferReply {
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
        InputPurpose::TaskEntry { start, .. } => {
            services
                .observer
                .task_steering_supported(start.kind)
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
    let published = (|| -> Result<()> {
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
    })();
    if let Err(error) = published {
        trace.record(session, "exchange.command.publication_failed", serde_json::json!({"command_id":command.as_str(),"source":"task_commit","update_step":step,"reason":error.to_string()}));
    }
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

/// Derive collection readiness from owned foreground work and explicit availability.
fn foreground_status(
    access: &ForegroundAccess,
    scope: Result<Option<crate::provider::InvocationScope>, crate::provider::ProviderError>,
    has_work: bool,
) -> ForegroundStatus {
    let scope = match scope {
        Ok(scope) => scope,
        Err(_) => return ForegroundStatus::Unavailable,
    };
    match access {
        ForegroundAccess::Open if has_work => {
            scope.map_or(ForegroundStatus::Local, ForegroundStatus::Active)
        }
        ForegroundAccess::Open => ForegroundStatus::Quiescent,
        ForegroundAccess::Collecting
        | ForegroundAccess::Unavailable(_)
        | ForegroundAccess::CollectionLost => ForegroundStatus::Unavailable,
    }
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
    runtime: ForegroundRuntime,
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
    mut runtime: ForegroundRuntime,
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
    runtime.dispatch_effects(ForegroundEffectGate::CollectionOnly);
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
                    ForegroundDispatch::Running { transaction, .. }
                    | ForegroundDispatch::Finalizing { transaction, .. },
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
    runtime: &mut ForegroundRuntime,
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

struct PreparedForeground {
    id: uuid::Uuid,
    transaction: ForegroundDispatch,
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

impl From<Option<&ForegroundRuntime>> for SteeringAdmission {
    /// Capture the exact foreground state without retaining its non-Sync runtime across await.
    fn from(foreground: Option<&ForegroundRuntime>) -> Self {
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
enum ForegroundHandoff {
    Preparing,
    Watching {
        scope: crate::provider::InvocationScope,
        observer: crate::provider::DispatchObserver,
    },
    Confirmed(Box<forge_provider_api::submission::DispatchConfirmation>),
    Unconfirmed,
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

struct ForegroundRuntime {
    id: uuid::Uuid,
    transaction: ForegroundDispatch,
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
    handoff: ForegroundHandoff,
    updates: Option<crate::broker::publication::SessionUpdateStream>,
    _execution: Option<crate::command::ExecutionPermit>,
}

impl ForegroundRuntime {
    /// Install all owned resources before polling another phase.
    fn new(
        prepared: PreparedForeground,
        publisher: Arc<super::publication::SessionPublications>,
    ) -> Self {
        let PreparedForeground {
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
            handoff: ForegroundHandoff::Preparing,
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
        mut prepared: PreparedForeground,
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
        if matches!(self.handoff, ForegroundHandoff::Preparing)
            && let Some((scope, observer)) = self.transaction.dispatch_observer()
        {
            self.handoff = ForegroundHandoff::Watching { scope, observer };
        }
        match &self.handoff {
            ForegroundHandoff::Watching { observer, .. } => Some(PendingHandoff {
                work_id: self.id,
                observer: observer.clone(),
            }),
            ForegroundHandoff::Preparing
            | ForegroundHandoff::Confirmed(_)
            | ForegroundHandoff::Unconfirmed => None,
        }
    }

    /// Consume evidence already available before the foreground can retire its native request.
    fn read_handoff(&mut self, trace: &TraceStore, session: &str, step: u64) {
        let observed = match &self.handoff {
            ForegroundHandoff::Watching { observer, .. } => match observer.current() {
                Ok(Some(confirmation)) => Some(Ok(confirmation)),
                Ok(None) => None,
                Err(error) => Some(Err(error)),
            },
            ForegroundHandoff::Preparing
            | ForegroundHandoff::Confirmed(_)
            | ForegroundHandoff::Unconfirmed => None,
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
        let ForegroundHandoff::Watching { scope, .. } = &self.handoff else {
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
                self.handoff = ForegroundHandoff::Confirmed(Box::new(confirmation));
            }
            Ok(_) => {
                trace.record(session, "exchange.dispatch.ignored", serde_json::json!({
                    "work_id":self.id,"scope":scope,"update_step":step,"reason":"scope_mismatch",
                }));
                self.handoff = ForegroundHandoff::Unconfirmed;
            }
            Err(_) => {
                trace.record(
                    session,
                    "exchange.dispatch.unconfirmed",
                    serde_json::json!({
                        "work_id":self.id,"scope":scope,"update_step":step,
                    }),
                );
                self.handoff = ForegroundHandoff::Unconfirmed;
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
        if matches!(&self.transaction, ForegroundDispatch::Running { transaction, .. }
            | ForegroundDispatch::Finalizing { transaction, .. } if transaction.live_host.is_some())
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
    fn dispatch_effects(&mut self, gate: ForegroundEffectGate) {
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

/// Resolve an observer without treating a dropped waiter as an execution failure.
fn notify(
    completion: ForegroundCompletion,
    id: uuid::Uuid,
    outcome: Result<DispatchResult>,
    trace: &TraceStore,
    session_id: &str,
    update_step: u64,
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
                            "exchange.foreground.terminal_publication_failed",
                            serde_json::json!({
                                "work_id":id,"update_step":update_step,"reason":error.to_string(),
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
        "exchange.foreground.response",
        serde_json::json!({
            "work_id":id,"update_step":update_step,"settled":settled,"response_succeeded":response_succeeded,"observed":observed,
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
