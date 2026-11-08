//! Own the exchange owner handle that callers use to submit commands and facts to its driver.

use std::sync::Arc;

use anyhow::{Context, Result};
use forge_protocol::outbound::MessageSender;
use tokio::sync::{mpsc, oneshot};

use super::command::{
    Command, CommandFailure, CommandId, CommandRequest, CommandResult, ExecuteCommandError,
};
use super::driver::*;
use super::input_delivery::{InputDeliveryOffer, SteeringDeliveryOffer};
use super::input_readiness::{InputOfferError, InputReadiness};
#[cfg(test)]
use super::input_receipts::{InputReceiptError, InputReceiptFact};
use super::journal::{CommandJournalError, CommandRecord};
use super::queue::{CommandQueue, CommandReply};
use crate::broker::DispatchResult;

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
    /// Create the permit for one driver update step of this session.
    pub(super) fn for_update_step(session: &'step str) -> Self {
        Self { session }
    }

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
            ExchangeDriver::new(receiver, child_sender, shutdown_retry, commands),
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

/// Carry one caller request or internal fact into the driver's intake.
pub(super) enum OwnerEvent {
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
