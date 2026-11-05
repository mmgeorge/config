//! Select queued input from committed question and delivery facts without acquiring transport resources.

use crate::{
    input::{InputDelivery, InputIntent, InputMessage, InputPurpose, InputSnapshot},
    question::QuestionController,
    task::command::SessionCommand,
};

/// Describe delivery progress without giving readiness observers execution capabilities.
#[derive(Clone, Copy)]
pub(crate) enum InputReadiness {
    /// The owner claimed and retained a delivery pipeline.
    Started,
    /// An occupied execution-admission permit can wake the scheduler when released.
    /// A free permit alone never proves that a retained native invocation is idle.
    WaitExecution,
    /// Another command, gate, ordering permit, or revision prevents admission.
    Waiting,
}

/// Distinguish declined readiness from failed durable admission or lost intake observation.
#[derive(Debug, thiserror::Error)]
pub(crate) enum InputOfferError {
    /// An internal completion attempted to settle input that is not its local directive.
    #[error("local input completion does not match a queued task directive")]
    InvalidLocalResult,
    /// Failed owner storage certification blocks any further input mutation.
    #[error("input state cannot be certified after owner storage failure")]
    StateUnavailable,
    #[error("exchange owner closed before input readiness intake")]
    OwnerClosed,
    #[error("input readiness result was not observed")]
    ObservationLost,
    #[error("input admission could not read or commit queue state")]
    Queue(#[source] anyhow::Error),
    #[error("input workflow authority is unavailable")]
    Authority(#[source] crate::input::InputAuthorityError),
}

impl InputOfferError {
    /// Get a bounded readiness failure code without exposing receipt or native protocol data.
    pub(super) fn code(&self) -> &'static str {
        match self {
            Self::InvalidLocalResult => "input_local_result_mismatch",
            Self::StateUnavailable => "input_state_unavailable",
            Self::OwnerClosed => "input_owner_closed",
            Self::ObservationLost => "input_offer_observation_lost",
            Self::Queue(_) => "input_queue_unavailable",
            Self::Authority(_) => "input_authority_unavailable",
        }
    }
}

/// Determine whether immediate input may bypass a draft waiting for a new turn.
pub(crate) enum InputQueuePolicy {
    /// Preserve order when no active receiver can accept immediate steering.
    Ordered,
    /// Allow immediate input to bypass queued drafts while a steering destination is active.
    AllowSteering,
}

/// Describe the next input while preserving blocked drafts and legacy task directives.
pub(crate) enum PendingInput {
    /// No unfinished queued input remains; task/check completion may be considered separately.
    Empty,
    /// Delivery evidence, reservations, or required questions block this queue.
    Waiting,
    /// Queued drafts must wait for execution to become available.
    WaitExecution,
    /// The observed queued revision is eligible for owner revalidation, not yet claimed.
    Ready(PendingInputSelection),
}

/// Keep an observed queued input paired with the directive parsed from that exact revision.
pub(crate) struct PendingInputSelection {
    message: Box<InputMessage>,
    command: Option<Box<SessionCommand>>,
}

impl PendingInputSelection {
    /// Consume the selected observation and its optional legacy directive without reparsing another revision.
    pub(crate) fn into_parts(self) -> (InputMessage, Option<SessionCommand>) {
        (*self.message, self.command.map(|command| *command))
    }
}

/// Keep automatic task work behind required questions and committed answers still awaiting delivery.
pub(crate) fn questions_block_task_work(
    input: &InputSnapshot,
    questions: &QuestionController,
) -> Result<bool, InputOfferError> {
    Ok(questions
        .snapshot()
        .map_err(InputOfferError::Queue)?
        .required_pending()
        || !questions
            .outbox()
            .map_err(InputOfferError::Queue)?
            .is_empty()
        || input.messages.iter().any(|message| {
            matches!(
                message.purpose,
                InputPurpose::Question { .. } | InputPurpose::QuestionContinuation { .. }
            ) && message.delivery() != InputDelivery::Accepted
        }))
}

/// Select one candidate using the same priority rules for owner intake and remaining legacy producers.
pub(crate) fn select_pending_input(
    input: InputSnapshot,
    questions: &QuestionController,
    policy: InputQueuePolicy,
) -> Result<PendingInput, InputOfferError> {
    if input
        .messages
        .iter()
        .any(|message| message.delivery() == InputDelivery::Unknown)
        || input.suspended
            && !input.messages.iter().any(|message| {
                matches!(
                    message.purpose,
                    InputPurpose::Question { .. } | InputPurpose::Clarification { .. }
                )
            })
    {
        return Ok(PendingInput::Waiting);
    }
    let questions_block_work = questions_block_task_work(&input, questions)?;
    let mut pending: Vec<_> = input
        .messages
        .into_iter()
        .filter(|message| {
            !matches!(
                message.delivery(),
                InputDelivery::Delivering | InputDelivery::Accepted
            )
        })
        .collect();
    if pending.is_empty() {
        return Ok(PendingInput::Empty);
    }

    // Preserve question authority and explicit input priority over automatic task continuations.
    let selected = pending
        .iter()
        .position(|message| !matches!(message.purpose, InputPurpose::TaskContinuation { .. }))
        .unwrap_or(0);
    let mut message = pending.remove(selected);
    if matches!(message.purpose, InputPurpose::QuestionContinuation { .. })
        && questions
            .snapshot()
            .map_err(InputOfferError::Queue)?
            .required_pending()
    {
        return Ok(PendingInput::Waiting);
    }
    if input.suspended
        && !matches!(
            message.purpose,
            InputPurpose::Question { .. } | InputPurpose::Clarification { .. }
        )
    {
        let Some(position) = pending.iter().position(|message| {
            matches!(
                message.purpose,
                InputPurpose::Question { .. } | InputPurpose::Clarification { .. }
            )
        }) else {
            return Ok(PendingInput::Waiting);
        };
        message = pending.remove(position);
    }
    if questions_block_work
        && matches!(
            message.purpose,
            InputPurpose::TaskEntry { .. } | InputPurpose::TaskContinuation { .. }
        )
    {
        let Some(position) = pending.iter().position(|message| {
            !matches!(
                message.purpose,
                InputPurpose::TaskEntry { .. } | InputPurpose::TaskContinuation { .. }
            )
        }) else {
            return Ok(PendingInput::Waiting);
        };
        message = pending.remove(position);
    }

    // Allow immediate steering to pass drafts without retrying failed or reserved input.
    let mut command = SessionCommand::queued_input(&message).map_err(InputOfferError::Queue)?;
    if command.is_none()
        && matches!(policy, InputQueuePolicy::AllowSteering)
        && message.intent == InputIntent::Queue
        && message.delivery() == InputDelivery::Queued
    {
        let Some(immediate) = pending.into_iter().find(|message| {
            !matches!(message.purpose, InputPurpose::TaskContinuation { .. })
                && (message.intent != InputIntent::Queue
                    || message.delivery() != InputDelivery::Queued)
        }) else {
            return Ok(PendingInput::WaitExecution);
        };
        message = immediate;
        command = SessionCommand::queued_input(&message).map_err(InputOfferError::Queue)?;
    }
    if message.delivery() != InputDelivery::Queued || message.reservation().is_some() {
        return Ok(PendingInput::Waiting);
    }
    Ok(PendingInput::Ready(PendingInputSelection {
        message: Box::new(message),
        command: command.map(Box::new),
    }))
}
