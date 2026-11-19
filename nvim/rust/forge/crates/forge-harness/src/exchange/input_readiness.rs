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

/// Hold the session facts that keep a queued message from starting a turn of its own.
///
/// The owner applies these gates when it is offered a message, and the status projection applies
/// them when it predicts that offer, so the two cannot disagree about which message starts.
#[derive(Clone, Copy)]
pub(crate) struct TurnOfferGates {
    pub checks_running: bool,
    pub has_approval: bool,
    pub has_elicitation: bool,
}

impl TurnOfferGates {
    /// Get whether `message` may start its own turn while these facts hold.
    pub(crate) fn admit(self, message: &InputMessage) -> bool {
        let Self {
            checks_running,
            has_approval,
            has_elicitation,
        } = self;
        let held_for_checks =
            checks_running && !matches!(message.purpose, InputPurpose::CheckResult { .. });
        let held_for_answer = message.intent == InputIntent::Queue && has_elicitation;
        !(held_for_checks || has_approval || held_for_answer)
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
        .position(|message| {
            matches!(
                message.purpose,
                InputPurpose::User
                    | InputPurpose::Clarification { .. }
                    | InputPurpose::Question { .. }
            )
        })
        .or_else(|| {
            pending.iter().position(|message| {
                !matches!(message.purpose, InputPurpose::TaskContinuation { .. })
            })
        })
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
            InputPurpose::TaskEntry { .. }
                | InputPurpose::TaskContinuation { .. }
                | InputPurpose::CheckResult { .. }
        )
    {
        let Some(position) = pending.iter().position(|message| {
            !matches!(
                message.purpose,
                InputPurpose::TaskEntry { .. }
                    | InputPurpose::TaskContinuation { .. }
                    | InputPurpose::CheckResult { .. }
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

/// Report whether a user turn is about to start with no further user action.
///
/// A finished exchange leaves the session idle until the owner claims the next queued message and
/// admits its exchange. This reports that interval: a user message already claimed for its own
/// turn, or the message the scheduler would offer next when `gates` admit it. A directive that
/// runs as a local command is excluded, because it may start no turn.
pub(crate) fn successor_turn_pending(
    input: InputSnapshot,
    questions: &QuestionController,
    gates: TurnOfferGates,
) -> Result<bool, InputOfferError> {
    let claimed = input.messages.iter().any(|message| {
        matches!(message.purpose, InputPurpose::User)
            && message.delivery() == InputDelivery::Delivering
            && message.route() == Some(crate::input::InputRoute::Turn)
    });
    if claimed {
        return Ok(true);
    }
    let PendingInput::Ready(selection) =
        select_pending_input(input, questions, InputQueuePolicy::Ordered)?
    else {
        return Ok(false);
    };
    let (message, command) = selection.into_parts();
    Ok(command.is_none() && matches!(message.purpose, InputPurpose::User) && gates.admit(&message))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Predict a successor turn only for a message the owner would start without the user.
    ///
    /// A prediction that outlives its turn leaves the status row reading `Working` on a session
    /// that will never start, so every fact that holds a message back must clear it.
    #[test]
    fn successor_turn_is_pending_only_for_a_message_the_owner_would_start() -> anyhow::Result<()> {
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let questions = QuestionController::open(root.path(), "session")?;
        let queued = |intent: InputIntent| {
            InputMessage::queued(crate::input::InputDraft {
                id: "follow-up".into(),
                text: "Continue".into(),
                purpose: InputPurpose::User,
                intent,
                attachments: vec![],
                config: None,
            })
        };
        let snapshot = |intent: InputIntent, suspended: bool| InputSnapshot {
            revision: 1,
            messages: vec![queued(intent)],
            suspended,
            control_revision: 0,
            control_operation_id: None,
        };
        let open = TurnOfferGates {
            checks_running: false,
            has_approval: false,
            has_elicitation: false,
        };
        let pending = |input: InputSnapshot, gates: TurnOfferGates| {
            successor_turn_pending(input, &questions, gates)
        };
        assert!(pending(snapshot(InputIntent::Queue, false), open)?);
        assert!(!pending(snapshot(InputIntent::Queue, true), open)?);
        assert!(!pending(
            snapshot(InputIntent::Auto, false),
            TurnOfferGates {
                checks_running: true,
                ..open
            }
        )?);
        assert!(!pending(
            snapshot(InputIntent::Auto, false),
            TurnOfferGates {
                has_approval: true,
                ..open
            }
        )?);
        assert!(!pending(
            snapshot(InputIntent::Queue, false),
            TurnOfferGates {
                has_elicitation: true,
                ..open
            }
        )?);
        assert!(pending(
            snapshot(InputIntent::Auto, false),
            TurnOfferGates {
                has_elicitation: true,
                ..open
            }
        )?);
        assert!(!pending(
            InputSnapshot {
                messages: vec![],
                ..snapshot(InputIntent::Queue, false)
            },
            open
        )?);
        Ok(())
    }
}
