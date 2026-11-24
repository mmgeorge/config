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
    /// Allow immediate input to pass drafts waiting for the active turn or checks.
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
        let held_for_checks = checks_running
            && !message.is_direct_prompt()
            && !matches!(message.purpose, InputPurpose::CheckResult { .. });
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
    let PendingInput::Ready(selection) = select_pending_input(
        input,
        questions,
        if gates.checks_running {
            InputQueuePolicy::AllowSteering
        } else {
            InputQueuePolicy::Ordered
        },
    )?
    else {
        return Ok(false);
    };
    let (message, command) = selection.into_parts();
    Ok(command.is_none() && matches!(message.purpose, InputPurpose::User) && gates.admit(&message))
}

/// Name the automatic task work that follows a settled exchange without user action.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum TaskSuccessor {
    /// A task entry or continuation turn for the provider.
    Turn,
    /// The accepted plan's verification checks.
    Checks,
}

/// Report the task work the owner will start next from committed task authority.
///
/// A task reserves its next turn in task state when the finished turn's evidence is applied; the
/// queued input for it appears only after the finished input settles, and Execute checks have no
/// queued input at all. Between those steps the session would otherwise read idle. This applies
/// the facts the producers apply before they start that work: the task runs, its input is still
/// pending, the exchange it continues completed (`source_complete`), no question, answer, queued
/// user message, suspension, or approval holds it back, and a check continuation starts only when
/// no check run is collecting.
pub(crate) fn task_successor_pending(
    task: Option<&crate::task::Task>,
    input: &InputSnapshot,
    questions: &QuestionController,
    gates: TurnOfferGates,
    source_complete: impl FnOnce(&crate::task::TaskTurnId) -> anyhow::Result<bool>,
) -> Result<Option<TaskSuccessor>, InputOfferError> {
    use crate::task::{TaskInputDelivery, TaskNextAction, TaskStatus};
    let Some(task) = task.filter(|task| task.status() == TaskStatus::Running) else {
        return Ok(None);
    };
    let user_waiting = input.messages.iter().any(|message| {
        matches!(message.purpose, InputPurpose::User)
            && !matches!(
                message.delivery(),
                InputDelivery::Accepted | InputDelivery::Delivering
            )
    });
    if input.suspended
        || user_waiting
        || gates.has_approval
        || questions_block_task_work(input, questions)?
    {
        return Ok(None);
    }
    if task
        .entry
        .as_ref()
        .is_some_and(|entry| entry.delivery == TaskInputDelivery::Pending)
    {
        return Ok(
            (task.next_action() == TaskNextAction::Model && !gates.checks_running)
                .then_some(TaskSuccessor::Turn),
        );
    }
    let Some((turn, continuation)) = task
        .turn
        .as_ref()
        .and_then(|turn| turn.continuation.as_ref().map(|work| (turn, work)))
        .filter(|(_, work)| work.delivery == TaskInputDelivery::Pending)
    else {
        return Ok(None);
    };
    let successor = match (continuation.work.is_checks(), task.next_action()) {
        (false, TaskNextAction::Model) => TaskSuccessor::Turn,
        (true, TaskNextAction::Checks) => TaskSuccessor::Checks,
        _ => return Ok(None),
    };
    if gates.checks_running || !source_complete(&turn.id).map_err(InputOfferError::Queue)? {
        return Ok(None);
    }
    Ok(Some(successor))
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
        assert!(pending(
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

    /// Predict reserved task work only while the owner would start it without the user.
    ///
    /// An Execute task that finished Implement reserves its verification checks with no queued
    /// input, so the status read idle between the turn and the check run. The prediction must
    /// also clear for every fact that holds the work back, or the status would never settle.
    #[test]
    fn task_successor_is_pending_only_for_work_the_owner_would_start() -> anyhow::Result<()> {
        use crate::plan::{PlanExecutionRecord, PlanPhase};
        use crate::task::{
            Task, TaskContinuation, TaskInputDelivery, TaskInputId, TaskTurn, TaskTurnId, TaskWork,
        };
        let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
        let questions = QuestionController::open(root.path(), "session")?;
        let mut record: PlanExecutionRecord = serde_json::from_value(serde_json::json!({
            "id":"execution", "session_id":"session", "plan_id":"accepted-plan",
            "state":"active", "phase":"checking", "original_revision":1,"revision":1,
            "generation":0, "lifecycle":[],"findings":[],
            "progress":crate::plan::execution::SemanticProgress::default(),
            "verification":[],"revision_history":[],"resume_after_review":false,
            "observed_source":{},"observed_document_version":0,"observed_check":[],
            "created_at_ms":1
        }))?;
        record.baseline_checkpoint = Some("captured-before-provider".into());
        assert_eq!(record.phase, PlanPhase::Checking);
        let mut task = Task::execute("Check the plan".into(), record, Default::default(), 1)?;
        let completed = TaskTurnId {
            exchange_ordinal: 1,
            turn_ordinal: 1,
        };
        task.edit(|snapshot| {
            if let Some(entry) = snapshot.entry.as_mut() {
                entry.delivery = TaskInputDelivery::Accepted;
            }
            snapshot.turn = Some(TaskTurn {
                id: completed.clone(),
                continuation: Some(TaskContinuation {
                    id: TaskInputId::fresh(),
                    reason: "Run the accepted plan's verification checks".into(),
                    display: None,
                    delivery: TaskInputDelivery::Pending,
                    work: TaskWork::Checks,
                }),
            });
            Ok(())
        })?;
        let idle = InputSnapshot {
            revision: 1,
            messages: vec![],
            suspended: false,
            control_revision: 0,
            control_operation_id: None,
        };
        let open = TurnOfferGates {
            checks_running: false,
            has_approval: false,
            has_elicitation: false,
        };
        let successor = |task: &Task, input: &InputSnapshot, gates: TurnOfferGates, complete| {
            task_successor_pending(Some(task), input, &questions, gates, |turn| {
                assert_eq!(turn, &completed);
                Ok(complete)
            })
        };
        assert_eq!(
            successor(&task, &idle, open, true)?,
            Some(TaskSuccessor::Checks)
        );
        // The finished exchange has not completed, so the continuation cannot start yet.
        assert_eq!(successor(&task, &idle, open, false)?, None);
        // A collecting run or a pending approval holds the checks back.
        let running = TurnOfferGates {
            checks_running: true,
            ..open
        };
        assert_eq!(successor(&task, &idle, running, true)?, None);
        let approval = TurnOfferGates {
            has_approval: true,
            ..open
        };
        assert_eq!(successor(&task, &idle, approval, true)?, None);
        let suspended = InputSnapshot {
            suspended: true,
            ..idle.clone()
        };
        assert_eq!(successor(&task, &suspended, open, true)?, None);
        // A queued user message runs first, so the checks are not what starts next.
        let user_waiting = InputSnapshot {
            messages: vec![InputMessage::queued(crate::input::InputDraft {
                id: "follow-up".into(),
                text: "Continue".into(),
                purpose: InputPurpose::User,
                intent: InputIntent::Queue,
                attachments: vec![],
                config: None,
            })],
            ..idle.clone()
        };
        assert_eq!(successor(&task, &user_waiting, open, true)?, None);
        // Accepted work already started; nothing remains to predict.
        task.edit(|snapshot| {
            if let Some(continuation) = snapshot
                .turn
                .as_mut()
                .and_then(|turn| turn.continuation.as_mut())
            {
                continuation.delivery = TaskInputDelivery::Accepted;
            }
            Ok(())
        })?;
        assert_eq!(successor(&task, &idle, open, true)?, None);
        assert_eq!(
            task_successor_pending(None, &idle, &questions, open, |_| Ok(true))?,
            None
        );
        Ok(())
    }
}
