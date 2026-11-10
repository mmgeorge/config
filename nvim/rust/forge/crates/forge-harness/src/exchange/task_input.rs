//! Produce task input from committed authority without owning native delivery or another command queue.

use super::{
    input_readiness::{InputOfferError, questions_block_task_work},
    services::OwnerServices,
};
use crate::{
    input::{InputDelivery, InputIntent, InputMessage, InputPurpose, InputUpdate},
    task::{TaskInputDelivery, TaskInputId, TaskNextAction, TaskPermit, TaskStatus},
};

/// Describe committed scheduling progress without conflating local work with native dispatch.
pub(super) enum TaskInputProduction {
    /// No eligible model input exists at this committed task boundary.
    Idle,
    /// The input journal retired stale task input or created or refreshed this task's input
    /// identity. Updates are ordered by the journal revisions they committed.
    Updated(Vec<InputUpdate>),
    /// User input superseded the identified pending continuation.
    ContinuationCancelled {
        task: crate::task::TaskId,
        input: TaskInputId,
    },
    /// Task authority changed before a scoped local mutation could commit.
    Superseded,
}

/// Reconcile one current entry/continuation while task replacement remains excluded during queue commit.
pub(super) fn reconcile(services: &OwnerServices) -> Result<TaskInputProduction, InputOfferError> {
    let mut updates = Vec::new();
    // Capture dependent input changes under the task mutation fence; never recursively mutate the task there.
    let prepared = services
        .tasks
        .with_task_snapshot(|task| {
            // Readiness serves pending input in order and waits on input whose authority is not
            // current, so a permanently stale task input at the head would block all later input.
            let retired: Vec<_> = services
                .input
                .snapshot()?
                .messages
                .iter()
                .filter(|message| crate::input::task_input_retirable(message))
                .filter(|message| task_input_retired(task.as_ref(), &message.purpose))
                .map(|message| (message.id.clone(), message.revision))
                .collect();
            if !retired.is_empty()
                && let Some(update) = services.input.retire_task_input(&retired)?
            {
                updates.push(update);
            }
            let Some(task) = task else {
                return Ok(PreparedProduction::Idle);
            };
            if !matches!(task.status(), TaskStatus::Running | TaskStatus::Waiting) {
                return Ok(PreparedProduction::Idle);
            }
            let snapshot = services.input.snapshot()?;
            if questions_block_task_work(&snapshot, &services.questions)?
                || task.next_action() != TaskNextAction::Model
            {
                return Ok(PreparedProduction::Idle);
            }
            let (id, text, purpose) = if let Some(entry) = task
                .entry
                .as_ref()
                .filter(|entry| entry.delivery == TaskInputDelivery::Pending)
            {
                (
                    &entry.id,
                    &entry.instruction,
                    InputPurpose::TaskEntry {
                        revision: task.revision.clone(),
                        id: entry.id.clone(),
                        start: (!entry.resumed).then(|| crate::exchange::TaskStart {
                            kind: task.kind(),
                            title: task.title().into(),
                        }),
                    },
                )
            } else if let Some((turn, continuation)) = task
                .turn
                .as_ref()
                .and_then(|turn| turn.continuation.as_ref().map(|input| (turn, input)))
                .filter(|(_, input)| input.delivery == TaskInputDelivery::Pending)
            {
                if continuation.work.is_checks()
                    || !crate::storage::SqliteStore::read_only(&services.root)?
                        .continuation_source_complete(services.observer.session_id(), &turn.id)?
                {
                    return Ok(PreparedProduction::Idle);
                }
                if snapshot.messages.iter().any(|message| {
                    matches!(message.purpose, InputPurpose::User)
                        && !matches!(
                            message.delivery(),
                            InputDelivery::Accepted | InputDelivery::Delivering
                        )
                }) {
                    return Ok(PreparedProduction::Cancel {
                        permit: task.permit(),
                        input: continuation.id.clone(),
                    });
                }
                let text = continuation
                    .work
                    .instruction()
                    .ok_or(InputOfferError::StateUnavailable)?;
                (
                    &continuation.id,
                    text,
                    InputPurpose::TaskContinuation {
                        revision: task.revision.clone(),
                        id: continuation.id.clone(),
                        completed_turn_id: turn.id.clone(),
                        reason: continuation.reason.clone(),
                    },
                )
            } else {
                return Ok(PreparedProduction::Idle);
            };
            // The typed input is what the provider receives; the text is its display label.
            let changed = services.input.reconcile_task_input(
                InputMessage::queued(crate::input::InputDraft {
                    id: id.as_str().into(),
                    text: text.label(),
                    purpose,
                    attachments: Vec::new(),
                    config: None,
                    intent: InputIntent::Auto,
                })
                .with_turn(text.clone()),
            )?;
            Ok(match changed {
                Some(update) => PreparedProduction::Updated(update),
                None => PreparedProduction::Idle,
            })
        })
        .map_err(|error| match error.downcast::<InputOfferError>() {
            Ok(error) => error,
            Err(error) => InputOfferError::Queue(error),
        })?;

    // Apply a cancellation through its own fenced transaction after releasing the snapshot guard.
    match prepared {
        PreparedProduction::Idle if updates.is_empty() => Ok(TaskInputProduction::Idle),
        PreparedProduction::Idle => Ok(TaskInputProduction::Updated(updates)),
        PreparedProduction::Updated(update) => {
            updates.push(update);
            Ok(TaskInputProduction::Updated(updates))
        }
        // Publish committed retirements before a separately fenced cancellation; readiness retries.
        PreparedProduction::Cancel { .. } if !updates.is_empty() => {
            Ok(TaskInputProduction::Updated(updates))
        }
        PreparedProduction::Cancel { permit, input } => {
            let mut store = crate::storage::SqliteStore::open(&services.root)
                .map_err(InputOfferError::Queue)?;
            let elapsed = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map_err(|_| InputOfferError::StateUnavailable)?;
            let now_ms = i64::try_from(elapsed.as_millis())
                .map_err(|_| InputOfferError::StateUnavailable)?;
            match services
                .tasks
                .cancel_pending_continuation(&mut store, &permit, &input, now_ms)
            {
                Ok(true) => Ok(TaskInputProduction::ContinuationCancelled {
                    task: permit.task_id().clone(),
                    input,
                }),
                Ok(false)
                | Err(
                    super::command::TaskCommandFailure::Changed
                    | super::command::TaskCommandFailure::MissingTask,
                ) => Ok(TaskInputProduction::Superseded),
                Err(error) => Err(InputOfferError::Queue(error.into())),
            }
        }
    }
}

/// Report whether queued task input can never regain authority under committed task state.
///
/// Task revisions are fresh identities that never repeat, and a completed turn or cancelled
/// continuation is never reopened. Status is deliberately ignored: a paused task keeps its pending
/// input, which becomes current again when the same revision resumes.
fn task_input_retired(task: Option<&crate::task::Task>, purpose: &InputPurpose) -> bool {
    let (revision, input_id) = match purpose {
        InputPurpose::TaskEntry { revision, id, .. }
        | InputPurpose::TaskContinuation { revision, id, .. } => (revision, id),
        InputPurpose::Clarification { .. }
        | InputPurpose::User
        | InputPurpose::Question { .. }
        | InputPurpose::QuestionContinuation { .. }
        | InputPurpose::WorkflowContinuation { .. } => return false,
    };
    let Some(task) = task.filter(|task| &task.revision == revision) else {
        return true;
    };
    match purpose {
        InputPurpose::TaskEntry { .. } => task.entry.as_ref().is_none_or(|entry| {
            &entry.id != input_id || entry.delivery == TaskInputDelivery::Cancelled
        }),
        InputPurpose::TaskContinuation {
            completed_turn_id, ..
        } => task.turn.as_ref().is_none_or(|turn| {
            &turn.id != completed_turn_id
                || turn.continuation.as_ref().is_none_or(|continuation| {
                    &continuation.id != input_id
                        || continuation.delivery == TaskInputDelivery::Cancelled
                })
        }),
        InputPurpose::Clarification { .. }
        | InputPurpose::User
        | InputPurpose::Question { .. }
        | InputPurpose::QuestionContinuation { .. }
        | InputPurpose::WorkflowContinuation { .. } => false,
    }
}

enum PreparedProduction {
    Idle,
    Updated(InputUpdate),
    Cancel {
        permit: TaskPermit,
        input: TaskInputId,
    },
}
