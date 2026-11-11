//! Bind user clarification input to its explicitly selected question source.

use super::{command::InputSubmissionTarget, services::OwnerServices};
use crate::input::{InputAction, InputOperation, InputPurpose};

/// Validate a persisted clarification source without selecting another current question.
pub(crate) fn is_current(
    root: &std::path::Path,
    session: &str,
    target: &InputSubmissionTarget,
    allow_started: bool,
) -> anyhow::Result<bool> {
    let store = crate::storage::HistoryReader::current(root)?;
    let valid =
        |questions: &crate::plan::PlanElicitation, set: &str, revision: u32, question: &str| {
            questions.question_set.id == set
                && questions.question(question).is_some()
                && (questions.revision == revision
                    || allow_started
                        && revision.checked_add(1) == Some(questions.revision)
                        && questions.clarification_active
                        && questions
                            .current_question()
                            .is_some_and(|current| current.id == question))
        };
    Ok(match target {
        InputSubmissionTarget::Interaction {
            id,
            set_id,
            revision,
            question_id,
        } => {
            let Some(exchange) = store.load_exchange(id.as_str())? else {
                return Ok(false);
            };
            exchange.session_id == session
                && (store
                    .latest_exchange(session)?
                    .is_some_and(|latest| latest.id == exchange.id)
                    || allow_started
                        && exchange.elicitation().is_some_and(|questions| {
                            questions.clarification_active
                                && revision.get().checked_add(1) == Some(questions.revision)
                        }))
                && exchange.elicitation().is_some_and(|questions| {
                    valid(
                        questions,
                        set_id.as_str(),
                        revision.get(),
                        question_id.as_str(),
                    )
                })
        }
        InputSubmissionTarget::Plan {
            id,
            set_id,
            revision,
            question_id,
        } => {
            let Some(plan) = store.load_plan(id.as_str())? else {
                return Ok(false);
            };
            plan.session_id == session
                && plan.state == crate::plan::PlanState::AwaitingInput
                && store.load_active_task(session)?.is_some_and(|task| {
                    matches!(&task.workflow,
                    crate::task::WorkflowSnapshot::Plan { plan } if plan.artifact.id == id.as_str())
                })
                && plan.elicitation.as_ref().is_some_and(|questions| {
                    valid(
                        questions,
                        set_id.as_str(),
                        revision.get(),
                        question_id.as_str(),
                    )
                })
        }
        InputSubmissionTarget::PlanReview {
            id,
            set_id,
            revision,
            question_id,
        } => {
            let Some(plan) = store.load_plan(id.as_str())? else {
                return Ok(false);
            };
            plan.session_id == session
                && plan.state == crate::plan::PlanState::AwaitingReview
                && plan.review.as_ref().is_some_and(|review| {
                    plan.review_digest.as_deref() == Some(review.review_digest.as_str())
                        && valid(
                            &review.elicitation,
                            set_id.as_str(),
                            revision.get(),
                            question_id.as_str(),
                        )
                })
        }
        InputSubmissionTarget::Conversation | InputSubmissionTarget::Question { .. } => false,
    })
}

/// Prepare the owner-controlled input purpose and any independent question outbox record.
pub(super) fn prepare(
    services: &OwnerServices,
    operation: &mut InputOperation,
    target: &InputSubmissionTarget,
) -> Result<Option<String>, super::input_command::InputCommandFailure> {
    use super::{command::CommandFailure, input_command::InputCommandFailure};
    let reject = || InputCommandFailure::Rejected(CommandFailure::ExchangeChanged);
    let InputAction::Submit { message } = &mut operation.action else {
        return Err(reject());
    };
    match target {
        InputSubmissionTarget::Conversation => Ok(None),
        InputSubmissionTarget::Question {
            question_id,
            revision,
        } => {
            services
                .questions
                .apply(crate::question::QuestionCommand {
                    operation_id: message.id.clone(),
                    question_id: question_id.clone(),
                    revision: revision.get(),
                    action: crate::question::QuestionAction::Clarify {
                        text: message.text.clone(),
                    },
                })
                .map_err(|_| reject())?;
            let outbox = services
                .questions
                .outbox()
                .map_err(InputCommandFailure::Domain)?
                .into_iter()
                .find(|outbox| outbox.id == message.id)
                .ok_or_else(reject)?;
            message.purpose = InputPurpose::Question {
                content_revision: Some(revision.get()),
                question_id: outbox.question_id,
                task_id: outbox.task_id,
                message_kind: outbox.kind,
                reply: outbox.reply,
            };
            Ok(Some(message.id.clone()))
        }
        InputSubmissionTarget::Plan { .. }
        | InputSubmissionTarget::Interaction { .. }
        | InputSubmissionTarget::PlanReview { .. } => {
            if !is_current(
                &services.root,
                services.observer.session_id(),
                target,
                false,
            )
            .map_err(InputCommandFailure::Domain)?
            {
                return Err(reject());
            }
            message.purpose = InputPurpose::Clarification {
                target: target.clone(),
            };
            Ok(None)
        }
    }
}
