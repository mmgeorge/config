//! Validate queued input against the exact task or question authority that created it.

use super::InputPurpose;

/// Revalidate input purpose without retargeting a stale input to the latest workflow.
pub(crate) fn input_purpose_is_current(
    tasks: &crate::task::SessionTaskController,
    questions: &crate::question::QuestionController,
    purpose: &InputPurpose,
    root: &std::path::Path,
    session: &str,
) -> Result<bool, InputAuthorityError> {
    let (revision, may_wait) = match purpose {
        InputPurpose::User => return Ok(true),
        InputPurpose::CheckResult { exchange_id, .. } => {
            return crate::storage::HistoryReader::current(root)
                .and_then(|reader| reader.load_exchange(exchange_id))
                .map(|exchange| {
                    exchange.is_some_and(|exchange| {
                        exchange.session_id == session
                            && matches!(
                                exchange.state(),
                                crate::exchange::ExchangeState::Running
                                    | crate::exchange::ExchangeState::Finalizing
                                    | crate::exchange::ExchangeState::Complete
                            )
                            && exchange.disposition()
                                == crate::exchange::HistoryDisposition::Current
                    })
                })
                .map_err(InputAuthorityError::Questions);
        }
        InputPurpose::Clarification { target } => {
            return crate::exchange::input_target::is_current(root, session, target, true)
                .map_err(InputAuthorityError::Questions);
        }
        InputPurpose::WorkflowContinuation { exchange } => {
            return questions
                .workflow_continuation_is_current(exchange)
                .map_err(InputAuthorityError::Questions);
        }
        InputPurpose::QuestionContinuation { exchange_id } => {
            return Ok(tasks
                .current()
                .map_err(InputAuthorityError::Task)?
                .is_none()
                && questions
                    .chat_continuation_is_current(exchange_id)
                    .map_err(InputAuthorityError::Questions)?
                && questions
                    .snapshot()
                    .map_err(InputAuthorityError::Questions)?
                    .questions
                    .iter()
                    .any(|question| {
                        question.exchange_id == *exchange_id
                            && question.requirement
                                == crate::question::QuestionRequirement::Required
                            && !matches!(
                                question.resolution,
                                crate::question::QuestionResolution::Retired { .. }
                            )
                    }));
        }
        InputPurpose::Question {
            question_id,
            content_revision,
            additional_replies,
            ..
        } => {
            if let Some(revision) = content_revision {
                let current = questions
                    .snapshot()
                    .map_err(InputAuthorityError::Questions)?;
                if !current.questions.iter().any(|question| {
                    &question.id == question_id && question.content.revision == *revision
                }) {
                    return Ok(false);
                }
            }
            for reply in additional_replies {
                let id = crate::question::QuestionId::try_from(reply.question_id.clone())
                    .map_err(InputAuthorityError::Questions)?;
                let current = questions
                    .snapshot()
                    .map_err(InputAuthorityError::Questions)?;
                if !current.questions.iter().any(|question| {
                    question.id == id && question.content.revision == reply.revision
                }) || !questions
                    .delivery_is_current(&id)
                    .map_err(InputAuthorityError::Questions)?
                {
                    return Ok(false);
                }
            }
            return questions
                .delivery_is_current(question_id)
                .map_err(InputAuthorityError::Questions);
        }
        InputPurpose::TaskEntry { revision, .. } => (revision, true),
        InputPurpose::TaskContinuation { revision, .. } => (revision, false),
    };
    Ok(tasks
        .current()
        .map_err(InputAuthorityError::Task)?
        .is_some_and(|task| {
            &task.revision == revision
                && match purpose {
                    InputPurpose::TaskContinuation {
                        id,
                        completed_turn_id,
                        ..
                    } => {
                        task.entry_accepted()
                            && task.turn.as_ref().is_some_and(|turn| {
                                &turn.id == completed_turn_id
                                    && turn.continuation.as_ref().is_some_and(|input| {
                                        &input.id == id
                                            && input.delivery
                                                != crate::task::TaskInputDelivery::Cancelled
                                    })
                            })
                    }
                    InputPurpose::TaskEntry { .. } => true,
                    InputPurpose::Clarification { .. }
                    | InputPurpose::User
                    | InputPurpose::CheckResult { .. }
                    | InputPurpose::Question { .. }
                    | InputPurpose::QuestionContinuation { .. }
                    | InputPurpose::WorkflowContinuation { .. } => {
                        unreachable!("non-task input returned before task authority validation")
                    }
                }
                && (task.status() == crate::task::TaskStatus::Running
                    || may_wait && task.status() == crate::task::TaskStatus::Waiting)
        }))
}

/// Distinguish unavailable task authority from unavailable question authority.
#[derive(Debug, thiserror::Error)]
pub(crate) enum InputAuthorityError {
    /// Existing task storage could not supply its committed authority.
    #[error("input task authority is unavailable")]
    Task(#[source] anyhow::Error),
    /// Existing question storage could not supply its committed authority.
    #[error("input question authority is unavailable")]
    Questions(#[source] anyhow::Error),
}
