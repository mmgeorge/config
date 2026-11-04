//! Commit revision-scoped plan decisions at the owner event boundary.

use super::{QuestionEventError, services::OwnerServices};
use crate::plan::{PlanQuestionAnswerCommit, PlanQuestionAnswerError, PlanQuestionAnswerRequest};

/// Commit an answer and its retained notification, then wake independent output delivery.
pub(super) fn commit(
    permit: &super::QuestionUpdatePermit<'_>,
    request: &PlanQuestionAnswerRequest,
    services: &OwnerServices,
) -> Result<PlanQuestionAnswerCommit, QuestionEventError> {
    let mut store = crate::storage::SqliteStore::open(&services.root)
        .map_err(PlanQuestionAnswerError::State)
        .map_err(QuestionEventError::PlanAnswer)?;
    let result =
        services
            .publications
            .ordered_mutation(QuestionEventError::AdmissionClosed, || {
                store
                    .commit_question_answer(permit, request)
                    .map_err(QuestionEventError::PlanAnswer)
            })?;
    services.publications.notify_ready();
    Ok(result)
}
