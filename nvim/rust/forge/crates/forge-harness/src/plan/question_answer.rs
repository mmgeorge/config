//! Validate plan-question decisions before admitting them to the session owner.

use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::broker::{ActiveElicitation, ElicitationOwner};

use super::{PlanElicitation, PlanQuestionResponse};

/// Select the persisted question source without exposing storage paths to callers.
#[derive(Clone, Copy, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum PlanQuestionOwner {
    /// The question set belongs to one exchange's persisted workflow state.
    Interaction,
    /// The question set belongs to the selected plan's planning decisions.
    Plan,
    /// The question set belongs to revision-scoped plan review evidence.
    #[serde(alias = "plan_acceptance")]
    PlanReview,
}

impl PlanQuestionOwner {
    /// Get the presentation owner corresponding to this selected source.
    pub(crate) fn elicitation_owner(self) -> ElicitationOwner {
        match self {
            Self::Interaction => ElicitationOwner::Interaction,
            Self::Plan => ElicitationOwner::Plan,
            Self::PlanReview => ElicitationOwner::PlanReview,
        }
    }
}

/// Distinguish ingress, stale authority, retry conflicts, and persistence failures.
#[derive(Debug, thiserror::Error)]
pub(crate) enum PlanQuestionAnswerError {
    /// The external answer or its response does not match the supported request schema.
    #[error("question answer request is invalid")]
    InvalidRequest(#[source] serde_json::Error),
    /// An operation, source, set, question, writer, or revision cannot identify valid authority.
    #[error("question answer requires nonempty identities and a positive revision")]
    InvalidIdentity,
    /// An immutable operation identity already belongs to a different answer payload.
    #[error("question answer identity was reused for another response")]
    IdentityConflict,
    /// The selected question source or its dependent planning task no longer accepts answers.
    #[error("question owner is no longer awaiting this answer")]
    TargetMissing,
    /// The selected content revision or question set differs from the stored source.
    #[error("question set changed before the answer could commit")]
    Changed,
    /// A saved result cannot be decoded using the historical answer-result schema.
    #[error("stored question answer result is invalid")]
    ReceiptCodec(#[source] serde_json::Error),
    /// A saved result identifies another operation, source, or question set.
    #[error("stored question answer result does not match its request")]
    ReceiptMismatch,
    /// The response is not valid for its identified question or dependent task state.
    #[error("question answer cannot be applied")]
    InvalidAnswer(#[source] anyhow::Error),
    /// Existing persistence/publication infrastructure could not certify the answer state.
    #[error("question answer state could not be read or committed")]
    State(#[source] anyhow::Error),
}

impl PlanQuestionAnswerError {
    /// Get a stable error code without interpreting diagnostic strings.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::InvalidRequest(_) | Self::InvalidIdentity => "question_answer_invalid",
            Self::IdentityConflict => "question_answer_identity_conflict",
            Self::TargetMissing => "question_target_missing",
            Self::Changed => "question_revision_changed",
            Self::ReceiptCodec(_) | Self::ReceiptMismatch => "question_answer_receipt_invalid",
            Self::InvalidAnswer(_) => "question_answer_rejected",
            Self::State(_) => "question_answer_state_unavailable",
        }
    }
}

impl From<rusqlite::Error> for PlanQuestionAnswerError {
    /// Preserve the SQL cause while keeping the public storage failure category stable.
    fn from(error: rusqlite::Error) -> Self {
        Self::State(error.into())
    }
}

impl From<serde_json::Error> for PlanQuestionAnswerError {
    /// Preserve failures encoding or reading the stored answer state.
    fn from(error: serde_json::Error) -> Self {
        Self::State(error.into())
    }
}

/// Distinguish a newly encoded answer from an immutable historical response representation.
enum AnswerReceipt {
    Current,
    Historical(Value),
}

/// Retain one validated answer and the exact old receipt representation for safe retries.
pub(crate) struct PlanQuestionAnswerRequest {
    request: RawPlanQuestionAnswer,
    receipt: Value,
}

impl PlanQuestionAnswerRequest {
    /// Parse immutable target authority once while preserving existing request-receipt identity.
    pub(crate) fn parse(receipt: Value) -> Result<Self, PlanQuestionAnswerError> {
        let request: RawPlanQuestionAnswer = serde_json::from_value(receipt.clone())
            .map_err(PlanQuestionAnswerError::InvalidRequest)?;
        if [
            request.operation_id.as_str(),
            request.target.id.as_str(),
            request.target.set_id.as_str(),
            request.question_id.as_str(),
        ]
        .iter()
        .any(|id| id.trim().is_empty())
            || request.target.revision == 0
            || request
                .answer_order
                .as_ref()
                .is_some_and(|(writer, _)| writer.trim().is_empty())
        {
            return Err(PlanQuestionAnswerError::InvalidIdentity);
        }
        Ok(Self { request, receipt })
    }

    /// Get the immutable operation identity used by answer receipts and owner logging.
    pub(crate) fn operation_id(&self) -> &str {
        &self.request.operation_id
    }

    /// Get the normalized storage source selected at ingress.
    pub(crate) fn owner(&self) -> PlanQuestionOwner {
        self.request.target.owner
    }

    /// Get the selected source identity without allowing queued work to choose a newer source.
    pub(crate) fn source_id(&self) -> &str {
        &self.request.target.id
    }

    /// Get the selected question set identity.
    pub(crate) fn set_id(&self) -> &str {
        &self.request.target.set_id
    }

    /// Get the positive question-content revision observed by the caller.
    pub(crate) fn revision(&self) -> u32 {
        self.request.target.revision
    }

    /// Get the selected question within its immutable set.
    pub(crate) fn question_id(&self) -> &str {
        &self.request.question_id
    }

    /// Get the normalized user response, including explicit skipping.
    pub(crate) fn response(&self) -> &PlanQuestionResponse {
        &self.request.response
    }

    /// Get the optional writer-local answer sequence used to reject late answers.
    pub(crate) fn answer_order(&self) -> Option<&(String, u64)> {
        self.request.answer_order.as_ref()
    }

    /// Encode the original request only at the persistence boundary; never normalize an old retry key.
    pub(crate) fn encode_receipt(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string(&self.receipt)
    }
}

/// Publish the committed answer with the same wire shape as historical receipts.
pub(crate) struct PlanQuestionAnswerCommit {
    operation_id: String,
    active_elicitation: ActiveElicitation,
    receipt: AnswerReceipt,
}

impl PlanQuestionAnswerCommit {
    /// Create a committed result from the validated request and its stored question state.
    pub(crate) fn new(
        request: &PlanQuestionAnswerRequest,
        elicitation: PlanElicitation,
    ) -> Result<Self, PlanQuestionAnswerError> {
        if elicitation.question_set.id != request.set_id()
            || elicitation.revision != request.revision()
        {
            return Err(PlanQuestionAnswerError::Changed);
        }
        let owner = request.owner().elicitation_owner();
        Ok(Self {
            operation_id: request.operation_id().into(),
            receipt: AnswerReceipt::Current,
            active_elicitation: ActiveElicitation {
                owner,
                plan_id: match owner {
                    ElicitationOwner::Interaction => None,
                    ElicitationOwner::Plan | ElicitationOwner::PlanReview => {
                        Some(request.source_id().into())
                    }
                },
                exchange_id: match owner {
                    ElicitationOwner::Interaction => Some(request.source_id().into()),
                    ElicitationOwner::Plan | ElicitationOwner::PlanReview => None,
                },
                elicitation,
            },
        })
    }

    /// Decode and validate an old result before returning its original committed answer snapshot.
    pub(crate) fn recover(
        request: &PlanQuestionAnswerRequest,
        encoded: &str,
    ) -> Result<Self, PlanQuestionAnswerError> {
        let receipt: Value =
            serde_json::from_str(encoded).map_err(PlanQuestionAnswerError::ReceiptCodec)?;
        let stored: StoredAnswerCommit = serde_json::from_value(receipt.clone())
            .map_err(PlanQuestionAnswerError::ReceiptCodec)?;
        let expected = Self::new(request, stored.active_elicitation.elicitation.clone())
            .map_err(|_| PlanQuestionAnswerError::ReceiptMismatch)?;
        if stored.operation_id != expected.operation_id
            || stored.active_elicitation.owner != expected.active_elicitation.owner
            || stored.active_elicitation.plan_id != expected.active_elicitation.plan_id
            || stored.active_elicitation.exchange_id != expected.active_elicitation.exchange_id
        {
            return Err(PlanQuestionAnswerError::ReceiptMismatch);
        }
        Ok(Self {
            operation_id: stored.operation_id,
            active_elicitation: stored.active_elicitation,
            receipt: AnswerReceipt::Historical(receipt),
        })
    }

    /// Encode the immutable notification at its wire boundary, retaining historical owner spellings.
    pub(crate) fn notification_payload(&self) -> Result<Value, serde_json::Error> {
        match &self.receipt {
            AnswerReceipt::Current => serde_json::to_value(&self.active_elicitation),
            AnswerReceipt::Historical(receipt) => Ok(receipt["active_elicitation"].clone()),
        }
    }
}

impl Serialize for PlanQuestionAnswerCommit {
    /// Preserve historical response fields and defaults instead of rewriting an immutable retry result.
    fn serialize<Serializer>(
        &self,
        serializer: Serializer,
    ) -> Result<Serializer::Ok, Serializer::Error>
    where
        Serializer: serde::Serializer,
    {
        match &self.receipt {
            AnswerReceipt::Current => AnswerCommitFields {
                operation_id: &self.operation_id,
                active_elicitation: &self.active_elicitation,
            }
            .serialize(serializer),
            AnswerReceipt::Historical(receipt) => receipt.serialize(serializer),
        }
    }
}

#[derive(Serialize)]
struct AnswerCommitFields<'answer> {
    operation_id: &'answer str,
    active_elicitation: &'answer ActiveElicitation,
}

#[derive(Deserialize)]
struct RawPlanQuestionAnswer {
    operation_id: String,
    target: RawQuestionTarget,
    question_id: String,
    response: PlanQuestionResponse,
    #[serde(default)]
    answer_order: Option<(String, u64)>,
}

#[derive(Deserialize)]
struct RawQuestionTarget {
    owner: PlanQuestionOwner,
    id: String,
    set_id: String,
    revision: u32,
}

#[derive(Deserialize)]
struct StoredAnswerCommit {
    operation_id: String,
    active_elicitation: ActiveElicitation,
}
