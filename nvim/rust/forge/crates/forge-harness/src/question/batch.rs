//! Validate an indivisible set of user decisions before owner admission.

use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

use super::{QuestionAction, QuestionAnswer, QuestionCommand, QuestionId};

/// Restrict batch entries to decisions; clarification and corrections remain independent.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum DecisionAction {
    Answer { answer: QuestionAnswer },
    Dismiss,
}

/// One answer or optional dismissal captured against the displayed revision.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QuestionDecision {
    pub(crate) question_id: QuestionId,
    pub(crate) revision: u64,
    pub(crate) action: DecisionAction,
}

/// An ordered, nonempty set with one operation identity and no duplicate questions.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "BatchRequest")]
pub(crate) struct QuestionBatch {
    operation_id: String,
    answers: Vec<QuestionDecision>,
}

#[derive(Deserialize)]
struct BatchRequest {
    operation_id: String,
    answers: Vec<QuestionDecision>,
}

impl QuestionBatch {
    /// Get the receipt identity for the complete submission.
    pub(crate) fn operation_id(&self) -> &str {
        &self.operation_id
    }

    /// Get the first target for owner-event tracing.
    pub(crate) fn first_question(&self) -> &QuestionId {
        &self.answers[0].question_id
    }

    /// Expand decisions for the shared mutation engine without committing between them.
    pub(super) fn into_commands(self) -> Vec<QuestionCommand> {
        self.answers
            .into_iter()
            .enumerate()
            .map(|(index, answer)| QuestionCommand {
                operation_id: format!("{}:{index}", self.operation_id),
                question_id: answer.question_id,
                revision: answer.revision,
                action: match answer.action {
                    DecisionAction::Answer { answer } => QuestionAction::Answer { answer },
                    DecisionAction::Dismiss => QuestionAction::Dismiss,
                },
            })
            .collect()
    }
}

impl TryFrom<BatchRequest> for QuestionBatch {
    type Error = anyhow::Error;

    /// Reject incomplete and ambiguous batches before entering the owner queue.
    fn try_from(request: BatchRequest) -> Result<Self> {
        ensure!(
            !request.operation_id.trim().is_empty(),
            "question operation identity is required"
        );
        ensure!(!request.answers.is_empty(), "question batch is empty");
        let mut ids = std::collections::BTreeSet::new();
        for answer in &request.answers {
            ensure!(answer.revision > 0, "question revision is required");
            ensure!(
                ids.insert(answer.question_id.as_str()),
                "duplicate question in batch"
            );
        }
        Ok(Self {
            operation_id: request.operation_id,
            answers: request.answers,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Reject ambiguous targets and nondecision actions before owner admission.
    #[test]
    fn rejects_invalid_batch_identity_and_entries() {
        let answer = serde_json::json!({"question_id":"question", "revision":1,
            "action":{"kind":"answer", "answer":{"kind":"other", "text":"Yes"}}});
        for value in [
            serde_json::json!({"operation_id":"", "answers":[answer]}),
            serde_json::json!({"operation_id":"batch", "answers":[]}),
            serde_json::json!({"operation_id":"batch", "answers":[answer, answer]}),
            serde_json::json!({"operation_id":"batch", "answers":[{"question_id":"question", "revision":0, "action":{"kind":"dismiss"}}]}),
            serde_json::json!({"operation_id":"batch", "answers":[{"question_id":"question", "revision":1, "action":{"kind":"clarify", "text":"Explain"}}]}),
        ] {
            assert!(serde_json::from_value::<QuestionBatch>(value).is_err());
        }
    }
}
