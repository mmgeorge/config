use anyhow::{Result, bail};
use serde::Serialize;

use super::{PlanRecord, PlanState};

/// Represents one legal transition applied to a plan's review lifecycle.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanEvent {
    QuestionAsked,
    FeedbackConsumed,
    PlanSubmitted,
    ChangesRequested,
    Accepted,
    Cancelled,
    RetryRequested,
    TransitionFailed,
}

/// Coordinates deterministic plan lifecycle transitions for the broker.
pub struct PlanStateMachine;

impl PlanStateMachine {
    /// Apply one lifecycle event while enforcing the plan's legal transition graph.
    pub fn apply(plan: &mut PlanRecord, event: PlanEvent, now_ms: i64) -> Result<()> {
        let next_state = match (plan.state, event) {
            (
                PlanState::Generating | PlanState::Revising | PlanState::AwaitingInput,
                PlanEvent::QuestionAsked,
            ) => PlanState::AwaitingInput,
            (PlanState::AwaitingInput, PlanEvent::FeedbackConsumed) => {
                if plan.model_revision > 0 {
                    PlanState::Revising
                } else {
                    PlanState::Generating
                }
            }
            (PlanState::Generating | PlanState::Revising, PlanEvent::PlanSubmitted) => {
                PlanState::AwaitingReview
            }
            (PlanState::AwaitingReview, PlanEvent::ChangesRequested) => PlanState::Revising,
            (PlanState::AwaitingReview, PlanEvent::Accepted) => PlanState::Accepted,
            (PlanState::Failed, PlanEvent::RetryRequested) => {
                if plan.model_revision > 0 {
                    PlanState::Revising
                } else {
                    PlanState::Generating
                }
            }
            (
                PlanState::Generating
                | PlanState::AwaitingInput
                | PlanState::AwaitingReview
                | PlanState::Revising,
                PlanEvent::Cancelled,
            ) => PlanState::Cancelled,
            (PlanState::Failed, PlanEvent::Cancelled) => PlanState::Cancelled,
            (
                PlanState::Generating
                | PlanState::AwaitingInput
                | PlanState::AwaitingReview
                | PlanState::Revising,
                PlanEvent::TransitionFailed,
            ) => PlanState::Failed,
            (state, event) => {
                bail!("invalid plan transition: {state:?} + {event:?}");
            }
        };
        plan.state = next_state;
        if matches!(
            event,
            PlanEvent::FeedbackConsumed
                | PlanEvent::PlanSubmitted
                | PlanEvent::Accepted
                | PlanEvent::Cancelled
                | PlanEvent::TransitionFailed
        ) {
            plan.elicitation = None;
        }
        plan.updated_at_ms = now_ms;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::{PlanEvent, PlanStateMachine};
    use crate::plan::{PlanRecord, PlanState};

    fn plan(state: PlanState) -> PlanRecord {
        PlanRecord {
            id: "plan".into(),
            session_id: "session".into(),
            request: "inspect".into(),
            title: "Inspect".into(),
            state,
            working_path: String::new(),
            document_version: 1,
            model_revision: 0,
            submitted_version: None,
            accepted_revision: None,
            user_revision: 0,
            review_digest: None,
            accepted_digest: None,
            elicitation: None,
            acceptance: None,
            question_ledger: Default::default(),
            generation: Default::default(),
            created_at_ms: 1,
            updated_at_ms: 1,
        }
    }

    #[test]
    fn submission_cannot_return_to_input_without_a_new_generation_phase() {
        let mut record = plan(PlanState::Generating);
        PlanStateMachine::apply(&mut record, PlanEvent::PlanSubmitted, 2).unwrap();
        assert_eq!(record.state, PlanState::AwaitingReview);
        assert!(PlanStateMachine::apply(&mut record, PlanEvent::QuestionAsked, 3).is_err());
    }

    #[test]
    fn feedback_consumption_returns_to_exactly_one_generation_phase() {
        let mut record = plan(PlanState::AwaitingInput);
        PlanStateMachine::apply(&mut record, PlanEvent::FeedbackConsumed, 2).unwrap();
        assert_eq!(record.state, PlanState::Generating);
        assert!(record.elicitation.is_none());
    }

    #[test]
    fn failed_generation_allows_only_retry_or_cancel_recovery() {
        let mut retry = plan(PlanState::Failed);
        PlanStateMachine::apply(&mut retry, PlanEvent::RetryRequested, 2).unwrap();
        assert_eq!(retry.state, PlanState::Generating);

        let mut cancel = plan(PlanState::Failed);
        PlanStateMachine::apply(&mut cancel, PlanEvent::Cancelled, 2).unwrap();
        assert_eq!(cancel.state, PlanState::Cancelled);

        let mut invalid = plan(PlanState::Failed);
        assert!(PlanStateMachine::apply(&mut invalid, PlanEvent::QuestionAsked, 2).is_err());
    }
}
