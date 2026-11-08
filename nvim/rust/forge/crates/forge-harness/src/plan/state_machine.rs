use anyhow::{Result, bail};
use serde::Serialize;

use super::{PlanRecord, PlanState};

/// Represents one legal transition applied to a plan's review lifecycle.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanEvent {
    /// Admits generation after the setup exchange publishes valid project checks.
    ChecksConfigured,
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
            (PlanState::ConfiguringChecks, PlanEvent::ChecksConfigured) => PlanState::Generating,
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
            (
                PlanState::Generating | PlanState::Revising | PlanState::AwaitingReview,
                PlanEvent::PlanSubmitted,
            ) => PlanState::AwaitingReview,
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
                PlanState::ConfiguringChecks
                | PlanState::Generating
                | PlanState::AwaitingInput
                | PlanState::AwaitingReview
                | PlanState::Revising,
                PlanEvent::Cancelled,
            ) => PlanState::Cancelled,
            (PlanState::Failed, PlanEvent::Cancelled) => PlanState::Cancelled,
            (
                PlanState::ConfiguringChecks
                | PlanState::Generating
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
