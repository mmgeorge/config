use serde::{Deserialize, Serialize};

use crate::session::continuation::ContinuationBudget;

/// Tracks continuation limits and progress evidence for one goal.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GoalRecord {
    pub id: String,
    pub session_id: String,
    pub objective: String,
    pub state: GoalState,
    #[serde(flatten)]
    pub continuation: ContinuationBudget,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}

pub use forge_provider_api::completion::TurnEvidence;

/// Track the Harness-owned lifecycle of one durable goal.
///
/// Harness applies total-turn and no-progress limits through [`ContinuationBudget`].
/// Provider execution failures pause the task through the ordinary failure path;
/// adapters do not publish native goal states or token-budget transitions.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum GoalState {
    /// Continue while progress and turn limits permit.
    Active,
    /// Await an explicit user resume request.
    Paused,
    /// Retain a goal whose completion gates passed.
    Complete,
    /// Await resolution of a reported constraint.
    Blocked,
    /// Await intervention after progress or continuation limits stop work.
    Stalled,
    /// Retain history after the user clears the goal.
    Cleared,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum GoalTermination {
    Complete,
    Blocked,
}

#[derive(Clone, Debug)]
pub(crate) struct GoalControl {
    pub permit: crate::task::TaskPermit,
    pub termination: GoalTermination,
    pub message: String,
}

/// Represents the broker action selected after a goal turn ends.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ContinuationDecision {
    Continue,
    RetryNoProgress,
    Complete,
    Blocked,
    Stalled,
    Stop,
}

impl GoalRecord {
    /// Resolve the next goal action from turn evidence and continuation guards.
    pub fn observe(&mut self, evidence: TurnEvidence, now_ms: i64) -> ContinuationDecision {
        if self.state == GoalState::Active {
            self.updated_at_ms = now_ms;
        }
        observe(&mut self.state, &mut self.continuation, evidence)
    }

    /// Resume goal execution with a fresh continuation budget.
    pub fn resume(&mut self, now_ms: i64) {
        self.state = GoalState::Active;
        self.continuation.reset();
        self.updated_at_ms = now_ms;
    }
}

pub(crate) fn observe(
    state: &mut GoalState,
    continuation: &mut ContinuationBudget,
    evidence: TurnEvidence,
) -> ContinuationDecision {
    if *state != GoalState::Active {
        return ContinuationDecision::Stop;
    }
    if evidence.structured_complete {
        *state = GoalState::Complete;
        return ContinuationDecision::Complete;
    }
    if evidence.structured_blocked {
        *state = GoalState::Blocked;
        return ContinuationDecision::Blocked;
    }
    let progress = evidence.workspace_changed;
    let may_continue = continuation.observe(progress);
    if !may_continue {
        *state = GoalState::Stalled;
        return ContinuationDecision::Stalled;
    }
    if progress {
        return ContinuationDecision::Continue;
    }
    ContinuationDecision::RetryNoProgress
}
