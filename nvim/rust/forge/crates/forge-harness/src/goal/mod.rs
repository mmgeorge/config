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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn goal_control_keeps_its_outcome_with_the_admitted_revision() {
        let previous = crate::task::Task::goal("Previous".into(), Default::default(), 1);
        let current = crate::task::Task::goal("Current".into(), Default::default(), 2);
        for termination in [GoalTermination::Complete, GoalTermination::Blocked] {
            let output = crate::provider::ProviderOutput {
                event: Vec::new(),
                completion: crate::provider::ProviderCompletion {
                    native: forge_provider_api::completion::ProviderCompletion {
                        evidence: TurnEvidence {
                            structured_complete: true,
                            structured_blocked: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    goal_control: Some(GoalControl {
                        permit: current.permit(),
                        termination,
                        message: "Observed goal outcome".into(),
                    }),
                    dispatched_task_permit: Some(previous.permit()),
                    ..Default::default()
                },
            };
            let (evidence, permit) = output.completion.goal_evidence();
            assert_eq!(
                evidence.structured_complete,
                termination == GoalTermination::Complete
            );
            assert_eq!(
                evidence.structured_blocked,
                termination == GoalTermination::Blocked
            );
            permit.as_ref().unwrap().validate(Some(&current)).unwrap();
            assert!(permit.unwrap().validate(Some(&previous)).is_err());
            let serialized = serde_json::to_value(&output).unwrap();
            assert!(serialized.get("goal_control").is_none());
            assert!(serialized.get("dispatched_task_permit").is_none());
        }
    }

    fn goal() -> GoalRecord {
        GoalRecord {
            id: "goal".into(),
            session_id: "session".into(),
            objective: "finish".into(),
            state: GoalState::Active,
            continuation: ContinuationBudget::default(),
            created_at_ms: 0,
            updated_at_ms: 0,
        }
    }

    #[test]
    fn stalls_after_five_turns_without_observable_progress() {
        let mut record = goal();
        for turn in 1..5 {
            assert_eq!(
                record.observe(TurnEvidence::default(), turn),
                ContinuationDecision::RetryNoProgress
            );
        }
        assert_eq!(
            record.observe(TurnEvidence::default(), 5),
            ContinuationDecision::Stalled
        );
        assert_eq!(record.state, GoalState::Stalled);
    }

    #[test]
    fn progress_resets_the_no_progress_guard() {
        let mut record = goal();
        record.observe(TurnEvidence::default(), 1);
        assert_eq!(
            record.observe(
                TurnEvidence {
                    workspace_changed: true,
                    ..TurnEvidence::default()
                },
                2
            ),
            ContinuationDecision::Continue
        );
        assert_eq!(record.continuation.consecutive_no_progress, 0);
    }

    #[test]
    fn stops_without_requesting_a_turn_beyond_the_configured_limit() {
        let mut record = goal();
        record.continuation.max_turn_count = 2;
        let progress = TurnEvidence {
            workspace_changed: true,
            ..TurnEvidence::default()
        };
        assert_eq!(record.observe(progress, 1), ContinuationDecision::Continue);
        assert_eq!(record.observe(progress, 2), ContinuationDecision::Stalled);
        assert_eq!(record.continuation.turn_count, 2);
    }

    #[test]
    fn inactive_goals_preserve_state_and_time_until_explicit_resume() {
        for state in [
            GoalState::Paused,
            GoalState::Complete,
            GoalState::Blocked,
            GoalState::Stalled,
            GoalState::Cleared,
        ] {
            let mut record = goal();
            record.state = state;
            record.updated_at_ms = 10;
            for evidence in [
                TurnEvidence::default(),
                TurnEvidence {
                    structured_complete: true,
                    ..TurnEvidence::default()
                },
                TurnEvidence {
                    structured_blocked: true,
                    ..TurnEvidence::default()
                },
            ] {
                assert_eq!(record.observe(evidence, 50), ContinuationDecision::Stop);
                assert_eq!(record.state, state);
                assert_eq!(record.updated_at_ms, 10);
                assert_eq!(record.continuation.turn_count, 0);
            }
            if matches!(
                state,
                GoalState::Paused | GoalState::Blocked | GoalState::Stalled
            ) {
                record.resume(60);
                assert_eq!(
                    record.observe(TurnEvidence::default(), 70),
                    ContinuationDecision::RetryNoProgress
                );
                assert_eq!(record.updated_at_ms, 70);
            }
        }
    }
}
