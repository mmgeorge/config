use serde::{Deserialize, Serialize};

use crate::session::continuation::ContinuationBudget;

/// Represents the lifecycle state of one persistent harness goal.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum GoalState {
    Active,
    Paused,
    Complete,
    Blocked,
    UsageLimited,
    BudgetLimited,
    Stalled,
    Cleared,
}

/// Tracks continuation limits and progress evidence for one goal.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GoalRecord {
    pub id: String,
    pub session_id: String,
    pub objective: String,
    pub state: GoalState,
    #[serde(flatten)]
    pub continuation: ContinuationBudget,
    pub native: bool,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}

/// Represents observable evidence produced by one backend turn.
#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
pub struct TurnEvidence {
    pub workspace_changed: bool,
    pub tool_called: bool,
    #[serde(default)]
    pub native_state: Option<GoalState>,
    pub structured_complete: bool,
    pub structured_blocked: bool,
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
}

/// Represents the broker action selected after a goal turn ends.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ContinuationDecision {
    Continue,
    RetryNoProgress,
    Complete,
    Blocked,
    Stalled,
    Settled,
    Stop,
}

impl GoalRecord {
    /// Resolve the next goal action from turn evidence and continuation guards.
    pub fn observe(&mut self, evidence: TurnEvidence, now_ms: i64) -> ContinuationDecision {
        if self.state == GoalState::Active {
            self.updated_at_ms = now_ms;
        }
        observe(
            &mut self.state,
            &mut self.continuation,
            self.native,
            evidence,
        )
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
    native: bool,
    evidence: TurnEvidence,
) -> ContinuationDecision {
    if *state != GoalState::Active {
        return ContinuationDecision::Stop;
    }
    if native
        && let Some(native_state) = evidence
            .native_state
            .filter(|state| *state != GoalState::Active)
    {
        *state = native_state;
        return ContinuationDecision::Settled;
    }
    if evidence.structured_complete {
        *state = GoalState::Complete;
        return ContinuationDecision::Complete;
    }
    if evidence.structured_blocked {
        *state = GoalState::Blocked;
        return ContinuationDecision::Blocked;
    }
    let progress = evidence.workspace_changed || evidence.tool_called;
    let may_continue = continuation.observe(progress);
    if !may_continue {
        *state = GoalState::Stalled;
        return ContinuationDecision::Stalled;
    }
    if progress {
        return ContinuationDecision::Continue;
    }
    if continuation.consecutive_no_progress == 1 {
        ContinuationDecision::RetryNoProgress
    } else {
        *state = GoalState::Stalled;
        ContinuationDecision::Stalled
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn goal_control_keeps_its_outcome_with_the_admitted_revision() {
        let previous =
            crate::task::ActiveTask::goal("Previous".into(), Default::default(), false, 1);
        let current = crate::task::ActiveTask::goal("Current".into(), Default::default(), false, 2);
        for termination in [GoalTermination::Complete, GoalTermination::Blocked] {
            let output = crate::backend::BackendOutput {
                event: Vec::new(),
                completion: crate::backend::BackendCompletion {
                    evidence: TurnEvidence {
                        structured_complete: true,
                        structured_blocked: true,
                        ..Default::default()
                    },
                    goal_control: Some(GoalControl {
                        permit: current.permit(),
                        termination,
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
            native: false,
            created_at_ms: 0,
            updated_at_ms: 0,
        }
    }

    #[test]
    fn stalls_after_two_turns_without_tools_or_workspace_change() {
        let mut record = goal();
        assert_eq!(
            record.observe(TurnEvidence::default(), 1),
            ContinuationDecision::RetryNoProgress
        );
        assert_eq!(
            record.observe(TurnEvidence::default(), 2),
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
                    tool_called: true,
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
            tool_called: true,
            ..TurnEvidence::default()
        };
        assert_eq!(record.observe(progress, 1), ContinuationDecision::Continue);
        assert_eq!(record.observe(progress, 2), ContinuationDecision::Stalled);
        assert_eq!(record.continuation.turn_count, 2);
    }

    #[test]
    fn native_settlement_overrides_progress_and_freezes_continuation() {
        for state in [
            GoalState::Paused,
            GoalState::Complete,
            GoalState::Blocked,
            GoalState::UsageLimited,
            GoalState::BudgetLimited,
            GoalState::Cleared,
        ] {
            let mut record = goal();
            record.native = true;
            let evidence = TurnEvidence {
                native_state: Some(state),
                tool_called: true,
                workspace_changed: true,
                structured_complete: true,
                ..TurnEvidence::default()
            };
            assert_eq!(record.observe(evidence, 20), ContinuationDecision::Settled);
            assert_eq!(record.state, state);
            assert_eq!(record.continuation.turn_count, 0);
            assert_eq!(record.observe(evidence, 40), ContinuationDecision::Stop);
            assert_eq!(record.updated_at_ms, 20);
            if matches!(
                state,
                GoalState::Paused
                    | GoalState::Blocked
                    | GoalState::UsageLimited
                    | GoalState::BudgetLimited
            ) {
                record.resume(50);
                assert_eq!(
                    record.observe(
                        TurnEvidence {
                            tool_called: true,
                            ..TurnEvidence::default()
                        },
                        60
                    ),
                    ContinuationDecision::Continue
                );
            }
        }
    }

    #[test]
    fn native_settlement_cannot_stop_harness_owned_plan_execution() {
        let mut record = goal();
        assert_eq!(
            record.observe(
                TurnEvidence {
                    native_state: Some(GoalState::Cleared),
                    tool_called: true,
                    ..TurnEvidence::default()
                },
                20
            ),
            ContinuationDecision::Continue
        );
        assert_eq!(record.state, GoalState::Active);
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
                    native_state: Some(GoalState::Complete),
                    ..TurnEvidence::default()
                },
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
