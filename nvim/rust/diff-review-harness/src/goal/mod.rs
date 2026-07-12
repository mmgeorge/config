use serde::{Deserialize, Serialize};

/// Represents the lifecycle state of one persistent harness goal.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum GoalState {
    Active,
    Paused,
    Complete,
    Blocked,
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
    pub continuation_count: u32,
    pub max_continuation: u32,
    pub consecutive_no_progress: u8,
    pub native: bool,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}

/// Represents observable evidence produced by one backend turn.
#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
pub struct TurnEvidence {
    pub workspace_changed: bool,
    pub tool_called: bool,
    pub native_complete: bool,
    pub structured_complete: bool,
    pub structured_blocked: bool,
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
        self.updated_at_ms = now_ms;
        if evidence.native_complete || evidence.structured_complete {
            self.state = GoalState::Complete;
            return ContinuationDecision::Complete;
        }
        if evidence.structured_blocked {
            self.state = GoalState::Blocked;
            return ContinuationDecision::Blocked;
        }
        if self.state != GoalState::Active {
            return ContinuationDecision::Stop;
        }
        if self.continuation_count >= self.max_continuation {
            self.state = GoalState::Stalled;
            return ContinuationDecision::Stalled;
        }

        self.continuation_count += 1;
        if self.continuation_count >= self.max_continuation {
            self.state = GoalState::Stalled;
            return ContinuationDecision::Stalled;
        }
        if evidence.workspace_changed || evidence.tool_called {
            self.consecutive_no_progress = 0;
            return ContinuationDecision::Continue;
        }

        self.consecutive_no_progress += 1;
        if self.consecutive_no_progress == 1 {
            ContinuationDecision::RetryNoProgress
        } else {
            self.state = GoalState::Stalled;
            ContinuationDecision::Stalled
        }
    }

    /// Resume goal execution with a fresh continuation budget.
    pub fn resume(&mut self, now_ms: i64) {
        self.state = GoalState::Active;
        self.continuation_count = 0;
        self.consecutive_no_progress = 0;
        self.updated_at_ms = now_ms;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn goal() -> GoalRecord {
        GoalRecord {
            id: "goal".into(),
            session_id: "session".into(),
            objective: "finish".into(),
            state: GoalState::Active,
            continuation_count: 0,
            max_continuation: 20,
            consecutive_no_progress: 0,
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
        assert_eq!(record.consecutive_no_progress, 0);
    }

    #[test]
    fn stops_without_requesting_a_turn_beyond_the_configured_limit() {
        let mut record = goal();
        record.max_continuation = 2;
        let progress = TurnEvidence {
            tool_called: true,
            ..TurnEvidence::default()
        };
        assert_eq!(record.observe(progress, 1), ContinuationDecision::Continue);
        assert_eq!(record.observe(progress, 2), ContinuationDecision::Stalled);
        assert_eq!(record.continuation_count, 2);
    }
}
