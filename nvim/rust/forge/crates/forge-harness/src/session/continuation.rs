use serde::{Deserialize, Serialize};

/// Own bounded turn and no-progress accounting for Harness continuation loops.
///
/// Exhausting either configured limit stops automatic continuation before another
/// input is scheduled. Native goal state and provider token budgets do not alter
/// this host-owned accounting.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ContinuationBudget {
    pub turn_count: u32,
    pub max_turn_count: u32,
    pub consecutive_no_progress: u8,
    pub max_consecutive_no_progress: u8,
}

impl ContinuationBudget {
    /// Build a fresh continuation budget from its total and no-progress limits.
    pub fn new(max_turn_count: u32, max_consecutive_no_progress: u8) -> Self {
        Self {
            turn_count: 0,
            max_turn_count,
            consecutive_no_progress: 0,
            max_consecutive_no_progress,
        }
    }

    /// Record one provider turn and report whether another turn remains available.
    pub fn observe(&mut self, progress: bool) -> bool {
        self.turn_count = self.turn_count.saturating_add(1);
        if progress {
            self.consecutive_no_progress = 0;
        } else {
            self.consecutive_no_progress = self.consecutive_no_progress.saturating_add(1);
        }
        self.turn_count < self.max_turn_count
            && self.consecutive_no_progress < self.max_consecutive_no_progress
    }

    /// Reset all consumed allowance while preserving configured limits.
    pub fn reset(&mut self) {
        self.turn_count = 0;
        self.consecutive_no_progress = 0;
    }

    /// Start a new progress interval after an external decision unblocks the loop.
    pub fn reset_no_progress(&mut self) {
        self.consecutive_no_progress = 0;
    }
}

impl Default for ContinuationBudget {
    fn default() -> Self {
        Self::new(20, 5)
    }
}
