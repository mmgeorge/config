use serde::{Deserialize, Serialize};

/// Owns bounded provider-turn accounting for broker-controlled continuation loops.
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
        Self::new(20, 2)
    }
}

#[cfg(test)]
mod test {
    use super::ContinuationBudget;

    #[test]
    fn exhausts_each_limit_without_requesting_an_extra_turn() {
        let mut no_progress = ContinuationBudget::new(20, 2);
        assert!(no_progress.observe(false));
        assert!(!no_progress.observe(false));

        let mut total = ContinuationBudget::new(2, 2);
        assert!(total.observe(true));
        assert!(!total.observe(true));
        assert_eq!(total.turn_count, 2);
    }

    #[test]
    fn reset_preserves_configuration() {
        let mut budget = ContinuationBudget::new(7, 3);
        budget.observe(false);
        budget.reset();
        assert_eq!(budget, ContinuationBudget::new(7, 3));
    }
}
