use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ContextUsage {
    pub used: u64,
    pub size: u64,
    pub remaining_percent: u8,
}

impl ContextUsage {
    pub fn reported(used: u64, size: u64) -> Option<Self> {
        Self::with_baseline(used, size, 0)
    }

    pub fn with_baseline(used: u64, size: u64, baseline: u64) -> Option<Self> {
        let effective_size = size.checked_sub(baseline)?;
        if effective_size == 0 {
            return None;
        }
        let effective_used = used.saturating_sub(baseline);
        let remaining = effective_size.saturating_sub(effective_used);
        let remaining_percent = ((remaining as f64 / effective_size as f64) * 100.0)
            .clamp(0.0, 100.0)
            .round() as u8;
        Some(Self {
            used,
            size,
            remaining_percent,
        })
    }
}
