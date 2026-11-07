use serde::{Deserialize, Serialize};

/// Report the native context window: tokens in use, total capacity, and the fixed native
/// overhead inside that capacity that a conversation can never reclaim.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ContextUsage {
    pub used: u64,
    pub size: u64,
    pub reserved: u64,
}

impl ContextUsage {
    /// Report usage for a window with no reserved native overhead.
    pub fn reported(used: u64, size: u64) -> Option<Self> {
        Self::with_reserved(used, size, 0)
    }

    /// Report usage whose window keeps `reserved` tokens for native instructions and tools.
    /// Rejects a window with no capacity beyond that reservation.
    pub fn with_reserved(used: u64, size: u64, reserved: u64) -> Option<Self> {
        (size > reserved).then_some(Self {
            used,
            size,
            reserved,
        })
    }
}
