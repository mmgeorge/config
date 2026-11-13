//! Project owner-held work without giving clients a second execution state machine.

use serde::Serialize;

/// Describe whether the session owner currently retains executable or cleanup work.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ActivityPhase {
    /// No command, invocation, or cleanup currently owns execution.
    #[default]
    Idle,
    /// Work remains owned, including preparation and cancellation cleanup.
    Working,
    /// Unsettled work or unavailable storage prevents further execution.
    Unavailable,
}

/// Identify an ordered activity projection within one runtime epoch.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize)]
pub struct ActivitySnapshot {
    pub revision: u64,
    pub phase: ActivityPhase,
    /// Explain why execution is unavailable, including the original startup failure.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unavailable_reason: Option<String>,
}

impl ActivitySnapshot {
    /// Advance only when the owner-held activity changes.
    pub(crate) fn changed(&self, phase: ActivityPhase, reason: Option<String>) -> Option<Self> {
        (self.phase != phase || self.unavailable_reason != reason).then(|| Self {
            revision: self
                .revision
                .checked_add(1)
                .expect("activity revision exhausted"),
            phase,
            unavailable_reason: reason,
        })
    }
}
