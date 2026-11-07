use serde::{Deserialize, Serialize};

/// Defines the approval policy selected for one Harness session.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ApprovalMode {
    #[default]
    /// Apply rules and request a decision for remaining tool requests.
    Ask,
    /// Reject tool requests that still require a decision.
    Deny,
    /// Approve remaining tool requests once, preserving explicit denials.
    Allow,
}

impl ApprovalMode {
    /// Return the stable user-facing label for this approval policy.
    pub const fn label(self) -> &'static str {
        match self {
            Self::Ask => "ask",
            Self::Deny => "deny",
            Self::Allow => "allow",
        }
    }
}
