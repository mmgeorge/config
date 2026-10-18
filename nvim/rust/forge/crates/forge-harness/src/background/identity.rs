use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
/// Captures the Git context last admitted to a Background conversation.
pub struct BackgroundIdentity {
    /// Full symbolic reference, or absent for detached HEAD.
    pub branch: Option<String>,
    /// Commit observed before generating, never a claim that generation committed it.
    pub head: Option<String>,
    /// Explains why this conversation started instead of resuming its predecessor.
    pub reason: String,
}
