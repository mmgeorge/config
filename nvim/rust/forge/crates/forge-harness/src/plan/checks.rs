use super::execution::{SemanticProgress, VerificationOutcome};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::PathBuf};

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Separates command completion from interruption and execution prerequisites.
pub enum CheckState {
    Pending,
    Running,
    Passed,
    Failed,
    Blocked,
    Interrupted,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Owns one exact accepted command and its durable output identities.
pub struct CommandCheck {
    pub id: String,
    pub command: String,
    pub state: CheckState,
    pub started_at_ms: Option<i64>,
    pub completed_at_ms: Option<i64>,
    pub exit_code: Option<i32>,
    pub error: Option<String>,
    pub stdout: PathBuf,
    pub stderr: PathBuf,
    pub output: PathBuf,
    pub output_bytes: u64,
    pub preview: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains a Harness-owned gate without creating a provider exchange.
pub struct CheckRun {
    pub id: String,
    pub revision: u32,
    pub completed_plan: Option<String>,
    pub shell: crate::session::CheckShell,
    pub executable: Option<PathBuf>,
    pub workspace: String,
    pub environment_digest: String,
    pub started_at_ms: i64,
    pub completed_at_ms: Option<i64>,
    pub state: CheckState,
    pub progress: SemanticProgress,
    pub workspace_identity: Option<String>,
    pub workspace_digest: BTreeMap<String, String>,
    pub commands: Vec<CommandCheck>,
}

impl CheckRun {
    /// Select the gate result from recorded failures rather than model interpretation.
    pub fn outcome(&self) -> VerificationOutcome {
        if !self.progress.missing.is_empty()
            || !self.progress.different.is_empty()
            || self
                .commands
                .iter()
                .any(|check| check.state == CheckState::Failed)
        {
            VerificationOutcome::Failed
        } else if self.progress.unverified.is_empty()
            && self
                .commands
                .iter()
                .all(|check| check.state == CheckState::Passed)
        {
            VerificationOutcome::Passed
        } else {
            VerificationOutcome::Blocked
        }
    }
}
