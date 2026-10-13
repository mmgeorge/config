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

impl CommandCheck {
    /// Formats the accepted command and its recorded lifecycle for both initial and timer renders.
    pub(crate) fn heading(&self) -> String {
        format!("{} · {:?}", crate::permissions::command::display_command(&self.command,
            crate::permissions::shell::CommandShell::default()).source, self.state)
    }

    /// Advances only running commands and retains recorded completion timestamps.
    pub(crate) fn elapsed_ms(&self, now_ms: i64) -> Option<u64> {
        let started_at_ms = self.started_at_ms?;
        let completed_at_ms = self.completed_at_ms.or_else(|| (self.state == CheckState::Running).then_some(now_ms))?;
        Some(completed_at_ms.saturating_sub(started_at_ms).max(0) as u64)
    }
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
    /// Projects check ownership and elapsed command work without including interrupted offline intervals.
    pub(crate) fn status(&self) -> crate::session::state_machine::SessionPhase {
        use crate::session::state_machine::{SessionPhase, WorkflowActivity};
        if self.state == CheckState::Interrupted { return SessionPhase::Paused; }
        if self.state != CheckState::Running { return SessionPhase::Idle; }
        let duration_ms = self.commands.iter()
            .filter(|check| matches!(check.state, CheckState::Passed | CheckState::Failed | CheckState::Blocked))
            .filter_map(|check| check.elapsed_ms(0)).fold(0_u64, u64::saturating_add);
        let started_at_ms = if let Some(check) = self.commands.iter().find(|check| check.state == CheckState::Running) {
            check.started_at_ms
        } else if self.commands.iter().any(|check| check.state == CheckState::Interrupted) {
            None
        } else {
            self.commands.iter().filter_map(|check|check.completed_at_ms).max().or(Some(self.started_at_ms))
        };
        SessionPhase::Working {duration_ms,started_at_ms,activity:WorkflowActivity::Checking,
            reasoning_summary:None,execution:None}
    }

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
