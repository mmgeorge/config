use crate::plan::execution::{SemanticProgress, VerificationOutcome};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::PathBuf};

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Retains setup's user-decision boundary across question continuations and host recovery.
pub enum CheckSetupState {
    /// Configuration discovery has not received the user's setup choices.
    Discovery,
    /// The user answered setup questions and configuration may be published.
    Reviewed,
}

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
    Skipped,
    ConfigurationError,
    ExecutionError,
    Stale,
    Stopped,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Owns one exact accepted command and its durable output identities.
pub struct CommandCheck {
    #[serde(default)]
    /// Membership in this run's selection, independent of execution outcome.
    pub selected: bool,
    #[serde(default)]
    /// Stable inputs and selection certified by this attempt.
    pub evidence: Option<super::evidence::CheckEvidence>,
    #[serde(default)]
    /// Original run and command when an automatic run reused recorded evidence.
    pub reused_from: Option<(String, String)>,
    pub id: String,
    pub command: String,
    #[serde(default)]
    pub context: Option<CheckCommand>,
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
        if let Some(context) = &self.context {
            let scope = match &context.selection {
                super::CheckSelection::All => "All".into(),
                super::CheckSelection::Files { files } => format!("{} files", files.len()),
                super::CheckSelection::Cases { cases } => format!("{} cases", cases.len()),
            };
            return format!("{} · {} · {:?}", context.name, scope, self.state);
        }
        format!(
            "{} · {:?}",
            crate::permissions::command::display_command(
                &self.command,
                crate::permissions::shell::CommandShell::default()
            )
            .source,
            self.state
        )
    }

    /// Advances only running commands and retains recorded completion timestamps.
    pub(crate) fn elapsed_ms(&self, now_ms: i64) -> Option<u64> {
        let started_at_ms = self.started_at_ms?;
        let completed_at_ms = self
            .completed_at_ms
            .or_else(|| (self.state == CheckState::Running).then_some(now_ms))?;
        Some(completed_at_ms.saturating_sub(started_at_ms).max(0) as u64)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains a Harness-owned gate without creating a provider exchange.
pub struct CheckRun {
    pub id: String,
    /// Exchange whose work this run validates, when execution has an exchange owner.
    pub exchange_id: Option<String>,
    pub revision: u32,
    #[serde(default)]
    pub context: Option<CheckRunContext>,
    #[serde(default)]
    pub lifecycle: Vec<CheckEvent>,
    pub completed_plan: Option<String>,
    pub shell: crate::session::CheckShell,
    pub executable: Option<PathBuf>,
    pub workspace: String,
    pub environment_digest: String,
    pub started_at_ms: i64,
    pub completed_at_ms: Option<i64>,
    #[serde(default)]
    /// Inputs of the cancelled attempt, used to prevent an automatic retry of unchanged work.
    pub cancelled_inputs: Option<crate::checkpoint::CheckpointRecord>,
    #[serde(default)]
    /// Interrupted run that prevents automatic execution of unchanged inputs.
    pub cancelled_run_id: Option<String>,
    pub state: CheckState,
    pub progress: SemanticProgress,
    pub workspace_identity: Option<String>,
    pub workspace_digest: BTreeMap<String, String>,
    pub commands: Vec<CommandCheck>,
}

impl CheckRun {
    /// Ends an interrupted run once, including cancellation during check preparation.
    pub(crate) fn interrupt(&mut self) {
        if self.state == CheckState::Interrupted {
            return;
        }
        self.state = CheckState::Interrupted;
        self.completed_at_ms = Some(super::supervisor::now_ms());
        self.record(
            CheckEventKind::Interrupted,
            None,
            "Checks interrupted".into(),
        );
    }

    /// Select the gate result from recorded failures rather than model interpretation.
    pub fn outcome(&self) -> VerificationOutcome {
        if self.commands.iter().any(|check| {
            matches!(
                check.state,
                CheckState::ConfigurationError | CheckState::Blocked | CheckState::ExecutionError
            )
        }) {
            return VerificationOutcome::Blocked;
        }
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
                .all(|check| matches!(check.state, CheckState::Passed | CheckState::Skipped))
        {
            VerificationOutcome::Passed
        } else {
            VerificationOutcome::Blocked
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains configured identity, requested coverage, and the handler's resolved coverage.
pub struct CheckCommand {
    #[serde(default)]
    /// Configured direct operation before package bindings resolve.
    pub operation: Option<super::CheckOperation>,
    #[serde(default)]
    /// Frozen direct invocation executed without shell interpolation.
    pub invocation: Option<super::invocation::CheckInvocation>,
    pub definition_id: String,
    pub name: String,
    /// Functional area retained with the run's frozen configuration.
    pub area: String,
    /// Reason this check was selected or excluded for this change set.
    pub reason: String,
    /// Trigger-matching paths before the handler resolves executable coverage.
    pub matched_files: Vec<String>,
    pub category: super::CheckCategory,
    pub directory: String,
    pub selection: super::CheckSelection,
    pub resolved: Option<super::CheckSelection>,
    pub depends_on: Vec<String>,
    pub receipt: PathBuf,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Freezes project configuration and user overrides independently from the design document.
pub struct CheckRunContext {
    #[serde(default)]
    /// Permits automatic execution to reuse current recorded coverage.
    pub automatic: bool,
    pub trigger: String,
    pub task_id: Option<String>,
    pub snapshot: super::ConfigSnapshot,
    pub overrides: BTreeMap<String, bool>,
    /// Complete task-relative path changes used to evaluate all check triggers.
    pub changed_files: Vec<String>,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
/// Records lifecycle transitions once, independently of timer or output refreshes.
pub enum CheckEventKind {
    Started,
    CheckStarted,
    CheckFinished,
    CheckSkipped,
    ConfigurationError,
    ExecutionError,
    Interrupted,
    Resumed,
    Finished,
    ConfigurationSaved,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Anchors a check transition at a stable chronological timeline position.
pub struct CheckEvent {
    pub id: String,
    pub created_at_ms: i64,
    pub kind: CheckEventKind,
    pub check_id: Option<String>,
    pub text: String,
}

impl CheckRun {
    /// Appends a stable event only when execution changes state.
    pub(crate) fn record(&mut self, kind: CheckEventKind, check_id: Option<String>, text: String) {
        self.lifecycle.push(CheckEvent {
            id: format!("{}:event:{}", self.id, self.lifecycle.len()),
            created_at_ms: super::supervisor::now_ms(),
            kind,
            check_id,
            text,
        });
    }

    /// Summarizes actual recorded results without labeling excluded checks as passed.
    pub fn summary(&self) -> String {
        let count = |state| {
            self.commands
                .iter()
                .filter(|check| check.state == state)
                .count()
        };
        let mut summary = vec![format!("{} passed", count(CheckState::Passed))];
        let failed = count(CheckState::Failed);
        if failed > 0 {
            summary.push(format!("{failed} failed"));
        }
        let skipped = count(CheckState::Skipped) + count(CheckState::Pending);
        if skipped > 0 {
            summary.push(format!("{skipped} not run"));
        }
        for (state, label) in [
            (CheckState::Interrupted, "interrupted"),
            (CheckState::Blocked, "blocked"),
            (CheckState::ConfigurationError, "configuration errors"),
            (CheckState::ExecutionError, "execution errors"),
            (CheckState::Stale, "stale"),
            (CheckState::Stopped, "stopped"),
        ] {
            let total = count(state);
            if total > 0 {
                summary.push(format!("{total} {label}"));
            }
        }
        summary.join(" · ")
    }

    /// Counts recorded execution time while excluding paused intervals.
    pub(crate) fn duration_ms(&self) -> u64 {
        self.commands
            .iter()
            .filter_map(|check| check.elapsed_ms(super::supervisor::now_ms()))
            .fold(0, u64::saturating_add)
    }
}
