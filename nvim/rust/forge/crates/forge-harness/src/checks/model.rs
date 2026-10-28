use crate::plan::execution::{SemanticProgress, VerificationOutcome};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::PathBuf};

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Retains check discovery context across follow-ups and host recovery.
pub enum CheckSetupState {
    /// Configuration discovery is active.
    Discovery,
    /// The user supplied clarification, independently of publication approval.
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
pub struct CommandCheck<CommandContext = Option<CheckCommand>> {
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
    pub context: CommandContext,
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
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Validated file-level outcomes from a structured runner, separate from this command's status.
    pub report: Option<super::CheckReport>,
}

impl<CommandContext> CommandCheck<CommandContext> {
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
#[serde(bound(
    deserialize = "RunContext: Deserialize<'de> + Default, CommandContext: Deserialize<'de> + Default"
))]
pub struct CheckRun<RunContext = Option<CheckRunContext>, CommandContext = Option<CheckCommand>> {
    pub id: String,
    /// Destination and source position captured before check execution begins.
    pub owner: Box<crate::timeline::TimelineOwner>,
    pub revision: u32,
    #[serde(default)]
    pub context: RunContext,
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
    pub commands: Vec<CommandCheck<CommandContext>>,
}

impl<RunContext, CommandContext> CheckRun<RunContext, CommandContext> {
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
    /// Configured operation before named inputs resolve.
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

impl CheckCommand {
    /// Includes this invocation's exact tests or its single selected file.
    pub(crate) fn label(&self) -> String {
        let names: Vec<&str> = if let Some(invocation) = &self.invocation
            && !invocation.cases.is_empty()
        {
            invocation
                .cases
                .iter()
                .map(|case| case.name.as_str())
                .collect()
        } else {
            match &self.selection {
                super::CheckSelection::Cases { cases } if cases.len() == 1 => {
                    vec![cases[0].name.as_str()]
                }
                super::CheckSelection::Files { files } if files.len() == 1 => {
                    vec![files[0].as_str()]
                }
                _ => Vec::new(),
            }
        };
        if names.is_empty() {
            self.name.clone()
        } else {
            format!("{} · {}", self.name, names.join(", "))
        }
    }
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
    #[serde(default)]
    /// Accepted plan cases available independently of explicit input overrides.
    pub planned_cases: Vec<super::CheckCase>,
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

impl<RunContext, CommandContext> CheckRun<RunContext, CommandContext> {
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
        CheckRun::summarize_states(self.commands.iter().map(|check| check.state), None)
    }
}
impl CheckRun {
    pub(crate) fn summarize_states(
        states: impl Iterator<Item = CheckState> + Clone,
        total: Option<usize>,
    ) -> String {
        let count = |state| {
            states
                .clone()
                .filter(|candidate| *candidate == state)
                .count()
        };
        let passed = count(CheckState::Passed);
        let mut summary = vec![match total {
            Some(total) => format!("{passed}/{total} passed"),
            None => format!("{passed} passed"),
        }];
        let failed = count(CheckState::Failed);
        if failed > 0 {
            summary.push(format!("{failed} failed"));
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
}
impl<RunContext, CommandContext> CheckRun<RunContext, CommandContext> {
    /// Counts recorded execution time while excluding paused intervals.
    pub(crate) fn duration_ms(&self) -> u64 {
        self.commands
            .iter()
            .filter_map(|check| check.elapsed_ms(super::supervisor::now_ms()))
            .fold(0, u64::saturating_add)
    }
}

pub(crate) type PreparedCheckRun = CheckRun<CheckRunContext, CheckCommand>;

impl<CommandContext> CommandCheck<CommandContext> {
    fn map_context<NextContext>(
        self,
        map: impl FnOnce(CommandContext) -> NextContext,
    ) -> CommandCheck<NextContext> {
        CommandCheck {
            selected: self.selected,
            evidence: self.evidence,
            reused_from: self.reused_from,
            id: self.id,
            command: self.command,
            context: map(self.context),
            state: self.state,
            started_at_ms: self.started_at_ms,
            completed_at_ms: self.completed_at_ms,
            exit_code: self.exit_code,
            error: self.error,
            stdout: self.stdout,
            stderr: self.stderr,
            output: self.output,
            output_bytes: self.output_bytes,
            preview: self.preview,
            report: self.report,
        }
    }
}
impl<RunContext, CommandContext> CheckRun<RunContext, CommandContext> {
    fn map_context<NextRun, NextCommand>(
        self,
        run_context: impl FnOnce(RunContext) -> NextRun,
        mut command_context: impl FnMut(CommandContext) -> NextCommand,
    ) -> CheckRun<NextRun, NextCommand> {
        CheckRun {
            id: self.id,
            owner: self.owner,
            revision: self.revision,
            context: run_context(self.context),
            lifecycle: self.lifecycle,
            completed_plan: self.completed_plan,
            shell: self.shell,
            executable: self.executable,
            workspace: self.workspace,
            environment_digest: self.environment_digest,
            started_at_ms: self.started_at_ms,
            completed_at_ms: self.completed_at_ms,
            cancelled_inputs: self.cancelled_inputs,
            cancelled_run_id: self.cancelled_run_id,
            state: self.state,
            progress: self.progress,
            workspace_identity: self.workspace_identity,
            workspace_digest: self.workspace_digest,
            commands: self
                .commands
                .into_iter()
                .map(|command| command.map_context(&mut command_context))
                .collect(),
        }
    }
}
impl PreparedCheckRun {
    pub(crate) fn into_record(self) -> CheckRun {
        self.map_context(Some, Some)
    }
}

impl CommandCheck<CheckCommand> {
    pub(crate) fn into_record(self) -> CommandCheck {
        self.map_context(Some)
    }
}

impl TryFrom<CheckRun> for PreparedCheckRun {
    type Error = anyhow::Error;
    fn try_from(mut run: CheckRun) -> anyhow::Result<Self> {
        let context = run
            .context
            .take()
            .ok_or_else(|| anyhow::anyhow!("legacy check run has no execution context"))?;
        let commands = std::mem::take(&mut run.commands)
            .into_iter()
            .map(|mut command| {
                let context = command
                    .context
                    .take()
                    .ok_or_else(|| anyhow::anyhow!("legacy command has no execution context"))?;
                Ok(command.map_context(|_| context))
            })
            .collect::<anyhow::Result<Vec<_>>>()?;
        Ok(CheckRun {
            id: run.id,
            owner: run.owner,
            revision: run.revision,
            context,
            lifecycle: run.lifecycle,
            completed_plan: run.completed_plan,
            shell: run.shell,
            executable: run.executable,
            workspace: run.workspace,
            environment_digest: run.environment_digest,
            started_at_ms: run.started_at_ms,
            completed_at_ms: run.completed_at_ms,
            cancelled_inputs: run.cancelled_inputs,
            cancelled_run_id: run.cancelled_run_id,
            state: run.state,
            progress: run.progress,
            workspace_identity: run.workspace_identity,
            workspace_digest: run.workspace_digest,
            commands,
        })
    }
}
