use crate::plan::execution::{SemanticProgress, VerificationStatus};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::PathBuf};

/// Classify one admitted command as verification or source preparation.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckPurpose {
    /// Certify unchanged inputs after successful execution.
    #[default]
    Verify,
    /// Permit source edits and retain the resulting inputs for later verification.
    Prepare,
}

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
    /// Validated structured outcome of the adapter's category contract.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub result: Option<super::adapter::AdapterResult>,
    /// Accepted live progress from the adapter's category contract.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub adapter_progress: Option<super::adapter::AdapterProgress>,
}

impl<CommandContext> CommandCheck<CommandContext> {
    /// Preserve the command failure and expose located error diagnostics to plan repair.
    pub(crate) fn findings(&self) -> Vec<String> {
        if !matches!(
            self.state,
            CheckState::Failed
                | CheckState::Blocked
                | CheckState::ConfigurationError
                | CheckState::ExecutionError
                | CheckState::Stale
        ) {
            return Vec::new();
        }
        let mut summary = format!(
            "{}: {:?}, exit {:?}. {}\nSaved output: {}",
            self.command,
            self.state,
            self.exit_code,
            self.error.as_deref().unwrap_or(""),
            self.output.display()
        );
        let mut located = Vec::new();
        if let Some(super::adapter::AdapterResult::Diagnostics(result)) = &self.result {
            for diagnostic in result
                .diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.severity == super::adapter::events::Severity::Error)
            {
                let message = diagnostic
                    .code
                    .as_ref()
                    .filter(|code| !code.is_empty())
                    .map_or_else(
                        || diagnostic.message.clone(),
                        |code| format!("{} [{code}]", diagnostic.message),
                    );
                if let (Some(path), Some(range)) = (&diagnostic.path, diagnostic.range) {
                    located.push(format!(
                        "{path}:{}:{}: {message}",
                        range.start().line(),
                        range.start().column()
                    ));
                } else {
                    summary.push_str("\nError: ");
                    if let Some(path) = &diagnostic.path {
                        summary.push_str(path);
                        summary.push_str(": ");
                    }
                    summary.push_str(&message);
                }
            }
            let omitted = result
                .truncated
                .get(&super::adapter::events::Severity::Error)
                .copied()
                .unwrap_or_default();
            if omitted > 0 {
                summary.push_str(&format!(
                    "\n{omitted} additional error diagnostics omitted; inspect saved output."
                ));
            }
            if result.truncated_bytes > 0 {
                summary.push_str(&format!(
                    "\n{} bytes of diagnostic text omitted; inspect saved output.",
                    result.truncated_bytes
                ));
            }
        }
        let mut findings = vec![summary];
        findings.extend(located);
        findings
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

/// Retains a Harness-owned gate without creating a provider exchange.
#[derive(Clone, Debug, Serialize)]
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
    /// Exact planned-test requirements and per-case evidence retained with this run.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub planned_tests: Option<super::PlannedTestExecution>,
    pub workspace_identity: Option<String>,
    pub workspace_digest: BTreeMap<String, String>,
    pub commands: Vec<CommandCheck<CommandContext>>,
}

/// Decode the complete run before validating references across planned and command evidence.
#[derive(Deserialize)]
#[serde(bound(
    deserialize = "RunContext: Deserialize<'de> + Default, CommandContext: Deserialize<'de> + Default"
))]
struct RawCheckRun<RunContext, CommandContext> {
    id: String,
    owner: Box<crate::timeline::TimelineOwner>,
    revision: u32,
    #[serde(default)]
    context: RunContext,
    #[serde(default)]
    lifecycle: Vec<CheckEvent>,
    completed_plan: Option<String>,
    shell: crate::session::CheckShell,
    executable: Option<PathBuf>,
    workspace: String,
    environment_digest: String,
    started_at_ms: i64,
    completed_at_ms: Option<i64>,
    #[serde(default)]
    cancelled_inputs: Option<crate::checkpoint::CheckpointRecord>,
    #[serde(default)]
    cancelled_run_id: Option<String>,
    state: CheckState,
    progress: SemanticProgress,
    #[serde(default)]
    planned_tests: Option<super::PlannedTestExecution>,
    workspace_identity: Option<String>,
    workspace_digest: BTreeMap<String, String>,
    commands: Vec<CommandCheck<CommandContext>>,
}

impl<'de, RunContext, CommandContext> Deserialize<'de> for CheckRun<RunContext, CommandContext>
where
    RunContext: Deserialize<'de> + Default + Clone + Into<Option<CheckRunContext>>,
    CommandContext: Deserialize<'de> + Default + Clone + Into<Option<CheckCommand>>,
{
    /// Reject planned results detached from the verification command that reported them.
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let RawCheckRun {
            id,
            owner,
            revision,
            context,
            lifecycle,
            completed_plan,
            shell,
            executable,
            workspace,
            environment_digest,
            started_at_ms,
            completed_at_ms,
            cancelled_inputs,
            cancelled_run_id,
            state,
            progress,
            planned_tests,
            workspace_identity,
            workspace_digest,
            commands,
        } = RawCheckRun::<RunContext, CommandContext>::deserialize(deserializer)?;
        if let Some(planned) = &planned_tests {
            planned
                .validate_context(context.clone().into().as_ref(), &owner, &workspace)
                .map_err(serde::de::Error::custom)?;
            planned
                .validate_commands(&commands)
                .map_err(serde::de::Error::custom)?;
        }
        Ok(Self {
            id,
            owner,
            revision,
            context,
            lifecycle,
            completed_plan,
            shell,
            executable,
            workspace,
            environment_digest,
            started_at_ms,
            completed_at_ms,
            cancelled_inputs,
            cancelled_run_id,
            state,
            progress,
            planned_tests,
            workspace_identity,
            workspace_digest,
            commands,
        })
    }
}

impl<RunContext, CommandContext> CheckRun<RunContext, CommandContext> {
    /// Collect declaration and check findings without converting warnings into repair work.
    pub(crate) fn findings(&self) -> Vec<String> {
        let mut findings = self.progress.findings();
        if let Some(planned) = &self.planned_tests {
            findings.extend(planned.findings());
        }
        findings.extend(self.commands.iter().flat_map(CommandCheck::findings));
        findings
    }

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
    pub fn status(&self) -> VerificationStatus {
        if self.commands.iter().any(|check| {
            matches!(
                check.state,
                CheckState::ConfigurationError | CheckState::Blocked | CheckState::ExecutionError
            )
        }) {
            return VerificationStatus::Blocked;
        }
        if self
            .planned_tests
            .as_ref()
            .is_some_and(|planned| !planned.satisfied())
            || !self.progress.missing.is_empty()
            || !self.progress.different.is_empty()
            || self
                .commands
                .iter()
                .any(|check| check.state == CheckState::Failed)
        {
            VerificationStatus::Failed
        } else if self.progress.unverified.is_empty()
            && self
                .commands
                .iter()
                .all(|check| matches!(check.state, CheckState::Passed | CheckState::Skipped))
        {
            VerificationStatus::Passed
        } else {
            VerificationStatus::Blocked
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains configured identity, requested coverage, and the handler's resolved coverage.
pub struct CheckCommand {
    /// Record whether this invocation verifies inputs or prepares changed source.
    pub purpose: super::CheckPurpose,
    /// Retain source snapshots even when preparation fails or is interrupted.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub preparation: Option<PreparationSnapshot>,
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
}

/// Distinguish an unfinished preparation capture from known edits or capture failure.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum PreparationSnapshot {
    /// The command may have started, but no after-snapshot was recorded yet.
    Started {
        before: Box<crate::checkpoint::CheckpointRecord>,
    },
    /// Both snapshots are available for inspecting all source changes.
    Captured {
        before: Box<crate::checkpoint::CheckpointRecord>,
        after: Box<crate::checkpoint::CheckpointRecord>,
    },
    /// Source may have changed, but the capture failure prevents claiming a known diff.
    Unknown {
        before: Box<crate::checkpoint::CheckpointRecord>,
        error: String,
    },
}

impl CheckCommand {
    /// Includes this invocation's exact tests or its single selected file.
    pub(crate) fn label(&self) -> String {
        let names: Vec<&str> = match &self.selection {
            super::CheckSelection::Files { files } if files.len() == 1 => vec![files[0].as_str()],
            super::CheckSelection::Tests { tests } if tests.ids.len() == 1 => {
                vec![tests.ids[0].as_str()]
            }
            _ => Vec::new(),
        };
        if names.is_empty() {
            self.name.clone()
        } else {
            format!("{} · {}", self.name, names.join(", "))
        }
    }
}

/// Freezes project configuration and user overrides independently from the design document.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "RawCheckRunContext")]
pub struct CheckRunContext {
    #[serde(default)]
    /// Permits automatic execution to reuse current recorded coverage.
    pub automatic: bool,
    pub trigger: String,
    pub task_id: Option<String>,
    pub snapshot: super::ConfigSnapshot,
    pub overrides: BTreeMap<String, bool>,
    /// Task settings captured before reviewer exclusions are applied.
    pub(crate) task_overrides: super::CheckOverrides,
    /// Complete task-relative path changes used to evaluate all check triggers.
    pub changed_files: Vec<String>,
}

/// Restore task-owned scope choices after decoding the authored configuration shape.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawCheckRunContext {
    #[serde(default)]
    automatic: bool,
    trigger: String,
    task_id: Option<String>,
    snapshot: super::ConfigSnapshot,
    overrides: BTreeMap<String, bool>,
    task_overrides: super::CheckOverrides,
    changed_files: Vec<String>,
}

impl TryFrom<RawCheckRunContext> for CheckRunContext {
    type Error = anyhow::Error;

    /// Preserve effective scopes without adding task preferences to authored check definitions.
    fn try_from(raw: RawCheckRunContext) -> anyhow::Result<Self> {
        let RawCheckRunContext {
            automatic,
            trigger,
            task_id,
            mut snapshot,
            overrides,
            task_overrides,
            changed_files,
        } = raw;
        for (id, scope) in &task_overrides.scopes {
            let definition = snapshot
                .config
                .definitions
                .iter_mut()
                .find(|definition| &definition.id == id)
                .ok_or_else(|| anyhow::anyhow!("task scope names an unknown check: {id}"))?;
            anyhow::ensure!(
                scope.supports(definition.category),
                "task scope is incompatible with check {id}"
            );
            definition.scope = *scope;
        }
        Ok(Self {
            automatic,
            trigger,
            task_id,
            snapshot,
            overrides,
            task_overrides,
            changed_files,
        })
    }
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
            result: self.result,
            adapter_progress: self.adapter_progress,
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
            planned_tests: self.planned_tests,
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
            planned_tests: run.planned_tests,
            workspace_identity: run.workspace_identity,
            workspace_digest: run.workspace_digest,
            commands,
        })
    }
}

#[cfg(test)]
mod finding_tests {
    use super::*;
    use serde_json::json;

    /// Preserve task coverage policy when authored definitions omit their derived scope.
    #[test]
    fn restored_context_preserves_task_scopes_and_rejects_invalid_scopes() {
        let source = json!({
            "trigger":"test", "task_id":"task",
            "snapshot":{"digest":"digest", "helper_digest":{}, "config":{
                "unit":{"action":{"category":"test","adapter":{"script":{"command":["unused"]}}}}
            }},
            "overrides":{}, "task_overrides":{"scopes":{"unit":"all"}},
            "changed_files":[]
        });
        let context: CheckRunContext = serde_json::from_value(source.clone()).unwrap();
        assert_eq!(
            context.snapshot.config.definitions[0].scope,
            super::super::CheckScope::All
        );
        let restored: CheckRunContext =
            serde_json::from_value(serde_json::to_value(&context).unwrap()).unwrap();
        assert_eq!(restored, context);
        for (id, scope, expected) in [
            ("unit", "paths", "incompatible"),
            ("unknown", "tests", "unknown check"),
            ("unit", "changed_cases", "removed"),
            ("unit", "new_cases", "removed"),
            ("unit", "changed_files", "removed"),
        ] {
            let mut invalid = source.clone();
            invalid["task_overrides"]["scopes"] = json!({id:scope});
            let error = serde_json::from_value::<CheckRunContext>(invalid).unwrap_err();
            assert!(error.to_string().contains(expected), "{error}");
        }
        let mut legacy = source;
        legacy["planned_cases"] = json!([]);
        assert!(
            serde_json::from_value::<CheckRunContext>(legacy)
                .unwrap_err()
                .to_string()
                .contains("planned_cases")
        );
    }

    /// Restore diagnostic results to exercise the same typed boundary as plan recovery.
    fn command(state: CheckState) -> CommandCheck {
        serde_json::from_value(json!({
            "selected":true,"id":"compiler","command":"cargo check","state":state,
            "started_at_ms":1,"completed_at_ms":2,"exit_code":1,"error":"compiler failed",
            "stdout":"stdout","stderr":"stderr","output":"compiler.log","output_bytes":0,"preview":"",
            "result":{"category":"diagnostics","selection":{"paths":[]},"widened":null,"targets":[],
                "diagnostics":[
                    {"severity":"error","message":"wrong type","path":"src/π.rs","range":{"start":{"line":3,"column":5}},"code":"E0308"},
                    {"severity":"error","message":"linker unavailable"},
                    {"severity":"error","message":"cannot parse file","path":"src/bad.rs"},
                    {"severity":"warning","message":"unused value","path":"src/π.rs","range":{"start":{"line":2,"column":1}}},
                    {"severity":"info","message":"additional context"}],
                "truncated":{"error":2,"warning":1},"truncated_bytes":9,"artifacts":[],
                "error":"compiler failed","protocol_error":null,"exit":1,"cancelled":false,"unsupported":false}
        })).unwrap()
    }

    /// Preserve failures and unknown locations without promoting advisory diagnostics.
    #[test]
    fn diagnostic_findings_preserve_locations_unlocated_errors_and_omissions() {
        let check = command(CheckState::Failed);
        let findings = check.findings();
        assert_eq!(findings.len(), 2);
        assert_eq!(findings[1], "src/π.rs:3:5: wrong type [E0308]");
        for expected in [
            "cargo check: Failed",
            "compiler failed",
            "compiler.log",
            "Error: linker unavailable",
            "Error: src/bad.rs: cannot parse file",
            "2 additional error diagnostics omitted",
            "9 bytes of diagnostic text omitted",
        ] {
            assert!(findings[0].contains(expected), "{}", findings[0]);
        }
        assert!(!findings.join("\n").contains("unused value"));
        assert!(!findings.join("\n").contains("additional context"));
        for state in [
            CheckState::Passed,
            CheckState::Skipped,
            CheckState::Interrupted,
            CheckState::Pending,
            CheckState::Running,
            CheckState::Stopped,
        ] {
            assert!(command(state).findings().is_empty(), "{state:?}");
        }
    }

    /// Keep declaration findings before each failed command's summary and located diagnostics.
    #[test]
    fn run_findings_keep_declaration_and_command_failure_order() {
        let mut run: CheckRun = serde_json::from_value(json!({
            "id":"run","owner":{"kind":"session","session_id":"session"},"revision":0,
            "shell":"system","executable":null,"workspace":".","environment_digest":"test",
            "started_at_ms":1,"completed_at_ms":2,"state":"failed",
            "progress":crate::plan::execution::SemanticProgress::default(),"workspace_identity":null,"workspace_digest":{},
            "commands":[command(CheckState::Failed),command(CheckState::Passed)]
        })).unwrap();
        run.progress.missing.push("missing.rs".into());
        let findings = run.findings();
        assert_eq!(findings.len(), 3);
        assert_eq!(findings[0], "missing.rs: missing planned file");
        assert!(findings[1].starts_with("cargo check: Failed"));
        assert_eq!(findings[2], "src/π.rs:3:5: wrong type [E0308]");
    }
}
