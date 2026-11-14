//! Retain exact plan resolution and require a passing result for every required-present case.

use super::inventory::{InventoryResult, PlannedInventory};
use super::resolution::{Outcome, Resolution};
use crate::checks::adapter::{DiscoverMode, TestItemId, TestSelection, events::CaseStatus};
use crate::plan::{DesignTestChange, DesignTestFile};
use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

/// Own a plan's authored inventory and the evidence produced by one admitted Check run.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "RawExecution")]
pub struct PlannedTestExecution {
    requested: Vec<DesignTestFile>,
    test_scopes: BTreeMap<String, crate::plan::PlanTestScope>,
    inputs: Option<super::inputs::ResolutionInputs>,
    state: ResolutionState,
}

impl PlannedTestExecution {
    /// Create unresolved execution requirements from the validated authored inventory.
    pub(crate) fn new(
        requested: Vec<DesignTestFile>,
        test_scopes: BTreeMap<String, crate::plan::PlanTestScope>,
    ) -> Result<Self> {
        crate::plan::validate_design_tests(&requested)?;
        ensure!(
            test_scopes.keys().all(|id| !id.is_empty()),
            "planned test scope has an empty check ID"
        );
        Ok(Self {
            requested,
            test_scopes,
            inputs: None,
            state: ResolutionState::Pending,
        })
    }

    /// Bind one immutable input before the attempt begins discovery.
    pub(super) fn bind_inputs(&mut self, inputs: super::inputs::ResolutionInputs) -> Result<()> {
        ensure!(
            self.inputs.is_none(),
            "planned resolution inputs were already captured"
        );
        self.inputs = Some(inputs);
        Ok(())
    }

    /// Get the captured ownership and verification input when discovery has started.
    pub(in crate::checks) fn inputs(&self) -> Option<&super::inputs::ResolutionInputs> {
        self.inputs.as_ref()
    }

    /// Get the reviewer coverage choices retained with the authored inventory.
    pub(super) fn test_scopes(&self) -> &BTreeMap<String, crate::plan::PlanTestScope> {
        &self.test_scopes
    }

    /// Validate the enclosing run without accessing mutable configuration or the filesystem.
    pub(in crate::checks) fn validate_context(
        &self,
        context: Option<&crate::checks::model::CheckRunContext>,
        owner: &crate::timeline::TimelineOwner,
        workspace: &str,
    ) -> Result<()> {
        if let Some(inputs) = &self.inputs {
            inputs.validate_context(
                context.ok_or_else(|| anyhow::anyhow!("planned execution has no run context"))?,
                &self.test_scopes,
            )?;
            inputs.verification().validate_owner(owner, workspace)?;
        }
        Ok(())
    }

    /// Get the reviewer's coverage choice, defaulting to the required planned cases.
    pub(in crate::checks) fn scope(&self, check: &str) -> crate::plan::PlanTestScope {
        self.test_scopes.get(check).copied().unwrap_or_default()
    }

    /// Reject prior evidence from another authored inventory or reviewer coverage choice.
    pub(crate) fn matches_request(
        &self,
        requested: &[DesignTestFile],
        scopes: &BTreeMap<String, crate::plan::PlanTestScope>,
    ) -> bool {
        self.requested == requested && &self.test_scopes == scopes
    }

    /// Compare the immutable request shared by an admitted and a recovered attempt.
    pub(in crate::checks) fn same_request(&self, other: &Self) -> bool {
        self.matches_request(&other.requested, &other.test_scopes)
    }

    /// Resolve all entries against one complete, exact cross-check inventory.
    pub(in crate::checks) fn resolve(
        &mut self,
        snapshot: String,
        inventories: &[PlannedInventory],
    ) {
        let entries = super::resolution::resolve(&self.requested, DiscoverMode::Exact, inventories)
            .into_iter()
            .map(|resolution| ResolvedTest {
                resolution,
                result: None,
            })
            .collect();
        self.state = ResolutionState::Resolved {
            snapshot,
            entries,
            inventories: inventories.to_vec(),
        };
    }

    /// Get whether any authored entry needs a discoverable owner or absence evidence.
    pub(in crate::checks) fn needs_discovery(&self) -> bool {
        !self.requested.is_empty()
    }

    /// Get the item selections without allowing an empty selection to mean the whole suite.
    pub(in crate::checks) fn selections(&self) -> BTreeMap<String, TestSelection> {
        let mut ids = BTreeMap::<String, Vec<TestItemId>>::new();
        for entry in self.entries() {
            if let Outcome::Resolved(candidate) = &entry.resolution.outcome {
                ids.entry(candidate.check.clone())
                    .or_default()
                    .push(candidate.id.clone());
            }
        }
        ids.into_iter()
            .map(|(check, mut ids)| {
                ids.sort();
                ids.dedup();
                (
                    check,
                    TestSelection {
                        ids,
                        affected: Vec::new(),
                    },
                )
            })
            .collect()
    }

    /// Get whether every entry has an exact presence or absence outcome before execution.
    pub(in crate::checks) fn resolved(&self) -> bool {
        matches!(&self.state, ResolutionState::Resolved { entries, .. }
            if entries.iter().all(|entry| !matches!(entry.resolution.outcome, Outcome::Problem(_))))
    }

    /// Invalidate the complete resolution when its source or configuration changes.
    pub(crate) fn invalidate(&mut self, reason: String) {
        let entries = self.entries().to_vec();
        let inventories = self.inventories().to_vec();
        self.state = ResolutionState::Invalidated {
            reason,
            entries,
            inventories,
        };
    }

    /// Get whether the supplied source is the one used for exact resolution.
    pub(in crate::checks) fn matches_snapshot(&self, snapshot: &str) -> bool {
        matches!(&self.state, ResolutionState::Resolved { snapshot: expected, .. } if expected == snapshot)
    }

    /// Retain each resolved case's own outcome rather than inferring it from its enclosing check.
    pub(in crate::checks) fn record_results<Context>(
        &mut self,
        commands: &[crate::checks::CommandCheck<Context>],
    ) where
        Context: Clone + Into<Option<crate::checks::model::CheckCommand>>,
    {
        let ResolutionState::Resolved { entries, .. } = &mut self.state else {
            return;
        };
        let observed = observed_results(commands);
        for entry in entries {
            let Outcome::Resolved(candidate) = &entry.resolution.outcome else {
                continue;
            };
            entry.result = observed
                .get(&(candidate.check.clone(), candidate.id.clone()))
                .cloned();
        }
    }

    /// Reject saved case outcomes that do not belong to this attempt's verification commands.
    pub(in crate::checks) fn validate_commands<Context>(
        &self,
        commands: &[crate::checks::CommandCheck<Context>],
    ) -> Result<()>
    where
        Context: Clone + Into<Option<crate::checks::model::CheckCommand>>,
    {
        let mut ids = BTreeSet::new();
        for command in commands {
            ensure!(
                !command.id.is_empty() && ids.insert(&command.id),
                "planned check run has an empty or duplicate command ID"
            );
            if let Some(crate::checks::adapter::AdapterResult::Tests(result)) = &command.result {
                let mut cases = BTreeSet::new();
                ensure!(
                    result.cases.iter().all(|case| cases.insert(&case.id)),
                    "planned verification command has duplicate case results"
                );
            }
        }
        let observed = observed_results(commands);
        for entry in self.entries() {
            let (Outcome::Resolved(candidate), Some(result)) =
                (&entry.resolution.outcome, &entry.result)
            else {
                continue;
            };
            ensure!(
                observed.get(&(candidate.check.clone(), candidate.id.clone())) == Some(result),
                "planned case {} / {} does not match its retained verification command {}",
                candidate.check,
                candidate.id.as_str(),
                result.command_id
            );
        }
        Ok(())
    }

    /// Require exact absence or a passing case at the recorded source snapshot.
    pub(crate) fn satisfied(&self) -> bool {
        matches!(&self.state, ResolutionState::Resolved { entries, .. } if entries.iter().all(|entry| {
            match entry.resolution.outcome {
                Outcome::Absent => true,
                Outcome::Resolved(_) => entry.status() == Some(CaseStatus::Passed),
                Outcome::Problem(_) => false,
            }
        }))
    }

    /// Explain unresolved, invalidated, skipped, unfinished, and failing plan obligations.
    pub(crate) fn findings(&self) -> Vec<String> {
        match &self.state {
            ResolutionState::Pending => vec!["Planned test resolution has not completed".into()],
            ResolutionState::Invalidated { reason, .. } => vec![reason.clone()],
            ResolutionState::Resolved { entries, .. } => entries
                .iter()
                .filter_map(|entry| {
                    let resolution = &entry.resolution;
                    match &resolution.outcome {
                        Outcome::Problem(_) => resolution.warning(),
                        Outcome::Absent => None,
                        Outcome::Resolved(candidate) => {
                            let status = match entry.status() {
                                Some(CaseStatus::Passed) => return None,
                                Some(CaseStatus::Skipped) | None => "not run",
                                Some(CaseStatus::Failed) => "failed",
                                Some(CaseStatus::TimedOut) => "timed out",
                                Some(CaseStatus::Errored) => "errored",
                            };
                            Some(format!(
                                "{}: planned test {} ({}, check {}) was {status}",
                                resolution.file,
                                resolution.name,
                                candidate.id.as_str(),
                                candidate.check
                            ))
                        }
                    }
                })
                .collect(),
        }
    }

    /// Get supplementary discovery diagnostics without treating them as failed obligations.
    pub(crate) fn diagnostics(&self) -> Vec<String> {
        self.inventories()
            .iter()
            .filter_map(PlannedInventory::diagnostic)
            .collect()
    }

    /// Describe retained ownership, source provenance, and outcomes for plan review.
    pub(crate) fn review_lines(&self) -> Vec<String> {
        let mut lines = match &self.state {
            ResolutionState::Pending => vec!["Planned tests have not been resolved.".into()],
            ResolutionState::Invalidated { reason, .. } => {
                vec![format!("Invalidated attempt: {reason}")]
            }
            ResolutionState::Resolved { .. } if self.requested.is_empty() => {
                vec!["No planned test obligations.".into()]
            }
            ResolutionState::Resolved { .. } => Vec::new(),
        };
        for entry in self.entries() {
            let resolution = &entry.resolution;
            let outcome = match &resolution.outcome {
                Outcome::Absent => "Removal verified".into(),
                Outcome::Problem(_) => resolution
                    .warning()
                    .expect("every resolution problem has an explanation"),
                Outcome::Resolved(candidate) => {
                    let status = match entry.status() {
                        Some(CaseStatus::Passed) => "Passed",
                        Some(CaseStatus::Failed) => "Failed",
                        Some(CaseStatus::TimedOut) => "Timed out",
                        Some(CaseStatus::Errored) => "Errored",
                        Some(CaseStatus::Skipped) | None => "Not run",
                    };
                    let source = match &candidate.source {
                        super::inventory::SourceLocation::Exact { path, line } => {
                            format!("exact source {}", source_location(path, *line))
                        }
                        super::inventory::SourceLocation::Static { path, line } => {
                            format!("static source {}", source_location(path, *line))
                        }
                        super::inventory::SourceLocation::Unverified => "source unverified".into(),
                    };
                    format!(
                        "{status}; check {} / {}; {source}",
                        candidate.check,
                        candidate.id.as_str()
                    )
                }
            };
            lines.push(format!(
                "{}: {} â€” {outcome}",
                resolution.file, resolution.name
            ));
        }
        lines.extend(self.diagnostics());
        lines
    }

    /// Get retained resolutions independently of validity for inspection and diagnostics.
    fn entries(&self) -> &[ResolvedTest] {
        match &self.state {
            ResolutionState::Pending => &[],
            ResolutionState::Resolved { entries, .. }
            | ResolutionState::Invalidated { entries, .. } => entries,
        }
    }

    /// Get the captured inventories that justify each retained resolution.
    fn inventories(&self) -> &[PlannedInventory] {
        match &self.state {
            ResolutionState::Pending => &[],
            ResolutionState::Resolved { inventories, .. }
            | ResolutionState::Invalidated { inventories, .. } => inventories,
        }
    }
}

/// Separate pending discovery from snapshot-bound evidence and explicitly invalidated evidence.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
enum ResolutionState {
    Pending,
    Resolved {
        snapshot: String,
        entries: Vec<ResolvedTest>,
        inventories: Vec<PlannedInventory>,
    },
    Invalidated {
        reason: String,
        entries: Vec<ResolvedTest>,
        inventories: Vec<PlannedInventory>,
    },
}

/// Keep case results separate from absence outcomes, which never fabricate a case result.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct ResolvedTest {
    resolution: Resolution,
    result: Option<CaseResult>,
}

impl ResolvedTest {
    /// Get the observed status without losing its owning verification command.
    fn status(&self) -> Option<CaseStatus> {
        self.result.as_ref().map(|result| result.status)
    }
}

/// Identify the command in this attempt that reported a required case's terminal status.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
struct CaseResult {
    command_id: String,
    status: CaseStatus,
}

/// Validate restored execution evidence before allowing it to participate in a completion gate.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawExecution {
    requested: Vec<DesignTestFile>,
    test_scopes: BTreeMap<String, crate::plan::PlanTestScope>,
    inputs: Option<super::inputs::ResolutionInputs>,
    state: ResolutionState,
}

impl TryFrom<RawExecution> for PlannedTestExecution {
    type Error = anyhow::Error;

    /// Reject mismatched inventories and outcomes that claim impossible presence or absence.
    fn try_from(raw: RawExecution) -> Result<Self> {
        let RawExecution {
            requested,
            test_scopes,
            inputs,
            state,
        } = raw;
        let mut result = Self::new(requested, test_scopes)?;
        result.state = state;
        result.inputs = inputs;
        match &result.state {
            ResolutionState::Pending => return Ok(result),
            ResolutionState::Resolved { snapshot, .. } => {
                ensure!(!snapshot.is_empty(), "planned test snapshot is empty");
                result
                    .inputs
                    .as_ref()
                    .ok_or_else(|| anyhow::anyhow!("planned resolution inputs are missing"))?
                    .validate_snapshot(snapshot)?;
            }
            ResolutionState::Invalidated {
                reason,
                entries,
                inventories,
            } => {
                ensure!(
                    !reason.is_empty(),
                    "planned test invalidation reason is empty"
                );
                // A source change may invalidate an attempt before it produces any inventory.
                if entries.is_empty() && inventories.is_empty() {
                    return Ok(result);
                }
            }
        }
        let inputs = result
            .inputs
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("planned resolution inputs are missing"))?;
        let expected_checks = if result.needs_discovery() {
            inputs.participants()
        } else {
            Vec::new()
        };
        ensure!(
            expected_checks
                .iter()
                .copied()
                .eq(result.inventories().iter().map(PlannedInventory::check)),
            "planned inventories differ from the captured participating checks"
        );
        let mut checks = BTreeSet::new();
        for inventory in result.inventories() {
            ensure!(
                checks.insert(inventory.check()),
                "duplicate planned inventory check"
            );
            if let InventoryResult::Available(discovery) = inventory.discovery() {
                ensure!(
                    discovery.mode() == DiscoverMode::Exact,
                    "retained planned inventory is not exact"
                );
            }
        }
        let resolutions = super::resolution::resolve(
            &result.requested,
            DiscoverMode::Exact,
            result.inventories(),
        );
        ensure!(
            result
                .entries()
                .iter()
                .map(|entry| &entry.resolution)
                .eq(resolutions.iter()),
            "retained planned resolutions differ from captured inventories"
        );
        let expected = result
            .requested
            .iter()
            .flat_map(|file| file.cases.iter().map(move |case| (&file.file, case)));
        ensure!(
            result.entries().len() == expected.clone().count(),
            "planned resolution inventory does not match its authored tests"
        );
        let mut selected = BTreeSet::new();
        for (entry, (file, case)) in result.entries().iter().zip(expected) {
            let resolution = &entry.resolution;
            ensure!(
                &resolution.file == file
                    && resolution.name == case.name
                    && resolution.change == case.change,
                "planned resolution identity differs from its authored test"
            );
            match &resolution.outcome {
                Outcome::Resolved(candidate) => {
                    ensure!(
                        entry
                            .result
                            .as_ref()
                            .is_none_or(|result| !result.command_id.is_empty()),
                        "planned case result has an empty verification command ID"
                    );
                    ensure!(
                        case.change != DesignTestChange::Removed,
                        "removed test cannot resolve to an execution case"
                    );
                    ensure!(
                        !candidate.check.is_empty() && !candidate.name.is_empty(),
                        "planned case identity is empty"
                    );
                    ensure!(
                        case.target.is_none(),
                        "target selector cannot have a resolved adapter case"
                    );
                    ensure!(
                        selected.insert((&candidate.check, &candidate.id)),
                        "multiple planned entries share one resolved case"
                    );
                }
                Outcome::Absent => ensure!(
                    case.change == DesignTestChange::Removed && entry.result.is_none(),
                    "only a removed test can have absence evidence"
                ),
                Outcome::Problem(_) => ensure!(
                    entry.result.is_none(),
                    "unresolved test cannot have a case result"
                ),
            }
        }
        Ok(result)
    }
}

/// Select the last reported case outcome from this attempt's own verification commands.
fn observed_results<Context>(
    commands: &[crate::checks::CommandCheck<Context>],
) -> BTreeMap<(String, TestItemId), CaseResult>
where
    Context: Clone + Into<Option<crate::checks::model::CheckCommand>>,
{
    let mut observed = BTreeMap::new();
    for command in commands {
        let Some(context) = command.context.clone().into() else {
            continue;
        };
        if !command.selected
            || command.reused_from.is_some()
            || command.started_at_ms.is_none()
            || matches!(
                command.state,
                crate::checks::CheckState::Pending
                    | crate::checks::CheckState::Running
                    | crate::checks::CheckState::Skipped
                    | crate::checks::CheckState::Blocked
            )
            || context.purpose != crate::checks::CheckPurpose::Verify
            || context.category != crate::checks::CheckCategory::Test
        {
            continue;
        }
        match &command.result {
            Some(crate::checks::adapter::AdapterResult::Tests(result)) => {
                for case in &result.cases {
                    if let Some(status) = case.status {
                        observed.insert(
                            (context.definition_id.clone(), case.id.clone()),
                            CaseResult {
                                command_id: command.id.clone(),
                                status,
                            },
                        );
                    }
                }
            }
            Some(
                crate::checks::adapter::AdapterResult::Diagnostics(_)
                | crate::checks::adapter::AdapterResult::Format(_),
            )
            | None => {}
        }
    }
    observed
}

/// Format a retained source path without inventing a line when discovery omitted it.
fn source_location(path: &str, line: Option<u32>) -> String {
    line.map_or_else(|| path.into(), |line| format!("{path}:{line}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checks::adapter::{Discovery, TestItem};
    use serde_json::json;

    /// Bind fixture execution to a complete configuration and an empty, owned verification ledger.
    fn execution(
        requested: Vec<DesignTestFile>,
        scopes: BTreeMap<String, crate::plan::PlanTestScope>,
    ) -> Result<PlannedTestExecution> {
        use crate::checks::*;
        let config = CheckConfig {
            version: 2,
            definitions: vec![CheckDefinition {
                id: "tests".into(),
                name: "tests".into(),
                category: CheckCategory::Test,
                enabled: true,
                auto: true,
                area: "tests".into(),
                trigger: CheckTrigger::AnyChange,
                scope: CheckScope::All,
                directory: ".".into(),
                runner: CheckRunner::Adapter(adapter::AdapterConfig::Script(
                    adapter::ScriptAdapterConfig {
                        command: vec!["unused".into()],
                    },
                )),
                depends_on: Vec::new(),
            }],
        };
        let helpers = BTreeMap::<String, String>::new();
        let snapshot = ConfigSnapshot {
            digest: crate::plan::digest(&serde_json::to_vec(&(&config, &helpers))?),
            config,
            helper_digest: helpers,
        };
        let context = crate::checks::model::CheckRunContext {
            automatic: true,
            trigger: "Plan execution".into(),
            task_id: Some("task".into()),
            snapshot,
            overrides: Default::default(),
            task_overrides: Default::default(),
            changed_files: Vec::new(),
        };
        let mut execution = PlannedTestExecution::new(requested, scopes.clone())?;
        execution.bind_inputs(serde_json::from_value(json!({
            "source_snapshot":"snapshot", "context":context, "test_scopes":scopes,
            "discovery":{"snapshot":context.snapshot,"task_id":context.task_id,"overrides":context.task_overrides},
            "verification":{"source_identity":"source", "record":{"version":crate::storage::state::VERSION,"session_id":"session","workspace":std::env::temp_dir().join("forge-planned-tests"),"generation":0,"requirements":{}}}
        }))?)?;
        Ok(execution)
    }

    /// Build a validated authored inventory containing each coverage obligation.
    fn requested() -> Vec<DesignTestFile> {
        serde_json::from_value(json!([{"file":"tests.rs","cases":[
            {"name":"new_case","change":"new","description":"new coverage"},
            {"name":"modified_case","change":"modified","description":"changed coverage"},
            {"name":"reused_case","change":"reused","description":"regression coverage"},
            {"name":"removed_case","change":"removed","description":"obsolete coverage"}
        ]}]))
        .unwrap()
    }

    /// Supply exact discovered cases with opaque identity and known source ownership.
    fn inventory() -> PlannedInventory {
        PlannedInventory::new(
            "tests".into(),
            Ok(Discovery::new(
                ["new_case", "modified_case", "reused_case"]
                    .into_iter()
                    .map(|name| TestItem {
                        id: name.to_string().try_into().unwrap(),
                        parent: None,
                        label: name.into(),
                        case: true,
                        name: Some(name.into()),
                        path: Some("tests.rs".into()),
                        line: Some(1),
                    })
                    .collect(),
                DiscoverMode::Exact,
            )
            .unwrap()),
        )
        .unwrap()
    }

    /// Build a passing check whose case statuses can still leave the plan unsatisfied.
    fn command(
        status: Option<CaseStatus>,
    ) -> crate::checks::CommandCheck<crate::checks::model::CheckCommand> {
        let cases = ["new_case", "modified_case", "reused_case"].into_iter().map(|id| json!({
            "id":id,"status":status,"duration_ms":0,"failures":[],"item":null,"output":null,"output_truncated_bytes":0
        })).collect::<Vec<_>>();
        let command: crate::checks::CommandCheck = serde_json::from_value(json!({
            "selected":true,"id":"run:0","command":"tests","state":"passed",
            "started_at_ms":1,"completed_at_ms":2,"exit_code":0,"error":null,
            "stdout":"stdout","stderr":"stderr","output":"output","output_bytes":0,"preview":"",
            "context":{"purpose":"verify","definition_id":"tests","name":"tests","area":"tests",
                "reason":"planned","matched_files":[],"category":"test","directory":".",
                "selection":{"kind":"all"},"resolved":null,"depends_on":[]},
            "result":{"category":"tests","selection":{"ids":[],"affected":[]},"items":[],"widened":null,
                "cases":cases,
                "not_found":[],"error":null,"protocol_error":null,"exit":0,"cancelled":false,"unsupported":false,"output_truncated_bytes":0}
        })).unwrap();
        crate::checks::CommandCheck {
            selected: command.selected,
            evidence: command.evidence,
            reused_from: command.reused_from,
            id: command.id,
            command: command.command,
            context: command.context.unwrap(),
            state: command.state,
            started_at_ms: command.started_at_ms,
            completed_at_ms: command.completed_at_ms,
            exit_code: command.exit_code,
            error: command.error,
            stdout: command.stdout,
            stderr: command.stderr,
            output: command.output,
            output_bytes: command.output_bytes,
            preview: command.preview,
            result: command.result,
            adapter_progress: command.adapter_progress,
        }
    }

    /// Restore a complete run so cross-field validation uses the production history boundary.
    fn run_record(
        execution: PlannedTestExecution,
        commands: Vec<crate::checks::CommandCheck<crate::checks::model::CheckCommand>>,
    ) -> serde_json::Value {
        let context = serde_json::from_value(
            serde_json::to_value(&execution).unwrap()["inputs"]["context"].clone(),
        )
        .unwrap();
        let run: crate::checks::CheckRun = crate::checks::CheckRun {
            id: "run".into(),
            owner: Box::new(crate::timeline::TimelineOwner::session("session")),
            revision: 0,
            context: Some(context),
            lifecycle: Vec::new(),
            completed_plan: None,
            shell: Default::default(),
            executable: None,
            workspace: std::env::temp_dir()
                .join("forge-planned-tests")
                .to_string_lossy()
                .into_owned(),
            environment_digest: String::new(),
            started_at_ms: 1,
            completed_at_ms: Some(2),
            cancelled_inputs: None,
            cancelled_run_id: None,
            state: crate::checks::CheckState::Passed,
            progress: Default::default(),
            planned_tests: Some(execution),
            workspace_identity: Some("snapshot".into()),
            workspace_digest: Default::default(),
            commands: commands
                .into_iter()
                .map(crate::checks::CommandCheck::into_record)
                .collect(),
        };
        serde_json::to_value(run).unwrap()
    }

    /// Saved passes must reference this attempt's selected verification result for the same case.
    #[test]
    fn restored_run_rejects_detached_planned_results() {
        let mut execution = execution(requested(), Default::default()).unwrap();
        execution.resolve("snapshot".into(), &[inventory()]);
        let first = command(Some(CaseStatus::Failed));
        let mut last = command(Some(CaseStatus::Passed));
        last.id = "run:1".into();
        let commands = vec![first, last];
        execution.record_results(&commands);
        let source = run_record(execution, commands);
        let restored: crate::checks::CheckRun = serde_json::from_value(source.clone()).unwrap();
        assert_eq!(
            restored.status(),
            crate::plan::execution::VerificationStatus::Passed
        );
        assert_eq!(
            source["planned_tests"]["state"]["entries"][0]["result"]["command_id"],
            "run:1"
        );

        let mutations = [
            (
                "/planned_tests/state/entries/0/result/status",
                json!("failed"),
            ),
            (
                "/planned_tests/state/entries/0/result/command_id",
                json!("run:0"),
            ),
            (
                "/planned_tests/state/entries/0/result/command_id",
                json!(""),
            ),
            ("/planned_tests/state/entries/0/result", json!("passed")),
            ("/commands/1/id", json!("different-command")),
            ("/commands/1/id", json!("run:0")),
            ("/commands/1/selected", json!(false)),
            ("/commands/1/state", json!("skipped")),
            ("/commands/1/state", json!("pending")),
            ("/commands/1/started_at_ms", json!(null)),
            ("/commands/1/context", json!(null)),
            ("/commands/1/context/purpose", json!("prepare")),
            ("/commands/1/context/category", json!("custom")),
            ("/commands/1/context/definition_id", json!("other")),
            (
                "/commands/1/reused_from",
                json!(["older-run", "older-command"]),
            ),
            ("/commands/1/result", json!(null)),
            ("/commands/1/result/cases/0/status", json!("skipped")),
            ("/commands/1/result/cases/0/id", json!("other-case")),
        ];
        for (path, value) in mutations {
            let mut invalid = source.clone();
            *invalid.pointer_mut(path).unwrap() = value;
            assert!(
                serde_json::from_value::<crate::checks::CheckRun>(invalid).is_err(),
                "accepted changed {path}"
            );
        }
        let mut missing = source.clone();
        missing["commands"].as_array_mut().unwrap().pop();
        assert!(serde_json::from_value::<crate::checks::CheckRun>(missing).is_err());
        let mut duplicate = source.clone();
        let case = duplicate["commands"][1]["result"]["cases"][0].clone();
        duplicate["commands"][1]["result"]["cases"]
            .as_array_mut()
            .unwrap()
            .push(case);
        assert!(serde_json::from_value::<crate::checks::CheckRun>(duplicate).is_err());
        let mut invalidated = source.clone();
        invalidated["planned_tests"]["state"]["state"] = json!("invalidated");
        invalidated["planned_tests"]["state"]["reason"] = json!("source changed");
        let restored: crate::checks::CheckRun =
            serde_json::from_value(invalidated.clone()).unwrap();
        assert_eq!(
            restored.status(),
            crate::plan::execution::VerificationStatus::Failed
        );
        invalidated["commands"][1]["result"] = json!(null);
        assert!(serde_json::from_value::<crate::checks::CheckRun>(invalidated).is_err());

        let mut unfinished = source;
        unfinished["planned_tests"]["state"]["entries"][0]["result"] = json!(null);
        let restored: crate::checks::CheckRun = serde_json::from_value(unfinished).unwrap();
        assert_eq!(
            restored.status(),
            crate::plan::execution::VerificationStatus::Failed
        );
    }

    /// Reject partial inventories and inputs attributed to another task, revision, or configuration.
    #[test]
    fn restored_run_requires_complete_consistent_resolution_inputs() {
        let mut planned = execution(requested(), Default::default()).unwrap();
        planned.resolve("snapshot".into(), &[inventory()]);
        let commands = vec![command(Some(CaseStatus::Passed))];
        planned.record_results(&commands);
        let source = run_record(planned, commands);
        let mutations = [
            ("/planned_tests/inputs", json!(null)),
            (
                "/planned_tests/inputs/source_snapshot",
                json!("other-source"),
            ),
            ("/planned_tests/inputs/context/task_id", json!("other-task")),
            (
                "/planned_tests/inputs/context/overrides",
                json!({"tests":false}),
            ),
            (
                "/planned_tests/inputs/discovery/task_id",
                json!("other-task"),
            ),
            (
                "/planned_tests/inputs/discovery/snapshot/digest",
                json!("wrong"),
            ),
            (
                "/planned_tests/inputs/test_scopes",
                json!({"tests":"whole_suite"}),
            ),
            (
                "/planned_tests/inputs/verification/record/session_id",
                json!("other-session"),
            ),
            (
                "/planned_tests/inputs/verification/record/version",
                json!(0),
            ),
            ("/planned_tests/state/inventories", json!([])),
            ("/context/task_id", json!("other-task")),
            ("/context/task_overrides/enabled", json!({"tests":false})),
        ];
        for (path, value) in mutations {
            let mut invalid = source.clone();
            *invalid.pointer_mut(path).unwrap() = value;
            assert!(
                serde_json::from_value::<crate::checks::CheckRun>(invalid).is_err(),
                "accepted changed {path}"
            );
        }
        let mut missing = source;
        missing["planned_tests"]["inputs"]
            .as_object_mut()
            .unwrap()
            .remove("verification");
        assert!(serde_json::from_value::<crate::checks::CheckRun>(missing).is_err());
    }

    /// A passing enclosing check never substitutes for each planned case's passing result.
    #[test]
    fn planned_gate_requires_each_present_case_and_preserves_absence() {
        let mut execution = execution(requested(), Default::default()).unwrap();
        assert!(!execution.satisfied());
        execution.resolve("snapshot".into(), &[inventory()]);
        assert!(execution.resolved());
        assert_eq!(execution.selections()["tests"].ids.len(), 3);
        for status in [
            None,
            Some(CaseStatus::Skipped),
            Some(CaseStatus::Failed),
            Some(CaseStatus::TimedOut),
            Some(CaseStatus::Errored),
        ] {
            execution.record_results(&[command(status)]);
            assert!(!execution.satisfied(), "unexpected pass for {status:?}");
            assert_eq!(execution.findings().len(), 3);
        }
        execution.record_results(&[command(Some(CaseStatus::Passed))]);
        assert!(execution.satisfied());
        assert!(execution.findings().is_empty());
        let restored: PlannedTestExecution =
            serde_json::from_value(serde_json::to_value(&execution).unwrap()).unwrap();
        assert!(restored.satisfied());
        execution.invalidate("source changed".into());
        assert!(!execution.satisfied());
        assert_eq!(execution.findings(), vec!["source changed"]);
    }

    /// Preserve supplementary provenance and verify absence only from the retained exact inventory.
    #[test]
    fn supplementary_sources_survive_restore_without_adding_static_cases() {
        let mut discovered = inventory();
        let InventoryResult::Available(primary) = discovered.discovery() else {
            panic!("fixture has no exact inventory");
        };
        let mut items = primary.items().to_vec();
        items[0].path = None;
        items[0].line = None;
        discovered = PlannedInventory::new(
            "tests".into(),
            Ok(Discovery::new(items, DiscoverMode::Exact).unwrap()),
        )
        .unwrap();
        discovered.supplement(Ok(Discovery::new(
            vec![
                TestItem {
                    id: "new_case".to_owned().try_into().unwrap(),
                    parent: None,
                    label: "new_case".into(),
                    case: true,
                    name: Some("new_case".into()),
                    path: Some("tests.rs".into()),
                    line: Some(2),
                },
                TestItem {
                    id: "removed_case".to_owned().try_into().unwrap(),
                    parent: None,
                    label: "removed_case".into(),
                    case: true,
                    name: Some("removed_case".into()),
                    path: Some("tests.rs".into()),
                    line: Some(3),
                },
            ],
            DiscoverMode::Static,
        )
        .unwrap()));
        let mut execution = execution(requested(), Default::default()).unwrap();
        execution.resolve("snapshot".into(), &[discovered]);
        execution.record_results(&[command(Some(CaseStatus::Passed))]);
        let restored: PlannedTestExecution =
            serde_json::from_slice(&serde_json::to_vec(&execution).unwrap()).unwrap();
        assert!(restored.satisfied());
        assert!(
            restored
                .review_lines()
                .iter()
                .any(|line| line.contains("check tests / new_case; static source tests.rs:2"))
        );
        assert!(
            restored
                .review_lines()
                .iter()
                .any(|line| line.contains("removed_case â€” Removal verified"))
        );
        let mut invalid = serde_json::to_value(&execution).unwrap();
        invalid["state"]["inventories"][0]["supplementary"]["Available"]["items"][0]["line"] =
            json!(8);
        assert!(serde_json::from_value::<PlannedTestExecution>(invalid).is_err());
    }

    /// Empty inventories need no tests, while removal requires a participating exact inventory.
    #[test]
    fn empty_and_removal_only_inventories_have_distinct_gates() {
        let mut empty = execution(Vec::new(), Default::default()).unwrap();
        empty.resolve("snapshot".into(), &[]);
        assert!(empty.satisfied());
        assert!(empty.selections().is_empty());
        let mut requested = requested();
        requested[0]
            .cases
            .retain(|case| case.change == DesignTestChange::Removed);
        let mut removed = execution(requested, Default::default()).unwrap();
        removed.resolve("snapshot".into(), &[]);
        assert!(!removed.satisfied());
        removed.resolve("snapshot".into(), &[inventory()]);
        assert!(removed.satisfied());
        assert!(removed.selections().is_empty());
    }

    /// Restore rejects changed identities, fabricated absence, missing entries, and duplicate coverage.
    #[test]
    fn restored_resolution_validates_its_authored_inventory() {
        let mut pending = execution(requested(), Default::default()).unwrap();
        pending.invalidate("source changed before discovery".into());
        let pending: PlannedTestExecution =
            serde_json::from_value(serde_json::to_value(pending).unwrap()).unwrap();
        assert!(!pending.satisfied());
        let mut execution = execution(requested(), Default::default()).unwrap();
        execution.resolve("snapshot".into(), &[inventory()]);
        let source = serde_json::to_value(execution).unwrap();
        let mut missing_scopes = source.clone();
        missing_scopes
            .as_object_mut()
            .unwrap()
            .remove("test_scopes");
        assert!(serde_json::from_value::<PlannedTestExecution>(missing_scopes).is_err());
        let mut missing = source.clone();
        missing["state"]["entries"].as_array_mut().unwrap().pop();
        assert!(serde_json::from_value::<PlannedTestExecution>(missing).is_err());
        let mut absent = source.clone();
        absent["state"]["entries"][0]["resolution"]["outcome"] = json!("Absent");
        assert!(serde_json::from_value::<PlannedTestExecution>(absent).is_err());
        let mut duplicate = source.clone();
        duplicate["state"]["entries"][1]["resolution"]["outcome"] =
            source["state"]["entries"][0]["resolution"]["outcome"].clone();
        assert!(serde_json::from_value::<PlannedTestExecution>(duplicate).is_err());
        let mut missing_inventories = source.clone();
        missing_inventories["state"]
            .as_object_mut()
            .unwrap()
            .remove("inventories");
        assert!(serde_json::from_value::<PlannedTestExecution>(missing_inventories).is_err());
        let mut duplicate_inventory = source.clone();
        let inventory = duplicate_inventory["state"]["inventories"][0].clone();
        duplicate_inventory["state"]["inventories"]
            .as_array_mut()
            .unwrap()
            .push(inventory);
        assert!(serde_json::from_value::<PlannedTestExecution>(duplicate_inventory).is_err());
        let mut changed_location = source.clone();
        changed_location["state"]["entries"][0]["resolution"]["outcome"]["Resolved"]["source"]["Exact"]
            ["path"] = json!("wrong.rs");
        assert!(serde_json::from_value::<PlannedTestExecution>(changed_location).is_err());
        let mut static_primary = source.clone();
        static_primary["state"]["inventories"][0]["discovery"]["Available"]["mode"] =
            json!("static");
        assert!(serde_json::from_value::<PlannedTestExecution>(static_primary).is_err());
        let mut removed_exists = source.clone();
        removed_exists["state"]["inventories"][0]["discovery"]["Available"]["items"].as_array_mut().unwrap().push(json!({"id":"removed_case","name":"removed_case","label":"removed","case":true,"path":"tests.rs","line":1}));
        assert!(serde_json::from_value::<PlannedTestExecution>(removed_exists).is_err());
        let mut identity = source;
        identity["state"]["entries"][0]["resolution"]["name"] = json!("other");
        assert!(serde_json::from_value::<PlannedTestExecution>(identity).is_err());
    }

    /// Whole-suite choices survive restoration and cannot excuse a skipped planned case.
    #[test]
    fn whole_suite_keeps_planned_obligations_and_binds_reuse() {
        let scopes = BTreeMap::from([("tests".into(), crate::plan::PlanTestScope::WholeSuite)]);
        let mut execution = execution(requested(), scopes.clone()).unwrap();
        assert!(execution.matches_request(&requested(), &scopes));
        assert!(!execution.matches_request(&requested(), &BTreeMap::new()));
        assert_eq!(
            execution.scope("tests"),
            crate::plan::PlanTestScope::WholeSuite
        );
        assert_eq!(
            execution.scope("other"),
            crate::plan::PlanTestScope::Planned
        );
        execution.resolve("snapshot".into(), &[inventory()]);
        execution.record_results(&[command(Some(CaseStatus::Skipped))]);
        assert!(!execution.satisfied());
        execution.record_results(&[command(Some(CaseStatus::Passed))]);
        assert!(execution.satisfied());
        let restored: PlannedTestExecution =
            serde_json::from_value(serde_json::to_value(execution).unwrap()).unwrap();
        assert!(restored.matches_request(&requested(), &scopes));
        assert!(restored.satisfied());
    }
}
