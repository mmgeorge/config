use super::{CheckDefinition, CheckPurpose, CheckRun, CheckSelection, CheckState, CommandCheck};
use crate::checkpoint::CheckpointRecord;
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    io::Write,
    path::{Path, PathBuf},
};

const VERSION: u32 = 5;

/// Identify the executable contract independently of selection and presentation policy.
#[derive(PartialEq)]
struct VerificationIdentity<'a> {
    id: &'a str,
    category: super::CheckCategory,
    directory: &'a str,
    runner: &'a super::CheckRunner,
    depends_on: &'a [String],
}

impl<'a> From<&'a CheckDefinition> for VerificationIdentity<'a> {
    /// Keep every execution field while explicitly excluding selection and display fields.
    fn from(definition: &'a CheckDefinition) -> Self {
        let CheckDefinition {
            id,
            name: _,
            category,
            enabled: _,
            auto: _,
            area: _,
            trigger: _,
            scope: _,
            directory,
            runner,
            depends_on,
        } = definition;
        Self {
            id,
            category: *category,
            directory,
            runner,
            depends_on,
        }
    }
}

/// Retain mutation obligations independently of prunable command history.
pub(super) struct VerificationRequirements {
    directory: PathBuf,
    record: RequirementsRecord,
}

/// Bind verification to the obligations present before its source snapshot.
pub(super) struct VerificationCapture {
    directory: PathBuf,
    generation: u64,
}

/// Retain the prior verification state until launch or proven nonmutation retires it.
pub(super) struct MutationPreparation(MutationReceipt);

/// Prove the ledger crossed its durable launch boundary before process execution.
pub(super) struct MutationLaunch(MutationReceipt);

struct MutationReceipt {
    directory: PathBuf,
    command: String,
    generation: u64,
    previous: BTreeMap<String, RequirementState>,
}

/// Describe a mutation whose whole-check verification remains outstanding.
#[derive(Clone, Debug, Serialize)]
pub(super) struct PendingVerification {
    pub mutation_command: String,
    pub check: String,
    pub task_id: Option<String>,
    pub definition: CheckDefinition,
    pub requested: CheckSelection,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct RequirementsRecord {
    version: u32,
    session_id: String,
    workspace: PathBuf,
    generation: u64,
    requirements: BTreeMap<String, Requirement>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct Requirement {
    generation: u64,
    task_id: Option<String>,
    definition: CheckDefinition,
    helper_digest: BTreeMap<String, String>,
    requested: CheckSelection,
    state: RequirementState,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case", deny_unknown_fields)]
enum RequirementState {
    Prepared,
    Pending,
    Satisfied {
        command: String,
        source_identity: String,
    },
}

/// Freeze the ledger and source identity from which planned verification obligations were derived.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "RawResolutionRequirements")]
pub(super) struct ResolutionRequirements {
    record: RequirementsRecord,
    source_identity: String,
}

impl ResolutionRequirements {
    /// Reject another mutation generation while permitting this run to satisfy captured obligations.
    pub(super) fn validate(&self, current: &VerificationRequirements) -> Result<()> {
        let actual = &current.record;
        ensure!(
            self.record.version == actual.version
                && self.record.session_id == actual.session_id
                && self.record.workspace == actual.workspace
                && self.record.generation == actual.generation
                && self.record.requirements.len() == actual.requirements.len()
                && self.record.requirements.iter().all(|(command, expected)| {
                    actual.requirements.get(command).is_some_and(|actual| {
                        expected.generation == actual.generation
                            && expected.task_id == actual.task_id
                            && expected.definition == actual.definition
                            && expected.helper_digest == actual.helper_digest
                            && expected.requested == actual.requested
                    })
                }),
            "verification requirements changed during planned test resolution; run Check again"
        );
        Ok(())
    }

    /// Derive the captured pending set without consulting mutable ledger state.
    pub(super) fn pending(&self, config: &super::ConfigSnapshot) -> Vec<PendingVerification> {
        self.record
            .pending(Some(&self.source_identity), Some(config))
    }

    /// Require retained obligations to belong to the run's session and workspace.
    pub(super) fn validate_owner(
        &self,
        owner: &crate::timeline::TimelineOwner,
        workspace: &str,
    ) -> Result<()> {
        ensure!(
            self.record.session_id == owner.session_id()
                && dunce::simplified(&self.record.workspace)
                    == dunce::simplified(Path::new(workspace)),
            "planned verification inputs belong to another session or workspace"
        );
        Ok(())
    }
}

/// Validate restored ledger snapshots independently of live filesystem access.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawResolutionRequirements {
    record: RequirementsRecord,
    source_identity: String,
}

impl TryFrom<RawResolutionRequirements> for ResolutionRequirements {
    type Error = anyhow::Error;

    /// Reject malformed obligations before they can alter planned execution selection.
    fn try_from(raw: RawResolutionRequirements) -> Result<Self> {
        let RawResolutionRequirements {
            record,
            source_identity,
        } = raw;
        record.validate()?;
        ensure!(
            !source_identity.is_empty(),
            "planned verification source identity is empty"
        );
        Ok(Self {
            record,
            source_identity,
        })
    }
}

impl RequirementsRecord {
    /// Validate the ledger's durable identities and supported mutation obligations.
    fn validate(&self) -> Result<()> {
        ensure!(
            self.version == VERSION,
            "unsupported verification ledger version {}",
            self.version
        );
        ensure!(
            !self.session_id.is_empty() && self.workspace.is_absolute(),
            "verification ledger owner is invalid"
        );
        for (command, requirement) in &self.requirements {
            ensure!(
                !command.is_empty() && !requirement.definition.id.is_empty(),
                "verification requirement identity is empty"
            );
            ensure!(
                requirement.generation > 0 && requirement.generation <= self.generation,
                "invalid verification requirement generation"
            );
            ensure!(
                matches!(
                    requirement.definition.category,
                    super::CheckCategory::Format | super::CheckCategory::Lint
                ),
                "verification requirement is not a fixable category"
            );
            let mut selection = requirement.requested.clone();
            selection.normalize()?;
            if let RequirementState::Satisfied {
                command,
                source_identity,
            } = &requirement.state
            {
                ensure!(
                    !command.is_empty() && !source_identity.is_empty(),
                    "verification satisfaction identity is empty"
                );
            }
        }
        Ok(())
    }

    /// Derive pending obligations from source and optional configuration identities.
    fn pending(
        &self,
        identity: Option<&str>,
        config: Option<&super::ConfigSnapshot>,
    ) -> Vec<PendingVerification> {
        self.requirements
            .iter()
            .filter(|(_, requirement)| {
                let satisfied = match &requirement.state {
                    RequirementState::Prepared | RequirementState::Pending => false,
                    RequirementState::Satisfied {
                        source_identity, ..
                    } => identity == Some(source_identity.as_str()),
                };
                !satisfied
                    || config.is_some_and(|config| {
                        !config.config.definitions.iter().any(|definition| {
                            definition.enabled
                                && VerificationIdentity::from(definition)
                                    == VerificationIdentity::from(&requirement.definition)
                        }) || config.helper_digest != requirement.helper_digest
                    })
            })
            .map(|(command, requirement)| PendingVerification {
                mutation_command: command.clone(),
                check: requirement.definition.id.clone(),
                task_id: requirement.task_id.clone(),
                definition: requirement.definition.clone(),
                requested: requirement.requested.clone(),
            })
            .collect()
    }
}

impl VerificationRequirements {
    /// Capture every obligation before planned discovery under the same source snapshot.
    pub(super) fn resolution_input(
        &self,
        snapshot: &CheckpointRecord,
    ) -> Result<ResolutionRequirements> {
        ensure!(
            Path::new(&snapshot.workspace).canonicalize()? == self.record.workspace,
            "verification snapshot belongs to another workspace"
        );
        Ok(ResolutionRequirements {
            record: self.record.clone(),
            source_identity: snapshot.check_identity(None, &[])?,
        })
    }

    /// Check whether completion must capture source before bypassing automatic checks.
    pub(super) fn has_requirements(&self) -> bool {
        !self.record.requirements.is_empty()
    }

    /// Add mandatory whole-check coverage after validating every originating definition.
    pub(super) fn require_in_run(
        &self,
        run: &mut super::PreparedCheckRun,
        snapshot: &CheckpointRecord,
    ) -> Result<()> {
        // Validate the complete required set before changing any selected command.
        let pending = match run
            .planned_tests
            .as_ref()
            .and_then(super::PlannedTestExecution::inputs)
        {
            Some(inputs) => {
                inputs.verification().validate(self)?;
                inputs.verification().pending(&run.context.snapshot)
            }
            None => self.pending_for_config(snapshot, &run.context.snapshot)?,
        };
        let enabled = if run.planned_tests.is_some() {
            &run.context.task_overrides.enabled
        } else {
            &run.context.overrides
        };
        let mut indices = std::collections::BTreeSet::new();
        for requirement in &pending {
            let original = &self.record.requirements[&requirement.mutation_command];
            let definition = run.context.snapshot.config.definitions.iter()
                .find(|definition| definition.id == requirement.check)
                .with_context(|| format!("Required verification {} is missing; restore the definition used by mutation {}", requirement.check, requirement.mutation_command))?;
            ensure!(
                VerificationIdentity::from(definition)
                    == VerificationIdentity::from(&original.definition)
                    && run.context.snapshot.helper_digest == original.helper_digest,
                "Required verification {} changed; restore the definition and helpers used by mutation {}",
                requirement.check,
                requirement.mutation_command
            );
            ensure!(
                definition.enabled && enabled.get(&requirement.check) != Some(&false),
                "Required verification {} is disabled; re-enable it to verify mutation {}",
                requirement.check,
                requirement.mutation_command
            );
            let index = run
                .commands
                .iter()
                .position(|command| command.context.definition_id == requirement.check)
                .context("required verification has no prepared command")?;
            ensure!(
                run.commands[index].context.purpose == CheckPurpose::Verify,
                "Required verification {} is not read-only",
                requirement.check
            );
            indices.insert(index);
        }
        let mut dependencies: Vec<_> = indices.iter().copied().collect();
        while let Some(index) = dependencies.pop() {
            for dependency in &run.commands[index].context.depends_on {
                let dependency_index = run
                    .commands
                    .iter()
                    .position(|command| &command.context.definition_id == dependency)
                    .with_context(|| {
                        format!("Required verification prerequisite {dependency} is missing")
                    })?;
                let definition = run
                    .context
                    .snapshot
                    .config
                    .definitions
                    .iter()
                    .find(|definition| &definition.id == dependency)
                    .context("required prerequisite has no definition")?;
                ensure!(
                    definition.enabled && enabled.get(dependency) != Some(&false),
                    "Required verification prerequisite {dependency} is disabled"
                );
                ensure!(
                    run.commands[dependency_index].context.purpose == CheckPurpose::Verify,
                    "Required verification prerequisite {dependency} changes source and cannot run automatically"
                );
                if indices.insert(dependency_index) {
                    dependencies.push(dependency_index);
                }
            }
        }
        // Promote only the commands whose definitions and dependencies passed preflight.
        for index in indices {
            let command = &mut run.commands[index];
            command.selected = true;
            command.state = CheckState::Pending;
            command.error = None;
            command.evidence = None;
            command.reused_from = None;
            command.result = None;
            command.adapter_progress = None;
            command.started_at_ms = None;
            command.completed_at_ms = None;
            command.exit_code = None;
            command.context.selection = CheckSelection::All;
            command.context.resolved = None;
            command.context.invocation = None;
            command.context.reason =
                "Required whole-check verification after source mutation".into();
        }
        Ok(())
    }

    /// Open this session's ledger, rejecting damaged or mismatched state explicitly.
    pub(super) fn open(root: &Path, workspace: &Path, session_id: &str) -> Result<Self> {
        let directory = root.join("verification");
        let workspace = workspace
            .canonicalize()
            .context("resolve verification workspace")?;
        if !directory.exists() {
            std::fs::create_dir(&directory).context("create verification ledger directory")?;
            let store = Self {
                directory,
                record: RequirementsRecord {
                    version: VERSION,
                    session_id: session_id.to_owned(),
                    workspace,
                    generation: 0,
                    requirements: BTreeMap::new(),
                },
            };
            store.write(&store.record)?;
            return Ok(store);
        }
        let path = directory.join("requirements.json");
        let source = std::fs::read(&path)
            .with_context(|| format!("read verification ledger {}", path.display()))?;
        let record: RequirementsRecord =
            serde_json::from_slice(&source).context("decode verification ledger")?;
        ensure!(
            record.version == VERSION,
            "unsupported verification ledger version {}",
            record.version
        );
        ensure!(
            record.session_id == session_id && record.workspace == workspace,
            "verification ledger belongs to another session or workspace"
        );
        record.validate()?;
        Ok(Self { directory, record })
    }

    /// Capture the generation before acquiring the read-only source snapshot.
    pub(super) fn capture(&self) -> VerificationCapture {
        VerificationCapture {
            directory: self.directory.clone(),
            generation: self.record.generation,
        }
    }

    /// Persist an adapter fix's whole-check obligation before process launch.
    pub(super) fn prepare(
        &mut self,
        run: &CheckRun,
        command: &CommandCheck,
    ) -> Result<MutationPreparation> {
        let context = command
            .context
            .as_ref()
            .context("mutation has no command context")?;
        let run_context = run
            .context
            .as_ref()
            .context("mutation has no run context")?;
        ensure!(
            context.purpose == CheckPurpose::Prepare,
            "verification requirement needs a mutation"
        );
        let definition = run_context
            .snapshot
            .config
            .definitions
            .iter()
            .find(|definition| definition.id == context.definition_id)
            .context("mutation has no captured definition")?;
        ensure!(
            matches!(definition.runner, super::CheckRunner::Adapter(_))
                && matches!(
                    definition.category,
                    super::CheckCategory::Format | super::CheckCategory::Lint
                ),
            "only format and lint adapter fixes create verification requirements"
        );
        ensure!(
            run.owner.session_id() == self.record.session_id,
            "mutation belongs to another session"
        );
        ensure!(
            !self.record.requirements.contains_key(&command.id),
            "mutation already has a verification requirement"
        );
        let previous = self
            .record
            .requirements
            .iter()
            .map(|(command, requirement)| (command.clone(), requirement.state.clone()))
            .collect();
        let mut next = self.record.clone();
        next.generation = next
            .generation
            .checked_add(1)
            .context("verification generation exhausted")?;
        // A later mutation invalidates earlier verification, even for another check.
        for requirement in next.requirements.values_mut() {
            requirement.state = RequirementState::Prepared;
        }
        next.requirements.insert(
            command.id.clone(),
            Requirement {
                generation: next.generation,
                task_id: run_context.task_id.clone(),
                definition: definition.clone(),
                helper_digest: run_context.snapshot.helper_digest.clone(),
                requested: context.selection.clone(),
                state: RequirementState::Prepared,
            },
        );
        self.write(&next)?;
        self.record = next;
        Ok(MutationPreparation(MutationReceipt {
            directory: self.directory.clone(),
            command: command.id.clone(),
            generation: self.record.generation,
            previous,
        }))
    }

    /// Persist that launch may occur before handing execution a launch receipt.
    pub(super) fn launch(&mut self, preparation: MutationPreparation) -> Result<MutationLaunch> {
        let MutationPreparation(receipt) = preparation;
        self.receipt_validate(&receipt)?;
        let mut next = self.record.clone();
        next.requirements
            .get_mut(&receipt.command)
            .context("mutation requirement missing")?
            .state = RequirementState::Pending;
        self.write(&next)?;
        self.record = next;
        Ok(MutationLaunch(receipt))
    }

    /// Retire preparation only while its caller still owns the unlaunched handle.
    pub(super) fn retire_unlaunched(&mut self, preparation: MutationPreparation) -> Result<()> {
        self.retire(preparation.0)
    }

    /// Retire a collected unsupported operation only with unchanged source and valid output.
    pub(super) fn retire_unsupported(
        &mut self,
        launch: MutationLaunch,
        command: &CommandCheck,
        before: &CheckpointRecord,
        after: &CheckpointRecord,
    ) -> Result<()> {
        use super::adapter::AdapterResult;
        let unsupported = match &command.result {
            Some(AdapterResult::Format(result)) => {
                result.unsupported
                    && !result.cancelled
                    && result.error.is_none()
                    && result.protocol_error.is_none()
                    && result.files.is_empty()
                    && result.exit.is_none_or(|code| code == 0)
            }
            Some(AdapterResult::Diagnostics(result)) => {
                result.unsupported
                    && !result.cancelled
                    && result.error.is_none()
                    && result.protocol_error.is_none()
                    && result.targets.is_empty()
                    && result.diagnostics.is_empty()
                    && result.artifacts.is_empty()
                    && result.exit.is_none_or(|code| code == 0)
            }
            Some(AdapterResult::Tests(_)) | None => false,
        };
        if !unsupported
            || command.id != launch.0.command
            || Path::new(&before.workspace).canonicalize()? != self.record.workspace
            || Path::new(&after.workspace).canonicalize()? != self.record.workspace
            || before.check_identity(None, &[])? != after.check_identity(None, &[])?
        {
            return Ok(());
        }
        self.retire(launch.0)
    }

    /// Reject receipts from another ledger or superseded mutation generation.
    fn receipt_validate(&self, receipt: &MutationReceipt) -> Result<()> {
        ensure!(
            receipt.directory == self.directory && receipt.generation == self.record.generation,
            "mutation receipt belongs to another ledger or generation"
        );
        ensure!(
            self.record
                .requirements
                .get(&receipt.command)
                .is_some_and(|requirement| requirement.generation == receipt.generation),
            "mutation receipt has no requirement"
        );
        Ok(())
    }

    /// Atomically remove a proven nonmutation and restore earlier verification states.
    fn retire(&mut self, receipt: MutationReceipt) -> Result<()> {
        self.receipt_validate(&receipt)?;
        let mut next = self.record.clone();
        next.requirements.remove(&receipt.command);
        for (command, state) in receipt.previous {
            if let Some(requirement) = next.requirements.get_mut(&command)
                && matches!(requirement.state, RequirementState::Prepared)
            {
                requirement.state = state;
            }
        }
        self.write(&next)?;
        self.record = next;
        Ok(())
    }

    /// Satisfy only earlier obligations covered by a durably recorded whole-check pass.
    pub(super) fn observe(
        &mut self,
        capture: VerificationCapture,
        run: &CheckRun,
        command: &CommandCheck,
        before: &CheckpointRecord,
        after: &CheckpointRecord,
    ) -> Result<()> {
        ensure!(
            capture.directory == self.directory,
            "verification capture belongs to another ledger"
        );
        let context = command
            .context
            .as_ref()
            .context("verification has no command context")?;
        let run_context = run
            .context
            .as_ref()
            .context("verification has no run context")?;
        if command.state != CheckState::Passed
            || context.purpose != CheckPurpose::Verify
            || command.reused_from.is_some()
            || !whole_selection(&context.selection)
            || !before.changed_paths(after)?.is_empty()
        {
            return Ok(());
        }
        ensure!(
            run.owner.session_id() == self.record.session_id,
            "verification belongs to another session"
        );
        ensure!(
            Path::new(&before.workspace).canonicalize()? == self.record.workspace
                && Path::new(&after.workspace).canonicalize()? == self.record.workspace,
            "verification snapshot belongs to another workspace"
        );
        let definition = run_context
            .snapshot
            .config
            .definitions
            .iter()
            .find(|definition| definition.id == context.definition_id)
            .context("verification has no captured definition")?;
        let enabled = if run.planned_tests.is_some() {
            &run_context.task_overrides.enabled
        } else {
            &run_context.overrides
        };
        if !definition.enabled || enabled.get(&definition.id) == Some(&false) {
            return Ok(());
        }
        let source_identity = after.check_identity(None, &[])?;
        let mut next = self.record.clone();
        let mut changed = false;
        for requirement in next.requirements.values_mut() {
            if requirement.generation <= capture.generation
                && VerificationIdentity::from(&requirement.definition)
                    == VerificationIdentity::from(definition)
                && requirement.helper_digest == run_context.snapshot.helper_digest
            {
                requirement.state = RequirementState::Satisfied {
                    command: command.id.clone(),
                    source_identity: source_identity.clone(),
                };
                changed = true;
            }
        }
        if changed {
            self.write(&next)?;
            self.record = next;
        }
        Ok(())
    }

    /// Get unresolved obligations, treating stale satisfied snapshots as unresolved.
    pub(super) fn pending(
        &self,
        snapshot: Option<&CheckpointRecord>,
    ) -> Result<Vec<PendingVerification>> {
        if let Some(snapshot) = snapshot {
            ensure!(
                Path::new(&snapshot.workspace).canonicalize()? == self.record.workspace,
                "verification snapshot belongs to another workspace"
            );
        }
        let identity = snapshot
            .map(|snapshot| snapshot.check_identity(None, &[]))
            .transpose()?;
        Ok(self.record.pending(identity.as_deref(), None))
    }

    /// Keep changed or removed definitions unresolved even when source paths are excluded.
    pub(super) fn pending_for_config(
        &self,
        snapshot: &CheckpointRecord,
        config: &super::ConfigSnapshot,
    ) -> Result<Vec<PendingVerification>> {
        ensure!(
            Path::new(&snapshot.workspace).canonicalize()? == self.record.workspace,
            "verification snapshot belongs to another workspace"
        );
        let identity = snapshot.check_identity(None, &[])?;
        Ok(self.record.pending(Some(&identity), Some(config)))
    }

    /// Atomically replace the ledger only after its bytes have reached the file.
    fn write(&self, record: &RequirementsRecord) -> Result<()> {
        let mut temporary = tempfile::NamedTempFile::new_in(&self.directory)
            .context("create verification ledger replacement")?;
        temporary.write_all(&serde_json::to_vec(record)?)?;
        temporary.as_file().sync_all()?;
        temporary
            .persist(self.directory.join("requirements.json"))
            .context("publish verification ledger")?;
        Ok(())
    }
}

/// Recognize whole-check selection without interpreting adapter-owned path or test IDs.
fn whole_selection(selection: &CheckSelection) -> bool {
    match selection {
        CheckSelection::All => true,
        CheckSelection::Paths { paths } => paths.paths.is_empty(),
        CheckSelection::Inputs { .. }
        | CheckSelection::Files { .. }
        | CheckSelection::Tests { .. } => false,
    }
}
