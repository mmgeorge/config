use super::HarnessBroker;
use crate::checkpoint::GitCheckpoint;
use crate::exchange::{Exchange, ExchangeNode};
use crate::goal::{GoalRecord, GoalState};
use crate::plan::execution::SemanticProgress;
use crate::plan::{PlanExecutionPromptKind, PlanExecutionRecord, PlanExecutionState, PlanState};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;

/// Explain the reduced progress evidence when no repository baseline exists.
pub(super) const CHECKPOINT_WARNING: &str =
    "Workspace-wide deviation detection is unavailable without a repository checkpoint.";

pub(super) struct ExecutionCapture {
    pub(super) permit: crate::task::TaskPermit,
    pub(super) record: PlanExecutionRecord,
}

/// Fence control against ownership/finalization changes, not streamed presentation updates.
#[derive(Clone, PartialEq)]
struct ExecutionControlExchange {
    id: crate::provider::ExchangeId,
    phase: crate::exchange::ExchangePhase,
    finalization_error: Option<String>,
}

impl ExecutionControlExchange {
    fn capture(exchange: &Exchange) -> Self {
        Self {
            id: exchange.id.as_str().into(),
            phase: exchange.phase.clone(),
            finalization_error: exchange.finalization_error.clone(),
        }
    }
}

/// Immutable authority and workspace evidence for a single control operation.
pub(crate) struct ExecutionControlJob {
    execution_id: String,
    generation: u64,
    permit: crate::task::TaskPermit,
    operation: Box<crate::plan::execution::ExecutionOperation>,
    exchange: ExecutionControlExchange,
    execution_digest: String,
    plan_digest: String,
    evidence: Option<(
        super::checks::ExecutionCheckEvidenceJob,
        ExecutionConformanceJob,
    )>,
}

/// Completed control evidence awaiting revision-checked broker application.
pub(crate) struct ExecutionControlEvidence {
    job: ExecutionControlJob,
    checks_current: bool,
    progress: SemanticProgress,
}

/// A proposed transition held while its implementation report is captured.
pub(super) struct ExecutionControlProposal {
    execution_id: String,
    permit: crate::task::TaskPermit,
    exchange: ExecutionControlExchange,
    execution_digest: String,
    plan_digest: String,
    execution: PlanExecutionRecord,
    plan: crate::plan::PlanRecord,
    nodes: Vec<ExchangeNode>,
    revised_manual: Vec<String>,
    title: String,
    phase_completion: bool,
    checks_current: bool,
    now_ms: i64,
}

/// The synchronous phase either completes or yields an owned report capture.
pub(super) enum ExecutionControlStage {
    Complete(Value),
    Report(Box<ExecutionControlReportJob>),
}

/// Capture the report independently while retaining its uncommitted proposal.
pub(crate) struct ExecutionControlReportJob {
    proposal: Box<ExecutionControlProposal>,
    report: ImplementationReportJob,
}

pub(crate) struct ExecutionControlReport {
    proposal: Box<ExecutionControlProposal>,
    report: ImplementationReportResult,
}

impl ExecutionControlReportJob {
    pub(super) async fn run(self) -> Result<ExecutionControlReport> {
        Ok(ExecutionControlReport {
            proposal: self.proposal,
            report: self.report.run().await?,
        })
    }
}

impl ExecutionControlJob {
    /// Collect phase evidence without holding a broker guard.
    pub(super) async fn run(mut self) -> Result<ExecutionControlEvidence> {
        let (checks_current, progress) = match self.evidence.take() {
            Some((checks, conformance)) => (checks.run().await?, conformance.run().await?),
            None => (false, SemanticProgress::default()),
        };
        Ok(ExecutionControlEvidence {
            job: self,
            checks_current,
            progress,
        })
    }
}

/// Own the immutable inputs needed to scan one execution without a broker borrow.
pub(crate) struct ExecutionScanJob {
    permit: crate::task::TaskPermit,
    execution: PlanExecutionRecord,
    design: crate::plan::DeclarationDesign,
    workspace: PathBuf,
    cancelled: std::sync::Arc<std::sync::atomic::AtomicBool>,
}

/// Retain the task permit alongside a completed scan until its broker commit.
pub(crate) struct ExecutionScanResult {
    permit: crate::task::TaskPermit,
    execution: PlanExecutionRecord,
    progress: SemanticProgress,
}

/// Own workspace checkpoint inputs for an unlocked capture.
pub(super) struct WorkspaceCaptureJob {
    workspace: PathBuf,
    data_root: PathBuf,
    objects: crate::storage::objects::ObjectStore,
    repositories: std::sync::Arc<forge_git::store::RepositoryStore>,
    session_id: String,
    captured_at_ms: i64,
}

impl WorkspaceCaptureJob {
    /// Capture repository or directory state without borrowing the broker.
    pub(super) async fn run(self) -> Result<crate::checkpoint::CheckpointRecord> {
        let Self {
            workspace,
            data_root,
            objects,
            repositories,
            session_id,
            captured_at_ms,
        } = self;
        if repositories.open(workspace.clone()).await?.is_some() {
            return GitCheckpoint::new(workspace.clone())
                .capture(&objects, &repositories, &session_id, captured_at_ms)
                .await;
        }
        tokio::task::spawn_blocking(move || {
            crate::checkpoint::capture_directory(
                &workspace,
                &data_root,
                &objects,
                session_id,
                captured_at_ms,
            )
        })
        .await?
    }
}

/// Own the accepted design and baseline needed for an unlocked conformance scan.
pub(super) struct ExecutionConformanceJob {
    design: crate::plan::DeclarationDesign,
    initial: Option<crate::checkpoint::CheckpointRecord>,
    capture: WorkspaceCaptureJob,
}

/// Own the immutable inputs needed to capture an implementation report.
pub(super) struct ImplementationReportJob {
    original: crate::plan::DeclarationDesign,
    initial: Option<crate::checkpoint::CheckpointRecord>,
    capture: WorkspaceCaptureJob,
    report: crate::plan::implementation_report::ImplementationReport,
}

/// Retain a captured report and the source digests needed at commit.
pub(super) struct ImplementationReportResult {
    report: crate::plan::implementation_report::ImplementationReport,
    digests: std::collections::BTreeMap<String, String>,
}

impl ImplementationReportJob {
    /// Capture the report and source identities without borrowing broker state.
    pub(super) async fn run(self) -> Result<ImplementationReportResult> {
        let Self {
            original,
            initial,
            capture,
            report,
        } = self;
        let workspace = capture.workspace.clone();
        let objects = capture.objects.clone();
        let current = if initial.is_some() {
            Some(capture.run().await?)
        } else {
            None
        };
        let (report, digests) = tokio::task::spawn_blocking(move || {
            report.capture(
                &original,
                initial.as_ref(),
                current.as_ref(),
                &objects,
                &workspace,
            )
        })
        .await??;
        Ok(ImplementationReportResult { report, digests })
    }
}

impl ExecutionConformanceJob {
    /// Compare the accepted design and captured workspace without borrowing broker state.
    pub(super) async fn run(self) -> Result<SemanticProgress> {
        let Self {
            design,
            initial,
            capture,
        } = self;
        let workspace = capture.workspace.clone();
        let objects = capture.objects.clone();
        let accepted_paths = design
            .baseline
            .keys()
            .chain(design.proposed.keys())
            .cloned()
            .collect::<std::collections::BTreeSet<_>>();
        let inspected_workspace = workspace.clone();
        let mut progress = tokio::task::spawn_blocking(move || {
            SemanticProgress::scan(&design, &inspected_workspace)
        })
        .await??;
        if let Some(initial) = initial {
            let current = capture.run().await?;
            for path in initial.changed_paths(&current)? {
                let path = path.as_str();
                if !forge_diff::syntax::DeclarationOverview::supports(path)
                    || accepted_paths.contains(path)
                {
                    continue;
                }
                let expected = initial.read(&objects, path)?;
                let source = crate::plan::workspace_source(&workspace, path)?;
                progress.source_digest.insert(
                    path.to_owned(),
                    source.as_deref().map_or_else(
                        || "absent".into(),
                        |text| crate::plan::digest(text.as_bytes()),
                    ),
                );
                let expected = expected.map(String::from_utf8).transpose()?;
                match (expected, source) {
                    (Some(expected), Some(source)) => {
                        let overview =
                            forge_diff::syntax::DeclarationOverview::extract(path, &expected)
                                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                        match crate::plan::conformance::inspect_in_workspace(
                            &workspace, path, &overview, &source,
                        ) {
                            Ok((differences, changes)) => {
                                progress.different.extend(
                                    differences
                                        .into_iter()
                                        .map(|item| format!("{path}: {item}")),
                                );
                                progress.declaration_changes.extend(changes);
                            }
                            Err(error) => progress.unverified.push(format!("{path}: {error:#}")),
                        }
                    }
                    (None, Some(source))
                        if forge_diff::syntax::ConfigurationFormat::for_path(path).is_none() =>
                    {
                        match crate::plan::conformance::inspect_in_workspace(
                            &workspace, path, "", &source,
                        ) {
                            Ok((differences, changes)) => {
                                progress.different.extend(
                                    differences
                                        .into_iter()
                                        .map(|item| format!("{path}: {item}")),
                                );
                                progress.declaration_changes.extend(changes);
                            }
                            Err(error) => progress.unverified.push(format!("{path}: {error:#}")),
                        }
                    }
                    (Some(_), None) => progress
                        .different
                        .push(format!("{path}: unplanned deletion")),
                    _ => progress
                        .warning
                        .push(format!("{path}: additional configuration artifact")),
                }
            }
        }
        Ok(progress)
    }
}

impl ExecutionScanJob {
    /// Scan immutable inputs without holding the broker or its task-store locks.
    pub(super) async fn run(self) -> Result<ExecutionScanResult> {
        let Self {
            permit,
            execution,
            design,
            workspace,
            cancelled,
        } = self;
        let progress = if execution.phase == crate::plan::PlanPhase::Verify {
            tokio::task::spawn_blocking(move || SemanticProgress::scan(&design, &workspace))
                .await??
        } else {
            SemanticProgress::default()
        };
        anyhow::ensure!(
            !cancelled.load(Ordering::Acquire),
            "execution interrupted during semantic comparison"
        );
        Ok(ExecutionScanResult {
            permit,
            execution,
            progress,
        })
    }
}

impl HarnessBroker {
    /// Capture the resources and timestamp needed by an unlocked workspace scan.
    pub(super) fn prepare_workspace_capture(&self) -> WorkspaceCaptureJob {
        WorkspaceCaptureJob {
            workspace: PathBuf::from(&self.session.workspace),
            data_root: self.data_root.clone(),
            objects: self.store.objects.clone(),
            repositories: self.repositories.clone(),
            session_id: self.session.id.clone(),
            captured_at_ms: self.clock.now_ms(),
        }
    }

    /// Seed the baseline for synthetic execution fixtures using the production capture.
    #[cfg(test)]
    pub(crate) async fn ensure_execution_baseline(&mut self) -> Result<()> {
        let task = self
            .task_controller
            .current()?
            .context("execution task is missing")?;
        let crate::task::WorkflowSnapshot::Execute { execution } = &task.workflow else {
            anyhow::bail!("execution admission requires Execute");
        };
        if execution.record.baseline_checkpoint.is_some() {
            return Ok(());
        }
        let baseline = self.prepare_workspace_capture().run().await?;
        self.store.save_checkpoint(&baseline)?;
        self.task_controller.apply(
            &mut self.store,
            &task.permit(),
            crate::task::TaskEvent::ExecutionBaselineCaptured(baseline.id),
            self.clock.now_ms(),
        )
    }

    /// Capture the execution record and its update permit from the same selected task revision.
    pub(super) fn capture_execution(&self) -> Result<Option<ExecutionCapture>> {
        let Some(task) = self.task_controller.current()? else {
            return Ok(None);
        };
        let permit = task.permit();
        let crate::task::WorkflowSnapshot::Execute { execution } = task.workflow.clone() else {
            return Ok(None);
        };
        anyhow::ensure!(
            execution.record.session_id == self.session.id,
            "execution belongs to another session"
        );
        Ok(Some(ExecutionCapture {
            permit,
            record: execution.record,
        }))
    }

    /// Capture conformance inputs for a scan that runs outside broker ownership.
    pub(super) fn prepare_execution_conformance(
        &self,
        execution: &PlanExecutionRecord,
        design: &crate::plan::DeclarationDesign,
    ) -> Result<ExecutionConformanceJob> {
        let initial = execution
            .baseline_checkpoint
            .as_ref()
            .map(|id| {
                self.store
                    .load_checkpoint(id)?
                    .context("original execution checkpoint is missing")
            })
            .transpose()?;
        Ok(ExecutionConformanceJob {
            design: design.clone(),
            initial,
            capture: self.prepare_workspace_capture(),
        })
    }

    fn verification_evidence(
        &self,
        execution_id: &str,
        interaction: &Exchange,
    ) -> Result<Vec<Value>> {
        let exchanges = self.store.list_exchange(&self.session.id)?;
        let execution = self
            .current_execution()?
            .filter(|execution| execution.id == execution_id)
            .context("execution evidence owner changed")?;
        Ok(exchanges.iter()
            .filter(|exchange| exchange.id != interaction.id && exchange.created_at_ms >= execution.created_at_ms)
            .chain(std::iter::once(interaction))
            .flat_map(|exchange| exchange.tools())
            .filter(|(_, tool)| matches!(tool.state(), crate::turn::ToolState::Completed | crate::turn::ToolState::Failed))
            .map(|(turn, tool)| json!({"id":tool.id,"call_id":format!("{}:{}", turn.id(), tool.id),"title":tool.title.chars().take(240).collect::<String>(),
                "state":if tool.state() == crate::turn::ToolState::Failed { "failed" } else { "completed" },
                "output_preview":tool.output.chars().take(512).collect::<String>()}))
            .collect())
    }

    pub(super) fn execution_status(
        status: &mut crate::session::state_machine::SessionPhase,
        execution: &PlanExecutionRecord,
    ) {
        crate::session::state_machine::ExecutionStatusSnapshot::from(execution).apply(status);
    }

    pub(super) fn prepare_execution_scan(&self, execution_id: &str) -> Result<ExecutionScanJob> {
        let ExecutionCapture {
            permit,
            record: execution,
        } = self
            .capture_execution()?
            .filter(|capture| capture.record.id == execution_id)
            .context("execution is missing")?;
        let document = self.plan_file.read_submitted_document(
            &self.session.id,
            &execution.plan_id,
            execution.revision,
        )?;
        let design = document
            .design
            .context("execution requires a semantic design")?;
        Ok(ExecutionScanJob {
            permit,
            execution,
            design,
            workspace: PathBuf::from(&self.session.workspace),
            cancelled: self.turn_cancellation.requested_flag(),
        })
    }

    /// Commit a completed scan only if its captured task permit is still current.
    pub(super) fn commit_execution_scan(&mut self, result: ExecutionScanResult) -> Result<Value> {
        let ExecutionScanResult {
            permit,
            mut execution,
            progress,
        } = result;
        permit.validate(self.task_controller.current()?.as_ref())?;
        anyhow::ensure!(
            !self.turn_cancellation.is_requested(),
            "execution interrupted during semantic comparison"
        );
        execution.progress = progress;
        if execution.baseline_checkpoint.is_none() {
            execution.progress.warning.push(CHECKPOINT_WARNING.into());
        }
        self.task_controller.apply(
            &mut self.store,
            &permit,
            crate::task::TaskEvent::ExecutionUpdated(Box::new(execution.clone())),
            self.clock.now_ms(),
        )?;
        Ok(
            json!({"execution_id":execution.id,"phase":execution.phase,"revision":execution.revision,"progress":execution.progress}),
        )
    }

    /// Capture the control authority and all inputs needed by its unlocked evidence job.
    pub(super) fn prepare_execution_control(
        &self,
        execution_id: &str,
        generation: u64,
        task_permit: &crate::task::TaskPermit,
        operation: crate::plan::execution::ExecutionOperation,
        interaction: &Exchange,
    ) -> Result<ExecutionControlJob> {
        use crate::plan::execution::ExecutionOperation;
        task_permit.validate(self.task_controller.current()?.as_ref())?;
        let execution = self
            .store
            .load_plan_execution(execution_id)?
            .context("execution is missing")?;
        anyhow::ensure!(
            execution.generation == generation && execution.state == PlanExecutionState::Active,
            "execution state changed: {}",
            execution.response()
        );
        anyhow::ensure!(
            self.active_goal()?.state == GoalState::Active,
            "execution goal is no longer active"
        );
        anyhow::ensure!(
            self.current_execution()?
                .is_some_and(|current| current.id == execution_id),
            "control belongs to another execution"
        );
        anyhow::ensure!(
            !self.turn_cancellation.is_requested(),
            "execution interrupted"
        );
        let plan = self
            .store
            .load_plan(&execution.plan_id)?
            .context("execution plan is missing")?;
        let evidence = if matches!(&operation, ExecutionOperation::Phase(_))
            && matches!(
                execution.phase,
                crate::plan::PlanPhase::Verify | crate::plan::PlanPhase::Resolve
            ) {
            let document = self.plan_file.read_submitted_document(
                &self.session.id,
                &plan.id,
                execution.revision,
            )?;
            anyhow::ensure!(
                plan.accepted_revision == Some(execution.revision)
                    && plan.accepted_digest.as_deref()
                        == Some(crate::plan::digest(&serde_json::to_vec(&document)?).as_str()),
                "accepted plan revision changed outside the revision workflow"
            );
            let design = document
                .design
                .context("execution requires a semantic design")?;
            Some((
                self.prepare_execution_check_evidence(&execution, &design)?,
                self.prepare_execution_conformance(&execution, &design)?,
            ))
        } else {
            None
        };
        Ok(ExecutionControlJob {
            execution_id: execution_id.to_owned(),
            generation,
            permit: task_permit.clone(),
            operation: Box::new(operation),
            exchange: ExecutionControlExchange::capture(interaction),
            execution_digest: crate::plan::digest(&serde_json::to_vec(&execution)?),
            plan_digest: crate::plan::digest(&serde_json::to_vec(&plan)?),
            evidence,
        })
    }

    /// Drive synthetic control fixtures through the production capture and commit stages.
    #[cfg(test)]
    pub(super) async fn handle_execution_control(
        &mut self,
        execution_id: &str,
        generation: u64,
        task_permit: &crate::task::TaskPermit,
        operation: crate::plan::execution::ExecutionOperation,
        current_interaction: &mut Exchange,
    ) -> Result<Value> {
        let evidence = self
            .prepare_execution_control(
                execution_id,
                generation,
                task_permit,
                operation,
                current_interaction,
            )?
            .run()
            .await?;
        match self.apply_execution_control(evidence, current_interaction)? {
            ExecutionControlStage::Complete(value) => Ok(value),
            ExecutionControlStage::Report(job) => {
                self.commit_execution_control_report(job.run().await?, current_interaction)
            }
        }
    }

    pub(super) fn apply_execution_control(
        &mut self,
        evidence: ExecutionControlEvidence,
        current_interaction: &mut Exchange,
    ) -> Result<ExecutionControlStage> {
        let ExecutionControlEvidence {
            job,
            checks_current,
            progress,
        } = evidence;
        let ExecutionControlJob {
            execution_id,
            generation,
            permit,
            operation,
            exchange,
            execution_digest,
            plan_digest,
            ..
        } = job;
        let task_permit = &permit;
        let execution_id = execution_id.as_str();
        anyhow::ensure!(
            ExecutionControlExchange::capture(current_interaction) == exchange,
            "exchange changed during execution control"
        );
        let captured_execution = self
            .store
            .load_plan_execution(execution_id)?
            .context("execution is missing")?;
        let captured_plan = self
            .store
            .load_plan(&captured_execution.plan_id)?
            .context("execution plan is missing")?;
        anyhow::ensure!(
            crate::plan::digest(&serde_json::to_vec(&captured_execution)?) == execution_digest
                && crate::plan::digest(&serde_json::to_vec(&captured_plan)?) == plan_digest,
            "execution or accepted plan changed during control validation"
        );
        use crate::plan::execution::{ExecutionOperation, SemanticProgress};
        task_permit.validate(self.task_controller.current()?.as_ref())?;
        let mut proposed_interaction = current_interaction.clone();
        let interaction = &mut proposed_interaction;
        let mut execution = self
            .store
            .load_plan_execution(execution_id)?
            .context("execution is missing")?;
        let mut revised_manual = Vec::new();
        let mut goal = self.active_goal()?;
        let mut plan = self
            .store
            .load_plan(&execution.plan_id)?
            .context("execution plan is missing")?;
        anyhow::ensure!(
            execution.generation == generation
                && execution.state == crate::plan::PlanExecutionState::Active
                && goal.state == GoalState::Active,
            "execution state changed: {}",
            execution.response()
        );
        anyhow::ensure!(
            !self.turn_cancellation.is_requested(),
            "execution interrupted"
        );
        anyhow::ensure!(
            self.current_execution()?
                .is_some_and(|current| current.id == execution_id),
            "control belongs to another execution"
        );
        if matches!(
            operation.as_ref(),
            ExecutionOperation::Draft(_) | ExecutionOperation::Submit { .. }
        ) {
            anyhow::ensure!(
                execution.phase == crate::plan::PlanPhase::Resolve,
                "plan revisions are only allowed in Resolve"
            );
        }
        let now_ms = self.clock.now_ms();
        let title;
        let phase_completion = matches!(operation.as_ref(), ExecutionOperation::Phase(_));
        match *operation {
            ExecutionOperation::Inspect => {
                let mut response = execution.response();
                let evidence = self.verification_evidence(execution_id, interaction)?;
                response["verification_evidence"] =
                    serde_json::to_value(evidence.into_iter().rev().take(32).collect::<Vec<_>>())?;
                response["progress"] = serde_json::to_value(&execution.progress)?;
                return Ok(ExecutionControlStage::Complete(response));
            }
            ExecutionOperation::Draft(mut document) => {
                let original = self
                    .plan_file
                    .read_working_document(&self.session.id, &plan.id)?;
                anyhow::ensure!(
                    original.version == plan.document_version,
                    "working plan changed before revision draft"
                );
                anyhow::ensure!(
                    document.plan_id == plan.id && document.version == plan.document_version + 1,
                    "execution revision draft is stale"
                );
                anyhow::ensure!(
                    matches!(plan.state, PlanState::Accepted | PlanState::Revising),
                    "revision is awaiting review"
                );
                if let Some(checkpoint_id) = &execution.baseline_checkpoint {
                    let checkpoint = self
                        .store
                        .load_checkpoint(checkpoint_id)?
                        .context("original execution checkpoint is missing")?;
                    let accepted = self.plan_file.read_submitted_document(
                        &self.session.id,
                        &plan.id,
                        execution.revision,
                    )?;
                    let accepted = accepted.design.context("accepted design is missing")?;
                    let design = document
                        .design
                        .as_mut()
                        .context("execution revision requires a semantic design")?;
                    let newly_captured = design
                        .baseline
                        .keys()
                        .filter(|path| !accepted.baseline.contains_key(*path))
                        .cloned()
                        .collect::<Vec<_>>();
                    for path in newly_captured {
                        if let Some(source) = checkpoint.read(&self.store.objects, &path)? {
                            let source_digest = crate::plan::digest(&source);
                            let source = String::from_utf8(source)?;
                            let (text, calls) =
                                forge_diff::syntax::DeclarationOverview::extract_with_calls(
                                    &path, &source,
                                )
                                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                            let text = forge_diff::syntax::DeclarationOverview::format_with_width(
                                &path,
                                &text,
                                design.line_width,
                            )
                            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                            design.baseline.insert(
                                path.clone(),
                                crate::plan::DeclarationFile {
                                    text,
                                    source_digest,
                                },
                            );
                            design
                                .baseline_calls
                                .insert(path, crate::plan::calls::from_extracted(calls));
                        } else {
                            design.baseline.remove(&path);
                            design.baseline_calls.remove(&path);
                        }
                    }
                }
                self.plan_file.write_working_document_if_unchanged(
                    &self.session.id,
                    &plan.id,
                    &original,
                    &document,
                )?;
                plan.state = PlanState::Revising;
                plan.document_version = document.version;
                plan.updated_at_ms = now_ms;
                self.task_controller.apply_with_effects(
                    &mut self.store,
                    task_permit,
                    crate::task::TaskEvent::ExecutionUpdated(Box::new(execution.clone())),
                    crate::storage::TaskEffects {
                        plan: vec![plan.clone()],
                        ..Default::default()
                    },
                    now_ms,
                )?;
                return Ok(ExecutionControlStage::Complete(
                    json!({"result":"draft_saved","version":document.version,"document":document,"execution":execution.response()}),
                ));
            }
            ExecutionOperation::Submit {
                baseline,
                document,
                reason,
            } => {
                anyhow::ensure!(
                    !reason.trim().is_empty(),
                    "execution revision requires a reason"
                );
                anyhow::ensure!(
                    plan.state == PlanState::Revising && document.version == plan.document_version,
                    "revision submission is stale"
                );
                let previous = self.plan_file.read_submitted_document(
                    &self.session.id,
                    &plan.id,
                    execution.revision,
                )?;
                plan.model_revision += 1;
                let (document, _, digest) = self.plan_file.submit_execution_revision_if_unchanged(
                    &self.session.id,
                    &plan.id,
                    plan.model_revision,
                    &baseline,
                    *document,
                )?;
                plan.review_digest = Some(digest.clone());
                plan.document_version = document.version;
                plan.submitted_version = Some(document.version);
                revised_manual = document
                    .design
                    .as_ref()
                    .context("execution revision design is missing")?
                    .document
                    .verification
                    .clone();
                let delta = crate::plan::revision::DeclarationDelta::between(
                    Some(&previous),
                    document
                        .design
                        .as_ref()
                        .context("accepted execution plan has no declaration design")?,
                )?;
                interaction.node_list.push(ExchangeNode::ArtifactChange {
                    change: crate::exchange::ArtifactChange {
                        id: format!("{}:artifact:{}", interaction.id, plan.model_revision),
                        path: plan.working_path.clone(),
                        diff_text: delta.files,
                        declaration: Some(crate::exchange::DeclarationRevision {
                            plan_id: plan.id.clone(),
                            revision: plan.model_revision,
                            document_diff: delta.document,
                            baseline_paths: delta.baseline_paths,
                        }),
                        created_at_ms: now_ms,
                        revision_request: true,
                    },
                });
                plan.state = PlanState::AwaitingReview;
                execution.pending_revision_reason = Some(reason);
                execution.resume_after_review = true;
                execution.state = PlanExecutionState::Active;
                title = "Plan revision awaiting review";
                execution.generation += 1;
            }
            ExecutionOperation::Phase(request) => {
                execution.validate_phase_request(&request)?;
                anyhow::ensure!(
                    self.agent_coordinator.active_count() == 0,
                    "child execution must settle before completing a plan phase. Wait for or interrupt active children first"
                );
                anyhow::ensure!(
                    plan.state == PlanState::Accepted,
                    "submit and accept the draft revision before completing a phase"
                );
                let document = self.plan_file.read_submitted_document(
                    &self.session.id,
                    &plan.id,
                    execution.revision,
                )?;
                anyhow::ensure!(
                    plan.accepted_revision == Some(execution.revision)
                        && plan.accepted_digest.as_deref()
                            == Some(crate::plan::digest(&serde_json::to_vec(&document)?).as_str()),
                    "accepted plan revision changed outside the revision workflow"
                );
                let design = document
                    .design
                    .context("execution requires a semantic design")?;
                if let Some(report) = &request.verification {
                    report.validate_checks(&design.document.verification)?;
                }
                let verifying = execution.phase == crate::plan::PlanPhase::Verify;
                let resolving = execution.phase == crate::plan::PlanPhase::Resolve;
                let mut progress = if verifying || resolving {
                    progress
                } else {
                    SemanticProgress::default()
                };
                for (path, digest) in &progress.source_digest {
                    let current =
                        crate::plan::workspace_source(Path::new(&self.session.workspace), path)?;
                    let current_digest = current.as_ref().map_or_else(
                        || "absent".into(),
                        |source| crate::plan::digest(source.as_bytes()),
                    );
                    anyhow::ensure!(
                        current_digest == *digest,
                        "{path}: workspace changed during validation. Retry phase completion."
                    );
                }
                let mut verification_tools = std::collections::BTreeMap::new();
                if let Some(report) = &request.verification {
                    let tools = self.verification_evidence(execution_id, interaction)?;
                    for reference in report
                        .evidence
                        .iter()
                        .chain(report.checks.iter().flat_map(|check| &check.evidence))
                    {
                        anyhow::ensure!(
                            tools
                                .iter()
                                .any(|tool| tool["id"].as_str() == Some(reference.as_str())),
                            "verification evidence {reference} is not a completed tool result in this execution. Call plan_overview_read with plan_id and use exact IDs from execution.verification_evidence."
                        );
                        if let Some(call_id) = tools
                            .iter()
                            .find(|tool| tool["id"].as_str() == Some(reference.as_str()))
                            .and_then(|tool| tool["call_id"].as_str())
                        {
                            verification_tools.insert(reference.clone(), call_id.to_owned());
                        }
                    }
                    let reused = execution.verification.iter().any(|evidence| {
                        evidence
                            .report
                            .evidence
                            .iter()
                            .chain(
                                evidence
                                    .report
                                    .checks
                                    .iter()
                                    .flat_map(|check| &check.evidence),
                            )
                            .any(|reference| {
                                report
                                    .evidence
                                    .iter()
                                    .chain(report.checks.iter().flat_map(|check| &check.evidence))
                                    .any(|observed| observed == reference)
                            })
                    });
                    anyhow::ensure!(
                        !reused
                            || report
                                .reuse_reason
                                .as_ref()
                                .is_some_and(|reason| !reason.trim().is_empty()),
                        "explain why reused verification evidence remains applicable"
                    );
                }
                anyhow::ensure!(
                    !self.turn_cancellation.is_requested(),
                    "execution interrupted during validation"
                );
                if execution.baseline_checkpoint.is_none() {
                    progress.warning.push(CHECKPOINT_WARNING.into());
                }
                execution.progress = progress.clone();
                let summary = request.summary.clone();
                let verification = request.verification.clone();
                let declaration_findings = progress.findings();
                let declaration_changes = progress.declaration_changes.clone();
                let verifying = execution.phase == crate::plan::PlanPhase::Verify;
                if verifying && !checks_current {
                    execution.phase = crate::plan::PlanPhase::Resolve;
                    execution.findings = vec!["Automated check evidence no longer covers the current workspace, configuration, or execution contract. Resolve the changed inputs and run Check again before completing verification.".into()];
                    execution.findings.extend(progress.findings());
                    execution.progress = progress;
                    execution.generation += 1;
                } else {
                    let conforms = progress.conforms();
                    execution.finish_phase(request, progress, now_ms)?;
                    if resolving && checks_current && conforms {
                        execution.findings.clear();
                        if design.document.verification.is_empty() {
                            execution.state = PlanExecutionState::Complete;
                            execution.completed_at_ms = Some(now_ms);
                        } else {
                            execution.phase = crate::plan::PlanPhase::Verify;
                        }
                    }
                }
                if verifying {
                    use crate::plan::execution::VerificationOutcome;
                    let outcome = match (execution.state, execution.phase) {
                        (PlanExecutionState::Blocked, _) => VerificationOutcome::Blocked,
                        (_, crate::plan::PlanPhase::Resolve) => VerificationOutcome::Failed,
                        _ => VerificationOutcome::Passed,
                    };
                    interaction
                        .node_list
                        .push(ExchangeNode::VerificationResult {
                            id: format!("{}:verification:{}", interaction.id, execution.generation),
                            result: Box::new(crate::exchange::VerificationResult {
                                outcome,
                                summary: Some(summary),
                                findings: execution.findings.clone(),
                                verification,
                                declaration_findings,
                                declaration_changes,
                                verification_tools,
                            }),
                        });
                }
                goal.state = match execution.state {
                    PlanExecutionState::Complete => GoalState::Complete,
                    PlanExecutionState::Blocked => GoalState::Blocked,
                    _ => GoalState::Active,
                };
                title = match (execution.state, execution.phase) {
                    (PlanExecutionState::Complete, _) => "Plan complete",
                    (PlanExecutionState::Blocked, _) => "Plan verification blocked",
                    (_, crate::plan::PlanPhase::Verify) => "Plan verification ready",
                    (_, crate::plan::PlanPhase::Resolve) => "Plan resolution ready",
                    _ => "Plan implementation",
                };
            }
        }
        plan.updated_at_ms = now_ms;
        let report = if phase_completion
            && matches!(
                execution.state,
                PlanExecutionState::Complete | PlanExecutionState::Blocked
            ) {
            Some(self.prepare_implementation_report(&execution)?)
        } else {
            None
        };
        let proposal = ExecutionControlProposal {
            execution_id: execution_id.to_owned(),
            permit,
            exchange,
            execution_digest,
            plan_digest,
            execution,
            plan,
            nodes: proposed_interaction
                .node_list
                .into_iter()
                .skip(current_interaction.node_list.len())
                .collect(),
            revised_manual,
            title: title.into(),
            phase_completion,
            checks_current,
            now_ms,
        };
        match report {
            Some(report) => Ok(ExecutionControlStage::Report(Box::new(
                ExecutionControlReportJob {
                    proposal: Box::new(proposal),
                    report,
                },
            ))),
            None => Ok(ExecutionControlStage::Complete(
                self.commit_execution_control(proposal, None, current_interaction)?,
            )),
        }
    }

    /// Commit a completed report only if the captured control authority is still current.
    pub(super) fn commit_execution_control_report(
        &mut self,
        captured: ExecutionControlReport,
        current_interaction: &mut Exchange,
    ) -> Result<Value> {
        self.commit_execution_control(
            *captured.proposal,
            Some(captured.report),
            current_interaction,
        )
    }

    fn commit_execution_control(
        &mut self,
        proposal: ExecutionControlProposal,
        report: Option<ImplementationReportResult>,
        current_interaction: &mut Exchange,
    ) -> Result<Value> {
        let ExecutionControlProposal {
            execution_id,
            permit,
            exchange,
            execution_digest,
            plan_digest,
            mut execution,
            plan,
            nodes,
            revised_manual,
            title,
            phase_completion,
            checks_current,
            now_ms,
        } = proposal;
        permit.validate(self.task_controller.current()?.as_ref())?;
        let current = self
            .store
            .load_plan_execution(&execution_id)?
            .context("execution is missing")?;
        let current_plan = self
            .store
            .load_plan(&current.plan_id)?
            .context("execution plan is missing")?;
        anyhow::ensure!(
            ExecutionControlExchange::capture(current_interaction) == exchange
                && crate::plan::digest(&serde_json::to_vec(&current)?) == execution_digest
                && crate::plan::digest(&serde_json::to_vec(&current_plan)?) == plan_digest,
            "execution control changed before commit"
        );
        anyhow::ensure!(
            !self.turn_cancellation.is_requested(),
            "execution interrupted during validation"
        );
        anyhow::ensure!(
            self.active_goal()?.state == GoalState::Active
                && self
                    .current_execution()?
                    .is_some_and(|active| active.id == execution_id),
            "execution authority changed during control validation"
        );
        if phase_completion {
            anyhow::ensure!(
                self.agent_coordinator.active_count() == 0,
                "child execution must settle before completing a plan phase"
            );
            let document = self.plan_file.read_submitted_document(
                &self.session.id,
                &current_plan.id,
                current.revision,
            )?;
            anyhow::ensure!(
                current_plan.accepted_revision == Some(current.revision)
                    && current_plan.accepted_digest.as_deref()
                        == Some(crate::plan::digest(&serde_json::to_vec(&document)?).as_str()),
                "accepted plan revision changed during control validation"
            );
            if checks_current {
                let design = document
                    .design
                    .as_ref()
                    .context("execution requires a semantic design")?;
                anyhow::ensure!(
                    !matches!(
                        self.prepare_execution_check_evidence(&current, design)?,
                        super::checks::ExecutionCheckEvidenceJob::Invalid
                    ),
                    "check configuration changed during control validation"
                );
            }
            for (path, expected) in &execution.progress.source_digest {
                let source =
                    crate::plan::workspace_source(Path::new(&self.session.workspace), path)?;
                let actual = source.as_ref().map_or_else(
                    || "absent".into(),
                    |text| crate::plan::digest(text.as_bytes()),
                );
                anyhow::ensure!(
                    actual == *expected,
                    "{path}: workspace changed during validation. Retry phase completion."
                );
            }
        }
        // Apply only this control's additions to the latest exchange; stream events
        // observed while evidence was collected remain owned by the foreground.
        let mut interaction = current_interaction.clone();
        interaction.node_list.extend(nodes);
        if let Some(report) = report {
            self.commit_implementation_report(&mut execution, &mut interaction, report)?;
        }
        let settled = self.task_controller.settle_execution(
            &mut self.store,
            &permit,
            crate::task::ExecutionSettlement {
                record: execution,
                plan,
                exchange: interaction,
                kind: if phase_completion {
                    crate::task::ExecutionSettlementKind::PhaseFinished
                } else {
                    crate::task::ExecutionSettlementKind::RevisionSubmitted {
                        manual_verification: revised_manual,
                    }
                },
                title,
            },
            now_ms,
        )?;
        execution = settled.record;
        let interaction = settled.exchange;
        if execution.state == PlanExecutionState::Complete {
            let task = self
                .task_controller
                .current()?
                .context("completed execution disappeared")?;
            self.task_controller.finish(
                &mut self.store,
                &task.permit(),
                crate::task::TaskOutcomeState::Complete,
                "Accepted plan completed".into(),
                now_ms,
            )?;
        }
        *current_interaction = interaction;
        if !phase_completion && execution.state == crate::plan::PlanExecutionState::Active {
            return self.revision_review_response(&execution_id);
        }
        let mut response = execution.response();
        response["result"] = json!("transitioned");
        response["instructions"] = if phase_completion {
            json!(
                "End this turn now, without starting the next phase. Forge starts the next phase in a separate exchange with its own instructions."
            )
        } else {
            json!(execution.instructions())
        };
        Ok(response)
    }

    /// Capture immutable report inputs without retaining an execution borrow.
    pub(super) fn prepare_implementation_report(
        &self,
        execution: &PlanExecutionRecord,
    ) -> Result<ImplementationReportJob> {
        let original = self
            .plan_file
            .read_submitted_document(
                &self.session.id,
                &execution.plan_id,
                execution.original_revision,
            )?
            .design
            .context("original accepted design is missing")?;
        let initial = execution
            .baseline_checkpoint
            .as_ref()
            .map(|id| self.store.load_checkpoint(id))
            .transpose()?
            .flatten();
        Ok(ImplementationReportJob {
            original,
            initial,
            capture: self.prepare_workspace_capture(),
            report: crate::plan::implementation_report::ImplementationReport::new(execution),
        })
    }

    /// Validate a captured report against current sources before committing its reference.
    pub(super) fn commit_implementation_report(
        &mut self,
        execution: &mut PlanExecutionRecord,
        interaction: &mut Exchange,
        captured: ImplementationReportResult,
    ) -> Result<()> {
        let ImplementationReportResult { report, digests } = captured;
        for (path, digest) in &digests {
            let source = crate::plan::workspace_source(Path::new(&self.session.workspace), path)?;
            let actual = source.as_deref().map_or_else(
                || "absent".into(),
                |source| crate::plan::digest(source.as_bytes()),
            );
            anyhow::ensure!(
                &actual == digest,
                "{path}: workspace changed while capturing implementation report; retry phase completion"
            );
            if execution.state == PlanExecutionState::Complete
                && let Some(verified) = execution.progress.source_digest.get(path)
            {
                anyhow::ensure!(
                    verified == digest,
                    "{path}: implementation report differs from verification source; retry phase completion"
                );
            }
        }
        anyhow::ensure!(
            execution.state != PlanExecutionState::Complete
                || !self.turn_cancellation.is_requested(),
            "execution interrupted while capturing implementation report"
        );
        let reference = report.save(
            &self.store.objects,
            execution.state != PlanExecutionState::Complete,
        )?;
        execution.implementation_report = Some(reference.clone());
        let id = format!("{}:implementation-report", interaction.id);
        interaction.node_list.retain(|node| node.id() != id);
        interaction
            .node_list
            .push(ExchangeNode::ImplementationReport {
                id,
                reference,
                report: Some(std::sync::Arc::new(report)),
            });
        Ok(())
    }

    pub(super) fn revision_review_response(&self, execution_id: &str) -> Result<Value> {
        let execution = self
            .store
            .load_plan_execution(execution_id)?
            .context("review execution is missing")?;
        let plan = self
            .store
            .load_plan(&execution.plan_id)?
            .context("review plan is missing")?;
        let mut response = execution.response();
        response["result"] = json!("reviewed");
        response["plan_state"] = serde_json::to_value(plan.state)?;
        response["manual_changes"] = serde_json::to_value(&plan.manual_changes)?;
        response["approval_comments"] = serde_json::to_value(&execution.approval_comments)?;
        response["document"] = serde_json::to_value(
            self.plan_file
                .read_working_document(&self.session.id, &plan.id)?,
        )?;
        Ok(response)
    }

    pub(super) fn plan_goal_prompt(
        &self,
        _goal: &GoalRecord,
        kind: PlanExecutionPromptKind,
    ) -> Result<Option<String>> {
        let Some(execution) = self.current_execution()? else {
            return Ok(None);
        };
        let plan = self
            .store
            .load_plan(&execution.plan_id)?
            .context("accepted plan record is missing")?;
        let accepted_revision = plan
            .accepted_revision
            .context("accepted plan revision is missing")?;
        let accepted = self.plan_file.read_submitted_document(
            &self.session.id,
            &plan.id,
            accepted_revision,
        )?;
        Ok(Some(super::PlanPrompt::with_manual_changes(
            super::PlanPrompt::execution(
                &execution,
                kind,
                &accepted.execution_json(execution.phase)?,
            ),
            &plan.manual_changes,
        )))
    }
}
