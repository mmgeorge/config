use super::HarnessBroker;
use crate::exchange::{Exchange, ExchangeNode};
use crate::goal::{GoalRecord, GoalState};
use crate::plan::execution::SemanticProgress;
use crate::plan::{
    PlanExecutionLifecycleEvent, PlanExecutionPromptKind, PlanExecutionRecord, PlanExecutionState,
    PlanState,
};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;

const CHECKPOINT_WARNING: &str =
    "Workspace-wide deviation detection is unavailable without a repository checkpoint.";

pub(super) struct ExecutionCapture {
    pub(super) permit: crate::task::TaskPermit,
    pub(super) record: PlanExecutionRecord,
}

impl HarnessBroker {
    /// Capture the execution record and its update permit from the same selected task revision.
    pub(super) fn capture_execution(&self) -> Result<Option<ExecutionCapture>> {
        let Some(task) = self.task_controller.current()? else {
            return Ok(None);
        };
        let permit = task.permit();
        let crate::task::TaskWorkflow::Execute { execution } = task.workflow else {
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

    pub(super) async fn scan_execution_conformance(
        &mut self,
        execution: &PlanExecutionRecord,
        design: &crate::plan::DeclarationDesign,
    ) -> Result<SemanticProgress> {
        let workspace = PathBuf::from(&self.session.workspace);
        let accepted_paths = design
            .baseline
            .keys()
            .chain(design.proposed.keys())
            .cloned()
            .collect::<std::collections::BTreeSet<_>>();
        let inspected = design.clone();
        let comparison_workspace = workspace.clone();
        let mut progress = {
            tokio::task::spawn_blocking(move || {
                SemanticProgress::scan(&inspected, &comparison_workspace)
            })
            .await??
        };
        if let Some(checkpoint_id) = &execution.baseline_checkpoint {
            let initial = self
                .store
                .load_checkpoint(checkpoint_id)?
                .context("original execution checkpoint is missing")?;
            let current = crate::checkpoint::GitCheckpoint::new(&self.session.workspace)
                .capture(
                    &self.store.objects,
                    &self.repositories,
                    &self.session.id,
                    self.clock.now_ms(),
                )
                .await?;
            for path in initial.changed_paths(&current)? {
                let path = path.as_str();
                if forge_diff::syntax::DeclarationOverview::supports(path)
                    && !accepted_paths.contains(path)
                {
                    let expected = initial.read(&self.store.objects, path)?;
                    let source =
                        crate::plan::workspace_source(Path::new(&self.session.workspace), path)?;
                    progress.source_digest.insert(
                        path.to_string(),
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
                                Err(error) => {
                                    progress.unverified.push(format!("{path}: {error:#}"))
                                }
                            }
                        }
                        (None, Some(source))
                            if forge_diff::syntax::ConfigurationFormat::for_path(path)
                                .is_none() =>
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
                                Err(error) => {
                                    progress.unverified.push(format!("{path}: {error:#}"))
                                }
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
        }
        Ok(progress)
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

    pub(super) async fn scan_execution(&mut self, execution_id: &str) -> Result<Value> {
        let ExecutionCapture {
            permit,
            record: mut execution,
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
        let workspace = PathBuf::from(&self.session.workspace);
        let progress = if execution.phase == crate::plan::PlanPhase::Verify {
            tokio::task::spawn_blocking(move || {
                crate::plan::execution::SemanticProgress::scan(&design, &workspace)
            })
            .await??
        } else {
            crate::plan::execution::SemanticProgress::default()
        };
        anyhow::ensure!(
            !self.turn_cancellation.requested.load(Ordering::Acquire),
            "execution interrupted during semantic comparison"
        );
        execution.progress = progress;
        if execution.baseline_checkpoint.is_none() {
            execution.progress.warning.push(CHECKPOINT_WARNING.into());
        }
        self.task_controller
            .update(&mut self.store, &permit, self.clock.now_ms(), |task| {
                let crate::task::TaskWorkflow::Execute { execution: active } = &mut task.workflow
                else {
                    anyhow::bail!("execution task changed");
                };
                active.record = execution.clone();
                Ok(())
            })?;
        Ok(
            json!({"execution_id":execution.id,"phase":execution.phase,"revision":execution.revision,"progress":execution.progress}),
        )
    }

    pub(super) async fn handle_execution_control(
        &mut self,
        execution_id: &str,
        generation: u64,
        task_permit: &crate::task::TaskPermit,
        operation: crate::plan::execution::ExecutionOperation,
        current_interaction: &mut Exchange,
    ) -> Result<Value> {
        use crate::plan::execution::{ExecutionOperation, ExecutionRevision, SemanticProgress};
        task_permit.validate(self.task_controller.current()?.as_ref())?;
        let mut proposed_interaction = current_interaction.clone();
        let interaction = &mut proposed_interaction;
        let mut execution = self
            .store
            .load_plan_execution(execution_id)?
            .context("execution is missing")?;
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
            !self.turn_cancellation.requested.load(Ordering::Acquire),
            "execution interrupted"
        );
        anyhow::ensure!(
            self.current_execution()?
                .is_some_and(|current| current.id == execution_id),
            "control belongs to another execution"
        );
        if matches!(
            &operation,
            ExecutionOperation::Draft(_) | ExecutionOperation::Submit { .. }
        ) {
            anyhow::ensure!(
                execution.phase == crate::plan::PlanPhase::Resolve,
                "plan revisions are only allowed in Resolve"
            );
        }
        let now_ms = self.clock.now_ms();
        let title;
        let phase_completion = matches!(&operation, ExecutionOperation::Phase(_));
        match operation {
            ExecutionOperation::Inspect => {
                let mut response = execution.response();
                let evidence = self.verification_evidence(execution_id, interaction)?;
                response["verification_evidence"] =
                    serde_json::to_value(evidence.into_iter().rev().take(32).collect::<Vec<_>>())?;
                response["progress"] = serde_json::to_value(&execution.progress)?;
                return Ok(response);
            }
            ExecutionOperation::Draft(mut document) => {
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
                self.plan_file
                    .write_working_document(&self.session.id, &plan.id, &document)?;
                plan.state = PlanState::Revising;
                plan.document_version = document.version;
                plan.updated_at_ms = now_ms;
                self.task_controller.update_with_effects(
                    &mut self.store,
                    task_permit,
                    now_ms,
                    |task| {
                        let crate::task::TaskWorkflow::Execute { execution: active } =
                            &mut task.workflow
                        else {
                            anyhow::bail!("execution task changed");
                        };
                        active.record = execution.clone();
                        Ok((
                            (),
                            crate::storage::TaskEffects {
                                plan: vec![plan.clone()],
                                ..Default::default()
                            },
                        ))
                    },
                )?;
                return Ok(
                    json!({"result":"draft_saved","version":document.version,"document":document,"execution":execution.response()}),
                );
            }
            ExecutionOperation::Submit { document, reason } => {
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
                let (document, _, digest) = self.plan_file.submit_execution_revision(
                    &self.session.id,
                    &plan.id,
                    plan.model_revision,
                    document,
                )?;
                plan.review_digest = Some(digest.clone());
                plan.document_version = document.version;
                plan.submitted_version = Some(document.version);
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
                let approved_review = matches!(
                    &plan.revision_approval,
                    Some(approval @ crate::plan::ReviewRevisionApproval::ResumeExecution { .. })
                        if approval.authorizes(plan.model_revision)
                );
                if approved_review || self.session.plan_auto_approve_revisions {
                    plan.revision_approval = None;
                    plan.accepted_digest = Some(digest);
                    plan.accepted_revision = Some(plan.model_revision);
                    plan.state = PlanState::Accepted;
                    execution.revision = plan.model_revision;
                    execution.revision_history.push(ExecutionRevision {
                        revision: execution.revision,
                        reason,
                        approval: "automatic".into(),
                        recorded_at_ms: now_ms,
                    });
                    execution.progress = SemanticProgress::default();
                    title = "Plan revision accepted (auto)";
                } else {
                    plan.state = PlanState::AwaitingReview;
                    execution.pending_revision_reason = Some(reason);
                    execution.review_paused = true;
                    execution.state = PlanExecutionState::Paused;
                    goal.state = GoalState::Paused;
                    title = "Plan revision awaiting review";
                }
                execution.generation += 1;
            }
            ExecutionOperation::Phase(request) => {
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
                let mut progress = if verifying {
                    self.scan_execution_conformance(&execution, &design).await?
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
                            "verification evidence {reference} is not a completed tool result in this execution. Call plan_read with only plan_id and use exact IDs from execution.verification_evidence."
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
                    !self.turn_cancellation.requested.load(Ordering::Acquire),
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
                execution.finish_phase(request, progress, now_ms)?;
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
        if phase_completion
            && matches!(
                execution.state,
                PlanExecutionState::Complete | PlanExecutionState::Blocked
            )
        {
            self.capture_implementation_report(&mut execution, interaction, now_ms)
                .await?;
        }
        goal.updated_at_ms = now_ms;
        plan.updated_at_ms = now_ms;
        if !phase_completion || execution.state == PlanExecutionState::Complete {
            execution.append_lifecycle(
                crate::plan::ExchangeAnchor::capture(interaction),
                now_ms,
                if phase_completion {
                    PlanExecutionLifecycleEvent::Completed {
                        title: plan.title.clone(),
                    }
                } else {
                    PlanExecutionLifecycleEvent::Phase {
                        title: title.into(),
                        phase: execution.phase,
                        revision: if plan.state == PlanState::AwaitingReview {
                            plan.model_revision
                        } else {
                            execution.revision
                        },
                        state: execution.state,
                    }
                },
            );
        }
        self.task_controller
            .update_with_effects(&mut self.store, task_permit, now_ms, |task| {
                let crate::task::TaskWorkflow::Execute { execution: active } = &mut task.workflow
                else {
                    anyhow::bail!("execution task changed");
                };
                active.record = execution.clone();
                Ok((
                    (),
                    crate::storage::TaskEffects {
                        plan: vec![plan.clone()],
                        exchange: vec![interaction.clone()],
                        ..Default::default()
                    },
                ))
            })?;
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
        *current_interaction = proposed_interaction;
        if !phase_completion && execution.state == crate::plan::PlanExecutionState::Active {
            return self.revision_review_response(execution_id);
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

    pub(super) async fn capture_implementation_report(
        &mut self,
        execution: &mut PlanExecutionRecord,
        interaction: &mut Exchange,
        now_ms: i64,
    ) -> Result<()> {
        let original = self.plan_file.read_submitted_document(
            &self.session.id,
            &execution.plan_id,
            execution.original_revision,
        )?;
        let original = original
            .design
            .context("original accepted design is missing")?;
        let initial = execution
            .baseline_checkpoint
            .as_ref()
            .map(|id| self.store.load_checkpoint(id))
            .transpose()?
            .flatten();
        let current = if initial.is_some() {
            Some(
                crate::checkpoint::GitCheckpoint::new(&self.session.workspace)
                    .capture(
                        &self.store.objects,
                        &self.repositories,
                        &self.session.id,
                        now_ms,
                    )
                    .await?,
            )
        } else {
            None
        };
        let workspace = PathBuf::from(&self.session.workspace);
        let objects = self.store.objects.clone();
        let report = crate::plan::implementation_report::ImplementationReport::new(execution);
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
                || !self.turn_cancellation.requested.load(Ordering::Acquire),
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

    pub(super) async fn record_execution_failure(
        &mut self,
        interaction: &mut Exchange,
        reason: String,
    ) -> Result<()> {
        let Some(ExecutionCapture {
            permit,
            record: mut execution,
        }) = self.capture_execution()?
        else {
            return Ok(());
        };
        if execution.state == PlanExecutionState::Complete {
            return Ok(());
        }
        let plan = self
            .store
            .load_plan(&execution.plan_id)?
            .context("execution plan is missing")?;
        execution.append_lifecycle(
            crate::plan::ExchangeAnchor::capture(interaction),
            self.clock.now_ms(),
            PlanExecutionLifecycleEvent::Failed {
                title: plan.title,
                reason,
            },
        );
        self.capture_partial_report(&mut execution, interaction)
            .await;
        self.task_controller
            .update(&mut self.store, &permit, self.clock.now_ms(), |task| {
                let crate::task::TaskWorkflow::Execute { execution: active } = &mut task.workflow
                else {
                    anyhow::bail!("execution task changed");
                };
                active.record = execution;
                Ok(())
            })
    }

    async fn capture_partial_report(
        &mut self,
        execution: &mut PlanExecutionRecord,
        interaction: &mut Exchange,
    ) {
        if let Err(error) = self
            .capture_implementation_report(execution, interaction, self.clock.now_ms())
            .await
        {
            let mut report =
                crate::plan::implementation_report::ImplementationReport::new(execution);
            report
                .unavailable
                .push(format!("Implementation comparison unavailable: {error:#}"));
            if let Ok(reference) = report.save(&self.store.objects, true) {
                execution.implementation_report = Some(reference.clone());
                interaction
                    .node_list
                    .push(ExchangeNode::ImplementationReport {
                        id: format!("{}:implementation-report-unavailable", interaction.id),
                        reference,
                        report: Some(std::sync::Arc::new(report)),
                    });
            }
        }
    }

    pub(super) fn observe_plan_progress(&mut self, interaction: &Exchange) -> Result<bool> {
        let Some(ExecutionCapture {
            permit,
            record: mut execution,
        }) = self.capture_execution()?
        else {
            return Ok(false);
        };
        let document = self.plan_file.read_submitted_document(
            &self.session.id,
            &execution.plan_id,
            execution.revision,
        )?;
        let design = document
            .design
            .as_ref()
            .context("execution requires a semantic design")?;
        let progress = if execution.phase == crate::plan::PlanPhase::Verify {
            crate::plan::execution::SemanticProgress::scan(
                design,
                Path::new(&self.session.workspace),
            )?
        } else {
            let mut progress = crate::plan::execution::SemanticProgress::default();
            for path in design.proposed.keys() {
                if let Some(source) =
                    crate::plan::workspace_source(Path::new(&self.session.workspace), path)?
                {
                    progress
                        .source_digest
                        .insert(path.clone(), crate::plan::digest(source.as_bytes()));
                }
            }
            progress
        };
        let mut changed = progress.source_digest != execution.observed_source
            || execution.generation != execution.observed_generation;
        if execution.phase == crate::plan::PlanPhase::Verify {
            let source = serde_json::to_string(&progress.source_digest)?;
            for tool in interaction
                .turn
                .iter()
                .flat_map(|turn| interaction.tools_for_turn(turn.id()))
                .filter(|tool| {
                    matches!(
                        tool.state(),
                        crate::turn::ToolState::Completed | crate::turn::ToolState::Failed
                    ) && (matches!(tool.kind.as_str(), "command" | "shell" | "execute")
                        || matches!(
                            tool.title.split('(').next().unwrap_or("").trim(),
                            "powershell" | "bash" | "shell" | "exec_command"
                        ))
                })
            {
                let identity = crate::plan::digest(
                    format!("{}\n{}\n{source}", tool.title, tool.output).as_bytes(),
                );
                changed |= execution.observed_check.insert(identity);
            }
        }
        execution.observed_source = progress.source_digest.clone();
        execution.observed_generation = execution.generation;
        execution.progress = progress;
        if execution.baseline_checkpoint.is_none() {
            execution.progress.warning.push(CHECKPOINT_WARNING.into());
        }
        self.task_controller
            .update(&mut self.store, &permit, self.clock.now_ms(), |task| {
                let crate::task::TaskWorkflow::Execute { execution: active } = &mut task.workflow
                else {
                    anyhow::bail!("execution task changed");
                };
                active.record = execution.clone();
                Ok(())
            })?;
        Ok(changed)
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
