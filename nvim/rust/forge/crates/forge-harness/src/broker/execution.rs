use super::HarnessBroker;
use crate::exchange::{Exchange, ExchangeNode};
use crate::goal::{GoalRecord, GoalState};
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

impl HarnessBroker {
    fn verification_evidence(&self, execution_id: &str, interaction: &Exchange) -> Result<Vec<Value>> {
        let exchanges = self.store.list_exchange(&self.session.id)?;
        Ok(exchanges.iter()
            .filter(|exchange| exchange.id != interaction.id
                && exchange.execution_id.as_deref() == Some(execution_id))
            .chain(std::iter::once(interaction))
            .flat_map(|exchange| &exchange.turn)
            .flat_map(|turn| turn.tools())
            .filter(|tool| matches!(tool.state(), crate::turn::ToolState::Completed | crate::turn::ToolState::Failed))
            .map(|tool| json!({"id":tool.id,"title":tool.title.chars().take(240).collect::<String>(),
                "state":if tool.state() == crate::turn::ToolState::Failed { "failed" } else { "completed" },
                "output_preview":tool.output.chars().take(512).collect::<String>()}))
            .collect())
    }

    pub(super) fn execution_review(&self, plan_id: &str) -> Result<Option<String>> {
        let Some(execution) = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .find(|execution| execution.plan_id == plan_id)
        else {
            return Ok(None);
        };
        let mut report = format!(
            "## {}: {:?}\n\nOriginal approved revision: {}. Current accepted revision: {}.\n\n### Semantic comparison\n\n",
            execution.phase.label(), execution.state, execution.original_revision, execution.revision
        );
        for (label, paths) in [
            ("Matched", &execution.progress.matched),
            ("Missing", &execution.progress.missing),
            ("Different", &execution.progress.different),
            ("Unverified", &execution.progress.unverified),
            ("Analyzer warning", &execution.progress.warning),
        ] {
            for path in paths {
                report.push_str(&format!("- {label}: {path}\n"));
            }
        }
        if !execution.findings.is_empty() {
            report.push_str("\n### Unresolved findings\n\n");
            for finding in &execution.findings {
                report.push_str(&format!("- {finding}\n"));
            }
        }
        report.push_str("\n### Verification assessments\n\n");
        for evidence in &execution.verification {
            report.push_str(&format!(
                "- Revision {} · {:?}: {}\n  Tool evidence: {}\n",
                evidence.revision,
                evidence.report.outcome,
                evidence.summary,
                evidence.report.evidence.join(", ")
            ));
            if let Some(reason) = &evidence.report.reuse_reason {
                report.push_str(&format!("  Evidence reuse: {reason}\n"));
            }
        }
        report.push_str("\n### Revision history\n\n");
        for revision in &execution.revision_history {
            report.push_str(&format!(
                "- Revision {} · {} approval: {}\n",
                revision.revision, revision.approval, revision.reason
            ));
        }
        if let Some(reason) = &execution.pending_revision_reason {
            report.push_str(&format!("- Awaiting review: {reason}\n"));
        }
        let original = self.plan_file.read_submitted_document(
            &self.session.id,
            plan_id,
            execution.original_revision,
        )?;
        let current = self.plan_file.read_submitted_document(
            &self.session.id,
            plan_id,
            execution.revision,
        )?;
        let delta = crate::plan::revision::DeclarationDelta::between(Some(&original), &current)?;
        report.push_str("\n### Original approved to current accepted revision\n\n");
        if delta.document.is_empty() && delta.files.is_empty() {
            report.push_str("No revisions to the approved design.\n");
        } else {
            report.push_str(&format!(
                "```diff\n{}{}\n```\n",
                delta.document, delta.files
            ));
        }
        Ok(Some(report))
    }

    pub(super) fn execution_status(
        status: &mut crate::session::state_machine::SessionPhase,
        execution: &PlanExecutionRecord,
    ) {
        if let crate::session::state_machine::SessionPhase::Working {
            execution: progress, ..
        } = status
        {
            *progress = Some(crate::session::state_machine::ExecutionStatus {
                id: execution.id.clone(),
                phase: execution.phase,
                progress: execution.progress.clone(),
            });
        }
    }

    pub(super) async fn scan_execution(&mut self, execution_id: &str) -> Result<Value> {
        let mut execution = self
            .store
            .load_plan_execution(execution_id)?
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
        let progress = tokio::task::spawn_blocking(move || {
            crate::plan::execution::SemanticProgress::scan(&design, &workspace)
        })
        .await??;
        anyhow::ensure!(
            !self.turn_cancellation.requested.load(Ordering::Acquire),
            "execution interrupted during semantic comparison"
        );
        execution.progress = progress;
        if execution.baseline_checkpoint.is_none() {
            execution.progress.warning.push(CHECKPOINT_WARNING.into());
        }
        self.store.save_plan_execution(&execution)?;
        Ok(
            json!({"execution_id":execution.id,"phase":execution.phase,"revision":execution.revision,"progress":execution.progress}),
        )
    }

    pub(super) async fn handle_execution_control(
        &mut self,
        execution_id: &str,
        generation: u64,
        operation: crate::plan::execution::ExecutionOperation,
        interaction: &mut Exchange,
    ) -> Result<Value> {
        use crate::plan::execution::{ExecutionOperation, ExecutionRevision, SemanticProgress};
        let mut execution = self
            .store
            .load_plan_execution(execution_id)?
            .context("execution is missing")?;
        let mut goal = self
            .store
            .load_goal(&execution.goal_id)?
            .context("execution goal is missing")?;
        let mut plan = self
            .store
            .load_plan(&execution.plan_id)?
            .context("execution plan is missing")?;
        anyhow::ensure!(
            execution.generation == generation
                && execution.state == PlanExecutionState::Active
                && goal.state == GoalState::Active,
            "execution state changed: {}",
            execution.response()
        );
        anyhow::ensure!(
            !self.turn_cancellation.requested.load(Ordering::Acquire),
            "execution interrupted"
        );
        anyhow::ensure!(
            interaction.execution_id.as_deref() == Some(execution_id),
            "control belongs to another interaction"
        );
        let now_ms = self.clock.now_ms();
        let title;
        let phase_completion = matches!(&operation, ExecutionOperation::Phase(_));
        match operation {
            ExecutionOperation::Inspect => {
                let mut response = execution.response();
                let evidence = self.verification_evidence(execution_id, interaction)?;
                response["verification_evidence"] = serde_json::to_value(evidence.into_iter().rev().take(32).collect::<Vec<_>>())?;
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
                        if let Some(source) = checkpoint.read(&self.store.objects, &path, 1024 * 1024)? {
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
                self.store
                    .save_execution_transition(&execution, &goal, Some(&plan), None)?;
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
                let delta =
                    crate::plan::revision::DeclarationDelta::between(Some(&previous), &document)?;
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
                    },
                });
                if self.session.plan_auto_approve_revisions {
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
                let workspace = PathBuf::from(&self.session.workspace);
                let accepted_paths = design
                    .baseline
                    .keys()
                    .chain(design.proposed.keys())
                    .cloned()
                    .collect::<std::collections::BTreeSet<_>>();
                let mut progress = tokio::task::spawn_blocking(move || {
                    SemanticProgress::scan(&design, &workspace)
                })
                .await??;
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
                            now_ms,
                        )
                        .await?;
                    let before = initial
                        .file
                        .iter()
                        .map(|file| (&file.path, &file.object_id))
                        .collect::<std::collections::BTreeMap<_, _>>();
                    let after = current
                        .file
                        .iter()
                        .map(|file| (&file.path, &file.object_id))
                        .collect::<std::collections::BTreeMap<_, _>>();
                    for path in before
                        .keys()
                        .chain(after.keys())
                        .copied()
                        .collect::<std::collections::BTreeSet<_>>()
                    {
                        if forge_diff::syntax::DeclarationOverview::supports(path)
                            && !accepted_paths.contains(path)
                            && before.get(path) != after.get(path)
                        {
                            progress.different.push(format!("{path}: source changed outside the accepted plan. Restore it or submit a justified revision."));
                        }
                    }
                }
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
                if let Some(report) = &request.verification {
                    let tools = self.verification_evidence(execution_id, interaction)?;
                    for reference in &report.evidence {
                        anyhow::ensure!(
                            tools.iter().any(|tool| tool["id"].as_str() == Some(reference.as_str())),
                            "verification evidence {reference} is not a completed tool result in this execution. Call harness_plan_read with only plan_id and use exact IDs from execution.verification_evidence."
                        );
                    }
                    let reused = execution.verification.iter().any(|evidence| {
                        evidence
                            .report
                            .evidence
                            .iter()
                            .any(|reference| report.evidence.contains(reference))
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
                self.store.save_plan_execution(&execution)?;
                execution.finish_phase(request, progress, now_ms)?;
                if let Some(phase) = &mut interaction.execution_phase {
                    use crate::plan::execution::VerificationOutcome;
                    phase.outcome = Some(match (execution.state, execution.phase) {
                        (PlanExecutionState::Blocked, _) => VerificationOutcome::Blocked,
                        (_, crate::plan::PlanPhase::Resolve) => VerificationOutcome::Failed,
                        _ => VerificationOutcome::Passed,
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
        if execution.state == PlanExecutionState::Complete
            && execution.revision != execution.original_revision
        {
            let original = self.plan_file.read_submitted_document(
                &self.session.id,
                &plan.id,
                execution.original_revision,
            )?;
            let implemented = self.plan_file.read_submitted_document(
                &self.session.id,
                &plan.id,
                execution.revision,
            )?;
            let delta =
                crate::plan::revision::DeclarationDelta::between(Some(&original), &implemented)?;
            interaction.node_list.push(ExchangeNode::ArtifactChange {
                change: crate::exchange::ArtifactChange {
                    id: format!("{}:implemented-plan-comparison", interaction.id),
                    path: format!(
                        "Original revision {} → implemented revision {}",
                        execution.original_revision, execution.revision
                    ),
                    diff_text: format!("{}{}", delta.document, delta.files),
                    declaration: None,
                    created_at_ms: now_ms,
                },
            });
        }
        goal.updated_at_ms = now_ms;
        plan.updated_at_ms = now_ms;
        if !phase_completion || execution.state == PlanExecutionState::Complete {
            execution.append_lifecycle(
                crate::plan::ExchangeAnchor::capture(interaction),
                now_ms,
                if phase_completion {
                    PlanExecutionLifecycleEvent::Completed { title: plan.title.clone() }
                } else {
                    PlanExecutionLifecycleEvent::Phase {
                        title: title.into(), phase: execution.phase,
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
        self.store
            .save_execution_transition(&execution, &goal, Some(&plan), Some(interaction))?;
        if !phase_completion && execution.state == PlanExecutionState::Active {
            return self.revision_review_response(execution_id);
        }
        let mut response = execution.response();
        response["result"] = json!("transitioned");
        response["instructions"] = if phase_completion { json!(
            "End this turn now. Do not start the next phase. Forge will start a separate exchange with its instructions."
        ) } else { json!(execution.instructions()) };
        Ok(response)
    }

    pub(super) fn revision_review_response(&self, execution_id: &str) -> Result<Value> {
        let execution = self.store.load_plan_execution(execution_id)?.context("review execution is missing")?;
        let plan = self.store.load_plan(&execution.plan_id)?.context("review plan is missing")?;
        let mut response = execution.response();
        response["result"] = json!("reviewed");
        response["plan_state"] = serde_json::to_value(plan.state)?;
        response["document"] = serde_json::to_value(self.plan_file.read_working_document(&self.session.id, &plan.id)?)?;
        Ok(response)
    }

    pub(super) fn record_execution_failure(&mut self, interaction: &Exchange, reason: String) -> Result<()> {
        let Some(execution_id) = &interaction.execution_id else { return Ok(()) };
        let mut execution = self.store.load_plan_execution(execution_id)?.context("execution is missing")?;
        if execution.state == PlanExecutionState::Complete { return Ok(()) }
        let plan = self.store.load_plan(&execution.plan_id)?.context("execution plan is missing")?;
        execution.append_lifecycle(
            crate::plan::ExchangeAnchor::capture(interaction), self.clock.now_ms(),
            PlanExecutionLifecycleEvent::Failed { title: plan.title, reason },
        );
        self.store.save_plan_execution(&execution)
    }

    pub(super) fn observe_plan_progress(&mut self, interaction: &Exchange) -> Result<bool> {
        let Some(execution_id) = interaction.execution_id.as_deref() else {
            return Ok(false);
        };
        let mut execution = self
            .store
            .load_plan_execution(execution_id)?
            .context("execution is missing")?;
        let document = self.plan_file.read_submitted_document(
            &self.session.id,
            &execution.plan_id,
            execution.revision,
        )?;
        let progress = crate::plan::execution::SemanticProgress::scan(
            document
                .design
                .as_ref()
                .context("execution requires a semantic design")?,
            Path::new(&self.session.workspace),
        )?;
        let mut changed = progress.source_digest != execution.observed_source
            || execution.generation != execution.observed_generation;
        if execution.phase == crate::plan::PlanPhase::Verify {
            let source = serde_json::to_string(&progress.source_digest)?;
            for tool in interaction
                .turn
                .iter()
                .flat_map(|turn| turn.tools())
                .filter(|tool| {
                    matches!(
                        tool.state(),
                        crate::turn::ToolState::Completed | crate::turn::ToolState::Failed
                    ) && (matches!(tool.kind.as_str(), "command" | "shell" | "execute")
                        || matches!(
                            tool.title.as_str(),
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
        self.store.save_plan_execution(&execution)?;
        Ok(changed)
    }

    pub(super) fn plan_goal_prompt(
        &self,
        goal: &GoalRecord,
        kind: PlanExecutionPromptKind,
    ) -> Result<Option<String>> {
        let Some(execution) = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .find(|execution| execution.goal_id == goal.id)
        else {
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
        Ok(Some(super::PlanPrompt::execution(&execution, kind, &accepted.model_json()?)))
    }

    pub(super) fn sync_plan_execution(&mut self, goal: &GoalRecord) -> Result<()> {
        let Some(mut execution) = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .find(|execution| execution.goal_id == goal.id)
        else {
            return Ok(());
        };
        let state = match goal.state {
            GoalState::Active => PlanExecutionState::Active,
            GoalState::Paused | GoalState::UsageLimited | GoalState::BudgetLimited => {
                PlanExecutionState::Paused
            }
            GoalState::Cleared => PlanExecutionState::Cancelled,
            GoalState::Complete => {
                anyhow::ensure!(
                    execution.state == PlanExecutionState::Complete,
                    "plan execution can complete only through its verification gate"
                );
                PlanExecutionState::Complete
            }
            GoalState::Blocked => PlanExecutionState::Blocked,
            GoalState::Stalled => PlanExecutionState::Stalled,
        };
        if execution.state != state {
            execution.state = state;
            execution.generation += 1;
            let status = match state {
                PlanExecutionState::Active => "resumed",
                PlanExecutionState::Paused => "paused",
                PlanExecutionState::Cancelled => "cancelled",
                PlanExecutionState::Complete => "completed",
                PlanExecutionState::Blocked => "blocked",
                PlanExecutionState::Stalled => "stopped at continuation limit",
            };
            execution.append_lifecycle(
                self.plan_exchange_anchor(&execution.plan_id)?,
                goal.updated_at_ms,
                PlanExecutionLifecycleEvent::Phase {
                    title: format!("{} {status}", execution.phase.label()),
                    phase: execution.phase,
                    revision: execution.revision,
                    state,
                },
            );
        }
        if matches!(
            state,
            PlanExecutionState::Complete | PlanExecutionState::Cancelled
        ) {
            execution.completed_at_ms.get_or_insert(goal.updated_at_ms);
        } else {
            execution.completed_at_ms = None;
        }
        self.store
            .save_execution_transition(&execution, goal, None, None)?;
        Ok(())
    }
}
