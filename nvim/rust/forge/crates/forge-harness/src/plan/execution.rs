use super::{DeclarationDesign, ExchangeAnchor, PlanDocument};
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;
use tokio::sync::{mpsc, oneshot};

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Identifies the work required before the next execution gate.
pub enum PlanPhase {
    Implement,
    Checking,
    Verify,
    Resolve,
}

impl PlanPhase {
    pub(crate) fn label(self) -> &'static str {
        crate::session::state_machine::WorkflowActivity::from(self).lifecycle_label()
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Separates execution suspension from its preserved phase.
pub enum PlanExecutionState {
    Active,
    Complete,
    Paused,
    Stalled,
    Blocked,
    Cancelled,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// States whether a manual check, or the Verify phase as a whole, passed, failed, or could not run.
pub enum VerificationStatus {
    Passed,
    Failed,
    Blocked,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Holds a submitted Verify report: one result per manual check, the defects found, and the
/// overall status Harness derives from them.
pub struct VerificationReport {
    pub status: VerificationStatus,
    #[serde(default)]
    pub manual_checks: Vec<ManualCheckResult>,
    /// Cites the evidence behind the findings.
    #[serde(default)]
    pub observed_in: Vec<String>,
    #[serde(default)]
    pub findings: Vec<String>,
    #[serde(default)]
    pub reason: Option<String>,
    #[serde(default)]
    pub reuse_reason: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Records one accepted manual check, what the agent observed, and where it observed it.
pub struct ManualCheckResult {
    /// Holds the accepted manual check text from `## Manual checks`.
    pub label: String,
    pub status: VerificationStatus,
    /// Describes what the agent observed while performing the check.
    pub note: String,
    /// Cites the tool results and passed checks that show the result.
    #[serde(default)]
    pub observed_in: Vec<String>,
}

enum VerificationAssessment<'report> {
    Passed,
    Failed,
    Blocked { reason: &'report str },
}

/// Derive the short, stable citation that `plan:evidence.md` shows for one evidence ID.
///
/// A content hash keeps the alias fixed as later tool calls join the evidence list, so a reading
/// taken earlier in the turn still names the same result.
pub(crate) fn evidence_alias(id: &str) -> String {
    format!("E{}", &super::digest(id.as_bytes())[..8])
}

impl VerificationReport {
    /// Iterate every evidence ID the report cites, from its findings and its manual checks.
    pub(crate) fn cited(&self) -> impl Iterator<Item = &String> {
        self.observed_in.iter().chain(
            self.manual_checks
                .iter()
                .flat_map(|check| &check.observed_in),
        )
    }

    fn assessment(&self) -> Result<VerificationAssessment<'_>> {
        match self.status {
            VerificationStatus::Passed => {
                ensure!(
                    self.manual_checks
                        .iter()
                        .all(|check| check.status == VerificationStatus::Passed),
                    "a passed report cannot contain failed or blocked checks"
                );
                ensure!(
                    self.findings.is_empty(),
                    "a passed report cannot retain unresolved findings"
                );
                ensure!(
                    self.cited().next().is_some(),
                    "a passed report requires cited evidence"
                );
                Ok(VerificationAssessment::Passed)
            }
            VerificationStatus::Failed => {
                ensure!(
                    self.findings
                        .iter()
                        .any(|finding| !finding.trim().is_empty())
                        || self
                            .manual_checks
                            .iter()
                            .any(|check| check.status == VerificationStatus::Failed),
                    "a failed report requires concrete findings"
                );
                Ok(VerificationAssessment::Failed)
            }
            VerificationStatus::Blocked => {
                let reason = self
                    .reason
                    .as_deref()
                    .filter(|reason| !reason.trim().is_empty())
                    .context("a blocked report requires a reason")?;
                Ok(VerificationAssessment::Blocked { reason })
            }
        }
    }

    /// Require exactly one noted result per accepted manual check, in plan order.
    pub(crate) fn validate_checks(&self, manual_checks: &[String]) -> Result<()> {
        ensure!(
            self.manual_checks.len() == manual_checks.len()
                && self
                    .manual_checks
                    .iter()
                    .zip(manual_checks)
                    .all(|(result, check)| result.label == *check),
            "the report must hold one result per accepted manual check, in plan order"
        );
        ensure!(
            self.manual_checks
                .iter()
                .all(|result| !result.note.trim().is_empty()),
            "every manual check result requires a note"
        );
        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Requests completion of exactly the phase and revision in the turn context.
pub struct PlanPhaseDone {
    pub phase: PlanPhase,
    pub revision: u32,
    pub summary: String,
    /// Parsed by Harness from the saved `plan:verification.md` text, never from model arguments.
    #[serde(skip)]
    pub verification: Option<VerificationReport>,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
/// Retains source identities and comparison findings without claiming behavioral proof.
pub struct SemanticProgress {
    pub matched: Vec<String>,
    pub missing: Vec<String>,
    pub different: Vec<String>,
    pub unverified: Vec<String>,
    pub warning: Vec<String>,
    pub source_digest: BTreeMap<String, String>,
    #[serde(default)]
    pub declaration_changes: Vec<DeclarationMismatch>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Retains an accepted-to-observed declaration patch at verification time.
pub struct DeclarationMismatch {
    pub path: String,
    pub name: String,
    pub diff: String,
    pub finding: String,
}

impl SemanticProgress {
    /// Compare every captured target against fresh implementation projections.
    pub fn scan(design: &DeclarationDesign, workspace: &Path) -> Result<Self> {
        let parsed = design.parsed()?;
        let mut progress = Self::default();
        for path in design
            .baseline
            .keys()
            .chain(parsed.proposed.keys())
            .cloned()
            .collect::<BTreeSet<_>>()
        {
            let expected = parsed.proposed.get(&path);
            let source = match super::workspace_source(workspace, &path) {
                Ok(source) => source,
                Err(error) => {
                    progress.unverified.push(format!("{path}: {error:#}"));
                    continue;
                }
            };
            progress.source_digest.insert(
                path.clone(),
                source.as_ref().map_or_else(
                    || "absent".into(),
                    |source| super::digest(source.as_bytes()),
                ),
            );
            match (expected, source) {
                (None, None) => progress.matched.push(path),
                (None, Some(_)) => progress
                    .different
                    .push(format!("{path}: planned deletion still exists")),
                (Some(expected), None) => {
                    progress
                        .declaration_changes
                        .push(super::conformance::declaration_mismatch(
                            &path,
                            "Missing file",
                            expected,
                            "",
                            "missing planned file",
                        )?);
                    progress.missing.push(path);
                }
                (Some(expected), Some(source)) => {
                    match super::conformance::inspect_in_workspace(
                        workspace, &path, expected, &source,
                    ) {
                        Ok((differences, changes)) => {
                            progress.declaration_changes.extend(changes);
                            if differences.is_empty() {
                                progress.matched.push(path);
                            } else {
                                progress.different.extend(
                                    differences
                                        .into_iter()
                                        .map(|difference| format!("{path}: {difference}")),
                                );
                            }
                        }
                        Err(error) => progress.unverified.push(format!("{path}: {error:#}")),
                    }
                }
            }
        }
        Ok(progress)
    }

    /// Report whether every comparable planned file satisfies the accepted target.
    pub fn conforms(&self) -> bool {
        self.missing.is_empty() && self.different.is_empty() && self.unverified.is_empty()
    }

    /// Return corrective findings without presenting warnings as mismatches.
    pub fn findings(&self) -> Vec<String> {
        self.missing
            .iter()
            .map(|path| format!("{path}: missing planned file"))
            .chain(self.different.iter().cloned())
            .chain(self.unverified.iter().cloned())
            .collect()
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Preserves verification provenance when later phases select checks to reuse.
pub struct VerificationEvidence {
    pub revision: u32,
    pub source_digest: BTreeMap<String, String>,
    pub summary: String,
    pub report: VerificationReport,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Audits revision decisions independently from phase transitions.
pub struct ExecutionRevision {
    pub revision: u32,
    pub reason: String,
    pub approval: String,
    pub recorded_at_ms: i64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
/// Describes a durable execution event for the native timeline.
pub enum PlanExecutionLifecycleEvent {
    Completed {
        title: String,
    },
    Failed {
        title: String,
        reason: String,
    },
    Phase {
        title: String,
        phase: PlanPhase,
        revision: u32,
        state: PlanExecutionState,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Positions an execution event within its owning exchange.
pub struct PlanExecutionLifecycleRecord {
    pub anchor: Option<ExchangeAnchor>,
    pub sequence: u64,
    pub after_exchange_id: Option<String>,
    pub occurred_at_ms: i64,
    #[serde(flatten)]
    pub event: PlanExecutionLifecycleEvent,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Owns one accepted semantic target through implementation and verification.
pub struct PlanExecutionRecord {
    pub id: String,
    pub session_id: String,
    pub plan_id: String,
    pub baseline_checkpoint: Option<String>,
    pub state: PlanExecutionState,
    pub phase: PlanPhase,
    pub original_revision: u32,
    pub revision: u32,
    pub generation: u64,
    /// Freeze accepted manual actions so Verify cannot exist without work to perform.
    #[serde(default)]
    pub manual_verification: Vec<String>,
    #[serde(default)]
    pub implementation_report: Option<super::implementation_report::ImplementationReportRef>,
    pub planning_provider_session_id: Option<String>,
    pub execution_provider_session_id: Option<String>,
    pub lifecycle: Vec<PlanExecutionLifecycleRecord>,
    pub findings: Vec<String>,
    pub progress: SemanticProgress,
    pub verification: Vec<VerificationEvidence>,
    /// Holds the text of `plan:verification.md` as the agent wrote it during Verify, until
    /// `plan_phase_done` parses and submits it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub verification_draft: Option<String>,
    #[serde(default)]
    pub check_runs: Vec<crate::checks::CheckRun>,
    pub revision_history: Vec<ExecutionRevision>,
    #[serde(default)]
    /// Carries accepted reviewer notes into initial, resumed, and live review responses.
    pub approval_comments: Vec<String>,
    pub pending_revision_reason: Option<String>,
    pub resume_after_review: bool,
    pub observed_source: BTreeMap<String, String>,
    #[serde(default)]
    pub observed_checkpoint: Option<String>,
    #[serde(default)]
    pub observed_document_version: u64,
    pub observed_check: BTreeSet<String>,
    pub created_at_ms: i64,
    pub completed_at_ms: Option<i64>,
}

impl PlanExecutionRecord {
    /// Get the selected commands of this revision's passing Harness check run.
    ///
    /// Verify tells the model to cite these instead of rerunning them, so an empty result
    /// means Harness ran no automated check for the revision and no such claim holds.
    pub(crate) fn passed_check_commands(
        &self,
    ) -> impl Iterator<Item = &crate::checks::CommandCheck> {
        self.check_runs
            .last()
            .filter(|run| {
                run.revision == self.revision
                    && run.state == crate::checks::CheckState::Passed
                    && run.status() == VerificationStatus::Passed
            })
            .into_iter()
            .flat_map(|run| run.commands.iter().filter(|command| command.selected))
    }

    /// Validate a completion request without changing the accepted state on rejection.
    pub fn finish_phase(
        &mut self,
        request: PlanPhaseDone,
        progress: SemanticProgress,
        now_ms: i64,
    ) -> Result<()> {
        self.validate_phase_request(&request)?;
        self.verification_draft = None;
        if self.phase != PlanPhase::Verify {
            ensure!(
                request.verification.is_none(),
                "verification assessment is only accepted in Verify"
            );
            self.phase = PlanPhase::Checking;
        } else {
            let report = request
                .verification
                .context("Verify requires a verification assessment")?;
            match report.assessment()? {
                VerificationAssessment::Passed => {
                    if progress.conforms() {
                        self.state = PlanExecutionState::Complete;
                        self.completed_at_ms = Some(now_ms);
                        self.findings.clear();
                    } else {
                        self.phase = PlanPhase::Resolve;
                        self.findings = progress.findings();
                    }
                }
                VerificationAssessment::Failed => {
                    self.phase = PlanPhase::Resolve;
                    self.findings = report.findings.clone();
                    self.findings.extend(
                        report
                            .manual_checks
                            .iter()
                            .filter(|result| result.status != VerificationStatus::Passed)
                            .map(|result| format!("{}: {}", result.label, result.note)),
                    );
                    self.findings.extend(progress.findings());
                }
                VerificationAssessment::Blocked { reason } => {
                    self.state = PlanExecutionState::Blocked;
                    self.findings = vec![reason.to_owned()];
                    self.findings.extend(progress.findings());
                }
            }
            self.verification.push(VerificationEvidence {
                revision: self.revision,
                source_digest: progress.source_digest.clone(),
                summary: request.summary,
                report,
            });
        }
        self.progress = progress;
        self.generation += 1;
        Ok(())
    }

    /// Validate phase identity and ownership before evidence collection or mutation.
    pub(crate) fn validate_phase_request(&self, request: &PlanPhaseDone) -> Result<()> {
        ensure!(
            self.state == PlanExecutionState::Active,
            "execution is {:?}, not active",
            self.state
        );
        ensure!(
            request.phase == self.phase && request.revision == self.revision,
            "stale phase completion: current phase is {:?}, revision {}",
            self.phase,
            self.revision
        );
        ensure!(
            !request.summary.trim().is_empty(),
            "phase completion requires a summary"
        );
        ensure!(
            self.pending_revision_reason.is_none(),
            "a plan revision awaits review"
        );
        ensure!(
            self.phase != PlanPhase::Checking,
            "Checking is owned by Harness, not the model"
        );
        Ok(())
    }

    /// Describe the committed state and its next legal operation.
    pub fn response(&self) -> Value {
        json!({"phase":self.phase,"status":self.state,"revision":self.revision,
            "generation":self.generation,"findings":self.findings,"instructions":self.instructions()})
    }

    /// Describe Verify work from the checks Harness actually ran for this revision, so the
    /// model never cites a passing automated check that no run recorded.
    fn verify_instructions(&self) -> String {
        let passed = self
            .passed_check_commands()
            .map(|command| {
                command
                    .context
                    .as_ref()
                    .map_or(command.command.as_str(), |context| context.name.as_str())
            })
            .collect::<Vec<_>>();
        let checks = if passed.is_empty() {
            "Harness ran no automated check commands for this revision, because no configured check runs automatically for these changes, so plan:verification.md lists no `Harness check` evidence. When an action requires a check, find it with check_definition_list, run it with check_run, and cite that check_run result as evidence. Do not edit source or revise the plan.".to_owned()
        } else {
            format!(
                "Harness has already passed the declaration check and these automated checks for this revision: {}. If an action requests one of them, report its recorded result instead of running it again, citing the matching `Harness check` evidence from plan:verification.md. Run any other check an action requires with check_run. Do not rerun passed checks, edit source, or revise the plan.",
                passed.join(", ")
            )
        };
        format!(
            "Perform only the accepted plan's manual verification actions, in order. {checks} Report each manual action as passed, failed, or blocked, with observations and tool evidence, and report blockers honestly. Observed defects go to Resolve. Read plan:verification.md with plan_read for the actions and evidence IDs before submitting results."
        )
    }

    /// Build phase-specific continuation guidance from durable state.
    pub fn instructions(&self) -> String {
        if self.state != PlanExecutionState::Active {
            return format!(
                "Execution is {:?}. End this turn, and do not continue automatically.",
                self.state
            );
        }
        if self.phase == PlanPhase::Checking {
            return "Harness runs the declaration and accepted checks locally. No model turn runs in this phase.".into();
        }
        let verify;
        let work = match self.phase {
            PlanPhase::Implement => {
                "Implement the accepted design and its tests. The accepted plan is frozen during Implement. Make justified deviations when needed, such as internal helpers, and explain them in your completion summary. Do not edit or submit the plan, update declaration metadata, or run builds, compiler checks, tests, linters, formatters, or runtime checks. Harness runs validation and conformance checks in the Checking phase. When the code is written, call plan_phase_done. Differences in declarations or in what a callable uses do not block finishing Implement."
            }
            PlanPhase::Resolve => {
                "Resolve the collected verification findings by fixing the code in the workspace. If an intentional change affects the accepted contract, propose one combined plan revision with a concrete reason, and continue with the returned revision once it is accepted. Internal helpers and differences in what a callable uses do not need a revision. Finish with plan_phase_done so Harness runs the declaration and automated checks again. Do not run those checks yourself."
            }
            PlanPhase::Checking => {
                "Harness runs the declaration and accepted checks locally. No model turn runs in this phase."
            }
            PlanPhase::Verify => {
                verify = self.verify_instructions();
                verify.as_str()
            }
        };
        let reporting = if self.phase == PlanPhase::Verify {
            " Write the report in plan:verification.md with plan_edit. Replace each `N. pending []` row with `N. passed|failed|blocked [IDs] note`, citing IDs from plan:evidence.md; a blocked note states the concrete cause. List each defect under `## Findings` as `path:line:column: message [IDs]`, or `-: message` when no file applies, without repeating row notes or Harness declaration diagnostics. Harness derives the overall result. Then call plan_phase_done, which reports any problem in the report by line. The UI shows the structured report, so keep the final response brief."
        } else {
            ""
        };
        format!(
            "{work}{reporting} Phase: {:?}. Accepted revision: {}. Inspect the plan with plan_status and read its artifacts and files with plan_read. Call plan_phase_done with this phase and revision when the phase's work is finished, then end the turn after it succeeds. Harness records the transition and supplies the next phase. Ending a turn does not end the phase. Do not call goal_complete for this execution.",
            self.phase, self.revision
        )
    }

    /// Append an event at the execution's current causal position.
    pub fn append_lifecycle(
        &mut self,
        anchor: ExchangeAnchor,
        occurred_at_ms: i64,
        event: PlanExecutionLifecycleEvent,
    ) {
        let sequence = self
            .lifecycle
            .last()
            .map_or(1, |record| record.sequence + 1);
        self.lifecycle.push(PlanExecutionLifecycleRecord {
            anchor: Some(anchor),
            sequence,
            after_exchange_id: None,
            occurred_at_ms,
            event,
        });
    }
}

/// Carries an execution control operation back to the broker that owns persistence.
pub enum ExecutionOperation {
    Inspect,
    Phase(PlanPhaseDone),
    /// Saves the text of `plan:verification.md` without finishing the phase.
    VerificationDraft(String),
    Draft(Box<PlanDocument>),
    Submit {
        baseline: Box<PlanDocument>,
        document: Box<PlanDocument>,
        reason: String,
    },
}

/// Associates one broker request with the execution generation captured at turn start.
pub struct ExecutionControl {
    pub(crate) task_permit: crate::task::TaskPermit,
    pub execution_id: String,
    pub generation: u64,
    pub operation: ExecutionOperation,
    pub response: oneshot::Sender<Result<Value>>,
}

#[derive(Clone)]
/// Restricts provider execution controls to their admitted broker turn.
pub struct ExecutionControlSender {
    pub(crate) task_controller: std::sync::Arc<crate::task::SessionTaskController>,
    pub(crate) task_permit: crate::task::TaskPermit,
    pub phase: PlanPhase,
    pub execution_id: String,
    pub generation: u64,
    pub sender: mpsc::Sender<ExecutionControl>,
}

impl std::fmt::Debug for ExecutionControlSender {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("ExecutionControlSender")
            .field("phase", &self.phase)
            .field("execution_id", &self.execution_id)
            .field("generation", &self.generation)
            .finish_non_exhaustive()
    }
}

impl ExecutionControlSender {
    /// Reject plan mutations outside Resolve before draft parsing or side effects.
    pub fn require_revision_phase(&self) -> Result<()> {
        ensure!(
            self.phase == PlanPhase::Resolve,
            "plan revisions are only allowed in Resolve; finish {} and let Verify collect deviations",
            self.phase.label()
        );
        Ok(())
    }

    /// Reject plan edits outside Resolve, which revises the plan, and Verify, which writes its
    /// report artifact.
    pub fn require_edit_phase(&self) -> Result<()> {
        if self.phase == PlanPhase::Verify {
            return Ok(());
        }
        self.require_revision_phase()
    }

    /// Await a committed broker result before acknowledging the provider's request.
    pub async fn send(&self, operation: ExecutionOperation) -> Result<Value> {
        let task_permit = self
            .task_controller
            .admit_dispatched(crate::task::TaskKind::Execute, &self.task_permit)?;
        let task = self
            .task_controller
            .current()?
            .context("execution task was cleared")?;
        task_permit.validate(Some(&task))?;
        let crate::task::WorkflowSnapshot::Execute { execution } = task.workflow.clone() else {
            return Err(crate::task::StaleTask.into());
        };
        if execution.record.id != self.execution_id
            || execution.record.generation != self.generation
            || execution.record.phase != self.phase
        {
            return Err(crate::task::StaleTask.into());
        }
        if matches!(
            &operation,
            ExecutionOperation::Draft(_) | ExecutionOperation::Submit { .. }
        ) {
            self.require_revision_phase()?;
        }
        let (response, receive) = oneshot::channel();
        self.sender
            .send(ExecutionControl {
                task_permit,
                execution_id: self.execution_id.clone(),
                generation: self.generation,
                operation,
                response,
            })
            .await
            .context("execution control owner closed")?;
        receive
            .await
            .context("execution control owner dropped its response")?
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn execution() -> PlanExecutionRecord {
        PlanExecutionRecord {
            id: "execution".into(),
            session_id: "session".into(),
            plan_id: "plan".into(),
            baseline_checkpoint: None,
            state: PlanExecutionState::Active,
            phase: PlanPhase::Implement,
            implementation_report: None,
            original_revision: 1,
            revision: 1,
            generation: 0,
            manual_verification: Vec::new(),
            planning_provider_session_id: None,
            execution_provider_session_id: None,
            lifecycle: Vec::new(),
            findings: Vec::new(),
            progress: SemanticProgress::default(),
            verification: Vec::new(),
            verification_draft: None,
            check_runs: Vec::new(),
            revision_history: Vec::new(),
            approval_comments: Vec::new(),
            pending_revision_reason: None,
            resume_after_review: false,
            observed_source: BTreeMap::new(),
            observed_checkpoint: None,
            observed_document_version: 0,
            observed_check: BTreeSet::new(),
            created_at_ms: 0,
            completed_at_ms: None,
        }
    }

    fn done(phase: PlanPhase, status: Option<VerificationStatus>) -> PlanPhaseDone {
        PlanPhaseDone {
            phase,
            revision: 1,
            summary: "Verified requirement".into(),
            verification: status.map(|status| VerificationReport {
                status,
                observed_in: vec!["check-result".into()],
                findings: if status == VerificationStatus::Failed {
                    vec!["Round reset retains score".into()]
                } else {
                    Vec::new()
                },
                reason: if status == VerificationStatus::Blocked {
                    Some("Compiler unavailable".into())
                } else {
                    None
                },
                reuse_reason: None,
                manual_checks: Vec::new(),
            }),
        }
    }

    #[test]
    fn finishing_a_phase_discards_the_verification_draft() {
        let mut execution = execution();
        execution.verification_draft = Some("# Verification\n".into());
        execution
            .finish_phase(
                done(PlanPhase::Implement, None),
                SemanticProgress::default(),
                1,
            )
            .unwrap();
        assert_eq!(execution.verification_draft, None);
    }

    #[test]
    fn implementation_defers_all_conformance_to_verification() {
        let mut execution = execution();
        let mismatch = SemanticProgress {
            missing: vec!["main.rs".into()],
            ..Default::default()
        };
        execution
            .finish_phase(done(PlanPhase::Implement, None), mismatch.clone(), 1)
            .unwrap();
        assert_eq!(execution.phase, PlanPhase::Checking);
        execution.phase = PlanPhase::Verify;
        execution
            .finish_phase(
                done(PlanPhase::Verify, Some(VerificationStatus::Passed)),
                mismatch.clone(),
                2,
            )
            .unwrap();
        assert_eq!(execution.phase, PlanPhase::Resolve);
        assert_eq!(execution.findings, ["main.rs: missing planned file"]);
        execution
            .finish_phase(done(PlanPhase::Resolve, None), mismatch, 3)
            .unwrap();
        assert_eq!(execution.phase, PlanPhase::Checking);
        execution.phase = PlanPhase::Verify;
        execution
            .finish_phase(
                done(PlanPhase::Verify, Some(VerificationStatus::Passed)),
                SemanticProgress::default(),
                4,
            )
            .unwrap();
        assert_eq!(execution.state, PlanExecutionState::Complete);
        assert_eq!(execution.completed_at_ms, Some(4));
        assert!(
            execution
                .finish_phase(
                    done(PlanPhase::Verify, Some(VerificationStatus::Passed)),
                    SemanticProgress::default(),
                    5
                )
                .is_err()
        );
    }

    #[test]
    fn semantic_scan_detects_source_configuration_deletions_and_parse_failure() {
        let workspace = tempfile::tempdir().unwrap();
        let design = DeclarationDesign::scratch(BTreeMap::from([
            ("main.rs".into(), "fn run();\n".into()),
            ("config.json".into(), "{\"enabled\":true}\n".into()),
        ]))
        .unwrap();
        assert_eq!(
            SemanticProgress::scan(&design, workspace.path())
                .unwrap()
                .missing
                .len(),
            2
        );
        std::fs::write(workspace.path().join("main.rs"), "fn run() {}\n").unwrap();
        std::fs::write(workspace.path().join("config.json"), "{\"enabled\":true}\n").unwrap();
        assert!(
            SemanticProgress::scan(&design, workspace.path())
                .unwrap()
                .conforms()
        );
        std::fs::write(workspace.path().join("main.rs"), "fn run( broken").unwrap();
        assert_eq!(
            SemanticProgress::scan(&design, workspace.path())
                .unwrap()
                .unverified
                .len(),
            1
        );
        std::fs::write(
            workspace.path().join("config.json"),
            "{\"enabled\":false}\n",
        )
        .unwrap();
        assert_eq!(
            SemanticProgress::scan(&design, workspace.path())
                .unwrap()
                .different
                .len(),
            1
        );
    }
}
