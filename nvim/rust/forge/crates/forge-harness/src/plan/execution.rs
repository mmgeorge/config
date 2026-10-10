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
/// Records the agent's interpretation of behavioral verification.
pub enum VerificationOutcome {
    Passed,
    Failed,
    Blocked,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Links an agent assessment to actual tool evidence and unresolved defects.
pub struct VerificationReport {
    pub outcome: VerificationOutcome,
    #[serde(default)]
    pub checks: Vec<VerificationCheck>,
    #[serde(default)]
    pub evidence: Vec<String>,
    #[serde(default)]
    pub findings: Vec<String>,
    #[serde(default)]
    pub reason: Option<String>,
    #[serde(default)]
    pub reuse_reason: Option<String>,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Identifies the accepted plan's verification group.
pub enum VerificationCategory {
    Automated,
    Manual,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Records one planned check and the tool evidence supporting its result.
pub struct VerificationCheck {
    pub category: VerificationCategory,
    pub label: String,
    pub outcome: VerificationOutcome,
    pub summary: String,
    #[serde(default)]
    pub evidence: Vec<String>,
}

impl VerificationReport {
    /// Validate check identities and evidence before a phase transition is committed.
    pub(crate) fn validate_checks(&self, actions: &[String]) -> Result<()> {
        ensure!(self.checks.len() == actions.len(), "report every accepted manual verification action");
        for (check, action) in self.checks.iter().zip(actions) {
            ensure!(check.category == VerificationCategory::Manual && check.label == *action,
                "verification must contain only accepted manual actions in order");
            ensure!(!check.summary.trim().is_empty(), "manual result requires observations");
        }
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
    #[serde(default)]
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
        let mut progress = Self::default();
        for path in design
            .baseline
            .keys()
            .chain(design.proposed.keys())
            .cloned()
            .collect::<BTreeSet<_>>()
        {
            let expected = design.proposed.get(&path);
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
                    progress.declaration_changes.push(super::conformance::declaration_mismatch(
                        &path, "Missing file", expected, "", "missing planned file")?);
                    progress.missing.push(path);
                }
                (Some(expected), Some(source)) => {
                    match super::conformance::inspect_in_workspace(workspace, &path, expected, &source) {
                        Ok((differences, changes)) => {
                            progress.declaration_changes.extend(changes);
                            if differences.is_empty() { progress.matched.push(path); }
                            else { progress.different.extend(differences.into_iter().map(|difference| format!("{path}: {difference}"))); }
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
    Completed { title: String },
    Failed { title: String, reason: String },
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
    pub goal_id: String,
    pub baseline_checkpoint: Option<String>,
    pub state: PlanExecutionState,
    pub phase: PlanPhase,
    pub original_revision: u32,
    pub revision: u32,
    pub generation: u64,
    #[serde(default)]
    pub implementation_report: Option<super::implementation_report::ImplementationReportRef>,
    pub planning_backend_session_id: Option<String>,
    pub execution_backend_session_id: Option<String>,
    pub lifecycle: Vec<PlanExecutionLifecycleRecord>,
    pub findings: Vec<String>,
    pub progress: SemanticProgress,
    pub verification: Vec<VerificationEvidence>,
    #[serde(default)]
    pub check_runs: Vec<super::checks::CheckRun>,
    pub revision_history: Vec<ExecutionRevision>,
    pub pending_revision_reason: Option<String>,
    pub review_paused: bool,
    pub observed_source: BTreeMap<String, String>,
    pub observed_generation: u64,
    pub observed_check: BTreeSet<String>,
    pub created_at_ms: i64,
    pub completed_at_ms: Option<i64>,
}

impl PlanExecutionRecord {
    /// Validate a completion request without changing the accepted state on rejection.
    pub fn finish_phase(
        &mut self,
        request: PlanPhaseDone,
        progress: SemanticProgress,
        now_ms: i64,
    ) -> Result<()> {
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
        ensure!(self.phase != PlanPhase::Checking, "Checking is owned by Harness, not the model");
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
            match report.outcome {
                VerificationOutcome::Passed => {
                    ensure!(report.checks.iter().all(|check| check.outcome == VerificationOutcome::Passed),
                        "a passed assessment cannot contain failed or blocked checks");
                    ensure!(
                        report.findings.is_empty(),
                        "a passed assessment cannot retain unresolved findings"
                    );
                    ensure!(
                        !report.evidence.is_empty(),
                        "a passed assessment requires verification evidence"
                    );
                    if progress.conforms() {
                        self.state = PlanExecutionState::Complete;
                        self.completed_at_ms = Some(now_ms);
                        self.findings.clear();
                    } else {
                        self.phase = PlanPhase::Resolve;
                        self.findings = progress.findings();
                    }
                }
                VerificationOutcome::Failed => {
                    ensure!(
                        report
                            .findings
                            .iter()
                            .any(|finding| !finding.trim().is_empty())
                            || report.checks.iter().any(|check| check.outcome == VerificationOutcome::Failed),
                        "failed verification requires concrete findings"
                    );
                    self.phase = PlanPhase::Resolve;
                    self.findings = report.findings.clone();
                    self.findings.extend(report.checks.iter()
                        .filter(|check| check.outcome != VerificationOutcome::Passed)
                        .map(|check| format!("{}: {}", check.label, check.summary)));
                    self.findings.extend(progress.findings());
                }
                VerificationOutcome::Blocked => {
                    ensure!(
                        report
                            .reason
                            .as_ref()
                            .is_some_and(|reason| !reason.trim().is_empty()),
                        "blocked verification requires a reason"
                    );
                    self.state = PlanExecutionState::Blocked;
                    self.findings = vec![report.reason.clone().unwrap()];
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

    /// Describe the committed state and its next legal operation.
    pub fn response(&self) -> Value {
        json!({"phase":self.phase,"status":self.state,"revision":self.revision,
            "generation":self.generation,"findings":self.findings,"instructions":self.instructions()})
    }

    /// Build phase-specific continuation guidance from durable state.
    pub fn instructions(&self) -> String {
        if self.state != PlanExecutionState::Active {
            return format!(
                "Execution is {:?}. End this turn. Do not continue automatically.",
                self.state
            );
        }
        let work = match self.phase {
            PlanPhase::Implement => {
                "Implement the accepted design and tests. The accepted plan is frozen during Implement. Make justified implementation deviations when needed, including internal helpers, and explain them in your completion summary. Do not edit or submit the plan, reconcile declaration metadata, or run builds, compiler checks, tests, linters, formatters, or runtime checks. Harness runs validation and conformance assessment in Checking. When writing is finished, call harness_plan_phase_done. Declarations and call/access differences do not gate Implement completion."
            }
            PlanPhase::Resolve => {
                "Resolve the collected verification findings. Correct code defects in the workspace. If an intentional change affects the accepted contract, propose one consolidated plan revision with a concrete reason. Internal helpers and Calls/Accesses differences do not require revisions. After acceptance, continue with the returned revision. Finish with harness_plan_phase_done so Harness can run declarations and automated checks again. Do not run those checks yourself."
            }
            PlanPhase::Checking => "Harness runs declarations and accepted checks locally. No model turn is admitted for this phase.",
            PlanPhase::Verify => {
                "Perform only the accepted plan's manual verification actions, in order. Harness has already passed declarations and automated checks. Do not rerun automated checks, edit source, or revise the plan. Report each manual action as passed, failed, or blocked with observations and tool evidence. Report blockers honestly. Send observed defects to Resolve. Retrieve exact evidence IDs with harness_plan_read before submitting results."
            }
        };
        let reporting = if self.phase == PlanPhase::Verify {
            " Report verification.checks with one entry per accepted manual verification action, in plan order. Each entry contains category (manual), the exact command or manual check label, outcome (passed, failed, or blocked), a concise result summary, and evidence tool IDs. Include checks that could not run as blocked with a concrete reason. Put only additional unresolved findings in verification.findings, without repeating check summaries or Harness declaration diagnostics. The UI renders the structured report. Keep the final response brief instead of repeating the inventory."
        } else { "" };
        format!(
            "{work}{reporting} Phase: {:?}. Accepted revision: {}. Read the target with harness_plan_read. Call harness_plan_phase_done with this phase and revision when its work is finished, then end the turn after success. Harness commits the transition and supplies the next phase. Ending a turn alone does not end the phase. Do not call harness_goal_complete for this execution.",
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
    Draft(PlanDocument),
    Submit {
        document: PlanDocument,
        reason: String,
    },
}

/// Associates one broker request with the execution generation captured at turn start.
pub struct ExecutionControl {
    pub execution_id: String,
    pub generation: u64,
    pub operation: ExecutionOperation,
    pub response: oneshot::Sender<Result<Value>>,
}

#[derive(Clone, Debug)]
/// Restricts provider execution controls to their admitted broker turn.
pub struct ExecutionControlSender {
    pub phase: PlanPhase,
    pub execution_id: String,
    pub generation: u64,
    pub sender: mpsc::Sender<ExecutionControl>,
}

impl ExecutionControlSender {
    /// Reject plan mutations outside Resolve before draft parsing or side effects.
    pub fn require_revision_phase(&self) -> Result<()> {
        ensure!(self.phase == PlanPhase::Resolve, "plan revisions are only allowed in Resolve; finish {} and let Verify collect deviations", self.phase.label());
        Ok(())
    }

    /// Await a committed broker result before acknowledging the provider's request.
    pub async fn send(&self, operation: ExecutionOperation) -> Result<Value> {
        if matches!(&operation, ExecutionOperation::Draft(_) | ExecutionOperation::Submit { .. }) { self.require_revision_phase()?; }
        let (response, receive) = oneshot::channel();
        self.sender
            .send(ExecutionControl {
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
    #[test]
    fn manual_results_match_the_accepted_inventory() {
        let mut report: VerificationReport = serde_json::from_value(json!({
            "outcome":"blocked", "checks":[
                {"category":"manual","label":"Open window","outcome":"blocked","summary":"No desktop interface","evidence":[]}
            ]
        })).unwrap();
        assert!(report.validate_checks(&["Open window".into()]).is_ok());
        assert!(report.validate_checks(&["Resize window".into()]).is_err());
        assert!(report.validate_checks(&[]).is_err());
        report.checks[0].category=VerificationCategory::Automated;
        assert!(report.validate_checks(&["Open window".into()]).is_err());
    }

    use super::*;

    fn execution() -> PlanExecutionRecord {
        PlanExecutionRecord {
            id: "execution".into(),
            session_id: "session".into(),
            plan_id: "plan".into(),
            goal_id: "goal".into(),
            baseline_checkpoint: None,
            state: PlanExecutionState::Active,
            phase: PlanPhase::Implement,
            implementation_report: None,
            original_revision: 1,
            revision: 1,
            generation: 0,
            planning_backend_session_id: None,
            execution_backend_session_id: None,
            lifecycle: Vec::new(),
            findings: Vec::new(),
            progress: SemanticProgress::default(),
            verification: Vec::new(),
            check_runs: Vec::new(),
            revision_history: Vec::new(),
            pending_revision_reason: None,
            review_paused: false,
            observed_source: BTreeMap::new(),
            observed_generation: 0,
            observed_check: BTreeSet::new(),
            created_at_ms: 0,
            completed_at_ms: None,
        }
    }

    #[test]
    fn continuation_prompts_retain_the_persisted_execution_phase_and_revision() {
        use crate::plan::{PlanExecutionPromptKind, PlanPrompt};
        let mut record = execution();
        record.revision = 3;
        record.findings.push("Reset still retains score".into());
        for phase in [PlanPhase::Implement, PlanPhase::Verify, PlanPhase::Resolve] {
            record.phase = phase;
            for (kind, prefix) in [
                (PlanExecutionPromptKind::Start, "Start the Execute task"),
                (PlanExecutionPromptKind::Continue, "Continue the same Execute task"),
                (PlanExecutionPromptKind::ResumeAfterInterruption, "Resume the same Execute task after interruption"),
            ] {
                let prompt = PlanPrompt::execution(&record, kind, "accepted document");
                assert!(prompt.starts_with(prefix));
                assert!(prompt.contains("Execution ID: execution\nPlan ID: plan"));
                assert!(prompt.contains(&format!("Phase: {phase:?}. Accepted revision: 3.")));
                assert!(prompt.contains("Reset still retains score"));
                assert!(prompt.ends_with("accepted document"));
                if phase == PlanPhase::Implement {
                    assert!(prompt.contains("Do not edit or submit the plan, reconcile declaration metadata, or run builds"));
                    assert!(prompt.contains("Harness runs validation and conformance assessment in Checking"));
                } else if phase == PlanPhase::Verify {
                    assert!(prompt.contains("Perform only the accepted plan's manual verification actions"));
                    assert!(!prompt.contains("Do not run builds"));
                }
                if kind == PlanExecutionPromptKind::ResumeAfterInterruption {
                    assert!(prompt.contains("Do not assume interruption rolled back changes or stopped a command"));
                }
            }
        }
    }

    fn done(phase: PlanPhase, outcome: Option<VerificationOutcome>) -> PlanPhaseDone {
        PlanPhaseDone {
            phase,
            revision: 1,
            summary: "Verified requirement".into(),
            verification: outcome.map(|outcome| VerificationReport {
                outcome,
                evidence: vec!["check-result".into()],
                findings: if outcome == VerificationOutcome::Failed {
                    vec!["Round reset retains score".into()]
                } else {
                    Vec::new()
                },
                reason: if outcome == VerificationOutcome::Blocked {
                    Some("Compiler unavailable".into())
                } else {
                    None
                },
                reuse_reason: None,
                checks: Vec::new(),
            }),
        }
    }

    #[test]
    fn implementation_defers_all_conformance_to_verification() {
        let mut execution = execution();
        let mismatch = SemanticProgress { missing: vec!["main.rs".into()], ..Default::default() };
        execution.finish_phase(done(PlanPhase::Implement, None), mismatch.clone(), 1).unwrap();
        assert_eq!(execution.phase, PlanPhase::Checking);
        execution.phase=PlanPhase::Verify;
        execution.finish_phase(done(PlanPhase::Verify, Some(VerificationOutcome::Passed)), mismatch.clone(), 2).unwrap();
        assert_eq!(execution.phase, PlanPhase::Resolve);
        assert_eq!(execution.findings, ["main.rs: missing planned file"]);
        execution.finish_phase(done(PlanPhase::Resolve, None), mismatch, 3).unwrap();
        assert_eq!(execution.phase, PlanPhase::Checking);
        execution.phase=PlanPhase::Verify;
        execution.finish_phase(done(PlanPhase::Verify, Some(VerificationOutcome::Passed)), SemanticProgress::default(), 4).unwrap();
        assert_eq!(execution.state, PlanExecutionState::Complete);
        assert_eq!(execution.completed_at_ms, Some(4));
        assert!(execution.finish_phase(done(PlanPhase::Verify, Some(VerificationOutcome::Passed)), SemanticProgress::default(), 5).is_err());
    }

    #[test]
    fn stale_or_incomplete_requests_cannot_advance_and_pass_cannot_override_semantic_difference() {
        let mut execution = execution();
        let mut stale = done(PlanPhase::Implement, None);
        stale.revision = 2;
        assert!(
            execution
                .finish_phase(stale, SemanticProgress::default(), 1)
                .is_err()
        );
        assert!(
            execution
                .finish_phase(
                    done(PlanPhase::Resolve, None),
                    SemanticProgress::default(),
                    1
                )
                .is_err()
        );
        execution
            .finish_phase(
                done(PlanPhase::Implement, None),
                SemanticProgress::default(),
                2,
            )
            .unwrap();
        assert!(
            execution
                .finish_phase(
                    done(PlanPhase::Verify, None),
                    SemanticProgress::default(),
                    3
                )
                .is_err()
        );
        execution.phase=PlanPhase::Verify;
        execution
            .finish_phase(
                done(PlanPhase::Verify, Some(VerificationOutcome::Passed)),
                SemanticProgress {
                    different: vec!["Missing registration".into()],
                    ..Default::default()
                },
                4,
            )
            .unwrap();
        assert_eq!(execution.phase, PlanPhase::Resolve);
        assert_eq!(execution.state, PlanExecutionState::Active);
    }

    #[test]
    fn blocked_verification_preserves_phase_and_evidence() {
        let mut execution = execution();
        execution
            .finish_phase(
                done(PlanPhase::Implement, None),
                SemanticProgress::default(),
                1,
            )
            .unwrap();
        execution.phase=PlanPhase::Verify;
        execution
            .finish_phase(
                done(PlanPhase::Verify, Some(VerificationOutcome::Blocked)),
                SemanticProgress::default(),
                2,
            )
            .unwrap();
        assert_eq!(execution.phase, PlanPhase::Verify);
        assert_eq!(execution.state, PlanExecutionState::Blocked);
        assert_eq!(execution.verification.len(), 1);
        assert!(execution.completed_at_ms.is_none());
    }

    #[test]
    fn semantic_scan_ignores_comments_and_collects_all_shape_differences() {
        let workspace = tempfile::tempdir().unwrap();
        let path = workspace.path().join("counter.rs");
        std::fs::write(&path, "/// Accepted documentation.\npub struct Counter { pub value: i32 }\npub fn run(count: i32) {}\n").unwrap();
        let mut design = DeclarationDesign::default();
        let (file, _) = design.source(workspace.path(), "counter.rs").unwrap();
        design.proposed.insert("counter.rs".into(), file.text);
        std::fs::write(&path, "/// Changed documentation.\npub struct Counter { pub value: i32 }\npub fn run(count: i32) {}\n").unwrap();
        assert!(SemanticProgress::scan(&design, workspace.path()).unwrap().conforms());
        std::fs::write(&path, "pub struct Counter { pub value: bool }\npub fn run(count: bool) {}\n").unwrap();
        let progress = SemanticProgress::scan(&design, workspace.path()).unwrap();
        assert_eq!(progress.different.len(), 2);
    }

    #[test]
    fn semantic_scan_checks_call_relationships_and_reverted_targets() {
        let workspace = tempfile::tempdir().unwrap();
        let path = workspace.path().join("main.rs");
        std::fs::write(&path, "fn register() {}\nfn run() { register(); }\n").unwrap();
        let mut design = DeclarationDesign::default();
        let (file, calls) = design.source(workspace.path(), "main.rs").unwrap();
        design.baseline.insert("main.rs".into(), file.clone());
        design.proposed.insert("main.rs".into(), file.text);
        design.proposed_calls.insert("main.rs".into(), calls);
        assert!(
            SemanticProgress::scan(&design, workspace.path())
                .unwrap()
                .conforms()
        );
        std::fs::write(&path, "fn register() {}\nfn run() {}\n").unwrap();
        let progress = SemanticProgress::scan(&design, workspace.path()).unwrap();
        assert!(progress.conforms());
        design.proposed_calls.clear();
        std::fs::write(&path, "fn register() {}\nfn run() {}\nfn extra() {}\n").unwrap();
        assert!(
            SemanticProgress::scan(&design, workspace.path())
                .unwrap()
                .conforms(),
            "additional internal helpers must not require a plan revision"
        );
    }

    #[test]
    fn semantic_scan_compares_authored_reference_groups_without_inventing_interleaving() {
        let workspace = tempfile::tempdir().unwrap();
        let path = workspace.path().join("score.lua");
        let source = "function run(state)\n  state.points = add(state.points)\n  check(state)\n  check(state)\nend\n";
        std::fs::write(&path, source).unwrap();
        let mut design = DeclarationDesign::default();
        let (file, calls) = design.source(workspace.path(), "score.lua").unwrap();
        let editable = super::super::calls::combined("score.lua", &file.text, &calls).unwrap();
        let (declarations, authored) =
            super::super::calls::parse("score.lua", &editable, &[]).unwrap();
        assert_ne!(authored[0].call, calls[0].call);
        design.proposed.insert("score.lua".into(), declarations);
        design.proposed_calls.insert("score.lua".into(), authored);
        assert!(
            SemanticProgress::scan(&design, workspace.path())
                .unwrap()
                .conforms()
        );
        for changed in [
            source.replace("add(state.points)", "subtract(state.points)"),
            source.replacen("  check(state)\n", "", 1),
            source.replace("state.points = add(state.points)", "state.points = add(1)"),
            "function run(state)\n  check(state)\n  state.points = add(state.points)\n  check(state)\nend\n".into(),
        ] {
            std::fs::write(&path, changed).unwrap();
            assert!(SemanticProgress::scan(&design, workspace.path()).unwrap().conforms());
        }
    }

    #[test]
    fn semantic_scan_detects_source_configuration_deletions_and_parse_failure() {
        let workspace = tempfile::tempdir().unwrap();
        let mut design = DeclarationDesign::default();
        design
            .proposed
            .insert("main.rs".into(), "fn run();\n".into());
        design
            .proposed
            .insert("config.json".into(), "{\"enabled\":true}\n".into());
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

#[cfg(test)]
mod revision_policy_tests {
    use super::*;
    #[test]
    fn stale_tools_cannot_revise_outside_resolve() {
        let (sender, _receiver) = mpsc::channel(1);
        for phase in [PlanPhase::Implement, PlanPhase::Verify, PlanPhase::Resolve] {
            let control = ExecutionControlSender { phase, execution_id: "e".into(), generation: 1, sender: sender.clone() };
            assert_eq!(control.require_revision_phase().is_ok(), phase == PlanPhase::Resolve);
            let tools = crate::control_tools::ControlToolRegistry.definition_list_for(Some(phase));
            for name in ["harness_design_apply_patch", "harness_plan_submit"] {
                assert_eq!(tools.iter().any(|tool| tool.name == name), phase == PlanPhase::Resolve);
            }
        }
    }
}
