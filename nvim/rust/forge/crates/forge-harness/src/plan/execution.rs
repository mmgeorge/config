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
    Verify,
    Resolve,
}

impl PlanPhase {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Implement => "Plan implementation",
            Self::Verify => "Plan verification",
            Self::Resolve => "Plan resolution",
        }
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
    pub evidence: Vec<String>,
    #[serde(default)]
    pub findings: Vec<String>,
    #[serde(default)]
    pub reason: Option<String>,
    #[serde(default)]
    pub reuse_reason: Option<String>,
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
                (Some(_), None) => progress.missing.push(path),
                (Some(expected), Some(_)) => match design.source(workspace, &path) {
                    Err(error) => progress.unverified.push(format!("{path}: {error:#}")),
                    Ok((actual, calls)) => {
                        if progress.source_digest.get(&path) != Some(&actual.source_digest) {
                            progress
                                .unverified
                                .push(format!("{path}: source changed during comparison"));
                            continue;
                        }
                        let expected_text = expected.replace("\r\n", "\n");
                        let actual_text = actual.text.replace("\r\n", "\n");
                        let declarations_match = expected_text.trim() == actual_text.trim();
                        let mut calls_match = true;
                        let mut call_difference = None;
                        for planned in design.proposed_calls.get(&path).into_iter().flatten() {
                            let Some(expected_calls) = &planned.call else {
                                progress.warning.push(format!(
                                    "{path}: {} has no planned call evidence",
                                    planned.owner
                                ));
                                continue;
                            };
                            let actual_calls = calls
                                .iter()
                                .find(|body| body.owner == planned.owner)
                                .and_then(|body| body.call.as_ref());
                            let owner_matches = actual_calls.is_some_and(|actual_calls| {
                                [
                                    super::calls::CallKind::Call,
                                    super::calls::CallKind::Property,
                                ]
                                .into_iter()
                                .all(|category| {
                                    expected_calls
                                        .iter()
                                        .filter(|call| call.kind.category() == category)
                                        .map(|call| call.name.as_str())
                                        .eq(actual_calls
                                            .iter()
                                            .filter(|call| call.kind.category() == category)
                                            .map(|call| call.name.as_str()))
                                })
                            });
                            calls_match &= owner_matches;
                            if !owner_matches && call_difference.is_none() {
                                let observed = actual_calls.map(Vec::as_slice).unwrap_or_default();
                                let preview = |references: &[super::calls::CallSite]| {
                                    [("Calls", super::calls::CallKind::Call),
                                     ("Accesses", super::calls::CallKind::Property)]
                                        .into_iter()
                                        .map(|(label, category)| {
                                            let names = references.iter()
                                                .filter(|reference| reference.kind.category() == category)
                                                .map(|reference| reference.name.as_str())
                                                .collect::<Vec<_>>();
                                            format!("{label} {} [{}]", names.len(),
                                                names.into_iter().take(32).collect::<Vec<_>>().join(", "))
                                        })
                                        .collect::<Vec<_>>().join(", ")
                                };
                                let expected_preview = preview(expected_calls);
                                let observed_preview = preview(observed);
                                call_difference = Some(format!(
                                    "planned call relationships differ for {}: expected {}; observed {}. Compare each group separately and author one block per group. Cross-group interleaving is not required.",
                                    planned.owner,
                                    expected_preview,
                                    observed_preview
                                ));
                            }
                        }
                        for call in calls
                            .iter()
                            .flat_map(|body| body.call.iter().flatten())
                            .filter(|call| call.unresolved)
                        {
                            progress
                                .warning
                                .push(format!("{path}: unresolved reference {}", call.name));
                        }
                        if declarations_match && calls_match {
                            progress.matched.push(path);
                        } else {
                            let difference = if !declarations_match {
                                let expected_line = expected_text.trim().lines().collect::<Vec<_>>();
                                let actual_line = actual_text.trim().lines().collect::<Vec<_>>();
                                let row = (0..expected_line.len().max(actual_line.len()))
                                    .find(|row| expected_line.get(*row) != actual_line.get(*row))
                                    .expect("different declarations have a differing line");
                                let preview = |line: Option<&&str>| line.map_or_else(
                                    || "<end of file>".into(),
                                    |line| line.chars().take(240).collect::<String>(),
                                );
                                format!("declarations or configuration differ at overview line {}: expected {:?}; observed {:?}. Correct the source to match the accepted overview before considering a design revision.",
                                    row + 1, preview(expected_line.get(row)), preview(actual_line.get(row)))
                            } else {
                                call_difference.unwrap_or_else(|| "planned call relationships differ".into())
                            };
                            progress.different.push(format!("{path}: {difference}"));
                        }
                    }
                },
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
    pub planning_backend_session_id: Option<String>,
    pub execution_backend_session_id: Option<String>,
    pub lifecycle: Vec<PlanExecutionLifecycleRecord>,
    pub findings: Vec<String>,
    pub progress: SemanticProgress,
    pub verification: Vec<VerificationEvidence>,
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
        if self.phase != PlanPhase::Verify {
            ensure!(
                request.verification.is_none(),
                "verification assessment is only accepted in Verify"
            );
            ensure!(
                progress.conforms(),
                "Semantic comparison failed. Stay in {:?}. Prefer correcting implementation to accepted revision {}. Otherwise submit a justified plan revision. Differences: {}",
                self.phase,
                self.revision,
                progress.findings().join("\n")
            );
            self.phase = PlanPhase::Verify;
        } else {
            let report = request
                .verification
                .context("Verify requires a verification assessment")?;
            match report.outcome {
                VerificationOutcome::Passed => {
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
                            .any(|finding| !finding.trim().is_empty()),
                        "failed verification requires concrete findings"
                    );
                    self.phase = PlanPhase::Resolve;
                    self.findings = report.findings.clone();
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
                "Implement the entire accepted semantic design in your chosen order, including the new, modified, and removed tests listed in plan.json tests. Preserve reused tests. Keep the inventory synchronized through a plan revision if test names, locations, or intended coverage change. Do not run builds, compiler checks, tests, linters, format checks, or manual/runtime verification in Implement, including targeted or preliminary checks. Defer all such checks to Verify even when commands appear in the accepted plan or earlier conversation. Use source inspection and Harness semantic comparison to complete Implement, then call harness_plan_phase_done so Harness can advance to Verify."
            }
            PlanPhase::Resolve => {
                "Correct the recorded findings while matching the accepted design."
            }
            PlanPhase::Verify => {
                "Read plan.json with harness_plan_read and verify its verification requirements and tests inventory. Confirm each new, modified, or reused test is covered by the executed checks, and confirm removed tests were intentionally removed. Do not treat a listed test as evidence that it ran. Run each nonblank line of verification.automated as a separate command in the project workspace, in listed order, using normal execution tools and permissions. Perform every verification.manual check and record the observed result. Report blocked when a required check cannot be performed, including checks requiring user action. Do not claim passed with outstanding checks. After running checks, call harness_plan_read with only plan_id to retrieve execution.verification_evidence. Use its exact tool IDs in verification.evidence, not command descriptions or output text. Select affected checks to rerun and justify any reused evidence. Report passed, failed, or blocked with evidence and concrete findings."
            }
        };
        format!(
            "{work} Phase: {:?}. Accepted revision: {}. Read the target with harness_plan_read. Call harness_plan_phase_done with this phase and revision when its work is finished, then end the turn after success. Harness commits the transition and supplies the next phase. Ending a turn alone does not end the phase. Prefer matching the plan. If a design change is necessary, use harness_design_apply_patch and harness_plan_submit with a revision reason. Submission waits for automatic acceptance or the user's review decision. Continue in this same turn using the returned revision and feedback. Do not call harness_goal_complete for this execution.",
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
    pub execution_id: String,
    pub generation: u64,
    pub sender: mpsc::Sender<ExecutionControl>,
}

impl ExecutionControlSender {
    /// Await a committed broker result before acknowledging the provider's request.
    pub async fn send(&self, operation: ExecutionOperation) -> Result<Value> {
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
            original_revision: 1,
            revision: 1,
            generation: 0,
            planning_backend_session_id: None,
            execution_backend_session_id: None,
            lifecycle: Vec::new(),
            findings: Vec::new(),
            progress: SemanticProgress::default(),
            verification: Vec::new(),
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
                    assert!(prompt.contains("Do not run builds, compiler checks, tests, linters, format checks, or manual/runtime verification in Implement, including targeted or preliminary checks."));
                    assert!(prompt.contains("Defer all such checks to Verify even when commands appear in the accepted plan or earlier conversation."));
                } else if phase == PlanPhase::Verify {
                    assert!(prompt.contains("Run each nonblank line of verification.automated"));
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
            }),
        }
    }

    #[test]
    fn semantic_rejection_preserves_phase_then_resolution_returns_to_verification() {
        let mut execution = execution();
        let mismatch = SemanticProgress {
            missing: vec!["main.rs".into()],
            ..Default::default()
        };
        assert!(
            execution
                .finish_phase(done(PlanPhase::Implement, None), mismatch.clone(), 1)
                .unwrap_err()
                .to_string()
                .contains("Prefer correcting")
        );
        assert_eq!(execution.phase, PlanPhase::Implement);
        assert_eq!(execution.generation, 0);
        execution
            .finish_phase(
                done(PlanPhase::Implement, None),
                SemanticProgress::default(),
                2,
            )
            .unwrap();
        execution
            .finish_phase(
                done(PlanPhase::Verify, Some(VerificationOutcome::Failed)),
                SemanticProgress::default(),
                3,
            )
            .unwrap();
        assert_eq!(execution.phase, PlanPhase::Resolve);
        assert_eq!(execution.findings, ["Round reset retains score"]);
        assert!(
            execution
                .finish_phase(done(PlanPhase::Resolve, None), mismatch, 4)
                .is_err()
        );
        execution
            .finish_phase(
                done(PlanPhase::Resolve, None),
                SemanticProgress::default(),
                5,
            )
            .unwrap();
        assert_eq!(execution.phase, PlanPhase::Verify);
        execution
            .finish_phase(
                done(PlanPhase::Verify, Some(VerificationOutcome::Passed)),
                SemanticProgress::default(),
                6,
            )
            .unwrap();
        assert_eq!(execution.state, PlanExecutionState::Complete);
        assert_eq!(execution.completed_at_ms, Some(6));
        assert!(
            execution
                .finish_phase(
                    done(PlanPhase::Verify, Some(VerificationOutcome::Passed)),
                    SemanticProgress::default(),
                    7
                )
                .is_err()
        );
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
    fn semantic_scan_reports_the_first_expected_and_observed_declaration() {
        let workspace = tempfile::tempdir().unwrap();
        let path = workspace.path().join("counter.rs");
        std::fs::write(&path, "/// Accepted documentation.\npub struct Counter { pub value: i32 }\n").unwrap();
        let mut design = DeclarationDesign::default();
        let (file, _) = design.source(workspace.path(), "counter.rs").unwrap();
        design.proposed.insert("counter.rs".into(), file.text);
        std::fs::write(&path, "/// Changed documentation.\npub struct Counter { pub value: i32 }\n").unwrap();
        let progress = SemanticProgress::scan(&design, workspace.path()).unwrap();
        assert!(!progress.conforms());
        assert!(progress.different[0].contains("overview line 1"));
        assert!(progress.different[0].contains("expected \"/// Accepted documentation.\""));
        assert!(progress.different[0].contains("observed \"/// Changed documentation.\""));
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
        assert_eq!(
            progress.different,
            [
                "main.rs: planned call relationships differ for run: expected Calls 1 [register], Accesses 0 []; observed Calls 0 [], Accesses 0 []. Compare each group separately and author one block per group. Cross-group interleaving is not required."
            ]
        );
        design.proposed_calls.clear();
        std::fs::write(&path, "fn register() {}\nfn run() {}\nfn extra() {}\n").unwrap();
        assert!(
            !SemanticProgress::scan(&design, workspace.path())
                .unwrap()
                .conforms(),
            "reverting a target to baseline must still validate the file"
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
            assert!(!SemanticProgress::scan(&design, workspace.path()).unwrap().conforms());
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
