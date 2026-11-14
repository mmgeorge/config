use super::*;
use crate::checks::{CheckRun, CheckState};

/// Retain the workspace comparison needed to validate prior check evidence.
pub(super) enum ExecutionCheckEvidenceJob {
    Invalid,
    Capture {
        workspace: super::execution::WorkspaceCaptureJob,
        expected: Option<String>,
        supervisor: Arc<crate::checks::CheckSupervisor>,
    },
}

impl ExecutionCheckEvidenceJob {
    /// Validate captured check evidence without retaining the broker lock.
    pub(super) async fn run(self) -> Result<bool> {
        match self {
            Self::Invalid => Ok(false),
            Self::Capture {
                workspace,
                expected,
                supervisor,
            } => {
                let snapshot = workspace.run().await?;
                let actual = crate::plan::digest(&serde_json::to_vec(&(
                    &snapshot.tree,
                    &snapshot.file,
                    &snapshot.deleted,
                    &snapshot.checkout,
                ))?);
                Ok(Some(actual) == expected && supervisor.verification_satisfied(&snapshot)?)
            }
        }
    }
}

/// Retain the task, accepted design and source while verification runs unlocked.
pub(crate) struct OwnedTaskChecks {
    task: crate::task::Task,
    permit: crate::task::TaskPermit,
    source: crate::task::TaskTurnId,
    input_id: crate::task::TaskInputId,
    execution: PlanExecutionRecord,
    accepted: crate::plan::PlanDocument,
    design: crate::plan::DeclarationDesign,
    interaction: Exchange,
    manual: bool,
    workspace_identity: Option<String>,
    scope: Option<crate::provider::InvocationScope>,
    attempt: CheckRun,
    deadline: crate::checks::CheckDeadline,
}

impl OwnedTaskChecks {
    /// Get the admitted history record before the initial scan starts.
    pub(super) fn attempt(&self) -> &CheckRun {
        &self.attempt
    }

    /// Preserve an admitted attempt interrupted before any command could start.
    pub(super) fn into_interrupted(self) -> (Self, CheckRun) {
        let mut run = self.attempt.clone();
        run.interrupt();
        (self, run)
    }
}

/// Scan conformance and workspace inputs without retaining broker ownership.
pub(crate) struct TaskCheckScan {
    conformance: super::execution::ExecutionConformanceJob,
    workspace: super::execution::WorkspaceCaptureJob,
    baseline: Option<crate::checkpoint::CheckpointRecord>,
    supervisor: Arc<crate::checks::CheckSupervisor>,
    deadline: Option<crate::checks::CheckDeadline>,
}

pub(crate) struct TaskCheckScanResult {
    progress: crate::plan::execution::SemanticProgress,
    workspace_identity: Option<String>,
    files: Option<Vec<String>>,
    verification_satisfied: bool,
    workspace: super::execution::ReservedWorkspace,
}

impl TaskCheckScan {
    pub(crate) async fn run(self) -> Result<TaskCheckScanResult> {
        if let Some(deadline) = self.deadline {
            deadline.check()?;
        }
        let workspace = self.workspace.run_reserved().await?;
        let progress = self.conformance.run().await?;
        if let Some(deadline) = self.deadline {
            deadline.check()?;
        }
        let snapshot = workspace.checkpoint();
        if let Some(deadline) = self.deadline {
            deadline.check()?;
        }
        let identity = crate::plan::digest(&serde_json::to_vec(&(
            &snapshot.tree,
            &snapshot.file,
            &snapshot.deleted,
            &snapshot.checkout,
        ))?);
        let files = self
            .baseline
            .map(|baseline| {
                baseline.changed_paths(snapshot).map(|paths| {
                    paths
                        .into_iter()
                        .map(|path| path.as_str().to_owned())
                        .collect()
                })
            })
            .transpose()?;
        Ok(TaskCheckScanResult {
            progress,
            workspace_identity: Some(identity),
            files,
            verification_satisfied: self.supervisor.verification_satisfied(snapshot)?,
            workspace,
        })
    }
}

/// Retain the check result and optional report capture until final state commit.
pub(crate) struct TaskCheckFinalization {
    state: OwnedTaskChecks,
    run: CheckRun,
    goal: crate::goal::GoalRecord,
    report: Option<super::execution::ImplementationReportJob>,
    workspace: super::execution::ReservedWorkspace,
}

pub(crate) struct TaskCheckReport(Option<super::execution::ImplementationReportResult>);

impl TaskCheckFinalization {
    pub(crate) async fn capture_report(&mut self) -> Result<TaskCheckReport> {
        if self.run.state == CheckState::Passed {
            self.state.deadline.check()?;
        }
        let report = TaskCheckReport(match self.report.take() {
            Some(job) => Some(job.run().await?),
            None => None,
        });
        if self.run.state == CheckState::Passed {
            self.state.deadline.check()?;
        }
        Ok(report)
    }

    /// Retain collected evidence without applying the prepared workflow transition.
    pub(super) fn into_interrupted(self) -> (OwnedTaskChecks, CheckRun) {
        let mut run = self.run;
        run.interrupt();
        run.completed_plan = None;
        (self.state, run)
    }
}

/// Publish collected interruption history without acknowledging or advancing task work.
pub(super) struct TaskCheckInterruption {
    supervisor: Arc<crate::checks::CheckSupervisor>,
    sink: publication::SessionUpdateSink,
    run: CheckRun,
}

impl TaskCheckInterruption {
    /// Persist the terminal check record before the owner releases collection gates.
    pub(super) async fn run(self) -> Result<()> {
        self.supervisor.publish(&self.run, Some(&self.sink)).await?;
        self.sink.flush().await?;
        Ok(())
    }
}

/// Publish a completed run after its durable task transition is committed.
pub(crate) struct TaskCheckPublication {
    supervisor: Arc<crate::checks::CheckSupervisor>,
    sink: Option<publication::SessionUpdateSink>,
    run: CheckRun,
    events: Vec<SessionEvent>,
    acknowledgment: TaskCheckAcknowledgment,
}

pub(crate) struct TaskCheckAcknowledgment {
    task: crate::task::Task,
    input_id: crate::task::TaskInputId,
    source: crate::task::TaskTurnId,
    run_id: Option<String>,
}

impl TaskCheckPublication {
    pub(crate) async fn run(self) -> Result<(Vec<SessionEvent>, TaskCheckAcknowledgment)> {
        self.supervisor
            .publish(&self.run, self.sink.as_ref())
            .await?;
        if let Some(sink) = &self.sink {
            sink.flush().await?;
        }
        Ok((self.events, self.acknowledgment))
    }
}

/// Own failure publication without retaining broker state during delivery.
pub(crate) struct TaskCheckFailurePublication {
    delivery: PendingProviderDelivery,
    supervisor: Arc<crate::checks::CheckSupervisor>,
    sink: Option<publication::SessionUpdateSink>,
    run: Option<CheckRun>,
    events: Vec<SessionEvent>,
    handled: bool,
    acknowledgment: Option<TaskCheckAcknowledgment>,
}

impl TaskCheckFailurePublication {
    pub(crate) async fn run(
        self,
    ) -> Result<(bool, Vec<SessionEvent>, Option<TaskCheckAcknowledgment>)> {
        let mut events = self.delivery.deliver_and_flush().await?;
        if let Some(run) = self.run {
            self.supervisor.publish(&run, self.sink.as_ref()).await?;
            if let Some(sink) = &self.sink {
                sink.flush().await?;
            }
        }
        events.extend(self.events);
        Ok((self.handled, events, self.acknowledgment))
    }
}

/// Distinguish editable gate inputs from failures in durable storage or execution infrastructure.
#[derive(Debug)]
struct CheckPreparationFailure(anyhow::Error);

impl std::fmt::Display for CheckPreparationFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "check preparation requires correction: {:#}",
            self.0
        )
    }
}

impl std::error::Error for CheckPreparationFailure {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(self.0.as_ref())
    }
}

impl CheckPreparationFailure {
    fn classify(error: anyhow::Error) -> anyhow::Error {
        if error.chain().any(|cause| cause.is::<std::io::Error>()) {
            error
        } else {
            Self(error).into()
        }
    }
}

impl HarnessBroker {
    /// Append interruption evidence using current state instead of the retired worker permit.
    pub(super) fn retain_interrupted_task_check(
        &mut self,
        state: OwnedTaskChecks,
        run: CheckRun,
        sink: publication::SessionUpdateSink,
    ) -> Result<TaskCheckInterruption> {
        anyhow::ensure!(
            state.execution.session_id == self.session.id
                && run.revision == state.execution.revision,
            "interrupted check changed its owning execution"
        );
        let current = self.task_controller.current()?;
        let active = current.as_ref().and_then(|task| {
            let crate::task::WorkflowSnapshot::Execute { execution } = &task.workflow else {
                return None;
            };
            (execution.record.id == state.execution.id).then_some((task, execution))
        });
        if let Some((task, execution)) = active {
            if task.id == state.task.id && execution.record.revision == state.execution.revision {
                let mut execution = execution.record.clone();
                Self::save_run(&mut execution, &run);
                // History collection must not retire a pause command waiting for this worker.
                self.persist_check_execution(&execution, &task.permit())?;
            }
        } else if let Some(mut execution) = self.store.load_plan_execution(&state.execution.id)?
            && execution.session_id == state.execution.session_id
            && execution.revision == state.execution.revision
        {
            // Replaced tasks retain their own history without modifying the successor task.
            Self::save_run(&mut execution, &run);
            self.store.save_plan_execution(&execution)?;
        }
        Ok(TaskCheckInterruption {
            supervisor: self.check_runner.clone(),
            sink,
            run,
        })
    }

    /// Resolve accepted checks and task overrides identically for execution and reuse.
    fn execution_check_selection(
        &self,
        design: &crate::plan::DeclarationDesign,
    ) -> Result<ExecutionCheckSelection> {
        let task_id = self
            .task_controller
            .current()?
            .map(|task| task.id.as_str().to_owned());
        let overrides = self.check_runner.overrides(task_id.as_deref())?;
        let mut snapshot = crate::checks::ConfigSnapshot::load(Path::new(&self.session.workspace))?;
        for definition in &mut snapshot.config.definitions {
            if let Some(auto) = overrides.auto.get(&definition.id) {
                definition.auto = *auto;
            }
            if let Some(scope) = overrides.scopes.get(&definition.id) {
                definition.scope = *scope;
            }
        }
        let mut enabled = overrides.enabled.clone();
        for id in &design.document.disabled_checks {
            enabled.insert(id.clone(), false);
        }
        Ok(ExecutionCheckSelection {
            task_id,
            snapshot,
            enabled,
            overrides,
        })
    }

    /// Validate passing evidence against the current contract, inputs, configuration, and shell.
    pub(super) fn prepare_execution_check_evidence(
        &self,
        execution: &PlanExecutionRecord,
        design: &crate::plan::DeclarationDesign,
    ) -> Result<ExecutionCheckEvidenceJob> {
        let Some(run) = execution.check_runs.last() else {
            return Ok(ExecutionCheckEvidenceJob::Invalid);
        };
        if run.revision != execution.revision
            || run.state != CheckState::Passed
            || run.status() != crate::plan::execution::VerificationStatus::Passed
            || run.shell != self.session.shell
            || run.executable != self.session.shell.executable().ok()
            || run.environment_digest != check_environment_digest()?
        {
            return Ok(ExecutionCheckEvidenceJob::Invalid);
        }
        let Some(context) = run.context.as_ref() else {
            return Ok(ExecutionCheckEvidenceJob::Invalid);
        };
        let selection = match self.execution_check_selection(design) {
            Ok(selection) => selection,
            Err(_) => return Ok(ExecutionCheckEvidenceJob::Invalid),
        };
        if context.snapshot != selection.snapshot
            || context.overrides != selection.enabled
            || context.task_overrides != selection.overrides
            || !run.planned_tests.as_ref().is_some_and(|planned| {
                planned.matches_request(&design.document.tests, &design.document.test_scopes)
            })
        {
            return Ok(ExecutionCheckEvidenceJob::Invalid);
        }
        Ok(ExecutionCheckEvidenceJob::Capture {
            workspace: self.prepare_workspace_capture(),
            expected: run.workspace_identity.clone(),
            supervisor: self.check_runner.clone(),
        })
    }

    /// Persist a check outcome against the still-current execution contract.
    fn persist_check_execution(
        &mut self,
        execution: &PlanExecutionRecord,
        permit: &crate::task::TaskPermit,
    ) -> Result<()> {
        let task = self
            .task_controller
            .current()?
            .context("execution task is missing")?;
        permit.validate(Some(&task))?;
        anyhow::ensure!(
            matches!(&task.workflow, crate::task::WorkflowSnapshot::Execute { execution: active } if active.record.id == execution.id),
            "execution task changed"
        );
        self.task_controller.apply(
            &mut self.store,
            &task.permit(),
            crate::task::TaskEvent::ExecutionUpdated(Box::new(execution.clone())),
            self.clock.now_ms(),
        )
    }

    fn save_run(execution: &mut PlanExecutionRecord, run: &CheckRun) {
        if let Some(saved) = execution
            .check_runs
            .iter_mut()
            .find(|saved| saved.id == run.id)
        {
            *saved = run.clone();
        } else {
            execution.check_runs.push(run.clone());
        }
    }
}

struct ExecutionCheckSelection {
    task_id: Option<String>,
    snapshot: crate::checks::ConfigSnapshot,
    enabled: std::collections::BTreeMap<String, bool>,
    overrides: crate::checks::CheckOverrides,
}

/// Fingerprint the same shell environment for execution and subsequent evidence validation.
fn check_environment_digest() -> Result<String> {
    Ok(crate::plan::digest(&serde_json::to_vec(
        &std::env::vars_os()
            .map(|(key, value)| {
                (
                    key.to_string_lossy().into_owned(),
                    value.to_string_lossy().into_owned(),
                )
            })
            .collect::<std::collections::BTreeMap<_, _>>(),
    )?))
}

impl HarnessBroker {
    /// Capture a pending verification turn and its first workspace scan.
    pub(crate) fn begin_owned_task_checks(&mut self) -> Result<(OwnedTaskChecks, TaskCheckScan)> {
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before running verification checks. Wait for or interrupt active children first"
        );
        let task = self
            .task_controller
            .current()?
            .context("check task was cleared")?;
        let turn = task
            .turn
            .as_ref()
            .context("check work has no source turn")?;
        let work = turn
            .continuation
            .as_ref()
            .context("check work was removed")?;
        anyhow::ensure!(work.work.is_checks(), "pending work is not checks");
        let permit = task.permit();
        let mut execution = self
            .capture_execution()?
            .context("execution task is missing")?
            .record;
        anyhow::ensure!(
            execution.phase == crate::plan::PlanPhase::Checking
                && execution.state == PlanExecutionState::Active,
            "check run requires an active Checking phase"
        );
        let accepted = self.plan_file.read_submitted_document(
            &self.session.id,
            &execution.plan_id,
            execution.revision,
        )?;
        let design = accepted
            .design
            .clone()
            .context("accepted declarations are missing")?;
        let interaction = self
            .store
            .latest_exchange(&self.session.id)?
            .context("plan checks require their execution exchange")?;
        let baseline = execution
            .baseline_checkpoint
            .as_ref()
            .map(|id| self.store.load_checkpoint(id))
            .transpose()?
            .flatten();
        if execution.baseline_checkpoint.is_some() {
            anyhow::ensure!(
                baseline.is_some(),
                "execution baseline checkpoint is missing"
            );
        }
        let deadline = self.check_runner.plan_deadline()?;
        let attempt = self
            .prepare_task_check_candidate(&interaction, &execution, &design, None)?
            .into_record();
        let scan = TaskCheckScan {
            conformance: self.prepare_execution_conformance(&execution, &design)?,
            workspace: self.prepare_workspace_capture(),
            baseline,
            supervisor: self.check_runner.clone(),
            deadline: Some(deadline),
        };
        let scope = interaction
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone());

        Self::save_run(&mut execution, &attempt);
        self.persist_check_execution(&execution, &permit)?;
        let state = OwnedTaskChecks {
            source: turn.id.clone(),
            input_id: work.id.clone(),
            manual: !design.document.manual_checks.is_empty(),
            permit,
            task,
            execution,
            accepted,
            design,
            interaction,
            workspace_identity: None,
            scope,
            attempt,
            deadline,
        };
        Ok((state, scan))
    }

    /// Prepare a check process after revalidating scan and input ownership.
    pub(crate) fn prepare_owned_task_check_run(
        &mut self,
        state: &mut OwnedTaskChecks,
        scan: TaskCheckScanResult,
    ) -> Result<super::check_admission::TaskCheckRun> {
        state
            .permit
            .validate(self.task_controller.current()?.as_ref())?;
        state.deadline.check()?;
        let candidate = self.prepare_task_check_candidate(
            &state.interaction,
            &state.execution,
            &state.design,
            scan.files,
        )?;
        let mut run = self
            .check_runner
            .continue_attempt(&state.attempt, candidate)?;
        run.progress = scan.progress;
        run.workspace_identity = scan.workspace_identity.clone();
        run.workspace_digest = run.progress.source_digest.clone();
        state.workspace_identity = scan.workspace_identity;
        state.attempt = run.clone().into_record();
        Self::save_run(&mut state.execution, &state.attempt);
        self.persist_check_execution(&state.execution, &state.permit)?;
        self.capture_task_check(
            &state.interaction,
            state.permit.clone(),
            run,
            state.deadline,
        )
    }

    /// Prepare selection policy without executing or publishing a check command.
    fn prepare_task_check_candidate(
        &self,
        interaction: &Exchange,
        execution: &PlanExecutionRecord,
        design: &crate::plan::DeclarationDesign,
        files: Option<Vec<String>>,
    ) -> Result<crate::checks::PreparedCheckRun> {
        let shell = self.session.shell;
        let ExecutionCheckSelection {
            task_id,
            snapshot,
            enabled,
            overrides,
        } = self
            .execution_check_selection(design)
            .map_err(CheckPreparationFailure::classify)?;
        let mut included_checks = design.document.checks.clone();
        for (id, scope) in &design.document.test_scopes {
            let definition = snapshot
                .config
                .definitions
                .iter()
                .find(|check| &check.id == id)
                .context("planned test scope has no configured check")?;
            if *scope == crate::plan::PlanTestScope::WholeSuite
                && enabled.get(id).copied().unwrap_or(definition.enabled)
            {
                included_checks.push(id.clone());
            }
        }
        included_checks.sort();
        included_checks.dedup();
        let mut run = self
            .check_runner
            .prepare(
                snapshot,
                crate::checks::CheckAdmission {
                    owner: crate::timeline::TimelineOwner::exchange(interaction),
                    request: crate::checks::RunRequest::Selected(crate::checks::SelectedChecks {
                        include: included_checks,
                        ..Default::default()
                    }),
                    overrides: enabled,
                    task_overrides: overrides,
                    files,
                    has_scope: true,
                    shell,
                    trigger: "Plan execution".into(),
                    task_id,
                    revision: execution.revision,
                },
            )
            .map_err(CheckPreparationFailure::classify)?;
        run.context.automatic = true;
        run.planned_tests = Some(crate::checks::PlannedTestExecution::new(
            design.document.tests.clone(),
            design.document.test_scopes.clone(),
        )?);
        Ok(run)
    }

    /// Capture the post-run scan and revalidate it before committing verification evidence.
    pub(crate) fn prepare_task_check_postscan(
        &self,
        state: &OwnedTaskChecks,
    ) -> Result<TaskCheckScan> {
        state
            .permit
            .validate(self.task_controller.current()?.as_ref())?;
        Ok(TaskCheckScan {
            conformance: self.prepare_execution_conformance(&state.execution, &state.design)?,
            workspace: self.prepare_workspace_capture(),
            baseline: None,
            supervisor: self.check_runner.clone(),
            // Failed and timed-out commands still need final source collection and findings.
            deadline: None,
        })
    }
}

impl HarnessBroker {
    /// Revalidate the retained execution and derive its verification outcome.
    pub(crate) fn prepare_task_check_finalization(
        &self,
        state: OwnedTaskChecks,
        result: super::check_admission::TaskCheckResult,
        scan: TaskCheckScanResult,
    ) -> Result<TaskCheckFinalization> {
        let interaction = self
            .store
            .load_exchange(&state.interaction.id)?
            .context("check exchange disappeared")?;
        let (_, mut run) = self
            .commit_task_check(&interaction, result)?
            .context("provider execution changed while checks ran")?;
        let mut execution = state.execution.clone();
        let mut goal = self.active_goal()?;
        if !scan.verification_satisfied {
            run.progress.unverified.push(
                "Required whole-check verification after source mutation remains pending".into(),
            );
        }
        if scan.workspace_identity != run.workspace_identity
            && let Some(planned) = &mut run.planned_tests
        {
            planned.invalidate("Workspace changed after test execution; run Check again".into());
        }
        if run.state == CheckState::Interrupted {
            execution.state = PlanExecutionState::Paused;
            goal.state = GoalState::Paused;
        } else {
            run.completed_at_ms = Some(self.clock.now_ms());
            execution.progress = run.progress.clone();
            execution.findings = run.findings();
            use crate::plan::execution::VerificationStatus;
            match run.status() {
                VerificationStatus::Failed => {
                    run.state = CheckState::Failed;
                    execution.phase = crate::plan::PlanPhase::Resolve;
                }
                VerificationStatus::Blocked => {
                    run.state = CheckState::Blocked;
                    execution.phase = crate::plan::PlanPhase::Resolve;
                }
                VerificationStatus::Passed => {
                    run.state = CheckState::Passed;
                    execution.findings.clear();
                    if state.manual {
                        execution.phase = crate::plan::PlanPhase::Verify;
                    } else {
                        execution.state = PlanExecutionState::Complete;
                        execution.completed_at_ms = run.completed_at_ms;
                        goal.state = GoalState::Complete;
                        run.completed_plan = Some(state.accepted.title.clone());
                    }
                }
            }
        }
        let report = if matches!(
            execution.state,
            PlanExecutionState::Complete | PlanExecutionState::Blocked
        ) {
            Some(self.prepare_implementation_report(&execution)?)
        } else {
            None
        };
        let mut state = state;
        state.execution = execution;
        state.interaction = interaction;
        Ok(TaskCheckFinalization {
            state,
            run,
            goal,
            report,
            workspace: scan.workspace,
        })
    }

    /// Reuse the original verification authority when report capture fails.
    pub(crate) fn prepare_task_check_report_failure(
        &mut self,
        finalization: &TaskCheckFinalization,
        error: &anyhow::Error,
    ) -> Result<TaskCheckFailurePublication> {
        self.prepare_owned_task_check_failure(&finalization.state, error)
    }

    /// Persist verification and advance its task continuation after report capture.
    pub(crate) fn commit_task_check_finalization(
        &mut self,
        mut finalization: TaskCheckFinalization,
        report: TaskCheckReport,
    ) -> Result<TaskCheckPublication> {
        let state = &mut finalization.state;
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution resumed before check result commit"
        );
        state
            .permit
            .validate(self.task_controller.current()?.as_ref())?;
        let current = self
            .store
            .load_exchange(&state.interaction.id)?
            .context("check exchange disappeared")?;
        anyhow::ensure!(
            state
                .scope
                .as_ref()
                .is_none_or(|scope| !self.host_settlement.is_settling(scope)),
            "provider execution changed before check commit"
        );
        anyhow::ensure!(
            current.turn.last().map(|turn| &turn.provider().scope)
                == state
                    .interaction
                    .turn
                    .last()
                    .map(|turn| &turn.provider().scope),
            "check exchange changed before commit"
        );
        let plan = self
            .store
            .load_plan(&state.execution.plan_id)?
            .context("check execution plan disappeared")?;
        if finalization.run.state == CheckState::Passed {
            let accepted = self.plan_file.read_submitted_document(
                &self.session.id,
                &plan.id,
                state.execution.revision,
            )?;
            let expected = crate::plan::digest(&serde_json::to_vec(&state.accepted)?);
            anyhow::ensure!(
                plan.accepted_revision == Some(state.execution.revision)
                    && plan.accepted_digest.as_deref() == Some(expected.as_str())
                    && crate::plan::digest(&serde_json::to_vec(&accepted)?) == expected,
                "accepted plan changed before check result commit"
            );
            let mut execution = state.execution.clone();
            Self::save_run(&mut execution, &finalization.run);
            anyhow::ensure!(
                !matches!(
                    self.prepare_execution_check_evidence(&execution, &state.design)?,
                    ExecutionCheckEvidenceJob::Invalid
                ),
                "check configuration changed before check result commit"
            );
            anyhow::ensure!(
                self.check_runner
                    .verification_satisfied(finalization.workspace.checkpoint())?,
                "required whole-check verification changed before check result commit"
            );
        }
        if let Some(report) = report.0 {
            self.commit_implementation_report(
                &mut state.execution,
                &mut state.interaction,
                report,
            )?;
        }
        state.execution.generation += 1;
        finalization.goal.updated_at_ms = self.clock.now_ms();
        Self::save_run(&mut state.execution, &finalization.run);
        self.task_controller.settle_execution(
            &mut self.store,
            &state.permit,
            crate::task::ExecutionSettlement {
                record: state.execution.clone(),
                plan,
                exchange: state.interaction.clone(),
                kind: crate::task::ExecutionSettlementKind::PhaseFinished,
                title: "Plan checks completed".into(),
                terminal_request: None,
            },
            self.clock.now_ms(),
        )?;
        let event = self.event("goal_changed", serde_json::to_value(&finalization.goal)?)?;
        let acknowledgment = TaskCheckAcknowledgment {
            task: state.task.clone(),
            input_id: state.input_id.clone(),
            source: state.source.clone(),
            run_id: Some(finalization.run.id.clone()),
        };
        Ok(TaskCheckPublication {
            supervisor: self.check_runner.clone(),
            sink: self.event_sink.clone(),
            run: finalization.run,
            events: vec![event],
            acknowledgment,
        })
    }
}

impl HarnessBroker {
    /// Record check failure under the admitting permit and return owned publication work.
    pub(crate) fn prepare_owned_task_check_failure(
        &mut self,
        state: &OwnedTaskChecks,
        error: &anyhow::Error,
    ) -> Result<TaskCheckFailurePublication> {
        state
            .permit
            .validate(self.task_controller.current()?.as_ref())?;
        let mut execution = self
            .current_execution()?
            .context("check execution disappeared")?;
        anyhow::ensure!(
            execution.id == state.execution.id,
            "check execution changed"
        );
        let mut run = self
            .check_runner
            .history()?
            .into_iter()
            .find(|run| run.id == state.attempt.id)
            .unwrap_or_else(|| state.attempt.clone());
        if run.state != CheckState::Interrupted {
            let fallback = if error.is::<CheckPreparationFailure>() {
                CheckState::ConfigurationError
            } else {
                CheckState::Blocked
            };
            run.state = crate::checks::failure_state(error, fallback);
            run.completed_at_ms = Some(self.clock.now_ms());
            run.completed_plan = None;
            for command in &mut run.commands {
                if matches!(command.state, CheckState::Pending | CheckState::Running) {
                    command.state = if command.started_at_ms.is_some() {
                        run.state
                    } else {
                        CheckState::Skipped
                    };
                    command.error = Some(format!("Check stopped: {error:#}"));
                }
            }
            run.record(
                if run.state == CheckState::ConfigurationError {
                    crate::checks::CheckEventKind::ConfigurationError
                } else {
                    crate::checks::CheckEventKind::ExecutionError
                },
                None,
                format!("Check stopped: {error:#}"),
            );
        }
        Self::save_run(&mut execution, &run);
        self.persist_check_execution(&execution, &state.permit)?;
        let mut publication = self.prepare_task_check_failure_for(
            &state.task,
            &state.permit,
            &state.source,
            &state.input_id,
            error,
        )?;
        publication.run = Some(run);
        Ok(publication)
    }

    /// Record a failure that occurred while capturing the initial verification inputs.
    pub(crate) fn prepare_task_check_start_failure(
        &mut self,
        task: &crate::task::Task,
        error: &anyhow::Error,
    ) -> Result<Option<TaskCheckFailurePublication>> {
        if self.agent_coordinator.active_count() != 0 {
            return Ok(None);
        }
        task.permit()
            .validate(self.task_controller.current()?.as_ref())?;
        anyhow::ensure!(
            matches!(
                &task.workflow,
                crate::task::WorkflowSnapshot::Execute { .. }
            ),
            "execution task is missing"
        );
        let turn = task
            .turn
            .as_ref()
            .context("check work has no source turn")?;
        let work = turn
            .continuation
            .as_ref()
            .context("check work was removed")?;
        anyhow::ensure!(work.work.is_checks(), "pending work is not checks");
        self.prepare_task_check_failure_for(task, &task.permit(), &turn.id, &work.id, error)
            .map(Some)
    }

    fn prepare_task_check_failure_for(
        &mut self,
        task: &crate::task::Task,
        permit: &crate::task::TaskPermit,
        source: &crate::task::TaskTurnId,
        input_id: &crate::task::TaskInputId,
        error: &anyhow::Error,
    ) -> Result<TaskCheckFailurePublication> {
        permit.validate(self.task_controller.current()?.as_ref())?;
        let mut events = Vec::new();
        let mut run = None;
        let handled = error.is::<CheckPreparationFailure>();
        let delivery = if handled {
            self.task_controller.apply(
                &mut self.store,
                permit,
                crate::task::TaskEvent::CheckPreparationFailed(format!("{error:#}")),
                self.clock.now_ms(),
            )?;
            let goal = self.active_goal()?;
            events.push(self.event("goal_changed", serde_json::to_value(goal)?)?);
            self.prepare_provider_delivery(AppliedProviderEvent::None)?
        } else {
            let mut execution = self
                .current_execution()?
                .context("check execution disappeared")?;
            execution.state = PlanExecutionState::Blocked;
            execution.findings = vec![format!("Checking could not finish: {error:#}")];
            for item in &mut execution.check_runs {
                if item.state == CheckState::Running {
                    item.state = CheckState::Blocked;
                }
                for command in &mut item.commands {
                    if command.state == CheckState::Running {
                        command.state = CheckState::Blocked;
                        command.error = Some(format!("{error:#}"));
                    }
                }
            }
            self.persist_check_execution(&execution, permit)?;
            run = execution.check_runs.last().cloned();
            self.prepare_provider_delivery(AppliedProviderEvent::Update(SessionUpdate::CheckFailure {
                run_id: "automatic-gate".into(),
                error: format!("Checking could not finish: {error:#}. Enable /checks edit on and request a correction in chat."),
            }))?
        };
        let acknowledgment = handled.then(|| TaskCheckAcknowledgment {
            task: task.clone(),
            input_id: input_id.clone(),
            source: source.clone(),
            run_id: None,
        });
        Ok(TaskCheckFailurePublication {
            delivery,
            supervisor: self.check_runner.clone(),
            sink: self.event_sink.clone(),
            run,
            events,
            handled,
            acknowledgment,
        })
    }
}

impl HarnessBroker {
    /// Advance only after the committed check result has been published.
    pub(crate) fn acknowledge_owned_task_check(
        &mut self,
        acknowledgment: TaskCheckAcknowledgment,
    ) -> Result<()> {
        let Some(current) = self.task_controller.current()? else {
            return Ok(());
        };
        if current.entry.as_ref().map(|entry| &entry.id)
            != acknowledgment.task.entry.as_ref().map(|entry| &entry.id)
            || !matches!((&acknowledgment.task.workflow, &current.workflow),
                (crate::task::WorkflowSnapshot::Execute { execution: original }, crate::task::WorkflowSnapshot::Execute { execution: current })
                    if original.record.id == current.record.id && acknowledgment.run_id.as_ref().is_none_or(|id| current.record.check_runs.iter().any(|run| &run.id == id)))
        {
            anyhow::bail!("check task changed before result publication");
        }
        self.task_controller.acknowledge_continuation(
            &mut self.store,
            &current.revision,
            &acknowledgment.input_id,
            self.clock.now_ms(),
        )?;
        self.reserve_execution_continuation(acknowledgment.source)
    }
}
