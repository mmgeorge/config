use super::*;
use crate::checks::{CheckRun, CheckState};

/// Retain the workspace comparison needed to validate prior check evidence.
pub(super) enum ExecutionCheckEvidenceJob {
    Invalid,
    Capture {
        workspace: super::execution::WorkspaceCaptureJob,
        expected: Option<String>,
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
            } => {
                let snapshot = workspace.run().await?;
                let actual = crate::plan::digest(&serde_json::to_vec(&(
                    &snapshot.tree,
                    &snapshot.file,
                    &snapshot.deleted,
                    &snapshot.checkout,
                ))?);
                Ok(Some(actual) == expected)
            }
        }
    }
}

/// Retain the current verification authority while evidence is checked unlocked.
pub(crate) struct VerificationGateScan {
    permit: crate::task::TaskPermit,
    source: crate::task::TaskTurnId,
    evidence: ExecutionCheckEvidenceJob,
}

impl VerificationGateScan {
    pub(crate) async fn run(self) -> Result<(Self, bool)> {
        let Self {
            permit,
            source,
            evidence,
        } = self;
        let current = evidence.run().await?;
        Ok((
            Self {
                permit,
                source,
                evidence: ExecutionCheckEvidenceJob::Invalid,
            },
            current,
        ))
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
    revision: Option<u64>,
}

/// Scan conformance and workspace inputs without retaining broker ownership.
pub(crate) struct TaskCheckScan {
    conformance: super::execution::ExecutionConformanceJob,
    workspace: super::execution::WorkspaceCaptureJob,
    baseline: Option<crate::checkpoint::CheckpointRecord>,
}

pub(crate) struct TaskCheckScanResult {
    progress: crate::plan::execution::SemanticProgress,
    workspace_identity: Option<String>,
    files: Option<Vec<String>>,
}

impl TaskCheckScan {
    pub(crate) async fn run(self) -> Result<TaskCheckScanResult> {
        let progress = self.conformance.run().await?;
        let snapshot = self.workspace.run().await?;
        let identity = crate::plan::digest(&serde_json::to_vec(&(
            &snapshot.tree,
            &snapshot.file,
            &snapshot.deleted,
            &snapshot.checkout,
        ))?);
        let files = self
            .baseline
            .map(|baseline| {
                baseline.changed_paths(&snapshot).map(|paths| {
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
        })
    }
}

/// Retain the check result and optional report capture until final state commit.
pub(crate) struct TaskCheckFinalization {
    state: OwnedTaskChecks,
    run: CheckRun,
    goal: crate::goal::GoalRecord,
    report: Option<super::execution::ImplementationReportJob>,
}

pub(crate) struct TaskCheckReport(Option<super::execution::ImplementationReportResult>);

impl TaskCheckFinalization {
    pub(crate) async fn capture_report(&mut self) -> Result<TaskCheckReport> {
        Ok(TaskCheckReport(match self.report.take() {
            Some(job) => Some(job.run().await?),
            None => None,
        }))
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
    /// Revalidate automated proof before admitting any manual verification turn.
    #[cfg(test)]
    pub(crate) async fn refresh_verification_gate(&mut self) -> Result<()> {
        let Some(task) = self.task_controller.current()? else {
            return Ok(());
        };
        let crate::task::WorkflowSnapshot::Execute { execution } = &task.workflow else {
            return Ok(());
        };
        if execution.record.phase != crate::plan::PlanPhase::Verify
            || execution.record.state != PlanExecutionState::Active
        {
            return Ok(());
        }
        let document = self.plan_file.read_submitted_document(
            &self.session.id,
            &execution.record.plan_id,
            execution.record.revision,
        )?;
        let design = document
            .design
            .as_ref()
            .context("accepted execution design is missing")?;
        if self
            .execution_checks_are_current(&execution.record, design)
            .await?
        {
            return Ok(());
        }
        let source = match &task.turn {
            Some(turn) => turn.id.clone(),
            None => {
                let exchange = self
                    .store
                    .latest_exchange(&self.session.id)?
                    .context("verification source is missing")?;
                crate::task::TaskTurnId {
                    exchange_ordinal: exchange.ordinal,
                    turn_ordinal: exchange.turn.len(),
                }
            }
        };
        self.task_controller.apply(
            &mut self.store,
            &task.permit(),
            crate::task::TaskEvent::VerificationInputsChanged(source),
            self.clock.now_ms(),
        )?;
        Ok(())
    }

    /// Drive test fixtures through the same captured verification stages as the service.
    #[cfg(test)]
    pub(super) async fn run_checks(&mut self) -> Result<Vec<SessionEvent>> {
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before running verification checks. Wait for or interrupt active children first"
        );
        let task = self
            .task_controller
            .current()?
            .context("check task was cleared")?;
        let pending = task
            .turn
            .as_ref()
            .and_then(|turn| turn.continuation.as_ref())
            .is_some_and(|work| {
                work.work.is_checks() && work.delivery == crate::task::TaskInputDelivery::Pending
            });
        if !pending {
            let exchange = self
                .store
                .latest_exchange(&self.session.id)?
                .context("check fixture has no exchange")?;
            let source = match &task.turn {
                Some(turn) => crate::task::TaskTurnId {
                    exchange_ordinal: turn.id.exchange_ordinal,
                    turn_ordinal: turn
                        .id
                        .turn_ordinal
                        .checked_add(1)
                        .context("check fixture turn exhausted")?,
                },
                None => crate::task::TaskTurnId {
                    exchange_ordinal: exchange.ordinal,
                    turn_ordinal: exchange.turn.len(),
                },
            };
            self.task_controller.reserve_checks(
                &mut self.store,
                &task.permit(),
                source,
                self.clock.now_ms(),
            )?;
        }
        let task = self
            .task_controller
            .current()?
            .context("check task disappeared")?;
        let result = async {
            let (mut state, scan) = self.begin_owned_task_checks()?;
            let scan = scan.run().await?;
            let run = self.prepare_owned_task_check_run(&mut state, scan)?;
            let result = run.run().await?;
            let scan = self.prepare_task_check_postscan(&state)?.run().await?;
            let mut finalization = self.prepare_task_check_finalization(state, result, scan)?;
            let report = finalization.capture_report().await?;
            let publication = self.commit_task_check_finalization(finalization, report)?;
            let (events, acknowledgment) = publication.run().await?;
            self.acknowledge_owned_task_check(acknowledgment)?;
            Ok::<_, anyhow::Error>(events)
        }
        .await;
        match result {
            Ok(events) => Ok(events),
            Err(error) => {
                let Some(publication) = self.prepare_task_check_start_failure(&task, &error)?
                else {
                    return Err(error);
                };
                let (handled, events, acknowledgment) = publication.run().await?;
                if let Some(acknowledgment) = acknowledgment {
                    self.acknowledge_owned_task_check(acknowledgment)?;
                }
                if handled { Ok(events) } else { Err(error) }
            }
        }
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
        let mut enabled = overrides.enabled;
        for id in &design.document.disabled_checks {
            enabled.insert(id.clone(), false);
        }
        Ok(ExecutionCheckSelection {
            task_id,
            snapshot,
            enabled,
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
            || run.outcome() != crate::plan::execution::VerificationOutcome::Passed
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
        if context.snapshot != selection.snapshot || context.overrides != selection.enabled {
            return Ok(ExecutionCheckEvidenceJob::Invalid);
        }
        Ok(ExecutionCheckEvidenceJob::Capture {
            workspace: self.prepare_workspace_capture(),
            expected: run.workspace_identity.clone(),
        })
    }

    /// Validate the latest passing checks against the current workspace.
    #[cfg(test)]
    pub(super) async fn execution_checks_are_current(
        &mut self,
        execution: &PlanExecutionRecord,
        design: &crate::plan::DeclarationDesign,
    ) -> Result<bool> {
        self.prepare_execution_check_evidence(execution, design)?
            .run()
            .await
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
    pub(crate) fn begin_owned_task_checks(&self) -> Result<(OwnedTaskChecks, TaskCheckScan)> {
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
        let execution = self
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
        let scan = TaskCheckScan {
            conformance: self.prepare_execution_conformance(&execution, &design)?,
            workspace: self.prepare_workspace_capture(),
            baseline,
        };
        let scope = interaction
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone());
        let revision = scope
            .as_ref()
            .map(|scope| self.reopened_execution.revision(scope));
        let state = OwnedTaskChecks {
            source: turn.id.clone(),
            input_id: work.id.clone(),
            manual: !design.document.verification.is_empty(),
            permit,
            task,
            execution,
            accepted,
            design,
            interaction,
            workspace_identity: None,
            scope,
            revision,
        };
        Ok((state, scan))
    }

    /// Prepare a check process after revalidating scan and input ownership.
    pub(crate) fn prepare_owned_task_check_run(
        &self,
        state: &mut OwnedTaskChecks,
        scan: TaskCheckScanResult,
    ) -> Result<super::check_admission::TaskCheckRun> {
        state
            .permit
            .validate(self.task_controller.current()?.as_ref())?;
        let execution = &state.execution;
        let design = &state.design;
        let shell = self.session.shell;
        let executable = shell.executable();
        let environment_digest = check_environment_digest()?;
        let ExecutionCheckSelection {
            task_id,
            snapshot,
            enabled,
        } = self
            .execution_check_selection(design)
            .map_err(CheckPreparationFailure::classify)?;
        let cases = design
            .document
            .tests
            .iter()
            .flat_map(|file| {
                file.cases
                    .iter()
                    .filter(|case| {
                        matches!(
                            case.change,
                            crate::plan::DesignTestChange::New
                                | crate::plan::DesignTestChange::Modified
                        )
                    })
                    .map(|case| crate::checks::CheckCase {
                        file: file.file.clone(),
                        name: case.name.clone(),
                        target: case.target.clone(),
                    })
            })
            .collect();
        let new_cases = design
            .document
            .tests
            .iter()
            .flat_map(|file| {
                file.cases
                    .iter()
                    .filter(|case| case.change == crate::plan::DesignTestChange::New)
                    .map(|case| crate::checks::CheckCase {
                        file: file.file.clone(),
                        name: case.name.clone(),
                        target: case.target.clone(),
                    })
            })
            .collect();
        let mut run = self
            .check_runner
            .prepare(
                snapshot,
                crate::checks::CheckAdmission {
                    owner: crate::timeline::TimelineOwner::exchange(&state.interaction),
                    request: crate::checks::RunRequest::Selected(crate::checks::SelectedChecks {
                        include: design.document.checks.clone(),
                        ..Default::default()
                    }),
                    overrides: enabled,
                    files: scan.files,
                    cases: Some(cases),
                    new_cases: Some(new_cases),
                    has_scope: true,
                    shell,
                    trigger: "Plan execution".into(),
                    task_id,
                    revision: execution.revision,
                },
            )
            .map_err(CheckPreparationFailure::classify)?;
        run.context.automatic = true;
        run.progress = scan.progress;
        run.workspace_identity = scan.workspace_identity.clone();
        run.workspace_digest = run.progress.source_digest.clone();
        let recovered = if execution.check_runs.is_empty() {
            self.check_runner
                .history()?
                .into_iter()
                .rev()
                .find(|previous| {
                    previous
                        .context
                        .as_ref()
                        .zip(Some(&run.context))
                        .is_some_and(|(previous, current)| {
                            previous.trigger == "Plan execution"
                                && previous.task_id == current.task_id
                        })
                })
        } else {
            None
        };
        let previous = execution
            .check_runs
            .last()
            .or(recovered.as_ref())
            .filter(|previous| {
                previous.state == CheckState::Interrupted
                    && previous.revision == run.revision
                    && previous.shell == shell
                    && previous.environment_digest == environment_digest
                    && previous.workspace_digest == run.workspace_digest
                    && previous.workspace_identity == run.workspace_identity
                    && previous.executable == executable.as_ref().ok().cloned()
                    && previous
                        .context
                        .as_ref()
                        .zip(Some(&run.context))
                        .is_some_and(|(previous, current)| {
                            previous.snapshot == current.snapshot
                                && previous.overrides == current.overrides
                        })
            });
        if let Some(previous) = previous {
            run = previous.clone().try_into()?;
            run.state = CheckState::Running;
            run.completed_at_ms = None;
            run.record(
                crate::checks::CheckEventKind::Resumed,
                None,
                "Checks resumed".into(),
            );
        }
        state.workspace_identity = scan.workspace_identity;
        self.capture_task_check(&state.interaction, state.permit.clone(), run)
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
        if scan.progress.source_digest != run.workspace_digest
            || scan.workspace_identity != state.workspace_identity
        {
            run.progress = scan.progress;
            run.progress.unverified.push("Workspace changed while checks ran. Run Checking again against the current source.".into());
        }
        if run.state == CheckState::Interrupted {
            execution.state = PlanExecutionState::Paused;
            goal.state = GoalState::Paused;
        } else {
            run.completed_at_ms = Some(self.clock.now_ms());
            execution.progress = run.progress.clone();
            execution.findings = run.progress.findings();
            for check in &run.commands {
                if matches!(
                    check.state,
                    CheckState::Failed
                        | CheckState::Blocked
                        | CheckState::ConfigurationError
                        | CheckState::ExecutionError
                        | CheckState::Stale
                ) {
                    execution.findings.push(format!(
                        "{}: {:?}, exit {:?}. {}\nSaved output: {}",
                        check.command,
                        check.state,
                        check.exit_code,
                        check.error.as_deref().unwrap_or(""),
                        check.output.display()
                    ));
                }
            }
            use crate::plan::execution::VerificationOutcome;
            match run.outcome() {
                VerificationOutcome::Failed => {
                    run.state = CheckState::Failed;
                    execution.phase = crate::plan::PlanPhase::Resolve;
                }
                VerificationOutcome::Blocked => {
                    run.state = CheckState::Blocked;
                    execution.phase = crate::plan::PlanPhase::Resolve;
                }
                VerificationOutcome::Passed => {
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
            state.scope.as_ref().is_none_or(|scope| state.revision
                == Some(self.reopened_execution.revision(scope))
                && !self.reopened_execution.active(scope)
                && !self.reopened_execution.is_settling(scope)),
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
        if let Some(report) = report.0 {
            self.commit_implementation_report(
                &mut state.execution,
                &mut state.interaction,
                report,
            )?;
            self.store.save_exchange(&state.interaction)?;
        }
        state.execution.generation += 1;
        finalization.goal.updated_at_ms = self.clock.now_ms();
        Self::save_run(&mut state.execution, &finalization.run);
        self.persist_check_execution(&state.execution, &state.permit)?;
        if state.execution.state == PlanExecutionState::Complete {
            let task = self
                .task_controller
                .current()?
                .context("completed execution disappeared")?;
            self.task_controller.finish(
                &mut self.store,
                &task.permit(),
                crate::task::TaskOutcomeState::Complete,
                "Accepted plan completed".into(),
                self.clock.now_ms(),
            )?;
        }
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
    /// Capture current verification evidence without holding the broker during workspace I/O.
    pub(crate) fn prepare_verification_gate(&self) -> Result<Option<VerificationGateScan>> {
        let Some(task) = self.task_controller.current()? else {
            return Ok(None);
        };
        let crate::task::WorkflowSnapshot::Execute { execution } = &task.workflow else {
            return Ok(None);
        };
        if execution.record.phase != crate::plan::PlanPhase::Verify
            || execution.record.state != PlanExecutionState::Active
        {
            return Ok(None);
        }
        let document = self.plan_file.read_submitted_document(
            &self.session.id,
            &execution.record.plan_id,
            execution.record.revision,
        )?;
        let design = document
            .design
            .as_ref()
            .context("accepted execution design is missing")?;
        let source = match &task.turn {
            Some(turn) => turn.id.clone(),
            None => {
                let exchange = self
                    .store
                    .latest_exchange(&self.session.id)?
                    .context("verification source is missing")?;
                crate::task::TaskTurnId {
                    exchange_ordinal: exchange.ordinal,
                    turn_ordinal: exchange.turn.len(),
                }
            }
        };
        Ok(Some(VerificationGateScan {
            permit: task.permit(),
            source,
            evidence: self.prepare_execution_check_evidence(&execution.record, design)?,
        }))
    }

    pub(crate) fn commit_verification_gate(
        &mut self,
        scan: VerificationGateScan,
        current: bool,
    ) -> Result<()> {
        scan.permit
            .validate(self.task_controller.current()?.as_ref())?;
        if !current {
            self.task_controller.apply(
                &mut self.store,
                &scan.permit,
                crate::task::TaskEvent::VerificationInputsChanged(scan.source),
                self.clock.now_ms(),
            )?;
        }
        Ok(())
    }
}

impl HarnessBroker {
    /// Record check failure under the admitting permit and return owned publication work.
    pub(crate) fn prepare_owned_task_check_failure(
        &mut self,
        state: &OwnedTaskChecks,
        error: &anyhow::Error,
    ) -> Result<TaskCheckFailurePublication> {
        self.prepare_task_check_failure_for(
            &state.task,
            &state.permit,
            &state.source,
            &state.input_id,
            error,
        )
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
