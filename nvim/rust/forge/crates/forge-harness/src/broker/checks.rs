use super::*;
use crate::checks::{CheckRun, CheckState};

impl HarnessBroker {
    /// Executes the accepted gate without admitting a model exchange.
    pub(super) async fn run_checks(&mut self) -> Result<Vec<SessionEvent>> {
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before running verification checks. Wait for or interrupt active children first"
        );
        let result = self.run_checks_inner().await;
        if let Err(error) = &result {
            if let Some(sink) = &self.event_sink {
                let _ = sink.send_wait(BackendEvent {received_at_ms:None,address:None,turn_boundary:None,
                    kind:"checks_configuration_error".into(),text:None,data:json!({"run_id":"automatic-gate","error":format!("Checking could not finish: {error:#}. Enable /checks edit on and request a correction in chat.")}),
                    activity:None,summary:None,task_update:None}).await;
            }
            let mut goal = self.active_goal()?;
            if let Some(mut execution) = self
                .store
                .list_plan_execution(&self.session.id)?
                .into_iter()
                .find(|execution| execution.goal_id == goal.id)
            {
                execution.state = PlanExecutionState::Blocked;
                execution.findings = vec![format!("Checking could not finish: {error:#}")];
                for run in &mut execution.check_runs {
                    if run.state == CheckState::Running {
                        run.state = CheckState::Blocked;
                    }
                    for command in &mut run.commands {
                        if command.state == CheckState::Running {
                            command.state = CheckState::Blocked;
                            command.error = Some(format!("{error:#}"));
                        }
                    }
                }
                goal.state = GoalState::Blocked;
                goal.updated_at_ms = self.clock.now_ms();
                self.store
                    .save_execution_transition(&execution, &goal, None, None, None)?;
                if let Some(run) = execution.check_runs.last().cloned() {
                    self.publish_check_run(&mut execution, &run).await?;
                }
            }
        }
        result
    }

    async fn run_checks_inner(&mut self) -> Result<Vec<SessionEvent>> {
        let mut goal = self.active_goal()?;
        let mut execution = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .find(|record| record.goal_id == goal.id)
            .context("check execution is missing")?;
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
            .as_ref()
            .context("accepted declarations are missing")?;

        let manual = !design.document.verification.is_empty();
        let progress = self.scan_execution_conformance(&execution, design).await?;
        let workspace_identity = self.check_workspace_identity().await?;
        let shell = self.session.shell;
        let executable = shell.executable();
        let environment_digest = crate::plan::digest(&serde_json::to_vec(
            &std::env::vars_os()
                .map(|(key, value)| {
                    (
                        key.to_string_lossy().into_owned(),
                        value.to_string_lossy().into_owned(),
                    )
                })
                .collect::<std::collections::BTreeMap<_, _>>(),
        )?);
        let task_id = self
            .current_task()?
            .filter(|task| !task.status.terminal())
            .map(|task| task.id);
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
        let files = self.check_task_files(&execution).await?;
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
        let mut enabled = overrides.enabled;
        for id in &design.document.disabled_checks {
            enabled.insert(id.clone(), false);
        }
        let mut run = self.check_runner.prepare(
            snapshot,
            crate::checks::CheckAdmission {
                owner: {
                    let exchange = self
                        .store
                        .latest_execution_exchange(&self.session.id, &execution.id)?
                        .context("plan checks require their execution exchange")?;
                    crate::timeline::TimelineOwner::exchange(&exchange)
                },
                request: crate::checks::RunRequest::Selected(crate::checks::SelectedChecks {
                    include: design.document.checks.clone(),
                    ..Default::default()
                }),
                overrides: enabled,
                files,
                cases: Some(cases),
                new_cases: Some(new_cases),
                has_scope: true,
                shell,
                trigger: "Plan execution".into(),
                task_id,
                revision: execution.revision,
            },
        )?;
        run.context.automatic = true;
        run.progress = progress;
        run.workspace_identity = workspace_identity.clone();
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
        let mut events = Vec::new();
        let mut run = self
            .check_runner
            .run(
                run,
                self.event_sink.clone(),
                Some(Arc::clone(&self.turn_cancellation.requested)),
            )
            .await?;
        let final_progress = self.scan_execution_conformance(&execution, design).await?;
        if final_progress.source_digest != run.workspace_digest
            || self.check_workspace_identity().await? != workspace_identity
        {
            run.progress = final_progress;
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
                    CheckState::Failed | CheckState::Blocked | CheckState::ConfigurationError
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
                    execution.state = PlanExecutionState::Blocked;
                    goal.state = GoalState::Blocked;
                }
                VerificationOutcome::Passed => {
                    run.state = CheckState::Passed;
                    execution.findings.clear();
                    if manual {
                        execution.phase = crate::plan::PlanPhase::Verify;
                    } else {
                        execution.state = PlanExecutionState::Complete;
                        execution.completed_at_ms = run.completed_at_ms;
                        goal.state = GoalState::Complete;
                        run.completed_plan = Some(accepted.title.clone());
                    }
                }
            }
        }
        if matches!(
            execution.state,
            PlanExecutionState::Complete | PlanExecutionState::Blocked
        ) && let Some(mut exchange) = self
            .store
            .latest_execution_exchange(&self.session.id, &execution.id)?
        {
            self.capture_implementation_report(&mut execution, &mut exchange, self.clock.now_ms())
                .await?;
            self.store.save_exchange(&exchange)?;
        }
        execution.generation += 1;
        goal.updated_at_ms = self.clock.now_ms();
        Self::save_run(&mut execution, &run);
        self.store
            .save_execution_transition(&execution, &goal, None, None, None)?;
        self.settle_task(None)?;
        self.publish_check_run(&mut execution, &run).await?;
        events.push(self.event("goal_changed", serde_json::to_value(&goal)?)?);
        Ok(events)
    }

    async fn check_workspace_identity(&mut self) -> Result<Option<String>> {
        if self
            .repositories
            .open(PathBuf::from(&self.session.workspace))
            .await?
            .is_none()
        {
            return Ok(None);
        }
        let snapshot = crate::checkpoint::GitCheckpoint::new(&self.session.workspace)
            .capture(
                &self.store.objects,
                &self.repositories,
                &self.session.id,
                self.clock.now_ms(),
            )
            .await?;
        Ok(Some(crate::plan::digest(&serde_json::to_vec(&(
            &snapshot.tree,
            &snapshot.file,
            &snapshot.deleted,
            &snapshot.checkout,
        ))?)))
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

    async fn publish_check_run(
        &mut self,
        execution: &mut PlanExecutionRecord,
        run: &CheckRun,
    ) -> Result<()> {
        Self::save_run(execution, run);
        self.store.save_plan_execution(execution)?;
        self.check_runner
            .publish(run, self.event_sink.as_ref())
            .await
    }
}
