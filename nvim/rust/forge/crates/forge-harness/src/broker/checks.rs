use super::*;
use crate::plan::checks::{CheckRun, CheckState, CommandCheck};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

impl HarnessBroker {
    /// Executes the accepted gate without admitting a model exchange.
    pub(super) async fn run_checks(&mut self) -> Result<Vec<SessionEvent>> {
        let result = self.run_checks_inner().await;
        if let Err(error) = &result {
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
                    .save_execution_transition(&execution, &goal, None, None)?;
                if let Some(run) = execution.check_runs.last().cloned() {
                    self.publish_check_run(&mut execution, &run, &mut Vec::new())
                        .await?;
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
        let commands = design.document.checks.clone();
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
        let resume = execution
            .check_runs
            .last()
            .filter(|run| {
                run.state == CheckState::Interrupted
                    && run.revision == execution.revision
                    && run.shell == shell
                    && run.environment_digest == environment_digest
                    && run.workspace_digest == progress.source_digest
                    && run.workspace_identity == workspace_identity
                    && run.executable == executable.as_ref().ok().cloned()
            })
            .cloned();
        let mut run = if let Some(run) = resume {
            run
        } else {
            let id = Uuid::new_v4().to_string();
            let directory = self
                .plan_file
                .check_output_directory(&self.session.id, &id)?;
            CheckRun {
                id: id.clone(),
                revision: execution.revision,
                completed_plan: None,
                shell,
                executable: executable.as_ref().ok().cloned(),
                workspace: self.session.workspace.clone(),
                environment_digest,
                started_at_ms: self.clock.now_ms(),
                completed_at_ms: None,
                state: CheckState::Running,
                workspace_identity: workspace_identity.clone(),
                workspace_digest: progress.source_digest.clone(),
                progress,
                commands: commands
                    .into_iter()
                    .enumerate()
                    .map(|(index, command)| CommandCheck {
                        id: format!("{id}:{index}"),
                        command,
                        state: CheckState::Pending,
                        started_at_ms: None,
                        completed_at_ms: None,
                        exit_code: None,
                        error: None,
                        stdout: directory.join(format!("{index}.stdout")),
                        stderr: directory.join(format!("{index}.stderr")),
                        output: directory.join(format!("{index}.output")),
                        output_bytes: 0,
                        preview: String::new(),
                    })
                    .collect(),
            }
        };
        run.state = CheckState::Running;
        run.completed_at_ms = None;
        let mut events = Vec::new();
        self.publish_check_run(&mut execution, &run, &mut events)
            .await?;
        for index in 0..run.commands.len() {
            if run.commands[index].state == CheckState::Passed
                || run.commands[index].state == CheckState::Failed
            {
                continue;
            }
            if self.turn_cancellation.requested.load(Ordering::Acquire) {
                run.state = CheckState::Interrupted;
                break;
            }
            run.commands[index].started_at_ms = Some(self.clock.now_ms());
            run.commands[index].state = CheckState::Running;
            run.commands[index].output_bytes = 0;
            run.commands[index].preview.clear();
            run.commands[index].error = None;
            run.commands[index].exit_code = None;
            run.commands[index].completed_at_ms = None;
            self.publish_check_run(&mut execution, &run, &mut events)
                .await?;
            let result = match &executable {
                Ok(executable) => {
                    self.execute_check(&mut execution, &mut run, index, executable, &mut events)
                        .await
                }
                Err(error) => Err(anyhow::anyhow!("{error:#}")),
            };
            if let Err(error) = result {
                let check = &mut run.commands[index];
                check.state = CheckState::Blocked;
                check.error = Some(format!("{error:#}"));
            }
            run.commands[index].completed_at_ms = Some(self.clock.now_ms());
            self.publish_check_run(&mut execution, &run, &mut events)
                .await?;
            if run.commands[index].state == CheckState::Interrupted {
                run.state = CheckState::Interrupted;
                break;
            }
        }
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
                if matches!(check.state, CheckState::Failed | CheckState::Blocked) {
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
        ) {
            if let Some(mut exchange) = self
                .store
                .list_exchange(&self.session.id)?
                .into_iter()
                .rev()
                .find(|exchange| exchange.execution_id.as_deref() == Some(&execution.id))
            {
                self.capture_implementation_report(
                    &mut execution,
                    &mut exchange,
                    self.clock.now_ms(),
                )
                .await?;
                self.store.save_exchange(&exchange)?;
            }
        }
        execution.generation += 1;
        goal.updated_at_ms = self.clock.now_ms();
        Self::save_run(&mut execution, &run);
        self.store
            .save_execution_transition(&execution, &goal, None, None)?;
        self.publish_check_run(&mut execution, &run, &mut events)
            .await?;
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
        events: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        Self::save_run(execution, run);
        self.store.save_plan_execution(execution)?;
        let (patch, status_patch) = {
            let mut presentation = self
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("presentation lock poisoned"))?;
            let patch = presentation.update_checks(run)?;
            let status = if run.state == CheckState::Running {
                crate::session::state_machine::SessionPhase::Working {
                    started_at_ms: run.started_at_ms,
                    activity: WorkflowActivity::Checking,
                    reasoning_summary: None,
                    execution: None,
                }
            } else {
                crate::session::state_machine::SessionPhase::Idle
            };
            (patch, presentation.update_live(None, None, status)?)
        };
        self.emit_timeline_patch(patch, events).await?;
        self.emit_timeline_patch(status_patch, events).await
    }

    async fn execute_check(
        &mut self,
        execution: &mut PlanExecutionRecord,
        run: &mut CheckRun,
        index: usize,
        executable: &std::path::Path,
        events: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let check = &run.commands[index];
        let mut command = run.shell.command(executable, &check.command)?;
        command
            .current_dir(&run.workspace)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());
        let mut child = crate::process::spawn_owned(command).context("start accepted check")?;
        let mut stdout = child
            .stdout()
            .take()
            .context("check stdout pipe is missing")?;
        let mut stderr = child
            .stderr()
            .take()
            .context("check stderr pipe is missing")?;
        let mut output = tokio::fs::File::create(&check.output).await?;
        let mut stdout_file = tokio::fs::File::create(&check.stdout).await?;
        let mut stderr_file = tokio::fs::File::create(&check.stderr).await?;
        let mut stdout_buffer = [0u8; 8192];
        let mut stderr_buffer = [0u8; 8192];
        let mut stdout_done = false;
        let mut stderr_done = false;
        let mut status = None;
        let mut cancelled = false;
        let mut published_bytes = 0;
        let mut published_second = 0;
        let mut ticker = tokio::time::interval(std::time::Duration::from_millis(100));
        while !stdout_done || !stderr_done || status.is_none() {
            tokio::select! {
                result = stdout.read(&mut stdout_buffer), if !stdout_done => {
                    let bytes = result?; stdout_done = bytes == 0;
                    if bytes > 0 { stdout_file.write_all(&stdout_buffer[..bytes]).await?; output.write_all(&stdout_buffer[..bytes]).await?;
                        run.commands[index].output_bytes += bytes as u64;
                        append_preview(&mut run.commands[index].preview,&stdout_buffer[..bytes]); }
                }
                result = stderr.read(&mut stderr_buffer), if !stderr_done => {
                    let bytes = result?; stderr_done = bytes == 0;
                    if bytes > 0 { stderr_file.write_all(&stderr_buffer[..bytes]).await?; output.write_all(&stderr_buffer[..bytes]).await?;
                        run.commands[index].output_bytes += bytes as u64;
                        append_preview(&mut run.commands[index].preview,&stderr_buffer[..bytes]); }
                }
                result = child.wait(), if status.is_none() => { status = Some(result?); }
                _ = ticker.tick() => {
                    if !cancelled && self.turn_cancellation.requested.load(Ordering::Acquire) {
                        child.start_kill()?; cancelled = true;
                    }
                    let second=self.clock.now_ms()/1000;
                    if published_bytes!=run.commands[index].output_bytes || published_second!=second || cancelled {
                        output.flush().await?;
                        self.publish_check_run(execution, run, events).await?;
                        published_bytes=run.commands[index].output_bytes;
                        published_second=second;
                    }
                }
            }
        }
        output.flush().await?;
        output.sync_all().await?;
        stdout_file.sync_all().await?;
        stderr_file.sync_all().await?;
        let check = &mut run.commands[index];
        let status = status.context("check exit status is missing")?;
        check.exit_code = status.code();
        check.state = if cancelled {
            CheckState::Interrupted
        } else if status.success() {
            CheckState::Passed
        } else {
            CheckState::Failed
        };
        Ok(())
    }
}

fn append_preview(preview: &mut String, bytes: &[u8]) {
    preview.push_str(&String::from_utf8_lossy(bytes));
    let start = preview
        .match_indices('\n')
        .rev()
        .nth(4)
        .map_or(0, |(index, _)| index + 1);
    if start > 0 {
        preview.drain(..start);
    }
    if preview.len() > 8192 {
        let mut start = preview.len() - 8192;
        while !preview.is_char_boundary(start) {
            start += 1;
        }
        preview.drain(..start);
    }
}
