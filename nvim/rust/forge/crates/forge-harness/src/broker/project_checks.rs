use super::*;

impl HarnessBroker {
    /// Refreshes the read-only check projection before acceptance without altering design metadata.
    pub(super) fn refresh_check_review(&mut self, plan_id: &str) -> Result<bool> {
        let mut plan = self
            .store
            .load_plan(plan_id)?
            .context("plan record is missing")?;
        if plan.state != PlanState::AwaitingReview {
            return Ok(false);
        }
        let mut document = self
            .plan_file
            .read_working_document(&self.session.id, plan_id)?;
        let Some(design) = &mut document.design else {
            return Ok(false);
        };
        let mut snapshot = crate::checks::ConfigSnapshot::load(Path::new(&self.session.workspace))?;
        let task = self.current_task()?.filter(|task| !task.status.terminal());
        let overrides = self
            .check_runner
            .overrides(task.as_ref().map(|task| task.id.as_str()))?;
        for definition in &mut snapshot.config.definitions {
            if let Some(enabled) = overrides.enabled.get(&definition.id) {
                definition.enabled = *enabled;
            }
            if let Some(scope) = overrides.scopes.get(&definition.id) {
                definition.scope = *scope;
            }
        }
        if design.check_configuration.as_ref() == Some(&snapshot) {
            return Ok(false);
        }
        design.check_configuration = Some(snapshot);
        let rendered = self.plan_file.render_review(&document)?;
        let (_, _, digest) = self.plan_file.persist_review_draft(
            &self.session.id,
            plan_id,
            document,
            rendered,
            None,
        )?;
        plan.submitted_version = None;
        plan.review_digest = Some(digest);
        plan.acceptance = None;
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        Ok(true)
    }

    pub(super) async fn project_checks(
        &mut self,
        method: HarnessMethod,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        self.check_runner.refresh_changes().await?;
        let task_id = self
            .current_task()?
            .filter(|task| !task.status.terminal())
            .map(|task| task.id);
        let result = match method {
            HarnessMethod::ChecksList => self.check_runner.catalog(task_id.as_deref())?,
            HarnessMethod::ChecksOverride => {
                let overrides = serde_json::from_value(params)?;
                self.check_runner
                    .set_overrides(task_id.as_deref(), overrides)?
            }
            HarnessMethod::ChecksRun => {
                anyhow::ensure!(
                    self.agent_coordinator.active_count() == 0
                        && !self.current_task_is_plan()?
                        && !self
                            .store
                            .list_plan_execution(&self.session.id)?
                            .iter()
                            .any(|execution| execution.state == PlanExecutionState::Active),
                    "checks cannot race active plan execution or writing agents"
                );
                let run = self.check_runner.prepare_request(
                    serde_json::from_value(params)?,
                    task_id,
                    self.session.shell,
                    "User requested".into(),
                )?;
                json!({"run_id":self.check_runner.start(run, self.event_sink.clone(),None).await?})
            }
            HarnessMethod::ChecksStatus => serde_json::to_value(
                self.check_runner
                    .status(
                        &required_text(&params, "run_id")?,
                        params["wait_ms"].as_u64().unwrap_or(0),
                    )
                    .await?,
            )?,
            HarnessMethod::ChecksCancel => {
                let id = required_text(&params, "run_id")?;
                self.check_runner.cancel(&id)?;
                json!({"run_id":id,"cancellation_requested":true})
            }
            HarnessMethod::ChecksSaveDefaults => {
                self.check_runner
                    .save_defaults(task_id.as_deref(), self.event_sink.as_ref())
                    .await?
            }
            _ => anyhow::bail!("unknown checks operation"),
        };
        Ok((result, Vec::new()))
    }

    pub(super) async fn setup_checks(
        &mut self,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        if self.agent_coordinator.active_count() != 0 || self.check_runner.active()?.is_some() {
            return Err(
                RequestNotAdmitted("wait for active checks and agents before setup").into(),
            );
        }
        if self.active_plan_awaits_input()? || self.active_plan_has_acceptance()? {
            return Err(RequestNotAdmitted(
                "answer the active plan questions before starting separate check setup",
            )
            .into());
        }
        let prompt = include_str!("../checks/setup.md");
        let selected = params
            .get("check_id")
            .and_then(Value::as_str)
            .unwrap_or("all definitions");
        let admission =
            ExchangeAdmission::chat(params.get("check_id").and_then(Value::as_str).map_or_else(
                || "/checks setup".into(),
                |id| format!("/checks setup {id}"),
            ));
        let plan = self.current_plan_id()?
            .as_ref()
            .map(|id| self.plan_file.read_working_document(&self.session.id, id))
            .transpose()?
            .map(|document| serde_json::to_string(&document))
            .transpose()?
            .unwrap_or_else(|| "No active plan.".into());
        self.check_setup = true;
        let result = self.run_interaction(format!("Configure project checks through harness_checks_configure. Focus: {selected}.\n\nActive proposed plan (read-only context):\n{plan}\n\n{prompt}"), PromptMode::Chat, Some(admission)).await;
        self.check_setup = false;
        if result.is_ok()
            && crate::checks::ConfigSnapshot::load(Path::new(&self.session.workspace)).is_ok()
            && let Some(plan_id) = self.current_plan_id()?
        {
            self.refresh_check_review(&plan_id)?;
        }
        result
    }

    pub(super) async fn check_task_files(
        &mut self,
        execution: &PlanExecutionRecord,
    ) -> Result<Option<Vec<String>>> {
        let Some(id) = &execution.baseline_checkpoint else {
            return Ok(None);
        };
        let baseline = self.store.load_checkpoint(id)?
            .context("execution baseline checkpoint is missing")?;
        let current = GitCheckpoint::new(&self.session.workspace)
            .capture(&self.store.objects, &self.repositories, &self.session.id, self.clock.now_ms())
            .await?;
        Ok(Some(baseline.changed_paths(&current)?.into_iter()
            .map(|path| path.as_str().to_owned()).collect()))
    }
}
