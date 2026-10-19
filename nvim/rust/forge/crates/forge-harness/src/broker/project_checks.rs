use super::*;

impl HarnessBroker {
    /// Preserves unfinished setup decisions across ordinary follow-ups without extending them to another task.
    pub(super) fn continued_check_setup(
        &self,
        admission: &ExchangeAdmission,
    ) -> Result<Option<crate::checks::CheckSetupState>> {
        if admission.kind != ExchangeKind::Chat
            || admission.execution_id.is_some()
            || admission.goal_id.is_some()
            || admission.agent_run_id.is_some()
        {
            return Ok(None);
        }
        let Some(previous) = self.store.list_exchange(&self.session.id)?.pop() else {
            return Ok(None);
        };
        if previous.kind != ExchangeKind::Chat
            || previous.plan_id != admission.plan_id
            || previous.goal_id.is_some()
            || previous.execution_id.is_some()
            || previous.state == ExchangeState::Cancelled
            || previous.disposition != crate::exchange::HistoryDisposition::Current
        {
            return Ok(None);
        }
        let Some(setup) = previous.check_setup else {
            return Ok(None);
        };
        let published = self.check_runner.history()?.iter().any(|run| {
            run.lifecycle.iter().any(|event| {
                matches!(
                    event.kind,
                    crate::checks::CheckEventKind::ConfigurationSaved
                ) && event.created_at_ms >= previous.created_at_ms
            })
        });
        Ok((!published).then_some(setup))
    }

    /// Runs check discovery while retaining the original plan request and its task identity.
    pub(super) async fn configure_plan_checks(
        &mut self,
        prompt: String,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let completed = Box::pin(
            self.advance_configured_plan((serde_json::to_value(self.snapshot()?)?, Vec::new())),
        )
        .await?;
        let plan_id = self
            .current_plan_id()?
            .context("check setup has no pending plan")?;
        let plan = self
            .store
            .load_plan(&plan_id)?
            .context("pending plan is missing")?;
        if plan.state != PlanState::ConfiguringChecks {
            return Ok(completed);
        }
        if self.find_active_interaction_elicitation()?.is_some() {
            return Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()));
        }
        let mut admission = ExchangeAdmission::chat(prompt);
        admission.plan_id = Some(plan_id);
        self.observer.set_check_editing(true)?;
        self.check_setup = true;
        let result = self.run_interaction(format!(
            "The user requested a plan. Project checks are missing or invalid, so configure them before beginning that plan. Do not design or implement the requested feature yet. Use the original request only to identify the intended tooling. Harness will resume the original planning request after successful configuration publication.\n\nOriginal planning request:\n{}\n\n{}",
            plan.request, include_str!("../checks/setup.md")), PromptMode::Chat, Some(admission)).await;
        self.check_setup = false;
        result
    }

    /// Starts the retained plan only after this session records a matching configuration publication.
    pub(super) async fn advance_configured_plan(
        &mut self,
        result: (Value, Vec<SessionEvent>),
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let Some(plan_id) = self.current_plan_id()? else {
            return Ok(result);
        };
        let Some(mut plan) = self.store.load_plan(&plan_id)? else {
            return Ok(result);
        };
        if plan.state != PlanState::ConfiguringChecks
            || self.current_task()?.is_none_or(|task| {
                !matches!(
                    task.status,
                    crate::task::TaskStatus::Running | crate::task::TaskStatus::Waiting
                )
            })
            || self.find_active_interaction_elicitation()?.is_some()
        {
            return Ok(result);
        }
        let Ok(snapshot) = crate::checks::ConfigSnapshot::load(Path::new(&self.session.workspace))
        else {
            return Ok(result);
        };
        let published = self.check_runner.history()?.iter().any(|run| {
            run.context
                .as_ref()
                .is_some_and(|context| context.snapshot.digest == snapshot.digest)
                && run.lifecycle.iter().any(|event| {
                    matches!(
                        event.kind,
                        crate::checks::CheckEventKind::ConfigurationSaved
                    ) && event.created_at_ms >= plan.created_at_ms
                })
        });
        if !published {
            return Ok(result);
        }
        let mut document = self
            .plan_file
            .read_working_document(&self.session.id, &plan_id)?;
        if document.design.is_none() {
            document.design = Some(crate::plan::DeclarationDesign::open(Path::new(
                &self.session.workspace,
            ))?);
            self.plan_file
                .write_working_document(&self.session.id, &plan_id, &document)?;
        }
        PlanStateMachine::apply(&mut plan, PlanEvent::ChecksConfigured, self.clock.now_ms())?;
        let mut task = self
            .current_task()?
            .context("pending plan task is missing")?;
        task.status = crate::task::TaskStatus::Running;
        task.phase = "draft".into();
        task.reason = None;
        task.updated_at_ms = self.clock.now_ms();
        self.store.select_task(
            &self.session,
            Some(&task),
            None,
            Some(&plan),
            &self.client_id,
        )?;
        self.check_setup = false;
        let mut admission = ExchangeAdmission::plan(String::new(), Some(plan_id), false);
        admission.lifecycle = Some(WorkflowActivity::Planning.event(LifecycleAction::Started));
        let (value, mut event) = self.run_planning_interaction(
            PlanPrompt::with_active_document(format!(
                "Project checks have been saved and validated at revision {}. Check setup is complete. Now begin the original planning request.\n\n{}",
                snapshot.digest, PlanPrompt::draft(&plan.request)), &document.model_json()?), Some(admission)).await?;
        let mut leading = result.1;
        leading.append(&mut event);
        Ok((value, leading))
    }

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
            HarnessMethod::ChecksList => {
                let settings = crate::settings::SettingsStore::open(&self.data_root)?
                    .get(
                        &self.session.workspace,
                        &self.session.id,
                        &self.session.backend,
                    )?
                    .desired;
                self.check_runner
                    .catalog_preview(task_id.as_deref(), self.session.shell, &settings)
                    .await?
            }
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
                let mut run = self.check_runner.prepare_request(
                    serde_json::from_value(params)?,
                    task_id,
                    self.session.shell,
                    "User requested".into(),
                )?;
                run.exchange_id = self
                    .store
                    .list_exchange(&self.session.id)?
                    .last()
                    .map(|exchange| exchange.id.clone());
                json!({"run_id":self.check_runner.start(run, self.event_sink.clone(),None,None).await?})
            }
            HarnessMethod::ChecksStatus => {
                let wait_for_completion = params["wait_for_completion"].as_bool().unwrap_or(false);
                let wait_ms = params["wait_ms"]
                    .as_u64()
                    .unwrap_or(if wait_for_completion { 60000 } else { 0 });
                serde_json::to_value(
                    self.check_runner
                        .status(
                            &required_text(&params, "run_id")?,
                            wait_ms,
                            wait_for_completion,
                        )
                        .await?,
                )?
            }
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
        if self.active_plan_awaits_input()?
            || self.active_plan_has_acceptance()?
            || self.find_active_interaction_elicitation()?.is_some()
        {
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
        let plan = self
            .current_plan_id()?
            .as_ref()
            .map(|id| self.plan_file.read_working_document(&self.session.id, id))
            .transpose()?
            .map(|document| serde_json::to_string(&document))
            .transpose()?
            .unwrap_or_else(|| "No active plan.".into());
        self.observer.set_check_editing(true)?;
        self.check_setup = true;
        let result = self.run_interaction(format!("Configure project checks through checks_configure. Focus: {selected}.\n\nActive proposed plan (read-only context):\n{plan}\n\n{prompt}"), PromptMode::Chat, Some(admission)).await;
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
        let baseline = self
            .store
            .load_checkpoint(id)?
            .context("execution baseline checkpoint is missing")?;
        let current = GitCheckpoint::new(&self.session.workspace)
            .capture(
                &self.store.objects,
                &self.repositories,
                &self.session.id,
                self.clock.now_ms(),
            )
            .await?;
        Ok(Some(
            baseline
                .changed_paths(&current)?
                .into_iter()
                .map(|path| path.as_str().to_owned())
                .collect(),
        ))
    }
}
