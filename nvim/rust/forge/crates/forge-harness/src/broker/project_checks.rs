use super::*;

impl HarnessBroker {
    /// Preserves unfinished setup decisions across ordinary follow-ups without extending them to another task.
    pub(super) fn continued_check_setup(
        &self,
        admission: &ExchangeAdmission,
    ) -> Result<Option<crate::checks::CheckSetupState>> {
        if self.task_controller.current()?.is_some() || admission.agent_run_id.is_some() {
            return Ok(None);
        }
        let current = self.store.current_exchange_state(&self.session.id)?;
        let Some(previous) = current.latest.as_ref() else {
            return Ok(None);
        };
        if previous.state == ExchangeState::Cancelled
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

    /// Starts the retained plan only after this session records a matching configuration publication.
    pub(super) async fn advance_configured_plan(
        &mut self,
        result: (Value, Vec<SessionEvent>),
    ) -> Result<(Value, Vec<SessionEvent>)> {
        self.advance_configured_plan_now(result)
    }

    pub(super) fn advance_configured_plan_now(
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
            || self.task_controller.current()?.is_none_or(|task| {
                !matches!(
                    task.status(),
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
            let original = document.clone();
            document.design = Some(crate::plan::DeclarationDesign::open(Path::new(
                &self.session.workspace,
            ))?);
            self.plan_file.write_working_document_if_unchanged(
                &self.session.id,
                &plan_id,
                &original,
                &document,
            )?;
        }
        if !self
            .plan_file
            .authoring_directory(&self.session.id, &plan_id)
            .exists()
        {
            self.plan_file
                .authoring_create(&self.session.id, &document)?;
        }
        PlanStateMachine::apply(&mut plan, PlanEvent::ChecksConfigured, self.clock.now_ms())?;
        let task = self
            .task_controller
            .current()?
            .context("pending plan task is missing")?;
        let now_ms = self.clock.now_ms();
        // The plan task section names the request; the step only says that setup finished.
        let instruction = PlanPrompt::checks_saved(&snapshot.digest);
        self.task_controller.apply(
            &mut self.store,
            &task.permit(),
            crate::task::TaskEvent::PlanArtifactUpdated(Box::new(plan)),
            now_ms,
        )?;
        self.check_setup = false;
        let task = self
            .task_controller
            .current()?
            .context("pending plan task is missing")?;
        let exchange = self
            .store
            .latest_exchange(&self.session.id)?
            .context("check setup exchange is missing")?;
        self.task_controller.reserve_continuation(
            &mut self.store,
            &task.permit(),
            crate::task::TaskTurnId {
                exchange_ordinal: exchange.ordinal,
                turn_ordinal: exchange.turn.len(),
            },
            instruction,
            "Project check setup completed".into(),
            now_ms,
        )?;
        Ok(result)
    }

    pub(super) async fn project_checks(
        &mut self,
        method: RuntimeMethod,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        self.check_runner.refresh_changes().await?;
        let task_id = self
            .task_controller
            .current()?
            .filter(|task| !task.status().terminal())
            .map(|task| task.id.as_str().to_owned());
        let result = match method {
            RuntimeMethod::SessionCheckList => {
                let settings = crate::settings::SettingsStore::open(&self.data_root)?
                    .get(
                        &self.session.workspace,
                        &self.session.id,
                        &self.session.provider,
                    )?
                    .desired;
                self.check_runner
                    .catalog_preview(task_id.as_deref(), self.session.shell, &settings)
                    .await?
            }
            RuntimeMethod::SessionCheckStatus => {
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
            _ => anyhow::bail!("unknown checks operation"),
        };
        Ok((result, Vec::new()))
    }
}
