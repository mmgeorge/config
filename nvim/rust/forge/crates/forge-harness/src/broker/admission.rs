use super::*;
impl HarnessBroker {
    pub(super) async fn run_admitted_provider(
        &mut self,
        mode: PromptMode,
        mut input: BackendInput,
        submission: Arc<crate::backend::submission::Submission>,
        new_runtime: bool,
        interaction: &mut Exchange,
        event: &mut Vec<SessionEvent>,
    ) -> Result<crate::backend::BackendOutput> {
        let trace = self.trace.clone();
        let session_id = self.session.id.clone();
        let exchange_id = interaction.id.clone();
        let prepared = trace
            .instrument(
                &session_id,
                &exchange_id,
                "turn.prepare",
                self.prepare_admitted_turn(interaction, new_runtime, event),
            )
            .await?;
        if mode == PromptMode::PlanDiscussion {
            input = match input {
                BackendInput::Text { text, attachments } => {
                    BackendInput::from_text(PlanPrompt::discussion(&text))
                        .with_attachments(attachments)
                }
                BackendInput::Skill {
                    name,
                    arguments,
                    attachments,
                } => BackendInput::Skill {
                    name,
                    arguments: PlanPrompt::discussion(&arguments),
                    attachments,
                },
            };
        }
        if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion)
            && !input.text().contains("Active declaration design:")
        {
            let current_plan_id = self.current_plan_id()?;
            let plan_id = current_plan_id
                .as_deref()
                .context("planning turn has no active plan")?;
            let document = self
                .plan_file
                .read_working_document(&self.session.id, plan_id)?;
            let context = PlanPrompt::with_active_document(String::new(), &document.model_json()?);
            input = match input {
                BackendInput::Text { text, attachments } => {
                    BackendInput::from_text(format!("{context}{text}"))
                        .with_attachments(attachments)
                }
                BackendInput::Skill {
                    name,
                    arguments,
                    attachments,
                } => BackendInput::Skill {
                    name,
                    arguments: format!("{context}{arguments}"),
                    attachments,
                },
            };
        }

        if self.session.backend_session_id.is_none() {
            input = self.rollback_context(&interaction.id, input)?;
        }
        let mut backend_request = self.backend_request(input, mode);
        backend_request.submission = Some(submission.clone());
        let owner = crate::backend::ExecutionOwner::root(&self.session.id);
        if let Some(feedback) = self.permission_coordinator.take_timeout_feedback(&owner)? {
            match &mut backend_request.input {
                BackendInput::Text { text, .. } => *text = format!("{feedback}\n\n{text}"),
                BackendInput::Skill { arguments, .. } => {
                    *arguments = format!("{feedback}\n\n{arguments}")
                }
            }
        }
        let policy = self
            .permission_store
            .read()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .compiled()
            .clone();
        let (repository_scope, repository_lease) =
            crate::control_tools::repository::RepositoryToolScope::open(
                self.session.id.clone(),
                interaction.id.clone(),
                PathBuf::from(&self.session.workspace),
                policy.clone(),
                Arc::clone(&self.turn_cancellation.requested),
                Arc::clone(&self.repositories),
                Arc::clone(&self.diff),
            )?;
        let (harness_scope, harness_lease) =
            crate::control_tools::harness::HarnessInspectionScope::open(
                self.session.clone(),
                self.data_root.clone(),
                self.permission_store
                    .read()
                    .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
                    .open()
                    .0,
                policy.clone(),
                Arc::clone(&self.turn_cancellation.requested),
                self.trace.clone(),
            )?;
        let (lsp_scope, lsp_lease) = self.lsp.scope(
            self.session.id.clone(),
            interaction.id.clone(),
            PathBuf::from(&self.session.workspace),
            policy,
            self.turn_cancellation.requested.clone(),
        );
        if let Some(context) = &mut backend_request.control_context {
            context.lsp = Some(lsp_scope);
            context.repository = Some(repository_scope);
            context.harness = Some(harness_scope);
            context.has_active_elicitation |= interaction.elicitation().is_some();
        }
        self.trace.record(
            &self.session.id,
            "model.requested",
            serde_json::to_value(&backend_request)?,
        );
        let mut execution =
            Box::pin(self.prompt_with_timeline(backend_request, interaction, event, prepared));
        // Automatic repair reenters provider execution while its parent request is still polling.
        // Grow temporary segments here without increasing the worker's stack allocation.
        let output = trace
            .instrument(
                &session_id,
                &exchange_id,
                "turn.execute",
                std::future::poll_fn(|context| {
                    stacker::maybe_grow(768 * 1024, 1024 * 1024, || {
                        std::future::Future::poll(execution.as_mut(), context)
                    })
                }),
            )
            .await;
        drop(execution);
        drop(repository_lease);
        drop(harness_lease);
        drop(lsp_lease);
        output
    }
}

/// Consumed by provider dispatch after the admitted exchange's prerequisites settle.
pub(super) struct PreparedTurn {
    exchange_id: String,
}

impl PreparedTurn {
    pub(super) fn consume(self, exchange: &Exchange) -> Result<()> {
        anyhow::ensure!(
            self.exchange_id == exchange.id,
            "prepared turn belongs to another exchange"
        );
        Ok(())
    }
}

impl HarnessBroker {
    /// Keeps visible admission independent of previous finalization and provider preparation.
    pub(super) async fn prepare_admitted_turn(
        &mut self,
        exchange: &mut Exchange,
        new_runtime: bool,
        event: &mut Vec<SessionEvent>,
    ) -> Result<PreparedTurn> {
        let cancellation = self.turn_cancellation.clone();
        let trace = self.trace.clone();
        let session_id = self.session.id.clone();
        let exchange_id = exchange.id.clone();
        if *cancellation.subscribe().borrow() {
            return Err(anyhow::Error::new(TurnCancelled));
        }
        loop {
            let (result, completed) = tokio::select! {
                biased;
                () = cancellation.cancelled() => return Err(anyhow::Error::new(TurnCancelled)),
                result = trace.instrument(&session_id, &exchange_id, "turn.previous_finalization", self.retry_finalization()) => result.context("finish previous exchange before provider execution")?,
            };
            event.extend(completed);
            if result.get("finalized_exchange_id").is_none() {
                break;
            }
        }
        if new_runtime {
            self.start_exchange_runtime(exchange, self.clock.now_ms())
                .await?;
        } else if self
            .exchange_runtime
            .as_ref()
            .is_none_or(|runtime| runtime.exchange_id != exchange.id)
        {
            self.resume_exchange_runtime(exchange)?;
        }
        tokio::select! {
            biased;
            () = cancellation.cancelled() => return Err(anyhow::Error::new(TurnCancelled)),
            result = trace.instrument(&session_id, &exchange_id, "provider.prepare", self.backend.prepare_turn(&self.session.id)) => result?,
        }
        Ok(PreparedTurn {
            exchange_id: exchange.id.clone(),
        })
    }
}
