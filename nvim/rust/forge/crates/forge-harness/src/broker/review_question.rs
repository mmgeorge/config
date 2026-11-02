use super::*;
use crate::plan::review_answer::ReviewAnswerScope;
use crate::plan::review_document::PlanReviewQuestion;
use crate::settings::AskModel;
use forge_buffer::identity::DocumentId;

pub(crate) struct AskModelConfiguration {
    selection: AskModel,
    authority: Box<configuration::ConfigurationAuthority>,
    revision: u64,
}

impl AskModelConfiguration {
    pub(crate) fn needs_catalog(&self) -> bool {
        self.selection.model.is_some()
    }
}

impl HarnessBroker {
    /// Capture admitted review questions without appending an exchange to their source session.
    pub(crate) fn prepare_review_questions(
        &self,
        params: &Value,
    ) -> Result<(DocumentId, Vec<PlanReviewQuestion>, AskModel)> {
        let input: forge_buffer::input::DocumentInput = serde_json::from_value(
            params
                .get("review")
                .cloned()
                .context("plan question requires its review identity")?,
        )?;
        let document = input.document.clone();
        if let Some(annotation) = params.get("draft_annotation") {
            let digest = params
                .get("draft_source_digest")
                .and_then(Value::as_str)
                .context("plan question requires its source digest")?;
            self.plan_review.save_annotations(
                &document,
                digest,
                serde_json::from_value(annotation.clone())?,
            )?;
        }
        let mut questions = self.plan_review.questions(input)?;
        if let Some(selected) = params.get("question_ids") {
            let selected: Vec<String> = serde_json::from_value(selected.clone())?;
            questions.retain(|question| selected.contains(&question.id));
        }
        let settings = crate::settings::SettingsStore::read_only(&self.data_root)?
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .ask_model()?;
        Ok((document, questions, settings))
    }

    /// Validate and persist provider-local Ask settings without changing the planning model.
    #[cfg(test)]
    pub(crate) async fn configure_ask_model(&mut self, value: Value) -> Result<Value> {
        let prepared = self.prepare_ask_model_configuration(value)?;
        let catalog = if prepared.needs_catalog() {
            let result = self.prepare_configuration_catalog()?.run().await?;
            self.commit_configuration_catalog(result)?
        } else {
            Vec::new()
        };
        self.commit_ask_model_configuration(prepared, &catalog)
    }

    pub(crate) fn prepare_ask_model_configuration(
        &self,
        value: Value,
    ) -> Result<AskModelConfiguration> {
        anyhow::ensure!(
            self.store
                .pending_configuration_intent(&self.session.id)?
                .is_none(),
            "session configuration requires durable recovery"
        );
        let selection: AskModel = if value.is_null() {
            AskModel::default()
        } else {
            serde_json::from_value(value)?
        };
        if selection.model.is_some() {
            anyhow::ensure!(
                self.capability.model_selection,
                "current provider does not support model selection"
            );
        }
        let revision = crate::settings::SettingsStore::read_only(&self.data_root)?
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .revision;
        Ok(AskModelConfiguration {
            selection,
            authority: self.capture_configuration_authority()?,
            revision,
        })
    }

    pub(crate) fn commit_ask_model_configuration(
        &mut self,
        prepared: AskModelConfiguration,
        catalog: &[crate::provider::ProviderModel],
    ) -> Result<Value> {
        self.validate_configuration_authority(&prepared.authority)?;
        let selection = prepared.selection;
        selection.resolve(&self.session, catalog)?;
        let controller = self.task_controller.clone();
        controller.with_task_snapshot(|current| {
            anyhow::ensure!(
                foreground_failure::same_retraction_task(
                    current.as_ref(),
                    prepared.authority.task.as_ref()
                ),
                "Ask configuration task authority changed before commit"
            );
            let mut settings = crate::settings::SettingsStore::open(&self.data_root)?;
            settings.save_provider(
                prepared.revision,
                &self.session.provider,
                &json!({"ask_model":selection}),
            )?;
            Ok(json!({"ask_model":selection}))
        })
    }

    /// Save the completed fork's final response on its unchanged source question.
    #[cfg(test)]
    pub(crate) async fn run_review_question(
        &mut self,
        scope: Arc<ReviewAnswerScope>,
        selection: &AskModel,
        progress: Option<(&forge_protocol::outbound::MessageSender, u64)>,
    ) -> Result<()> {
        let catalog = if selection.model.is_some() {
            let result = self
                .prepare_configuration_catalog_for_mode(PromptMode::PlanQuestion)?
                .run()
                .await?;
            self.commit_configuration_catalog(result)?
        } else {
            Vec::new()
        };
        let prompt = self.prepare_review_question(&scope, selection, &catalog, progress)?;
        let (sink, updates) = publication::channel();
        let dispatch =
            self.begin_review_question_dispatch(prompt, scope.question.body.clone(), sink)?;
        let result = self.drive_local_foreground(dispatch, updates).await?;
        let value = result.response.result().ok_or_else(|| {
            anyhow::anyhow!(
                "review question dispatch failed: {:?}",
                result.response.error()
            )
        })?;
        self.finish_review_question(scope, value)
    }

    /// Resolve the review model and capture its question before owned dispatch.
    pub(crate) fn prepare_review_question(
        &mut self,
        scope: &ReviewAnswerScope,
        selection: &AskModel,
        catalog: &[crate::provider::ProviderModel],
        progress: Option<(&forge_protocol::outbound::MessageSender, u64)>,
    ) -> Result<String> {
        scope.check()?;
        let selected = selection.resolve(&self.session, catalog)?;
        if selection.model.is_some() {
            self.session.context_window = None;
        }
        self.session.model = selected.model.context("Ask model is unresolved")?;
        self.session.resolved_model =
            (self.session.model != "default").then(|| self.session.model.clone());
        self.session.effort = selected.effort.unwrap_or_default();
        self.session.service_tier = selected.service_tier.unwrap_or_default();
        self.session.access.write_access = crate::session::WriteScope::Off;
        self.save_session()?;
        scope.check()?;
        if let Some((sink, request_id)) = progress {
            sink.send(forge_protocol::message::Message::RequestEvent(
                forge_protocol::message::RequestEvent {
                    request_id,
                    event: forge_protocol::contract::RequestEventName::SessionPlanQuestionAnswerProgress
                        .as_str().into(),
                    payload: json!({
                        "question_id": scope.question.id,
                        "model": self.session.model,
                        "effort": if self.session.effort.is_empty() { "Default" } else { &self.session.effort },
                    }),
                },
            ))?;
        }
        Ok(format!(
            "Answer this Plan Review question in 1-2 sentences in your final response. Do not edit files or the plan.\n\n{}",
            scope.question.prompt
        ))
    }

    /// Publish only the final answer belonging to the unchanged source question.
    pub(crate) fn finish_review_question(
        &self,
        scope: Arc<ReviewAnswerScope>,
        result: &Value,
    ) -> Result<()> {
        let exchange = result
            .pointer("/exchange/id")
            .and_then(Value::as_str)
            .context("review question turn has no exchange")?;
        let exchange = self
            .store
            .load_exchange(exchange)?
            .context("review question exchange is missing")?;
        anyhow::ensure!(
            exchange.phase.state() == crate::exchange::ExchangeState::Complete,
            "review question stopped before its answer completed"
        );
        let answer = exchange
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .filter(|message| {
                message.kind() == crate::turn::MessageKind::Assistant
                    && message.delivery() == crate::turn::MessageDelivery::Final
            })
            .map(|message| message.text())
            .collect::<Vec<_>>()
            .join("\n\n");
        scope.submit(answer)
    }
}
