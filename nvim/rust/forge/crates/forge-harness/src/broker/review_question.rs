use super::*;
use crate::plan::review_answer::ReviewAnswerScope;
use crate::plan::review_document::PlanReviewQuestion;
use crate::settings::AskModel;
use forge_buffer::identity::DocumentId;

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
                &self.session.backend,
            )?
            .ask_model()?;
        Ok((document, questions, settings))
    }

    /// Validate and persist provider-local Ask settings without changing the planning model.
    pub(crate) async fn configure_ask_model(&mut self, value: Value) -> Result<Value> {
        let selection: AskModel = if value.is_null() {
            AskModel::default()
        } else {
            serde_json::from_value(value)?
        };
        let catalog = if selection.model.is_some() {
            anyhow::ensure!(
                self.capability.model_selection,
                "current provider does not support model selection"
            );
            self.backend
                .model_list(self.backend_request(BackendInput::from_text(""), PromptMode::Chat))
                .await?
        } else {
            Vec::new()
        };
        selection.resolve(&self.session, &catalog)?;
        let mut settings = crate::settings::SettingsStore::open(&self.data_root)?;
        let snapshot = settings.get(
            &self.session.workspace,
            &self.session.id,
            &self.session.backend,
        )?;
        settings.save_provider(
            snapshot.revision,
            &self.session.backend,
            &json!({"ask_model":selection}),
        )?;
        Ok(json!({"ask_model":selection}))
    }

    /// Save the completed fork's final response on its unchanged source question.
    pub(crate) async fn run_review_question(
        &mut self,
        scope: Arc<ReviewAnswerScope>,
        selection: &AskModel,
    ) -> Result<()> {
        let catalog = if selection.model.is_some() {
            self.backend
                .model_list(
                    self.backend_request(BackendInput::from_text(""), PromptMode::PlanQuestion),
                )
                .await?
        } else {
            Vec::new()
        };
        let selected = selection.resolve(&self.session, &catalog)?;
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
        let prompt = format!(
            "Answer this Plan Review question in 1-2 sentences in your final response. Do not edit files or the plan.\n\n{}",
            scope.question.prompt
        );
        let admission = ExchangeAdmission::chat(scope.question.body.clone());
        let (result, _) = self
            .run_interaction(prompt, PromptMode::PlanQuestion, Some(admission))
            .await?;
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
