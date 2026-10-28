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
        let digest = params
            .get("draft_source_digest")
            .and_then(Value::as_str)
            .context("plan question requires its source digest")?;
        let annotation = serde_json::from_value(
            params
                .get("draft_annotation")
                .cloned()
                .context("plan questions require captured annotations")?,
        )?;
        let document = input.document.clone();
        self.plan_review
            .save_annotations(&document, digest, annotation)?;
        let questions = self.plan_review.questions(input)?;
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

    /// Continue only this fork until its dedicated tool accepts the captured review answer.
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
        self.review_answer = Some(Arc::clone(&scope));
        let result = self.review_question_turns(&scope).await;
        self.review_answer = None;
        result
    }

    async fn review_question_turns(&mut self, scope: &ReviewAnswerScope) -> Result<()> {
        let mut prompt = format!(
            "Answer this Plan Review question in 1-2 sentences. Call plan_review_answer with question_id {} and the answer, then end the turn. Do not edit files or the plan.\n\n{}",
            scope.question.id, scope.question.prompt
        );
        loop {
            scope.check()?;
            let mut admission = ExchangeAdmission::chat(scope.question.body.clone());
            admission.plan_id = Some(scope.question.plan_id.clone());
            let (result, _) = self
                .run_interaction(prompt, PromptMode::PlanQuestion, Some(admission))
                .await?;
            if scope.answered()? {
                return Ok(());
            }
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
                "review question stopped before its answer was submitted"
            );
            prompt = format!(
                "The answer has not been submitted. Call plan_review_answer with question_id {} and your answer. A final message alone does not complete this request.",
                scope.question.id
            );
        }
    }
}

#[cfg(test)]
pub(super) mod test {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    pub(crate) struct RetryReviewBackend {
        delegate: Arc<dyn Backend>,
        pub(crate) attempts: AtomicUsize,
    }

    impl RetryReviewBackend {
        pub(crate) fn new(delegate: Arc<dyn Backend>) -> Self {
            Self {
                delegate,
                attempts: AtomicUsize::new(0),
            }
        }
    }

    #[async_trait::async_trait]
    impl Backend for RetryReviewBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            self.delegate.descriptor()
        }
        async fn shutdown(&self) -> Result<()> {
            self.delegate.shutdown().await
        }
        async fn has_active_turn(&self, session: &str) -> bool {
            self.delegate.has_active_turn(session).await
        }
        async fn active_session_id_for(&self, session: &str) -> Option<String> {
            self.delegate.active_session_id_for(session).await
        }
        async fn cleanup_execution(&self, session: &str) -> Result<()> {
            self.delegate.cleanup_execution(session).await
        }
        async fn prompt_stream(
            &self,
            mut request: BackendRequest,
            sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            if request.mode == PromptMode::PlanQuestion {
                let attempt = self.attempts.fetch_add(1, Ordering::SeqCst);
                if attempt == 0 {
                    request.control_context.as_mut().unwrap().review_answer = None;
                } else {
                    assert!(
                        request
                            .input
                            .text()
                            .contains("The answer has not been submitted")
                    );
                }
            }
            self.delegate.prompt_stream(request, sink).await
        }
    }
}
