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
                &self.session.provider,
            )?
            .ask_model()?;
        Ok((document, questions, settings))
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
        self.session.effort = selected.effort.unwrap_or_else(|| "default".into());
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
}
