use super::*;
use crate::plan::review_answer::ReviewAnswerScope;
use crate::plan::review_document::PlanReviewQuestion;
use crate::settings::AskModel;
use forge_buffer::identity::DocumentId;

/// Capture the source session's model defaults before dispatch to a potentially reused fork.
#[derive(Clone, Eq, PartialEq)]
pub(crate) enum ReviewAnswerModel {
    Inherited {
        selection: AskModel,
        context_window: Option<String>,
    },
    Override(AskModel),
}

impl HarnessBroker {
    /// Capture admitted review questions without appending an exchange to their source session.
    pub(crate) fn prepare_review_questions(
        &self,
        params: &Value,
    ) -> Result<(DocumentId, Vec<PlanReviewQuestion>, ReviewAnswerModel)> {
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
        let mut questions = self.plan_review.questions(&self.session.id, input)?;
        if let Some(selected) = params.get("question_ids") {
            let selected: Vec<String> = serde_json::from_value(selected.clone())?;
            questions.retain(|question| selected.contains(&question.id));
        }
        let (selection, _) = self.review_model_preferences()?;
        Ok((document, questions, selection))
    }

    fn review_model_preferences(
        &self,
    ) -> Result<(ReviewAnswerModel, crate::settings::SettingsSnapshot)> {
        let preferences = crate::settings::SettingsStore::read_only(&self.data_root)?.get(
            &self.session.workspace,
            &self.session.id,
            &self.session.provider,
        )?;
        let settings = preferences.ask_model()?;
        let model = if settings.model.is_some() {
            ReviewAnswerModel::Override(settings)
        } else {
            ReviewAnswerModel::Inherited {
                selection: settings.resolve(&self.session, &[])?,
                context_window: self.session.context_window.clone(),
            }
        };
        Ok((model, preferences))
    }

    /// Read the source conversation boundary and configuration a standby fork must match.
    pub(crate) fn review_settings(&self) -> Result<(ReviewAnswerModel, Value)> {
        let (model, preferences) = self.review_model_preferences()?;
        let current = self.store.current_exchange_state(&self.session.id)?;
        let source = json!({
            "provider": self.session.provider, "native_session": self.session.provider_session_id,
            "checkpoint": self.session.provider_checkpoint_id,
            "exchange": current.latest.as_ref().map(|exchange| json!([exchange.id, exchange.state, exchange.disposition])),
            "access": self.session.access, "approval": self.session.approval_mode,
            "preferences": preferences.desired, "provider_settings": preferences.provider,
        });
        Ok((model, source))
    }

    /// Resolve the review model and capture its question before owned dispatch.
    pub(crate) fn prepare_review_question(
        &mut self,
        scope: &ReviewAnswerScope,
        selection: &ReviewAnswerModel,
        catalog: &[crate::provider::ProviderModel],
        progress: Option<(&forge_protocol::outbound::MessageSender, u64)>,
    ) -> Result<String> {
        scope.check()?;
        self.apply_review_model(selection, catalog)?;
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

    /// Apply the same read-only model configuration to a warm binding and an answer turn.
    pub(crate) fn apply_review_model(
        &mut self,
        selection: &ReviewAnswerModel,
        catalog: &[crate::provider::ProviderModel],
    ) -> Result<()> {
        let (selected, context_window) = match selection {
            ReviewAnswerModel::Inherited {
                selection,
                context_window,
            } => (selection.clone(), context_window.clone()),
            ReviewAnswerModel::Override(settings) => {
                (settings.resolve(&self.session, catalog)?, None)
            }
        };
        self.session.context_window = context_window;
        let model = selected.model.context("Ask model is unresolved")?;
        let model_label = crate::session::catalog_model_label(catalog, &model);
        self.session.select_model(model, model_label);
        self.session.effort = selected.effort.unwrap_or_else(|| "default".into());
        self.session.service_tier = selected.service_tier.unwrap_or_default();
        self.session.access.write_access = crate::session::WriteScope::Off;
        self.session.access.writable_directory.clear();
        self.save_session()
    }
}
