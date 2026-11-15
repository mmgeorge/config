/// Preserve message meaning independently of active-turn delivery.
#[derive(Clone, Debug, serde::Deserialize, Eq, PartialEq, serde::Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum UserInputContent {
    UserText {
        text: String,
    },
    /// Preserve host check evidence through steering and transcript replay.
    CheckResult {
        result: Box<forge_provider_api::forge::CheckResult>,
    },
    QuestionResponse {
        responses: Vec<QuestionReplyContent>,
    },
}

impl UserInputContent {
    pub(crate) fn label(&self) -> String {
        match self {
            Self::UserText { text } => text.clone(),
            Self::CheckResult { result } => forge_provider_api::forge::TurnInput::new(
                forge_provider_api::forge::TurnKind::CheckResult {
                    result: result.as_ref().clone(),
                },
            )
            .label(),
            Self::QuestionResponse { responses } => forge_provider_api::forge::TurnInput::new(
                forge_provider_api::forge::TurnKind::QuestionBatch {
                    responses: responses
                        .iter()
                        .map(|response| response.reply.clone())
                        .collect(),
                },
            )
            .label(),
        }
    }

    pub(crate) fn owns_reply(&self, question_id: &str, message_id: &str) -> bool {
        match self {
            Self::UserText { .. } | Self::CheckResult { .. } => false,
            Self::QuestionResponse { responses } => responses.iter().any(|response| {
                response.reply.question_id == question_id && response.reply.message_id == message_id
            }),
        }
    }

    /// Preserve semantic input content when recording an active-turn delivery.
    pub(crate) fn exchange_input(&self) -> (crate::exchange::InputIntent, String) {
        match self {
            Self::UserText { text } => (
                crate::exchange::InputIntent::user_text(text.clone()),
                text.clone(),
            ),
            Self::QuestionResponse { .. } | Self::CheckResult { .. } => (
                crate::exchange::InputIntent::User {
                    content: self.clone(),
                },
                self.label(),
            ),
        }
    }

    pub(crate) fn is_question_reply(&self) -> bool {
        matches!(self, Self::QuestionResponse { .. })
    }
}

/// Freeze the answered revision and its display value with the exact delivered reply.
#[derive(Clone, Debug, serde::Deserialize, Eq, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub struct QuestionReplyContent {
    pub reply: forge_provider_api::QuestionReply,
    pub question: crate::plan::PlanQuestion,
    pub value: crate::plan::PlanQuestionResponse,
}

#[cfg(test)]
mod tests {
    use super::UserInputContent;
    #[test]
    fn malformed_question_content_never_becomes_text() {
        for encoded in [
            serde_json::json!({"kind":"question_response", "text":"JSON"}),
            serde_json::json!({"kind":"question_response", "responses":[{"answer":"JSON"}]}),
            serde_json::json!({"kind":"user_text", "text":"JSON", "responses":[]}),
        ] {
            assert!(serde_json::from_value::<UserInputContent>(encoded).is_err());
        }
    }
}
