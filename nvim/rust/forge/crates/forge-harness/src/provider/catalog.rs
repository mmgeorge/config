use serde::{Deserialize, Serialize};

/// Retain the host owner's published session defaults for resource preparation and UI reads.
/// Native discovery receives an explicit workspace or binding context instead of this snapshot.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderCatalogRequest {
    /// Identify the independent agent owner while preserving the visible settings scope.
    pub execution_owner: Option<super::ThreadOwner>,
    /// Identify the host session whose owner publishes these defaults.
    pub harness_session_id: String,
    /// Select the settings and resource workspace.
    pub workspace: String,
    /// Retain confirmed host filesystem policy.
    pub access: crate::session::AccessPolicy,
    /// Retain confirmed host approval policy.
    pub approval_mode: crate::session::ApprovalMode,
    /// Identify durable history without granting authority to open a second binding.
    pub provider_session_id: Option<forge_provider_api::ProviderSessionId>,
}

impl ProviderCatalogRequest {
    /// Get the runtime key owned by the published session snapshot.
    pub fn runtime_key(&self) -> Result<String, crate::provider::ThreadOwnerError> {
        Ok(self.owner()?.runtime_key())
    }

    /// Resolve the exact host owner without changing the visible preference scope.
    pub fn owner(&self) -> Result<super::ThreadOwner, crate::provider::ThreadOwnerError> {
        super::ThreadOwner::resolve(&self.execution_owner, &self.harness_session_id)
    }
}

/// Applies saved model controls while retaining provider defaults for unset values.
pub(crate) fn select_model_settings(
    model_list: &mut [super::ProviderModel],
    preference: Option<&crate::session::HarnessPreference>,
) {
    for model in model_list {
        let setting = preference.and_then(|preference| preference.model_setting.get(&model.id));
        model.selected_reasoning = setting
            .and_then(|setting| setting.reasoning.clone())
            .filter(|choice| model.reasoning.contains(choice))
            .or_else(|| model.default_reasoning.clone());
        model.selected_context_window = setting
            .and_then(|setting| setting.context_window.clone())
            .filter(|choice| {
                model
                    .context_window
                    .iter()
                    .any(|window| &window.id == choice)
            })
            .or_else(|| model.default_context_window.clone());
    }
}

/// One logical input: its typed Harness content and the files admitted with it.
///
/// [`forge_provider_api::forge::TurnInput`] decides which `<forge>` elements and instructions the
/// turn carries; the user's own words exist only in the kinds where the user typed something.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderInput {
    turn: forge_provider_api::forge::TurnInput,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Files explicitly admitted with this input, never discovered from its contents.
    attachments: Vec<crate::attachment::InputAttachment>,
}

impl ProviderInput {
    /// Build input for one typed turn.
    pub fn new(turn: forge_provider_api::forge::TurnInput) -> Self {
        Self {
            turn,
            attachments: Vec::new(),
        }
    }

    /// Build input for one turn kind.
    pub fn kind(kind: forge_provider_api::forge::TurnKind) -> Self {
        Self::new(forge_provider_api::forge::TurnInput::new(kind))
    }

    /// Build ordinary provider input from admitted user text.
    pub fn from_text(text: impl Into<String>) -> Self {
        Self::new(forge_provider_api::forge::TurnInput::user(
            forge_provider_api::PromptInput::Text(text.into()),
        ))
    }

    /// Build input for one queued message: its typed Harness input, or the user's text.
    pub(crate) fn from_message(message: &crate::input::InputMessage) -> Self {
        match message.turn() {
            Some(turn) => Self::new(turn.clone()),
            None => Self::from_text(message.text.clone()),
        }
    }

    /// Build a structured response to one of the model's questions.
    pub fn question_reply(response: forge_provider_api::QuestionReply) -> Self {
        Self::kind(forge_provider_api::forge::TurnKind::QuestionResponse { response })
    }

    /// Carry files selected at user-input admission without discovering files in prompt text.
    pub(crate) fn with_attachments(
        mut self,
        attachments: Vec<crate::attachment::InputAttachment>,
    ) -> Self {
        self.attachments = attachments;
        self
    }

    /// Return only the files already admitted alongside this input.
    pub(crate) fn attachments(&self) -> &[crate::attachment::InputAttachment] {
        &self.attachments
    }

    /// Parse a skill selector only when `$` starts the first non-whitespace token.
    pub fn parse(text: &str) -> Self {
        Self::new(forge_provider_api::forge::TurnInput::user(
            parse_user_input(text),
        ))
    }

    /// Get the typed turn content.
    pub fn turn(&self) -> &forge_provider_api::forge::TurnInput {
        &self.turn
    }

    /// Re-type a user message, keeping the user's words, for example as plan discussion.
    ///
    /// Inputs of any other kind are returned unchanged.
    pub(crate) fn retype_user_message(
        self,
        kind: impl FnOnce(forge_provider_api::PromptInput) -> forge_provider_api::forge::TurnKind,
    ) -> Self {
        let forge_provider_api::forge::TurnKind::UserMessage { input } = self.turn.kind() else {
            return self;
        };
        let mut retyped = forge_provider_api::forge::TurnInput::new(kind(input.clone()));
        retyped.push_child_results(self.turn.child_results().iter().cloned());
        for notice in self.turn.notices() {
            retyped.push_notice(notice.clone());
        }
        Self {
            turn: retyped,
            attachments: self.attachments,
        }
    }

    /// Add a side condition, such as an approval timeout, to this input.
    pub(crate) fn push_notice(&mut self, notice: forge_provider_api::forge::Notice) {
        self.turn.push_notice(notice);
    }

    /// Report settled child results with this input.
    pub(crate) fn push_child_results(
        &mut self,
        results: impl IntoIterator<Item = forge_provider_api::forge::AgentResult>,
    ) {
        self.turn.push_child_results(results);
    }

    /// Describe this input for transcripts: the user's text, or a short label for Harness input.
    pub fn text(&self) -> String {
        self.turn.label()
    }

    /// Supply the typed input without encoding any provider-specific envelope.
    pub(crate) fn normalized(&self) -> forge_provider_api::forge::TurnInput {
        self.turn.clone()
    }
}

impl From<String> for ProviderInput {
    /// Build ordinary user text input.
    fn from(text: String) -> Self {
        Self::from_text(text)
    }
}

impl From<&str> for ProviderInput {
    /// Build ordinary user text input.
    fn from(text: &str) -> Self {
        Self::from_text(text)
    }
}

/// Parse a skill selector only when `$` starts the first non-whitespace token.
pub(crate) fn parse_user_input(text: &str) -> forge_provider_api::PromptInput {
    let trimmed = text.trim_start();
    let Some(selector) = trimmed.strip_prefix('$') else {
        return forge_provider_api::PromptInput::Text(text.to_owned());
    };
    let name_length = selector.find(char::is_whitespace).unwrap_or(selector.len());
    let name = &selector[..name_length];
    if name.is_empty()
        || !name
            .chars()
            .all(|character| character.is_ascii_alphanumeric() || "-_.:".contains(character))
    {
        return forge_provider_api::PromptInput::Text(text.to_owned());
    }
    forge_provider_api::PromptInput::Skill {
        name: name.to_owned(),
        arguments: selector[name_length..].trim_start().to_owned(),
    }
}

pub use forge_provider_api::catalog::*;
