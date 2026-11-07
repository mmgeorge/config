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
    pub fn runtime_key(&self) -> Result<String, forge_provider_api::ThreadOwnerError> {
        Ok(self.owner()?.runtime_key())
    }

    /// Resolve the exact host owner without changing the visible preference scope.
    pub fn owner(&self) -> Result<super::ThreadOwner, forge_provider_api::ThreadOwnerError> {
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

/// Represents either ordinary user text or an explicit provider skill invocation.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ProviderInput {
    Text {
        text: String,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        /// Files explicitly admitted with this text, never discovered from its contents.
        attachments: Vec<crate::attachment::InputAttachment>,
    },
    Skill {
        name: String,
        arguments: String,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        /// Files explicitly admitted with the skill invocation.
        attachments: Vec<crate::attachment::InputAttachment>,
    },
    QuestionReply {
        reply: forge_provider_api::QuestionReply,
        /// Carry host workflow context separately from the structured reply.
        #[serde(default, skip_serializing_if = "String::is_empty")]
        context: String,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        attachments: Vec<crate::attachment::InputAttachment>,
    },
}

impl ProviderInput {
    /// Return workflow instructions that must not be encoded inside a native question reply.
    pub(crate) fn additional_instructions(&self) -> &str {
        match self {
            Self::QuestionReply { context, .. } => context,
            _ => "",
        }
    }
    /// Build ordinary provider input from admitted user text.
    pub fn from_text(text: impl Into<String>) -> Self {
        Self::Text {
            text: text.into(),
            attachments: Vec::new(),
        }
    }

    /// Carry files selected at user-input admission without discovering files in prompt text.
    pub(crate) fn with_attachments(
        mut self,
        attachments: Vec<crate::attachment::InputAttachment>,
    ) -> Self {
        match &mut self {
            Self::Text {
                attachments: selected,
                ..
            }
            | Self::Skill {
                attachments: selected,
                ..
            }
            | Self::QuestionReply {
                attachments: selected,
                ..
            } => *selected = attachments,
        }
        self
    }

    /// Return only the files already admitted alongside this input.
    pub(crate) fn attachments(&self) -> &[crate::attachment::InputAttachment] {
        match self {
            Self::Text { attachments, .. }
            | Self::Skill { attachments, .. }
            | Self::QuestionReply { attachments, .. } => attachments,
        }
    }

    /// Parse a skill selector only when `$` starts the first non-whitespace token.
    pub fn parse(text: &str) -> Self {
        let trimmed = text.trim_start();
        let Some(selector) = trimmed.strip_prefix('$') else {
            return Self::from_text(text);
        };
        let name_length = selector.find(char::is_whitespace).unwrap_or(selector.len());
        let name = &selector[..name_length];
        if name.is_empty()
            || !name
                .chars()
                .all(|character| character.is_ascii_alphanumeric() || "-_.:".contains(character))
        {
            return Self::from_text(text);
        }
        Self::Skill {
            name: name.to_owned(),
            arguments: selector[name_length..].trim_start().to_owned(),
            attachments: Vec::new(),
        }
    }

    /// Return the provider-visible text corresponding to this admitted input.
    pub fn text(&self) -> String {
        match self {
            Self::Text { text, .. } => text.clone(),
            Self::Skill {
                name, arguments, ..
            } if arguments.is_empty() => format!("${name}"),
            Self::Skill {
                name, arguments, ..
            } => format!("${name} {arguments}"),
            Self::QuestionReply { reply, .. } => match &reply.response {
                forge_provider_api::QuestionReplyResponse::Answer { answer, .. }
                | forge_provider_api::QuestionReplyResponse::Correction { answer, .. } => {
                    answer.clone()
                }
                forge_provider_api::QuestionReplyResponse::Clarification { text, .. } => {
                    text.clone()
                }
            },
        }
    }

    /// Supply structured input without encoding any provider-specific envelope.
    pub(crate) fn normalized(&self) -> forge_provider_api::PromptInput {
        match self {
            Self::Text { text, .. } => forge_provider_api::PromptInput::Text(text.clone()),
            Self::Skill {
                name, arguments, ..
            } => forge_provider_api::PromptInput::Skill {
                name: name.clone(),
                arguments: arguments.clone(),
            },
            Self::QuestionReply { reply, .. } => {
                forge_provider_api::PromptInput::QuestionReply(reply.clone())
            }
        }
    }
}

pub use forge_provider_api::catalog::*;

impl From<&ProviderInput> for forge_provider_api::input::PromptInput {
    fn from(input: &ProviderInput) -> Self {
        match input {
            ProviderInput::Text { text, .. } => Self::Text(text.clone()),
            ProviderInput::Skill {
                name, arguments, ..
            } => Self::Skill {
                name: name.clone(),
                arguments: arguments.clone(),
            },
            ProviderInput::QuestionReply { reply, .. } => Self::QuestionReply(reply.clone()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::ProviderInput;

    #[test]
    fn parses_only_a_leading_skill_selector() {
        assert_eq!(
            ProviderInput::parse("  $walkthrough explain this"),
            ProviderInput::Skill {
                name: "walkthrough".into(),
                arguments: "explain this".into(),
                attachments: Vec::new(),
            }
        );
        assert_eq!(
            ProviderInput::parse("explain $walkthrough"),
            ProviderInput::from_text("explain $walkthrough")
        );
    }
}
