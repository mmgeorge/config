use serde::{Deserialize, Serialize};

/// Applies saved model controls while retaining provider defaults for unset values.
pub(crate) fn select_model_settings(
    model_list: &mut [super::ProviderModel],
    preference: Option<&crate::session::HarnessPreference>,
) {
    for model in model_list {
        let setting = preference.and_then(|preference| preference.model_setting.get(&model.id));
        model.selected_reasoning = setting
            .and_then(|setting| setting.reasoning.clone())
            .or_else(|| model.default_reasoning.clone());
        model.selected_context_window = setting
            .and_then(|setting| setting.context_window.clone())
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
}

impl ProviderInput {
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
            } => *selected = attachments,
        }
        self
    }

    /// Return only the files already admitted alongside this input.
    pub(crate) fn attachments(&self) -> &[crate::attachment::InputAttachment] {
        match self {
            Self::Text { attachments, .. } | Self::Skill { attachments, .. } => attachments,
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
        }
    }
}
