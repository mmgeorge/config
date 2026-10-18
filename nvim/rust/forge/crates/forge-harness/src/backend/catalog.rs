use crate::session::PermissionMode;
use serde::{Deserialize, Serialize};

/// Applies saved model controls while retaining provider defaults for unset values.
pub(crate) fn select_model_settings(
    model_list: &mut [super::BackendModel],
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

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Identifies the definition's scope independently of its enablement preference.
pub enum ResourceScope {
    /// Makes a definition available across workspaces.
    Global,
    /// Makes a definition available in its owning workspace.
    Workspace,
}

/// Describes provider-owned catalogs and their active-turn mutation behavior.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct CatalogCapability {
    pub skill: bool,
    pub mcp: bool,
    pub live_mcp_mutation: bool,
}

/// Carries stable session and workspace identity for out-of-band provider catalogs.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendCatalogRequest {
    /// Selects one independent agent conversation without changing portable preference scope.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution_owner: Option<super::ExecutionOwner>,
    pub harness_session_id: String,
    pub workspace: String,
    pub access: crate::session::AccessPolicy,
    pub execution_mode: PermissionMode,
    pub backend_session_id: Option<String>,
}

impl BackendCatalogRequest {
    /// Resolve the independent catalog runtime without changing portable preference scope.
    pub fn runtime_key(&self) -> anyhow::Result<String> {
        Ok(self.owner()?.runtime_key())
    }
    /// Validate the connection owner independently of the settings session identity.
    pub fn owner(&self) -> anyhow::Result<super::ExecutionOwner> {
        super::ExecutionOwner::resolve(&self.execution_owner, &self.harness_session_id)
    }
}

/// Represents either ordinary user text or an explicit provider skill invocation.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum BackendInput {
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

impl BackendInput {
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

/// Describes one provider skill exposed to Harness callers.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SkillDefinition {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Identifies the shared definition when the skill has an explicit source binding.
    pub resource_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Records the definition's scope when Forge owns its source.
    pub scope: Option<ResourceScope>,
    pub name: String,
    pub description: String,
    pub enabled: bool,
    pub user_invocable: bool,
    pub path: Option<String>,
    pub source: Option<String>,
    pub argument_hint: Option<String>,
}

/// Defines the normalized lifecycle state shown by the MCP picker.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum McpStatus {
    Disabled,
    Loading,
    Connected,
    NeedsAuthentication,
    Failed,
    Unavailable,
}

/// Describes one cached tool nested under its owning MCP server.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct McpToolDefinition {
    pub name: String,
    pub description: Option<String>,
}

/// Describes one complete provider MCP row and its cached tool inventory.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct McpDefinition {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Identifies the shared definition independently of its provider-visible name.
    pub resource_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Records the definition's scope when Forge owns its source.
    pub scope: Option<ResourceScope>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Identifies the source configuration without exposing launch credentials.
    pub source_path: Option<String>,
    pub name: String,
    pub transport: String,
    pub enabled: bool,
    pub status: McpStatus,
    pub status_detail: Option<String>,
    pub token_count: Option<u64>,
    pub token_estimated: bool,
    #[serde(default)]
    pub tools: Vec<McpToolDefinition>,
    pub tool_error: Option<String>,
}

/// Reports a completed provider mutation and any required turn restart.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CatalogMutation {
    pub name: String,
    pub enabled: bool,
    pub restart_required: bool,
}

#[cfg(test)]
mod test {
    use super::BackendInput;

    #[test]
    fn parses_only_a_leading_skill_selector() {
        assert_eq!(
            BackendInput::parse("  $walkthrough explain this"),
            BackendInput::Skill {
                name: "walkthrough".into(),
                arguments: "explain this".into(),
                attachments: Vec::new(),
            }
        );
        assert_eq!(
            BackendInput::parse("explain $walkthrough"),
            BackendInput::from_text("explain $walkthrough")
        );
    }
}
