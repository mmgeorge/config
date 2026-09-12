use crate::session::ExecutionMode;
use serde::{Deserialize, Serialize};

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
    pub harness_session_id: String,
    pub workspace: String,
    pub execution_mode: ExecutionMode,
    pub backend_session_id: Option<String>,
}

/// Represents either ordinary user text or an explicit provider skill invocation.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum BackendInput {
    Text { text: String },
    Skill { name: String, arguments: String },
}

impl BackendInput {
    /// Build ordinary provider input from admitted user text.
    pub fn from_text(text: impl Into<String>) -> Self {
        Self::Text { text: text.into() }
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
        }
    }

    /// Return the provider-visible text corresponding to this admitted input.
    pub fn text(&self) -> String {
        match self {
            Self::Text { text } => text.clone(),
            Self::Skill { name, arguments } if arguments.is_empty() => format!("${name}"),
            Self::Skill { name, arguments } => format!("${name} {arguments}"),
        }
    }
}

/// Describes one provider skill exposed to Harness callers.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SkillDefinition {
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
            }
        );
        assert_eq!(
            BackendInput::parse("explain $walkthrough"),
            BackendInput::from_text("explain $walkthrough")
        );
    }
}
