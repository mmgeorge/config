use crate::approval::ApprovalMode;
use serde::{Deserialize, Serialize};

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
#[derive(Clone, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
pub struct CatalogCapability {
    pub skill: bool,
    pub mcp: bool,
    pub live_mcp_mutation: bool,
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

/// Identify one normalized MCP connection transport.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum McpTransportKind {
    /// Launch a local process and communicate over standard input and output.
    Stdio,
    /// Connect to a remote HTTP endpoint.
    Http,
    /// Connect to a remote server-sent events endpoint.
    Sse,
    /// Report a provider-defined transport without exposing its native spelling.
    Unknown,
}

impl McpTransportKind {
    /// Get the stable normalized transport label for presentation or serialization.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Stdio => "stdio",
            Self::Http => "http",
            Self::Sse => "sse",
            Self::Unknown => "unknown",
        }
    }
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
    pub transport: McpTransportKind,
    pub enabled: bool,
    pub status: McpStatus,
    /// Carry sanitized user-facing readiness detail when status alone is insufficient.
    pub status_detail: Option<crate::diagnostic::SanitizedMessage>,
    pub token_count: Option<u64>,
    pub token_estimated: bool,
    #[serde(default)]
    pub tools: Vec<McpToolDefinition>,
    /// Carry sanitized catalog failure context without leaking native error text.
    pub tool_error: Option<crate::diagnostic::SanitizedMessage>,
}

/// Reports a completed provider mutation and any required turn restart.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CatalogMutation {
    pub name: String,
    pub enabled: bool,
    pub restart_required: bool,
}

/// Carries stable session and workspace identity for out-of-band provider catalogs.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderCatalogRequest {
    /// Selects one independent agent conversation without changing portable preference scope.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution_owner: Option<crate::ExecutionOwner>,
    pub harness_session_id: String,
    pub workspace: String,
    pub access: crate::access::AccessPolicy,
    pub approval_mode: ApprovalMode,
    #[serde(alias = "backend_session_id")]
    pub provider_session_id: Option<crate::ProviderSessionId>,
}

impl ProviderCatalogRequest {
    /// Resolve the independent catalog runtime without changing portable preference scope.
    pub fn runtime_key(&self) -> Result<String, crate::ExecutionOwnerError> {
        Ok(self.owner()?.runtime_key())
    }
    /// Validate the connection owner independently of the settings session identity.
    pub fn owner(&self) -> Result<crate::ExecutionOwner, crate::ExecutionOwnerError> {
        crate::ExecutionOwner::resolve(&self.execution_owner, &self.harness_session_id)
    }
}
