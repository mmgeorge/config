use serde::{Deserialize, Serialize};

/// Identifies the definition's scope independently of its enablement preference.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ResourceScope {
    /// Makes a definition available across workspaces.
    Global,
    /// Makes a definition available in its owning workspace.
    Workspace,
}

/// Describe provider-owned resource discovery and enforceable selection support.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
pub struct CatalogCapability {
    /// Enforceable skill discovery/selection support advertised by the integration.
    pub skill: bool,
    /// Enforceable MCP discovery/selection support advertised by the integration.
    pub mcp: bool,
}

/// Describes one provider skill exposed to Harness callers.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SkillDefinition {
    /// Identifies the shared definition when the skill has an explicit source binding.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub resource_id: Option<String>,
    /// Records the definition's scope when Forge owns its source.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub scope: Option<ResourceScope>,
    /// Provider-visible skill identity, opaque outside catalog selection.
    pub name: String,
    /// User-facing description supplied by the owning catalog.
    pub description: String,
    /// Effective selection state observed for this query target.
    pub enabled: bool,
    /// Whether a user may explicitly select this skill as input.
    pub user_invocable: bool,
    /// Optional catalog-exposed skill location; not an instruction to read or execute it.
    pub path: Option<String>,
    /// Optional native source label for display.
    pub source: Option<String>,
    /// Optional user-facing hint for explicit skill arguments.
    pub argument_hint: Option<String>,
}

/// Defines the normalized lifecycle state shown by the MCP picker.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum McpStatus {
    /// The selected configuration excludes this server.
    Disabled,
    /// Readiness has not completed in an observed runtime.
    Loading,
    /// The existing runtime positively reports a ready connection.
    Connected,
    /// Readiness requires explicit authentication outside this read operation.
    NeedsAuthentication,
    /// The existing runtime reported connection or tool discovery failure.
    Failed,
    /// No authoritative live readiness is available for this configured server.
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
    /// Opaque provider-normalized tool identity within its owning server.
    pub name: String,
    /// Optional user-facing description of the observed tool.
    pub description: Option<String>,
}

/// Describes one complete provider MCP row and its cached tool inventory.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct McpDefinition {
    /// Identifies the shared definition independently of its provider-visible name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub resource_id: Option<String>,
    /// Records the definition's scope when Forge owns its source.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub scope: Option<ResourceScope>,
    /// Identifies the source configuration without exposing launch credentials.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_path: Option<String>,
    /// Normalized server identity within this query target.
    pub name: String,
    /// Normalized transport category; native launch credentials are excluded.
    pub transport: McpTransportKind,
    /// Selected configuration state, independent of live readiness.
    pub enabled: bool,
    /// Observed readiness, or Unavailable when only configured definitions are known.
    pub status: McpStatus,
    /// Carry sanitized user-facing readiness detail when status alone is insufficient.
    pub status_detail: Option<crate::diagnostic::SanitizedMessage>,
    /// Optional observed or estimated context cost of this resource.
    pub token_count: Option<u64>,
    /// Whether token_count is an estimate instead of an authoritative native count.
    pub token_estimated: bool,
    /// Observed cached tool inventory; configured mode need not populate it.
    #[serde(default)]
    pub tools: Vec<McpToolDefinition>,
    /// Carry sanitized catalog failure context without leaking native error text.
    pub tool_error: Option<crate::diagnostic::SanitizedMessage>,
}

/// Own an explicit workspace discovery target without binding or configuration authority.
/// Discovery may use adapter-owned read-only runtime resources, but this value cannot select
/// a writable history, import preferences, or configure an already active binding.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct WorkspaceContext {
    /// Select the directory used for native discovery.
    pub workspace: String,
}

/// Own one exhaustive discovery target, with no optional owner/session fallback precedence.
/// A Binding query resolves only that live generation. Missing or retired bindings reject
/// rather than silently opening another session or switching to workspace discovery.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum QueryContext {
    /// Inspect definitions and catalogs without opening a writable session.
    Workspace(WorkspaceContext),
    /// Inspect the existing exact live binding; never implicitly open or restart it.
    Binding(crate::BindingId),
}

/// Distinguish fast configured definitions from potentially slower readiness observation.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum McpQuery {
    /// Read configured definitions without starting or waiting for MCP servers.
    Configured(QueryContext),
    /// Observe readiness and tools on an existing runtime without changing configuration.
    Observed(QueryContext),
}
