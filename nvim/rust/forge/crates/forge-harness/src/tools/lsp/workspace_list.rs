use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use serde_json::json;

pub(super) const NAME: &str = "lsp_workspace_list";

/// Describe workspace_list queries and their accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    let (properties, required) = super::schema::base_schema("workspace_list");

    ControlToolDefinition {
        name: NAME,
        description: "List authorized Cargo.toml and package.json workspaces without starting language servers.",
        input_schema: json!({"type":"object","additionalProperties":false,"required":required,"properties":properties}),
    }
}

/// Execute lsp_workspace_list within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let scope = runtime
        .context
        .lsp
        .as_ref()
        .context("LSP tools require an enabled active editor interaction")?;
    let arguments =
        super::request::Arguments::parse(NAME, invocation.arguments).context("invalid_argument")?;
    let message = scope.query(arguments).await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
