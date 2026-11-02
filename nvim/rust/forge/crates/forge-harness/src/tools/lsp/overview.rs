use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use serde_json::json;

pub(super) const NAME: &str = "lsp_overview";

/// Describe overview queries and their accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    let (mut properties, required) = super::schema::base_schema("overview");
    let fields = &mut properties;
    fields.insert(
        "declaration_depth".into(),
        json!({"type":"integer","minimum":1,"maximum":8,"default":2}),
    );
    ControlToolDefinition {
        name: NAME,
        description: "Map public declarations, members, and methods under a directory. Tree-sitter supplies source \
                structure. Reuse returned positions with context and semantic tools.",
        input_schema: json!({"type":"object","additionalProperties":false,"required":required,"properties":properties}),
    }
}

/// Execute lsp_overview within the current turn authority.
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
