use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use serde_json::json;

pub(super) const NAME: &str = "lsp_references";

/// Describe references queries and their accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    let (mut properties, required) = super::schema::base_schema("references");
    let fields = &mut properties;
    fields.insert(
        "include_declaration".into(),
        json!({"type":"boolean","default":false}),
    );
    ControlToolDefinition {
        name: NAME,
        description: "Find semantic uses, grouped by file and enclosing signature, with one source line before and \
                after each occurrence. Matching rows use >.",
        input_schema: json!({"type":"object","additionalProperties":false,"required":required,"properties":properties}),
    }
}

/// Execute lsp_references within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::backend::BackendCompletion,
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
