use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use serde_json::json;

pub(super) const NAME: &str = "lsp_diagnostics";

/// Describe diagnostics queries and their accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    let (mut properties, required) = super::schema::base_schema("diagnostics");
    let fields = &mut properties;
    fields.insert("severity".into(), json!({"type":"string","enum":["all","error","warning","information","hint"],"default":"all"}));
    ControlToolDefinition {
        name: NAME,
        description: "Read file diagnostics when path names a file, or aggregate workspace diagnostics when path is \
                omitted or names a directory. Reports server coverage and freshness. Unsupported or failed \
                servers leave explicit gaps. Empty results do not establish a successful project typecheck.",
        input_schema: json!({"type":"object","additionalProperties":false,"required":required,"properties":properties}),
    }
}

/// Execute lsp_diagnostics within the current turn authority.
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
