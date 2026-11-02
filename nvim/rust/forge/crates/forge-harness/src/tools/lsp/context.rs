use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use serde_json::json;

pub(super) const NAME: &str = "lsp_context";

/// Describe context queries and their accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    let (mut properties, required) = super::schema::base_schema("context");
    let fields = &mut properties;
    fields.insert("include".into(), json!({"type":"array","uniqueItems":true,"maxItems":4,"items":{"type":"string","enum":["references","callers","callees","implementations"]},"description":"Optional related signatures, at most five per relation, sharing the budget."}));
    fields.insert("start_line".into(), json!({"type":"integer","minimum":1,"description":"Continue at returned next_line, preserving the original target position."}));
    ControlToolDefinition {
        name: NAME,
        description: "Read the containing declaration, source docs, and full body within token_budget. Optional \
                semantic relationships are signature summaries. Live unsaved source is included.",
        input_schema: json!({"type":"object","additionalProperties":false,"required":required,"properties":properties}),
    }
}

/// Execute lsp_context within the current turn authority.
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
