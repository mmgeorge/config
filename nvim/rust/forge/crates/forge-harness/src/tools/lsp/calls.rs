use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use serde_json::json;

pub(super) const NAME: &str = "lsp_calls";

/// Describe calls queries and their accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    let (mut properties, mut required) = super::schema::base_schema("calls");
    let fields = &mut properties;
    fields.insert(
        "direction".into(),
        json!({"type":"string","enum":["incoming","outgoing"]}),
    );
    required.push("direction");
    ControlToolDefinition {
        name: NAME,
        description: "Find one level of incoming callers or outgoing callees. Returns a symbol tree with aligned \
                definition locations. The root is the selected symbol. Incoming children call the root. The root \
                calls outgoing children.",
        input_schema: json!({"type":"object","additionalProperties":false,"required":required,"properties":properties}),
    }
}

/// Execute lsp_calls within the current turn authority.
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
