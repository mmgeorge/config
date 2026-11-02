use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use serde_json::json;

pub(super) const NAME: &str = "lsp_paths";

/// Describe paths queries and their accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    let (mut properties, mut required) = super::schema::base_schema("paths");
    let fields = &mut properties;
    let target = json!({"type":"object","additionalProperties":false,"properties":{
                    "path":{"type":"string","minLength":1},
                    "symbol":{"type":"string","minLength":1},
                    "line":{"type":"integer","minimum":1},"column":{"type":"integer","minimum":1}
                },"description":"Use a symbol, optionally scoped to a file, or a file/line/column occurrence. A file/line/symbol selects a unique spelling on that line."});
    fields.remove("path");
    fields.insert("from".into(), target.clone());
    fields.insert("to".into(), target);
    fields.insert("max_depth".into(), json!({"type":"integer","minimum":1,"maximum":8,"default":5,"description":"Maximum call edges in a returned path."}));
    fields.insert(
        "max_paths".into(),
        json!({"type":"integer","minimum":1,"maximum":10,"default":3}),
    );
    fields.insert("max_requests".into(), json!({"type":"integer","minimum":2,"maximum":128,"default":64,"description":"Maximum native LSP requests for the entire search, including endpoint lookup."}));
    required.extend(["from", "to"]);
    ControlToolDefinition {
        name: NAME,
        description: "Find bounded static call paths from one symbol to another. Returns complete aligned symbol trees. \
                Cycles are excluded. Partial searches are explicit. This does not prove runtime reachability.",
        input_schema: json!({"type":"object","additionalProperties":false,"required":required,"properties":properties}),
    }
}

/// Execute lsp_paths within the current turn authority.
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
