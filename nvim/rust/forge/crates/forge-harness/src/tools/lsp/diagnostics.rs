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
        description: "Use after edits for fast diagnostic checks. Refresh the named file, or changed files in the \
                selected workspace, then return all matching push and pull diagnostics from its language server. \
                Reports errors, warnings, information, and hints; request failures are explicit.",
        input_schema: json!({"type":"object","additionalProperties":false,"required":required,"properties":properties}),
    }
}

/// Execute lsp_diagnostics within the current turn authority.
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
    let diagnostic_paths = match (&arguments.path, &runtime.context.repository) {
        (None, Some(repository)) => repository.diagnostic_paths().await?,
        (Some(path), Some(repository)) if scope.workspace.join(path).is_dir() => {
            repository.diagnostic_paths().await?
        }
        _ => Vec::new(),
    };
    let message = scope
        .query_with_diagnostics(arguments, diagnostic_paths)
        .await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
