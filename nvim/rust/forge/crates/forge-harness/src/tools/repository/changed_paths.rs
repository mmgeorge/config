use super::schema::input_schema as repository_input_schema;
use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;

pub(super) const NAME: &str = "repository_changed_paths";

/// Describe the repository_changed_paths tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "List changed repository paths with exact path_hex identities for the active interaction. At most \
                256 paths per fresh page.",
        input_schema: repository_input_schema(false, false),
    }
}

/// Execute repository_changed_paths within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let scope = runtime
        .context
        .repository
        .as_ref()
        .context("repository inspection requires an authenticated active interaction")?;
    let message = scope.invoke(NAME, invocation.arguments).await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
