use super::schema::input_schema as repository_input_schema;
use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;

pub(super) const NAME: &str = "repository_file_diff";

/// Describe the repository_file_diff tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Inspect a changed file selected by exact path_hex from repository status. Inherits active \
                interaction workspace and read policy. Returns bounded read-only staged or unstaged hunks.",
        input_schema: repository_input_schema(true, true),
    }
}

/// Execute repository_file_diff within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::backend::BackendCompletion,
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
