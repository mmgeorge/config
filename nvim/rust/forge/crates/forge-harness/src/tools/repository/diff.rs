use super::schema::input_schema as repository_input_schema;
use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;

pub(super) const NAME: &str = "repository_diff";

/// Describe the repository_diff tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Inspect staged or unstaged changes in the active interaction repository. At most 8 files per \
                fresh page. Each file retains its complete source and hunks regardless of size. Read-only exact \
                hunks include source hashes.",
        input_schema: repository_input_schema(true, false),
    }
}

/// Execute repository_diff within the current turn authority.
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
