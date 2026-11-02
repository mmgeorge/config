use super::schema::{cursor, filters};
use crate::tools::error::ToolContext;
use crate::tools::{ControlToolDefinition, schema::strict_object_input_schema};
pub(super) const NAME: &str = "harness_session_list";
/// Describe the harness_session_list tool and its input.
pub(super) fn definition() -> ControlToolDefinition {
    let mut properties = filters();
    properties.push(cursor());
    ControlToolDefinition {
        name: NAME,
        description: "List accessible session metadata without conversation bodies. Optional repository_id and \
                workspace narrow results. No filters means all accessible sessions. Follow next_cursor with the \
                same arguments. Repository IDs identify a canonical Git common directory, not a remote URL.",
        input_schema: strict_object_input_schema(properties, &[]),
    }
}

/// Execute harness_session_list within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let scope = runtime
        .context
        .harness
        .as_ref()
        .context("Harness inspection requires an authenticated root interaction")?;
    let message = scope.invoke(NAME, invocation.arguments).await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
