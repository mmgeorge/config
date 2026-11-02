use super::schema::{cursor, session};
use crate::tools::error::ToolContext;
use crate::tools::{ControlToolDefinition, schema::strict_object_input_schema};
pub(super) const NAME: &str = "harness_session_exchange_list";
/// Describe the harness_session_exchange_list tool and its input.
pub(super) fn definition() -> ControlToolDefinition {
    let mut properties = vec![session()];
    properties.push(cursor());
    ControlToolDefinition {
        name: NAME,
        description: "Read ordered user messages and final answers without commentary, reasoning or tool output. Omit \
                session_id for the current session. Follow next_cursor with the same arguments. Incomplete \
                exchanges remain explicit.",
        input_schema: strict_object_input_schema(properties, &[]),
    }
}

/// Execute harness_session_exchange_list within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::backend::BackendCompletion,
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
