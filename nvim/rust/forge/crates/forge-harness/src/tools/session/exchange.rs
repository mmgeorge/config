use super::schema::{cursor, session};
use crate::tools::error::ToolContext;
use crate::tools::{
    ControlToolDefinition,
    schema::{strict_object_input_schema, string_schema},
};
pub(super) const NAME: &str = "harness_session_exchange";
/// Describe the harness_session_exchange tool and its input.
pub(super) fn definition() -> ControlToolDefinition {
    let mut properties = vec![session(), ("exchange_id", string_schema())];
    properties.push(cursor());
    ControlToolDefinition {
        name: NAME,
        description: "Inspect one exchange conversation and compact tool/decision/check activity. Omit session_id for \
                current. Does not hydrate tool output. Use returned tool_call_id and turn_id for exact tool \
                details.",
        input_schema: strict_object_input_schema(properties, &["exchange_id"]),
    }
}

/// Execute harness_session_exchange within the current turn authority.
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
