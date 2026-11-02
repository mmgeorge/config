use super::schema::{cursor, session};
use crate::tools::error::ToolContext;
use crate::tools::{ControlToolDefinition, schema::strict_object_input_schema};
pub(super) const NAME: &str = "harness_session_status";
/// Describe the harness_session_status tool and its input.
pub(super) fn definition() -> ControlToolDefinition {
    let mut properties = vec![session()];
    properties.push(cursor());
    ControlToolDefinition {
        name: NAME,
        description: "Inspect approval mode, filesystem access and active task independently. Omit session_id for \
                current. Current session access is the authenticated turn-start observation. Another session's \
                settings never grant authority.",
        input_schema: strict_object_input_schema(properties, &[]),
    }
}

/// Execute harness_session_status within the current turn authority.
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
