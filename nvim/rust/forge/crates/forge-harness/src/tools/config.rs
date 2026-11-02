use crate::tools::error::ToolContext;
use crate::tools::schema::string_schema;
use crate::tools::{ControlToolDefinition, schema::strict_object_input_schema};
pub(in crate::tools) const NAME: &str = "harness_config";
/// Describe the harness_config tool and its input.
pub(in crate::tools) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Read current Harness configuration, approval and access settings, saved permission rules, feature \
                toggles and source/settings locations. Always available in interactive Harness even when session \
                history tools are disabled. No mutation. Follow next_cursor with cursor to retrieve complete \
                configuration.",
        input_schema: strict_object_input_schema(vec![("cursor", string_schema())], &[]),
    }
}

/// Execute harness_config within the current turn authority.
pub(in crate::tools) async fn execute(
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
