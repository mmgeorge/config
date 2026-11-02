use super::schema::{cursor, session};
use crate::tools::error::ToolContext;
use crate::tools::{
    ControlToolDefinition,
    schema::{strict_object_input_schema, string_schema},
};
use serde_json::json;
pub(super) const NAME: &str = "harness_session_tool";
/// Describe the harness_session_tool tool and its input.
pub(super) fn definition() -> ControlToolDefinition {
    let mut properties = vec![
        session(),
        ("exchange_id", string_schema()),
        ("tool_call_id", string_schema()),
        ("turn_id", string_schema()),
        (
            "fields",
            json!({"type":"array","minItems":1,"uniqueItems":true,"items":{"enum":["input","output","normalized_changes"]}}),
        ),
    ];
    properties.push(cursor());
    ControlToolDefinition {
        name: NAME,
        description: "Read selected details of a tool occurrence without executing it. Omit session_id for current. \
                turn_id disambiguates reused tool IDs. fields defaults to output. input is explicitly unavailable \
                if not retained. Large outputs continue without truncation.",
        input_schema: strict_object_input_schema(properties, &["exchange_id", "tool_call_id"]),
    }
}

/// Execute harness_session_tool within the current turn authority.
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
