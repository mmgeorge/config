use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "agent_tool_output";

/// Describe the agent_tool_output tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Read existing output for a call_ref from agent_activity. Works during execution and after child \
                follow-ups. Offsets and limits count Unicode characters. Default limit 8000, maximum 32000. \
                Follow next_offset for more currently available output. A running call may produce more later. \
                Never executes the tool again.",
        input_schema: strict_object_input_schema(
            vec![
                ("call_ref", string_schema()),
                ("offset", json!({"type":"integer","minimum":0})),
                (
                    "limit",
                    json!({"type":"integer","minimum":1,"maximum":32000}),
                ),
            ],
            &["call_ref"],
        ),
    }
}

/// Execute agents_tool_output within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let scope = runtime
        .context
        .agent
        .as_ref()
        .context("agent tools require an authenticated active root turn")?;
    let message = scope.invoke(NAME, invocation.arguments).await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
