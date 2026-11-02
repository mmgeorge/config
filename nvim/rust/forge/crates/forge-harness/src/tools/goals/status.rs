use crate::tools::ControlToolDefinition;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;

pub(super) const NAME: &str = "goal_status";

/// Describe the goal_status tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Report observed nonterminal progress toward an active Goal task. This does not complete an \
                Execute phase or change permissions.",
        input_schema: strict_object_input_schema(vec![("status", string_schema())], &["status"]),
    }
}

/// Execute goals_status within the current turn authority.
pub(super) async fn execute(
    _runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    Ok(crate::tools::ControlToolResult {
        invocation: Some(invocation),
        message: "Goal control accepted".into(),
    })
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    _invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<()> {
    output.evidence.tool_called = true;
    Ok(())
}
