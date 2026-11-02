use crate::tools::ControlToolDefinition;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;

pub(super) const NAME: &str = "goal_blocked";

/// Describe the goal_blocked tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Report an active Goal task blocked when no authorized progress remains, with a concrete cause and \
                what is needed to resume. Not for Execute tasks. Do not use for ordinary effort or user \
                cancellation. End the turn after success.",
        input_schema: strict_object_input_schema(vec![("reason", string_schema())], &["reason"]),
    }
}

/// Execute goals_blocked within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    runtime.terminal = true;
    Ok(crate::tools::ControlToolResult {
        invocation: Some(invocation),
        message: "Goal control accepted".into(),
    })
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<()> {
    output.evidence.structured_blocked = true;
    if let Some(permit) = &invocation.task_permit {
        output.goal_control = Some(crate::goal::GoalControl {
            permit: permit.clone(),
            termination: crate::goal::GoalTermination::Blocked,
        });
    }
    Ok(())
}
