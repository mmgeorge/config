use crate::tools::ControlToolDefinition;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;

pub(super) const NAME: &str = "goal_complete";

/// Describe the goal_complete tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Complete an active Goal task only when its objective and all required work are satisfied. Not for \
                Execute tasks, which use plan_phase_done. End the turn after success.",
        input_schema: strict_object_input_schema(vec![("summary", string_schema())], &["summary"]),
    }
}

/// Execute goals_complete within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
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
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    #[derive(serde::Deserialize)]
    struct Input {
        summary: String,
    }
    let input: Input = serde_json::from_value(invocation.arguments.clone())?;
    if let Some(permit) = &invocation.task_permit {
        output.goal_control = Some(crate::goal::GoalControl {
            permit: permit.clone(),
            termination: crate::goal::GoalTermination::Complete,
            message: input.summary,
        });
    }
    Ok(())
}
