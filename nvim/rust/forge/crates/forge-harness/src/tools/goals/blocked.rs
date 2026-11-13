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
                cancellation. This records a request; Harness evaluates it after this turn ends.",
        input_schema: strict_object_input_schema(vec![("reason", string_schema())], &["reason"]),
    }
}

/// Execute goals_blocked within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    crate::tools::error::ensure!(
        invocation.arguments["reason"]
            .as_str()
            .is_some_and(|text| !text.trim().is_empty()),
        "goal control requires a nonempty reason"
    );
    runtime.terminal = true;
    Ok(crate::tools::ControlToolResult {
        invocation: Some(invocation),
        message: "Goal transition requested. Finish this turn; Harness will evaluate the request after it ends.".into(),
    })
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    #[derive(serde::Deserialize)]
    struct Input {
        reason: String,
    }
    let input: Input = serde_json::from_value(invocation.arguments.clone())?;
    if let Some(permit) = &invocation.task_permit {
        output.goal_control = Some(crate::goal::GoalControl {
            request_id: format!(
                "goal:{}:{}",
                permit.revision().as_str(),
                invocation
                    .call_id
                    .as_ref()
                    .map(|id| id.as_str())
                    .unwrap_or("legacy")
            ),
            permit: permit.clone(),
            termination: crate::goal::GoalTermination::Blocked,
            message: input.reason,
        });
    }
    Ok(())
}
