use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "agent_activity";

/// Describe the agent_activity tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Inspect a child's active run, or its latest run when idle. Returns chronological recent thoughts \
                (up to 4000 characters each) and tool metadata with stable call_ref values, without tool output. \
                Includes elapsed time, last activity, and approval state. Defaults to 5 entries. Pass limit \
                (1–20) to request a different count. Does not message or restart the child.",
        input_schema: strict_object_input_schema(
            vec![
                ("target", string_schema()),
                ("limit", json!({"type":"integer","minimum":1,"maximum":20})),
            ],
            &["target"],
        ),
    }
}

/// Execute agents_activity within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::backend::BackendCompletion,
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
