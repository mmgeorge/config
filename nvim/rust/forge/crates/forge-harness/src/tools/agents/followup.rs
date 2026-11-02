use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;

pub(super) const NAME: &str = "agent_followup";

/// Describe the agent_followup tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Admit another task into an existing child's retained conversation while its profile is enabled. \
                Serializes busy-child follow-ups with at most 32 pending tasks. Retains the child's provider \
                selection and history. Current session delegation and profile settings govern every new task.",
        input_schema: strict_object_input_schema(
            vec![("target", string_schema()), ("message", string_schema())],
            &["target", "message"],
        ),
    }
}

/// Execute agents_followup within the current turn authority.
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
