use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;

pub(super) const NAME: &str = "agent_interrupt";

/// Describe the agent_interrupt tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Interrupt one child and cancel its queued follow-ups. Retains its identity, history, and \
                informational inbox. Execution capacity remains reserved until cleanup settles.",
        input_schema: strict_object_input_schema(vec![("target", string_schema())], &["target"]),
    }
}

/// Execute agents_interrupt within the current turn authority.
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
