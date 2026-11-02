use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;

pub(super) const NAME: &str = "agent_spawn";

/// Describe the agent_spawn tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Start a fresh child using a user-configured profile and task text. The profile owns provider, \
                model, thinking, speed, and instructions. Omit profile to use the configured default. Returns its \
                unique identity without waiting for completion. Child results arrive as separate messages. When \
                only delegated work remains, end your turn. Harness waits for the children and resumes you with \
                their results. New user input can resume you sooner.",
        input_schema: strict_object_input_schema(
            vec![("profile", string_schema()), ("text", string_schema())],
            &["text"],
        ),
    }
}

/// Execute agents_spawn within the current turn authority.
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
