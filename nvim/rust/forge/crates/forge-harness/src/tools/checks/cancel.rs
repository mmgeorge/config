use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;

pub(super) const NAME: &str = "check_cancel";

/// Describe the check_cancel tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Stop an active check run identified by check_run or check_list. Supply run_id, for example \
                {run_id:\"run-123\"}. Get cancellation_requested:true, which acknowledges the request rather than \
                completed process cleanup. Preserves recorded output and does not disable the check definition. \
                Use check_status to wait for the terminal state and check_detail afterward for logs. Unknown or \
                inactive run IDs report errors.",
        input_schema: strict_object_input_schema(vec![("run_id", string_schema())], &["run_id"]),
    }
}

/// Execute checks_cancel within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let scope = runtime
        .context
        .checks
        .as_ref()
        .context("check tools require an authenticated root interaction")?;
    let message = scope.invoke(NAME, invocation.arguments).await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
