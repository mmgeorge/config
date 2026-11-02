use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "check_status";

/// Describe the check_status tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Poll or wait for a run identified by check_run or check_list. Supply run_id and optional wait_ms \
                from 0 to 60000, default 0, for example {run_id:\"run-123\",wait_ms:60000}. Waiting ends on \
                completion or the original deadline without cancelling execution. Get compact state, exit code, \
                progress, elapsed time, latest nonempty output line, failure reason, and executed/reused counts, \
                without logs or configuration snapshots. Use check_detail after completion for exact invocation \
                and full paginated logs, or check_cancel to request cancellation.",
        input_schema: strict_object_input_schema(
            vec![
                ("run_id", string_schema()),
                (
                    "wait_ms",
                    json!({"type":"integer", "minimum":0,"maximum":60000}),
                ),
            ],
            &["run_id"],
        ),
    }
}

/// Execute checks_status within the current turn authority.
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
