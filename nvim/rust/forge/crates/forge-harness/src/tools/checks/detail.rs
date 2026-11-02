use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;

pub(super) const NAME: &str = "check_detail";

/// Describe the check_detail tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Inspect execution details after check_status reports completion. Supply run_id from check_run, \
                optional stream (combined by default, stdout, or stderr), and optional cursor, for example \
                {run_id:\"run-123\",stream:\"stderr\"}. Get exact executable/argv, working directory, resolved \
                inputs, exit status, timing, reuse provenance, and a log page. Follow next_cursor with the same \
                run_id and stream until null to read every selected command's complete log. Running jobs return \
                only a message to wait with check_status. Unknown IDs, invalid cursors, and log I/O or decoding \
                failures report errors. Use check_definition_details with check_id instead for saved \
                configuration.",
        input_schema: serde_json::to_value(schemars::schema_for!(crate::checks::DetailRequest))
            .expect("check detail schema is serializable"),
    }
}

/// Execute checks_detail within the current turn authority.
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
