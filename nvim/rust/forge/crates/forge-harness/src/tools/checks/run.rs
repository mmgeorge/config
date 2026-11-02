use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;

pub(super) const NAME: &str = "check_run";

/// Describe the check_run tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Start saved checks selected from check_definition_list. Supply check as a name or list and \
                optional declared inputs, for example \
                {check:\"unit\",inputs:{package:\"engine\",test:\"tests::example\"}} when those names are \
                declared. Omit inputs for defaults/context sources, or use all:true for full context, never both. \
                Commands remain opaque and callers cannot replace them. Every admitted request immediately \
                returns only run_id, even if execution finishes immediately. Never waits. Use check_status to \
                wait or poll, check_detail after completion for logs, and check_cancel to stop work. Unavailable \
                during plan execution or while writing agents are active.",
        input_schema: serde_json::to_value(schemars::schema_for!(crate::checks::CheckRunRequest))
            .expect("check run schema is serializable"),
    }
}

/// Execute checks_run within the current turn authority.
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
