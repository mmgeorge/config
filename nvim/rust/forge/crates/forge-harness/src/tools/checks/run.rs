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
                declared. Test adapters accept tests:{ids:[\"opaque item ID\"],affected:[\"src/file\"]}. \
                Typecheck, format, lint, and build adapters accept paths:[\"src/file\"]. \
                Format and lint adapters accept apply:true for source-changing fixes. \
                Omit selection for defaults/context sources, or use all:true for full context. Never combine \
                inputs, tests, paths, and all. \
                Commands remain opaque and callers cannot replace them. Every admitted request immediately \
                returns only run_id, even if execution finishes immediately. Harness delivers a check_result \
                message after collection; continue independent work or end the turn to wait, without polling. \
                Use check_status for explicit inspection, recovery, or cancellation confirmation, check_detail \
                for additional diagnostics and logs, and check_cancel to stop work. Unavailable \
                while writing agents are active. During plan execution, verification requires Verify \
                and apply requires Resolve.",
        input_schema: serde_json::to_value(schemars::schema_for!(crate::checks::CheckRunRequest))
            .expect("check run schema is serializable"),
    }
}

/// Execute checks_run within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let scope = runtime
        .context
        .checks
        .as_ref()
        .context("check tools require an authenticated root interaction")?;
    let identity = invocation
        .call_id
        .context("check_run requires authenticated tool-call identity")?;
    let message = scope
        .invoke_with_identity(NAME, invocation.arguments, identity)
        .await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
