use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;

pub(super) const NAME: &str = "check_list";

/// Describe the check_list tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Find currently running check jobs in this session. Call with no arguments. Get runs containing \
                the same compact summaries as check_status, including each run_id and latest nonempty output \
                line. An empty runs array means no jobs are running, not that checks passed. Omits completed \
                history and saved definitions, and never starts or waits for work. Use check_status with a \
                returned run_id to wait, check_cancel to stop it, or check_definition_list to discover saved \
                checks.",
        input_schema: strict_object_input_schema(vec![], &[]),
    }
}

/// Execute checks_list within the current turn authority.
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
