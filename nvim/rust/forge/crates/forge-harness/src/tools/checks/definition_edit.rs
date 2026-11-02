use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;

pub(super) const NAME: &str = "check_definition_edit";

/// Describe the check_definition_edit tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Publish saved check definitions and managed helper changes when check editing is enabled. Read \
                check_definition_list for expected_revision and check_definition_details for definitions to \
                preserve, then supply the complete config map and optional captured helpers. Declare typed \
                action.inputs for fixed argv substitution or input:true JSON callbacks. Commands remain opaque, \
                without runner discovery or rewriting. Get the new revision, changed check/helper names, and \
                checks_executed:false. Every submission requires review outside YOLO. Does not run commands or \
                validate helper syntax. Use check_run separately for execution.",
        input_schema: serde_json::to_value(schemars::schema_for!(crate::checks::ConfigureChecks))
            .expect("check configuration schema is serializable"),
    }
}

/// Execute checks_definition_edit within the current turn authority.
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
