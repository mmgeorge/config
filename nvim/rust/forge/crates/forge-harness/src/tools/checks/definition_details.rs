use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;

pub(super) const NAME: &str = "check_definition_details";

/// Describe the check_definition_details tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Inspect one saved check definition before execution or editing. Supply check_id from \
                check_definition_list, for example {check_id:\"unit\"}. Get its complete trigger/action \
                configuration, command, directory, inputs, defaults, dependencies, and configuration revision. \
                Does not execute a check. Use check_run to start it or check_definition_edit to save changes. Use \
                check_detail with run_id instead for completed execution logs.",
        input_schema: strict_object_input_schema(
            vec![("check_id", string_schema())],
            &["check_id"],
        ),
    }
}

/// Execute checks_definition_details within the current turn authority.
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
