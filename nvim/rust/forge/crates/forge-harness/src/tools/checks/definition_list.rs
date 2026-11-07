use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;

pub(super) const NAME: &str = "check_definition_list";

/// Describe the check_definition_list tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Discover saved check definitions before choosing or editing a check. Call with no arguments. Get \
                check_id, label, category, enablement, automatic selection, declared inputs with descriptions, \
                configuration revision, live automatic-check policy, and whether check editing is currently enabled, without history, logs, or previews. Use \
                check_definition_details with a listed check_id for the full definition, check_run to execute it, \
                or check_definition_edit to publish changes. Use check_list instead to find running jobs.",
        input_schema: strict_object_input_schema(vec![], &[]),
    }
}

/// Execute checks_definition_list within the current turn authority.
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
    let message = scope.invoke(NAME, invocation.arguments).await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
