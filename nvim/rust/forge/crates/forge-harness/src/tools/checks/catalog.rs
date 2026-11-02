use crate::tools::ControlToolDefinition;

/// List the checks tools in presentation order.
pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    vec![
        super::definition_list::definition(),
        super::definition_details::definition(),
        super::definition_edit::definition(),
        super::run::definition(),
        super::status::definition(),
        super::list::definition(),
        super::detail::definition(),
        super::cancel::definition(),
    ]
}

/// Identify a tool owned by the checks family.
pub(in crate::tools) fn is_tool(name: &str) -> bool {
    [
        super::definition_list::NAME,
        super::definition_details::NAME,
        super::definition_edit::NAME,
        super::run::NAME,
        super::status::NAME,
        super::list::NAME,
        super::detail::NAME,
        super::cancel::NAME,
    ]
    .contains(&name)
}

/// Dispatch an accepted checks call to its owning tool.
pub(in crate::tools) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    match invocation.name.as_str() {
        super::definition_list::NAME => {
            super::definition_list::execute(runtime, invocation, output).await
        }
        super::definition_details::NAME => {
            super::definition_details::execute(runtime, invocation, output).await
        }
        super::definition_edit::NAME => {
            super::definition_edit::execute(runtime, invocation, output).await
        }
        super::run::NAME => super::run::execute(runtime, invocation, output).await,
        super::status::NAME => super::status::execute(runtime, invocation, output).await,
        super::list::NAME => super::list::execute(runtime, invocation, output).await,
        super::detail::NAME => super::detail::execute(runtime, invocation, output).await,
        super::cancel::NAME => super::cancel::execute(runtime, invocation, output).await,
        name => crate::tools::error::bail!("unknown checks tool: {name}"),
    }
}

/// Record checks completion fields for provider-neutral replay.
pub(in crate::tools) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    _output: &mut crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<()> {
    let _ = invocation;
    Ok(())
}
