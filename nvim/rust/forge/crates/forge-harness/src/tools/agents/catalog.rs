use crate::tools::ControlToolDefinition;

/// List the agents tools in presentation order.
pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    vec![
        super::spawn::definition(),
        super::send::definition(),
        super::followup::definition(),
        super::interrupt::definition(),
        super::list::definition(),
        super::activity::definition(),
        super::tool_output::definition(),
    ]
}

/// Identify a tool owned by the agents family.
pub(crate) fn is_tool(name: &str) -> bool {
    [
        super::spawn::NAME,
        super::send::NAME,
        super::followup::NAME,
        super::interrupt::NAME,
        super::list::NAME,
        super::activity::NAME,
        super::tool_output::NAME,
    ]
    .contains(&name)
}

/// Dispatch an accepted agents call to its owning tool.
pub(in crate::tools) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    match invocation.name.as_str() {
        super::spawn::NAME => super::spawn::execute(runtime, invocation, output).await,
        super::send::NAME => super::send::execute(runtime, invocation, output).await,
        super::followup::NAME => super::followup::execute(runtime, invocation, output).await,
        super::interrupt::NAME => super::interrupt::execute(runtime, invocation, output).await,
        super::list::NAME => super::list::execute(runtime, invocation, output).await,
        super::activity::NAME => super::activity::execute(runtime, invocation, output).await,
        super::tool_output::NAME => super::tool_output::execute(runtime, invocation, output).await,
        name => crate::tools::error::bail!("unknown agents tool: {name}"),
    }
}

/// Record agents completion fields for provider-neutral replay.
pub(in crate::tools) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    _output: &mut crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<()> {
    let _ = invocation;
    Ok(())
}
