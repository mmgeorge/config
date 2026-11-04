use crate::tools::ControlToolDefinition;

/// List the questions tools in presentation order.
pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    vec![
        super::ask::definition(),
        super::reply::definition(),
        super::revise::definition(),
        super::withdraw::definition(),
    ]
}

/// Identify a tool owned by the questions family.
pub(in crate::tools) fn is_tool(name: &str) -> bool {
    [
        super::ask::NAME,
        super::reply::NAME,
        super::revise::NAME,
        super::withdraw::NAME,
    ]
    .contains(&name)
}

/// Dispatch an accepted questions call to its owning tool.
pub(in crate::tools) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    match invocation.name.as_str() {
        super::ask::NAME => super::ask::execute(runtime, invocation, output).await,
        super::reply::NAME => super::reply::execute(runtime, invocation, output).await,
        super::revise::NAME => super::revise::execute(runtime, invocation, output).await,
        super::withdraw::NAME => super::withdraw::execute(runtime, invocation, output).await,
        name => crate::tools::error::bail!("unknown questions tool: {name}"),
    }
}

/// Question mutations commit through their owner rather than deferred completion fields.
pub(in crate::tools) fn record(
    _invocation: &crate::tools::ControlToolInvocation,
    _output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    Ok(())
}
