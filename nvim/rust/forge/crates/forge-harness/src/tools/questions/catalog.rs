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
    output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    match invocation.name.as_str() {
        super::ask::NAME => super::ask::execute(runtime, invocation, output).await,
        super::reply::NAME => super::reply::execute(runtime, invocation, output).await,
        super::revise::NAME => super::revise::execute(runtime, invocation, output).await,
        super::withdraw::NAME => super::withdraw::execute(runtime, invocation, output).await,
        name => crate::tools::error::bail!("unknown questions tool: {name}"),
    }
}

/// Record questions completion fields for provider-neutral replay.
pub(in crate::tools) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<()> {
    match invocation.name.as_str() {
        super::ask::NAME => super::ask::record(invocation, output),
        super::withdraw::NAME => super::withdraw::record(invocation, output),
        _ => Ok(()),
    }
}
