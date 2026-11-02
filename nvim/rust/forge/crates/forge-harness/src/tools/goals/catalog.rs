use crate::tools::ControlToolDefinition;

/// List the goals tools in presentation order.
pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    vec![
        super::complete::definition(),
        super::blocked::definition(),
        super::status::definition(),
    ]
}

/// Identify a tool owned by the goals family.
pub(in crate::tools) fn is_tool(name: &str) -> bool {
    [
        super::complete::NAME,
        super::blocked::NAME,
        super::status::NAME,
    ]
    .contains(&name)
}

/// Dispatch an accepted goals call to its owning tool.
pub(in crate::tools) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    match invocation.name.as_str() {
        super::complete::NAME => super::complete::execute(runtime, invocation, output).await,
        super::blocked::NAME => super::blocked::execute(runtime, invocation, output).await,
        super::status::NAME => super::status::execute(runtime, invocation, output).await,
        name => crate::tools::error::bail!("unknown goals tool: {name}"),
    }
}

/// Record goals completion fields for provider-neutral replay.
pub(in crate::tools) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    match invocation.name.as_str() {
        super::complete::NAME => super::complete::record(invocation, output),
        super::blocked::NAME => super::blocked::record(invocation, output),
        super::status::NAME => super::status::record(invocation, output),
        _ => Ok(()),
    }
}
