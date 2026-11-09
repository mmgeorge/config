use crate::tools::ControlToolDefinition;

/// List the plan tools in presentation order.
pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    vec![
        super::submit::definition(),
        super::phase_done::definition(),
        super::status::definition(),
        super::capture::definition(),
        super::read::definition(),
        super::edit::definition(),
    ]
}

/// Identify a tool owned by the plan family.
pub(in crate::tools) fn is_tool(name: &str) -> bool {
    [
        super::submit::NAME,
        super::phase_done::NAME,
        super::status::NAME,
        super::capture::NAME,
        super::read::NAME,
        super::edit::NAME,
    ]
    .contains(&name)
}

/// Dispatch an accepted plan call to its owning tool.
pub(in crate::tools) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    match invocation.name.as_str() {
        super::submit::NAME => super::submit::execute(runtime, invocation, output).await,
        super::phase_done::NAME => super::phase_done::execute(runtime, invocation, output).await,
        super::status::NAME => super::status::execute(runtime, invocation, output).await,
        super::capture::NAME => super::capture::execute(runtime, invocation, output).await,
        super::read::NAME => super::read::execute(runtime, invocation, output).await,
        super::edit::NAME => super::edit::execute(runtime, invocation, output).await,
        name => crate::tools::error::bail!("unknown plan tool: {name}"),
    }
}

/// Record plan completion fields for provider-neutral replay.
pub(in crate::tools) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    match invocation.name.as_str() {
        super::submit::NAME => super::submit::record(invocation, output),
        super::status::NAME => super::status::record(invocation, output),
        super::read::NAME => super::read::record(invocation, output),
        super::edit::NAME => super::edit::record(invocation, output),
        _ => Ok(()),
    }
}
