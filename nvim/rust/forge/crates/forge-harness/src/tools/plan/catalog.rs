use crate::tools::ControlToolDefinition;

/// List the plan tools in presentation order.
pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    vec![
        super::submit::definition(),
        super::phase_done::definition(),
        super::file_edit::definition(),
        super::file_read::definition(),
        super::overview_read::definition(),
        super::overview_edit::definition(),
    ]
}

/// Identify a tool owned by the plan family.
pub(in crate::tools) fn is_tool(name: &str) -> bool {
    [
        super::submit::NAME,
        super::phase_done::NAME,
        super::file_edit::NAME,
        super::file_read::NAME,
        super::overview_read::NAME,
        super::overview_edit::NAME,
    ]
    .contains(&name)
}

/// Dispatch an accepted plan call to its owning tool.
pub(in crate::tools) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    match invocation.name.as_str() {
        super::submit::NAME => super::submit::execute(runtime, invocation, output).await,
        super::phase_done::NAME => super::phase_done::execute(runtime, invocation, output).await,
        super::file_edit::NAME => super::file_edit::execute(runtime, invocation, output).await,
        super::file_read::NAME => super::file_read::execute(runtime, invocation, output).await,
        super::overview_read::NAME => {
            super::overview_read::execute(runtime, invocation, output).await
        }
        super::overview_edit::NAME => {
            super::overview_edit::execute(runtime, invocation, output).await
        }
        name => crate::tools::error::bail!("unknown plan tool: {name}"),
    }
}

/// Record plan completion fields for provider-neutral replay.
pub(in crate::tools) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<()> {
    match invocation.name.as_str() {
        super::submit::NAME => super::submit::record(invocation, output),
        super::file_edit::NAME => super::file_edit::record(invocation, output),
        super::file_read::NAME => super::file_read::record(invocation, output),
        super::overview_read::NAME => super::overview_read::record(invocation, output),
        super::overview_edit::NAME => super::overview_edit::record(invocation, output),
        _ => Ok(()),
    }
}
