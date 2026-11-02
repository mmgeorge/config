use crate::tools::ControlToolDefinition;

/// List the repository tools in presentation order.
pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    vec![
        super::status::definition(),
        super::changed_paths::definition(),
        super::diff::definition(),
        super::file_diff::definition(),
    ]
}

/// Identify a tool owned by the repository family.
pub(in crate::tools) fn is_tool(name: &str) -> bool {
    [
        super::status::NAME,
        super::changed_paths::NAME,
        super::diff::NAME,
        super::file_diff::NAME,
    ]
    .contains(&name)
}

/// Dispatch an accepted repository call to its owning tool.
pub(in crate::tools) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    match invocation.name.as_str() {
        super::status::NAME => super::status::execute(runtime, invocation, output).await,
        super::changed_paths::NAME => {
            super::changed_paths::execute(runtime, invocation, output).await
        }
        super::diff::NAME => super::diff::execute(runtime, invocation, output).await,
        super::file_diff::NAME => super::file_diff::execute(runtime, invocation, output).await,
        name => crate::tools::error::bail!("unknown repository tool: {name}"),
    }
}

/// Record repository completion fields for provider-neutral replay.
pub(in crate::tools) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    _output: &mut crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<()> {
    let _ = invocation;
    Ok(())
}
