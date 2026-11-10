use crate::tools::ControlToolDefinition;

/// Toolset name shared by every session-history tool; `toolsets` tests keep it in step.
pub(in crate::tools) const TOOLSET: &str = "harness_session_*";

pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    vec![
        super::list::definition(),
        super::search::definition(),
        super::exchange_list::definition(),
        super::exchange::definition(),
        super::tool::definition(),
        super::log::definition(),
        super::status::definition(),
    ]
}
pub(in crate::tools) fn is_tool(name: &str) -> bool {
    [
        super::list::NAME,
        super::search::NAME,
        super::exchange_list::NAME,
        super::exchange::NAME,
        super::tool::NAME,
        super::log::NAME,
        super::status::NAME,
    ]
    .contains(&name)
}

/// Dispatch an accepted session call to its owning tool.
pub(in crate::tools) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    match invocation.name.as_str() {
        super::list::NAME => super::list::execute(runtime, invocation, output).await,
        super::search::NAME => super::search::execute(runtime, invocation, output).await,
        super::exchange_list::NAME => {
            super::exchange_list::execute(runtime, invocation, output).await
        }
        super::exchange::NAME => super::exchange::execute(runtime, invocation, output).await,
        super::tool::NAME => super::tool::execute(runtime, invocation, output).await,
        super::log::NAME => super::log::execute(runtime, invocation, output).await,
        super::status::NAME => super::status::execute(runtime, invocation, output).await,
        name => crate::tools::error::bail!("unknown session tool: {name}"),
    }
}

/// Record session completion fields for provider-neutral replay.
pub(in crate::tools) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    _output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    let _ = invocation;
    Ok(())
}
