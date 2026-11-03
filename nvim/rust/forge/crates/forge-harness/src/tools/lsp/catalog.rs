use crate::tools::ControlToolDefinition;
/// List source-navigation tools.
pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    let mut tools = vec![
        super::overview::definition(),
        super::symbols::definition(),
        super::definition::definition(),
        super::context::definition(),
        super::references::definition(),
        super::calls::definition(),
        super::paths::definition(),
        super::diagnostics::definition(),
        super::status::definition(),
    ];
    tools.extend(super::rename::definitions());
    tools
}
/// Identify a source-navigation tool.
pub(crate) fn is_tool(name: &str) -> bool {
    if super::rename::is_mutation(name) {
        return true;
    }
    [
        super::overview::NAME,
        super::symbols::NAME,
        super::definition::NAME,
        super::context::NAME,
        super::references::NAME,
        super::calls::NAME,
        super::paths::NAME,
        super::diagnostics::NAME,
        super::status::NAME,
    ]
    .contains(&name)
}

/// Dispatch an accepted lsp call to its owning tool.
pub(in crate::tools) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    if super::rename::is_mutation(&invocation.name) {
        return Box::pin(super::rename::execute(runtime, invocation)).await;
    }
    match invocation.name.as_str() {
        super::overview::NAME => super::overview::execute(runtime, invocation, output).await,
        super::symbols::NAME => super::symbols::execute(runtime, invocation, output).await,
        super::definition::NAME => super::definition::execute(runtime, invocation, output).await,
        super::context::NAME => super::context::execute(runtime, invocation, output).await,
        super::references::NAME => super::references::execute(runtime, invocation, output).await,
        super::calls::NAME => super::calls::execute(runtime, invocation, output).await,
        super::paths::NAME => super::paths::execute(runtime, invocation, output).await,
        super::diagnostics::NAME => super::diagnostics::execute(runtime, invocation, output).await,
        super::status::NAME => super::status::execute(runtime, invocation, output).await,
        name => crate::tools::error::bail!("unknown lsp tool: {name}"),
    }
}

/// Record lsp completion fields for provider-neutral replay.
pub(in crate::tools) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    _output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    let _ = invocation;
    Ok(())
}
