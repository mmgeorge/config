use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;

pub(super) const NAME: &str = "check_discover";

/// Describe discovery without conflating listing with execution evidence.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Discover test item IDs from a saved test adapter. Supply check and optionally within IDs, \
            files, and mode. Static is the default and must not build or invoke test binaries. Exact may build \
            and list binaries but never runs cases; it requires check execution authority and no active writers. \
            Within and files intersect. Discovery returns tree metadata and result accuracy, not passing evidence.",
        input_schema: serde_json::to_value(schemars::schema_for!(
            crate::checks::adapter::CheckDiscoverRequest
        ))
        .expect("discovery request schema is serializable"),
    }
}

/// Discover under the current interaction's configuration and execution authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let scope = runtime
        .context
        .checks
        .as_ref()
        .context("test discovery requires an authenticated root interaction")?;
    let message = scope.invoke(NAME, invocation.arguments).await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
