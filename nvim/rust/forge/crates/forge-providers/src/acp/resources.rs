use forge_provider_api::{
    ProviderError,
    capability::UnsupportedCapability,
    session::{McpLaunch, McpTransport},
};
use serde_json::{Value, json};

/// Encode admitted MCP launches only after checking native transport limitations.
pub(super) fn mcp_launches(launches: &[McpLaunch]) -> Result<Vec<Value>, ProviderError> {
    let mut names = std::collections::HashSet::with_capacity(launches.len());
    launches.iter().map(|launch| {
        if launch.name == "forge-harness-control" || !names.insert(&launch.name) {
            return Err(ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::SessionOpen,
                forge_provider_api::ProviderFailureKind::Rejected,
            )));
        }
        if launch.timeout.is_some()
            || launch.working_directory.is_some()
            || matches!(launch.tools, Some(forge_provider_api::session::McpToolSelection::Only(_)))
        {
            return Err(UnsupportedCapability(forge_provider_api::capability::ProviderFeature::McpLaunchRequirements).into());
        }
        let environment = |values: &std::collections::BTreeMap<String, String>| {
            values.iter().map(|(name, value)| json!({"name": name, "value": value})).collect::<Vec<_>>()
        };
        Ok(match &launch.transport {
            McpTransport::Stdio { command, arguments, environment: variables } => json!({
                "name": launch.name, "command": command, "args": arguments,
                "env": environment(variables),
            }),
            McpTransport::Http { url, headers } => json!({
                "type": "http", "name": launch.name, "url": url, "headers": environment(headers),
            }),
            McpTransport::Sse { url, headers } => json!({
                "type": "sse", "name": launch.name, "url": url, "headers": environment(headers),
            }),
        })
    }).collect()
}
