use forge_provider_api::{
    ProviderError,
    capability::UnsupportedCapability,
    session::{McpLaunch, McpTransport},
};
use serde_json::{Value, json};

/// Encode admitted MCP launches only after checking native transport limitations.
pub(super) fn mcp_launches(
    operation: forge_provider_api::ProviderOperation,
    launches: &[McpLaunch],
) -> Result<Vec<Value>, ProviderError> {
    let mut names = std::collections::HashSet::with_capacity(launches.len());
    launches.iter().map(|launch| {
        if launch.name == super::permission::HOST_TOOL_SERVER || !names.insert(&launch.name) {
            return Err(ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                operation,
                forge_provider_api::ProviderFailureKind::Rejected,
            )));
        }
        if mcp_launch_incompatibility(launch).is_some() {
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
            McpTransport::Sse { .. } => unreachable!("launch compatibility excludes SSE"),
        })
    }).collect()
}

/// Describe the first launch option an Antigravity session cannot honor.
pub(crate) fn mcp_launch_incompatibility(launch: &McpLaunch) -> Option<&'static str> {
    if matches!(launch.transport, McpTransport::Sse { .. }) {
        Some("Antigravity requires stdio or Streamable HTTP. Replace legacy SSE in the MCP definition")
    } else if launch.timeout.is_some() {
        Some("Antigravity does not support a per-server timeout. Remove the timeout from the MCP definition")
    } else if launch.working_directory.is_some() {
        Some("Antigravity uses the workspace for stdio servers. Remove the working directory from the MCP definition")
    } else if matches!(launch.tools, Some(forge_provider_api::session::McpToolSelection::Only(_))) {
        Some("Antigravity cannot restrict a server to selected tools")
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{ProviderOperation, capability::ProviderFeature};

    /// Reject legacy SSE before sending a native open that only supports Streamable HTTP.
    #[test]
    fn rejects_legacy_sse_transport() {
        let launch = McpLaunch {
            name: "fixture".into(),
            transport: McpTransport::Sse {
                url: "http://localhost/sse".into(),
                headers: Default::default(),
            },
            timeout: None,
            working_directory: None,
            tools: None,
        };
        let result = mcp_launches(ProviderOperation::ThreadOpen, &[launch]);
        assert!(matches!(
            result,
            Err(ProviderError::Unsupported(UnsupportedCapability(
                ProviderFeature::McpLaunchRequirements,
            )))
        ));
    }
}
