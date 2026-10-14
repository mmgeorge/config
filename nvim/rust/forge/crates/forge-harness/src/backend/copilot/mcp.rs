use super::{SharedCopilotSession, mcp_launch_request};
use crate::trace::TraceStore;
use anyhow::{Context, Result};
use futures_util::{StreamExt, stream};
use github_copilot_sdk::rpc::McpDisableRequest;
use github_copilot_sdk::session_events::McpServerStatus;
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::time::Duration;

/// Starts selected servers independently and retains failures for catalog inspection.
/// Failure to enforce the selected server set still rejects the session.
pub(super) async fn start(
    session: &SharedCopilotSession,
    launch: &[(String, Value)],
    trace: &TraceStore,
    harness_session_id: &str,
    timeout: Duration,
) -> Result<HashMap<String, String>> {
    let request = launch
        .iter()
        .map(|(name, payload)| mcp_launch_request(name, payload))
        .collect::<Vec<_>>();
    let mut startup = stream::iter(request)
        .map(|request| async move {
            let name = request.server_name.clone();
            let result = tokio::time::timeout(timeout, session.rpc().mcp().start_server(request))
                .await
                .context("Copilot MCP startup timed out")
                .and_then(|result| result.context("Copilot MCP startup failed"));
            (name, result)
        })
        .buffer_unordered(4);
    let mut failure = HashMap::new();
    while let Some((name, result)) = startup.next().await {
        if let Err(error) = result {
            let error = format!("{error:#}");
            trace.record(
                harness_session_id,
                "copilot.mcp.start_failed",
                json!({"server": name, "error": error}),
            );
            failure.insert(name, error);
        }
    }
    let logged_failure: HashSet<_> = failure.keys().cloned().collect();
    let selected = tokio::time::timeout(timeout, async {
        let loaded = session
            .rpc()
            .mcp()
            .list()
            .await
            .context("inspect Copilot MCP selection")?;
        for server in loaded
            .servers
            .iter()
            .filter(|server| !launch.iter().any(|(name, _)| name == &server.name))
        {
            session
                .rpc()
                .mcp()
                .disable(McpDisableRequest {
                    server_name: server.name.clone(),
                })
                .await
                .with_context(|| {
                    format!("exclude unselected Copilot MCP server {}", server.name)
                })?;
        }
        for (name, _) in launch {
            match loaded.servers.iter().find(|server| &server.name == name) {
                Some(server) if server.status == McpServerStatus::Connected => {
                    failure.remove(name);
                }
                Some(server)
                    if matches!(
                        server.status,
                        McpServerStatus::Failed
                            | McpServerStatus::Disabled
                            | McpServerStatus::NotConfigured
                    ) =>
                {
                    failure.entry(name.clone()).or_insert_with(|| {
                        server
                            .error
                            .clone()
                            .unwrap_or_else(|| format!("Copilot MCP server is {:?}", server.status))
                    });
                }
                None => {
                    failure
                        .entry(name.clone())
                        .or_insert_with(|| "Copilot MCP server was not registered".into());
                }
                _ => {}
            }
        }
        Ok::<_, anyhow::Error>(())
    })
    .await
    .context("Copilot MCP selection verification timed out")
    .and_then(|result| result);
    for (name, error) in &failure {
        if logged_failure.contains(name) {
            continue;
        }
        trace.record(
            harness_session_id,
            "copilot.mcp.start_failed",
            json!({"server": name, "error": error}),
        );
    }
    selected?;
    Ok(failure)
}

#[cfg(test)]
mod test;
