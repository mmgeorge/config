use anyhow::{Context, Result};
use forge_provider_api::trace::ProviderTrace;
use futures_util::{StreamExt, stream};
use github_copilot_sdk::rpc::{McpDisableRequest, McpStartServerRequest};
use github_copilot_sdk::session_events::McpServerStatus;
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Duration;

/// Start selected servers independently and retain failures for catalog inspection.
/// Failure to enforce the selected server set still rejects the session.
pub async fn start(
    session: &Arc<super::handle::CopilotSession>,
    launch: &[(String, serde_json::Map<String, Value>)],
    trace: &dyn ProviderTrace,
    harness_session_id: &str,
    timeout: Duration,
) -> Result<HashMap<String, String>> {
    let session = &session.inner;
    // Timed-out starts may remain active natively; exclude unselected servers before launching them.
    trace.record(harness_session_id, "copilot.mcp.selection_begin", json!({}));
    let selected = tokio::time::timeout(timeout, async {
        trace.record(
            harness_session_id,
            "copilot.mcp.selection_step",
            json!({"phase":"list.begin"}),
        );
        let loaded = session
            .rpc()
            .mcp()
            .list_configured()
            .await
            .context("inspect Copilot MCP selection")?;
        trace.record(
            harness_session_id,
            "copilot.mcp.selection_step",
            json!({"phase":"list.complete"}),
        );
        for server in loaded
            .servers
            .iter()
            .filter(|server| server.enabled && !launch.iter().any(|(name, _)| name == &server.name))
        {
            trace.record(
                harness_session_id,
                "copilot.mcp.selection_step",
                json!({"phase":"disable.begin", "server":server.name}),
            );
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
            trace.record(
                harness_session_id,
                "copilot.mcp.selection_step",
                json!({"phase":"disable.complete", "server":server.name}),
            );
        }
        let verified = session
            .rpc()
            .mcp()
            .list_configured()
            .await
            .context("verify excluded Copilot MCP servers")?;
        anyhow::ensure!(
            verified.servers.iter().all(
                |server| !server.enabled || launch.iter().any(|(name, _)| name == &server.name)
            ),
            "Copilot retained an unselected enabled MCP server"
        );
        Ok::<_, anyhow::Error>(())
    })
    .await
    .context("Copilot MCP selection verification timed out")
    .and_then(|result| result);
    match &selected {
        Ok(()) => trace.record(
            harness_session_id,
            "copilot.mcp.selection_complete",
            json!({"status":"ok"}),
        ),
        Err(error) => trace.record(
            harness_session_id,
            "copilot.mcp.selection_complete",
            json!({"status":"failed", "error":format!("{error:#}")}),
        ),
    }
    selected?;
    // Start selected servers independently without extending another server's deadline.
    let request = launch
        .iter()
        .map(|(name, payload)| launch_request(name, &Value::Object(payload.clone())))
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
    // Recheck exclusion before exposing native status for the selected servers.
    let verified = tokio::time::timeout(timeout, session.rpc().mcp().list_configured())
        .await
        .context("Copilot MCP status inspection timed out")?
        .context("inspect Copilot MCP status")?;
    anyhow::ensure!(
        verified
            .servers
            .iter()
            .all(|server| !server.enabled || launch.iter().any(|(name, _)| name == &server.name)),
        "Copilot retained an unselected enabled MCP server after startup"
    );
    for (name, _) in launch {
        match verified.servers.iter().find(|server| &server.name == name) {
            Some(server) if server.enabled && server.live.status == McpServerStatus::Connected => {
                failure.remove(name);
            }
            Some(server) if !server.enabled => {
                failure
                    .entry(name.clone())
                    .or_insert_with(|| "Copilot MCP server is disabled".into());
            }
            Some(server)
                if matches!(
                    server.live.status,
                    McpServerStatus::Failed
                        | McpServerStatus::Disabled
                        | McpServerStatus::NotConfigured
                ) =>
            {
                failure.entry(name.clone()).or_insert_with(|| {
                    server.live.error.clone().unwrap_or_else(|| {
                        format!("Copilot MCP server is {:?}", server.live.status)
                    })
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
    Ok(failure)
}

/// Create one selected server's startup request with an explicit default tool set.
pub fn launch_request(name: &str, payload: &Value) -> McpStartServerRequest {
    let mut config = payload.clone();
    if config.get("tools").is_none() {
        config["tools"] = json!(["*"]);
    }
    McpStartServerRequest {
        server_name: name.to_owned(),
        config: Some(config),
    }
}
