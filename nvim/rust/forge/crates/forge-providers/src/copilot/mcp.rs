use anyhow::{Context, Result};
use futures_util::{FutureExt, StreamExt, stream};
use github_copilot_sdk::rpc::{McpDisableRequest, McpStartServerRequest};
use github_copilot_sdk::session_events::McpServerStatus;
use serde_json::{Value, json};
use std::collections::HashMap;
use std::sync::Arc;

/// Disable every enabled server outside the selected set before the binding is exposed.
///
/// These are bounded native configuration calls, so they run under the open deadline; failure to
/// enforce the selected server set rejects the session.
pub async fn exclude_unselected(
    session: &Arc<super::handle::CopilotSession>,
    launch: &[(String, serde_json::Map<String, Value>)],
) -> Result<()> {
    let session = &session.inner;
    super::operation::wait(async {
        let loaded = session
            .rpc()
            .mcp()
            .list_configured()
            .await
            .context("inspect Copilot MCP selection")?;
        for server in loaded
            .servers
            .iter()
            .filter(|server| server.enabled && !launch.iter().any(|(name, _)| name == &server.name))
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
    .and_then(|result| result)
}

/// Put every selected server's start request on the wire, then settle them in the returned future.
///
/// Native startup waits for each server's handshake, which a slow or hung server can hold for the
/// runtime's own startup bound. Callers therefore settle the returned future after the binding
/// opens, outside any host operation deadline; until it settles, the catalog reports the servers'
/// live native status.
///
/// The SDK commits a request frame on its first poll, so this synchronous pass sends every start
/// before the binding can accept input: a hung server cannot delay another server's start, and
/// Copilot's foreground load already exists when accepted input releases it through
/// [`super::handle::CopilotSession::release_mcp_loading`].
pub fn start_selected(
    binding: Arc<super::handle::CopilotSession>,
    launch: Vec<(String, serde_json::Map<String, Value>)>,
) -> impl std::future::Future<Output = Result<HashMap<String, String>>> + Send + 'static {
    let mut startup = launch
        .iter()
        .map(|(name, payload)| launch_request(name, &Value::Object(payload.clone())))
        .map(|request| {
            let session = binding.inner.clone();
            async move {
                let name = request.server_name.clone();
                // The first poll runs inside the open operation, whose deadline must not bound a
                // startup the native runtime already bounds; the request is awaited unwrapped.
                let result = session
                    .rpc()
                    .mcp()
                    .start_server(request)
                    .await
                    .context("Copilot MCP startup failed");
                (name, result)
            }
        })
        .collect::<stream::FuturesUnordered<_>>();
    let mut settled = Vec::new();
    while let Some(Some(result)) = startup.next().now_or_never() {
        settled.push(result);
    }
    async move {
        settled.extend(startup.collect::<Vec<_>>().await);
        settle_selected(&binding, &launch, settled).await
    }
}

/// Combine startup results with the verified native status of every selected server.
async fn settle_selected(
    binding: &Arc<super::handle::CopilotSession>,
    launch: &[(String, serde_json::Map<String, Value>)],
    settled: Vec<(String, Result<()>)>,
) -> Result<HashMap<String, String>> {
    let session = &binding.inner;
    let mut failure = HashMap::new();
    for (name, result) in settled {
        if result.is_err() {
            failure.insert(name, "MCP server startup failed".to_owned());
        }
    }
    // Startup can re-enable servers natively; enforce the selected set again before reading status.
    exclude_unselected(binding, launch).await?;
    let verified = super::operation::wait(session.rpc().mcp().list_configured())
        .await
        .context("Copilot MCP status inspection timed out")?
        .context("inspect Copilot MCP status")?;
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
                failure
                    .entry(name.clone())
                    .or_insert_with(|| "MCP server reported a failed state".to_owned());
            }
            None => {
                failure
                    .entry(name.clone())
                    .or_insert_with(|| "Copilot MCP server was not registered".into());
            }
            _ => {}
        }
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
