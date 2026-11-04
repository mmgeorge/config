use anyhow::{Context, Result};
use forge_harness::provider::McpStatus;
use serde_json::json;
use std::time::Duration;

#[tokio::test]
#[ignore = "requires installed authenticated Codex and configured MCP servers"]
async fn live_mcp_catalog() {
    let workspace = tempfile::tempdir().unwrap();
    let provider = super::build_provider(
        "codex",
        vec!["codex".into(), "app-server".into()],
        workspace.path(),
        workspace.path(),
    )
    .unwrap();
    let result = tokio::time::timeout(
        Duration::from_secs(30),
        provider.mcp_list(forge_provider_api::McpQuery::Configured(
            forge_provider_api::QueryContext::Workspace(forge_provider_api::WorkspaceContext {
                workspace: workspace.path().to_string_lossy().into_owned(),
            }),
        )),
    )
    .await;
    provider.shutdown().await.unwrap();
    result.expect("MCP inventory exceeded 30 seconds").unwrap();
}

#[tokio::test]
#[ignore = "requires the pinned authenticated Copilot CLI to list models without a model turn"]
async fn default_cli_resolution_starts_and_collects_without_a_session() {
    let workspace = tempfile::tempdir().unwrap();
    let provider =
        super::build_provider("copilot", Vec::new(), workspace.path(), workspace.path()).unwrap();
    let request = super::catalog_request(workspace.path(), "copilot-startup");
    let result = tokio::time::timeout(
        Duration::from_secs(30),
        provider.model_list(provider.query_context(&request).await.unwrap()),
    )
    .await;
    provider.shutdown().await.unwrap();
    assert!(
        !result
            .expect("Copilot model list timed out")
            .unwrap()
            .is_empty()
    );
    assert!(
        provider
            .retained_session_id(&request.owner().unwrap())
            .await
            .is_none(),
        "model discovery created a conversation"
    );
}

#[tokio::test]
#[ignore = "requires authenticated Codex and Neovim and makes one model request"]
async fn live_terminal_polling_preserves_mcp_inventory() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let definitions = fixture.path().join("rulesync-global/.rulesync");
    std::fs::create_dir_all(&definitions)?;
    let script = fixture.path().join("mcp.lua");
    std::fs::write(
        &script,
        r#"
for line in io.lines() do
  local request = vim.json.decode(line)
  if request.id ~= nil then
    local result = vim.empty_dict()
    if request.method == 'initialize' then
      result = {protocolVersion=request.params.protocolVersion, capabilities={tools=vim.empty_dict()}, serverInfo={name='fixture',version='1'}}
    elseif request.method == 'tools/list' then
      result = {tools={{name='ready',description='Read fixture readiness',inputSchema={type='object',properties=vim.empty_dict()}}}}
    elseif request.method == 'tools/call' then
      result = {content={{type='text',text='ready'}}}
    end
    io.write(vim.json.encode({jsonrpc='2.0',id=request.id,result=result}) .. '\n')
    io.flush()
  end
end
"#,
    )?;
    std::fs::write(
        definitions.join("mcp.json"),
        json!({"mcpServers":{
            "fixture":{"command":"nvim","args":["--headless","-u","NONE","-i","NONE","-l",script]},
            "missing":{"command":"nvim","envVars":["FORGE_LIVE_MCP_MISSING_608C"]}
        }})
        .to_string(),
    )?;
    let data_root = fixture.path().join("data");
    let initialize = serde_json::from_value(json!({
        "config_dir": fixture.path(), "data_root": data_root, "workspace": fixture.path(),
        "client_id": "live-mcp-terminal", "model": "gpt-6-luna", "effort": "low",
        "provider": { "kind": "codex", "command": ["codex", "app-server"] },
    }))?;
    let mut service = super::workflows::WorkflowService::open(initialize).await?;
    let outcome = tokio::time::timeout(Duration::from_secs(90), async {
        let initial = service
            .request("session.provider.mcp.list", json!({}))
            .await?;
        let configured: Vec<forge_provider_api::catalog::McpDefinition> =
            serde_json::from_value(initial)?;
        let missing_id = configured
            .iter()
            .find(|server| server.name == "missing")
            .context("configured missing-credential row was not exposed")?
            .resource_id
            .clone()
            .context("managed MCP definition has no resource identity")?;
        let settings = service.request("settings.get", json!({})).await?;
        let mut desired = settings["desired"].clone();
        desired["mcp"][missing_id.as_str()] = json!(true);
        let operation = service.request("settings.update", json!({
            "scope": "session", "expected_revision": settings["revision"], "desired": desired,
        })).await?;
        loop {
            let applied = service
                .request(
                    "session.command.operation.get",
                    json!({ "operation_id": operation["id"] }),
                )
                .await?;
            anyhow::ensure!(
                applied["state"] != "failed",
                "MCP settings application failed: {applied}"
            );
            if applied["state"] == "completed" {
                break;
            }
            tokio::time::sleep(Duration::from_millis(25)).await;
        }
        service
            .request(
                "session.prompt.submit",
                json!({ "text": "Reply OK. Do not use tools or change files." }),
            )
            .await?;
        service.wait_idle().await?;
        for _ in 0..2 {
            let servers = loop {
                let rows = service
                    .request("session.provider.mcp.list", json!({}))
                    .await?;
                let servers: Vec<forge_provider_api::catalog::McpDefinition> =
                    serde_json::from_value(rows)?;
                if servers.iter().any(|server| {
                    server.name == "fixture"
                        && server.status == McpStatus::Connected
                        && server.tools.iter().any(|tool| tool.name == "ready")
                }) {
                    break servers;
                }
                tokio::time::sleep(Duration::from_millis(25)).await;
            };
            let unavailable = servers
                .iter()
                .find(|server| server.name == "missing")
                .context(
                    "missing credential row disappeared during native readiness observation",
                )?;
            anyhow::ensure!(
                unavailable.status == McpStatus::Unavailable,
                "missing credential did not stay isolated"
            );
            anyhow::ensure!(
                unavailable
                    .status_detail
                    .as_ref()
                    .map(forge_provider_api::SanitizedMessage::as_str)
                    .is_some_and(|reason| reason.contains("FORGE_LIVE_MCP_MISSING_608C")),
                "missing credential explanation disappeared"
            );
            let before = service.request("session.state.get", json!({})).await?;
            service.terminal_snapshot().await?;
            let after = service.request("session.state.get", json!({})).await?;
            anyhow::ensure!(
                before["session"]["provider_session_id"] == after["session"]["provider_session_id"],
                "terminal observation replaced native history"
            );
        }
        Ok::<_, anyhow::Error>(())
    })
    .await;
    service.shutdown().await?;
    outcome.context("live MCP smoke test timed out")??;
    Ok(())
}
