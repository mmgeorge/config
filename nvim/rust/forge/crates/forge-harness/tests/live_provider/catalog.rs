use anyhow::{Context, Result};
use forge_harness::backend::approval::PermissionCoordinator;
use forge_harness::backend::codex::CodexBackend;
use forge_harness::backend::copilot::CopilotBackend;
use forge_harness::backend::{Backend, BackendInput, McpStatus};
use forge_harness::trace::TraceStore;
use serde_json::json;
use std::sync::Arc;
use std::time::Duration;

#[tokio::test]
#[ignore = "requires installed authenticated Codex and configured MCP servers"]
async fn live_mcp_catalog() {
    let workspace = tempfile::tempdir().unwrap();
    let backend = CodexBackend::new(vec!["codex".into(), "app-server".into()]).unwrap();
    let result = tokio::time::timeout(
        Duration::from_secs(30),
        backend.mcp_list(super::catalog_request(workspace.path(), "mcp-catalog-test")),
    )
    .await;
    backend.shutdown().await.unwrap();
    result.expect("MCP inventory exceeded 30 seconds").unwrap();
}

#[tokio::test]
#[ignore = "requires the pinned authenticated Copilot CLI to list models without a model turn"]
async fn default_cli_resolution_starts_and_collects_without_a_session() {
    let workspace = tempfile::tempdir().unwrap();
    let backend = CopilotBackend::new(Vec::new()).unwrap();
    let request = super::catalog_request(workspace.path(), "copilot-startup");
    let result = tokio::time::timeout(
        Duration::from_secs(30),
        backend.model_list(super::prompt_request(&request)),
    )
    .await;
    backend.shutdown().await.unwrap();
    assert!(
        !result
            .expect("Copilot model list timed out")
            .unwrap()
            .is_empty()
    );
    assert_eq!(backend.client_start_count(), 1);
    assert!(
        backend
            .active_session_id_for(&request.harness_session_id)
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
    forge_harness::settings::SettingsStore::open(fixture.path())?
        .configure_catalog(fixture.path())?;
    let workspace = fixture.path().to_string_lossy().into_owned();
    let backend = CodexBackend::new_with_permission_coordinator(
        vec!["codex".into(), "app-server".into()],
        PermissionCoordinator::transient(&workspace)?,
        Arc::new(TraceStore::open(fixture.path())?),
        Some(fixture.path().into()),
    )?;
    let mut request = super::catalog_request(fixture.path(), "mcp-terminal-smoke");
    let mut settings = forge_harness::settings::SettingsStore::open(fixture.path())?;
    let configuration = backend.mcp_configuration(request.clone()).await?;
    let snapshot = settings.get(&request.workspace, &request.harness_session_id, "codex")?;
    let mut desired = snapshot.desired;
    let missing = configuration
        .iter()
        .find(|server| server.name == "missing")
        .context("missing MCP definition was not exposed")?;
    let missing_id = missing
        .resource_id
        .clone()
        .context("managed MCP definition has no resource identity")?;
    desired.mcp.insert(missing_id, true);
    settings.update(
        snapshot.revision,
        forge_harness::settings::SettingScope::Session,
        &request.workspace,
        &request.harness_session_id,
        &desired,
    )?;
    drop(settings);
    let outcome = tokio::time::timeout(Duration::from_secs(60), async {
        backend
            .reconcile_settings(request.clone(), &desired)
            .await?;
        let mut prompt = super::prompt_request(&request);
        prompt.input = BackendInput::from_text("Reply OK. Do not use tools or change files.");
        prompt.effort = "low".into();
        anyhow::ensure!(
            !backend.model_list(prompt.clone()).await?.is_empty(),
            "model catalog missing"
        );
        request.backend_session_id = backend
            .prompt_stream(prompt, None)
            .await?
            .backend_session_id;
        for _ in 0..2 {
            let servers = backend.mcp_list(request.clone()).await?;
            let unavailable = servers
                .iter()
                .find(|server| server.name == "missing")
                .context("missing MCP row disappeared")?;
            anyhow::ensure!(
                unavailable.status == McpStatus::Unavailable,
                "missing credential did not stay isolated"
            );
            anyhow::ensure!(
                unavailable
                    .status_detail
                    .as_deref()
                    .is_some_and(|reason| reason.contains("FORGE_LIVE_MCP_MISSING_608C")),
                "missing credential reason disappeared"
            );
            let server = servers
                .iter()
                .find(|server| server.name == "fixture")
                .context("fixture MCP absent")?;
            anyhow::ensure!(
                server.status == McpStatus::Connected,
                "fixture MCP status: {:?}",
                server.status
            );
            anyhow::ensure!(
                server.tools.iter().any(|tool| tool.name == "ready"),
                "fixture tool missing"
            );
            backend.background_terminals(request.clone()).await?;
            backend
                .reconcile_settings(request.clone(), &Default::default())
                .await?;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await;
    backend.shutdown().await?;
    outcome.context("live MCP smoke test timed out")??;
    Ok(())
}
