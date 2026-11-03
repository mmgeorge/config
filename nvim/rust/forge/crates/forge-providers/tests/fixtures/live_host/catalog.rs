use super::collect::ProviderTestExt as _;
use anyhow::{Context, Result};
use forge_harness::provider::{McpStatus, ProviderInput};
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
        provider.mcp_list(super::catalog_request(workspace.path(), "mcp-catalog-test")),
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
        provider.model_list(super::prompt_request(&request)),
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
            .active_session_id_for(&request.harness_session_id)
            .await
            .unwrap()
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
    let test_session = super::provider_session(
        "codex",
        vec!["codex".into(), "app-server".into()],
        fixture.path(),
        fixture.path(),
    )?;
    let provider = test_session.provider_handle();
    let mut request = super::catalog_request(fixture.path(), "mcp-terminal-smoke");
    let mut settings = forge_harness::settings::SettingsStore::open(fixture.path())?;
    let configuration = provider.mcp_configuration(request.clone()).await?;
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
        provider
            .reconcile_settings(request.clone(), &desired)
            .await?;
        let mut prompt = super::prompt_request(&request);
        prompt.input = ProviderInput::from_text("Reply OK. Do not use tools or change files.");
        prompt.effort = "low".into();
        anyhow::ensure!(
            !provider.model_list(prompt.clone()).await?.is_empty(),
            "model catalog missing"
        );
        request.provider_session_id = test_session
            .session_prompt_and_collect(prompt)
            .await?
            .completion
            .native
            .provider_session_id;
        for _ in 0..2 {
            let servers = provider.mcp_list(request.clone()).await?;
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
                    .as_ref()
                    .map(forge_provider_api::SanitizedMessage::as_str)
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
            provider.background_terminals(request.clone()).await?;
            provider
                .reconcile_settings(request.clone(), &Default::default())
                .await?;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await;
    provider.shutdown().await?;
    outcome.context("live MCP smoke test timed out")??;
    Ok(())
}
