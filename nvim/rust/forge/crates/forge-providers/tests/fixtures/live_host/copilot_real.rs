use forge_harness::broker::{HarnessBroker, InitializeRequest};
use forge_harness::provider::{ProviderCatalogRequest, ProviderLaunch, TextGeneration};
use forge_harness::session::ApprovalMode;
use forge_protocol::message::Request;
use serde_json::{Value, json};
use std::process::Command;
use std::time::Duration;

#[tokio::test]
#[ignore = "requires authenticated Copilot and performs a real model request"]
async fn isolated_commit_generation_uses_existing_login_without_creating_persistent_history() {
    let temporary = tempfile::tempdir().unwrap();
    let provider =
        super::build_provider("copilot", Vec::new(), temporary.path(), temporary.path()).unwrap();
    let request = ProviderCatalogRequest {
        execution_owner: None,
        harness_session_id: "copilot-isolated-commit".into(),
        workspace: temporary.path().to_string_lossy().into_owned(),
        access: Default::default(),
        approval_mode: ApprovalMode::Ask,
        provider_session_id: None,
    };
    let message = tokio::time::timeout(Duration::from_secs(120), provider.generate_text(
        request.clone(), TextGeneration::CommitMessage, "gpt-6-luna", Some("low"),
        "diff --git a/README.md b/README.md\n--- a/README.md\n+++ b/README.md\n@@ -1 +1 @@\n-# Instalattion\n+# Installation\n",
    )).await.expect("Isolated commit generation timed out").unwrap();
    assert!(!message.is_empty());
    assert!(
        provider
            .retained_session_id(&request.owner().unwrap())
            .await
            .is_none()
    );
    provider.shutdown().await.unwrap();
}

#[tokio::test]
#[ignore = "requires authenticated pinned Copilot and docs-mcp on PATH"]
async fn starts_shared_copilot_mcp_server_without_arguments() {
    let temporary = tempfile::tempdir().unwrap();
    let config = temporary.path().join("config");
    let resource = config.join("rulesync-global/.rulesync");
    let workspace = temporary.path().join("workspace");
    std::fs::create_dir_all(&resource).unwrap();
    std::fs::create_dir_all(&workspace).unwrap();
    std::fs::write(
        resource.join("mcp.json"),
        json!({
            "mcpServers": {
                "docs": {"command":"docs-mcp"},
                "broken": {"command":"forge-missing-mcp-startup-test"}
            }
        })
        .to_string(),
    )
    .unwrap();
    let data = temporary.path().join("data");
    forge_harness::settings::SettingsStore::open(&data)
        .unwrap()
        .configure_catalog(&config)
        .unwrap();
    let trace = std::sync::Arc::new(forge_harness::trace::TraceStore::open(&data).unwrap());
    trace.configure(true).unwrap();
    let test_session = super::provider_session("copilot", Vec::new(), &data, &workspace).unwrap();
    let provider = test_session.provider_handle();
    let request = ProviderCatalogRequest {
        execution_owner: None,
        harness_session_id: "copilot-mcp-no-args-test".into(),
        workspace: workspace.to_string_lossy().into_owned(),
        access: Default::default(),
        approval_mode: ApprovalMode::Ask,
        provider_session_id: None,
    };
    test_session
        .thread_open(super::prompt_request(&request))
        .await
        .unwrap();
    let server = tokio::time::timeout(
        Duration::from_secs(40),
        provider.mcp_list(forge_provider_api::McpQuery::Observed(
            provider.query_context(&request).await.unwrap(),
        )),
    )
    .await
    .expect("Copilot MCP startup timed out")
    .unwrap();
    assert_eq!(server.len(), 2);
    assert_eq!(
        server
            .iter()
            .find(|server| server.name == "docs")
            .unwrap()
            .status,
        forge_harness::provider::McpStatus::Connected
    );
    let failed = server
        .iter()
        .find(|server| server.name == "broken")
        .unwrap();
    assert_eq!(
        failed.status,
        forge_harness::provider::McpStatus::Unavailable
    );
    assert!(
        failed
            .status_detail
            .as_ref()
            .is_some_and(|detail| !detail.as_str().is_empty())
    );
    let log =
        std::fs::read_to_string(trace.session_status(&request.harness_session_id).path).unwrap();
    assert!(!log.contains("copilot.mcp.start_failed"));
    let session_id = provider
        .retained_session_id(&request.owner().unwrap())
        .await
        .unwrap();
    let server = provider
        .mcp_list(forge_provider_api::McpQuery::Observed(
            provider.query_context(&request).await.unwrap(),
        ))
        .await
        .unwrap();
    assert_eq!(
        server
            .iter()
            .find(|server| server.name == "docs")
            .unwrap()
            .status,
        forge_harness::provider::McpStatus::Connected
    );
    assert_eq!(
        provider
            .retained_session_id(&request.owner().unwrap())
            .await
            .as_ref()
            .map(|id| id.as_str()),
        Some(session_id.as_str())
    );
    let terminals = tokio::time::timeout(Duration::from_secs(10), provider.terminal_list(request))
        .await
        .expect("Copilot terminal status timed out")
        .unwrap();
    assert!(terminals.complete);
    assert!(terminals.terminal.is_empty());
    provider.shutdown().await.unwrap();
}

#[tokio::test]
#[ignore = "requires an installed and authenticated Copilot CLI"]
async fn lists_native_copilot_skills_and_mcp_rows() {
    let temporary = tempfile::tempdir().unwrap();
    let provider =
        super::build_provider("copilot", Vec::new(), temporary.path(), temporary.path()).unwrap();
    let request = ProviderCatalogRequest {
        execution_owner: None,
        harness_session_id: "copilot-catalog".into(),
        workspace: temporary.path().to_string_lossy().into_owned(),
        access: Default::default(),
        approval_mode: ApprovalMode::Ask,
        provider_session_id: None,
    };
    let skill_list = tokio::time::timeout(
        Duration::from_secs(30),
        provider.skill_list(provider.query_context(&request).await.unwrap()),
    )
    .await
    .expect("Copilot skill list timed out")
    .unwrap();
    assert!(skill_list.iter().all(|skill| skill.user_invocable));
    let mut new_session_request = request;
    new_session_request.harness_session_id = "copilot-new-catalog".into();
    let mcp_list = tokio::time::timeout(
        Duration::from_secs(30),
        provider.mcp_list(forge_provider_api::McpQuery::Configured(
            provider.query_context(&new_session_request).await.unwrap(),
        )),
    )
    .await
    .expect("Copilot MCP list timed out")
    .unwrap();
    assert!(mcp_list.iter().all(|server| !server.name.is_empty()));
}

#[tokio::test]
#[ignore = "requires authenticated Copilot and performs a real model request"]
async fn streams_one_native_copilot_sdk_turn_through_the_broker() {
    let temporary = tempfile::tempdir().unwrap();
    let workspace = temporary.path().join("workspace");
    std::fs::create_dir_all(&workspace).unwrap();
    let status = Command::new("git")
        .args(["init", "--quiet"])
        .current_dir(&workspace)
        .status()
        .unwrap();
    assert!(status.success());
    std::fs::write(workspace.join("README.md"), "# Copilot fixture\n").unwrap();

    let mut broker = HarnessBroker::initialize(InitializeRequest {
        background: None,
        provider_launch_by_provider: Default::default(),
        config_dir: None,
        data_root: temporary.path().join("data").to_string_lossy().into_owned(),
        permission_file: None,
        workspace: workspace.to_string_lossy().into_owned(),
        client_id: "real-copilot-test".into(),
        provider: ProviderLaunch {
            kind: forge_providers::registry::select("copilot").unwrap(),
            command: Vec::new(),
        },
        model: "default".into(),
        effort: "low".into(),
        session_id: None,
        new_session_name: None,
        goal_max_turns: 20,
        lease_conflict_action: None,
    })
    .unwrap();

    let model_response = broker
        .dispatch(Request {
            id: 1,
            method: "session.provider.model.list".into(),
            params: Value::Null,
        })
        .await;
    assert!(model_response.response.error().is_none());
    let model_list = model_response
        .response
        .result()
        .and_then(Value::as_array)
        .expect("Copilot model catalog");
    assert!(model_list.iter().all(|model| model.get("label").is_none()));
    assert!(model_list.iter().all(|model| {
        model.get("reasoning").is_some()
            && model.get("context_window").is_some()
            && model.get("vision").is_some()
    }));
    assert!(model_list.iter().any(|model| {
        model
            .get("context_window")
            .and_then(Value::as_array)
            .is_some_and(|context_window| !context_window.is_empty())
    }));
    let selected_model = model_list
        .iter()
        .find(|model| {
            model.get("id").and_then(Value::as_str).is_some_and(|id| {
                let normalized = id.to_ascii_lowercase();
                id != "auto"
                    && (normalized.contains("mini")
                        || normalized.contains("haiku")
                        || normalized.contains("flash"))
            })
        })
        .or_else(|| {
            model_list.iter().find(|model| {
                model.get("id").and_then(Value::as_str) != Some("auto")
                    && model
                        .get("reasoning")
                        .and_then(Value::as_array)
                        .is_some_and(|effort_list| effort_list.iter().any(|effort| effort == "low"))
            })
        })
        .or_else(|| {
            model_list
                .iter()
                .find(|model| model.get("id").and_then(Value::as_str) != Some("auto"))
        })
        .and_then(|model| model.get("id"))
        .and_then(Value::as_str)
        .unwrap_or("default")
        .to_owned();
    let configured = broker
        .dispatch(Request {
            id: 2,
            method: "session.configure".into(),
            params: json!({ "model": selected_model, "effort": "low" }),
        })
        .await;
    assert!(configured.response.error().is_none());

    let response = tokio::time::timeout(
        Duration::from_secs(120),
        broker.dispatch(Request {
            id: 3,
            method: "session.prompt.submit".into(),
            params: json!({
                "text": "Reply with exactly COPILOT_OK. Do not call tools."
            }),
        }),
    )
    .await
    .expect("real Copilot SDK turn timeout");
    assert!(
        response.response.error().is_none(),
        "{:?}",
        response.response.error()
    );
    let snapshot = broker.snapshot().unwrap();
    assert_eq!(snapshot.session.provider_label, "Copilot CLI");
    let persisted = serde_json::to_string(&snapshot.exchange).unwrap();
    assert!(persisted.contains("COPILOT_OK"), "{persisted}");
}
