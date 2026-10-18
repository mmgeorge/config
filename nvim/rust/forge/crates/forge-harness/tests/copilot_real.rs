use forge_harness::backend::copilot::CopilotBackend;
use forge_harness::backend::{Backend, BackendCatalogRequest, BackendLaunch, TextGeneration};
use forge_harness::broker::{HarnessBroker, InitializeRequest};
use forge_harness::session::PermissionMode;
use forge_protocol::message::Request;
use serde_json::{Value, json};
use std::process::Command;
use std::time::Duration;

#[tokio::test]
#[ignore = "requires an authenticated Copilot CLI and performs a real model turn"]
async fn isolated_commit_generation_uses_existing_login_without_starting_shared_client() {
    let temporary = tempfile::tempdir().unwrap();
    let backend = CopilotBackend::new(Vec::new()).unwrap();
    let request = BackendCatalogRequest {
        execution_owner: None,
        harness_session_id: "copilot-isolated-commit".into(),
        workspace: temporary.path().to_string_lossy().into_owned(),
        access: Default::default(),
        execution_mode: PermissionMode::Read,
        backend_session_id: None,
    };
    let message = tokio::time::timeout(Duration::from_secs(120), backend.generate_text(
        request.clone(), TextGeneration::CommitMessage, "gpt-6-luna", Some("low"),
        "diff --git a/README.md b/README.md\n--- a/README.md\n+++ b/README.md\n@@ -1 +1 @@\n-# Instalattion\n+# Installation\n",
    )).await.expect("Isolated commit generation timed out").unwrap();
    assert!(!message.is_empty());
    assert_eq!(backend.client_start_count(), 0, "generation started the shared conversation client");
    assert!(backend.active_session_id_for(&request.harness_session_id).await.is_none());
    backend.shutdown().await.unwrap();
}

#[tokio::test]
#[ignore = "requires an authenticated pinned Copilot CLI and docs-mcp on PATH"]
async fn starts_shared_copilot_mcp_server_without_arguments() {
    let temporary = tempfile::tempdir().unwrap();
    let config = temporary.path().join("config");
    let resource = config.join("rulesync-global/.rulesync");
    let workspace = temporary.path().join("workspace");
    std::fs::create_dir_all(&resource).unwrap();
    std::fs::create_dir_all(&workspace).unwrap();
    std::fs::write(resource.join("mcp.json"), json!({
        "mcpServers": {
            "docs": {"command":"docs-mcp"},
            "broken": {"command":"forge-missing-mcp-startup-test"}
        }
    }).to_string()).unwrap();
    let data = temporary.path().join("data");
    forge_harness::settings::SettingsStore::open(&data).unwrap().configure_catalog(&config).unwrap();
    let trace = std::sync::Arc::new(forge_harness::trace::TraceStore::open(&data).unwrap());
    trace.configure(true).unwrap();
    let backend = CopilotBackend::new_with_permission_coordinator(
        Vec::new(), forge_harness::backend::approval::PermissionCoordinator::transient(&workspace).unwrap(),
        std::sync::Arc::clone(&trace), Some(data),
    ).unwrap();
    let request = BackendCatalogRequest {
        execution_owner: None, harness_session_id: "copilot-mcp-no-args-test".into(),
        workspace: workspace.to_string_lossy().into_owned(), access: Default::default(),
        execution_mode: PermissionMode::Read, backend_session_id: None,
    };
    let server = tokio::time::timeout(Duration::from_secs(40), backend.mcp_list(request.clone()))
        .await.expect("Copilot MCP startup timed out").unwrap();
    assert_eq!(server.len(), 2);
    assert_eq!(server.iter().find(|server| server.name == "docs").unwrap().status,
        forge_harness::backend::McpStatus::Connected);
    let failed = server.iter().find(|server| server.name == "broken").unwrap();
    assert_eq!(failed.status, forge_harness::backend::McpStatus::Unavailable);
    assert!(failed.status_detail.as_ref().is_some_and(|detail| !detail.is_empty()));
    let log = std::fs::read_to_string(trace.session_status(&request.harness_session_id).path).unwrap();
    assert!(!log.contains("copilot.mcp.start_failed"));
    let session_id = backend.active_session_id_for(&request.harness_session_id).await.unwrap();
    let server = backend.mcp_list(request.clone()).await.unwrap();
    assert_eq!(server.iter().find(|server| server.name == "docs").unwrap().status,
        forge_harness::backend::McpStatus::Connected);
    assert_eq!(backend.active_session_id_for(&request.harness_session_id).await.as_deref(),
        Some(session_id.as_str()));
    let terminals = tokio::time::timeout(Duration::from_secs(10), backend.background_terminals(request))
        .await.expect("Copilot terminal status timed out").unwrap();
    assert!(terminals.supported);
    assert!(terminals.terminal.is_empty());
    backend.shutdown().await.unwrap();
}

#[tokio::test]
#[ignore = "requires an authenticated Copilot CLI"]
async fn lists_native_copilot_skills_and_mcp_rows() {
    let temporary = tempfile::tempdir().unwrap();
    let backend = CopilotBackend::new(Vec::new()).unwrap();
    let request = BackendCatalogRequest {
        execution_owner: None,
        harness_session_id: "copilot-catalog".into(),
        workspace: temporary.path().to_string_lossy().into_owned(),
        access: Default::default(),
        execution_mode: PermissionMode::Read,
        backend_session_id: None,
    };
    let skill_list =
        tokio::time::timeout(Duration::from_secs(30), backend.skill_list(request.clone()))
            .await
            .expect("Copilot skill list timed out")
            .unwrap();
    assert!(skill_list.iter().all(|skill| skill.user_invocable));
    let mut new_session_request = request;
    new_session_request.harness_session_id = "copilot-new-catalog".into();
    let mcp_list = tokio::time::timeout(
        Duration::from_secs(30),
        backend.mcp_list(new_session_request),
    )
    .await
    .expect("Copilot MCP list timed out")
    .unwrap();
    assert!(mcp_list.iter().all(|server| !server.name.is_empty()));
    assert_eq!(backend.client_start_count(), 1);
}

#[tokio::test]
#[ignore = "requires an authenticated Copilot CLI and performs a real model turn"]
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
        backend_launch_by_provider: Default::default(),
            config_dir: None,
        data_root: temporary.path().join("data").to_string_lossy().into_owned(),
        permission_file: None,
        workspace: workspace.to_string_lossy().into_owned(),
        client_id: "real-copilot-test".into(),
        backend: BackendLaunch {
            kind: "copilot".into(),
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
            method: "backend.models".into(),
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
            method: "prompt.submit".into(),
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
