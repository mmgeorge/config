use diff_review_harness::backend::copilot::CopilotBackend;
use diff_review_harness::backend::{Backend, BackendCatalogRequest, BackendLaunch};
use diff_review_harness::broker::{HarnessBroker, InitializeRequest};
use diff_review_harness::protocol::BrokerRequest;
use diff_review_harness::session::ExecutionMode;
use serde_json::{Value, json};
use std::process::Command;
use std::time::Duration;

#[tokio::test]
#[ignore = "requires an authenticated Copilot CLI"]
async fn lists_native_copilot_skills_and_mcp_rows() {
    let temporary = tempfile::tempdir().unwrap();
    let backend = CopilotBackend::new(Vec::new()).unwrap();
    let request = BackendCatalogRequest {
        harness_session_id: "copilot-catalog".into(),
        workspace: temporary.path().to_string_lossy().into_owned(),
        execution_mode: ExecutionMode::Read,
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
        .dispatch(BrokerRequest {
            id: 1,
            method: "backend.models".into(),
            params: Value::Null,
        })
        .await;
    assert!(model_response.response.error.is_none());
    let model_list = model_response
        .response
        .result
        .as_ref()
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
        .dispatch(BrokerRequest {
            id: 2,
            method: "session.configure".into(),
            params: json!({ "model": selected_model, "effort": "low" }),
        })
        .await;
    assert!(configured.response.error.is_none());

    let response = tokio::time::timeout(
        Duration::from_secs(120),
        broker.dispatch(BrokerRequest {
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
        response.response.error.is_none(),
        "{:?}",
        response.response.error
    );
    let snapshot = broker.snapshot().unwrap();
    assert_eq!(snapshot.session.provider_label, "Copilot CLI");
    let persisted = serde_json::to_string(&snapshot.interaction).unwrap();
    assert!(persisted.contains("COPILOT_OK"), "{persisted}");
}
