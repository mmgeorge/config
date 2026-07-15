use diff_review_harness::backend::BackendLaunch;
use diff_review_harness::broker::{HarnessBroker, InitializeRequest};
use diff_review_harness::protocol::BrokerRequest;
use diff_review_harness::session::ExecutionMode;
use serde_json::{Value, json};
use std::process::Command;
use std::time::Duration;

#[tokio::test]
#[ignore = "requires an authenticated Codex CLI and performs a real model turn"]
async fn asks_for_feedback_then_creates_a_plan_without_native_collaboration_mode() {
    let temporary = tempfile::tempdir().unwrap();
    let workspace = temporary.path().join("workspace");
    std::fs::create_dir_all(&workspace).unwrap();
    let status = Command::new("git")
        .args(["init", "--quiet"])
        .current_dir(&workspace)
        .status()
        .unwrap();
    assert!(status.success());
    std::fs::write(workspace.join("README.md"), "# Fixture\n").unwrap();

    let mut broker = HarnessBroker::initialize(InitializeRequest {
        data_root: temporary.path().join("data").to_string_lossy().into_owned(),
        permission_file: None,
        workspace: workspace.to_string_lossy().into_owned(),
        client_id: "real-codex-test".into(),
        backend: BackendLaunch {
            kind: "codex".into(),
            command: vec!["codex".into(), "app-server".into()],
        },
        model: "default".into(),
        effort: "low".into(),
        session_id: None,
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
        .expect("Codex model catalog");
    let selected_model = model_list
        .iter()
        .find(|model| {
            model
                .get("id")
                .and_then(Value::as_str)
                .is_some_and(|id| id.contains("mini"))
        })
        .or_else(|| model_list.first())
        .and_then(|model| model.get("id"))
        .and_then(Value::as_str)
        .expect("at least one Codex model")
        .to_owned();
    let configured = broker
        .dispatch(BrokerRequest {
            id: 2,
            method: "session.configure".into(),
            params: json!({ "model": selected_model, "effort": "low", "fast_mode": true }),
        })
        .await;
    assert!(configured.response.error.is_none());

    let planned = tokio::time::timeout(
        Duration::from_secs(120),
        broker.dispatch(BrokerRequest {
            id: 3,
            method: "prompt.submit".into(),
            params: json!({
                "text": "/plan add a Testing section to README.md without changing any files. Before creating the plan, ask me whether the section should describe unit tests or integration tests. Do not choose for me."
            }),
        }),
    )
    .await
    .expect("real Codex planning timeout");
    assert!(
        planned.response.error.is_none(),
        "{:?}",
        planned.response.error
    );
    let paused_snapshot = broker.snapshot().unwrap();
    assert!(paused_snapshot.artifact.is_empty());
    let paused_plan = paused_snapshot.active_plan.expect("paused planning state");
    assert_eq!(
        paused_plan.state,
        diff_review_harness::plan::PlanState::AwaitingInput
    );
    let elicitation = paused_plan.elicitation.expect("pending elicitation");
    let question = elicitation
        .current_question()
        .expect("current planning question");
    let question_id = question.id.clone();
    let selected_option = question
        .options
        .iter()
        .find(|option| option.label.to_ascii_lowercase().contains("integration"))
        .or_else(|| question.options.first())
        .expect("Codex planning question should provide a selectable answer")
        .label
        .clone();

    let answered = broker
        .dispatch(BrokerRequest {
            id: 4,
            method: "question.answer".into(),
            params: json!({
                "question_id": question_id,
                "response": {
                    "kind": "selected",
                    "option": selected_option,
                    "feedback": null
                }
            }),
        })
        .await;
    assert!(
        answered.response.error.is_none(),
        "{:?}",
        answered.response.error
    );

    let resumed = tokio::time::timeout(
        Duration::from_secs(120),
        broker.dispatch(BrokerRequest {
            id: 5,
            method: "question.continue".into(),
            params: Value::Null,
        }),
    )
    .await
    .expect("real Codex planning continuation timeout");
    assert!(
        resumed.response.error.is_none(),
        "{:?}",
        resumed.response.error
    );
    let snapshot = broker.snapshot().unwrap();
    assert_eq!(snapshot.artifact.len(), 1);
    assert!(std::path::Path::new(&snapshot.artifact[0].working_path).exists());
    assert_eq!(snapshot.session.execution_mode, ExecutionMode::Read);

    let new_session = broker
        .dispatch(BrokerRequest {
            id: 6,
            method: "session.new".into(),
            params: Value::Null,
        })
        .await;
    assert!(new_session.response.error.is_none());
    assert_eq!(
        broker.snapshot().unwrap().session.execution_mode,
        ExecutionMode::Read
    );
    let write_mode = broker
        .dispatch(BrokerRequest {
            id: 7,
            method: "session.execution_mode".into(),
            params: json!({ "mode": "write" }),
        })
        .await;
    assert!(write_mode.response.error.is_none());
    let write_result = tokio::time::timeout(
        Duration::from_secs(120),
        broker.dispatch(BrokerRequest {
            id: 8,
            method: "prompt.submit".into(),
            params: json!({
                "text": "Create mode-write-proof.txt in the workspace with exactly this content: `Harness native Write mode verified`. Do not change any other file."
            }),
        }),
    )
    .await
    .expect("real Codex Write-mode timeout");
    assert!(
        write_result.response.error.is_none(),
        "{:?}",
        write_result.response.error
    );
    let write_proof = std::fs::read_to_string(workspace.join("mode-write-proof.txt"))
        .expect("Write mode should create the requested workspace file");
    assert_eq!(write_proof.trim_end(), "Harness native Write mode verified");
}
