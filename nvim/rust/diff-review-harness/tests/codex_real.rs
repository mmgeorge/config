use diff_review_harness::backend::{BackendLaunch, TrustPolicy};
use diff_review_harness::broker::{HarnessBroker, InitializeRequest};
use diff_review_harness::protocol::BrokerRequest;
use serde_json::{Value, json};
use std::process::Command;
use std::time::Duration;

#[tokio::test]
#[ignore = "requires an authenticated Codex CLI and performs a real model turn"]
async fn creates_a_reviewable_plan_without_native_collaboration_mode() {
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
        workspace: workspace.to_string_lossy().into_owned(),
        client_id: "real-codex-test".into(),
        backend: BackendLaunch {
            kind: "codex".into(),
            command: vec!["codex".into(), "app-server".into()],
        },
        model: "default".into(),
        effort: "low".into(),
        trust_profile: "workspace".into(),
        trust_policy: TrustPolicy::default(),
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
                "text": "/plan add a Testing section to README.md without changing any files"
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
    let snapshot = broker.snapshot().unwrap();
    assert_eq!(snapshot.artifact.len(), 1);
    assert!(std::path::Path::new(&snapshot.artifact[0].working_path).exists());
    assert_eq!(
        snapshot.session.write_mode,
        diff_review_harness::session::WriteMode::Read
    );
}
