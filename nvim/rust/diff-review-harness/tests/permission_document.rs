use diff_review_harness::backend::BackendLaunch;
use diff_review_harness::broker::{HarnessBroker, InitializeRequest};
use diff_review_harness::permissions::store::PermissionStore;
use diff_review_harness::protocol::BrokerRequest;
use diff_review_harness::session::ExecutionMode;
use serde_json::json;
use std::process::Command;

#[test]
fn repository_permission_document_matches_the_runtime_schema() {
    let manifest_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let permission_path = manifest_root.join("../../diff_review/permissions.json");
    PermissionStore::load(&permission_path, manifest_root)
        .unwrap_or_else(|error| panic!("validate {}: {error:#}", permission_path.display()));
}

fn broker_fixture() -> (tempfile::TempDir, HarnessBroker) {
    let temporary = tempfile::tempdir().unwrap();
    let workspace = temporary.path().join("workspace");
    std::fs::create_dir_all(&workspace).unwrap();
    assert!(
        Command::new("git")
            .arg("init")
            .arg("--quiet")
            .arg(&workspace)
            .status()
            .unwrap()
            .success()
    );
    let broker = HarnessBroker::initialize(InitializeRequest {
        data_root: temporary.path().join("data").to_string_lossy().into_owned(),
        permission_file: Some(
            temporary
                .path()
                .join("permissions.json")
                .to_string_lossy()
                .into_owned(),
        ),
        workspace: workspace.to_string_lossy().into_owned(),
        client_id: "permission-test".into(),
        backend: BackendLaunch {
            kind: "mock".into(),
            command: vec!["mock".into()],
        },
        model: "mock".into(),
        effort: "low".into(),
        session_id: None,
        new_session_name: None,
        goal_max_turns: 20,
        lease_conflict_action: None,
    })
    .unwrap();
    (temporary, broker)
}

#[tokio::test]
async fn opens_validates_and_atomically_replaces_the_permission_document() {
    let (temporary, mut broker) = broker_fixture();
    assert_eq!(
        broker.snapshot().unwrap().session.execution_mode,
        ExecutionMode::Read
    );

    let opened = broker
        .dispatch(BrokerRequest {
            id: 1,
            method: "permissions.open".into(),
            params: json!({}),
        })
        .await;
    assert!(opened.response.error.is_none());

    let source = json!({
        "$schema": "https://raw.githubusercontent.com/dyoshikawa/rulesync/main/schemas/permissions.schema.json",
        "permission": {
            "read": { "*": "allow" },
            "edit": { ".": "allow" },
            "bash": { "git *": "allow", "git commit *": "deny" },
            "webfetch": { "*": "allow" },
            "mcp": { "*": "ask" }
        }
    })
    .to_string();
    let saved = broker
        .dispatch(BrokerRequest {
            id: 2,
            method: "permissions.save".into(),
            params: json!({ "source": source }),
        })
        .await;
    assert!(saved.response.error.is_none());

    let invalid = broker
        .dispatch(BrokerRequest {
            id: 3,
            method: "permissions.save".into(),
            params: json!({ "source": "not json" }),
        })
        .await;
    assert!(invalid.response.error.is_some());
    let persisted = std::fs::read_to_string(temporary.path().join("permissions.json")).unwrap();
    assert!(persisted.contains("git commit *"));
}

#[tokio::test]
async fn cycles_explicit_modes_without_changing_mode_during_plan_control() {
    let (_temporary, mut broker) = broker_fixture();
    for (command, expected) in [
        ("/write", ExecutionMode::Write),
        ("/full", ExecutionMode::Full),
        ("/yolo", ExecutionMode::Yolo),
        ("/read", ExecutionMode::Read),
    ] {
        let response = broker
            .dispatch(BrokerRequest {
                id: 10,
                method: "prompt.submit".into(),
                params: json!({ "text": command }),
            })
            .await;
        assert!(response.response.error.is_none());
        assert_eq!(broker.snapshot().unwrap().session.execution_mode, expected);
    }

    broker
        .dispatch(BrokerRequest {
            id: 11,
            method: "prompt.submit".into(),
            params: json!({ "text": "/write" }),
        })
        .await;
    let planned = broker
        .dispatch(BrokerRequest {
            id: 12,
            method: "prompt.submit".into(),
            params: json!({ "text": "/plan preserve the selected mode" }),
        })
        .await;
    assert!(planned.response.error.is_none());
    assert_eq!(
        broker.snapshot().unwrap().session.execution_mode,
        ExecutionMode::Write
    );
    let accepted = broker
        .dispatch(BrokerRequest {
            id: 13,
            method: "plan.accept".into(),
            params: json!({}),
        })
        .await;
    assert!(accepted.response.error.is_none());
    assert_eq!(
        broker.snapshot().unwrap().session.execution_mode,
        ExecutionMode::Write
    );
}
