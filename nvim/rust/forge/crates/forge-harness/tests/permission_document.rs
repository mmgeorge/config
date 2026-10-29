use forge_harness::backend::BackendLaunch;
use forge_harness::broker::{HarnessBroker, InitializeRequest};
use forge_harness::permissions::store::PermissionStore;
use forge_harness::session::ApprovalMode;
use forge_protocol::message::Request;
use serde_json::json;
use std::process::Command;

#[test]
fn repository_permission_document_matches_the_runtime_schema() {
    let manifest_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let permission_path = manifest_root.join("../../../../forge/permissions.json");
    PermissionStore::load(&permission_path, manifest_root)
        .unwrap_or_else(|error| panic!("validate {}: {error:#}", permission_path.display()));
}

fn broker_fixture() -> (tempfile::TempDir, HarnessBroker) {
    let temporary = tempfile::tempdir().unwrap();
    let workspace = temporary.path().join("workspace");
    std::fs::create_dir_all(&workspace).unwrap();
    std::fs::write(
        workspace.join(".forge.json"),
        json!({"checks":{}}).to_string(),
    )
    .unwrap();
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
        background: None,
        backend_launch_by_provider: Default::default(),
        config_dir: None,
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
        broker.snapshot().unwrap().session.approval_mode,
        ApprovalMode::Ask
    );

    let opened = broker
        .dispatch(Request {
            id: 1,
            method: "permissions.document.open".into(),
            params: json!({}),
        })
        .await;
    assert!(opened.response.error().is_none());

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
        .dispatch(Request {
            id: 2,
            method: "permissions.document.save".into(),
            params: json!({ "source": source }),
        })
        .await;
    assert!(saved.response.error().is_none());

    let invalid = broker
        .dispatch(Request {
            id: 3,
            method: "permissions.document.save".into(),
            params: json!({ "source": "not json" }),
        })
        .await;
    assert!(invalid.response.error().is_some());
    let persisted = std::fs::read_to_string(temporary.path().join("permissions.json")).unwrap();
    assert!(persisted.contains("git commit *"));
}

#[tokio::test]
async fn cycles_explicit_modes_without_changing_mode_during_plan_control() {
    let (_temporary, mut broker) = broker_fixture();
    for (command, expected) in [
        ("/write", ApprovalMode::Ask),
        ("/yolo", ApprovalMode::Allow),
        ("/ask", ApprovalMode::Ask),
    ] {
        let response = broker
            .dispatch(Request {
                id: 10,
                method: "session.prompt.submit".into(),
                params: json!({ "text": command }),
            })
            .await;
        assert!(response.response.error().is_none());
        assert_eq!(broker.snapshot().unwrap().session.approval_mode, expected);
    }

    broker
        .dispatch(Request {
            id: 11,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/write" }),
        })
        .await;
    for command in ["/plan", "  /plan \n", "/plan"] {
        let changed = broker
            .dispatch(Request {
                id: 12,
                method: "session.prompt.submit".into(),
                params: json!({ "text": command }),
            })
            .await;
        assert!(changed.response.error().is_none());
        let snapshot = broker.snapshot().unwrap();
        assert!(snapshot.session.active_task.is_none());
        assert_eq!(snapshot.session.approval_mode, ApprovalMode::Ask);
        assert!(snapshot.active_plan.is_none());
        assert!(snapshot.exchange.is_empty());
    }
    let planned = broker
        .dispatch(Request {
            id: 13,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/plan preserve the selected mode" }),
        })
        .await;
    assert!(planned.response.error().is_none());
    assert_eq!(
        broker.snapshot().unwrap().session.approval_mode,
        ApprovalMode::Ask
    );
    assert!(broker.snapshot().unwrap().session.active_task.is_some());
    let before = broker.snapshot().unwrap();
    let retained = broker
        .dispatch(Request {
            id: 15,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/plan" }),
        })
        .await;
    assert!(retained.response.error().is_none());
    let after = broker.snapshot().unwrap();
    let plan = after.active_plan.as_ref().unwrap();
    assert_eq!(plan.id, before.active_plan.as_ref().unwrap().id);
    assert_eq!(plan.state, forge_harness::plan::PlanState::AwaitingReview);
    assert_eq!(after.exchange.len(), before.exchange.len());
    let accepted = broker
        .dispatch(Request {
            id: 14,
            method: "session.plan.accept".into(),
            params: json!({
                "plan_id": plan.id,
                "digest": plan.review_digest.as_deref().unwrap(),
                "approval_mode": "ask"
            }),
        })
        .await;
    assert!(
        accepted.response.error().is_none(),
        "{:?}",
        accepted.response.error()
    );
    assert_eq!(
        broker.snapshot().unwrap().session.approval_mode,
        ApprovalMode::Ask
    );
    assert!(broker.snapshot().unwrap().session.active_task.is_some());
}
