use forge_harness::broker::{HarnessBroker, InitializeRequest};
use forge_harness::provider::ProviderLaunch;
use forge_harness::session::ApprovalMode;
use forge_protocol::message::Request;
use serde_json::json;

fn broker_fixture() -> (tempfile::TempDir, HarnessBroker) {
    let temporary = tempfile::tempdir().unwrap();
    let workspace = temporary.path().join("workspace");
    std::fs::create_dir_all(&workspace).unwrap();
    std::fs::write(
        workspace.join(".forge.json"),
        json!({"checks":{}}).to_string(),
    )
    .unwrap();
    forge_git::test_support::init_repository(&workspace);
    let broker = HarnessBroker::initialize(InitializeRequest {
        background: None,
        provider_launch_by_provider: Default::default(),
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
        provider: ProviderLaunch {
            kind: forge_providers::registry::nonselectable_id().expect("registry fixture"),
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
