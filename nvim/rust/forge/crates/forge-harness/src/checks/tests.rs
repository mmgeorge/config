use super::*;
use anyhow::Result;
use std::sync::{Arc, Mutex};

fn definition(id: &str, command: &str) -> CheckDefinition {
    CheckDefinition {
        auto: true,
        id: id.into(),
        name: id.into(),
        category: CheckCategory::Test,
        purpose: CheckPurpose::Verify,
        enabled: true,
        area: "Project".into(),
        trigger: CheckTrigger::AnyChange,
        scope: CheckScope::All,
        directory: ".".into(),
        all: CheckOperation {
            input: false,
            args: None,
            inputs: None,
            command: command.into(),
        },
        files: None,
        cases: None,
        depends_on: Vec::new(),
    }
}

fn save(workspace: &std::path::Path, definitions: Vec<CheckDefinition>) {
    std::fs::write(workspace.join(".forge.json"),serde_json::to_vec(&serde_json::json!({"branch_prefix":"feature","checks":CheckConfig { version: 2, definitions }})).unwrap()).unwrap();
}

fn make_runner(workspace: &std::path::Path, data: &std::path::Path) -> Arc<CheckSupervisor> {
    let mut presentation = crate::buffer::session::SessionPresentation::new("checks-test".into());
    presentation.initialize(Vec::new()).unwrap();
    CheckSupervisor::open(
        super::CheckSupervisorContext {
            session_id: "checks-test".into(),
            data_root: data.into(),
            workspace: workspace.into(),
            root: data.into(),
        },
        Arc::new(Mutex::new(presentation)),
        crate::storage::objects::ObjectStore::open(data).unwrap(),
        Arc::new(forge_git::store::RepositoryStore::default()),
        Arc::new(crate::trace::TraceStore::open(data).unwrap()),
    )
    .unwrap()
}

/// A torn run record is skipped rather than failing the supervisor that session open builds.
#[test]
fn undecodable_check_record_does_not_block_session_open() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    std::fs::create_dir_all(data.path().join("torn-run"))?;
    std::fs::write(data.path().join("torn-run/record.json"), br#"{"version":"#)?;
    let runner = make_runner(workspace.path(), data.path());
    assert!(runner.history()?.is_empty());
    assert!(CheckSupervisor::saved_history(data.path())?.is_empty());
    assert!(data.path().join("torn-run/record.json").exists());
    Ok(())
}

#[tokio::test]
async fn revoked_callback_cannot_write_check_configuration() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("unit", "exit 0")]);
    let before = std::fs::read(workspace.path().join(".forge.json"))?;
    let runner = make_runner(workspace.path(), data.path());
    let catalog = runner.catalog(None)?;
    let (_root, mut task_store, controller) = crate::task::SessionTaskController::test_goal();
    let task = controller.current()?.unwrap();
    let mut context = crate::tools::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
    context.bind_test_task_controller(controller.clone());
    let authority = crate::tools::TaskAuthority::capture(&context).unwrap();
    controller.change(
        &mut task_store,
        Some(&task.revision),
        crate::task::TaskChange::Clear,
        2,
    )?;
    let result = runner
        .configure_with_authority(
            crate::timeline::TimelineOwner::session("checks-test"),
            crate::checks::ConfigureChecks {
                expected_revision: catalog["revision"].as_str().unwrap().into(),
                config: CheckConfig {
                    version: 2,
                    definitions: Vec::new(),
                },
                helpers: Vec::new(),
            },
            None,
            None,
            Some(&authority),
        )
        .await;
    assert!(result.is_err());
    assert_eq!(std::fs::read(workspace.path().join(".forge.json"))?, before);
    Ok(())
}
