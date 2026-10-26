use super::*;
use anyhow::Result;
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

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
            package_manager: None,
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

#[test]
fn check_order_prioritizes_categories_and_preserves_ties() -> Result<()> {
    let definitions = [
        ("build", CheckCategory::Build),
        ("test-first", CheckCategory::Test),
        ("check", CheckCategory::Check),
        ("lint", CheckCategory::Lint),
        ("format", CheckCategory::Format),
        ("test-second", CheckCategory::Test),
    ]
    .into_iter()
    .map(|(id, category)| {
        let mut check = definition(id, "exit 0");
        check.category = category;
        check
    })
    .collect();
    let config = CheckConfig {
        version: 2,
        definitions,
    };
    assert_eq!(config.order()?, vec![4, 2, 3, 1, 5, 0]);
    assert!(
        CheckConfig {
            version: 2,
            definitions: Vec::new()
        }
        .order()?
        .is_empty()
    );
    Ok(())
}

#[test]
fn check_order_runs_build_consumers_after_build_without_delaying_unit_tests() -> Result<()> {
    let mut integration = definition("integration", "exit 0");
    integration.depends_on = vec!["build".into()];
    let mut build = definition("build", "exit 0");
    build.category = CheckCategory::Build;
    let config = CheckConfig {
        version: 2,
        definitions: vec![integration, build, definition("unit", "exit 0")],
    };
    assert_eq!(config.order()?, vec![2, 1, 0]);
    Ok(())
}

#[test]
fn check_order_prepares_source_before_read_only_formatting() -> Result<()> {
    let mut verification = definition("format-check", "exit 0");
    verification.category = CheckCategory::Format;
    let mut preparation = definition("generate", "exit 0");
    preparation.purpose = CheckPurpose::Prepare;
    let config = CheckConfig {
        version: 2,
        definitions: vec![verification, preparation],
    };
    assert_eq!(config.order()?, vec![1, 0]);
    Ok(())
}

#[test]
fn check_order_promotes_transitive_preparation_prerequisites_only_when_selected() -> Result<()> {
    let mut build = definition("generator-build", "exit 0");
    build.category = CheckCategory::Build;
    build.depends_on = vec!["generator-check".into()];
    let mut preparation = definition("generate", "exit 0");
    preparation.purpose = CheckPurpose::Prepare;
    preparation.depends_on = vec!["generator-build".into()];
    let config = CheckConfig {
        version: 2,
        definitions: vec![
            definition("unit", "exit 0"),
            build,
            preparation,
            definition("generator-check", "exit 0"),
        ],
    };
    assert_eq!(config.order()?, vec![3, 1, 2, 0]);
    assert_eq!(
        config.order_selected(&["unit".into(), "generator-build".into()].into())?,
        vec![0, 3, 1, 2],
    );
    Ok(())
}

#[test]
fn check_order_rejects_missing_and_cyclic_prerequisites() {
    let mut check = definition("test", "exit 0");
    check.depends_on = vec!["missing".into()];
    let mut config = CheckConfig {
        version: 2,
        definitions: vec![check],
    };
    assert!(config.order().is_err());
    config.definitions[0].purpose = CheckPurpose::Prepare;
    assert!(config.order().is_err());
    config.definitions[0].depends_on = vec!["test".into()];
    assert!(config.order().is_err());
    let mut build = definition("build", "exit 0");
    build.category = CheckCategory::Build;
    build.depends_on = vec!["test".into()];
    config.definitions[0].depends_on = vec!["build".into()];
    config.definitions.push(build);
    assert!(config.order().is_err());
}

#[test]
fn relative_check_paths_validate_structure_without_a_byte_ceiling() -> Result<()> {
    let path = format!("{}helper.nu", "directory/".repeat(512));
    assert_eq!(
        super::config::relative_path(&path)?,
        std::path::PathBuf::from(&path)
    );
    for invalid in ["../outside", "/absolute", "invalid\0path"] {
        assert!(super::config::relative_path(invalid).is_err());
    }
    Ok(())
}

#[test]
fn large_check_configuration_and_helpers_preserve_complete_content() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let definitions = (0..129)
        .map(|index| definition(&format!("check-{index}"), "print ok"))
        .collect::<Vec<_>>();
    let document = serde_json::json!({
        "checks":CheckConfig { version: 2, definitions },
        "unrelated":"λ".repeat(1024 * 1024)
    });
    std::fs::write(
        workspace.path().join(".forge.json"),
        serde_json::to_vec(&document)?,
    )?;
    let helpers = workspace.path().join(".forge/checks");
    std::fs::create_dir_all(&helpers)?;
    let source = "# retained helper\n".repeat(32_769);
    assert!(source.len() > 256 * 1024);
    for index in 0..129 {
        std::fs::write(
            helpers.join(format!("helper-{index}.nu")),
            if index == 0 {
                source.as_str()
            } else {
                "print ok"
            },
        )?;
    }
    let snapshot = ConfigSnapshot::load(workspace.path())?;
    assert_eq!(snapshot.config.definitions.len(), 129);
    assert_eq!(snapshot.helper_digest.len(), 129);
    assert_eq!(
        snapshot.helper_digest[".forge/checks/helper-0.nu"],
        crate::plan::digest(source.as_bytes())
    );
    assert_eq!(super::config::read_document(workspace.path())?, document);
    Ok(())
}

fn make_runner(workspace: &std::path::Path, data: &std::path::Path) -> Arc<CheckSupervisor> {
    let mut presentation = crate::buffer::session::SessionPresentation::new("checks-test".into());
    presentation.initialize(Vec::new()).unwrap();
    CheckSupervisor::open(
        "checks-test".into(),
        data.into(),
        workspace.into(),
        data.into(),
        Arc::new(Mutex::new(presentation)),
        crate::storage::objects::ObjectStore::open(data).unwrap(),
        Arc::new(forge_git::store::RepositoryStore::default()),
    )
    .unwrap()
}

#[test]
fn reopening_history_excludes_obsolete_check_configuration_without_rewriting_it() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("test", "echo test")]);
    let runner = make_runner(workspace.path(), data.path());
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest::default(),
        None,
        crate::session::CheckShell::System,
        "User requested".into(),
    )?;
    let current_path = data.path().join(&run.id).join("record.json");
    super::supervisor::write_record(&current_path, &run)?;
    let mut obsolete = crate::codec::to_value(&run)?;
    obsolete["id"] = serde_json::json!("obsolete");
    obsolete["context"]["snapshot"]["config"] =
        serde_json::json!({"version": 2, "definitions": []});
    let obsolete = crate::codec::to_vec(&serde_json::json!({"version": 3, "run": obsolete}))?;
    let obsolete_path = data.path().join("obsolete").join("record.json");
    std::fs::create_dir_all(obsolete_path.parent().unwrap())?;
    std::fs::write(&obsolete_path, &obsolete)?;
    drop(runner);

    let reopened = make_runner(workspace.path(), data.path());
    let history = reopened.history()?;
    assert_eq!(history.len(), 1);
    assert_eq!(history[0].id, run.id);
    assert_eq!(std::fs::read(obsolete_path)?, obsolete);
    assert_eq!(
        history[0]
            .context
            .as_ref()
            .unwrap()
            .snapshot
            .config
            .definitions
            .len(),
        1
    );
    let mut invalid: serde_json::Value = crate::codec::from_slice(&std::fs::read(&current_path)?)?;
    invalid["run"]["context"]["snapshot"]["config"] =
        serde_json::json!({"version": 2, "definitions": []});
    std::fs::write(&current_path, crate::codec::to_vec(&invalid)?)?;
    let error = CheckSupervisor::saved_history(data.path()).unwrap_err();
    let message = format!("{error:#}");
    assert!(message.contains(current_path.to_str().unwrap()));
    assert!(message.contains("expected struct Check"));
    Ok(())
}

#[test]
fn progress_counts_selected_checks_instead_of_the_configured_catalog() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(
        workspace.path(),
        (0..33)
            .map(|index| definition(&format!("check-{index}"), "print done"))
            .collect(),
    );
    let runner = make_runner(workspace.path(), data.path());
    let mut run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            ids: (0..4).map(|index| format!("check-{index}")).collect(),
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "User requested".into(),
    )?;
    assert_eq!(run.commands.len(), 33);
    let activity = CheckActivity::from_run(&run).unwrap();
    assert_eq!((activity.completed_count, activity.total_count), (0, 4));
    let states = [
        CheckState::Passed,
        CheckState::Failed,
        CheckState::Running,
        CheckState::Pending,
    ];
    for (command, state) in run
        .commands
        .iter_mut()
        .filter(|command| command.selected)
        .zip(states)
    {
        command.state = state;
    }
    let activity = CheckActivity::from_run(&run).unwrap();
    assert_eq!((activity.completed_count, activity.total_count), (2, 4));
    let running = run
        .commands
        .iter_mut()
        .find(|command| command.state == CheckState::Running)
        .unwrap();
    let context = running.context.as_mut().unwrap();
    let current = CheckCase {
        file: "src/lib.rs".into(),
        name: "tests::current".into(),
        target: Some("lib".into()),
    };
    let queued = CheckCase {
        name: "tests::queued".into(),
        ..current.clone()
    };
    context.selection = CheckSelection::Cases {
        cases: vec![current.clone(), queued],
    };
    context.invocation = Some(super::invocation::CheckInvocation {
        inputs: BTreeMap::new(),
        program: "cargo".into(),
        args: Vec::new(),
        directory: ".".into(),
        package: None,
        files: Vec::new(),
        cases: vec![current],
        discovery: None,
    });
    running.preview = "Compiling test executable".into();
    let label = running.heading();
    assert!(label.contains("tests::current"));
    assert!(!label.contains("tests::queued"));
    let activity = CheckActivity::from_run(&run).unwrap();
    assert!(activity.command_list[0].title.contains("tests::current"));
    assert!(!activity.command_list[0].title.contains("tests::queued"));
    assert_eq!(
        activity.command_list[0].output_line.as_deref(),
        Some("Compiling test executable")
    );
    let running = run
        .commands
        .iter_mut()
        .find(|command| command.state == CheckState::Running)
        .unwrap();
    let context = running.context.as_mut().unwrap();
    context.invocation = None;
    context.selection = CheckSelection::Files {
        files: vec!["nvim/tests/forge/harness_health.lua".into()],
    };
    assert!(
        running
            .heading()
            .contains("nvim/tests/forge/harness_health.lua")
    );
    let activity = CheckActivity::from_run(&run).unwrap();
    assert!(
        activity.command_list[0]
            .title
            .contains("nvim/tests/forge/harness_health.lua")
    );
    let mut package_command = run
        .commands
        .iter()
        .find(|command| command.state == CheckState::Passed)
        .unwrap()
        .clone();
    package_command.id = "expanded-package".into();
    package_command.state = CheckState::Pending;
    run.commands.push(package_command);
    let activity = CheckActivity::from_run(&run).unwrap();
    assert_eq!((activity.completed_count, activity.total_count), (1, 4));
    run.commands.last_mut().unwrap().state = CheckState::Passed;
    run.commands
        .iter_mut()
        .find(|command| command.selected && command.state == CheckState::Pending)
        .unwrap()
        .state = CheckState::Skipped;
    let restored: CheckRun = serde_json::from_slice(&serde_json::to_vec(&run)?)?;
    let activity = CheckActivity::from_run(&restored).unwrap();
    assert_eq!((activity.completed_count, activity.total_count), (3, 4));
    Ok(())
}

fn direct_operation(
    command: &str,
    args: &[&str],
    manager: Option<PackageManager>,
) -> CheckOperation {
    CheckOperation {
        input: false,
        command: command.into(),
        args: Some(args.iter().map(|argument| (*argument).into()).collect()),
        package_manager: manager,
    }
}

#[tokio::test]
async fn completion_wait_ignores_progress_and_observes_terminal_states() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("wait", "print done")]);
    let runner = make_runner(workspace.path(), data.path());
    for state in [
        CheckState::Passed,
        CheckState::Failed,
        CheckState::Interrupted,
        CheckState::ConfigurationError,
    ] {
        let run = runner.prepare_request(
            crate::timeline::TimelineOwner::session("checks-test"),
            RunRequest {
                all: true,
                ..Default::default()
            },
            None,
            crate::session::CheckShell::Nushell,
            "wait test".into(),
        )?;
        runner.publish(&run, None).await?;
        let waiting = runner.status(&run.id, 1000, true);
        tokio::pin!(waiting);
        assert!(futures_util::poll!(waiting.as_mut()).is_pending());
        runner.publish(&run, None).await?;
        assert!(
            futures_util::poll!(waiting.as_mut()).is_pending(),
            "progress completed a terminal wait"
        );
        let progress = runner.status(&run.id, 1000, false);
        tokio::pin!(progress);
        assert!(futures_util::poll!(progress.as_mut()).is_pending());
        runner.publish(&run, None).await?;
        assert_eq!(progress.await?.state, CheckState::Running);
        let mut completed = run.clone();
        completed.state = state;
        runner.publish(&completed, None).await?;
        assert_eq!(waiting.await?.state, state);
        assert_eq!(runner.status(&run.id, 1000, true).await?.state, state);
    }
    assert!(runner.status("missing", 1000, true).await.is_err());
    Ok(())
}

#[tokio::test]
async fn completion_wait_returns_running_when_deadline_expires() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("wait", "print done")]);
    let runner = make_runner(workspace.path(), data.path());
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "wait test".into(),
    )?;
    runner.publish(&run, None).await?;
    assert_eq!(
        runner.status(&run.id, 0, true).await?.state,
        CheckState::Running
    );
    assert_eq!(
        runner.status(&run.id, 1, true).await?.state,
        CheckState::Running
    );
    Ok(())
}

async fn begin_recorded_task(
    workspace: &std::path::Path,
    data: &std::path::Path,
    runner: &CheckSupervisor,
) -> Result<()> {
    assert!(
        std::process::Command::new("git")
            .args(["init", "--quiet"])
            .current_dir(workspace)
            .status()?
            .success()
    );
    let checkpoint = crate::checkpoint::GitCheckpoint::new(workspace)
        .capture(
            &crate::storage::objects::ObjectStore::open(data)?,
            &Arc::new(forge_git::store::RepositoryStore::default()),
            "recorded-task",
            1,
        )
        .await?;
    runner.begin_changes("recorded-task".into(), &checkpoint)
}

#[tokio::test]
async fn automatic_package_coverage_reuses_only_the_package_already_checked() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    node_workspace(workspace.path(), false)?;
    for name in ["0", "1"] {
        std::fs::write(
            workspace
                .path()
                .join(format!("packages/{name}/src/code.ts")),
            "before",
        )?;
    }
    let mut check = definition("package", "print all");
    check.trigger = CheckTrigger::FilesChanged {
        include: vec!["**/*.ts".into()],
        exclude: Vec::new(),
    };
    check.scope = CheckScope::ChangedFiles;
    check.files = Some(direct_operation(
        "nu",
        &["--no-config-file", "-c", "print '{package.name}'"],
        Some(PackageManager::Npm),
    ));
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    begin_recorded_task(workspace.path(), data.path(), &runner).await?;
    for name in ["0", "1"] {
        std::fs::write(
            workspace
                .path()
                .join(format!("packages/{name}/src/code.ts")),
            "after",
        )?;
    }
    runner.refresh_changes().await?;
    let explicit = RunRequest {
        ids: vec!["package".into()],
        selections: BTreeMap::from([(
            "package".into(),
            CheckSelection::Files {
                files: vec!["packages/0/src/code.ts".into()],
            },
        )]),
        ..Default::default()
    };
    let first = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        explicit,
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Explicit".into(),
    )?;
    let first = runner.run(first, None, None).await?;
    assert_eq!(first.state, CheckState::Passed);
    assert!(first.commands[0].evidence.is_some());
    let mut next = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest::default(),
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Automatic".into(),
    )?;
    next.context.as_mut().unwrap().automatic = true;
    let next = runner.run(next, None, None).await?;
    assert_eq!(next.commands.len(), 2);
    assert!(next.commands[0].reused_from.is_some());
    assert!(next.commands[1].reused_from.is_none());
    std::fs::write(
        workspace.path().join("packages/0/src/code.ts"),
        "changed again",
    )?;
    runner.refresh_changes().await?;
    let mut changed = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest::default(),
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Automatic".into(),
    )?;
    changed.context.as_mut().unwrap().automatic = true;
    let changed = runner.run(changed, None, None).await?;
    assert!(changed.commands[0].reused_from.is_none());
    assert!(changed.commands[1].reused_from.is_some());
    Ok(())
}

#[tokio::test]
async fn automatic_failure_reuses_evidence_and_stops_unchanged_repairs() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    std::fs::write(workspace.path().join("source.txt"), "before")?;
    save(workspace.path(), vec![definition("failure", "exit 7")]);
    let runner = make_runner(workspace.path(), data.path());
    begin_recorded_task(workspace.path(), data.path(), &runner).await?;
    std::fs::write(workspace.path().join("source.txt"), "after")?;
    runner.refresh_changes().await?;
    let first = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest::default(),
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Explicit".into(),
    )?;
    let first = runner.run(first, None, None).await?;
    let mut automatic = super::automatic::AutomaticChecks::open(data.path(), "recorded-task")?;
    assert!(automatic.observe(&first)?.is_some());
    let mut pending = super::automatic::AutomaticChecks::open(data.path(), "recorded-task")?;
    assert!(
        pending.observe(&first)?.is_some(),
        "an unadmitted repair must survive recovery"
    );
    automatic.repair_exchange = Some("repair".into());
    automatic.save()?;
    let mut next = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest::default(),
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Automatic".into(),
    )?;
    next.context.as_mut().unwrap().automatic = true;
    let next = runner.run(next, None, None).await?;
    assert!(next.commands[0].reused_from.is_some());
    assert!(automatic.observe(&next)?.is_none());
    assert!(automatic.blocked.is_some());
    let reopened = super::automatic::AutomaticChecks::open(data.path(), "recorded-task")?;
    assert!(reopened.pending && reopened.blocked.is_some());
    Ok(())
}

#[test]
fn explicit_plan_checks_include_nonautomatic_checks_and_prerequisites() -> Result<()> {
    let mut optional = definition("optional", "print optional");
    optional.auto = false;
    optional.depends_on = vec!["prerequisite".into()];
    let mut prerequisite = definition("prerequisite", "print prerequisite");
    prerequisite.auto = false;
    let config = CheckConfig {
        version: 2,
        definitions: vec![prerequisite, optional],
    };
    let automatic = super::applicability::resolve(
        &RunRequest::default(),
        &config,
        Some(&["source.rs".into()]),
        true,
    )?;
    assert!(automatic.iter().all(|selection| !selection.selected));
    let explicit = super::applicability::resolve(
        &RunRequest {
            include: vec!["optional".into()],
            ..Default::default()
        },
        &config,
        Some(&[]),
        true,
    )?;
    assert!(explicit.iter().all(|selection| selection.selected));
    assert!(config.validate_selection(&["missing".into()]).is_err());
    Ok(())
}

#[tokio::test]
async fn plan_check_choices_disable_automatic_dependents_and_restore_prerequisites() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let prerequisite = definition("prerequisite", "print prerequisite");
    let mut optional = definition("optional", "print optional");
    optional.auto = false;
    optional.depends_on = vec!["prerequisite".into()];
    let mut automatic = definition("automatic", "print automatic");
    automatic.depends_on = vec!["prerequisite".into()];
    save(workspace.path(), vec![prerequisite, optional, automatic]);
    let mut design = crate::plan::DeclarationDesign::open(workspace.path())?;
    design.check_configuration = Some(super::ConfigSnapshot::load(workspace.path())?);
    design
        .proposed
        .insert("source.rs".into(), "fn source();".into());
    let config = design.check_configuration.as_ref().unwrap().config.clone();
    design
        .document
        .set_checks(&config, &BTreeMap::from([("prerequisite".into(), false)]))?;
    let preview = super::preview_plan(workspace.path(), &design).await?;
    assert!(preview.iter().all(|check| !check.selected));
    assert_eq!(design.document.disabled_checks.len(), 3);
    design
        .document
        .set_checks(&config, &BTreeMap::from([("optional".into(), true)]))?;
    let preview = super::preview_plan(workspace.path(), &design).await?;
    assert!(
        preview
            .iter()
            .find(|check| check.id == "optional")
            .unwrap()
            .selected
    );
    assert!(
        preview
            .iter()
            .find(|check| check.id == "prerequisite")
            .unwrap()
            .selected
    );
    assert!(
        !preview
            .iter()
            .find(|check| check.id == "automatic")
            .unwrap()
            .selected
    );
    assert_eq!(
        super::ConfigSnapshot::load(workspace.path())?.config,
        config
    );
    let previous = design.document.clone();
    assert!(
        design
            .document
            .set_checks(&config, &BTreeMap::from([("missing".into(), true)]))
            .is_err()
    );
    assert_eq!(design.document, previous);
    Ok(())
}

#[tokio::test]
async fn preparation_changes_inputs_and_allows_dependent_verification() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    std::fs::write(workspace.path().join("source.txt"), "before")?;
    let mut preparation = definition("prepare", "'after' | save --force source.txt");
    preparation.purpose = CheckPurpose::Prepare;
    let mut verification = definition("verify", "if (open --raw source.txt) != 'after' { exit 1 }");
    verification.depends_on = vec!["prepare".into()];
    save(workspace.path(), vec![preparation, verification]);
    let runner = make_runner(workspace.path(), data.path());
    begin_recorded_task(workspace.path(), data.path(), &runner).await?;
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            all: true,
            ..Default::default()
        },
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Explicit".into(),
    )?;
    let run = runner.run(run, None, None).await?;
    assert_eq!(run.state, CheckState::Passed, "{:?}", run.commands);
    assert!(
        run.commands
            .iter()
            .all(|command| command.evidence.is_some())
    );
    assert_eq!(run.commands[0].exit_code, Some(0));
    assert!(run.commands[1].started_at_ms.is_some());
    Ok(())
}

#[tokio::test]
async fn check_order_matches_preview_and_execution_without_success_dependencies() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    std::fs::write(workspace.path().join("source.txt"), "before")?;
    let mut build = definition("build", "if (open --raw source.txt) != 'after' { exit 1 }");
    build.category = CheckCategory::Build;
    let mut preparation = definition("prepare", "'after' | save --force source.txt");
    preparation.purpose = CheckPurpose::Prepare;
    let mut format = definition("format", "if (open --raw source.txt) != 'after' { exit 1 }");
    format.category = CheckCategory::Format;
    let mut integration = definition("integration", "exit 0");
    integration.depends_on = vec!["build".into()];
    save(
        workspace.path(),
        vec![
            integration,
            build,
            definition("unit", "exit 1"),
            format,
            preparation,
        ],
    );
    let config = ConfigSnapshot::load(workspace.path())?.config;
    let preview = super::preview::resolve(
        workspace.path(),
        &config,
        &["source.txt".into()],
        None,
        None,
        false,
        &[],
    )
    .await?;
    assert_eq!(
        preview
            .iter()
            .map(|check| check.id.as_str())
            .collect::<Vec<_>>(),
        vec!["prepare", "format", "unit", "build", "integration"],
    );
    let runner = make_runner(workspace.path(), data.path());
    begin_recorded_task(workspace.path(), data.path(), &runner).await?;
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            all: true,
            ..Default::default()
        },
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Explicit".into(),
    )?;
    assert_eq!(
        run.commands
            .iter()
            .map(|command| { command.context.as_ref().unwrap().definition_id.as_str() })
            .collect::<Vec<_>>(),
        vec!["prepare", "format", "unit", "build", "integration"],
    );
    let run = runner.run(run, None, None).await?;
    assert_eq!(run.state, CheckState::Failed, "{:?}", run.commands);
    assert_eq!(
        run.commands
            .iter()
            .map(|command| command.state)
            .collect::<Vec<_>>(),
        vec![
            CheckState::Passed,
            CheckState::Passed,
            CheckState::Failed,
            CheckState::Passed,
            CheckState::Passed
        ],
    );
    assert!(
        run.commands
            .iter()
            .all(|command| command.evidence.is_some())
    );
    Ok(())
}

#[tokio::test]
async fn discovery_compilation_failure_preserves_project_output() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    std::fs::create_dir(workspace.path().join("src"))?;
    std::fs::write(
        workspace.path().join("Cargo.toml"),
        "[package]\nname='broken-discovery'\nversion='0.1.0'\nedition='2024'\n",
    )?;
    std::fs::write(
        workspace.path().join("src/lib.rs"),
        "#[test] fn selected() { unavailable_symbol(); }\n",
    )?;
    let mut check = definition("tests", "exit 99");
    check.cases = Some(direct_operation(
        "cargo",
        &[
            "test",
            "--offline",
            "--package",
            "{package.name}",
            "{case.target_args}",
            "--",
            "{case.name}",
            "--exact",
        ],
        Some(PackageManager::Cargo),
    ));
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            selections: BTreeMap::from([(
                "tests".into(),
                CheckSelection::Cases {
                    cases: vec![CheckCase {
                        file: "src/lib.rs".into(),
                        name: "selected".into(),
                        target: Some("lib".into()),
                    }],
                },
            )]),
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Explicit".into(),
    )?;
    let run = runner.run(run, None, None).await?;
    assert_eq!(run.state, CheckState::Failed, "{:?}", run.commands);
    assert_ne!(run.commands[0].exit_code, Some(0));
    assert!(std::fs::read_to_string(&run.commands[0].stderr)?.contains("unavailable_symbol"));
    assert!(std::fs::read_to_string(&run.commands[0].output)?.contains("E0425"));
    Ok(())
}

#[tokio::test]
async fn changing_source_during_a_check_cannot_certify_stale_inputs() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    std::fs::write(workspace.path().join("source.txt"), "before")?;
    save(
        workspace.path(),
        vec![definition("mutating", "'after' | save --force source.txt")],
    );
    let runner = make_runner(workspace.path(), data.path());
    begin_recorded_task(workspace.path(), data.path(), &runner).await?;
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            ids: vec!["mutating".into()],
            ..Default::default()
        },
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Explicit".into(),
    )?;
    let run = runner.run(run, None, None).await?;
    assert_eq!(run.state, CheckState::Stale);
    assert!(run.commands[0].evidence.is_none());
    assert!(
        run.commands[0]
            .error
            .as_deref()
            .unwrap()
            .contains("inputs changed")
    );
    Ok(())
}

#[tokio::test]
async fn automatic_coverage_reuses_a_helpers_resolved_case_mapping() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    std::fs::write(workspace.path().join("source.txt"), "before")?;
    let mut check = definition("mapped", "print all");
    check.scope = CheckScope::ChangedFiles;
    check.files = Some(CheckOperation { input: false, command: "do { let request = ($in | from json); {version: 1, selection: {kind: cases, cases: [{file: source.txt, name: mapped}]}} | to json | save --force $request.receipt_path; print checked }".into(), args: None, package_manager: None });
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    begin_recorded_task(workspace.path(), data.path(), &runner).await?;
    std::fs::write(workspace.path().join("source.txt"), "after")?;
    runner.refresh_changes().await?;
    let initial = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest::default(),
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Explicit".into(),
    )?;
    let initial = runner.run(initial, None, None).await?;
    assert_eq!(initial.state, CheckState::Passed);
    assert!(matches!(
        initial.commands[0].context.as_ref().unwrap().resolved,
        Some(CheckSelection::Cases { .. })
    ));
    let mut repeated = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest::default(),
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Automatic".into(),
    )?;
    repeated.context.as_mut().unwrap().automatic = true;
    let repeated = runner.run(repeated, None, None).await?;
    assert!(repeated.commands[0].reused_from.is_some());
    Ok(())
}

fn node_workspace(root: &std::path::Path, pnpm: bool) -> Result<()> {
    std::fs::write(
        root.join("package.json"),
        r#"{"name":"root","private":true,"workspaces":["packages/*","!packages/excluded"]}"#,
    )?;
    if pnpm {
        std::fs::write(
            root.join("pnpm-workspace.yaml"),
            "packages:\n  - 'packages/*'\n  - '!packages/excluded'\n",
        )?;
    }
    for name in ["0", "1", "excluded"] {
        let directory = root.join("packages").join(name);
        std::fs::create_dir_all(directory.join("src"))?;
        std::fs::write(
            directory.join("package.json"),
            serde_json::to_vec(&serde_json::json!({"name":name}))?,
        )?;
    }
    Ok(())
}

#[tokio::test]
async fn package_resolution_deduplicates_and_respects_membership() -> Result<()> {
    for manager in [PackageManager::Npm, PackageManager::Pnpm] {
        let workspace = tempfile::tempdir()?;
        node_workspace(workspace.path(), manager == PackageManager::Pnpm)?;
        let operation = direct_operation(
            "npm",
            &["test", "--workspace={package.name}"],
            Some(manager),
        );
        let mut resolver = super::invocation::InvocationResolver::default();
        let invocations = resolver
            .resolve(
                workspace.path(),
                ".",
                &operation,
                &CheckSelection::Files {
                    files: vec![
                        "packages/0/src/one.ts".into(),
                        "packages/0/src/deleted.ts".into(),
                        "packages/1/src/two.ts".into(),
                    ],
                },
            )
            .await?;
        assert_eq!(invocations.len(), 2);
        assert_eq!(invocations[0].args, ["test", "--workspace=0"]);
        assert_eq!(invocations[0].files.len(), 2);
        assert!(
            resolver
                .resolve(
                    workspace.path(),
                    ".",
                    &operation,
                    &CheckSelection::Files {
                        files: vec!["packages/excluded/src/one.ts".into()]
                    }
                )
                .await
                .is_err()
        );
        assert!(
            resolver
                .resolve(
                    workspace.path(),
                    ".",
                    &operation,
                    &CheckSelection::Files {
                        files: vec!["package-lock.json".into()]
                    }
                )
                .await
                .is_err()
        );
    }
    for manager in [PackageManager::Npm, PackageManager::Pnpm] {
        let workspace = tempfile::tempdir()?;
        std::fs::write(
            workspace.path().join("package.json"),
            r#"{"name":"single"}"#,
        )?;
        let operation = direct_operation(
            "npm",
            &["--prefix", "{package.directory}", "test"],
            Some(manager),
        );
        let invocations = super::invocation::InvocationResolver::default()
            .resolve(
                workspace.path(),
                ".",
                &operation,
                &CheckSelection::Files {
                    files: vec!["src/one.js".into(), "src/two.js".into()],
                },
            )
            .await?;
        assert_eq!(invocations.len(), 1);
        assert_eq!(
            invocations[0].package.as_ref().unwrap().name.as_deref(),
            Some("single")
        );
        assert!(std::path::Path::new(&invocations[0].args[1]).is_absolute());
    }
    Ok(())
}

#[tokio::test]
async fn discovery_cancellation_stops_the_owned_process() -> Result<()> {
    let cancelled = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let requested = cancelled.clone();
    let signal = tokio::spawn(async move {
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        requested.store(true, std::sync::atomic::Ordering::Release);
    });
    let start = std::time::Instant::now();
    let result = super::invocation::capture(
        "nu",
        &["--no-config-file".into(), "-c".into(), "sleep 10sec".into()],
        std::path::Path::new("."),
        Some(cancelled.as_ref()),
        None,
        3,
    )
    .await;
    signal.await?;
    assert!(result.unwrap_err().to_string().contains("interrupted"));
    assert!(start.elapsed() < std::time::Duration::from_secs(2));
    Ok(())
}

#[tokio::test]
async fn output_directory_failure_does_not_retain_execution_admission() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("one", "print done")]);
    let runner = make_runner(workspace.path(), data.path());
    let prepared = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            include: Vec::new(),
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "test".into(),
    )?;
    std::fs::write(data.path().join(&prepared.id), "blocks directory creation")?;
    assert!(runner.run(prepared, None, None).await.is_err());
    assert!(runner.active()?.is_none());
    Ok(())
}

#[tokio::test]
async fn direct_file_arguments_do_not_become_shell_source() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    std::fs::write(
        workspace.path().join("echo.nu"),
        "def main [value: string] { print $value }",
    )?;
    let mut check = definition("file", "exit 91");
    check.scope = CheckScope::ChangedFiles;
    check.files = Some(direct_operation(
        "nu",
        &["--no-config-file", "echo.nu", "{file.path}"],
        None,
    ));
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let path = "src/spaces $value; [quoted].rs";
    let prepared = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            include: Vec::new(),
            ids: vec!["file".into()],
            selections: BTreeMap::from([(
                "file".into(),
                CheckSelection::Files {
                    files: vec![path.into()],
                },
            )]),
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "test".into(),
    )?;
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(run.state, CheckState::Passed);
    assert_eq!(
        std::fs::read_to_string(&run.commands[0].stdout)?.trim(),
        path
    );
    Ok(())
}

#[tokio::test]
async fn every_package_prerequisite_must_pass() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    node_workspace(workspace.path(), false)?;
    let mut prerequisite = definition("packages", "exit 0");
    prerequisite.scope = CheckScope::ChangedFiles;
    prerequisite.files = Some(direct_operation(
        "nu",
        &["--no-config-file", "-c", "exit {package.name}"],
        Some(PackageManager::Npm),
    ));
    let mut dependent = definition("dependent", "'ran' | save should-not-exist.txt");
    dependent.depends_on = vec!["packages".into()];
    save(workspace.path(), vec![prerequisite, dependent]);
    let runner = make_runner(workspace.path(), data.path());
    let prepared = runner.prepare(
        ConfigSnapshot::load(workspace.path())?,
        CheckAdmission {
            owner: crate::timeline::TimelineOwner::session("checks-test"),
            request: Default::default(),
            overrides: BTreeMap::new(),
            files: Some(vec![
                "packages/0/src/one.ts".into(),
                "packages/1/src/two.ts".into(),
            ]),
            cases: None,
            new_cases: None,
            has_scope: true,
            shell: crate::session::CheckShell::Nushell,
            trigger: "test".into(),
            task_id: None,
            revision: 0,
        },
    )?;
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(run.commands.len(), 3);
    assert_eq!(run.commands[0].state, CheckState::Passed);
    assert_eq!(run.commands[1].state, CheckState::Failed);
    assert_eq!(run.commands[2].state, CheckState::Skipped);
    assert!(!workspace.path().join("should-not-exist.txt").exists());
    Ok(())
}

#[test]
fn new_case_scope_keeps_modified_cases_separate() -> Result<()> {
    let mut check = definition("new", "exit 0");
    check.scope = CheckScope::NewCases;
    check.cases = Some(CheckOperation {
        input: false,
        command: "handler".into(),
        args: None,
        package_manager: None,
    });
    let config = CheckConfig {
        version: 2,
        definitions: vec![check],
    };
    let new = CheckCase {
        file: "src/lib.rs".into(),
        name: "new_case".into(),
        target: Some("lib".into()),
    };
    let modified = CheckCase {
        name: "modified_case".into(),
        ..new.clone()
    };
    let selected = RunRequest::default().selections(
        &config,
        &["src/lib.rs".into()],
        Some(&[new.clone(), modified]),
        Some(std::slice::from_ref(&new)),
        true,
    )?;
    assert_eq!(selected["new"], CheckSelection::Cases { cases: vec![new] });
    Ok(())
}

#[test]
fn advertised_schema_accepts_file_trigger_and_direct_package_arguments() -> Result<()> {
    let mut check = definition("packages", "exit 0");
    check.scope = CheckScope::ChangedFiles;
    check.files = Some(direct_operation(
        "cargo",
        &["test", "--package", "{package.name}", "--tests"],
        Some(PackageManager::Cargo),
    ));
    let mut value = serde_json::to_value(CheckConfig {
        version: 2,
        definitions: vec![check],
    })?;
    value["packages"]["trigger"] = serde_json::json!({"paths":["**/*.rs"]});
    serde_json::from_value::<CheckConfig>(value.clone())?.validate()?;
    let schema = serde_json::to_value(schemars::schema_for!(CheckConfig))?;
    let validator = jsonschema::validator_for(&schema).unwrap();
    assert!(
        validator.is_valid(&value),
        "advertised schema rejects files_changed: {}",
        validator
            .iter_errors(&value)
            .map(|error| error.to_string())
            .collect::<Vec<_>>()
            .join("; ")
    );
    Ok(())
}

#[tokio::test]
async fn cargo_discovery_respects_the_operation_toolchain_and_cache_key() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    std::fs::create_dir(workspace.path().join("src"))?;
    std::fs::write(
        workspace.path().join("Cargo.toml"),
        "[package]\nname='toolchain-fixture'\nversion='0.1.0'\nedition='2024'\n",
    )?;
    std::fs::write(
        workspace.path().join("Cargo.lock"),
        "version = 4\n[[package]]\nname = 'toolchain-fixture'\nversion = '0.1.0'\n",
    )?;
    std::fs::write(workspace.path().join("src/lib.rs"), "")?;
    let mut resolver = super::invocation::InvocationResolver::default();
    let mut operation = direct_operation(
        "cargo",
        &["test", "--package", "{package.name}"],
        Some(PackageManager::Cargo),
    );
    let selection = CheckSelection::Files {
        files: vec!["src/lib.rs".into()],
    };
    let invocation = resolver
        .resolve(workspace.path(), ".", &operation, &selection)
        .await?;
    assert!(
        invocation[0]
            .args
            .iter()
            .any(|argument| argument == "toolchain-fixture")
    );
    operation
        .args
        .as_mut()
        .unwrap()
        .insert(0, "+forge-checks-missing-toolchain".into());
    let failure = resolver
        .resolve(workspace.path(), ".", &operation, &selection)
        .await
        .unwrap_err();
    assert!(
        failure
            .to_string()
            .contains("forge-checks-missing-toolchain"),
        "{failure:#}"
    );
    Ok(())
}

#[tokio::test]
async fn cargo_package_and_new_case_checks_execute_exact_coverage() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    std::fs::create_dir_all(workspace.path().join("src"))?;
    std::fs::create_dir_all(workspace.path().join("tests"))?;
    std::fs::write(
        workspace.path().join("Cargo.toml"),
        "[package]\nname='native-fixture'\nversion='0.1.0'\nedition='2024'\n",
    )?;
    std::fs::write(
        workspace.path().join("Cargo.lock"),
        "version = 4\n[[package]]\nname = 'native-fixture'\nversion = '0.1.0'\n",
    )?;
    std::fs::write(
        workspace.path().join("src/lib.rs"),
        "#[test] fn added() {}\n#[test] fn existing() {}\n#[test] #[ignore] fn ignored() {}\n",
    )?;
    std::fs::write(
        workspace.path().join("tests/gameplay.rs"),
        "#[test] fn integration() {}\n",
    )?;
    let mut packages = definition("packages", "exit 99");
    packages.scope = CheckScope::ChangedFiles;
    packages.files = Some(direct_operation(
        "cargo",
        &[
            "test",
            "--release",
            "--locked",
            "--package",
            "{package.name}",
            "--tests",
        ],
        Some(PackageManager::Cargo),
    ));
    let mut new = definition("new", "exit 99");
    new.scope = CheckScope::NewCases;
    new.cases = Some(direct_operation(
        "cargo",
        &[
            "test",
            "--release",
            "--locked",
            "--package",
            "{package.name}",
            "{case.target_args}",
            "--",
            "{case.name}",
            "--exact",
        ],
        Some(PackageManager::Cargo),
    ));
    save(workspace.path(), vec![packages, new]);
    let runner = make_runner(workspace.path(), data.path());
    let case = CheckCase {
        file: "src/lib.rs".into(),
        name: "added".into(),
        target: Some("lib".into()),
    };
    let prepared = runner.prepare(
        ConfigSnapshot::load(workspace.path())?,
        CheckAdmission {
            owner: crate::timeline::TimelineOwner::session("checks-test"),
            request: Default::default(),
            overrides: BTreeMap::new(),
            files: Some(vec!["src/lib.rs".into()]),
            cases: Some(vec![case.clone()]),
            new_cases: Some(vec![case.clone()]),
            has_scope: true,
            shell: crate::session::CheckShell::Nushell,
            trigger: "plan".into(),
            task_id: None,
            revision: 1,
        },
    )?;
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(run.state, CheckState::Passed, "{:?}", run.commands);
    assert!(std::fs::read_to_string(&run.commands[0].stdout)?.contains("integration"));
    let output = std::fs::read_to_string(&run.commands[1].stdout)?;
    assert!(output.contains("test added ... ok"));
    assert!(!output.contains("test existing ... ok"));
    for name in ["missing", "ignored"] {
        let request = RunRequest {
            include: Vec::new(),
            ids: vec!["new".into()],
            selections: BTreeMap::from([(
                "new".into(),
                CheckSelection::Cases {
                    cases: vec![CheckCase {
                        name: name.into(),
                        ..case.clone()
                    }],
                },
            )]),
            ..Default::default()
        };
        let prepared = runner.prepare_request(
            crate::timeline::TimelineOwner::session("checks-test"),
            request,
            None,
            crate::session::CheckShell::Nushell,
            "explicit".into(),
        )?;
        let run = runner.run(prepared, None, None).await?;
        assert_eq!(
            run.commands
                .iter()
                .find(|command| command.context.as_ref().unwrap().definition_id == "new")
                .unwrap()
                .state,
            CheckState::ConfigurationError
        );
    }
    std::fs::write(
        workspace.path().join("tests/custom.rs"),
        "fn main() { std::fs::write(\"custom-ran\", \"yes\").unwrap(); }",
    )?;
    let manifest = workspace.path().join("Cargo.toml");
    std::fs::write(
        &manifest,
        std::fs::read_to_string(&manifest)? + "\n[[test]]\nname='custom'\nharness=false\n",
    )?;
    let request = RunRequest {
        include: Vec::new(),
        ids: vec!["new".into()],
        selections: BTreeMap::from([(
            "new".into(),
            CheckSelection::Cases {
                cases: vec![CheckCase {
                    file: "tests/custom.rs".into(),
                    name: "declared".into(),
                    target: Some("test:custom".into()),
                }],
            },
        )]),
        ..Default::default()
    };
    let prepared = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        request,
        None,
        crate::session::CheckShell::Nushell,
        "custom".into(),
    )?;
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(run.state, CheckState::ConfigurationError);
    assert!(
        !workspace.path().join("custom-ran").exists(),
        "custom harness must not run during native discovery"
    );
    Ok(())
}

#[tokio::test]
async fn preview_matches_execution_and_manifest_changes_invalidate_bindings() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    node_workspace(workspace.path(), false)?;
    let mut check = definition("packages", "exit 0");
    check.scope = CheckScope::ChangedFiles;
    check.files = Some(direct_operation(
        "nu",
        &["--no-config-file", "-c", "exit {package.name}"],
        Some(PackageManager::Npm),
    ));
    save(workspace.path(), vec![check]);
    let snapshot = ConfigSnapshot::load(workspace.path())?;
    let files = vec!["packages/0/src/new.js".into()];
    let preview = super::preview::resolve(
        workspace.path(),
        &snapshot.config,
        &files,
        None,
        None,
        false,
        &[],
    )
    .await?;
    assert!(preview[0].selected && !preview[0].pending);
    let pending = super::preview::resolve(
        workspace.path(),
        &snapshot.config,
        &files,
        None,
        None,
        true,
        &[],
    )
    .await?;
    assert!(pending[0].pending);
    let runner = make_runner(workspace.path(), data.path());
    let prepared = runner.prepare(
        snapshot,
        CheckAdmission {
            owner: crate::timeline::TimelineOwner::session("checks-test"),
            request: Default::default(),
            overrides: BTreeMap::new(),
            files: Some(files),
            cases: None,
            new_cases: None,
            has_scope: true,
            shell: crate::session::CheckShell::Nushell,
            trigger: "test".into(),
            task_id: None,
            revision: 0,
        },
    )?;
    let resolved = runner.resolve(prepared).await?;
    assert_eq!(preview[0].command, resolved.commands[0].command);
    assert!(
        runner.history()?.is_empty(),
        "preview must not create run history"
    );
    std::fs::write(
        workspace.path().join("packages/0/package.json"),
        r#"{"name":"renamed"}"#,
    )?;
    let run = runner.run(resolved, None, None).await?;
    assert_eq!(run.state, CheckState::Stale);
    assert!(
        run.commands[0]
            .error
            .as_deref()
            .unwrap()
            .contains("configuration changed")
    );
    Ok(())
}

#[tokio::test]
async fn changes_select_overlapping_areas_and_required_builds_without_running_unrelated_checks()
-> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut build = definition("build", "print build");
    build.trigger = CheckTrigger::Manual;
    let mut rust = definition("rust", "print rust");
    rust.area = "Rust".into();
    rust.trigger = CheckTrigger::Paths {
        include: vec!["src/**/*.rs".into(), "Cargo.lock".into()],
        exclude: vec!["src/generated/**".into()],
    };
    let mut native = definition("native", "print native");
    native.area = "Native UI".into();
    native.trigger = rust.trigger.clone();
    native.depends_on = vec!["build".into()];
    let mut other = definition("other", "'unexpected' | save unrelated.txt");
    other.trigger = CheckTrigger::Paths {
        include: vec!["other/**".into()],
        exclude: Vec::new(),
    };
    save(
        workspace.path(),
        vec![
            build,
            rust,
            native,
            other,
            definition("whitespace", "print whitespace"),
        ],
    );
    let runner = make_runner(workspace.path(), data.path());
    let prepared = runner.prepare(
        ConfigSnapshot::load(workspace.path())?,
        super::CheckAdmission {
            owner: crate::timeline::TimelineOwner::session("checks-test"),
            request: Default::default(),
            overrides: Default::default(),
            files: Some(vec![
                "src/nested/removed.rs".into(),
                "src/generated/ignored.rs".into(),
            ]),
            cases: None,
            new_cases: None,
            has_scope: true,
            shell: crate::session::CheckShell::Nushell,
            trigger: "Task".into(),
            task_id: Some("task".into()),
            revision: 0,
        },
    )?;
    assert_eq!(
        prepared.commands[0].context.as_ref().unwrap().reason,
        "Required by native"
    );
    assert_eq!(
        prepared.commands[1].context.as_ref().unwrap().matched_files,
        ["src/nested/removed.rs"]
    );
    assert_eq!(prepared.commands[3].state, CheckState::Skipped);
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(run.state, CheckState::Passed);
    assert!(!workspace.path().join("unrelated.txt").exists());
    assert_eq!(
        run.commands
            .iter()
            .filter(|check| check.state == CheckState::Passed)
            .count(),
        4
    );
    Ok(())
}

#[test]
fn scoped_selection_receives_only_matching_files_and_cases() -> Result<()> {
    let mut check = definition("scoped", "runner");
    check.trigger = CheckTrigger::Paths {
        include: vec!["src/**/*.rs".into()],
        exclude: vec!["src/generated/**".into()],
    };
    check.scope = CheckScope::ChangedFiles;
    check.files = Some(CheckOperation {
        input: false,
        args: None,
        package_manager: None,
        command: "files".into(),
    });
    let mut config = CheckConfig {
        version: 2,
        definitions: vec![check],
    };
    let files = vec![
        "src/root.rs".into(),
        "src/nested/file.rs".into(),
        "src/generated/file.rs".into(),
        "other/file.rs".into(),
    ];
    let request = RunRequest::default();
    let selected = request.selections(&config, &files, None, None, true)?;
    assert_eq!(
        selected["scoped"],
        CheckSelection::Files {
            files: vec!["src/nested/file.rs".into(), "src/root.rs".into()]
        }
    );
    config.definitions[0].scope = CheckScope::ChangedCases;
    let cases = files
        .iter()
        .map(|file| CheckCase {
            target: None,
            file: file.clone(),
            name: "case".into(),
        })
        .collect::<Vec<_>>();
    let selected = request.selections(&config, &files, Some(&cases), None, true)?;
    assert!(matches!(&selected["scoped"], CheckSelection::Cases { cases } if cases.len() == 2));
    assert!(
        super::applicability::resolve(&request, &config, Some(&[]), true)?
            .iter()
            .all(|check| !check.selected)
    );
    assert!(
        super::applicability::resolve(&request, &config, Some(&files), false)?
            .iter()
            .all(|check| !check.selected)
    );
    config.definitions[0].trigger = CheckTrigger::Paths {
        include: vec!["src/[abc].rs".into()],
        exclude: Vec::new(),
    };
    assert!(config.validate().is_err());
    Ok(())
}

#[tokio::test]
async fn missing_case_inventory_and_empty_explicit_selection_cannot_skip_applicable_checks()
-> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut check = definition("unit", "'unexpected full suite' | save unexpected.txt");
    check.scope = CheckScope::ChangedCases;
    check.cases = Some(CheckOperation { input: false, args: None, package_manager: None,command:"do { let request = ($in | from json); {version: 1, selection: $request.selection} | to json | save --force $request.receipt_path; 'selected case' | save --force selected.txt }".into()});
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let case_selection = CheckSelection::Cases {
        cases: vec![CheckCase {
            target: None,
            file: "tests/unit.rs".into(),
            name: "selected_case".into(),
        }],
    };
    for (inventory, selection, expected) in [
        (None, None, CheckState::ConfigurationError),
        (Some(Vec::new()), None, CheckState::Skipped),
        (
            None,
            Some(CheckSelection::Cases { cases: Vec::new() }),
            CheckState::ConfigurationError,
        ),
        (None, Some(case_selection.clone()), CheckState::Passed),
    ] {
        let request = RunRequest {
            include: Vec::new(),
            all: false,
            ids: vec!["unit".into()],
            selections: selection
                .map(|selection| BTreeMap::from([("unit".into(), selection)]))
                .unwrap_or_default(),
        };
        let prepared = runner.prepare(
            ConfigSnapshot::load(workspace.path())?,
            CheckAdmission {
                owner: crate::timeline::TimelineOwner::session("checks-test"),
                request,
                overrides: Default::default(),
                files: Some(vec!["src/source.rs".into()]),
                cases: inventory,
                new_cases: None,
                has_scope: true,
                shell: crate::session::CheckShell::Nushell,
                trigger: "Case inventory regression".into(),
                task_id: None,
                revision: 0,
            },
        )?;
        let run = runner.run(prepared, None, None).await?;
        assert_eq!(run.state, expected, "{:?}", run.commands[0].error);
        assert_eq!(run.commands[0].state, expected);
        if expected == CheckState::ConfigurationError {
            assert!(
                run.commands[0]
                    .error
                    .as_deref()
                    .is_some_and(|error| error.contains("inventory is unavailable")
                        || error.contains("selection is empty"))
            );
        }
        assert!(
            !workspace.path().join("unexpected.txt").exists(),
            "selection failure ran the full suite"
        );
    }
    assert!(workspace.path().join("selected.txt").exists());
    Ok(())
}

#[tokio::test]
async fn checkpoint_selection_excludes_existing_changes_and_survives_renames_deletions_and_reopen()
-> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    assert!(
        std::process::Command::new("git")
            .args(["init", "--quiet"])
            .current_dir(workspace.path())
            .status()?
            .success()
    );
    std::fs::write(
        workspace.path().join("preexisting.txt"),
        "unrelated dirty content",
    )?;
    std::fs::write(workspace.path().join("old.rs"), "before rename")?;
    std::fs::write(workspace.path().join("deleted.rs"), "before delete")?;
    save(workspace.path(), vec![definition("all", "exit 0")]);
    let objects = crate::storage::objects::ObjectStore::open(data.path())?;
    let repositories = Arc::new(forge_git::store::RepositoryStore::default());
    let checkpoint = crate::checkpoint::GitCheckpoint::new(workspace.path());
    let baseline = checkpoint
        .capture(&objects, &repositories, "session", 1)
        .await?;
    let runner = make_runner(workspace.path(), data.path());
    runner.begin_changes("task".into(), &baseline)?;
    std::fs::rename(
        workspace.path().join("old.rs"),
        workspace.path().join("new.rs"),
    )?;
    std::fs::remove_file(workspace.path().join("deleted.rs"))?;
    runner.refresh_changes().await?;
    assert_eq!(
        runner.changes.files()?.unwrap(),
        ["deleted.rs", "new.rs", "old.rs"]
    );
    let later = checkpoint
        .capture(&objects, &repositories, "session", 2)
        .await?;
    runner.begin_changes("task".into(), &later)?;
    let reopened = make_runner(workspace.path(), data.path());
    reopened.refresh_changes().await?;
    assert_eq!(
        reopened.changes.files()?.unwrap(),
        ["deleted.rs", "new.rs", "old.rs"]
    );
    let catalog = reopened.catalog(None)?;
    assert_eq!(catalog["applicability"][0]["selected"], true);
    assert_eq!(catalog["changed_files"].as_array().unwrap().len(), 3);
    Ok(())
}

#[test]
fn configuration_distinguishes_missing_empty_and_invalid_dependencies() {
    let workspace = tempfile::tempdir().unwrap();
    assert!(
        ConfigSnapshot::load(workspace.path())
            .unwrap_err()
            .to_string()
            .contains("checks_not_configured")
    );
    save(workspace.path(), Vec::new());
    assert!(
        ConfigSnapshot::load(workspace.path())
            .unwrap()
            .config
            .definitions
            .is_empty()
    );
    let mut dependent = definition("dependent", "exit 0");
    dependent.depends_on = vec!["missing".into()];
    save(workspace.path(), vec![dependent.clone()]);
    assert!(ConfigSnapshot::load(workspace.path()).is_err());
    dependent.depends_on = vec![dependent.id.clone()];
    assert!(
        CheckConfig {
            version: 2,
            definitions: vec![dependent]
        }
        .validate()
        .is_err()
    );
}

#[test]
fn selection_deduplicates_without_broadening_and_rejects_escape_paths() -> Result<()> {
    let mut selection = CheckSelection::Files {
        files: vec!["src/a $name.rs".into(), "src/a $name.rs".into()],
    };
    selection.normalize()?;
    assert_eq!(
        selection,
        CheckSelection::Files {
            files: vec!["src/a $name.rs".into()]
        }
    );
    assert!(
        CheckSelection::Files {
            files: vec!["../outside.rs".into()]
        }
        .normalize()
        .is_err()
    );
    assert!(
        CheckReceipt {
            version: 1,
            selection: CheckSelection::All
        }
        .validate()
        .is_err()
    );
    assert!(
        CheckReceipt {
            version: 1,
            selection: CheckSelection::Cases { cases: Vec::new() }
        }
        .validate()
        .is_err()
    );
    assert!(
        CheckReceipt {
            version: 1,
            selection: CheckSelection::Files {
                files: vec!["src/other.rs".into()]
            }
        }
        .validate_request(&selection)
        .is_err()
    );
    let requested = CheckSelection::Cases {
        cases: vec![CheckCase {
            target: None,
            file: "src/test.rs".into(),
            name: "one".into(),
        }],
    };
    assert!(
        CheckReceipt {
            version: 1,
            selection: CheckSelection::Files {
                files: vec!["src/test.rs".into()]
            }
        }
        .validate_request(&requested)
        .is_err()
    );
    Ok(())
}

#[tokio::test]
async fn independent_failures_continue_and_dependents_skip() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut dependent = definition("dependent", "'unexpected' | save unexpected.txt");
    dependent.depends_on = vec!["failure".into()];
    save(
        workspace.path(),
        vec![
            dependent,
            definition("failure", "exit 7"),
            definition("independent", "print 'independent output'"),
        ],
    );
    let runner = make_runner(workspace.path(), data.path());
    let prepared = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            include: Vec::new(),
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(run.commands[0].state, CheckState::Failed);
    assert_eq!(run.commands[1].state, CheckState::Skipped);
    assert_eq!(run.commands[2].state, CheckState::Passed);
    assert!(!workspace.path().join("unexpected.txt").exists());
    assert!(std::fs::read_to_string(&run.commands[2].output)?.contains("independent output"));
    let event_count = run.lifecycle.len();
    assert_eq!(runner.history()?[0].lifecycle.len(), event_count);
    assert!(
        run.lifecycle
            .iter()
            .any(|event| matches!(event.kind, CheckEventKind::Finished))
    );
    let reopened = make_runner(workspace.path(), data.path());
    assert_eq!(reopened.history()?[0].lifecycle.len(), event_count);
    Ok(())
}

#[tokio::test]
async fn scoped_handlers_need_receipts_and_never_fallback_to_all() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut check = definition("unit", "'wrong all' | save all-ran.txt");
    check.files = Some(CheckOperation {
        input: false,
        args: None,
        package_manager: None,
        command: "print 'no receipt'".into(),
    });
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let request = RunRequest {
        include: Vec::new(),
        all: false,
        ids: vec!["unit".into()],
        selections: BTreeMap::from([(
            "unit".into(),
            CheckSelection::Files {
                files: vec!["src/special $name.rs".into()],
            },
        )]),
    };
    let prepared = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        request,
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(run.commands[0].state, CheckState::ConfigurationError);
    assert!(!workspace.path().join("all-ran.txt").exists());
    Ok(())
}

#[tokio::test]
async fn scoped_input_preserves_paths_and_valid_receipts() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut check = definition("unit", "exit 0");
    check.files = Some(CheckOperation { input: false, args: None, package_manager: None,command:"do { let request = ($in | from json); {version: 1, selection: $request.selection} | to json | save --force $request.receipt_path; $request.selection.files | to json | save --force received.json }".into()});
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let path = "src/name $var; [case].rs".to_owned();
    let mut files = (0..5000)
        .map(|index| format!("src/{}-{index:05}.rs", "retained".repeat(40)))
        .collect::<Vec<_>>();
    files.push(path);
    files.sort();
    let selection = CheckSelection::Files {
        files: files.clone(),
    };
    let prepared = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            include: Vec::new(),
            all: false,
            ids: vec!["unit".into()],
            selections: BTreeMap::from([("unit".into(), selection.clone())]),
        },
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(
        run.commands[0].state,
        CheckState::Passed,
        "{}",
        run.commands[0].error.as_deref().unwrap_or("")
    );
    assert_eq!(
        run.commands[0].context.as_ref().unwrap().resolved,
        Some(selection)
    );
    assert!(
        std::fs::metadata(&run.commands[0].context.as_ref().unwrap().receipt)?.len() > 1024 * 1024
    );
    assert_eq!(
        serde_json::from_slice::<Vec<String>>(&std::fs::read(
            workspace.path().join("received.json")
        )?)?,
        files
    );
    Ok(())
}

#[tokio::test]
async fn setup_validates_helpers_before_publication() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let runner = make_runner(workspace.path(), data.path());
    let revision = runner.catalog(None)?["revision"]
        .as_str()
        .unwrap()
        .to_owned();
    let config = CheckConfig {
        version: 2,
        definitions: Vec::new(),
    };
    let helper_source = format!(
        "{}let selected = $in | from json\nprint $selected\n",
        "# retained helper\n".repeat(32_769)
    );
    let helper = super::tools::CheckHelper {
        path: ".forge/checks/select.nu".into(),
        source: helper_source.clone(),
        expected_digest: None,
    };
    runner
        .configure(
            crate::timeline::TimelineOwner::session("checks-test"),
            crate::checks::ConfigureChecks {
                expected_revision: revision,
                config: config.clone(),
                helpers: vec![helper],
            },
            None,
            None,
        )
        .await?;
    assert_eq!(
        std::fs::read_to_string(workspace.path().join(".forge/checks/select.nu"))?,
        helper_source
    );
    let revision = runner.catalog(None)?["revision"]
        .as_str()
        .unwrap()
        .to_owned();
    let helper = super::tools::CheckHelper {
        path: ".forge/checks/invalid.nu".into(),
        source: "let = [".into(),
        expected_digest: None,
    };
    assert!(
        runner
            .configure(
                crate::timeline::TimelineOwner::session("checks-test"),
                crate::checks::ConfigureChecks {
                    expected_revision: revision,
                    config,
                    helpers: vec![helper]
                },
                None,
                None
            )
            .await
            .is_err()
    );
    assert!(!workspace.path().join(".forge/checks/invalid.nu").exists());
    Ok(())
}

#[tokio::test]
async fn setup_preserves_fields_and_task_overrides_do_not_leak() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("unit", "exit 0")]);
    let mut original = super::config::read_document(workspace.path())?;
    let unrelated = "λ".repeat(1024 * 1024);
    original["unrelated"] = serde_json::json!(unrelated);
    std::fs::write(
        workspace.path().join(".forge.json"),
        serde_json::to_vec(&original)?,
    )?;
    let runner = make_runner(workspace.path(), data.path());
    let catalog = runner.catalog(None)?;
    runner
        .configure(
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
        )
        .await?;
    assert_eq!(
        super::config::read_document(workspace.path())?["branch_prefix"],
        "feature"
    );
    assert_eq!(
        super::config::read_document(workspace.path())?["unrelated"],
        unrelated
    );
    save(workspace.path(), vec![definition("unit", "exit 0")]);
    runner.set_overrides(
        Some("task-one"),
        super::tools::CheckOverrides {
            auto: Default::default(),
            enabled: BTreeMap::from([("unit".into(), false)]),
            scopes: BTreeMap::new(),
        },
    )?;
    assert!(runner.overrides(Some("task-two"))?.enabled.is_empty());
    assert!(
        runner
            .prepare_request(
                crate::timeline::TimelineOwner::session("checks-test"),
                RunRequest {
                    include: Vec::new(),
                    all: false,
                    ids: vec!["unit".into()],
                    selections: BTreeMap::new()
                },
                Some("task-one".into()),
                crate::session::CheckShell::Nushell,
                "Test".into()
            )
            .is_err()
    );
    assert!(
        runner
            .configure(
                crate::timeline::TimelineOwner::session("checks-test"),
                crate::checks::ConfigureChecks {
                    expected_revision: "stale".into(),
                    config: CheckConfig {
                        version: 2,
                        definitions: Vec::new()
                    },
                    helpers: Vec::new()
                },
                None,
                None
            )
            .await
            .is_err()
    );
    Ok(())
}

#[tokio::test]
async fn setup_bootstraps_an_empty_non_git_directory_and_records_configuration_event() -> Result<()>
{
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let runner = make_runner(workspace.path(), data.path());
    let catalog = runner.catalog(None)?;
    assert_eq!(catalog["configured"], false);
    runner
        .configure(
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
        )
        .await?;
    assert!(
        ConfigSnapshot::load(workspace.path())?
            .config
            .definitions
            .is_empty()
    );
    assert!(matches!(
        runner.history()?[0].lifecycle[0].kind,
        CheckEventKind::ConfigurationSaved
    ));
    Ok(())
}

#[tokio::test]
async fn disabled_prerequisite_is_a_configuration_error_and_independent_checks_continue()
-> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut disabled = definition("prerequisite", "exit 0");
    disabled.enabled = false;
    let mut dependent = definition("dependent", "'unexpected' | save unexpected.txt");
    dependent.depends_on = vec![disabled.id.clone()];
    save(
        workspace.path(),
        vec![disabled, dependent, definition("independent", "print 'ok'")],
    );
    let runner = make_runner(workspace.path(), data.path());
    let prepared = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            include: Vec::new(),
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(run.state, CheckState::ConfigurationError);
    assert_eq!(run.commands[1].state, CheckState::ConfigurationError);
    assert_eq!(run.commands[2].state, CheckState::Passed);
    assert!(!workspace.path().join("unexpected.txt").exists());
    Ok(())
}

#[tokio::test]
async fn concurrent_start_is_rejected_and_cancel_preserves_admission() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("slow", "sleep 5sec")]);
    let runner = make_runner(workspace.path(), data.path());
    let first = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            include: Vec::new(),
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let second = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            include: Vec::new(),
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let id = runner.start(first, None, None, None).await?;
    assert!(runner.start(second, None, None, None).await.is_err());
    runner.cancel(&id)?;
    assert_eq!(runner.active()?.as_deref(), Some(id.as_str()));
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        while runner.active()?.is_some() {
            runner.status(&id, 500, false).await?;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await??;
    assert_eq!(
        runner.status(&id, 0, false).await?.state,
        CheckState::Interrupted
    );
    let mut automatic = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Automatic".into(),
    )?;
    automatic.context.as_mut().unwrap().automatic = true;
    let stopped = tokio::time::timeout(
        std::time::Duration::from_secs(1),
        runner.run(automatic, None, None),
    )
    .await??;
    assert_eq!(stopped.state, CheckState::Stopped);
    assert!(stopped.commands[0].started_at_ms.is_none());
    Ok(())
}

#[tokio::test]
async fn provider_turn_cancellation_collects_its_background_checks() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("slow", "sleep 5sec")]);
    let runner = make_runner(workspace.path(), data.path());
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            include: Vec::new(),
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Agent requested".into(),
    )?;
    let cancelled = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let id = runner
        .start(run, None, Some(Arc::clone(&cancelled)), None)
        .await?;
    cancelled.store(true, std::sync::atomic::Ordering::Release);
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        while runner.active()?.is_some() {
            runner.status(&id, 500, false).await?;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await??;
    assert_eq!(
        runner.status(&id, 0, false).await?.state,
        CheckState::Interrupted
    );
    Ok(())
}

#[tokio::test]
async fn cancellation_collects_process_while_progress_delivery_is_blocked() -> Result<()> {
    use std::time::Duration;
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let ready = data.path().join("ready");
    save(
        workspace.path(),
        vec![definition(
            "slow",
            &format!(
                "'ready' | save --force '{}'; sleep 30sec; 'orphan' | save orphan.txt",
                ready.display()
            ),
        )],
    );
    let runner = make_runner(workspace.path(), data.path());
    let (sink, mut stream) = crate::backend::events::channel();
    let filler = crate::backend::BackendEvent {
        received_at_ms: None,
        address: None,
        turn_boundary: None,
        kind: "assistant_message".into(),
        text: Some("x".repeat(16_384)),
        data: serde_json::Value::Null,
        activity: None,
        summary: None,
        task_update: None,
    };
    while let Ok(result) =
        tokio::time::timeout(Duration::from_millis(20), sink.send_wait(filler.clone())).await
    {
        result?;
    }
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Backpressure test".into(),
    )?;
    let admission = Arc::new(tokio::sync::Semaphore::new(1));
    let permit = Arc::clone(&admission).acquire_owned().await?;
    let id = tokio::time::timeout(
        Duration::from_secs(1),
        runner.start(run, Some(sink), None, Some(permit)),
    )
    .await??;
    tokio::time::timeout(Duration::from_secs(5), async {
        while !ready.exists() {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await?;
    runner.cancel(&id)?;
    tokio::time::timeout(Duration::from_secs(1), async {
        while runner.active()?.is_some() {
            runner.status(&id, 50, false).await?;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await??;
    let snapshot = runner.snapshot()?;
    assert!(snapshot.active_run.is_none());
    assert!(
        Arc::clone(&admission).try_acquire_owned().is_ok(),
        "presentation retained execution admission"
    );
    let finished = snapshot.history.iter().find(|run| run.id == id).unwrap();
    assert_eq!(finished.state, CheckState::Interrupted);
    assert!(finished.completed_at_ms.is_some());
    assert!(!workspace.path().join("orphan.txt").exists());
    // Drain the same transport to prove that the final invalidation survives coalescing.
    tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            let event = stream.recv().await.unwrap().unwrap();
            if event.kind == "document_changed" && event.data["checks_running"] == false {
                break;
            }
        }
    })
    .await?;
    Ok(())
}

#[tokio::test]
async fn cancelled_inputs_require_manual_retry_or_relevant_changes() -> Result<()> {
    use std::time::Duration;
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let ready = data.path().join("ready");
    let allow = data.path().join("allow");
    save(
        workspace.path(),
        vec![definition(
            "controlled",
            &format!(
                "if not ('{}' | path exists) {{ 'ready' | save --force '{}'; sleep 30sec }}; print done",
                allow.display(),
                ready.display()
            ),
        )],
    );
    std::fs::write(workspace.path().join("source.txt"), "before")?;
    let runner = make_runner(workspace.path(), data.path());
    begin_recorded_task(workspace.path(), data.path(), &runner).await?;
    std::fs::write(workspace.path().join("source.txt"), "after")?;
    runner.refresh_changes().await?;
    let prepare = || {
        runner.prepare_request(
            crate::timeline::TimelineOwner::session("checks-test"),
            RunRequest {
                all: true,
                ..Default::default()
            },
            Some("recorded-task".into()),
            crate::session::CheckShell::Nushell,
            "Cancellation test".into(),
        )
    };
    let first = runner.start(prepare()?, None, None, None).await?;
    tokio::time::timeout(Duration::from_secs(5), async {
        while !ready.exists() {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await?;
    runner.cancel(&first)?;
    let cancelled = runner.status(&first, 5000, true).await?;
    assert_eq!(cancelled.state, CheckState::Interrupted);
    assert!(cancelled.cancelled_inputs.is_some());
    let mut automatic = prepare()?;
    automatic.context.as_mut().unwrap().automatic = true;
    let blocked =
        tokio::time::timeout(Duration::from_secs(2), runner.run(automatic, None, None)).await??;
    assert_eq!(blocked.commands[0].state, CheckState::Stopped);
    assert!(blocked.commands[0].started_at_ms.is_none());
    let mut policy = AutomaticChecks::open(data.path(), "recorded-task")?;
    assert!(policy.observe(&blocked)?.is_none());
    assert!(policy.pending && policy.blocked.is_none());
    let reopened = make_runner(workspace.path(), data.path());
    let mut recovered = reopened.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            all: true,
            ..Default::default()
        },
        Some("recorded-task".into()),
        crate::session::CheckShell::Nushell,
        "Recovered automatic run".into(),
    )?;
    recovered.context.as_mut().unwrap().automatic = true;
    assert_eq!(
        reopened.run(recovered, None, None).await?.state,
        CheckState::Stopped
    );
    std::fs::write(&allow, "allow")?;
    let retry = runner.run(prepare()?, None, None).await?;
    assert_eq!(retry.state, CheckState::Passed);
    let mut automatic = prepare()?;
    automatic.context.as_mut().unwrap().automatic = true;
    let repeated = runner.run(automatic, None, None).await?;
    assert_eq!(repeated.state, CheckState::Passed);
    assert!(repeated.commands[0].reused_from.is_some());
    std::fs::write(workspace.path().join("source.txt"), "new inputs")?;
    let mut automatic = prepare()?;
    automatic.context.as_mut().unwrap().automatic = true;
    let changed = runner.run(automatic, None, None).await?;
    assert_eq!(changed.state, CheckState::Passed);
    assert!(changed.commands[0].reused_from.is_none());
    Ok(())
}

#[test]
fn summary_retains_a_single_failure_and_interrupted_work() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(
        workspace.path(),
        vec![
            definition("failure", "exit 1"),
            definition("cancelled", "exit 0"),
            definition("pending", "exit 0"),
            definition("skipped", "exit 0"),
        ],
    );
    let runner = make_runner(workspace.path(), data.path());
    let mut run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Summary".into(),
    )?;
    run.commands[0].state = CheckState::Failed;
    run.commands[1].state = CheckState::Interrupted;
    run.commands[3].state = CheckState::Skipped;
    assert_eq!(run.summary(), "0 passed · 1 failed · 1 interrupted");
    run.commands[0].state = CheckState::Passed;
    run.commands[1].state = CheckState::Stale;
    assert_eq!(run.summary(), "1 passed · 1 stale");
    Ok(())
}

#[tokio::test]
async fn output_failure_collects_the_process_before_releasing_admission() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(
        workspace.path(),
        vec![definition(
            "output",
            "sleep 250ms; 'orphan' | save orphan.txt",
        )],
    );
    let runner = make_runner(workspace.path(), data.path());
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest {
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Output failure".into(),
    )?;
    std::fs::create_dir_all(&run.commands[0].output)?;
    let run = tokio::time::timeout(
        std::time::Duration::from_secs(2),
        runner.run(run, None, None),
    )
    .await??;
    assert_eq!(run.state, CheckState::ExecutionError);
    assert!(runner.active()?.is_none());
    tokio::time::sleep(std::time::Duration::from_millis(300)).await;
    assert!(!workspace.path().join("orphan.txt").exists());
    Ok(())
}
