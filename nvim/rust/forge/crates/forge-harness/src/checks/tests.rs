use super::supervisor::ScriptedCheckOutcome;
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

fn hold_until_cancelled() -> ScriptedCheckOutcome {
    ScriptedCheckOutcome {
        hold_until_cancelled: true,
        ..ScriptedCheckOutcome::default()
    }
}

fn direct_operation(command: &str, args: &[&str], inputs: Option<CheckInputMap>) -> CheckOperation {
    CheckOperation {
        input: false,
        command: command.into(),
        args: Some(args.iter().map(|argument| (*argument).into()).collect()),
        inputs,
    }
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
    check.all = direct_operation(
        "nu",
        &["--no-config-file", "echo.nu", "{file}"],
        Some(serde_json::from_value(serde_json::json!({
            "file":{"type":"string","description":"File argument","required":true}
        }))?),
    );
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let invocations = Arc::new(Mutex::new(Vec::<(String, Vec<String>)>::new()));
    let recorded_invocations = Arc::clone(&invocations);
    runner.script_commands(move |request| {
        let (program, args) = request
            .invocation
            .expect("a direct check launches without shell source");
        assert!(request.command.is_empty());
        recorded_invocations
            .lock()
            .unwrap()
            .push((program.to_owned(), args.to_vec()));
        ScriptedCheckOutcome {
            stdout: format!(
                "{}
",
                args.last().expect("file argument")
            ),
            ..ScriptedCheckOutcome::default()
        }
    });
    let path = "src/spaces $value; [quoted].rs";
    let prepared = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        crate::checks::RunRequest::Selected(crate::checks::SelectedChecks {
            include: Vec::new(),
            ids: vec!["file".into()],
            selections: BTreeMap::from([(
                "file".into(),
                CheckSelection::Inputs {
                    inputs: BTreeMap::from([("file".into(), serde_json::json!(path))]),
                },
            )]),
        }),
        None,
        crate::session::CheckShell::Nushell,
        "test".into(),
    )?;
    let run = runner.run(prepared, None, None).await?;
    assert_eq!(run.state, CheckState::Passed);
    assert_eq!(
        *invocations.lock().unwrap(),
        [(
            "nu".to_owned(),
            vec![
                "--no-config-file".to_owned(),
                "echo.nu".to_owned(),
                path.to_owned()
            ]
        )]
    );
    assert_eq!(
        std::fs::read_to_string(&run.commands[0].stdout)?.trim(),
        path
    );
    Ok(())
}

#[tokio::test]
async fn changes_select_overlapping_areas_and_required_builds_without_running_unrelated_checks()
-> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut build = definition("build", "print build");
    build.auto = false;
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
    assert_eq!(prepared.commands[0].context.reason, "Required by native");
    assert_eq!(
        prepared.commands[1].context.matched_files,
        ["src/nested/removed.rs"]
    );
    assert_eq!(
        prepared
            .commands
            .iter()
            .find(|command| { command.context.definition_id == "other" })
            .unwrap()
            .state,
        CheckState::Skipped
    );
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

#[tokio::test]
async fn explicit_case_selection_rejects_empty_requests_without_running_the_helper() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut check = definition("unit", "exit 99");
    check.all = CheckOperation { input: true, args: None, inputs: None,command:"do { let request = ($in | from json); {version: 1, selection: $request.selection} | to json | save --force $request.receipt_path; 'selected case' | save --force selected.txt }".into()};
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    runner.script_commands(|request| {
        std::fs::write(request.directory.join("selected.txt"), "selected case")
            .expect("scripted helper writes its marker");
        std::fs::write(
            request.receipt,
            serde_json::to_vec(&serde_json::json!({"version": 1, "selection": request.selection}))
                .unwrap(),
        )
        .expect("scripted helper writes its receipt");
        ScriptedCheckOutcome::default()
    });
    let case_selection = CheckSelection::Cases {
        cases: vec![CheckCase {
            target: None,
            file: "tests/unit.rs".into(),
            name: "selected_case".into(),
        }],
    };
    for (selection, expected) in [
        (
            CheckSelection::Cases { cases: Vec::new() },
            CheckState::ConfigurationError,
        ),
        (case_selection.clone(), CheckState::Passed),
    ] {
        let request = crate::checks::RunRequest::Selected(crate::checks::SelectedChecks {
            include: Vec::new(),

            ids: vec!["unit".into()],
            selections: BTreeMap::from([("unit".into(), selection)]),
        });
        let prepared = runner.prepare(
            ConfigSnapshot::load(workspace.path())?,
            CheckAdmission {
                owner: crate::timeline::TimelineOwner::session("checks-test"),
                request,
                overrides: Default::default(),
                files: Some(vec!["src/source.rs".into()]),
                cases: None,
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
                    .is_some_and(|error| error.contains("selection is empty"))
            );
            assert!(!workspace.path().join("selected.txt").exists());
        }
    }
    assert!(workspace.path().join("selected.txt").exists());
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
        crate::checks::RunRequest::All,
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
    check.scope = CheckScope::ChangedFiles;
    check.all = CheckOperation {
        input: true,
        args: None,
        inputs: None,
        command: "print 'no receipt'".into(),
    };
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    runner.script_commands(|_| ScriptedCheckOutcome {
        stdout: "no receipt
"
        .into(),
        ..ScriptedCheckOutcome::default()
    });
    let request = crate::checks::RunRequest::Selected(crate::checks::SelectedChecks {
        include: Vec::new(),

        ids: vec!["unit".into()],
        selections: BTreeMap::from([(
            "unit".into(),
            CheckSelection::Files {
                files: vec!["src/special $name.rs".into()],
            },
        )]),
    });
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

#[tokio::test]
async fn concurrent_start_is_rejected_and_cancel_preserves_admission() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("slow", "sleep 5sec")]);
    let runner = make_runner(workspace.path(), data.path());
    runner.script_commands(|_| hold_until_cancelled());
    let first = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        crate::checks::RunRequest::All,
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let second = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        crate::checks::RunRequest::All,
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let id = runner.start(first, None, None, None).await?;
    assert!(runner.start(second, None, None, None).await.is_err());
    runner.cancel(&id)?;
    assert_eq!(runner.active()?.as_deref(), Some(id.as_str()));
    async {
        while runner.active()?.is_some() {
            runner.status(&id, 500, false).await?;
        }
        Ok::<_, anyhow::Error>(())
    }
    .await?;
    assert_eq!(
        runner.status(&id, 0, false).await?.state,
        CheckState::Interrupted
    );
    let mut automatic = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        crate::checks::RunRequest::All,
        None,
        crate::session::CheckShell::Nushell,
        "Automatic".into(),
    )?;
    automatic.context.automatic = true;
    let stopped = runner.run(automatic, None, None).await?;
    assert_eq!(stopped.state, CheckState::Stopped);
    assert!(stopped.commands[0].started_at_ms.is_none());
    Ok(())
}

/// Kill a timed-out shell and stop the remaining check commands with a retained error.
///
/// The configured hour-long deadline never elapses during the test. The supervisor's forced
/// expiry fires the same deadline path while the shell sleeps, because Windows child pipes
/// hold blocking reads that keep a paused clock from auto-advancing.
#[tokio::test]
async fn shell_timeout_kills_the_process_and_stops_the_check_run() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(
        workspace.path(),
        vec![
            definition(
                "a-slow",
                "print ready; sleep 30sec; 'orphan' | save orphan.txt",
            ),
            definition("b-never", "'unexpected' | save unexpected.txt"),
        ],
    );
    let runner = make_runner(workspace.path(), data.path());
    runner.script_commands(|request| {
        if request.command.starts_with("print ready") {
            ScriptedCheckOutcome {
                stdout: "ready
"
                .into(),
                hold_until_cancelled: true,
                ..ScriptedCheckOutcome::default()
            }
        } else {
            panic!(
                "a command after the timed-out one must not start: {}",
                request.command
            )
        }
    });
    let mut settings = crate::settings::SettingsStore::open(data.path())?;
    settings.patch(
        crate::settings::SettingTarget {
            scope: crate::settings::SettingScope::Global,
            workspace: workspace.path().to_str().unwrap(),
            session: "checks-test",
        },
        &crate::settings::PortableSettings {
            tool_timeout: Some(crate::session::InputTimeout {
                amount: 1,
                unit: crate::session::TimeoutUnit::H,
            }),
            ..Default::default()
        },
    )?;
    let request = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        RunRequest::All,
        None,
        crate::session::CheckShell::Nushell,
        "Timeout regression".into(),
    )?;
    runner.expire_command_deadline();
    let failure = runner.run(request, None, None).await.unwrap_err();
    assert!(failure.to_string().contains("timeout"));
    assert!(runner.active()?.is_none());
    let history = runner.history()?;
    let run = history.last().unwrap();
    assert_eq!(run.state, CheckState::ExecutionError);
    assert!(
        run.commands[0]
            .error
            .as_deref()
            .unwrap()
            .contains("timeout")
    );
    assert!(run.commands[1].started_at_ms.is_none());
    assert!(!workspace.path().join("orphan.txt").exists());
    assert!(!workspace.path().join("unexpected.txt").exists());
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
    let ready_marker = ready.clone();
    runner.script_commands(move |_| {
        std::fs::write(&ready_marker, "ready").expect("scripted command signals readiness");
        hold_until_cancelled()
    });
    let (sink, mut stream) = crate::broker::publication::channel();
    let filler = crate::broker::publication::SessionUpdate::CheckFailure {
        run_id: "backpressure-filler".into(),
        error: "x".repeat(16_384),
    };
    // A bounded channel refuses a send exactly when it is full, so poll each send once.
    loop {
        let mut pending_send = std::pin::pin!(sink.send_wait(filler.clone()));
        match futures_util::poll!(pending_send.as_mut()) {
            std::task::Poll::Ready(result) => result?,
            std::task::Poll::Pending => break,
        }
    }
    let run = runner.prepare_request(
        crate::timeline::TimelineOwner::session("checks-test"),
        crate::checks::RunRequest::All,
        None,
        crate::session::CheckShell::Nushell,
        "Backpressure test".into(),
    )?;
    let admission = crate::command::SessionCoordinator::new(Arc::new(
        crate::input::SessionInputQueue::default(),
    ))
    .execution
    .clone();
    let permit = Arc::clone(&admission).acquire_owned().await?;
    let id = runner.start(run, Some(sink), None, Some(permit)).await?;
    async {
        while !ready.exists() {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    }
    .await;
    runner.cancel(&id)?;
    async {
        while runner.active()?.is_some() {
            runner.status(&id, 50, false).await?;
        }
        Ok::<_, anyhow::Error>(())
    }
    .await?;
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
    async {
        loop {
            let event = stream.recv().await.unwrap().unwrap();
            if matches!(event, crate::broker::publication::SessionUpdate::DocumentChanged { patch }
            if patch.operation.iter().any(|operation| matches!(operation,
                crate::timeline::stream::TimelineOperation::Insert {
                    entry: crate::timeline::TimelineEntry::Status { checks: None, .. }, ..
                } | crate::timeline::stream::TimelineOperation::Replace {
                    entry: crate::timeline::TimelineEntry::Status { checks: None, .. }, ..
                }
            ))) {
                break;
            }
        }
    }
    .await;
    Ok(())
}
