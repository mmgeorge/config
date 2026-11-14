use super::*;
use anyhow::Result;
use std::sync::{Arc, Mutex};

fn definition(id: &str, command: &str) -> CheckDefinition {
    CheckDefinition {
        auto: true,
        id: id.into(),
        name: id.into(),
        category: CheckCategory::Custom,
        enabled: true,
        area: "Project".into(),
        trigger: CheckTrigger::AnyChange,
        scope: CheckScope::All,
        directory: ".".into(),
        runner: CheckRunner::Command(CheckOperation {
            prepare: false,
            args: None,
            inputs: None,
            command: command.into(),
        }),
        depends_on: Vec::new(),
    }
}

fn save(workspace: &std::path::Path, definitions: Vec<CheckDefinition>) {
    std::fs::write(workspace.join(".forge.json"),serde_json::to_vec(&serde_json::json!({"branch_prefix":"feature","checks":CheckConfig { version: 2, definitions }})).unwrap()).unwrap();
}

/// Reject invalid fix batches before admission, preserving read-only dependency purpose.
#[test]
fn apply_preflight_rejects_mixed_categories_and_implicit_mutations() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    let mut prerequisite = definition("prerequisite", "unused");
    let mut formatter = definition("format", "unused");
    formatter.category = CheckCategory::Format;
    formatter.runner = CheckRunner::Adapter(adapter::AdapterConfig::Script(
        adapter::ScriptAdapterConfig {
            command: vec!["unused".into()],
        },
    ));
    formatter.depends_on = vec![prerequisite.id.clone()];
    save(
        workspace.path(),
        vec![formatter.clone(), prerequisite.clone()],
    );
    let runner = make_runner(workspace.path(), data.path());
    let prepare = |names: Vec<String>| {
        runner.prepare(
            ConfigSnapshot::load(workspace.path())?,
            CheckAdmission {
                owner: runner.session_owner(),
                request: RunRequest::Selected(SelectedChecks {
                    purpose: CheckPurpose::Prepare,
                    selections: names
                        .iter()
                        .map(|id| (id.clone(), CheckSelection::All))
                        .collect(),
                    ids: names,
                    ..Default::default()
                }),
                overrides: Default::default(),
                task_overrides: Default::default(),
                files: Some(Vec::new()),
                has_scope: true,
                shell: Default::default(),
                trigger: "apply preflight".into(),
                task_id: None,
                revision: 0,
            },
        )
    };
    let run = prepare(vec!["format".into()])?;
    assert_eq!(run.commands[0].context.definition_id, "prerequisite");
    assert_eq!(run.commands[0].context.purpose, CheckPurpose::Verify);
    assert_eq!(run.commands[1].context.purpose, CheckPurpose::Prepare);
    assert!(prepare(vec!["format".into(), "prerequisite".into()]).is_err());
    prerequisite.category = CheckCategory::Custom;
    let CheckRunner::Command(operation) = &mut prerequisite.runner else {
        unreachable!()
    };
    operation.prepare = true;
    save(workspace.path(), vec![formatter, prerequisite]);
    assert!(prepare(vec!["format".into()]).is_err());
    assert!(runner.history()?.is_empty());
    Ok(())
}

/// Schedule each run's actual preparation operations before unrelated verification.
#[test]
fn preparation_order_follows_operations_and_keeps_dependency_checks_readonly() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    let config: CheckConfig = serde_json::from_value(serde_json::json!({
        "compiler":{"action":{"category":"typecheck","adapter":{"script":{"command":["unused"]}}}},
        "build":{"action":{"category":"build","adapter":{"script":{"command":["unused"]}}}},
        "prepare":{"action":{"category":"custom","command":["unused"],"prepare":true,"after":["build"]}},
        "format":{"action":{"category":"format","after":["build"],"adapter":{"script":{"command":["unused"]}}}},
        "lint":{"action":{"category":"lint","after":["format"],"adapter":{"script":{"command":["unused"]}}}}
    }))?;
    save(workspace.path(), config.definitions);
    let runner = make_runner(workspace.path(), data.path());
    let prepare = |request| {
        runner.prepare(
            ConfigSnapshot::load(workspace.path())?,
            CheckAdmission {
                owner: runner.session_owner(),
                request,
                overrides: Default::default(),
                task_overrides: Default::default(),
                files: Some(Vec::new()),
                has_scope: true,
                shell: Default::default(),
                trigger: "operation order".into(),
                task_id: None,
                revision: 0,
            },
        )
    };
    let mut reordered = ConfigSnapshot::load(workspace.path())?.config;
    let mut alpha = reordered
        .definitions
        .iter()
        .find(|definition| definition.id == "compiler")
        .unwrap()
        .clone();
    alpha.id = "alpha".into();
    reordered.definitions.push(alpha);
    reordered.definitions.reverse();
    assert_eq!(
        reordered
            .order()?
            .into_iter()
            .map(|index| reordered.definitions[index].id.as_str())
            .collect::<Vec<_>>(),
        vec!["build", "prepare", "alpha", "compiler", "format", "lint"]
    );
    let ordinary = prepare(RunRequest::All)?;
    let actual = ordinary
        .commands
        .iter()
        .map(|command| {
            (
                command.context.definition_id.as_str(),
                command.context.purpose,
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        actual,
        vec![
            ("build", CheckPurpose::Verify),
            ("prepare", CheckPurpose::Prepare),
            ("compiler", CheckPurpose::Verify),
            ("format", CheckPurpose::Verify),
            ("lint", CheckPurpose::Verify)
        ]
    );
    let restored: CheckRun = serde_json::from_value(serde_json::to_value(ordinary.into_record())?)?;
    let custom = restored
        .commands
        .iter()
        .find(|command| command.context.as_ref().unwrap().definition_id == "prepare")
        .unwrap();
    assert_eq!(
        custom.context.as_ref().unwrap().purpose,
        CheckPurpose::Prepare
    );
    assert!(
        custom
            .context
            .as_ref()
            .unwrap()
            .operation
            .as_ref()
            .unwrap()
            .prepare
    );
    for (names, expected) in [
        (
            vec!["lint"],
            vec![
                ("build", CheckPurpose::Verify),
                ("format", CheckPurpose::Verify),
                ("lint", CheckPurpose::Prepare),
            ],
        ),
        (
            vec!["format", "lint"],
            vec![
                ("build", CheckPurpose::Verify),
                ("format", CheckPurpose::Prepare),
                ("lint", CheckPurpose::Prepare),
            ],
        ),
    ] {
        let request = RunRequest::Selected(SelectedChecks {
            purpose: CheckPurpose::Prepare,
            ids: names.iter().map(|id| (*id).into()).collect(),
            selections: names
                .iter()
                .map(|id| ((*id).into(), CheckSelection::All))
                .collect(),
            ..Default::default()
        });
        let run = prepare(request)?;
        assert_eq!(
            run.commands
                .iter()
                .filter(|command| command.selected)
                .map(|command| (
                    command.context.definition_id.as_str(),
                    command.context.purpose
                ))
                .collect::<Vec<_>>(),
            expected
        );
        assert_eq!(run.commands[0].context.definition_id, "build");
        assert!(
            !run.commands
                .iter()
                .find(|command| command.context.definition_id == "prepare")
                .unwrap()
                .selected
        );
    }
    assert!(runner.history()?.is_empty());
    Ok(())
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

/// Keep implicit empty inputs inert while admitting explicitly requested whole coverage.
#[tokio::test]
async fn adapter_empty_defaults_skip_but_explicit_empty_selectors_admit_all() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    let config: CheckConfig = serde_json::from_value(serde_json::json!({
        "unit":{"action":{"category":"test","adapter":{"script":{"command":["unused"]}}}},
        "compiler":{"action":{"category":"typecheck","adapter":{"script":{"command":["unused"]}}}}
    }))?;
    save(workspace.path(), config.definitions);
    let runner = make_runner(workspace.path(), data.path());
    for (id, explicit) in [
        (
            "unit",
            CheckSelection::Tests {
                tests: Default::default(),
            },
        ),
        (
            "compiler",
            CheckSelection::Paths {
                paths: Default::default(),
            },
        ),
    ] {
        for selection in [None, Some(explicit)] {
            let is_explicit = selection.is_some();
            let run = runner.prepare(
                ConfigSnapshot::load(workspace.path())?,
                CheckAdmission {
                    owner: runner.session_owner(),
                    request: RunRequest::Selected(SelectedChecks {
                        ids: vec![id.into()],
                        selections: selection
                            .map(|selection| [(id.into(), selection)].into())
                            .unwrap_or_default(),
                        ..Default::default()
                    }),
                    overrides: Default::default(),
                    task_overrides: Default::default(),
                    files: Some(Vec::new()),
                    has_scope: true,
                    shell: Default::default(),
                    trigger: "empty selection".into(),
                    task_id: None,
                    revision: 0,
                },
            )?;
            let run = runner.resolve(run).await?;
            let command = run
                .commands
                .iter()
                .find(|command| command.context.definition_id == id)
                .unwrap();
            assert_eq!(
                command.state,
                if is_explicit {
                    CheckState::Pending
                } else {
                    CheckState::Skipped
                }
            );
            assert!(command.started_at_ms.is_none());
            if !is_explicit {
                assert_eq!(command.error.as_deref(), Some("No matching task changes"));
            }
        }
    }
    assert!(runner.history()?.is_empty());
    Ok(())
}

/// Reject removed authored input sources without rewriting the user's configuration.
#[test]
fn removed_planned_case_input_reports_migration_without_changing_configuration() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let source = serde_json::to_vec(&serde_json::json!({"checks":{
        "legacy":{"action":{"category":"custom","command":["unused","{cases}"],"inputs":{
            "cases":{"type":"array","description":"planned cases","source":"planned_cases"}
        }}}
    }}))?;
    let path = workspace.path().join(".forge.json");
    std::fs::write(&path, &source)?;
    let error = ConfigSnapshot::load(workspace.path()).unwrap_err();
    let message = format!("{error:#}");
    assert!(
        message.contains("legacy: input `cases`")
            && message.contains("planned_cases")
            && message.contains("test adapter"),
        "{message}"
    );
    assert_eq!(std::fs::read(path)?, source);
    Ok(())
}

/// Restore the retained custom result without consulting obsolete callback sidecars.
#[test]
fn custom_history_ignores_legacy_sidecars_and_excludes_other_versions() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    save(workspace.path(), vec![definition("custom", "unused")]);
    let runner = make_runner(workspace.path(), data.path());
    let mut run = runner
        .prepare(
            ConfigSnapshot::load(workspace.path())?,
            CheckAdmission {
                owner: runner.session_owner(),
                request: RunRequest::Selected(SelectedChecks {
                    ids: vec!["custom".into()],
                    selections: [("custom".into(), CheckSelection::All)].into(),
                    ..Default::default()
                }),
                overrides: Default::default(),
                task_overrides: Default::default(),
                files: Some(Vec::new()),
                has_scope: true,
                shell: Default::default(),
                trigger: "history test".into(),
                task_id: None,
                revision: 0,
            },
        )?
        .into_record();
    run.state = CheckState::Passed;
    run.commands[0].state = CheckState::Passed;
    let directory = data.path().join(&run.id);
    let path = directory.join("record.json");
    super::supervisor::write_record(&path, &run)?;
    for name in ["0.selection.json", "0.selection.report.json"] {
        std::fs::write(directory.join(name), b"malformed obsolete callback data")?;
    }
    let restored = CheckSupervisor::saved_history(data.path())?;
    assert_eq!(restored.len(), 1);
    assert_eq!(restored[0].state, CheckState::Passed);
    assert!(restored[0].commands[0].result.is_none());
    let original = std::fs::read(&path)?;
    let mut record: serde_json::Value = serde_json::from_slice(&original)?;
    let version = record["version"].as_u64().unwrap();
    for incompatible in [version - 1, version + 1] {
        record["version"] = incompatible.into();
        std::fs::write(&path, serde_json::to_vec(&record)?)?;
        assert!(CheckSupervisor::saved_history(data.path())?.is_empty());
    }
    std::fs::write(&path, original)?;
    assert_eq!(CheckSupervisor::saved_history(data.path())?.len(), 1);
    Ok(())
}

/// Recovery retains collected cases, unfinished cases, and an already committed outcome.
#[test]
fn recovery_preserves_partial_results_and_committed_outcomes() -> Result<()> {
    use adapter::events::{CaseStatus, TestEvent, TestEventSink};

    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    let mut check = definition("unit", "unused");
    check.category = CheckCategory::Test;
    check.runner = CheckRunner::Adapter(adapter::AdapterConfig::Script(
        adapter::ScriptAdapterConfig {
            command: vec!["unused".into()],
        },
    ));
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let admitted = runner
        .prepare(
            ConfigSnapshot::load(workspace.path())?,
            CheckAdmission {
                owner: runner.session_owner(),
                request: RunRequest::All,
                overrides: Default::default(),
                task_overrides: Default::default(),
                files: None,
                has_scope: false,
                shell: Default::default(),
                trigger: "recovery".into(),
                task_id: None,
                revision: 1,
            },
        )?
        .into_record();
    let mut observed = admitted.clone();
    let mut events = TestEventSink::new(Default::default(), data.path())?;
    events.accept(TestEvent::Plan {
        cases: vec![
            "first".to_owned().try_into()?,
            "second".to_owned().try_into()?,
            "third".to_owned().try_into()?,
        ],
        widened: None,
        items: vec![],
    })?;
    events.accept(TestEvent::Started {
        id: "first".to_owned().try_into()?,
    })?;
    events.accept(TestEvent::Finished {
        id: "first".to_owned().try_into()?,
        status: CaseStatus::Passed,
        duration_ms: Some(1),
        failures: vec![],
    })?;
    events.accept(TestEvent::Started {
        id: "second".to_owned().try_into()?,
    })?;
    observed.commands[0].state = CheckState::Running;
    observed.commands[0].started_at_ms = Some(observed.started_at_ms);
    observed.commands[0].result = Some(adapter::AdapterResult::Tests(events.finish(None, true)));
    let path = data.path().join(&admitted.id).join("record.json");
    super::supervisor::write_record(&path, &observed)?;
    let recovered =
        super::recover_runs(data.path(), "checks-test", std::slice::from_ref(&admitted))?;
    let run = &recovered[0];
    assert_eq!(run.state, CheckState::Interrupted);
    assert!(run.completed_at_ms.is_some());
    assert_eq!(run.commands[0].state, CheckState::Interrupted);
    assert_eq!(run.commands[0].completed_at_ms, run.completed_at_ms);
    let cases = &run.commands[0]
        .result
        .as_ref()
        .unwrap()
        .tests()
        .unwrap()
        .cases;
    assert_eq!(cases[0].status, Some(CaseStatus::Passed));
    assert!(cases[1..].iter().all(|case| case.status.is_none()));
    let repeated =
        super::recover_runs(data.path(), "checks-test", std::slice::from_ref(&admitted))?;
    assert_eq!(
        serde_json::to_value(&recovered)?,
        serde_json::to_value(repeated)?
    );

    let mut uncommitted = observed.clone();
    uncommitted.state = CheckState::Passed;
    uncommitted.completed_plan = Some("Not yet committed".into());
    uncommitted.commands[0].state = CheckState::Passed;
    let Some(adapter::AdapterResult::Tests(result)) = &mut uncommitted.commands[0].result else {
        anyhow::bail!("test fixture lost its test result");
    };
    result.cancelled = false;
    result.exit = Some(0);
    for case in &mut result.cases {
        case.status = Some(CaseStatus::Passed);
    }
    super::supervisor::write_record(&path, &uncommitted)?;
    let unfinished =
        super::recover_runs(data.path(), "checks-test", std::slice::from_ref(&admitted))?;
    assert_eq!(unfinished[0].state, CheckState::Interrupted);
    assert!(unfinished[0].completed_plan.is_none());
    assert_eq!(unfinished[0].commands[0].state, CheckState::Passed);

    let mut committed = run.clone();
    committed.state = CheckState::Failed;
    super::supervisor::write_record(&path, &observed)?;
    let recovered =
        super::recover_runs(data.path(), "checks-test", std::slice::from_ref(&committed))?;
    assert_eq!(
        serde_json::to_value(&recovered[0])?,
        serde_json::to_value(&committed)?
    );
    let restored = CheckSupervisor::saved_history(data.path())?;
    assert_eq!(
        serde_json::to_value(restored)?,
        serde_json::to_value(recovered)?
    );
    Ok(())
}

/// Pending mutation obligations survive reopen and require a current whole-check pass.
#[test]
fn verification_requirements_survive_history_loss_and_reject_stale_passes() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    let mut check = definition("format", "unused");
    check.category = CheckCategory::Format;
    check.runner = CheckRunner::Adapter(adapter::AdapterConfig::Script(
        adapter::ScriptAdapterConfig {
            command: vec!["nu".into(), "format.nu".into()],
        },
    ));
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let mut mutation = runner
        .prepare(
            ConfigSnapshot::load(workspace.path())?,
            CheckAdmission {
                owner: runner.session_owner(),
                request: RunRequest::All,
                overrides: Default::default(),
                task_overrides: Default::default(),
                files: Some(Vec::new()),
                has_scope: true,
                shell: Default::default(),
                trigger: "fix".into(),
                task_id: Some("task".into()),
                revision: 0,
            },
        )?
        .into_record();
    let mut verification = mutation.clone();
    mutation.commands[0].context.as_mut().unwrap().purpose = CheckPurpose::Prepare;
    verification.commands[0].id = "verify-1".into();
    verification.commands[0].state = CheckState::Passed;
    let before = crate::checkpoint::CheckpointRecord {
        id: "before".into(),
        session_id: "checks-test".into(),
        workspace: workspace.path().to_string_lossy().into_owned(),
        head: "UNBORN".into(),
        tree: None,
        file: Vec::new(),
        deleted: Vec::new(),
        checkout: Default::default(),
        excluded: Default::default(),
        created_at_ms: 0,
    };
    let mut ledger = runner.verification.lock().unwrap();
    ledger.prepare(&mutation, &mutation.commands[0])?;
    assert_eq!(ledger.pending(Some(&before))?.len(), 1);
    drop(ledger);
    // No command history is needed to recover the frozen definition and original selection.
    let mut ledger =
        verification::VerificationRequirements::open(data.path(), workspace.path(), "checks-test")?;
    assert_eq!(ledger.pending(Some(&before))?[0].definition.id, "format");
    let frozen = ledger.resolution_input(&before)?;
    let frozen: verification::ResolutionRequirements =
        serde_json::from_value(serde_json::to_value(frozen)?)?;
    assert_eq!(
        frozen
            .pending(&verification.context.as_ref().unwrap().snapshot)
            .len(),
        1
    );
    frozen.validate(&ledger)?;
    let mut scheduled: PreparedCheckRun = verification.clone().try_into()?;
    scheduled.commands[0].selected = false;
    scheduled.commands[0].state = CheckState::Skipped;
    scheduled.commands[0].context.selection = CheckSelection::Files { files: Vec::new() };
    ledger.require_in_run(&mut scheduled, &before)?;
    assert!(scheduled.commands[0].selected);
    assert_eq!(scheduled.commands[0].state, CheckState::Pending);
    assert_eq!(scheduled.commands[0].context.selection, CheckSelection::All);
    let mut disabled = scheduled.clone();
    disabled.context.overrides.insert("format".into(), false);
    assert!(
        ledger
            .require_in_run(&mut disabled, &before)
            .unwrap_err()
            .to_string()
            .contains("disabled")
    );
    // Reviewer exclusions do not waive mutation obligations, but task disabling still blocks them.
    disabled.planned_tests = Some(super::PlannedTestExecution::new(
        Vec::new(),
        Default::default(),
    )?);
    ledger.require_in_run(&mut disabled, &before)?;
    assert!(disabled.commands[0].selected);
    disabled
        .context
        .task_overrides
        .enabled
        .insert("format".into(), false);
    assert!(ledger.require_in_run(&mut disabled, &before).is_err());
    let mut altered = scheduled.clone();
    altered.context.snapshot.config.definitions[0].directory = "changed-directory".into();
    assert!(
        ledger
            .require_in_run(&mut altered, &before)
            .unwrap_err()
            .to_string()
            .contains("changed")
    );
    let mut missing = scheduled.clone();
    missing.context.snapshot.config.definitions.clear();
    assert!(
        ledger
            .require_in_run(&mut missing, &before)
            .unwrap_err()
            .to_string()
            .contains("missing")
    );
    let capture = ledger.capture();
    verification.commands[0].context.as_mut().unwrap().selection = CheckSelection::Paths {
        paths: adapter::PathSelection {
            paths: vec!["one.rs".into()],
        },
    };
    ledger.observe(
        capture,
        &verification,
        &verification.commands[0],
        &before,
        &before,
    )?;
    assert_eq!(ledger.pending(Some(&before))?.len(), 1);
    verification.commands[0].context.as_mut().unwrap().selection = CheckSelection::All;
    let capture = ledger.capture();
    ledger.observe(
        capture,
        &verification,
        &verification.commands[0],
        &before,
        &before,
    )?;
    assert!(ledger.pending(Some(&before))?.is_empty());
    frozen.validate(&ledger)?;
    assert_eq!(
        frozen
            .pending(&verification.context.as_ref().unwrap().snapshot)
            .len(),
        1
    );
    // Never-launched and proven unsupported fixes restore earlier verification.
    let mut unsupported = mutation.clone();
    unsupported.commands[0].id = "not-launched".into();
    let preparation = ledger.prepare(&unsupported, &unsupported.commands[0])?;
    assert!(frozen.validate(&ledger).is_err());
    assert_eq!(ledger.pending(Some(&before))?.len(), 2);
    ledger.retire_unlaunched(preparation)?;
    assert!(ledger.pending(Some(&before))?.is_empty());
    unsupported.commands[0].id = "unsupported".into();
    let preparation = ledger.prepare(&unsupported, &unsupported.commands[0])?;
    let launched = ledger.launch(preparation)?;
    let mut events = adapter::events::FormatSink::new(
        adapter::events::FormatOperation::Apply,
        adapter::PathSelection::default(),
    )?;
    events.accept_line(r#"{"event":"unsupported"}"#)?;
    unsupported.commands[0].result = Some(adapter::AdapterResult::Format(events.finish(
        Some(adapter::RunExit {
            success: true,
            code: Some(0),
        }),
        false,
    )));
    ledger.retire_unsupported(launched, &unsupported.commands[0], &before, &before)?;
    assert!(ledger.pending(Some(&before))?.is_empty());
    assert_eq!(
        verification::VerificationRequirements::open(data.path(), workspace.path(), "checks-test")?
            .pending(Some(&before))?
            .len(),
        0
    );
    // Unsupported output does not excuse an actual source change.
    unsupported.commands[0].id = "unsupported-with-edits".into();
    let preparation = ledger.prepare(&unsupported, &unsupported.commands[0])?;
    let launched = ledger.launch(preparation)?;
    let mut edited = before.clone();
    edited.file.push(crate::checkpoint::CheckpointFile {
        path: "edited.rs".into(),
        object_id: "changed".into(),
        mode: 0o100644,
    });
    ledger.retire_unsupported(launched, &unsupported.commands[0], &before, &edited)?;
    assert_eq!(ledger.pending(Some(&before))?.len(), 2);
    let capture = ledger.capture();
    ledger.observe(
        capture,
        &verification,
        &verification.commands[0],
        &before,
        &before,
    )?;
    assert!(ledger.pending(Some(&before))?.is_empty());
    let mut changed_config = scheduled.context.snapshot.clone();
    changed_config
        .helper_digest
        .insert("ignored-helper.nu".into(), "replacement".into());
    assert_eq!(
        ledger.pending_for_config(&before, &changed_config)?.len(),
        2
    );
    changed_config.helper_digest.clear();
    changed_config.config.definitions.clear();
    assert_eq!(
        ledger.pending_for_config(&before, &changed_config)?.len(),
        2
    );
    let mut changed = before.clone();
    changed.file.push(crate::checkpoint::CheckpointFile {
        path: "changed.rs".into(),
        object_id: "new-content".into(),
        mode: 0o100644,
    });
    assert_eq!(ledger.pending(Some(&changed))?.len(), 2);
    // A fix admitted after verification captured its generation cannot be cleared by that run.
    let capture = ledger.capture();
    mutation.commands[0].id = "fix-2".into();
    ledger.prepare(&mutation, &mutation.commands[0])?;
    ledger.observe(
        capture,
        &verification,
        &verification.commands[0],
        &before,
        &before,
    )?;
    assert_eq!(ledger.pending(Some(&before))?.len(), 1);
    let capture = ledger.capture();
    verification
        .context
        .as_mut()
        .unwrap()
        .snapshot
        .helper_digest
        .insert("format.nu".into(), "changed".into());
    ledger.observe(
        capture,
        &verification,
        &verification.commands[0],
        &before,
        &before,
    )?;
    assert_eq!(ledger.pending(Some(&before))?.len(), 1);
    let path = data.path().join("verification/requirements.json");
    let saved = std::fs::read(&path)?;
    std::fs::remove_file(&path)?;
    std::fs::create_dir(&path)?;
    mutation.commands[0].id = "fix-storage-failure".into();
    assert!(ledger.prepare(&mutation, &mutation.commands[0]).is_err());
    assert_eq!(ledger.pending(Some(&before))?.len(), 1);
    std::fs::remove_dir(&path)?;
    std::fs::write(&path, saved)?;
    drop(ledger);
    let ledger =
        verification::VerificationRequirements::open(data.path(), workspace.path(), "checks-test")?;
    assert_eq!(ledger.pending(Some(&before))?.len(), 1);
    Ok(())
}

/// Freeze every task preference and reject stale discovery admission, including unchanged enablement.
#[test]
fn discovery_inputs_reject_changed_task_preferences_and_missing_retained_settings() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    save(workspace.path(), vec![definition("check", "unused")]);
    let runner = make_runner(workspace.path(), data.path());
    let mut overrides = CheckOverrides::default();
    overrides.enabled.insert("check".into(), true);
    overrides.auto.insert("check".into(), false);
    runner.set_overrides(Some("task"), overrides.clone())?;
    let run = runner.prepare_request(
        runner.session_owner(),
        RunRequest::All,
        Some("task".into()),
        Default::default(),
        "test".into(),
    )?;
    let captured = super::discovery::DiscoveryInputs::for_run(&runner, &run)?;
    captured.validate(&runner)?;
    assert_eq!(run.context.task_overrides, overrides);
    let mut encoded = serde_json::to_value(run.clone().into_record())?;
    encoded["context"]
        .as_object_mut()
        .unwrap()
        .remove("task_overrides");
    assert!(serde_json::from_value::<CheckRun>(encoded).is_err());
    overrides.auto.insert("check".into(), true);
    runner.set_overrides(Some("task"), overrides)?;
    assert!(captured.validate(&runner).is_err());
    assert!(super::discovery::DiscoveryInputs::for_run(&runner, &run).is_err());
    Ok(())
}

/// Compare saved warning inputs against their original task, independently of conversation edits.
#[tokio::test]
async fn submission_warnings_compare_the_captured_preference_namespace() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    save(workspace.path(), vec![definition("check", "unused")]);
    let runner = make_runner(workspace.path(), data.path());
    let mut design = crate::plan::DeclarationDesign::default();
    let snapshot = runner.changes.capture(&runner.session_id).await?;
    design.submission_warnings = Some(SubmissionWarnings::new(
        &design,
        1,
        snapshot.id,
        discovery::DiscoveryInputs::capture(&runner, Some("planned-task"))?,
        Vec::new(),
    )?);
    let restored = serde_json::from_slice(&serde_json::to_vec(&design)?)?;
    assert!(matches!(
        runner.submission_warning_status(&restored).await,
        Some(SubmissionWarningStatus::Matched)
    ));
    let mut preferences = CheckOverrides::default();
    preferences.auto.insert("check".into(), false);
    runner.set_overrides(None, preferences.clone())?;
    assert!(matches!(
        runner.submission_warning_status(&restored).await,
        Some(SubmissionWarningStatus::Matched)
    ));
    runner.set_overrides(Some("planned-task"), preferences)?;
    assert!(
        matches!(runner.submission_warning_status(&restored).await, Some(SubmissionWarningStatus::Stale { reason }) if reason.contains("task settings changed"))
    );
    assert!(runner.history()?.is_empty());
    Ok(())
}

/// Restore enablement without losing the tool identity captured by an earlier fix.
#[test]
fn verification_identity_survives_policy_changes_but_rejects_tool_changes() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    let mut check = definition("format", "unused");
    check.category = CheckCategory::Format;
    check.enabled = false;
    check.runner = CheckRunner::Adapter(adapter::AdapterConfig::Script(
        adapter::ScriptAdapterConfig {
            command: vec!["nu".into(), "format.nu".into()],
        },
    ));
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let mut run = runner.prepare(
        ConfigSnapshot::load(workspace.path())?,
        CheckAdmission {
            owner: runner.session_owner(),
            request: RunRequest::All,
            overrides: [("format".into(), true)].into(),
            task_overrides: CheckOverrides {
                enabled: [("format".into(), true)].into(),
                ..Default::default()
            },
            files: Some(Vec::new()),
            has_scope: true,
            shell: Default::default(),
            trigger: "test".into(),
            task_id: Some("task".into()),
            revision: 0,
        },
    )?;
    let mut mutation = run.clone().into_record();
    mutation.commands[0].context.as_mut().unwrap().purpose = CheckPurpose::Prepare;
    let snapshot = crate::checkpoint::CheckpointRecord {
        id: "source".into(),
        session_id: "checks-test".into(),
        workspace: workspace.path().to_string_lossy().into_owned(),
        head: "UNBORN".into(),
        tree: None,
        file: Vec::new(),
        deleted: Vec::new(),
        checkout: Default::default(),
        excluded: Default::default(),
        created_at_ms: 0,
    };
    // A baseline owner is source provenance, not the namespace of ad hoc preferences.
    runner.changes.begin("baseline-owner".into(), &snapshot)?;
    runner.set_overrides(None, run.context.task_overrides.clone())?;
    let ad_hoc = runner.prepare_request(
        runner.session_owner(),
        RunRequest::All,
        None,
        Default::default(),
        "ad hoc".into(),
    )?;
    assert!(ad_hoc.context.task_id.is_none());
    assert_eq!(ad_hoc.context.task_overrides, run.context.task_overrides);
    super::discovery::DiscoveryInputs::for_run(&runner, &ad_hoc)?;
    let mut changed_preferences = run.context.task_overrides.clone();
    changed_preferences.auto.insert("format".into(), false);
    runner.set_overrides(None, changed_preferences)?;
    assert!(super::discovery::DiscoveryInputs::for_run(&runner, &ad_hoc).is_err());
    runner
        .verification
        .lock()
        .unwrap()
        .prepare(&mutation, &mutation.commands[0])?;
    let mut ledger =
        verification::VerificationRequirements::open(data.path(), workspace.path(), "checks-test")?;
    let mut verification = run.clone().into_record();
    verification.commands[0].state = CheckState::Passed;
    ledger.observe(
        ledger.capture(),
        &verification,
        &verification.commands[0],
        &snapshot,
        &snapshot,
    )?;
    assert_eq!(ledger.pending(Some(&snapshot))?.len(), 1);
    assert!(
        ledger
            .require_in_run(&mut run, &snapshot)
            .unwrap_err()
            .to_string()
            .contains("disabled")
    );

    // Whole-check verification overrides these policies but still uses the original tool.
    let definition = &mut run.context.snapshot.config.definitions[0];
    definition.enabled = true;
    definition.auto = false;
    definition.scope = CheckScope::Paths;
    definition.trigger = CheckTrigger::Manual;
    definition.name = "Renamed display label".into();
    definition.area = "Another display group".into();
    ledger.require_in_run(&mut run, &snapshot)?;
    assert_eq!(run.commands[0].context.selection, CheckSelection::All);
    verification = run.clone().into_record();
    verification.commands[0].state = CheckState::Passed;
    // Every execution field participates even when a malformed restored run claims success.
    let edits: [fn(&mut CheckDefinition); 5] = [
        |definition| definition.id = "other".into(),
        |definition| definition.category = CheckCategory::Lint,
        |definition| definition.directory = "elsewhere".into(),
        |definition| {
            definition.runner = CheckRunner::Command(CheckOperation {
                prepare: false,
                command: "other".into(),
                args: None,
                inputs: None,
            })
        },
        |definition| definition.depends_on = vec!["other".into()],
    ];
    for edit in edits {
        let mut changed = verification.clone();
        edit(
            &mut changed
                .context
                .as_mut()
                .unwrap()
                .snapshot
                .config
                .definitions[0],
        );
        if changed
            .context
            .as_ref()
            .unwrap()
            .snapshot
            .config
            .definitions[0]
            .id
            == "other"
        {
            changed.commands[0].context.as_mut().unwrap().definition_id = "other".into();
        }
        ledger.observe(
            ledger.capture(),
            &changed,
            &changed.commands[0],
            &snapshot,
            &snapshot,
        )?;
        assert_eq!(ledger.pending(Some(&snapshot))?.len(), 1);
        assert!(
            ledger
                .require_in_run(&mut changed.try_into()?, &snapshot)
                .is_err()
        );
    }
    let mut disabled = verification.clone();
    disabled
        .context
        .as_mut()
        .unwrap()
        .overrides
        .insert("format".into(), false);
    ledger.observe(
        ledger.capture(),
        &disabled,
        &disabled.commands[0],
        &snapshot,
        &snapshot,
    )?;
    assert_eq!(ledger.pending(Some(&snapshot))?.len(), 1);
    ledger.observe(
        ledger.capture(),
        &verification,
        &verification.commands[0],
        &snapshot,
        &snapshot,
    )?;
    assert!(ledger.pending(Some(&snapshot))?.is_empty());
    let mut config = run.context.snapshot.clone();
    assert!(ledger.pending_for_config(&snapshot, &config)?.is_empty());
    config.config.definitions[0].enabled = false;
    assert_eq!(ledger.pending_for_config(&snapshot, &config)?.len(), 1);
    Ok(())
}

/// Corrupt, missing, or mismatched ledgers cannot silently turn into empty requirements.
#[test]
fn verification_ledger_rejects_invalid_restore_without_resetting() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    let ledger =
        verification::VerificationRequirements::open(data.path(), workspace.path(), "session")?;
    assert!(ledger.pending(None)?.is_empty());
    let path = data.path().join("verification/requirements.json");
    let original = std::fs::read(&path)?;
    assert!(
        verification::VerificationRequirements::open(data.path(), workspace.path(), "different")
            .is_err()
    );
    let mut wrong: serde_json::Value = serde_json::from_slice(&original)?;
    let version = wrong["version"].as_u64().unwrap();
    for incompatible in [version - 1, version + 1] {
        wrong["version"] = incompatible.into();
        std::fs::write(&path, serde_json::to_vec(&wrong)?)?;
        assert!(
            verification::VerificationRequirements::open(data.path(), workspace.path(), "session")
                .is_err()
        );
        assert_eq!(std::fs::read(&path)?, serde_json::to_vec(&wrong)?);
    }
    std::fs::write(&path, b"{")?;
    assert!(
        verification::VerificationRequirements::open(data.path(), workspace.path(), "session")
            .is_err()
    );
    std::fs::remove_file(&path)?;
    assert!(
        verification::VerificationRequirements::open(data.path(), workspace.path(), "session")
            .is_err()
    );
    std::fs::write(&path, &original)?;
    assert!(
        verification::VerificationRequirements::open(data.path(), workspace.path(), "session")
            .is_ok()
    );
    Ok(())
}

/// Mandatory checks include read-only prerequisites, but never launch a source-changing prerequisite.
#[test]
fn required_verification_selects_readonly_prerequisites() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    let mut build = definition("build", "build");
    build.category = CheckCategory::Build;
    build.runner = CheckRunner::Adapter(adapter::AdapterConfig::Script(
        adapter::ScriptAdapterConfig {
            command: vec!["nu".into(), "build.nu".into()],
        },
    ));
    let mut format = definition("format", "unused");
    format.category = CheckCategory::Format;
    format.depends_on = vec!["build".into()];
    format.runner = CheckRunner::Adapter(adapter::AdapterConfig::Script(
        adapter::ScriptAdapterConfig {
            command: vec!["nu".into(), "format.nu".into()],
        },
    ));
    save(workspace.path(), vec![build, format]);
    let runner = make_runner(workspace.path(), data.path());
    let mut run = runner.prepare(
        ConfigSnapshot::load(workspace.path())?,
        CheckAdmission {
            owner: runner.session_owner(),
            request: RunRequest::All,
            overrides: Default::default(),
            task_overrides: Default::default(),
            files: Some(Vec::new()),
            has_scope: true,
            shell: Default::default(),
            trigger: "test".into(),
            task_id: None,
            revision: 0,
        },
    )?;
    let index = run
        .commands
        .iter()
        .position(|command| command.context.definition_id == "format")
        .unwrap();
    let mut mutation = run.clone().into_record();
    mutation.commands[index].context.as_mut().unwrap().purpose = CheckPurpose::Prepare;
    let snapshot = crate::checkpoint::CheckpointRecord {
        id: "source".into(),
        session_id: "checks-test".into(),
        workspace: workspace.path().to_string_lossy().into_owned(),
        head: "UNBORN".into(),
        tree: None,
        file: Vec::new(),
        deleted: Vec::new(),
        checkout: Default::default(),
        excluded: Default::default(),
        created_at_ms: 0,
    };
    let mut ledger = runner.verification.lock().unwrap();
    ledger.prepare(&mutation, &mutation.commands[index])?;
    for command in &mut run.commands {
        command.selected = false;
        command.state = CheckState::Skipped;
    }
    ledger.require_in_run(&mut run, &snapshot)?;
    assert!(run.commands.iter().all(|command| command.selected
        && command.state == CheckState::Pending
        && command.context.selection == CheckSelection::All));
    let build = run
        .commands
        .iter_mut()
        .find(|command| command.context.definition_id == "build")
        .unwrap();
    build.context.purpose = CheckPurpose::Prepare;
    assert!(
        ledger
            .require_in_run(&mut run, &snapshot)
            .unwrap_err()
            .to_string()
            .contains("changes source")
    );
    Ok(())
}

/// A torn run record is skipped rather than failing the supervisor that session open builds.
#[test]
fn undecodable_check_record_does_not_block_session_open() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    std::fs::create_dir_all(data.path().join("torn-run"))?;
    std::fs::write(data.path().join("torn-run/record.json"), br#"{"version":"#)?;
    let runner = make_runner(workspace.path(), data.path());
    assert!(runner.history()?.is_empty());
    assert!(CheckSupervisor::saved_history(data.path())?.is_empty());
    assert!(data.path().join("torn-run/record.json").exists());
    Ok(())
}

/// Discovery and execution share admission, without adding synthetic history records.
#[test]
fn discovery_reservation_excludes_runs_and_releases_without_history() -> Result<()> {
    use super::adapter::DiscoverMode::{Exact, Static};
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    save(workspace.path(), vec![definition("unit", "exit 0")]);
    let runner = make_runner(workspace.path(), data.path());
    let peer_data = tempfile::tempdir()?;
    let peer = make_runner(workspace.path(), peer_data.path());
    let preparation = reservation::CheckReservation::acquire(
        &(workspace.path()).join("harness-state"),
        workspace.path(),
        CheckPurpose::Prepare,
    )?;
    assert!(
        runner
            .discovery_reserve("blocked-by-preparation", Exact)
            .is_err()
    );
    assert!(
        peer.discovery_reserve("different-data-root", Exact)
            .is_err()
    );
    let static_discovery = runner.discovery_reserve("static-during-preparation", Static)?;
    assert!(runner.discovery_reserve("other-static", Static).is_err());
    assert!(
        runner
            .discovery_reserve("exact-during-static", Exact)
            .is_err()
    );
    drop(static_discovery);
    assert!(runner.history()?.is_empty());
    drop(preparation);
    let peer_reader = peer.discovery_reserve("peer-reader", Exact)?;
    let reservation = runner.discovery_reserve("first-discovery", Exact)?;
    assert!(
        reservation::CheckReservation::acquire(
            &(workspace.path()).join("harness-state"),
            workspace.path(),
            CheckPurpose::Prepare,
        )
        .is_err()
    );
    assert!(
        runner
            .discovery_reserve("second-discovery", Static)
            .is_err()
    );
    let run = runner.prepare(
        ConfigSnapshot::load(workspace.path())?,
        CheckAdmission {
            owner: runner.session_owner(),
            request: RunRequest::Selected(SelectedChecks {
                ids: vec!["unit".into()],
                selections: [("unit".into(), CheckSelection::All)].into(),
                ..Default::default()
            }),
            overrides: Default::default(),
            task_overrides: Default::default(),
            files: Some(Vec::new()),
            has_scope: true,
            shell: Default::default(),
            trigger: "test discovery exclusion".into(),
            task_id: None,
            revision: 0,
        },
    )?;
    let failure = runner.launch_owned(run, None, None).unwrap_err();
    assert!(failure.to_string().contains("another check operation"));
    assert!(runner.history()?.is_empty());
    assert!(runner.active()?.is_none());
    drop(reservation);
    drop(peer_reader);
    let _next = runner.discovery_reserve("next-discovery", Exact)?;
    Ok(())
}

/// Replacing a downstream tool cannot reuse a script pass even when the recorded signature matches.
#[test]
fn script_evidence_does_not_reuse_an_undeclared_tool_identity() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
    let tool = data.path().join("external-tool");
    std::fs::write(&tool, "old tool")?;
    let mut check = definition("unit", "unused");
    check.category = CheckCategory::Test;
    check.runner = CheckRunner::Adapter(adapter::AdapterConfig::Script(
        adapter::ScriptAdapterConfig {
            command: vec!["nu".into(), "--stdin".into(), "runner.nu".into()],
        },
    ));
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let mut run = runner
        .prepare(
            ConfigSnapshot::load(workspace.path())?,
            CheckAdmission {
                owner: runner.session_owner(),
                request: RunRequest::Selected(SelectedChecks {
                    ids: vec!["unit".into()],
                    selections: [("unit".into(), CheckSelection::All)].into(),
                    ..Default::default()
                }),
                overrides: Default::default(),
                task_overrides: Default::default(),
                files: Some(Vec::new()),
                has_scope: true,
                shell: Default::default(),
                trigger: "evidence test".into(),
                task_id: Some("task".into()),
                revision: 0,
            },
        )?
        .into_record();
    run.commands[0].state = CheckState::Passed;
    let checkpoint = crate::checkpoint::CheckpointRecord {
        id: "snapshot".into(),
        session_id: "checks-test".into(),
        workspace: workspace.path().to_string_lossy().into_owned(),
        head: "UNBORN".into(),
        tree: None,
        file: Vec::new(),
        deleted: Vec::new(),
        checkout: Default::default(),
        excluded: Default::default(),
        created_at_ms: 0,
    };
    let evidence = evidence::CheckEvidence {
        checkpoint: checkpoint.clone(),
        signature: evidence::signature(&run, &run.commands[0])?,
        source_identity: "same source snapshot".into(),
    };
    std::fs::write(tool, "replacement tool reporting the same version")?;
    assert_eq!(
        evidence.signature,
        evidence::signature(&run, &run.commands[0])?
    );
    assert!(!evidence.covers(&run, &run.commands[0], &checkpoint)?);
    assert!(evidence.matches_inputs(&run, &run.commands[0], &checkpoint)?);
    assert_eq!(run.commands[0].state, CheckState::Passed);

    // Invocation purpose, rather than the shared definition, owns evidence semantics.
    let before = checkpoint.clone();
    let mut after = checkpoint.clone();
    after.id = "after-preparation".into();
    let verified =
        evidence::CheckEvidence::capture(&run, &run.commands[0], before.clone(), &after)?
            .expect("valid verification inputs produce evidence");
    assert_eq!(verified.checkpoint.id, before.id);
    run.commands[0].context.as_mut().unwrap().purpose = CheckPurpose::Prepare;
    let prepared = evidence::CheckEvidence::capture(&run, &run.commands[0], before, &after)?
        .expect("valid preparation inputs produce evidence");
    assert_eq!(prepared.checkpoint.id, after.id);
    assert_ne!(verified.signature, prepared.signature);
    assert!(!evidence.matches_inputs(&run, &run.commands[0], &checkpoint)?);
    let restored: CheckRun = serde_json::from_slice(&serde_json::to_vec(&run)?)?;
    assert_eq!(
        restored.commands[0].context.as_ref().unwrap().purpose,
        CheckPurpose::Prepare
    );
    run.context
        .as_mut()
        .unwrap()
        .snapshot
        .config
        .definitions
        .clear();
    assert!(evidence::signature(&run, &run.commands[0]).is_err());
    Ok(())
}

#[tokio::test]
async fn revoked_callback_cannot_write_check_configuration() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    crate::storage::layout::prepare(data.path())?;
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
