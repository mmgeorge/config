use super::*;
use anyhow::Result;
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

fn definition(id: &str, command: &str) -> CheckDefinition {
    CheckDefinition {
        id: id.into(),
        name: id.into(),
        category: CheckCategory::Test,
        enabled: true,
        area: "Project".into(),
        trigger: CheckTrigger::AnyChange,
        scope: CheckScope::All,
        directory: ".".into(),
        all: CheckOperation {
            command: command.into(),
        },
        files: None,
        cases: None,
        depends_on: Vec::new(),
    }
}

fn save(workspace: &std::path::Path, definitions: Vec<CheckDefinition>) {
    std::fs::write(workspace.join(".forge.json"),serde_json::to_vec(&serde_json::json!({"branch_prefix":"feature","checks":{"version":2,"definitions":definitions}})).unwrap()).unwrap();
}

fn make_runner(workspace: &std::path::Path, data: &std::path::Path) -> Arc<CheckRunner> {
    let mut presentation = crate::buffer::session::SessionPresentation::new("checks-test".into());
    presentation.initialize(Vec::new()).unwrap();
    CheckRunner::open(
        workspace.into(),
        data.into(),
        Arc::new(Mutex::new(presentation)),
        crate::storage::objects::ObjectStore::open(data).unwrap(),
        Arc::new(forge_git::store::RepositoryStore::default()),
    )
    .unwrap()
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
            request: Default::default(),
            overrides: Default::default(),
            files: Some(vec![
                "src/nested/removed.rs".into(),
                "src/generated/ignored.rs".into(),
            ]),
            cases: None,
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
    let selected = request.selections(&config, &files, None, true)?;
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
            file: file.clone(),
            name: "case".into(),
        })
        .collect::<Vec<_>>();
    let selected = request.selections(&config, &files, Some(&cases), true)?;
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
    check.cases = Some(CheckOperation {command:"do { let request = ($in | from json); {version: 1, selection: $request.selection} | to json | save --force $request.receipt_path; 'selected case' | save --force selected.txt }".into()});
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let case_selection = CheckSelection::Cases {
        cases: vec![CheckCase {
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
            all: false,
            ids: vec!["unit".into()],
            selections: selection
                .map(|selection| BTreeMap::from([("unit".into(), selection)]))
                .unwrap_or_default(),
        };
        let prepared = runner.prepare(
            ConfigSnapshot::load(workspace.path())?,
            CheckAdmission {
                request,
                overrides: Default::default(),
                files: Some(vec!["src/source.rs".into()]),
                cases: inventory,
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
        RunRequest {
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
        command: "print 'no receipt'".into(),
    });
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let request = RunRequest {
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
    check.files = Some(CheckOperation {command:"do { let request = ($in | from json); {version: 1, selection: $request.selection} | to json | save --force $request.receipt_path; $request.selection.files | to json | save --force received.json }".into()});
    save(workspace.path(), vec![check]);
    let runner = make_runner(workspace.path(), data.path());
    let path = "src/name $var; [case].rs".to_owned();
    let selection = CheckSelection::Files {
        files: vec![path.clone()],
    };
    let prepared = runner.prepare_request(
        RunRequest {
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
    assert_eq!(
        serde_json::from_slice::<Vec<String>>(&std::fs::read(
            workspace.path().join("received.json")
        )?)?,
        vec![path]
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
    let helper = super::tools::CheckHelper {
        path: ".forge/checks/select.nu".into(),
        source: "let selected = $in | from json\nprint $selected\n".into(),
        expected_digest: None,
    };
    runner
        .configure(
            crate::checks::ConfigureChecks {
                expected_revision: revision,
                config: config.clone(),
                helpers: vec![helper],
            },
            None,
        )
        .await?;
    assert!(workspace.path().join(".forge/checks/select.nu").is_file());
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
                crate::checks::ConfigureChecks {
                    expected_revision: revision,
                    config,
                    helpers: vec![helper]
                },
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
    let runner = make_runner(workspace.path(), data.path());
    let catalog = runner.catalog(None)?;
    runner
        .configure(
            crate::checks::ConfigureChecks {
                expected_revision: catalog["revision"].as_str().unwrap().into(),
                config: CheckConfig {
                    version: 2,
                    definitions: Vec::new(),
                },
                helpers: Vec::new(),
            },
            None,
        )
        .await?;
    assert_eq!(
        super::config::read_document(workspace.path())?["branch_prefix"],
        "feature"
    );
    save(workspace.path(), vec![definition("unit", "exit 0")]);
    runner.set_overrides(
        Some("task-one"),
        super::tools::CheckOverrides {
            enabled: BTreeMap::from([("unit".into(), false)]),
            scopes: BTreeMap::new(),
        },
    )?;
    assert!(runner.overrides(Some("task-two"))?.enabled.is_empty());
    assert!(
        runner
            .prepare_request(
                RunRequest {
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
                crate::checks::ConfigureChecks {
                    expected_revision: "stale".into(),
                    config: CheckConfig {
                        version: 2,
                        definitions: Vec::new()
                    },
                    helpers: Vec::new()
                },
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
            crate::checks::ConfigureChecks {
                expected_revision: catalog["revision"].as_str().unwrap().into(),
                config: CheckConfig {
                    version: 2,
                    definitions: Vec::new(),
                },
                helpers: Vec::new(),
            },
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
        RunRequest {
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
        RunRequest {
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let second = runner.prepare_request(
        RunRequest {
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Test".into(),
    )?;
    let id = runner.start(first, None, None).await?;
    assert!(runner.start(second, None, None).await.is_err());
    runner.cancel(&id)?;
    assert_eq!(runner.active()?.as_deref(), Some(id.as_str()));
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        while runner.active()?.is_some() {
            runner.status(&id, 500).await?;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await??;
    assert_eq!(runner.status(&id, 0).await?.state, CheckState::Interrupted);
    Ok(())
}

#[tokio::test]
async fn provider_turn_cancellation_collects_its_background_checks() -> Result<()> {
    let workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    save(workspace.path(), vec![definition("slow", "sleep 5sec")]);
    let runner = make_runner(workspace.path(), data.path());
    let run = runner.prepare_request(
        RunRequest {
            all: true,
            ..Default::default()
        },
        None,
        crate::session::CheckShell::Nushell,
        "Agent requested".into(),
    )?;
    let cancelled = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let id = runner
        .start(run, None, Some(Arc::clone(&cancelled)))
        .await?;
    cancelled.store(true, std::sync::atomic::Ordering::Release);
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        while runner.active()?.is_some() {
            runner.status(&id, 500).await?;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await??;
    assert_eq!(runner.status(&id, 0).await?.state, CheckState::Interrupted);
    Ok(())
}
