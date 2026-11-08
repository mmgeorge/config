use super::*;
use crate::test_support::git;

#[test]
fn diagnostic_retains_exit_context_and_final_error_within_utf8_budget() {
    let input = format!("Git exited 1: {}\nfinal hook error", "界".repeat(2000));
    let diagnostic = bounded_diagnostic(&input);
    assert!(diagnostic.len() <= 2048);
    assert!(diagnostic.starts_with("Git exited 1:"));
    assert!(diagnostic.contains("[Diagnostic truncated]"));
    assert!(diagnostic.ends_with("final hook error"));
    assert_eq!(bounded_diagnostic("short error"), "short error");
}

async fn fixture() -> (
    tempfile::TempDir,
    Arc<RepositoryStore>,
    Arc<RepositoryState>,
    GitWriteService,
) {
    let directory = tempfile::tempdir().unwrap();
    git(directory.path(), &["init", "--initial-branch=main"]);
    git(directory.path(), &["config", "user.name", "Forge Test"]);
    git(
        directory.path(),
        &["config", "user.email", "forge@example.invalid"],
    );
    git(directory.path(), &["config", "core.autocrlf", "false"]);
    std::fs::write(directory.path().join("sample.txt"), "original\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "initial"]);
    let store = Arc::new(RepositoryStore::default());
    let repository = store
        .open(directory.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let service = GitWriteService::new(Arc::clone(&store));
    (directory, store, repository, service)
}

#[tokio::test]
async fn repository_config_write_retains_capture_and_rejects_external_edit() {
    let (directory, _, repository, service) = fixture().await;
    let replacement = b"{\"issues\":[42]}\n".to_vec();
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::UpdateRepositoryConfig {
            expected: None,
            replacement: replacement.clone(),
        },
    )
    .await;
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_eq!(outcome.affected, vec![path(".forge.json")]);
    let intent = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::UpdateRepositoryConfig {
                expected: Some(replacement),
                replacement: b"{}\n".to_vec(),
            },
        )
        .await
        .unwrap();
    std::fs::write(
        directory.path().join(".forge.json"),
        b"{\"external\":true}\n",
    )
    .unwrap();
    let outcome = service.submit(intent).unwrap().finish().await.unwrap();
    assert_eq!(outcome.target[0].completion, TargetCompletion::Rejected);
    assert_eq!(
        std::fs::read(directory.path().join(".forge.json")).unwrap(),
        b"{\"external\":true}\n"
    );
    service.acknowledge(outcome.operation).unwrap();
}

fn path(name: &str) -> RepositoryPath {
    RepositoryPath::new(name.as_bytes().to_vec()).unwrap()
}

async fn run(
    service: &GitWriteService,
    repository: &Arc<RepositoryState>,
    action: GitWriteAction,
) -> Arc<WriteOutcome> {
    let intent = service
        .prepare(Arc::clone(repository), action)
        .await
        .unwrap();
    let outcome = service.submit(intent).unwrap().finish().await.unwrap();
    assert!(service.acknowledge(outcome.operation).is_some());
    outcome
}

#[tokio::test]
async fn whole_file_stage_uses_current_source_after_queue_handoff() {
    let (directory, store, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "captured\n").unwrap();
    let blocker = store
        .writes
        .admit(
            vec![MutationScope::Index(
                repository.identity.worktree.clone().unwrap(),
            )],
            0,
        )
        .unwrap();
    let guard = store.writes.start(blocker).unwrap().unwrap();
    let observed = repository.observe(&store).await.unwrap();
    let intent = service
        .reserve(
            Arc::clone(&repository),
            GitWriteAction::Stage {
                path: vec![path("sample.txt")],
            },
        )
        .unwrap();
    intent
        .validate_observed_sources(&observed.head, &observed.path)
        .unwrap();
    let ticket = service.submit(intent).unwrap();
    std::fs::write(directory.path().join("sample.txt"), "new text\n").unwrap();
    guard.finish(OperationCompletion::Completed).unwrap();
    store.writes.take_receipt(blocker);
    let outcome = ticket.finish().await.unwrap();
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"new text\n"
    );
    service.acknowledge(outcome.operation).unwrap();
}

#[tokio::test]
async fn preparation_waits_for_earlier_staging_before_capturing_commit_index() {
    let (directory, store, repository, service) = fixture().await;
    std::fs::write(
        directory.path().join("sample.txt"),
        "staged before commit capture\n",
    )
    .unwrap();
    let staged = service
        .reserve(
            Arc::clone(&repository),
            GitWriteAction::Stage {
                path: vec![path("sample.txt")],
            },
        )
        .unwrap();
    let preparing = service.prepare(
        Arc::clone(&repository),
        GitWriteAction::Commit {
            message: "commit after optimistic stage".into(),
        },
    );
    tokio::pin!(preparing);
    let waiting = std::future::poll_fn(|context| {
        std::task::Poll::Ready(std::future::Future::poll(preparing.as_mut(), context).is_pending())
    })
    .await;
    assert!(
        waiting,
        "commit captured the index before earlier staging completed"
    );
    let outcome = service.submit(staged).unwrap().finish().await.unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    service.acknowledge(outcome.operation).unwrap();
    let intent = preparing.await.unwrap();
    let committed = service.submit(intent).unwrap().finish().await.unwrap();
    assert!(
        committed
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{committed:?}"
    );
    service.acknowledge(committed.operation).unwrap();
    assert_eq!(
        git(directory.path(), &["show", "HEAD:sample.txt"]),
        b"staged before commit capture\n"
    );
    assert_eq!(store.writes.usage().operations, 0);
}

#[tokio::test]
async fn unverified_outcome_blocks_writes_until_read_only_reconciliation() {
    let (directory, store, repository, service) = fixture().await;
    let scope = MutationScope::Index(repository.identity.worktree.clone().unwrap());
    let operation = store.writes.admit(vec![scope.clone()], 1).unwrap();
    store
        .writes
        .start(operation)
        .unwrap()
        .unwrap()
        .finish_quarantined(OperationCompletion::Uncertain)
        .unwrap();
    assert!(store.writes.take_receipt(operation).is_none());
    assert!(
        service
            .reserve(
                Arc::clone(&repository),
                GitWriteAction::Stage {
                    path: vec![path("sample.txt")]
                }
            )
            .is_err()
    );
    service.reconcile(&repository).await.unwrap();
    assert!(store.writes.quarantined(&[scope]).is_empty());
    std::fs::write(
        directory.path().join("sample.txt"),
        "after reconciliation\n",
    )
    .unwrap();
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Stage {
            path: vec![path("sample.txt")],
        },
    )
    .await;
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"after reconciliation\n"
    );
}

#[tokio::test]
async fn unrelated_index_change_does_not_reject_selected_path() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "changed\n").unwrap();
    let intent = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::Stage {
                path: vec![path("sample.txt")],
            },
        )
        .await
        .unwrap();
    std::fs::write(directory.path().join("other.txt"), "other\n").unwrap();
    git(directory.path(), &["add", "other.txt"]);
    let outcome = service.submit(intent).unwrap().finish().await.unwrap();
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_eq!(git(directory.path(), &["show", ":other.txt"]), b"other\n");
    service.acknowledge(outcome.operation).unwrap();
}

#[tokio::test]
async fn branch_write_invalidates_linked_worktrees() {
    let (directory, store, repository, service) = fixture().await;
    let linked = directory.path().join("linked");
    git(
        directory.path(),
        &["worktree", "add", "-b", "linked", linked.to_str().unwrap()],
    );
    let sibling = store.open(linked).await.unwrap().unwrap();
    let before = sibling.generation();
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::CreateBranch {
            name: "feature".into(),
        },
    )
    .await;
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_ne!(sibling.generation(), before);
}

#[tokio::test]
async fn dropped_unsubmitted_intent_releases_queue_and_input() {
    let (_directory, store, repository, service) = fixture().await;
    let intent = service
        .prepare(
            repository,
            GitWriteAction::Stage {
                path: vec![path("sample.txt")],
            },
        )
        .await
        .unwrap();
    assert_eq!(store.writes.usage().operations, 1);
    drop(intent);
    assert_eq!(store.writes.usage().operations, 0);
    assert_eq!(store.writes.usage().input_bytes, 0);
}

#[tokio::test]
async fn literal_pathspec_does_not_stage_neighbors() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("[file].txt"), "literal\n").unwrap();
    std::fs::write(directory.path().join("f.txt"), "neighbor\n").unwrap();
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Stage {
            path: vec![path("[file].txt")],
        },
    )
    .await;
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_eq!(
        git(directory.path(), &["diff", "--cached", "--name-only"]),
        b"[file].txt\n"
    );
}

#[tokio::test]
async fn untracked_discard_deletes_only_captured_file() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("untracked.txt"), "discard\n").unwrap();
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Discard {
            source: DiscardSource::Index,
            path: vec![path("untracked.txt")],
        },
    )
    .await;
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert!(!directory.path().join("untracked.txt").exists());
    assert!(directory.path().join("sample.txt").exists());
}

#[test]
fn batch_rejects_duplicate_global_and_nested_actions() {
    let stage = || GitWriteAction::Stage {
        path: vec![path("sample.txt")],
    };
    assert!(
        GitWriteAction::Batch {
            action: vec![stage(), stage()]
        }
        .validate()
        .is_err()
    );
    assert!(
        GitWriteAction::Batch {
            action: vec![GitWriteAction::Push]
        }
        .validate()
        .is_err()
    );
    assert!(
        GitWriteAction::Batch {
            action: vec![GitWriteAction::Batch {
                action: vec![stage()]
            }]
        }
        .validate()
        .is_err()
    );
    assert_eq!(OperationId::from_value(0).unwrap().value(), 0);
    assert!(OperationId::from_value(9_007_199_254_740_992).is_err());
}

#[tokio::test]
async fn progress_sequence_spans_commands_with_independent_streams() {
    let (_directory, _, repository, service) = fixture().await;
    let mut intent = service
        .prepare(
            repository,
            GitWriteAction::Stage {
                path: vec![path("sample.txt")],
            },
        )
        .await
        .unwrap();
    let collected = Arc::new(Mutex::new(Vec::new()));
    let sink = Arc::clone(&collected);
    intent.set_progress(Arc::new(move |chunk| {
        sink.lock().unwrap().push((chunk.stream, chunk.sequence));
        Ok(())
    }));
    for stream in [
        CommandStream::Stdout,
        CommandStream::Stderr,
        CommandStream::Stdout,
        CommandStream::Stderr,
    ] {
        intent.progress.as_ref().unwrap()(crate::command::CommandProgress {
            stream,
            sequence: 0,
            bytes: vec![b'x'],
        })
        .unwrap();
    }
    assert_eq!(
        *collected.lock().unwrap(),
        vec![
            (CommandStream::Stdout, 0),
            (CommandStream::Stderr, 0),
            (CommandStream::Stdout, 1),
            (CommandStream::Stderr, 1)
        ]
    );
}

#[tokio::test]
async fn failed_path_group_reconciles_every_target_without_claiming_partial_success() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "changed\n").unwrap();
    std::fs::write(directory.path().join(".gitignore"), "ignored.txt\n").unwrap();
    std::fs::write(directory.path().join("ignored.txt"), "ignored\n").unwrap();
    std::fs::write(directory.path().join("last.txt"), "last\n").unwrap();
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Stage {
            path: vec![path("sample.txt"), path("ignored.txt"), path("last.txt")],
        },
    )
    .await;
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::OutcomeUnknown)
    );
    assert!(outcome.settled.is_some());
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"changed\n"
    );
    assert_eq!(git(directory.path(), &["show", ":last.txt"]), b"last\n");
}

#[tokio::test]
async fn cancelled_waiter_keeps_recoverable_outcome_until_acknowledged() {
    let (directory, store, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "changed\n").unwrap();
    let blocker = store
        .writes
        .admit(
            vec![MutationScope::Index(
                repository.identity.worktree.clone().unwrap(),
            )],
            0,
        )
        .unwrap();
    let guard = store.writes.start(blocker).unwrap().unwrap();
    let intent = service
        .reserve(
            repository,
            GitWriteAction::Stage {
                path: vec![path("sample.txt")],
            },
        )
        .unwrap();
    let ticket = service.submit(intent).unwrap();
    let operation = ticket.operation;
    drop(ticket);
    guard.finish(OperationCompletion::Completed).unwrap();
    store.writes.take_receipt(blocker);
    let outcome = async {
        loop {
            if let Some(outcome) = service.outcome(operation) {
                break outcome;
            }
            tokio::task::yield_now().await;
        }
    }
    .await;
    assert_eq!(outcome.target[0].completion, TargetCompletion::NotStarted);
    assert_eq!(store.writes.usage().operations, 1);
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"original\n"
    );
    assert_eq!(
        service.acknowledge(operation),
        Some(OperationCompletion::CancelledBeforeStart)
    );
    assert_eq!(store.writes.usage().input_bytes, 0);
}

#[tokio::test]
async fn commit_with_large_summary_reports_completed() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "changed\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    let message = format!("test: {}", "large summary ".repeat(9000));
    let completed = run(
        &service,
        &repository,
        GitWriteAction::Commit {
            message: message.clone(),
        },
    )
    .await;
    assert_eq!(completed.target[0].completion, TargetCompletion::Completed);
    let recorded = Command::new("git")
        .arg("-C")
        .arg(directory.path())
        .args(["log", "-1", "--format=%s"])
        .output()
        .unwrap();
    assert!(recorded.status.success());
    assert_eq!(
        recorded.stdout,
        format!("{}\n", message.trim_end()).as_bytes()
    );
}

#[tokio::test]
async fn commit_captures_message_and_rejects_changed_staged_input() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "changed\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    let intent = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::Commit {
                message: "captured message\n\nbody\n".into(),
            },
        )
        .await
        .unwrap();
    std::fs::write(directory.path().join("sample.txt"), "new staged\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    let rejected = service.submit(intent).unwrap().finish().await.unwrap();
    assert_eq!(rejected.target[0].completion, TargetCompletion::Rejected);
    service.acknowledge(rejected.operation).unwrap();
    let completed = run(
        &service,
        &repository,
        GitWriteAction::Commit {
            message: "captured message\n\nbody\n".into(),
        },
    )
    .await;
    assert_eq!(completed.target[0].completion, TargetCompletion::Completed);
    assert_eq!(
        git(directory.path(), &["log", "-1", "--format=%B"]),
        b"captured message\n\nbody\n\n"
    );
}

#[tokio::test]
async fn consumer_settlement_failure_preserves_confirmed_git_completion() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "settled\n").unwrap();
    let mut intent = service
        .prepare(
            repository,
            GitWriteAction::Stage {
                path: vec![path("sample.txt")],
            },
        )
        .await
        .unwrap();
    intent
        .set_settlement_handler(Arc::new(|outcome| {
            assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
            anyhow::bail!("private metadata unavailable")
        }))
        .unwrap();
    let outcome = service.submit(intent).unwrap().finish().await.unwrap();
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert!(
        outcome
            .settlement_diagnostic
            .as_ref()
            .unwrap()
            .contains("private metadata unavailable")
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"settled\n"
    );
    service.acknowledge(outcome.operation).unwrap();
}

#[tokio::test]
async fn publish_branch_rejects_head_changed_before_prepare() {
    let (directory, _, repository, service) = fixture().await;
    let captured = String::from_utf8(git(directory.path(), &["rev-parse", "HEAD"]))
        .unwrap()
        .trim()
        .to_owned();
    git(
        directory.path(),
        &["commit", "--allow-empty", "-m", "after generation"],
    );
    let result = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::PublishBranch {
                name: "main".into(),
                expected_head: Some(captured),
            },
        )
        .await;
    let failure = match result {
        Ok(_) => panic!("stale generated HEAD was accepted"),
        Err(failure) => failure,
    };
    assert!(format!("{failure:#}").contains("publish HEAD changed after generation capture"));
}

#[tokio::test]
async fn whole_file_stage_accepts_large_sources_and_changes_after_preparation() {
    let (directory, _, repository, service) = fixture().await;
    let large = vec![b'x'; 9 * 1024 * 1024];
    std::fs::write(directory.path().join("large.bin"), &large).unwrap();
    let intent = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::Batch {
                action: vec![
                    GitWriteAction::Stage {
                        path: vec![path("large.bin")],
                    },
                    GitWriteAction::Stage {
                        path: vec![path("sample.txt")],
                    },
                ],
            },
        )
        .await
        .unwrap();
    std::fs::write(directory.path().join("sample.txt"), b"new index\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(directory.path().join("sample.txt"), b"execution contents\n").unwrap();
    let outcome = service.submit(intent).unwrap().finish().await.unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{outcome:?}"
    );
    let mut command = git_command(&repository).unwrap();
    command.args(["show", ":large.bin"]);
    let staged = tokio::task::spawn_blocking(move || {
        crate::command::read_command(
            &mut command,
            CommandLimits {
                stderr_bytes: 4096,
                timeout: Duration::from_secs(30),
            },
            || Ok(()),
        )
    })
    .await
    .unwrap()
    .unwrap();
    assert!(staged.status.success());
    assert_eq!(staged.stdout, large);
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"execution contents\n"
    );
    service.acknowledge(outcome.operation).unwrap();
}

fn scoped_patch_target(old: &str, new: &str, replacement: &str, reverse: bool) -> PatchTarget {
    use forge_diff::source::{Representation, SourceVersion};
    let source = |text: &str| {
        SourceVersion::new(text.as_bytes().to_vec(), Representation::GitCanonical).unwrap()
    };
    let analysis = compute_hunks(SourcePair {
        old: source(old),
        new: source(new),
    });
    let payload = compute_hunks(if reverse {
        SourcePair {
            old: source(replacement),
            new: source(new),
        }
    } else {
        SourcePair {
            old: source(old),
            new: source(replacement),
        }
    });
    let scoped = EditSelection::capture(
        &payload,
        &payload
            .hunks()
            .iter()
            .map(|hunk| hunk.id)
            .collect::<Vec<_>>(),
        if reverse {
            EditDirection::Reverse
        } else {
            EditDirection::Forward
        },
    )
    .unwrap();
    PatchTarget {
        path: path("sample.txt"),
        selected: analysis.hunks().iter().map(|hunk| hunk.id).collect(),
        analysis,
        scoped: Some(Box::new(scoped)),
    }
}

#[tokio::test]
async fn scoped_patch_rejects_foreign_baselines_and_unselected_raw_regions() {
    let (_, _, repository, service) = fixture().await;
    let old = "one\ntwo\nthree\nfour\nfive\n";
    let new = "ONE\ntwo\nthree\nfour\nFIVE\n";
    let mut target = scoped_patch_target(old, new, "one\ntwo\nthree\nfour\nFIVE\n", false);
    target.selected.truncate(1);
    let error = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::Patch {
                direction: PatchDirection::Stage,
                target,
            },
        )
        .await
        .err()
        .unwrap();
    assert!(error.to_string().contains("ownership anchor"), "{error:#}");
    let target = scoped_patch_target(old, new, "ONE\ntwo\nthree\nfour\nfive\n", false);
    let error = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::Patch {
                direction: PatchDirection::Discard,
                target,
            },
        )
        .await
        .err()
        .unwrap();
    assert!(error.to_string().contains("source changed"), "{error:#}");
}

#[tokio::test]
async fn scoped_discard_validates_excluded_source_bytes_before_writing() {
    let (directory, _, repository, service) = fixture().await;
    let old = "public_before\nprivate_before\n";
    let new = "public_after\nprivate_after\n";
    std::fs::write(directory.path().join("sample.txt"), old).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(directory.path().join("sample.txt"), new).unwrap();
    let target = scoped_patch_target(old, new, "public_before\nprivate_after\n", true);
    let intent = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::Patch {
                direction: PatchDirection::Discard,
                target,
            },
        )
        .await
        .unwrap();
    let external = "public_after\nprivate_external\n";
    std::fs::write(directory.path().join("sample.txt"), external).unwrap();
    let outcome = service.submit(intent).unwrap().finish().await.unwrap();
    assert_eq!(outcome.target[0].completion, TargetCompletion::Rejected);
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        external.as_bytes()
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        old.as_bytes()
    );
    service.acknowledge(outcome.operation).unwrap();
}
