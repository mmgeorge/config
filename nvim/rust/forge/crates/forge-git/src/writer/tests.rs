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
async fn stage_unstage_and_discard_use_exact_targets_and_settlement() {
    let (directory, store, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "changed\n").unwrap();
    let staged = run(
        &service,
        &repository,
        GitWriteAction::Stage {
            path: vec![path("sample.txt")],
        },
    )
    .await;
    assert_eq!(staged.target[0].completion, TargetCompletion::Completed);
    assert!(staged.settled.as_ref().is_some_and(|settled| settled.path.len() == 1));
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"changed\n"
    );
    let unstaged = run(
        &service,
        &repository,
        GitWriteAction::Unstage {
            path: vec![path("sample.txt")],
        },
    )
    .await;
    assert_eq!(unstaged.target[0].completion, TargetCompletion::Completed);
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"original\n"
    );
    let discarded = run(
        &service,
        &repository,
        GitWriteAction::Discard {
            source: DiscardSource::Index,
            path: vec![path("sample.txt")],
        },
    )
    .await;
    assert_eq!(discarded.target[0].completion, TargetCompletion::Completed);
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"original\n"
    );
    assert_eq!(store.writes.usage().operations, 0);
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
    intent.validate_observed_sources(&observed.head, &observed.path).unwrap();
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
    std::fs::write(directory.path().join("sample.txt"), "staged before commit capture\n").unwrap();
    let staged = service.reserve(Arc::clone(&repository), GitWriteAction::Stage { path: vec![path("sample.txt")] }).unwrap();
    let preparing = service.prepare(Arc::clone(&repository), GitWriteAction::Commit { message: "commit after optimistic stage".into() });
    tokio::pin!(preparing);
    let waiting = std::future::poll_fn(|context| std::task::Poll::Ready(std::future::Future::poll(preparing.as_mut(), context).is_pending())).await;
    assert!(waiting, "commit captured the index before earlier staging completed");
    let outcome = service.submit(staged).unwrap().finish().await.unwrap();
    assert!(outcome.target.iter().all(|target| target.completion == TargetCompletion::Completed));
    service.acknowledge(outcome.operation).unwrap();
    let intent = tokio::time::timeout(Duration::from_secs(10), preparing).await.unwrap().unwrap();
    let committed = service.submit(intent).unwrap().finish().await.unwrap();
    assert!(committed.target.iter().all(|target| target.completion == TargetCompletion::Completed), "{committed:?}");
    service.acknowledge(committed.operation).unwrap();
    assert_eq!(git(directory.path(), &["show", "HEAD:sample.txt"]), b"staged before commit capture\n");
    assert_eq!(store.writes.usage().operations, 0);
}

#[tokio::test]
async fn unverified_outcome_blocks_writes_until_read_only_reconciliation() {
    let (directory, store, repository, service) = fixture().await;
    let scope = MutationScope::Index(repository.identity.worktree.clone().unwrap());
    let operation = store.writes.admit(vec![scope.clone()], 1).unwrap();
    store.writes.start(operation).unwrap().unwrap().finish_quarantined(OperationCompletion::Uncertain).unwrap();
    assert!(store.writes.take_receipt(operation).is_none());
    assert!(service.reserve(Arc::clone(&repository), GitWriteAction::Stage { path: vec![path("sample.txt")] }).is_err());
    service.reconcile(&repository).await.unwrap();
    assert!(store.writes.quarantined(&[scope]).is_empty());
    std::fs::write(directory.path().join("sample.txt"), "after reconciliation\n").unwrap();
    let outcome = run(&service, &repository, GitWriteAction::Stage { path: vec![path("sample.txt")] }).await;
    assert!(outcome.target.iter().all(|target| target.completion == TargetCompletion::Completed));
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"after reconciliation\n");
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

#[tokio::test]
async fn patch_stages_one_raw_hunk_and_unstages_it() {
    use forge_diff::{
        raw::compute_hunks,
        source::{Representation, SourcePair, SourceVersion},
    };
    let (directory, _, repository, service) = fixture().await;
    let old = b"one\ntwo\nthree\nfour\nfive\n";
    let new = b"ONE\ntwo\nthree\nfour\nFIVE\n";
    std::fs::write(directory.path().join("sample.txt"), old).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "multiline"]);
    std::fs::write(directory.path().join("sample.txt"), new).unwrap();
    let analysis = compute_hunks(SourcePair {
        old: SourceVersion::new(old.to_vec(), Representation::GitCanonical).unwrap(),
        new: SourceVersion::new(new.to_vec(), Representation::GitCanonical).unwrap(),
    })
    .unwrap();
    assert_eq!(analysis.hunks().len(), 2);
    let selected = vec![analysis.hunks()[0].id];
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Patch {
            direction: PatchDirection::Stage,
            target: PatchTarget {
                path: path("sample.txt"),
                analysis,
                selected,
            },
        },
    )
    .await;
    assert_eq!(
        outcome.target[0].completion,
        TargetCompletion::Completed,
        "{outcome:?}"
    );
    let staged = git(directory.path(), &["show", ":sample.txt"]);
    assert_eq!(staged, b"ONE\ntwo\nthree\nfour\nfive\n");
    let analysis = compute_hunks(SourcePair {
        old: SourceVersion::new(old.to_vec(), Representation::GitCanonical).unwrap(),
        new: SourceVersion::new(staged, Representation::GitCanonical).unwrap(),
    })
    .unwrap();
    let selected = vec![analysis.hunks()[0].id];
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Patch {
            direction: PatchDirection::Unstage,
            target: PatchTarget {
                path: path("sample.txt"),
                analysis,
                selected,
            },
        },
    )
    .await;
    assert_eq!(
        outcome.target[0].completion,
        TargetCompletion::Completed,
        "{outcome:?}"
    );
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), old);
}

#[tokio::test]
async fn staged_hunk_discard_preserves_other_staged_and_unstaged_changes() {
    use forge_diff::{
        raw::compute_hunks,
        source::{Representation, SourcePair, SourceVersion},
    };
    let (directory, _, repository, service) = fixture().await;
    let original = b"one\ntwo\nthree\nfour\nfive\n";
    let staged = b"ONE\ntwo\nthree\nfour\nFIVE\n";
    std::fs::write(directory.path().join("sample.txt"), original).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "baseline"]);
    std::fs::write(directory.path().join("sample.txt"), staged).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(
        directory.path().join("sample.txt"),
        b"ONE\nTWO\nthree\nfour\nFIVE\n",
    )
    .unwrap();
    let analysis = compute_hunks(SourcePair {
        old: SourceVersion::new(original.to_vec(), Representation::GitCanonical).unwrap(),
        new: SourceVersion::new(staged.to_vec(), Representation::GitCanonical).unwrap(),
    })
    .unwrap();
    let selected = vec![analysis.hunks()[0].id];
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Patch {
            direction: PatchDirection::DiscardStaged,
            target: PatchTarget {
                path: path("sample.txt"),
                analysis,
                selected,
            },
        },
    )
    .await;
    assert_eq!(
        outcome.target[0].completion,
        TargetCompletion::Completed,
        "{outcome:?}"
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"one\ntwo\nthree\nfour\nFIVE\n"
    );
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"one\nTWO\nthree\nfour\nFIVE\n"
    );
}

#[tokio::test]
async fn staged_hunk_conflict_fails_preflight_without_modifying_index() {
    use forge_diff::{
        raw::compute_hunks,
        source::{Representation, SourcePair, SourceVersion},
    };
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), b"staged\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(
        directory.path().join("sample.txt"),
        b"overlapping unstaged\n",
    )
    .unwrap();
    let analysis = compute_hunks(SourcePair {
        old: SourceVersion::new(b"original\n".to_vec(), Representation::GitCanonical).unwrap(),
        new: SourceVersion::new(b"staged\n".to_vec(), Representation::GitCanonical).unwrap(),
    })
    .unwrap();
    let selected = vec![analysis.hunks()[0].id];
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Patch {
            direction: PatchDirection::DiscardStaged,
            target: PatchTarget {
                path: path("sample.txt"),
                analysis,
                selected,
            },
        },
    )
    .await;
    assert_ne!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"staged\n");
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"overlapping unstaged\n"
    );
}

#[tokio::test]
async fn parent_intent_stages_and_unstages_more_than_256_captured_paths() {
    let (directory, _, repository, service) = fixture().await;
    let selected: Vec<_> = (0..257)
        .map(|index| {
            let name = format!("selected-{index:04}.txt");
            std::fs::write(directory.path().join(&name), "content\n").unwrap();
            path(&name)
        })
        .collect();
    let staged = run(
        &service,
        &repository,
        GitWriteAction::Stage {
            path: selected.clone(),
        },
    )
    .await;
    assert_eq!(staged.target.len(), 257);
    assert!(
        staged
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{staged:?}"
    );
    assert_eq!(staged.settled.as_ref().unwrap().path.len(), 257);
    let unstaged = run(
        &service,
        &repository,
        GitWriteAction::Unstage {
            path: selected.clone(),
        },
    )
    .await;
    assert!(
        unstaged
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{unstaged:?}"
    );
    assert_eq!(
        git(directory.path(), &["diff", "--cached", "--name-only"]),
        b""
    );
    let discarded = run(
        &service,
        &repository,
        GitWriteAction::Discard {
            path: selected,
            source: DiscardSource::Index,
        },
    )
    .await;
    assert!(
        discarded
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    assert!(!directory.path().join("selected-0000.txt").exists());
    assert!(!directory.path().join("selected-0256.txt").exists());
    assert!(directory.path().join("sample.txt").exists());
}

#[tokio::test]
async fn batch_preserves_per_target_actions_under_one_receipt() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "changed\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(directory.path().join("added.txt"), "new\n").unwrap();
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Batch {
            action: vec![
                GitWriteAction::Unstage {
                    path: vec![path("sample.txt")],
                },
                GitWriteAction::Stage {
                    path: vec![path("added.txt")],
                },
            ],
        },
    )
    .await;
    assert_eq!(outcome.target.len(), 2);
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"original\n"
    );
    assert_eq!(git(directory.path(), &["show", ":added.txt"]), b"new\n");
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
async fn completed_target_survives_later_rejection_and_remaining_targets_do_not_run() {
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
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_eq!(
        outcome.target[1].completion,
        TargetCompletion::OutcomeUnknown
    );
    assert_eq!(outcome.target[2].completion, TargetCompletion::NotStarted);
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"changed\n"
    );
    assert_eq!(git(directory.path(), &["ls-files", "last.txt"]), b"");
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
    let outcome = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if let Some(outcome) = service.outcome(operation) {
                break outcome;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
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
async fn source_selection_preserves_staged_text_or_restores_head_explicitly() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("sample.txt"), "staged\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(directory.path().join("sample.txt"), "working\n").unwrap();
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Discard {
            source: DiscardSource::Index,
            path: vec![path("sample.txt")],
        },
    )
    .await;
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"staged\n"
    );
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Discard {
            source: DiscardSource::Head,
            path: vec![path("sample.txt")],
        },
    )
    .await;
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"original\n"
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"original\n"
    );
}

#[tokio::test]
async fn head_discard_handles_staged_addition_and_deletion() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("added.txt"), "added\n").unwrap();
    git(directory.path(), &["add", "added.txt"]);
    git(directory.path(), &["rm", "sample.txt"]);
    let outcome = run(
        &service,
        &repository,
        GitWriteAction::Discard {
            source: DiscardSource::Head,
            path: vec![path("added.txt"), path("sample.txt")],
        },
    )
    .await;
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{outcome:?}"
    );
    assert!(!directory.path().join("added.txt").exists());
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"original\n"
    );
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
async fn patch_addition_uses_missing_source_and_round_trips() {
    use forge_diff::{
        raw::compute_hunks,
        source::{Representation, SourcePair, SourceVersion},
    };
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("added.txt"), "added\n").unwrap();
    let analysis = compute_hunks(SourcePair {
        old: SourceVersion::new(Vec::new(), Representation::GitCanonical).unwrap(),
        new: SourceVersion::new(b"added\n".to_vec(), Representation::GitCanonical).unwrap(),
    })
    .unwrap();
    let selected = vec![analysis.hunks()[0].id];
    let target = PatchTarget {
        path: path("added.txt"),
        analysis,
        selected,
    };
    let staged = run(
        &service,
        &repository,
        GitWriteAction::Patch {
            direction: PatchDirection::Stage,
            target: target.clone(),
        },
    )
    .await;
    assert_eq!(
        staged.target[0].completion,
        TargetCompletion::Completed,
        "{staged:?}"
    );
    let unstaged = run(
        &service,
        &repository,
        GitWriteAction::Patch {
            direction: PatchDirection::Unstage,
            target,
        },
    )
    .await;
    assert_eq!(
        unstaged.target[0].completion,
        TargetCompletion::Completed,
        "{unstaged:?}"
    );
    assert_eq!(git(directory.path(), &["ls-files", "added.txt"]), b"");
    assert!(directory.path().join("added.txt").exists());
}

#[tokio::test]
async fn publish_branch_sets_missing_upstream_and_rejects_changed_head() {
    let (directory, _, repository, service) = fixture().await;
    let remote = tempfile::tempdir().unwrap();
    git(remote.path(), &["init", "--bare"]);
    git(
        directory.path(),
        &["remote", "add", "origin", remote.path().to_str().unwrap()],
    );
    let first = run(
        &service,
        &repository,
        GitWriteAction::PublishBranch {
            name: "main".into(),
            expected_head: None,
        },
    )
    .await;
    assert_eq!(first.target[0].completion, TargetCompletion::Completed);
    assert_eq!(
        git(
            directory.path(),
            &["rev-parse", "--abbrev-ref", "@{upstream}"]
        ),
        b"origin/main\n"
    );
    let second = run(
        &service,
        &repository,
        GitWriteAction::PublishBranch {
            name: "main".into(),
            expected_head: None,
        },
    )
    .await;
    assert_eq!(second.target[0].completion, TargetCompletion::Completed);
    let intent = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::PublishBranch {
                name: "main".into(),
                expected_head: None,
            },
        )
        .await
        .unwrap();
    git(directory.path(), &["switch", "-c", "other"]);
    let rejected = service.submit(intent).unwrap().finish().await.unwrap();
    assert_eq!(rejected.target[0].completion, TargetCompletion::Rejected);
    service.acknowledge(rejected.operation).unwrap();
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
        .set_settlement_handler(
            Arc::new(|outcome| {
                assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
                anyhow::bail!("private metadata unavailable")
            }),
            0,
        )
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
async fn compound_discard_reverses_overlap_and_preserves_unselected_changes() {
    use forge_diff::{
        raw::compute_hunks,
        source::{Representation, SourcePair, SourceVersion},
    };
    let (directory, _, repository, service) = fixture().await;
    let original = b"one\ntwo\nthree\nfour\nfive\nsix\nseven\n";
    let staged = b"ONE\ntwo\nthree\nfour\nfive\nsix\nSEVEN\n";
    let worktree = b"changed again\ntwo\nthree\nFOUR\nfive\nsix\nSEVEN\n";
    std::fs::write(directory.path().join("sample.txt"), original).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "baseline"]);
    std::fs::write(directory.path().join("sample.txt"), staged).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(directory.path().join("sample.txt"), worktree).unwrap();
    let target = |old: &[u8], new: &[u8]| {
        let analysis = compute_hunks(SourcePair {
            old: SourceVersion::new(old.to_vec(), Representation::GitCanonical).unwrap(),
            new: SourceVersion::new(new.to_vec(), Representation::GitCanonical).unwrap(),
        })
        .unwrap();
        let selected = vec![analysis.hunks()[0].id];
        PatchTarget {
            path: path("sample.txt"),
            analysis,
            selected,
        }
    };
    let action = GitWriteAction::DiscardCombined {
        staged: target(original, staged),
        unstaged: target(staged, worktree),
    };
    let stale = service
        .prepare(Arc::clone(&repository), action.clone())
        .await
        .unwrap();
    std::fs::write(directory.path().join("sample.txt"), b"later\n").unwrap();
    let rejected = service.submit(stale).unwrap().finish().await.unwrap();
    assert_ne!(rejected.target[0].completion, TargetCompletion::Completed);
    service.acknowledge(rejected.operation).unwrap();
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), staged);
    std::fs::write(directory.path().join("sample.txt"), worktree).unwrap();
    let outcome = run(&service, &repository, action).await;
    assert_eq!(outcome.target.len(), 1);
    assert_eq!(
        outcome.target[0].completion,
        TargetCompletion::Completed,
        "{outcome:?}"
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"one\ntwo\nthree\nfour\nfive\nsix\nSEVEN\n"
    );
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"one\ntwo\nthree\nFOUR\nfive\nsix\nSEVEN\n"
    );
}
#[tokio::test]
async fn compound_discard_reports_completed_unstaged_step_before_staged_conflict() {
    use forge_diff::{
        raw::compute_hunks,
        source::{Representation, SourcePair, SourceVersion},
    };
    let (directory, _, repository, service) = fixture().await;
    let original = b"one\ntwo\nthree\nfour\nfive\nsix\nseven\n";
    let staged = b"ONE\ntwo\nthree\nfour\nfive\nsix\nSEVEN\n";
    let worktree = b"changed again\ntwo\nthree\nFOUR\nfive\nsix\nSEVEN\n";
    std::fs::write(directory.path().join("sample.txt"), original).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "baseline"]);
    std::fs::write(directory.path().join("sample.txt"), staged).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(directory.path().join("sample.txt"), worktree).unwrap();
    let target = |old: &[u8], new: &[u8]| {
        let analysis = compute_hunks(SourcePair {
            old: SourceVersion::new(old.to_vec(), Representation::GitCanonical).unwrap(),
            new: SourceVersion::new(new.to_vec(), Representation::GitCanonical).unwrap(),
        })
        .unwrap();
        let selected = vec![analysis.hunks()[0].id];
        PatchTarget {
            path: path("sample.txt"),
            analysis,
            selected,
        }
    };
    let action = GitWriteAction::DiscardCombined {
        staged: target(original, staged),
        unstaged: target(staged, worktree),
    };
    let mut action = action;
    if let GitWriteAction::DiscardCombined { unstaged, .. } = &mut action {
        unstaged.selected = vec![unstaged.analysis.hunks()[1].id];
    }
    let outcome = run(&service, &repository, action).await;
    assert_eq!(outcome.target.len(), 1);
    assert_ne!(outcome.target[0].completion, TargetCompletion::Completed);
    assert!(
        outcome.target[0]
            .diagnostic
            .as_deref()
            .unwrap_or_default()
            .contains("Selected unstaged hunks were discarded"),
        "{outcome:?}"
    );
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), staged);
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"changed again\ntwo\nthree\nfour\nfive\nsix\nSEVEN\n"
    );
}

#[tokio::test]
async fn whole_file_stage_accepts_large_sources_and_changes_after_preparation() {
    let (directory, _, repository, service) = fixture().await;
    let large = vec![b'x'; 9 * 1024 * 1024];
    std::fs::write(directory.path().join("large.bin"), &large).unwrap();
    let intent = service.prepare(Arc::clone(&repository), GitWriteAction::Batch {
        action: vec![
            GitWriteAction::Stage { path: vec![path("large.bin")] },
            GitWriteAction::Stage { path: vec![path("sample.txt")] },
        ],
    }).await.unwrap();
    std::fs::write(directory.path().join("sample.txt"), b"new index\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(directory.path().join("sample.txt"), b"execution contents\n").unwrap();
    let outcome = service.submit(intent).unwrap().finish().await.unwrap();
    assert!(outcome.target.iter().all(|target| target.completion == TargetCompletion::Completed), "{outcome:?}");
    let staged = crate::command::read_command(
        git_command(&repository).unwrap().args(["show", ":large.bin"]),
        CommandLimits { stdout_bytes: 10 * 1024 * 1024, stderr_bytes: 4096, timeout: Duration::from_secs(30) },
        || Ok(()),
    ).unwrap();
    assert!(staged.status.success());
    assert_eq!(staged.stdout, large);
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"execution contents\n");
    service.acknowledge(outcome.operation).unwrap();
}
