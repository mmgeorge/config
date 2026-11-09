use super::*;
use crate::test_support::git;

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
async fn whole_file_stage_completes_when_deletion_was_staged_after_preparation() {
    let (directory, _, repository, service) = fixture().await;
    std::fs::write(directory.path().join("neighbor.txt"), "neighbor\n").unwrap();
    std::fs::remove_file(directory.path().join("sample.txt")).unwrap();
    let intent = service
        .prepare(
            Arc::clone(&repository),
            GitWriteAction::Stage {
                path: vec![path("sample.txt"), path("neighbor.txt")],
            },
        )
        .await
        .unwrap();
    git(
        directory.path(),
        &["rm", "--cached", "--quiet", "sample.txt"],
    );
    let outcome = service.submit(intent).unwrap().finish().await.unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{outcome:?}"
    );
    service.acknowledge(outcome.operation).unwrap();
    assert_eq!(
        git(directory.path(), &["diff", "--cached", "--name-status"]),
        b"A\tneighbor.txt\nD\tsample.txt\n"
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
    let (_directory, _, repository, service) = fixture().await;
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
