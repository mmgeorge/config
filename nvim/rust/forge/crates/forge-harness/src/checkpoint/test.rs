use super::*;
use crate::storage::SqliteStore;
use std::collections::BTreeSet;

#[tokio::test]
async fn capture_waits_for_each_repository_scope_before_reading() {
    use std::task::Poll;
    use std::time::Duration;
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let repositories = Arc::new(RepositoryStore::default());
    let handle = repositories
        .open(repository.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    for (sequence, scope) in checkpoint_scopes(&handle).unwrap().into_iter().enumerate() {
        let predecessor = repositories.writes.admit(vec![scope], 0).unwrap();
        let running = repositories.writes.start(predecessor).unwrap().unwrap();
        let mut capture = Box::pin(checkpoint.capture(&store.objects, &repositories, "session", 1));
        tokio::time::timeout(
            Duration::from_secs(2),
            std::future::poll_fn(|context| {
                assert!(capture.as_mut().poll(context).is_pending());
                if repositories.writes.usage().operations == 2 {
                    Poll::Ready(())
                } else {
                    context.waker().wake_by_ref();
                    Poll::Pending
                }
            }),
        )
        .await
        .unwrap();
        assert_eq!(repositories.reads.status().active_jobs, 0);
        let content = format!("scope predecessor {sequence}\n");
        fs::write(repository.path().join("tracked.txt"), &content).unwrap();
        running.finish(OperationCompletion::Completed).unwrap();
        repositories.writes.take_receipt(predecessor).unwrap();
        let captured = capture.await.unwrap();
        let tracked = captured
            .file
            .iter()
            .find(|file| file.path == "tracked.txt")
            .unwrap();
        assert_eq!(
            store
                .objects
                .get(&tracked.object_id, 4096)
                .unwrap()
                .unwrap(),
            content.as_bytes()
        );
        assert_eq!(repositories.writes.usage().operations, 0);
        assert_eq!(repositories.writes.usage().input_bytes, 0);
    }
}

#[tokio::test]
async fn dropping_capture_queued_on_a_scope_removes_its_admission() {
    use std::task::Poll;
    use std::time::Duration;
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let repositories = Arc::new(RepositoryStore::default());
    let handle = repositories
        .open(repository.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let predecessor = repositories
        .writes
        .admit(checkpoint_scopes(&handle).unwrap(), 0)
        .unwrap();
    let running = repositories.writes.start(predecessor).unwrap().unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    let mut capture = Box::pin(checkpoint.capture(&store.objects, &repositories, "session", 1));
    tokio::time::timeout(
        Duration::from_secs(2),
        std::future::poll_fn(|context| {
            assert!(capture.as_mut().poll(context).is_pending());
            if repositories.writes.usage().operations == 2 {
                Poll::Ready(())
            } else {
                context.waker().wake_by_ref();
                Poll::Pending
            }
        }),
    )
    .await
    .unwrap();
    drop(capture);
    assert_eq!(repositories.writes.usage().operations, 1);
    assert_eq!(repositories.writes.usage().input_bytes, 0);
    assert!(
        store
            .objects
            .get(&crate::plan::digest(b"before\n"), 4096)
            .is_err()
    );
    running.finish(OperationCompletion::Completed).unwrap();
    repositories.writes.take_receipt(predecessor).unwrap();
    assert_eq!(repositories.writes.usage().operations, 0);
}

#[tokio::test]
async fn cancelled_capture_worker_retains_scopes_until_native_exit() {
    use std::time::Duration;
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let repositories = Arc::new(RepositoryStore::default());
    let checkpoint = GitCheckpoint::new(repository.path());
    let capture = checkpoint
        .admit_capture(Arc::clone(&repositories), "session".into(), 1)
        .await
        .unwrap();
    let operation = capture.admission.operation;
    let scope = checkpoint_scopes(&capture.repository).unwrap();
    let objects = store.objects.clone();
    let (started, ready) = tokio::sync::oneshot::channel();
    let (release, wait) = std::sync::mpsc::channel();
    let task = repositories
        .reads
        .submit(capture.input_bytes, move |cancellation| {
            started.send(()).unwrap();
            wait.recv_timeout(Duration::from_secs(5)).unwrap();
            capture.run(&objects, cancellation)
        })
        .unwrap();
    ready.await.unwrap();
    drop(task);
    let successor = repositories.writes.admit(scope, 0).unwrap();
    assert!(repositories.writes.start(successor).unwrap().is_none());
    assert_eq!(repositories.reads.status().active_jobs, 1);
    assert!(repositories.writes.usage().input_bytes > 0);
    release.send(()).unwrap();
    let running = tokio::time::timeout(
        Duration::from_secs(2),
        repositories.writes.wait_start(successor).unwrap(),
    )
    .await
    .unwrap()
    .unwrap();
    running.finish(OperationCompletion::Completed).unwrap();
    repositories.writes.take_receipt(successor).unwrap();
    assert!(
        repositories
            .reads
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished
            .is_empty()
    );
    assert!(repositories.writes.state(operation).is_none());
    assert_eq!(repositories.writes.usage().input_bytes, 0);
    assert!(
        store
            .objects
            .get(&crate::plan::digest(b"before\n"), 4096)
            .is_err()
    );
}

#[tokio::test]
async fn capture_from_nested_path_uses_the_canonical_worktree() {
    let repository = repository();
    fs::create_dir(repository.path().join("nested")).unwrap();
    fs::write(repository.path().join("nested/file.txt"), b"nested\n").unwrap();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let repositories = Arc::new(RepositoryStore::default());
    let handle = repositories
        .open(repository.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let captured = GitCheckpoint::new(repository.path().join("nested"))
        .capture(&store.objects, &repositories, "session", 1)
        .await
        .unwrap();
    assert_eq!(
        captured.workspace,
        handle
            .identity
            .worktree_root
            .as_ref()
            .unwrap()
            .to_string_lossy()
    );
    assert!(captured.resolved().unwrap().contains_key("tracked.txt"));
    assert!(
        captured
            .file
            .iter()
            .any(|file| file.path == "nested/file.txt")
    );
}

#[tokio::test]
async fn admitted_capture_rejects_cancellation_and_invalidation_before_native_work() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let repositories = Arc::new(RepositoryStore::default());
    let checkpoint = GitCheckpoint::new(repository.path());
    for invalidate in [false, true] {
        let capture = checkpoint
            .admit_capture(Arc::clone(&repositories), "session".into(), 1)
            .await
            .unwrap();
        if invalidate {
            capture.repository.invalidate().unwrap();
        } else {
            repositories
                .writes
                .cancel(capture.admission.operation)
                .unwrap();
        }
        let objects = store.objects.clone();
        let error = repositories
            .reads
            .submit(capture.input_bytes, move |cancellation| {
                capture
                    .run(&objects, cancellation)
                    .map(|(record, _capture)| record)
            })
            .unwrap()
            .finish()
            .await
            .unwrap_err();
        assert!(error.to_string().contains(if invalidate {
            "invalidation"
        } else {
            "cancelled"
        }));
        assert_eq!(repositories.writes.usage().operations, 0);
        assert_eq!(repositories.writes.usage().input_bytes, 0);
        assert!(
            store
                .objects
                .get(&crate::plan::digest(b"before\n"), 4096)
                .is_err()
        );
    }
}

#[tokio::test]
async fn completed_capture_releases_scopes_but_retains_its_receipt_until_collection() {
    use forge_git::coordinator::OperationState;
    use std::time::Duration;
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let repositories = Arc::new(RepositoryStore::default());
    let capture = GitCheckpoint::new(repository.path())
        .admit_capture(Arc::clone(&repositories), "session".into(), 1)
        .await
        .unwrap();
    let operation = capture.admission.operation;
    let scope = checkpoint_scopes(&capture.repository).unwrap();
    let objects = store.objects.clone();
    let task = repositories
        .reads
        .submit(capture.input_bytes, move |cancellation| {
            capture.run(&objects, cancellation)
        })
        .unwrap();
    tokio::time::timeout(Duration::from_secs(2), async {
        while repositories.writes.state(operation)
            != Some(OperationState::Finished(OperationCompletion::Completed))
        {
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    assert_eq!(repositories.writes.usage().operations, 1);
    assert!(repositories.writes.usage().input_bytes > 0);
    let successor = repositories.writes.admit(scope, 0).unwrap();
    let running = repositories.writes.start(successor).unwrap().unwrap();
    running.finish(OperationCompletion::Completed).unwrap();
    repositories.writes.take_receipt(successor).unwrap();
    let (record, capture) = task.finish().await.unwrap();
    assert!(!record.resolved().unwrap().is_empty());
    assert_eq!(repositories.writes.usage().operations, 1);
    drop(capture);
    assert_eq!(repositories.writes.usage().operations, 0);
    assert_eq!(repositories.writes.usage().input_bytes, 0);
}

#[tokio::test]
async fn capture_uses_shared_admission_before_acquisition() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let repositories = Arc::new(RepositoryStore::new(1, 1, 4096).unwrap());
    let reads = &repositories.reads;
    let checkpoint = GitCheckpoint::new(repository.path());
    let occupied = reads.submit(0, |_| Ok(())).unwrap();
    let error = checkpoint
        .capture(&store.objects, &repositories, "session", 1)
        .await
        .unwrap_err();
    assert!(error.to_string().contains("capacity is full"));
    occupied.finish().await.unwrap();
    let captured = checkpoint
        .capture(&store.objects, &repositories, "session", 1)
        .await
        .unwrap();
    assert_eq!(captured.resolved().unwrap().len(), 2);
    assert!(captured.file.is_empty());
    assert_eq!(
        captured
            .read(&store.objects, "tracked.txt", 4096)
            .unwrap()
            .unwrap(),
        b"before\n"
    );
    assert_eq!(reads.status().active_jobs, 0);
    assert!(
        reads
            .shutdown(std::time::Duration::ZERO)
            .await
            .unfinished
            .is_empty()
    );
    let error = checkpoint
        .capture(&store.objects, &repositories, "session", 2)
        .await
        .unwrap_err();
    assert!(error.to_string().contains("closed"));
}

#[test]
fn cancelled_queued_capture_keeps_admission_and_does_not_acquire_objects() {
    use std::task::Poll;
    use std::time::Duration;
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .max_blocking_threads(1)
        .build()
        .unwrap();
    runtime.block_on(async {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let repositories = Arc::new(RepositoryStore::new(1, 1, 4096).unwrap());
        let reads = &repositories.reads;
        let checkpoint = GitCheckpoint::new(repository.path());
        let (started, ready) = tokio::sync::oneshot::channel();
        let (release, wait) = std::sync::mpsc::channel();
        let blocker = tokio::task::spawn_blocking(move || {
            started.send(()).unwrap();
            wait.recv_timeout(Duration::from_secs(5)).unwrap();
        });
        ready.await.unwrap();
        let mut capture = Box::pin(checkpoint.capture(&store.objects, &repositories, "session", 1));
        std::future::poll_fn(|context| {
            assert!(capture.as_mut().poll(context).is_pending());
            Poll::Ready(())
        })
        .await;
        drop(capture);
        assert_eq!(reads.status().active_jobs, 1);
        assert!(reads.status().reserved_input_bytes > 0);
        release.send(()).unwrap();
        blocker.await.unwrap();
        assert!(
            reads
                .shutdown(Duration::from_secs(2))
                .await
                .unfinished
                .is_empty()
        );
        assert_eq!(reads.status().reserved_input_bytes, 0);
        assert!(
            store
                .objects
                .get(&crate::plan::digest(b"before\n"), 4096)
                .is_err()
        );
    });
}

#[test]
fn capture_cancellation_after_object_publication_returns_no_record() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    fs::write(repository.path().join("tracked.txt"), b"changed\n").unwrap();
    let identity = crate::plan::digest(b"changed\n");
    let result = checkpoint.capture_native(&store.objects, "session", 1, || {
        anyhow::ensure!(
            store.objects.get(&identity, 4096).is_err(),
            "capture cancelled"
        );
        Ok(())
    });
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("capture cancelled")
    );
    assert_eq!(
        fs::read(repository.path().join("tracked.txt")).unwrap(),
        b"changed\n"
    );
    assert_eq!(
        store.objects.get(&identity, 4096).unwrap().unwrap(),
        b"changed\n"
    );
    assert!(
        checkpoint
            .capture_native(&store.objects, "session", 2, || Ok(()))
            .is_ok()
    );
}

fn git(workspace: &Path, args: &[&str]) {
    let status = std::process::Command::new("git")
        .args(args)
        .current_dir(workspace)
        .status()
        .unwrap();
    assert!(status.success(), "git {} failed", args.join(" "));
}

fn repository() -> tempfile::TempDir {
    let temporary = tempfile::tempdir().unwrap();
    git(temporary.path(), &["init", "-q"]);
    git(
        temporary.path(),
        &["config", "user.email", "harness@example.invalid"],
    );
    git(temporary.path(), &["config", "user.name", "Harness Test"]);
    fs::write(
        temporary.path().join(".gitignore"),
        "ignored.tmp\ntarget/\n",
    )
    .unwrap();
    fs::write(temporary.path().join("tracked.txt"), "before\n").unwrap();
    git(temporary.path(), &["add", "."]);
    git(temporary.path(), &["commit", "-qm", "seed"]);
    temporary
}

#[cfg(windows)]
#[test]
fn locked_checkpoint_source_reports_path_and_can_be_retried() {
    use std::os::windows::fs::OpenOptionsExt;

    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let snapshot = GitCheckpoint::new(repository.path());
    let path = repository.path().join("tracked.txt");
    fs::write(&path, "changed before lock\n").unwrap();
    let locked = fs::OpenOptions::new()
        .read(true)
        .share_mode(0)
        .open(&path)
        .unwrap();
    let error = snapshot
        .capture_native(&store.objects, "session", 1, || Ok(()))
        .unwrap_err();
    assert!(
        format!("{error:#}").contains(&format!("capture checkpoint source {}", path.display()))
    );
    drop(locked);
    let checkpoint = snapshot
        .capture_native(&store.objects, "session", 2, || Ok(()))
        .unwrap();
    assert!(
        checkpoint
            .file
            .iter()
            .any(|file| file.path == "tracked.txt")
    );
}

#[tokio::test]
async fn diffs_and_restores_tracked_and_nonignored_untracked_files() {
    let engine = DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4);
    let reads = BlockingReadPool::new(4, 1024 * 1024).unwrap();
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let snapshot = GitCheckpoint::new(repository.path());
    let before = snapshot
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            1,
        )
        .await
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
    fs::write(repository.path().join("new.txt"), "new\n").unwrap();
    fs::write(repository.path().join("ignored.tmp"), "ignored\n").unwrap();
    fs::create_dir_all(repository.path().join("target/generated/deep")).unwrap();
    fs::write(
        repository.path().join("target/generated/deep/artifact.txt"),
        "ignored nested artifact\n",
    )
    .unwrap();
    let after = snapshot
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            2,
        )
        .await
        .unwrap();
    let diff = checkpoint_diff(&store.objects, &reads, &engine, &before, &after)
        .await
        .unwrap();
    assert!(diff.contains("tracked.txt"));
    assert!(diff.contains("new.txt"));
    assert!(!diff.contains("ignored.tmp"));
    assert!(!diff.contains("target/generated/deep/artifact.txt"));

    let repositories = Arc::new(RepositoryStore::default());
    snapshot
        .admit_restore(Arc::clone(&repositories), after.clone(), before.clone())
        .await
        .unwrap()
        .restore(store.objects.clone())
        .await
        .unwrap();
    assert_eq!(repositories.writes.usage().operations, 0);
    assert_eq!(repositories.writes.usage().input_bytes, 0);
    assert_eq!(
        fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
        "before\n"
    );
    assert!(!repository.path().join("new.txt").exists());
    assert!(repository.path().join("ignored.tmp").exists());
    assert!(
        repository
            .path()
            .join("target/generated/deep/artifact.txt")
            .exists()
    );
}

#[tokio::test]
async fn queued_checkpoint_restore_rechecks_sources_after_scope_handoff() {
    use std::future::Future;
    use std::task::Poll;
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    let before = checkpoint
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            1,
        )
        .await
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
    let after = checkpoint
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            2,
        )
        .await
        .unwrap();
    let repositories = Arc::new(RepositoryStore::default());
    let handle = repositories
        .open(repository.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let first = repositories
        .writes
        .admit(
            vec![MutationScope::WorktreeFiles(
                handle.identity.worktree.clone().unwrap(),
            )],
            0,
        )
        .unwrap();
    let running = repositories.writes.start(first).unwrap().unwrap();
    let mut waiting = Box::pin(checkpoint.admit_restore(
        Arc::clone(&repositories),
        after.clone(),
        before.clone(),
    ));
    tokio::time::timeout(
        std::time::Duration::from_secs(2),
        std::future::poll_fn(|context| {
            assert!(waiting.as_mut().poll(context).is_pending());
            if repositories.writes.usage().operations == 2 {
                Poll::Ready(())
            } else {
                context.waker().wake_by_ref();
                Poll::Pending
            }
        }),
    )
    .await
    .unwrap();
    fs::write(repository.path().join("tracked.txt"), "external change\n").unwrap();
    running.finish(OperationCompletion::Completed).unwrap();
    let admitted = waiting.await.unwrap();
    assert!(admitted.restore(store.objects.clone()).await.is_err());
    assert_eq!(
        fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
        "external change\n"
    );
    repositories.writes.take_receipt(first).unwrap();
    assert_eq!(repositories.writes.usage().operations, 0);
    assert_eq!(repositories.writes.usage().input_bytes, 0);
}

#[tokio::test]
async fn dropping_queued_checkpoint_restore_releases_its_receipt_and_input_charge() {
    use std::future::Future;
    use std::task::Poll;
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    let before = checkpoint
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            1,
        )
        .await
        .unwrap();
    let repositories = Arc::new(RepositoryStore::default());
    let handle = repositories
        .open(repository.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let first = repositories
        .writes
        .admit(
            vec![MutationScope::Index(
                handle.identity.worktree.clone().unwrap(),
            )],
            0,
        )
        .unwrap();
    let running = repositories.writes.start(first).unwrap().unwrap();
    let mut waiting = Box::pin(checkpoint.admit_restore(
        Arc::clone(&repositories),
        before.clone(),
        before.clone(),
    ));
    tokio::time::timeout(
        std::time::Duration::from_secs(2),
        std::future::poll_fn(|context| {
            assert!(waiting.as_mut().poll(context).is_pending());
            if repositories.writes.usage().operations == 2 {
                Poll::Ready(())
            } else {
                context.waker().wake_by_ref();
                Poll::Pending
            }
        }),
    )
    .await
    .unwrap();
    assert!(repositories.writes.usage().input_bytes > 0);
    drop(waiting);
    assert_eq!(repositories.writes.usage().operations, 1);
    assert_eq!(repositories.writes.usage().input_bytes, 0);
    running.finish(OperationCompletion::Completed).unwrap();
    repositories.writes.take_receipt(first).unwrap();
    let admitted = GitCheckpoint::new(repository.path())
        .admit_restore(Arc::clone(&repositories), before.clone(), before.clone())
        .await
        .unwrap();
    drop(admitted);
    assert_eq!(repositories.writes.usage().operations, 0);
    assert_eq!(repositories.writes.usage().input_bytes, 0);
}

#[test]
fn rejects_checkpoint_records_from_a_different_worktree() {
    let repository = repository();
    let foreign = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    let mut record = checkpoint
        .capture_native(&store.objects, "session", 1, || Ok(()))
        .unwrap();
    record.workspace = foreign.path().to_string_lossy().into_owned();
    let error = checkpoint
        .restore(&store.objects, &record, &record)
        .unwrap_err();
    assert!(error.to_string().contains("worktree"));
    assert_eq!(
        fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
        "before\n"
    );
}

#[test]
fn corrupt_restore_object_is_rejected_before_any_worktree_deletion() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    fs::write(
        repository.path().join("tracked.txt"),
        "captured dirty baseline\n",
    )
    .unwrap();
    let before = checkpoint
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
    fs::write(repository.path().join("new.txt"), "keep until verified\n").unwrap();
    let after = checkpoint
        .capture_native(&objects, "session", 2, || Ok(()))
        .unwrap();
    let identity = &before
        .file
        .iter()
        .find(|file| file.path == "tracked.txt")
        .unwrap()
        .object_id;
    let object = data
        .path()
        .join("objects/sha256")
        .join(&identity[..2])
        .join(&identity[2..]);
    fs::write(object, "corrupt checkpoint").unwrap();
    assert!(checkpoint.restore(&objects, &after, &before).is_err());
    assert_eq!(
        fs::read_to_string(repository.path().join("new.txt")).unwrap(),
        "keep until verified\n"
    );
    assert_eq!(
        fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
        "after\n"
    );
}

#[test]
fn invalid_later_restore_path_is_rejected_before_any_worktree_deletion() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    let mut before = checkpoint
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(repository.path().join("new.txt"), "keep until verified\n").unwrap();
    let after = checkpoint
        .capture_native(&objects, "session", 2, || Ok(()))
        .unwrap();
    before.file.push(CheckpointFile {
        mode: 0o100644,
        path: "../outside".into(),
        object_id: objects.put(b"invalid target").unwrap(),
    });
    assert!(checkpoint.restore(&objects, &after, &before).is_err());
    assert_eq!(
        fs::read_to_string(repository.path().join("new.txt")).unwrap(),
        "keep until verified\n"
    );
}

#[test]
fn cancelled_blocking_restore_keeps_admission_until_its_worker_exits() {
    use std::future::Future;
    use std::task::Poll;
    let executor = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .max_blocking_threads(1)
        .build()
        .unwrap();
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    let before = checkpoint
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
    let after = checkpoint
        .capture_native(&objects, "session", 2, || Ok(()))
        .unwrap();
    executor.block_on(async {
        let repositories = Arc::new(RepositoryStore::default());
        let admitted = checkpoint
            .admit_restore(Arc::clone(&repositories), after, before)
            .await
            .unwrap();
        let (started, started_receiver) = tokio::sync::oneshot::channel();
        let (release, release_receiver) = std::sync::mpsc::channel();
        let blocker = tokio::task::spawn_blocking(move || {
            started.send(()).unwrap();
            release_receiver
                .recv_timeout(std::time::Duration::from_secs(5))
                .unwrap();
        });
        started_receiver.await.unwrap();
        let mut restoring = Box::pin(admitted.restore(objects));
        std::future::poll_fn(|context| {
            assert!(restoring.as_mut().poll(context).is_pending());
            Poll::Ready(())
        })
        .await;
        drop(restoring);
        assert_eq!(repositories.writes.usage().operations, 1);
        assert!(repositories.writes.usage().input_bytes > 0);
        release.send(()).unwrap();
        blocker.await.unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            while repositories.writes.usage().operations != 0 {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        assert_eq!(
            fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
            "after\n"
        );
    });
}

#[test]
fn uncollected_completed_restore_keeps_its_receipt_for_reconciliation() {
    use forge_git::coordinator::OperationState;
    use std::future::Future;
    use std::task::Poll;
    let executor = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .max_blocking_threads(4)
        .build()
        .unwrap();
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path());
    let before = checkpoint
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
    let after = checkpoint
        .capture_native(&objects, "session", 2, || Ok(()))
        .unwrap();
    executor.block_on(async {
        let repositories = Arc::new(RepositoryStore::default());
        let admitted = checkpoint
            .admit_restore(Arc::clone(&repositories), after, before)
            .await
            .unwrap();
        let operation = admitted.operation;
        let (release, release_receiver) = std::sync::mpsc::channel();
        let blocker = tokio::task::spawn_blocking(move || {
            release_receiver
                .recv_timeout(std::time::Duration::from_secs(5))
                .unwrap();
        });
        let mut restoring = Box::pin(admitted.restore(objects));
        std::future::poll_fn(|context| {
            assert!(restoring.as_mut().poll(context).is_pending());
            Poll::Ready(())
        })
        .await;
        release.send(()).unwrap();
        blocker.await.unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            while !matches!(
                repositories.writes.state(operation),
                Some(OperationState::Finished(_))
            ) {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        drop(restoring);
        assert_eq!(repositories.writes.usage().operations, 1);
        assert!(repositories.writes.usage().input_bytes > 0);
        assert_eq!(
            repositories.writes.take_receipt(operation),
            Some(OperationCompletion::Completed)
        );
        assert_eq!(
            fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
            "before\n"
        );
    });
}

#[tokio::test]
async fn diffs_only_selected_checkpoint_paths() {
    let engine = DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4);
    let reads = BlockingReadPool::new(4, 1024 * 1024).unwrap();
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let snapshot = GitCheckpoint::new(repository.path());
    let before = snapshot
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            1,
        )
        .await
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
    fs::write(repository.path().join("external.txt"), "external\n").unwrap();
    let after = snapshot
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            2,
        )
        .await
        .unwrap();
    let selected = BTreeSet::from(["tracked.txt".to_owned()]);

    let diff =
        checkpoint_diff_for_paths(&store.objects, &reads, &engine, &before, &after, &selected)
            .await
            .unwrap();

    assert!(diff.contains("tracked.txt"));
    assert!(!diff.contains("external.txt"));
    assert_eq!(engine.usage().cache.cached_entries, 1);
    assert_eq!(
        checkpoint_diff_for_paths(&store.objects, &reads, &engine, &before, &after, &selected)
            .await
            .unwrap(),
        diff
    );
    assert_eq!(engine.usage().cache.cached_entries, 1);
    assert!(
        checkpoint_diff_for_paths(
            &store.objects,
            &reads,
            &engine,
            &before,
            &after,
            &BTreeSet::new()
        )
        .await
        .unwrap()
        .is_empty()
    );
    engine.close();
    let error =
        checkpoint_diff_for_paths(&store.objects, &reads, &engine, &before, &after, &selected)
            .await
            .unwrap_err();
    assert!(error.to_string().contains("Closed"));
}

#[tokio::test]
async fn oversized_checkpoint_sources_report_unavailable_without_hiding_other_changes() {
    let engine = DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4);
    let reads = BlockingReadPool::new(4, 1024 * 1024).unwrap();
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let snapshot = GitCheckpoint::new(repository.path());
    let before = snapshot
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            1,
        )
        .await
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), b"after\n").unwrap();
    let mut after = snapshot
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            2,
        )
        .await
        .unwrap();
    let large = store
        .objects
        .put(&vec![b'x'; MAX_SOURCE_BYTES + 1])
        .unwrap();
    after.file.push(CheckpointFile {
        mode: 0o100644,
        path: "large.txt".into(),
        object_id: large.clone(),
    });
    for (source, target) in [(&before, &after), (&after, &before)] {
        let diff = checkpoint_diff(&store.objects, &reads, &engine, source, target)
            .await
            .unwrap();
        assert!(diff.contains("diff --git a/large.txt b/large.txt\nDiff unavailable:"));
        assert!(diff.contains("8388608 bytes"));
        assert!(diff.contains("diff --git a/tracked.txt b/tracked.txt"));
        assert!(diff.contains("after"));
    }
    let destination = repository.path().join("restored-large.txt");
    store.objects.restore_file(&large, &destination).unwrap();
    assert_eq!(
        fs::metadata(destination).unwrap().len(),
        (MAX_SOURCE_BYTES + 1) as u64
    );
}

#[tokio::test]
async fn checkpoint_diff_distinguishes_binary_content_from_unsupported_encoding() {
    let engine = DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4);
    let reads = BlockingReadPool::new(4, 1024 * 1024).unwrap();
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let snapshot = GitCheckpoint::new(repository.path());
    let before = snapshot
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            1,
        )
        .await
        .unwrap();
    fs::write(repository.path().join("binary.dat"), b"text\0data").unwrap();
    fs::write(repository.path().join("encoded.txt"), [0xff, 0xfe]).unwrap();
    let after = snapshot
        .capture(
            &store.objects,
            &Arc::new(RepositoryStore::default()),
            "session",
            2,
        )
        .await
        .unwrap();
    let diff = checkpoint_diff(&store.objects, &reads, &engine, &before, &after)
        .await
        .unwrap();
    assert!(diff.contains("Binary files a/binary.dat and b/binary.dat differ"));
    assert!(diff.contains("diff --git a/encoded.txt b/encoded.txt\nDiff unavailable: checkpoint source is not valid UTF-8"));
    assert!(!diff.contains('\0'));
}

#[tokio::test]
async fn checkpoint_source_uses_shared_read_admission_and_preserves_exact_bytes() {
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let reads = BlockingReadPool::new(1, 4096).unwrap();
    let old = store.objects.put(b"old\r\n").unwrap();
    let new = store.objects.put(b"new without newline").unwrap();
    let occupied = reads.submit(0, |_| Ok(())).unwrap();
    let error = checkpoint_source(
        &store.objects,
        &reads,
        "",
        Some(&ResolvedFile {
            source: super::super::manifest::Content::Stored(old.clone()),
            mode: 0o100644,
        }),
        Some(&ResolvedFile {
            source: super::super::manifest::Content::Stored(new.clone()),
            mode: 0o100644,
        }),
    )
    .await
    .unwrap_err();
    assert!(error.to_string().contains("capacity is full"));
    occupied.finish().await.unwrap();
    let CheckpointSource::Text(pair) = checkpoint_source(
        &store.objects,
        &reads,
        "",
        Some(&ResolvedFile {
            source: super::super::manifest::Content::Stored(old.clone()),
            mode: 0o100644,
        }),
        Some(&ResolvedFile {
            source: super::super::manifest::Content::Stored(new.clone()),
            mode: 0o100644,
        }),
    )
    .await
    .unwrap() else {
        panic!("text source must remain available");
    };
    assert_eq!(pair.old.bytes(), b"old\r\n");
    assert_eq!(pair.new.bytes(), b"new without newline");
    assert_eq!(reads.status().active_jobs, 0);
    assert_eq!(reads.status().reserved_input_bytes, 0);
    assert!(
        reads
            .shutdown(std::time::Duration::ZERO)
            .await
            .unfinished
            .is_empty()
    );
    let error = checkpoint_source(
        &store.objects,
        &reads,
        "",
        Some(&ResolvedFile {
            source: super::super::manifest::Content::Stored(old.clone()),
            mode: 0o100644,
        }),
        Some(&ResolvedFile {
            source: super::super::manifest::Content::Stored(new.clone()),
            mode: 0o100644,
        }),
    )
    .await
    .unwrap_err();
    assert!(error.to_string().contains("closed"));
}

#[test]
fn refuses_rollback_after_workspace_divergence() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let snapshot = GitCheckpoint::new(repository.path());
    let before = snapshot
        .capture_native(&store.objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
    let after = snapshot
        .capture_native(&store.objects, "session", 2, || Ok(()))
        .unwrap();
    fs::write(
        repository.path().join("tracked.txt"),
        "unrelated later edit\n",
    )
    .unwrap();
    assert!(snapshot.restore(&store.objects, &after, &before).is_err());
}

#[test]
fn rejects_checkpoint_paths_outside_the_worktree() {
    let repository = repository();
    let snapshot = GitCheckpoint::new(repository.path());
    assert!(snapshot.file_path("../escape.txt").is_err());
    assert!(snapshot.file_path("/absolute.txt").is_err());
}

#[test]
fn scopes_identical_checkpoint_content_to_its_session() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let snapshot = GitCheckpoint::new(repository.path());
    let first = snapshot
        .capture_native(&store.objects, "session-one", 1, || Ok(()))
        .unwrap();
    let second = snapshot
        .capture_native(&store.objects, "session-two", 1, || Ok(()))
        .unwrap();
    assert_ne!(first.id, second.id);
}

#[test]
fn captures_an_unborn_git_worktree() {
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "-q"]);
    fs::write(repository.path().join("first.txt"), "first\n").unwrap();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path())
        .capture_native(&store.objects, "session", 1, || Ok(()))
        .unwrap();
    assert_eq!(checkpoint.head, "UNBORN");
    assert_eq!(checkpoint.file.len(), 1);
}

#[test]
fn clean_checkpoint_keeps_git_baseline_without_duplicate_objects() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let checkpoint = GitCheckpoint::new(repository.path())
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    assert!(checkpoint.file.is_empty());
    assert!(checkpoint.deleted.is_empty());
    assert!(checkpoint.tree.is_some());
    assert_eq!(
        fs::read_dir(data.path().join("objects/sha256"))
            .unwrap()
            .count(),
        0
    );
    assert_eq!(
        checkpoint
            .read(&objects, "tracked.txt", 4096)
            .unwrap()
            .unwrap(),
        b"before\n"
    );
}

#[test]
fn staged_changes_do_not_block_confirmed_overwrite_or_change_index() {
    use super::super::restore::{RestoreJournal, RestorePreview};
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let adapter = GitCheckpoint::new(repository.path());
    fs::write(repository.path().join("tracked.txt"), "user baseline\n").unwrap();
    git(repository.path(), &["add", "tracked.txt"]);
    let before = adapter
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "agent edit\n").unwrap();
    let after = adapter
        .capture_native(&objects, "session", 2, || Ok(()))
        .unwrap();
    git(repository.path(), &["add", "tracked.txt"]);
    let index_before = fs::read(repository.path().join(".git/index")).unwrap();
    fs::write(repository.path().join("tracked.txt"), "later user edit\n").unwrap();
    fs::write(repository.path().join("unrelated.txt"), "retain\n").unwrap();
    let preview = RestorePreview::prepare(&objects, repository.path(), &after, &before).unwrap();
    assert_eq!(preview.path.len(), 1);
    assert!(
        preview
            .warning
            .iter()
            .any(|warning| warning.contains("tracked.txt"))
    );
    let mut journal = RestoreJournal::begin(&objects, preview).unwrap();
    journal.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(
        fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
        "user baseline\n"
    );
    assert_eq!(
        fs::read(repository.path().join(".git/index")).unwrap(),
        index_before
    );
    assert_eq!(
        fs::read_to_string(repository.path().join("unrelated.txt")).unwrap(),
        "retain\n"
    );
    let recovery = journal.reverse(&objects).unwrap();
    let mut recovery = RestoreJournal::begin(&objects, recovery).unwrap();
    recovery.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(
        fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
        "later user edit\n"
    );
}

#[test]
fn stale_preview_rejects_before_any_file_changes() {
    use super::super::restore::{RestoreJournal, RestorePreview};
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let adapter = GitCheckpoint::new(repository.path());
    let before = adapter
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "agent\n").unwrap();
    let after = adapter
        .capture_native(&objects, "session", 2, || Ok(()))
        .unwrap();
    let preview = RestorePreview::prepare(&objects, repository.path(), &after, &before).unwrap();
    fs::write(repository.path().join("tracked.txt"), "new user edit\n").unwrap();
    assert!(
        RestoreJournal::begin(&objects, preview)
            .unwrap_err()
            .to_string()
            .contains("stale")
    );
    assert_eq!(
        fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
        "new user edit\n"
    );
    assert!(RestoreJournal::load(&objects, "session").unwrap().is_none());
}

#[test]
fn interrupted_restore_resumes_from_durable_file_states() {
    use super::super::restore::{RestoreJournal, RestorePreview, RestoreState};
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let adapter = GitCheckpoint::new(repository.path());
    let before = adapter
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "changed\n").unwrap();
    fs::write(repository.path().join("a-new.txt"), "new\n").unwrap();
    let after = adapter
        .capture_native(&objects, "session", 2, || Ok(()))
        .unwrap();
    let preview = RestorePreview::prepare(&objects, repository.path(), &after, &before).unwrap();
    let mut journal = RestoreJournal::begin(&objects, preview).unwrap();
    let mut calls = 0;
    assert!(
        journal
            .apply(&objects, &mut || {
                calls += 1;
                anyhow::ensure!(calls < 2, "cancelled");
                Ok(())
            })
            .is_err()
    );
    let mut recovered = RestoreJournal::load(&objects, "session").unwrap().unwrap();
    assert_eq!(recovered.state, RestoreState::Applying);
    assert_eq!(recovered.applied, 1);
    recovered.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(recovered.state, RestoreState::FilesComplete);
    assert!(!repository.path().join("a-new.txt").exists());
    assert_eq!(
        fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
        "before\n"
    );
}

#[test]
fn commit_changes_preserve_content_equivalence_and_restore_exactly() {
    use super::super::restore::{RestoreJournal, RestorePreview};
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let adapter = GitCheckpoint::new(repository.path());
    let before = adapter
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(repository.path().join("tracked.txt"), "committed edit\n").unwrap();
    let dirty = adapter
        .capture_native(&objects, "session", 2, || Ok(()))
        .unwrap();
    git(repository.path(), &["add", "tracked.txt"]);
    git(repository.path(), &["commit", "-qm", "new base"]);
    let committed = adapter
        .capture_native(&objects, "session", 3, || Ok(()))
        .unwrap();
    assert!(dirty.equivalent(&committed, &objects).unwrap());
    let preview = RestorePreview::prepare(&objects, repository.path(), &dirty, &before).unwrap();
    assert!(
        preview
            .warning
            .iter()
            .any(|warning| warning.contains("HEAD"))
    );
    let mut journal = RestoreJournal::begin(&objects, preview).unwrap();
    journal.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(
        fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
        "before\n"
    );
}

#[test]
fn two_prompt_checkpoints_preserve_initial_staging_and_unrelated_edits() {
    use super::super::restore::{RestoreJournal, RestorePreview};
    let repository = repository();
    let root = repository.path();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let adapter = GitCheckpoint::new(root);
    for name in ["a", "b", "c"] {
        fs::write(root.join(name), name).unwrap();
    }
    git(root, &["add", "."]);
    git(root, &["commit", "-qm", "fixture"]);
    fs::write(root.join("a"), "initial staged a").unwrap();
    git(root, &["add", "a"]);
    fs::write(root.join("b"), "initial unstaged b").unwrap();
    let index = fs::read(root.join(".git/index")).unwrap();
    let zero = adapter
        .capture_native(&objects, "session", 0, || Ok(()))
        .unwrap();
    fs::write(root.join("a"), "first a").unwrap();
    fs::write(root.join("b"), "first b").unwrap();
    let first = adapter
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(root.join("a"), "second a").unwrap();
    fs::write(root.join("c"), "second c").unwrap();
    let second = adapter
        .capture_native(&objects, "session", 2, || Ok(()))
        .unwrap();
    assert_eq!(
        first.file.iter().find(|file| file.path == "b"),
        second.file.iter().find(|file| file.path == "b")
    );
    fs::write(root.join("unrelated"), "retain this").unwrap();
    let preview = RestorePreview::prepare(&objects, root, &second, &first).unwrap();
    let mut journal = RestoreJournal::begin(&objects, preview).unwrap();
    journal.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(fs::read_to_string(root.join("a")).unwrap(), "first a");
    assert_eq!(fs::read_to_string(root.join("b")).unwrap(), "first b");
    assert_eq!(fs::read_to_string(root.join("c")).unwrap(), "c");
    let preview = RestorePreview::prepare(&objects, root, &first, &zero).unwrap();
    let mut journal = RestoreJournal::begin(&objects, preview).unwrap();
    journal.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(
        fs::read_to_string(root.join("a")).unwrap(),
        "initial staged a"
    );
    assert_eq!(
        fs::read_to_string(root.join("b")).unwrap(),
        "initial unstaged b"
    );
    assert_eq!(
        fs::read_to_string(root.join("unrelated")).unwrap(),
        "retain this"
    );
    assert_eq!(fs::read(root.join(".git/index")).unwrap(), index);
}

#[test]
fn captured_checkout_conversion_survives_changed_configuration() {
    use super::super::restore::{RestoreJournal, RestorePreview};
    let repository = repository();
    let root = repository.path();
    git(root, &["config", "core.autocrlf", "false"]);
    fs::write(root.join(".gitattributes"), "tracked.txt text eol=crlf\n").unwrap();
    git(root, &["add", ".gitattributes"]);
    git(root, &["commit", "-qm", "attributes"]);
    fs::write(root.join("tracked.txt"), "before\r\n").unwrap();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let adapter = GitCheckpoint::new(root);
    let before = adapter
        .capture_native(&objects, "session", 0, || Ok(()))
        .unwrap();
    assert!(!before.file.iter().any(|file| file.path == "tracked.txt"));
    fs::write(root.join("tracked.txt"), "changed\n").unwrap();
    let after = adapter
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    fs::write(root.join(".gitattributes"), "tracked.txt -text\n").unwrap();
    let preview = RestorePreview::prepare(&objects, root, &after, &before).unwrap();
    let mut journal = RestoreJournal::begin(&objects, preview).unwrap();
    journal.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(fs::read(root.join("tracked.txt")).unwrap(), b"before\r\n");
}

#[cfg(windows)]
#[test]
fn case_only_rename_restores_original_spelling() {
    use super::super::restore::{RestoreJournal, RestorePreview};
    let repository = repository();
    let root = repository.path();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let adapter = GitCheckpoint::new(root);
    let before = adapter
        .capture_native(&objects, "session", 0, || Ok(()))
        .unwrap();
    fs::rename(root.join("tracked.txt"), root.join("TRACKED.txt")).unwrap();
    let after = adapter
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    assert!(after.deleted.iter().any(|path| path == "tracked.txt"));
    assert!(after.file.iter().any(|file| file.path == "TRACKED.txt"));
    let preview = RestorePreview::prepare(&objects, root, &after, &before).unwrap();
    let mut journal = RestoreJournal::begin(&objects, preview).unwrap();
    journal.apply(&objects, &mut || Ok(())).unwrap();
    let names = fs::read_dir(root)
        .unwrap()
        .map(|entry| entry.unwrap().file_name())
        .collect::<Vec<_>>();
    assert!(names.iter().any(|name| name == "tracked.txt"));
    assert!(!names.iter().any(|name| name == "TRACKED.txt"));
}

#[test]
fn large_git_baseline_streams_without_private_content_copy_at_capture() {
    use super::super::restore::{RestoreJournal, RestorePreview};
    let repository = repository();
    let root = repository.path();
    let original = vec![42; 9 * 1024 * 1024];
    fs::write(root.join("large.bin"), &original).unwrap();
    git(root, &["add", "large.bin"]);
    git(root, &["commit", "-qm", "large source"]);
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let adapter = GitCheckpoint::new(root);
    let before = adapter
        .capture_native(&objects, "session", 0, || Ok(()))
        .unwrap();
    assert!(before.file.is_empty());
    fs::write(root.join("large.bin"), "changed").unwrap();
    let after = adapter
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    let preview = RestorePreview::prepare(&objects, root, &after, &before).unwrap();
    let mut journal = RestoreJournal::begin(&objects, preview).unwrap();
    journal.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(fs::read(root.join("large.bin")).unwrap(), original);
}

#[test]
fn restore_handles_file_directory_transitions_without_deleting_unrelated_content() {
    use super::super::restore::{RestoreJournal, RestorePreview};
    let repository = repository();
    let root = repository.path();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let adapter = GitCheckpoint::new(root);
    let before = adapter
        .capture_native(&objects, "session", 0, || Ok(()))
        .unwrap();
    fs::remove_file(root.join("tracked.txt")).unwrap();
    fs::create_dir(root.join("tracked.txt")).unwrap();
    fs::write(root.join("tracked.txt/child"), "child").unwrap();
    let after = adapter
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    let preview = RestorePreview::prepare(&objects, root, &after, &before).unwrap();
    let mut journal = RestoreJournal::begin(&objects, preview).unwrap();
    journal.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(fs::read(root.join("tracked.txt")).unwrap(), b"before\n");
    let reverse = journal.reverse(&objects).unwrap();
    let mut journal = RestoreJournal::begin(&objects, reverse).unwrap();
    journal.apply(&objects, &mut || Ok(())).unwrap();
    assert_eq!(fs::read(root.join("tracked.txt/child")).unwrap(), b"child");
}

#[test]
fn info_attributes_override_nested_checkout_rules_without_external_filters() {
    let repository = repository();
    let root = repository.path();
    fs::write(root.join(".gitattributes"), "tracked.txt -text\n").unwrap();
    fs::write(
        root.join(".git/info/attributes"),
        "tracked.txt text eol=crlf\n",
    )
    .unwrap();
    fs::write(root.join("tracked.txt"), "before\r\n").unwrap();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let captured = GitCheckpoint::new(root)
        .capture_native(&objects, "session", 0, || Ok(()))
        .unwrap();
    assert!(captured.checkout["tracked.txt"].crlf);
    assert!(!captured.file.iter().any(|file| file.path == "tracked.txt"));
    fs::write(
        root.join(".git/info/attributes"),
        "tracked.txt filter=never-run\n",
    )
    .unwrap();
    let captured = GitCheckpoint::new(root)
        .capture_native(&objects, "session", 1, || Ok(()))
        .unwrap();
    assert!(captured.file.iter().any(|file| file.path == "tracked.txt"));
    assert_eq!(
        captured.read(&objects, "tracked.txt", 64).unwrap().unwrap(),
        b"before\r\n"
    );
}

#[cfg(unix)]
#[test]
fn raw_path_and_symlink_checkpoint_does_not_follow_external_content() {
    use std::os::unix::{ffi::OsStrExt, fs::symlink};
    let repository = repository();
    let root = repository.path();
    let name = std::ffi::OsStr::from_bytes(b"raw-\xff");
    fs::write(root.join(name), b"exact").unwrap();
    symlink("/outside/never-read", root.join("link")).unwrap();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let captured = GitCheckpoint::new(root)
        .capture_native(&objects, "session", 0, || Ok(()))
        .unwrap();
    let key = forge_git::checkpoint::path_key(name.as_bytes());
    assert_eq!(
        captured.read(&objects, &key, 64).unwrap().unwrap(),
        b"exact"
    );
    assert_eq!(
        captured.read(&objects, "link", 64).unwrap().unwrap(),
        b"/outside/never-read"
    );
}

#[test]
fn sparse_checkout_does_not_record_omitted_files_as_deletions() {
    let repository = repository();
    let root = repository.path();
    fs::create_dir(root.join("keep")).unwrap();
    fs::create_dir(root.join("omit")).unwrap();
    fs::write(root.join("keep/a"), "keep").unwrap();
    fs::write(root.join("omit/b"), "omit").unwrap();
    git(root, &["add", "."]);
    git(root, &["commit", "-qm", "sparse fixture"]);
    git(
        root,
        &["sparse-checkout", "init", "--cone", "--sparse-index"],
    );
    git(root, &["sparse-checkout", "set", "keep"]);
    assert!(!root.join("omit/b").exists());
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let captured = GitCheckpoint::new(root)
        .capture_native(&objects, "session", 0, || Ok(()))
        .unwrap();
    assert!(captured.deleted.is_empty());
    assert_eq!(
        captured.read(&objects, "omit/b", 64).unwrap().unwrap(),
        b"omit"
    );
}
