use super::*;
use crate::storage::SqliteStore;

#[tokio::test]
async fn reuses_unchanged_checkpoint_and_detects_external_dirty_edits() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let store = SqliteStore::open(data.path()).unwrap();
    let repositories = Arc::new(RepositoryStore::default());
    let checkpoint = GitCheckpoint::new(repository.path());
    let original = checkpoint
        .capture(&store.objects, &repositories, "reuse", 1)
        .await
        .unwrap();
    fs::write(repository.path().join("ignored.tmp"), "ignored").unwrap();
    let reused = checkpoint
        .capture(&store.objects, &repositories, "reuse", 2)
        .await
        .unwrap();
    assert_eq!(reused.id, original.id);
    assert_eq!(reused.created_at_ms, 1);

    let path = repository.path().join("tracked.txt");
    fs::write(&path, "first!\n").unwrap();
    let dirty = checkpoint
        .capture(&store.objects, &repositories, "reuse", 3)
        .await
        .unwrap();
    let modified = fs::metadata(&path).unwrap().modified().unwrap();
    fs::write(&path, "second\n").unwrap();
    fs::File::options()
        .write(true)
        .open(&path)
        .unwrap()
        .set_times(fs::FileTimes::new().set_modified(modified))
        .unwrap();
    let edited = checkpoint
        .capture(&store.objects, &repositories, "reuse", 4)
        .await
        .unwrap();
    assert_ne!(
        edited.id, dirty.id,
        "same-length edits with restored mtime must invalidate reuse"
    );
    let captured = edited
        .file
        .iter()
        .find(|file| file.path == "tracked.txt")
        .unwrap();
    assert_eq!(store.objects.get(&captured.object_id).unwrap(), b"second\n");

    fs::write(repository.path().join("new.txt"), "external").unwrap();
    let added = checkpoint
        .capture(&store.objects, &repositories, "reuse", 5)
        .await
        .unwrap();
    assert_ne!(added.id, edited.id);
    fs::remove_file(path).unwrap();
    let removed = checkpoint
        .capture(&store.objects, &repositories, "reuse", 6)
        .await
        .unwrap();
    assert!(removed.deleted.contains(&"tracked.txt".to_owned()));
    git(repository.path(), &["add", "."]);
    git(repository.path(), &["commit", "-qm", "external changes"]);
    let committed = checkpoint
        .capture(&store.objects, &repositories, "reuse", 7)
        .await
        .unwrap();
    assert_ne!(committed.head, removed.head);
    assert_ne!(committed.id, removed.id);
}

#[tokio::test]
async fn admitted_capture_honors_cancellation_but_accepts_repository_invalidation() {
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
        let result = repositories
            .reads
            .submit(capture.input_bytes, move |cancellation| {
                capture
                    .run(&objects, cancellation)
                    .map(|(record, _capture)| record)
            })
            .unwrap()
            .finish()
            .await;
        if invalidate {
            assert!(
                result.is_ok(),
                "repository invalidation must not fail capture: {result:?}"
            );
        } else {
            assert!(result.unwrap_err().to_string().contains("cancelled"));
            assert!(
                store
                    .objects
                    .get(&crate::plan::digest(b"before\n"))
                    .is_err()
            );
        }
        assert_eq!(repositories.writes.usage().operations, 0);
        assert_eq!(repositories.writes.usage().input_bytes, 0);
    }
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
        anyhow::ensure!(store.objects.get(&identity).is_err(), "capture cancelled");
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
    assert_eq!(store.objects.get(&identity).unwrap(), b"changed\n");
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
    let object = crate::storage::state::directory(data.path())
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
                .recv()
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
        async {
            while repositories.writes.usage().operations != 0 {
                tokio::task::yield_now().await;
            }
        }
        .await;
        assert_eq!(
            fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
            "after\n"
        );
    });
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

#[cfg(unix)]
#[test]
fn raw_path_and_symlink_checkpoint_does_not_follow_external_content() {
    use std::os::unix::{ffi::OsStrExt, fs::symlink};
    let repository = repository();
    let root = repository.path();
    #[cfg(target_os = "macos")]
    let name = std::ffi::OsStr::from_bytes(b"raw-\xce\xbb");
    #[cfg(not(target_os = "macos"))]
    let name = std::ffi::OsStr::from_bytes(b"raw-\xff");
    fs::write(root.join(name), b"exact").unwrap();
    symlink("/outside/never-read", root.join("link")).unwrap();
    let data = tempfile::tempdir().unwrap();
    let objects = ObjectStore::open(data.path()).unwrap();
    let captured = GitCheckpoint::new(root)
        .capture_native(&objects, "session", 0, || Ok(()))
        .unwrap();
    let key = forge_git::checkpoint::path_key(name.as_bytes());
    assert_eq!(captured.read(&objects, &key).unwrap().unwrap(), b"exact");
    assert_eq!(
        captured.read(&objects, "link").unwrap().unwrap(),
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
    assert_eq!(captured.read(&objects, "omit/b").unwrap().unwrap(), b"omit");
}
