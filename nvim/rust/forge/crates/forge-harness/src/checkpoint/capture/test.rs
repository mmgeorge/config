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
    // Stage by name, because `ignored.tmp` exists and the in-process helper ignores `.gitignore`.
    forge_git::test_support::stage_and_commit(
        repository.path(),
        &["tracked.txt", "new.txt"],
        "external changes",
    );
    let committed = checkpoint
        .capture(&store.objects, &repositories, "reuse", 7)
        .await
        .unwrap();
    assert_ne!(committed.head, removed.head);
    assert_ne!(committed.id, removed.id);
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
    forge_git::test_support::init_repository(temporary.path());
    forge_git::test_support::set_identity(temporary.path());
    fs::write(
        temporary.path().join(".gitignore"),
        "ignored.tmp\ntarget/\n",
    )
    .unwrap();
    fs::write(temporary.path().join("tracked.txt"), "before\n").unwrap();
    forge_git::test_support::stage_and_commit(
        temporary.path(),
        &[".gitignore", "tracked.txt"],
        "seed",
    );
    temporary
}

/// Compare submodule commit pointers without requiring their objects in the parent repository.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn check_identity_preserves_gitlinks_without_loading_submodule_commits() -> anyhow::Result<()>
{
    let repository = repository();
    let data = tempfile::tempdir()?;
    let objects = ObjectStore::open(data.path())?;
    let repositories = Arc::new(RepositoryStore::default());
    let checkpoint = GitCheckpoint::new(repository.path());
    fs::create_dir(repository.path().join("submodule"))?;
    forge_git::test_support::stage_gitlink(
        repository.path(),
        "submodule",
        "1725b6e1be1f349e0250000824d92d3dc2193e64".parse()?,
    );
    forge_git::test_support::commit_index(repository.path(), "record submodule pointer");
    let initial = checkpoint
        .capture(&objects, &repositories, "gitlink", 1)
        .await?;
    let baseline =
        forge_git::checkpoint::tree_file(repository.path(), initial.tree.as_deref(), "submodule")?
            .unwrap();
    assert_eq!(baseline.mode, 0o160000);
    let identity = initial.check_identity(&objects, None, &[])?;
    assert!(initial.equivalent(&initial, &objects)?);
    let unchanged = checkpoint
        .capture(&objects, &repositories, "gitlink", 2)
        .await?;
    assert_eq!(identity, unchanged.check_identity(&objects, None, &[])?);
    forge_git::test_support::stage_gitlink(
        repository.path(),
        "submodule",
        "2725b6e1be1f349e0250000824d92d3dc2193e64".parse()?,
    );
    forge_git::test_support::commit_index(repository.path(), "change submodule pointer");
    let changed = checkpoint
        .capture(&objects, &repositories, "gitlink", 2)
        .await?;
    assert_ne!(identity, changed.check_identity(&objects, None, &[])?);
    assert!(!initial.equivalent(&changed, &objects)?);
    assert_eq!(initial.changed_paths(&changed)?, vec!["submodule"]);
    let diff = checkpoint_diff(
        &objects,
        &repositories.reads,
        &DiffEngine::new(Default::default(), 1),
        &initial,
        &changed,
    )
    .await?;
    assert!(
        diff.contains("-Subproject commit 1725b6e1be1f349e0250000824d92d3dc2193e64"),
        "{diff}"
    );
    assert!(
        diff.contains("+Subproject commit 2725b6e1be1f349e0250000824d92d3dc2193e64"),
        "{diff}"
    );
    let restore =
        RestorePreview::prepare(&objects, repository.path(), &changed, &initial).unwrap_err();
    assert!(
        restore
            .to_string()
            .contains("submodule restore is unavailable"),
        "{restore:#}"
    );
    assert!(repository.path().join("submodule").is_dir());
    Ok(())
}

/// Missing ordinary blob content must still fail verification instead of becoming a pointer.
#[test]
fn check_identity_rejects_missing_file_blobs() -> anyhow::Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let objects = ObjectStore::open(data.path())?;
    let baseline = forge_git::checkpoint::BaseFile {
        object_id: "1725b6e1be1f349e0250000824d92d3dc2193e64".into(),
        mode: 0o100644,
        checkout: Default::default(),
    };
    assert!(
        objects
            .baseline_identity(repository.path(), &baseline)
            .is_err()
    );
    Ok(())
}

/// Committing verified bytes must not invalidate their check identity.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn check_identity_survives_committing_unchanged_worktree() -> anyhow::Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let objects = ObjectStore::open(data.path())?;
    let repositories = Arc::new(RepositoryStore::default());
    let checkpoint = GitCheckpoint::new(repository.path());
    fs::write(repository.path().join("tracked.txt"), "verified content\n")?;
    let dirty = checkpoint
        .capture(&objects, &repositories, "checks", 1)
        .await?;
    let identity = dirty.check_identity(&objects, None, &[])?;
    let repeated = checkpoint
        .capture(&objects, &repositories, "checks", 2)
        .await?;
    assert_eq!(identity, repeated.check_identity(&objects, None, &[])?);
    forge_git::test_support::stage_and_commit(
        repository.path(),
        &["tracked.txt"],
        "verified content",
    );
    let committed = checkpoint
        .capture(&objects, &repositories, "checks", 3)
        .await?;
    assert!(dirty.equivalent(&committed, &objects)?);
    assert_eq!(
        identity,
        committed.check_identity(&objects, None, &[])?,
        "committing identical content invalidated verification"
    );
    fs::write(repository.path().join("tracked.txt"), "changed content\n")?;
    let changed = checkpoint
        .capture(&objects, &repositories, "checks", 4)
        .await?;
    assert_ne!(identity, changed.check_identity(&objects, None, &[])?);
    Ok(())
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
