use std::{path::Path, process::Command};

use forge_git::{RepositoryPath, discover_identity, resolve_argument, validate_path};

fn git(root: &Path, arguments: &[&str]) {
    let output = Command::new("git")
        .arg("-c")
        .arg("commit.gpgsign=false")
        .arg("-c")
        .arg(format!(
            "core.hooksPath={}",
            root.join("disabled-fixture-hooks").display()
        ))
        .arg("-C")
        .arg(root)
        .args(arguments)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn unborn_worktree_identity_is_stable_from_nested_paths() {
    let root = tempfile::tempdir().unwrap();
    git(root.path(), &["init", "--quiet"]);
    let nested = root.path().join("nested");
    std::fs::create_dir(&nested).unwrap();
    let file = nested.join("file.txt");
    std::fs::write(&file, "content").unwrap();
    let identity = discover_identity(root.path()).unwrap().unwrap();
    assert_eq!(identity, discover_identity(&nested).unwrap().unwrap());
    assert_eq!(identity, discover_identity(&file).unwrap().unwrap());
    assert_eq!(
        identity.worktree_root,
        Some(dunce::canonicalize(root.path()).unwrap())
    );
    assert_eq!(identity.git_directory, identity.common_directory);
    assert!(!identity.index.exists());
    assert!(identity.worktree.is_some());
}

#[test]
fn linked_worktrees_have_distinct_indexes_and_shared_storage_identity() {
    let root = tempfile::tempdir().unwrap();
    let main = root.path().join("main");
    std::fs::create_dir(&main).unwrap();
    git(&main, &["init", "--quiet"]);
    git(
        &main,
        &[
            "-c",
            "user.name=Forge fixture",
            "-c",
            "user.email=fixture@example.invalid",
            "commit",
            "--quiet",
            "--allow-empty",
            "-m",
            "initial",
        ],
    );
    git(
        &main,
        &["worktree", "add", "--quiet", "--detach", "../linked"],
    );
    let original = discover_identity(&main).unwrap().unwrap();
    let linked = discover_identity(&root.path().join("linked"))
        .unwrap()
        .unwrap();
    assert_ne!(original.worktree, linked.worktree);
    assert_ne!(original.index, linked.index);
    assert_ne!(original.git_directory, linked.git_directory);
    assert_eq!(original.storage, linked.storage);
    assert_eq!(original.common_directory, linked.common_directory);
}

#[test]
fn bare_repository_has_storage_without_a_worktree() {
    let root = tempfile::tempdir().unwrap();
    git(root.path(), &["init", "--bare", "--quiet"]);
    let identity = discover_identity(root.path()).unwrap().unwrap();
    assert!(identity.worktree.is_none());
    assert!(identity.worktree_root.is_none());
    assert_eq!(
        identity.common_directory,
        dunce::canonicalize(root.path()).unwrap()
    );
}

#[test]
fn paths_preserve_spaces_unicode_and_literal_argument_boundaries() {
    let root = tempfile::tempdir().unwrap();
    for raw in ["directory/file name.txt", "-leading-option", "🦀/source.rs"] {
        let path = RepositoryPath::new(raw.as_bytes().to_vec()).unwrap();
        assert_eq!(path.raw(), raw.as_bytes());
        assert_eq!(path.display_label(), raw);
        assert_eq!(resolve_argument(&path).unwrap(), raw);
        assert_eq!(
            validate_path(root.path(), &path).unwrap(),
            dunce::canonicalize(root.path()).unwrap().join(raw)
        );
    }
}

#[test]
fn paths_reject_empty_absolute_and_escaping_components() {
    for raw in [
        b"".as_slice(),
        b"/absolute",
        b"../outside",
        b"a/../outside",
        b"a//b",
        b"a/./b",
        b"a/",
        b"a\0b",
    ] {
        assert!(RepositoryPath::new(raw.to_vec()).is_err(), "{raw:?}");
    }
}

#[cfg(windows)]
#[test]
fn windows_paths_reject_native_aliases_and_devices() {
    for raw in [
        "C:/outside",
        "a\\..\\outside",
        "file:stream",
        "name.",
        "name ",
        "CON",
        "con.txt",
        "COM1",
        "LPT¹",
    ] {
        let path = RepositoryPath::new(raw.as_bytes().to_vec()).unwrap();
        assert!(resolve_argument(&path).is_err(), "{raw}");
    }
    let path = RepositoryPath::new(vec![0xff]).unwrap();
    assert!(resolve_argument(&path).is_err());
    assert_eq!(path.raw(), &[0xff]);
}

#[cfg(unix)]
#[test]
fn invalid_utf8_paths_keep_raw_identity_separate_from_display() {
    use std::os::unix::ffi::OsStrExt;
    let first = RepositoryPath::new(vec![b'a', 0xff]).unwrap();
    let second = RepositoryPath::new(vec![b'a', 0xfe]).unwrap();
    assert_ne!(first, second);
    assert_eq!(first.display_label(), second.display_label());
    assert_eq!(resolve_argument(&first).unwrap().as_bytes(), first.raw());
}

#[cfg(unix)]
#[test]
fn path_validation_rejects_escaping_parents_without_following_leaf_symlinks() {
    use std::os::unix::fs::symlink;
    let root = tempfile::tempdir().unwrap();
    let outside = tempfile::tempdir().unwrap();
    symlink(outside.path(), root.path().join("escape")).unwrap();
    symlink(outside.path().join("missing"), root.path().join("leaf")).unwrap();
    assert!(
        validate_path(
            root.path(),
            &RepositoryPath::new(b"escape/file".to_vec()).unwrap()
        )
        .is_err()
    );
    assert!(validate_path(root.path(), &RepositoryPath::new(b"leaf".to_vec()).unwrap()).is_ok());
}

#[test]
fn path_validation_rejects_a_file_as_an_existing_parent() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(root.path().join("parent"), "file").unwrap();
    let path = RepositoryPath::new(b"parent/child".to_vec()).unwrap();
    assert!(validate_path(root.path(), &path).is_err());
}

#[test]
fn missing_repository_and_invalid_repository_configuration_are_distinct() {
    let root = tempfile::tempdir().unwrap();
    assert!(discover_identity(root.path()).unwrap().is_none());
    git(root.path(), &["init", "--quiet"]);
    std::fs::write(root.path().join(".git/config"), "[broken\n").unwrap();
    assert!(discover_identity(root.path()).is_err());
}
