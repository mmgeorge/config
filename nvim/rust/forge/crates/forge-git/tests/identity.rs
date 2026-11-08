use forge_git::RepositoryPath;

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
