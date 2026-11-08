use forge_git::test_support as support;
use support::{commit_index, git, init_repository, move_tracked, stage};

use forge_git::snapshot::{ChangeKind, PathState, parse_porcelain_v2};

fn parse(input: &[u8]) -> anyhow::Result<Vec<forge_git::snapshot::PathRecord>> {
    parse_porcelain_v2(input, gix::hash::Kind::Sha1)
}

fn tracked(kind: &str, pair: &str, suffix: &str) -> Vec<u8> {
    format!(
        "{kind} {pair} N... 100644 100755 100755 {} {} {suffix}\0",
        "1".repeat(40),
        "2".repeat(40)
    )
    .into_bytes()
}

#[test]
fn preserves_independent_changes_modes_and_raw_paths() {
    let mut input = tracked("1", "MM", "a space\n");
    input.insert(input.len() - 1, 0xff);
    let record = parse(&input).unwrap().remove(0);
    assert_eq!(record.staged, ChangeKind::Modified);
    assert_eq!(record.unstaged, ChangeKind::Modified);
    assert_eq!(record.path.raw(), b"a space\n\xff");
    let PathState::Tracked {
        head,
        index,
        relocation,
    } = record.state
    else {
        panic!()
    };
    assert_eq!(head.mode, 0o100644);
    assert_eq!(index.mode, 0o100755);
    assert_ne!(head.object, index.object);
    assert!(relocation.is_none());
}

#[test]
fn rejects_truncation_invalid_metadata_and_limits() {
    let valid = tracked("1", "M.", "path");
    assert!(parse(&valid[..valid.len() - 1]).is_err());
    assert!(parse(&tracked("1", "R.", "path")).is_err());
    assert!(parse(&tracked("2", "R.", "R101 path\0origin")).is_err());
    assert!(parse(&tracked("2", "R.", "C100 path\0origin")).is_err());
    assert!(parse(&tracked("2", "R.", "R100 path")).is_err());
    assert!(parse(b"? ../escape\0").is_err());
    assert!(parse(b"\0").is_err());
    assert!(parse(b"? repeated\0? repeated\0").is_err());
    assert!(parse_porcelain_v2(&valid, gix::hash::Kind::Sha256).is_err());
    assert!(parse_porcelain_v2(&valid, gix::hash::Kind::Sha1).is_ok());
}

#[test]
fn reads_real_git_staged_unstaged_rename_and_unborn_output() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    init_repository(root);
    std::fs::write(root.join("tracked"), "initial\n").unwrap();
    stage(root, &["tracked"]);
    let unborn = status(root);
    assert_eq!(unborn.len(), 1);
    assert_eq!(unborn[0].staged, ChangeKind::Added);
    let PathState::Tracked { head, .. } = &unborn[0].state else {
        panic!()
    };
    assert!(head.object.is_null());
    commit_index(root, "initial");
    std::fs::write(root.join("tracked"), "staged\n").unwrap();
    stage(root, &["tracked"]);
    std::fs::write(root.join("tracked"), "unstaged\n").unwrap();
    let changed = status(root);
    assert_eq!(changed.len(), 1);
    assert_eq!(
        (changed[0].staged, changed[0].unstaged),
        (ChangeKind::Modified, ChangeKind::Modified)
    );
    std::fs::write(root.join("tracked"), "initial\n").unwrap();
    stage(root, &["tracked"]);
    move_tracked(root, "tracked", "renamed with space");
    std::fs::write(root.join("empty"), "").unwrap();
    let renamed = status(root);
    assert_eq!(renamed.len(), 2);
    let record = renamed
        .iter()
        .find(|record| record.path.raw() == b"renamed with space")
        .unwrap();
    let PathState::Tracked {
        relocation: Some(relocation),
        ..
    } = &record.state
    else {
        panic!()
    };
    assert_eq!(relocation.origin.raw(), b"tracked");
    assert_eq!(relocation.similarity, 100);
}

fn status(root: &std::path::Path) -> Vec<forge_git::snapshot::PathRecord> {
    parse(&git(
        root,
        &[
            "status",
            "--porcelain=v2",
            "-z",
            "--untracked-files=all",
            "--ignore-submodules=none",
            "--find-renames=50%",
        ],
    ))
    .unwrap()
}
