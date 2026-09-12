mod support;
use support::git;

use forge_git::{
    RepositoryPath,
    snapshot::{ChangeKind, PathState, parse_porcelain_v2},
};

fn parse(input: &[u8]) -> anyhow::Result<Vec<forge_git::snapshot::PathRecord>> {
    parse_porcelain_v2(input, gix::hash::Kind::Sha1, 4096, 32)
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
fn rename_affects_origin_but_copy_does_not() {
    for (pair, score, affects_origin) in [("RM", "R100", true), ("C.", "C75", false)] {
        let mut input = tracked("2", pair, &format!("{score} destination"));
        input.extend_from_slice(b"origin\0? next\0");
        let record = parse(&input).unwrap();
        assert_eq!(record.len(), 2);
        assert_eq!(
            record[0].affects(&RepositoryPath::new(b"origin".to_vec()).unwrap()),
            affects_origin
        );
        assert!(record[0].affects(&RepositoryPath::new(b"destination".to_vec()).unwrap()));
        assert!(matches!(record[1].state, PathState::Untracked));
    }
}

#[test]
fn preserves_conflict_stages_and_submodule_flags() {
    let input = format!(
        "u UU SCMU 100644 100755 120000 160000 {} {} {} conflict\0",
        "1".repeat(40),
        "2".repeat(40),
        "3".repeat(40)
    );
    let record = parse(input.as_bytes()).unwrap().remove(0);
    let PathState::Conflict { base, ours, theirs } = record.state else {
        panic!()
    };
    assert_eq!(
        (base.mode, ours.mode, theirs.mode),
        (0o100644, 0o100755, 0o120000)
    );
    assert_eq!(record.worktree_mode, Some(0o160000));
    let submodule = record.submodule.unwrap();
    assert!(submodule.commit_changed && submodule.tracked_changed && submodule.untracked_changed);
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
    assert!(parse_porcelain_v2(&valid, gix::hash::Kind::Sha256, 4096, 32).is_err());
    assert!(parse_porcelain_v2(&valid, gix::hash::Kind::Sha1, valid.len() - 1, 32).is_err());
    assert!(parse_porcelain_v2(&valid, gix::hash::Kind::Sha1, 4096, 0).is_err());
}

#[test]
fn permits_empty_status_unknown_headers_and_sha256() {
    assert!(parse(b"").unwrap().is_empty());
    assert!(parse(b"# future extension\0").unwrap().is_empty());
    let input = format!(
        "1 A. N... 000000 100644 100644 {} {} empty\0",
        "0".repeat(64),
        "a".repeat(64)
    );
    assert_eq!(
        parse_porcelain_v2(input.as_bytes(), gix::hash::Kind::Sha256, 4096, 1)
            .unwrap()
            .len(),
        1
    );
}

#[test]
fn reads_real_git_staged_unstaged_rename_and_unborn_output() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    git(root, &["init", "--quiet"]);
    std::fs::write(root.join("tracked"), "initial\n").unwrap();
    git(root, &["add", "tracked"]);
    let unborn = status(root);
    assert_eq!(unborn.len(), 1);
    assert_eq!(unborn[0].staged, ChangeKind::Added);
    let PathState::Tracked { head, .. } = &unborn[0].state else {
        panic!()
    };
    assert!(head.object.is_null());
    git(
        root,
        &[
            "-c",
            "user.name=Forge",
            "-c",
            "user.email=forge@example.invalid",
            "commit",
            "--quiet",
            "-m",
            "initial",
        ],
    );
    std::fs::write(root.join("tracked"), "staged\n").unwrap();
    git(root, &["add", "tracked"]);
    std::fs::write(root.join("tracked"), "unstaged\n").unwrap();
    let changed = status(root);
    assert_eq!(changed.len(), 1);
    assert_eq!(
        (changed[0].staged, changed[0].unstaged),
        (ChangeKind::Modified, ChangeKind::Modified)
    );
    git(root, &["reset", "--hard", "HEAD"]);
    git(root, &["mv", "tracked", "renamed with space"]);
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

#[tokio::test]
async fn process_reader_uses_store_admission_and_reports_backend() {
    let directory = tempfile::tempdir().unwrap();
    git(directory.path(), &["init", "--quiet"]);
    std::fs::write(directory.path().join("new file"), "content\n").unwrap();
    let store = forge_git::store::RepositoryStore::new(4, 1, 1024 * 1024).unwrap();
    let repository = store
        .open(directory.path().to_path_buf())
        .await
        .unwrap()
        .unwrap();
    let generation = repository.generation();
    let status = forge_git::reader::StatusReader::status(&store, repository)
        .await
        .unwrap();
    assert_eq!(status.generation, generation);
    assert_eq!(status.value.backend, forge_git::reader::StatusBackend::Gix);
    assert_eq!(status.value.path.len(), 1);
    assert_eq!(status.value.path[0].path.raw(), b"new file");
    assert_eq!(
        status.value.path[0].content,
        forge_git::snapshot::ContentClassification::Unknown
    );
    assert_eq!(
        status.value.path[0].staged_stats,
        forge_git::snapshot::LineStats::Unknown
    );
}

#[tokio::test]
async fn process_reader_supports_sha256_and_rejects_bare_repositories() {
    let directory = tempfile::tempdir().unwrap();
    git(
        directory.path(),
        &["init", "--quiet", "--object-format=sha256"],
    );
    std::fs::write(directory.path().join("staged"), "content\n").unwrap();
    git(directory.path(), &["add", "staged"]);
    let store = forge_git::store::RepositoryStore::new(4, 1, 1024 * 1024).unwrap();
    let repository = store
        .open(directory.path().to_path_buf())
        .await
        .unwrap()
        .unwrap();
    let status = forge_git::reader::StatusReader::status(&store, repository)
        .await
        .unwrap();
    let PathState::Tracked { index, .. } = &status.value.path[0].state else {
        panic!()
    };
    assert_eq!(index.object.kind(), gix::hash::Kind::Sha256);
    let bare = tempfile::tempdir().unwrap();
    git(bare.path(), &["init", "--quiet", "--bare"]);
    let repository = store
        .open(bare.path().to_path_buf())
        .await
        .unwrap()
        .unwrap();
    let error = forge_git::reader::StatusReader::status(&store, repository)
        .await
        .err()
        .unwrap();
    assert!(error.to_string().contains("requires a worktree"));
}

#[test]
fn reads_real_git_conflict_stage_objects() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    git(root, &["init", "--quiet"]);
    std::fs::write(root.join("conflicted"), "base\n").unwrap();
    git(root, &["add", "conflicted"]);
    git(
        root,
        &[
            "-c",
            "user.name=Forge",
            "-c",
            "user.email=forge@example.invalid",
            "commit",
            "--quiet",
            "-m",
            "base",
        ],
    );
    let base = git(root, &["rev-parse", "HEAD:conflicted"]);
    std::fs::write(root.join("conflicted"), "ours\n").unwrap();
    let ours = git(root, &["hash-object", "-w", "conflicted"]);
    std::fs::write(root.join("conflicted"), "theirs\n").unwrap();
    let theirs = git(root, &["hash-object", "-w", "conflicted"]);
    let stage = [(1, base), (2, ours), (3, theirs)]
        .map(|(number, object)| {
            format!(
                "100644 {} {number}\tconflicted\n",
                String::from_utf8(object).unwrap().trim()
            )
        })
        .concat();
    // Build a conflicted index without depending on branch names or merge drivers.
    git(root, &["update-index", "--force-remove", "conflicted"]);
    let mut child = std::process::Command::new("git")
        .arg("-C")
        .arg(root)
        .args(["update-index", "--index-info"])
        .stdin(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    use std::io::Write;
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stage.as_bytes())
        .unwrap();
    assert!(child.wait().unwrap().success());
    let record = status(root);
    let PathState::Conflict { base, ours, theirs } = &record[0].state else {
        panic!()
    };
    assert_ne!(base.object, ours.object);
    assert_ne!(ours.object, theirs.object);
    assert_eq!(
        (record[0].staged, record[0].unstaged),
        (ChangeKind::Unmerged, ChangeKind::Unmerged)
    );
}
