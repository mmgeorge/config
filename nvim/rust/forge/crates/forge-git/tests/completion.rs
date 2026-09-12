mod support;

use forge_git::{
    completion::{CandidateLimits, RevisionBackend},
    store::RepositoryStore,
};
use std::{fs, path::Path, sync::Arc};
use support::git;

#[tokio::test]
async fn local_remote_ambiguous_and_symbolic_candidates_match_git() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    initialize(root);
    git(root, &["branch", "topic/nested"]);
    git(root, &["branch", "feature-é"]);
    git(root, &["tag", "main"]);
    git(root, &["update-ref", "refs/remotes/origin/main", "HEAD"]);
    git(
        root,
        &[
            "symbolic-ref",
            "refs/remotes/origin/HEAD",
            "refs/remotes/origin/main",
        ],
    );
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let candidates = repository
        .refresh_revisions(&store, CandidateLimits::default())
        .await
        .unwrap();
    let expected = git(
        root,
        &[
            "for-each-ref",
            "--format=%(refname:short)",
            "refs/heads",
            "refs/remotes",
        ],
    );
    let mut expected = expected
        .split(|byte| *byte == b'\n')
        .filter(|value| !value.is_empty())
        .map(<[u8]>::to_vec)
        .collect::<Vec<_>>();
    expected.sort_unstable();
    assert_eq!(candidates.values, expected);
    assert!(!candidates.truncated);
    assert_eq!(candidates.backend, RevisionBackend::GitForEachRef);
    assert!(Arc::ptr_eq(
        &candidates,
        &repository.current_revisions().unwrap()
    ));
    git(root, &["branch", "added", "HEAD"]);
    let refreshed = repository
        .refresh_revisions(&store, CandidateLimits::default())
        .await
        .unwrap();
    assert!(refreshed.revision > candidates.revision);
    assert_ne!(refreshed.reference_digest, candidates.reference_digest);
    assert!(refreshed.values.contains(&b"added".to_vec()));
    assert!(!candidates.values.contains(&b"added".to_vec()));
    let limited = repository
        .refresh_revisions(&store, CandidateLimits::new(2, 2048).unwrap())
        .await
        .unwrap();
    assert_eq!(limited.values.len(), 2);
    assert!(limited.truncated);
    let empty = repository
        .refresh_revisions(&store, CandidateLimits::new(0, 0).unwrap())
        .await
        .unwrap();
    assert!(empty.values.is_empty() && empty.truncated);
}

#[tokio::test]
async fn failed_refresh_preserves_previously_accepted_candidates() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    initialize(root);
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let accepted = repository
        .refresh_revisions(&store, CandidateLimits::default())
        .await
        .unwrap();
    fs::write(root.join(".git/config"), b"[invalid\n").unwrap();
    let failure = repository
        .refresh_revisions(&store, CandidateLimits::default())
        .await
        .unwrap_err();
    assert!(
        failure
            .to_string()
            .contains("Git revision enumeration failed")
    );
    assert!(Arc::ptr_eq(
        &accepted,
        &repository.current_revisions().unwrap()
    ));
}

#[tokio::test]
async fn unborn_and_bare_repositories_return_valid_snapshots() {
    let fixture = tempfile::tempdir().unwrap();
    let store = RepositoryStore::default();
    for bare in [false, true] {
        let root = fixture.path().join(if bare { "bare" } else { "worktree" });
        fs::create_dir(&root).unwrap();
        if bare {
            git(&root, &["init", "--quiet", "--bare"]);
        } else {
            git(&root, &["init", "--quiet"]);
        }
        let repository = store.open(root).await.unwrap().unwrap();
        let candidates = repository
            .refresh_revisions(&store, CandidateLimits::default())
            .await
            .unwrap();
        assert!(candidates.values.is_empty());
        assert!(!candidates.truncated);
    }
}

#[tokio::test]
async fn linked_worktree_storage_invalidation_clears_each_candidate_cache() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path().join("main");
    fs::create_dir(&root).unwrap();
    initialize(&root);
    let linked = fixture.path().join("linked");
    git(
        &root,
        &[
            "worktree",
            "add",
            "--quiet",
            "--detach",
            linked.to_str().unwrap(),
        ],
    );
    let store = RepositoryStore::default();
    let main = store.open(root).await.unwrap().unwrap();
    let linked = store.open(linked).await.unwrap().unwrap();
    let first = main
        .refresh_revisions(&store, CandidateLimits::default())
        .await
        .unwrap();
    let second = linked
        .refresh_revisions(&store, CandidateLimits::default())
        .await
        .unwrap();
    assert_eq!(first.values, second.values);
    store.invalidate_storage(&main.identity.storage).unwrap();
    assert!(main.current_revisions().is_none());
    assert!(linked.current_revisions().is_none());
}

fn initialize(root: &Path) {
    git(root, &["init", "--quiet", "--initial-branch=main"]);
    fs::write(root.join("file"), b"base\n").unwrap();
    git(root, &["add", "file"]);
    git(
        root,
        &[
            "-c",
            "user.name=Forge Test",
            "-c",
            "user.email=forge@example.test",
            "commit",
            "--quiet",
            "-m",
            "base",
        ],
    );
}
