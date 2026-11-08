use forge_git::test_support as support;

use forge_git::{completion::RevisionProvider, store::RepositoryStore};
use std::{fs, path::Path, sync::Arc};
use support::{
    create_branch, create_tag, git, head_commit, init_repository, set_reference,
    set_symbolic_reference, stage_and_commit,
};

#[tokio::test]
async fn local_remote_ambiguous_and_symbolic_candidates_match_git() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    initialize(root);
    create_branch(root, "topic/nested");
    create_branch(root, "feature-é");
    create_tag(root, "main");
    set_reference(root, "refs/remotes/origin/main", head_commit(root));
    set_symbolic_reference(root, "refs/remotes/origin/HEAD", "refs/remotes/origin/main");
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let candidates = repository.refresh_revisions(&store).await.unwrap();
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
    assert_eq!(candidates.provider, RevisionProvider::Gix);
    assert!(Arc::ptr_eq(
        &candidates,
        &repository.current_revisions().unwrap()
    ));
    create_branch(root, "added");
    let refreshed = repository.refresh_revisions(&store).await.unwrap();
    assert!(refreshed.revision > candidates.revision);
    assert_ne!(refreshed.reference_digest, candidates.reference_digest);
    assert!(refreshed.values.contains(&b"added".to_vec()));
    assert!(!candidates.values.contains(&b"added".to_vec()));
}

fn initialize(root: &Path) {
    init_repository(root);
    fs::write(root.join("file"), b"base\n").unwrap();
    stage_and_commit(root, &["file"], "base");
}
