mod support;

use forge_git::{
    RepositoryPath,
    revision::{RevisionOrigin, resolve_file},
    store::RepositoryStore,
};
use std::fs;
use support::git;

#[tokio::test]
async fn file_revision_resolves_commit_and_index_without_starting_git() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    git(root, &["config", "user.email", "fixture@example.test"]);
    git(root, &["config", "user.name", "Fixture"]);
    fs::create_dir(root.join("nested")).unwrap();
    fs::write(root.join("nested/file.txt"), b"committed\n").unwrap();
    git(root, &["add", "nested/file.txt"]);
    git(root, &["commit", "--quiet", "-m", "initial"]);
    let original = git(root, &["rev-parse", "HEAD:nested/file.txt"]);
    fs::write(root.join("nested/file.txt"), b"staged\n").unwrap();
    git(root, &["add", "nested/file.txt"]);
    let staged = git(root, &["rev-parse", ":0:nested/file.txt"]);

    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let path = RepositoryPath::new(b"nested/file.txt".to_vec()).unwrap();
    let commit = resolve_file(&store, repository.clone(), "HEAD".into(), path.clone())
        .await
        .unwrap()
        .value;
    assert!(matches!(commit.origin, RevisionOrigin::Commit(_)));
    assert_eq!(commit.blob.to_string().as_bytes(), original.trim_ascii());
    let index = resolve_file(&store, repository.clone(), ":0".into(), path.clone())
        .await
        .unwrap()
        .value;
    assert!(matches!(index.origin, RevisionOrigin::Index));
    assert_eq!(index.blob.to_string().as_bytes(), staged.trim_ascii());
    let missing = resolve_file(
        &store,
        repository,
        "HEAD".into(),
        RepositoryPath::new(b"missing.txt".to_vec()).unwrap(),
    )
    .await
    .err()
    .unwrap();
    assert!(missing.to_string().contains("revision file is unavailable"));
}
