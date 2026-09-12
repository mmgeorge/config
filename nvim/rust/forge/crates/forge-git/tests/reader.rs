mod support;

use std::sync::Arc;

use forge_git::{
    snapshot::{HeadState, PathState, WorktreeStamp},
    store::RepositoryStore,
};
use support::git;

#[tokio::test]
async fn concurrent_document_observations_each_complete() {
    let fixture = tempfile::tempdir().unwrap();
    git(
        fixture.path(),
        &["init", "--quiet", "--initial-branch=main"],
    );
    let store = RepositoryStore::default();
    let repository = store
        .open(fixture.path().to_path_buf())
        .await
        .unwrap()
        .unwrap();
    let (first, second, third) = tokio::join!(
        repository.observe(&store),
        repository.observe(&store),
        repository.observe(&store),
    );
    let observation = [first.unwrap(), second.unwrap(), third.unwrap()];
    let newest = observation.iter().max_by_key(|value| value.id).unwrap();
    assert!(Arc::ptr_eq(
        newest,
        &repository.current_observation().unwrap()
    ));
    assert!(
        observation
            .iter()
            .all(|value| value.generation == repository.generation())
    );
    assert_ne!(observation[0].id, observation[1].id);
    assert_ne!(observation[1].id, observation[2].id);
}

#[tokio::test]
async fn observation_adopts_unborn_then_born_head_and_distinct_sources() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet", "--initial-branch=main"]);
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let unborn = repository.observe(&store).await.unwrap();
    assert!(
        matches!(&unborn.head, HeadState::Unborn { reference } if reference == b"refs/heads/main")
    );
    assert!(unborn.index.index.is_none());
    assert!(unborn.path.is_empty());
    std::fs::write(root.join("tracked"), "base\n").unwrap();
    git(root, &["add", "tracked"]);
    commit(root);
    std::fs::write(root.join("tracked"), "staged\n").unwrap();
    git(root, &["add", "tracked"]);
    std::fs::write(root.join("tracked"), "unstaged contents\n").unwrap();
    let observed = repository.observe(&store).await.unwrap();
    assert!(observed.id > unborn.id);
    assert!(
        matches!(&observed.head, HeadState::Attached { reference, .. } if reference == b"refs/heads/main")
    );
    assert!(observed.index.index.is_some());
    assert_eq!(observed.path.len(), 1);
    let PathState::Tracked { head, index, .. } = &observed.path[0].change.state else {
        panic!()
    };
    assert_ne!(head.object, index.object);
    let WorktreeStamp::File(metadata) = &observed.path[0].worktree else {
        panic!()
    };
    assert_eq!(metadata.length, 18);
    assert!(Arc::ptr_eq(
        &observed,
        &repository.current_observation().unwrap()
    ));
    repository.invalidate().unwrap();
    assert!(repository.current_observation().is_none());
}

#[tokio::test]
async fn split_index_backing_files_are_part_of_the_observation() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    std::fs::write(root.join("tracked"), "base\n").unwrap();
    git(root, &["add", "tracked"]);
    git(root, &["update-index", "--split-index"]);
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let observed = repository.observe(&store).await.unwrap();
    assert!(observed.index.index.is_some());
    assert!(!observed.index.shared.is_empty());
    assert_eq!(observed.path.len(), 1);
}

#[tokio::test]
async fn linked_detached_worktrees_keep_independent_observations() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path().join("main");
    std::fs::create_dir(&root).unwrap();
    git(&root, &["init", "--quiet", "--initial-branch=main"]);
    std::fs::write(root.join("tracked"), "base\n").unwrap();
    git(&root, &["add", "tracked"]);
    commit(&root);
    let linked = fixture.path().join("linked");
    git(
        &root,
        &[
            "worktree",
            "add",
            "--detach",
            linked.to_str().unwrap(),
            "HEAD",
        ],
    );
    std::fs::write(root.join("tracked"), "modified\n").unwrap();
    let store = RepositoryStore::default();
    let main = store.open(root).await.unwrap().unwrap();
    let detached = store.open(linked).await.unwrap().unwrap();
    let observed_main = main.observe(&store).await.unwrap();
    let observed_detached = detached.observe(&store).await.unwrap();
    assert_eq!(main.identity.storage, detached.identity.storage);
    assert_ne!(observed_main.worktree, observed_detached.worktree);
    assert!(matches!(observed_detached.head, HeadState::Detached { .. }));
    assert_eq!(observed_main.path.len(), 1);
    assert!(observed_detached.path.is_empty());
    store.invalidate_storage(&main.identity.storage).unwrap();
    assert!(main.current_observation().is_none());
    assert!(detached.current_observation().is_none());
}

#[tokio::test]
async fn rename_observation_leaves_worktree_metadata_unobserved() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    std::fs::write(root.join("origin"), "base\n").unwrap();
    git(root, &["add", "origin"]);
    commit(root);
    git(root, &["mv", "origin", "destination"]);
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let observed = repository.observe(&store).await.unwrap();
    assert_eq!(observed.path.len(), 1);
    assert_eq!(observed.path[0].origin, Some(WorktreeStamp::Unobserved));
    assert_eq!(observed.path[0].worktree, WorktreeStamp::Unobserved);
}

#[tokio::test]
async fn unstaged_only_observation_preserves_head_baseline() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    std::fs::write(root.join("tracked"), "before\n").unwrap();
    git(root, &["add", "tracked"]);
    commit(root);
    std::fs::write(root.join("tracked"), "after\n").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let observation = repository.observe(&store).await.unwrap();
    let PathState::Tracked { head, index, .. } = &observation.path[0].change.state else {
        panic!("tracked file lost its baseline");
    };
    assert_eq!(head, index);
    assert!(!head.object.is_null());
}

fn commit(root: &std::path::Path) {
    git(
        root,
        &[
            "-c",
            "user.name=Forge",
            "-c",
            "user.email=forge@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "base",
        ],
    );
}
