use forge_git::test_support as support;

use std::sync::Arc;

use forge_git::{
    snapshot::{HeadState, PathState, WorktreeStamp},
    store::RepositoryStore,
};
use support::{init_repository, stage, stage_and_commit};

#[tokio::test]
async fn observation_adopts_unborn_then_born_head_and_distinct_sources() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    init_repository(root);
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let unborn = repository.observe(&store).await.unwrap();
    assert!(
        matches!(&unborn.head, HeadState::Unborn { reference } if reference == b"refs/heads/main")
    );
    assert!(unborn.index.index.is_none());
    assert!(unborn.path.is_empty());
    std::fs::write(root.join("tracked"), "base\n").unwrap();
    stage_and_commit(root, &["tracked"], "base");
    std::fs::write(root.join("tracked"), "staged\n").unwrap();
    stage(root, &["tracked"]);
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
