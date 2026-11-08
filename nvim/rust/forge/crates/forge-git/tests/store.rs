use forge_git::test_support as support;

use std::sync::Arc;
use std::time::Duration;

use forge_git::store::RepositoryStore;
use support::{
    add_detached_worktree, commit_index, head_commit, init_bare_repository, init_repository,
};

fn fixture() -> tempfile::TempDir {
    let root = tempfile::tempdir().unwrap();
    init_repository(root.path());
    root
}

#[tokio::test]
async fn concurrent_discovery_reuses_one_worktree_handle() {
    let root = fixture();
    let nested = root.path().join("nested");
    std::fs::create_dir(&nested).unwrap();
    let store = RepositoryStore::new(1, 2).unwrap();
    let (first, second) = tokio::join!(store.open(root.path().to_owned()), store.open(nested));
    let first = first.unwrap().unwrap();
    let second = second.unwrap().unwrap();
    assert!(Arc::ptr_eq(&first, &second));
    let read = store
        .read(first.clone(), 0, |repository, _| {
            Ok(repository.git_dir().to_owned())
        })
        .await
        .unwrap();
    assert_eq!(
        dunce::canonicalize(read.value).unwrap(),
        first.identity.git_directory
    );
    assert_eq!(read.generation, first.generation());
    assert_eq!(store.evict_idle(), 0);
    drop(first);
    drop(second);
    assert_eq!(store.evict_idle(), 1);
}

#[tokio::test]
async fn capacity_rejects_active_leases_and_evicts_least_recent_idle_handle() {
    let first_root = fixture();
    let second_root = fixture();
    let third_root = fixture();
    let store = RepositoryStore::new(2, 2).unwrap();
    let first = store
        .open(first_root.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let second = store
        .open(second_root.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    assert!(store.open(third_root.path().to_owned()).await.is_err());
    let first_weak = Arc::downgrade(&first);
    let second_weak = Arc::downgrade(&second);
    drop(first);
    drop(second);
    drop(store.open(first_root.path().to_owned()).await.unwrap());
    let third = store
        .open(third_root.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    assert!(first_weak.upgrade().is_some());
    assert!(second_weak.upgrade().is_none());
    drop(third);
    assert_eq!(store.evict_idle(), 2);
}

#[tokio::test]
async fn invalidation_rejects_a_native_result_started_before_the_change() {
    let root = fixture();
    let store = Arc::new(RepositoryStore::new(1, 1).unwrap());
    let repository = store.open(root.path().to_owned()).await.unwrap().unwrap();
    let generation = repository.generation();
    let (started, started_receiver) = tokio::sync::oneshot::channel();
    let (release, release_receiver) = std::sync::mpsc::channel();
    let reader_store = store.clone();
    let reader_repository = repository.clone();
    let reader = tokio::spawn(async move {
        reader_store
            .read(reader_repository, 0, move |_, _| {
                started.send(()).unwrap();
                release_receiver.recv()?;
                Ok(42)
            })
            .await
    });
    started_receiver.await.unwrap();
    assert_eq!(
        store
            .invalidate_storage(&repository.identity.storage)
            .unwrap(),
        1
    );
    release.send(()).unwrap();
    let error = reader.await.unwrap().err().unwrap();
    assert!(error.to_string().contains("superseded"));
    assert_ne!(generation, repository.generation());
}

#[tokio::test]
async fn cancelled_waiter_cannot_evict_an_active_native_worker() {
    let root = fixture();
    let store = Arc::new(RepositoryStore::new(1, 1).unwrap());
    let repository = store.open(root.path().to_owned()).await.unwrap().unwrap();
    let (started, started_receiver) = tokio::sync::oneshot::channel();
    let (release, release_receiver) = std::sync::mpsc::channel();
    let reader_store = store.clone();
    let reader = tokio::spawn(async move {
        reader_store
            .read(repository, 0, move |_, _| {
                started.send(()).unwrap();
                release_receiver.recv()?;
                Ok(())
            })
            .await
    });
    started_receiver.await.unwrap();
    reader.abort();
    assert!(reader.await.err().unwrap().is_cancelled());
    assert_eq!(store.evict_idle(), 0);
    assert_eq!(store.shutdown(Duration::ZERO).await.unfinished.len(), 1);
    release.send(()).unwrap();
    assert!(
        store
            .shutdown(Duration::from_secs(5))
            .await
            .unfinished
            .is_empty()
    );
    assert_eq!(store.evict_idle(), 1);
    assert!(store.open(root.path().to_owned()).await.is_err());
}

#[tokio::test]
async fn foreign_store_leases_are_rejected() {
    let root = fixture();
    let owner = RepositoryStore::new(1, 1).unwrap();
    let other = RepositoryStore::new(1, 1).unwrap();
    let repository = owner.open(root.path().to_owned()).await.unwrap().unwrap();
    let _other_repository = other.open(root.path().to_owned()).await.unwrap().unwrap();
    assert!(other.read(repository, 0, |_, _| Ok(())).await.is_err());
}

#[tokio::test]
async fn linked_worktrees_share_invalidation_but_keep_separate_handles() {
    let root = fixture();
    commit_index(root.path(), "initial");
    let linked = root.path().join("linked");
    add_detached_worktree(root.path(), &linked, head_commit(root.path()));
    let unrelated_root = fixture();
    let store = RepositoryStore::new(3, 2).unwrap();
    let main = store.open(root.path().to_owned()).await.unwrap().unwrap();
    let linked = store.open(linked).await.unwrap().unwrap();
    let unrelated = store
        .open(unrelated_root.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    assert!(!Arc::ptr_eq(&main, &linked));
    assert_eq!(main.identity.storage, linked.identity.storage);
    let linked_generation = linked.generation();
    let unrelated_generation = unrelated.generation();
    assert_eq!(store.invalidate_storage(&main.identity.storage).unwrap(), 2);
    assert_ne!(linked_generation, linked.generation());
    assert_eq!(unrelated_generation, unrelated.generation());
}

#[tokio::test]
async fn bare_repository_and_missing_repository_remain_distinct() {
    let root = tempfile::tempdir().unwrap();
    let store = RepositoryStore::new(1, 1).unwrap();
    assert!(store.open(root.path().to_owned()).await.unwrap().is_none());
    init_bare_repository(root.path());
    let repository = store.open(root.path().to_owned()).await.unwrap().unwrap();
    assert!(repository.identity.worktree.is_none());
    assert!(
        store
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished
            .is_empty()
    );
    assert!(store.read(repository, 0, |_, _| Ok(())).await.is_err());
}
