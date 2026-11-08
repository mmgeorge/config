use forge_git::test_support as support;

use std::sync::Arc;

use forge_git::store::RepositoryStore;
use support::init_repository;

fn fixture() -> tempfile::TempDir {
    let root = tempfile::tempdir().unwrap();
    init_repository(root.path());
    root
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
