use forge_github::model::GithubRepositoryId;
use forge_github::queue::{RemoteCompletion, RemoteIntent, RemoteQueue, RemoteResource};

fn resource(hostname: &str, number: u64) -> RemoteResource {
    RemoteResource::new(
        GithubRepositoryId::new(hostname, "Owner", "Repo").unwrap(),
        number,
    )
    .unwrap()
}

#[tokio::test]
async fn unknown_completion_rejects_waiting_writes_until_reconciliation() {
    let queue = RemoteQueue::default();
    let key = resource("github.com", 1);
    let mut active = queue
        .enqueue(key.clone(), RemoteIntent::Mutation, 0)
        .unwrap()
        .ready()
        .await
        .unwrap();
    let waiting = queue
        .enqueue(key.clone(), RemoteIntent::Mutation, 0)
        .unwrap();
    let reconcile = queue
        .enqueue(key.clone(), RemoteIntent::Reconcile, 0)
        .unwrap();
    active.begin_mutation().unwrap();
    drop(active);
    assert!(
        waiting
            .ready()
            .await
            .err()
            .unwrap()
            .to_string()
            .contains("OutcomeUnknown")
    );
    assert!(
        queue
            .enqueue(key.clone(), RemoteIntent::Mutation, 0)
            .is_err()
    );
    reconcile
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Unchanged)
        .unwrap();
    assert!(
        queue
            .enqueue(key.clone(), RemoteIntent::Mutation, 0)
            .is_err()
    );
    queue
        .enqueue(key.clone(), RemoteIntent::Reconcile, 0)
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Reconciled)
        .unwrap();
    queue
        .enqueue(key, RemoteIntent::Mutation, 0)
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Confirmed)
        .unwrap();
}
