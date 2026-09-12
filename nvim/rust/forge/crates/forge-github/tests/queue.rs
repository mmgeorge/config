use std::time::Duration;

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
async fn retained_input_bytes_bound_waiting_work_and_release_after_completion() {
    let queue = RemoteQueue::default();
    let key = resource("github.com", 1);
    let first = queue
        .enqueue(key.clone(), RemoteIntent::Mutation, 8 * 1024 * 1024)
        .unwrap()
        .ready()
        .await
        .unwrap();
    let second = queue
        .enqueue(key.clone(), RemoteIntent::Mutation, 8 * 1024 * 1024)
        .unwrap();
    assert!(
        queue
            .enqueue(resource("github.com", 2), RemoteIntent::Mutation, 1)
            .is_err()
    );
    assert!(
        queue
            .enqueue(key.clone(), RemoteIntent::Mutation, usize::MAX)
            .is_err()
    );
    first.complete(RemoteCompletion::Confirmed).unwrap();
    let third = queue
        .enqueue(key, RemoteIntent::Mutation, 8 * 1024 * 1024)
        .unwrap();
    second
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Confirmed)
        .unwrap();
    drop(third);
    assert!(
        queue
            .enqueue(
                resource("github.com", 2),
                RemoteIntent::Mutation,
                16 * 1024 * 1024
            )
            .is_ok()
    );
}

#[tokio::test]
async fn same_resource_is_fifo_and_other_hosts_and_numbers_are_independent() {
    let queue = RemoteQueue::default();
    let first = queue
        .enqueue(resource("github.com", 1), RemoteIntent::Mutation, 0)
        .unwrap()
        .ready()
        .await
        .unwrap();
    let second = queue
        .enqueue(resource("github.com", 1), RemoteIntent::Mutation, 0)
        .unwrap()
        .ready();
    let third = queue
        .enqueue(resource("github.com", 1), RemoteIntent::Mutation, 0)
        .unwrap()
        .ready();
    tokio::pin!(second, third);
    assert!(
        tokio::time::timeout(Duration::from_millis(1), &mut second)
            .await
            .is_err()
    );
    assert!(
        tokio::time::timeout(Duration::from_millis(1), &mut third)
            .await
            .is_err()
    );
    for key in [resource("enterprise.test", 1), resource("github.com", 2)] {
        queue
            .enqueue(key, RemoteIntent::Mutation, 0)
            .unwrap()
            .ready()
            .await
            .unwrap()
            .complete(RemoteCompletion::Confirmed)
            .unwrap();
    }
    first.complete(RemoteCompletion::Confirmed).unwrap();
    let second = second.await.unwrap();
    assert!(
        tokio::time::timeout(Duration::from_millis(1), &mut third)
            .await
            .is_err()
    );
    second.complete(RemoteCompletion::Confirmed).unwrap();
    third
        .await
        .unwrap()
        .complete(RemoteCompletion::Confirmed)
        .unwrap();
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

#[tokio::test]
async fn cancelled_waiter_does_not_release_the_active_owner_and_close_retains_it() {
    let queue = RemoteQueue::default();
    let key = resource("github.com", 1);
    let mut active = queue
        .enqueue(key.clone(), RemoteIntent::Mutation, 0)
        .unwrap()
        .ready()
        .await
        .unwrap();
    let cancelled = queue
        .enqueue(key.clone(), RemoteIntent::Mutation, 0)
        .unwrap();
    let waiting = queue
        .enqueue(key.clone(), RemoteIntent::Mutation, 0)
        .unwrap();
    drop(cancelled);
    queue.close();
    assert!(
        waiting
            .ready()
            .await
            .err()
            .unwrap()
            .to_string()
            .contains("closed")
    );
    assert!(queue.enqueue(key, RemoteIntent::Reconcile, 0).is_err());
    active.begin_mutation().unwrap();
    active.complete(RemoteCompletion::Confirmed).unwrap();
}

#[tokio::test]
async fn uncertain_resource_capacity_cannot_grow_and_existing_resources_can_reconcile() {
    let queue = RemoteQueue::default();
    for number in 1..=64 {
        queue
            .enqueue(resource("github.com", number), RemoteIntent::Mutation, 0)
            .unwrap()
            .ready()
            .await
            .unwrap()
            .complete(RemoteCompletion::Uncertain)
            .unwrap();
    }
    assert!(
        queue
            .enqueue(resource("github.com", 65), RemoteIntent::Mutation, 0)
            .is_err()
    );
    queue
        .enqueue(resource("github.com", 1), RemoteIntent::Reconcile, 0)
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Reconciled)
        .unwrap();
    queue
        .enqueue(resource("github.com", 65), RemoteIntent::Mutation, 0)
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Confirmed)
        .unwrap();
}

#[tokio::test]
async fn queued_capacity_is_reclaimed_after_cancellation_and_identity_is_canonical() {
    let queue = RemoteQueue::default();
    let key = resource("github.com", 1);
    let mut retained = Vec::new();
    for _ in 0..64 {
        retained.push(
            queue
                .enqueue(key.clone(), RemoteIntent::Mutation, 0)
                .unwrap(),
        );
    }
    assert!(
        queue
            .enqueue(resource("github.com", 2), RemoteIntent::Mutation, 0)
            .is_err()
    );
    retained.pop();
    assert!(
        queue
            .enqueue(resource("github.com", 2), RemoteIntent::Mutation, 0)
            .is_ok()
    );
    drop(retained);
    let canonical = RemoteResource::new(
        GithubRepositoryId::new("GITHUB.COM.", "OWNER", "REPO").unwrap(),
        1,
    )
    .unwrap();
    assert_eq!(key, canonical);
    assert!(
        RemoteResource::new(
            GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
            0
        )
        .is_err()
    );
}

#[tokio::test]
async fn unrelated_pr_or_comment_observation_cannot_clear_unknown_comment_write() {
    use forge_github::queue::RemoteScope;
    let queue = RemoteQueue::default();
    let target = resource("github.com", 7);
    let scope = RemoteScope::Comment("COMMENT_one".into());
    let mut write = queue
        .enqueue_scoped(target.clone(), scope.clone(), RemoteIntent::Mutation, 0)
        .unwrap()
        .ready()
        .await
        .unwrap();
    write.begin_mutation().unwrap();
    drop(write);
    queue
        .enqueue(target.clone(), RemoteIntent::Reconcile, 0)
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Reconciled)
        .unwrap();
    assert!(
        queue
            .enqueue(target.clone(), RemoteIntent::Mutation, 0)
            .is_err()
    );
    queue
        .enqueue_scoped(
            target.clone(),
            RemoteScope::Comment("COMMENT_other".into()),
            RemoteIntent::Reconcile,
            0,
        )
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Reconciled)
        .unwrap();
    assert!(
        queue
            .enqueue_scoped(target.clone(), scope.clone(), RemoteIntent::Mutation, 0)
            .is_err()
    );
    queue
        .enqueue_scoped(target.clone(), scope, RemoteIntent::Reconcile, 0)
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Reconciled)
        .unwrap();
    queue
        .enqueue(target, RemoteIntent::Mutation, 0)
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Confirmed)
        .unwrap();
}

#[tokio::test]
async fn all_comment_scopes_share_the_pr_fifo_and_reject_writes_queued_before_uncertainty() {
    use forge_github::queue::RemoteScope;
    let queue = RemoteQueue::default();
    let target = resource("github.com", 7);
    let mut first = queue
        .enqueue_scoped(
            target.clone(),
            RemoteScope::Creation("operation-1".into()),
            RemoteIntent::Mutation,
            0,
        )
        .unwrap()
        .ready()
        .await
        .unwrap();
    let second = queue
        .enqueue_scoped(
            target.clone(),
            RemoteScope::Comment("COMMENT_two".into()),
            RemoteIntent::Mutation,
            0,
        )
        .unwrap();
    let mut waiting = Box::pin(second.ready());
    assert!(
        tokio::time::timeout(std::time::Duration::from_millis(10), &mut waiting)
            .await
            .is_err()
    );
    first.begin_mutation().unwrap();
    drop(first);
    assert!(waiting.await.is_err());
    queue
        .enqueue_scoped(
            target.clone(),
            RemoteScope::Creation("operation-2".into()),
            RemoteIntent::Reconcile,
            0,
        )
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Reconciled)
        .unwrap();
    assert!(
        queue
            .enqueue(target.clone(), RemoteIntent::Mutation, 0)
            .is_err()
    );
    queue
        .enqueue_scoped(
            target.clone(),
            RemoteScope::Creation("operation-1".into()),
            RemoteIntent::Reconcile,
            0,
        )
        .unwrap()
        .ready()
        .await
        .unwrap()
        .complete(RemoteCompletion::Reconciled)
        .unwrap();
    assert!(queue.enqueue(target, RemoteIntent::Mutation, 0).is_ok());
}

#[tokio::test]
async fn invalid_scope_and_byte_overflow_do_not_consume_admission() {
    use forge_github::queue::RemoteScope;
    let queue = RemoteQueue::default();
    let target = resource("github.com", 7);
    for identity in ["".to_owned(), "x".repeat(257), "bad identity".to_owned()] {
        assert!(
            queue
                .enqueue_scoped(
                    target.clone(),
                    RemoteScope::Comment(identity.into_boxed_str()),
                    RemoteIntent::Mutation,
                    0
                )
                .is_err()
        );
    }
    assert!(
        queue
            .enqueue_scoped(
                target.clone(),
                RemoteScope::Creation("request".into()),
                RemoteIntent::Mutation,
                usize::MAX
            )
            .is_err()
    );
    assert!(
        queue
            .enqueue_scoped(
                target.clone(),
                RemoteScope::Comment("comment".into()),
                RemoteIntent::Mutation,
                16 * 1024 * 1024
            )
            .is_err()
    );
    let read = queue
        .enqueue(target.clone(), RemoteIntent::Reconcile, 0)
        .unwrap()
        .ready()
        .await
        .unwrap();
    assert!(read.complete(RemoteCompletion::Uncertain).is_err());
    assert!(
        queue
            .enqueue(target, RemoteIntent::Mutation, 16 * 1024 * 1024)
            .is_ok()
    );
}
