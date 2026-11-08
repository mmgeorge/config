use forge_git::test_support as support;

use forge_git::coordinator::{MutationCoordinator, OperationState};
use forge_git::mutation::{MutationScope, OperationCompletion};
use forge_git::{RepositoryIdentity, discover_identity};
use support::init_repository;

fn fixture() -> (tempfile::TempDir, RepositoryIdentity) {
    let root = tempfile::tempdir().unwrap();
    init_repository(root.path());
    let identity = discover_identity(root.path()).unwrap().unwrap();
    (root, identity)
}

#[test]
fn intersecting_scopes_keep_admission_order_without_blocking_unrelated_work() {
    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(4).unwrap();
    let index = MutationScope::Index(identity.worktree.clone().unwrap());
    let files = MutationScope::WorktreeFiles(identity.worktree.unwrap());
    let first = coordinator.admit(vec![index.clone()], 0).unwrap();
    let second = coordinator.admit(vec![index, files.clone()], 0).unwrap();
    let third = coordinator.admit(vec![files], 0).unwrap();
    let unrelated = coordinator
        .admit(vec![MutationScope::SharedRefs(identity.storage)], 0)
        .unwrap();
    assert!(coordinator.start(second).unwrap().is_none());
    assert!(coordinator.start(third).unwrap().is_none());
    coordinator
        .start(unrelated)
        .unwrap()
        .unwrap()
        .finish(OperationCompletion::Completed)
        .unwrap();
    coordinator
        .start(first)
        .unwrap()
        .unwrap()
        .finish(OperationCompletion::Completed)
        .unwrap();
    let running = coordinator.start(second).unwrap().unwrap();
    assert!(coordinator.start(third).unwrap().is_none());
    running.finish(OperationCompletion::Completed).unwrap();
    coordinator
        .start(third)
        .unwrap()
        .unwrap()
        .finish(OperationCompletion::Completed)
        .unwrap();
}

#[test]
fn running_cancellation_retains_scopes_until_the_owner_records_termination() {
    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(2).unwrap();
    let scope = MutationScope::Index(identity.worktree.unwrap());
    let first = coordinator.admit(vec![scope.clone()], 0).unwrap();
    let second = coordinator.admit(vec![scope], 0).unwrap();
    let running = coordinator.start(first).unwrap().unwrap();
    coordinator.cancel(first).unwrap();
    assert!(running.cancellation_requested());
    assert!(coordinator.start(second).unwrap().is_none());
    assert!(coordinator.take_receipt(first).is_none());
    running.finish(OperationCompletion::Uncertain).unwrap();
    assert_eq!(
        coordinator.take_receipt(first),
        Some(OperationCompletion::Uncertain)
    );
    coordinator
        .start(second)
        .unwrap()
        .unwrap()
        .finish(OperationCompletion::Completed)
        .unwrap();
}

#[test]
fn dropped_running_owner_quarantines_scopes_and_survives_shutdown() {
    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(2).unwrap();
    let scope = MutationScope::Index(identity.worktree.unwrap());
    let first = coordinator.admit(vec![scope.clone()], 0).unwrap();
    let second = coordinator.admit(vec![scope.clone()], 0).unwrap();
    drop(coordinator.start(first).unwrap().unwrap());
    assert_eq!(coordinator.state(first), Some(OperationState::Abandoned));
    assert!(coordinator.start(second).unwrap().is_none());
    assert_eq!(coordinator.close(), vec![first]);
    assert!(coordinator.take_receipt(first).is_none());
    assert_eq!(
        coordinator.take_receipt(second),
        Some(OperationCompletion::CancelledBeforeStart)
    );
    assert!(coordinator.admit(vec![scope], 0).is_err());
}

#[tokio::test]
async fn asynchronous_handoff_observes_completion_before_its_first_poll() {
    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(2).unwrap();
    let scope = MutationScope::Index(identity.worktree.unwrap());
    let first = coordinator.admit(vec![scope.clone()], 0).unwrap();
    let second = coordinator.admit(vec![scope], 0).unwrap();
    let running = coordinator.start(first).unwrap().unwrap();
    let waiting = coordinator.wait_start(second).unwrap();
    assert!(coordinator.wait_start(second).is_err());
    assert!(coordinator.start(second).is_err());
    running.finish(OperationCompletion::Completed).unwrap();
    let next = waiting.await.unwrap();
    next.finish(OperationCompletion::Completed).unwrap();
}

#[tokio::test]
async fn cancelled_waiter_records_a_receipt_and_wakes_the_next_scope_owner() {
    use std::future::Future;
    use std::task::Poll;

    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(3).unwrap();
    let index = MutationScope::Index(identity.worktree.clone().unwrap());
    let files = MutationScope::WorktreeFiles(identity.worktree.unwrap());
    let first = coordinator.admit(vec![index.clone()], 0).unwrap();
    let second = coordinator.admit(vec![index, files.clone()], 0).unwrap();
    let third = coordinator.admit(vec![files], 0).unwrap();
    let running = coordinator.start(first).unwrap().unwrap();
    let second_wait = coordinator.wait_start(second).unwrap();
    let mut third_wait = Box::pin(coordinator.wait_start(third).unwrap());
    std::future::poll_fn(|context| {
        assert!(third_wait.as_mut().poll(context).is_pending());
        Poll::Ready(())
    })
    .await;
    drop(second_wait);
    assert_eq!(
        coordinator.take_receipt(second),
        Some(OperationCompletion::CancelledBeforeStart)
    );
    let next = third_wait.await.unwrap();
    next.finish(OperationCompletion::Completed).unwrap();
    running.finish(OperationCompletion::Completed).unwrap();
}
