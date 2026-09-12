use std::path::Path;
use std::process::Command;

use forge_git::coordinator::{MutationCoordinator, OperationState};
use forge_git::mutation::{MutationScope, OperationCompletion};
use forge_git::{RepositoryIdentity, discover_identity};

fn git(root: &Path, arguments: &[&str]) {
    let output = Command::new("git")
        .args(["-c", "commit.gpgsign=false", "-c"])
        .arg(format!(
            "core.hooksPath={}",
            root.join("disabled-hooks").display()
        ))
        .arg("-C")
        .arg(root)
        .args(arguments)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

fn fixture() -> (tempfile::TempDir, RepositoryIdentity) {
    let root = tempfile::tempdir().unwrap();
    git(root.path(), &["init", "--quiet"]);
    let identity = discover_identity(root.path()).unwrap().unwrap();
    (root, identity)
}

#[test]
fn intersecting_scopes_keep_admission_order_without_blocking_unrelated_work() {
    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(4, 4096).unwrap();
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
    let coordinator = MutationCoordinator::new(2, 4096).unwrap();
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
    let coordinator = MutationCoordinator::new(2, 4096).unwrap();
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

#[test]
fn completed_receipts_consume_capacity_until_acknowledged() {
    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(1, 4096).unwrap();
    let scope = MutationScope::Index(identity.worktree.unwrap());
    let first = coordinator.admit(vec![scope.clone()], 0).unwrap();
    coordinator.cancel(first).unwrap();
    assert!(coordinator.admit(vec![scope.clone()], 0).is_err());
    assert!(coordinator.start(first).is_err());
    assert_eq!(
        coordinator.take_receipt(first),
        Some(OperationCompletion::CancelledBeforeStart)
    );
    let next = coordinator.admit(vec![scope], 0).unwrap();
    assert_ne!(first, next);
}

#[test]
fn linked_worktrees_have_independent_indexes_and_one_shared_ref_queue() {
    let (root, identity) = fixture();
    git(
        root.path(),
        &[
            "-c",
            "user.name=Fixture",
            "-c",
            "user.email=fixture@example.invalid",
            "commit",
            "--allow-empty",
            "-m",
            "initial",
            "--quiet",
        ],
    );
    git(
        root.path(),
        &["worktree", "add", "--quiet", "--detach", "linked"],
    );
    let linked = discover_identity(&root.path().join("linked"))
        .unwrap()
        .unwrap();
    let coordinator = MutationCoordinator::new(4, 4096).unwrap();
    let main_index = coordinator
        .admit(vec![MutationScope::Index(identity.worktree.unwrap())], 0)
        .unwrap();
    let linked_index = coordinator
        .admit(vec![MutationScope::Index(linked.worktree.unwrap())], 0)
        .unwrap();
    let main_refs = coordinator
        .admit(vec![MutationScope::SharedRefs(identity.storage)], 0)
        .unwrap();
    let linked_refs = coordinator
        .admit(vec![MutationScope::SharedRefs(linked.storage)], 0)
        .unwrap();
    let main_guard = coordinator.start(main_index).unwrap().unwrap();
    let linked_guard = coordinator.start(linked_index).unwrap().unwrap();
    let refs_guard = coordinator.start(main_refs).unwrap().unwrap();
    assert!(coordinator.start(linked_refs).unwrap().is_none());
    main_guard.finish(OperationCompletion::Completed).unwrap();
    linked_guard.finish(OperationCompletion::Completed).unwrap();
    refs_guard.finish(OperationCompletion::Completed).unwrap();
    coordinator
        .start(linked_refs)
        .unwrap()
        .unwrap()
        .finish(OperationCompletion::Completed)
        .unwrap();
}

#[test]
fn invalid_scope_requests_do_not_consume_admission() {
    let (_root, identity) = fixture();
    assert!(MutationCoordinator::new(0, 4096).is_err());
    let coordinator = MutationCoordinator::new(1, 4096).unwrap();
    let scope = MutationScope::Index(identity.worktree.unwrap());
    assert!(coordinator.admit(vec![], 0).is_err());
    assert!(
        coordinator
            .admit(vec![scope.clone(), scope.clone()], 0)
            .is_err()
    );
    assert!(coordinator.admit(vec![scope.clone(); 65], 0).is_err());
    assert!(coordinator.admit(vec![scope], 0).is_ok());
}

#[tokio::test]
async fn asynchronous_handoff_observes_completion_before_its_first_poll() {
    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(2, 4096).unwrap();
    let scope = MutationScope::Index(identity.worktree.unwrap());
    let first = coordinator.admit(vec![scope.clone()], 0).unwrap();
    let second = coordinator.admit(vec![scope], 0).unwrap();
    let running = coordinator.start(first).unwrap().unwrap();
    let waiting = coordinator.wait_start(second).unwrap();
    assert!(coordinator.wait_start(second).is_err());
    assert!(coordinator.start(second).is_err());
    running.finish(OperationCompletion::Completed).unwrap();
    let next = tokio::time::timeout(std::time::Duration::from_secs(1), waiting)
        .await
        .unwrap()
        .unwrap();
    next.finish(OperationCompletion::Completed).unwrap();
}

#[tokio::test]
async fn cancelled_waiter_records_a_receipt_and_wakes_the_next_scope_owner() {
    use std::future::Future;
    use std::task::Poll;

    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(3, 4096).unwrap();
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
    let next = tokio::time::timeout(std::time::Duration::from_secs(1), third_wait)
        .await
        .unwrap()
        .unwrap();
    next.finish(OperationCompletion::Completed).unwrap();
    running.finish(OperationCompletion::Completed).unwrap();
}

#[tokio::test]
async fn shutdown_wakes_queued_and_running_owners_without_releasing_running_scopes() {
    use std::future::Future;
    use std::task::Poll;

    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(2, 4096).unwrap();
    let scope = MutationScope::Index(identity.worktree.unwrap());
    let first = coordinator.admit(vec![scope.clone()], 0).unwrap();
    let second = coordinator.admit(vec![scope], 0).unwrap();
    let running = coordinator.start(first).unwrap().unwrap();
    let waiting = coordinator.wait_start(second).unwrap();
    {
        let mut cancellation = Box::pin(running.cancelled());
        std::future::poll_fn(|context| {
            assert!(cancellation.as_mut().poll(context).is_pending());
            Poll::Ready(())
        })
        .await;
        assert_eq!(coordinator.close(), vec![first]);
        tokio::time::timeout(std::time::Duration::from_secs(1), cancellation)
            .await
            .unwrap()
            .unwrap();
    }
    assert!(waiting.await.is_err());
    assert!(coordinator.take_receipt(first).is_none());
    running.finish(OperationCompletion::Uncertain).unwrap();
    assert_eq!(
        coordinator.take_receipt(first),
        Some(OperationCompletion::Uncertain)
    );
}

#[test]
fn input_budget_is_independent_from_count_and_releases_only_on_receipt_adoption() {
    let (_root, identity) = fixture();
    assert!(MutationCoordinator::new(1, 0).is_err());
    let coordinator = MutationCoordinator::new(3, 10).unwrap();
    let scope = MutationScope::Index(identity.worktree.unwrap());
    let first = coordinator.admit(vec![scope.clone()], 6).unwrap();
    assert!(coordinator.admit(vec![scope.clone()], 5).is_err());
    assert!(coordinator.admit(vec![scope.clone()], usize::MAX).is_err());
    assert_eq!(coordinator.usage().operations, 1);
    assert_eq!(coordinator.usage().input_bytes, 6);
    let second = coordinator.admit(vec![scope.clone()], 4).unwrap();
    let running = coordinator.start(first).unwrap().unwrap();
    coordinator.cancel(first).unwrap();
    assert_eq!(coordinator.usage().input_bytes, 10);
    running.finish(OperationCompletion::Uncertain).unwrap();
    assert_eq!(coordinator.usage().input_bytes, 10);
    assert!(coordinator.admit(vec![scope.clone()], 1).is_err());
    assert_eq!(
        coordinator.take_receipt(first),
        Some(OperationCompletion::Uncertain)
    );
    assert_eq!(coordinator.usage().input_bytes, 4);
    assert!(coordinator.take_receipt(first).is_none());
    coordinator.admit(vec![scope], 6).unwrap();
    coordinator.cancel(second).unwrap();
    assert_eq!(coordinator.usage().input_bytes, 10);
    coordinator.take_receipt(second).unwrap();
    assert_eq!(coordinator.usage().input_bytes, 6);
}

#[test]
fn cancelled_waiter_and_abandoned_owner_retain_their_input_charges() {
    let (_root, identity) = fixture();
    let coordinator = MutationCoordinator::new(2, 10).unwrap();
    let scope = MutationScope::Index(identity.worktree.unwrap());
    let first = coordinator.admit(vec![scope.clone()], 6).unwrap();
    let second = coordinator.admit(vec![scope], 4).unwrap();
    let running = coordinator.start(first).unwrap().unwrap();
    drop(coordinator.wait_start(second).unwrap());
    assert_eq!(coordinator.usage().input_bytes, 10);
    assert_eq!(
        coordinator.take_receipt(second),
        Some(OperationCompletion::CancelledBeforeStart)
    );
    assert_eq!(coordinator.usage().input_bytes, 6);
    drop(running);
    assert_eq!(coordinator.close(), vec![first]);
    assert!(coordinator.take_receipt(first).is_none());
    assert_eq!(coordinator.usage().input_bytes, 6);
}
