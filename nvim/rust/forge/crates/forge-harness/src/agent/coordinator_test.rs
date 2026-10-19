use super::coordinator::*;
use super::{AgentSelection, AgentSource};
use crate::{
    backend::{BackendKind, ServiceTier},
    session::PermissionMode,
};

fn selection(provider: BackendKind) -> AgentSelection {
    AgentSelection {
        profile: "worker".into(),
        instructions: String::new(),
        provider,
        model: "model".into(),
        reasoning_effort: "low".into(),
        context_window: None,
        service_tier: ServiceTier::Standard,
        access: Default::default(),
        execution_mode: PermissionMode::Write,
    }
}

fn request(operation_id: &str, task_name: &str) -> (AgentRequest, SpawnAgentRequest) {
    let spawn = SpawnAgentRequest {
        profile: Some(task_name.into()),
        text: "bounded task".into(),
    };
    (
        AgentRequest {
            operation_id: operation_id.into(),
            source: AgentSource::User,
            operation: AgentOperation::Spawn(spawn.clone()),
        },
        spawn,
    )
}

fn admit(coordinator: &AgentCoordinator, operation_id: &str, task_name: &str) -> AgentAdmission {
    let (request, spawn) = request(operation_id, task_name);
    let admission = coordinator
        .reserve_spawn(&request, &spawn, selection(BackendKind::Mock), "parent", 1)
        .unwrap();
    assert!(coordinator.commit(&admission).unwrap());
    admission
}

#[test]
fn capacity_counts_uncommitted_and_cleanup_ownership_without_orphaning_ninth() {
    let coordinator = AgentCoordinator::new("root".into());
    let admitted: Vec<_> = (0..8)
        .map(|ordinal| {
            let (request, spawn) =
                request(&format!("operation-{ordinal}"), &format!("child-{ordinal}"));
            coordinator
                .reserve_spawn(&request, &spawn, selection(BackendKind::Mock), "parent", 1)
                .unwrap()
        })
        .collect();
    let (ninth, spawn) = request("ninth", "ninth");
    assert!(
        coordinator
            .reserve_spawn(&ninth, &spawn, selection(BackendKind::Mock), "parent", 1)
            .is_err()
    );
    assert!(coordinator.snapshot(None).is_empty());
    for admission in &admitted {
        coordinator.commit(admission).unwrap();
    }
    assert_eq!(coordinator.active_count(), 8);
    let first = &admitted[0];
    coordinator
        .settled(&first.agent.id, &first.task.id, true)
        .unwrap();
    assert_eq!(coordinator.active_count(), 8);
    assert!(
        coordinator
            .reserve_spawn(&ninth, &spawn, selection(BackendKind::Mock), "parent", 1)
            .is_err()
    );
    coordinator
        .settled(&first.agent.id, &first.task.id, false)
        .unwrap();
    assert!(
        coordinator
            .reserve_spawn(&ninth, &spawn, selection(BackendKind::Mock), "parent", 1)
            .is_ok()
    );
}

#[test]
fn retries_resolve_one_identity_and_rejected_persistence_releases_reservation() {
    let coordinator = AgentCoordinator::new("root".into());
    let (request, spawn) = request("operation", "review");
    let first = coordinator
        .reserve_spawn(
            &request,
            &spawn,
            selection(BackendKind::Claude),
            "parent",
            1,
        )
        .unwrap();
    let replay = coordinator
        .reserve_spawn(
            &request,
            &spawn,
            selection(BackendKind::Claude),
            "parent",
            2,
        )
        .unwrap();
    assert_eq!(first.agent.id, replay.agent.id);
    assert_eq!(first.task.id, replay.task.id);
    coordinator.commit(&first).unwrap();
    assert!(!coordinator.commit(&replay).unwrap());
    let mut conflict = spawn.clone();
    conflict.text = "different work".into();
    assert!(
        coordinator
            .reserve_spawn(
                &request,
                &conflict,
                selection(BackendKind::Claude),
                "parent",
                3
            )
            .is_err()
    );
    let (duplicate, spawn) = super::coordinator_test::request("another-operation", "review");
    assert!(
        coordinator
            .reserve_spawn(
                &duplicate,
                &spawn,
                selection(BackendKind::Claude),
                "parent",
                3
            )
            .is_ok()
    );
    coordinator.reject("another-operation");
    let (failed, spawn) = super::coordinator_test::request("failed", "failed");
    coordinator
        .reserve_spawn(&failed, &spawn, selection(BackendKind::Claude), "parent", 3)
        .unwrap();
    coordinator.reject("failed");
    let retried = coordinator
        .reserve_spawn(&failed, &spawn, selection(BackendKind::Claude), "parent", 4)
        .unwrap();
    coordinator.commit(&retried).unwrap();
    assert_eq!(coordinator.snapshot(None).len(), 2);
}

#[test]
fn names_and_targets_remain_root_owned() {
    let coordinator = AgentCoordinator::new("first-root".into());
    let admission = admit(&coordinator, "operation", "review");
    for target in [
        &admission.agent.id,
        &admission.agent.task_path,
        &admission.agent.task_name,
    ] {
        assert_eq!(coordinator.resolve(target).unwrap().id, admission.agent.id);
    }
    let other = AgentCoordinator::new("second-root".into());
    assert!(other.resolve(&admission.agent.id).is_err());
    assert!(coordinator.resolve("/root").is_err());
    assert!(coordinator.resolve("/other/review").is_err());
}

#[test]
fn followups_serialize_and_interruption_cancels_pending_without_restarting() {
    let coordinator = AgentCoordinator::new("root".into());
    let first = admit(&coordinator, "operation", "review");
    let cancellation = coordinator
        .cancellation(&first.agent.id, &first.task.id)
        .unwrap();
    for ordinal in 0..32 {
        let request = AgentRequest {
            operation_id: format!("followup-{ordinal}"),
            source: AgentSource::User,
            operation: AgentOperation::Followup {
                target: first.agent.id.clone(),
                message: "next task".into(),
            },
        };
        let admission = coordinator
            .reserve_followup(&request, &first.agent.id, "next task", "parent", 2)
            .unwrap();
        assert!(!admission.launch);
        coordinator.commit(&admission).unwrap();
    }
    let overflow = AgentRequest {
        operation_id: "overflow".into(),
        source: AgentSource::User,
        operation: AgentOperation::Followup {
            target: first.agent.id.clone(),
            message: "next task".into(),
        },
    };
    assert!(
        coordinator
            .reserve_followup(&overflow, &first.agent.id, "next task", "parent", 2)
            .is_err()
    );
    let cancelled = coordinator.interrupt(&first.agent.id).unwrap();
    assert_eq!(cancelled.len(), 32);
    assert!(*cancellation.borrow());
    assert!(
        coordinator
            .reserve_followup(&overflow, &first.agent.id, "next task", "parent", 2)
            .is_err()
    );
    assert!(
        coordinator
            .settled(&first.agent.id, &first.task.id, false)
            .unwrap()
            .is_none()
    );
    assert_eq!(coordinator.active_count(), 0);
    assert!(coordinator.resolve("worker-1").is_ok());
    let explicit = coordinator
        .reserve_followup(&overflow, &first.agent.id, "next task", "parent", 3)
        .unwrap();
    assert!(explicit.launch);
    coordinator.commit(&explicit).unwrap();
    assert!(
        !*coordinator
            .cancellation(&explicit.agent.id, &explicit.task.id)
            .unwrap()
            .borrow()
    );
    assert!(
        coordinator
            .settled(&first.agent.id, &first.task.id, false)
            .is_err()
    );
}

#[test]
fn settled_task_selects_only_the_next_queued_task_and_retains_selection() {
    let coordinator = AgentCoordinator::new("root".into());
    let first = admit(&coordinator, "first", "review");
    let request = AgentRequest {
        operation_id: "followup".into(),
        source: AgentSource::User,
        operation: AgentOperation::Followup {
            target: "worker-1".into(),
            message: "next".into(),
        },
    };
    let second = coordinator
        .reserve_followup(&request, "worker-1", "next", "parent", 2)
        .unwrap();
    coordinator.commit(&second).unwrap();
    let next = coordinator
        .settled(&first.agent.id, &first.task.id, false)
        .unwrap()
        .unwrap();
    assert_eq!(next.task.id, second.task.id);
    assert_eq!(next.agent.selection.unwrap().provider, BackendKind::Mock);
    assert_eq!(coordinator.active_count(), 1);
    assert!(
        coordinator
            .settled(&second.agent.id, &second.task.id, false)
            .unwrap()
            .is_none()
    );
}

#[tokio::test]
async fn requests_are_callable_without_holding_receiver_lock_across_await() {
    let coordinator = AgentCoordinator::new("root".into());
    let mut revision = coordinator.subscribe();
    let sender = coordinator.sender();
    let (request, _) = request("operation", "review");
    let pending = tokio::spawn(async move { sender.request(request).await });
    tokio::time::timeout(std::time::Duration::from_secs(1), revision.changed())
        .await
        .unwrap()
        .unwrap();
    let Some(AgentCommand::Request { response, .. }) = coordinator.try_receive() else {
        panic!("missing admitted request")
    };
    assert!(coordinator.try_receive().is_none());
    response
        .send(Ok(serde_json::json!({"accepted":true})))
        .unwrap();
    assert_eq!(pending.await.unwrap().unwrap()["accepted"], true);
}

#[test]
fn overlapping_policy_transitions_close_new_admission_and_preserve_durable_replays() {
    let coordinator = AgentCoordinator::new("root".into());
    let first = admit(&coordinator, "first", "review");
    let transition = coordinator.begin_quiescence();
    let cancellation = coordinator.begin_quiescence();
    let (next, spawn) = request("second", "second");
    assert!(
        coordinator
            .reserve_spawn(&next, &spawn, selection(BackendKind::Mock), "parent", 2)
            .is_err()
    );
    assert!(
        coordinator
            .reserve_followup(&next, &first.agent.id, "next", "parent", 2)
            .is_err()
    );
    let (replay, spawn) = request("first", "review");
    assert_eq!(
        coordinator
            .reserve_spawn(&replay, &spawn, selection(BackendKind::Mock), "parent", 2)
            .unwrap()
            .task
            .id,
        first.task.id
    );
    drop(transition);
    assert!(
        coordinator
            .reserve_followup(&next, &first.agent.id, "next", "parent", 2)
            .is_err()
    );
    drop(cancellation);
    assert!(
        coordinator
            .reserve_followup(&next, &first.agent.id, "next", "parent", 2)
            .is_ok()
    );
}
