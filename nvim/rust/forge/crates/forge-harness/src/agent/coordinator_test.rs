use super::coordinator::*;
use super::{AgentSelection, AgentSource};
use crate::{
    provider::{ProviderId, ServiceTier},
    session::ApprovalMode,
};

fn selection(provider: ProviderId) -> AgentSelection {
    AgentSelection {
        profile: "worker".into(),
        instructions: String::new(),
        provider,
        model: "model".into(),
        reasoning_effort: "low".into(),
        context_window: None,
        service_tier: ServiceTier::Standard,
        access: Default::default(),
        approval_mode: ApprovalMode::Ask,
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

pub(super) fn admit(
    coordinator: &AgentCoordinator,
    operation_id: &str,
    task_name: &str,
) -> AgentWork {
    let (request, spawn) = request(operation_id, task_name);
    let admission = coordinator
        .reserve_spawn(
            &request,
            &spawn,
            selection(
                forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
            ),
            "parent",
            1,
        )
        .unwrap();
    match admission.commit().unwrap() {
        AgentCommit::Admitted(work) => work,
        AgentCommit::Existing(_) => panic!("new request reused an admission"),
    }
}

/// A check closes child admission without parking the root, including a reservation race.
#[test]
fn check_launch_hold_fences_reserved_children_until_collection() {
    let coordinator = AgentCoordinator::new("root".into());
    let (initial_request, spawn) = request("reserved", "worker");
    let reserved = coordinator
        .reserve_spawn(
            &initial_request,
            &spawn,
            selection(forge_providers::registry::nonselectable_id().unwrap()),
            "parent",
            1,
        )
        .unwrap();
    let first = coordinator.hold_launches().unwrap();
    let second = coordinator.hold_launches().unwrap();
    assert!(!coordinator.is_quiescing());
    let AgentCommit::Admitted(work) = reserved.commit().unwrap() else {
        panic!("first commit cannot replay existing work");
    };
    assert!(!work.launch);
    assert_eq!(coordinator.active_count(), 0);
    assert!(coordinator.ready_launch().unwrap().is_none());
    let (next_request, next_spawn) = request("new", "worker");
    assert!(
        coordinator
            .reserve_spawn(
                &next_request,
                &next_spawn,
                selection(forge_providers::registry::nonselectable_id().unwrap()),
                "parent",
                2,
            )
            .is_err()
    );
    drop(first);
    assert!(coordinator.ready_launch().unwrap().is_none());
    drop(second);
    assert_eq!(
        coordinator.ready_launch().unwrap().unwrap().task.id,
        work.task.id
    );
    assert!(coordinator.hold_launches().is_err());
}

#[test]
fn followups_serialize_and_interruption_cancels_pending_without_restarting() {
    let coordinator = AgentCoordinator::new("root".into());
    let first = admit(&coordinator, "operation", "review");
    let cancellation = coordinator
        .cancellation(&first.agent.id, &first.task.id)
        .unwrap();
    for ordinal in 0..MAX_PENDING_TASKS {
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
        admission.commit().unwrap();
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
    let cancelled = coordinator
        .interrupt(&first.agent.id, AgentSource::User)
        .unwrap();
    assert_eq!(cancelled.len(), MAX_PENDING_TASKS);
    assert!(*cancellation.borrow());
    assert!(
        coordinator
            .reserve_followup(&overflow, &first.agent.id, "next task", "parent", 2)
            .is_err()
    );
    assert!(
        coordinator
            .settled(
                &first.agent.id,
                &first.task.id,
                super::AgentSettlementDisposition::Ready
            )
            .unwrap()
            .is_none()
    );
    assert_eq!(coordinator.active_count(), 0);
    assert!(coordinator.resolve("worker-1").is_ok());
    let explicit = coordinator
        .reserve_followup(&overflow, &first.agent.id, "next task", "parent", 3)
        .unwrap();
    assert!(explicit.launch);
    let explicit = match explicit.commit().unwrap() {
        AgentCommit::Admitted(work) => work,
        AgentCommit::Existing(_) => panic!("new follow-up reused an admission"),
    };
    assert!(
        !*coordinator
            .cancellation(&explicit.agent.id, &explicit.task.id)
            .unwrap()
            .borrow()
    );
    assert!(
        coordinator
            .settled(
                &first.agent.id,
                &first.task.id,
                super::AgentSettlementDisposition::Ready
            )
            .is_err()
    );
}

/// Admit one model-delegated child and return its committed work.
fn admitted_model_child(coordinator: &AgentCoordinator) -> super::AgentWork {
    let spawn = SpawnAgentRequest {
        profile: Some("review".into()),
        text: "bounded task".into(),
    };
    let request = AgentRequest {
        operation_id: "model-spawn".into(),
        source: AgentSource::Model,
        operation: AgentOperation::Spawn(spawn.clone()),
    };
    match coordinator
        .reserve_spawn(
            &request,
            &spawn,
            selection(
                forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
            ),
            "parent",
            1,
        )
        .unwrap()
        .commit()
        .unwrap()
    {
        AgentCommit::Admitted(work) => work,
        AgentCommit::Existing(_) => panic!("new request reused an admission"),
    }
}

fn model_followup(operation_id: &str, target: &str) -> AgentRequest {
    AgentRequest {
        operation_id: operation_id.into(),
        source: AgentSource::Model,
        operation: AgentOperation::Followup {
            target: target.into(),
            message: "next task".into(),
        },
    }
}

/// A model retry of a stopped child is rejected before it reserves anything, so the parent has
/// no queued descendant that only a user could release.
#[test]
fn model_followup_of_stopped_child_is_rejected_without_reservation() {
    let coordinator = AgentCoordinator::new("root".into());
    let first = admitted_model_child(&coordinator);
    let mut stopped = first.task.clone();
    stopped.state = crate::exchange::ExchangeState::Interrupted;
    stopped.completed_at_ms = Some(2);
    let mut agent = first.agent.clone();
    agent.continuation = super::AgentContinuationPolicy::RequiresAuthorization {
        task_id: stopped.id.clone().into(),
    };
    coordinator.retain(agent.clone(), stopped.clone()).unwrap();
    coordinator
        .settled(
            &agent.id,
            &stopped.id,
            super::AgentSettlementDisposition::RequiresAuthorization,
        )
        .unwrap();
    let followup = model_followup("model-followup", &agent.id);
    let error = coordinator
        .reserve_followup(&followup, &agent.id, "next task", "parent", 3)
        .err()
        .expect("a stopped child rejects model work");
    let required = error
        .downcast_ref::<super::ContinuationRequired>()
        .expect("rejection is typed");
    assert_eq!(required.agent_id, agent.id);
    assert_eq!(required.stopped_task_id.as_str(), stopped.id);
    assert!(!coordinator.has_active_task("parent"));
    assert!(coordinator.ready_launch().unwrap().is_none());
    assert_eq!(coordinator.collection_count().queued, 0);
    // A user grant naming the exact stopped task still admits one launchable task.
    let user = AgentRequest {
        operation_id: "user-followup".into(),
        source: AgentSource::User,
        operation: AgentOperation::Followup {
            target: agent.id.clone(),
            message: "next task".into(),
        },
    };
    let granted = coordinator
        .reserve_followup(&user, &agent.id, "next task", "parent", 4)
        .unwrap();
    assert!(granted.launch);
    assert_eq!(
        granted.continuation_authorization().map(|id| id.as_str()),
        Some(stopped.id.as_str())
    );
}

/// A stop that requires authorization releases its execution only after every follow-up queued
/// behind it is terminal; none moves to the launch queue where it could never start.
#[test]
fn child_stop_requires_settled_followups() {
    let coordinator = AgentCoordinator::new("root".into());
    let first = admitted_model_child(&coordinator);
    let queued = coordinator
        .reserve_followup(
            &model_followup("queued-followup", &first.agent.id),
            &first.agent.id,
            "next task",
            "parent",
            2,
        )
        .unwrap();
    assert!(!queued.launch);
    let queued = match queued.commit().unwrap() {
        AgentCommit::Admitted(work) => work,
        AgentCommit::Existing(_) => panic!("new follow-up reused an admission"),
    };
    assert_eq!(
        coordinator
            .queued_followups(&first.agent.id)
            .iter()
            .map(|task| task.id.clone())
            .collect::<Vec<_>>(),
        vec![queued.task.id.clone()]
    );
    let mut stopped = first.task.clone();
    stopped.state = crate::exchange::ExchangeState::Failed;
    stopped.completed_at_ms = Some(3);
    let mut agent = first.agent.clone();
    agent.continuation = super::AgentContinuationPolicy::RequiresAuthorization {
        task_id: stopped.id.clone().into(),
    };
    coordinator.retain(agent.clone(), stopped.clone()).unwrap();
    assert!(
        coordinator
            .settled(
                &agent.id,
                &stopped.id,
                super::AgentSettlementDisposition::RequiresAuthorization,
            )
            .is_err(),
        "an unsettled follow-up must not be stranded"
    );
    assert_eq!(coordinator.active_count(), 1);
    let mut cancelled = queued.task.clone();
    cancelled.state = crate::exchange::ExchangeState::Cancelled;
    cancelled.completed_at_ms = Some(3);
    coordinator.retain(agent.clone(), cancelled).unwrap();
    assert!(
        coordinator
            .settled(
                &agent.id,
                &stopped.id,
                super::AgentSettlementDisposition::RequiresAuthorization,
            )
            .unwrap()
            .is_none()
    );
    assert_eq!(coordinator.active_count(), 0);
    assert_eq!(coordinator.collection_count().queued, 0);
    assert!(!coordinator.has_active_task("parent"));
    assert!(coordinator.ready_launch().unwrap().is_none());
}
