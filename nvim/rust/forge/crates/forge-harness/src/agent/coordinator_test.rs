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

fn admit(coordinator: &AgentCoordinator, operation_id: &str, task_name: &str) -> AgentWork {
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
    let cancelled = coordinator.interrupt(&first.agent.id).unwrap();
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
