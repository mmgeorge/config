use anyhow::Result;
use serde_json::json;

use super::{SessionTaskController, Task, TaskChange, TaskEvent, TaskPhase, TaskTurnId};

/// Reject inconsistent loaded summaries instead of letting editor state choose a workflow.
#[test]
fn persisted_task_rejects_contradictory_phase_lifecycle_and_work() -> Result<()> {
    let task = Task::goal("Inspect the result".into(), Default::default(), 1);
    let valid = serde_json::to_value(&task)?;
    let restored: Task = serde_json::from_value(valid.clone())?;
    assert_eq!(restored.id, task.id);
    for (field, value) in [("phase", json!("check")), ("status", json!("paused"))] {
        let mut invalid = valid.clone();
        invalid[field] = value;
        assert!(serde_json::from_value::<Task>(invalid).is_err());
    }
    for work in [
        json!({"kind":"checks"}),
        json!({"kind":"provider","instruction":" "}),
    ] {
        let mut invalid = valid.clone();
        invalid["turn"] = json!({
            "id":{"exchange_ordinal":1,"turn_ordinal":1},
            "continuation":{"id":"invalid-work","reason":"Continue","delivery":"pending","work":work}
        });
        assert!(serde_json::from_value::<Task>(invalid).is_err());
    }
    Ok(())
}

/// Prove that rejected gate bypasses leave both the database and published admission unchanged.
#[test]
fn implementation_cannot_jump_to_manual_verification_or_completion() -> Result<()> {
    use crate::plan::{PlanExecutionRecord, PlanExecutionState, PlanPhase};
    let (_root, mut store, controller) = SessionTaskController::test_goal();
    let previous = controller.current()?.unwrap();
    let record: PlanExecutionRecord = serde_json::from_value(json!({
        "id":"execution", "session_id":"session", "plan_id":"accepted-plan",
        "state":"active", "phase":"implement", "original_revision":1,"revision":1,
        "generation":0, "lifecycle":[],"findings":[],"progress":crate::plan::execution::SemanticProgress::default(),
        "verification":[],"revision_history":[],"resume_after_review":false,
        "observed_source":{},"observed_document_version":0,"observed_check":[],"created_at_ms":1
    }))?;
    let installed = controller
        .change(
            &mut store,
            Some(&previous.revision),
            TaskChange::Install(Box::new(Task::execute(
                "Implement accepted plan".into(),
                record.clone(),
                Default::default(),
                1,
            )?)),
            2,
        )?
        .unwrap();
    for phase in [PlanPhase::Verify, PlanPhase::Resolve] {
        let mut invalid = record.clone();
        invalid.phase = phase;
        invalid.generation += 1;
        assert!(
            controller
                .apply(
                    &mut store,
                    &installed.permit(),
                    TaskEvent::ExecutionUpdated(Box::new(invalid)),
                    3
                )
                .is_err()
        );
        assert_eq!(controller.current()?.unwrap().revision, installed.revision);
        assert_eq!(
            store.load_active_task("session")?.unwrap().phase(),
            TaskPhase::Implement
        );
    }
    let mut invalid = record.clone();
    invalid.state = PlanExecutionState::Complete;
    assert!(
        controller
            .apply(
                &mut store,
                &installed.permit(),
                TaskEvent::ExecutionUpdated(Box::new(invalid)),
                3
            )
            .is_err()
    );
    let mut checking = record;
    checking.baseline_checkpoint = Some("captured-before-provider".into());
    checking.phase = PlanPhase::Checking;
    checking.generation += 1;
    controller.apply(
        &mut store,
        &installed.permit(),
        TaskEvent::ExecutionUpdated(Box::new(checking)),
        4,
    )?;
    assert_eq!(controller.current()?.unwrap().phase(), TaskPhase::Check);
    assert!(
        installed
            .permit()
            .validate(controller.current()?.as_ref())
            .is_err()
    );
    let completed_turn = TaskTurnId {
        exchange_ordinal: 1,
        turn_ordinal: 1,
    };
    controller.observe_goal(
        &mut store,
        &installed.permit(),
        completed_turn.clone(),
        crate::goal::TurnEvidence::default(),
        5,
    )?;
    let observed = controller.current()?.unwrap();
    let super::WorkflowSnapshot::Execute { execution } = &observed.workflow else {
        unreachable!()
    };
    assert_eq!(
        execution.continuation.turn_count, 1,
        "a phase handoff evaded model turn accounting"
    );
    let (_, duplicate) = controller.observe_goal(
        &mut store,
        &installed.permit(),
        completed_turn,
        crate::goal::TurnEvidence::default(),
        6,
    )?;
    assert_eq!(duplicate, crate::goal::ContinuationDecision::Stop);
    let paused = controller
        .change(&mut store, Some(&observed.revision), TaskChange::Pause, 7)?
        .unwrap();
    let resumed = controller
        .change(&mut store, Some(&paused.revision), TaskChange::Resume, 8)?
        .unwrap();
    assert!(
        controller
            .observe_goal(
                &mut store,
                &installed.permit(),
                TaskTurnId {
                    exchange_ordinal: 2,
                    turn_ordinal: 1
                },
                crate::goal::TurnEvidence::default(),
                9
            )
            .is_err(),
        "late evidence affected resumed work"
    );
    let super::WorkflowSnapshot::Execute { execution } = &resumed.workflow else {
        unreachable!()
    };
    assert_eq!(execution.continuation.turn_count, 0);
    Ok(())
}

/// Keep a valid deferred operation admissible when newer user exchanges exist.
#[test]
fn continuation_checks_its_exact_source_instead_of_the_latest_exchange() -> Result<()> {
    let (_root, mut store, _controller) = SessionTaskController::test_goal();
    for ordinal in 1..=2 {
        let exchange: crate::exchange::Exchange = serde_json::from_value(json!({
            "id":format!("exchange-{ordinal}"),"session_id":"session",
            "agent_id":crate::session::HarnessSession::primary_agent_id("session"),
            "ordinal":ordinal,"prompt":"Inspect","state":"complete",
            "created_at_ms":ordinal,"completed_at_ms":ordinal+1,
            "node_list":[],"attributed_matches_checkpoint":false
        }))?;
        store.save_exchange(&exchange)?;
    }
    assert!(store.continuation_source_complete(
        "session",
        &TaskTurnId {
            exchange_ordinal: 1,
            turn_ordinal: 0
        }
    )?);
    assert!(!store.continuation_source_complete(
        "session",
        &TaskTurnId {
            exchange_ordinal: 1,
            turn_ordinal: 1
        }
    )?);
    Ok(())
}
