use anyhow::Result;
use serde_json::json;

use super::{SessionTaskController, Task, TaskChange, TaskEvent, TaskPhase, TaskTurnId};

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
