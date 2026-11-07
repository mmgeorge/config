use anyhow::Result;
use serde_json::json;

use super::{
    SessionTaskController, Task, TaskChange, TaskEvent, TaskPhase, TaskStatus, TaskTurnId,
};
use crate::session::SessionStore;

/// Keep acceptance with its task across durable recovery, never with the session or next task.
#[test]
fn plan_acceptance_survives_revision_and_recovery_but_not_task_replacement() -> Result<()> {
    for enabled in [false, true] {
        let (root, mut store, controller) = SessionTaskController::test_goal();
        let previous = controller.current()?.unwrap();
        assert!(
            controller
                .change(
                    &mut store,
                    Some(&previous.revision),
                    TaskChange::SetPlanAcceptance(true),
                    1,
                )
                .is_err()
        );
        let mut artifact: crate::plan::PlanRecord = serde_json::from_value(json!({
            "id":"acceptance-plan", "session_id":"session", "request":"Plan the change",
            "state":"configuring_checks", "working_path":"working.json", "model_revision":0,
            "user_revision":0, "created_at_ms":1, "updated_at_ms":1
        }))?;
        let initial = controller
            .change(
                &mut store,
                Some(&previous.revision),
                TaskChange::Install(Box::new(Task::plan(artifact.clone(), 1)?)),
                1,
            )?
            .unwrap();
        assert!(!initial.auto_accept_plan);
        for selection in [true, enabled] {
            controller.change(
                &mut store,
                Some(&initial.revision),
                TaskChange::SetPlanAcceptance(selection),
                2,
            )?;
        }
        artifact.document_version += 1;
        controller.apply(
            &mut store,
            &initial.permit(),
            TaskEvent::PlanArtifactUpdated(Box::new(artifact.clone())),
            3,
        )?;
        let revised = controller.current()?.unwrap();
        assert_eq!(revised.auto_accept_plan, enabled);
        let paused = controller
            .change(&mut store, Some(&revised.revision), TaskChange::Pause, 4)?
            .unwrap();
        drop(controller);
        drop(store);

        let mut store = crate::storage::SqliteStore::open(root.path())?;
        let controller = SessionTaskController::new(store.session_admission("session")?)?;
        let recovered = controller.current()?.unwrap();
        assert_eq!(recovered.id, initial.id);
        assert_eq!(recovered.revision, paused.revision);
        assert_eq!(recovered.status(), TaskStatus::Paused);
        assert_eq!(recovered.auto_accept_plan, enabled);
        let resumed = controller
            .change(&mut store, Some(&recovered.revision), TaskChange::Resume, 5)?
            .unwrap();
        assert_eq!(resumed.auto_accept_plan, enabled);
        assert_eq!(resumed.id, initial.id);
        controller.change(&mut store, Some(&resumed.revision), TaskChange::Clear, 6)?;
        artifact.id = "replacement-plan".into();
        let replacement = controller
            .change(
                &mut store,
                None,
                TaskChange::Install(Box::new(Task::plan(artifact, 7)?)),
                7,
            )?
            .unwrap();
        assert_ne!(replacement.id, initial.id);
        assert!(!replacement.auto_accept_plan);
        assert!(!store.load_active_task("session")?.unwrap().auto_accept_plan);
    }
    Ok(())
}

/// Exercise the shared budget and explicit resume against committed SQLite state.
#[test]
fn goal_stall_resume_and_completion_preserve_identity_without_execution_gates() -> Result<()> {
    let (_root, mut store, controller) = SessionTaskController::test_goal();
    let original = controller.current()?.unwrap();
    for turn_count in 1..=5 {
        let current = controller.current()?.unwrap();
        let (_, decision) = controller.observe_goal(
            &mut store,
            &current.permit(),
            TaskTurnId {
                exchange_ordinal: turn_count,
                turn_ordinal: 1,
            },
            crate::goal::TurnEvidence {
                tool_called: true,
                ..Default::default()
            },
            i64::try_from(turn_count)?,
        )?;
        let current = controller.current()?.unwrap();
        assert_eq!(current.id, original.id);
        assert_eq!(current.phase(), TaskPhase::Work);
        if turn_count < 5 {
            assert_eq!(decision, crate::goal::ContinuationDecision::RetryNoProgress);
            assert_eq!(current.status(), TaskStatus::Running);
        } else {
            assert_eq!(decision, crate::goal::ContinuationDecision::Stalled);
            assert_eq!(current.status(), TaskStatus::Blocked);
            assert!(current.turn.as_ref().unwrap().continuation.is_none());
        }
    }
    let stalled = controller.current()?.unwrap();
    let resumed = controller
        .change(&mut store, Some(&stalled.revision), TaskChange::Resume, 6)?
        .unwrap();
    assert_eq!(resumed.id, original.id);
    assert_ne!(resumed.revision, stalled.revision);
    assert!(original.permit().validate(Some(&resumed)).is_err());
    assert_eq!(resumed.phase(), TaskPhase::Work);
    controller.observe_goal(
        &mut store,
        &resumed.permit(),
        TaskTurnId {
            exchange_ordinal: 6,
            turn_ordinal: 1,
        },
        crate::goal::TurnEvidence {
            structured_complete: true,
            ..Default::default()
        },
        7,
    )?;
    assert!(controller.current()?.is_none());
    assert!(
        store
            .load_session("session")?
            .unwrap()
            .active_task
            .is_none()
    );
    assert!(
        store
            .list_session_event("session")?
            .iter()
            .any(|event| matches!(
                event.detail,
                crate::timeline::SessionEventKind::TaskFinished { .. }
            ))
    );
    Ok(())
}

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

/// Keep unsuccessful prerequisite authoring in bounded Draft work and preserve setup across resume.
#[test]
fn draft_prerequisites_continue_then_suspend_with_their_setup_position() -> Result<()> {
    let (_root, mut store, controller) = SessionTaskController::test_goal();
    let original = controller.current()?.unwrap();
    let artifact = serde_json::from_value(json!({
        "id":"prerequisites", "session_id":"session", "request":"Plan the change",
        "state":"configuring_checks", "working_path":"working.json", "model_revision":0,
        "user_revision":0, "created_at_ms":1, "updated_at_ms":1
    }))?;
    controller.change(
        &mut store,
        Some(&original.revision),
        TaskChange::Install(Box::new(Task::plan(artifact, 1)?)),
        1,
    )?;
    for ordinal in 1..=5 {
        let task = controller.current()?.unwrap();
        let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
            "id":format!("setup-{ordinal}"), "session_id":"session", "agent_id":"agent",
            "ordinal":ordinal, "prompt":"Configure checks", "state":"running", "created_at_ms":1,
            "node_list":[], "attributed_matches_checkpoint":false
        }))?;
        let address = crate::turn::test_support::address(&exchange, "response");
        let mut turn = crate::turn::Turn::new("response".into(), address, 1);
        turn.finish(
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            2,
        )?;
        exchange.turn.push(turn);
        let (_, continuing) = controller
            .observe_plan_response(&mut store, &task.permit(), &exchange, false, 2)?
            .unwrap();
        assert_eq!(continuing, ordinal < 5);
        let current = controller.current()?.unwrap();
        assert_eq!(current.phase(), TaskPhase::Draft);
        if ordinal < 5 {
            assert!(
                current
                    .turn
                    .as_ref()
                    .unwrap()
                    .continuation
                    .as_ref()
                    .unwrap()
                    .work
                    .instruction()
                    .unwrap()
                    .contains("check")
            );
        } else {
            assert_eq!(current.status(), TaskStatus::Blocked);
        }
    }
    let blocked = controller.current()?.unwrap();
    let resumed = controller
        .change(&mut store, Some(&blocked.revision), TaskChange::Resume, 3)?
        .unwrap();
    let super::WorkflowSnapshot::Plan { plan } = &resumed.workflow else {
        unreachable!()
    };
    assert_eq!(
        plan.artifact.state,
        crate::plan::PlanState::ConfiguringChecks
    );
    assert_eq!(plan.artifact.generation.budget.turn_count, 0);
    assert_eq!(resumed.id, blocked.id);
    Ok(())
}
