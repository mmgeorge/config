use super::super::HarnessBroker;
use super::test_support::{check_fixture, repository};
use anyhow::Result;
use serde_json::{Value, json};

/// Build a real persisted execution gate without an external model or platform-specific command.
async fn workflow_gate_fixture() -> Result<(tempfile::TempDir, tempfile::TempDir, HarnessBroker)> {
    workflow_gate_fixture_with_commands(vec!["exit 0".into()]).await
}

/// Build the gate with an explicit check command list. An empty list records a run without
/// spawning a shell, for tests that exercise gate state rather than command execution.
async fn workflow_gate_fixture_with_commands(
    commands: Vec<String>,
) -> Result<(tempfile::TempDir, tempfile::TempDir, HarnessBroker)> {
    let workspace = repository();
    let data = tempfile::tempdir()?;
    let broker = check_fixture(workspace.path(), data.path(), commands, true).await;
    Ok((workspace, data, broker))
}

/// A completed execution turn must schedule its next phase, including local checks.
#[tokio::test]
async fn execution_turn_completion_reserves_check_continuation_once() -> Result<()> {
    let (_workspace, _data, mut broker) = workflow_gate_fixture().await?;
    let task = broker.task_controller.current()?.unwrap();
    assert_eq!(task.phase(), crate::task::TaskPhase::Check);
    let turn = crate::task::TaskTurnId {
        exchange_ordinal: 1,
        turn_ordinal: 1,
    };
    let mut events = Vec::new();
    assert!(broker.apply_goal_evidence_strict(
        crate::goal::TurnEvidence {
            workspace_changed: true,
            ..Default::default()
        },
        Some(task.permit()),
        turn.clone(),
        &mut events,
    )?);
    let current = broker.task_controller.current()?.unwrap();
    let continuation = current
        .turn
        .as_ref()
        .unwrap()
        .continuation
        .as_ref()
        .unwrap();
    assert!(continuation.work.is_checks());
    assert_eq!(
        continuation.delivery,
        crate::task::TaskInputDelivery::Pending
    );
    let id = continuation.id.clone();
    assert!(!broker.apply_goal_evidence_strict(
        Default::default(),
        Some(current.permit()),
        turn,
        &mut events,
    )?);
    assert_eq!(
        broker
            .task_controller
            .current()?
            .unwrap()
            .turn
            .as_ref()
            .unwrap()
            .continuation
            .as_ref()
            .unwrap()
            .id,
        id
    );
    broker.run_checks().await?;
    assert_eq!(
        broker.task_controller.current()?.unwrap().phase(),
        crate::task::TaskPhase::Verify
    );
    Ok(())
}

/// Invalidate automated evidence for changed files, configuration, task selection, and contracts.
#[tokio::test]
async fn workflow_check_evidence_tracks_every_completion_input() -> Result<()> {
    let (workspace, _data, mut broker) = workflow_gate_fixture().await?;
    broker.run_checks().await?;
    let record = broker.current_execution()?.unwrap();
    let document = broker.plan_file.read_submitted_document(
        &broker.session.id,
        &record.plan_id,
        record.revision,
    )?;
    let design = document.design.unwrap();
    assert!(
        broker
            .execution_checks_are_current(&record, &design)
            .await?
    );
    std::fs::write(workspace.path().join("helper.rs"), "fn helper() {}\n")?;
    assert!(
        !broker
            .execution_checks_are_current(&record, &design)
            .await?
    );
    std::fs::remove_file(workspace.path().join("helper.rs"))?;
    assert!(
        broker
            .execution_checks_are_current(&record, &design)
            .await?
    );
    let original = std::fs::read(workspace.path().join(".forge.json"))?;
    let mut config: Value = serde_json::from_slice(&original)?;
    config["checks"]["check-0"]["action"]["command"] = json!("exit 1");
    std::fs::write(
        workspace.path().join(".forge.json"),
        serde_json::to_vec(&config)?,
    )?;
    assert!(
        !broker
            .execution_checks_are_current(&record, &design)
            .await?
    );
    std::fs::write(workspace.path().join(".forge.json"), original)?;
    let task = broker.task_controller.current()?.unwrap();
    broker.check_runner.set_overrides(
        Some(task.id.as_str()),
        serde_json::from_value(json!({"enabled":{"check-0":false}}))?,
    )?;
    assert!(
        !broker
            .execution_checks_are_current(&record, &design)
            .await?
    );
    broker
        .check_runner
        .set_overrides(Some(task.id.as_str()), Default::default())?;
    assert!(
        broker
            .execution_checks_are_current(&record, &design)
            .await?
    );
    let mut revised = record.clone();
    revised.revision += 1;
    assert!(
        !broker
            .execution_checks_are_current(&revised, &design)
            .await?
    );
    let mut failed = record;
    failed.check_runs.last_mut().unwrap().state = crate::checks::CheckState::Failed;
    assert!(
        !broker
            .execution_checks_are_current(&failed, &design)
            .await?
    );
    Ok(())
}

/// Exercise every execution edge out of one original phase against real committed state and
/// reject the complement atomically. Each original phase runs as its own test because every
/// one builds a fresh Git-backed gate fixture.
async fn assert_transitions_from(original: crate::plan::PlanPhase) -> Result<()> {
    use crate::plan::PlanPhase;
    {
        let (_workspace, _data, mut broker) =
            workflow_gate_fixture_with_commands(Vec::new()).await?;
        broker.run_checks().await?;
        let checked = broker.current_execution()?.unwrap();
        for target in [
            PlanPhase::Implement,
            PlanPhase::Checking,
            PlanPhase::Resolve,
            PlanPhase::Verify,
        ] {
            let task = broker.task_controller.current()?.unwrap();
            broker
                .task_controller
                .update(&mut broker.store, &task.permit(), 10, |draft| {
                    let crate::task::WorkflowSnapshot::Execute { execution } = &mut draft.workflow
                    else {
                        unreachable!()
                    };
                    execution.record = checked.clone();
                    execution.record.phase = original;
                    Ok(())
                })?;
            let before = broker.task_controller.current()?.unwrap();
            let mut next = broker.current_execution()?.unwrap();
            next.phase = target;
            next.generation += 1;
            let result = broker.task_controller.apply(
                &mut broker.store,
                &before.permit(),
                crate::task::TaskEvent::ExecutionUpdated(Box::new(next)),
                11,
            );
            let allowed = original == target
                || match original {
                    PlanPhase::Implement => target == PlanPhase::Checking,
                    PlanPhase::Checking => matches!(target, PlanPhase::Resolve | PlanPhase::Verify),
                    PlanPhase::Resolve => matches!(target, PlanPhase::Checking | PlanPhase::Verify),
                    PlanPhase::Verify => matches!(target, PlanPhase::Resolve | PlanPhase::Checking),
                };
            assert_eq!(
                result.is_ok(),
                allowed,
                "{original:?} -> {target:?}: {result:?}"
            );
            if !allowed {
                assert_eq!(
                    broker.task_controller.current()?.unwrap().revision,
                    before.revision
                );
                assert_eq!(
                    broker
                        .store
                        .load_active_task(&broker.session.id)?
                        .unwrap()
                        .revision,
                    before.revision
                );
            }
        }
    }
    Ok(())
}

#[tokio::test]
async fn workflow_execution_transitions_from_implement_preserve_the_gate_graph() -> Result<()> {
    assert_transitions_from(crate::plan::PlanPhase::Implement).await
}

/// Reject persisted gate payloads that omit the facts required by their phase variants.
#[tokio::test]
async fn workflow_verification_payload_cannot_omit_its_contract_or_evidence() -> Result<()> {
    let (_workspace, _data, mut broker) = workflow_gate_fixture().await?;
    broker.run_checks().await?;
    let task = broker.task_controller.current()?.unwrap();
    let original = serde_json::to_value(&task)?;
    for (pointer, replacement) in [
        ("/execution/record/baseline_checkpoint", Value::Null),
        ("/execution/record/manual_verification", json!([])),
        ("/execution/record/check_runs", json!([])),
        ("/execution/record/check_runs/0/state", json!("failed")),
        ("/execution/record/revision", json!(2)),
    ] {
        let mut invalid = original.clone();
        *invalid.pointer_mut(pointer).unwrap() = replacement;
        assert!(
            serde_json::from_value::<crate::task::Task>(invalid).is_err(),
            "accepted incomplete payload at {pointer}"
        );
    }
    assert_eq!(
        broker
            .store
            .load_active_task(&broker.session.id)?
            .unwrap()
            .revision,
        task.revision
    );
    Ok(())
}

/// Route unchanged repairs directly to Verify and require fresh checks after a source repair.
/// Each workspace variant runs as its own test because each builds a fresh Git-backed fixture.
async fn assert_resolution_routing(changed: bool) -> Result<()> {
    {
        let (workspace, _data, mut broker) =
            workflow_gate_fixture_with_commands(Vec::new()).await?;
        broker.run_checks().await?;
        let mut record = broker.current_execution()?.unwrap();
        assert_eq!(record.phase, crate::plan::PlanPhase::Verify);
        record.phase = crate::plan::PlanPhase::Resolve;
        record.generation += 1;
        record.findings = vec!["Inspect the manual observation again".into()];
        let task = broker.task_controller.current()?.unwrap();
        broker.task_controller.apply(
            &mut broker.store,
            &task.permit(),
            crate::task::TaskEvent::ExecutionUpdated(Box::new(record.clone())),
            3,
        )?;
        if changed {
            std::fs::write(workspace.path().join("helper.rs"), "fn helper() {}\n")?;
        }
        let task = broker.task_controller.current()?.unwrap();
        let history_count = broker.store.list_exchange(&broker.session.id)?.len();
        let mut exchange = broker.store.latest_exchange(&broker.session.id)?.unwrap();
        broker
            .handle_execution_control(
                &record.id,
                record.generation,
                &task.permit(),
                crate::plan::execution::ExecutionOperation::Phase(
                    crate::plan::execution::PlanPhaseDone {
                        phase: crate::plan::PlanPhase::Resolve,
                        revision: record.revision,
                        summary: "Resolved the recorded observation".into(),
                        verification: None,
                    },
                ),
                &mut exchange,
            )
            .await?;
        let next = broker.current_execution()?.unwrap();
        assert_eq!(
            next.phase,
            if changed {
                crate::plan::PlanPhase::Checking
            } else {
                crate::plan::PlanPhase::Verify
            }
        );
        assert_eq!(next.check_runs.len(), 1);
        assert_eq!(
            broker.store.list_exchange(&broker.session.id)?.len(),
            history_count,
            "local gate transitions created an unsolicited provider exchange"
        );
        assert_eq!(
            broker.active_goal()?.continuation.turn_count,
            0,
            "local checks consumed a model turn"
        );
        assert!(
            task.permit()
                .validate(broker.task_controller.current()?.as_ref())
                .is_err()
        );
        if changed {
            broker.run_checks().await?;
            let checked = broker.current_execution()?.unwrap();
            assert_eq!(checked.phase, crate::plan::PlanPhase::Verify);
            assert_eq!(checked.check_runs.len(), 2);
        }
    }
    Ok(())
}

#[tokio::test]
async fn workflow_resolution_reruns_checks_after_a_source_repair() -> Result<()> {
    assert_resolution_routing(true).await
}

/// A control captured before a later exchange update must not commit stale evidence.
#[tokio::test]
async fn execution_control_rejects_exchange_changed_during_evidence_capture() -> Result<()> {
    let (_workspace, _data, mut broker) = workflow_gate_fixture().await?;
    broker.run_checks().await?;
    let record = broker.current_execution()?.unwrap();
    let task = broker.task_controller.current()?.unwrap();
    let mut exchange = broker.store.latest_exchange(&broker.session.id)?.unwrap();
    let job = broker.prepare_execution_control(
        &record.id,
        record.generation,
        &task.permit(),
        crate::plan::execution::ExecutionOperation::Phase(crate::plan::execution::PlanPhaseDone {
            phase: record.phase,
            revision: record.revision,
            summary: "Validate the execution".into(),
            verification: None,
        }),
        &exchange,
    )?;
    let evidence = job.run().await?;
    exchange.finalization_error = Some("newer exchange state".into());
    assert!(
        broker
            .apply_execution_control(evidence, &mut exchange)
            .is_err()
    );
    let current = broker.current_execution()?.unwrap();
    assert_eq!(current.generation, record.generation);
    assert_eq!(
        exchange.finalization_error.as_deref(),
        Some("newer exchange state")
    );
    Ok(())
}
