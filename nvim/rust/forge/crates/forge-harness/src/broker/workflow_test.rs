/// Build a real persisted execution gate without an external model or platform-specific command.
async fn workflow_gate_fixture() -> Result<(tempfile::TempDir, tempfile::TempDir, HarnessBroker)> {
    let workspace = repository();
    let data = tempfile::tempdir()?;
    let broker = check_fixture(workspace.path(), data.path(), vec!["exit 0".into()], true).await;
    Ok((workspace, data, broker))
}

/// Invalidate automated evidence for changed files, configuration, task selection, and contracts.
#[tokio::test]
async fn workflow_check_evidence_tracks_every_completion_input() -> Result<()> {
    let (workspace, _data, mut broker) = workflow_gate_fixture().await?;
    broker.run_checks().await?;
    let record = broker.current_execution()?.unwrap();
    let document = broker.plan_file.read_submitted_document(&broker.session.id, &record.plan_id, record.revision)?;
    let design = document.design.unwrap();
    assert!(broker.execution_checks_are_current(&record, &design).await?);
    std::fs::write(workspace.path().join("helper.rs"), "fn helper() {}\n")?;
    assert!(!broker.execution_checks_are_current(&record, &design).await?);
    std::fs::remove_file(workspace.path().join("helper.rs"))?;
    assert!(broker.execution_checks_are_current(&record, &design).await?);
    let original = std::fs::read(workspace.path().join(".forge.json"))?;
    let mut config: Value = serde_json::from_slice(&original)?;
    config["checks"]["check-0"]["action"]["command"] = json!("exit 1");
    std::fs::write(workspace.path().join(".forge.json"), serde_json::to_vec(&config)?)?;
    assert!(!broker.execution_checks_are_current(&record, &design).await?);
    std::fs::write(workspace.path().join(".forge.json"), original)?;
    let task = broker.task_controller.current()?.unwrap();
    broker.check_runner.set_overrides(Some(task.id.as_str()),
        serde_json::from_value(json!({"enabled":{"check-0":false}}))?)?;
    assert!(!broker.execution_checks_are_current(&record, &design).await?);
    broker.check_runner.set_overrides(Some(task.id.as_str()), Default::default())?;
    assert!(broker.execution_checks_are_current(&record, &design).await?);
    let mut revised = record.clone();
    revised.revision += 1;
    assert!(!broker.execution_checks_are_current(&revised, &design).await?);
    let mut failed = record;
    failed.check_runs.last_mut().unwrap().state = crate::checks::CheckState::Failed;
    assert!(!broker.execution_checks_are_current(&failed, &design).await?);
    Ok(())
}

/// Repair malformed repository configuration without consuming model work or suspending Check.
#[tokio::test]
async fn workflow_check_preparation_errors_enter_resolution() -> Result<()> {
    let (workspace, _data, mut broker) = workflow_gate_fixture().await?;
    std::fs::write(workspace.path().join(".forge.json"), "{ invalid configuration }")?;
    broker.run_checks().await?;
    let task = broker.task_controller.current()?.unwrap();
    assert_eq!(task.phase(), crate::task::TaskPhase::Resolve);
    assert_eq!(task.status(), crate::task::TaskStatus::Running);
    let record = broker.current_execution()?.unwrap();
    assert!(record.findings.iter().any(|finding| finding.contains("check preparation requires correction")));
    assert_eq!(broker.active_goal()?.continuation.turn_count, 0);
    Ok(())
}

/// Replace stale Verify provider work with one durable local gate before another model admission.
#[tokio::test]
async fn workflow_verification_revalidates_before_provider_admission() -> Result<()> {
    let (workspace, _data, mut broker) = workflow_gate_fixture().await?;
    broker.run_checks().await?;
    let task = broker.task_controller.current()?.unwrap();
    let source = crate::task::TaskTurnId { exchange_ordinal: 1, turn_ordinal: 0 };
    broker.task_controller.reserve_continuation(&mut broker.store, &task.permit(), source,
        "Verify accepted manual behavior".into(), "Manual action remains".into(), 3)?;
    let original = broker.task_controller.current()?.unwrap();
    broker.refresh_verification_gate().await?;
    assert_eq!(broker.task_controller.current()?.unwrap().revision, original.revision);
    std::fs::write(workspace.path().join("helper.rs"), "fn helper() {}\n")?;
    broker.refresh_verification_gate().await?;
    let changed = broker.task_controller.current()?.unwrap();
    assert_eq!(changed.id, original.id);
    assert_eq!(changed.phase(), crate::task::TaskPhase::Check);
    let work = changed.turn.as_ref().unwrap().continuation.as_ref().unwrap();
    assert!(work.work.is_checks());
    assert_eq!(work.delivery, crate::task::TaskInputDelivery::Pending);
    assert!(original.permit().validate(Some(&changed)).is_err());
    let work_id = work.id.clone();
    broker.refresh_verification_gate().await?;
    assert_eq!(broker.task_controller.current()?.unwrap().turn.as_ref().unwrap().continuation.as_ref().unwrap().id, work_id);
    broker.run_task_checks().await?;
    assert_eq!(broker.task_controller.current()?.unwrap().phase(), crate::task::TaskPhase::Verify);
    assert_eq!(broker.active_goal()?.continuation.turn_count, 0);
    Ok(())
}

/// Exercise every execution edge against real committed state and reject the complement atomically.
#[tokio::test]
async fn workflow_execution_transition_matrix_preserves_the_gate_graph() -> Result<()> {
    use crate::plan::PlanPhase;
    for original in [PlanPhase::Implement, PlanPhase::Checking, PlanPhase::Resolve, PlanPhase::Verify] {
        let (_workspace, _data, mut broker) = workflow_gate_fixture().await?;
        broker.run_checks().await?;
        let checked = broker.current_execution()?.unwrap();
        for target in [PlanPhase::Implement, PlanPhase::Checking, PlanPhase::Resolve, PlanPhase::Verify] {
            let task = broker.task_controller.current()?.unwrap();
            broker.task_controller.update(&mut broker.store, &task.permit(), 10, |draft| {
                let crate::task::WorkflowSnapshot::Execute { execution } = &mut draft.workflow else { unreachable!() };
                execution.record = checked.clone();
                execution.record.phase = original;
                Ok(())
            })?;
            let before = broker.task_controller.current()?.unwrap();
            let mut next = broker.current_execution()?.unwrap();
            next.phase = target;
            next.generation += 1;
            let result = broker.task_controller.apply(&mut broker.store, &before.permit(),
                crate::task::TaskEvent::ExecutionUpdated(next), 11);
            let allowed = original == target || match original {
                PlanPhase::Implement => target == PlanPhase::Checking,
                PlanPhase::Checking => matches!(target, PlanPhase::Resolve | PlanPhase::Verify),
                PlanPhase::Resolve => matches!(target, PlanPhase::Checking | PlanPhase::Verify),
                PlanPhase::Verify => matches!(target, PlanPhase::Resolve | PlanPhase::Checking),
            };
            assert_eq!(result.is_ok(), allowed, "{original:?} -> {target:?}: {result:?}");
            if !allowed {
                assert_eq!(broker.task_controller.current()?.unwrap().revision, before.revision);
                assert_eq!(broker.store.load_active_task(&broker.session.id)?.unwrap().revision, before.revision);
            }
        }
    }
    Ok(())
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
        assert!(serde_json::from_value::<crate::task::Task>(invalid).is_err(), "accepted incomplete payload at {pointer}");
    }
    assert_eq!(broker.store.load_active_task(&broker.session.id)?.unwrap().revision, task.revision);
    Ok(())
}

/// Credit Goal work for observable content changes while leaving tool activity and phases alone.
#[tokio::test]
async fn workflow_goal_progress_uses_workspace_content_without_a_check_phase() -> Result<()> {
    let workspace = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(workspace.path(), data.path(), false);
    broker.task_controller.change(&mut broker.store, None,
        crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
            "Repair the workspace".into(), Default::default(), false, 1))), 1)?;
    assert!(!broker.observe_goal_workspace().await?);
    assert!(!broker.observe_goal_workspace().await?);
    std::fs::write(workspace.path().join("goal.rs"), "fn changed() {}\n")?;
    assert!(broker.observe_goal_workspace().await?);
    assert!(!broker.observe_goal_workspace().await?);
    std::fs::remove_file(workspace.path().join("goal.rs"))?;
    assert!(broker.observe_goal_workspace().await?);
    let task = broker.task_controller.current()?.unwrap();
    assert_eq!(task.phase(), crate::task::TaskPhase::Work);
    assert_eq!(task.next_action(), crate::task::TaskNextAction::Model);
    assert_eq!(broker.active_goal()?.continuation.turn_count, 0);
    Ok(())
}

/// Route unchanged repairs directly to Verify and require fresh checks after a source repair.
#[tokio::test]
async fn workflow_resolution_reuses_only_current_passing_checks() -> Result<()> {
    for changed in [false, true] {
        let (workspace, _data, mut broker) = workflow_gate_fixture().await?;
        broker.run_checks().await?;
        let mut record = broker.current_execution()?.unwrap();
        assert_eq!(record.phase, crate::plan::PlanPhase::Verify);
        record.phase = crate::plan::PlanPhase::Resolve;
        record.generation += 1;
        record.findings = vec!["Inspect the manual observation again".into()];
        let task = broker.task_controller.current()?.unwrap();
        broker.task_controller.apply(&mut broker.store, &task.permit(),
            crate::task::TaskEvent::ExecutionUpdated(record.clone()), 3)?;
        if changed {
            std::fs::write(workspace.path().join("helper.rs"), "fn helper() {}\n")?;
        }
        let task = broker.task_controller.current()?.unwrap();
        let history_count = broker.store.list_exchange(&broker.session.id)?.len();
        let mut exchange = broker.store.latest_exchange(&broker.session.id)?.unwrap();
        broker.handle_execution_control(&record.id, record.generation, &task.permit(),
            crate::plan::execution::ExecutionOperation::Phase(crate::plan::execution::PlanPhaseDone {
                phase: crate::plan::PlanPhase::Resolve, revision: record.revision,
                summary: "Resolved the recorded observation".into(), verification: None,
            }), &mut exchange).await?;
        let next = broker.current_execution()?.unwrap();
        assert_eq!(next.phase, if changed { crate::plan::PlanPhase::Checking } else { crate::plan::PlanPhase::Verify });
        assert_eq!(next.check_runs.len(), 1);
        assert_eq!(broker.store.list_exchange(&broker.session.id)?.len(), history_count,
            "local gate transitions created an unsolicited provider exchange");
        assert_eq!(broker.active_goal()?.continuation.turn_count, 0, "local checks consumed a model turn");
        assert!(task.permit().validate(broker.task_controller.current()?.as_ref()).is_err());
        if changed {
            broker.run_checks().await?;
            let checked = broker.current_execution()?.unwrap();
            assert_eq!(checked.phase, crate::plan::PlanPhase::Verify);
            assert_eq!(checked.check_runs.len(), 2);
        }
    }
    Ok(())
}
