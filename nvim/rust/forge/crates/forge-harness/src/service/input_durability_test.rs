use super::*;

/// Answer admission must remain automatic when no UI exists and legacy control/broker resources are held.
#[tokio::test]
async fn question_input_owner_admits_headlessly_without_waiting_for_legacy_locks() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller.coordinator.control.acquire().await?;
    let broker = controller.broker.lock().await;
    let questions = controller.questions.ask(
        "headless-question",
        "question-exchange",
        crate::question::QuestionOrigin::Discussion(None),
        vec![crate::question::QuestionDraft {
            header: "Scope".into(),
            question: "Which scope?".into(),
            requirement: crate::question::QuestionRequirement::Optional,
            options: Vec::new(),
        }],
    )?;
    controller
        .questions
        .apply(crate::question::QuestionCommand {
            operation_id: "headless-answer".into(),
            question_id: questions[0].clone(),
            revision: 1,
            action: crate::question::QuestionAction::Answer {
                answer: crate::question::QuestionAnswer::Other {
                    text: "Workspace".into(),
                },
            },
        })?;
    let message = controller.questions.outbox()?[0].clone();
    let receipt = tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            let receipt = controller
                .coordinator
                .submissions
                .admission_receipt(&format!("question:{}", message.id))?;
            if let Some(receipt) = receipt
                && controller.questions.outbox()?.is_empty()
            {
                break Ok::<_, anyhow::Error>(receipt);
            }
            tokio::task::yield_now().await;
        }
    })
    .await??;
    assert!(receipt.error_code.is_none());
    let queued = controller.coordinator.submissions.snapshot()?;
    assert_eq!(queued.messages.len(), 1);
    assert_eq!(queued.messages[0].id, message.id);
    assert!(
        matches!(&queued.messages[0].purpose, crate::input::InputPurpose::Question { question_id, reply, .. } if question_id == &questions[0] && reply == &message.reply)
    );
    assert!(controller.coordinator.execution.try_acquire().is_err());
    assert!(
        controller
            .coordinator
            .control
            .clone()
            .try_acquire_owned()
            .is_err()
    );
    drop(broker);
    drop(control);
    drop(execution);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Recovery after source acknowledgment failure must preserve both input and notification identity.
#[tokio::test]
async fn question_input_acknowledgment_retry_reuses_immutable_receipt_and_publication() -> Result<()>
{
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let questions = controller.questions.ask(
        "retry-question",
        "question-exchange",
        crate::question::QuestionOrigin::Discussion(None),
        vec![crate::question::QuestionDraft {
            header: "Scope".into(),
            question: "Which scope?".into(),
            requirement: crate::question::QuestionRequirement::Optional,
            options: Vec::new(),
        }],
    )?;
    controller
        .questions
        .apply(crate::question::QuestionCommand {
            operation_id: "retry-answer".into(),
            question_id: questions[0].clone(),
            revision: 1,
            action: crate::question::QuestionAction::Answer {
                answer: crate::question::QuestionAnswer::Other {
                    text: "Workspace".into(),
                },
            },
        })?;
    let message = controller.questions.outbox()?[0].clone();
    let connection = rusqlite::Connection::open(controller.data_root.join("harness.sqlite3"))?;
    connection.busy_timeout(Duration::from_secs(5))?;
    connection.execute_batch("CREATE TRIGGER reject_question_ack BEFORE UPDATE ON session_question_state WHEN json_array_length(json_extract(OLD.payload,'$.outbox')) > json_array_length(json_extract(NEW.payload,'$.outbox')) BEGIN SELECT RAISE(ABORT,'injected question acknowledgment failure'); END;")?;
    let receipt = tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            if let Some(receipt) = controller
                .coordinator
                .submissions
                .admission_receipt(&format!("question:{}", message.id))?
            {
                break Ok::<_, anyhow::Error>(receipt);
            }
            tokio::task::yield_now().await;
        }
    })
    .await??;
    assert_eq!(controller.questions.outbox()?.len(), 1);
    controller
        .coordinator
        .submissions
        .suspend(true, Some("intervening-control"))?;
    assert!(controller.coordinator.submissions.snapshot()?.revision > receipt.revision);
    connection.execute_batch("DROP TRIGGER reject_question_ack;")?;
    tokio::time::timeout(Duration::from_secs(2), async {
        while !controller.questions.outbox()?.is_empty() {
            tokio::task::yield_now().await;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await??;
    let recovered = controller
        .coordinator
        .submissions
        .admission_receipt(&format!("question:{}", message.id))?
        .unwrap();
    assert_eq!(recovered.revision, receipt.revision);
    assert_eq!(recovered.control_revision, receipt.control_revision);
    let count: i64 = connection.query_row("SELECT COUNT(*) FROM owner_publication_origin WHERE session_id=?1 AND json_extract(origin,'$.kind')='question_input'", [controller.observer.session_id()], |row| row.get(0))?;
    assert_eq!(count, 1);
    assert_eq!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .len(),
        1
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// A cancelled origin cannot be reintroduced by a pending automatic answer-admission notification.
#[tokio::test]
async fn question_input_owner_does_not_revive_a_cancelled_answer() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let questions = controller.questions.ask(
        "cancelled-question",
        "question-exchange",
        crate::question::QuestionOrigin::Discussion(None),
        vec![crate::question::QuestionDraft {
            header: "Scope".into(),
            question: "Which scope?".into(),
            requirement: crate::question::QuestionRequirement::Optional,
            options: Vec::new(),
        }],
    )?;
    controller
        .questions
        .apply(crate::question::QuestionCommand {
            operation_id: "cancelled-answer".into(),
            question_id: questions[0].clone(),
            revision: 1,
            action: crate::question::QuestionAction::Answer {
                answer: crate::question::QuestionAnswer::Other {
                    text: "Workspace".into(),
                },
            },
        })?;
    controller
        .questions
        .cancel_chat(Some("question-exchange"))?;
    controller.exchange_owner.reconcile_questions(None).await?;
    assert!(controller.questions.outbox()?.is_empty());
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Owner reconciliation reuses the task's original entry identity without manufacturing native delivery.
#[tokio::test]
async fn task_input_producer_reuses_original_entry_without_dispatch() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let mut task = crate::task::Task::goal("Produce once".into(), Default::default(), 1);
    task.request_entry("Original task instruction".into(), 1)?;
    let input = task.entry.as_ref().unwrap().id.clone();
    controller.task_controller.change(
        &mut SqliteStore::open(&controller.data_root)?,
        None,
        crate::task::TaskChange::Install(Box::new(task)),
        1,
    )?;
    let (sink, _output) = forge_protocol::outbound::channel();
    controller
        .exchange_owner
        .reconcile_task_input(sink.clone())
        .await?;
    controller.exchange_owner.reconcile_task_input(sink).await?;
    let snapshot = controller.coordinator.submissions.snapshot()?;
    assert_eq!(snapshot.messages.len(), 1);
    let queued = &snapshot.messages[0];
    assert_eq!(queued.id, input.as_str());
    assert_eq!(queued.text, "Original task instruction");
    assert_eq!(queued.delivery(), crate::input::InputDelivery::Queued);
    assert!(queued.submission_id().is_none());
    assert_eq!(
        controller
            .task_controller
            .current()?
            .unwrap()
            .entry
            .as_ref()
            .unwrap()
            .delivery,
        crate::task::TaskInputDelivery::Pending
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Native dispatch and later cancellation remain owned when no UI publisher has ever attached.
#[tokio::test]
async fn task_creation_without_transport_retains_publications_and_confirms_dispatch() -> Result<()>
{
    use crate::exchange::command::{
        CancelExchangeRequest, CancelledInputPolicy, Command, CommandOutcome, NewGoal, NewPlan,
        TaskCommand, TaskCommandOutcome,
    };
    use crate::provider::mock_response::MockResponseCommand;
    for kind in [crate::task::TaskKind::Goal, crate::task::TaskKind::Plan] {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let controller = receipt_test_controller(root.path()).await?;
        let session = controller.observer.session_id().to_owned();
        let provider = controller.provider_handle();
        let script = provider.mock_response_control().unwrap();
        let held = script
            .dispatch(
                &session,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold = held["id"].as_str().unwrap().to_owned();
        let command = match kind {
            crate::task::TaskKind::Goal => {
                TaskCommand::StartGoal(NewGoal::new("Headless goal".into())?)
            }
            crate::task::TaskKind::Plan => {
                TaskCommand::StartPlan(NewPlan::new("Headless plan".into())?)
            }
            crate::task::TaskKind::Execute => unreachable!("execution requires reviewed evidence"),
        };
        let completed = tokio::time::timeout(
            Duration::from_secs(5),
            controller
                .exchange_owner
                .execute_command(Command::Task(command)),
        )
        .await??;
        let CommandOutcome::Task(TaskCommandOutcome::Started { task, input, .. }) =
            completed.outcome
        else {
            anyhow::bail!("headless startup did not confirm dispatch");
        };
        tokio::time::timeout(
            Duration::from_secs(5),
            script.dispatch(&session, MockResponseCommand::Wait { id: hold }),
        )
        .await??;
        let current = controller.task_controller.current()?.unwrap();
        assert_eq!(current.id.as_str(), task.id().as_str());
        assert_eq!(current.entry.as_ref().unwrap().id, input);
        assert_eq!(current.kind(), kind);
        let connection = rusqlite::Connection::open(controller.data_root.join("harness.sqlite3"))?;
        let retained: i64 = connection.query_row(
            "SELECT COUNT(*) FROM owner_publication WHERE session_id=?1",
            [&session],
            |row| row.get(0),
        )?;
        let offered: i64 = connection.query_row("SELECT COUNT(*) FROM owner_publication WHERE session_id=?1 AND json_extract(state,'$.state')!='pending'", [&session], |row| row.get(0))?;
        assert!(retained > 0);
        assert_eq!(offered, 0);
        tokio::time::timeout(
            Duration::from_secs(5),
            controller
                .exchange_owner
                .execute_command(Command::CancelExchange(CancelExchangeRequest {
                    input: CancelledInputPolicy::Retain,
                })),
        )
        .await??;
        assert_eq!(
            controller.task_controller.current()?.unwrap().status(),
            crate::task::TaskStatus::Paused,
            "exchange cancellation must pause its selected task"
        );
        controller.finish_exchange_owner().await?;
    }
    Ok(())
}

/// Saved-plan preflight cannot collect unrelated work or retarget a changed working plan.
#[tokio::test]
async fn plan_source_rejection_precedes_collection_and_task_mutation() -> Result<()> {
    use crate::exchange::command::{
        Command, CommandFailure, CommandId, CommandRequest, ExecuteCommandError, InputControl,
        PlanRevisionSelection, PlanSelection, TaskCommand, TaskCommandFailure,
    };
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let mut store = SqliteStore::open(&controller.data_root)?;
    let task = controller
        .task_controller
        .change(
            &mut store,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Keep current work".into(),
                Default::default(),
                1,
            ))),
            1,
        )?
        .unwrap();
    let mut foreign = store
        .load_session(controller.observer.session_id())?
        .unwrap();
    foreign.id = "foreign-source-owner".into();
    foreign.primary_agent_id = crate::session::HarnessSession::primary_agent_id(&foreign.id);
    foreign.workspace = root
        .path()
        .join("another-workspace")
        .to_string_lossy()
        .into_owned();
    store.save_session(&foreign)?;
    let mut artifact: crate::plan::PlanRecord = serde_json::from_value(json!({
        "id":"guarded-source","session_id":foreign.id,"request":"Saved design","state":"generating",
        "working_path":"working.json","document_version":1,"model_revision":1,"user_revision":0,
        "created_at_ms":1,"updated_at_ms":1,
    }))?;
    store.save_plan(&artifact)?;
    let source = PlanSelection::capture(&artifact)?;
    let (sink, _output) = forge_protocol::outbound::channel();
    for (index, (command, expected_code)) in [
        (
            TaskCommand::AttachPlan(source.clone()),
            "plan_foreign_session",
        ),
        (
            TaskCommand::ForkPlan(source.clone()),
            "plan_foreign_session",
        ),
        (
            TaskCommand::Replan(PlanRevisionSelection::new(source, 1)?),
            "plan_foreign_workspace",
        ),
    ]
    .into_iter()
    .enumerate()
    {
        let result = controller
            .exchange_owner
            .execute_identified(
                CommandRequest {
                    id: CommandId::new(format!("rejected-source-{index}"))?,
                    command: Command::Task(command),
                    input_control: InputControl::AtExecution,
                },
                sink.clone(),
            )
            .await;
        assert!(
            matches!(result, Err(ExecuteCommandError::Failed(CommandFailure::Task(ref error))) if error.code() == expected_code)
        );
        assert_eq!(
            controller.task_controller.current()?.unwrap().revision,
            task.revision
        );
        assert!(!*controller.cancellation.subscribe().borrow());
        assert!(!controller.coordinator.submissions.snapshot()?.suspended);
    }
    artifact.session_id = controller.observer.session_id().into();
    artifact.id = "stale-working-source".into();
    store.save_plan(&artifact)?;
    let stale = PlanSelection::capture(&artifact)?;
    artifact.document_version += 1;
    store.save_plan(&artifact)?;
    assert!(matches!(
        controller
            .exchange_owner
            .execute_identified(
                CommandRequest {
                    id: CommandId::new("rejected-stale-source")?,
                    command: Command::Task(TaskCommand::ForkPlan(stale)),
                    input_control: InputControl::AtExecution,
                },
                sink.clone()
            )
            .await,
        Err(ExecuteCommandError::Failed(CommandFailure::Task(
            TaskCommandFailure::PlanChanged
        )))
    ));
    assert_eq!(
        controller.task_controller.current()?.unwrap().revision,
        task.revision
    );
    artifact.accepted_revision = Some(1);
    artifact.state = crate::plan::PlanState::Accepted;
    store.save_plan(&artifact)?;
    let accepted = PlanSelection::capture(&artifact)?;
    assert!(matches!(
        controller
            .exchange_owner
            .execute_identified(
                CommandRequest {
                    id: CommandId::new("rejected-accepted-attachment")?,
                    command: Command::Task(TaskCommand::AttachPlan(accepted)),
                    input_control: InputControl::AtExecution,
                },
                sink
            )
            .await,
        Err(ExecuteCommandError::Failed(CommandFailure::Task(
            TaskCommandFailure::PlanRequiresFork
        )))
    ));
    assert_eq!(
        controller.task_controller.current()?.unwrap().revision,
        task.revision
    );
    assert!(!*controller.cancellation.subscribe().borrow());
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// A saved plan awaiting decisions attaches without model input, and reattachment preserves its answers and authority.
#[tokio::test]
async fn waiting_plan_attachment_completes_without_dispatch_and_preserves_decisions() -> Result<()>
{
    use crate::exchange::command::{
        Command, CommandId, CommandOutcome, CommandRequest, InputCommand, InputControl,
        PlanSelection, TaskCommand, TaskCommandOutcome,
    };
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let mut store = SqliteStore::open(&controller.data_root)?;
    controller.task_controller.change(
        &mut store,
        None,
        crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
            "Displaced task".into(),
            Default::default(),
            1,
        ))),
        1,
    )?;
    let plan: crate::plan::PlanRecord = serde_json::from_value(json!({
        "id":"waiting-source","session_id":controller.observer.session_id(),"request":"Decide first",
        "state":"awaiting_input","working_path":"waiting.json","document_version":1,"model_revision":1,
        "user_revision":0,"created_at_ms":1,"updated_at_ms":1,
        "elicitation":{"revision":1,"answer_revision":7,"current_index":1,
            "question_set":{"id":"saved-decisions","questions":[
                {"id":"answered","header":"First","question":"Keep the answer?"},
                {"id":"pending","header":"Second","question":"Choose the remaining behavior"}
            ]},"answer_order":{"answered":["saved-message",7]},
            "answer":[{"question_id":"answered","response":{"kind":"other","text":"Keep it"}}]
        }
    }))?;
    store.save_plan(&plan)?;
    let source = PlanSelection::capture(&plan)?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let request = CommandRequest {
        id: CommandId::new("attach-waiting-source")?,
        command: Command::Task(TaskCommand::AttachPlan(source.clone())),
        input_control: InputControl::AtExecution,
    };
    let result = tokio::time::timeout(
        Duration::from_secs(5),
        controller
            .exchange_owner
            .execute_identified(request.clone(), sink.clone()),
    )
    .await??;
    assert!(
        matches!(result.outcome, CommandOutcome::Task(TaskCommandOutcome::Attached { ref plan, .. }) if plan.as_str() == "waiting-source")
    );
    let attached = controller.task_controller.current()?.unwrap();
    assert_eq!(attached.status(), crate::task::TaskStatus::Waiting);
    assert!(attached.entry.is_none());
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    assert!(
        store
            .list_exchange(controller.observer.session_id())?
            .is_empty()
    );
    assert_eq!(
        serde_json::to_value(store.load_plan("waiting-source")?.unwrap().elicitation)?,
        serde_json::to_value(&plan.elicitation)?
    );
    tokio::time::timeout(
        Duration::from_secs(5),
        controller
            .exchange_owner
            .execute_command(Command::Input(InputCommand::ResumeDelivery)),
    )
    .await??;
    let before = serde_json::to_value(&attached)?;
    let timeline = store
        .list_session_event(controller.observer.session_id())?
        .len();
    let confirmed = tokio::time::timeout(
        Duration::from_secs(5),
        controller.exchange_owner.execute_identified(
            CommandRequest {
                id: CommandId::new("confirm-current-waiting-plan")?,
                command: Command::Task(TaskCommand::AttachPlan(source)),
                input_control: InputControl::AtExecution,
            },
            sink.clone(),
        ),
    )
    .await??;
    assert!(matches!(
        confirmed.outcome,
        CommandOutcome::Task(TaskCommandOutcome::Attached { .. })
    ));
    assert_eq!(
        serde_json::to_value(controller.task_controller.current()?.unwrap())?,
        before
    );
    assert_eq!(
        store
            .list_session_event(controller.observer.session_id())?
            .len(),
        timeline
    );
    assert_eq!(
        serde_json::to_value(store.load_plan("waiting-source")?.unwrap().elicitation)?,
        serde_json::to_value(&plan.elicitation)?
    );
    let replay = controller
        .exchange_owner
        .execute_identified(request, sink)
        .await?;
    assert_eq!(replay.sequence, result.sequence);
    assert_eq!(
        serde_json::to_value(controller.task_controller.current()?.unwrap())?,
        before
    );
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Policy updates neither wait for native lanes nor let a replay overwrite a later committed policy.
#[tokio::test]
async fn task_policy_commits_with_control_and_execution_held_without_replaying_later_policy()
-> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandOutcome, CommandRequest, InputControl, PlanAcceptancePolicy,
        TaskCommand, TaskCommandOutcome, TaskSelection,
    };
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let plan: crate::plan::PlanRecord = serde_json::from_value(json!({
        "id":"owned-policy-plan","session_id":controller.observer.session_id(),"request":"Plan a change",
        "state":"configuring_checks","working_path":"working.json","model_revision":0,"user_revision":0,"created_at_ms":1,"updated_at_ms":1,
    }))?;
    let task = controller
        .task_controller
        .change(
            &mut SqliteStore::open(&controller.data_root)?,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::plan(plan, 1)?)),
            1,
        )?
        .unwrap();
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let request = CommandRequest {
        id: CommandId::new("accept-policy-on")?,
        command: Command::Task(TaskCommand::SetAutoAccept(PlanAcceptancePolicy {
            target: TaskSelection::capture(&task),
            enabled: true,
        })),
        input_control: InputControl::AtExecution,
    };
    let committed = tokio::time::timeout(
        Duration::from_secs(5),
        controller
            .exchange_owner
            .execute_identified(request.clone(), sink.clone()),
    )
    .await??;
    assert!(matches!(
        committed.outcome,
        CommandOutcome::Task(TaskCommandOutcome::AutoAcceptChanged { enabled: true, .. })
    ));
    controller
        .exchange_owner
        .execute_command(Command::Task(TaskCommand::SetAutoAccept(
            PlanAcceptancePolicy {
                target: TaskSelection::capture(&task),
                enabled: false,
            },
        )))
        .await?;
    let replay = controller
        .exchange_owner
        .execute_identified(request, sink)
        .await?;
    assert_eq!(replay.sequence, committed.sequence);
    assert!(
        !controller
            .task_controller
            .current()?
            .unwrap()
            .auto_accept_plan
    );
    assert_eq!(
        controller.task_controller.current()?.unwrap().revision,
        task.revision
    );
    assert_eq!(controller.coordinator.control.available_permits(), 0);
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    drop(control);
    drop(execution);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Reject a stale task target before changing input policy or collecting another task's work.
#[tokio::test]
async fn task_control_rejects_replaced_task_before_suspending_or_collecting_work() -> Result<()> {
    use crate::exchange::command::{
        Command, CommandFailure, ExecuteCommandError, TaskCommand, TaskCommandFailure,
        TaskSelection,
    };
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let mut store = SqliteStore::open(&controller.data_root)?;
    let original = controller
        .task_controller
        .change(
            &mut store,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Original".into(),
                Default::default(),
                1,
            ))),
            1,
        )?
        .unwrap();
    let replacement = controller
        .task_controller
        .change(
            &mut store,
            Some(&original.revision),
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Replacement".into(),
                Default::default(),
                2,
            ))),
            2,
        )?
        .unwrap();
    let result = controller
        .exchange_owner
        .execute_command(Command::Task(TaskCommand::Pause(TaskSelection::capture(
            &original,
        ))))
        .await;
    assert!(matches!(
        result,
        Err(ExecuteCommandError::Failed(CommandFailure::Task(
            TaskCommandFailure::Changed
        )))
    ));
    assert_eq!(
        controller.task_controller.current()?.unwrap().revision,
        replacement.revision
    );
    assert!(!controller.coordinator.submissions.snapshot()?.suspended);
    assert!(!*controller.cancellation.subscribe().borrow());
    assert_eq!(controller.coordinator.control.available_permits(), 1);
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// A replay uses the original task target even after clear removes the current task.
#[tokio::test]
async fn task_controls_replay_original_target_after_clear_without_nested_admission() -> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandOutcome, CommandRequest, InputControl, TaskCommand,
        TaskCommandOutcome, TaskSelection,
    };
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let mut store = SqliteStore::open(&controller.data_root)?;
    let task = controller
        .task_controller
        .change(
            &mut store,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Pause and clear once".into(),
                Default::default(),
                1,
            ))),
            1,
        )?
        .unwrap();
    let request = CommandRequest {
        id: CommandId::new("pause-once")?,
        command: Command::Task(TaskCommand::Pause(TaskSelection::capture(&task))),
        input_control: InputControl::AtExecution,
    };
    let (sink, _output) = forge_protocol::outbound::channel();
    let paused = tokio::time::timeout(
        Duration::from_secs(5),
        controller
            .exchange_owner
            .execute_identified(request.clone(), sink.clone()),
    )
    .await??;
    assert!(matches!(
        paused.outcome,
        CommandOutcome::Task(TaskCommandOutcome::Paused { .. })
    ));
    let current = controller.task_controller.current()?.unwrap();
    let clear = tokio::time::timeout(
        Duration::from_secs(5),
        controller
            .exchange_owner
            .execute_command(Command::Task(TaskCommand::Clear(TaskSelection::capture(
                &current,
            )))),
    )
    .await??;
    assert!(matches!(
        clear.outcome,
        CommandOutcome::Task(TaskCommandOutcome::Cleared { .. })
    ));
    assert!(controller.task_controller.current()?.is_none());
    let legacy =
        crate::task::command::SessionCommand::parse_text("/goal pause", Some("pause-once"))?
            .unwrap();
    super::task_state::apply_command(
        &controller,
        controller.observer.session_id(),
        &legacy,
        None,
        20,
        &sink,
    )
    .await?;
    assert!(controller.task_controller.current()?.is_none());
    let replay = controller
        .exchange_owner
        .execute_identified(request, sink)
        .await?;
    assert_eq!(replay.sequence, paused.sequence);
    assert_eq!(replay.revision, paused.revision);
    assert_eq!(controller.coordinator.control.available_permits(), 1);
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Readiness selects current input, bypasses queued drafts, and retains sends after a waiter drops.
#[tokio::test]
async fn input_readiness_owns_steering_burst_without_waiting_for_acknowledgments() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    controller.coordinator.submissions.apply(serde_json::from_value(json!({
        "id":"submit-draft","action":{"kind":"submit","message":{"id":"draft","text":"Wait for a new turn","intent":"queue"}},
    }))?)?;
    queued_offer_input(&controller, "first", "First immediate input", None)?;
    queued_offer_input(&controller, "second", "Second immediate input", None)?;
    let execution = controller.coordinator.execution.acquire().await?;
    let mut active = controller.parent_input.activate(None)?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let mut readiness = Box::pin(controller.exchange_owner.input_ready(sink.clone()));
    assert!(futures_util::poll!(readiness.as_mut()).is_pending());
    drop(readiness);
    let first = tokio::time::timeout(Duration::from_secs(5), active.receive())
        .await?
        .context("dropped readiness waiter cancelled its admitted send")?;
    assert_eq!(first.text, "First immediate input");
    let first_receipt = first
        .submission
        .as_ref()
        .context("first input has no receipt")?
        .clone();
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            let changed = controller.coordinator.submissions.changed.notified();
            match controller.exchange_owner.input_ready(sink.clone()).await? {
                crate::exchange::InputReadiness::Started => return Ok::<(), anyhow::Error>(()),
                crate::exchange::InputReadiness::Waiting => changed.await,
                crate::exchange::InputReadiness::WaitExecution => {
                    anyhow::bail!("second immediate input waited for native acknowledgment")
                }
            }
        }
    })
    .await??;
    let second = tokio::time::timeout(Duration::from_secs(5), active.receive())
        .await?
        .context("second immediate input was not handed off")?;
    assert_eq!(second.text, "Second immediate input");
    let second_receipt = second
        .submission
        .as_ref()
        .context("second input has no receipt")?
        .clone();
    assert_eq!(
        first_receipt.record()?.delivery,
        crate::provider::submission::SubmissionState::Prepared
    );
    assert_ne!(first_receipt.receipt_id(), second_receipt.receipt_id());
    second_receipt.sending(None)?;
    second_receipt.accepted(None)?;
    second.complete(Ok(())).await;
    first_receipt.sending(None)?;
    first_receipt.accepted(None)?;
    first.complete(Ok(())).await;
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            let changed = controller.coordinator.submissions.changed.notified();
            let queue = controller.coordinator.submissions.snapshot()?;
            if queue.messages.len() == 1 {
                assert_eq!(queue.messages[0].id, "draft");
                assert_eq!(
                    queue.messages[0].delivery(),
                    crate::input::InputDelivery::Queued
                );
                return Ok::<(), anyhow::Error>(());
            }
            changed.await;
        }
    })
    .await??;
    drop(active);
    drop(execution);
    controller.finish_exchange_owner().await?;
    let registry = controller.broker.lock().await.execution_registry();
    registry.close().await?;
    Ok(())
}

/// A closed execution fence cannot be bypassed merely because an old steering receiver remains open.
#[tokio::test]
async fn input_readiness_does_not_steer_through_closed_execution_admission() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    queued_offer_input(&controller, "fenced", "Preserve this input", None)?;
    let mut active = controller.parent_input.activate(None)?;
    controller.coordinator.execution.close();
    let (sink, _output) = forge_protocol::outbound::channel();
    assert!(matches!(
        controller.exchange_owner.input_ready(sink).await?,
        crate::exchange::InputReadiness::Waiting
    ));
    assert!(futures_util::poll!(std::pin::pin!(active.receive())).is_pending());
    assert_eq!(
        controller.coordinator.submissions.snapshot()?.messages[0].delivery(),
        crate::input::InputDelivery::Queued
    );
    assert_eq!(controller.coordinator.control.available_permits(), 1);
    drop(active);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Steering remains owner-controlled after its offer waiter drops, and handoff precedes acknowledgment.
#[tokio::test]
async fn steering_owner_retains_dropped_offer_and_settles_receipt_after_handoff() -> Result<()> {
    #[derive(Debug)]
    enum ControlChange {
        Unchanged,
        Cancellation,
    }
    for change in [ControlChange::Unchanged, ControlChange::Cancellation] {
        let root = tempfile::tempdir()?;
        let controller = receipt_test_controller(root.path()).await?;
        let input_id = "owned-steering";
        let input = queued_offer_input(&controller, input_id, "Keep the captured target", None)?;
        let execution = controller.coordinator.execution.acquire().await?;
        let mut active = controller.parent_input.activate(None)?;
        let ordering = Arc::new(tokio::sync::Semaphore::new(1));
        let control = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        let order = ordering.clone().acquire_owned().await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let mut offered = Box::pin(controller.exchange_owner.offer_steering(
            crate::exchange::SteeringDeliveryOffer {
                input,
                control,
                order,
            },
            sink.clone(),
        ));
        assert!(futures_util::poll!(offered.as_mut()).is_pending());
        drop(offered);
        let command = tokio::time::timeout(Duration::from_secs(5), active.receive())
            .await
            .with_context(|| format!("{change:?}: receive original steering"))?
            .context("owned steering was not delivered")?;
        let receipt = command
            .submission
            .as_ref()
            .context("steering has no durable receipt")?
            .clone();
        assert_eq!(
            receipt.record()?.input_message_id.as_deref(),
            Some(input_id)
        );
        let handoff =
            tokio::time::timeout(Duration::from_secs(5), ordering.clone().acquire_owned())
                .await
                .with_context(|| format!("{change:?}: release original handoff ordering"))??;
        assert_eq!(
            receipt.record()?.delivery,
            crate::provider::submission::SubmissionState::Prepared
        );
        drop(handoff);
        receipt.sending(None)?;
        receipt.accepted(None)?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let changed = controller.coordinator.submissions.changed.notified();
                if controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .iter()
                    .any(|message| {
                        message.id == input_id
                            && message.delivery() == crate::input::InputDelivery::Accepted
                    })
                {
                    return Ok::<(), anyhow::Error>(());
                }
                changed.await;
            }
        })
        .await
        .with_context(|| format!("{change:?}: observe original receipt acceptance"))??;
        // The original receipt remains observable after dispatch authority changes.
        match change {
            ControlChange::Unchanged => {}
            ControlChange::Cancellation => controller.cancellation.request_native_cancel(),
        }
        command.complete(Ok(())).await;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let changed = controller.coordinator.submissions.changed.notified();
                let snapshot = controller.coordinator.submissions.snapshot()?;
                assert!(
                    snapshot
                        .messages
                        .iter()
                        .all(|message| message.delivery() != crate::input::InputDelivery::Failed),
                    "accepted receipt completion failed its input"
                );
                if snapshot.messages.is_empty() {
                    return Ok::<(), anyhow::Error>(());
                }
                changed.await;
            }
        })
        .await
        .with_context(|| format!("{change:?}: settle original accepted receipt"))??;
        controller.cancellation.arm();

        // Reuse the same captured lane after receipt settlement, rather than recovering a fenced owner.
        let input = queued_offer_input(
            &controller,
            "reused-steering",
            "Use the same owner again",
            None,
        )?;
        assert!(
            controller
                .exchange_owner
                .offer_steering(
                    crate::exchange::SteeringDeliveryOffer {
                        input,
                        control: controller
                            .coordinator
                            .control
                            .clone()
                            .acquire_owned()
                            .await?,
                        order: ordering.clone().acquire_owned().await?,
                    },
                    sink.clone()
                )
                .await?
        );
        let reused = tokio::time::timeout(Duration::from_secs(5), active.receive())
            .await
            .with_context(|| format!("{change:?}: receive successor steering"))?
            .context("receipt settlement fenced subsequent steering")?;
        let reused_receipt = reused
            .submission
            .as_ref()
            .context("reused steering has no receipt")?
            .clone();
        reused_receipt.sending(None)?;
        reused_receipt.accepted(None)?;
        controller
            .exchange_owner
            .observe_input_receipt(
                crate::exchange::InputReceiptFact::Acceptance {
                    submission: reused_receipt.receipt_id().clone(),
                },
                None,
            )
            .await?;
        reused.complete(Ok(())).await;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let changed = controller.coordinator.submissions.changed.notified();
                let snapshot = controller.coordinator.submissions.snapshot()?;
                assert!(
                    snapshot
                        .messages
                        .iter()
                        .all(|message| message.delivery() != crate::input::InputDelivery::Failed)
                );
                if snapshot.messages.is_empty() {
                    return Ok::<(), anyhow::Error>(());
                }
                changed.await;
            }
        })
        .await
        .with_context(|| format!("{change:?}: settle successor accepted receipt"))??;
        controller
            .exchange_owner
            .command_status(crate::exchange::command::CommandId::new(
                "settled-receipt-barrier",
            )?)
            .await?;
        while let Ok(frame) = output.try_recv() {
            if let forge_protocol::message::Message::Event(event) =
                serde_json::from_slice(frame.bytes())?
            {
                assert!(
                    !(event.event == "provider_event" && event.payload["kind"] == "error"),
                    "successful receipt completion published failure: {:?}",
                    event.payload
                );
            }
        }
        assert!(
            SqliteStore::read_only(&controller.data_root)?
                .latest_exchange(controller.observer.session_id())?
                .is_none(),
            "steering must not manufacture a new foreground exchange"
        );
        drop(active);
        drop(execution);
        controller.finish_exchange_owner().await?;
        let registry = controller.broker.lock().await.execution_registry();
        registry.close().await?;
    }
    Ok(())
}

/// Stale readiness cannot claim a revised input or strand the producer's ordering capabilities.
#[tokio::test]
async fn steering_owner_rejects_stale_offer_without_claiming_input() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let mut input = queued_offer_input(
        &controller,
        "stale-steering",
        "Keep this input queued",
        None,
    )?;
    input.revision += 1;
    let active = controller.parent_input.activate(None)?;
    let ordering = Arc::new(tokio::sync::Semaphore::new(1));
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let order = ordering.clone().acquire_owned().await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    assert!(
        !controller
            .exchange_owner
            .offer_steering(
                crate::exchange::SteeringDeliveryOffer {
                    input,
                    control,
                    order
                },
                sink
            )
            .await?
    );
    assert_eq!(ordering.available_permits(), 1);
    assert_eq!(controller.coordinator.control.available_permits(), 1);
    assert_eq!(
        controller.coordinator.submissions.snapshot()?.messages[0].delivery(),
        crate::input::InputDelivery::Queued
    );
    drop(active);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Legacy steering must commit one shared input, replay its receipt, and never send directly.
#[tokio::test]
async fn legacy_steering_uses_durable_input_command_without_direct_dispatch() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let execution = controller.coordinator.execution.acquire().await?;
    let mut active = controller.parent_input.activate(None)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let mut replies = Vec::new();
    for id in [1, 2] {
        controller.input_request(Request {
            id, method: "session.turn.steer".into(),
            params: json!({"operation_id":"steering-command","text":"Keep the stored input"}),
        }, RuntimeMethod::SessionTurnSteer, &sink).await?;
        loop {
            let frame = output
                .recv()
                .await?
                .context("input command response missing")?;
            let reply: Value = serde_json::from_slice(frame.bytes())?;
            if reply["id"] == id {
                assert!(reply["error"].is_null());
                assert_eq!(
                    reply
                        .pointer("/result/outcome/result/change/change")
                        .and_then(Value::as_str),
                    Some("submitted")
                );
                replies.push(reply["result"].clone());
                break;
            }
        }
    }
    assert_eq!(replies[0], replies[1]);
    let stored = controller.coordinator.submissions.snapshot()?;
    assert_eq!(stored.messages.len(), 1);
    assert_eq!(stored.messages[0].id, "legacy-input:steering-command");
    assert_eq!(
        stored.messages[0].delivery(),
        crate::input::InputDelivery::Queued
    );
    {
        let mut receiving = std::pin::pin!(active.receive());
        assert!(futures_util::poll!(receiving.as_mut()).is_pending());
    }

    let rejected = controller
        .input_request(
            Request {
                id: 3,
                method: "session.turn.steer".into(),
                params: json!({"text":"Cannot target native child work", "target":"native-child"}),
            },
            RuntimeMethod::SessionTurnSteer,
            &sink,
        )
        .await;
    assert!(rejected.is_err());
    assert_eq!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .len(),
        1
    );
    drop(active);
    drop(execution);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Activate a task from durable receipt evidence and settle that receipt after the task is cleared.
#[tokio::test]
async fn receipt_owner_activates_task_before_exposing_acceptance() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let mut task = crate::task::Task::goal(
        "Receipt owner".into(),
        crate::session::continuation::ContinuationBudget::new(20, 2),
        1,
    );
    task.request_entry("Begin owned work".into(), 1)?;
    let task = controller
        .task_controller
        .change(
            &mut SqliteStore::open(&controller.data_root)?,
            None,
            crate::task::TaskChange::Install(Box::new(task)),
            1,
        )?
        .unwrap();
    let id = task.entry.as_ref().unwrap().id.clone();
    let (_, submission) = registered_receipt(
        &controller,
        id.as_str(),
        crate::input::InputPurpose::TaskEntry {
            revision: task.revision.clone(),
            id: id.clone(),
            start: crate::exchange::TaskStart {
                kind: task.kind(),
                title: task.title().into(),
            },
        },
    )
    .await?;
    assert!(
        !controller
            .task_controller
            .current()?
            .unwrap()
            .entry_accepted()
    );
    submission.sending(None)?;
    submission.accepted(None)?;
    controller
        .exchange_owner
        .observe_input_receipt(
            crate::exchange::InputReceiptFact::Acceptance {
                submission: submission.receipt_id().clone(),
            },
            None,
        )
        .await?;
    assert!(
        controller
            .task_controller
            .current()?
            .unwrap()
            .entry_accepted()
    );
    // Task control may supersede dispatch authority after the original receipt was accepted.
    let accepted_task = controller
        .task_controller
        .current()?
        .context("accepted task disappeared")?;
    controller.task_controller.change(
        &mut SqliteStore::open(&controller.data_root)?,
        Some(&accepted_task.revision),
        crate::task::TaskChange::Clear,
        2,
    )?;
    assert!(controller.task_controller.current()?.is_none());
    let settled = controller
        .exchange_owner
        .observe_input_receipt(
            crate::exchange::InputReceiptFact::Settled {
                submission: submission.receipt_id().clone(),
                result: crate::exchange::InputExecutionResult::Completed,
                activity: crate::exchange::InputDispatchActivity::Active,
            },
            None,
        )
        .await?
        .context("accepted receipt settlement produced no input update")?;
    assert!(
        settled.error.is_none(),
        "task replacement failed accepted receipt settlement: {:?}",
        settled.error
    );
    assert!(settled.error_code.is_none());
    assert!(
        controller.task_controller.current()?.is_none(),
        "old receipt revived a cleared task"
    );
    assert!(matches!(
        submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Accepted { .. }
    ));
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Keep both uncertain receipts observed when their workers finish and acceptance arrives together.
#[tokio::test]
async fn owner_retains_uncertain_receipts_and_does_not_lose_ready_siblings() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let (first, first_receipt) =
        registered_receipt(&controller, "first", crate::input::InputPurpose::User).await?;
    let (second, second_receipt) =
        registered_receipt(&controller, "second", crate::input::InputPurpose::User).await?;
    // Concurrent sends require retained steering flights; independent prompt sends are fenced.
    let _first_flight = first_receipt.begin_steering()?;
    let _second_flight = second_receipt.begin_steering()?;
    first_receipt.sending(None).context("first sending")?;
    second_receipt.sending(None).context("second sending")?;
    for input in [first, second] {
        controller
            .exchange_owner
            .observe_input_receipt(
                crate::exchange::InputReceiptFact::WorkerFailed { input },
                None,
            )
            .await
            .context("commit failed worker receipt")?;
    }
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .iter()
            .all(|input| input.delivery() == crate::input::InputDelivery::Unknown)
    );
    first_receipt.accepted(None).context("first accepted")?;
    second_receipt.accepted(None).context("second accepted")?;
    tokio::time::timeout(Duration::from_secs(3), async {
        loop {
            let notified = controller.coordinator.submissions.changed.notified();
            if controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty()
            {
                return Ok::<(), anyhow::Error>(());
            }
            notified.await;
        }
    })
    .await??;
    controller
        .finish_exchange_owner()
        .await
        .context("close after both acceptances")?;
    Ok(())
}

/// Recover acceptance committed after the old owner stopped without replaying the stored prompt.
#[tokio::test]
async fn owner_recovers_stored_acceptance_without_dispatching_input() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let session = controller.observer.session_id().to_owned();
    let (_, submission) =
        registered_receipt(&controller, "recover", crate::input::InputPurpose::User).await?;
    controller.finish_exchange_owner().await?;
    submission.sending(None)?;
    submission.accepted(None)?;
    drop(submission);
    drop(controller);
    let controller =
        SessionController::new(HarnessBroker::initialize(serde_json::from_value(json!({
            "workspace":root.path(),"data_root":root.path().join("data"),
            "session_id":session,"client_id":"receipt-recovery",
            "provider":{"kind":"mock","command":[]},
        }))?)?)
        .await?;
    controller
        .exchange_owner
        .command_status(crate::exchange::command::CommandId::new(
            "receipt-recovery-barrier",
        )?)
        .await?;
    let input = controller.coordinator.submissions.snapshot()?;
    assert!(input.suspended);
    assert!(input.messages.is_empty());
    assert!(
        SqliteStore::read_only(&controller.data_root)?
            .latest_exchange(&session)?
            .is_none()
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Preserve cancellation's suspended queue when a readiness listener offers input afterward.
#[tokio::test]
async fn input_offer_after_fifo_cancel_preserves_suspended_queue() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let input = queued_offer_input(&controller, "interrupted", "Queued input", None)?;
    controller
        .exchange_owner
        .execute_command(crate::exchange::command::Command::CancelExchange(
            crate::exchange::command::CancelExchangeRequest::default(),
        ))
        .await?;
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let (messages, _output) = forge_protocol::outbound::channel();
    assert!(
        !controller
            .exchange_owner
            .offer_input(
                crate::exchange::InputDeliveryOffer {
                    input,
                    execution,
                    control
                },
                messages
            )
            .await?
    );
    let input = controller.coordinator.submissions.snapshot()?;
    assert!(input.suspended);
    assert_eq!(
        input.messages[0].delivery(),
        crate::input::InputDelivery::Queued
    );
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Reject stale readiness without claiming input or creating native execution.
#[tokio::test]
async fn input_offer_rejects_stale_revision_without_claiming() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let mut input = queued_offer_input(&controller, "stale", "Queued input", None)?;
    input.revision += 1;
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let (messages, _output) = forge_protocol::outbound::channel();
    assert!(
        !controller
            .exchange_owner
            .offer_input(
                crate::exchange::InputDeliveryOffer {
                    input,
                    execution,
                    control
                },
                messages
            )
            .await?
    );
    assert_eq!(
        controller.coordinator.submissions.snapshot()?.messages[0].delivery(),
        crate::input::InputDelivery::Queued
    );
    assert!(
        SqliteStore::read_only(&controller.data_root)?
            .latest_exchange(controller.observer.session_id())?
            .is_none()
    );
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Commit queued model-only input through internal preparation without creating a prompt exchange.
#[tokio::test]
async fn input_owner_commits_queued_settings_before_local_completion() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let input = queued_offer_input(
        &controller,
        "model-only",
        "",
        Some(json!({"model":"mock-model","effort":"high"})),
    )?;
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let (messages, _output) = forge_protocol::outbound::channel();
    assert!(
        controller
            .exchange_owner
            .offer_input(
                crate::exchange::InputDeliveryOffer {
                    input,
                    execution,
                    control
                },
                messages
            )
            .await?
    );
    tokio::time::timeout(Duration::from_secs(10), async {
        loop {
            let notified = controller.coordinator.submissions.changed.notified();
            if controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty()
            {
                return Ok::<(), anyhow::Error>(());
            }
            notified.await;
        }
    })
    .await??;
    let session = controller.observer.snapshot()?.session;
    assert_eq!(session.model, "mock-model");
    assert_eq!(session.effort, "high");
    assert!(
        SqliteStore::read_only(&controller.data_root)?
            .latest_exchange(controller.observer.session_id())?
            .is_none()
    );
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Persist the selected input model in the actual native receipt after its settings have committed.
#[tokio::test]
async fn input_owner_captures_receipt_after_queued_model_settings() -> Result<()> {
    use rusqlite::OptionalExtension;
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let input = queued_offer_input(
        &controller,
        "queued-choice",
        "Describe this workspace",
        Some(json!({"model":"mock-model","effort":"high"})),
    )?;
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let (messages, _output) = forge_protocol::outbound::channel();
    assert!(
        controller
            .exchange_owner
            .offer_input(
                crate::exchange::InputDeliveryOffer {
                    input,
                    execution,
                    control
                },
                messages
            )
            .await?
    );
    let connection = rusqlite::Connection::open_with_flags(
        controller.data_root.join("provider-submissions.sqlite3"),
        rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY,
    )?;
    let receipt = tokio::time::timeout(Duration::from_secs(10), async {
        loop {
            let notified = controller.coordinator.submissions.changed.notified();
            let id = connection
                .query_row(
                    "SELECT id FROM submission WHERE json_extract(payload,'$.input_message_id')=?1",
                    ["queued-choice"],
                    |row| row.get::<_, String>(0),
                )
                .optional()?;
            if let Some(id) = id {
                return controller.submission_store.submission(&id)?.record();
            }
            if controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .iter()
                .any(|input| input.delivery() == crate::input::InputDelivery::Failed)
            {
                anyhow::bail!("queued model preparation failed before receipt capture");
            }
            notified.await;
        }
    })
    .await??;
    assert_eq!(receipt.request.model, "mock-model");
    assert_eq!(receipt.request.effort, "high");
    tokio::time::timeout(Duration::from_secs(10), async {
        loop {
            let notified = controller.coordinator.submissions.changed.notified();
            if controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty()
            {
                return Ok::<(), anyhow::Error>(());
            }
            notified.await;
        }
    })
    .await??;
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Reject contextless queued compaction with a normalized error and keep later commands usable.
#[tokio::test]
async fn queued_compaction_preflight_does_not_fence_later_commands() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let input = queued_offer_input(&controller, "compact", "/compact", None)?;
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let (messages, mut output) = forge_protocol::outbound::channel();
    assert!(
        controller
            .exchange_owner
            .offer_input(
                crate::exchange::InputDeliveryOffer {
                    input,
                    execution,
                    control
                },
                messages
            )
            .await?
    );
    let code = tokio::time::timeout(Duration::from_secs(3), async {
        loop {
            let frame = output
                .recv()
                .await?
                .context("input compaction publication ended")?;
            if let forge_protocol::message::Message::Event(event) =
                serde_json::from_slice(frame.bytes())?
                && event.event == "provider_event"
                && event.payload["kind"] == "error"
            {
                return Ok::<_, anyhow::Error>(
                    event.payload["code"]
                        .as_str()
                        .context("missing normalized input failure code")?
                        .to_owned(),
                );
            }
        }
    })
    .await??;
    assert!(matches!(
        code.as_str(),
        "compaction_no_conversation" | "compaction_unsupported"
    ));
    controller
        .exchange_owner
        .execute_command(crate::exchange::command::Command::ConfigureSession(
            crate::exchange::command::ConfigurationChange::Permission(
                crate::exchange::command::PermissionChange::SetCheckEditing { enabled: true },
            ),
        ))
        .await?;
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    assert!(
        SqliteStore::read_only(&controller.data_root)?
            .latest_exchange(controller.observer.session_id())?
            .is_none()
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Accept a draft into the actual queue without starting its legacy readiness listener.
fn queued_offer_input(
    controller: &SessionController,
    id: &str,
    text: &str,
    config: Option<Value>,
) -> Result<crate::exchange::command::InputRevision> {
    controller.coordinator.submissions.apply(serde_json::from_value(json!({
        "id":format!("submit-{id}"),"action":{"kind":"submit","message":{"id":id,"text":text,"intent":"auto","config":config}},
    }))?)?;
    let message = controller
        .coordinator
        .submissions
        .snapshot()?
        .messages
        .into_iter()
        .find(|message| message.id == id)
        .unwrap();
    Ok(crate::exchange::command::InputRevision {
        id: crate::exchange::command::InputId::new(id)?,
        revision: message.revision,
    })
}

/// Retire cancelled input without erasing accepted or uncertain native delivery facts.
#[tokio::test]
async fn cancelled_input_settlement_preserves_acceptance_and_uncertainty() -> Result<()> {
    for (sent, accepted) in [(false, false), (true, false), (true, true)] {
        let root = tempfile::tempdir()?;
        let controller = receipt_test_controller(root.path()).await?;
        let (_, submission) =
            registered_receipt(&controller, "cancelled", crate::input::InputPurpose::User).await?;
        if sent {
            submission.sending(None)?;
        }
        if accepted {
            submission.accepted(None)?;
        }
        let update = controller
            .exchange_owner
            .observe_input_receipt(
                crate::exchange::InputReceiptFact::Settled {
                    submission: submission.receipt_id().clone(),
                    result: crate::exchange::InputExecutionResult::Cancelled,
                    activity: crate::exchange::InputDispatchActivity::Idle,
                },
                None,
            )
            .await?
            .context("cancelled receipt returned no update")?;
        let snapshot = controller.coordinator.submissions.snapshot()?;
        if sent && !accepted {
            assert_eq!(
                snapshot.messages[0].delivery(),
                crate::input::InputDelivery::Unknown
            );
            assert!(update.retracted.is_none());
        } else {
            assert!(
                snapshot.messages.is_empty(),
                "settled cancellation retained blocking input"
            );
            assert!(update.error.is_none());
            assert_eq!(update.retracted.as_deref(), (!sent).then_some("cancelled"));
        }
        controller.finish_exchange_owner().await?;
    }
    Ok(())
}

/// Create the real session owner without starting input dispatch or a live native process.
async fn receipt_test_controller(root: &std::path::Path) -> Result<Arc<SessionController>> {
    SessionController::new(HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":root,"data_root":root.join("data"),"client_id":"owner-receipts",
        "provider":{"kind":"mock","command":[]},
    }))?)?)
    .await
}

/// Register the real input journal and receipt binding used by owner observation tests.
async fn registered_receipt(
    controller: &SessionController,
    id: &str,
    purpose: crate::input::InputPurpose,
) -> Result<(
    crate::exchange::command::InputRevision,
    Arc<crate::provider::submission::Submission>,
)> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation, InputRoute};
    controller.coordinator.submissions.apply(InputOperation {
        id: format!("submit-{id}"),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: id.into(),
                text: "Owned receipt".into(),
                purpose: purpose.clone(),
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Auto,
            })),
        },
    })?;
    let message = controller
        .coordinator
        .submissions
        .claim(id, 1, InputRoute::Turn)?
        .unwrap();
    let catalog = controller.catalog_request.read().await.clone();
    let provider = controller.provider_handle().descriptor().kind;
    let input = crate::provider::ProviderInput::from_text(message.text);
    let submission = match purpose {
        crate::input::InputPurpose::TaskEntry { id, .. }
        | crate::input::InputPurpose::TaskContinuation { id, .. } => controller
            .submission_store
            .prepare_task_input(&catalog, input, provider, &id)?,
        crate::input::InputPurpose::Clarification { .. }
        | crate::input::InputPurpose::User
        | crate::input::InputPurpose::Question { .. }
        | crate::input::InputPurpose::QuestionContinuation { .. }
        | crate::input::InputPurpose::WorkflowContinuation { .. } => controller
            .submission_store
            .prepare_input(&catalog, input, provider, Some(id.into()))?,
    };
    let selected = crate::exchange::command::InputRevision {
        id: crate::exchange::command::InputId::new(id)?,
        revision: message.revision,
    };
    controller
        .exchange_owner
        .observe_input_receipt(
            crate::exchange::InputReceiptFact::Prepared {
                input: selected.clone(),
                submission: submission.clone(),
            },
            None,
        )
        .await?;
    Ok((selected, submission))
}

/// Restore server-acknowledged drafts through the real controller constructor without dispatch.
#[tokio::test]
async fn controller_reopens_acknowledged_queue_suspended() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let initialize: InitializeRequest = serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"input-durability", "provider":{"kind":"mock","command":[]},
    }))?;
    let controller = SessionController::new(HarnessBroker::initialize(initialize.clone())?).await?;
    let session_id = controller.observer.session_id().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    for (request_id, message_id) in [(1, "first"), (2, "second")] {
        controller.input_request(Request {
            id: request_id, method: "session.input.apply".into(),
            params: json!({"id":format!("submit-{message_id}"), "action":{"kind":"submit","message":{
                "id":message_id,"text":format!("draft {message_id}"),"intent":"queue",
                "attachments":[{"id":"attachment","name":"資料.txt"}],"config":{"model":"mock"}
            }}}),
        }, RuntimeMethod::SessionInputApply, &sink).await?;
        loop {
            let frame = output
                .recv()
                .await?
                .context("input acknowledgment channel closed")?;
            let response: Value = serde_json::from_slice(frame.bytes())?;
            if response["id"] == request_id {
                assert!(response["error"].is_null());
                break;
            }
        }
    }
    let before = controller.coordinator.submissions.snapshot()?;
    drop(controller);
    let mut reopened = initialize;
    reopened.session_id = Some(session_id);
    let controller = SessionController::new(HarnessBroker::initialize(reopened)?).await?;
    let restored = controller.coordinator.submissions.snapshot()?;
    assert!(restored.suspended);
    assert_eq!(
        serde_json::to_value(restored.messages)?,
        serde_json::to_value(before.messages)?
    );
    assert!(
        controller
            .coordinator
            .submissions
            .operation("submit-first")?
            .is_some()
    );
    assert!(
        controller
            .coordinator
            .submissions
            .claim("first", 1, crate::input::InputRoute::Turn)?
            .is_none()
    );
    Ok(())
}

/// Retry pending deletion cleanup, including an unloaded queue, before removing its session.
#[tokio::test]
async fn interrupted_session_deletion_retries_queue_cleanup() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let initialize: InitializeRequest = serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"input-deletion", "provider":{"kind":"mock","command":[]},
    }))?;
    let controller = SessionController::new(HarnessBroker::initialize(initialize)?).await?;
    let session_id = controller.observer.session_id().to_owned();
    let root = controller.data_root.clone();
    let submissions = controller.submission_store.clone();
    controller.coordinator.submissions.apply(serde_json::from_value(json!({
        "id":"queued-before-deletion", "action":{"kind":"submit","message":{"id":"delete-message","text":"retained","intent":"queue"}}
    }))?)?;
    drop(controller);
    let mut store = SqliteStore::open(&root)?;
    assert!(
        store
            .delete_session_with(&session_id, || anyhow::bail!("interrupted cleanup"))
            .is_err()
    );
    store.recover_session_deletions()?;
    assert!(store.load_session(&session_id)?.is_none());
    let queue = crate::input::SessionInputQueue::open(&root, &session_id, &submissions)?;
    assert!(queue.snapshot()?.messages.is_empty());
    assert!(queue.operation("queued-before-deletion")?.is_none());
    Ok(())
}

/// Reuse preparation after a crash before the queue can link its submission ID.
#[tokio::test]
async fn queue_claim_preparation_gap_reuses_only_one_native_submission() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let initialize: InitializeRequest = serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"input-gap", "provider":{"kind":"mock","command":[]},
    }))?;
    let controller = SessionController::new(HarnessBroker::initialize(initialize.clone())?).await?;
    let session_id = controller.observer.session_id().to_owned();
    controller.coordinator.submissions.apply(serde_json::from_value(json!({
        "id":"submit-gap", "action":{"kind":"submit","message":{"id":"gap","text":"once","intent":"queue"}}
    }))?)?;
    controller
        .coordinator
        .submissions
        .claim("gap", 1, crate::input::InputRoute::Turn)?
        .context("claim failed")?;
    let catalog = controller.catalog_request.read().await.clone();
    let journal = controller.submission_store.clone();
    let prepared = journal.prepare_input(
        &catalog,
        crate::provider::ProviderInput::from_text("once"),
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        Some("gap".into()),
    )?;
    drop(controller);
    let mut resumed = initialize;
    resumed.session_id = Some(session_id);
    let controller = SessionController::new(HarnessBroker::initialize(resumed)?).await?;
    assert!(controller.coordinator.submissions.snapshot()?.suspended);
    let again = journal.prepare_input(
        &catalog,
        crate::provider::ProviderInput::from_text("once"),
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        Some("gap".into()),
    )?;
    assert_eq!(prepared.id(), again.id());
    let edited = journal.prepare_input(
        &catalog,
        crate::provider::ProviderInput::from_text("edited"),
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        Some("gap".into()),
    )?;
    assert_ne!(prepared.id(), edited.id());
    assert!(matches!(
        prepared.record()?.delivery,
        crate::provider::submission::SubmissionState::Cancelled
    ));
    assert_eq!(
        journal
            .pending(
                &edited.record()?.request,
                forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                None
            )?
            .len(),
        1
    );
    Ok(())
}

/// Targeted clarification retains one input identity, question revision, and replay receipt.
#[tokio::test]
async fn targeted_clarification_uses_scoped_input_and_rejects_changed_question() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let ids = controller.questions.ask(
        "clarification-source",
        "source-exchange",
        crate::question::QuestionOrigin::Discussion(None),
        vec![crate::question::QuestionDraft {
            header: "Scope".into(),
            question: "Which scope?".into(),
            requirement: crate::question::QuestionRequirement::Optional,
            options: Vec::new(),
        }],
    )?;
    let operation: crate::input::InputOperation = serde_json::from_value(json!({
        "id":"clarify-once", "action":{"kind":"submit","message":{"id":"clarify-input",
        "text":"Explain this scope", "intent":"auto", "config":null, "attachments":[]}}
    }))?;
    let command = crate::exchange::command::CommandRequest::from_targeted_input(
        operation,
        crate::exchange::command::InputSubmissionTarget::Question {
            question_id: ids[0].clone(),
            revision: std::num::NonZeroU64::new(1).unwrap(),
        },
    )?;
    let (sink, _output) = forge_protocol::outbound::channel();
    controller.execute_command(command.clone(), &sink).await?;
    controller.execute_command(command, &sink).await?;
    let snapshot = controller.coordinator.submissions.snapshot()?;
    assert_eq!(snapshot.messages.len(), 1);
    let message = &snapshot.messages[0];
    assert_eq!(message.id, "clarify-input");
    assert!(
        matches!(&message.purpose, crate::input::InputPurpose::Question {
        question_id, content_revision: Some(1), message_kind: crate::question::QuestionMessageKind::Clarification, ..
    } if question_id == &ids[0])
    );
    assert_eq!(
        controller.questions.snapshot()?.questions[0]
            .discussion
            .len(),
        1
    );
    assert!(controller.questions.outbox()?.is_empty());
    controller
        .questions
        .apply(crate::question::QuestionCommand {
            operation_id: "revise-question".into(),
            question_id: ids[0].clone(),
            revision: 1,
            action: crate::question::QuestionAction::Revise {
                draft: crate::question::QuestionDraft {
                    header: "Changed".into(),
                    question: "Which changed scope?".into(),
                    requirement: crate::question::QuestionRequirement::Optional,
                    options: Vec::new(),
                },
            },
        })?;
    assert!(!crate::input::input_purpose_is_current(
        &controller.task_controller,
        &controller.questions,
        &message.purpose,
        &controller.data_root,
        controller.observer.session_id()
    )?);
    controller.finish_exchange_owner().await?;
    Ok(())
}
