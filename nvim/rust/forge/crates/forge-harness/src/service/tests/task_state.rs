use super::super::{HarnessBroker, SessionController};
use super::bootstrap::{initialize, service};
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::json;
use std::sync::Arc;
use std::time::Duration;

/// Keep goal completion and final prose distinct from native settlement, then admit another input.
#[tokio::test]
async fn goal_completion_settles_final_response_and_accepts_next_input() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let provider = controller.provider_handle();
    provider
        .mock_response_control()
        .context("mock controls missing")?
        .dispatch(
            &session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![
                        TestPromptStep::ToolCall {
                            name: "goal_complete".into(),
                            arguments: json!({"summary":"Goal finished"}),
                        },
                        TestPromptStep::Message {
                            content: TestMessageContent::Response,
                            text: "Implemented the goal".into(),
                        },
                    ],
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    service
        .dispatch(
            Some(session_id.clone()),
            goal_command_request("rpc", "goal", 2),
            &sink,
        )
        .await?;
    let store = SqliteStore::open(&controller.data_root)?;
    for ordinal in 1..=2 {
        tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                if let Some(exchange) = store.latest_exchange(&session_id)?
                    && exchange.ordinal == ordinal
                    && exchange.completed_at_ms().is_some()
                    && controller.coordinator.execution.available_permits() == 1
                {
                    assert_eq!(exchange.state(), crate::exchange::ExchangeState::Complete);
                    break Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("completed goal stranded the session")??;
        assert!(controller.task_controller.current()?.is_none());
        if ordinal == 1 {
            service
                .dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: 3,
                        method: "session.prompt.submit".into(),
                        params: json!({"operation_id":"after-goal","text":"Continue after completion"}),
                    },
                    &sink,
                )
                .await?;
        }
    }
    controller.finish_exchange_owner().await?;
    reader.abort();
    let result = reader.await;
    assert!(result.is_ok() || result.is_err_and(|error| error.is_cancelled()));
    Ok(())
}

/// Exercise the real mock-provider file boundary, immutable submission, and dirty-draft guard.
#[tokio::test]
async fn directory_planning_submits_real_files_and_preserves_review_snapshot() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"allow-draft-files","text":"/yolo"}),
            },
            &sink,
        )
        .await?;
    service.dispatch(Some(session_id.clone()), Request {
        id: 3, method: "session.prompt.submit".into(),
        params: json!({"operation_id":"directory-plan","text":"/plan Add an isolated callable"}),
    }, &sink).await?;
    let plan = tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            let current = controller.task_controller.current()?;
            if let Some(task) = current
                && let crate::task::WorkflowSnapshot::Plan { plan } = &task.workflow
                && plan.artifact.state == crate::plan::PlanState::AwaitingReview
                && controller.coordinator.execution.available_permits() == 1
            {
                break Ok::<_, anyhow::Error>(plan.artifact.clone());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("filesystem planning never reached review")??;
    let files = crate::plan::PlanFileStore::new(&controller.data_root, root.path())?;
    let directory = files.authoring_directory(&session_id, &plan.id);
    assert_eq!(std::fs::read_dir(&directory)?.count(), 2);
    let proposal = directory.join("files/src/change.rs");
    assert!(std::fs::read_to_string(&proposal)?.contains("pub fn requested_change();"));
    let accepted_candidate =
        files.read_submitted_document(&session_id, &plan.id, plan.model_revision)?;
    files.authoring_require_current(&session_id, &accepted_candidate)?;
    std::fs::write(&proposal, "/// Revise the proposal.\npub fn changed();\n")?;
    assert!(
        files
            .authoring_require_current(&session_id, &accepted_candidate)
            .is_err()
    );
    assert_eq!(
        files.read_submitted_document(&session_id, &plan.id, plan.model_revision)?,
        accepted_candidate
    );
    controller.finish_exchange_owner().await?;
    reader.abort();
    let result = reader.await;
    assert!(result.is_ok() || result.is_err_and(|error| error.is_cancelled()));
    Ok(())
}

fn goal_command_request(transport: &str, command: &str, id: u64) -> Request {
    let operation = format!("{transport}-{command}-{id}");
    let text = if command == "goal" {
        "/goal Preserve the held provider".to_owned()
    } else {
        format!("/goal {command}")
    };
    let (method, params) = match transport {
        "transition" => (
            "session.command.apply".to_owned(),
            if command == "goal" {
                json!({"operation_id":operation,"action":"goal","text":"Preserve the held provider"})
            } else {
                json!({"operation_id":operation,"action":command})
            },
        ),
        "rpc" => (
            if command == "goal" {
                "session.goal.set".into()
            } else {
                format!("session.goal.{command}")
            },
            json!({"operation_id":operation,"objective":"Preserve the held provider"}),
        ),
        "prompt" => (
            "session.prompt.submit".into(),
            json!({"operation_id":operation,"text":text}),
        ),
        "steer" => (
            "session.turn.steer".into(),
            json!({"operation_id":operation,"text":text}),
        ),
        "input" => (
            "session.input.apply".into(),
            json!({"id":operation,"operation_id":operation,
            "action":{"kind":"submit","message":{"id":operation,"text":text,"intent":"auto","attachments":[]}}}),
        ),
        _ => unreachable!(),
    };
    Request { id, method, params }
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_rpc_pause() -> Result<()>
{
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("rpc", "pause").await
}

/// Run one transport and action variant of the scenario described above, each in its own test so a
/// variant reports independently and stays within the per-test time limit.
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_case(
    transport: &str,
    action: &str,
) -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    controller.task_controller.change(
        &mut SqliteStore::open(&controller.data_root)?,
        None,
        crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
            "Collect selected task work".into(),
            Default::default(),
            1,
        ))),
        1,
    )?;
    let provider = controller.provider_handle();
    let script = provider.mock_response_control().unwrap();
    let held = script
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let hold_id = held["id"].as_str().unwrap().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.input.apply".into(),
                params: json!({"id":"held-task-input","action":{"kind":"submit","message":{
            "id":"held-task-input","text":"Inspect startup","intent":"auto","attachments":[]}}}),
            },
            &sink,
        )
        .await?;
    script
        .dispatch(&session_id, MockResponseCommand::Wait { id: hold_id })
        .await?;
    let store = SqliteStore::open(&controller.data_root)?;
    let original = store.latest_exchange(&session_id)?.unwrap().id;
    let request = goal_command_request(transport, action, 10);
    service
        .dispatch(Some(session_id.clone()), request.clone(), &sink)
        .await?;
    async {
        loop {
            let current = controller.task_controller.current()?;
            let settled = if action == "pause" {
                current.is_some_and(|task| task.status() == crate::task::TaskStatus::Paused)
            } else {
                current.is_none()
            };
            if settled
                && controller.coordinator.execution.available_permits() == 1
                && controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .is_empty()
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await?;
    let execution = controller.broker.lock().await.execution_registry();
    let active = execution
        .existing_owner(&crate::provider::ThreadOwner::root(&session_id))?
        .map(|engine| engine.active_invocation_snapshot())
        .transpose()?
        .flatten();
    assert!(active.is_none());
    assert!(Arc::ptr_eq(&provider, &controller.provider_handle()));
    let before = serde_json::to_value(controller.task_controller.current()?)?;
    let receipt_id =
        crate::exchange::command::CommandId::new(request.params["operation_id"].as_str().unwrap())?;
    assert!(
        controller
            .exchange_owner
            .command_status(receipt_id.clone())
            .await?
            .unwrap()
            .completion()
            .unwrap()
            .is_ok()
    );
    service
        .dispatch(Some(session_id.clone()), request, &sink)
        .await?;
    assert_eq!(
        serde_json::to_value(controller.task_controller.current()?)?,
        before,
        "{transport} {action} replay changed task authority"
    );
    let history = store.list_exchange(&session_id)?;
    assert_eq!(
        history.len(),
        1,
        "task control must not submit a replacement prompt"
    );
    assert_eq!(history[0].id, original);
    assert!(!*controller.cancellation.subscribe().borrow());
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    reader.await??;

    Ok(())
}

#[tokio::test]
async fn task_creation_completes_at_dispatch_and_replay_preserves_replacement_goal() -> Result<()> {
    task_creation_completes_at_dispatch_and_replay_preserves_replacement_case(
        crate::task::TaskKind::Goal,
    )
    .await
}

async fn task_creation_completes_at_dispatch_and_replay_preserves_replacement_case(
    kind: crate::task::TaskKind,
) -> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandOutcome, CommandRequest, InputControl, NewGoal, NewPlan,
        TaskCommand, TaskCommandOutcome,
    };
    use crate::provider::mock_response::MockResponseCommand;
    {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let service = service();
        service.open_session(1, initialize(&root, "mock")).await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let provider = controller.provider_handle();
        let script = provider.mock_response_control().unwrap();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let mut previous = None;
        let mut previous_plan: Option<String> = None;
        let mut first = None;
        for index in 1..=2 {
            let held = script
                .dispatch(
                    &session_id,
                    MockResponseCommand::Hold {
                        mode: None,
                        any_session: false,
                    },
                )
                .await?;
            let hold = held["id"].as_str().unwrap().to_owned();
            let request = CommandRequest {
                id: CommandId::new(format!("owned-task-{index}"))?,
                command: Command::Task(match kind {
                    crate::task::TaskKind::Goal => {
                        TaskCommand::StartGoal(NewGoal::new(format!("Goal {index}"))?)
                    }
                    crate::task::TaskKind::Plan => {
                        TaskCommand::StartPlan(NewPlan::new(format!("Plan {index}"))?)
                    }
                    crate::task::TaskKind::Execute => {
                        unreachable!("execution requires reviewed plan evidence")
                    }
                }),
                input_control: InputControl::AtExecution,
            };
            let completed = controller
                .exchange_owner
                .execute_identified(request.clone(), sink.clone())
                .await
                .with_context(|| {
                    format!(
                        "task creation kind={kind:?} index={index}; input={:?}; task={:?}",
                        controller.coordinator.submissions.snapshot(),
                        controller.task_controller.current()
                    )
                })?;
            let CommandOutcome::Task(TaskCommandOutcome::Started { task, input, .. }) =
                &completed.outcome
            else {
                anyhow::bail!("task startup did not confirm its initial input handoff")
            };
            script
                .dispatch(&session_id, MockResponseCommand::Wait { id: hold })
                .await?;
            let current = controller.task_controller.current()?.unwrap();
            assert_eq!(current.kind(), kind);
            if let Some(plan) = previous_plan.as_ref() {
                assert_eq!(
                    SqliteStore::read_only(&controller.data_root)?
                        .load_plan(plan)?
                        .unwrap()
                        .state,
                    crate::plan::PlanState::Cancelled
                );
            }
            if let crate::task::WorkflowSnapshot::Plan { plan } = &current.workflow {
                previous_plan = Some(plan.artifact.id.clone());
            }
            assert_eq!(&current.id, task.id());
            assert_eq!(current.entry.as_ref().unwrap().id, *input);
            if let Some(previous) = previous {
                assert_ne!(current.id, previous);
            }
            previous = Some(current.id.clone());
            assert!(
                !SqliteStore::read_only(&controller.data_root)?
                    .latest_exchange(&session_id)?
                    .unwrap()
                    .is_finished()
            );
            assert!(Arc::ptr_eq(&provider, &controller.provider_handle()));
            if first.is_none() {
                first = Some((request, completed));
            }
        }
        let before = controller.task_controller.current()?.unwrap();
        let (request, original) = first.unwrap();
        let replay = controller
            .exchange_owner
            .execute_identified(request, sink.clone())
            .await?;
        assert_eq!(original.sequence, replay.sequence);
        assert_eq!(
            controller.task_controller.current()?.unwrap().revision,
            before.revision
        );
        assert_eq!(
            SqliteStore::read_only(&controller.data_root)?
                .list_exchange(&session_id)?
                .len(),
            2
        );
        controller
            .exchange_owner
            .execute_command(Command::CancelExchange(Default::default()))
            .await?;
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        reader.await??;
    }
    Ok(())
}

/// Reject `/goal continue` for a task that cannot continue without fencing the session.
///
/// Preparation refuses a paused goal. That refusal is a rejection of
/// the one command; reporting it as uncommitted storage closed every later command, Ctrl-C
/// included, for the session's lifetime.
#[tokio::test]
async fn goal_continue_on_paused_task_is_rejected_without_fencing_the_session() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, Command, CommandFailure, ExchangeContinuationSelection,
        ExecuteCommandError, TaskCommandFailure, WorkflowContinuationRequest,
        WorkflowContinuationTarget,
    };
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?
        .dispatch(
            &session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![TestPromptStep::Message {
                        content: TestMessageContent::Response,
                        text: "Answered".into(),
                    }],
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"before-continue","text":"Answer once"}),
            },
            &sink,
        )
        .await?;
    let store = SqliteStore::open(&controller.data_root)?;
    let exchange = tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            if let Some(exchange) = store.latest_exchange(&session_id)?
                && exchange.is_finished()
                && controller.coordinator.execution.available_permits() == 1
            {
                break Ok::<_, anyhow::Error>(exchange);
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("the first prompt never settled")??;
    let installed = controller
        .task_controller
        .change(
            &mut SqliteStore::open(&controller.data_root)?,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "A paused goal".into(),
                Default::default(),
                1,
            ))),
            1,
        )?
        .context("installed task is missing")?;
    let task = controller
        .task_controller
        .change(
            &mut SqliteStore::open(&controller.data_root)?,
            Some(&installed.revision),
            crate::task::TaskChange::Pause,
            2,
        )?
        .context("paused task is missing")?;
    assert_eq!(task.status(), crate::task::TaskStatus::Paused);
    let continued = controller
        .exchange_owner
        .execute_command(Command::ContinueWorkflow(WorkflowContinuationRequest {
            target: WorkflowContinuationTarget::Exchange(ExchangeContinuationSelection::capture(
                &exchange,
                Some(&task),
            )?),
            message: None,
        }))
        .await;
    assert!(
        matches!(
            continued,
            Err(ExecuteCommandError::Failed(CommandFailure::Task(
                TaskCommandFailure::Changed
            )))
        ),
        "{continued:?}"
    );
    let cancelled = controller
        .exchange_owner
        .execute_command(Command::CancelExchange(CancelExchangeRequest::default()))
        .await;
    assert!(cancelled.is_ok(), "{cancelled:?}");
    controller.finish_exchange_owner().await?;
    reader.abort();
    let result = reader.await;
    assert!(result.is_ok() || result.is_err_and(|error| error.is_cancelled()));
    Ok(())
}

/// A durable pause releases the command FIFO and replay never schedules goal work.
#[tokio::test]
async fn task_pause_completes_at_local_commit_and_replay_preserves_fifo() -> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandOutcome, CommandRequest, InputCommand, InputControl, InputId,
        InputSubmission, TaskCommand, TaskCommandOutcome, TaskSelection,
    };
    let root = tempfile::tempdir()?;
    let broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    let controller = SessionController::new(broker).await?;
    let task = controller
        .task_controller
        .change(
            &mut SqliteStore::open(&controller.data_root)?,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Retain the selected task".into(),
                Default::default(),
                1,
            ))),
            1,
        )?
        .context("installed task is missing")?;
    let request = CommandRequest {
        id: CommandId::new("owned-task-pause")?,
        command: Command::Task(TaskCommand::Pause(TaskSelection::capture(&task))),
        input_control: InputControl::AtExecution,
    };
    let (sink, _output) = forge_protocol::outbound::channel();
    let mut original = Box::pin(
        controller
            .exchange_owner
            .execute_identified(request.clone(), sink.clone()),
    );
    assert!(futures_util::poll!(original.as_mut()).is_pending());
    drop(original);
    let replay = controller
        .exchange_owner
        .execute_identified(request.clone(), sink.clone());
    let following =
        controller
            .exchange_owner
            .execute_command(Command::Input(InputCommand::Submit(InputSubmission {
                target: crate::exchange::command::InputSubmissionTarget::Conversation,
                id: InputId::new("after-task-pause")?,
                text: "Accept after the local pause commits".into(),
                attachments: Vec::new(),
                config: None,
                intent: crate::input::InputIntent::Queue,
            })));
    let (paused, accepted) = async { tokio::join!(replay, following) }.await;
    assert!(matches!(
        paused?.outcome,
        CommandOutcome::Task(TaskCommandOutcome::Paused { .. })
    ));
    assert!(matches!(accepted?.outcome, CommandOutcome::Input { .. }));
    assert_eq!(
        controller.task_controller.current()?.unwrap().status(),
        crate::task::TaskStatus::Paused
    );
    assert!(controller.coordinator.submissions.snapshot()?.suspended);
    let receipt = controller
        .exchange_owner
        .command_status(request.id)
        .await?
        .context("pause receipt missing")?;
    assert!(matches!(
        receipt.state,
        crate::exchange::journal::CommandState::Finished { .. }
    ));
    controller.finish_exchange_owner().await?;
    Ok(())
}
