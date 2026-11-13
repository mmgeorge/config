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
    goal_request_waits_for_native_completion(false, false).await
}

/// Keep a blocking request pending until the provider's final response settles.
#[tokio::test]
async fn goal_blocking_waits_for_native_completion() -> Result<()> {
    goal_request_waits_for_native_completion(true, false).await
}

/// Native interruption must retain its cause and reject the earlier completion request.
#[tokio::test]
async fn goal_completion_does_not_override_native_interruption() -> Result<()> {
    goal_request_waits_for_native_completion(false, true).await
}

/// Exercise terminal requests through the same native response barrier.
async fn goal_request_waits_for_native_completion(blocked: bool, interrupted: bool) -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestHoldId, TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
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
                            name: if blocked {
                                "goal_blocked"
                            } else {
                                "goal_complete"
                            }
                            .into(),
                            arguments: if blocked {
                                json!({"reason":"Needs external input"})
                            } else {
                                json!({"summary":"Goal finished"})
                            },
                        },
                        TestPromptStep::CollectionBarrier {
                            id: TestHoldId::new("goal-collected")?,
                        },
                        TestPromptStep::Barrier {
                            id: TestHoldId::new("goal-requested")?,
                        },
                        TestPromptStep::Message {
                            content: TestMessageContent::Response,
                            text: "Implemented the goal".into(),
                        },
                    ],
                    terminal: if interrupted {
                        TestPromptTerminal::Cancelled
                    } else {
                        TestPromptTerminal::Completed
                    },
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
    let control = provider
        .mock_response_control()
        .context("mock controls missing")?;
    tokio::time::timeout(
        Duration::from_secs(2),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: "goal-requested".into(),
            },
        ),
    )
    .await??;
    let task = controller
        .task_controller
        .current()?
        .context("goal disappeared before native completion")?;
    assert_eq!(task.status(), crate::task::TaskStatus::Running);
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    control
        .dispatch(
            &session_id,
            MockResponseCommand::Release {
                id: "goal-requested".into(),
            },
        )
        .await?;
    tokio::time::timeout(
        Duration::from_secs(2),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: "goal-collected".into(),
            },
        ),
    )
    .await??;
    assert_eq!(
        controller
            .task_controller
            .current()?
            .context("goal changed before collection")?
            .status(),
        crate::task::TaskStatus::Running
    );
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    control
        .dispatch(
            &session_id,
            MockResponseCommand::Release {
                id: "goal-collected".into(),
            },
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
                    assert_eq!(
                        exchange.state(),
                        if interrupted {
                            crate::exchange::ExchangeState::Interrupted
                        } else {
                            crate::exchange::ExchangeState::Complete
                        }
                    );
                    break Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("completed goal stranded the session")??;
        if interrupted {
            assert_eq!(
                controller
                    .task_controller
                    .current()?
                    .context("interrupted goal disappeared")?
                    .status(),
                crate::task::TaskStatus::Running
            );
            assert_eq!(
                store.list_exchange(&session_id)?.len(),
                1,
                "interrupted request must not reserve another turn"
            );
            break;
        }
        if blocked {
            assert_eq!(
                controller
                    .task_controller
                    .current()?
                    .context("blocked goal disappeared")?
                    .status(),
                crate::task::TaskStatus::Blocked
            );
            break;
        }
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

/// Approval retains the planning invocation until its own native terminal result.
#[tokio::test]
async fn plan_acceptance_waits_for_planning_and_keeps_cancel_available() -> Result<()> {
    plan_acceptance_case(false, false).await
}

/// A later explicit cancellation supersedes approval without admitting Execute.
#[tokio::test]
async fn cancellation_supersedes_pending_plan_acceptance() -> Result<()> {
    plan_acceptance_case(true, false).await
}

/// Automatic approval uses the same non-interrupting native and collection boundary.
#[tokio::test]
async fn automatic_plan_acceptance_waits_for_planning() -> Result<()> {
    plan_acceptance_case(false, true).await
}

async fn plan_acceptance_case(cancel: bool, automatic: bool) -> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandRequest, InputControl, PlanExecutionSelection, TaskCommand,
    };
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestHoldId, TestInputAcceptance, TestPromptScript, TestPromptStep, TestPromptTerminal,
    };
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let files = crate::plan::PlanFileStore::new(&controller.data_root, root.path())?;
    let mut document = crate::plan::test_fixture("acceptance-fixture", "Retain planning ownership");
    let mut design = crate::plan::DeclarationDesign::open(root.path())?;
    design.document.objective = "Retain planning ownership".into();
    design.document.background = "A native turn is still responding.".into();
    design.document.design = "Wait for native completion before execution.".into();
    design.proposed_source.insert(
        "src/owner.rs".into(),
        "/// Own state.\npub struct Owner;\n".into(),
    );
    design.parsed = crate::plan::DeclarationDesign::scratch(design.proposed_source.clone())?.parsed;
    document.design = Some(design);
    let mut plan = files
        .prepare_entry(
            &session_id,
            "Retain planning ownership".into(),
            Some(document),
            1,
        )?
        .record;
    let (document, _, digest) =
        files.submit_document_revision(&session_id, &plan.id, 1, plan.document_version)?;
    plan.document_version = document.version;
    plan.submitted_version = Some(document.version);
    plan.model_revision = 1;
    plan.review_digest = Some(digest);
    plan.state = crate::plan::PlanState::AwaitingReview;
    let mut store = SqliteStore::open(&controller.data_root)?;
    store.save_plan(&plan)?;
    let mut planning = crate::task::TaskSnapshot::plan(plan.clone(), 1);
    if automatic {
        planning.plan_execution = Some(crate::task::PlanExecutionChoice {
            execution: crate::task::ExecutionEntry {
                approval_mode: crate::session::ApprovalMode::Allow,
                executor: crate::session::PlanExecutor::default(),
                compact: false,
            },
            comment: None,
        });
    }
    controller.task_controller.change(
        &mut store,
        None,
        crate::task::TaskChange::Install(Box::new(crate::task::Task::try_from(planning)?)),
        1,
    )?;
    let provider = controller.provider_handle();
    let script = provider
        .mock_response_control()
        .context("mock controls missing")?;
    script
        .dispatch(
            &session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![TestPromptStep::Barrier {
                        id: TestHoldId::new("planning-response")?,
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
    service.dispatch(Some(session_id.clone()), Request { id: 2, method: "session.prompt.submit".into(),
        params: json!({"operation_id":"planning-response-input","text":"Explain the proposed implementation."}) }, &sink).await?;
    tokio::time::timeout(
        Duration::from_secs(2),
        script.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: "planning-response".into(),
            },
        ),
    )
    .await??;
    let current = controller.task_controller.current()?;
    let selection = PlanExecutionSelection::capture(
        &plan,
        current.as_ref(),
        crate::task::command::ExecuteSelection {
            approval_mode: Some(crate::session::ApprovalMode::Allow),
            plan_compact: Some(false),
            ..Default::default()
        },
    )?;
    let command = CommandRequest {
        id: CommandId::new("accept-while-planning")?,
        command: Command::Task(TaskCommand::ExecutePlan(selection)),
        input_control: InputControl::AtExecution,
    };
    let mut duplicate_command = command.clone();
    let accepted_owner = controller.clone();
    let accepted_sink = sink.clone();
    let automatic_command = if automatic {
        controller
            .exchange_owner
            .execute_command(Command::Task(TaskCommand::SetAutoAccept(
                crate::exchange::command::PlanAcceptancePolicy {
                    target: crate::exchange::command::TaskSelection::capture(
                        current.as_ref().unwrap(),
                    ),
                    enabled: true,
                },
            )))
            .await?;
        Some(
            crate::task::command::SessionCommand::approved_revision(
                &controller.task_controller.current()?.unwrap(),
            )?
            .context("automatic approval was not eligible")?,
        )
    } else {
        None
    };
    if let Some(automatic_command) = &automatic_command {
        duplicate_command.command = Command::Task(
            TaskCommand::from_session_action(
                &automatic_command.action,
                current.as_ref(),
                Some(&plan),
                None,
            )?
            .context("automatic command must select execution")?,
        );
    }
    let terminal_id = format!(
        "plan_acceptance:{}",
        automatic_command
            .as_ref()
            .map_or("accept-while-planning", |command| command
                .operation_id
                .as_str())
    );
    let acceptance = tokio::spawn(async move {
        if let Some(automatic_command) = automatic_command {
            super::super::task_state::apply_command(
                &accepted_owner,
                accepted_owner.observer.session_id(),
                &automatic_command,
                None,
                8,
                &accepted_sink,
            )
            .await?;
        } else {
            accepted_owner
                .exchange_owner
                .execute_identified(command, accepted_sink)
                .await?;
        }
        Ok::<(), anyhow::Error>(())
    });
    tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            if matches!(
                store.terminal_disposition(&session_id, &terminal_id)?,
                Some(crate::storage::TerminalDisposition::Pending)
            ) {
                break Ok::<(), anyhow::Error>(());
            }
            if acceptance.is_finished() {
                let decision = controller
                    .exchange_owner
                    .command_status(CommandId::new(
                        terminal_id.strip_prefix("plan_acceptance:").unwrap(),
                    )?)
                    .await?;
                anyhow::bail!("acceptance finished before the planning turn: {decision:?}");
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("approval did not reach its pending boundary")??;
    assert_eq!(
        controller.task_controller.current()?.unwrap().kind(),
        crate::task::TaskKind::Plan
    );
    assert!(!acceptance.is_finished());
    if cancel {
        tokio::time::timeout(
            Duration::from_secs(2),
            controller
                .exchange_owner
                .execute_command(Command::CancelExchange(Default::default())),
        )
        .await??;
        assert!(acceptance.await?.is_err());
        assert_eq!(
            controller.task_controller.current()?.unwrap().kind(),
            crate::task::TaskKind::Plan
        );
    } else {
        script
            .dispatch(
                &session_id,
                MockResponseCommand::Script {
                    script: TestPromptScript {
                        acceptance: TestInputAcceptance::BeforeResponse,
                        steps: vec![
                            TestPromptStep::Barrier { id: TestHoldId::new("implement-ready")? },
                            TestPromptStep::ToolCall {
                                name: "plan_phase_done".into(),
                                arguments: json!({"phase":"implement","revision":1,"summary":"Added the owner"}),
                            },
                            TestPromptStep::CollectionBarrier { id: TestHoldId::new("implement-collected")? },
                            TestPromptStep::Barrier { id: TestHoldId::new("implement-requested")? },
                            TestPromptStep::Message {
                                content: forge_provider_api::testing::TestMessageContent::Response,
                                text: "Implementation finished; Harness will check it.".into(),
                            },
                        ],
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await?;
        let duplicate_owner = controller.clone();
        let duplicate_sink = sink.clone();
        let mut duplicate_command = duplicate_command;
        let mut repeated_command = duplicate_command.clone();
        repeated_command.id = CommandId::new("repeat-committed-revision")?;
        duplicate_command.id = CommandId::new("same-approved-revision")?;
        let duplicate = tokio::spawn(async move {
            duplicate_owner
                .exchange_owner
                .execute_identified(duplicate_command, duplicate_sink)
                .await
        });
        let journal = crate::exchange::journal::CommandJournal::open_metadata(
            &controller.data_root,
            &session_id,
        )?;
        tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                if journal
                    .lookup(&CommandId::new("same-approved-revision")?)?
                    .is_some_and(|record| {
                        matches!(
                            record.state,
                            crate::exchange::journal::CommandState::Active { .. }
                        )
                    })
                {
                    break Ok::<(), anyhow::Error>(());
                }
                anyhow::ensure!(
                    !duplicate.is_finished(),
                    "duplicate approval did not join the pending decision"
                );
                tokio::task::yield_now().await;
            }
        })
        .await??;
        script
            .dispatch(
                &session_id,
                MockResponseCommand::Release {
                    id: "planning-response".into(),
                },
            )
            .await?;
        tokio::time::timeout(Duration::from_secs(3), acceptance).await???;
        tokio::time::timeout(Duration::from_secs(3), duplicate).await???;
        let before = controller.task_controller.current()?.unwrap();
        let Command::Task(TaskCommand::ExecutePlan(original)) = &repeated_command.command else {
            unreachable!()
        };
        let accepted = store.load_plan(&plan.id)?.unwrap();
        let repeated_selection =
            PlanExecutionSelection::capture(&accepted, Some(&before), original.request().clone())?;
        let mut changed_settings = original.request().clone();
        changed_settings.approval_mode = Some(crate::session::ApprovalMode::Deny);
        let different =
            PlanExecutionSelection::capture(&accepted, Some(&before), changed_settings)?;
        assert!(
            store
                .committed_plan_acceptance(&session_id, &different)?
                .is_none()
        );
        repeated_command.command = Command::Task(TaskCommand::ExecutePlan(repeated_selection));
        controller
            .exchange_owner
            .execute_identified(repeated_command, sink.clone())
            .await?;
        assert_eq!(
            controller.task_controller.current()?.unwrap().revision,
            before.revision
        );

        let history = store.list_exchange(&session_id)?;
        assert_eq!(
            history
                .iter()
                .find(|exchange| exchange.ordinal == 1)
                .unwrap()
                .state(),
            crate::exchange::ExchangeState::Complete
        );
        assert!(matches!(
            store.terminal_disposition(&session_id, &terminal_id)?,
            Some(crate::storage::TerminalDisposition::Committed { .. })
        ));
        tokio::time::timeout(
            Duration::from_secs(2),
            script.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: "implement-ready".into(),
                },
            ),
        )
        .await??;
        std::fs::create_dir_all(root.path().join("src"))?;
        std::fs::write(
            root.path().join("src/owner.rs"),
            "/// Own state.\npub struct Owner;\n",
        )?;
        script
            .dispatch(
                &session_id,
                MockResponseCommand::Release {
                    id: "implement-ready".into(),
                },
            )
            .await?;
        for barrier in ["implement-requested", "implement-collected"] {
            tokio::time::timeout(
                Duration::from_secs(2),
                script.dispatch(
                    &session_id,
                    MockResponseCommand::Wait { id: barrier.into() },
                ),
            )
            .await??;
            assert_eq!(
                controller.task_controller.current()?.unwrap().phase(),
                crate::task::TaskPhase::Implement
            );
            assert_eq!(controller.coordinator.execution.available_permits(), 0);
            script
                .dispatch(
                    &session_id,
                    MockResponseCommand::Release { id: barrier.into() },
                )
                .await?;
        }
        tokio::time::timeout(Duration::from_secs(3), async {
            loop {
                if controller.task_controller.current()?.is_none()
                    && controller.coordinator.execution.available_permits() == 1
                {
                    break Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("phase request did not finish through native collection and checking")??;
        assert!(
            store
                .list_exchange(&session_id)?
                .iter()
                .all(|exchange| exchange.state() == crate::exchange::ExchangeState::Complete)
        );
    }
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    reader.await??;
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
                turn: None,
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
