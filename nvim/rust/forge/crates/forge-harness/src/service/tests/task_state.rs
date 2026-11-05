use super::super::{HarnessBroker, SessionController, prepare_new_session};
use super::bootstrap::{initialize, service};
use super::reviewed_plan::{
    reviewed_execution_fixture, reviewed_execution_fixture_with_acceptance,
};
use crate::protocol::RuntimeMethod;
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::Value;
use serde_json::json;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Duration;

enum PlanningRestart {
    Permissions,
    Effort,
}

/// Configuration restarts must not exhaust a plan before any uninterrupted response finishes.
#[tokio::test]
async fn planning_permission_restarts_preserve_no_progress_budget() -> Result<()> {
    planning_configuration_restarts(PlanningRestart::Permissions).await
}

/// Model effort uses the same planning accounting as execution permission changes.
#[tokio::test]
async fn planning_effort_restarts_preserve_no_progress_budget() -> Result<()> {
    planning_configuration_restarts(PlanningRestart::Effort).await
}

/// Drive real owner restarts with held native turns across both configuration routes.
async fn planning_configuration_restarts(setting: PlanningRestart) -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{TestPromptScript, TestPromptStep, TestPromptTerminal};
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let provider = controller.provider_handle();
    let control = provider.mock_response_control().unwrap();
    let response = TestPromptScript {
        acceptance: Default::default(),
        steps: vec![TestPromptStep::Message {
            role: forge_provider_api::MessageRole::Response,
            text: "I have not changed the plan yet.".into(),
        }],
        terminal: TestPromptTerminal::Completed,
    };
    control
        .dispatch(
            &session_id,
            MockResponseCommand::Script {
                script: response.clone(),
            },
        )
        .await?;
    let hold = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let start = Request {
        id: 2,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"restart-plan","action":"plan","text":"Plan a small game"}),
    };
    service.prepare_task(Some(&session_id), &start).await?;
    service
        .dispatch(Some(session_id.clone()), start, &sink)
        .await?;
    tokio::time::timeout(
        Duration::from_secs(5),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: hold["id"].as_str().unwrap().into(),
            },
        ),
    )
    .await??;
    let task_id = controller.task_controller.current()?.unwrap().id.clone();
    let scenario = async {
        let mut current_hold = hold["id"].as_str().unwrap().to_owned();
        for index in 0..6 {
            control.dispatch(&session_id, MockResponseCommand::Script {script:response.clone()}).await?;
            let hold = control.dispatch(&session_id, MockResponseCommand::Hold {mode:None,any_session:false}).await?;
            let request = match setting {
                PlanningRestart::Effort => crate::service::native_settings::decode_command(&Request {
                id:10+index,method:"session.command.apply".into(),
                params:json!({"operation_id":format!("planning-effort-{index}"),"action":"configure","config":{"effort":if index%2==0 {"high"} else {"low"}}}),
            })?,
                PlanningRestart::Permissions => crate::service::native_permission::decode_command(&Request {
                id:10+index,method:"session.command.apply".into(),
                params:json!({"operation_id":format!("planning-toggle-{index}"),"action":"permission","change":{"kind":"toggle_write_enabled"}}),
            })?,
            };
            tokio::time::timeout(Duration::from_secs(10), controller.exchange_owner.execute_identified(request,sink.clone())).await??;
            tokio::time::timeout(Duration::from_secs(5), control.dispatch(&session_id, MockResponseCommand::Wait {id:hold["id"].as_str().unwrap().into()})).await??;
            current_hold = hold["id"].as_str().unwrap().to_owned();
            let task = controller.task_controller.current()?.unwrap();
            assert_eq!(task.id, task_id);
            let crate::task::WorkflowSnapshot::Plan { plan } = &task.workflow else { panic!("planning task replaced"); };
            assert_eq!(plan.artifact.generation.budget.turn_count, 0, "configuration interrupted turn counted at toggle {index}");
            assert_eq!(plan.artifact.generation.budget.consecutive_no_progress, 0);
            assert_eq!(plan.artifact.state, crate::plan::PlanState::Generating);
        }
        let continuation = control.dispatch(&session_id, MockResponseCommand::Hold {mode:None,any_session:false}).await?;
        control.dispatch(&session_id, MockResponseCommand::Release {id:current_hold}).await?;
        tokio::time::timeout(Duration::from_secs(5), control.dispatch(&session_id, MockResponseCommand::Wait {id:continuation["id"].as_str().unwrap().into()})).await??;
        let task = controller.task_controller.current()?.unwrap();
        let crate::task::WorkflowSnapshot::Plan { plan } = &task.workflow else { panic!("planning task replaced"); };
        assert_eq!(task.id, task_id);
        assert_eq!(plan.artifact.generation.budget.turn_count, 1, "one completed response must count after interrupted attempts");
        assert_eq!(plan.artifact.generation.budget.consecutive_no_progress, 1);
        assert_eq!(plan.artifact.state, crate::plan::PlanState::Generating);
        Ok::<(),anyhow::Error>(())
    }.await;
    service.shutdown(Duration::from_secs(10)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(5), reader).await???;
    scenario
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

/// Task pause/clear aliases collect native work and replay their owner receipts without replacing the provider.
#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    for transport in ["transition", "rpc", "prompt", "input", "steer"] {
        for action in ["pause", "clear"] {
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
            service.dispatch(Some(session_id.clone()), Request {
                id: 2, method: "session.input.apply".into(), params: json!({"id":"held-task-input","action":{"kind":"submit","message":{
                    "id":"held-task-input","text":"Inspect startup","intent":"auto","attachments":[]}}}),
            }, &sink).await?;
            tokio::time::timeout(
                Duration::from_secs(5),
                script.dispatch(&session_id, MockResponseCommand::Wait { id: hold_id }),
            )
            .await??;
            let store = SqliteStore::open(&controller.data_root)?;
            let original = store.latest_exchange(&session_id)?.unwrap().id;
            let request = goal_command_request(transport, action, 10);
            tokio::time::timeout(
                Duration::from_secs(5),
                service.dispatch(Some(session_id.clone()), request.clone(), &sink),
            )
            .await??;
            tokio::time::timeout(Duration::from_secs(5), async {
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
            })
            .await
            .context("task alias did not collect work and settle its queued directive")??;
            let execution = controller.broker.lock().await.execution_registry();
            let active = execution
                .existing_owner(&crate::provider::ExecutionOwner::root(&session_id))?
                .map(|engine| engine.active_invocation_snapshot())
                .transpose()?
                .flatten();
            assert!(active.is_none());
            assert!(Arc::ptr_eq(&provider, &controller.provider_handle()));
            let before = serde_json::to_value(controller.task_controller.current()?)?;
            let receipt_id = crate::exchange::command::CommandId::new(
                request.params["operation_id"].as_str().unwrap(),
            )?;
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
            tokio::time::timeout(
                Duration::from_secs(5),
                service.dispatch(Some(session_id.clone()), request, &sink),
            )
            .await??;
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
            tokio::time::timeout(Duration::from_secs(2), reader).await???;
        }
    }
    Ok(())
}

#[tokio::test]
async fn goal_continue_aliases_reuse_the_completed_source_and_receipt() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    for transport in ["transition", "prompt", "input", "steer"] {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
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
                    params: json!({"text":"Inspect startup"}),
                },
                &sink,
            )
            .await?;
        let mut store = SqliteStore::open(&controller.data_root)?;
        let source = tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if let Some(source) = store.latest_exchange(&session_id)?
                    && source.is_finished()
                    && controller.coordinator.execution.available_permits() == 1
                {
                    break Ok::<_, anyhow::Error>(source);
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("initial prompt did not finish")??;
        controller.task_controller.change(
            &mut store,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Preserve the held provider".into(),
                crate::session::continuation::ContinuationBudget::new(20, 2),
                1,
            ))),
            1,
        )?;
        let mock_provider = controller.provider_handle();
        let control = mock_provider.mock_response_control().unwrap();
        let hold = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"].as_str().unwrap().to_owned();
        let request = goal_command_request(transport, "continue", 3);
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(Some(session_id.clone()), request.clone(), &sink),
        )
        .await??;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        let task = serde_json::to_value(controller.task_controller.current()?)?;
        assert!(
            controller
                .exchange_owner
                .command_status(crate::exchange::command::CommandId::new(
                    request.params["operation_id"].as_str().unwrap()
                )?)
                .await?
                .unwrap()
                .completion()
                .unwrap()
                .is_ok()
        );
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(Some(session_id.clone()), request, &sink),
        )
        .await??;
        assert_eq!(
            serde_json::to_value(controller.task_controller.current()?)?,
            task
        );
        let history = store.list_exchange(&session_id)?;
        assert_eq!(history.len(), 2);
        assert_eq!(history[0].id, source.id);
        assert!(matches!(
            history[1].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert!(history[1].prompt.contains("Preserve the held provider"));
        assert!(!history[1].prompt.trim_start().starts_with("/goal"));
        assert!(!*controller.cancellation.subscribe().borrow());
        service
            .dispatch(
                Some(session_id.clone()),
                goal_command_request("transition", "clear", 4),
                &sink,
            )
            .await?;
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        assert_eq!(store.list_exchange(&session_id)?.len(), 2);
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn queued_input_edited_to_goal_starts_once_and_preserves_other_input() -> Result<()> {
    use crate::input::{InputAction, InputEdit, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = tempfile::tempdir()?;
    crate::checks::configure_fixture(fixture.path());
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let mock_provider = controller.provider_handle();
    let control = mock_provider.mock_response_control().unwrap();
    let hold = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let hold_id = hold["id"].as_str().unwrap().to_owned();
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
                params: json!({"id":"original","action":{"kind":"submit","message":{
            "id":"original","text":"Inspect startup","intent":"auto","attachments":[]}}}),
            },
            &sink,
        )
        .await?;
    tokio::time::timeout(
        Duration::from_secs(5),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: hold_id.clone(),
            },
        ),
    )
    .await??;
    let store = SqliteStore::open(&controller.data_root)?;
    let original = store.latest_exchange(&session_id)?.unwrap();
    controller.coordinator.submissions.apply(InputOperation {
        id: "queue-earlier".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: "earlier".into(),
                text: "Keep this earlier queued follow-up".into(),
                purpose: Default::default(),
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Queue,
            })),
        },
    })?;
    controller.coordinator.submissions.apply(InputOperation {
        id: "queue-original".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: "queued".into(),
                text: "Ordinary queued follow-up".into(),
                purpose: Default::default(),
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Auto,
            })),
        },
    })?;
    let queued = controller
        .coordinator
        .submissions
        .snapshot()?
        .messages
        .into_iter()
        .find(|message| message.id == "queued")
        .unwrap();
    let reserved = controller.coordinator.submissions.apply(InputOperation {
        id: "reserve-edit".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Reserve {
            id: queued.id.clone(),
            revision: queued.revision,
            reservation: "editor".into(),
        },
    })?;
    let reserved_revision = reserved
        .upsert
        .iter()
        .find(|message| message.id == "queued")
        .unwrap()
        .revision;
    let edit = InputOperation {
        id: "commit-edit".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Commit {
            reservation: "editor".into(),
            edits: vec![InputEdit {
                id: queued.id,
                revision: reserved_revision,
                text: "/goal Preserve the edited intent".into(),
                attachments: Vec::new(),
                config: None,
            }],
        },
    };
    let replacement = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let update = controller.coordinator.submissions.apply(edit.clone())?;
    let revision = update
        .upsert
        .iter()
        .find(|message| message.id == "queued")
        .unwrap()
        .revision;
    let receipt_id = format!("input-command:queued:{revision}");
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller
                .exchange_owner
                .command_status(crate::exchange::command::CommandId::new(&receipt_id)?)
                .await?
                .is_some()
                && controller
                    .task_controller
                    .current()?
                    .is_some_and(|task| task.entry_accepted())
                && !controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .iter()
                    .any(|message| message.id == "queued")
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await
    .with_context(|| {
        format!(
            "edited goal command was not admitted; input={:?}; task={:?}",
            controller.coordinator.submissions.snapshot(),
            controller.task_controller.current()
        )
    })??;
    tokio::time::timeout(
        Duration::from_secs(5),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: replacement["id"].as_str().unwrap().into(),
            },
        ),
    )
    .await??;
    let task = controller.task_controller.current()?.unwrap();
    assert_eq!(task.title(), "Preserve the edited intent");
    let event_count = store.list_session_event(&session_id)?.len();
    assert!(
        controller
            .exchange_owner
            .command_status(crate::exchange::command::CommandId::new(&receipt_id)?)
            .await?
            .unwrap()
            .completion()
            .unwrap()
            .is_ok()
    );
    controller.coordinator.submissions.apply(edit)?;
    assert_eq!(
        controller.task_controller.current()?.unwrap().revision,
        task.revision
    );
    assert!(
        !controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .iter()
            .any(|message| message.id == "queued")
    );
    assert_eq!(store.list_session_event(&session_id)?.len(), event_count);
    let earlier = controller
        .coordinator
        .submissions
        .snapshot()?
        .messages
        .into_iter()
        .find(|message| message.id == "earlier")
        .context("edited command removed earlier queued input")?;
    assert_eq!(earlier.text, "Keep this earlier queued follow-up");
    assert_eq!(earlier.intent, InputIntent::Queue);
    assert!(!*controller.cancellation.subscribe().borrow());
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    let history = store.list_exchange(&session_id)?;
    assert_eq!(history.len(), 2);
    assert!(
        history[0].is_finished(),
        "task replacement left the original exchange running"
    );
    assert_eq!(history[0].id, original.id);
    assert!(
        matches!(&history[1].origin, crate::exchange::ExchangeOrigin::TaskStarted { start }
        if start.title == "Preserve the edited intent")
    );
    assert!(!history[0].node_list.iter().any(|node| matches!(node,
        crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.text.starts_with("/goal"))));
    controller.coordinator.submissions.apply(InputOperation {
        id: "remove-fixture-follow-up".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Remove {
            id: earlier.id,
            revision: earlier.revision,
        },
    })?;
    service
        .dispatch(
            Some(session_id.clone()),
            goal_command_request("transition", "clear", 3),
            &sink,
        )
        .await?;
    tokio::time::timeout(Duration::from_secs(5), async {
        while controller.coordinator.execution.available_permits() == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert_eq!(store.list_exchange(&session_id)?.len(), 2);
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    Ok(())
}

#[tokio::test]
async fn plan_replacement_collects_prior_work_before_initial_dispatch() -> Result<()> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    for planning in [false, true] {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let mock_provider = controller.provider_handle();
        let control = mock_provider.mock_response_control().unwrap();
        let held = control
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
        if planning {
            let request = Request {
                id: 2,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"original-plan","action":"plan","text":"Original plan"}),
            };
            service.prepare_task(Some(&session_id), &request).await?;
            service
                .dispatch(Some(session_id.clone()), request, &sink)
                .await?;
        } else {
            controller.coordinator.submissions.apply(InputOperation {
                id: "original".into(),
                control_revision: 0,
                after_control: None,
                action: InputAction::Submit {
                    message: Box::new(InputMessage::queued(crate::input::InputDraft {
                        id: "original-input".into(),
                        text: "Inspect startup".into(),
                        purpose: Default::default(),
                        attachments: Vec::new(),
                        config: None,
                        intent: InputIntent::Auto,
                    })),
                },
            })?;
        }
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let original = store.latest_exchange(&session_id)?.unwrap();
        let request = Request {
            id: 3,
            method: "session.command.apply".into(),
            params: json!({"operation_id":"replacement-plan","action":"plan","text":"Replacement plan"}),
        };
        service.prepare_task(Some(&session_id), &request).await?;
        tokio::time::timeout(
            Duration::from_secs(5),
            service.dispatch(Some(session_id.clone()), request.clone(), &sink),
        )
        .await??;
        let pending = controller.task_controller.current()?.unwrap();
        assert_eq!(pending.title(), "Replacement plan");
        assert!(!*controller.cancellation.subscribe().borrow());
        assert_eq!(store.list_exchange(&session_id)?.len(), 2);
        assert_ne!(store.latest_exchange(&session_id)?.unwrap().id, original.id);
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let current = controller.task_controller.current()?.unwrap();
                if matches!(&current.workflow, crate::task::WorkflowSnapshot::Plan { plan }
                    if plan.artifact.state == crate::plan::PlanState::AwaitingReview)
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("replacement plan did not reach review")??;
        let current = controller.task_controller.current()?.unwrap();
        assert!(current.entry_accepted());
        let crate::task::WorkflowSnapshot::Plan { plan } = &current.workflow else {
            unreachable!()
        };
        assert_eq!(
            store.load_plan(&plan.artifact.id)?.unwrap().state,
            crate::plan::PlanState::AwaitingReview
        );
        let observed = controller.observer.snapshot()?;
        assert_eq!(
            serde_json::to_value(observed.active_plan.as_ref().unwrap())?,
            serde_json::to_value(&plan.artifact)?
        );
        let exchanges = store.list_exchange(&session_id)?;
        assert_eq!(exchanges.len(), 2);
        assert!(
            matches!(&exchanges[1].origin, crate::exchange::ExchangeOrigin::TaskStarted { start }
            if start.title == "Replacement plan")
        );
        assert!(exchanges[1].prompt.contains("Replacement plan"));
        service
            .dispatch(Some(session_id.clone()), request, &sink)
            .await?;
        assert_eq!(
            controller.task_controller.current()?.unwrap().revision,
            current.revision
        );
        assert_eq!(store.list_exchange(&session_id)?.len(), 2);
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn goal_continuation_uses_durable_input_and_yields_to_user_queue() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    for queued_user in [false, true] {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let mock_provider = controller.provider_handle();
        let control = mock_provider.mock_response_control().unwrap();
        let held = control
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
        let access = crate::session::AccessPolicy {
            write_access: crate::session::WriteScope::Off,
            ..Default::default()
        };
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 4,
                    method: "session.configure".into(),
                    params: json!({"access":access}),
                },
                &sink,
            )
            .await?;
        controller.attach_input(&sink).await?;
        service.dispatch(Some(session_id.clone()), Request {
            id: 2, method: "session.command.apply".into(),
            params: json!({"operation_id":"start-goal","action":"goal","text":"Finish the work"}),
        }, &sink).await?;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        if queued_user {
            controller
                .input_request(
                    Request {
                        id: 3,
                        method: "session.input.apply".into(),
                        params: json!({"id":"queued-user","action":{"kind":"submit","message":{
                            "id":"user-message", "text":"Use the revised requirement",
                            "intent":"queue", "attachments":[],"config":null,
                            "reservation":null,"submission_id":null,"error":null
                        }}}),
                    },
                    RuntimeMethod::SessionInputApply,
                    &sink,
                )
                .await?;
        }
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if controller.task_controller.current()?.is_some_and(|task| {
                    matches!(task.workflow.clone(), crate::task::WorkflowSnapshot::Goal { goal }
                        if goal.state == crate::goal::GoalState::Stalled)
                }) && controller.coordinator.execution.available_permits() > 0
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("goal queue did not settle")??;
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let exchanges = store.list_exchange(&session_id)?;
        assert_eq!(exchanges.len(), 5);
        assert!(
            exchanges
                .iter()
                .all(|exchange| exchange.state() == crate::exchange::ExchangeState::Complete)
        );
        if queued_user {
            assert!(matches!(
                exchanges[1].origin,
                crate::exchange::ExchangeOrigin::User
            ));
            assert_eq!(exchanges[1].prompt, "Use the revised requirement");
        } else {
            assert!(matches!(
                exchanges[1].origin,
                crate::exchange::ExchangeOrigin::TaskContinuation { .. }
            ));
            assert_eq!(
                exchanges[1].prompt,
                "Continue working toward this goal: Finish the work"
            );
        }
        let task = store.load_active_task(&session_id)?.unwrap();
        assert_eq!(
            task.turn.as_ref().unwrap().id.exchange_ordinal,
            exchanges.last().unwrap().ordinal
        );
        assert!(!*controller.cancellation.subscribe().borrow());
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn task_recovery_keeps_one_unsent_continuation() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = tempfile::tempdir()?;
    let original_service = service();
    original_service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = original_service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let access = crate::session::AccessPolicy {
        write_access: crate::session::WriteScope::Off,
        ..Default::default()
    };
    original_service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.configure".into(),
                params: json!({"access":access}),
            },
            &sink,
        )
        .await?;
    let mock_provider = controller.provider_handle();
    let control = mock_provider.mock_response_control().unwrap();
    let held = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let hold_id = held["id"].as_str().unwrap().to_owned();
    controller.attach_input(&sink).await?;
    original_service.dispatch(Some(session_id.clone()), Request {
        id: 3, method: "session.command.apply".into(),
        params: json!({"operation_id":"restart-goal","action":"goal","text":"Finish after restart"}),
    }, &sink).await?;
    tokio::time::timeout(
        Duration::from_secs(5),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: hold_id.clone(),
            },
        ),
    )
    .await??;
    let admission = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    control
        .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
        .await?;
    let pending = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if let Some(task) = controller.task_controller.current()?
                && let Some(input) = task
                    .turn
                    .as_ref()
                    .and_then(|turn| turn.continuation.as_ref())
                && controller.coordinator.execution.available_permits() > 0
            {
                break Ok::<_, anyhow::Error>(input.clone());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .with_context(|| {
        format!(
            "pending continuation did not settle: permits={}, task={:?}",
            controller.coordinator.execution.available_permits(),
            controller
                .task_controller
                .current()
                .map(|task| task.map(|task| (task.status(), task.turn.clone())))
        )
    })??;
    assert_eq!(pending.delivery, crate::task::TaskInputDelivery::Pending);
    original_service.shutdown(Duration::from_secs(2)).await?;
    drop(admission);
    drop(controller);
    drop(registry);
    drop(original_service);
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;

    let recovered_service = service();
    let mut request = initialize(&fixture, "mock");
    request.session_id = Some(session_id.clone());
    recovered_service.open_session(4, request).await?;
    let registry = recovered_service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    let task = controller.task_controller.current()?.unwrap();
    assert_eq!(task.status(), crate::task::TaskStatus::Paused);
    assert_eq!(
        task.turn
            .as_ref()
            .unwrap()
            .continuation
            .as_ref()
            .unwrap()
            .delivery,
        crate::task::TaskInputDelivery::Cancelled
    );
    assert_eq!(
        task.turn
            .as_ref()
            .unwrap()
            .continuation
            .as_ref()
            .unwrap()
            .id,
        pending.id
    );
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    tokio::task::yield_now().await;
    let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    assert_eq!(
        store.list_exchange(&session_id)?.len(),
        1,
        "restart dispatched without explicit resume"
    );
    recovered_service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 5,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"explicit-recovery-resume","action":"resume"}),
            },
            &sink,
        )
        .await?;
    let resumed = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller.task_controller.current()?.is_some_and(|task| {
                matches!(task.workflow.clone(), crate::task::WorkflowSnapshot::Goal { goal }
                    if goal.state == crate::goal::GoalState::Stalled)
            }) && controller.coordinator.execution.available_permits() > 0
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await;
    resumed.with_context(|| {
        format!(
            "recovered continuation did not settle: input={:?}, task={:?}",
            controller.coordinator.submissions.snapshot(),
            controller
                .task_controller
                .current()
                .map(|task| task.map(|task| (task.status(), task.turn.clone())))
        )
    })??;
    let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let exchanges = store.list_exchange(&session_id)?;
    assert_eq!(exchanges.len(), 6);
    assert!(matches!(
        exchanges.last().unwrap().origin,
        crate::exchange::ExchangeOrigin::TaskContinuation { .. }
    ));
    assert_eq!(
        exchanges.last().unwrap().prompt,
        pending.work.instruction().unwrap()
    );
    recovered_service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    Ok(())
}

#[tokio::test]
async fn running_goal_startup_collects_prior_work_and_preserves_stale_input_guards() -> Result<()> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let mock_provider = controller.provider_handle();
    let control = mock_provider.mock_response_control().unwrap();
    let held = control
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
    let accepted_session = Arc::new(AtomicBool::new(false));
    let observed_session = accepted_session.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] == "session_changed"
                && event["payload"]["snapshot"]["session"]["active_task"]["entry"]["delivery"]
                    == "accepted"
            {
                observed_session.store(true, Ordering::Release);
            }
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    controller.coordinator.submissions.apply(InputOperation {
        id: "original".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: "original-input".into(),
                text: "Inspect startup".into(),
                purpose: Default::default(),
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Auto,
            })),
        },
    })?;
    tokio::time::timeout(
        Duration::from_secs(5),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: hold_id.clone(),
            },
        ),
    )
    .await??;
    let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let original = store
        .latest_exchange(&session_id)?
        .context("original exchange missing")?;
    let goal_held = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let goal_hold_id = goal_held["id"].as_str().unwrap().to_owned();
    let request = Request {
        id: 2,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"live-goal","action":"goal","text":"Finish startup work"}),
    };
    service.prepare_task(Some(&session_id), &request).await?;
    tokio::time::timeout(
        Duration::from_secs(5),
        service.dispatch(Some(session_id.clone()), request, &sink),
    )
    .await??;
    tokio::time::timeout(
        Duration::from_secs(5),
        control.dispatch(&session_id, MockResponseCommand::Wait { id: goal_hold_id }),
    )
    .await??;
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller
                .task_controller
                .current()?
                .is_some_and(|task| task.entry_accepted())
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await
    .context("initial goal input was not acknowledged")??;
    tokio::time::timeout(Duration::from_secs(2), async {
        while !accepted_session.load(Ordering::Acquire) {
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("acknowledged task state was not published to the client")?;
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    assert!(!*controller.cancellation.subscribe().borrow());
    let task = controller.task_controller.current()?.unwrap();
    controller.task_controller.admit_goal_control(true)?;
    let entry = task.entry.clone().unwrap();
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if store
                .load_session_event(
                    &session_id,
                    &format!("{session_id}:task-start:{}", entry.id.as_str()),
                )?
                .is_some()
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await
    .context("acknowledged task entry was not published")??;
    let exchanges = store.list_exchange(&session_id)?;
    assert_eq!(exchanges.len(), 2);
    assert_eq!(exchanges[0].id, original.id);
    assert_ne!(exchanges[1].id, original.id);
    assert_eq!(
        exchanges[1].state(),
        crate::exchange::ExchangeState::Running
    );
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 4,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"pause-live-goal","action":"pause"}),
            },
            &sink,
        )
        .await?;
    controller.coordinator.submissions.apply(InputOperation {
        id: "failed-old-entry".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: "failed-old-entry".into(),
                text: "Obsolete entry".into(),
                purpose: crate::input::InputPurpose::TaskEntry {
                    revision: task.revision.clone(),
                    id: crate::task::TaskInputId::fresh(),
                    start: crate::exchange::TaskStart {
                        kind: task.kind(),
                        title: task.title().into(),
                    },
                },
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Auto,
            })),
        },
    })?;
    controller.coordinator.submissions.delivery(
        "failed-old-entry",
        Some(crate::input::InputDelivery::Failed),
        None,
        Some("old delivery failure".into()),
    )?;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 5,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"resume-live-goal","action":"resume"}),
            },
            &sink,
        )
        .await?;
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller
                .task_controller
                .current()?
                .is_some_and(|current| {
                    current.revision != task.revision && current.entry_accepted()
                })
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await
    .context("resume was blocked by a stale failed entry")??;
    assert!(
        !controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .iter()
            .any(|message| message.id == "failed-old-entry")
    );
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 3,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"clear-live-goal","action":"clear"}),
            },
            &sink,
        )
        .await?;
    assert!(!*controller.cancellation.subscribe().borrow());
    tokio::time::timeout(Duration::from_secs(5), async {
        while controller.coordinator.execution.available_permits() == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert!(controller.task_controller.current()?.is_none());
    assert_eq!(store.list_exchange(&session_id)?.len(), 3);
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader)
        .await
        .context("shutdown retained a session subscription sender")???;
    Ok(())
}

/// Scoped tools may finish their work before native acknowledgment, without forging it.
#[tokio::test]
async fn plan_submission_precedes_terminal_native_acceptance() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    crate::checks::configure_fixture(fixture.path());
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let observed_review = Arc::new(AtomicBool::new(false));
    let refreshed_review = observed_review.clone();
    let observer = controller.observer.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] == "plan_created" || event["event"] == "plan_revision_created" {
                // These notifications request a state refresh; they do not carry a session snapshot.
                let snapshot = observer.snapshot()?;
                if snapshot.session.active_task.as_ref().is_some_and(|task| {
                    matches!(&task.workflow, crate::task::WorkflowSnapshot::Plan { plan }
                        if plan.artifact.state == crate::plan::PlanState::AwaitingReview)
                }) {
                    refreshed_review.store(true, Ordering::Release);
                }
            }
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let selection = reviewed_execution_fixture_with_acceptance(
        &service,
        &controller,
        &session_id,
        &sink,
        forge_provider_api::testing::TestInputAcceptance::AfterResponse,
        true,
    )
    .await?;
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller
                .task_controller
                .current()?
                .is_some_and(|task| task.entry_accepted())
            {
                break Ok::<(), anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("terminal native acceptance was not acknowledged")??;
    let store = SqliteStore::read_only(&controller.data_root)?;
    let plan_id = selection["plan_id"]
        .as_str()
        .context("fixture plan id missing")?;
    let committed = store
        .load_plan(plan_id)?
        .context("submitted plan missing")?;
    assert_eq!(
        committed.document_version, 2,
        "the edit must commit once before submission"
    );
    assert_eq!(
        committed.model_revision, 2,
        "both same-invocation submissions must commit exactly once"
    );
    let document = controller
        .observer
        .plan_file
        .read_working_document(&session_id, plan_id)?;
    assert_eq!(
        document
            .design
            .as_ref()
            .context("edited design missing")?
            .document
            .design,
        "Commit each plan change before returning its tool response."
    );
    service.dispatch(Some(session_id.clone()), Request {
        id: 32, method: "session.command.apply".into(),
        params: json!({"operation_id":"fixture-plan","action":"attach_plan","plan_id":plan_id}),
    }, &sink).await?;
    assert_eq!(
        serde_json::to_value(store.load_plan(plan_id)?.unwrap())?,
        serde_json::to_value(&committed)?,
        "replaying the command must not apply the tool effects again"
    );
    let receipt = controller
        .exchange_owner
        .command_status(crate::exchange::command::CommandId::new("fixture-plan")?)
        .await?
        .context("initial plan command receipt missing")?;
    let Some(Ok(crate::exchange::command::CommandResult {
        outcome:
            crate::exchange::command::CommandOutcome::Task(
                crate::exchange::command::TaskCommandOutcome::Started { input, .. },
            ),
        ..
    })) = receipt.completion()
    else {
        anyhow::bail!("initial plan command did not dispatch: {receipt:?}");
    };
    let receipts = rusqlite::Connection::open_with_flags(
        controller.data_root.join("provider-submissions.sqlite3"),
        rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY,
    )?;
    let native_receipt: String = receipts.query_row(
        "SELECT id FROM submission WHERE json_extract(payload,'$.input_message_id')=?1",
        [input.as_str()],
        |row| row.get(0),
    )?;
    let trace = controller.trace.clone();
    let path = trace.session_status(&session_id).path;
    let retained_trace = path.clone();
    let events = tokio::task::spawn_blocking(move || -> Result<Vec<Value>> {
        trace.flush()?;
        std::fs::read_to_string(path)?
            .lines()
            .map(|line| serde_json::from_str(line).map_err(Into::into))
            .collect()
    })
    .await??;
    let submissions = events
        .iter()
        .enumerate()
        .filter(|(_, event)| event["event"] == "plan_submitted")
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    assert_eq!(
        submissions.len(),
        2,
        "submit, edit, and resubmit must survive task revision advancement"
    );
    let submitted = submissions
        .last()
        .copied()
        .context("plan_submit did not commit its review artifact")?;
    let accepted = events
        .iter()
        .position(|event| {
            event["event"] == "provider.submission"
                && event["payload"]["submission_id"] == native_receipt
                && event["payload"]["delivery"] == "accepted"
        })
        .context("selected native accepted receipt was not recorded")?;
    if submitted >= accepted {
        let retained_fixture = fixture.keep();
        anyhow::bail!(
            "plan submission did not precede its native acceptance; fixture={}; trace={retained_trace}; input={}; native_receipt={native_receipt}; submitted={}; accepted={}",
            retained_fixture.display(),
            input.as_str(),
            events[submitted],
            events[accepted],
        );
    }
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    if !observed_review.load(Ordering::Acquire) {
        let retained_fixture = fixture.keep();
        anyhow::bail!(
            "plan notification refresh did not expose committed review state; fixture={}; trace={retained_trace}; native_receipt={native_receipt}",
            retained_fixture.display(),
        );
    }
    Ok(())
}

/// Apply live policy changes to the first full submission without replacing provider work.
#[tokio::test]
async fn plan_accept_policy_controls_initial_submission() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    for policy in ["default", "on", "off"] {
        let enabled = policy == "on";
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let mock_provider = controller.provider_handle();
        let control = mock_provider.mock_response_control().unwrap();
        let hold = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: Some(crate::provider::PromptMode::Plan),
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"].as_str().unwrap().to_owned();
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.command.apply".into(),
                    params: json!({"action":"plan","text":"Inspect ownership"}),
                },
                &sink,
            )
            .await?;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        let before = controller.task_controller.current()?.unwrap();
        assert!(
            !before.auto_accept_plan,
            "new Plan tasks must require review"
        );
        for selected in match policy {
            "on" => vec![true],
            "off" => vec![true, false],
            _ => Vec::new(),
        } {
            service.dispatch(Some(session_id.clone()), Request {
                id: 3, method: "session.prompt.submit".into(),
                params: json!({"text":if selected {"/plan accept on"} else {"/plan accept off"}}),
            }, &sink).await?;
        }
        assert_eq!(
            controller.task_controller.current()?.unwrap().revision,
            before.revision
        );
        let next = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: Some(crate::provider::PromptMode::ExecutePlan),
                    any_session: false,
                },
            )
            .await?;
        let next_id = next["id"].as_str().unwrap().to_owned();
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        if enabled {
            tokio::time::timeout(
                Duration::from_secs(5),
                control.dispatch(
                    &session_id,
                    MockResponseCommand::Wait {
                        id: next_id.clone(),
                    },
                ),
            )
            .await??;
            let task = controller.task_controller.current()?.unwrap();
            assert!(task.auto_accept_plan);
            assert_eq!(
                task.id, before.id,
                "Plan acceptance replaced the durable task"
            );
            let crate::task::WorkflowSnapshot::Execute { execution } = &task.workflow else {
                unreachable!()
            };
            assert_eq!(
                execution.continuation.turn_count, 1,
                "Plan acceptance lost its completed model turn"
            );
            assert!(matches!(
                task.workflow.clone(),
                crate::task::WorkflowSnapshot::Execute { .. }
            ));
        } else {
            tokio::time::timeout(Duration::from_secs(5), async {
                loop {
                    if let Some(task) = controller.task_controller.current()?
                        && let crate::task::WorkflowSnapshot::Plan { plan } = &task.workflow
                        && plan.artifact.state == crate::plan::PlanState::AwaitingReview
                        && controller.coordinator.execution.available_permits() > 0
                    {
                        assert!(!task.auto_accept_plan);
                        assert_eq!(plan.artifact.accepted_revision, None);
                        break Ok::<(), anyhow::Error>(());
                    }
                    tokio::task::yield_now().await;
                }
            })
            .await??;
        }
        control
            .dispatch(&session_id, MockResponseCommand::Cancel { id: next_id })
            .await?;
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

/// Keep feedback in its exchange and honor task policy changes before revised submission.
#[tokio::test]
async fn accepted_inline_feedback_revises_before_execution_without_changing_policy() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    for decision in [
        "accept-feedback",
        "execute-direct-feedback",
        "accept-empty",
        "request-changes",
        "accept-off",
        "request-on",
    ] {
        let request_changes = decision.starts_with("request");
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let selection =
            reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
        let plan_id = selection["plan_id"].as_str().unwrap();
        let store = SqliteStore::read_only(&controller.data_root)?;
        let initial = store.load_plan(plan_id)?.unwrap();
        let original_exchange = store.latest_exchange(&session_id)?.unwrap();
        let source = controller
            .observer
            .capture_plan_review(plan_id, initial.review_digest.as_deref().unwrap())?;
        let line = source.rendered.navigation.anchor.first().unwrap().line;
        let feedback = "Conversation context:\nPlan question:\nWhy this design?\n\nPlan answer:\nIt preserves ownership.\n\nRequested change:\nClarify the ownership boundary.";
        let annotations = if decision == "accept-empty" {
            json!([])
        } else {
            json!([{"start_line":line,"end_line":line,"body":feedback}])
        };
        if decision == "execute-direct-feedback" {
            let before_task = controller.task_controller.current()?.unwrap();
            let before_document = controller
                .observer
                .plan_file
                .read_working_document(&session_id, plan_id)?;
            for stale_source in [false, true] {
                let mut invalid: crate::task::command::ExecuteSelection =
                    serde_json::from_value(selection.clone())?;
                invalid.saved_source_digest = Some(if stale_source {
                    "stale-source".into()
                } else {
                    source.saved_digest.clone()
                });
                invalid.annotations = vec![crate::plan::PlanAnnotationInput {
                    start_line: if stale_source { line } else { 0 },
                    end_line: if stale_source { line } else { 0 },
                    body: feedback.into(),
                }];
                assert!(
                    controller
                        .observer
                        .prepare_execution(&invalid, 20, 100)
                        .is_err()
                );
                assert_eq!(
                    controller.task_controller.current()?.unwrap().revision,
                    before_task.revision
                );
                assert_eq!(
                    serde_json::to_value(store.load_plan(plan_id)?.unwrap())?,
                    serde_json::to_value(&initial)?
                );
                assert_eq!(
                    controller
                        .observer
                        .plan_file
                        .read_working_document(&session_id, plan_id)?,
                    before_document
                );
            }
        }
        let mock_provider = controller.provider_handle();
        let control = mock_provider.mock_response_control().unwrap();
        if decision != "accept-empty" {
            let revised_version = initial
                .document_version
                .checked_add(1)
                .context("fixture plan version exhausted")?;
            control.dispatch(&session_id, MockResponseCommand::Script {
                script: forge_provider_api::testing::TestPromptScript {
                    acceptance: Default::default(),
                    steps: vec![
                        forge_provider_api::testing::TestPromptStep::ToolCall {
                            name: "plan_overview_edit".into(),
                            arguments: json!({"plan_id":plan_id,"expected_version":initial.document_version,
                                "design":"Keep application state in its owner. Clarify the ownership boundary without changing the public API."}),
                        },
                        forge_provider_api::testing::TestPromptStep::ToolCall {
                            name: "plan_submit".into(),
                            arguments: json!({"plan_id":plan_id,"expected_version":revised_version}),
                        },
                    ],
                    terminal: forge_provider_api::testing::TestPromptTerminal::Completed,
                },
            }).await?;
        }
        let hold = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"].as_str().unwrap().to_owned();
        let mut decision_params = json!({
            "operation_id":"review-decision",
            "action":if request_changes { "request_changes" } else { "execute" },
            "plan_id":plan_id,"digest":initial.review_digest,
            "saved_source_digest":source.saved_digest,
            "comment":"Preserve the public API.","annotations":annotations,
            "approval_mode":"ask",
        });
        if !request_changes {
            decision_params["plan_compact"] = json!(false);
        }
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 32,
                    method: "session.command.apply".into(),
                    params: decision_params,
                },
                &sink,
            )
            .await?;
        let pending = tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        let revising = decision != "accept-empty";
        assert_eq!(
            pending["mode"],
            serde_json::to_value(if revising {
                crate::provider::PromptMode::Plan
            } else {
                crate::provider::PromptMode::ExecutePlan
            })?
        );
        if revising {
            assert_eq!(
                store.latest_exchange(&session_id)?.unwrap().id,
                original_exchange.id
            );
            assert!(pending["prompt"].as_str().unwrap().contains(feedback));
            let saved = store.load_plan(plan_id)?.unwrap();
            assert_eq!(saved.state, crate::plan::PlanState::Revising);
            assert_eq!(saved.accepted_revision, None);
            if decision == "execute-direct-feedback" {
                assert!(
                    saved.review.is_none(),
                    "direct feedback created an acceptance overlay"
                );
                let receipt = controller
                    .exchange_owner
                    .command_status(crate::exchange::command::CommandId::new("review-decision")?)
                    .await?
                    .context("execute receipt missing")?;
                let crate::exchange::command::Command::Task(
                    crate::exchange::command::TaskCommand::ExecutePlan(selection),
                ) = &receipt.request.command
                else {
                    anyhow::bail!("feedback did not use ExecutePlan");
                };
                assert_eq!(selection.request().annotations[0].body, feedback);
            }
            assert!(
                !controller
                    .task_controller
                    .current()?
                    .unwrap()
                    .auto_accept_plan
            );
            if decision == "accept-off" || decision == "request-on" {
                let before = controller.task_controller.current()?.unwrap();
                service.dispatch(Some(session_id.clone()), Request {
                    id: 34, method: "session.prompt.submit".into(),
                    params: json!({"text":if decision == "accept-off" {"/plan accept off"} else {"/plan accept on"}}),
                }, &sink).await?;
                let after = controller.task_controller.current()?.unwrap();
                assert_eq!(
                    before.revision, after.revision,
                    "policy changes must not invalidate provider work"
                );
                assert_eq!(after.auto_accept_plan, decision == "request-on");
            }
            let next = control
                .dispatch(
                    &session_id,
                    MockResponseCommand::Hold {
                        mode: Some(crate::provider::PromptMode::ExecutePlan),
                        any_session: false,
                    },
                )
                .await?;
            let next_id = next["id"].as_str().unwrap().to_owned();
            control
                .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
                .await?;
            if decision == "request-on" {
                tokio::time::timeout(
                    Duration::from_secs(5),
                    control.dispatch(
                        &session_id,
                        MockResponseCommand::Wait {
                            id: next_id.clone(),
                        },
                    ),
                )
                .await??;
                let accepted = store.load_plan(plan_id)?.unwrap();
                assert_eq!(accepted.accepted_revision, Some(initial.model_revision + 1));
                assert!(
                    controller
                        .task_controller
                        .current()?
                        .unwrap()
                        .auto_accept_plan
                );
                assert!(matches!(
                    controller.task_controller.current()?.unwrap().workflow,
                    crate::task::WorkflowSnapshot::Execute { .. }
                ));
            } else {
                tokio::time::timeout(Duration::from_secs(5), async {
                    loop {
                        let plan = store.load_plan(plan_id)?.unwrap();
                        if plan.state == crate::plan::PlanState::AwaitingReview
                            && controller.coordinator.execution.available_permits() > 0
                        {
                            assert_eq!(plan.accepted_revision, None);
                            assert!(
                                !controller
                                    .task_controller
                                    .current()?
                                    .unwrap()
                                    .auto_accept_plan
                            );
                            break Ok::<(), anyhow::Error>(());
                        }
                        tokio::task::yield_now().await;
                    }
                })
                .await??;
            }
            control
                .dispatch(&session_id, MockResponseCommand::Cancel { id: next_id })
                .await?;
        } else {
            let accepted = store.load_plan(plan_id)?.unwrap();
            assert_eq!(accepted.accepted_revision, Some(initial.model_revision));
            control
                .dispatch(&session_id, MockResponseCommand::Cancel { id: hold_id })
                .await?;
        }
        if let Err(error) = service.shutdown(Duration::from_secs(2)).await {
            let trace_path = controller.trace.session_status(&session_id).path;
            let trace = std::fs::read_to_string(&trace_path).unwrap_or_default();
            let tail = trace
                .lines()
                .rev()
                .take(50)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .collect::<Vec<_>>()
                .join("\n");
            let root = fixture.keep();
            return Err(error.context(format!("inline feedback decision {decision}; retained fixture {}; trace {trace_path}\n{tail}", root.display())));
        }
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn execute_collects_then_clear_preserves_receipt() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    for acceptance in ["transition", "alias", "review"] {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let mut stage = "prepare reviewed plan";
        let scenario: Result<()> = async {
        let mut params =
            reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 32,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"fixture-pause-plan","action":"pause"}),
                },
                &sink,
            )
            .await?;
        stage = "install held chat response";
        let mock_provider = controller.provider_handle();
        let control = mock_provider.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = held["id"].as_str().unwrap().to_owned();
        stage = "submit held chat input";
        controller
            .input_request(
                Request {
                    id: 33,
                    method: "session.input.apply".into(),
                    params: json!({
                        "id":"held-chat", "action":{"kind":"submit","message":{"id":"held-chat",
                        "text":"Inspect startup", "intent":"auto", "attachments":[], "config":null,
                        "reservation":null,"submission_id":null,"error":null}}
                    }),
                },
                RuntimeMethod::SessionInputApply,
                &sink,
            )
            .await?;
        stage = "resume held chat input";
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 99,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"resume-fixture-input", "action":"resume"}),
                },
                &sink,
            )
            .await?;
        // Task Resume restores the plan workflow; input Resume separately releases
        // the queue suspended by Pause. A stale submit control revision cannot do that.
        stage = "resume held chat queue";
        let input_control_revision = controller.coordinator.submissions.snapshot()?.control_revision;
        controller.input_request(
            Request {
                id: 100,
                method: "session.input.apply".into(),
                params: json!({"id":"resume-held-chat-queue", "control_revision":input_control_revision,
                    "action":{"kind":"resume"}}),
            },
            RuntimeMethod::SessionInputApply,
            &sink,
        ).await?;
        let resumed_input = controller.coordinator.submissions
            .operation("resume-held-chat-queue")?
            .context("input resume receipt missing")?;
        assert!(resumed_input.error.is_none(), "input resume rejected: {:?}", resumed_input.error);
        assert!(!resumed_input.suspended, "input resume left delivery suspended");
        stage = "wait for held chat provider response";
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        stage = "prepare acceptance and review decisions";
        let method = if acceptance == "alias" {
            params.as_object_mut().unwrap().remove("action");
            params["approval_mode"] = json!("ask");
            "session.plan.accept"
        } else if acceptance == "review" {
            let mut preparation = params.clone();
            preparation["operation_id"] = json!("prepare-held-review");
            preparation.as_object_mut().unwrap().remove("action");
            let preparation = Request { id: 34, method: "session.plan.accept".into(), params: preparation };
            tokio::time::timeout(Duration::from_secs(1), service.dispatch(
                Some(session_id.clone()), preparation.clone(), &sink,
            )).await??;
            let receipt = controller.exchange_owner.command_status(
                crate::exchange::command::CommandId::new("prepare-held-review")?
            ).await?.context("review preparation receipt missing")?;
            assert!(matches!(receipt.completion(), Some(Ok(crate::exchange::command::CommandResult {
                outcome: crate::exchange::command::CommandOutcome::Task(
                    crate::exchange::command::TaskCommandOutcome::AwaitingInput { .. }
                ), ..
            }))));
            let store = SqliteStore::open(&controller.data_root)?;
            let plan_id = params["plan_id"].as_str().unwrap();
            let plan = store.load_plan(plan_id)?.unwrap();
            let questions = plan.review_questions().context("review questions missing")?;
            let question = questions.question_set.questions.first().context("approval question missing")?;
            let target = json!({"owner":"plan_review", "id":plan_id,
                "set_id":questions.question_set.id, "revision":questions.revision});
            let original_questions = serde_json::to_value(questions)?;
            service.dispatch(Some(session_id.clone()), preparation.clone(), &sink).await?;
            assert_eq!(serde_json::to_value(store.load_plan(plan_id)?.unwrap().review_questions().unwrap())?, original_questions,
                "replaying AwaitingInput duplicated or changed review questions");
            assert!(!*controller.cancellation.subscribe().borrow());
            let before = serde_json::to_value(controller.task_controller.current()?.unwrap())?;
            for (index, (plan_id, digest)) in [
                ("replaced-plan".to_owned(), params["digest"].as_str().unwrap().to_owned()),
                (params["plan_id"].as_str().unwrap().to_owned(), "stale-digest".to_owned()),
            ].into_iter().enumerate() {
                tokio::time::timeout(Duration::from_secs(1), service.dispatch(Some(session_id.clone()),
                    Request { id:50 + u64::try_from(index)?, method:"session.plan.accept".into(), params:json!({
                        "operation_id":format!("stale-review-{index}"), "plan_id":plan_id,
                        "digest":digest, "approval_mode":"ask",
                    }) }, &sink)).await??;
                if let Some(rejected) = controller.exchange_owner.command_status(
                    crate::exchange::command::CommandId::new(format!("stale-review-{index}"))?
                ).await? {
                    assert!(matches!(rejected.completion(), Some(Err(_))), "stale reviewed source must fail");
                }
                assert_eq!(serde_json::to_value(controller.task_controller.current()?.unwrap())?, before);
                assert!(!*controller.cancellation.subscribe().borrow());
                assert_eq!(controller.coordinator.execution.available_permits(), 0);
            }
            controller.exchange_owner.question_events().answer_plan(
                crate::plan::PlanQuestionAnswerRequest::parse(json!({
                    "operation_id":"answer-held-review", "target":target,
                    "question_id":question.id, "response":{"kind":"selected", "option":"ask"},
                }))?
            ).await?;
            assert!(matches!(controller.task_controller.current()?.unwrap().workflow, crate::task::WorkflowSnapshot::Plan { .. }),
                "answering approval questions implicitly started execution");
            service.dispatch(Some(session_id.clone()), preparation, &sink).await?;
            assert!(matches!(controller.task_controller.current()?.unwrap().workflow, crate::task::WorkflowSnapshot::Plan { .. }),
                "replaying AwaitingInput implicitly approved answered questions");
            params = json!({"operation_id":"continue-held-review", "target":target});
            "session.question.continue"
        } else {
            params["approval_mode"] = json!("ask");
            "session.command.apply"
        };
        stage = "install held execution response";
        // Collection retires controls owned by the old invocation. Register the
        // future hold independently; once claimed it belongs to the real executor.
        let execution_control = format!("{session_id}:future-execution");
        let execution_hold = control.dispatch(&execution_control, MockResponseCommand::Hold {
            mode: Some(crate::provider::PromptMode::ExecutePlan), any_session: true,
        }).await?;
        let execution_hold_id = execution_hold["id"].as_str().unwrap().to_owned();
        let request = Request {
            id: 35,
            method: method.into(),
            params,
        };
        stage = "dispatch execution or replay receipt";
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(Some(session_id.clone()), request.clone(), &sink),
        )
        .await??;
        stage = "wait for held execution provider response";
        tokio::time::timeout(Duration::from_secs(1), control.dispatch(
            &execution_control, MockResponseCommand::Wait { id: execution_hold_id },
        )).await??;
        let execution_receipt = controller.exchange_owner.command_status(
            crate::exchange::command::CommandId::new(
                request.params["operation_id"].as_str().context("execution operation id missing")?
            )?
        ).await?.context("execution receipt missing")?;
        let task = controller.task_controller.current()?.context("execution task missing")?;
        anyhow::ensure!(task.kind() == crate::task::TaskKind::Execute,
            "execution did not install Execute task; receipt={execution_receipt:?}; task={task:?}");
        anyhow::ensure!(task.entry_accepted(),
            "execute completed before dispatch confirmation; receipt={execution_receipt:?}");
        stage = "dispatch execution or replay receipt";
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(Some(session_id.clone()), request.clone(), &sink),
        )
        .await??;
        assert_eq!(
            serde_json::to_value(controller.task_controller.current()?.unwrap())?,
            serde_json::to_value(&task)?,
            "repeated acceptance mutated execution"
        );
        stage = "clear accepted execution";
        let entry = task.entry.as_ref().unwrap();
        let mut store = SqliteStore::open(&controller.data_root)?;
        let exchange_count = store.list_exchange(&session_id)?.len();
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 35,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"clear-pending-execution","action":"clear"}),
                },
                &sink,
            )
            .await?;
        assert!(!controller.task_controller.acknowledge_entry(
            &mut store,
            &task.revision,
            &entry.id,
            42
        )?);
        // Clear has collected the owned invocation; its old mock control is retired.
        assert!(controller.task_controller.current()?.is_none());
        stage = "replay execution after clear";
        service.dispatch(Some(session_id.clone()), request, &sink).await?;
        assert!(controller.task_controller.current()?.is_none(), "old execution receipt revived a cleared task");
        assert_eq!(store.list_exchange(&session_id)?.len(), exchange_count);
        stage = "shutdown service";
        service.shutdown(Duration::from_secs(2)).await?;
        stage = "drain outbound reader";
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
        Ok(())
        }.await;
        if let Err(error) = scenario {
            let snapshot = controller
                .observer
                .snapshot()
                .and_then(|snapshot| serde_json::to_value(snapshot).map_err(Into::into));
            let trace = controller.trace.clone();
            let trace_path = trace.session_status(&session_id).path;
            let diagnostic_path = trace_path.clone();
            let tail = tokio::task::spawn_blocking(move || -> Result<String> {
                trace.flush()?;
                let content = std::fs::read_to_string(diagnostic_path)?;
                Ok(content
                    .lines()
                    .rev()
                    .take(120)
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev()
                    .collect::<Vec<_>>()
                    .join("\n"))
            })
            .await;
            let retained_fixture = fixture.keep();
            return Err(error.context(format!(
                "acceptance={acceptance}; stage={stage}; fixture={}; trace_path={}; snapshot={snapshot:?}; trace={tail:?}",
                retained_fixture.display(), trace_path,
            )));
        }
    }
    Ok(())
}

#[tokio::test]
async fn reattaching_current_plan_preserves_active_work_and_rejects_foreign_sources() -> Result<()>
{
    use crate::provider::mock_response::MockResponseCommand;
    for action in ["attach_plan"] {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let reviewed =
            reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
        let plan_id = reviewed["plan_id"].as_str().unwrap().to_owned();
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 41,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"pause-saved-source","action":"pause"}),
                },
                &sink,
            )
            .await?;
        let mock_provider = controller.provider_handle();
        let control = mock_provider.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = held["id"].as_str().unwrap().to_owned();
        controller.input_request(Request {
            id: 42, method: "session.input.apply".into(),
            params: json!({"id":"held-plan-action-chat","action":{"kind":"submit","message":{
                "id":"held-plan-action-chat","text":"Inspect current workspace","intent":"auto",
                "attachments":[],"config":null,"reservation":null,"submission_id":null,"error":null}}}),
        }, RuntimeMethod::SessionInputApply, &sink).await?;
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 99,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"resume-fixture-input", "action":"resume"}),
                },
                &sink,
            )
            .await?;
        let input_control_revision = controller
            .coordinator
            .submissions
            .snapshot()?
            .control_revision;
        controller.input_request(Request {
            id: 100,
            method: "session.input.apply".into(),
            params: json!({"id":"resume-reattach-chat-queue", "control_revision":input_control_revision,
                "action":{"kind":"resume"}}),
        }, RuntimeMethod::SessionInputApply, &sink).await?;
        let resumed_input = controller
            .coordinator
            .submissions
            .operation("resume-reattach-chat-queue")?
            .context("reattach input resume receipt missing")?;
        anyhow::ensure!(
            resumed_input.error.is_none() && !resumed_input.suspended,
            "reattach input resume rejected: {resumed_input:?}"
        );
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await
        .context("reattach: waiting for held chat provider response")??;
        let mut store = SqliteStore::open(&controller.data_root)?;
        let held_exchange = store.latest_exchange(&session_id)?.unwrap();
        let count = store.list_exchange(&session_id)?.len();
        let before = controller.task_controller.current()?.unwrap();
        let child = prepare_new_session(
            &controller.data_root,
            &registry.initialize.client_id,
            "mock",
            false,
            &session_id,
            &json!({"name":"foreign plan owner"}),
            43,
        )?;
        let mut foreign_plan = store.load_plan(&plan_id)?.unwrap();
        foreign_plan.id = uuid::Uuid::new_v4().to_string();
        foreign_plan.session_id = child.id;
        store.save_plan(&foreign_plan)?;
        let failure = controller
            .exchange_owner
            .execute_identified(
                crate::exchange::command::CommandRequest {
                    id: crate::exchange::command::CommandId::new("foreign-current-attachment")?,
                    command: crate::exchange::command::Command::Task(
                        crate::exchange::command::TaskCommand::AttachPlan(
                            crate::exchange::command::PlanSelection::capture(&foreign_plan)?,
                        ),
                    ),
                    input_control: crate::exchange::command::InputControl::AtExecution,
                },
                sink.clone(),
            )
            .await
            .expect_err("foreign plan selection must fail");
        assert_eq!(failure.code(), "plan_foreign_session");
        assert_eq!(
            serde_json::to_value(controller.task_controller.current()?.unwrap())?,
            serde_json::to_value(&before)?
        );
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 44,
                    method: "session.command.apply".into(),
                    params: json!({
                        "operation_id":format!("saved-{action}"),"action":action,"plan_id":plan_id,
                    }),
                },
                &sink,
            ),
        )
        .await
        .context("reattach: applying current plan selection")??;
        let selected = controller.task_controller.current()?.unwrap();
        let crate::task::WorkflowSnapshot::Plan { plan } = &selected.workflow else {
            panic!("saved plan action did not select a Plan task")
        };
        assert_eq!(plan.artifact.id, plan_id);
        assert_eq!(plan.artifact.state, crate::plan::PlanState::AwaitingReview);
        assert!(
            selected
                .entry
                .as_ref()
                .is_none_or(|entry| entry.delivery != crate::task::TaskInputDelivery::Pending)
        );
        assert!(!*controller.cancellation.subscribe().borrow());
        assert_eq!(controller.coordinator.execution.available_permits(), 0);
        assert_eq!(store.list_exchange(&session_id)?.len(), count);
        assert_eq!(
            store.latest_exchange(&session_id)?.unwrap().id,
            held_exchange.id
        );
        assert_eq!(
            store.latest_exchange(&session_id)?.unwrap().state(),
            crate::exchange::ExchangeState::Running
        );
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 45,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"clear-saved-action","action":"clear"}),
                },
                &sink,
            )
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("reattach: waiting for Clear to collect execution")?;
        assert_eq!(store.list_exchange(&session_id)?.len(), count);
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn checking_continuation_returns_to_verify_without_provider_exchange() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    crate::checks::configure_fixture(fixture.path());
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let mut params = reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
    let admission = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let mut store = SqliteStore::open(&controller.data_root)?;
    let plan_id = params["plan_id"].as_str().unwrap();
    let mut plan = store.load_plan(plan_id)?.unwrap();
    let mut document = controller
        .observer
        .plan_file
        .read_working_document(&session_id, plan_id)?;
    document
        .design
        .as_mut()
        .unwrap()
        .document
        .verification
        .push("Inspect the completed result".into());
    controller
        .observer
        .plan_file
        .write_working_document(&session_id, plan_id, &document)?;
    plan.model_revision += 1;
    let (_, _, digest) = controller.observer.plan_file.submit_document_revision(
        &session_id,
        plan_id,
        plan.model_revision,
        document.version,
    )?;
    plan.review_digest = Some(digest.clone());
    let planning = controller.task_controller.current()?.unwrap();
    controller
        .task_controller
        .update(&mut store, &planning.permit(), 99, |task| {
            let crate::task::WorkflowSnapshot::Plan { plan: current } = &mut task.workflow else {
                unreachable!()
            };
            current.artifact = plan;
            Ok(())
        })?;
    params["digest"] = json!(digest);
    let selection = serde_json::from_value(params)?;
    let (mut task, effects) = controller.observer.prepare_execution(&selection, 20, 100)?;
    std::fs::create_dir_all(fixture.path().join("src"))?;
    std::fs::write(
        fixture.path().join("src/owner.rs"),
        "/// Own application state.\npub struct Owner;\n",
    )?;
    task.edit(|draft| {
        draft.entry.as_mut().unwrap().execution = None;
        Ok(())
    })?;
    let current = controller.task_controller.current()?.unwrap();
    controller.task_controller.change(
        &mut store,
        Some(&current.revision),
        crate::task::TaskChange::Install(Box::new(task)),
        100,
    )?;
    for plan in effects.plan {
        store.save_plan(&plan)?;
    }
    let task = controller.task_controller.current()?.unwrap();
    controller.task_controller.acknowledge_entry(
        &mut store,
        &task.revision,
        &task.entry.as_ref().unwrap().id,
        101,
    )?;
    controller
        .broker
        .lock()
        .await
        .ensure_execution_baseline()
        .await?;
    controller
        .task_controller
        .update(&mut store, &task.permit(), 102, |task| {
            let crate::task::WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
                unreachable!()
            };
            execution.record.phase = crate::plan::PlanPhase::Checking;
            Ok(())
        })?;
    let task = controller.task_controller.current()?.unwrap();
    let source = store.latest_exchange(&session_id)?.unwrap();
    let count = store.list_exchange(&session_id)?.len();
    controller.task_controller.reserve_checks(
        &mut store,
        &task.permit(),
        crate::task::TaskTurnId {
            exchange_ordinal: source.ordinal,
            turn_ordinal: source.turn.len(),
        },
        103,
    )?;
    let before = controller.task_controller.current()?.unwrap();
    assert_eq!(
        before
            .turn
            .as_ref()
            .unwrap()
            .continuation
            .as_ref()
            .unwrap()
            .work,
        crate::task::TaskWork::Checks
    );
    let (mut checking, scan) = controller.broker.lock().await.begin_owned_task_checks()?;
    let scanned = scan.run().await?;
    let run = controller
        .broker
        .lock()
        .await
        .prepare_owned_task_check_run(&mut checking, scanned)?;
    let checked = run.run().await?;
    let post = controller
        .broker
        .lock()
        .await
        .prepare_task_check_postscan(&checking)?;
    let scanned = post.run().await?;
    let mut finish = controller
        .broker
        .lock()
        .await
        .prepare_task_check_finalization(checking, checked, scanned)?;
    let report = finish.capture_report().await?;
    let delivery = controller
        .broker
        .lock()
        .await
        .commit_task_check_finalization(finish, report)?;
    let (_, acknowledgment) = delivery.run().await?;
    controller
        .broker
        .lock()
        .await
        .acknowledge_owned_task_check(acknowledgment)?;
    let after = controller.task_controller.current()?.unwrap();
    assert!(
        matches!(&after.workflow, crate::task::WorkflowSnapshot::Execute { execution }
        if execution.record.phase == crate::plan::PlanPhase::Verify),
        "check continuation did not reach Verify: {:?}",
        after.workflow
    );
    let continuation = after.turn.as_ref().unwrap().continuation.as_ref().unwrap();
    assert!(matches!(
        continuation.work,
        crate::task::TaskWork::Provider { .. }
    ));
    assert!(continuation.work.instruction().unwrap().contains("Verify"));
    assert_eq!(store.list_exchange(&session_id)?.len(), count);
    controller.task_controller.change(
        &mut store,
        Some(&after.revision),
        crate::task::TaskChange::Clear,
        104,
    )?;
    drop(admission);
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    Ok(())
}

#[tokio::test]
async fn task_creation_completes_at_dispatch_and_replay_preserves_replacement() -> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandOutcome, CommandRequest, InputControl, NewGoal, NewPlan,
        TaskCommand, TaskCommandOutcome,
    };
    use crate::provider::mock_response::MockResponseCommand;
    for kind in [crate::task::TaskKind::Goal, crate::task::TaskKind::Plan] {
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
            let completed = tokio::time::timeout(
                Duration::from_secs(5),
                controller
                    .exchange_owner
                    .execute_identified(request.clone(), sink.clone()),
            )
            .await?
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
            tokio::time::timeout(
                Duration::from_secs(5),
                script.dispatch(&session_id, MockResponseCommand::Wait { id: hold }),
            )
            .await??;
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
        let replay = tokio::time::timeout(
            Duration::from_secs(5),
            controller
                .exchange_owner
                .execute_identified(request, sink.clone()),
        )
        .await??;
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
        tokio::time::timeout(
            Duration::from_secs(5),
            controller
                .exchange_owner
                .execute_command(Command::CancelExchange(Default::default())),
        )
        .await??;
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

/// Working forks and submitted replans dispatch once, preserve their source files, and replay after replacement.
#[tokio::test]
async fn saved_plan_startup_uses_captured_sources_and_confirms_dispatch() -> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandOutcome, CommandRequest, InputControl, PlanRevisionSelection,
        PlanSelection, TaskCommand, TaskCommandOutcome,
    };
    use crate::provider::mock_response::MockResponseCommand;
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
    let reviewed = reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
    let plan_id = reviewed["plan_id"].as_str().unwrap();
    let store = SqliteStore::read_only(&controller.data_root)?;
    let source = store.load_plan(plan_id)?.unwrap();
    let selected = PlanSelection::capture(&source)?;
    let document = controller
        .observer
        .plan_file
        .read_working_document(&session_id, plan_id)?;
    let provider = controller.provider_handle();
    let script = provider.mock_response_control().unwrap();
    let mut first = None;
    for (index, intent) in [
        TaskCommand::ForkPlan(selected.clone()),
        TaskCommand::Replan(PlanRevisionSelection::new(selected, 1)?),
    ]
    .into_iter()
    .enumerate()
    {
        // Keep the future hold outside the root generation being collected by this command.
        // Once claimed, the mock records the actual root owner for scoped cancellation.
        let hold_owner = format!("{session_id}-saved-startup-{index}");
        let held = script
            .dispatch(
                &hold_owner,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: true,
                },
            )
            .await?;
        let hold = held["id"].as_str().unwrap().to_owned();
        let request = CommandRequest {
            id: CommandId::new(format!("saved-source-{index}"))?,
            command: Command::Task(intent),
            input_control: InputControl::AtExecution,
        };
        let result = tokio::time::timeout(
            Duration::from_secs(5),
            controller
                .exchange_owner
                .execute_identified(request.clone(), sink.clone()),
        )
        .await
        .map_err(anyhow::Error::from)
        .and_then(|result| result.map_err(anyhow::Error::from));
        let result = match result {
            Ok(result) => result,
            Err(error) => {
                controller.trace.flush()?;
                let path = controller.trace.session_status(&session_id).path;
                let content = std::fs::read_to_string(&path)?;
                let tail = content
                    .lines()
                    .rev()
                    .take(100)
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev()
                    .collect::<Vec<_>>()
                    .join("\n");
                let retained = root.keep();
                return Err(error.context(format!(
                    "saved-plan startup index {index}; fixture={}; trace={path}\n{tail}",
                    retained.display()
                )));
            }
        };
        let CommandOutcome::Task(TaskCommandOutcome::Started { task, input, .. }) = &result.outcome
        else {
            anyhow::bail!("saved-plan startup did not confirm its handoff")
        };
        tokio::time::timeout(
            Duration::from_secs(5),
            script.dispatch(&hold_owner, MockResponseCommand::Wait { id: hold }),
        )
        .await??;
        let current = controller.task_controller.current()?.unwrap();
        assert_eq!(&current.id, task.id());
        assert_eq!(&current.entry.as_ref().unwrap().id, input);
        let crate::task::WorkflowSnapshot::Plan { plan } = &current.workflow else {
            anyhow::bail!("saved-plan action installed a different task kind")
        };
        assert_ne!(plan.artifact.id, plan_id);
        assert_eq!(plan.artifact.state, crate::plan::PlanState::Generating);
        assert_eq!(
            serde_json::to_value(
                controller
                    .observer
                    .plan_file
                    .read_working_document(&session_id, plan_id)?
            )?,
            serde_json::to_value(&document)?
        );
        assert!(!store.latest_exchange(&session_id)?.unwrap().is_finished());
        assert!(Arc::ptr_eq(&provider, &controller.provider_handle()));
        if first.is_none() {
            first = Some((request, result));
        }
    }
    let before = controller.task_controller.current()?.unwrap();
    let (request, first_result) = first.unwrap();
    let replay = controller
        .exchange_owner
        .execute_identified(request, sink.clone())
        .await?;
    assert_eq!(replay.sequence, first_result.sequence);
    assert_eq!(
        controller.task_controller.current()?.unwrap().revision,
        before.revision
    );
    assert_eq!(store.list_exchange(&session_id)?.len(), 3);
    tokio::time::timeout(
        Duration::from_secs(5),
        controller
            .exchange_owner
            .execute_command(Command::CancelExchange(Default::default())),
    )
    .await??;
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    Ok(())
}

/// Attach an unfinished saved design through dispatch, then confirm current ownership without another invocation.
#[tokio::test]
async fn unfinished_plan_attachment_dispatches_once_and_reattach_keeps_running_work() -> Result<()>
{
    use crate::exchange::command::{
        Command, CommandId, CommandOutcome, CommandRequest, InputControl, PlanSelection,
        TaskCommand, TaskCommandOutcome, TaskSelection,
    };
    use crate::provider::mock_response::MockResponseCommand;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let prepared = controller.observer.plan_file.prepare_entry(
        &session_id,
        "Finish the saved design".into(),
        None,
        1,
    )?;
    SqliteStore::open(&controller.data_root)?.save_plan(&prepared.record)?;
    let source = PlanSelection::capture(&prepared.record)?;
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
    let hold = held["id"].as_str().unwrap().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let result = tokio::time::timeout(
        Duration::from_secs(5),
        controller.exchange_owner.execute_identified(
            CommandRequest {
                id: CommandId::new("attach-unfinished-plan")?,
                command: Command::Task(TaskCommand::AttachPlan(source)),
                input_control: InputControl::AtExecution,
            },
            sink.clone(),
        ),
    )
    .await??;
    let CommandOutcome::Task(TaskCommandOutcome::Started {
        task, input, scope, ..
    }) = result.outcome
    else {
        anyhow::bail!("unfinished plan attachment did not confirm its model handoff")
    };
    tokio::time::timeout(
        Duration::from_secs(5),
        script.dispatch(&session_id, MockResponseCommand::Wait { id: hold }),
    )
    .await??;
    let current = controller.task_controller.current()?.unwrap();
    let crate::task::WorkflowSnapshot::Plan { plan } = &current.workflow else {
        anyhow::bail!("attachment changed workflow kind")
    };
    assert_eq!(plan.artifact.id, prepared.record.id);
    assert_eq!(&current.id, task.id());
    assert_eq!(&current.entry.as_ref().unwrap().id, &input);
    let current_source = PlanSelection::capture(
        &SqliteStore::read_only(&controller.data_root)?
            .load_plan(&prepared.record.id)?
            .unwrap(),
    )?;
    let confirmation = tokio::time::timeout(
        Duration::from_secs(5),
        controller.exchange_owner.execute_identified(
            CommandRequest {
                id: CommandId::new("confirm-running-attachment")?,
                command: Command::Task(TaskCommand::AttachPlan(current_source)),
                input_control: InputControl::AtExecution,
            },
            sink.clone(),
        ),
    )
    .await??;
    assert!(matches!(
        confirmation.outcome,
        CommandOutcome::Task(TaskCommandOutcome::Attached { .. })
    ));
    let still_current = controller.task_controller.current()?.unwrap();
    assert_eq!(still_current.id, current.id);
    assert_eq!(still_current.entry.as_ref().unwrap().id, input);
    let store = SqliteStore::read_only(&controller.data_root)?;
    assert_eq!(store.list_exchange(&session_id)?.len(), 1);
    assert_eq!(
        store.latest_exchange(&session_id)?.unwrap().id.as_str(),
        scope.exchange_id.as_str()
    );
    assert!(!store.latest_exchange(&session_id)?.unwrap().is_finished());
    assert!(Arc::ptr_eq(&provider, &controller.provider_handle()));
    tokio::time::timeout(
        Duration::from_secs(5),
        controller
            .exchange_owner
            .execute_command(Command::Task(TaskCommand::Clear(TaskSelection::capture(
                &still_current,
            )))),
    )
    .await??;
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
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
    let (paused, accepted) = tokio::time::timeout(Duration::from_secs(5), async {
        tokio::join!(replay, following)
    })
    .await?;
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
