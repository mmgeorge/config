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
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_transition_pause()
-> Result<()> {
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("transition", "pause")
        .await
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_transition_clear()
-> Result<()> {
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("transition", "clear")
        .await
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_rpc_pause() -> Result<()>
{
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("rpc", "pause").await
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_rpc_clear() -> Result<()>
{
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("rpc", "clear").await
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_prompt_pause()
-> Result<()> {
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("prompt", "pause").await
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_prompt_clear()
-> Result<()> {
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("prompt", "clear").await
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_input_pause()
-> Result<()> {
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("input", "pause").await
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_input_clear()
-> Result<()> {
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("input", "clear").await
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_steer_pause()
-> Result<()> {
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("steer", "pause").await
}

#[tokio::test]
async fn task_control_aliases_collect_active_work_and_replay_owner_receipts_steer_clear()
-> Result<()> {
    task_control_aliases_collect_active_work_and_replay_owner_receipts_case("steer", "clear").await
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
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        script.dispatch(&session_id, MockResponseCommand::Wait { id: hold_id }),
    )
    .await??;
    let store = SqliteStore::open(&controller.data_root)?;
    let original = store.latest_exchange(&session_id)?.unwrap().id;
    let request = goal_command_request(transport, action, 10);
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        service.dispatch(Some(session_id.clone()), request.clone(), &sink),
    )
    .await??;
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;

    Ok(())
}

#[tokio::test]
async fn goal_continue_aliases_reuse_the_completed_source_and_receipt_transition() -> Result<()> {
    goal_continue_aliases_reuse_the_completed_source_and_receipt_case("transition").await
}

#[tokio::test]
async fn goal_continue_aliases_reuse_the_completed_source_and_receipt_prompt() -> Result<()> {
    goal_continue_aliases_reuse_the_completed_source_and_receipt_case("prompt").await
}

#[tokio::test]
async fn goal_continue_aliases_reuse_the_completed_source_and_receipt_input() -> Result<()> {
    goal_continue_aliases_reuse_the_completed_source_and_receipt_case("input").await
}

/// Run one transport variant of the scenario described above, each in its own test so a
/// variant reports independently and stays within the per-test time limit.
async fn goal_continue_aliases_reuse_the_completed_source_and_receipt_case(
    transport: &str,
) -> Result<()> {
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
    let source = tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
        crate::hang_guard::HANG_GUARD,
        service.dispatch(Some(session_id.clone()), request.clone(), &sink),
    )
    .await??;
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
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
        crate::hang_guard::HANG_GUARD,
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while controller.coordinator.execution.available_permits() == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert_eq!(store.list_exchange(&session_id)?.len(), 2);
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;

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
        crate::hang_guard::HANG_GUARD,
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
        crate::hang_guard::HANG_GUARD,
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while controller.coordinator.execution.available_permits() == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert_eq!(store.list_exchange(&session_id)?.len(), 2);
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;
    Ok(())
}

#[tokio::test]
async fn plan_replacement_collects_prior_work_before_initial_dispatch_queued_input() -> Result<()> {
    plan_replacement_collects_prior_work_before_initial_dispatch_case(false).await
}

#[tokio::test]
async fn plan_replacement_collects_prior_work_before_initial_dispatch_planning() -> Result<()> {
    plan_replacement_collects_prior_work_before_initial_dispatch_case(true).await
}

async fn plan_replacement_collects_prior_work_before_initial_dispatch_case(
    planning: bool,
) -> Result<()> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    {
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
            crate::hang_guard::HANG_GUARD,
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
            crate::hang_guard::HANG_GUARD,
            service.dispatch(Some(session_id.clone()), request.clone(), &sink),
        )
        .await??;
        let pending = controller.task_controller.current()?.unwrap();
        assert_eq!(pending.title(), "Replacement plan");
        assert!(!*controller.cancellation.subscribe().borrow());
        assert_eq!(store.list_exchange(&session_id)?.len(), 2);
        assert_ne!(store.latest_exchange(&session_id)?.unwrap().id, original.id);
        tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
        tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;
    }
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
        crate::hang_guard::HANG_GUARD,
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
        crate::hang_guard::HANG_GUARD,
        service.dispatch(Some(session_id.clone()), request, &sink),
    )
    .await??;
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        control.dispatch(&session_id, MockResponseCommand::Wait { id: goal_hold_id }),
    )
    .await??;
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
    let exchanges = store.list_exchange(&session_id)?;
    assert_eq!(exchanges.len(), 2);
    assert_eq!(exchanges[0].id, original.id);
    assert_ne!(exchanges[1].id, original.id);
    assert_eq!(
        exchanges[1].state(),
        crate::exchange::ExchangeState::Running
    );
    // The goal entry starts its own exchange after collecting prior work, so the exchange
    // origin records the task start; a task-start session event exists only for steered entry.
    assert!(
        matches!(&exchanges[1].origin, crate::exchange::ExchangeOrigin::TaskStarted { start }
        if start.title == task.title())
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
                    start: Some(crate::exchange::TaskStart {
                        kind: task.kind(),
                        title: task.title().into(),
                    }),
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while controller.coordinator.execution.available_permits() == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert!(controller.task_controller.current()?.is_none());
    assert_eq!(store.list_exchange(&session_id)?.len(), 3);
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader)
        .await
        .context("shutdown retained a session subscription sender")???;
    Ok(())
}

/// Clearing a task closes the exchange its collection parked, because no admission follows.
#[tokio::test]
async fn clearing_running_goal_closes_its_parked_exchange() -> Result<()> {
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
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
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
    let request = Request {
        id: 2,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"held-goal","action":"goal","text":"Hold the goal entry"}),
    };
    service.prepare_task(Some(&session_id), &request).await?;
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        service.dispatch(Some(session_id.clone()), request, &sink),
    )
    .await??;
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        control.dispatch(&session_id, MockResponseCommand::Wait { id: hold_id }),
    )
    .await??;
    let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let entry = store
        .latest_exchange(&session_id)?
        .context("goal entry exchange missing")?;
    assert_eq!(entry.state(), crate::exchange::ExchangeState::Running);
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 3,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"clear-held-goal","action":"clear"}),
            },
            &sink,
        )
        .await?;
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while controller.coordinator.execution.available_permits() == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert!(controller.task_controller.current()?.is_none());
    // Task controls park collected work for a later admission; clearing schedules none.
    let settled = store
        .load_exchange(&entry.id)?
        .context("cleared goal exchange disappeared")?;
    assert!(
        settled.is_finished(),
        "cleared goal left its exchange {:?}",
        settled.state()
    );
    assert!(
        store
            .list_exchange(&session_id)?
            .iter()
            .all(|exchange| exchange.state() != crate::exchange::ExchangeState::Running)
    );
    // The transcript must leave "Saving exchange" without waiting for an unrelated command.
    let publications =
        crate::exchange::SessionPublications::open(&controller.data_root, &session_id)?;
    let publication_limit = std::num::NonZeroU16::new(u16::MAX).unwrap();
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            let last_status = publications
                .after(None, publication_limit)?
                .iter()
                .rev()
                .find(|publication| {
                    publication.event().event == "document_changed"
                        && publication.event().payload.get("status").is_some()
                })
                .map(|publication| publication.event().payload["status"]["kind"].clone());
            if last_status.is_some_and(|kind| kind != "finalizing") {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await
    .context("clearing the goal left its published document finalizing")??;
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader)
        .await
        .context("shutdown retained a session subscription sender")???;
    Ok(())
}

/// Clearing a task settles an exchange its collection left finalizing.
///
/// Parked collection defers the final checkpoint to the next admission, and Clear schedules
/// none. The exchange must finish, and its document must stop showing "Saving exchange",
/// before the clear reports completion rather than when unrelated input next arrives.
#[tokio::test]
async fn clearing_task_settles_its_finalizing_exchange() -> Result<()> {
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
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
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
    let request = Request {
        id: 2,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"finalizing-goal","action":"goal","text":"Hold the goal entry"}),
    };
    service.prepare_task(Some(&session_id), &request).await?;
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        service.dispatch(Some(session_id.clone()), request, &sink),
    )
    .await??;
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        control.dispatch(&session_id, MockResponseCommand::Wait { id: hold_id }),
    )
    .await??;
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        service.dispatch(
            Some(session_id.clone()),
            Request {
                id: 3,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"pause-finalizing-goal","action":"pause"}),
            },
            &sink,
        ),
    )
    .await??;
    let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let mut parked = store
        .latest_exchange(&session_id)?
        .context("goal entry exchange missing")?;
    assert_eq!(parked.state(), crate::exchange::ExchangeState::Running);
    // Collection whose provider turn already completed leaves the exchange finalizing.
    parked.begin_finalization(crate::exchange::ExchangeOutcome::Complete, 1)?;
    store.save_exchange(&parked)?;
    let publications =
        crate::exchange::SessionPublications::open(&controller.data_root, &session_id)?;
    let publication_limit = std::num::NonZeroU16::new(u16::MAX).unwrap();
    let before_clear = publications
        .after(None, publication_limit)?
        .last()
        .map(|publication| publication.sequence());
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        service.dispatch(
            Some(session_id.clone()),
            Request {
                id: 4,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"clear-finalizing-goal","action":"clear"}),
            },
            &sink,
        ),
    )
    .await??;
    assert!(controller.task_controller.current()?.is_none());
    let settled = store
        .load_exchange(&parked.id)?
        .context("cleared goal exchange disappeared")?;
    assert!(
        settled.is_finished(),
        "clear left the collected exchange {:?}",
        settled.state()
    );
    let cleared = publications.after(before_clear, publication_limit)?;
    assert!(
        cleared.iter().any(|publication| {
            matches!(
                publication.event().event.as_str(),
                "exchange_complete" | "exchange_updated"
            ) && publication.event().payload["id"] == parked.id.as_str()
        }),
        "clear never published the settled exchange"
    );
    let last_status = cleared
        .iter()
        .rev()
        .find(|publication| {
            publication.event().event == "document_changed"
                && publication.event().payload.get("status").is_some()
        })
        .map(|publication| publication.event().payload["status"]["kind"].clone());
    assert_ne!(
        last_status,
        Some(json!("finalizing")),
        "clear left the published document finalizing"
    );
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader)
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
    // Plan notifications use the `refresh` route: the editor re-reads session state to open the
    // review. Outbound delivery trails the committed publication stream, so a refresh can observe
    // later work such as the scripted overview edit. The invariant is therefore revision-based:
    // every refresh must expose at least the plan revision its notification announced, which a
    // notification published before its commit cannot satisfy.
    let delivered_plan_notification = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let reader_plan_notification = delivered_plan_notification.clone();
    let observer = controller.observer.clone();
    let reader = tokio::spawn(async move {
        let mut stale_refresh = Vec::new();
        while let Some(frame) = output.recv().await? {
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] == "plan_created" || event["event"] == "plan_revision_created" {
                let notified_revision = event["payload"]["plan"]["model_revision"]
                    .as_u64()
                    .context("plan notification omitted its model revision")?;
                let snapshot = observer.snapshot()?;
                let refreshed_revision =
                    snapshot
                        .session
                        .active_task
                        .as_ref()
                        .and_then(|task| match &task.workflow {
                            crate::task::WorkflowSnapshot::Plan { plan } => {
                                Some((plan.artifact.state, u64::from(plan.artifact.model_revision)))
                            }
                            _ => None,
                        });
                if refreshed_revision.is_none_or(|(_, revision)| revision < notified_revision) {
                    stale_refresh.push(format!(
                        "{} announced revision {notified_revision}; refresh saw {refreshed_revision:?}",
                        event["event"]
                    ));
                }
                reader_plan_notification.fetch_add(1, Ordering::AcqRel);
            }
        }
        Ok::<Vec<String>, anyhow::Error>(stale_refresh)
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
        crate::storage::state::directory(&controller.data_root)
            .join("provider-submissions.sqlite3"),
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
    // Shutdown does not drain committed publications to a live sink; they stay retained for the
    // next attachment. Await delivery of both submissions' notifications so the refresh checks
    // cover the initial review and the resubmitted revision.
    let delivered = tokio::time::timeout(Duration::from_secs(5), async {
        while delivered_plan_notification.load(Ordering::Acquire) < 2 {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await;
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    let stale_refresh = tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;
    if delivered.is_err() || !stale_refresh.is_empty() {
        let retained_fixture = fixture.keep();
        anyhow::bail!(
            "plan notification refresh did not expose its committed revision; fixture={}; trace={retained_trace}; native_receipt={native_receipt}; delivered={}; stale={stale_refresh:?}",
            retained_fixture.display(),
            delivered_plan_notification.load(Ordering::Acquire),
        );
    }
    Ok(())
}

/// Apply live policy changes to the first full submission without replacing provider work.
#[tokio::test]
async fn plan_accept_policy_controls_initial_submission_default() -> Result<()> {
    plan_accept_policy_controls_initial_submission_case("default").await
}

#[tokio::test]
async fn plan_accept_policy_controls_initial_submission_off() -> Result<()> {
    plan_accept_policy_controls_initial_submission_case("off").await
}

/// Run one policy variant of the scenario described above, each in its own test so a
/// variant reports independently and stays within the per-test time limit.
async fn plan_accept_policy_controls_initial_submission_case(policy: &str) -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
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
        crate::hang_guard::HANG_GUARD,
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
            crate::hang_guard::HANG_GUARD,
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
        tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;

    Ok(())
}

/// Keep feedback in its exchange and honor task policy changes before revised submission.
#[tokio::test]
async fn accepted_inline_feedback_revises_before_execution_without_changing_policy_accept_feedback()
-> Result<()> {
    accepted_inline_feedback_revises_before_execution_without_changing_policy_case(
        "accept-feedback",
    )
    .await
}

#[tokio::test]
async fn accepted_inline_feedback_revises_before_execution_without_changing_policy_execute_direct_feedback()
-> Result<()> {
    accepted_inline_feedback_revises_before_execution_without_changing_policy_case(
        "execute-direct-feedback",
    )
    .await
}

#[tokio::test]
async fn accepted_inline_feedback_revises_before_execution_without_changing_policy_accept_empty()
-> Result<()> {
    accepted_inline_feedback_revises_before_execution_without_changing_policy_case("accept-empty")
        .await
}

#[tokio::test]
async fn accepted_inline_feedback_revises_before_execution_without_changing_policy_request_changes()
-> Result<()> {
    accepted_inline_feedback_revises_before_execution_without_changing_policy_case(
        "request-changes",
    )
    .await
}

#[tokio::test]
async fn accepted_inline_feedback_revises_before_execution_without_changing_policy_accept_off()
-> Result<()> {
    accepted_inline_feedback_revises_before_execution_without_changing_policy_case("accept-off")
        .await
}

/// Run one decision variant of the scenario described above, each in its own test so a
/// variant reports independently and stays within the per-test time limit.
async fn accepted_inline_feedback_revises_before_execution_without_changing_policy_case(
    decision: &str,
) -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
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
    let selection = reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
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
        crate::hang_guard::HANG_GUARD,
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
        serde_json::to_value(
            if revising {
                crate::provider::PromptMode::Plan
            } else {
                crate::provider::PromptMode::ExecutePlan
            }
            .execution()
        )?
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
                crate::hang_guard::HANG_GUARD,
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
            tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
        return Err(error.context(format!(
            "inline feedback decision {decision}; retained fixture {}; trace {trace_path}\n{tail}",
            root.display()
        )));
    }
    drop(sink);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;

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
            crate::hang_guard::HANG_GUARD,
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
            crate::hang_guard::HANG_GUARD,
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
        tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("reattach: waiting for Clear to collect execution")?;
        assert_eq!(store.list_exchange(&session_id)?.len(), count);
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;
    Ok(())
}

#[tokio::test]
async fn task_creation_completes_at_dispatch_and_replay_preserves_replacement_goal() -> Result<()> {
    task_creation_completes_at_dispatch_and_replay_preserves_replacement_case(
        crate::task::TaskKind::Goal,
    )
    .await
}

#[tokio::test]
async fn task_creation_completes_at_dispatch_and_replay_preserves_replacement_plan() -> Result<()> {
    task_creation_completes_at_dispatch_and_replay_preserves_replacement_case(
        crate::task::TaskKind::Plan,
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
            let completed = tokio::time::timeout(
                crate::hang_guard::HANG_GUARD,
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
                crate::hang_guard::HANG_GUARD,
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
            crate::hang_guard::HANG_GUARD,
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
            crate::hang_guard::HANG_GUARD,
            controller
                .exchange_owner
                .execute_command(Command::CancelExchange(Default::default())),
        )
        .await??;
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;
    }
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
        crate::hang_guard::HANG_GUARD,
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
        crate::hang_guard::HANG_GUARD,
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
        crate::hang_guard::HANG_GUARD,
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
        crate::hang_guard::HANG_GUARD,
        controller
            .exchange_owner
            .execute_command(Command::Task(TaskCommand::Clear(TaskSelection::capture(
                &still_current,
            )))),
    )
    .await??;
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, reader).await???;
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
    let (paused, accepted) = tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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

/// Conversation-only undo reaches the owner through the same alias set at admission and dispatch.
#[tokio::test]
async fn task_admission_accepts_conversation_only_undo() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let session_id = service.registry().await?.initial_session_id().to_owned();
    let undo = Request {
        id: 2,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"conversation-undo","action":"undo","rollback":{
            "scope":"conversation","exchange_id":"exchange","latest_exchange_id":"exchange"}}),
    };
    service.prepare_task(Some(&session_id), &undo).await?;
    let unknown = Request {
        id: 3,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"unknown","action":"unknown-action"}),
    };
    assert!(
        service
            .prepare_task(Some(&session_id), &unknown)
            .await
            .is_err()
    );
    Ok(())
}
