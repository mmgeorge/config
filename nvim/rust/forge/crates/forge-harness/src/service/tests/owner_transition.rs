use super::super::{HarnessBroker, SessionController, prepare_new_session, prompt};
use super::super::{native_permission, native_settings};
use super::bootstrap::initialize;
use super::owner_fixture::owner_transition_fixture;
use crate::storage::SqliteStore;
use anyhow::{Context, Result, ensure};
use forge_protocol::message::Request;
use serde_json::Value;
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Duration;

/// Observe independent FIFO configuration completions before queued pause collects the replacement.
#[tokio::test]
async fn owner_model_changes_precede_pause_and_preserve_transcript() -> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandRequest, InputControl, TaskCommand, TaskSelection,
    };
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    let script = fixture.provider.mock_response_control().unwrap();
    for _ in 0..2 {
        script
            .dispatch(
                &fixture.session,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
    }
    let request = |id: &str, model: &str| {
        native_settings::decode_command(&Request {
            id: 3,
            method: "session.command.apply".into(),
            params: json!({"operation_id":id,"action":"configure","config":{"model":model}}),
        })
    };
    let mut first = Box::pin(fixture.controller.exchange_owner.execute_identified(
        request("owner-model-b", "mock-model")?,
        fixture.sink.clone(),
    ));
    assert!(futures_util::poll!(first.as_mut()).is_pending());
    let mut second = Box::pin(
        fixture
            .controller
            .exchange_owner
            .execute_identified(request("owner-model-c", "default")?, fixture.sink.clone()),
    );
    assert!(futures_util::poll!(second.as_mut()).is_pending());
    let pause = CommandRequest {
        id: CommandId::new("owner-after-models-pause")?,
        command: Command::Task(TaskCommand::Pause(TaskSelection::capture(
            &fixture.controller.task_controller.current()?.unwrap(),
        ))),
        input_control: InputControl::AtExecution,
    };
    let mut paused = Box::pin(
        fixture
            .controller
            .exchange_owner
            .execute_identified(pause, fixture.sink.clone()),
    );
    assert!(futures_util::poll!(paused.as_mut()).is_pending());
    let (first, second, paused) = tokio::join!(first, second, paused);
    let (first, second, paused) = (first?, second?, paused?);
    assert!(first.sequence < second.sequence && second.sequence < paused.sequence);
    assert_eq!(
        fixture.controller.observer.snapshot()?.session.model,
        "default"
    );
    assert_eq!(
        fixture
            .controller
            .task_controller
            .current()?
            .unwrap()
            .status(),
        crate::task::TaskStatus::Paused
    );
    assert!(fixture.engine.active_invocation_snapshot()?.is_none());
    let store = SqliteStore::read_only(&fixture.controller.data_root)?;
    let original = store.load_exchange(&fixture.original)?.unwrap();
    assert!(!original.turn.is_empty());
    assert!(original.prompt.contains("Keep this transcript"));
    assert_eq!(
        store.latest_exchange(&fixture.session)?.unwrap().id,
        fixture.original
    );
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}

/// A fresh conversation must accept new input after cancellation proves its first input was unsent.
#[tokio::test]
async fn owner_cancel_before_first_acceptance_allows_immediate_new_input() -> Result<()> {
    use crate::exchange::command::{CancelExchangeRequest, CancelledInputPolicy, Command};
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let (prepared, mut preparation) = tokio::sync::mpsc::unbounded_channel();
    let (dispatched, mut dispatches) = tokio::sync::mpsc::unbounded_channel();
    let owner: Arc<std::sync::Mutex<std::sync::Weak<SessionController>>> = Arc::default();
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, _cancelled| {
            let dispatched = dispatched.clone();
            async move {
                let address = crate::provider::TurnAddress {
                    scope: request.scope.clone(),
                    thread_id: request.thread_id.clone(),
                    turn_id: request.turn_id.clone(),
                };
                start.observed(1).await?;

                let _ = dispatched.send(request.input.clone());
                for event in crate::provider::test_support::message(
                    &address,
                    "reply",
                    Some(crate::provider::TestMessageContent::Response),
                    "New input completed",
                ) {
                    sink.send_wait(crate::provider::test_support::activity(event)?)
                        .await?;
                }

                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                ))
            }
        },
    );
    native.with_models(vec![serde_json::from_value(json!({
        "id":"fixture-model", "reasoning":["low","medium","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?]);
    let preparations = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    native.with_pre_dispatch(move |_| {
        let first = preparations.fetch_add(1, Ordering::AcqRel) == 0;
        let prepared = prepared.clone();
        async move {
            if first {
                let _ = prepared.send(());
                return std::future::pending().await;
            }
            Ok(())
        }
    });
    let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider.provider_handle());
    let controller = SessionController::new(broker).await?;
    *owner.lock().unwrap() = Arc::downgrade(&controller);
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let scenario = async {
        controller.start_provider(&sink).await;
        controller.attach_input(&sink).await?;
        prompt::apply(&controller, Request {
            id: 700, method: "session.prompt.submit".into(),
            params: json!({"operation_id":"first-before-acceptance","text":"First prompt"}),
        }, &sink).await?;
        preparation.recv().await
            .context("first input never reached pre-acceptance preparation")?;
        controller.exchange_owner.execute_command(Command::CancelExchange(CancelExchangeRequest {
            input: CancelledInputPolicy::RestoreIfUnconsumed,
        })).await?;
        prompt::apply(&controller, Request {
            id: 701, method: "session.prompt.submit".into(),
            params: json!({"operation_id":"new-after-early-cancel","text":"New prompt after cancellation"}),
        }, &sink).await?;
        let received = dispatches.recv().await;
        assert!(matches!(received, Some(forge_provider_api::input::PromptInput::Text(text))
            if text == "New prompt after cancellation"));
        Ok::<(), anyhow::Error>(())
    }.await;
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    controller.finish_exchange_owner().await?;
    provider.provider_handle().shutdown().await?;
    controller.finish_session_events().await?;
    reader.abort();
    scenario
}

/// Two queued model changes must finish locally before cancellation settles their retained replacement.
#[tokio::test]
async fn owner_two_model_changes_then_cancel_settles_latest_retained_work_once() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, CancellationOutcome, Command, CommandId, CommandOutcome,
        CommandRequest, ConfigurationOutcome, InputControl,
    };
    use crate::exchange::journal::CommandState;
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    fixture.controller.trace.configure(true)?;
    let ids = [
        "owner-cancel-model-b",
        "owner-cancel-model-c",
        "owner-cancel-after-models",
    ];
    let scenario = async {
        let script = fixture.provider.mock_response_control().context("mock control missing")?;
        for _ in 0..2 {
            script.dispatch(&fixture.session, MockResponseCommand::Hold { mode: None, any_session: false }).await?;
        }
        let request = |id: &str, model: &str| native_settings::decode_command(&Request {
            id: 42,
            method: "session.command.apply".into(),
            params: json!({"operation_id":id,"action":"configure","config":{"model":model}}),
        });
        let barrier = fixture.controller.coordinator.control.clone().acquire_owned().await?;
        let mut first = Box::pin(fixture.controller.exchange_owner.execute_identified(request(ids[0], "mock-model")?, fixture.sink.clone()));
        assert!(futures_util::poll!(first.as_mut()).is_pending());
        let mut second = Box::pin(fixture.controller.exchange_owner.execute_identified(request(ids[1], "default")?, fixture.sink.clone()));
        assert!(futures_util::poll!(second.as_mut()).is_pending());
        let mut cancellation = Box::pin(fixture.controller.exchange_owner.execute_identified(CommandRequest {
            id: CommandId::new(ids[2])?,
            command: Command::CancelExchange(CancelExchangeRequest::default()),
            input_control: InputControl::AtExecution,
        }, fixture.sink.clone()));
        assert!(futures_util::poll!(cancellation.as_mut()).is_pending());
        async {
            loop {
                let first = fixture.controller.exchange_owner.command_status(CommandId::new(ids[0])?).await?;
                let second = fixture.controller.exchange_owner.command_status(CommandId::new(ids[1])?).await?;
                let cancellation = fixture.controller.exchange_owner.command_status(CommandId::new(ids[2])?).await?;
                if matches!(first.as_ref().map(|record| &record.state), Some(CommandState::Active { .. }))
                    && matches!(second.as_ref().map(|record| &record.state), Some(CommandState::Queued { .. }))
                    && matches!(cancellation.as_ref().map(|record| &record.state), Some(CommandState::Queued { .. })) {
                    break Ok::<_, anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        }.await?;
        drop(barrier);
        let (first, second, cancellation) = async {
            tokio::join!(first, second, cancellation)
        }.await;
        let (first, second, cancellation) = (first?, second?, cancellation?);
        ensure!(first.sequence < second.sequence && second.sequence < cancellation.sequence, "configuration and cancel completion receipts lost FIFO order");
        ensure!(matches!(first.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings { .. })), "first model change lost its own result");
        ensure!(matches!(second.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings { .. })), "second model change lost its own result");
        ensure!(matches!(cancellation.outcome, CommandOutcome::Cancelled(CancellationOutcome::Cancelled { ref exchange }) if exchange.as_str() == fixture.original), "cancel did not settle the replacement in the original exchange");
        ensure!(fixture.controller.observer.snapshot()?.session.model == "default", "cancel reverted the second committed model");
        ensure!(fixture.engine.active_invocation_snapshot()?.is_none(), "cancel left a successor invocation running");
        ensure!(fixture.controller.coordinator.submissions.snapshot()?.suspended, "cancel did not suspend further input");
        let store = SqliteStore::read_only(&fixture.controller.data_root)?;
        let exchange = store.load_exchange(&fixture.original)?.context("configuration lost original history")?;
        ensure!(exchange.state() == crate::exchange::ExchangeState::Cancelled && !exchange.turn.is_empty(), "cancel lost terminal state or accepted transcript");
        ensure!(exchange.prompt.contains("Keep this transcript"), "configuration replaced the original prompt");
        for id in ids {
            let record = fixture.controller.exchange_owner.command_status(CommandId::new(id)?).await?.context("command receipt disappeared")?;
            ensure!(matches!(record.state, CommandState::Finished { outcome: Ok(_), .. }), "returned command result was not durable");
        }
        let trace = fixture.controller.trace.clone();
        let path = trace.session_status(&fixture.session).path;
        let records = tokio::task::spawn_blocking(move || -> Result<Vec<Value>> {
            trace.flush()?;
            std::fs::read_to_string(path)?.lines().map(|line| serde_json::from_str(line).map_err(Into::into)).collect()
        }).await??;
        for id in ids {
            let count = |event| records.iter().filter(|record| record["event"].as_str() == Some(event)
                && record["payload"]["command_id"].as_str() == Some(id)).count();
            ensure!(count("exchange.command.completed") == 1 && count("exchange.command.response") == 1, "command {id} completed or responded more than once");
        }
        Ok::<_, anyhow::Error>(())
    }.await;
    scenario?;
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}

/// Resolve configuration at retained scheduling so later FIFO work can cancel a prepared successor.
#[tokio::test]
async fn owner_model_changes_then_cancel_collects_undispatched_replacement() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, CancellationOutcome, Command, CommandId, CommandOutcome,
        CommandRequest, ConfigurationOutcome, InputControl, InvocationDisposition,
    };
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let invoked = Arc::new(tokio::sync::Notify::new());
    let dispatched = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let preparations = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let (prepared, mut preparation) = tokio::sync::mpsc::unbounded_channel();
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(descriptor, {
        let invoked = invoked.clone();
        let dispatched = dispatched.clone();
        move |request, _sink, start, mut cancelled| {
            let invoked = invoked.clone();
            let dispatched = dispatched.clone();
            async move {
                dispatched.fetch_add(1, Ordering::AcqRel);
                start.observed(1).await?;

                invoked.notify_one();
                while !*cancelled.borrow_and_update() {
                    cancelled
                        .changed()
                        .await
                        .map_err(|_| crate::provider::ProviderError::Cancelled)?;
                }

                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Interrupted,
                ))
            }
        }
    });
    native.with_models(vec![crate::provider::ProviderModel {
        id: "mock-model".into(),
        service_tiers: Vec::new(),
        reasoning: vec!["low".into(), "medium".into(), "high".into()],
        default_reasoning: Some("medium".into()),
        selected_reasoning: None,
        context_window: vec![forge_provider_api::model::ProviderContextWindow {
            id: "default".into(),
            token_limit: Some(100_000),
        }],
        default_context_window: Some("default".into()),
        selected_context_window: None,
        vision: false,
        description: None,
        is_default: true,
    }]);
    native.with_pre_dispatch({
        let preparations = preparations.clone();
        move |request| {
            let first = preparations.fetch_add(1, Ordering::AcqRel) == 0;
            let prepared = prepared.clone();
            async move {
                if first {
                    return Ok(());
                }
                prepared
                    .send(request.scope)
                    .expect("preparation observer stays attached");
                std::future::pending().await
            }
        }
    });
    let provider_fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    controller.trace.configure_default(true)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let scenario = async {
        controller.start_provider(&sink).await;
        async {
            while !controller.provider_ready().await { tokio::task::yield_now().await; }
        }.await;
        controller.attach_input(&sink).await?;
        controller.task_controller.change(&mut SqliteStore::open(&controller.data_root)?, None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Continue through retained preparation".into(), Default::default(), 1,
            ))), 1)?;
        prompt::apply(&controller, Request { id: 50, method: "session.prompt.submit".into(),
            params: json!({"operation_id":"retained-original","text":"Keep original accepted history"}),
        }, &sink).await?;
        invoked.notified().await;
        let session = controller.observer.session_id();
        let original = SqliteStore::read_only(&controller.data_root)?.latest_exchange(session)?.unwrap().id;
        let configure = |id: &str, model: &str| native_settings::decode_command(&Request {
            id: 51, method: "session.command.apply".into(),
            params: json!({"operation_id":id,"action":"configure","config":{"model":model}}),
        });
        let first = controller.exchange_owner.execute_identified(configure("retained-model-a", "mock-model")?, sink.clone()).await?;
        let held = preparation.recv().await
            .context("configured successor never reached held preparation")?;
        ensure!(held.exchange_id.as_str() == original, "preparation changed exchange identity");
        ensure!(matches!(first.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings {
            disposition: InvocationDisposition::Committed, ..
        })), "model A did not complete at committed scheduling");
        let mut second = Box::pin(controller.exchange_owner.execute_identified(
            configure("retained-model-b", "default")?, sink.clone()));
        assert!(futures_util::poll!(second.as_mut()).is_pending());
        let cancel = controller.exchange_owner.execute_identified(CommandRequest {
            id: CommandId::new("retained-cancel")?,
            command: Command::CancelExchange(CancelExchangeRequest::default()),
            input_control: InputControl::AtExecution,
        }, sink.clone());
        let (second, cancel) = async {
            tokio::join!(second, cancel)
        }.await;
        let (second, cancel) = (second?, cancel?);
        ensure!(first.sequence < second.sequence && second.sequence < cancel.sequence, "retained work lost FIFO order");
        ensure!(matches!(second.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings {
            disposition: InvocationDisposition::Committed, ..
        })), "model B lost committed scheduling result");
        ensure!(matches!(cancel.outcome, CommandOutcome::Cancelled(CancellationOutcome::Cancelled {
            ref exchange,
        }) if exchange.as_str() == original), "cancel did not settle retained original exchange");
        ensure!(dispatched.load(Ordering::Acquire) == 1, "a held successor reached native handoff");
        ensure!(controller.observer.snapshot()?.session.model == "default", "cancel reverted model B");
        let engine = controller.broker.lock().await.execution_registry()
            .existing_owner(&crate::provider::ThreadOwner::root(session))?.context("execution owner missing")?;
        ensure!(engine.active_invocation_snapshot()?.is_none(), "cancel left pending native work");
        let exchange = SqliteStore::read_only(&controller.data_root)?.load_exchange(&original)?.unwrap();
        ensure!(exchange.state() == crate::exchange::ExchangeState::Cancelled
            && exchange.prompt.contains("Keep original accepted history"), "cancel lost original accepted history");
        Ok::<(), anyhow::Error>(())
    }.await;
    scenario?;
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    controller.finish_exchange_owner().await?;
    provider_fixture.provider_handle().shutdown().await?;
    controller.finish_session_events().await?;
    reader.abort();
    Ok(())
}

/// Reject unavailable selections before touching active work and leave later commands usable.
#[tokio::test]
async fn owner_invalid_model_rejects_before_collection_and_preserves_queue() -> Result<()> {
    use crate::exchange::command::{CommandFailure, ConfigurationRejection, ExecuteCommandError};
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    let rejected = native_settings::decode_command(&Request {
        id: 30,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"owner-invalid-model","action":"configure","config":{"model":"missing-owner-model"}}),
    })?;
    let outcome = fixture
        .controller
        .exchange_owner
        .execute_identified(rejected, fixture.sink.clone())
        .await;
    assert_eq!(
        outcome
            .as_ref()
            .err()
            .context("unavailable model unexpectedly succeeded")?
            .code(),
        "configuration_model_unavailable"
    );
    assert!(matches!(
        outcome,
        Err(ExecuteCommandError::Failed(
            CommandFailure::ConfigurationRejected(ConfigurationRejection::ModelUnavailable)
        ))
    ));
    assert!(
        fixture.engine.active_invocation_snapshot()?.is_some(),
        "invalid model preflight must not collect the original invocation"
    );
    assert_eq!(
        SqliteStore::read_only(&fixture.controller.data_root)?
            .latest_exchange(&fixture.session)?
            .unwrap()
            .id,
        fixture.original
    );
    fixture
        .provider
        .mock_response_control()
        .unwrap()
        .dispatch(
            &fixture.session,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let valid = native_settings::decode_command(&Request {
        id: 31,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"owner-valid-model-after-rejection","action":"configure","config":{"model":"mock-model"}}),
    })?;
    fixture
        .controller
        .exchange_owner
        .execute_identified(valid, fixture.sink.clone())
        .await?;
    assert_eq!(
        fixture.controller.observer.snapshot()?.session.model,
        "mock-model"
    );
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}

/// Permission changes collect child work as well as the root before the replacement can run.
#[tokio::test]
async fn owner_permission_change_collects_child_before_replacement() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    let child_provider = crate::provider::test_support::build_mock(
        fixture.controller.data_root.join("child-provider"),
        fixture.controller.permission.clone(),
        fixture.controller.trace.clone(),
    )?;
    fixture
        .controller
        .broker
        .lock()
        .await
        .agent_provider_factory
        .register(child_provider.clone());
    let script = child_provider.mock_response_control().unwrap();
    let child_hold = script
        .dispatch(
            &fixture.session,
            MockResponseCommand::Hold {
                mode: None,
                any_session: true,
            },
        )
        .await?;
    fixture.controller.attach_agents(&fixture.sink).await?;
    let spawned = fixture
        .controller
        .agent_coordinator
        .sender()
        .request(crate::agent::AgentRequest {
            operation_id: "owner-child-for-permission".into(),
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
                profile: Some("worker".into()),
                text: "Wait for collection".into(),
            }),
        })
        .await?;
    let child = spawned["agent"]["id"]
        .as_str()
        .context("spawned child identity missing")?
        .to_owned();
    let arrived = script
        .dispatch(
            &fixture.session,
            MockResponseCommand::Wait {
                id: child_hold["id"].as_str().unwrap().into(),
            },
        )
        .await;
    let arrived = arrived.context("registered child provider did not reach its hold")?;
    assert_eq!(
        arrived["session_id"],
        crate::provider::ThreadOwner::new(
            fixture.session.clone().into(),
            forge_provider_api::AgentId::new(child).expect("registered child identity is nonempty")
        )
        .expect("fixture identities are nonempty")
        .runtime_key()
    );
    assert!(fixture.controller.agent_coordinator.active_count() > 0);
    let command = native_permission::decode_command(&Request {
        id: 4,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"owner-collect-child","action":"permission","change":{"kind":"set_approval","mode":"deny"}}),
    })?;
    let completed = fixture
        .controller
        .exchange_owner
        .execute_identified(command, fixture.sink.clone())
        .await
        .map_err(anyhow::Error::from);
    completed.context("permission command did not collect the held child")?;
    assert_eq!(fixture.controller.agent_coordinator.active_count(), 0);
    assert!(!fixture.controller.agent_coordinator.is_quiescing());
    assert_eq!(
        fixture
            .controller
            .observer
            .snapshot()?
            .session
            .approval_mode,
        crate::session::ApprovalMode::Deny
    );
    assert!(
        SqliteStore::read_only(&fixture.controller.data_root)?
            .load_exchange(&fixture.original)?
            .is_some()
    );
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}

/// A timed-out cleanup keeps its actual worker and leaves the requested policy uncommitted.
#[tokio::test]
async fn owner_permission_timeout_retains_cleanup_and_prior_policy() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let invoked = Arc::new(tokio::sync::Notify::new());
    let started = Arc::new(tokio::sync::Notify::new());
    let release = Arc::new(tokio::sync::Semaphore::new(0));
    let collected = Arc::new(AtomicBool::new(false));
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(descriptor, {
        let invoked = invoked.clone();
        move |request, _sink, start, mut cancelled| {
            let invoked = invoked.clone();
            async move {
                start.observed(1).await?;

                invoked.notify_one();
                while !*cancelled.borrow_and_update() {
                    cancelled
                        .changed()
                        .await
                        .map_err(|_| crate::provider::ProviderError::Cancelled)?;
                }

                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Interrupted,
                ))
            }
        }
    });
    native.with_models(vec![serde_json::from_value(json!({
        "id":"fixture-model", "reasoning":["low","medium","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?]);
    native.with_cleanup({
        let started = started.clone();
        let release = release.clone();
        let collected = collected.clone();
        move |_| {
            let started = started.clone();
            let release = release.clone();
            let collected = collected.clone();
            async move {
                if !collected.load(Ordering::Acquire) {
                    started.notify_one();
                    release.acquire().await.unwrap().forget();
                    collected.store(true, Ordering::Release);
                }
                Ok(())
            }
        }
    });
    let provider_fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    controller.trace.configure_default(true)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    while !controller.provider_ready().await {
        tokio::task::yield_now().await;
    }
    controller.attach_input(&sink).await?;
    prompt::apply(
        &controller,
        Request {
            id: 50,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"cleanup-live-input","text":"Wait for cleanup"}),
        },
        &sink,
    )
    .await?;
    invoked.notified().await;
    let before = controller.observer.snapshot()?.session.access;
    let command = native_permission::decode_command(&Request {
        id: 51,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"owner-cleanup-timeout","action":"permission","change":{"kind":"toggle_write_enabled"}}),
    })?;
    let mut operation = Box::pin(
        controller
            .exchange_owner
            .execute_identified(command, sink.clone()),
    );
    tokio::select! {
        result = &mut operation => anyhow::bail!("cleanup unexpectedly completed before barrier: {result:?}"),
        () = started.notified() => {}
    }
    tokio::time::pause();
    for _ in 0..=forge_provider_api::CleanupPolicy::Cancellation
        .response_after()
        .as_secs()
    {
        tokio::time::advance(Duration::from_secs(1)).await;
    }
    let result = operation.await;
    tokio::time::resume();
    assert!(
        matches!(
            &result,
            Err(crate::exchange::command::ExecuteCommandError::Failed(
                crate::exchange::command::CommandFailure::CollectionTimedOut
            ))
        ),
        "{result:?}"
    );
    assert_eq!(controller.observer.snapshot()?.session.access, before);
    assert!(!collected.load(Ordering::Acquire));
    assert_eq!(release.available_permits(), 0);
    release.add_permits(1);
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    controller
        .finish_exchange_owner()
        .await
        .context("owner did not finish after cleanup release")?;
    assert!(collected.load(Ordering::Acquire));
    provider_fixture.provider_handle().shutdown().await?;
    controller.finish_session_events().await?;
    reader.abort();
    Ok(())
}

/// Conversation rollback collects a live selected exchange before publishing removed history.
#[tokio::test]
async fn owner_conversation_rollback_collects_selected_live_work() -> Result<()> {
    use crate::exchange::{
        command::{Command, CommandId, CommandRequest, InputControl},
        rollback::{PreparedRollbackToken, RollbackCommand, RollbackSelection},
    };
    let fixture = owner_transition_fixture().await?;
    let file = fixture.root.path().join("rollback-must-preserve.txt");
    std::fs::write(&file, "unchanged")?;
    let rollback =
        fixture
            .controller
            .broker
            .lock()
            .await
            .capture_rollback(&RollbackCommand::Apply(
                PreparedRollbackToken::Conversation {
                    selection: RollbackSelection::new(
                        fixture.original.clone(),
                        fixture.original.clone(),
                    ),
                },
            ))?;
    let request = CommandRequest {
        id: CommandId::new("owner-conversation-rollback")?,
        command: Command::Rollback(rollback),
        input_control: InputControl::AtExecution,
    };
    let mut result = Box::pin(
        fixture
            .controller
            .exchange_owner
            .execute_identified(request, fixture.sink.clone()),
    );
    assert!(futures_util::poll!(result.as_mut()).is_pending());
    result.await?;
    assert!(fixture.engine.active_invocation_snapshot()?.is_none());
    assert_eq!(
        SqliteStore::read_only(&fixture.controller.data_root)?
            .load_exchange(&fixture.original)?
            .unwrap()
            .disposition(),
        crate::exchange::HistoryDisposition::RolledBack
    );
    assert_eq!(std::fs::read_to_string(file)?, "unchanged");
    fixture
        .service
        .shutdown(Duration::from_secs(5))
        .await
        .context("rollback session shutdown failed")?;
    fixture.reader.abort();
    Ok(())
}

/// Closing one session must preserve a sibling invocation owned by the shared runtime.
#[tokio::test]
async fn owner_close_preserves_shared_runtime_sibling() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    let registry = fixture.service.registry().await?;
    let sibling_record = prepare_new_session(
        std::path::Path::new(&registry.initialize.data_root),
        &registry.initialize.client_id,
        "mock",
        false,
        &fixture.session,
        &json!({"name":"shutdown sibling"}),
        1,
    )?;
    let sibling = registry.resolve(&sibling_record.id).await?;
    let sibling_registry = sibling.broker.lock().await.execution_registry();
    let sibling_engine = sibling_registry
        .owner(crate::provider::ThreadOwner::root(&sibling_record.id))
        .await?;
    assert!(Arc::ptr_eq(&fixture.provider, &sibling.provider_handle()));
    sibling.start_provider(&fixture.sink).await;
    let script = fixture.provider.mock_response_control().unwrap();
    let hold = script
        .dispatch(
            &sibling_record.id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    sibling.attach_input(&fixture.sink).await?;
    prompt::apply(&sibling, Request { id: 701, method: "session.prompt.submit".into(),
        params: json!({"operation_id":"shared-sibling-input","text":"Keep running while another session closes"}) }, &fixture.sink).await?;
    let result = async {
        script
            .dispatch(
                &sibling_record.id,
                MockResponseCommand::Wait {
                    id: hold["id"].as_str().unwrap().into(),
                },
            )
            .await?;
        fixture
            .controller
            .input_stopping
            .store(true, Ordering::Release);
        fixture
            .controller
            .coordinator
            .submissions
            .changed
            .notify_one();
        fixture.controller.finish_exchange_owner().await?;
        anyhow::ensure!(
            sibling_engine.active_invocation_snapshot()?.is_some(),
            "closing one owner collected its sibling"
        );
        script
            .dispatch(
                &sibling_record.id,
                MockResponseCommand::Release {
                    id: hold["id"].as_str().unwrap().into(),
                },
            )
            .await?;
        while sibling_engine.active_invocation_snapshot()?.is_some()
            || sibling.coordinator.execution.available_permits() == 0
        {
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await;
    result.context("shared sibling did not survive owner close")?;
    fixture
        .service
        .shutdown(Duration::from_secs(5))
        .await
        .context("shared runtime shutdown failed")?;
    fixture.reader.abort();
    Ok(())
}
/// Saturated presentation output cannot delay owner shutdown or discard its retained prompt response.
#[tokio::test]
async fn owner_shutdown_collects_while_prompt_ui_output_is_saturated() -> Result<()> {
    let fixture = owner_transition_fixture().await?;
    let (output, mut receiver) = forge_protocol::outbound::channel();
    for sequence in 0..forge_protocol::MAX_PENDING_FRAMES - forge_protocol::RESERVED_CONTROL_RECORDS
    {
        output.send(sequence)?;
    }
    for sequence in 0..forge_protocol::RESERVED_CONTROL_RECORDS {
        output.send_control(sequence)?;
    }
    let attempting_output = Arc::new(tokio::sync::Notify::new());
    let observed = attempting_output.clone();
    let output = output.observe_responses(move |response| {
        if response.id == 9001 {
            observed.notify_one();
        }
        Ok(())
    });
    let controller = fixture.controller.clone();
    let prompt_output = output.clone();
    let prompt = tokio::spawn(async move {
        prompt::apply(&controller, Request { id: 9001, method: "session.prompt.submit".into(),
            params: json!({"operation_id":"saturated-prompt","input_id":"saturated-prompt-input","text":"Retain this accepted input","queued":true}) }, &prompt_output).await
    });
    attempting_output.notified().await;
    assert!(
        !output.is_closed(),
        "fixture must retain a live backpressured observer"
    );
    assert!(fixture.engine.active_invocation_snapshot()?.is_some());
    let shutdown = fixture.service.shutdown(Duration::from_secs(5)).await;
    shutdown?;
    assert!(fixture.engine.active_invocation_snapshot()?.is_none());
    prompt.await??;
    let response = async {
        loop {
            let frame = receiver
                .recv()
                .await?
                .context("retained UI output closed before prompt response")?;
            let value: Value = serde_json::from_slice(frame.bytes())?;
            if value.get("id").and_then(Value::as_u64) == Some(9001) {
                return Ok::<_, anyhow::Error>(value);
            }
        }
    }
    .await?;
    assert_eq!(response["result"]["accepted"], true);
    assert_eq!(response["result"]["input_id"], "saturated-prompt-input");
    fixture.reader.abort();
    Ok(())
}

/// Real owner traces must show FIFO starts on distinct updates, after preceding durable completion.
#[tokio::test]
async fn owner_starts_one_fifo_command_per_update_after_committed_completion() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, Command, CommandId, CommandOutcome, CommandRequest,
        ConfigurationChange, ConfigurationOutcome, InputControl, PermissionChange,
    };
    use crate::exchange::journal::CommandState;
    let fixture = owner_transition_fixture().await?;
    fixture.controller.trace.configure(true)?;
    let ids = [
        "step-budget-cancel",
        "step-budget-checks-on",
        "step-budget-checks-off",
    ];
    let scenario = async {
        // Hold asynchronous collection while all three caller intents reach durable FIFO admission.
        let collection_barrier = fixture.controller.coordinator.control.clone().acquire_owned().await?;
        let request = |id: &str, command| -> Result<CommandRequest> {
            Ok(CommandRequest { id: CommandId::new(id)?, command, input_control: InputControl::AtExecution })
        };
        let mut cancelled = Box::pin(fixture.controller.execute_command(
            request(ids[0], Command::CancelExchange(CancelExchangeRequest::default()))?, &fixture.sink));
        assert!(futures_util::poll!(cancelled.as_mut()).is_pending());
        let mut enabled = Box::pin(fixture.controller.execute_command(
            request(ids[1], Command::ConfigureSession(ConfigurationChange::Permission(
                PermissionChange::SetCheckEditing { enabled: true })))?, &fixture.sink));
        assert!(futures_util::poll!(enabled.as_mut()).is_pending());
        let mut disabled = Box::pin(fixture.controller.execute_command(
            request(ids[2], Command::ConfigureSession(ConfigurationChange::Permission(
                PermissionChange::SetCheckEditing { enabled: false })))?, &fixture.sink));
        assert!(futures_util::poll!(disabled.as_mut()).is_pending());
        async {
            loop {
                let first = fixture.controller.exchange_owner.command_status(CommandId::new(ids[0])?).await?;
                let second = fixture.controller.exchange_owner.command_status(CommandId::new(ids[1])?).await?;
                let third = fixture.controller.exchange_owner.command_status(CommandId::new(ids[2])?).await?;
                if matches!(first.as_ref().map(|record| &record.state), Some(CommandState::Active { .. }))
                    && matches!(second.as_ref().map(|record| &record.state), Some(CommandState::Queued { .. }))
                    && matches!(third.as_ref().map(|record| &record.state), Some(CommandState::Queued { .. })) {
                    break Ok::<_, anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        }.await?;
        drop(collection_barrier);
        let (cancelled, enabled, disabled) = async {
            tokio::join!(cancelled, enabled, disabled)
        }.await;
        let (cancelled, enabled, disabled) = (cancelled?, enabled?, disabled?);
        ensure!(cancelled.sequence < enabled.sequence && enabled.sequence < disabled.sequence, "completion receipts lost FIFO sequence");
        ensure!(matches!(cancelled.outcome, CommandOutcome::Cancelled(_)), "collection did not finish cancellation");
        ensure!(matches!(enabled.outcome, CommandOutcome::Configured(ConfigurationOutcome::CheckEditing { enabled: true })), "first local command lost its own result");
        ensure!(matches!(disabled.outcome, CommandOutcome::Configured(ConfigurationOutcome::CheckEditing { enabled: false })), "second local command lost its own result");
        ensure!(!fixture.controller.observer.snapshot()?.check_editing, "final configuration did not follow FIFO order");
        for id in ids {
            let record = fixture.controller.exchange_owner.command_status(CommandId::new(id)?).await?.context("completed command disappeared")?;
            ensure!(matches!(record.state, CommandState::Finished { outcome: Ok(_), .. }), "caller completion was not durable");
        }

        // Read the emitted owner contract only after the real trace writer has drained.
        let trace = fixture.controller.trace.clone();
        let path = trace.session_status(&fixture.session).path;
        let records = tokio::task::spawn_blocking(move || -> Result<Vec<Value>> {
            trace.flush()?;
            std::fs::read_to_string(path)?.lines().map(|line| serde_json::from_str(line).map_err(Into::into)).collect()
        }).await??;
        let mut starts = Vec::new();
        let mut completions = Vec::new();
        for (position, record) in records.iter().enumerate() {
            let Some(id) = record["payload"]["command_id"].as_str() else { continue; };
            if !ids.contains(&id) { continue; }
            match record["event"].as_str() {
                Some("exchange.command.started") => starts.push((id, position,
                    record["payload"]["update_step"].as_u64().context("start trace omitted its update step")?)),
                Some("exchange.command.completed") => completions.push((id, position,
                    record["payload"]["update_step"].as_u64().context("completion trace omitted its update step")?)),
                _ => {}
            }
        }
        ensure!(starts.len() == ids.len() && completions.len() == ids.len(), "each admitted command must start and finish exactly once: starts={starts:?}, completions={completions:?}");
        ensure!(starts.iter().map(|start| start.0).collect::<Vec<_>>() == ids, "trace starts were not FIFO: {starts:?}");
        ensure!(completions.iter().map(|completion| completion.0).collect::<Vec<_>>() == ids, "trace completions were not FIFO: {completions:?}");
        for index in 0..ids.len() {
            ensure!(starts[index].1 < completions[index].1, "completion preceded its own start");
            if index + 1 < ids.len() {
                ensure!(starts[index].2 < starts[index + 1].2, "two commands started in one owner update: {starts:?}");
                ensure!(completions[index].1 < starts[index + 1].1, "successor started before preceding completion committed: starts={starts:?}, completions={completions:?}");
            }
        }
        ensure!(starts[0].2 < completions[0].2, "held collection did not span owner updates");
        Ok::<_, anyhow::Error>(())
    }.await;
    scenario?;
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}
