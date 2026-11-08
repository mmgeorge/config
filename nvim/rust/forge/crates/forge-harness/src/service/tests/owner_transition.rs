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
use std::sync::atomic::Ordering;
use std::time::Duration;

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
