use super::super::{HarnessBroker, SessionController, prompt};
use super::bootstrap::initialize;
use crate::storage::SqliteStore;
use anyhow::{Context, Result, ensure};
use forge_protocol::message::Request;
use forge_protocol::outbound::MessageSender;
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::Ordering;
use std::time::Duration;

/// Select the native boundary held by a cancellation scenario.
#[derive(Clone, Copy, Eq, PartialEq)]
enum CollectionBoundary {
    Active,
    /// Hold native execution after the provider echoes the submitted user message, as Codex does.
    ActiveAfterUserEcho,
    /// Hold native execution inside a started shell tool until its deadline stops the turn.
    ActiveInTool,
}

/// Hold one real owner at a selected boundary while native collection remains delayed.
struct SlowCollectionFixture {
    root: tempfile::TempDir,
    provider: crate::provider::test_support::ProviderTestSession,
    controller: Arc<SessionController>,
    engine: Arc<crate::provider::session_execution::SessionExecution>,
    sink: MessageSender,
    reader: tokio::task::JoinHandle<Result<()>>,
    dispatches: tokio::sync::mpsc::UnboundedReceiver<forge_provider_api::input::PromptInput>,
    events: Arc<std::sync::Mutex<Vec<(tokio::time::Instant, serde_json::Value)>>>,
    events_changed: Arc<tokio::sync::Notify>,
    collection_count: Arc<std::sync::atomic::AtomicUsize>,
}

/// Start a prompt held before acceptance or after native start, with delayed first collection.
async fn slow_collection_fixture(
    collection_delay: Duration,
    boundary: CollectionBoundary,
) -> Result<SlowCollectionFixture> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let (started, mut starts) = tokio::sync::mpsc::unbounded_channel();
    let executions = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let steered_submissions = Arc::new(std::sync::Mutex::new(Vec::new()));
    let observed_steering = steered_submissions.clone();
    let (steering_accepted, _steering_receipts) = tokio::sync::mpsc::unbounded_channel();
    let (dispatched, dispatches) = tokio::sync::mpsc::unbounded_channel();
    let owner: Arc<std::sync::Mutex<std::sync::Weak<SessionController>>> = Arc::default();
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, mut cancelled| {
            let dispatched = dispatched.clone();
            let started = started.clone();
            let steered_submissions = steered_submissions.clone();
            let held_active = matches!(
                boundary,
                CollectionBoundary::Active
                    | CollectionBoundary::ActiveAfterUserEcho
                    | CollectionBoundary::ActiveInTool
            ) && executions.fetch_add(1, Ordering::AcqRel) == 0;
            let echo_user_message =
                held_active && boundary == CollectionBoundary::ActiveAfterUserEcho;
            let run_tool = held_active && boundary == CollectionBoundary::ActiveInTool;
            async move {
                start.observed(1).await?;
                if echo_user_message {
                    let item_id = forge_provider_api::ItemId::new("echoed-user-message")
                        .expect("fixture item ID is valid");
                    let text = match &request.input {
                        forge_provider_api::input::PromptInput::Text(text) => text.clone(),
                        _ => String::new(),
                    };
                    for event in [
                        forge_provider_api::ItemEvent::Started(
                            forge_provider_api::ItemStart::UserMessage(
                                forge_provider_api::UserMessageStart {
                                    text: text.clone(),
                                    submission_id: Some(request.submission_id.clone()),
                                },
                            ),
                        ),
                        forge_provider_api::ItemEvent::Completed(
                            forge_provider_api::ItemCompletion::UserMessage(
                                forge_provider_api::MessageCompletion {
                                    text,
                                    outcome: forge_provider_api::MessageOutcome::Completed,
                                },
                            ),
                        ),
                    ] {
                        sink.send_wait(forge_provider_api::AdapterActivity::Item {
                            thread_id: request.thread_id.clone(),
                            turn_id: request.turn_id.clone(),
                            item_id: item_id.clone(),
                            event,
                        })
                        .await?;
                    }
                }
                let tool_event = |event| forge_provider_api::AdapterActivity::Item {
                    thread_id: request.thread_id.clone(),
                    turn_id: request.turn_id.clone(),
                    item_id: forge_provider_api::ItemId::new("held-shell-command")
                        .expect("fixture item ID is valid"),
                    event,
                };
                if run_tool {
                    for event in [
                        forge_provider_api::ItemEvent::Started(
                            forge_provider_api::ItemStart::ToolCall(
                                forge_provider_api::ToolDescriptor {
                                    semantics: Default::default(),
                                    name: "shell".into(),
                                    title: "Held fixture command".into(),
                                    arguments: forge_provider_api::InspectableArguments::new(
                                        json!({"command":"ping -n 30 127.0.0.1"}),
                                    ),
                                },
                            ),
                        ),
                        forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::ExecutionStarted,
                        },
                    ] {
                        sink.send_wait(tool_event(event)).await?;
                    }
                }
                let result = if held_active {
                    let _ = started.send(());
                    while !*cancelled.borrow() {
                        cancelled
                            .changed()
                            .await
                            .map_err(|_| crate::provider::ProviderError::Cancelled)?;
                    }
                    if run_tool {
                        let completion = forge_provider_api::ToolCompletion {
                            outcome: crate::provider::ToolOutcome::Failed {
                                diagnostic: forge_provider_api::ToolDiagnostic {
                                    kind: forge_provider_api::ProviderFailureKind::Timeout,
                                    message: forge_provider_api::diagnostic::SanitizedMessage::new(
                                        "fixture tool interrupted",
                                    )
                                    .expect("static diagnostic is valid"),
                                },
                            },
                            descriptor: None,
                            output: None,
                            changes: Default::default(),
                        };
                        sink.send_wait(tool_event(forge_provider_api::ItemEvent::Completed(
                            forge_provider_api::ItemCompletion::ToolCall(completion),
                        )))
                        .await?;
                    }
                    forge_provider_api::turn::TurnOutcome::Interrupted
                } else {
                    let _ = dispatched.send(request.input.clone());
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    }
                };
                let mut covered = vec![forge_provider_api::CoveredInput::new(
                    request.submission_id.clone(),
                    request.attempt_id.clone(),
                )];
                covered.extend(std::mem::take(
                    &mut *steered_submissions
                        .lock()
                        .expect("fixture steering receipts poisoned"),
                ));
                // A native history boundary lets an output-free interruption authorize retraction.
                let boundary = echo_user_message.then(|| {
                    forge_provider_api::CheckpointId::new(format!(
                        "echo-boundary:{}",
                        request.submission_id.as_str()
                    ))
                    .expect("fixture checkpoint ID is valid")
                });
                forge_provider_api::TurnResult::new(result, 2, None, covered, Vec::new(), boundary)
                    .map_err(|_| {
                        crate::provider::ProviderError::Observation(
                            forge_provider_api::ObservationFailure::Protocol,
                        )
                    })
            }
        },
    );
    native.with_steering(move |request| {
        let observed_steering = observed_steering.clone();
        let steering_accepted = steering_accepted.clone();
        async move {
            if let Some(receipt) = request.delivery.receipt() {
                observed_steering
                    .lock()
                    .expect("fixture steering receipts poisoned")
                    .push(forge_provider_api::CoveredInput::new(
                        receipt.id().clone(),
                        receipt.attempt_id()?,
                    ));
                let _ = steering_accepted.send(receipt.id().clone());
            }
            Ok(())
        }
    });
    native.with_models(vec![serde_json::from_value(json!({
        "id":"fixture-model", "reasoning":["low","medium","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?]);
    let collections = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let collection_count = collections.clone();
    native.with_cleanup_request(move |_| {
        let first = collections.fetch_add(1, Ordering::AcqRel) == 0;
        async move {
            if first {
                tokio::time::sleep(collection_delay).await;
            }
            Ok(())
        }
    });
    let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider.provider_handle());
    let controller = SessionController::new(broker).await?;
    controller.trace.configure_default(true)?;
    *owner.lock().unwrap() = Arc::downgrade(&controller);
    let (sink, mut output) = forge_protocol::outbound::channel();
    let events = Arc::new(std::sync::Mutex::new(Vec::new()));
    let received_events = events.clone();
    let events_changed = Arc::new(tokio::sync::Notify::new());
    let received_changed = events_changed.clone();
    let reader = tokio::spawn(async move {
        while let Some(message) = output.recv().await? {
            received_events
                .lock()
                .expect("fixture events poisoned")
                .push((
                    tokio::time::Instant::now(),
                    serde_json::from_slice(message.bytes())?,
                ));
            received_changed.notify_one();
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    controller.attach_input(&sink).await?;
    prompt::apply(
        &controller,
        Request {
            id: 800,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"slow-first","text":"First prompt"}),
        },
        &sink,
    )
    .await?;
    starts
        .recv()
        .await
        .context("first input never emitted its native start observation")?;
    let engine = controller
        .broker
        .lock()
        .await
        .execution_registry()
        .existing_owner(&crate::provider::ThreadOwner::root(
            controller.observer.session_id(),
        ))?
        .context("fixture broker has no retained root execution owner")?;
    Ok(SlowCollectionFixture {
        root,
        provider,
        controller,
        engine,
        sink,
        reader,
        dispatches,
        events,
        events_changed,
        collection_count,
    })
}

impl SlowCollectionFixture {
    /// Stop owner work and provider collection after a scenario, preserving the scenario result.
    async fn finish(self, scenario: Result<()>) -> Result<()> {
        let teardown = async {
            self.controller
                .input_stopping
                .store(true, Ordering::Release);
            self.controller.coordinator.submissions.changed.notify_one();
            self.controller.finish_exchange_owner().await?;
            self.provider.provider_handle().shutdown().await?;
            self.controller.finish_session_events().await
        };
        let teardown = teardown.await;
        let trace_tail = scenario.is_err().then(|| {
            let session = self.controller.observer.session_id();
            let _ = self.controller.trace.flush();
            let path = self.controller.trace.session_status(session).path;
            let trace = std::fs::read_to_string(path).unwrap_or_default();
            let mut tail: Vec<&str> = trace.lines().rev().take(600).collect();
            tail.reverse();
            tail.join("\n")
        });
        self.reader.abort();
        drop(self.root);
        if let Err(error) = scenario {
            return Err(error.context(format!(
                "owner trace tail:\n{}",
                trace_tail.unwrap_or_default()
            )));
        }
        teardown
    }
}

/// Restore an output-free prompt when Ctrl-C arrives after the provider echoes it.
///
/// Codex reports the submitted prompt as a native user-message item before any assistant
/// output. That echo is not provider activity, so a restoring cancellation must still retract
/// the native turn, delete the provisional exchange, and publish `retracted` for the input.
#[tokio::test]
async fn owner_cancel_after_user_message_echo_retracts_output_free_prompt() -> Result<()> {
    use crate::exchange::command::{CancelExchangeRequest, CancelledInputPolicy, Command};
    let mut fixture =
        slow_collection_fixture(Duration::ZERO, CollectionBoundary::ActiveAfterUserEcho).await?;
    let scenario = async {
        // The echo precedes the fixture's start signal, so wait until the broker applied it.
        let exchange_id = async {
            loop {
                let changed = fixture.events_changed.notified();
                let echoed = SqliteStore::read_only(&fixture.controller.data_root)?
                    .list_exchange(fixture.controller.observer.session_id())?
                    .into_iter()
                    .find(|exchange| {
                        exchange
                            .turn
                            .iter()
                            .flat_map(|turn| turn.messages())
                            .next()
                            .is_some()
                    })
                    .map(|exchange| exchange.id);
                if let Some(exchange_id) = echoed {
                    break Ok::<_, anyhow::Error>(exchange_id);
                }
                changed.await;
            }
        }
        .await?;
        fixture
            .controller
            .exchange_owner
            .execute_command(Command::CancelExchange(CancelExchangeRequest {
                input: CancelledInputPolicy::RestoreIfUnconsumed,
            }))
            .await
            .context("restoring cancellation failed")?;
        // Input settlement publishes after the cancellation command commits its outcome.
        let input_updates = || {
            fixture
                .events
                .lock()
                .expect("fixture events poisoned")
                .iter()
                .filter(|(_, event)| event["event"] == "input_changed")
                .map(|(_, event)| event["payload"].clone())
                .collect::<Vec<_>>()
        };
        async {
            loop {
                let changed = fixture.events_changed.notified();
                if input_updates()
                    .iter()
                    .any(|update| update["retracted"].is_string())
                {
                    break;
                }
                changed.await;
            }
        }
        .await;
        ensure!(
            SqliteStore::read_only(&fixture.controller.data_root)?
                .load_exchange(&exchange_id)?
                .is_none(),
            "retracted prompt retained its provisional exchange"
        );
        let retained_name = crate::session::SessionStore::load_session(
            &SqliteStore::read_only(&fixture.controller.data_root)?,
            fixture.controller.observer.session_id(),
        )?
        .context("fixture session is missing")?
        .name;
        ensure!(
            retained_name.is_empty(),
            "retracted first prompt still names the session: {retained_name:?}"
        );
        // Retraction commits the collected invocation, so the execution owner admits new work.
        prompt::apply(
            &fixture.controller,
            Request {
                id: 802,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"after-retraction","text":"Prompt after retraction"}),
            },
            &fixture.sink,
        )
        .await?;
        async {
            loop {
                match fixture.dispatches.recv().await {
                    Some(forge_provider_api::input::PromptInput::Text(text))
                        if text == "Prompt after retraction" =>
                    {
                        break Ok(());
                    }
                    Some(_) => {}
                    None => break Err(anyhow::anyhow!("fixture dispatch channel closed")),
                }
            }
        }
        .await?;
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish(scenario).await
}

/// A tool deadline fails its exchange once, collects the turn, and dispatches the next prompt.
///
/// The fixture holds a started shell tool, as Codex does for `ping -n 30`. The test forces the
/// armed watchdog's expiry instead of waiting on the real clock.
#[tokio::test]
async fn tool_timeout_fails_exchange_once_and_dispatches_next_prompt() -> Result<()> {
    let mut fixture =
        slow_collection_fixture(Duration::ZERO, CollectionBoundary::ActiveInTool).await?;
    let scenario = async {
        async {
            loop {
                let changed = fixture.events_changed.notified();
                if fixture
                    .controller
                    .observer
                    .session_events()
                    .expire_running_tool_deadlines()?
                    > 0
                {
                    break Ok::<(), anyhow::Error>(());
                }
                changed.await;
            }
        }
        .await?;
        async {
            loop {
                let changed = fixture.events_changed.notified();
                let failed = SqliteStore::read_only(&fixture.controller.data_root)?
                    .list_exchange(fixture.controller.observer.session_id())?
                    .into_iter()
                    .any(|exchange| exchange.state() == crate::exchange::ExchangeState::Failed);
                if failed {
                    break Ok::<(), anyhow::Error>(());
                }
                changed.await;
            }
        }
        .await?;
        prompt::apply(
            &fixture.controller,
            Request {
                id: 803,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"after-timeout","text":"Prompt after timeout"}),
            },
            &fixture.sink,
        )
        .await?;
        async {
            loop {
                match fixture.dispatches.recv().await {
                    Some(forge_provider_api::input::PromptInput::Text(text))
                        if text == "Prompt after timeout" =>
                    {
                        break Ok(());
                    }
                    Some(_) => {}
                    None => break Err(anyhow::anyhow!("fixture dispatch channel closed")),
                }
            }
        }
        .await?;
        let timeout_notices: Vec<_> = fixture
            .events
            .lock()
            .expect("fixture events poisoned")
            .iter()
            .filter(|(_, event)| event.to_string().contains("tool_timeout"))
            .map(|(_, event)| event["event"].clone())
            .collect();
        ensure!(
            timeout_notices
                .iter()
                .filter(|event| *event == "error")
                .count()
                <= 1,
            "tool timeout was announced more than once: {timeout_notices:?}"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish(scenario).await
}

/// Retain actual native execution cleanup after the cancellation response times out.
#[tokio::test(start_paused = true)]
async fn owner_active_overdue_cancel_recovers_when_late_collection_completes() -> Result<()> {
    overdue_cancel_recovers(CollectionBoundary::Active).await
}

/// Exercise one response deadline independently of retained preparation or native collection.
async fn overdue_cancel_recovers(boundary: CollectionBoundary) -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, Command, CommandFailure, CommandId, CommandRequest,
        ExecuteCommandError, InputControl,
    };
    let mut fixture = slow_collection_fixture(Duration::from_secs(40), boundary).await?;
    let scenario = async {
        if boundary == CollectionBoundary::Active {
            async {
                while fixture.engine.active_invocation_scope()?.is_none() {
                    tokio::time::sleep(Duration::from_millis(1)).await;
                }
                Ok::<(), anyhow::Error>(())
            }.await?;
        }
        let operation_id = "overdue-owner-cancel";
        let started = tokio::time::Instant::now();
        let command_id = CommandId::new(operation_id)?;
        let controller = Arc::clone(&fixture.controller);
        let sink = fixture.sink.clone();
        let cancellation = tokio::spawn(async move {
            controller.exchange_owner.execute_identified(CommandRequest {
                id: command_id,
                command: Command::CancelExchange(CancelExchangeRequest::default()),
                input_control: InputControl::AtExecution,
            }, sink).await
        });
        let pending = async {
            loop {
                let pending = fixture.events.lock().expect("fixture events poisoned")
                    .iter().find(|(_, event)| event["event"] == "provider_event"
                        && event["payload"]["kind"] == "cleanup_progress"
                        && event["payload"]["data"]["operation_id"] == operation_id)
                    .cloned();
                if let Some(pending) = pending { break pending; }
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
        }.await;
        ensure!(pending.0.duration_since(started) >= Duration::from_secs(10));
        ensure!(pending.1["payload"]["data"]["policy"] == "cancellation");
        ensure!(pending.1["payload"]["data"]["state"] == "pending");
        ensure!(pending.1["payload"]["text"] == "Cancellation still pending");
        ensure!(!cancellation.is_finished(), "pending progress completed the command");
        let overdue = cancellation.await?;
        ensure!(matches!(overdue,
            Err(ExecuteCommandError::Failed(CommandFailure::CollectionTimedOut))
        ), "overdue cancellation did not preserve its normalized failure");
        ensure!(started.elapsed() >= Duration::from_secs(30));
        ensure!(started.elapsed() < Duration::from_secs(31), "cancellation restarted its budget");
        ensure!(fixture.controller.coordinator.execution.available_permits() == 0,
            "timeout released execution before native cleanup settled");

        tokio::time::sleep(Duration::from_secs(20)).await;
        ensure!(started.elapsed() >= Duration::from_secs(50));
        ensure!(fixture.collection_count.load(Ordering::Acquire) == 1, "late native cleanup did not remain singular");
        ensure!(fixture.engine.active_invocation_scope()?.is_none(), "native execution remained active after late collection");
        fixture.controller.exchange_owner.execute_command(
            Command::CancelExchange(CancelExchangeRequest::default())
        ).await.context("cancel was refused after late collection completed")?;
        let completions = fixture.events.lock().expect("fixture events poisoned")
            .iter().filter(|(_, event)| event["event"] == "command_completed"
                && event["payload"]["id"] == operation_id).count();
        ensure!(completions == 1, "late cleanup emitted another command completion");
        prompt::apply(&fixture.controller, Request {
            id: 802,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"after-overdue-cancel","text":"Prompt after late settlement"}),
        }, &fixture.sink).await?;
        let input = fixture.dispatches.recv().await;
        ensure!(matches!(input, Some(forge_provider_api::input::PromptInput::Text(text))
            if text == "Prompt after late settlement"));
        Ok::<(), anyhow::Error>(())
    }.await;
    fixture.finish(scenario).await
}

/// Retain the selected invocation when cancellation exhausts its budget waiting for control.
#[tokio::test(start_paused = true)]
async fn owner_cancel_control_acquisition_timeout_collects_original_before_reuse() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, Command, CommandFailure, CommandId, CommandRequest,
        ExecuteCommandError, InputCommand, InputControl,
    };
    let mut fixture = slow_collection_fixture(Duration::ZERO, CollectionBoundary::Active).await?;
    let scenario = async {
        let original = async {
            loop {
                if let Some(scope) = fixture.engine.active_invocation_scope()? {
                    break Ok::<_, anyhow::Error>(scope);
                }
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
        }.await?;
        prompt::apply(&fixture.controller, Request {
            id: 803, method: "session.prompt.submit".into(),
            params: json!({"operation_id":"queued-before-control-cancel", "input_id":"retained-control-successor",
                "text":"Queued successor before cancellation", "queued":true}),
        }, &fixture.sink).await?;
        ensure!(fixture.controller.coordinator.submissions.snapshot()?.messages.iter()
            .any(|input| input.id == "retained-control-successor"), "ordinary successor was not durably queued");
        let held = fixture.controller.coordinator.control.clone().acquire_owned().await?;
        let id = CommandId::new("cancel-waits-for-control")?;
        let controller = Arc::clone(&fixture.controller);
        let sink = fixture.sink.clone();
        let started = tokio::time::Instant::now();
        let cancellation = tokio::spawn(async move {
            controller.exchange_owner.execute_identified(CommandRequest {
                id, command: Command::CancelExchange(CancelExchangeRequest::default()),
                input_control: InputControl::AtExecution,
            }, sink).await
        });
        async {
            loop {
                let pending = fixture.events.lock().expect("fixture events poisoned").iter()
                    .any(|(_, event)| event["event"] == "provider_event"
                        && event["payload"]["kind"] == "cleanup_progress"
                        && event["payload"]["data"]["operation_id"] == "cancel-waits-for-control"
                        && event["payload"]["data"]["state"] == "pending");
                if pending { break; }
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
        }.await;
        ensure!(!cancellation.is_finished());
        let waiting = fixture.controller.coordinator.submissions.snapshot()?;
        ensure!(waiting.suspended, "cancellation did not suspend delivery before waiting for control");
        ensure!(waiting.messages.iter().any(|input| input.id == "retained-control-successor"
            && input.delivery() == crate::input::InputDelivery::Queued), "control wait lost the queued successor");
        ensure!(matches!(fixture.dispatches.try_recv(), Err(tokio::sync::mpsc::error::TryRecvError::Empty)),
            "queued input dispatched while cancellation was waiting for control");

        let overdue = cancellation.await?;
        ensure!(matches!(overdue, Err(ExecuteCommandError::Failed(CommandFailure::CollectionTimedOut))));
        ensure!(started.elapsed() >= Duration::from_secs(30) && started.elapsed() < Duration::from_secs(31));
        ensure!(fixture.engine.active_invocation_scope()?.as_ref() == Some(&original),
            "control timeout lost the invocation selected before acquisition");
        ensure!(fixture.collection_count.load(Ordering::Acquire) == 0,
            "native collection ran before control acquisition");
        ensure!(fixture.controller.coordinator.execution.available_permits() == 0,
            "control timeout released a binding with retained execution");
        drop(held);
        async {
            while fixture.engine.active_invocation_scope()?.is_some()
                || fixture.controller.coordinator.execution.available_permits() == 0 {
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
            Ok::<(), anyhow::Error>(())
        }.await?;
        ensure!(fixture.collection_count.load(Ordering::Acquire) == 1);
        let settled = fixture.controller.coordinator.submissions.snapshot()?;
        ensure!(settled.suspended, "late cleanup resumed delivery without an explicit command");
        ensure!(settled.messages.iter().any(|input| input.id == "retained-control-successor"
            && input.delivery() == crate::input::InputDelivery::Queued), "late cleanup consumed the retained successor");
        ensure!(matches!(fixture.dispatches.try_recv(), Err(tokio::sync::mpsc::error::TryRecvError::Empty)),
            "late cleanup automatically dispatched the retained successor");
        fixture.controller.exchange_owner.execute_command(Command::Input(InputCommand::ResumeDelivery)).await?;
        let input = fixture.dispatches.recv().await;
        ensure!(matches!(input, Some(forge_provider_api::input::PromptInput::Text(text))
            if text == "Queued successor before cancellation"));
        ensure!(fixture.collection_count.load(Ordering::Acquire) == 1,
            "retained cancellation targeted the successor invocation");
        Ok::<(), anyhow::Error>(())
    }.await;
    fixture.finish(scenario).await
}
