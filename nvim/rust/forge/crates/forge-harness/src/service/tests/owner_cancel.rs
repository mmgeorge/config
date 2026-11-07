use super::super::native_settings;
use super::super::{HarnessBroker, SessionController, prompt};
use super::bootstrap::initialize;
use super::owner_fixture::{owner_transition_fixture, retain_owner_transition_failure};
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
    Preparing,
    Active,
    /// Hold native execution after the provider echoes the submitted user message, as Codex does.
    ActiveAfterUserEcho,
    /// Hold native execution inside a started shell tool until its deadline stops the turn.
    ActiveInTool,
    PreparingThenActive,
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
    starts: tokio::sync::mpsc::UnboundedReceiver<()>,
    steering_receipts: tokio::sync::mpsc::UnboundedReceiver<forge_provider_api::SubmissionId>,
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
    let (prepared, mut preparation) = tokio::sync::mpsc::unbounded_channel();
    let (started, mut starts) = tokio::sync::mpsc::unbounded_channel();
    let executions = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let steered_submissions = Arc::new(std::sync::Mutex::new(Vec::new()));
    let observed_steering = steered_submissions.clone();
    let (steering_accepted, steering_receipts) = tokio::sync::mpsc::unbounded_channel();
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
                    | CollectionBoundary::PreparingThenActive
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
    let preparations = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    native.with_pre_dispatch(move |_| {
        let first = preparations.fetch_add(1, Ordering::AcqRel) == 0;
        let prepared = prepared.clone();
        async move {
            if first
                && matches!(
                    boundary,
                    CollectionBoundary::Preparing | CollectionBoundary::PreparingThenActive
                )
            {
                let _ = prepared.send(());
                return std::future::pending().await;
            }
            Ok(())
        }
    });
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
    match boundary {
        CollectionBoundary::Preparing | CollectionBoundary::PreparingThenActive => {
            tokio::time::timeout(Duration::from_secs(5), preparation.recv())
                .await?
                .context("first input never reached pre-acceptance preparation")?;
        }
        CollectionBoundary::Active
        | CollectionBoundary::ActiveAfterUserEcho
        | CollectionBoundary::ActiveInTool => {
            tokio::time::timeout(Duration::from_secs(5), starts.recv())
                .await?
                .context("first input never emitted its native start observation")?;
        }
    }
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
        starts,
        steering_receipts,
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
        let teardown = tokio::time::timeout(Duration::from_secs(900), teardown).await;
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
        teardown.context("owner teardown never completed")?
    }
}

/// Preserve native observation across pre-acceptance cancellation and a later active cancellation.
#[tokio::test]
async fn owner_preaccept_cancel_then_active_cancel_preserves_observation_and_next_prompt()
-> Result<()> {
    use crate::exchange::command::{CancelExchangeRequest, CancelledInputPolicy, Command};
    let mut fixture =
        slow_collection_fixture(Duration::ZERO, CollectionBoundary::PreparingThenActive).await?;
    let scenario = async {
        fixture.controller.exchange_owner.execute_command(Command::CancelExchange(
            CancelExchangeRequest { input: CancelledInputPolicy::RestoreIfUnconsumed },
        )).await.context("pre-acceptance cancellation failed")?;
        prompt::apply(&fixture.controller, Request {
            id: 804,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"resume-after-preaccept-cancel","input_id":"resumed-turn-after-preaccept","text":"Resume after early cancellation"}),
        }, &fixture.sink).await?;
        // The output-free first prompt is retracted rather than retained as a cancelled exchange.
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let changed = fixture.events_changed.notified();
                if fixture.events.lock().expect("fixture events poisoned").iter()
                    .any(|(_, event)| event["event"] == "input_changed"
                        && event["payload"]["retracted"] == "prompt-input:slow-first") { break; }
                changed.await;
            }
        }).await.context("pre-acceptance cancellation did not retract its output-free prompt")?;
        tokio::time::timeout(Duration::from_secs(5), fixture.starts.recv())
            .await.context("resumed input never emitted native Started")?
            .context("native start observation channel closed")?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let changed = fixture.events_changed.notified();
                if fixture.engine.active_invocation_scope()?.is_some() { break; }
                changed.await;
            }
            Ok::<(), anyhow::Error>(())
        }).await.context("resumed native Started was never consumed")??;
        // This boundary restores the cancelled text to the caller. Submit another
        // auto-routed input after native start to require real steering acceptance.
        prompt::apply(&fixture.controller, Request {
            id: 805,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"steer-after-preaccept-cancel","input_id":"accepted-steering-after-preaccept","text":"Steer the resumed native execution"}),
        }, &fixture.sink).await?;
        let receipt_id = tokio::time::timeout(Duration::from_secs(5), fixture.steering_receipts.recv())
            .await.context("fresh input never reached native steering")?
            .context("fixture steering receipt channel closed")?;
        let receipt = fixture.controller.broker.lock().await.submission_store
            .submission(receipt_id.as_str())?;
        let mut accepted = receipt.acceptance();
        tokio::time::timeout(Duration::from_secs(5), accepted.wait_for(|accepted| *accepted))
            .await.context("fresh input never reached accepted steering before active cancellation")??;
        let record = receipt.record()?;
        ensure!(record.input_message_id.as_deref() == Some("accepted-steering-after-preaccept")
            && matches!(record.delivery, crate::provider::submission::SubmissionState::Accepted { .. }),
            "native steering did not durably accept the selected fresh input");
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let changed = fixture.events_changed.notified();
                let routes_observed = {
                    let events = fixture.events.lock().expect("fixture events poisoned");
                    let inputs: Vec<_> = events.iter()
                        .filter(|(_, event)| event["event"] == "input_changed")
                        .filter_map(|(_, event)| event["payload"]["upsert"].as_array())
                        .flatten().collect();
                    inputs.iter().any(|input| input["id"] == "resumed-turn-after-preaccept"
                        && input["route"] == "turn")
                        && inputs.iter().any(|input| input["id"] == "accepted-steering-after-preaccept"
                            && input["route"] == "steering")
                };
                if routes_observed { break; }
                changed.await;
            }
        }).await.context("native turn and steering routing were not both observed")?;
        fixture.controller.exchange_owner.execute_command(Command::CancelExchange(
            CancelExchangeRequest { input: CancelledInputPolicy::Retain },
        )).await.context("active cancellation after resumed input failed")?;
        ensure!(fixture.engine.is_healthy().await,
            "active cancellation retired the session observation owner");
        prompt::apply(&fixture.controller, Request {
            id: 806,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"after-two-cancellations","text":"Prompt after both cancellations"}),
        }, &fixture.sink).await?;
        let input = tokio::time::timeout(Duration::from_secs(5), fixture.dispatches.recv())
            .await.context("following prompt was not dispatched")?;
        ensure!(matches!(input, Some(forge_provider_api::input::PromptInput::Text(text))
            if text == "Prompt after both cancellations"));
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let changed = fixture.events_changed.notified();
                let completed = fixture.events.lock().expect("fixture events poisoned")
                    .iter().filter(|(_, event)| event["event"] == "exchange_complete").count();
                if completed >= 2 { break; }
                changed.await;
            }
        }).await.context("following prompt never completed its publication")?;
        ensure!(fixture.engine.is_healthy().await,
            "final publication disconnected native observation");
        ensure!(!fixture.events.lock().expect("fixture events poisoned").iter()
            .any(|(_, event)| event["event"] == "provider_event"
                && event["payload"]["kind"] == "error"),
            "cancellation or final publication emitted a provider observation error");
        Ok::<(), anyhow::Error>(())
    }.await;
    fixture.finish(scenario).await
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
        let exchange_id = tokio::time::timeout(Duration::from_secs(5), async {
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
        })
        .await
        .context("provider user-message echo was never applied")??;
        tokio::time::timeout(
            Duration::from_secs(10),
            fixture
                .controller
                .exchange_owner
                .execute_command(Command::CancelExchange(CancelExchangeRequest {
                    input: CancelledInputPolicy::RestoreIfUnconsumed,
                })),
        )
        .await
        .context("restoring cancellation never settled")?
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
        tokio::time::timeout(Duration::from_secs(5), async {
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
        })
        .await
        .with_context(|| {
            format!(
                "output-free cancellation published no retracted input update: {:?}",
                input_updates()
            )
        })?;
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
        tokio::time::timeout(Duration::from_secs(10), async {
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
        })
        .await
        .context("session stayed unavailable after retraction")??;
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
        tokio::time::timeout(Duration::from_secs(5), async {
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
        })
        .await
        .context("tool deadline never armed")??;
        let failed = tokio::time::timeout(Duration::from_secs(10), async {
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
        })
        .await;
        failed.context("timed-out exchange never settled as failed")??;
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
        tokio::time::timeout(Duration::from_secs(10), async {
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
        })
        .await
        .context("prompt after tool timeout stayed queued")??;
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

/// Collect a delayed cancellation inside its command budget and reopen input.
#[tokio::test(start_paused = true)]
async fn owner_cancel_with_slow_native_collection_settles_and_reopens_input() -> Result<()> {
    use crate::exchange::command::{CancelExchangeRequest, CancelledInputPolicy, Command};
    let mut fixture =
        slow_collection_fixture(Duration::from_secs(20), CollectionBoundary::Preparing).await?;
    let scenario = async {
        fixture
            .controller
            .exchange_owner
            .execute_command(Command::CancelExchange(CancelExchangeRequest {
                input: CancelledInputPolicy::RestoreIfUnconsumed,
            }))
            .await
            .context("cancel failed although native collection was healthy")?;
        prompt::apply(
            &fixture.controller,
            Request {
                id: 801,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"after-slow-cancel","text":"Prompt after slow cancellation"}),
            },
            &fixture.sink,
        )
        .await?;
        let received = tokio::time::timeout(Duration::from_secs(120), fixture.dispatches.recv())
            .await
            .context("session stayed unavailable after a slow cancellation")?;
        ensure!(matches!(
            received,
            Some(forge_provider_api::input::PromptInput::Text(text))
                if text == "Prompt after slow cancellation"
        ));
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish(scenario).await
}

/// A second cancellation during collection joins the first instead of failing as uncertain.
#[tokio::test(start_paused = true)]
async fn owner_second_cancel_during_collection_resolves_with_the_first() -> Result<()> {
    use crate::exchange::command::{CancelExchangeRequest, CancelledInputPolicy, Command};
    let fixture =
        slow_collection_fixture(Duration::from_secs(20), CollectionBoundary::Preparing).await?;
    let scenario = async {
        let command = || {
            Command::CancelExchange(CancelExchangeRequest {
                input: CancelledInputPolicy::Retain,
            })
        };
        let first = fixture.controller.exchange_owner.execute_command(command());
        let second = fixture.controller.exchange_owner.execute_command(command());
        let (first, second) = tokio::time::timeout(Duration::from_secs(120), async {
            tokio::join!(first, second)
        })
        .await
        .context("repeated cancellation never resolved")?;
        first.context("first cancellation failed")?;
        second.context("second cancellation was refused while collection was in flight")?;
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish(scenario).await
}

/// Cancel followed by a model change cancels the exchange, then commits the model.
#[tokio::test]
async fn owner_cancel_then_model_change_cancels_exchange_and_commits_model() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, CancellationOutcome, Command, CommandId, CommandOutcome,
        CommandRequest, ConfigurationOutcome, InputControl,
    };
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    let scenario = async {
        let script = fixture
            .provider
            .mock_response_control()
            .context("mock control missing")?;
        script
            .dispatch(
                &fixture.session,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let cancellation = fixture.controller.exchange_owner.execute_identified(
            CommandRequest {
                id: CommandId::new("owner-cancel-then-model")?,
                command: Command::CancelExchange(CancelExchangeRequest::default()),
                input_control: InputControl::AtExecution,
            },
            fixture.sink.clone(),
        );
        let model = fixture.controller.exchange_owner.execute_identified(
            native_settings::decode_command(&Request {
                id: 43,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"owner-model-after-cancel","action":"configure","config":{"model":"mock-model"}}),
            })?,
            fixture.sink.clone(),
        );
        let (cancellation, model) = tokio::time::timeout(Duration::from_secs(10), async {
            tokio::join!(cancellation, model)
        })
        .await
        .context("cancel then model change did not settle")?;
        let (cancellation, model) = (cancellation?, model?);
        ensure!(matches!(
            cancellation.outcome,
            CommandOutcome::Cancelled(CancellationOutcome::Cancelled { ref exchange })
                if exchange.as_str() == fixture.original
        ));
        ensure!(matches!(
            model.outcome,
            CommandOutcome::Configured(ConfigurationOutcome::Settings { .. })
        ));
        ensure!(
            cancellation.sequence < model.sequence,
            "model change overtook the earlier cancellation"
        );
        ensure!(
            fixture.controller.observer.snapshot()?.session.model == "mock-model",
            "model change was not committed"
        );
        let exchange = SqliteStore::read_only(&fixture.controller.data_root)?
            .load_exchange(&fixture.original)?
            .context("original exchange missing")?;
        ensure!(
            exchange.state() == crate::exchange::ExchangeState::Cancelled,
            "model change resumed the cancelled exchange"
        );
        Ok::<_, anyhow::Error>(())
    }
    .await;
    if let Err(error) = scenario {
        return Err(retain_owner_transition_failure(fixture, error));
    }
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}

/// Retain pre-acceptance cleanup after the cancellation response times out.
#[tokio::test(start_paused = true)]
async fn owner_overdue_cancel_recovers_when_late_collection_completes() -> Result<()> {
    overdue_cancel_recovers(CollectionBoundary::Preparing).await
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
            tokio::time::timeout(Duration::from_secs(5), async {
                while fixture.engine.active_invocation_scope()?.is_none() {
                    tokio::time::sleep(Duration::from_millis(1)).await;
                }
                Ok::<(), anyhow::Error>(())
            }).await.context("overdue fixture never established active native execution")??;
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
        let pending = tokio::time::timeout(Duration::from_secs(11), async {
            loop {
                let pending = fixture.events.lock().expect("fixture events poisoned")
                    .iter().find(|(_, event)| event["event"] == "provider_event"
                        && event["payload"]["kind"] == "cleanup_progress"
                        && event["payload"]["data"]["operation_id"] == operation_id)
                    .cloned();
                if let Some(pending) = pending { break pending; }
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
        }).await.context("cancellation did not publish pending progress at ten seconds")?;
        ensure!(pending.0.duration_since(started) >= Duration::from_secs(10));
        ensure!(pending.1["payload"]["data"]["policy"] == "cancellation");
        ensure!(pending.1["payload"]["data"]["state"] == "pending");
        ensure!(pending.1["payload"]["text"] == "Cancellation still pending");
        ensure!(!cancellation.is_finished(), "pending progress completed the command");
        let overdue = tokio::time::timeout(Duration::from_secs(25), cancellation)
            .await.context("overdue cancel never reported a result")??;
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
        let input = tokio::time::timeout(Duration::from_secs(5), fixture.dispatches.recv())
            .await.context("late settlement did not reopen input")?;
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
        let original = tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if let Some(scope) = fixture.engine.active_invocation_scope()? {
                    break Ok::<_, anyhow::Error>(scope);
                }
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
        }).await.context("fixture invocation never became active before the control barrier")??;
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
        tokio::time::timeout(Duration::from_secs(11), async {
            loop {
                let pending = fixture.events.lock().expect("fixture events poisoned").iter()
                    .any(|(_, event)| event["event"] == "provider_event"
                        && event["payload"]["kind"] == "cleanup_progress"
                        && event["payload"]["data"]["operation_id"] == "cancel-waits-for-control"
                        && event["payload"]["data"]["state"] == "pending");
                if pending { break; }
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
        }).await.context("control wait omitted ten-second cancellation progress")?;
        ensure!(!cancellation.is_finished());
        let waiting = fixture.controller.coordinator.submissions.snapshot()?;
        ensure!(waiting.suspended, "cancellation did not suspend delivery before waiting for control");
        ensure!(waiting.messages.iter().any(|input| input.id == "retained-control-successor"
            && input.delivery() == crate::input::InputDelivery::Queued), "control wait lost the queued successor");
        ensure!(matches!(fixture.dispatches.try_recv(), Err(tokio::sync::mpsc::error::TryRecvError::Empty)),
            "queued input dispatched while cancellation was waiting for control");

        let overdue = tokio::time::timeout(Duration::from_secs(25), cancellation)
            .await.context("control acquisition restarted the cancellation budget")??;
        ensure!(matches!(overdue, Err(ExecuteCommandError::Failed(CommandFailure::CollectionTimedOut))));
        ensure!(started.elapsed() >= Duration::from_secs(30) && started.elapsed() < Duration::from_secs(31));
        ensure!(fixture.engine.active_invocation_scope()?.as_ref() == Some(&original),
            "control timeout lost the invocation selected before acquisition");
        ensure!(fixture.collection_count.load(Ordering::Acquire) == 0,
            "native collection ran before control acquisition");
        ensure!(fixture.controller.coordinator.execution.available_permits() == 0,
            "control timeout released a binding with retained execution");
        drop(held);
        tokio::time::timeout(Duration::from_secs(5), async {
            while fixture.engine.active_invocation_scope()?.is_some()
                || fixture.controller.coordinator.execution.available_permits() == 0 {
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
            Ok::<(), anyhow::Error>(())
        }).await.context("late control acquisition never collected the original invocation")??;
        ensure!(fixture.collection_count.load(Ordering::Acquire) == 1);
        let settled = fixture.controller.coordinator.submissions.snapshot()?;
        ensure!(settled.suspended, "late cleanup resumed delivery without an explicit command");
        ensure!(settled.messages.iter().any(|input| input.id == "retained-control-successor"
            && input.delivery() == crate::input::InputDelivery::Queued), "late cleanup consumed the retained successor");
        ensure!(matches!(fixture.dispatches.try_recv(), Err(tokio::sync::mpsc::error::TryRecvError::Empty)),
            "late cleanup automatically dispatched the retained successor");
        fixture.controller.exchange_owner.execute_command(Command::Input(InputCommand::ResumeDelivery)).await?;
        let input = tokio::time::timeout(Duration::from_secs(5), fixture.dispatches.recv()).await?;
        ensure!(matches!(input, Some(forge_provider_api::input::PromptInput::Text(text))
            if text == "Queued successor before cancellation"));
        ensure!(fixture.collection_count.load(Ordering::Acquire) == 1,
            "retained cancellation targeted the successor invocation");
        Ok::<(), anyhow::Error>(())
    }.await;
    fixture.finish(scenario).await
}
