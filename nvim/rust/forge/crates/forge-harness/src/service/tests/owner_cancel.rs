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
    /// Hold native execution after the provider reports context usage and then echoes the
    /// submitted user message, as Codex does.
    ActiveAfterUserEcho,
    /// Hold the first native open until it is interrupted, as a provider still starting does,
    /// then let the next open succeed and hold its turn as `ActiveAfterUserEcho` does.
    OpenInterruptedThenEcho,
    /// Hold native execution inside a started shell tool until its deadline stops the turn.
    ActiveInTool,
    /// Accept the input without starting its turn, hold it until it is interrupted, then lose the
    /// provider's lifecycle observation and fail the interrupt's collection, as a binding does
    /// when it is invalidated because its interrupt was never answered.
    ActiveLosesObservation,
    /// Hold the prompt before dispatch, as the host's baseline checkpoint does, then fail every
    /// interrupt that collects it, as an adapter does when it cannot prove its turn ended.
    HeldBeforeDispatch,
    /// Hold native execution after its start, then fail every interrupt of the first binding, as
    /// an adapter does when its abort is not answered.
    ActiveFailsInterrupt,
}

/// Hold one real owner at a selected boundary while native collection remains delayed.
struct SlowCollectionFixture {
    provider: crate::provider::test_support::ProviderTestSession,
    controller: Arc<SessionController>,
    engine: Arc<crate::provider::session_execution::SessionExecution>,
    sink: MessageSender,
    reader: tokio::task::JoinHandle<Result<()>>,
    dispatches: tokio::sync::mpsc::UnboundedReceiver<forge_provider_api::input::PromptInput>,
    events: Arc<std::sync::Mutex<Vec<(tokio::time::Instant, serde_json::Value)>>>,
    events_changed: Arc<tokio::sync::Notify>,
    collection_count: Arc<std::sync::atomic::AtomicUsize>,
    /// Completes the native open `OpenInterruptedThenEcho` holds.
    open_release: Arc<tokio::sync::Notify>,
    // Declared last: fields drop in order, and Windows cannot delete files still open.
    root: tempfile::TempDir,
}

/// Start a prompt held before acceptance or after native start, with delayed first collection.
async fn slow_collection_fixture(
    collection_delay: Duration,
    boundary: CollectionBoundary,
) -> Result<SlowCollectionFixture> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let (started, mut starts) = tokio::sync::mpsc::unbounded_channel();
    let held_started = started.clone();
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
                    | CollectionBoundary::OpenInterruptedThenEcho
                    | CollectionBoundary::ActiveInTool
                    | CollectionBoundary::ActiveLosesObservation
                    | CollectionBoundary::ActiveFailsInterrupt
            ) && executions.fetch_add(1, Ordering::AcqRel) == 0;
            let lose_observation =
                held_active && boundary == CollectionBoundary::ActiveLosesObservation;
            let echo_user_message = held_active
                && matches!(
                    boundary,
                    CollectionBoundary::ActiveAfterUserEcho
                        | CollectionBoundary::OpenInterruptedThenEcho
                );
            let run_tool = held_active && boundary == CollectionBoundary::ActiveInTool;
            async move {
                // A binding that is still starting accepts the input without starting its turn.
                if !lose_observation {
                    start.observed(1).await?;
                }
                if echo_user_message {
                    // Ordered delivery applies this report before the echo a scenario waits for.
                    sink.send_wait(forge_provider_api::AdapterActivity::Thread {
                        thread_id: request.thread_id.clone(),
                        event: forge_provider_api::ThreadEvent::ContextUsage(echoed_turn_usage()),
                    })
                    .await?;
                    let item_id = forge_provider_api::ItemId::new("echoed-user-message")
                        .expect("fixture item ID is valid");
                    let text = match request.input.user_input() {
                        Some(forge_provider_api::input::PromptInput::Text(text)) => text.clone(),
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
                    if lose_observation {
                        return Err(crate::provider::ProviderError::Observation(
                            forge_provider_api::ObservationFailure::LifecycleUnavailable,
                        ));
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
                    let _ =
                        dispatched.send(request.input.user_input().cloned().unwrap_or(
                            forge_provider_api::input::PromptInput::Text(String::new()),
                        ));
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
    let open_release = Arc::new(tokio::sync::Notify::new());
    if boundary == CollectionBoundary::OpenInterruptedThenEcho {
        let opens = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let held = held_started.clone();
        let release = open_release.clone();
        native.with_session_open(move |_| {
            let first = opens.fetch_add(1, Ordering::AcqRel) == 0;
            let held = held.clone();
            let release = release.clone();
            async move {
                if first {
                    let _ = held.send(());
                    // A stop detaches from a starting open rather than ending it, so the open
                    // completes only when the scenario releases it.
                    release.notified().await;
                }
                Ok(())
            }
        });
    }
    if boundary == CollectionBoundary::HeldBeforeDispatch {
        let held_prompts = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let held = held_started.clone();
        native.with_pre_dispatch(move |_| {
            let first = held_prompts.fetch_add(1, Ordering::AcqRel) == 0;
            let held = held.clone();
            async move {
                if first {
                    let _ = held.send(());
                    // Only the interrupt ends this hold; the prompt is never dispatched.
                    std::future::pending::<()>().await;
                }
                Ok(())
            }
        });
        // The held turn settles as a rejection its adapter cannot certify, so every interrupt of
        // the first binding fails alike. The restarted provider's binding interrupts normally.
        let held_binding: Arc<std::sync::Mutex<Option<forge_provider_api::BindingId>>> =
            Arc::default();
        native.with_interrupt_settlement(move |binding| {
            let uncertified = held_binding
                .lock()
                .expect("fixture binding lock poisoned")
                .get_or_insert_with(|| binding.clone())
                == &binding;
            async move {
                if uncertified {
                    return Err(forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnInterrupt,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                    .into());
                }
                Ok(())
            }
        });
    }
    if boundary == CollectionBoundary::ActiveFailsInterrupt {
        // The restarted provider's binding interrupts normally.
        let failing_binding: Arc<std::sync::Mutex<Option<forge_provider_api::BindingId>>> =
            Arc::default();
        native.with_interrupt_settlement(move |binding| {
            let failing = failing_binding
                .lock()
                .expect("fixture binding lock poisoned")
                .get_or_insert_with(|| binding.clone())
                == &binding;
            async move {
                if failing {
                    return Err(forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnInterrupt,
                        forge_provider_api::ProviderFailureKind::Transport,
                    )
                    .into());
                }
                Ok(())
            }
        });
    }
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
    if boundary == CollectionBoundary::ActiveLosesObservation {
        // The interrupt itself fails too: its collection cannot prove the native turn ended.
        let failed_collections = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        native.with_cleanup(move |_| {
            let first = failed_collections.fetch_add(1, Ordering::AcqRel) == 0;
            async move {
                if first {
                    return Err(forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnInterrupt,
                        forge_provider_api::ProviderFailureKind::Transport,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Observation,
                        forge_provider_api::FailureDisposition::Invalidated,
                    )
                    .into());
                }
                Ok(())
            }
        });
    }
    let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider.provider_handle());
    // A reopened session already has progress; the next owner must continue its revision.
    crate::exchange::journal::CommandJournal::open(
        &broker.data_root,
        broker.observer().session_id(),
    )?
    .append_core(
        &crate::exchange::core_log::CoreRecord::CancellationProgress {
            progress: crate::exchange::CancellationProgress {
                operation_id: crate::exchange::command::CommandId::new("prior-cancellation")?,
                exchange_id: None,
                revision: 40,
                phase: crate::exchange::CancellationPhase::Settled,
                response_timed_out: false,
                started_at_ms: 1,
                updated_at_ms: 2,
                blockers: Default::default(),
            },
        },
    )?;
    let controller = SessionController::new(broker).await?;
    controller
        .trace
        .configure_default(crate::trace::LogLevel::Trace)?;
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
        .context("first input never reached its held boundary")?;
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
        open_release,
    })
}

/// Get the context usage the fixture provider reports before it echoes the user message.
fn echoed_turn_usage() -> forge_provider_api::context::ContextUsage {
    forge_provider_api::context::ContextUsage::reported(40_000, 200_000)
        .expect("fixture window has capacity")
}

impl SlowCollectionFixture {
    /// Cancel the echoed, output-free prompt with restoration and wait for its retraction.
    ///
    /// Returns the provisional exchange the echo created.
    async fn retract_echoed_prompt(&self) -> Result<String> {
        // The echo precedes the fixture's start signal, so wait until the broker applied it.
        let exchange_id = loop {
            let changed = self.events_changed.notified();
            let echoed = SqliteStore::read_only(&self.controller.data_root)?
                .list_exchange(self.controller.observer.session_id())?
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
                break exchange_id;
            }
            changed.await;
        };
        self.cancel_restoring_prompt().await?;
        Ok(exchange_id)
    }

    /// Count the inputs the owner has published as retracted.
    fn retracted_input_count(&self) -> usize {
        self.events
            .lock()
            .expect("fixture events poisoned")
            .iter()
            .filter(|(_, event)| {
                event["event"] == "input_changed" && event["payload"]["retracted"].is_string()
            })
            .count()
    }

    /// Cancel the running prompt as the editor's `Ctrl+C` does and wait for its retraction.
    async fn cancel_restoring_prompt(&self) -> Result<()> {
        use crate::exchange::command::{CancelExchangeRequest, CancelledInputPolicy, Command};
        let retracted_before = self.retracted_input_count();
        self.controller
            .exchange_owner
            .execute_command(Command::CancelExchange(CancelExchangeRequest {
                input: CancelledInputPolicy::RestoreIfUnconsumed,
            }))
            .await
            .context("restoring cancellation failed")?;
        // Input settlement publishes after the cancellation command commits its outcome.
        loop {
            let changed = self.events_changed.notified();
            if self.retracted_input_count() > retracted_before {
                return Ok(());
            }
            changed.await;
        }
    }

    /// Submit one prompt and wait until the fixture provider runs it to completion.
    async fn run_prompt(&mut self, request_id: u64, text: &str) -> Result<()> {
        prompt::apply(
            &self.controller,
            Request {
                id: request_id,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":format!("prompt-{request_id}"),"text":text}),
            },
            &self.sink,
        )
        .await?;
        loop {
            match self.dispatches.recv().await {
                Some(forge_provider_api::input::PromptInput::Text(dispatched))
                    if dispatched == text =>
                {
                    break;
                }
                Some(_) => {}
                None => anyhow::bail!("fixture dispatch channel closed"),
            }
        }
        let session = self.controller.observer.session_id().to_owned();
        loop {
            let changed = self.events_changed.notified();
            let finished = SqliteStore::read_only(&self.controller.data_root)?
                .list_exchange(&session)?
                .iter()
                .all(crate::exchange::Exchange::is_finished);
            if finished && self.controller.coordinator.execution.available_permits() == 1 {
                return Ok(());
            }
            changed.await;
        }
    }

    /// Require the session to name its native conversation and to admit a `/compact`.
    async fn ensure_conversation_retained(&self) -> Result<()> {
        let broker = self.controller.broker.lock().await;
        ensure!(
            broker.snapshot()?.session.provider_session_id.is_some(),
            "the session ran a turn but never recorded its provider conversation"
        );
        ensure!(
            broker.owner_compaction_available().is_ok(),
            "the session refused compaction: {:?}",
            broker.owner_compaction_available()
        );
        drop(broker);
        let saved = crate::session::SessionStore::load_session(
            &SqliteStore::read_only(&self.controller.data_root)?,
            self.controller.observer.session_id(),
        )?
        .context("fixture session is missing")?;
        ensure!(
            saved.provider_session_id.is_some(),
            "the saved session lost its provider conversation, so a restart starts a new one"
        );
        Ok(())
    }

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
        // A failed scenario can leave an owner that never stops. Bounding its teardown reports
        // the scenario's failure instead of a hang; a paused clock runs the bound out at once.
        let teardown = if scenario.is_err() {
            tokio::time::timeout(Duration::from_secs(120), teardown)
                .await
                .unwrap_or_else(|_| {
                    Err(anyhow::anyhow!("the failed scenario's owner never stopped"))
                })
        } else {
            teardown.await
        };
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
/// The context usage reported during the retracted turn survives the restoration.
#[tokio::test]
async fn owner_cancel_after_user_message_echo_retracts_output_free_prompt() -> Result<()> {
    let mut fixture =
        slow_collection_fixture(Duration::ZERO, CollectionBoundary::ActiveAfterUserEcho).await?;
    let scenario = async {
        let exchange_id = fixture.retract_echoed_prompt().await?;
        ensure!(
            SqliteStore::read_only(&fixture.controller.data_root)?
                .load_exchange(&exchange_id)?
                .is_none(),
            "retracted prompt retained its provisional exchange"
        );
        let restored_session = crate::session::SessionStore::load_session(
            &SqliteStore::read_only(&fixture.controller.data_root)?,
            fixture.controller.observer.session_id(),
        )?
        .context("fixture session is missing")?;
        ensure!(
            restored_session.name.is_empty(),
            "retracted first prompt still names the session: {:?}",
            restored_session.name
        );
        // The report measures the native context rather than the prompt, so restoring the
        // session as it stood before the prompt must not bring back a superseded figure.
        ensure!(
            restored_session.context_usage
                == crate::session::ContextUsage::from_report(&echoed_turn_usage()),
            "retraction replaced the reported context usage with {:?}",
            restored_session.context_usage
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

/// A session keeps its provider conversation after its first prompt was stopped while the
/// provider was still starting.
///
/// The first `Ctrl+C` lands while the native open is still running, so that prompt returns to the
/// composer. The next prompt opens the conversation and is itself stopped and retracted, as a
/// second early `Ctrl+C` does; the conversation it opened stays live and every later turn runs in
/// it. Retraction restored the session as it stood before that prompt, which had no conversation
/// yet, and no later turn reported the identity again: `/compact` was refused for want of a
/// conversation, and a restart resumed none.
#[tokio::test]
async fn first_prompt_stopped_while_the_provider_starts_keeps_the_later_conversation() -> Result<()>
{
    let mut fixture =
        slow_collection_fixture(Duration::ZERO, CollectionBoundary::OpenInterruptedThenEcho)
            .await?;
    let scenario = async {
        fixture.cancel_restoring_prompt().await?;
        fixture.open_release.notify_one();
        prompt::apply(
            &fixture.controller,
            Request {
                id: 801,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"opens-conversation","text":"Second prompt"}),
            },
            &fixture.sink,
        )
        .await?;
        fixture.retract_echoed_prompt().await?;
        fixture.run_prompt(802, "Answered prompt").await?;
        fixture.ensure_conversation_retained().await
    }
    .await;
    fixture.finish(scenario).await
}

/// A session keeps the provider conversation its retracted first prompt opened.
///
/// The open succeeds and the stop lands in the turn itself, so no stop during provider startup is
/// involved: a retraction of whichever prompt opened the conversation used to discard it.
#[tokio::test]
async fn retracted_first_prompt_keeps_the_conversation_it_opened() -> Result<()> {
    let mut fixture =
        slow_collection_fixture(Duration::ZERO, CollectionBoundary::ActiveAfterUserEcho).await?;
    let scenario = async {

        let request = Request { id: 124, method: "session.configure".into(),
            params: json!({"tool_timeout":{"amount":43,"unit":"s"},"approval_timeout":{"amount":45,"unit":"s"}}) };
        let before = fixture.controller.broker.lock().await.snapshot()?.session;
        let command = super::super::native_settings::decode_command(&request)?;
        fixture.controller.exchange_owner.execute_identified(command,fixture.sink.clone()).await?;
        let configured = fixture.controller.broker.lock().await.snapshot()?.session;
        assert_ne!(before.tool_timeout,configured.tool_timeout, "tool timeout was unchanged before retraction");
        assert_ne!(before.approval_timeout,configured.approval_timeout, "approval timeout was unchanged before retraction");
        fixture.retract_echoed_prompt().await?;
        let after = fixture.controller.broker.lock().await.snapshot()?.session;
        assert_eq!(after.approval_timeout,configured.approval_timeout,"retraction lost approval timeout");
        assert_eq!(after.tool_timeout,configured.tool_timeout,"retraction lost the configured tool timeout");
        use crate::session::SessionStore;
        let stored = SqliteStore::read_only(&fixture.controller.data_root)?.load_session(&after.id)?.context("missing session")?;
        assert_eq!(stored.tool_timeout,configured.tool_timeout,"retraction persisted the old tool timeout");
        fixture.run_prompt(802, "Answered prompt").await?;
        fixture.ensure_conversation_retained().await
    }
    .await;
    fixture.finish(scenario).await
}

/// Retain actual native execution cleanup after the cancellation response times out.
#[tokio::test(start_paused = true)]
async fn owner_active_overdue_cancel_recovers_when_late_collection_completes() -> Result<()> {
    overdue_cancel_recovers(CollectionBoundary::Active).await
}

/// Reconfiguration must publish retained cleanup uncertainty without charging it as model work.
#[tokio::test(start_paused = true)]
async fn owner_overdue_reconfiguration_preserves_stopped_clock_and_uncertain_status() -> Result<()>
{
    let mut fixture =
        slow_collection_fixture(Duration::from_secs(40), CollectionBoundary::Active).await?;
    let scenario = async {
        let command = super::super::native_permission::decode_command(&Request {
            id: 810,
            method: "session.command.apply".into(),
            params: json!({"operation_id":"overdue-settings","action":"permission","change":{"kind":"toggle_write_enabled"}}),
        })?;
        let before = fixture.controller.observer.snapshot()?.session.access;
        let result = fixture.controller.exchange_owner
            .execute_identified(command, fixture.sink.clone()).await;
        ensure!(result.is_err(), "unfinished cleanup reported successful configuration");
        ensure!(fixture.controller.observer.snapshot()?.session.access == before,
            "timed-out configuration changed settings");
        let exchange = SqliteStore::open(&fixture.controller.data_root)?
            .list_exchange(fixture.controller.observer.session_id())?
            .pop().context("cleanup lost its exchange")?;
        ensure!(exchange.execution_started_at_ms().is_none(),
            "cleanup timeout left the model execution clock running");
        let retained = serde_json::to_value(&exchange)?;
        ensure!(retained["collection"] == "timed_out",
            "cleanup timeout was not persisted on its original exchange: {retained}");
        let status = crate::session::state_machine::SessionPhase::resolve_status(None, None, Some(&exchange));
        ensure!(matches!(status, crate::session::state_machine::SessionPhase::Stopping {
            collection: crate::exchange::ExchangeCollection::TimedOut
        }) && !status.animated(), "overdue cleanup still advertises active generation");
        ensure!(exchange.elapsed(exchange.created_at_ms + 2_000_000) == exchange.duration_ms(),
            "overdue cleanup kept accumulating display duration");
        ensure!(fixture.controller.coordinator.execution.available_permits() == 0,
            "timeout released the execution owner before native settlement");
        tokio::time::sleep(Duration::from_secs(20)).await;
        ensure!(fixture.collection_count.load(Ordering::Acquire) == 1,
            "late cleanup was collected more than once");
        let collected = SqliteStore::open(&fixture.controller.data_root)?
            .load_exchange(&exchange.id)?.context("late cleanup lost its exchange")?;
        ensure!(collected.collection.is_none(), "late collection retained cleanup uncertainty");
        fixture.controller.exchange_owner.execute_command(
            crate::exchange::command::Command::CancelExchange(Default::default())
        ).await?;
        let settled = SqliteStore::open(&fixture.controller.data_root)?
            .load_exchange(&exchange.id)?.context("settlement lost its exchange")?;
        ensure!(settled.collection.is_none(), "settled exchange retained cleanup uncertainty");
        prompt::apply(&fixture.controller, Request {
            id: 811,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"after-settings-timeout","text":"Fresh work after cleanup"}),
        }, &fixture.sink).await?;
        ensure!(matches!(fixture.dispatches.recv().await,
            Some(forge_provider_api::input::PromptInput::Text(text)) if text == "Fresh work after cleanup"));
        Ok::<(), anyhow::Error>(())
    }.await;
    fixture.finish(scenario).await
}

/// A configuration change whose stop fails releases the exchange its collection parked.
///
/// The change stops the running turn to apply the setting and parks the exchange, labelled
/// `Stopping`, for the turn that runs again. When the interrupt cannot prove the turn ended, the
/// command fails and the owner restarts the provider; nothing then runs the parked exchange or
/// settles it, so its status rested on `Stopping` until the user pressed `Ctrl+C`.
#[tokio::test(start_paused = true)]
async fn failed_reconfiguration_stop_releases_the_parked_exchange() -> Result<()> {
    let mut fixture =
        slow_collection_fixture(Duration::ZERO, CollectionBoundary::ActiveFailsInterrupt).await?;
    let scenario = async {
        // The paused clock runs each bound below out at once when nothing else can progress.
        tokio::time::timeout(Duration::from_secs(120), async {
            while fixture.engine.active_invocation_scope()?.is_none() {
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("the held turn never started")??;
        let command = super::super::native_settings::decode_command(&Request {
            id: 812,
            method: "session.configure".into(),
            params: json!({"effort":"high"}),
        })?;
        let configured = tokio::time::timeout(
            Duration::from_secs(120),
            fixture
                .controller
                .exchange_owner
                .execute_identified(command, fixture.sink.clone()),
        )
        .await
        .context("the configuration change was never answered")?;
        ensure!(
            configured.is_err(),
            "a configuration change whose stop failed reported success"
        );
        let session = fixture.controller.observer.session_id().to_owned();
        let settled = tokio::time::timeout(Duration::from_secs(120), async {
            loop {
                let changed = fixture.events_changed.notified();
                let exchange = SqliteStore::read_only(&fixture.controller.data_root)?
                    .list_exchange(&session)?
                    .into_iter()
                    .next()
                    .context("the stopped exchange disappeared")?;
                if exchange.collection.is_none() && !exchange.is_parked() {
                    break Ok::<_, anyhow::Error>(exchange);
                }
                changed.await;
            }
        })
        .await;
        let exchange = match settled {
            Ok(settled) => settled?,
            Err(_) => {
                let exchange = SqliteStore::read_only(&fixture.controller.data_root)?
                    .list_exchange(&session)?
                    .into_iter()
                    .next()
                    .context("the stopped exchange disappeared")?;
                anyhow::bail!(
                    "the failed configuration left its parked exchange {:?} with collection {:?}",
                    exchange.state(),
                    exchange.collection
                );
            }
        };
        ensure!(
            exchange.state() == crate::exchange::ExchangeState::Failed,
            "the parked exchange of a failed configuration settled {:?}",
            exchange.state()
        );
        ensure!(
            exchange.finalization_error.is_some(),
            "the parked exchange settled failed without stating why"
        );
        prompt::apply(
            &fixture.controller,
            Request {
                id: 813,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"after-failed-configuration","text":"Prompt after the restart"}),
            },
            &fixture.sink,
        )
        .await?;
        let input = tokio::time::timeout(Duration::from_secs(120), fixture.dispatches.recv())
            .await
            .context("the session ran no prompt after the failed configuration")?;
        ensure!(matches!(input, Some(forge_provider_api::input::PromptInput::Text(text))
            if text == "Prompt after the restart"));
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish(scenario).await
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
            Err(ExecuteCommandError::Failed(CommandFailure::StopTimedOut))
        ), "overdue cancellation did not preserve its normalized failure");
        ensure!(started.elapsed() >= Duration::from_secs(30));
        ensure!(started.elapsed() < Duration::from_secs(31), "cancellation restarted its budget");
        ensure!(fixture.controller.coordinator.execution.available_permits() == 0,
            "timeout released execution before native cleanup settled");

        tokio::time::sleep(Duration::from_secs(20)).await;
        ensure!(started.elapsed() >= Duration::from_secs(50));
        ensure!(fixture.collection_count.load(Ordering::Acquire) == 1, "late native cleanup did not remain singular");
        ensure!(fixture.engine.active_invocation_scope()?.is_none(), "native execution remained active after late collection");
        let exchange = SqliteStore::open(&fixture.controller.data_root)?
            .list_exchange(fixture.controller.observer.session_id())?
            .pop().context("late collection lost its exchange")?;
        ensure!(exchange.state() == crate::exchange::ExchangeState::Cancelled,
            "late collection required another cancel to settle: {:?}", exchange.state());
        let progress = fixture.controller.observer.snapshot()?.cancellation
            .context("late cancellation lost its durable progress")?;
        ensure!(progress.revision > 40, "new owner reset the saved cancellation revision");
        ensure!(progress.phase == crate::exchange::CancellationPhase::Settled,
            "late cancellation did not publish settlement");
        ensure!(progress.response_timed_out, "settlement lost the response timeout");
        // Late collection must release child admission without another cancellation command.
        ensure!(!fixture.controller.agent_coordinator.is_quiescing(),
            "the overdue stop retained its child quiescence after it settled");
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

/// Answer every cancellation queued behind a stop that lost the provider.
///
/// A user who sees no response presses cancel again, so several cancellations queue behind the
/// first. When that first stop cannot prove the provider idle, the owner restarts the provider;
/// the queued cancellations must still be answered, and the session must run a later prompt.
///
/// The cancellations ask to restore an unconsumed prompt, as the editor's cancel key does. That
/// policy makes the failed turn wait for the owner's collection before it settles, so the turn
/// must also learn that the collection failed.
#[tokio::test(start_paused = true)]
async fn cancels_queued_behind_a_failed_stop_are_answered() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, CancelledInputPolicy, Command, CommandFailure, CommandId,
        CommandRequest, ExecuteCommandError, InputControl,
    };
    let mut fixture = slow_collection_fixture(
        Duration::from_secs(10),
        CollectionBoundary::ActiveLosesObservation,
    )
    .await?;
    let scenario = async {
        // The paused clock runs this bound out at once if the turn never starts, so a missed
        // start fails with this message instead of spinning until the runner kills the test.
        tokio::time::timeout(Duration::from_secs(120), async {
            while fixture.engine.active_invocation_scope()?.is_none() {
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("the held turn never started")??;
        let cancel = |operation_id: &str| -> Result<_> {
            let command_id = CommandId::new(operation_id)?;
            let controller = Arc::clone(&fixture.controller);
            let sink = fixture.sink.clone();
            Ok(tokio::spawn(async move {
                controller
                    .exchange_owner
                    .execute_identified(
                        CommandRequest {
                            id: command_id,
                            command: Command::CancelExchange(CancelExchangeRequest {
                                input: CancelledInputPolicy::RestoreIfUnconsumed,
                            }),
                            input_control: InputControl::AtExecution,
                        },
                        sink,
                    )
                    .await
            }))
        };
        let first = cancel("first-cancel")?;
        tokio::time::sleep(Duration::from_secs(2)).await;
        let second = cancel("second-cancel")?;
        tokio::time::sleep(Duration::from_secs(1)).await;
        let third = cancel("third-cancel")?;
        ensure!(
            !first.is_finished(),
            "the first stop finished before the later cancellations queued behind it"
        );
        let first = first.await?;
        ensure!(
            matches!(
                first,
                Err(ExecuteCommandError::Failed(CommandFailure::StopFailed))
            ),
            "a stop that lost the provider did not report the failed stop"
        );
        // The paused clock runs this bound out at once when nothing answers the queue.
        let queued = tokio::time::timeout(Duration::from_secs(120), async {
            Ok::<_, anyhow::Error>((second.await?, third.await?))
        })
        .await
        .context("the cancellations queued behind the failed stop were never answered")??;
        for response in [queued.0, queued.1] {
            ensure!(matches!(response,
                Err(ExecuteCommandError::Failed(CommandFailure::StopFailed))),
                "a joined cancellation did not receive the original operation failure");
        }
        prompt::apply(
            &fixture.controller,
            Request {
                id: 803,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"after-failed-stop","text":"Prompt after the restart"}),
            },
            &fixture.sink,
        )
        .await?;
        let input = tokio::time::timeout(Duration::from_secs(120), fixture.dispatches.recv())
            .await
            .context("the session ran no prompt after the failed stop")?;
        ensure!(matches!(input, Some(forge_provider_api::input::PromptInput::Text(text))
            if text == "Prompt after the restart"));
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish(scenario).await
}

/// A stop that fails while the prompt is still held before dispatch leaves the session usable.
///
/// The host holds a prompt before dispatch until its baseline checkpoint is captured. A cancel
/// in that window retires the journal entry and asks the adapter to collect its preparation. When
/// that collection fails, the stop fails and the owner must restart the provider, but the turn
/// has no failure of its own yet: no dispatch was attempted and no terminal event will arrive.
/// The failed collection must therefore end the turn, or the restart waits for a turn that never
/// settles and every later prompt and cancel queues behind it.
///
/// Nothing proved the provider left the prompt unconsumed, so the prompt returns to the queue as
/// failed instead of being restored or resent. Retrying it is the user's decision, and the
/// restarted provider must then run it.
#[tokio::test(start_paused = true)]
async fn failed_stop_of_a_prompt_held_before_dispatch_restarts_the_provider() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, CancelledInputPolicy, Command, CommandFailure, CommandId,
        CommandRequest, ExecuteCommandError, InputControl,
    };
    use crate::input::InputDelivery;
    use crate::protocol::RuntimeMethod;
    let mut fixture =
        slow_collection_fixture(Duration::ZERO, CollectionBoundary::HeldBeforeDispatch).await?;
    let scenario = async {
        // The paused clock runs each bound below out at once when nothing else can progress.
        let stopped = tokio::time::timeout(
            Duration::from_secs(120),
            fixture.controller.exchange_owner.execute_identified(
                CommandRequest {
                    id: CommandId::new("cancel-before-dispatch")?,
                    command: Command::CancelExchange(CancelExchangeRequest {
                        input: CancelledInputPolicy::RestoreIfUnconsumed,
                    }),
                    input_control: InputControl::AtExecution,
                },
                fixture.sink.clone(),
            ),
        )
        .await
        .context("the stop was never answered")?;
        ensure!(
            matches!(
                stopped,
                Err(ExecuteCommandError::Failed(CommandFailure::StopFailed))
            ),
            "a stop whose collection failed did not report the failed stop: {stopped:?}"
        );
        let (held_revision, control_revision) =
            tokio::time::timeout(Duration::from_secs(120), async {
                loop {
                    let changed = fixture.events_changed.notified();
                    let snapshot = fixture.controller.coordinator.submissions.snapshot()?;
                    let held = snapshot
                        .messages
                        .iter()
                        .find(|message| message.id == "prompt-input:slow-first")
                        .context("the cancelled prompt left the queue")?;
                    if held.delivery() == InputDelivery::Failed {
                        break Ok::<_, anyhow::Error>((held.revision, snapshot.control_revision));
                    }
                    changed.await;
                }
            })
            .await
            .context("the turn held before dispatch never settled after the failed stop")??;
        // The cancel paused the queue, so the user retries the prompt and resumes delivery. A
        // session that never leaves the stop holds both requests, so the bound covers them.
        let input = tokio::time::timeout(Duration::from_secs(120), async {
            for (request_id, params) in [
                (
                    804,
                    json!({"id":"retry-held-prompt","action":{
                        "kind":"retry","id":"prompt-input:slow-first","revision":held_revision}}),
                ),
                (
                    805,
                    json!({"id":"resume-queue","control_revision":control_revision,
                        "action":{"kind":"resume"}}),
                ),
            ] {
                fixture
                    .controller
                    .input_request(
                        Request {
                            id: request_id,
                            method: "session.input.apply".into(),
                            params,
                        },
                        RuntimeMethod::SessionInputApply,
                        &fixture.sink,
                    )
                    .await?;
            }
            Ok::<_, anyhow::Error>(fixture.dispatches.recv().await)
        })
        .await
        .context("the restarted provider ran no prompt after the failed stop")??;
        ensure!(
            matches!(&input, Some(forge_provider_api::input::PromptInput::Text(text))
                if text == "First prompt"),
            "the restarted provider ran another prompt: {input:?}"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish(scenario).await
}

/// Keep the queue running when `Ctrl-C` arrives with nothing to stop.
///
/// The status row can still read `Working` for a moment after a turn ends, so an idle session
/// receives interrupts. With no exchange running and no task to pause, such a cancel has nothing
/// to stop; pausing the queue there stranded a message held for editing, which nothing resent
/// once the edit was saved.
#[tokio::test]
async fn idle_cancel_leaves_a_held_follow_up_deliverable() -> Result<()> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let controller =
        SessionController::new(HarnessBroker::initialize(serde_json::from_value(json!({
            "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"idle-cancel",
            "provider":{"kind":"mock","command":[]},
        }))?)?)
        .await?;
    let session = controller.observer.session_id().to_owned();
    // Queue and reserve the follow-up before input dispatch starts, as an editor holding it
    // open in the composer does.
    let input = &controller.coordinator.submissions;
    input.apply(InputOperation {
        id: "submit-held".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: "held".into(),
                text: "Original follow-up".into(),
                purpose: crate::input::InputPurpose::User,
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Queue,
            })),
        },
    })?;
    input.apply(InputOperation {
        id: "reserve-held".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Reserve {
            id: "held".into(),
            revision: 1,
            reservation: "editor".into(),
        },
    })?;
    let control = controller
        .provider_handle()
        .mock_response_control()
        .context("mock control")?;
    let hold = control
        .dispatch(
            &session,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<_, anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    super::super::cancellation::apply(
        &controller,
        &Request {
            id: 1,
            method: "session.turn.cancel".into(),
            params: json!({"operation_id":"idle-cancel","restore_prompt_if_no_output":true}),
        },
        &sink,
    )
    .await?;
    ensure!(
        !input.snapshot()?.suspended,
        "a cancel with nothing running paused the queue"
    );
    let revision = input.snapshot()?.messages[0].revision;
    controller
        .input_request(
            Request {
                id: 2,
                method: "session.input.apply".into(),
                params: json!({"id":"commit-held","action":{"kind":"commit","reservation":"editor",
                    "edits":[{"id":"held","revision":revision,"text":"Edited follow-up","config":null}]}}),
            },
            crate::protocol::RuntimeMethod::SessionInputApply,
            &sink,
        )
        .await?;
    let delivered = control
        .dispatch(
            &session,
            MockResponseCommand::Wait {
                id: hold["id"].as_str().context("hold identity")?.into(),
            },
        )
        .await?;
    ensure!(
        delivered["prompt"]
            .as_str()
            .is_some_and(|prompt| prompt.contains("Edited follow-up")),
        "the saved follow-up never reached the provider: {delivered}"
    );
    control
        .dispatch(
            &session,
            MockResponseCommand::Release {
                id: hold["id"].as_str().context("hold identity")?.into(),
            },
        )
        .await?;
    controller.finish_exchange_owner().await?;
    drop(sink);
    reader.abort();
    Ok(())
}

////// Own one mock-provider session whose scenario holds work at a chosen boundary.
struct MockOwnerFixture {
    controller: Arc<SessionController>,
    session: String,
    sink: MessageSender,
    reader: tokio::task::JoinHandle<Result<()>>,
    /// Holds a writing child in repository admission until the scenario releases it.
    reservation: Option<crate::repository_access::RepositoryReservation>,
    // Declared last: fields drop in order, and Windows cannot delete files still open.
    root: tempfile::TempDir,
}

impl MockOwnerFixture {
    /// Start an owner on the mock provider with the agent tools enabled.
    async fn start(client_id: &str) -> Result<Self> {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let controller =
            SessionController::new(HarnessBroker::initialize(serde_json::from_value(json!({
                "workspace":root.path(),"data_root":root.path().join("data"),"client_id":client_id,
                "provider":{"kind":"mock","command":[]},
            }))?)?)
            .await?;
        let session = controller.observer.session_id().to_owned();
        controller.agent_coordinator.set_enabled(true);
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<_, anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        Ok(Self {
            controller,
            session,
            sink,
            reader,
            reservation: None,
            root,
        })
    }

    fn control(&self) -> Result<crate::provider::mock_response::MockResponseControl> {
        self.controller
            .provider_handle()
            .mock_response_control()
            .context("mock control")
    }

    /// Queue one user prompt, as the editor's composer does.
    fn submit(&self, text: &str) -> Result<()> {
        use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
        self.controller
            .coordinator
            .submissions
            .apply(InputOperation {
                id: "submit-prompt".into(),
                control_revision: 0,
                after_control: None,
                action: InputAction::Submit {
                    message: Box::new(InputMessage::queued(crate::input::InputDraft {
                        id: "prompt".into(),
                        text: text.into(),
                        purpose: crate::input::InputPurpose::User,
                        attachments: Vec::new(),
                        config: None,
                        intent: InputIntent::Auto,
                    })),
                },
            })?;
        Ok(())
    }

    /// Delegate one child, then wait until the parent's own turn has finished and parked for it.
    async fn park_parent_for_child(&mut self) -> Result<()> {
        use crate::provider::mock_response::MockResponseCommand;
        use forge_provider_api::testing::{
            TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
            TestPromptTerminal,
        };
        // Retain an exclusive repository operation while the parent parks for its child.
        self.reservation = Some(crate::repository_access::RepositoryReservation::acquire(
            &self.controller.data_root,
            self.root.path(),
            crate::repository_access::RepositoryAccess::Write,
        )?);
        self.control()?
            .dispatch(
                &self.session,
                MockResponseCommand::Script {
                    script: TestPromptScript {
                        acceptance: TestInputAcceptance::BeforeResponse,
                        steps: vec![
                            TestPromptStep::ToolCall {
                                name: "agent_spawn".into(),
                                arguments: json!({"profile":"worker","text":"Inspect the sources"}),
                            },
                            TestPromptStep::Message {
                                content: TestMessageContent::Response,
                                text: "Waiting for the child".into(),
                            },
                        ],
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await?;
        self.submit("Delegate an inspection")?;
        loop {
            let parked = self.controller.agent_coordinator.active_count() == 1
                && crate::storage::HistoryReader::current(&self.controller.data_root)?
                    .list_exchange(&self.session)?
                    .first()
                    .and_then(|exchange| exchange.turn.last())
                    .is_some_and(|turn| {
                        matches!(turn.state(), crate::turn::TurnState::Finished { .. })
                    });
            if parked {
                return Ok(());
            }
            tokio::task::yield_now().await;
        }
    }

    /// Submit a prompt and wait until the provider holds its running turn.
    async fn hold_running_turn(&self) -> Result<()> {
        use crate::provider::mock_response::MockResponseCommand;
        let control = self.control()?;
        let hold = control
            .dispatch(
                &self.session,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        self.submit("Inspect the sources")?;
        control
            .dispatch(
                &self.session,
                MockResponseCommand::Wait {
                    id: hold["id"].as_str().context("hold identity")?.into(),
                },
            )
            .await?;
        Ok(())
    }

    /// Read the session's exchanges from durable history.
    fn exchanges(&self) -> Result<Vec<crate::exchange::Exchange>> {
        SqliteStore::read_only(&self.controller.data_root)?.list_exchange(&self.session)
    }

    /// Stop the owner as the editor's exit does, then reopen the session's history.
    async fn exit_and_reopen(mut self) -> Result<Vec<crate::exchange::Exchange>> {
        self.controller.finish_exchange_owner().await?;
        self.reservation.take();
        let data_root = self.controller.data_root.clone();
        let session = self.session.clone();
        drop(self.sink);
        self.reader.abort();
        drop(self.controller);
        let mut store = SqliteStore::open(&data_root)?;
        store.interrupt_detached_execution(&session)?;
        let exchanges = store.list_exchange(&session)?;
        drop(store);
        drop(self.root);
        Ok(exchanges)
    }

    /// Release the held child and stop the owner.
    async fn finish(mut self) -> Result<()> {
        self.reservation.take();
        self.controller.finish_exchange_owner().await?;
        drop(self.sink);
        self.reader.abort();
        Ok(())
    }
}

/// Describe an exchange's state, finalization error, and turn states for a failed assertion.
fn exchange_summary(exchange: &crate::exchange::Exchange) -> String {
    format!(
        "{:?}: {:?} {:?}",
        exchange.state(),
        exchange.finalization_error,
        exchange
            .turn
            .iter()
            .map(|turn| format!("{:?}", turn.state()))
            .collect::<Vec<_>>()
    )
}

/// A cancel that arrives while the parent waits for its children settles the exchange cancelled.
///
/// The parent's own turn had already completed, so the cancel woke its parked result while the
/// children still ran: finalization then failed on the unsettled delegation tree, or, once the
/// children had settled, recorded the completed result and the exchange read as complete.
#[tokio::test]
async fn cancel_while_the_parent_waits_for_children_settles_cancelled() -> Result<()> {
    let mut fixture = MockOwnerFixture::start("wait-cancel").await?;
    fixture.park_parent_for_child().await?;
    super::super::cancellation::apply(
        &fixture.controller,
        &Request {
            id: 1,
            method: "session.turn.cancel".into(),
            params: json!({"operation_id":"wait-cancel"}),
        },
        &fixture.sink,
    )
    .await?;
    super::owner_transition::settle_exchanges(&fixture.controller, &fixture.session, 1).await?;
    let exchanges = fixture.exchanges()?;
    ensure!(
        exchanges[0].state() == crate::exchange::ExchangeState::Cancelled,
        "the interrupted parent settled {}",
        exchange_summary(&exchanges[0])
    );
    // A stop label on a settled exchange outlives the stop: the heading reads `Stopping` forever.
    ensure!(
        exchanges[0].collection.is_none(),
        "the settled parent kept the stop label {:?}",
        exchanges[0].collection
    );
    fixture.finish().await
}

/// Turning the agent tools off while a child runs applies the setting at once and stops nothing.
///
/// The toggle changes the root's tool set, which the provider receives when a turn starts. A
/// restart of the parked parent finalized it over its running child and failed the exchange on
/// the descendant guard, so the change waits for the next turn start and the child's result
/// still continues the parent.
#[tokio::test]
async fn agent_tools_off_while_a_child_runs_keeps_the_exchange_running() -> Result<()> {
    let mut fixture = MockOwnerFixture::start("tools-off").await?;
    fixture.park_parent_for_child().await?;
    let command = super::super::native_settings::decode_command(&Request {
        id: 1,
        method: "session.configure".into(),
        params: json!({"subagent_tools":false}),
    })?;
    fixture
        .controller
        .exchange_owner
        .execute_identified(command, fixture.sink.clone())
        .await
        .context("the agent tools toggle failed")?;
    ensure!(
        !fixture.controller.agent_coordinator.enabled(),
        "the agent tools stayed enabled after the toggle completed"
    );
    ensure!(
        fixture.controller.agent_coordinator.active_count() == 1,
        "the toggle stopped the running child"
    );
    fixture.reservation.take();
    super::owner_transition::settle_exchanges(&fixture.controller, &fixture.session, 1).await?;
    let exchanges = fixture.exchanges()?;
    ensure!(
        exchanges[0].state() == crate::exchange::ExchangeState::Complete
            && exchanges[0].turn.len() == 2,
        "the parent did not continue with its child's result: {}",
        exchange_summary(&exchanges[0])
    );
    fixture.finish().await
}

/// A reasoning change while the parent waits for its children stops and restarts the turn.
///
/// The parent's own turn had already completed and parked for its children. The restart woke
/// that parked result while the children still ran, finalization then refused to capture a
/// checkpoint over the unsettled delegation tree, the exchange failed, and the change was lost.
/// The change instead cancels and settles the children first, runs the parent again with the
/// new setting, and commits the setting.
#[tokio::test]
async fn reasoning_change_while_the_parent_waits_for_children_restarts_the_turn() -> Result<()> {
    let mut fixture = MockOwnerFixture::start("wait-configure").await?;
    fixture.park_parent_for_child().await?;
    let command = super::super::native_settings::decode_command(&Request {
        id: 1,
        method: "session.configure".into(),
        params: json!({"effort":"high"}),
    })?;
    fixture
        .controller
        .exchange_owner
        .execute_identified(command, fixture.sink.clone())
        .await
        .context("the reasoning change failed")?;
    ensure!(
        fixture.controller.agent_coordinator.active_count() == 0,
        "the reasoning change left the child running"
    );
    fixture.reservation.take();
    super::owner_transition::settle_exchanges(&fixture.controller, &fixture.session, 1).await?;
    let effort = fixture
        .controller
        .broker
        .lock()
        .await
        .snapshot()?
        .session
        .effort;
    ensure!(
        effort == "high",
        "the reasoning change was lost: {effort:?}"
    );
    let exchanges = fixture.exchanges()?;
    ensure!(
        exchanges[0].state() == crate::exchange::ExchangeState::Complete
            && exchanges[0].turn.len() == 2,
        "the parent did not run again with the new setting: {}",
        exchange_summary(&exchanges[0])
    );
    ensure!(
        exchanges[0].collection.is_none(),
        "the restarted parent kept the stop label {:?}",
        exchanges[0].collection
    );
    fixture.finish().await
}

/// Keep a queued prompt when shutdown wins before preparation can publish a foreground.
#[tokio::test]
async fn exit_during_input_preparation_keeps_the_unsent_prompt_queued() -> Result<()> {
    let fixture = MockOwnerFixture::start("exit-preparation").await?;
    let generation = fixture.controller.provider_lane.write().await;
    fixture.submit("Keep this unsent prompt")?;
    let queue = &fixture.controller.coordinator.submissions;
    while !queue.snapshot()?.messages.iter().any(|message| {
        message.delivery() == crate::input::InputDelivery::Delivering
    }) {
        tokio::task::yield_now().await;
    }
    fixture.controller.exchange_owner.close().await;
    while fixture.controller.exchange_owner.input_ready(fixture.sink.clone()).await.is_ok() {}
    drop(generation);
    tokio::time::timeout(Duration::from_secs(1), fixture.controller.finish_exchange_owner())
        .await
        .context("the closed owner waited on a new foreground handoff")??;

    let snapshot = queue.snapshot()?;
    assert_eq!(snapshot.messages.len(), 1);
    assert_eq!(snapshot.messages[0].delivery(), crate::input::InputDelivery::Queued);
    assert!(fixture.exchanges()?.is_empty());
    fixture.finish().await
}

/// Exiting the editor during a running turn leaves the reopened exchange cancelled.
#[tokio::test]
async fn editor_exit_during_a_running_turn_reopens_cancelled() -> Result<()> {
    let fixture = MockOwnerFixture::start("exit-running").await?;
    fixture.hold_running_turn().await?;
    let exchanges = fixture.exit_and_reopen().await?;
    ensure!(
        exchanges[0].state() == crate::exchange::ExchangeState::Cancelled,
        "the exchange the exit interrupted reopened {}",
        exchange_summary(&exchanges[0])
    );
    Ok(())
}

/// Exiting the editor while the parent waits for its children leaves the reopened exchange
/// cancelled, as an explicit cancel does.
#[tokio::test]
async fn editor_exit_while_the_parent_waits_for_children_reopens_cancelled() -> Result<()> {
    let mut fixture = MockOwnerFixture::start("exit-waiting").await?;
    fixture.park_parent_for_child().await?;
    let exchanges = fixture.exit_and_reopen().await?;
    ensure!(
        exchanges[0].state() == crate::exchange::ExchangeState::Cancelled,
        "the exchange the exit interrupted reopened {}",
        exchange_summary(&exchanges[0])
    );
    Ok(())
}

/// An exit whose shutdown stop outlives the host still reopens the exchange cancelled.
///
/// The shutdown stop is a cancellation, but recovery saw only a running exchange with a pending
/// collection, which a reconfiguration's restart leaves as well, and interrupted it.
#[tokio::test]
async fn editor_exit_that_outlives_the_host_reopens_cancelled() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = MockOwnerFixture::start("exit-unfinished").await?;
    fixture.hold_running_turn().await?;
    fixture
        .control()?
        .dispatch(&fixture.session, MockResponseCommand::HoldInterrupt)
        .await?;
    let controller = fixture.controller.clone();
    let shutdown = tokio::spawn(async move { controller.finish_exchange_owner().await });
    let collecting = loop {
        let exchange = fixture.exchanges()?.into_iter().next();
        if let Some(exchange) = exchange.filter(|exchange| exchange.collection.is_some()) {
            break exchange;
        }
        tokio::task::yield_now().await;
    };
    shutdown.abort();
    let mut recovered = collecting.clone();
    crate::storage::interrupt_detached_exchange(
        &mut recovered,
        &crate::session::HarnessSession::primary_agent_id(&fixture.session),
        crate::storage::DetachedSettlement::Outcome,
    )?;
    ensure!(
        recovered.state() == crate::exchange::ExchangeState::Cancelled,
        "the exchange the exit was stopping reopened {}",
        exchange_summary(&recovered)
    );
    Ok(())
}

/// A request that reaches the owner during a burst of ready turn events is taken between those
/// events rather than after the burst.
///
/// The owner applies a bounded run of ready events in one update step and received nothing from
/// its inbox until that run ended. On Copilot each child event took about 40 ms to apply, so the
/// agents picker's interrupt waited 742 ms and reached a child that had already completed.
#[tokio::test]
async fn owner_takes_a_request_between_the_events_of_a_burst() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestHoldId, TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    const BURST: usize = 24;
    let fixture = MockOwnerFixture::start("request-burst").await?;
    let mut steps = vec![TestPromptStep::Barrier {
        id: TestHoldId::new("before-burst")?,
    }];
    steps.extend((0..BURST).map(|index| TestPromptStep::Message {
        content: TestMessageContent::Commentary,
        text: format!("Progress note {index}"),
    }));
    fixture
        .control()?
        .dispatch(
            &fixture.session,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps,
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    fixture.submit("Report progress")?;
    let control = fixture.control()?;
    control
        .dispatch(
            &fixture.session,
            MockResponseCommand::Wait {
                id: "before-burst".into(),
            },
        )
        .await?;
    control
        .dispatch(
            &fixture.session,
            MockResponseCommand::Release {
                id: "before-burst".into(),
            },
        )
        .await?;
    let applied = || -> Result<usize> {
        Ok(fixture
            .exchanges()?
            .first()
            .and_then(|exchange| exchange.turn.last())
            .map_or(0, |turn| turn.messages().len()))
    };
    // Send the request once the owner has started applying the burst.
    let before = loop {
        let applied = applied()?;
        if applied > 0 {
            break applied;
        }
        tokio::task::yield_now().await;
    };
    fixture
        .controller
        .exchange_owner
        .command_status(crate::exchange::command::CommandId::new("burst-status")?)
        .await?;
    // The request may wait for the one event being applied when it arrived, never for more.
    let after = applied()?;
    ensure!(
        after <= before + 1,
        "the owner answered after the burst went on: {before} of {BURST} messages applied when \
         the request arrived, {after} when it was answered"
    );
    fixture.finish().await
}
