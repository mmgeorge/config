use super::*;

/// Replacing the current invocation must never reset an older worker's cancellation.
#[tokio::test]
async fn cancellation_signal_remains_bound_to_its_invocation() {
    let controller = TurnCancellation::new();
    let first = controller.capture();
    controller.request();
    controller.arm();
    let second = controller.capture();
    assert!(first.requested.load(Ordering::Acquire));
    assert!(first.submission_cancelled.load(Ordering::Acquire));
    assert!(!second.requested.load(Ordering::Acquire));
    first.request();
    assert!(!second.requested.load(Ordering::Acquire));
    controller.request();
    assert!(second.requested.load(Ordering::Acquire));
    first.cancelled().await;
    second.cancelled().await;
}
use crate::provider::{ProviderCompletion, TurnEvent};
use crate::turn::{ToolState, TurnOutcome, TurnState};
use futures_util::FutureExt;
use std::sync::atomic::AtomicUsize;

/// Preserve multiple bounded observation windows and await their final publication before readiness.
#[tokio::test]
#[ignore = "exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md"]
async fn foreground_prefix_transfer_preserves_order_and_waits_for_last_publication() -> Result<()> {
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        let workspace = super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.checkpoint = None;
        broker.turn_cancellation.arm();
        let (publication, _updates) = publication::channel();
        let ForegroundDispatch::Admitting(admission) = broker.begin_foreground_dispatch(
            Request {
                id: 1,
                method: "session.prompt.submit".into(),
                params: json!({"text":"Observe every frame"}),
            },
            publication,
        )?
        else {
            anyhow::bail!("fixture did not create foreground admission");
        };
        let mut staged = broker.stage_foreground_interaction(
            "Observe every frame",
            PromptMode::Chat,
            Some(&ExchangeAdmission::chat("Observe every frame".into())),
            ProviderInput::from_text("Observe every frame"),
        )?;
        broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
        let engine = broker
            .execution_registry()
            .owner(crate::provider::ThreadOwner::root(&broker.session.id))
            .await?;
        let mut transaction = broker.construct_foreground_transaction(&staged, engine.clone())?;
        // Replace the never-polled submission with accepted observation fixtures, retaining its real host owner.
        transaction.native.pending_submission.take();
        transaction.native.task.abort_all();
        while let Some(result) = transaction.native.task.join_next().await {
            match result {
                Err(error) => assert!(error.is_cancelled()),
                Ok(_) => anyhow::bail!("replacement observation fixture dispatched its submission"),
            }
        }
        let scope = transaction.state.scope.clone();
        let (native, stream) = crate::provider::events::channel(scope.clone());
        transaction.native.events = stream;
        transaction.native_open = false;
        transaction.session_events =
            Arc::new(crate::provider::session_execution::SessionObservationInbox::default());
        transaction.question_enabled = false;
        transaction.execution_open = false;
        transaction.agent_revision.borrow_and_update();
        let turn = crate::provider::TurnAddress {
            thread_id: (scope).owner().thread_id(),
            turn_id: (scope).turn_id().unwrap(),
            scope,
        };
        let mut frames = vec![ProviderEvent::Turn {
            thread_id: (turn.clone()).thread_id.clone(),
            turn_id: (turn.clone()).turn_id.clone(),
            event: TurnEvent::Started(forge_provider_api::turn::TurnStarted { started_at_ms: 1 }),
        }];
        let mut expected = String::new();
        // Keep the native-channel prefix non-coalescible so it fills the same staging budget.
        for index in 0..80 {
            let text = format!("prefix-{index:03}: α🦀\n");
            expected.push_str(&text);
            frames.extend(crate::provider::test_support::message(
                &turn,
                format!("prefix-{index}"),
                Some(crate::provider::TestMessageContent::Response),
                text,
            ));
        }
        let message = (
            turn.clone(),
            forge_provider_api::ItemId::new("streamed-prefix").unwrap(),
        );
        frames.push(ProviderEvent::Item {
            thread_id: (message.clone()).0.thread_id.clone(),
            turn_id: (message.clone()).0.turn_id.clone(),
            item_id: (message.clone()).1.clone(),
            event: forge_provider_api::ItemEvent::Started(
                forge_provider_api::ItemStart::AgentMessage(
                    forge_provider_api::AgentMessageStart {
                        text: "stream:\n".into(),
                        classification: forge_provider_api::message::MessageClassification::Known(
                            forge_provider_api::AgentMessageRole::Response,
                        ),
                    },
                ),
            ),
        });
        let mut streamed = String::from("stream:\n");
        for index in 0..(forge_protocol::MAX_PENDING_FRAMES - 64) {
            let text = format!("frame-{index:03}: α🦀\n");
            streamed.push_str(&text);
            frames.push(ProviderEvent::Item {
                thread_id: (message.clone()).0.thread_id.clone(),
                turn_id: (message.clone()).0.turn_id.clone(),
                item_id: (message.clone()).1.clone(),
                event: forge_provider_api::ItemEvent::Message {
                    event: forge_provider_api::MessageItemEvent::Delta(text),
                },
            });
        }
        frames.push(ProviderEvent::Item {
            thread_id: (message).0.thread_id.clone(),
            turn_id: (message).0.turn_id.clone(),
            item_id: (message).1.clone(),
            event: forge_provider_api::ItemEvent::Completed(
                forge_provider_api::ItemCompletion::AgentMessage(
                    forge_provider_api::MessageCompletion {
                        text: streamed.clone(),
                        outcome: forge_provider_api::MessageOutcome::Completed,
                    },
                ),
            ),
        });
        frames.push(ProviderEvent::Turn {
            thread_id: (turn.clone()).thread_id.clone(),
            turn_id: (turn.clone()).turn_id.clone(),
            event: forge_provider_api::TurnEvent::Completed(
                crate::provider::test_support::completed_turn(
                    (turn.clone()).scope.clone(),
                    forge_provider_api::TurnResult::new(
                        TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        },
                        2,
                        None,
                        Vec::new(),
                        Vec::new(),
                        None,
                    )
                    .unwrap(),
                )
                .unwrap(),
            ),
        });
        expected.push_str(&streamed);
        let frames: Vec<_> = frames
            .into_iter()
            .map(|event| crate::provider::test_support::native_event(turn.scope.clone(), event))
            .collect();
        transaction.pending_session_observations.extend(
            frames[..64]
                .iter()
                .cloned()
                .map(crate::provider::session_execution::OwnerObservation::Event),
        );
        for event in &frames[64..160] {
            native.send(event.clone())?;
        }
        for event in &frames[160..] {
            transaction.session_events.push(event.clone())?;
        }
        transaction.pending_native_ready = Some(Ok(ProviderCompletion::default()));
        broker.collect_foreground_observations(&mut transaction)?;
        assert_eq!(
            transaction.pending_session_observations.len(),
            forge_protocol::MAX_PENDING_FRAMES
        );
        assert!(
            matches!(
                transaction.next().now_or_never(),
                Some(Ok(ForegroundSignal::SessionObservationsReady))
            ),
            "a full staging prefix must be applied before reading another frame"
        );
        let mut dispatch = ForegroundDispatch::Running {
            transaction: Box::new(transaction),
            admission,
        };
        let mut action = ForegroundAction::Wait;
        let mut publications = 0;
        loop {
            action = match action {
                ForegroundAction::Wait => {
                    let ForegroundDispatch::Running { transaction, .. } = &mut dispatch else {
                        anyhow::bail!("prefix application changed foreground ownership");
                    };
                    let signal = transaction.next().await?;
                    if matches!(
                        &signal,
                        ForegroundSignal::Native(NativeForegroundSignal::NativeReady(_))
                    ) {
                        assert_eq!(
                            publications,
                            frames.len(),
                            "native readiness bypassed its accepted prefix"
                        );
                        assert!(!transaction.observation_publication_in_flight);
                        let messages: String = transaction
                            .state
                            .interaction
                            .turn
                            .iter()
                            .flat_map(|turn| turn.messages())
                            .map(|message| message.text())
                            .collect();
                        assert_eq!(
                            messages, expected,
                            "bounded transfer changed native bytes/order"
                        );
                        assert_eq!(
                            transaction
                                .state
                                .interaction
                                .turn
                                .last()
                                .context("missing native turn")?
                                .state(),
                            TurnState::Finished {
                                outcome: TurnOutcome::Completed {
                                    stop_reason:
                                        forge_provider_api::turn::TurnCompletionReason::Normal
                                }
                            }
                        );
                        break;
                    }
                    broker.step_foreground(&mut dispatch, signal)?
                }
                ForegroundAction::Effect(job @ ForegroundJob::ObservationPublication(_)) => {
                    let ForegroundDispatch::Running { transaction, .. } = &mut dispatch else {
                        anyhow::bail!("publication lost its foreground owner");
                    };
                    assert!(transaction.observation_publication_in_flight);
                    assert!(
                        transaction.next().now_or_never().is_none(),
                        "native result escaped before publication acknowledgment"
                    );
                    let signal = match job.run().await? {
                        ForegroundSignal::ObservationPublicationNeedsForward { events } => {
                            ForegroundSignal::ObservationPublicationDelivered(Ok(events))
                        }
                        signal => signal,
                    };
                    publications += 1;
                    broker.step_foreground(&mut dispatch, signal)?
                }
                _ => {
                    anyhow::bail!("unexpected foreground effect while transferring accepted prefix")
                }
            };
        }
        drop(native);
        engine.close().await?;
        Ok(())
    })
    .await
    .context("bounded foreground prefix did not settle")?
}

#[derive(Clone, Copy)]
enum ProviderFailure {
    Cancelled,
    SimilarMessage,
    Stalled,
}

struct CancellationFixtureStats {
    calls: Arc<AtomicUsize>,
    cancellations: Arc<AtomicUsize>,
}

fn cancellation_fixture(
    failure: ProviderFailure,
    stats: Arc<CancellationFixtureStats>,
) -> Arc<crate::provider::test_support::EventFixtureProvider> {
    // Managed skill preparation resolves the descriptor's provider identity through the registry.
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id()
            .expect("registry supplies a fixture identity"),
    )
    .expect("fixture provider is registered");
    crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, mut cancelled| {
            let stats = stats.clone();
            async move {
                let native_outcome;
                let native_completed_at_ms;
                start.observed(0).await?;

                // Match native adapters: await the host checkpoint before producing turn/tool work.
                request.turn_control.before_send(&request.scope).await?;
                let call = stats.calls.fetch_add(1, Ordering::SeqCst);
                let turn = crate::provider::TurnAddress {
                    thread_id: (request.scope.clone()).owner().thread_id(),
                    turn_id: (request.scope.clone()).turn_id().unwrap(),
                    scope: request.scope.clone(),
                };
                if let Some(history) = Some(
                    "fixture-thread"
                        .to_owned()
                        .try_into()
                        .expect("fixture ID is valid"),
                ) {
                    sink.send_wait(forge_provider_api::AdapterActivity::Thread {
                        thread_id: request.thread_id.clone(),
                        event: forge_provider_api::ThreadEvent::IdentityResolved(history),
                    })
                    .await?;
                }

                if call > 0 {
                    for event in crate::provider::test_support::message(
                        &turn,
                        "recovered",
                        Some(crate::provider::TestMessageContent::Response),
                        "AFTER-CANCEL-OK",
                    ) {
                        sink.send_wait(crate::provider::test_support::activity(event)?)
                            .await?;
                    }

                    native_outcome = TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    };
                    native_completed_at_ms = 5;
                    request
                        .host_effects
                        .settle_foreground(sink.binding_id(), &request.scope)
                        .await
                        .map_err(|_| {
                            ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::TurnStart,
                                forge_provider_api::ProviderFailureKind::Runtime,
                            ))
                        })?;

                    return Ok(forge_provider_api::TurnResult::new(
                        native_outcome,
                        native_completed_at_ms,
                        None,
                        request.input_coverage(),
                        Vec::new(),
                        None,
                    )
                    .expect("fixture native coverage is valid"));
                }
                let tool = (
                    turn.clone(),
                    forge_provider_api::ItemId::new("denied-native-command").unwrap(),
                );
                for update in [
                    forge_provider_api::ItemEvent::Started(
                        forge_provider_api::ItemStart::ToolCall(
                            forge_provider_api::ToolDescriptor {
                                semantics: Default::default(),
                                name: "shell".into(),
                                title: "Denied fixture command".into(),
                                arguments: forge_provider_api::InspectableArguments::new(
                                    json!({"command":"write denied"}),
                                ),
                            },
                        ),
                    ),
                    forge_provider_api::ItemEvent::Tool {
                        event: forge_provider_api::ToolItemEvent::ExecutionStarted,
                    },
                ] {
                    sink.send_wait(forge_provider_api::AdapterActivity::Item {
                        thread_id: turn.thread_id.clone(),
                        turn_id: turn.turn_id.clone(),
                        item_id: tool.1.clone(),
                        event: update,
                    })
                    .await?;
                }
                if matches!(failure, ProviderFailure::Stalled) {
                    // Keep producing output so only the original deadline, not idleness, can stop it.
                    let tool_event = |event| forge_provider_api::AdapterActivity::Item {
                        thread_id: tool.0.thread_id.clone(),
                        turn_id: tool.0.turn_id.clone(),
                        item_id: tool.1.clone(),
                        event,
                    };
                    loop {
                        tokio::select! {
                            changed = cancelled.changed() => {
                                if changed.is_ok() && *cancelled.borrow() {
                                    stats.cancellations.fetch_add(1, Ordering::SeqCst);
                                    let completion = forge_provider_api::ToolCompletion {
                                        outcome: crate::provider::ToolOutcome::Failed {
                                            diagnostic: forge_provider_api::ToolDiagnostic {
                                                kind: forge_provider_api::ProviderFailureKind::Timeout,
                                                message: forge_provider_api::diagnostic::SanitizedMessage::new("fixture tool timed out").expect("static diagnostic is valid"),
                                            },
                                        },
                                        descriptor: None,
                                        output: None,
                                        changes: Default::default(),
                                    };
                                    sink.send_wait(tool_event(forge_provider_api::ItemEvent::Completed(
                                        forge_provider_api::ItemCompletion::ToolCall(completion),
                                    ))).await?;
                                    break;
                                }
                            }
                            _ = tokio::time::sleep(std::time::Duration::from_millis(20)) => {
                                sink.send_wait(tool_event(forge_provider_api::ItemEvent::Tool {
                                    event: forge_provider_api::ToolItemEvent::Output(
                                        crate::provider::TextUpdate::Append("still running\n".into()),
                                    ),
                                })).await?;
                            }
                        }
                    }
                    request
                        .host_effects
                        .settle_foreground(sink.binding_id(), &request.scope)
                        .await
                        .map_err(|_| {
                            ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::TurnStart,
                                forge_provider_api::ProviderFailureKind::Runtime,
                            ))
                        })?;
                    return Ok(forge_provider_api::TurnResult::new(
                        TurnOutcome::Interrupted,
                        4,
                        None,
                        request.input_coverage(),
                        Vec::new(),
                        None,
                    )
                    .expect("fixture native coverage is valid"));
                }
                sink.send_wait(crate::provider::test_support::activity(
                    ProviderEvent::Item {
                        thread_id: (tool).0.thread_id.clone(),
                        turn_id: (tool).0.turn_id.clone(),
                        item_id: (tool).1.clone(),
                        event: {
                            let completion = forge_provider_api::ToolCompletion {
                                outcome: crate::provider::ToolOutcome::Denied {
                                    reason: forge_provider_api::diagnostic::SanitizedMessage::new(
                                        "workspace policy",
                                    )
                                    .expect("static denial is nonempty"),
                                },
                                descriptor: None,
                                output: None,
                                changes: Default::default(),
                            };
                            forge_provider_api::ItemEvent::Completed(
                                forge_provider_api::ItemCompletion::ToolCall(completion),
                            )
                        },
                    },
                )?)
                .await?;
                let outcome = match failure {
                    ProviderFailure::Cancelled | ProviderFailure::Stalled => {
                        TurnOutcome::Interrupted
                    }
                    ProviderFailure::SimilarMessage => TurnOutcome::Failed {
                        error: forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Unknown,
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Observation,
                            forge_provider_api::FailureDisposition::Uncertain,
                        ),
                        retry: forge_provider_api::turn::RetryDecision::NotAllowed,
                    },
                };
                native_outcome = outcome;
                native_completed_at_ms = 4;

                request
                    .host_effects
                    .settle_foreground(sink.binding_id(), &request.scope)
                    .await
                    .map_err(|_| {
                        ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Runtime,
                        ))
                    })?;

                Ok(forge_provider_api::TurnResult::new(
                    native_outcome,
                    native_completed_at_ms,
                    None,
                    request.input_coverage(),
                    Vec::new(),
                    None,
                )
                .expect("fixture native coverage is valid"))
            }
        },
    )
}
/// Preserve native denied-tool history while classifying the enclosing exchange as cancelled.
#[tokio::test]
async fn native_cancelled_error_preserves_denied_history_and_allows_next_prompt() -> Result<()> {
    assert_native_failure(ProviderFailure::Cancelled).await
}

/// Similar provider text must not gain cancellation semantics or trigger automatic replay.
#[tokio::test]
async fn provider_failure_named_cancelled_remains_failed() -> Result<()> {
    assert_native_failure(ProviderFailure::SimilarMessage).await
}

/// Stop a noisy shell command at its deadline, persist failure, and keep the next prompt usable.
///
/// The hour-long deadline never elapses during the test. Once the watchdog arms it, the test
/// forces the watchdog's expiry path, because broker dispatch depends on timers that stall
/// under a paused clock.
#[tokio::test]
async fn shell_timeout_stops_execution_without_replaying_the_command() -> Result<()> {
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        assert_native_failure(ProviderFailure::Stalled),
    )
    .await?
}

/// Exercise public dispatch, persisted exchange history, and reuse through the real broker.
async fn assert_native_failure(failure: ProviderFailure) -> Result<()> {
    let repository = super::test::repository();
    let data = tempfile::tempdir()?;
    let mut broker = super::test::planning_question_broker(repository.path(), data.path(), false);
    if matches!(failure, ProviderFailure::Stalled) {
        broker.session.tool_timeout = crate::session::InputTimeout {
            amount: 1,
            unit: crate::session::TimeoutUnit::H,
        };
        broker.trace.configure(true)?;
    }
    let stats = Arc::new(CancellationFixtureStats {
        calls: Arc::new(AtomicUsize::new(0)),
        cancellations: Arc::new(AtomicUsize::new(0)),
    });
    let native = cancellation_fixture(failure, stats.clone());
    let fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    broker.provider = fixture.provider_handle();
    broker.turn_cancellation.arm();
    let session_events = broker.observer.session_events();
    let expire_stalled_tool = async {
        if !matches!(failure, ProviderFailure::Stalled) {
            return Ok::<_, anyhow::Error>(());
        }
        // The deadline arms when the broker observes the started tool, not when the fixture
        // sends it, so force expiry only after a watchdog holds the deadline.
        while session_events.expire_running_tool_deadlines()? == 0 {
            tokio::task::yield_now().await;
        }
        Ok(())
    };
    let (result, expired) = tokio::join!(
        broker.dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Attempt the native command"}),
        }),
        expire_stalled_tool,
    );
    expired?;
    let expected = match failure {
        ProviderFailure::Cancelled => ExchangeState::Cancelled,
        ProviderFailure::SimilarMessage | ProviderFailure::Stalled => ExchangeState::Failed,
    };
    let expected_code = match failure {
        ProviderFailure::Cancelled => "turn_cancelled",
        ProviderFailure::SimilarMessage => "request_failed",
        ProviderFailure::Stalled => "tool_timeout",
    };
    assert_eq!(
        result.response.error().context("fixture should fail")?.code,
        expected_code,
        "native failure response: {:?}",
        result.response.error()
    );
    assert_eq!(
        stats.calls.load(Ordering::SeqCst),
        1,
        "terminal provider results must not replay; response: {:?}",
        result.response.error()
    );
    let history = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(history.len(), 1);
    let exchange = &history[0];
    assert_eq!(exchange.state(), expected);
    assert!(exchange.provider_retry.is_none());
    assert!(
        exchange.finalization_error.is_none(),
        "unexpected finalization error: {:?}; response: {:?}",
        exchange.finalization_error,
        result.response.error()
    );
    let expected_turn = match failure {
        ProviderFailure::Cancelled | ProviderFailure::Stalled => TurnOutcome::Interrupted,
        ProviderFailure::SimilarMessage => TurnOutcome::Failed {
            error: forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Unknown,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            ),
            retry: forge_provider_api::turn::RetryDecision::NotAllowed,
        },
    };
    assert_eq!(
        exchange.turn[0].state(),
        TurnState::Finished {
            outcome: expected_turn
        }
    );
    let tool = exchange
        .tools()
        .next()
        .context("denied native tool disappeared")?
        .1;
    assert_eq!(tool.id, "denied-native-command");
    if matches!(failure, ProviderFailure::Stalled) {
        assert_eq!(tool.state(), ToolState::Failed);
        assert_eq!(stats.cancellations.load(Ordering::SeqCst), 1);
        // The foreground alone collects the timed-out turn: it classifies the failure, stops the
        // exchange through its scope, and joins native work before settling the exchange as failed.
        broker.trace.flush()?;
        let trace = std::fs::read_to_string(broker.trace.session_status(&broker.session.id).path)?;
        let collection: Vec<String> = trace
            .lines()
            .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
            .filter_map(|record| match record["event"].as_str()? {
                "exchange.foreground.failure_classified" => Some(format!(
                    "classified:{}",
                    record["payload"]["error_code"].as_str()?
                )),
                "foreground.cleanup" => {
                    Some(format!("cleanup:{}", record["payload"]["phase"].as_str()?))
                }
                _ => None,
            })
            .collect();
        assert_eq!(
            collection,
            [
                "classified:foreground_failure",
                "cleanup:engine_close_started",
                "cleanup:engine_closed",
                "cleanup:native_joined",
            ],
            "timeout collection order: {collection:?}"
        );
    } else {
        assert_eq!(tool.state(), ToolState::Denied);
    }
    assert!(
        broker
            .snapshot()?
            .timeline
            .iter()
            .any(|entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == &exchange.id))
    );
    // Service admission rearms cancellation only after the previous execution settles.
    broker.turn_cancellation.arm();
    let recovered = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Run the next ordinary prompt"}),
        })
        .await;
    assert!(
        recovered.response.error().is_none(),
        "{:?}",
        recovered.response.error()
    );
    assert_eq!(stats.calls.load(Ordering::SeqCst), 2);
    let history = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(history.len(), 2);
    assert_eq!(history[0].state(), expected);
    assert_eq!(history[1].state(), ExchangeState::Complete);
    assert!(
        history[1]
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .any(|message| message.text() == "AFTER-CANCEL-OK")
    );
    Ok(())
}

/// A cancellation admitted after preparation wins before the native submission gate opens.
#[tokio::test]
async fn prepared_foreground_collection_prevents_native_submission() -> Result<()> {
    let workspace = super::test::repository();
    let data = tempfile::tempdir()?;
    let mut broker = super::test::planning_question_broker(workspace.path(), data.path(), false);
    broker.checkpoint = None;
    broker.turn_cancellation.arm();
    let (publication, _updates) = publication::channel();
    let ForegroundDispatch::Admitting(admission) = broker.begin_foreground_dispatch(
        Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Prepared before cancellation"}),
        },
        publication,
    )?
    else {
        anyhow::bail!("fixture did not create foreground admission");
    };
    let mut staged = broker.stage_foreground_interaction(
        "Prepared before cancellation",
        PromptMode::Chat,
        Some(&ExchangeAdmission::chat(
            "Prepared before cancellation".into(),
        )),
        ProviderInput::from_text("Prepared before cancellation"),
    )?;
    broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
    let engine = broker
        .execution_registry()
        .owner(crate::provider::ThreadOwner::root(&broker.session.id))
        .await?;
    let mut transaction = broker.construct_foreground_transaction(&staged, engine)?;
    assert!(
        transaction.native.task.is_empty(),
        "prepared admission started native work before owner authorization"
    );
    let calls = Arc::new(AtomicUsize::new(0));
    let invoked = calls.clone();
    transaction.native.pending_submission = Some(Box::pin(async move {
        invoked.fetch_add(1, Ordering::SeqCst);
        std::future::pending::<Result<ProviderAttempt>>().await
    }));
    let mut dispatch = ForegroundDispatch::Running {
        transaction: Box::new(transaction),
        admission,
    };
    broker.step_foreground(
        &mut dispatch,
        ForegroundSignal::CollectionStarted(ForegroundCollection::Cancel(
            crate::exchange::command::CancelledInputPolicy::Retain,
        )),
    )?;
    // The ordinary phase can reopen after the command settles; this collected generation remains fenced.
    dispatch.authorize_submission();
    tokio::task::yield_now().await;
    assert_eq!(calls.load(Ordering::SeqCst), 0);
    Ok(())
}

/// An owner collection drains real success and failure without repeating its cancellation signal.
#[tokio::test]
async fn collected_foreground_retains_actual_native_success_and_failure() -> Result<()> {
    for succeeds in [true, false] {
        let cancellation = Arc::new(TurnCancellation::new());
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ThreadOwner::root("collected"),
            "exchange",
        );
        let (_sink, events) = crate::provider::events::channel(scope);
        let watchdog = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(
            Default::default(),
        )?);
        let started = Arc::new(tokio::sync::Notify::new());
        let release = Arc::new(tokio::sync::Notify::new());
        let worker_started = started.clone();
        let worker_release = release.clone();
        let mut native = ForegroundWait::start(
            events,
            async move {
                worker_started.notify_one();
                worker_release.notified().await;
                Ok(if succeeds {
                    Ok(17u32)
                } else {
                    Err(crate::provider::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Rejected,
                        ),
                    ))
                })
            },
            cancellation.capture(),
            watchdog,
        )?;
        native.authorize_submission();
        started.notified().await;
        cancellation.request();
        release.notify_one();
        let NativeForegroundSignal::NativeReady(result) = native.next_collected().await? else {
            anyhow::bail!(
                "collection repeated a cancellation signal instead of draining native completion"
            )
        };
        if succeeds {
            assert_eq!(result.unwrap(), 17);
        } else {
            assert!(
                matches!(result, Err(crate::provider::ProviderError::Failure(failure))
            if failure.kind() == forge_provider_api::ProviderFailureKind::Rejected)
            );
        }
    }
    Ok(())
}

/// Keep a steering readiness refresh and its observation stream alive after cancellation.
#[tokio::test]
async fn cancelled_foreground_retains_readiness_refresh_and_observation_stream() -> Result<()> {
    let cancellation = TurnCancellation::new();
    let scope = crate::provider::InvocationScope::new(
        crate::provider::ThreadOwner::root("cancelled-refresh"),
        "exchange",
    );
    let (sink, events) = crate::provider::events::channel(scope.clone());
    let watchdog = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(
        Default::default(),
    )?);
    let mut native = ForegroundWait::start(
        events,
        async { Ok(17u32) },
        cancellation.capture(),
        watchdog,
    )?;
    native.authorize_submission();
    assert!(matches!(
        native.next_collected().await?,
        NativeForegroundSignal::NativeReady(17)
    ));
    native.acknowledge_result();
    cancellation.request();
    let (release, ready) = tokio::sync::oneshot::channel();
    native.restart_observation(async move {
        ready
            .await
            .context("retained readiness refresh was dropped")?;
        Ok(23u32)
    })?;
    native.authorize_submission();
    assert!(
        native.has_dispatched_task(),
        "cancellation discarded the retained readiness refresh"
    );
    sink.send(crate::provider::test_support::native_event(
        scope.clone(),
        ProviderEvent::Turn {
            thread_id: scope.owner().thread_id(),
            turn_id: scope.turn_id().unwrap(),
            event: TurnEvent::Started(forge_provider_api::turn::TurnStarted { started_at_ms: 1 }),
        },
    ))?;
    assert!(
        matches!(
            native.next_collected().await?,
            NativeForegroundSignal::ProviderEvent(_)
        ),
        "refresh replaced the original observation stream"
    );
    release
        .send(())
        .map_err(|_| anyhow::anyhow!("refresh ownership was lost"))?;
    assert!(matches!(
        native.next_collected().await?,
        NativeForegroundSignal::NativeReady(23)
    ));
    Ok(())
}

/// Collection through the complete foreground wait preserves late native evidence over level cancellation.
#[tokio::test]
async fn collected_transaction_drains_native_result_without_recancelling() -> Result<()> {
    for succeeds in [true, false] {
        let workspace = super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.checkpoint = None;
        broker.turn_cancellation.arm();
        let mut staged = broker.stage_foreground_interaction(
            "collect",
            PromptMode::Chat,
            Some(&ExchangeAdmission::chat("collect".into())),
            ProviderInput::from_text("collect"),
        )?;
        broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
        let engine = broker
            .execution_registry()
            .owner(crate::provider::ThreadOwner::root(&broker.session.id))
            .await?;
        let mut transaction = broker.construct_foreground_transaction(&staged, engine)?;
        transaction.scan_enabled = false;
        transaction.question_enabled = false;
        transaction.agent_command_active = true;
        transaction.execution_open = false;
        let entered = Arc::new(tokio::sync::Notify::new());
        let release = Arc::new(tokio::sync::Notify::new());
        let worker_entered = entered.clone();
        let worker_release = release.clone();
        transaction.native.pending_submission = Some(Box::pin(async move {
            worker_entered.notify_one();
            worker_release.notified().await;
            let output = if succeeds {
                Ok(ProviderCompletion::default())
            } else {
                Err(ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Rejected,
                    ),
                ))
            };
            Ok(ProviderAttempt::Ready(Box::new(output)))
        }));
        transaction.native.authorize_submission();
        entered.notified().await;
        transaction.collection = Some(ForegroundCollection::Cancel(
            crate::exchange::command::CancelledInputPolicy::Retain,
        ));
        broker.turn_cancellation.request();
        release.notify_one();
        let ready = tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
            loop {
                match transaction.next().await? {
                    ForegroundSignal::Native(NativeForegroundSignal::NativeReady(
                        ProviderAttempt::Ready(ready),
                    )) => break Ok::<_, anyhow::Error>(ready),
                    ForegroundSignal::Native(
                        NativeForegroundSignal::Cancelled | NativeForegroundSignal::ToolExpired(_),
                    ) => anyhow::bail!(
                        "collection repeated cancellation instead of draining native evidence"
                    ),
                    _ => {}
                }
            }
        })
        .await??;
        assert_eq!(ready.is_ok(), succeeds);
        assert!(
            transaction.native.has_dispatched_task(),
            "native result must remain accounted until the broker consumes it"
        );
        // Collection can be processed before the already-read native result reaches the broker.
        transaction.native_open = transaction.native.has_dispatched_task();
        transaction.acknowledge_native_result();
        assert!(
            !transaction.native_open,
            "consumed native result left an empty native poll enabled"
        );
    }
    Ok(())
}

/// Collection stops semantic progress scans, which its closed effect gate could never dispatch.
#[tokio::test(start_paused = true)]
async fn collected_transaction_requests_no_plan_scan() -> Result<()> {
    let workspace = super::test::repository();
    let data = tempfile::tempdir()?;
    let mut broker = super::test::planning_question_broker(workspace.path(), data.path(), false);
    broker.checkpoint = None;
    let mut staged = broker.stage_foreground_interaction(
        "scan",
        PromptMode::Chat,
        Some(&ExchangeAdmission::chat("scan".into())),
        ProviderInput::from_text("scan"),
    )?;
    broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
    let engine = broker
        .execution_registry()
        .owner(crate::provider::ThreadOwner::root(&broker.session.id))
        .await?;
    let mut transaction = broker.construct_foreground_transaction(&staged, engine)?;
    transaction.scan_enabled = true;
    transaction.question_enabled = false;
    transaction.agent_command_active = true;
    transaction.execution_open = false;
    let horizon = std::time::Duration::from_secs(600);
    let scanned = |signal: Result<Result<ForegroundSignal>, tokio::time::error::Elapsed>| {
        matches!(signal, Ok(Ok(ForegroundSignal::PlanScan)))
    };
    assert!(
        scanned(tokio::time::timeout(horizon, transaction.next()).await),
        "an uncollected execution must keep scanning progress"
    );
    transaction.collection = Some(ForegroundCollection::Cancel(
        crate::exchange::command::CancelledInputPolicy::Retain,
    ));
    assert!(
        !scanned(tokio::time::timeout(horizon, transaction.next()).await),
        "collection requested a scan its closed effect gate cannot dispatch"
    );
    Ok(())
}
