use super::*;
use crate::provider::{
    Compaction, CompactionResult, CompactionState, EventScope, ExecutionOwner, ProviderDiagnostic,
    ProviderEvent,
};
use forge_provider_api::provider::Provider as _;

/// Keep an accepted native prompt live until its observation boundary is lost.
#[derive(Default)]
struct ForegroundObservationLoss {
    calls: std::sync::atomic::AtomicUsize,
    active: AtomicBool,
    fail: Notify,
    submission_id: std::sync::Mutex<Option<String>>,
}

fn observation_descriptor() -> forge_provider_api::ProviderDescriptor {
    forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
        label: "session observation fixture".into(),
        capability: Default::default(),
    }
}

fn session_test_diagnostic(message: impl Into<String>) -> ProviderDiagnostic {
    ProviderDiagnostic {
        kind: forge_provider_api::ProviderFailureKind::Unknown,
        message: forge_provider_api::SanitizedMessage::new(message)
            .expect("fixture diagnostic is nonempty"),
        fatal: false,
    }
}

fn observation_provider(
    foreground: Option<Arc<ForegroundObservationLoss>>,
) -> Arc<crate::provider::test_support::EventFixtureProvider> {
    crate::provider::test_support::EventFixtureProvider::new(
        observation_descriptor(),
        move |request, sink, _cancelled| {
            let foreground = foreground.clone();
            async move {
                let Some(foreground) = foreground else {
                    return Ok(());
                };
                foreground.calls.fetch_add(1, Ordering::AcqRel);
                foreground.active.store(true, Ordering::Release);
                *foreground.submission_id.lock().unwrap() =
                    Some(request.submission_id.as_str().to_owned());
                let turn = crate::provider::TurnAddress {
                    scope: request.scope.clone(),
                    thread_id: "fixture-thread".into(),
                    turn_id: "live-turn".into(),
                };
                let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
                    request.scope.clone(),
                    forge_provider_api::lifecycle::ExecutionUpdate::Started {
                        address: turn.clone(),
                    },
                )?;
                sink.send_wait(ProviderEvent::Execution { observation })
                    .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: turn,
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
                })
                .await?;
                foreground.fail.notified().await;
                foreground.active.store(false, Ordering::Release);
                let failure =
                    forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable;
                sink.fail(forge_provider_api::events::EventDeliveryError::Observation(
                    failure.clone(),
                ));
                Err(forge_provider_api::ProviderError::Observation(failure))
            }
        },
    )
}

/// Deliver the old failure before installing a fresh owner observation generation.
#[tokio::test]
async fn failed_session_observation_restarts_for_explicit_new_work() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"replacement-observation-fixture", "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session_id = broker.snapshot()?.session.id.clone();
    let owner = ExecutionOwner::root(&session_id);
    let provider = observation_provider(None);
    let _provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(_provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (message_sink, mut output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&message_sink).await?;
    let old = provider.event_sink(&owner).await?;
    old.fail(crate::provider::events::EventDeliveryError::Closed);
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller
                .session_event_task
                .lock()
                .await
                .as_ref()
                .is_some_and(|task| task.is_finished())
            {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("failed observation did not finish")?;
    let frame = output.recv().await?.context("failure was not delivered")?;
    let failure: Value = serde_json::from_slice(frame.bytes())?;
    assert!(failure.to_string().contains("Session observation failed"));
    controller.recover_session_events(&message_sink).await?;
    let replacement = provider.event_sink(&owner).await?;
    let scope = EventScope::SessionOperation {
        owner,
        operation_id: forge_provider_api::OperationId::new("replacement")
            .expect("fixture operation identity is nonempty"),
    };
    replacement
        .bind_scope(scope.clone())?
        .send_wait(ProviderEvent::Diagnostic {
            scope,
            diagnostic: session_test_diagnostic("new generation"),
        })
        .await?;
    let frame = tokio::time::timeout(Duration::from_secs(5), output.recv())
        .await??
        .context("replacement event missing")?;
    let event: Value = serde_json::from_slice(frame.bytes())?;
    assert!(event.to_string().contains("new generation"));
    assert!(
        old.send_wait(ProviderEvent::Diagnostic {
            scope: EventScope::SessionOperation {
                owner: ExecutionOwner::root(&session_id),
                operation_id: forge_provider_api::OperationId::new("old")
                    .expect("fixture operation identity is nonempty")
            },
            diagnostic: session_test_diagnostic("stale"),
        })
        .await
        .is_err()
    );
    Ok(())
}

/// Settle accepted input and its open documents before releasing a failed foreground owner.
#[tokio::test]
async fn accepted_observation_loss_publishes_failed_timeline_and_collects_input_delivery()
-> Result<()> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    use crate::provider::submission::SubmissionState;
    use crate::session::state_machine::SessionPhase;
    use forge_buffer::identity::{DocumentId, ViewId};
    use forge_buffer::width::WidthProfile;

    let fixture = tempfile::tempdir()?;
    std::fs::write(fixture.path().join(".forge.json"), r#"{"checks":{}}"#)?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"accepted-observation-loss", "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session_id = broker.snapshot()?.session.id.clone();
    let foreground = Arc::new(ForegroundObservationLoss::default());
    let owner = ExecutionOwner::root(&session_id);
    let provider = observation_provider(Some(foreground.clone()));
    let _provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(_provider_fixture.provider_handle());
    let presentation = broker.presentation();
    let document = DocumentId("transcript:observation-loss".into());
    let opened = presentation.lock().unwrap().open(
        document.clone(),
        ViewId("view:observation-loss".into()),
        WidthProfile::default(),
    )?;
    let controller = SessionController::new(broker).await?;
    let execution_registry = controller.broker.lock().await.execution_registry();
    let engine = execution_registry.owner(owner.clone()).await?;
    controller.trace.configure(true)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let delivered = Arc::new(std::sync::Mutex::new(Vec::<Value>::new()));
    let observed = delivered.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            observed
                .lock()
                .unwrap()
                .push(serde_json::from_slice(frame.bytes())?);
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_session_events(&sink).await?;
    controller.attach_input(&sink).await?;
    controller.coordinator.submissions.apply(InputOperation {
        id: "submit-observation-loss".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: "accepted-input".into(),
                text: "Keep this accepted instruction exactly once".into(),
                purpose: crate::input::InputPurpose::User,
                intent: InputIntent::Auto,
                attachments: vec![],
                config: None,
            })),
        },
    })?;
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            let broker = controller.broker.lock().await;
            if matches!(broker.snapshot()?.status, SessionPhase::Working { .. }) {
                let status = presentation
                    .lock()
                    .unwrap()
                    .snapshot(&opened.status.document)?;
                if status
                    .block
                    .iter()
                    .any(|block| block.text.rows().any(|row| row.contains("Working")))
                {
                    break Ok::<(), anyhow::Error>(());
                }
            }
            drop(broker);
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("accepted native prompt did not publish working status")??;
    assert!(engine.active_invocation_snapshot()?.is_some());
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    let submission_id = foreground
        .submission_id
        .lock()
        .unwrap()
        .clone()
        .context("input was not accepted")?;
    let submission = controller.submission_store.submission(&submission_id)?;
    assert!(matches!(
        submission.record()?.delivery,
        SubmissionState::Accepted { .. }
    ));
    let before_loss = delivered.lock().unwrap().len();

    foreground.fail.notify_one();
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller.coordinator.execution.available_permits() > 0
                && controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .is_empty()
                && delivered.lock().unwrap()[before_loss..]
                    .iter()
                    .any(|event| {
                        event
                            .to_string()
                            .contains("native lifecycle observation is unavailable")
                    })
            {
                break Ok::<(), anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("observation loss did not settle input delivery")??;
    let broker = controller.broker.lock().await;
    assert!(
        broker.input_submission.is_none(),
        "failed input retained its submission owner"
    );
    let snapshot = broker.snapshot()?;
    assert!(matches!(snapshot.status, SessionPhase::Idle));
    assert_eq!(snapshot.exchange.len(), 1, "accepted input was replayed");
    assert_eq!(
        snapshot.exchange[0].state(),
        crate::exchange::ExchangeState::Failed
    );
    drop(broker);
    assert!(engine.active_invocation_snapshot()?.is_none());
    assert_eq!(foreground.calls.load(Ordering::Acquire), 1);
    assert!(
        matches!(
            submission.record()?.delivery,
            SubmissionState::Accepted { .. }
        ),
        "lifecycle loss must not make accepted input replayable"
    );
    {
        let owner = presentation.lock().unwrap();
        assert!(
            owner.snapshot(&opened.status.document)?.block.is_empty(),
            "failed exchange left the open status document working"
        );
        let transcript = owner.snapshot(&document)?;
        assert!(
            !transcript
                .block
                .iter()
                .any(|block| block.text.rows().any(|row| row.contains("Thinking"))),
            "failed exchange left an unsettled response placeholder"
        );
    }
    controller.trace.flush()?;
    let trace = std::fs::read_to_string(controller.trace.session_status(&session_id).path)?;
    let records = trace
        .lines()
        .map(serde_json::from_str::<Value>)
        .collect::<std::result::Result<Vec<_>, _>>()?;
    for event in ["input.dispatch_finished", "input.forwarder_finished"] {
        assert!(
            records.iter().any(|record| record["event"] == event),
            "missing delivery postlude: {event}"
        );
    }
    assert!(
        delivered.lock().unwrap()[before_loss..]
            .iter()
            .any(|event| event["event"] == "document_changed"),
        "failed timeline was not delivered to the open documents"
    );

    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    if let Some(task) = controller.input_task.lock().await.take() {
        tokio::time::timeout(Duration::from_secs(5), task).await??;
    }
    let native_shutdown = crate::provider::operation::OperationControl::fresh(
        forge_provider_api::ProviderOperation::Shutdown,
    )?;
    provider.shutdown(native_shutdown.context()).await?;
    if let Err(error) =
        tokio::time::timeout(Duration::from_secs(5), controller.finish_session_events()).await?
    {
        assert!(
            matches!(
                error.downcast_ref::<crate::provider::ProviderError>(),
                Some(crate::provider::ProviderError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable
                ))
            ),
            "unexpected observation collector failure: {error:#}"
        );
    }
    drop(controller);
    drop(sink);
    reader.await??;
    Ok(())
}

/// Drain bounded provider intake while execution owns the broker, then publish in source order.
#[tokio::test]
async fn session_observation_intake_does_not_block_on_foreground_broker() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"observation-fixture", "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session_id = broker.snapshot()?.session.id.clone();
    let owner = ExecutionOwner::root(&session_id);
    let scope = EventScope::SessionOperation {
        owner: owner.clone(),
        operation_id: forge_provider_api::OperationId::new("observation-fixture")
            .expect("fixture operation identity is nonempty"),
    };
    let provider = observation_provider(None);
    let _provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(_provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (message_sink, mut output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&message_sink).await?;
    let broker_guard = controller.broker.lock().await;
    let diagnostic_count = forge_protocol::MAX_PENDING_FRAMES / 2;
    // Keep the native session stream open after the producer finishes. Dropping
    // its last sender would be an unexpected disconnect, not a drained session.
    let provider_sink = provider
        .event_sink(&owner)
        .await?
        .bind_scope(scope.clone())?;
    let session_sink = provider_sink.clone();
    let producer = tokio::spawn(async move {
        provider_sink
            .send_wait(ProviderEvent::Compaction {
                scope: scope.clone(),
                update: Box::new(Compaction {
                    id: "durable-observation".into(),
                    thread_id: "native-observation".into(),
                    state: CompactionState::Completed,
                    manual: false,
                    started_at_ms: Some(10),
                    completed_at_ms: Some(20),
                    result: CompactionResult {
                        context_tokens_before: Some(100),
                        context_tokens_after: Some(25),
                        ..Default::default()
                    },
                }),
            })
            .await?;
        for sequence in 0..diagnostic_count {
            provider_sink
                .send_wait(ProviderEvent::Diagnostic {
                    scope: scope.clone(),
                    diagnostic: session_test_diagnostic(format!("observation-{sequence}")),
                })
                .await?;
        }
        Ok::<_, crate::provider::ProviderError>(())
    });
    tokio::time::timeout(Duration::from_secs(2), producer)
        .await
        .context("bounded provider intake waited for the held broker lock")???;
    assert!(controller.observer.session_events().has_pending()?);
    assert!(
        output.try_recv().is_err(),
        "observations published before broker release"
    );
    drop(broker_guard);

    tokio::time::timeout(Duration::from_secs(5), async {
        let mut expected_sequence = 0;
        while expected_sequence < diagnostic_count {
            let frame = output
                .recv()
                .await?
                .context("session observation output closed")?;
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] != "provider_event" {
                continue;
            }
            assert_eq!(event["session_id"], session_id);
            // Provider initialization can publish runtime metadata before the
            // fixture diagnostics; only diagnostic updates carry data.
            if event["payload"]["kind"] != "error" {
                continue;
            }
            assert_eq!(
                event["payload"]["data"]["kind"], "unknown",
                "unexpected provider error: {event}"
            );
            assert_eq!(
                event["payload"]["text"],
                format!("observation-{expected_sequence}")
            );
            expected_sequence += 1;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await
    .context("queued session observations did not publish after broker release")??;
    let stored = SqliteStore::read_only(&controller.data_root)?.list_session_event(&session_id)?;
    let compaction = stored
        .iter()
        .find_map(|record| match &record.detail {
            crate::timeline::SessionEventKind::Compaction { compaction }
                if compaction.id == "durable-observation" =>
            {
                Some(compaction)
            }
            _ => None,
        })
        .context("session observation was not persisted")?;
    assert_eq!(compaction.state, CompactionState::Completed);
    assert_eq!(compaction.result.context_tokens_before, Some(100));
    assert_eq!(compaction.result.context_tokens_after, Some(25));
    drop(session_sink);
    Ok(())
}

/// Overflow retains the accepted prefix and reports the original typed loss.
#[tokio::test]
async fn session_observation_overflow_retains_prefix_and_reports_loss() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"observation-overflow", "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session_id = broker.snapshot()?.session.id.clone();
    let owner = ExecutionOwner::root(&session_id);
    let scope = EventScope::SessionOperation {
        owner: owner.clone(),
        operation_id: forge_provider_api::OperationId::new("overflow")
            .expect("fixture operation identity is nonempty"),
    };
    let provider = observation_provider(None);
    let _provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(_provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (output, _receiver) = forge_protocol::outbound::channel();
    controller.attach_session_events(&output).await?;
    let sink = provider
        .event_sink(&owner)
        .await?
        .bind_scope(scope.clone())?;
    let guard = controller.broker.lock().await;
    for sequence in 0..(forge_protocol::MAX_PENDING_FRAMES * 2) {
        if sink
            .send_wait(ProviderEvent::Diagnostic {
                scope: scope.clone(),
                diagnostic: session_test_diagnostic(format!("observation-{sequence}")),
            })
            .await
            .is_err()
        {
            break;
        }
    }
    let inbox = controller.observer.session_events();
    tokio::time::timeout(Duration::from_secs(5), async {
        while !inbox.is_closed() {
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("overflow did not close observation")?;
    let accepted = inbox.drain()?;
    assert!(
        !accepted.is_empty(),
        "overflow discarded the accepted prefix"
    );
    for (sequence, event) in accepted.iter().enumerate() {
        let ProviderEvent::Diagnostic { diagnostic, .. } = event else {
            panic!("unexpected accepted event: {event:?}")
        };
        assert_eq!(
            diagnostic.message.as_str(),
            format!("observation-{sequence}")
        );
    }
    let error = inbox
        .drain()
        .expect_err("overflow must report observation loss");
    assert!(
        matches!(
            &error,
            crate::provider::ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Overflow
            )
        ),
        "unexpected failure: {error:#}"
    );
    drop(guard);
    Ok(())
}

/// Drain shutdown evidence through the real collector and store before releasing its lease.
#[tokio::test]
async fn shutdown_drains_background_terminal_evidence_before_releasing_lease() -> Result<()> {
    use crate::provider::{
        TextUpdate, ToolActivityKind, ToolAddress, ToolInvocation, ToolOutcome, ToolUpdate,
    };

    let fixture = tempfile::tempdir()?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"shutdown-observation-fixture", "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session_id = broker.snapshot()?.session.id.clone();
    let owner = ExecutionOwner::root(&session_id);
    let scope = crate::provider::InvocationScope::new(owner.clone(), "background-exchange");
    let address = ToolAddress {
        turn: crate::provider::TurnAddress {
            scope: scope.clone(),
            thread_id: "background-thread".into(),
            turn_id: "background-turn".into(),
        },
        tool_id: "background-tool".into(),
    };
    let mut turn =
        crate::turn::test_support::TestTurn::new("background-turn".into(), address.turn.clone(), 1);
    for update in [
        ToolUpdate::Requested {
            invocation: ToolInvocation {
                semantics: Default::default(),
                kind: ToolActivityKind::Command,
                name: "exec".into(),
                title: "Background command".into(),
                arguments: Value::Null,
            },
        },
        ToolUpdate::Started,
        ToolUpdate::Output {
            update: TextUpdate::Append("BEGIN\n".into()),
        },
        ToolUpdate::Backgrounded,
    ] {
        turn.record_tool("background-tool", update, 2)?;
    }
    turn.finish(crate::turn::TurnOutcome::Completed, 3)?;
    let mut exchange = turn.into_exchange();
    exchange.finish(crate::exchange::ExchangeOutcome::Complete, 3)?;
    let data_root = fixture.path().join("data");
    SqliteStore::open(&data_root)?.save_exchange(&exchange)?;

    let provider = observation_provider(None);
    let delivery = Arc::new(std::sync::Mutex::new(Some(vec![
        ProviderEvent::Tool {
            address: address.clone(),
            update: ToolUpdate::Output {
                update: TextUpdate::Append("STOPPED\n".into()),
            },
        },
        ProviderEvent::Tool {
            address,
            update: ToolUpdate::Finished {
                outcome: ToolOutcome::Interrupted,
            },
        },
    ])));
    let weak_provider = Arc::downgrade(&provider);
    let shutdown_scope = scope.clone();
    provider.with_shutdown(move || {
        let provider = weak_provider
            .upgrade()
            .expect("fixture provider remains alive during shutdown");
        let delivery = delivery.clone();
        let scope = shutdown_scope.clone();
        async move {
            let sink = provider.invocation_sink(&scope).await?;
            let events = delivery.lock().unwrap().take().unwrap_or_default();
            for event in events {
                sink.send_wait(event).await?;
            }
            Ok(())
        }
    });
    let _provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(_provider_fixture.provider_handle());
    let execution = broker.execution_registry();
    let initialization = broker
        .initialization_request()?
        .context("fixture initialization missing")?;
    let bridge = broker.provider_handle();
    let controller = SessionController::new(broker).await?;
    let (message_sink, _output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&message_sink).await?;
    // Persisted background work requires an opened native binding, not just attached UI output.
    execution.owner(owner.clone()).await?;
    bridge.session_open(initialization).await?;
    let broker_guard = controller.broker.lock().await;
    let shutdown_controller = controller.clone();
    let shutdown = tokio::spawn(async move {
        execution.begin_orderly_shutdown().await?;
        let native_shutdown = crate::provider::operation::OperationControl::fresh(
            forge_provider_api::ProviderOperation::Shutdown,
        )?;
        provider.shutdown(native_shutdown.context()).await?;
        execution.close().await?;
        shutdown_controller.finish_session_events().await?;
        shutdown_controller.broker.lock().await.release_lease()?;
        Ok::<_, anyhow::Error>(())
    });
    let observed = tokio::time::timeout(Duration::from_secs(2), async {
        while !controller.observer.session_events().has_pending()? {
            tokio::task::yield_now().await;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await;
    if observed.is_err() && shutdown.is_finished() {
        drop(broker_guard);
        shutdown.await.context("shutdown worker failed")??;
        anyhow::bail!("shutdown finished without publishing terminal evidence");
    }
    observed.context("shutdown did not publish terminal evidence")??;
    assert!(
        !shutdown.is_finished(),
        "shutdown released its owner before queued evidence drained"
    );
    assert!(
        SqliteStore::read_only(&data_root)?
            .load_session(&session_id)?
            .unwrap()
            .lease_owner
            .is_some()
    );
    drop(broker_guard);
    tokio::time::timeout(Duration::from_secs(2), shutdown).await???;

    let store = SqliteStore::read_only(&data_root)?;
    let saved = store.load_exchange(&exchange.id)?.unwrap();
    let tool = saved.tools().next().unwrap().1;
    assert_eq!(tool.state(), crate::turn::ToolState::Interrupted);
    assert_eq!(tool.output, "BEGIN\nSTOPPED\n");
    assert_eq!(saved.state(), crate::exchange::ExchangeState::Complete);
    assert!(
        store
            .load_session(&session_id)?
            .unwrap()
            .lease_owner
            .is_none()
    );
    Ok(())
}

/// A cancelled shutdown wait must retain the collector until the provider closes its stream.
#[tokio::test]
async fn cancelled_session_observation_join_retains_collector_for_retry() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"observation-retry-fixture", "provider":{"kind":"mock","command":[]},
    }))?)?;
    let _owner = ExecutionOwner::root(&broker.snapshot()?.session.id);
    let provider = observation_provider(None);
    let provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let execution = broker.execution_registry();
    let controller = SessionController::new(broker).await?;
    let (message_sink, _output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&message_sink).await?;
    assert!(
        tokio::time::timeout(Duration::ZERO, controller.finish_session_events())
            .await
            .is_err()
    );
    assert!(controller.session_event_task.lock().await.is_some());
    execution.begin_orderly_shutdown().await?;
    let native_shutdown = crate::provider::operation::OperationControl::fresh(
        forge_provider_api::ProviderOperation::Shutdown,
    )?;
    provider.shutdown(native_shutdown.context()).await?;
    execution.close().await?;
    tokio::time::timeout(Duration::from_secs(2), controller.finish_session_events()).await??;
    assert!(controller.session_event_task.lock().await.is_none());
    Ok(())
}

/// Retiring an observation listener never waits for an obsolete client's outbound capacity.
#[tokio::test]
async fn saturated_observer_transport_does_not_block_recovery() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"saturated-observer", "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session_id = broker.snapshot()?.session.id.clone();
    let owner = ExecutionOwner::root(&session_id);
    let provider = observation_provider(None);
    let _provider =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(_provider.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (messages, _blocked_output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&messages).await?;
    for index in 0..forge_protocol::MAX_PENDING_FRAMES - forge_protocol::RESERVED_CONTROL_RECORDS {
        messages.send(index)?;
    }
    let old_inbox = controller.observer.session_events();
    provider
        .event_sink(&owner)
        .await?
        .fail(crate::provider::events::EventDeliveryError::Closed);
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller
                .session_event_task
                .lock()
                .await
                .as_ref()
                .is_some_and(|task| task.is_finished())
            {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("observer stayed blocked on stale outbound capacity")?;
    let (retained, _) = controller
        .publications
        .snapshot(None, std::num::NonZeroU16::new(128).unwrap())?
        .into_parts();
    assert!(
        retained.iter().any(|record| record
            .event()
            .payload
            .to_string()
            .contains("Session observation failed")),
        "observation diagnostic was not retained before listener completion"
    );
    tokio::time::timeout(
        Duration::from_secs(5),
        controller.recover_session_events(&messages),
    )
    .await??;
    assert!(!Arc::ptr_eq(
        &old_inbox,
        &controller.observer.session_events()
    ));
    assert!(!controller.task_controller.configuration_is_uncertain());
    Ok(())
}
