use super::*;
use crate::provider::ScopeOwner;
use crate::provider::{
    Compaction, CompactionResult, CompactionState, EventScope, ProviderEvent, ThreadOwner,
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

fn session_test_diagnostic(message: impl Into<String>) -> forge_provider_api::AdapterActivity {
    forge_provider_api::AdapterActivity::Warning(forge_provider_api::ProviderWarning {
        scope: forge_provider_api::WarningScope::Provider,
        kind: forge_provider_api::ProviderWarningKind::NativeNotice {
            category: forge_provider_api::ProviderFailureKind::Unknown,
        },
        message: forge_provider_api::SanitizedMessage::new(message)
            .expect("fixture diagnostic is nonempty"),
    })
}

fn observation_provider(
    foreground: Option<Arc<ForegroundObservationLoss>>,
) -> Arc<crate::provider::test_support::EventFixtureProvider> {
    crate::provider::test_support::EventFixtureProvider::new(
        observation_descriptor(),
        move |request, _sink, start, _cancelled| {
            let foreground = foreground.clone();
            async move {
                let Some(foreground) = foreground else {
                    // Acknowledge the native turn start so the owner can complete the turn.
                    start.observed(1).await?;
                    return Ok(crate::provider::test_support::native_result(
                        &request,
                        forge_provider_api::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        },
                    ));
                };
                foreground.calls.fetch_add(1, Ordering::AcqRel);
                foreground.active.store(true, Ordering::Release);
                *foreground.submission_id.lock().unwrap() =
                    Some(request.submission_id.as_str().to_owned());
                start.observed(1).await?;
                foreground.fail.notified().await;
                foreground.active.store(false, Ordering::Release);
                let failure =
                    forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable;
                Err(forge_provider_api::ProviderError::Observation(failure))
            }
        },
    )
}

/// Open the root native binding through the owner's first prompt and wait for it to settle.
///
/// Interactive sessions open their native binding lazily at the first prompt, so a fixture
/// that injects binding-scoped observations must first run real input through owner intake.
/// The fixture's provider must complete that prompt without blocking.
async fn open_root_binding_through_first_prompt(
    controller: &Arc<SessionController>,
    provider: &crate::provider::test_support::EventFixtureProvider,
    owner: &ThreadOwner,
    message_sink: &forge_protocol::outbound::MessageSender,
) -> Result<()> {
    open_root_binding_through_prompt(controller, provider, owner, message_sink, "binding-open")
        .await
}

/// Submit one prompt through owner intake and wait until it completes with a live root binding.
async fn open_root_binding_through_prompt(
    controller: &Arc<SessionController>,
    provider: &crate::provider::test_support::EventFixtureProvider,
    owner: &ThreadOwner,
    message_sink: &forge_protocol::outbound::MessageSender,
    operation_id: &str,
) -> Result<()> {
    use crate::session::state_machine::SessionPhase;
    controller.start_provider(message_sink).await;
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while !controller.provider_ready().await {
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("provider did not become ready")?;
    controller.attach_input(message_sink).await?;
    let prior_exchange_count = controller.broker.lock().await.snapshot()?.exchange.len();
    prompt::apply(
        controller,
        Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":operation_id,"text":"Open the native binding"}),
        },
        message_sink,
    )
    .await?;
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            let settled = {
                let broker = controller.broker.lock().await;
                let snapshot = broker.snapshot()?;
                matches!(snapshot.status, SessionPhase::Idle)
                    && snapshot.exchange.len() == prior_exchange_count + 1
                    && snapshot.exchange.last().is_some_and(|exchange| {
                        exchange.state() == crate::exchange::ExchangeState::Complete
                    })
            };
            if settled
                && controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .is_empty()
                && !controller.observer.session_events().has_pending()?
                && provider.event_sink(owner).await.is_ok()
            {
                break Ok::<(), anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("first prompt did not open and settle the root native binding")?
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
    let owner = ThreadOwner::root(&session_id);
    let provider = observation_provider(None);
    let _provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(_provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (message_sink, mut output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&message_sink).await?;
    open_root_binding_through_first_prompt(&controller, &provider, &owner, &message_sink).await?;
    let old = provider.event_sink(&owner).await?;
    old.fail(crate::provider::events::EventDeliveryError::Closed);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
    // The first prompt published its own frames ahead of the observation failure.
    loop {
        let frame = output.recv().await?.context("failure was not delivered")?;
        let event: Value = serde_json::from_slice(frame.bytes())?;
        if event.to_string().contains("Session observation failed") {
            break;
        }
    }
    controller.recover_session_events(&message_sink).await?;
    // Observation loss retires the old binding; explicit new work opens its replacement.
    open_root_binding_through_prompt(
        &controller,
        &provider,
        &owner,
        &message_sink,
        "replacement-binding-open",
    )
    .await?;
    let replacement = provider.event_sink(&owner).await?;
    replacement
        .send_wait(session_test_diagnostic("new generation"))
        .await?;
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            let frame = output.recv().await?.context("replacement event missing")?;
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event.to_string().contains("new generation") {
                break Ok::<(), anyhow::Error>(());
            }
        }
    })
    .await??;
    assert!(
        old.send_wait(session_test_diagnostic("stale"))
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
    let owner = ThreadOwner::root(&session_id);
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            // Working status can publish before deferred native open admits the invocation.
            if !foreground.active.load(Ordering::Acquire)
                || engine.active_invocation_snapshot()?.is_none()
            {
                tokio::task::yield_now().await;
                continue;
            }
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
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            // Observation loss fences foreground execution until explicit new work recovers
            // the binding, so settlement shows through the drained input queue instead.
            if controller
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
    assert_eq!(
        controller.coordinator.execution.available_permits(),
        0,
        "failed observation left foreground execution open before recovery"
    );
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
    for event in [
        "exchange.foreground.failure_settled",
        "exchange.input.completed",
    ] {
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
        tokio::time::timeout(crate::hang_guard::HANG_GUARD, task).await??;
    }
    let native_shutdown = crate::provider::operation::OperationControl::fresh(
        forge_provider_api::ProviderOperation::Shutdown,
    )?;
    provider.shutdown(native_shutdown.context()).await?;
    if let Err(error) = tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        controller.finish_session_events(),
    )
    .await?
    {
        assert!(
            // The collector wraps the native loss in its observation-commit error.
            error.chain().any(|cause| matches!(
                cause.downcast_ref::<crate::provider::ProviderError>(),
                Some(crate::provider::ProviderError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable
                ))
            )),
            "unexpected observation collector failure: {error:#}"
        );
    }
    drop(controller);
    drop(sink);
    // The session owner keeps its own output handle until owner shutdown, so the reader
    // cannot observe end-of-stream here; every assertion has already run.
    reader.abort();
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
    let owner = ThreadOwner::root(&session_id);
    let scope = EventScope::SessionOperation {
        thread_id: forge_provider_api::ThreadId::from(owner.clone()),
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
    open_root_binding_through_first_prompt(&controller, &provider, &owner, &message_sink).await?;
    let broker_guard = controller.broker.lock().await;
    // Ordering and non-blocking intake need only a few distinct observations; saturation of
    // the intake bound is covered by the overflow test below.
    let diagnostic_count = 16;
    // Keep the native session stream open after the producer finishes. Dropping
    // its last sender would be an unexpected disconnect, not a drained session.
    let provider_sink = provider.event_sink(&owner).await?;
    let session_sink = provider_sink.clone();
    let producer = tokio::spawn(async move {
        let thread_id = scope.owner().thread_id();
        provider_sink
            .send_wait(forge_provider_api::AdapterActivity::Thread {
                // The sink rejects a compaction whose thread differs from its envelope.
                thread_id: thread_id.clone(),
                event: forge_provider_api::ThreadEvent::CompactionUpdated(Box::new(Compaction {
                    id: "durable-observation".into(),
                    thread_id: thread_id.as_str().into(),
                    state: CompactionState::Completed,
                    manual: false,
                    started_at_ms: Some(10),
                    completed_at_ms: Some(20),
                    result: CompactionResult {
                        context_tokens_before: Some(100),
                        context_tokens_after: Some(25),
                        ..Default::default()
                    },
                })),
            })
            .await?;
        for sequence in 0..diagnostic_count {
            provider_sink
                .send_wait(session_test_diagnostic(format!("observation-{sequence}")))
                .await?;
        }
        Ok::<_, crate::provider::ProviderError>(())
    });
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, producer)
        .await
        .context("bounded provider intake waited for the held broker lock")???;
    assert!(controller.observer.session_events().has_pending()?);
    // Publication delivery can still flush the first prompt's retained frames, which do not
    // need the broker; only the fixture's own observations must wait for its release.
    while let Ok(frame) = output.try_recv() {
        let event: Value = serde_json::from_slice(frame.bytes())?;
        let fixture_diagnostic = event["payload"]["text"]
            .as_str()
            .is_some_and(|text| text.starts_with("observation-"));
        assert!(
            !fixture_diagnostic && !event.to_string().contains("\"durable-observation\""),
            "observations published before broker release: {event}"
        );
    }
    drop(broker_guard);

    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
    let owner = ThreadOwner::root(&session_id);
    let provider = observation_provider(None);
    let _provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(_provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (output, _receiver) = forge_protocol::outbound::channel();
    controller.attach_session_events(&output).await?;
    open_root_binding_through_first_prompt(&controller, &provider, &owner, &output).await?;
    let sink = provider.event_sink(&owner).await?;
    let guard = controller.broker.lock().await;
    for sequence in 0..(forge_protocol::MAX_PENDING_FRAMES * 2) {
        if sink
            .send_wait(session_test_diagnostic(format!("observation-{sequence}")))
            .await
            .is_err()
        {
            break;
        }
    }
    let inbox = controller.observer.session_events();
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
        let crate::provider::session_execution::OwnerObservation::Event(
            crate::provider::HarnessEvent::Native {
                event: native_event,
                ..
            },
        ) = event
        else {
            panic!("unexpected accepted event: {event:?}")
        };
        let ProviderEvent::Provider {
            event: forge_provider_api::ProviderConnectionEvent::Warning(warning),
        } = native_event.as_ref()
        else {
            panic!("unexpected accepted native event: {native_event:?}")
        };
        assert_eq!(warning.message.as_str(), format!("observation-{sequence}"));
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
    use forge_provider_api::message::{ItemCompletion, ItemEvent, ItemStart};
    use forge_provider_api::tool::{
        InspectableArguments, TextUpdate, ToolCompletion, ToolDescriptor, ToolItemEvent,
        ToolOutcome, ToolSemantics,
    };
    use forge_provider_api::{AdapterActivity, ItemId};
    let fixture = tempfile::tempdir()?;
    crate::checks::configure_fixture(fixture.path());
    let data_root = fixture.path().join("data");
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":data_root,
        "client_id":"shutdown-observation-fixture", "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session_id = broker.snapshot()?.session.id.clone();
    let recorded = Arc::new(std::sync::Mutex::new(None));
    let capture = recorded.clone();
    let provider = crate::provider::test_support::EventFixtureProvider::new(
        observation_descriptor(),
        move |request, writer, start, _| {
            let capture = capture.clone();
            async move {
                start.observed(1).await?;
                *capture.lock().unwrap() = Some(request.scope.clone());
                let item_id = ItemId::new("background-tool").unwrap();
                for event in [
                    ItemEvent::Started(ItemStart::CommandExecution(ToolDescriptor {
                        name: "exec".into(),
                        title: "Background command".into(),
                        semantics: ToolSemantics::Other,
                        arguments: InspectableArguments::new(Value::Null),
                    })),
                    ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted,
                    },
                    ItemEvent::Tool {
                        event: ToolItemEvent::Output(TextUpdate::Append("BEGIN\n".into())),
                    },
                    ItemEvent::Tool {
                        event: ToolItemEvent::Backgrounded,
                    },
                ] {
                    writer
                        .send_wait(AdapterActivity::Item {
                            thread_id: request.thread_id.clone(),
                            turn_id: request.turn_id.clone(),
                            item_id: item_id.clone(),
                            event,
                        })
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
    let weak_provider = Arc::downgrade(&provider);
    let shutdown_recorded = recorded.clone();
    provider.with_shutdown(move || {
        let provider = weak_provider
            .upgrade()
            .expect("fixture remains alive during shutdown");
        let scope = shutdown_recorded
            .lock()
            .unwrap()
            .clone()
            .expect("foreground request registered its background item");
        async move {
            let writer = provider.invocation_sink(&scope).await?;
            for event in [
                ItemEvent::Tool {
                    event: ToolItemEvent::Output(TextUpdate::Append("STOPPED\n".into())),
                },
                ItemEvent::Completed(ItemCompletion::CommandExecution(ToolCompletion {
                    outcome: ToolOutcome::Interrupted,
                    descriptor: None,
                    output: None,
                    changes: Default::default(),
                })),
            ] {
                writer
                    .send_wait(AdapterActivity::Item {
                        thread_id: scope.owner().thread_id(),
                        turn_id: scope.turn_id().unwrap(),
                        item_id: ItemId::new("background-tool").unwrap(),
                        event,
                    })
                    .await?;
            }
            Ok(())
        }
    });
    let provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let execution = broker.execution_registry();
    let controller = SessionController::new(broker).await?;
    let (message_sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<_, anyhow::Error>(())
    });
    controller.attach_session_events(&message_sink).await?;
    controller.attach_input(&message_sink).await?;
    controller
        .coordinator
        .submissions
        .apply(crate::input::InputOperation {
            id: "submit-background-command".into(),
            control_revision: 0,
            after_control: None,
            action: crate::input::InputAction::Submit {
                message: Box::new(crate::input::InputMessage::queued(
                    crate::input::InputDraft {
                        id: "background-input".into(),
                        text: "Run background command".into(),
                        purpose: crate::input::InputPurpose::User,
                        intent: crate::input::InputIntent::Auto,
                        attachments: Vec::new(),
                        config: None,
                    },
                )),
            },
        })?;
    let exchange_id = tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            let scope = recorded.lock().unwrap().clone();
            // The owner commits its settlement ticket through the broker after the exchange
            // reaches Complete; shutdown must not start while that commit is still pending.
            if let Some(scope) = scope
                && controller.coordinator.execution.available_permits() > 0
                && SqliteStore::read_only(&data_root)?
                    .load_exchange(scope.exchange_id.as_str())?
                    .is_some_and(|exchange| {
                        exchange.state() == crate::exchange::ExchangeState::Complete
                    })
            {
                break Ok::<_, anyhow::Error>(scope.exchange_id.as_str().to_owned());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("background fixture never completed foreground work")??;
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
    let observed = tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
    if shutdown.is_finished() {
        drop(broker_guard);
        let outcome = shutdown.await.context("shutdown worker failed")?;
        anyhow::bail!("shutdown released its owner before queued evidence drained: {outcome:?}");
    }
    assert!(
        SqliteStore::read_only(&data_root)?
            .load_session(&session_id)?
            .unwrap()
            .lease_owner
            .is_some()
    );
    drop(broker_guard);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, shutdown).await???;

    let store = SqliteStore::read_only(&data_root)?;
    let saved = store.load_exchange(&exchange_id)?.unwrap();
    let tool = saved.tools().next().unwrap().1;
    assert_eq!(tool.state(), crate::turn::ToolState::Interrupted);
    assert_eq!(tool.output, "BEGIN\nSTOPPED\n");
    assert_eq!(saved.state(), crate::exchange::ExchangeState::Complete);
    reader.abort();
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
    let _owner = ThreadOwner::root(&broker.snapshot()?.session.id);
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
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        controller.finish_session_events(),
    )
    .await??;
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
    let owner = ThreadOwner::root(&session_id);
    let provider = observation_provider(None);
    let _provider =
        crate::provider::test_support::ProviderTestSession::from_native(provider.clone())?;
    broker.set_provider_for_test(_provider.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (messages, _blocked_output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&messages).await?;
    open_root_binding_through_first_prompt(&controller, &provider, &owner, &messages).await?;
    // The unread first-prompt frames already hold part of the normal lane; fill the rest.
    for index in 0..forge_protocol::MAX_PENDING_FRAMES - forge_protocol::RESERVED_CONTROL_RECORDS {
        if messages.send(index).is_err() {
            break;
        }
    }
    assert!(
        messages.send(0).is_err(),
        "fixture did not saturate the obsolete client's normal lane"
    );
    let old_inbox = controller.observer.session_events();
    provider
        .event_sink(&owner)
        .await?
        .fail(crate::provider::events::EventDeliveryError::Closed);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
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
        crate::hang_guard::HANG_GUARD,
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
