//! Prove that owner commands never interrupt a turn while its provider dispatch is in flight.

use super::super::{HarnessBroker, SessionController, prompt};
use super::bootstrap::initialize;
use crate::exchange::command::{
    CancelExchangeRequest, CancelledInputPolicy, Command, ConfigurationChange,
    SessionConfigurationPatch,
};
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use forge_protocol::outbound::MessageSender;
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use tokio::sync::{mpsc, oneshot};

/// Bound how many scheduler passes a held dispatch gets before the test releases it.
const OWNER_SETTLE_YIELDS: usize = 200;

/// Select how the first dispatch's held send ends once the test releases it.
#[derive(Clone, Copy)]
enum HeldSendEnd {
    Confirm,
    Fail,
}

/// Select where the first prompt's provider send pauses.
#[derive(Clone, Copy, PartialEq, Eq)]
enum HoldPoint {
    /// Before `Sending` is recorded, while a stop can still prevent the send.
    BeforeSend,
    /// After `Sending` is recorded and before `confirm()`, while the provider may hold the input.
    DuringSend,
}

/// Drive a real session whose first send pauses after `Sending` is recorded and before `confirm()`.
struct HeldDispatch {
    controller: Arc<SessionController>,
    provider: crate::provider::test_support::ProviderTestSession,
    sink: MessageSender,
    reader: tokio::task::JoinHandle<Result<()>>,
    held: mpsc::UnboundedReceiver<()>,
    release: Option<oneshot::Sender<HeldSendEnd>>,
    dispatches: mpsc::UnboundedReceiver<String>,
    interrupted_during_send: Arc<AtomicBool>,
    commands_started_at_hold: usize,
    hold: HoldPoint,
    // Declared last: fields drop in order, and Windows cannot delete files still open.
    _root: tempfile::TempDir,
}

impl HeldDispatch {
    /// Create the session and submit one prompt whose send stays in flight until released.
    async fn start(hold: HoldPoint) -> Result<Self> {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let (held_sender, held) = mpsc::unbounded_channel();
        let (dispatched, dispatches) = mpsc::unbounded_channel();
        let (release, release_receiver) = oneshot::channel::<HeldSendEnd>();
        let release_receiver = Arc::new(std::sync::Mutex::new(Some(release_receiver)));
        let descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().unwrap(),
        )
        .unwrap();
        let native = crate::provider::test_support::EventFixtureProvider::new(
            descriptor,
            move |request, sink, start, cancelled| {
                let dispatched = dispatched.clone();
                async move {
                    let address = crate::provider::TurnAddress {
                        scope: request.scope.clone(),
                        thread_id: request.thread_id.clone(),
                        turn_id: request.turn_id.clone(),
                    };
                    start.observed(1).await?;
                    // A provider that was interrupted before its response stops without output.
                    if *cancelled.borrow() {
                        return Ok(crate::provider::test_support::native_result(
                            &request,
                            forge_provider_api::turn::TurnOutcome::Interrupted,
                        ));
                    }
                    if let forge_provider_api::input::PromptInput::Text(text) = &request.input {
                        let _ = dispatched.send(text.clone());
                    }
                    // Each turn replies under its own message identity, as a provider does.
                    let reply_id = format!("reply-{}", address.turn_id.as_str());
                    for event in crate::provider::test_support::message(
                        &address,
                        reply_id,
                        Some(crate::provider::TestMessageContent::Response),
                        "Reply",
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
        native.with_models(vec![
            serde_json::from_value(json!({
                "id":"fixture-model", "reasoning":["low","medium","high"],
                "context_window":[], "vision":false, "is_default":true,
            }))?,
            serde_json::from_value(json!({
                "id":"fixture-model-next", "reasoning":["low","medium","high"],
                "context_window":[], "vision":false, "is_default":false,
            }))?,
        ]);
        let sends = Arc::new(AtomicUsize::new(0));
        let interrupted_during_send = Arc::new(AtomicBool::new(false));
        let interruption_flag = interrupted_during_send.clone();
        let hold_send = move |interrupted: Option<tokio::sync::watch::Receiver<bool>>| {
            let first = sends.fetch_add(1, Ordering::AcqRel) == 0;
            let held_sender = held_sender.clone();
            let interruption_flag = interruption_flag.clone();
            let release = first
                .then(|| release_receiver.lock().unwrap().take())
                .flatten();
            async move {
                let Some(mut release) = release else {
                    return Ok(());
                };
                let _ = held_sender.send(());
                let end = match interrupted {
                    None => release.await,
                    // A send in progress cannot honor interruption, so record it and keep waiting.
                    Some(mut interrupted) => loop {
                        tokio::select! {
                            end = &mut release => break end,
                            changed = interrupted.changed(), if !interruption_flag.load(Ordering::Acquire) => {
                                if changed.is_err() || *interrupted.borrow() {
                                    interruption_flag.store(true, Ordering::Release);
                                }
                            }
                        }
                    },
                };
                match end {
                    Ok(HeldSendEnd::Confirm) => Ok(()),
                    Ok(HeldSendEnd::Fail) | Err(_) => {
                        Err(forge_provider_api::ProviderError::DeliveryUncertain)
                    }
                }
            }
        };
        match hold {
            HoldPoint::BeforeSend => {
                native.with_pre_dispatch(move |_| hold_send(None));
            }
            HoldPoint::DuringSend => {
                native.with_sending(move |_, interrupted| hold_send(Some(interrupted)));
            }
        }
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
        broker.set_provider_for_test(provider.provider_handle());
        let controller = SessionController::new(broker).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller
            .trace
            .configure_default(crate::trace::LogLevel::Trace)?;
        controller.start_provider(&sink).await;
        controller.attach_input(&sink).await?;
        let mut fixture = Self {
            _root: root,
            controller,
            provider,
            sink,
            reader,
            held,
            release: Some(release),
            dispatches,
            interrupted_during_send,
            commands_started_at_hold: 0,
            hold,
        };
        fixture.submit(800, "held-first", "First prompt").await?;
        fixture
            .held
            .recv()
            .await
            .context("first prompt never reached its in-flight send")?;
        fixture.commands_started_at_hold = fixture.started_command_count();
        Ok(fixture)
    }

    /// Submit one prompt through the public session input path.
    async fn submit(&self, id: u64, operation: &str, text: &str) -> Result<()> {
        prompt::apply(
            &self.controller,
            Request {
                id,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":operation,"text":text}),
            },
            &self.sink,
        )
        .await
    }

    /// Start one owner command concurrently so the test can release the held send while it waits.
    fn spawn_command(
        &self,
        command: Command,
    ) -> tokio::task::JoinHandle<
        Result<
            crate::exchange::command::CommandResult,
            crate::exchange::command::ExecuteCommandError,
        >,
    > {
        let controller = self.controller.clone();
        tokio::spawn(async move { controller.exchange_owner.execute_command(command).await })
    }

    /// Let the owner run every step it can take before the held send ends.
    async fn settle(&self) {
        for _ in 0..OWNER_SETTLE_YIELDS {
            tokio::task::yield_now().await;
        }
    }

    /// Get the event names the session trace has recorded so far, oldest first.
    fn trace_events(&self) -> impl Iterator<Item = String> {
        let path = self
            .controller
            .trace
            .session_status(self.controller.observer.session_id())
            .path;
        std::fs::read_to_string(path)
            .unwrap_or_default()
            .lines()
            .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
            .filter_map(|record| record["event"].as_str().map(str::to_owned))
            .collect::<Vec<_>>()
            .into_iter()
    }

    /// Count owner commands the session trace records as started.
    fn started_command_count(&self) -> usize {
        self.trace_events()
            .filter(|event| event == "exchange.command.started")
            .count()
    }

    /// End the held send, first proving that no command started or interrupted it in flight.
    fn release(&mut self, end: HeldSendEnd) {
        if self.hold == HoldPoint::DuringSend {
            self.assert_send_uninterrupted();
        }
        assert_eq!(
            self.started_command_count(),
            self.commands_started_at_hold,
            "an owner command started before the turn's dispatch settled"
        );
        self.end_send(end);
    }

    /// End the held send after a cancel, which may start but must not interrupt a send in flight.
    fn release_after_cancel(&mut self, end: HeldSendEnd) {
        self.assert_send_uninterrupted();
        self.end_send(end);
    }

    /// Prove that nothing interrupted the turn while its send was in flight.
    fn assert_send_uninterrupted(&self) {
        assert!(
            !self.interrupted_during_send.load(Ordering::Acquire),
            "the owner interrupted a turn while its dispatch was in flight"
        );
    }

    /// Resolve the held send exactly once.
    fn end_send(&mut self, end: HeldSendEnd) {
        let release = self.release.take().expect("the held send is released once");
        let _ = release.send(end);
    }

    /// Stop the owner, the provider, and the event readers after a scenario finishes.
    async fn finish(self) -> Result<()> {
        self.finish_reporting_observation().await?
    }

    /// Stop the owner and the provider, then report how the session's observation listener ended.
    ///
    /// The outer result covers owner and provider shutdown. The inner result is the listener's
    /// own outcome, which carries the stream's failure when no restart replaced it.
    async fn finish_reporting_observation(self) -> Result<Result<()>> {
        self.controller
            .input_stopping
            .store(true, Ordering::Release);
        self.controller.coordinator.submissions.changed.notify_one();
        self.controller.finish_exchange_owner().await?;
        self.provider.provider_handle().shutdown().await?;
        let observation = self.controller.finish_session_events().await;
        self.reader.abort();
        Ok(observation)
    }
}

/// Create a settings command that replaces the root invocation by selecting another model.
fn model_change() -> Result<Command> {
    let patch: SessionConfigurationPatch =
        serde_json::from_value(json!({"model":"fixture-model-next"}))?;
    Ok(Command::ConfigureSession(ConfigurationChange::Settings(
        patch,
    )))
}

/// Create the cancel command the editor sends for Ctrl-C.
fn cancel() -> Command {
    Command::CancelExchange(CancelExchangeRequest {
        input: CancelledInputPolicy::RestoreIfUnconsumed,
    })
}

/// A model change sent while the first prompt is mid-send waits for dispatch, then applies.
#[tokio::test]
async fn model_change_during_dispatch_waits_and_applies() -> Result<()> {
    let mut fixture = HeldDispatch::start(HoldPoint::DuringSend).await?;
    let scenario = async {
        let configured = fixture.spawn_command(model_change()?);
        fixture.settle().await;
        fixture.release(HeldSendEnd::Confirm);
        configured
            .await?
            .context("model change failed after an in-flight dispatch")?;
        let cancelled = fixture.spawn_command(cancel());
        cancelled
            .await?
            .context("cancel failed after the model change applied")?;
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish().await?;
    scenario
}

/// Cancel sent while the first prompt is mid-send waits for dispatch, then stops the turn.
#[tokio::test]
async fn cancel_during_dispatch_waits_and_stops_the_turn() -> Result<()> {
    let mut fixture = HeldDispatch::start(HoldPoint::DuringSend).await?;
    let scenario = async {
        let cancelled = fixture.spawn_command(cancel());
        fixture.settle().await;
        fixture.release_after_cancel(HeldSendEnd::Confirm);
        cancelled
            .await?
            .context("cancel failed after an in-flight dispatch")?;
        fixture.submit(801, "after-cancel", "Second prompt").await?;
        let mut sent = Vec::new();
        while let Some(text) = fixture.dispatches.recv().await {
            sent.push(text);
            if sent.last().is_some_and(|text| text == "Second prompt") {
                break;
            }
        }
        assert_eq!(sent.last().map(String::as_str), Some("Second prompt"));
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish().await?;
    scenario
}

/// Closing the session while the first prompt is mid-send finishes once the send ends.
#[tokio::test]
async fn shutdown_during_dispatch_finishes() -> Result<()> {
    let mut fixture = HeldDispatch::start(HoldPoint::DuringSend).await?;
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
    let controller = fixture.controller.clone();
    let closing = tokio::spawn(async move { controller.finish_exchange_owner().await });
    fixture.settle().await;
    fixture.release(HeldSendEnd::Confirm);
    closing.await??;
    fixture.provider.provider_handle().shutdown().await?;
    fixture.controller.finish_session_events().await?;
    fixture.reader.abort();
    Ok(())
}

/// A send whose worker fails before `confirm()` ends the turn without wedging the owner or resending.
#[tokio::test]
async fn failed_send_keeps_cancel_available_and_never_resends() -> Result<()> {
    let mut fixture = HeldDispatch::start(HoldPoint::DuringSend).await?;
    let scenario = async {
        fixture.release(HeldSendEnd::Fail);
        fixture.settle().await;
        let cancelled = fixture.spawn_command(cancel());
        cancelled
            .await?
            .context("cancel failed after a send that ended unconfirmed")?;
        assert!(
            fixture.dispatches.try_recv().is_err(),
            "the uncertain first prompt reached the provider handler"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish().await?;
    scenario
}

/// A model change sent before the first prompt records `Sending` waits for dispatch, then applies.
#[tokio::test]
async fn model_change_before_send_waits_and_applies() -> Result<()> {
    let mut fixture = HeldDispatch::start(HoldPoint::BeforeSend).await?;
    let scenario = async {
        let configured = fixture.spawn_command(model_change()?);
        fixture.settle().await;
        fixture.release(HeldSendEnd::Confirm);
        configured
            .await?
            .context("model change failed after waiting for an unsent dispatch")?;
        let sent: Vec<String> = std::iter::from_fn(|| fixture.dispatches.try_recv().ok()).collect();
        assert_eq!(
            sent.iter()
                .filter(|text| text.ends_with("First prompt"))
                .count(),
            1,
            "the first prompt reached the provider more than once: {sent:?}"
        );
        let cancelled = fixture.spawn_command(cancel());
        cancelled
            .await?
            .context("cancel failed after the model change applied")?;
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish().await?;
    scenario
}

/// A restart whose provider initialization fails leaves the session failed, and cancel reports it.
#[tokio::test]
async fn failed_restart_fails_the_session_and_reports_it() -> Result<()> {
    let mut fixture = HeldDispatch::start(HoldPoint::DuringSend).await?;
    let scenario = async {
        fixture
            .controller
            .initialization_fails
            .store(true, Ordering::Release);
        fixture.release(HeldSendEnd::Fail);
        fixture.settle().await;
        let cancelled = fixture.spawn_command(cancel()).await?;
        assert!(
            matches!(
                cancelled,
                Err(crate::exchange::command::ExecuteCommandError::Failed(
                    crate::exchange::command::CommandFailure::ProviderFailed
                ))
            ),
            "cancel after a failed restart did not report the failed provider: {cancelled:?}"
        );
        assert!(
            fixture
                .trace_events()
                .any(|event| event == "exchange.restart.failed"),
            "the session trace never recorded the failed restart"
        );
        assert!(
            fixture.dispatches.try_recv().is_err(),
            "the uncertain first prompt reached the provider handler"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    // A failed session must still shut down, and its listener reports the stream no restart replaced.
    let observation = fixture.finish_reporting_observation().await?;
    scenario?;
    assert!(
        observation.is_err(),
        "the failed observation stream ended cleanly although no restart replaced it"
    );
    Ok(())
}

/// A direct prompt sent while the first prompt is mid-send queues as a command, and admission
/// starts it only after the first turn's dispatch settles, so each prompt is sent exactly once.
#[tokio::test]
async fn direct_input_during_dispatch_waits_for_the_send() -> Result<()> {
    let mut fixture = HeldDispatch::start(HoldPoint::DuringSend).await?;
    let scenario = async {
        // The submit request answers only once the owner admits the input, so it runs beside the hold.
        let controller = fixture.controller.clone();
        let sink = fixture.sink.clone();
        let submitted = tokio::spawn(async move {
            prompt::apply(
                &controller,
                Request {
                    id: 801,
                    method: "session.prompt.submit".into(),
                    params: json!({"operation_id":"held-second","text":"Second prompt"}),
                },
                &sink,
            )
            .await
        });
        fixture.settle().await;
        fixture.release(HeldSendEnd::Confirm);
        submitted.await??;
        let mut sent = Vec::new();
        while let Some(text) = fixture.dispatches.recv().await {
            let second_dispatched = text.ends_with("Second prompt");
            sent.push(text);
            if second_dispatched {
                break;
            }
        }
        sent.extend(std::iter::from_fn(|| fixture.dispatches.try_recv().ok()));
        for prompt in ["First prompt", "Second prompt"] {
            assert_eq!(
                sent.iter().filter(|text| text.ends_with(prompt)).count(),
                1,
                "{prompt:?} reached the provider other than once: {sent:?}"
            );
        }
        assert!(
            !fixture
                .trace_events()
                .any(|event| event == "exchange.restart.requested"),
            "a direct prompt during dispatch forced a provider restart"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.finish().await?;
    scenario
}
