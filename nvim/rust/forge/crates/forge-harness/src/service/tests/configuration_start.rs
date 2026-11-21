//! Order a configuration change against a turn whose provider holds the input but has not yet
//! reported the turn's native start.

use super::super::{HarnessBroker, SessionController, prompt};
use super::bootstrap::initialize;
use anyhow::{Context, Result, ensure};
use forge_protocol::message::Request;
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::time::Duration;

/// A configuration change sent after the provider took a turn's input, but before the provider
/// reported that turn's native start, waits for the start and then restarts the turn.
///
/// Copilot confirms dispatch when it hands the input to its runtime, and the runtime takes a
/// moment more to begin the turn. A thinking-level change in that moment interrupted the runtime
/// before the turn it was about to start, so the adapter settled the input as stopped while the
/// runtime went on to run it. The observer rejected that turn as unexpected, the stop failed, the
/// exchange failed or stayed `Stopping`, and the change was lost.
#[tokio::test(start_paused = true)]
async fn configuration_before_the_native_start_waits_for_the_start_then_restarts_the_turn()
-> Result<()> {
    configuration_before_native_start(false).await
}

/// Interrupt accepted work while a configuration change is waiting for native start.
#[tokio::test(start_paused = true)]
async fn stop_overtakes_configuration_waiting_for_native_start() -> Result<()> {
    configuration_before_native_start(true).await
}

/// Drive the same deferred configuration with and without an explicit stop.
async fn configuration_before_native_start(stop: bool) -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let execution_count = Arc::new(AtomicUsize::new(0));
    let interrupted_before_start = Arc::new(AtomicBool::new(false));
    let start_release = Arc::new(tokio::sync::Notify::new());
    let (accepted, mut acceptances) = tokio::sync::mpsc::unbounded_channel();
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().context("no fixture provider")?,
    )
    .context("no fixture provider descriptor")?;
    let native = {
        let execution_count = execution_count.clone();
        let interrupted_before_start = interrupted_before_start.clone();
        let start_release = start_release.clone();
        crate::provider::test_support::EventFixtureProvider::new(
            descriptor,
            move |request, _writer, start, mut cancelled| {
                let first = execution_count.fetch_add(1, Ordering::AcqRel) == 0;
                let accepted = accepted.clone();
                let interrupted_before_start = interrupted_before_start.clone();
                let start_release = start_release.clone();
                async move {
                    let outcome = if first {
                        // The fixture has confirmed dispatch and accepted the input; the native
                        // turn starts only when the scenario releases it.
                        let _ = accepted.send(());
                        tokio::select! {
                            () = start_release.notified() => {}
                            changed = cancelled.changed() => {
                                let _ = changed;
                                interrupted_before_start.store(true, Ordering::Release);
                            }
                        }
                        start.observed(1).await?;
                        while !*cancelled.borrow() {
                            cancelled
                                .changed()
                                .await
                                .map_err(|_| crate::provider::ProviderError::Cancelled)?;
                        }
                        forge_provider_api::turn::TurnOutcome::Interrupted
                    } else {
                        start.observed(1).await?;
                        forge_provider_api::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        }
                    };
                    forge_provider_api::TurnResult::new(
                        outcome,
                        2,
                        None,
                        vec![forge_provider_api::CoveredInput::new(
                            request.submission_id.clone(),
                            request.attempt_id.clone(),
                        )],
                        Vec::new(),
                        None,
                    )
                    .map_err(|_| {
                        crate::provider::ProviderError::Observation(
                            forge_provider_api::ObservationFailure::Protocol,
                        )
                    })
                }
            },
        )
    };
    native.with_models(vec![serde_json::from_value(json!({
        "id":"fixture-model", "reasoning":["low","medium","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?]);
    let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider.provider_handle());
    let controller = SessionController::new(broker).await?;
    let session = controller.observer.session_id().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    controller.attach_input(&sink).await?;
    let scenario = async {
        prompt::apply(
            &controller,
            Request {
                id: 1,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"held-start","text":"First prompt"}),
            },
            &sink,
        )
        .await?;
        acceptances
            .recv()
            .await
            .context("the provider never accepted the first input")?;
        let command = super::super::native_settings::decode_command(&Request {
            id: 2,
            method: "session.configure".into(),
            params: json!({"effort":"high"}),
        })?;
        let configuring = {
            let controller = controller.clone();
            let sink = sink.clone();
            tokio::spawn(async move {
                controller
                    .exchange_owner
                    .execute_identified(command, sink)
                    .await
            })
        };
        // The paused clock advances only once the owner has nothing left to do, so the change
        // has had every chance to stop the turn before the provider starts it.
        tokio::time::sleep(Duration::from_secs(1)).await;
        ensure!(
            !interrupted_before_start.load(Ordering::Acquire),
            "the configuration change interrupted the turn before the provider started it"
        );
        if stop {
            let request = Request {
                id: 3,
                method: "session.turn.cancel".into(),
                params: json!({"operation_id":"held-start-stop","restore_prompt_if_no_output":true}),
            };
            tokio::time::timeout(
                Duration::from_secs(10),
                super::super::cancellation::apply(&controller, &request, &sink),
            )
            .await
            .context("stop remained blocked by deferred configuration")??;
            ensure!(interrupted_before_start.load(Ordering::Acquire), "provider never received stop");
            configuring.await??;
            ensure!(controller.broker.lock().await.snapshot()?.session.effort == "high");
            ensure!(execution_count.load(Ordering::Acquire) == 1);
            return Ok(());
        }
        start_release.notify_one();
        tokio::time::timeout(Duration::from_secs(120), configuring)
            .await
            .context("the configuration change was never answered")??
            .context("the configuration change failed")?;
        super::owner_transition::settle_exchanges(&controller, &session, 1).await?;
        let effort = controller.broker.lock().await.snapshot()?.session.effort;
        ensure!(
            effort == "high",
            "the thinking-level change was lost: {effort:?}"
        );
        let exchanges = crate::storage::HistoryReader::current(&controller.data_root)?
            .list_exchange(&session)?;
        ensure!(
            exchanges[0].state() == crate::exchange::ExchangeState::Complete
                && exchanges[0].turn.len() == 2
                && exchanges[0].collection.is_none(),
            "the turn did not run again on the new setting: {:?} {:?} with {} turns",
            exchanges[0].state(),
            exchanges[0].collection,
            exchanges[0].turn.len()
        );
        ensure!(
            execution_count.load(Ordering::Acquire) == 2,
            "the provider ran {} turns instead of the stopped turn and its rerun",
            execution_count.load(Ordering::Acquire)
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let teardown = async {
        controller.input_stopping.store(true, Ordering::Release);
        controller.coordinator.submissions.changed.notify_one();
        controller.finish_exchange_owner().await?;
        provider.provider_handle().shutdown().await?;
        controller.finish_session_events().await
    };
    let teardown = tokio::time::timeout(Duration::from_secs(120), teardown)
        .await
        .unwrap_or_else(|_| Err(anyhow::anyhow!("the owner never stopped")));
    reader.abort();
    drop(root);
    scenario.and(teardown)
}
