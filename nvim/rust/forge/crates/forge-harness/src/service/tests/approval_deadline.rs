use super::super::{HarnessBroker, SessionController, prompt};
use super::bootstrap::initialize;
use crate::storage::SqliteStore;
use anyhow::{Context, Result, ensure};
use forge_protocol::message::Request;
use forge_provider_api::permissions::{
    PermissionRequest, PermissionResponse, PermissionSubject, PermissionTarget,
};
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;

/// Native item identity of the command whose approval outlasts the tool timeout.
const COMMAND_ITEM: &str = "approval-gated-command";

/// A command whose approval outlasts the tool timeout times out the approval, not the turn.
///
/// Codex announces a command as running before it asks to run it, so the tool deadline starts
/// before the approval opens. Both timeouts default to ten minutes, and the tool deadline used to
/// fire first: it interrupted the turn, cancelled the approval as `TurnInterrupted`, and failed
/// the exchange. The approval's own timeout must decide instead, rejecting the request
/// automatically and continuing the requested work in a permission-timeout turn.
#[tokio::test(start_paused = true)]
async fn approval_outlasting_the_tool_timeout_continues_instead_of_failing_the_turn() -> Result<()>
{
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let (continued, mut continuations) = tokio::sync::mpsc::unbounded_channel();
    let runs = Arc::new(AtomicUsize::new(0));
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, _cancelled| {
            let first = runs.fetch_add(1, Ordering::AcqRel) == 0;
            let continued = continued.clone();
            async move {
                start.observed(1).await?;
                if first {
                    let item_id = forge_provider_api::ItemId::new(COMMAND_ITEM)
                        .expect("fixture item ID is valid");
                    let tool_event = |event| forge_provider_api::AdapterActivity::Item {
                        thread_id: request.thread_id.clone(),
                        turn_id: request.turn_id.clone(),
                        item_id: item_id.clone(),
                        event,
                    };
                    for event in [
                        forge_provider_api::ItemEvent::Started(
                            forge_provider_api::ItemStart::CommandExecution(
                                forge_provider_api::ToolDescriptor {
                                    semantics: Default::default(),
                                    name: "shell".into(),
                                    title: "Approval-gated fixture command".into(),
                                    arguments: forge_provider_api::InspectableArguments::new(
                                        json!({"command":"fixture"}),
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
                    // Open the approval after the tool deadline starts, as Codex does, so the
                    // unsuspended tool deadline would expire before the approval timeout.
                    tokio::time::sleep(Duration::from_secs(1)).await;
                    let response = request
                        .permissions
                        .authorize(
                            &request.scope,
                            PermissionRequest {
                                id: "fixture-approval".into(),
                                provider: forge_providers::registry::nonselectable_id().unwrap(),
                                subject: PermissionSubject::Tool(item_id.clone()),
                                reason: None,
                                target_list: vec![PermissionTarget::Tool {
                                    target: "fixture-approval".into(),
                                }],
                            },
                        )
                        .await;
                    let outcome = match response {
                        PermissionResponse::Allow => crate::provider::ToolOutcome::Succeeded,
                        PermissionResponse::Deny { .. } | PermissionResponse::Cancelled => {
                            crate::provider::ToolOutcome::Denied {
                                reason: forge_provider_api::diagnostic::SanitizedMessage::new(
                                    "fixture approval was not granted",
                                )
                                .expect("static diagnostic is valid"),
                            }
                        }
                    };
                    sink.send_wait(tool_event(forge_provider_api::ItemEvent::Completed(
                        forge_provider_api::ItemCompletion::CommandExecution(
                            forge_provider_api::ToolCompletion {
                                outcome,
                                descriptor: None,
                                output: None,
                                changes: Default::default(),
                            },
                        ),
                    )))
                    .await?;
                } else {
                    let _ = continued.send(matches!(
                        request.input.kind(),
                        forge_provider_api::forge::TurnKind::PermissionTimeout { feedback }
                            if feedback.contains("Continue authorized work")
                                && !feedback.contains("\nRequest\n")
                    ));
                }
                forge_provider_api::TurnResult::new(
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
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
    );
    native.with_models(vec![serde_json::from_value(json!({
        "id":"fixture-model", "reasoning":["low","medium","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?]);
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    let provider =
        crate::provider::test_support::ProviderTestSession::from_native_with_permissions(
            native,
            broker.permission_coordinator(),
        )?;
    broker.set_provider_for_test(provider.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    // Session services include the approval-timeout watcher that dispatches the continuation.
    controller.attach_session_events(&sink).await?;
    controller.attach_input(&sink).await?;
    let scenario = async {
        prompt::apply(
            &controller,
            Request {
                id: 900,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"approval-deadline","text":"Run the gated command"}),
            },
            &sink,
        )
        .await?;
        // The paused clock runs this bound out at once when no continuation is dispatched, so a
        // lost continuation fails with the exchange state instead of hanging.
        let continuation =
            tokio::time::timeout(Duration::from_secs(3600), continuations.recv()).await;
        let exchange = SqliteStore::open(&controller.data_root)?
            .list_exchange(controller.observer.session_id())?
            .into_iter()
            .find(|exchange| exchange.ordinal == 1)
            .context("the gated command lost its exchange")?;
        ensure!(
            exchange.state() == crate::exchange::ExchangeState::Complete,
            "the tool timeout ended the gated turn as {:?}",
            exchange.state()
        );
        let resolution = serde_json::to_value(&exchange.approval_resolution)?;
        ensure!(
            resolution[0]["Resolved"]["targets"][0]["source"] == "Timeout",
            "the approval did not end through its own timeout: {resolution}"
        );
        ensure!(
            resolution[0]["Resolved"]["scope"]["Item"]["item_id"] == COMMAND_ITEM,
            "the approval was not scoped to its tool item: {resolution}"
        );
        ensure!(
            continuation
                .ok()
                .flatten()
                .context("the timed-out approval dispatched no continuation turn")?,
            "the continuation turn was not a permission-timeout turn"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    let teardown = async {
        controller.finish_exchange_owner().await?;
        provider.provider_handle().shutdown().await?;
        controller.finish_session_events().await
    }
    .await;
    reader.abort();
    drop(controller);
    drop(root);
    scenario?;
    teardown
}
