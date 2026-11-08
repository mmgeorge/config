use super::super::{HarnessBroker, SessionController, prepare_new_session, prompt};
use super::bootstrap::initialize;
use super::owner_fixture::owner_transition_fixture;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
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
