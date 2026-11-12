use super::super::{HarnessBroker, SessionController, prepare_new_session, prompt};
use super::bootstrap::initialize;
use super::owner_fixture::owner_transition_fixture;
use crate::session::SessionStore;
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

                let _ = dispatched.send(request.input.user_input().cloned());
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
        assert!(matches!(received, Some(Some(forge_provider_api::input::PromptInput::Text(text)))
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

/// Wait until every submitted input settled into a finished exchange and the owner is idle.
pub(super) async fn settle_exchanges(
    controller: &SessionController,
    session_id: &str,
    count: usize,
) -> Result<()> {
    loop {
        let exchanges =
            crate::storage::SqliteStore::open(&controller.data_root)?.list_exchange(session_id)?;
        if exchanges.len() == count
            && exchanges.iter().all(|exchange| exchange.is_finished())
            && controller.coordinator.execution.available_permits() == 1
            && controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty()
        {
            return Ok(());
        }
        tokio::task::yield_now().await;
    }
}

/// Submit one user input at the queue's current control revision, as the editor does, so input
/// submitted after a control operation such as undo resumes the suspended queue.
pub(super) async fn submit_input(
    service: &super::super::HarnessService,
    controller: &SessionController,
    session_id: &str,
    request_id: u64,
    input_id: &str,
    text: &str,
    sink: &forge_protocol::outbound::MessageSender,
) -> Result<()> {
    let control_revision = controller
        .coordinator
        .submissions
        .snapshot()?
        .control_revision;
    service
        .dispatch(
            Some(session_id.to_owned()),
            Request {
                id: request_id,
                method: "session.input.apply".into(),
                params: json!({"id":input_id,"control_revision":control_revision,
                    "action":{"kind":"submit","message":{
                    "id":input_id,"text":text,"intent":"auto","attachments":[]}}}),
            },
            sink,
        )
        .await
}

/// Start a session's provider and finish its first exchange, so the session holds provider
/// history that a fork copies. A session that ran no turn has none, and its fork copies nothing.
pub(super) async fn finish_first_exchange(
    service: &super::super::HarnessService,
    controller: &std::sync::Arc<SessionController>,
    session_id: &str,
    sink: &forge_protocol::outbound::MessageSender,
) -> Result<()> {
    controller.start_provider(sink).await;
    controller.attach_input(sink).await?;
    submit_input(
        service,
        controller,
        session_id,
        10,
        "settled-input",
        "Remember the word harbor",
        sink,
    )
    .await?;
    settle_exchanges(controller, session_id, 1).await
}

/// Conversation undo rewinds the provider's own history and keeps the provider session, so the
/// next prompt carries no replayed transcript.
#[tokio::test]
async fn conversation_undo_rewinds_native_history_and_keeps_provider_session() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = super::bootstrap::service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let script = controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let responses = Arc::new(std::sync::Mutex::new(Vec::<serde_json::Value>::new()));
    let received = responses.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            received
                .lock()
                .unwrap()
                .push(serde_json::from_slice(frame.bytes())?);
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    controller.attach_input(&sink).await?;
    let scenario = async {
        submit_input(&service, &controller, &session_id, 10, "kept-input", "Keep this exchange", &sink).await?;
        settle_exchanges(&controller, &session_id, 1).await?;
        submit_input(&service, &controller, &session_id, 11, "removed-input", "Roll back this exchange", &sink)
            .await?;
        settle_exchanges(&controller, &session_id, 2).await?;
        let store = crate::storage::SqliteStore::open(&controller.data_root)?;
        let exchanges = store.list_exchange(&session_id)?;
        let native = script
            .dispatch(&session_id, MockResponseCommand::History)
            .await?;
        let checkpoints = native["checkpoints"]
            .as_array()
            .context("history checkpoints missing")?
            .clone();
        assert_eq!(checkpoints.len(), 2, "{native}");
        assert_eq!(
            exchanges[0].provider_checkpoint_id.as_deref(),
            checkpoints[0].as_str()
        );
        let provider_session = store.load_session(&session_id)?.unwrap().provider_session_id;
        assert!(provider_session.is_some());

        let removed = exchanges[1].id.clone();
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 20,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"rewind-undo","action":"undo","rollback":{
                        "scope":"conversation","exchange_id":removed,"latest_exchange_id":removed}}),
                },
                &sink,
            )
            .await?;
        let response = loop {
            let response = responses
                .lock()
                .unwrap()
                .iter()
                .find(|message| message["id"] == 20)
                .cloned();
            if let Some(response) = response {
                break response;
            }
            tokio::task::yield_now().await;
        };
        assert!(response["error"].is_null(), "{response}");

        let native = script
            .dispatch(&session_id, MockResponseCommand::History)
            .await?;
        assert_eq!(native["checkpoints"], json!([checkpoints[0]]));
        let store = crate::storage::SqliteStore::open(&controller.data_root)?;
        let session = store.load_session(&session_id)?.unwrap();
        assert_eq!(session.provider_session_id, provider_session);
        assert_eq!(
            session.provider_checkpoint_id.as_deref(),
            checkpoints[0].as_str()
        );
        assert_eq!(
            store.list_exchange(&session_id)?[1].disposition(),
            crate::exchange::HistoryDisposition::RolledBack
        );

        let hold = script
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"].as_str().context("hold id missing")?.to_owned();
        submit_input(&service, &controller, &session_id, 30, "after-undo", "Continue after undo", &sink).await?;
        let pending = script
            .dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            )
            .await?;
        let prompt = pending["prompt"].as_str().context("held prompt missing")?;
        assert!(prompt.contains("Continue after undo"), "{prompt}");
        assert!(
            !prompt.contains("Roll back this exchange") && !prompt.contains("Keep this exchange"),
            "the next prompt must not replay retained history: {prompt}"
        );
        script
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        settle_exchanges(&controller, &session_id, 3).await
    }
    .await;
    service.shutdown(Duration::from_secs(5)).await?;
    reader.abort();
    scenario
}

/// A reconnect captured while a turn is still running carries a transcript revision no lower
/// than the newest `document_changed` wake already sent. Wakes are not retained, so the
/// snapshot's revision is the only thing that heals an editor that missed one.
#[tokio::test]
async fn reconnect_during_a_running_turn_covers_every_sent_document_wake() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = super::bootstrap::service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let script = controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let frames = Arc::new(std::sync::Mutex::new(Vec::<serde_json::Value>::new()));
    let received = frames.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            received
                .lock()
                .unwrap()
                .push(serde_json::from_slice(frame.bytes())?);
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    controller.attach_input(&sink).await?;
    let newest_wake = || {
        frames
            .lock()
            .unwrap()
            .iter()
            .filter(|frame| frame["event"] == "document_changed")
            .filter_map(|frame| frame["payload"]["revision"].as_u64())
            .max()
    };
    let scenario = async {
        let hold = script
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"].as_str().context("hold id missing")?.to_owned();
        submit_input(
            &service,
            &controller,
            &session_id,
            10,
            "running-input",
            "Stay running",
            &sink,
        )
        .await?;
        script
            .dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            )
            .await?;
        while newest_wake().is_none() {
            tokio::task::yield_now().await;
        }

        let reconnect = controller
            .exchange_owner
            .reconnect_snapshot(None, std::num::NonZeroU16::new(1).unwrap())
            .await?;
        let (response, confirmation) = reconnect.into_parts();
        let captured = response["timeline_revision"]
            .as_u64()
            .context("reconnect snapshot carries no transcript revision")?;
        let sent = newest_wake().context("the running turn sent no document wake")?;
        assert!(
            captured >= sent,
            "reconnect captured revision {captured} behind sent wake {sent}"
        );
        assert!(
            response["owner_publication"]["events"]
                .as_array()
                .context("publication page missing")?
                .iter()
                .all(|record| record["event"]["event"] != "document_changed"),
            "a document wake entered the retained outbox"
        );
        controller
            .exchange_owner
            .confirm_reconnect_to(confirmation, Some(sink.clone()))
            .await?;
        script
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        settle_exchanges(&controller, &session_id, 1).await
    }
    .await;
    service.shutdown(Duration::from_secs(5)).await?;
    reader.abort();
    scenario
}
