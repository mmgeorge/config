mod approval_deadline;
mod bootstrap;
mod compaction;
mod fork;
mod native_agents;
mod owner_cancel;
mod owner_dispatch;
mod owner_fixture;
mod owner_storage_failure;
mod owner_transition;
mod plan_review_workflow;
mod reconnect_overlap;
mod rollback;
mod session_lifecycle;
mod session_reopen;
mod settings_defaults;
mod stop_backstop;
mod task_state;

use super::*;
use crate::provider::{ProviderCatalogRequest, TextGeneration};
use bootstrap::{initialize, service};

#[tokio::test]
async fn lsp_callbacks_enforce_session_and_request_ownership() -> Result<()> {
    async fn callback(
        service: &HarnessService,
        session_id: &str,
        request: Request,
        sink: &MessageSender,
        output: &mut forge_protocol::outbound::MessageReceiver,
    ) -> Result<Value> {
        let envelope = json!({"id":request.id,"method":request.method,
            "params":request.params,"session_id":session_id});
        service
            .dispatch(Some(session_id.into()), request, sink)
            .await?;
        let response: Value = serde_json::from_slice(
            output
                .recv()
                .await?
                .context("LSP callback response missing")?
                .bytes(),
        )?;
        assert_eq!(response["id"], envelope["id"]);
        assert!(response.get("error").is_none(), "{response}");
        eprintln!(
            "API_CAPTURE {}",
            json!({"request":envelope,"response":response})
        );
        Ok(response["result"].clone())
    }

    let fixture = tempfile::tempdir()?;
    let source = "pub fn callback_source() {}\n";
    let path = fixture.path().join("source.rs");
    std::fs::write(&path, source)?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let registry = service.registry().await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    registry.runtime.lsp.attach(sink.clone());
    registry.runtime.lsp.set_enabled(&session_id, true);
    let policy = Arc::new(
        crate::permissions::matcher::CompiledPermissionDocument::compile(
            crate::permissions::document::default_permission_document(),
            fixture.path(),
        ),
    );
    let (scope, lease) = registry.runtime.lsp.scope(
        session_id.clone(),
        "callback-interaction".into(),
        registry.initialize.data_root.clone().into(),
        fixture.path().into(),
        policy,
        tokio::sync::watch::channel(false).1,
    );
    let work = tokio::spawn(async move {
        scope
            .invoke(
                "lsp_context",
                json!({"path":"source.rs","line":1,"column":1}),
            )
            .await
    });
    let event: Value = serde_json::from_slice(
        output
            .recv()
            .await?
            .context("LSP editor request missing")?
            .bytes(),
    )?;
    let editor_request_id = event["payload"]["id"]
        .as_str()
        .context("LSP editor identity missing")?;
    let authorization = Request {
        id: 2,
        method: "session.lsp.authorize".into(),
        params: json!({"id":editor_request_id,"paths":[path]}),
    };
    assert!(
        service
            .dispatch(
                Some("unrelated-session".into()),
                authorization.clone(),
                &sink
            )
            .await
            .is_err()
    );
    let authorized = callback(&service, &session_id, authorization, &sink, &mut output).await?;
    assert_eq!(authorized[path.to_string_lossy().as_ref()], true);
    let analyzed = callback(
        &service,
        &session_id,
        Request {
            id: 3,
            method: "session.lsp.analyze".into(),
            params: json!({"id":editor_request_id,"path":path,"source":source}),
        },
        &sink,
        &mut output,
    )
    .await?;
    assert!(
        analyzed["declarations"]
            .as_array()
            .unwrap()
            .iter()
            .any(|declaration| declaration["name"] == "callback_source")
    );
    let response = Request {
        id: 4,
        method: "session.lsp.respond".into(),
        params: json!({"id":editor_request_id,"result":{"rows":[{"path":path,
            "line":1,"column":8,"text":"pub fn callback_source()","modified":false}],"source":"buffer"}}),
    };
    assert_eq!(
        callback(&service, &session_id, response.clone(), &sink, &mut output).await?["accepted"],
        true
    );
    assert!(work.await??.contains("callback_source"));
    assert!(
        service
            .dispatch(Some(session_id), response, &sink)
            .await
            .is_err()
    );
    drop(lease);
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn isolated_generation_uses_configured_model_without_a_conversation() -> Result<()> {
    let requests = Arc::new(Mutex::new(Vec::new()));
    let shutdown = Arc::new(AtomicBool::new(false));
    let native = generation_provider(requests.clone(), shutdown.clone(), false);
    let provider_fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let request = ProviderCatalogRequest {
        execution_owner: None,
        harness_session_id: "generation-fixture".into(),
        workspace: "commit-workspace".into(),
        access: Default::default(),
        approval_mode: crate::session::ApprovalMode::Ask,
        provider_session_id: None,
    };
    let text = generate_isolated_text(
        provider_fixture.as_provider(),
        request.clone(),
        TextGeneration::CommitMessage,
        generation_selection(),
        "Staged diff:\n+change",
        crate::background::CancellationToken::new(),
    )
    .await?;
    assert_eq!(text, "fix: preserve drafts\n\nKeep existing text.");
    assert!(shutdown.load(Ordering::Acquire));
    {
        let requests = requests.lock().await;
        assert_eq!(requests.len(), 1);
        let request = &requests[0];
        assert_eq!(request.workspace, "commit-workspace");
        assert_eq!(request.model, "selected-model");
        assert_eq!(request.service_tier, forge_provider_api::ServiceTier::Fast);
        assert!(
            matches!(request.reasoning, forge_provider_api::generation::ReasoningSelection::Level(ref level) if level == "low")
        );
        assert!(request.instructions.contains("Conventional Commit"));
        assert_eq!(request.input, "Staged diff:\n+change");
    }
    let rejected_calls = Arc::new(Mutex::new(Vec::new()));
    let rejected_shutdown = Arc::new(AtomicBool::new(false));
    let rejected_native =
        generation_provider(rejected_calls.clone(), rejected_shutdown.clone(), false);
    let rejected_provider =
        crate::provider::test_support::ProviderTestSession::from_native(rejected_native)?;
    let mut unsupported_speed = generation_selection();
    unsupported_speed.service_tier = forge_provider_api::ServiceTier::Ultrafast;
    let error = generate_isolated_text(
        rejected_provider.as_provider(),
        request.clone(),
        TextGeneration::CommitMessage,
        unsupported_speed,
        "diff",
        crate::background::CancellationToken::new(),
    )
    .await
    .unwrap_err();
    assert!(format!("{error:#}").contains("ServiceTier"));
    assert!(rejected_calls.lock().await.is_empty());
    assert!(rejected_shutdown.load(Ordering::Acquire));
    let cancelled_calls = Arc::new(Mutex::new(Vec::new()));
    let cancelled_shutdown = Arc::new(AtomicBool::new(false));
    let cancelled_native =
        generation_provider(cancelled_calls.clone(), cancelled_shutdown.clone(), false);
    let cancelled_provider =
        crate::provider::test_support::ProviderTestSession::from_native(cancelled_native)?;
    let cancellation = crate::background::CancellationToken::new();
    cancellation.cancel();
    let error = generate_isolated_text(
        cancelled_provider.as_provider(),
        request.clone(),
        TextGeneration::CommitMessage,
        generation_selection(),
        "diff",
        cancellation,
    )
    .await
    .unwrap_err();
    assert!(format!("{error:#}").contains("Cancelled"));
    assert!(cancelled_calls.lock().await.is_empty());
    assert!(cancelled_shutdown.load(Ordering::Acquire));
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
    )
    .expect("mock fixture descriptor");
    let unsupported_native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        |request, _, start, _| async move {
            start.observed(1).await?;
            Ok(crate::provider::test_support::native_result(
                &request,
                forge_provider_api::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
            ))
        },
    );
    let unsupported =
        crate::provider::test_support::ProviderTestSession::from_native(unsupported_native)?;
    let error = generate_isolated_text(
        unsupported.as_provider(),
        request,
        TextGeneration::CommitMessage,
        generation_selection(),
        "Staged diff:\n+change",
        crate::background::CancellationToken::new(),
    )
    .await
    .unwrap_err();
    let diagnostic = format!("{error:#}");
    assert!(
        diagnostic.contains("commit_message generation failed"),
        "{diagnostic}"
    );
    assert!(diagnostic.contains("ModelSelection"), "{diagnostic}");
    let requests = Arc::new(Mutex::new(Vec::new()));
    let shutdown = Arc::new(AtomicBool::new(false));
    let failing_native = generation_provider(requests, shutdown.clone(), true);
    let failing = crate::provider::test_support::ProviderTestSession::from_native(failing_native)?;
    let request = ProviderCatalogRequest {
        execution_owner: None,
        harness_session_id: "generation-failure".into(),
        workspace: "commit-workspace".into(),
        access: Default::default(),
        approval_mode: crate::session::ApprovalMode::Ask,
        provider_session_id: None,
    };
    let failure = generate_isolated_text(
        failing.as_provider(),
        request,
        TextGeneration::CommitMessage,
        generation_selection(),
        "Staged diff:\n+change",
        crate::background::CancellationToken::new(),
    )
    .await
    .unwrap_err();
    let diagnostic = format!("{failure:#}");
    assert!(diagnostic.contains("TurnStart"), "{diagnostic}");
    assert!(!diagnostic.contains("fixture generation failure"));
    assert!(shutdown.load(Ordering::Acquire));
    Ok(())
}

struct GenerationCall {
    workspace: String,
    model: String,
    reasoning: forge_provider_api::generation::ReasoningSelection,
    service_tier: forge_provider_api::ServiceTier,
    instructions: String,
    input: String,
}

/// Select accelerated generation independently of reasoning effort.
fn generation_selection() -> forge_provider_api::ModelSelection {
    forge_provider_api::ModelSelection {
        model: Some("selected-model".into()),
        reasoning: forge_provider_api::ReasoningSelection::Level("low".into()),
        service_tier: forge_provider_api::ServiceTier::Fast,
        ..Default::default()
    }
}

fn generation_provider(
    requests: Arc<Mutex<Vec<GenerationCall>>>,
    shutdown: Arc<AtomicBool>,
    fail: bool,
) -> Arc<crate::provider::test_support::EventFixtureProvider> {
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
    )
    .expect("mock fixture descriptor");
    let calls = requests.clone();
    let provider = crate::provider::test_support::EventFixtureProvider::new(descriptor, move |request, sink, start, _| {
        let calls = calls.clone();
        async move {
            let mut calls = calls.lock().await;
            let opened = calls.last_mut().expect("open precedes input");
            opened.input = match request.input.user_input() {
                Some(forge_provider_api::PromptInput::Text(text)) => text.clone(),
                _ => panic!("utility input must preserve plain text"),
            };
            assert!(request.tools.is_empty());
            assert!(request.attachments.is_empty());
            assert!(request.recovered_inputs.is_empty());
            drop(calls);
            if fail {
                return Err(forge_provider_api::ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(forge_provider_api::ProviderOperation::TurnStart, forge_provider_api::ProviderFailureKind::Runtime),
                ));
            }
            let address = crate::provider::TurnAddress { scope: request.scope.clone(), thread_id: request.thread_id.clone(), turn_id: request.turn_id.clone() };
            start.observed(1).await?;
            for event in crate::provider::test_support::message(&address, "thought", Some(crate::provider::TestMessageContent::Reasoning(forge_provider_api::ReasoningKind::Full)), "private reasoning") {
                sink.send_wait(crate::provider::test_support::activity(event)?).await?;
            }
            for event in crate::provider::test_support::message(&address, "answer", Some(crate::provider::TestMessageContent::Response), "```text\nfix: preserve drafts\n\nKeep existing text.\n```") {
                sink.send_wait(crate::provider::test_support::activity(event)?).await?;
            }
            Ok(crate::provider::test_support::native_result(&request, forge_provider_api::turn::TurnOutcome::Completed { stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal }))
        }
    }).with_models(vec![serde_json::from_value(json!({"id":"selected-model","reasoning":["low","high"],"service_tiers":["default","fast"],"context_window":[],"vision":false,"is_default":true})).expect("normalized fixture model")]);
    provider.with_session_open(move |request| {
        let requests = requests.clone();
        async move {
            assert!(matches!(
                request.history,
                forge_provider_api::SessionHistory::Temporary
            ));
            assert!(request.tools.is_empty());
            assert!(request.tool_endpoint.is_none());
            assert!(request.configuration.resources.skills.is_empty());
            assert!(request.configuration.resources.mcp.is_empty());
            assert_eq!(
                request.configuration.approval,
                crate::session::ApprovalMode::Deny
            );
            requests.lock().await.push(GenerationCall {
                workspace: request.workspace,
                model: request
                    .configuration
                    .model
                    .model
                    .expect("catalog resolved explicit model"),
                reasoning: request.configuration.model.reasoning,
                service_tier: request.configuration.model.service_tier,
                instructions: request.system_instructions,
                input: String::new(),
            });
            Ok(())
        }
    });
    let shutdown_called = shutdown.clone();
    provider.with_shutdown(move || {
        let shutdown_called = shutdown_called.clone();
        async move {
            shutdown_called.store(true, Ordering::Release);
            Ok(())
        }
    });
    provider
}

#[tokio::test]
async fn session_queries_complete_while_execution_owns_the_broker() {
    let fixture = tempfile::tempdir().unwrap();
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await
        .unwrap();
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let registry = service.registry().await.unwrap();
    let controller = registry.resolve(&session_id).await.unwrap();
    let inactive = prepare_new_session(
        std::path::Path::new(&registry.initialize.data_root),
        &registry.initialize.client_id,
        "mock",
        false,
        &session_id,
        &json!({"name":"inactive deletion target"}),
        0,
    )
    .unwrap();
    let _execution = Arc::clone(&controller.coordinator.execution)
        .try_acquire_owned()
        .unwrap();
    let broker = controller.broker.lock().await;
    let (sink, mut output) = forge_protocol::outbound::channel();
    for (method, params) in [
        (
            "session.prompt.history.record",
            json!({"text":"prompt recorded during execution"}),
        ),
        (
            "session.rename",
            json!({"session_id":session_id,"name":"active rename"}),
        ),
        ("session.delete", json!({"session_id":inactive.id})),
        ("session.state.get", json!({})),
        ("session.plan.list", json!({})),
        ("session.task.get", json!({})),
        ("session.exchange.list", json!({})),
        ("session.list", json!({"scope":"all"})),
        ("session.preview", json!({"session_id":session_id})),
        ("permissions.document.open", json!({})),
    ] {
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: method.into(),
                    params,
                },
                &sink,
            )
            .await
            .unwrap();
        // A state read reattaches the session event stream, so events may precede the response.
        let value = response_for(&mut output, 2).await.unwrap();
        assert!(value["error"].is_null(), "{method}: {value}");
        if method == "session.state.get" {
            assert_eq!(value["result"]["session"]["name"], "active rename");
            assert_eq!(
                value["result"]["prompt_history"][0]["text"],
                "prompt recorded during execution"
            );
            assert_eq!(
                value["result"]["prompt_history"][0]["attachments"],
                json!([])
            );
        }
    }
    assert_eq!(broker.snapshot().unwrap().session.name, "active rename");
    for (offset, method) in ["session.new"].into_iter().enumerate() {
        let request_id = 10 + offset as u64;
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: request_id,
                    method: method.into(),
                    params: json!({}),
                },
                &sink,
            )
            .await
            .unwrap();
        loop {
            let frame = output.recv().await.unwrap().unwrap();
            let response: Value = serde_json::from_slice(frame.bytes()).unwrap();
            if response["id"] != request_id {
                continue;
            }
            assert!(response["error"].is_null(), "{response}");
            assert_ne!(response["result"]["session"]["id"], session_id);
            break;
        }
    }
    drop(broker);
    drop(_execution);
    service.shutdown(Duration::from_secs(1)).await.unwrap();
}

/// Read one response for `id` from a sink, skipping interleaved events.
async fn response_for(
    output: &mut forge_protocol::outbound::MessageReceiver,
    id: u64,
) -> Result<Value> {
    loop {
        let frame = output.recv().await?.context("response stream closed")?;
        let message: Value = serde_json::from_slice(frame.bytes())?;
        if message["id"] == id {
            return Ok(message);
        }
    }
}

#[tokio::test]
async fn task_input_reads_the_current_session_revision() -> Result<()> {
    use crate::input::InputPurpose;
    use crate::task::{Task, TaskChange};
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
    let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let task = controller
        .task_controller
        .change(
            &mut store,
            None,
            TaskChange::Install(Box::new(Task::goal("Finish".into(), Default::default(), 1))),
            1,
        )?
        .unwrap();
    let completed_turn = crate::task::TaskTurnId {
        exchange_ordinal: 1,
        turn_ordinal: 1,
    };
    controller.task_controller.observe_goal(
        &mut store,
        &task.permit(),
        completed_turn.clone(),
        crate::goal::TurnEvidence {
            tool_called: true,
            ..Default::default()
        },
        2,
    )?;
    let task = controller.task_controller.current()?.unwrap();
    let pending = InputPurpose::TaskContinuation {
        revision: task.revision.clone(),
        id: task
            .turn
            .as_ref()
            .unwrap()
            .continuation
            .as_ref()
            .unwrap()
            .id
            .clone(),
        completed_turn_id: completed_turn,
        reason: "Continue".into(),
    };
    assert!(controller.task_input_is_current(&pending)?);
    let paused = controller
        .task_controller
        .change(&mut store, Some(&task.revision), TaskChange::Pause, 2)?
        .unwrap();
    assert!(!controller.task_input_is_current(&pending)?);
    assert!(controller.task_input_is_current(&InputPurpose::User)?);
    let replacement = controller
        .task_controller
        .change(
            &mut store,
            Some(&paused.revision),
            TaskChange::Install(Box::new(Task::goal(
                "Replacement".into(),
                Default::default(),
                3,
            ))),
            3,
        )?
        .unwrap();
    assert!(!controller.task_input_is_current(&pending)?);
    assert!(controller.task_input_is_current(&InputPurpose::TaskEntry {
        revision: replacement.revision.clone(),
        id: crate::task::TaskInputId::fresh(),
        start: Some(crate::exchange::TaskStart {
            kind: replacement.kind(),
            title: replacement.title().into(),
        }),
    })?);
    controller.task_controller.change(
        &mut store,
        Some(&replacement.revision),
        TaskChange::Clear,
        4,
    )?;
    assert!(!controller.task_input_is_current(&pending)?);
    assert!(controller.task_input_is_current(&InputPurpose::User)?);
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

/// Retract a cleanly cancelled, undispatched followup so the composer restores it, and never
/// replay it when later input arrives.
#[tokio::test]
async fn cancelled_deferred_input_is_retracted_without_replay() -> Result<()> {
    use crate::input::InputOperation;
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let execution = controller
        .coordinator
        .execution
        .clone()
        .acquire_owned()
        .await?;
    let mut active = controller.parent_input.activate(None)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let source = fixture.path().join("retained.png");
    std::fs::write(&source, b"\x89PNG\r\n\x1a\nretained image")?;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.attachment.import".into(),
                params: json!({"path":source,"number":1}),
            },
            &sink,
        )
        .await?;
    let imported: Value = serde_json::from_slice(
        output
            .recv()
            .await?
            .context("missing imported attachment")?
            .bytes(),
    )?;
    let attachment = imported["result"].clone();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let operation: InputOperation = serde_json::from_value(json!({
        "id":"deferred-op","action":{"kind":"submit","message":{
            "id":"deferred-input","text":"[Image #1] café 東京","intent":"auto","attachments":[attachment]
        }}
    }))?;
    controller.coordinator.submissions.apply(operation)?;
    controller.attach_input(&sink).await?;
    let command = active.receive().await.context("missing deferred input")?;
    let submission = command
        .submission
        .as_ref()
        .context("missing deferred receipt")?
        .clone();
    assert_eq!(
        submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Prepared
    );
    controller.coordinator.submissions.suspend(true, None)?;
    // The user's cancel, not lane retirement alone, retracts undispatched steering.
    controller.cancellation.request();
    active.retire();
    command.cancel_undispatched().await?;
    let deferred_retained = || -> Result<bool> {
        Ok(controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .iter()
            .any(|message| message.id == "deferred-input"))
    };
    async {
        while deferred_retained()? {
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await?;
    assert_eq!(
        submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Cancelled
    );
    let fresh: InputOperation = serde_json::from_value(json!({
        "id":"fresh-op","control_revision":controller.coordinator.submissions.snapshot()?.control_revision,
        "action":{"kind":"submit","message":{"id":"fresh-input","text":"A distinct request","intent":"auto"}}
    }))?;
    controller.coordinator.submissions.apply(fresh)?;
    assert!(!deferred_retained()?, "cancelled input was replayed");
    controller.coordinator.submissions.suspend(true, None)?;
    drop(active);
    drop(execution);
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await??;
    Ok(())
}

/// Steering that a turn never dispatched before ending on a user decision returns to the queue.
///
/// Settlement cancels the undispatched receipt when the turn's lane retires, so without a user
/// cancel the queue must keep the message instead of treating that retirement as a retraction.
#[tokio::test]
async fn retired_lane_returns_undispatched_steering_to_the_queue() -> Result<()> {
    use crate::input::{InputDelivery, InputOperation};
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let execution = controller
        .coordinator
        .execution
        .clone()
        .acquire_owned()
        .await?;
    let mut active = controller.parent_input.activate(None)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let operation: InputOperation = serde_json::from_value(json!({
        "id":"steer-op","action":{"kind":"submit","message":{
            "id":"steer-input","text":"Also update the changelog","intent":"auto"
        }}
    }))?;
    controller.coordinator.submissions.apply(operation)?;
    controller.attach_input(&sink).await?;
    let command = active.receive().await.context("missing steering input")?;
    let submission = command
        .submission
        .as_ref()
        .context("missing steering receipt")?
        .clone();
    // Hold delivery while the turn ends so the requeued message stays observable.
    controller.coordinator.submissions.suspend(true, None)?;
    active.retire();
    command.cancel_undispatched().await?;
    let steering_delivery = || -> Result<Option<InputDelivery>> {
        Ok(controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .iter()
            .find(|message| message.id == "steer-input")
            .map(crate::input::InputMessage::delivery))
    };
    async {
        while steering_delivery()? == Some(InputDelivery::Delivering) {
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await?;
    assert_eq!(
        steering_delivery()?,
        Some(InputDelivery::Queued),
        "undispatched steering was removed instead of returning to the queue"
    );
    assert_eq!(
        submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Cancelled
    );
    drop(active);
    drop(execution);
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await??;
    Ok(())
}

/// An unlocked broker does not mean its foreground owner has settled.
#[tokio::test]
async fn active_cancel_collects_native_terminal_and_preserves_queued_input() -> Result<()> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    // Pause is a task control and requires a selected task, so only turn cancellation collects
    // this bare root turn.
    for (method, params) in [("session.turn.cancel", json!({}))] {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let provider = controller.provider_handle();
        let control = provider.mock_response_control().unwrap();
        let hold = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let responses = Arc::new(std::sync::Mutex::new(Vec::<Value>::new()));
        let seen = responses.clone();
        let reader = tokio::spawn(async move {
            while let Some(frame) = output.recv().await? {
                seen.lock()
                    .unwrap()
                    .push(serde_json::from_slice(frame.bytes())?);
            }
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let submit = |id: &str, intent: InputIntent| InputOperation {
            id: format!("op-{id}"),
            control_revision: 0,
            after_control: None,
            action: InputAction::Submit {
                message: Box::new(InputMessage::queued(crate::input::InputDraft {
                    id: id.into(),
                    text: id.into(),
                    purpose: crate::input::InputPurpose::User,
                    intent,
                    attachments: vec![],
                    config: None,
                })),
            },
        };
        controller
            .coordinator
            .submissions
            .apply(submit("active", InputIntent::Auto))?;
        control
            .dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold["id"].as_str().unwrap().into(),
                },
            )
            .await?;
        controller
            .coordinator
            .submissions
            .apply(submit("queued", InputIntent::Queue))?;
        // A reducer step may still hold the broker on the blocking pool; wait for it to finish.
        drop(controller.broker.lock().await);
        assert!(
            controller.broker.try_lock().is_ok(),
            "fixture must exercise an unlocked broker"
        );
        let question_ids = controller.questions.ask(
            "active-cancel-questions",
            &SqliteStore::read_only(&controller.data_root)?
                .latest_exchange(&session_id)?
                .context("active exchange is missing")?
                .id,
            crate::question::QuestionOrigin::Tool(None),
            [
                crate::question::QuestionRequirement::Required,
                crate::question::QuestionRequirement::Optional,
            ]
            .into_iter()
            .map(|requirement| crate::question::QuestionDraft {
                header: "Layout".into(),
                question: "Which layout?".into(),
                requirement,
                options: Vec::new(),
            })
            .collect(),
        )?;
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 23,
                    method: method.into(),
                    params,
                },
                &sink,
            )
            .await?;
        async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        }
        .await;
        async {
            loop {
                if let Some(response) = responses
                    .lock()
                    .unwrap()
                    .iter()
                    .find(|value| value["id"] == 23)
                    .cloned()
                {
                    anyhow::ensure!(
                        response.get("error").is_none(),
                        "cancel RPC failed: {response}"
                    );
                    break Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        }
        .await?;
        let store = SqliteStore::open(&controller.data_root)?;
        let exchanges = store.list_exchange(&session_id)?;
        assert_eq!(exchanges.len(), 1);
        assert_eq!(
            exchanges[0].state(),
            crate::exchange::ExchangeState::Cancelled
        );
        assert!(matches!(
            controller.questions.snapshot()?.questions[0].resolution,
            crate::question::QuestionResolution::Retired { .. }
        ));
        assert!(
            controller
                .questions
                .apply(crate::question::QuestionCommand {
                    operation_id: "required-after-cancel".into(),
                    question_id: question_ids[0].clone(),
                    revision: 1,
                    action: crate::question::QuestionAction::Answer {
                        answer: crate::question::QuestionAnswer::Other {
                            text: "Tabs".into()
                        }
                    },
                })
                .is_err()
        );
        controller
            .questions
            .apply(crate::question::QuestionCommand {
                operation_id: "optional-after-cancel".into(),
                question_id: question_ids[1].clone(),
                revision: 1,
                action: crate::question::QuestionAction::Answer {
                    answer: crate::question::QuestionAnswer::Other {
                        text: "Tabs".into(),
                    },
                },
            })?;
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        assert!(controller.questions.outbox()?.is_empty());
        assert!(
            !controller.task_input_is_current(&crate::input::InputPurpose::Question {
                additional_replies: Vec::new(),
                content_revision: None,
                question_id: question_ids[1].clone(),
                task_id: None,
                message_kind: crate::question::QuestionMessageKind::Answer,
                reply: crate::provider::test_support::question_reply(
                    &question_ids[1],
                    forge_provider_api::QuestionReplyResponse::Answer {
                        answer: "Tabs".into(),
                        required_questions_remain: false
                    }
                ),
            })?
        );
        assert_eq!(
            SqliteStore::read_only(&controller.data_root)?
                .list_exchange(&session_id)?
                .len(),
            1,
            "late answer dispatched a new exchange"
        );
        assert!(controller.coordinator.submissions.snapshot()?.suspended);
        assert!(
            controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .iter()
                .any(|message| message.id == "queued")
        );
        assert!(!controller.observer.session_events().is_closed());
        service.shutdown(Duration::from_secs(5)).await?;
        drop(sink);
        reader.await??;
    }
    Ok(())
}

/// Keep the session working from a finished turn until its queued follow-up starts.
///
/// The finished exchange alone resolves to an idle phase, and the follow-up's exchange is
/// admitted a few owner steps later. Publishing the idle phase in between closed the editor's
/// status row and reopened it, which scrolled the transcript twice at every hand-off.
#[tokio::test]
async fn queued_follow_up_hand_off_never_publishes_idle() -> Result<()> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let provider = controller.provider_handle();
    let control = provider.mock_response_control().unwrap();
    let hold = || MockResponseCommand::Hold {
        mode: None,
        any_session: false,
    };
    let held_id = |held: &Value| held["id"].as_str().unwrap().to_owned();
    let first = held_id(&control.dispatch(&session_id, hold()).await?);
    let (sink, mut output) = forge_protocol::outbound::channel();
    let published = Arc::new(std::sync::Mutex::new(Vec::<String>::new()));
    let recorded = published.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] == "document_changed"
                && let Some(kind) = event["payload"]["status"]["kind"].as_str()
            {
                // The hand-off is a working phase marked by its activity.
                let phase = event["payload"]["status"]["activity"]
                    .as_str()
                    .filter(|activity| *activity == "handing_off")
                    .unwrap_or(kind);
                recorded.lock().unwrap().push(phase.to_owned());
            }
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let submit = |id: &str, intent: InputIntent| InputOperation {
        id: format!("operation-{id}"),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: id.into(),
                text: format!("input {id}"),
                purpose: crate::input::InputPurpose::User,
                intent,
                attachments: vec![],
                config: None,
            })),
        },
    };
    controller
        .coordinator
        .submissions
        .apply(submit("active", InputIntent::Auto))?;
    control
        .dispatch(&session_id, MockResponseCommand::Wait { id: first.clone() })
        .await?;
    let second = held_id(&control.dispatch(&session_id, hold()).await?);
    controller
        .coordinator
        .submissions
        .apply(submit("follow-up", InputIntent::Queue))?;
    // The reader records frames asynchronously, so the idle phase that attachment published may
    // still be queued behind this point. The channel is ordered, so once the reader has recorded
    // the held exchange's working phase, every earlier idle frame precedes the hand-off window.
    let hand_off = loop {
        let working = {
            let recorded = published.lock().unwrap();
            recorded
                .iter()
                .any(|phase| phase != "idle")
                .then_some(recorded.len())
        };
        if let Some(recorded) = working {
            break recorded;
        }
        tokio::task::yield_now().await;
    };
    control
        .dispatch(&session_id, MockResponseCommand::Release { id: first })
        .await?;
    control
        .dispatch(
            &session_id,
            MockResponseCommand::Wait { id: second.clone() },
        )
        .await?;
    control
        .dispatch(&session_id, MockResponseCommand::Release { id: second })
        .await?;
    // The last phase is idle only after the follow-up's own exchange settled, which orders
    // every hand-off publication before it.
    let phases = async {
        loop {
            let settled = SqliteStore::read_only(&controller.data_root)?
                .list_exchange(&session_id)?
                .iter()
                .filter(|exchange| exchange.is_finished())
                .count()
                == 2;
            let phases = published.lock().unwrap()[hand_off..].to_vec();
            if settled && phases.last().is_some_and(|phase| phase == "idle") {
                break Ok::<_, anyhow::Error>(phases);
            }
            tokio::task::yield_now().await;
        }
    }
    .await?;
    let idle = phases
        .iter()
        .position(|phase| phase == "idle")
        .context("the settled session published no idle phase")?;
    assert!(
        phases[idle..].iter().all(|phase| phase == "idle"),
        "the session published an idle phase before its queued follow-up started: {phases:?}"
    );
    assert!(
        phases[..idle].iter().any(|phase| phase == "handing_off"),
        "the hand-off between the turns was not marked as one: {phases:?}"
    );
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await??;
    Ok(())
}

/// Keep a goal working from a finished turn until the continuation it reserved starts.
///
/// The goal reserves its continuation in task state when the turn's evidence is applied, and the
/// continuation becomes queued input only after the finished input is removed. The hand-off
/// predicted only queued user messages, so the session published an idle phase in between and
/// the editor closed the status split for a frame at every goal turn.
#[tokio::test]
async fn goal_continuation_hand_off_never_publishes_idle() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let provider = controller.provider_handle();
    let control = provider.mock_response_control().unwrap();
    let hold = || MockResponseCommand::Hold {
        mode: None,
        any_session: false,
    };
    let held_id = |held: &Value| held["id"].as_str().unwrap().to_owned();
    let first = held_id(&control.dispatch(&session_id, hold()).await?);
    let (sink, mut output) = forge_protocol::outbound::channel();
    // Each published phase with the clock it reads, from the observed session snapshot.
    let published = Arc::new(std::sync::Mutex::new(Vec::<(String, Option<i64>)>::new()));
    let recorded = published.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let event: Value = serde_json::from_slice(frame.bytes())?;
            let status = &event["payload"]["snapshot"]["status"];
            if event["event"] == "session_changed"
                && let Some(kind) = status["kind"].as_str()
            {
                let phase = status["activity"]
                    .as_str()
                    .filter(|activity| *activity == "handing_off")
                    .unwrap_or(kind);
                recorded
                    .lock()
                    .unwrap()
                    .push((phase.to_owned(), status["started_at_ms"].as_i64()));
            }
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.goal.set".into(),
                params: json!({"operation_id":"goal-hand-off","objective":"Keep working"}),
            },
            &sink,
        )
        .await?;
    control
        .dispatch(&session_id, MockResponseCommand::Wait { id: first.clone() })
        .await?;
    let second = held_id(&control.dispatch(&session_id, hold()).await?);
    control
        .dispatch(&session_id, MockResponseCommand::Release { id: first })
        .await?;
    control
        .dispatch(
            &session_id,
            MockResponseCommand::Wait { id: second.clone() },
        )
        .await?;
    // The continuation's own clock marks the end of the hand-off; the channel is ordered, so
    // every hand-off publication was recorded before it.
    let phases = loop {
        let phases = published.lock().unwrap().clone();
        let clocks: std::collections::BTreeSet<_> =
            phases.iter().filter_map(|(_, clock)| *clock).collect();
        if clocks.len() >= 2 {
            break phases;
        }
        tokio::task::yield_now().await;
    };
    let first_clock = phases.iter().find_map(|(_, clock)| *clock);
    let working = phases
        .iter()
        .position(|(_, clock)| clock.is_some())
        .context("the goal published no working phase")?;
    let continued = phases
        .iter()
        .position(|(_, clock)| clock.is_some() && *clock != first_clock)
        .context("the continuation published no working phase")?;
    let hand_off: Vec<_> = phases[working..continued]
        .iter()
        .map(|(phase, _)| phase.as_str())
        .collect();
    assert!(
        hand_off.iter().all(|phase| *phase != "idle"),
        "the goal published an idle phase between its turns: {hand_off:?}"
    );
    assert!(
        hand_off.contains(&"handing_off"),
        "the goal's hand-off between turns was not marked as one: {hand_off:?}"
    );
    control
        .dispatch(&session_id, MockResponseCommand::Cancel { id: second })
        .await?;
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await??;
    Ok(())
}

/// A queued turn that fails before it starts leaves the status idle, not `Working · 0s`.
///
/// While the follow-up was claimed, the status predicted its turn as already working. The turn
/// failed before dispatch and its input settled without any exchange event, so nothing projected
/// the status again and the `Working · 0s` hand-off stayed until the next turn.
#[tokio::test]
async fn queued_turn_that_fails_before_starting_leaves_the_status_idle() -> Result<()> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{TestInputAcceptance, TestPromptScript, TestPromptTerminal};
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let control = controller
        .provider_handle()
        .mock_response_control()
        .unwrap();
    let first = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let published = Arc::new(std::sync::Mutex::new(Vec::<String>::new()));
    let recorded = published.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] == "document_changed"
                && let Some(kind) = event["payload"]["status"]["kind"].as_str()
            {
                recorded.lock().unwrap().push(kind.to_owned());
            }
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let submit = |id: &str, intent: InputIntent| InputOperation {
        id: format!("operation-{id}"),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: id.into(),
                text: format!("input {id}"),
                purpose: crate::input::InputPurpose::User,
                intent,
                attachments: vec![],
                config: None,
            })),
        },
    };
    controller
        .coordinator
        .submissions
        .apply(submit("active", InputIntent::Auto))?;
    control
        .dispatch(&session_id, MockResponseCommand::Wait { id: first.clone() })
        .await?;
    // The mock assigns a script to the next prompt it starts, so the follow-up's failure is
    // scripted only once the held first turn has started.
    control
        .dispatch(
            &session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![],
                    terminal: TestPromptTerminal::RejectedBeforeDispatch(
                        forge_provider_api::ProviderFailureKind::Runtime,
                    ),
                },
            },
        )
        .await?;
    controller
        .coordinator
        .submissions
        .apply(submit("follow-up", InputIntent::Queue))?;
    control
        .dispatch(&session_id, MockResponseCommand::Release { id: first })
        .await?;
    // The rejected follow-up settles as a failed entry once no turn holds the session.
    tokio::time::timeout(Duration::from_secs(4), async {
        loop {
            let settled = controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .iter()
                .any(|message| {
                    message.id == "follow-up"
                        && message.delivery() == crate::input::InputDelivery::Failed
                });
            let finished = SqliteStore::read_only(&controller.data_root)?
                .list_exchange(&session_id)?
                .iter()
                .all(crate::exchange::Exchange::is_finished);
            if settled && finished && controller.coordinator.execution.available_permits() == 1 {
                break Ok::<(), anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("the failed follow-up never settled")??;
    // The owner publishes the refreshed status after it settles the input, so the published
    // phases must leave `working` without any further turn.
    let left_working = tokio::time::timeout(Duration::from_secs(4), async {
        while published
            .lock()
            .unwrap()
            .last()
            .is_none_or(|phase| phase == "working")
        {
            tokio::task::yield_now().await;
        }
    })
    .await;
    let phases = published.lock().unwrap().clone();
    assert!(
        left_working.is_ok(),
        "the status kept the failed turn's hand-off: {phases:?}"
    );
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await??;
    Ok(())
}

/// A question a running tool waits on stops the exchange clock, as an approval does.
///
/// The model's turn stays open while its question tool waits for the user's answer, and that wait
/// counted as model work: the heading and status read `Working · 213s` under a popup that said
/// work was paused, and the tool's own duration included the wait. A pending question with a
/// waiting tool is a user-wait blocker that pauses the clock and reads as waiting for an answer.
#[tokio::test]
async fn waiting_question_pauses_the_exchange_clock_until_answered() -> Result<()> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let control = controller
        .provider_handle()
        .mock_response_control()
        .unwrap();
    control
        .dispatch(
            &session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![
                        TestPromptStep::ToolCall {
                            name: "question_ask".into(),
                            arguments: json!({"wait":true,"questions":[
                                {"header":"Scope","question":"Which scope?","options":[]}]}),
                        },
                        TestPromptStep::Message {
                            content: TestMessageContent::Response,
                            text: "Scoped".into(),
                        },
                    ],
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    controller.coordinator.submissions.apply(InputOperation {
        id: "operation-ask".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: "ask".into(),
                text: "Ask me the scope".into(),
                purpose: crate::input::InputPurpose::User,
                intent: InputIntent::Auto,
                attachments: vec![],
                config: None,
            })),
        },
    })?;
    let store = SqliteStore::read_only(&controller.data_root)?;
    // Wait for the exchange to record the wait; the outer bound only guards a hung run.
    let (question, waiting) = tokio::time::timeout(Duration::from_secs(4), async {
        loop {
            let question = controller
                .questions
                .snapshot()?
                .questions
                .into_iter()
                .find(crate::question::QuestionRecord::pending);
            let exchange = store.list_exchange(&session_id)?.into_iter().next();
            if let (Some(question), Some(exchange)) = (question, exchange)
                && exchange.metrics.awaiting_user()
            {
                break Ok::<_, anyhow::Error>((question, exchange));
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("the waiting question never paused its exchange")??;
    assert!(
        waiting.execution_started_at_ms().is_none(),
        "the exchange clock kept running while its tool waited for an answer"
    );
    assert_eq!(
        waiting.activity(),
        crate::exchange::ExchangeActivity::AwaitingInput
    );
    // The status reads the same wait, so the status split shows its waiting row instead of
    // falling to idle beside a heading that reads `Waiting for your answer`.
    let status = controller.observer.snapshot()?.status;
    assert!(
        matches!(
            &status,
            crate::session::state_machine::SessionPhase::AwaitingInput {
                exchange_id: Some(id),
                ..
            } if *id == waiting.id
        ),
        "a question that its tool waits on left the status at {status:?}"
    );
    controller
        .exchange_owner
        .question_events()
        .change(crate::exchange::QuestionChange::user(
            crate::question::QuestionCommand {
                operation_id: "answer-scope".into(),
                question_id: question.id,
                revision: question.content.revision,
                action: crate::question::QuestionAction::Answer {
                    answer: crate::question::QuestionAnswer::Other {
                        text: "Workspace".into(),
                    },
                },
            },
        )?)
        .await?;
    let settled = tokio::time::timeout(Duration::from_secs(4), async {
        loop {
            if let Some(exchange) = store
                .list_exchange(&session_id)?
                .into_iter()
                .find(crate::exchange::Exchange::is_finished)
            {
                break Ok::<_, anyhow::Error>(exchange);
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("the answered question never let its turn finish")??;
    assert_eq!(settled.state(), crate::exchange::ExchangeState::Complete);
    // The answered question sits where the answer was given: after the call that asked it and
    // before the response that followed, not as the exchange's last row.
    let observed = controller
        .observer
        .snapshot()?
        .exchange
        .into_iter()
        .find(|exchange| exchange.id == settled.id)
        .context("the settled exchange is missing from the snapshot")?;
    let position = |matches: &dyn Fn(&crate::exchange::ExchangeNode) -> bool| {
        observed.node_list.iter().position(matches)
    };
    let asked = position(&|node| {
        matches!(
            node,
            crate::exchange::ExchangeNode::TurnContent {
                item: crate::turn::TurnItem::Tool { .. },
                ..
            }
        )
    })
    .context("the question call has no row")?;
    let answered = position(&|node| {
        matches!(
            node,
            crate::exchange::ExchangeNode::QuestionPresented { .. }
        )
    })
    .context("the answered question has no row")?;
    let response = position(&|node| {
        matches!(
            node,
            crate::exchange::ExchangeNode::TurnContent {
                item: crate::turn::TurnItem::Message { .. },
                ..
            }
        )
    })
    .context("the response has no row")?;
    assert!(
        asked < answered && answered < response,
        "the answered question is out of order: call {asked}, question {answered}, response {response}"
    );
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await??;
    Ok(())
}

/// Re-project the predicted hand-off whenever a queue change adds or withdraws its message.
///
/// Between turns the status predicts the next queued turn as `Working`. An edit reservation
/// that lands before the owner claims that message must return the status to idle, or the
/// status row spins `Working · 0s` with nothing running for as long as the edit stays open.
#[tokio::test]
async fn reserving_the_predicted_follow_up_publishes_idle() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    // Holding the execution permit keeps the owner from claiming the follow-up, which is the
    // interval between a settled turn and the next dispatch.
    let _execution = Arc::clone(&controller.coordinator.execution)
        .try_acquire_owned()
        .unwrap();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let published = Arc::new(std::sync::Mutex::new(Vec::<String>::new()));
    let recorded = published.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] == "document_changed"
                && let Some(kind) = event["payload"]["status"]["kind"].as_str()
            {
                recorded.lock().unwrap().push(kind.to_owned());
            }
        }
        Ok::<(), anyhow::Error>(())
    });
    let published_phase = |expected: &'static str| {
        let published = published.clone();
        async move {
            while published.lock().unwrap().last().map(String::as_str) != Some(expected) {
                tokio::task::yield_now().await;
            }
        }
    };
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 1,
                method: "session.input.apply".into(),
                params: json!({"id":"submit-follow-up","action":{"kind":"submit","message":{
                    "id":"follow-up","text":"Queued follow-up","intent":"queue","attachments":[]}}}),
            },
            &sink,
        )
        .await?;
    // The queued follow-up is the next turn, so the status predicts it as already working.
    published_phase("working").await;
    let revision = controller.coordinator.submissions.snapshot()?.messages[0].revision;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.input.apply".into(),
                params: json!({"id":"reserve-follow-up","action":{"kind":"reserve",
                    "id":"follow-up","revision":revision,"reservation":"editor"}}),
            },
            &sink,
        )
        .await?;
    published_phase("idle").await;
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await??;
    Ok(())
}

#[tokio::test]
async fn host_owned_input_steers_and_preserves_edit_and_pause_barriers() -> Result<()> {
    use crate::input::{InputAction, InputEdit, InputIntent, InputMessage, InputOperation};
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
    let session_id = registry.initial_session_id().to_owned();
    let mock_provider = controller.provider_handle();
    let control = mock_provider.mock_response_control().unwrap();
    let held = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let hold_id = held["id"].as_str().unwrap().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let execution_idle = Arc::new(AtomicBool::new(false));
    let observed_idle = execution_idle.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] == "activity_changed" {
                observed_idle.store(
                    event["payload"]["activity"]["phase"] == "idle",
                    Ordering::Release,
                );
            }
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let message = |id: &str, intent: InputIntent| InputOperation {
        id: format!("operation-{id}"),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: id.into(),
                text: format!("input {id}"),
                purpose: crate::input::InputPurpose::User,
                intent,
                attachments: vec![],
                config: None,
            })),
        },
    };
    controller
        .coordinator
        .submissions
        .apply(message("first", InputIntent::Auto))?;
    control
        .dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: hold_id.clone(),
            },
        )
        .await?;
    async {
        loop {
            if controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .iter()
                .any(|message| {
                    message.id == "first"
                        && message.delivery() == crate::input::InputDelivery::Accepted
                })
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await?;
    assert_eq!(
        controller.coordinator.execution.available_permits(),
        0,
        "delivery acknowledgement must not release active execution"
    );
    controller
        .coordinator
        .submissions
        .apply(message("queued", InputIntent::Queue))?;
    controller
        .coordinator
        .submissions
        .apply(message("steered", InputIntent::Auto))?;
    async {
        loop {
            if !controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .iter()
                .any(|message| message.id == "steered")
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await?;
    controller
        .coordinator
        .submissions
        .apply(message("queued", InputIntent::Queue))?;
    controller.coordinator.submissions.apply(InputOperation {
        id: "edit".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Reserve {
            id: "queued".into(),
            revision: 1,
            reservation: "editor".into(),
        },
    })?;
    controller.coordinator.submissions.suspend(true, None)?;
    control
        .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
        .await?;
    async {
        while controller.coordinator.execution.available_permits() == 0 {
            tokio::task::yield_now().await;
        }
    }
    .await;
    let queued = controller
        .coordinator
        .submissions
        .snapshot()?
        .messages
        .into_iter()
        .find(|message| message.id == "queued")
        .unwrap();
    assert_eq!(queued.reservation(), Some("editor"));
    controller.coordinator.submissions.apply(InputOperation {
        id: "commit".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Commit {
            reservation: "editor".into(),
            edits: vec![InputEdit {
                id: "queued".into(),
                revision: queued.revision,
                text: "edited follow-up".into(),
                attachments: vec![],
                config: None,
            }],
        },
    })?;
    assert!(controller.coordinator.submissions.snapshot()?.suspended);
    controller.coordinator.submissions.suspend(false, None)?;
    async {
        loop {
            if controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty()
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await?;
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    async {
        while !execution_idle.load(Ordering::Acquire) {
            tokio::task::yield_now().await;
        }
    }
    .await;
    let exchange = SqliteStore::open(&controller.data_root)?.list_exchange(&session_id)?;
    assert_eq!(
        exchange
            .iter()
            .filter(|exchange| exchange.input_message_id.as_deref() == Some("first"))
            .count(),
        1
    );
    assert_eq!(exchange.iter().flat_map(|exchange| &exchange.node_list).filter(|node|
        matches!(node, crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.id == "steered")
    ).count(), 1, "steering must be recorded once under its original message identity");
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await.context("input delivery boundary 5")??;
    Ok(())
}

#[tokio::test]
async fn input_retry_sends_once_and_never_replays_uncertain_delivery() -> Result<()> {
    use crate::input::{InputDelivery, InputOperation};
    use crate::provider::submission::SubmissionStore;
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
    let session_id = registry.initial_session_id().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let message = |id: &str| -> Result<InputOperation> {
        Ok(serde_json::from_value(
            json!({"id":format!("submit-{id}"), "action":{
                "kind":"submit", "message":{"id":id,"text":"retry regression","intent":"auto"}
            }}),
        )?)
    };
    controller
        .coordinator
        .submissions
        .apply(message("retry-message")?)?;
    let failed = controller.coordinator.submissions.delivery(
        "retry-message",
        Some(InputDelivery::Failed),
        None,
        Some("failed before dispatch".into()),
    )?;
    let params = json!({"id":"retry-operation","action":{"kind":"retry", "id":"retry-message", "revision":failed.upsert[0].revision}});
    for id in [2, 3] {
        controller
            .input_request(
                Request {
                    id,
                    method: "session.input.apply".into(),
                    params: params.clone(),
                },
                RuntimeMethod::SessionInputApply,
                &sink,
            )
            .await?;
    }
    controller.attach_input(&sink).await?;
    async {
        while !controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
        {
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await?;
    let exchange = SqliteStore::open(&controller.data_root)?.list_exchange(&session_id)?;
    assert_eq!(
        exchange
            .iter()
            .filter(|exchange| exchange.input_message_id.as_deref() == Some("retry-message"))
            .count(),
        1
    );

    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    controller
        .coordinator
        .submissions
        .apply(message("uncertain-message")?)?;
    let journal = SubmissionStore::open(&controller.data_root, controller.trace.clone())?;
    let submission = journal.prepare_input(
        &controller.catalog_request.read().await.clone(),
        crate::provider::ProviderInput::from_user_text("uncertain"),
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        Some("uncertain-message".into()),
    )?;
    submission.sending(None)?;
    // Uncertain delivery is reachable only after the queue claimed the input on a route.
    let queued_revision = controller.coordinator.submissions.snapshot()?.messages[0].revision;
    controller.coordinator.submissions.claim(
        "uncertain-message",
        queued_revision,
        crate::input::InputRoute::Turn,
    )?;
    let uncertain = controller.coordinator.submissions.delivery(
        "uncertain-message",
        Some(InputDelivery::Unknown),
        Some(submission.id().into()),
        Some("lost acknowledgement".into()),
    )?;
    drop(control);
    // Input rejections reach the client as a failure response rather than a transport error.
    let (retry_sink, mut retry_output) = forge_protocol::outbound::channel();
    controller.input_request(Request { id:4, method:"session.input.apply".into(),
        params:json!({"id":"retry-uncertain","action":{"kind":"retry","id":"uncertain-message","revision":uncertain.upsert[0].revision}})
    }, RuntimeMethod::SessionInputApply, &retry_sink).await?;
    let failure = response_for(&mut retry_output, 4).await?;
    assert_eq!(
        failure["error"]["code"], "input_delivery_uncertain",
        "{failure}"
    );
    assert_eq!(
        controller.coordinator.submissions.snapshot()?.messages[0].delivery(),
        InputDelivery::Unknown
    );
    assert_eq!(
        SqliteStore::open(&controller.data_root)?
            .list_exchange(&session_id)?
            .len(),
        exchange.len()
    );
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await??;
    Ok(())
}
