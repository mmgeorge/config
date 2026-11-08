use crate::provider::ScopeOwner;
mod agent_admission;
mod bootstrap;
mod configuration_retry;
mod live_goal;
mod owner_cancel;
mod owner_fixture;
mod owner_storage_failure;
mod owner_transition;
mod reconnect_overlap;
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
        "selected-model",
        Some("low"),
        "Staged diff:\n+change",
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
        assert!(
            matches!(request.reasoning, forge_provider_api::generation::ReasoningSelection::Level(ref level) if level == "low")
        );
        assert!(request.instructions.contains("Conventional Commit"));
        assert_eq!(request.input, "Staged diff:\n+change");
    }
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
        "selected-model",
        Some("low"),
        "Staged diff:\n+change",
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
        "selected-model",
        Some("low"),
        "Staged diff:\n+change",
    )
    .await
    .unwrap_err();
    let diagnostic = format!("{failure:#}");
    assert!(diagnostic.contains("TurnStart"), "{diagnostic}");
    assert!(!diagnostic.contains("fixture generation failure"));
    assert!(shutdown.load(Ordering::Acquire));
    Ok(())
}

#[tokio::test]
async fn isolated_generation_never_acquires_another_editors_session_lease() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let owner = service();
    let opened = owner.open_session(1, initialize(&fixture, "mock")).await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let data_root = fixture.path().join("data");
    let store = SqliteStore::open(&data_root)?;
    let before = store.load_session(&session_id)?.unwrap();
    let registry = owner.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    let broker = controller.broker.lock().await;
    let generator = service();
    let text = generator
        .generate_text(
            TextGenerationConfig {
                auto_compaction: Default::default(),
                provider: crate::provider::ProviderLaunch {
                    kind: forge_providers::registry::nonselectable_id()
                        .expect("registry supplies a nonselectable fixture identity"),
                    command: vec![],
                },
                model: "default".into(),
                reasoning_effort: Some("low".into()),
                service_tier: Default::default(),
                data_root,
            },
            &before.workspace,
            TextGeneration::CommitMessage,
            "Staged diff:\n+change",
        )
        .await?;
    // An unscripted mock turn echoes its prompt ("Unscripted mock turns" in
    // `contributing/forge/editor-tests.md`), so the generated text proves the isolated request ran.
    assert_eq!(
        text,
        "Mock response: Staged diff:
+change"
    );
    assert!(generator.registry.lock().await.is_none());
    let after = store.load_session(&session_id)?.unwrap();
    assert_eq!(after.lease_owner, before.lease_owner);
    assert_eq!(after.model, before.model);
    assert_eq!(after.updated_at_ms, before.updated_at_ms);
    drop(broker);
    owner.shutdown(Duration::from_secs(1)).await?;
    generator.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

struct GenerationCall {
    workspace: String,
    model: String,
    reasoning: forge_provider_api::generation::ReasoningSelection,
    instructions: String,
    input: String,
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
            opened.input = match &request.input {
                forge_provider_api::PromptInput::Text(text) => text.clone(),
                forge_provider_api::PromptInput::Skill { .. } | forge_provider_api::PromptInput::QuestionReply(_) => panic!("utility input must preserve plain text"),
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
    }).with_models(vec![serde_json::from_value(json!({"id":"selected-model","reasoning":["low","high"],"context_window":[],"vision":false,"is_default":true})).expect("normalized fixture model")]);
    provider.with_session_open(move |request| {
        let requests = requests.clone();
        async move {
            assert!(matches!(
                request.history,
                forge_provider_api::SessionHistory::IsolatedText(_)
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
async fn required_chat_questions_enqueue_one_continuation_after_answer_delivery() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let controller = service.registry().await?.resolve(&session_id).await?;
    let execution = controller.coordinator.execution.acquire().await?;
    let mut broker = controller.broker.lock().await;
    let result = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Original request awaiting a choice"}),
        })
        .await;
    assert!(result.response.result().is_some());
    drop(broker);
    let original = crate::storage::SqliteStore::read_only(&controller.data_root)?
        .latest_exchange(&session_id)?
        .unwrap();
    let ids = controller.questions.ask(
        "required",
        &original.id,
        crate::question::QuestionOrigin::Chat,
        vec![crate::question::QuestionDraft {
            header: "Layout".into(),
            question: "Which layout?".into(),
            requirement: crate::question::QuestionRequirement::Required,
            options: Vec::new(),
        }],
    )?;
    let (sink, _output) = forge_protocol::outbound::channel();
    controller
        .exchange_owner
        .reconcile_questions(Some(sink.clone()))
        .await?;
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    controller
        .questions
        .apply(crate::question::QuestionCommand {
            operation_id: "answer-required".into(),
            question_id: ids[0].clone(),
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
    controller
        .exchange_owner
        .reconcile_questions(Some(sink.clone()))
        .await?;
    let queued = controller.coordinator.submissions.snapshot()?;
    assert_eq!(queued.messages.len(), 2);
    assert!(matches!(
        queued.messages[0].purpose,
        crate::input::InputPurpose::Question { .. }
    ));
    assert!(
        matches!(&queued.messages[1].purpose, crate::input::InputPurpose::QuestionContinuation { exchange_id } if exchange_id == &original.id)
    );
    assert_eq!(queued.messages[1].intent, crate::input::InputIntent::Queue);
    assert!(queued.messages[1].text.contains(&original.prompt));
    drop(execution);
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
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

#[tokio::test]
async fn attachment_import_bypasses_active_execution_and_retains_source_bytes() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let source = fixture.path().join("screen shot.png");
    let bytes = b"\x89PNG\r\n\x1a\nretained image";
    std::fs::write(&source, bytes)?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let registry = service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    let broker = controller.broker.lock().await;
    let (sink, mut output) = forge_protocol::outbound::channel();
    service
        .dispatch(
            Some(session_id),
            Request {
                id: 2,
                method: "session.attachment.import".into(),
                params: json!({"path":source, "number":3}),
            },
            &sink,
        )
        .await?;
    let frame = output
        .recv()
        .await?
        .context("attachment response channel closed")?;
    assert!(
        !frame.is_terminal(),
        "attachment reply must keep the host connection open"
    );
    let response: Value = serde_json::from_slice(frame.bytes())?;
    let selection = response["result"].clone();
    assert_eq!(selection["label"], "[Image #3]");
    std::fs::remove_file(source)?;
    let selected = crate::attachment::resolve_input(
        &fixture.path().join("data"),
        &json!({"attachments":[selection]}),
    )
    .await?;
    let input = selected
        .iter()
        .map(crate::attachment::InputAttachment::prepare)
        .collect::<std::result::Result<Vec<_>, _>>()?;
    assert_eq!(input.len(), 1);
    assert_eq!(input[0].label, "[Image #3]");
    assert_eq!(input[0].contents.as_ref(), bytes.as_slice());
    assert_eq!(std::fs::read(&input[0].path)?, bytes);
    assert!(controller.broker.try_lock().is_err());
    drop(broker);
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
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

/// ACP agents list models only from an open binding; the root catalog must use that binding.
#[tokio::test]
async fn binding_only_model_catalog_lists_from_the_open_root_binding() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let (control_sink, mut control) = forge_protocol::outbound::channel();
    let control_request = |id: u64, method: &str, params: Value| {
        service.dispatch(
            Some(session_id.clone()),
            Request {
                id,
                method: method.into(),
                params,
            },
            &control_sink,
        )
    };
    control_request(
        10,
        "session.provider.mock_response",
        json!({"operation":"require_binding_model_catalog"}),
    )
    .await?;
    assert!(response_for(&mut control, 10).await?["error"].is_null());
    let unbound = control_request(15, "session.provider.model.list", json!({})).await;
    assert!(
        unbound
            .as_ref()
            .is_err_and(|error| error.to_string().contains("ModelSelection")),
        "workspace discovery must stay unsupported before the binding opens: {unbound:?}"
    );
    control_request(
        11,
        "session.provider.mock_response",
        json!({"operation":"hold","mode":"chat"}),
    )
    .await?;
    let held = response_for(&mut control, 11).await?;
    let hold_id = held["result"]["id"].clone();
    let admission = service
        .prepare(
            Some(session_id.as_str()),
            RuntimeMethod::SessionPromptSubmit,
        )
        .await?;
    let (prompt_sink, _prompt_output) = forge_protocol::outbound::channel();
    let submit = service.dispatch_admitted(
        Some(session_id.clone()),
        Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"held prompt"}),
        },
        &prompt_sink,
        admission,
    );
    let inspect = async {
        control_request(
            12,
            "session.provider.mock_response",
            json!({"operation":"wait","id":hold_id}),
        )
        .await?;
        assert!(response_for(&mut control, 12).await?["error"].is_null());
        control_request(13, "session.provider.model.list", json!({})).await?;
        let models = response_for(&mut control, 13).await?;
        control_request(
            14,
            "session.provider.mock_response",
            json!({"operation":"release","id":hold_id}),
        )
        .await?;
        response_for(&mut control, 14).await?;
        Ok::<_, anyhow::Error>(models)
    };
    let (_, models) = async { tokio::try_join!(submit, inspect) }.await?;
    assert!(models["error"].is_null(), "{models}");
    assert_eq!(models["result"][0]["id"], "mock-model");
    // Between turns the binding remains open, so the catalog must still come from it.
    let controller = service.registry().await?.resolve(&session_id).await?;
    let idle_execution = controller.coordinator.execution.acquire().await?;
    let idle = control_request(16, "session.provider.model.list", json!({})).await;
    drop(idle_execution);
    assert!(idle.is_ok(), "idle binding catalog failed: {idle:?}");
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn child_approval_resolution_uses_the_owned_runtime() -> Result<()> {
    use crate::permissions::matcher::{PermissionRequest, PermissionTarget};
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "visible-blocking"))
        .await?;
    let session_id = opened.result().context("session did not open")?["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let registry = service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    controller.attach_agents(&sink).await?;
    let admitted = controller
        .agent_coordinator
        .sender()
        .request(crate::agent::AgentRequest {
            operation_id: "approval-child".into(),
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
                profile: Some("worker".into()),
                text: "Wait".into(),
            }),
        })
        .await?;
    let agent_id = admitted["agent"]["id"].as_str().unwrap().to_owned();
    let task_id = admitted["task"]["id"].as_str().unwrap().to_owned();
    let owner = crate::provider::ThreadOwner::new(
        (session_id.clone()).into(),
        forge_provider_api::AgentId::new(agent_id.clone())
            .expect("registered agent identity is nonempty"),
    )
    .expect("registered session and agent identities are nonempty");
    let permission = controller.permission.clone();
    let (event_sink, mut events) =
        crate::provider::events::channel(crate::provider::InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(owner.clone()),
            exchange_id: task_id.clone().into(),
            invocation_id: "approval-invocation".into(),
        });
    let pending = tokio::spawn(async move {
        permission
            .authorize(
                crate::session::ApprovalMode::Ask,
                PermissionRequest {
                    id: "child-request".into(),
                    provider: forge_provider_api::ProviderId::new("fixture")
                        .expect("fixture identity is valid"),
                    reason: None,
                    target_list: vec![PermissionTarget::Command {
                        command: "probe".into(),
                        shell: None,
                    }],
                },
                Some(crate::provider::TurnAddress {
                    scope: event_sink.invocation_scope()?.clone(),
                    thread_id: event_sink.invocation_scope()?.owner().thread_id(),
                    turn_id: event_sink.invocation_scope()?.turn_id()?,
                }),
                Some(&event_sink),
            )
            .await
    });
    let requested = events.recv().await?.unwrap();
    controller
        .agent_coordinator
        .sender()
        .send(crate::agent::AgentCommand::Event {
            owner,
            task_id: task_id.clone(),
            event: requested.into(),
        })
        .await?;
    let requested_view = async {
        loop {
            let frame = output.recv().await?.context("event stream ended")?;
            let value: Value = serde_json::from_slice(frame.bytes())?;
            if value["payload"]["kind"] == "approval_requested" {
                return Ok::<_, anyhow::Error>(value["payload"]["data"].clone());
            }
        }
    }
    .await?;
    // The wire request carries the child's owner; the editor answers by request and target IDs.
    assert_eq!(requested_view["execution_owner"]["agent_id"], agent_id);
    let approval_id = requested_view["request_id"].as_str().unwrap();
    let answers: Vec<_> = requested_view["targets"]
        .as_array()
        .unwrap()
        .iter()
        .map(|target| json!({"item_id":target["target_id"],"choice_id":"allow_once"}))
        .collect();
    assert!(
        route_control_request(
            &controller,
            &Request {
                id: 2,
                method: "session.approval.resolve".into(),
                params: json!({"approval_id":approval_id,"answer_list":answers})
            },
            RuntimeMethod::SessionApprovalResolve,
            &sink,
        )
        .await?
    );
    assert_eq!(
        pending.await??,
        crate::provider::approval::ApprovalResolution::AllowOnce
    );
    let resolved = events.recv().await?.unwrap();
    assert!(matches!(
        &resolved,
        crate::provider::session_execution::OwnerObservation::Event(
            crate::provider::HarnessEvent::Host {
                event: crate::provider::HostEvent::PermissionApproval {
                    execution_kind: crate::provider::ApprovalExecutionKind::Managed,
                    event: crate::provider::PermissionApprovalEvent::Resolved(_)
                },
                ..
            }
        )
    ));
    controller
        .agent_coordinator
        .sender()
        .send(crate::agent::AgentCommand::Event {
            owner: crate::provider::ThreadOwner::new(
                (session_id.clone()).into(),
                forge_provider_api::AgentId::new(agent_id.clone())
                    .expect("registered agent identity is nonempty"),
            )
            .expect("registered session and agent identities are nonempty"),
            task_id: task_id.clone(),
            event: resolved.into(),
        })
        .await?;
    async {
        loop {
            let frame = output.recv().await?.context("event stream ended")?;
            let value: Value = serde_json::from_slice(frame.bytes())?;
            if value["payload"]["kind"] == "approval_resolved" {
                assert_eq!(
                    value["payload"]["data"]["execution_owner"]["agent_id"],
                    agent_id
                );
                return Ok::<_, anyhow::Error>(());
            }
        }
    }
    .await?;
    assert!(controller.permission.pending_list()?.is_empty());
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn finalized_execution_releases_admission_before_response_delivery() -> Result<()> {
    let semaphore = SessionCoordinator::new(Arc::new(crate::input::SessionInputQueue::default()))
        .execution
        .clone();
    let mut admission = Some(Arc::clone(&semaphore).acquire_owned().await?);
    let (sink, mut output) = forge_protocol::outbound::channel();
    for sequence in 0..forge_protocol::MAX_PENDING_FRAMES - forge_protocol::RESERVED_CONTROL_RECORDS
    {
        sink.send(sequence)?;
    }
    let mut completion = Box::pin(publish_execution_completion(
        &sink,
        Response::success(2, json!({"completed":true}))?,
        &mut admission,
        false,
    ));
    assert!(
        futures_util::poll!(completion.as_mut()).is_pending(),
        "fixture must block response delivery"
    );
    let next = Arc::clone(&semaphore)
        .try_acquire_owned()
        .expect("finished execution still owns admission while its completion waits for delivery");
    drop(output.recv().await?.context("expected queued frame")?);
    completion.await?;
    drop(next);
    assert!(admission.is_none());
    Ok(())
}

/// The scheduler only wakes the exchange owner at a deadline; the owner retries behind a busy
/// broker and revalidates the schedule, so no deadline reserves execution admission and a
/// superseded or cancelled wait dispatches nothing once the broker is released.
#[tokio::test(start_paused = true)]
async fn input_timeout_waits_without_admission_and_releases_stale_waits() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    controller.attach_timeout(&sink).await?;
    let broker = controller.broker.lock().await;
    tokio::task::yield_now().await;
    tokio::time::advance(Duration::from_secs(60)).await;
    tokio::task::yield_now().await;
    assert_eq!(controller.coordinator.execution.available_permits(), 1);

    let schedule = crate::session::InputSchedule {
        revision: 1,
        identity: Some("exchange:question".into()),
        question_revision: Some(1),
        deadline: crate::session::InputTimeout {
            amount: 1,
            unit: crate::session::TimeoutUnit::S,
        }
        .deadline(0)?,
        approval_allowed: false,
    };
    controller
        .observer
        .input_schedule
        .send_replace(schedule.clone());
    tokio::task::yield_now().await;
    tokio::time::advance(Duration::from_millis(999)).await;
    tokio::task::yield_now().await;
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    tokio::time::advance(Duration::from_millis(1)).await;
    tokio::task::yield_now().await;
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    controller
        .observer
        .input_schedule
        .send_replace(crate::session::InputSchedule {
            revision: 2,
            ..Default::default()
        });
    tokio::task::yield_now().await;
    assert_eq!(controller.coordinator.execution.available_permits(), 1);

    let execution = controller.coordinator.execution.acquire().await?;
    controller
        .observer
        .input_schedule
        .send_replace(crate::session::InputSchedule {
            revision: 3,
            ..schedule
        });
    tokio::task::yield_now().await;
    controller.cancellation.request();
    tokio::task::yield_now().await;
    drop(execution);
    tokio::task::yield_now().await;
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    drop(broker);
    for _ in 0..8 {
        tokio::task::yield_now().await;
    }
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    assert!(controller.observer.snapshot()?.exchange.is_empty());
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

/// Collect an invocation-owned check through the same cancellation command as other work.
#[tokio::test]
async fn exchange_cancellation_collects_invocation_check() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    std::fs::write(
        fixture.path().join(".forge.json"),
        serde_json::to_vec(&json!({
            "checks":{"waiting":{"trigger":{"auto":false},"action":{
                "command":["custom-runner","wait"],
                "label":"Waiting check","category":"test"
            }}}
        }))?,
    )?;
    let service = service();
    assert!(
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?
            .error()
            .is_none()
    );
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    controller.checks.script_commands(|request| {
        assert_eq!(
            request.invocation.map(|(program, _)| program),
            Some("custom-runner")
        );
        crate::checks::ScriptedCheckOutcome {
            hold_until_cancelled: true,
            ..Default::default()
        }
    });
    let (sink, mut output) = forge_protocol::outbound::channel();
    let drain = tokio::spawn(async move { while output.recv().await.ok().flatten().is_some() {} });
    for (id, method, params) in [
        (2, "session.configure", json!({"approval_mode":"allow"})),
        (
            3,
            "session.provider.mock_response",
            json!({"operation":"script","script":{
                "steps":[{"kind":"tool_call","name":"check_run","arguments":{"check":"waiting"}}],
                "terminal":"completed"
            }}),
        ),
        (
            4,
            "session.prompt.submit",
            json!({"text":"Run the waiting check through its tool."}),
        ),
    ] {
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id,
                    method: method.into(),
                    params,
                },
                &sink,
            )
            .await?;
    }
    let run_id = async {
        loop {
            if let Some(id) = controller.checks.active()? {
                let run = controller.checks.status(&id, 100, false).await?;
                if run
                    .commands
                    .iter()
                    .any(|command| command.state == crate::checks::CheckState::Running)
                {
                    break Ok::<_, anyhow::Error>(id);
                }
            }
            tokio::task::yield_now().await;
        }
    }
    .await?;
    assert!(controller.observer.snapshot()?.checks_running);
    service
        .dispatch(
            Some(session_id),
            Request {
                id: 5,
                method: "session.turn.cancel".into(),
                params: json!({"operation_id":"collect-invocation-check"}),
            },
            &sink,
        )
        .await?;
    assert!(controller.checks.active()?.is_none());
    assert_eq!(
        controller.checks.status(&run_id, 0, false).await?.state,
        crate::checks::CheckState::Interrupted
    );
    assert!(!controller.observer.snapshot()?.checks_running);
    service.shutdown(Duration::from_secs(1)).await?;
    drop(sink);
    drain.abort();
    Ok(())
}

#[tokio::test]
async fn failed_initialization_can_retry_and_concurrent_open_has_one_owner() {
    let fixture = tempfile::tempdir().unwrap();
    let blocked = fixture.path().join("blocked");
    std::fs::write(&blocked, "file").unwrap();
    let service = service();
    let mut initialize = initialize(&fixture, "mock");
    initialize.data_root = blocked.to_string_lossy().into_owned();
    assert!(
        service
            .open_session(1, initialize.clone())
            .await
            .unwrap()
            .error()
            .is_some()
    );
    assert!(service.registry.lock().await.is_none());

    initialize.data_root = fixture.path().join("data").to_string_lossy().into_owned();
    let (first, second) = tokio::join!(
        service.open_session(2, initialize.clone()),
        service.open_session(3, initialize.clone()),
    );
    let first = first.unwrap();
    let second = second.unwrap();
    assert_ne!(first.result().is_some(), second.result().is_some());
    service.shutdown(Duration::from_secs(1)).await.unwrap();
    assert!(service.registry.lock().await.is_none());
    assert!(service.open_session(4, initialize).await.is_err());
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

#[tokio::test]
async fn user_input_cannot_impersonate_task_continuation() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let result = controller.input_request(Request {
        id: 2,
        method: "session.input.apply".into(),
        params: json!({
            "id":"forged-continuation", "action":{"kind":"submit", "message":{
                "id":"forged-message", "text":"Continue automatically",
                "purpose":{"kind":"task_continuation", "revision":"untrusted", "completed_turn_id":"turn", "reason":"forged"}
            }}
        }),
    }, RuntimeMethod::SessionInputApply, &sink).await;
    assert!(
        result.is_err(),
        "user-supplied continuation authority was accepted"
    );
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
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

#[tokio::test]
async fn idle_reopen_applies_retained_question_effect() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
    controller.trace.configure_default(true)?;
    let child_started = Arc::new(Notify::new());
    let child_release = Arc::new(tokio::sync::Semaphore::new(0));
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().context("missing mock fixture")?,
    )?;
    let behavior = {
        let child_started = child_started.clone();
        let child_release = child_release.clone();
        move |request: forge_provider_api::PromptRequest,
              _: forge_provider_api::AdapterActivityWriter,
              start: forge_provider_api::NativeTurnStart,
              _: tokio::sync::watch::Receiver<bool>| {
            let child_started = child_started.clone();
            let child_release = child_release.clone();
            async move {
                start.observed(1).await?;
                if request.scope.owner().is_root() {
                    crate::provider::test_support::call_tool(&request, "question_ask", json!({
                        "questions":[{"header":"Choice", "question":"Which active route?", "options":[], "allow_freeform":true}]
                    })).await?;
                } else {
                    child_started.notify_one();
                    crate::provider::test_support::wait_gate(&child_release).await?;
                }
                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                ))
            }
        }
    };
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        behavior.clone(),
    );
    let child_native =
        crate::provider::test_support::EventFixtureProvider::new(descriptor, behavior);
    let provider_fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let child_provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(child_native)?;
    {
        let mut broker = controller.broker.lock().await;
        broker.set_provider_for_test(provider_fixture.provider_handle());
        broker
            .agent_provider_factory
            .register(child_provider_fixture.provider_handle());
    }
    *controller.provider.write().unwrap() = provider_fixture.provider_handle();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let observed = Arc::new(std::sync::Mutex::new(Vec::new()));
    let observed_output = observed.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let message: Value = serde_json::from_slice(frame.bytes())?;
            observed_output.lock().unwrap().push(message);
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_session_events(&sink).await?;
    service
        .dispatch(
            Some(registry.initial_session_id().to_owned()),
            Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({"text":"hello"}),
            },
            &sink,
        )
        .await?;
    async {
        loop {
            let ready = {
                let broker = controller.broker.lock().await;
                let snapshot = broker.snapshot()?;
                snapshot.exchange.last().is_some_and(|exchange| {
                    exchange
                        .turn
                        .last()
                        .is_some_and(|turn| turn.completed().is_some())
                }) && !snapshot.questions.questions.is_empty()
                    && controller.coordinator.execution.available_permits() > 0
            };
            if ready {
                break Ok::<(), anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    }
    .await?;
    let (scope, engine) = {
        let mut broker = controller.broker.lock().await;
        let snapshot = broker.snapshot()?;
        let exchange = snapshot
            .exchange
            .last()
            .context("missing committed root exchange")?;
        let scope = exchange
            .turn
            .last()
            .context("missing committed root turn")?
            .provider()
            .scope
            .clone();
        let engine = broker
            .execution_registry()
            .owner(scope.owner().clone())
            .await?;
        (scope, engine)
    };
    let late_effect = engine
        .publish_idle_test_effect(
            scope.clone(),
            crate::provider::ProviderCompletion {
                plan_question: Some(crate::plan::PlanQuestionSet {
                    id: "idle-question".into(),
                    questions: vec![crate::plan::PlanQuestion {
                        id: "idle-choice".into(),
                        header: "Choice".into(),
                        question: "Which route?".into(),
                        options: Vec::new(),
                        allow_freeform: true,
                    }],
                }),
                ..Default::default()
            },
        )
        .await;
    assert!(
        late_effect.is_err(),
        "a retired root cannot acquire a new host effect"
    );
    assert!(
        engine
            .steer(
                scope.clone(),
                crate::provider::SteeringInput::AgentResult("continue".into())
            )
            .await
            .is_err(),
        "late child results cannot reopen the root turn"
    );
    {
        let broker = controller.broker.lock().await;
        let snapshot = broker.snapshot()?;
        let exchange = snapshot
            .exchange
            .iter()
            .find(|exchange| exchange.id == scope.exchange_id.as_str())
            .context("original root exchange disappeared")?;
        assert_eq!(exchange.turn.len(), 1);
        assert_eq!(snapshot.questions.questions.len(), 1);
        assert_eq!(
            snapshot.questions.questions[0].exchange_id,
            scope.exchange_id.as_str()
        );
        assert_eq!(
            snapshot.questions.questions[0].content.question,
            "Which active route?"
        );
        let published = observed
            .lock()
            .unwrap()
            .iter()
            .filter(|message| {
                message["event"] == "question"
                    && message["payload"]["exchange_id"] == scope.exchange_id.as_str()
                    && message
                        .pointer("/payload/elicitation/question_set/id")
                        .and_then(Value::as_str)
                        == Some("idle-question")
            })
            .count();
        assert_eq!(
            published, 0,
            "a rejected late root effect cannot publish a question"
        );
    }
    controller.attach_agents(&sink).await?;
    let admitted = controller
        .agent_coordinator
        .sender()
        .request(crate::agent::AgentRequest {
            operation_id: "independent-idle-child".into(),
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
                profile: Some("worker".into()),
                text: "Keep the independent child lifecycle".into(),
            }),
        })
        .await?;
    let child_task = admitted["task"]["id"]
        .as_str()
        .context("missing child task")?
        .to_owned();
    let child_agent = admitted["agent"]["id"]
        .as_str()
        .context("missing child agent")?
        .to_owned();
    child_started.notified().await;
    let store = SqliteStore::read_only(&controller.data_root)?;
    async {
        loop {
            if let Some(child) = store.load_exchange(&child_task)?
                && let Some(turn) = child.turn.first()
            {
                assert_eq!(
                    turn.provider().scope.owner().agent_id().as_str(),
                    child_agent
                );
                assert_ne!(turn.provider().scope, scope);
                assert!(turn.completed().is_none());
                break Ok::<(), anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    }
    .await?;
    child_release.add_permits(1);
    async {
        loop {
            if controller
                .agent_coordinator
                .task(&child_task)
                .is_some_and(|task| task.completed_at_ms.is_some())
            {
                let child = store
                    .load_exchange(&child_task)?
                    .context("child exchange missing")?;
                assert!(
                    child
                        .turn
                        .first()
                        .context("child turn missing")?
                        .completed()
                        .is_some()
                );
                break Ok::<(), anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    }
    .await?;
    let snapshot = controller.broker.lock().await.snapshot()?;
    let original = snapshot
        .exchange
        .iter()
        .find(|exchange| exchange.id == scope.exchange_id.as_str())
        .context("root exchange missing")?;
    assert_eq!(
        original.turn.len(),
        1,
        "independent child completion reopened the retired root"
    );
    assert_eq!(
        snapshot.questions.questions.len(),
        1,
        "independent child cannot invent root question authority"
    );
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
        crate::provider::ProviderInput::from_text("uncertain"),
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
    assert!(
        failure["error"]["message"]
            .as_str()
            .is_some_and(|message| message.contains("unconfirmed")),
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

#[tokio::test]
async fn reopening_active_and_deleting_idle_cached_sessions() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let first_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let registry = service.registry().await?;
    let first = registry.resolve(&first_id).await?;
    let second = prepare_new_session(
        std::path::Path::new(&registry.initialize.data_root),
        &registry.initialize.client_id,
        "mock",
        false,
        &first_id,
        &json!({"name":"second"}),
        1,
    )?;
    registry.resolve(&second.id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    service.start_provider_session(&first_id, &sink).await?;
    while !first.provider_ready().await {
        tokio::task::yield_now().await;
    }
    let execution = first.coordinator.execution.clone().acquire_owned().await?;
    let broker = first.broker.lock().await;
    service
        .dispatch(
            Some(second.id.clone()),
            Request {
                id: 2,
                method: "session.resume".into(),
                params: json!({"session_id":first_id}),
            },
            &sink,
        )
        .await?;
    let frame = output.recv().await?.context("resume response is missing")?;
    let reply: Value = serde_json::from_slice(frame.bytes())?;
    assert_eq!(reply["result"]["session"]["id"], first_id);
    assert!(
        service
            .dispatch(
                Some(second.id.clone()),
                Request {
                    id: 3,
                    method: "session.delete".into(),
                    params: json!({"session_id":first_id})
                },
                &sink
            )
            .await
            .is_err()
    );
    drop(broker);
    drop(execution);
    service
        .dispatch(
            Some(second.id.clone()),
            Request {
                id: 4,
                method: "session.delete".into(),
                params: json!({"session_id":first_id}),
            },
            &sink,
        )
        .await?;
    assert!(
        !registry
            .controller_by_id
            .read()
            .await
            .contains_key(&first_id)
    );
    assert!(
        SqliteStore::open(std::path::Path::new(&registry.initialize.data_root))?
            .load_session(&first_id)?
            .is_none()
    );
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

/// Forking a root whose children still execute is rejected before a child session exists.
#[tokio::test]
async fn fork_rejects_running_children_without_creating_a_child_session() {
    let fixture = tempfile::tempdir().unwrap();
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await
        .unwrap();
    assert!(opened.error().is_none(), "{opened:?}");
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let registry = service.registry().await.unwrap();
    let controller = registry
        .controller_by_id
        .read()
        .await
        .get(&session_id)
        .cloned()
        .unwrap();
    let spawn = crate::agent::SpawnAgentRequest {
        profile: Some("worker".into()),
        text: "bounded task".into(),
    };
    let request = crate::agent::AgentRequest {
        operation_id: "running-child".into(),
        source: crate::agent::AgentSource::User,
        operation: crate::agent::AgentOperation::Spawn(spawn.clone()),
    };
    let selection = crate::agent::AgentSelection {
        profile: "worker".into(),
        instructions: String::new(),
        provider: forge_providers::registry::nonselectable_id().unwrap(),
        model: "mock-model".into(),
        reasoning_effort: "low".into(),
        context_window: None,
        service_tier: Default::default(),
        access: Default::default(),
        approval_mode: crate::session::ApprovalMode::Ask,
    };
    let crate::agent::AgentCommit::Admitted(running) = controller
        .agent_coordinator
        .reserve_spawn(&request, &spawn, selection, "parent", 1)
        .unwrap()
        .commit()
        .unwrap()
    else {
        panic!("new request reused an admission");
    };
    assert_eq!(controller.agent_coordinator.active_count(), 1);
    let sessions_before = registry.controller_by_id.read().await.len();
    let (sink, _output) = forge_protocol::outbound::channel();
    let error = service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.fork".into(),
                params: json!({"name":"fork while running"}),
            },
            &sink,
        )
        .await
        .unwrap_err();
    assert!(
        format!("{error:#}").contains("cannot fork while child agents are running"),
        "{error:#}"
    );
    assert_eq!(
        registry.controller_by_id.read().await.len(),
        sessions_before
    );
    controller
        .agent_coordinator
        .settled(
            &running.agent.id,
            &running.task.id,
            crate::agent::AgentSettlementDisposition::Ready,
        )
        .unwrap();
    service.shutdown(Duration::from_secs(1)).await.unwrap();
}
