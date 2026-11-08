use crate::provider::ScopeOwner;
#[path = "tests/agent_admission.rs"]
mod agent_admission;
#[path = "tests/bootstrap.rs"]
mod bootstrap;
#[path = "tests/configuration_retry.rs"]
mod configuration_retry;
#[path = "tests/live_goal.rs"]
mod live_goal;
#[path = "tests/owner_cancel.rs"]
mod owner_cancel;
#[path = "tests/owner_fixture.rs"]
mod owner_fixture;
#[path = "tests/owner_storage_failure.rs"]
mod owner_storage_failure;
#[path = "tests/owner_transition.rs"]
mod owner_transition;
#[path = "tests/reconnect_overlap.rs"]
mod reconnect_overlap;
#[path = "tests/reviewed_plan.rs"]
mod reviewed_plan;
#[path = "tests/task_presentation.rs"]
mod task_presentation;
#[path = "tests/task_state.rs"]
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
async fn shared_requests_work_before_any_session_exists() -> Result<()> {
    async fn shared_response(
        service: &HarnessService,
        method: &str,
        params: Value,
    ) -> Result<Value> {
        let (sink, mut output) = forge_protocol::outbound::channel();
        service
            .dispatch(
                None,
                Request {
                    id: 42,
                    method: method.into(),
                    params,
                },
                &sink,
            )
            .await?;
        loop {
            let frame = output.recv().await?.context("shared response missing")?;
            let value: Value = serde_json::from_slice(frame.bytes())?;
            if value["id"] == 42 {
                ensure!(
                    value.get("error").is_none(),
                    "shared request failed: {value}"
                );
                return Ok(value["result"].clone());
            }
        }
    }
    let fixture = tempfile::tempdir()?;
    let service = service();
    let response = service
        .initialize_services(1, initialize(&fixture, "mock"))
        .await?;
    assert!(response.error().is_none());
    assert_eq!(
        shared_response(&service, "session.list", json!({})).await?,
        json!([])
    );
    let settings = shared_response(&service, "settings.get", json!({})).await?;
    let saved = shared_response(&service, "settings.update", json!({
        "expected_revision":settings["revision"],"scope":"global","operation_id":"shared-setting",
        "desired":{"plan_compact":true}
    })).await?;
    assert_eq!(saved["state"], "saved");
    assert!(
        shared_response(
            &service,
            "settings.update",
            json!({
                "expected_revision":saved["revision"],"scope":"session","desired":{}
            })
        )
        .await
        .is_err()
    );
    let profiles = shared_response(&service, "agent.profile.get", json!({})).await?;
    assert!(profiles["document"].is_object());
    let permissions = shared_response(&service, "permissions.document.open", json!({})).await?;
    assert!(permissions["source"].is_string());
    let tracing =
        shared_response(&service, "host.trace.configure", json!({"enabled":true})).await?;
    assert_eq!(tracing["enabled"], true);
    assert!(
        tracing["path"]
            .as_str()
            .unwrap()
            .ends_with("harness-trace.jsonl")
    );
    assert_eq!(
        shared_response(
            &service,
            "provider.model.pin.list",
            json!({"provider":"mock"})
        )
        .await?,
        json!([])
    );
    assert!(
        service
            .registry()
            .await?
            .controller_by_id
            .read()
            .await
            .is_empty()
    );
    let created =
        shared_response(&service, "session.new", json!({"name":"Explicit creation"})).await?;
    let id = created["session"]["id"]
        .as_str()
        .context("new session ID missing")?;
    let inherited = shared_response(
        &service,
        "session.new",
        json!({"source_session_id":id,"name":"Explicit inheritance"}),
    )
    .await?;
    assert_ne!(inherited["session"]["id"], created["session"]["id"]);
    assert_eq!(
        shared_response(&service, "session.list", json!({}))
            .await?
            .as_array()
            .unwrap()
            .len(),
        2
    );
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn shared_runtime_initialization_does_not_create_or_select_a_session() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let request = initialize(&fixture, "mock");
    let response = service.initialize_services(1, request.clone()).await?;
    assert!(response.error().is_none(), "{:?}", response.error());
    assert!(response.result().unwrap().get("session").is_none());
    let registry = service.registry().await?;
    assert!(registry.initial_session_id.is_none());
    assert!(registry.controller_by_id.read().await.is_empty());
    let database = rusqlite::Connection::open(crate::storage::state::database(
        &fixture.path().join("data"),
    ))?;
    let count: i64 =
        database.query_row("SELECT count(*) FROM session_record", [], |row| row.get(0))?;
    assert_eq!(count, 0);
    drop(registry);
    let opened = service.open_session(2, request.clone()).await?;
    assert!(opened.error().is_none(), "{:?}", opened.error());
    let id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let response = service.initialize_services(3, request).await?;
    assert!(response.error().is_none());
    assert_eq!(
        service.registry().await?.initial_session_id.as_deref(),
        Some(id.as_str())
    );
    let count: i64 =
        database.query_row("SELECT count(*) FROM session_record", [], |row| row.get(0))?;
    assert_eq!(count, 1);
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

/// Classify reads of a deleted session as terminal so clients stop polling it.
#[tokio::test]
async fn document_reads_for_a_missing_session_report_session_not_found() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let response = service
        .initialize_services(1, initialize(&fixture, "mock"))
        .await?;
    assert!(response.error().is_none(), "{:?}", response.error());
    let error = service
        .document(
            Some("deleted-session".into()),
            serde_json::from_value(
                json!({"operation":"sync","document":"transcript","revision":0}),
            )?,
        )
        .await
        .expect_err("a missing session resolved a presentation controller");
    assert_eq!(
        crate::broker::request_failure_code(&error),
        "session_not_found",
        "{error:#}"
    );
    assert!(
        service
            .registry()
            .await?
            .controller_by_id
            .read()
            .await
            .is_empty(),
        "a missing session registered a controller"
    );
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

struct TerminalInventoryProvider {
    inner: Arc<dyn forge_provider_api::provider::Provider>,
    started: Notify,
    release: Notify,
    calls: AtomicU64,
}

#[async_trait::async_trait]
impl forge_provider_api::provider::Provider for TerminalInventoryProvider {
    async fn model_list(
        &self,
        context: forge_provider_api::OperationContext,
        query: forge_provider_api::catalog::QueryContext,
    ) -> std::result::Result<Vec<crate::provider::ProviderModel>, crate::provider::ProviderError>
    {
        self.inner.model_list(context, query).await
    }

    async fn thread_open(
        &self,
        context: forge_provider_api::OperationContext,
        request: forge_provider_api::SessionRequest,
    ) -> std::result::Result<forge_provider_api::OpenedSession, crate::provider::ProviderError>
    {
        self.inner.thread_open(context, request).await
    }
    async fn thread_configure(
        &self,
        context: forge_provider_api::OperationContext,
        binding: &forge_provider_api::BindingId,
        configuration: forge_provider_api::SessionConfiguration,
    ) -> std::result::Result<forge_provider_api::ConfiguredSession, crate::provider::ProviderError>
    {
        self.inner
            .thread_configure(context, binding, configuration)
            .await
    }
    async fn thread_close(
        &self,
        context: forge_provider_api::OperationContext,
        binding: &forge_provider_api::BindingId,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        self.inner.thread_close(context, binding).await
    }
    async fn turn_interrupt(
        &self,
        context: forge_provider_api::OperationContext,
        binding: &forge_provider_api::BindingId,
        request: forge_provider_api::CancelRequest,
    ) -> std::result::Result<forge_provider_api::TurnInterruptResult, crate::provider::ProviderError>
    {
        self.inner.turn_interrupt(context, binding, request).await
    }
    async fn turn_start(
        &self,
        context: forge_provider_api::OperationContext,
        binding: &forge_provider_api::BindingId,
        request: forge_provider_api::PromptRequest,
    ) -> std::result::Result<forge_provider_api::InputAcceptance, crate::provider::ProviderError>
    {
        self.inner.turn_start(context, binding, request).await
    }
    async fn inspect(
        &self,
        operation: forge_provider_api::OperationContext,
        context: forge_provider_api::catalog::WorkspaceContext,
    ) -> std::result::Result<forge_provider_api::ProviderSetup, crate::provider::ProviderError>
    {
        self.inner.inspect(operation, context).await
    }
    async fn shutdown(
        &self,
        context: forge_provider_api::OperationContext,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        self.inner.shutdown(context).await
    }
    async fn terminal_list(
        &self,
        context: forge_provider_api::OperationContext,
        _: &forge_provider_api::BindingId,
    ) -> std::result::Result<
        crate::provider::terminal::TerminalInventory,
        crate::provider::ProviderError,
    > {
        context.check_admission(forge_provider_api::ProviderOperation::TerminalList)?;
        self.calls.fetch_add(1, Ordering::AcqRel);
        self.started.notify_one();
        tokio::select! {
            _ = self.release.notified() => {},
            interruption = context.interrupted() => return Err(interruption.failure(forge_provider_api::ProviderOperation::TerminalList, forge_provider_api::ProviderPhase::Observation, forge_provider_api::FailureDisposition::Unchanged)),
        }
        Err(forge_provider_api::ProviderError::Failure(
            forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TerminalList,
                forge_provider_api::ProviderFailureKind::Transport,
            ),
        ))
    }
}

#[tokio::test]
async fn terminal_inventory_discards_configuration_races_but_reports_unrelated_errors() -> Result<()>
{
    let fixture = tempfile::tempdir()?;
    let mut broker = HarnessBroker::initialize(initialize(&fixture, "mock"))?;
    let inner = forge_providers::registry::create(
        crate::provider::ProviderLaunch {
            kind: forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
            command: Vec::new(),
        },
        broker.data_root.clone(),
    )?;
    let mut descriptor = inner.descriptor().clone();
    descriptor.capability.background_terminals = true;
    let provider = Arc::new(TerminalInventoryProvider {
        inner: inner.adapter().clone(),
        started: Notify::new(),
        release: Notify::new(),
        calls: AtomicU64::new(0),
    });
    let provider_fixture = crate::provider::test_support::ProviderTestSession::from_registered(
        forge_providers::registry::RegisteredProvider::new(descriptor, provider.clone(), None),
    )?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    assert!(controller.background_terminals().await?.starting);
    assert_eq!(provider.calls.load(Ordering::Acquire), 0);
    let (sink, _output) = forge_protocol::outbound::channel();
    controller.start_provider(&sink).await;
    while !controller.provider_ready().await {
        tokio::task::yield_now().await;
    }
    // Startup leaves the native conversation unopened, which has no shells to query.
    let unopened = controller.background_terminals().await?;
    assert!(unopened.supported && !unopened.starting && unopened.terminal.is_empty());
    assert_eq!(provider.calls.load(Ordering::Acquire), 0);
    // Open the binding the way first input does so the race below observes a live inventory.
    let open = controller.broker.lock().await.provider_request(
        crate::provider::ProviderInput::from_text(""),
        crate::provider::PromptMode::Chat,
    )?;
    provider_fixture.provider_handle().thread_open(open).await?;
    let replacement = controller.provider_lane.clone().write_owned().await;
    assert!(controller.background_terminals().await?.starting);
    assert_eq!(provider.calls.load(Ordering::Acquire), 0);
    drop(replacement);
    let query = controller.background_terminals();
    tokio::pin!(query);
    tokio::select! {
        result = &mut query => panic!("terminal query settled before release: {result:?}"),
        _ = provider.started.notified() => {}
    }
    controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
    provider.release.notify_one();
    assert!(query.await?.starting);
    provider.release.notify_one();
    let error = controller
        .background_terminals()
        .await
        .unwrap_err()
        .to_string();
    assert!(error.to_lowercase().contains("transport"));
    assert!(!error.contains("connection closed"));
    // Follow production shutdown: binding collection may wake the observation relay after the
    // owner closes, which explicit stopping classifies as a completed owner lifetime.
    controller.input_stopping.store(true, Ordering::Release);
    controller.finish_exchange_owner().await?;
    provider_fixture.provider_handle().shutdown().await?;
    controller.finish_session_events().await?;
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
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_timeout_edits_preserve_latest_value_while_busy() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = Arc::new(service());
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let registry = service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    let execution = controller.coordinator.execution.acquire().await?;
    let broker = controller.broker.lock().await;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let ready = Arc::new(tokio::sync::Barrier::new(3));
    // Each edit commits settings durably, so a few interleaved edits per writer are enough to
    // exercise latest-value ordering without scaling the test with commit latency.
    const EDITS_PER_WRITER: u64 = 4;
    let mut writer_list = Vec::new();
    for (offset, field) in [(10, "approval_timeout"), (30, "approval_timeout")] {
        let service = service.clone();
        let session_id = session_id.clone();
        let sink = sink.clone();
        let ready = ready.clone();
        writer_list.push(tokio::spawn(async move {
            ready.wait().await;
            for amount in 1..=EDITS_PER_WRITER {
                service
                    .dispatch(
                        Some(session_id.clone()),
                        Request {
                            id: offset + amount,
                            method: "session.input.timeout.configure".into(),
                            params: json!({field:{"amount":amount,"unit":"m"}}),
                        },
                        &sink,
                    )
                    .await?;
            }
            Ok::<_, anyhow::Error>(())
        }));
    }
    ready.wait().await;
    for writer in writer_list {
        writer.await??;
    }
    for _ in 0..2 * EDITS_PER_WRITER {
        let frame = output
            .recv()
            .await?
            .context("timeout response is missing")?;
        let response: Value = serde_json::from_slice(frame.bytes())?;
        assert!(response.get("error").is_none(), "{response}");
    }
    let approval = controller.permission.approval_timeout(&session_id)?;
    assert_eq!(u64::from(approval.amount), EDITS_PER_WRITER);
    assert!(controller.coordinator.execution.try_acquire().is_err());
    let saved = crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?
        .get(&fixture.path().to_string_lossy(), &session_id, "mock")?
        .desired;
    assert_eq!(saved.approval_timeout, Some(approval));
    drop(broker);
    drop(execution);
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

/// Publish new questions once without turning state refreshes into an idle notification loop.
#[tokio::test]
async fn unchanged_question_relay_does_not_publish_refresh_notifications() -> Result<()> {
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
    let (sink, mut output) = forge_protocol::outbound::channel();
    controller
        .exchange_owner
        .reconcile_questions(Some(sink.clone()))
        .await?;
    let initial = output
        .recv()
        .await?
        .context("initial question notification is missing")?;
    assert_eq!(
        serde_json::from_slice::<Value>(initial.bytes())?["event"],
        "questions_changed"
    );
    controller
        .exchange_owner
        .reconcile_questions(Some(sink.clone()))
        .await?;
    assert!(
        matches!(output.try_recv(), Err(error) if error.kind() == std::io::ErrorKind::WouldBlock)
    );

    controller.questions.ask(
        "new-question",
        "exchange",
        crate::question::QuestionOrigin::Chat,
        vec![crate::question::QuestionDraft {
            header: "Layout".into(),
            question: "Which layout?".into(),
            requirement: crate::question::QuestionRequirement::Optional,
            options: Vec::new(),
        }],
    )?;
    controller
        .exchange_owner
        .reconcile_questions(Some(sink.clone()))
        .await?;
    let changed = output
        .recv()
        .await?
        .context("changed question notification is missing")?;
    assert_eq!(
        serde_json::from_slice::<Value>(changed.bytes())?["payload"]["questions"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    controller
        .exchange_owner
        .reconcile_questions(Some(sink.clone()))
        .await?;
    assert!(
        matches!(output.try_recv(), Err(error) if error.kind() == std::io::ErrorKind::WouldBlock)
    );
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
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
async fn persistent_question_answers_commit_and_relay_while_provider_owns_execution() -> Result<()>
{
    let fixture = tempfile::tempdir()?;
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
    let ids = controller.questions.ask(
        "ask",
        "exchange",
        crate::question::QuestionOrigin::Chat,
        vec![crate::question::QuestionDraft {
            header: "Layout".into(),
            question: "Which layout?".into(),
            requirement: crate::question::QuestionRequirement::Optional,
            options: Vec::new(),
        }],
    )?;
    let execution = controller.coordinator.execution.acquire().await?;
    let broker = controller.broker.lock().await;
    let (sink, _output) = forge_protocol::outbound::channel();
    let command = crate::question::QuestionCommand {
        operation_id: "answer".into(),
        question_id: ids[0].clone(),
        revision: 1,
        action: crate::question::QuestionAction::Answer {
            answer: crate::question::QuestionAnswer::Other {
                text: "Tabs".into(),
            },
        },
    };
    let mut clarification = Request {
        id: 3,
        method: "session.input.apply".into(),
        params: json!({
            "id":"clarification",
            "target":{"owner":"question","question_id":ids[0],"revision":1},
            "action":{"kind":"submit","message":{
                "id":"clarification-input","revision":0,"text":"Which scope?",
                "purpose":{"kind":"user"},"intent":"auto","attachments":[]
            }}
        }),
    };
    let control = controller.coordinator.control.acquire().await?;
    service
        .normalize_request(Some(&session_id), &mut clarification)
        .await?;
    assert_eq!(clarification.method, "session.input.apply");
    assert!(
        service
            .prepare(
                Some(&session_id),
                RuntimeMethod::decode(&clarification.method)?
            )
            .await?
            .is_none()
    );
    drop(control);
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.question.answer".into(),
                params: json!({"command": command}),
            },
            &sink,
        )
        .await?;
    assert!(!controller.questions.snapshot()?.questions[0].pending());
    controller
        .exchange_owner
        .reconcile_questions(Some(sink.clone()))
        .await?;
    controller
        .exchange_owner
        .reconcile_questions(Some(sink.clone()))
        .await?;
    assert!(controller.questions.outbox()?.is_empty());
    let queued = controller.coordinator.submissions.snapshot()?;
    assert_eq!(queued.messages.len(), 1);
    assert!(matches!(
        queued.messages[0].purpose,
        crate::input::InputPurpose::Question { .. }
    ));
    assert!(controller.questions_block_work()?);
    assert!(controller.coordinator.execution.try_acquire().is_err());
    drop(broker);
    drop(execution);
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn input_timeout_settings_do_not_interrupt_a_busy_session() -> Result<()> {
    let fixture = tempfile::tempdir()?;
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
    let execution = controller.coordinator.execution.acquire().await?;
    let broker = controller.broker.lock().await;
    let (sink, _output) = forge_protocol::outbound::channel();
    service.dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.input.timeout.configure".into(),
                params: json!({"approval_timeout":{"amount":2,"unit":"s"},"tool_timeout":{"amount":3,"unit":"m"}}),
            },
            &sink,
        ).await?;
    let approval = controller.permission.approval_timeout(&session_id)?;
    assert_eq!(approval.amount, 2);
    assert!(controller.coordinator.execution.try_acquire().is_err());
    assert_eq!(broker.snapshot()?.session.approval_timeout, approval);
    assert_eq!(broker.snapshot()?.session.tool_timeout.amount, 3);
    assert_eq!(
        broker.snapshot()?.session.tool_timeout.unit,
        crate::session::TimeoutUnit::M
    );
    drop(broker);
    drop(execution);
    let settings = crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?;
    assert_eq!(
        settings
            .get(&fixture.path().to_string_lossy(), &session_id, "mock")?
            .desired
            .approval_timeout,
        Some(approval)
    );
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn check_editing_toggles_without_prompt_or_execution_admission() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    assert_eq!(opened.result().unwrap()["check_editing"], false);
    let registry = service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    let execution = controller.coordinator.execution.acquire().await?;
    let broker = controller.broker.lock().await;
    let (sink, _output) = forge_protocol::outbound::channel();
    for (index, enabled) in [true, false, true].into_iter().enumerate() {
        assert!(
            service
                .prepare(Some(&session_id), RuntimeMethod::SessionCheckEditingSet)
                .await?
                .is_none()
        );
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: index as u64 + 2,
                    method: "session.check.editing.set".into(),
                    params: json!({"enabled": enabled}),
                },
                &sink,
            )
            .await?;
        let snapshot = controller.observer.snapshot()?;
        assert_eq!(snapshot.check_editing, enabled);
        assert!(snapshot.exchange.is_empty());
        assert!(controller.coordinator.execution.try_acquire().is_err());
        let settings = crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?;
        assert_eq!(
            settings
                .layer_settings(
                    crate::settings::SettingScope::Session,
                    &snapshot.session.workspace,
                    &session_id
                )?
                .check_editing,
            Some(enabled)
        );
        assert_eq!(
            settings
                .get(&snapshot.session.workspace, "another-session", "mock")?
                .desired
                .check_editing,
            None
        );
    }
    drop(broker);
    drop(execution);
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn status_thought_settings_apply_and_persist_without_interrupting_a_busy_session()
-> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    assert_eq!(opened.result().unwrap()["status_thoughts"], true);
    assert_eq!(opened.result().unwrap()["automatic_checks"], false);
    let registry = service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    // The session owner commits settings itself, so only provider execution models busy work.
    let execution = controller.coordinator.execution.acquire().await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    for (index, enabled) in [true, false].into_iter().enumerate() {
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: index as u64 + 2,
                    method: "session.configure".into(),
                    params: json!({"status_thoughts": enabled}),
                },
                &sink,
            )
            .await?;
        assert_eq!(controller.observer.snapshot()?.status_thoughts, enabled);
        assert!(controller.coordinator.execution.try_acquire().is_err());
        let settings = crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?;
        assert_eq!(
            settings
                .get(&fixture.path().to_string_lossy(), &session_id, "mock")?
                .desired
                .status_thoughts,
            Some(enabled)
        );
    }
    drop(execution);
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn settings_application_orders_progress_and_retains_value_free_timeline() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let observation = tokio::spawn(async move {
        let mut state = Vec::new();
        while let Some(frame) = output.recv().await? {
            let frame: Value = serde_json::from_slice(frame.bytes())?;
            if frame["event"] == "session_configured" {
                assert!(frame["payload"]["snapshot"]["session"].is_object());
                state.push("configured");
            } else if frame["event"] == "command_completed"
                && frame["payload"]["id"] == "settings-retry"
            {
                state.push("completed");
                break;
            }
        }
        Ok::<_, anyhow::Error>(state)
    });
    let request = Request {
        id: 2,
        method: "settings.update".into(),
        params: json!({"operation_id":"settings-retry","expected_revision":0,"scope":"global","desired":{"plan_compact":true}}),
    };
    service
        .dispatch(Some(session_id.clone()), request.clone(), &sink)
        .await?;
    let state = observation.await??;
    assert_eq!(state, ["configured", "completed"]);
    let (retry_sink, mut retry_output) = forge_protocol::outbound::channel();
    service
        .dispatch(Some(session_id.clone()), request.clone(), &retry_sink)
        .await?;
    let replay = retry_output
        .recv()
        .await?
        .context("settings retry response is missing")?;
    let replay: Value = serde_json::from_slice(replay.bytes())?;
    assert_eq!(replay["result"]["id"], "settings-retry");
    assert_eq!(replay["result"]["state"], "completed");
    let mut conflicting = request;
    conflicting.params["desired"]["plan_compact"] = json!(false);
    assert!(
        service
            .dispatch(Some(session_id.clone()), conflicting, &retry_sink)
            .await
            .unwrap_err()
            .to_string()
            .contains("reused with a different request")
    );
    let root = fixture.path().join("data");
    let settings = crate::settings::SettingsStore::open(&root)?;
    let application = settings.application(&session_id)?;
    assert_eq!(application.state, "applied");
    assert_eq!(application.generation, 1);
    assert_eq!(settings.get("workspace", &session_id, "mock")?.revision, 1);
    let store = SqliteStore::open(&root)?;
    let event = store.list_session_event(&session_id)?;
    assert_eq!(event.len(), 1);
    let crate::timeline::SessionEventKind::Configuration { state, field, .. } = &event[0].detail
    else {
        panic!("configuration event is missing");
    };
    assert_eq!(state, "completed");
    assert_eq!(field, &["plan_compact"]);
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn question_answers_commit_without_waiting_for_provider_execution() -> Result<()> {
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
    let mut store = SqliteStore::open(&controller.data_root)?;
    let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
        "id":"question-exchange","session_id":session_id,"agent_id":format!("{session_id}:agent:primary"),
        "ordinal":1,"prompt":"question","kind":"chat","state":"complete","turn":[],"node_list":[],
        "created_at_ms":1,"attributed_matches_checkpoint":false,"awaiting_input":true
    }))?;
    exchange.set_elicitation(Some(crate::plan::PlanElicitation::new(serde_json::from_value(
        json!({
            "id":"set","questions":[{"id":"choice","header":"Choice","question":"Which?",
                "options":[{"label":"Yes","description":"Proceed","recommended":true}],"allow_freeform":true}]
        }),
    )?)));
    store.save_exchange(&exchange)?;
    let _busy = controller.broker.lock().await;
    let _execution = controller.coordinator.execution.acquire().await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let answer = "retained answer ".repeat(4096);
    service.dispatch_admitted(
        Some(session_id.clone()), Request { id: 2, method: "session.question.answer".into(), params: json!({
            "_command":{"id":"answer"}, "operation_id":"answer",
            "target":{"owner":"interaction","id":"question-exchange","set_id":"set","revision":1},
            "question_id":"choice","response":{"kind":"other","text":answer}
        })}, &sink, None,
    ).await?;
    assert_eq!(
        store
            .load_exchange("question-exchange")?
            .unwrap()
            .elicitation()
            .unwrap()
            .answer
            .len(),
        1
    );
    assert_eq!(
        controller.coordinator.status("answer")?["state"],
        "completed"
    );
    Ok(())
}

#[tokio::test]
async fn configuration_dispatch_keeps_owned_phase_state_bounded() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().context("session missing")?["session"]["id"]
        .as_str()
        .context("session identity missing")?
        .to_owned();
    let registry = service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let action = json!({"action":"configure","config":{"model":"fixture"}});
    let request = Request {
        id: 1,
        method: "session.command.apply".into(),
        params: action,
    };
    let dispatch = || native_settings::apply(&controller, &request, &sink);
    let bytes = crate::trace::future_bytes(&dispatch);
    assert!(
        bytes <= 32 * 1024,
        "configuration dispatcher retains {bytes} inline bytes"
    );
    service.shutdown(Duration::from_secs(5)).await?;
    Ok(())
}

#[tokio::test]
async fn pending_configuration_patches_execute_without_losing_fields() -> Result<()> {
    let fixture = tempfile::tempdir()?;
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
    let store = SqliteStore::open(&controller.data_root)?;
    let (sink, _output) = forge_protocol::outbound::channel();
    // Configuration validates each model against the provider catalog, so the patch selects the
    // mock provider's advertised model rather than an arbitrary identity.
    for (index, config) in [json!({"model":"mock-model"}), json!({"effort":"high"})]
        .into_iter()
        .enumerate()
    {
        let request = Request {
            id: u64::try_from(index)? + 2,
            method: "session.command.apply".into(),
            params: json!({"operation_id":format!("configure-{index}"),"action":"configure","config":config}),
        };
        controller
            .exchange_owner
            .execute_identified(native_settings::decode_command(&request)?, sink.clone())
            .await?;
    }
    let session = store.load_session(&session_id)?.unwrap();
    assert_eq!(session.model, "mock-model");
    assert_eq!(session.effort, "high");
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn idle_execution_configuration_preserves_running_goal_without_resume() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let mut task = crate::task::Task::goal(
        "Retain the running objective".into(),
        crate::session::continuation::ContinuationBudget::new(20, 2),
        1,
    );
    task.edit(|draft| {
        if let crate::task::WorkflowSnapshot::Goal { goal } = &mut draft.workflow {
            goal.continuation.turn_count = 4;
        }
        Ok(())
    })?;
    task.request_entry("Continue the retained objective".into(), 1)?;
    controller.task_controller.change(
        &mut store,
        None,
        crate::task::TaskChange::Install(Box::new(task)),
        1,
    )?;
    let task = controller.task_controller.current()?.unwrap();
    controller.task_controller.acknowledge_entry(
        &mut store,
        &task.revision,
        &task.entry.as_ref().unwrap().id,
        2,
    )?;
    let before = controller.task_controller.current()?.unwrap();
    assert_eq!(before.status(), crate::task::TaskStatus::Running);
    assert_eq!(controller.coordinator.execution.available_permits(), 1);
    let status_thoughts = !controller.observer.snapshot()?.status_thoughts;
    let request = Request {
        id: 2,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"idle-running-goal-config","action":"configure",
            "config":{"status_thoughts":status_thoughts}}),
    };
    let (sink, _output) = forge_protocol::outbound::channel();
    controller
        .exchange_owner
        .execute_identified(native_settings::decode_command(&request)?, sink.clone())
        .await?;
    assert_eq!(
        controller.observer.snapshot()?.status_thoughts,
        status_thoughts
    );
    assert_eq!(
        serde_json::to_value(controller.task_controller.current()?.unwrap())?,
        serde_json::to_value(&before)?,
    );
    assert!(!*controller.cancellation.subscribe().borrow());
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn provider_authentication_rejects_active_execution_and_pending_cleanup_while_setup_inspects()
-> Result<()> {
    let fixture = tempfile::tempdir()?;
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
    let (sink, _output) = forge_protocol::outbound::channel();
    for pending_cleanup in [false, true] {
        let occupied = if pending_cleanup {
            (
                Some(
                    controller
                        .coordinator
                        .control
                        .clone()
                        .acquire_owned()
                        .await?,
                ),
                None,
            )
        } else {
            (
                None,
                Some(controller.coordinator.execution.acquire().await?),
            )
        };
        // Setup is a read-only inspection handshake, so only authentication, which can change
        // native credentials and MCP discovery, requires idle execution and settled cleanup.
        let error = service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.provider.authenticate".into(),
                    params: json!({"method":"oauth-personal"}),
                },
                &sink,
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains(if pending_cleanup {
            "cleanup is pending"
        } else {
            "Finish active work"
        }));
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 3,
                    method: "session.provider.setup".into(),
                    params: json!({}),
                },
                &sink,
            )
            .await?;
        drop(occupied);
    }
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn mock_response_controls_bypass_active_execution_and_broker_lock() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "blocking"))
        .await?;
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let registry = service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    // Resolve the execution registry before holding the broker, which the rest of the test keeps
    // locked to prove mock controls never wait on it.
    let execution_registry = controller.broker.lock().await.execution_registry();
    let broker = controller.broker.lock().await;
    let execution = controller.coordinator.execution.acquire().await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.provider.mock_response".into(),
                params: json!({"operation":"hold","mode":"chat"}),
            },
            &sink,
        )
        .await?;
    let frame = output.recv().await?.context("missing hold response")?;
    let held: Value = serde_json::from_slice(frame.bytes())?;
    let hold_id = held["result"]["id"]
        .as_str()
        .context("missing hold ID")?
        .to_owned();
    let engine = execution_registry
        .owner(crate::provider::ThreadOwner::root(&session_id))
        .await?;
    let submissions = controller.submission_store.clone();
    let prompt_session = session_id.clone();
    let prompt = tokio::spawn(async move {
        let request = crate::provider::ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: None,
            harness_session_id: prompt_session,
            workspace: ".".into(),
            input: crate::provider::ProviderInput::from_text("held prompt"),
            mode: crate::provider::PromptMode::Chat,
            model: "mock-model".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
        };
        let scope =
            crate::provider::InvocationScope::new(request.owner()?, "mock-response-controls");
        let (sink, mut stream) = crate::provider::events::channel(scope);
        let execution = engine.run_once(request, sink, &submissions);
        tokio::pin!(execution);
        let mut event = Vec::new();
        let mut open = true;
        let completion = loop {
            tokio::select! {
                result = &mut execution => break result?,
                observed = stream.recv(), if open => match observed? {
                    Some(crate::provider::session_execution::OwnerObservation::Event(observed)) => event.push(observed),
                    Some(crate::provider::session_execution::OwnerObservation::InputAcceptance { .. }) => anyhow::bail!("fixture has no steering acceptance"),
                    Some(crate::provider::session_execution::OwnerObservation::HostFileChange { .. }) => anyhow::bail!("fixture has no host mutation callback"),
                    None => open = false,
                },
            }
        };
        while let Some(observed) = stream.try_next()? {
            match observed {
                crate::provider::session_execution::OwnerObservation::Event(observed) => {
                    event.push(observed)
                }
                crate::provider::session_execution::OwnerObservation::InputAcceptance {
                    ..
                } => anyhow::bail!("fixture has no steering acceptance"),
                crate::provider::session_execution::OwnerObservation::HostFileChange { .. } => {
                    anyhow::bail!("fixture has no host mutation callback")
                }
            }
        }
        Ok::<_, anyhow::Error>(crate::provider::ProviderOutput { event, completion })
    });
    for (request_id, operation) in [(3, "wait"), (4, "release")] {
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: request_id,
                    method: "session.provider.mock_response".into(),
                    params: json!({"operation":operation,"id":hold_id}),
                },
                &sink,
            )
            .await?;
        let result: Value = loop {
            let frame = output
                .recv()
                .await?
                .context("missing mock control response")?;
            let message: Value = serde_json::from_slice(frame.bytes())?;
            if message["id"] == request_id {
                break message;
            }
        };
        assert!(result.get("error").is_none(), "{result}");
        if operation == "wait" {
            assert_eq!(result["result"]["prompt"], "held prompt");
            assert!(!prompt.is_finished(), "arrival released the response");
        } else {
            assert_eq!(result["result"]["released"], true);
        }
    }
    let completed = prompt.await??;
    assert!(completed.event.iter().any(|event| matches!(
        event,
        crate::provider::HarnessEvent::Native { event: boxed_native_event, .. }  if matches!(boxed_native_event.as_ref(), crate::provider::ProviderEvent::Item { event: forge_provider_api::message::ItemEvent::Started(forge_provider_api::message::ItemStart::AgentMessage(message)), .. } if message.text == "Mock response: held prompt"))));
    drop(execution);
    drop(broker);
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn shutdown_cancels_an_unclaimed_mock_response_wait() -> Result<()> {
    let fixture = tempfile::tempdir()?;
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
    let mock_provider = controller.provider_handle();
    let control = mock_provider.mock_response_control().unwrap();
    let held = control
        .dispatch(
            &session_id,
            crate::provider::mock_response::MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let mut waiting = Box::pin(service.dispatch(
        Some(session_id),
        Request {
            id: 2,
            method: "session.provider.mock_response".into(),
            params: json!({"operation":"wait", "id":held["id"]}),
        },
        &sink,
    ));
    assert!(futures_util::poll!(&mut waiting).is_pending());
    assert!(
        service.activity.try_write().is_err(),
        "wait never entered dispatch"
    );
    let (waited, shutdown) = tokio::join!(waiting, service.shutdown(Duration::from_secs(2)));
    assert!(waited.unwrap_err().to_string().contains("cancelled"));
    shutdown?;
    Ok(())
}

#[tokio::test]
async fn model_pins_bypass_the_active_provider_broker_lock() {
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
    let broker = controller.broker.lock().await;
    let (sink, mut output) = forge_protocol::outbound::channel();
    for (method, parameter) in [
        (
            "provider.model.pin.set",
            json!({"provider":forge_providers::registry::default_id().as_str(), "model":"pinned-model", "pinned":true}),
        ),
        (
            "provider.model.pin.list",
            json!({"provider":forge_providers::registry::default_id().as_str()}),
        ),
    ] {
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: method.into(),
                    params: parameter,
                },
                &sink,
            )
            .await
            .unwrap();
        let frame = output.recv().await.unwrap().unwrap();
        let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
        assert_eq!(value["result"], json!(["pinned-model"]));
    }
    drop(broker);
    service.shutdown(Duration::from_secs(1)).await.unwrap();
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

#[tokio::test]
async fn configuration_acknowledgement_does_not_wait_for_mcp_discovery() {
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
    let (sink, mut output) = forge_protocol::outbound::channel();
    // Configuration invalidates MCP discovery by revision; it must never wait for discovery itself.
    let discovery = controller.mcp_discovery.lock().await;
    let revision_before = controller.mcp_revision.load(Ordering::Acquire);
    service.dispatch(Some(session_id.clone()), Request {
            id: 2, method: "session.command.apply".into(),
            params: json!({"operation_id":"configure-responsive","action":"configure","config":{"effort":"low"}}),
        }, &sink).await
    .unwrap();
    let value = response_for(&mut output, 2).await.unwrap();
    assert_eq!(value["result"]["state"], "completed", "{value}");
    assert!(controller.mcp_revision.load(Ordering::Acquire) > revision_before);
    drop(discovery);
    service.shutdown(Duration::from_secs(1)).await.unwrap();
}

#[tokio::test]
async fn root_model_catalog_bypasses_the_active_broker_lock() {
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
    let mut store = SqliteStore::open(&fixture.path().join("data")).unwrap();
    let preference = serde_json::from_value(json!({
        "model":"default", "effort":"medium",
        "model_setting":{"mock-model":{"reasoning":"high","context_window":"default"}}
    }))
    .unwrap();
    store
        .save_preference(fixture.path().to_str().unwrap(), "mock", &preference)
        .unwrap();
    let broker = controller.broker.lock().await;
    let (sink, mut output) = forge_protocol::outbound::channel();
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.provider.model.list".into(),
                params: json!({}),
            },
            &sink,
        )
        .await
        .unwrap();
    let frame = output.recv().await.unwrap().unwrap();
    let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
    assert_eq!(value["id"], 2, "{value}");
    assert!(value["error"].is_null(), "{value}");
    assert_eq!(value["result"][0]["id"], "mock-model");
    assert_eq!(value["result"][0]["selected_reasoning"], "high");
    assert_eq!(value["result"][0]["selected_context_window"], "default");
    drop(broker);
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
/// The catalog's default entry is a configured preference, not native evidence of the model that
/// runs, so listing the catalog leaves the actual model unknown until `ModelResolved` confirms it
/// (see the model-resolution paragraphs in `contributing/forge/architecture/providers.md`).
async fn idle_root_model_catalog_keeps_the_default_model_unresolved() {
    let fixture = tempfile::tempdir().unwrap();
    let seed_service = service();
    let seeded = seed_service
        .open_session(1, initialize(&fixture, "mock"))
        .await
        .unwrap();
    assert!(seeded.result().unwrap()["session"]["resolved_model"].is_null());
    let seed_id = seeded.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    seed_service.shutdown(Duration::from_secs(1)).await.unwrap();
    let mut store = SqliteStore::open(&fixture.path().join("data")).unwrap();
    let mut seed = store.load_session(&seed_id).unwrap().unwrap();
    seed.resolved_model = Some("previous-default".into());
    store.save_session(&seed).unwrap();
    drop(store);
    let service = service();
    let mut request = initialize(&fixture, "mock");
    request.new_session_name = Some("Cached default".into());
    let opened = service.open_session(1, request).await.unwrap();
    // A new conversation keeps its actual model unknown until native confirmation, so another
    // session's resolved default never leaks into it before the catalog resolves.
    assert!(opened.result().unwrap()["session"]["resolved_model"].is_null());
    assert_eq!(opened.result().unwrap()["session"]["model"], "default");
    let session_id = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    assert_ne!(session_id, seed_id);
    let (sink, mut output) = forge_protocol::outbound::channel();
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.provider.model.list".into(),
                params: json!({}),
            },
            &sink,
        )
        .await
        .unwrap();
    let mut resolved = false;
    loop {
        let frame = output.recv().await.unwrap().unwrap();
        let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
        if value["event"] == "session_configured" {
            resolved |= !value["payload"]["resolved_model"].is_null();
        }
        if value["id"] == 2 {
            assert_eq!(value["result"][0]["id"], "mock-model");
            assert_eq!(value["result"][0]["selected_reasoning"], "medium");
            break;
        }
    }
    assert!(
        !resolved,
        "idle catalog published an unconfirmed model resolution"
    );
    let store = SqliteStore::open(&fixture.path().join("data")).unwrap();
    assert!(
        store
            .load_session(&session_id)
            .unwrap()
            .unwrap()
            .resolved_model
            .is_none()
    );
    service.shutdown(Duration::from_secs(1)).await.unwrap();
}

#[tokio::test]
async fn child_model_catalog_bypasses_the_active_root_broker_lock() {
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
    let broker = controller.broker.lock().await;
    let (sink, mut output) = forge_protocol::outbound::channel();
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.agent.model.list".into(),
                params: json!({"provider":"mock"}),
            },
            &sink,
        )
        .await
        .unwrap();
    loop {
        let frame = output.recv().await.unwrap().unwrap();
        let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
        if value["id"] == 2 {
            assert!(value["error"].is_null(), "{value}");
            assert!(
                value["result"]["models"]
                    .as_array()
                    .is_some_and(|models| !models.is_empty()),
                "{value}"
            );
            assert_eq!(value["result"]["capability"]["fast_mode"], true, "{value}");
            assert_eq!(
                value["result"]["capability"]["ultrafast_mode"], true,
                "{value}"
            );
            break;
        }
    }
    drop(broker);
    service.shutdown(Duration::from_secs(1)).await.unwrap();
}

fn steering_provider(
    requests: Arc<Mutex<Vec<(String, String)>>>,
) -> Arc<crate::provider::test_support::EventFixtureProvider> {
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
    )
    .expect("mock fixture descriptor");
    let provider = crate::provider::test_support::EventFixtureProvider::new(
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
    let steering_requests = requests.clone();
    provider.with_steering(move |request| {
        let requests = steering_requests.clone();
        async move {
            requests
                .lock()
                .await
                .push(("steering".into(), format!("{:?}", request.input)));
            Ok(())
        }
    });
    let cleanup_requests = requests.clone();
    provider.with_cleanup(move |_| {
        let requests = cleanup_requests.clone();
        async move {
            requests
                .lock()
                .await
                .push(("cleanup".into(), String::new()));
            Err(forge_provider_api::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnInterrupt,
                    forge_provider_api::ProviderFailureKind::Transport,
                ),
            ))
        }
    });
    provider
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

/// The session owner admits a queued composer prompt and acknowledges its token exactly once.
#[tokio::test]
async fn queued_prompt_acknowledges_composer_admission_once() -> Result<()> {
    use forge_buffer::identity::{DocumentId, ViewId};
    use forge_buffer::width::WidthProfile;

    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    // Composer acknowledgements belong to an open transcript document.
    let document = DocumentId("queued-composer".into());
    controller.presentation.lock().unwrap().open(
        document.clone(),
        ViewId("queued-composer-view".into()),
        WidthProfile::default(),
    )?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let mut queued = Box::pin(service.dispatch_admitted(
        Some(session_id.clone()),
        Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Queued after finalization","queued":true,
                "submission":{"document":document,"token":42}}),
        },
        &sink,
        None,
    ));
    assert!(futures_util::poll!(queued.as_mut()).is_pending());
    assert!(
        controller.coordinator.control.try_acquire().is_ok(),
        "queued work blocked cancellation"
    );
    // Control responses bypass the ordinary event lane, so the acknowledgement may follow the
    // response. Collect every frame until the host releases the sink, then count once.
    let reader = tokio::spawn(async move {
        let mut frames = Vec::new();
        while let Some(frame) = output.recv().await? {
            frames.push(serde_json::from_slice::<Value>(frame.bytes())?);
        }
        Ok::<_, anyhow::Error>(frames)
    });
    queued.await?;
    controller.finish_prompt_workers().await?;
    drop(controller);
    service.shutdown(Duration::from_secs(1)).await?;
    drop(sink);
    let frames = reader.await??;
    let response = frames
        .iter()
        .find(|message| message["id"] == 2)
        .context("queued response missing")?;
    assert!(response.get("error").is_none(), "{response}");
    let acknowledgements: Vec<&Value> = frames
        .iter()
        .filter(|message| {
            message["event"] == "provider_event"
                && message["payload"]["kind"] == "prompt_submission"
        })
        .collect();
    assert_eq!(
        acknowledgements.len(),
        1,
        "queued admission was not acknowledged exactly once"
    );
    assert_eq!(acknowledgements[0]["payload"]["data"]["token"], 42);
    assert_eq!(acknowledgements[0]["payload"]["data"]["state"], "accepted");
    Ok(())
}

/// Transport preparation never reserves execution or changes cancellation policy.
#[tokio::test]
async fn prompt_preparation_leaves_busy_execution_owned() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
    let cleanup = controller.coordinator.control.acquire().await?;
    let execution = controller.coordinator.execution.acquire().await?;
    let cancelled = *controller.cancellation.subscribe().borrow();
    let prepared = service
        .prepare(
            Some(registry.initial_session_id()),
            RuntimeMethod::SessionPromptSubmit,
        )
        .await?;
    assert!(prepared.is_none());
    assert_eq!(*controller.cancellation.subscribe().borrow(), cancelled);
    drop(execution);
    drop(cleanup);
    assert!(controller.coordinator.execution.try_acquire().is_ok());
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

/// A queued prompt reaches durable input acceptance while existing execution stays owned.
#[tokio::test]
async fn queued_prompt_commits_input_without_acquiring_execution() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
    let execution = controller.coordinator.execution.acquire().await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    prompt::apply(&controller, Request {
        id: 2, method: "session.prompt.submit".into(),
        params: json!({"operation_id":"queued-prompt", "input_id":"queued-message", "text":"Wait for current work", "queued":true}),
    }, &sink).await?;
    let snapshot = controller.coordinator.submissions.snapshot()?;
    let message = snapshot
        .messages
        .iter()
        .find(|message| message.id == "queued-message")
        .context("queued prompt was not retained")?;
    assert_eq!(message.intent, crate::input::InputIntent::Queue);
    assert_eq!(message.delivery(), crate::input::InputDelivery::Queued);
    assert!(matches!(message.purpose, crate::input::InputPurpose::User));
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    controller.finish_prompt_workers().await?;
    drop(execution);
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
async fn configuration_waits_for_maintenance_without_provider_cleanup() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let broker = HarnessBroker::initialize(initialize(&fixture, "mock"))?;
    let controller = SessionController::new(broker).await.unwrap();
    let provider_requests = Arc::new(Mutex::new(Vec::new()));
    let provider = steering_provider(provider_requests.clone());
    let provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider)?;
    *controller.provider.write().unwrap() = provider_fixture.provider_handle();
    for method in [RuntimeMethod::SessionConfigure] {
        let maintenance = controller.broker.lock().await;
        let mut acquisition = Box::pin(acquire_request_broker(&controller, method));
        assert!(futures_util::poll!(acquisition.as_mut()).is_pending());
        assert!(
            provider_requests.lock().await.is_empty(),
            "maintenance cannot trigger provider cleanup"
        );
        assert!(controller.coordinator.execution.try_acquire().is_err());
        drop(maintenance);
        let admitted = acquisition.await?;
        assert!(controller.coordinator.execution.try_acquire().is_err());
        drop(admitted);
        assert!(controller.coordinator.execution.try_acquire().is_ok());
    }
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
async fn trace_controls_complete_while_a_turn_owns_the_broker() {
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
    let registry = service.registry.lock().await.clone().unwrap();
    let controller = registry.resolve(&session_id).await.unwrap();
    let owner = controller.broker.lock().await;
    let (sink, mut output) = forge_protocol::outbound::channel();
    for (id, method, params) in [
        (2, "host.trace.configure", json!({"enabled":true})),
        (3, "host.trace.status", json!({})),
        (4, "session.trace.clear", json!({})),
        (5, "host.trace.configure", json!({"enabled":false})),
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
            .await
            .unwrap();
        let frame = output.recv().await.unwrap().unwrap();
        let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
        assert_eq!(value["result"]["enabled"], id != 5);
        assert!(
            value["result"]["path"]
                .as_str()
                .unwrap()
                .contains(&session_id)
        );
    }
    drop(owner);
    service.shutdown(Duration::from_secs(1)).await.unwrap();
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

#[tokio::test]
async fn steering_burst_reaches_the_active_turn_before_any_acknowledgment() -> Result<()> {
    use crate::input::{InputDelivery, InputIntent, InputOperation};
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
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
    for (id, intent) in [
        ("queued", InputIntent::Queue),
        ("first", InputIntent::Auto),
        ("second", InputIntent::Auto),
        ("third", InputIntent::Auto),
    ] {
        let operation: InputOperation = serde_json::from_value(json!({
            "id": format!("submit-{id}"), "action": {"kind":"submit", "message": {
                "id":id, "text": format!("steering {id}"), "intent":intent
            }}
        }))?;
        controller.coordinator.submissions.apply(operation)?;
    }
    controller.attach_input(&sink).await?;
    let mut commands = Vec::new();
    for id in ["first", "second", "third"] {
        let command = active.receive().await.context("steering lane closed")?;
        assert_eq!(command.text, format!("steering {id}"));
        let submission = command
            .submission
            .as_ref()
            .context("missing input identity")?;
        assert_eq!(submission.record()?.input_message_id.as_deref(), Some(id));
        submission.sending(None)?;
        commands.push(command);
    }
    let snapshot = controller.coordinator.submissions.snapshot()?;
    assert_eq!(
        snapshot
            .messages
            .iter()
            .filter(|message| message.delivery() == InputDelivery::Delivering)
            .count(),
        3
    );
    assert!(
        snapshot
            .messages
            .iter()
            .any(|message| message.id == "queued" && message.delivery() == InputDelivery::Queued)
    );
    // Acknowledgments may arrive in a different order from FIFO dispatch.
    for index in [1, 0, 0] {
        let command = commands.remove(index);
        let id = command
            .submission
            .as_ref()
            .unwrap()
            .record()?
            .input_message_id
            .unwrap();
        command.submission.as_ref().unwrap().accepted(Some(
            forge_provider_api::RecoveryToken::new(format!("accepted {id}").into_bytes())?,
        ))?;
        command.complete(Ok(())).await;
    }
    async {
        loop {
            let snapshot = controller.coordinator.submissions.snapshot()?;
            if snapshot.messages.len() == 1 && snapshot.messages[0].id == "queued" {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await?;
    controller.coordinator.submissions.suspend(true, None)?;
    drop(active);
    drop(execution);
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.await??;
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

/// Pause and the following UI cancel share one native cancellation.
#[tokio::test]
async fn planning_pause_then_cancel_preserves_terminal_and_queued_input() -> Result<()> {
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
    service.dispatch(
        Some(session_id.clone()),
        Request {
            id: 22,
            method: "session.command.apply".into(),
            params: json!({"operation_id":"plan", "action":"plan", "text":"Inspect the project"}),
        },
        &sink,
    ).await?;
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
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 24,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"pause", "action":"pause"}),
            },
            &sink,
        )
        .await?;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 23,
                method: "session.turn.cancel".into(),
                params: json!({}),
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
    for id in [22, 24] {
        let response = async {
            loop {
                let response = responses
                    .lock()
                    .unwrap()
                    .iter()
                    .find(|value| value["id"] == id)
                    .cloned();
                if let Some(response) = response {
                    break response;
                }
                tokio::task::yield_now().await;
            }
        }
        .await;
        anyhow::ensure!(
            response.get("error").is_none(),
            "command RPC failed: {response}"
        );
    }
    let store = SqliteStore::open(&controller.data_root)?;
    let exchanges = store.list_exchange(&session_id)?;
    assert_eq!(exchanges.len(), 1);
    assert_eq!(
        exchanges[0].state(),
        crate::exchange::ExchangeState::Cancelled
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

/// Report SQLite contention through the host's error path and keep the host usable afterwards.
#[tokio::test]
async fn sqlite_contention_fails_explicitly_and_leaves_host_usable() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let configuration = initialize(&fixture, "mock");
    let root = PathBuf::from(&configuration.data_root);
    let opened = service.open_session(1, configuration).await?;
    let session = opened.result().unwrap()["session"]["id"]
        .as_str()
        .unwrap()
        .to_owned();
    let mut external = rusqlite::Connection::open(crate::storage::state::database(&root))?;
    let lock = external.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    // The current-thread test runtime opens the host's history handle on this thread, so a zero
    // busy timeout turns the held lock into an immediate contention failure.
    let contention = crate::storage::database::override_busy_timeout(Duration::ZERO);
    let failure = service
        .dispatch(
            Some(session.clone()),
            Request {
                id: 2,
                method: "session.prompt.history.record".into(),
                params: json!({"text":"blocked write"}),
            },
            &sink,
        )
        .await
        .unwrap_err();
    drop(contention);
    let diagnostic = crate::storage::error_message(&failure);
    assert!(diagnostic.contains("SQLite contention"), "{diagnostic}");
    assert!(diagnostic.contains("5 seconds"), "{diagnostic}");
    lock.rollback()?;
    service
        .dispatch(
            Some(session),
            Request {
                id: 3,
                method: "session.prompt.history.record".into(),
                params: json!({"text":"successful retry"}),
            },
            &sink,
        )
        .await?;
    let reply = response_for(&mut output, 3).await?;
    assert!(reply["error"].is_null(), "{reply}");
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn startup_reports_session_loading_and_presentation_timings() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    let opened = service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let timing = &opened.result().unwrap()["startup_timing"];
    for (scope, stages) in [
        (
            "phases",
            &[
                "registry_ready_ms",
                "settings_catalog_ms",
                "repository_ready_ms",
                "runtime_ready_ms",
                "broker_ready_ms",
                "snapshot_serialized_ms",
                "controller_ready_ms",
            ][..],
        ),
        (
            "broker",
            &[
                "database_open_ms",
                "deletion_recovery_ms",
                "session_selected_ms",
                "settings_applied_ms",
                "session_recovered_ms",
                "services_created_ms",
                "timeline_loaded_ms",
                "presentation_initialized_ms",
            ][..],
        ),
    ] {
        let mut previous = 0.0;
        for stage in stages {
            let elapsed = timing[scope][stage]
                .as_f64()
                .expect("startup phase missing");
            assert!(elapsed.is_finite() && elapsed >= previous);
            previous = elapsed;
        }
    }
    service.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn fresh_session_resolves_inherited_tuning_before_publication() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let request = initialize(&fixture, "mock");
    let broker = HarnessBroker::initialize(request.clone())?;
    let workspace = broker.snapshot()?.session.workspace;
    drop(broker);
    let mut store = SqliteStore::open(&fixture.path().join("data"))?;
    let mut preference = store.load_preference(&workspace, "mock")?.unwrap();
    preference.model = "mock-model".into();
    preference.effort = "removed-effort".into();
    preference.model_setting.insert(
        "mock-model".into(),
        crate::session::ModelSetting {
            reasoning: Some("removed-effort".into()),
            context_window: Some("removed-context".into()),
        },
    );
    store.save_preference(&workspace, "mock", &preference)?;
    drop(store);
    let mut request = request;
    request.new_session_name = Some("catalog-resolved".into());
    let service = service();
    service.open_session(1, request).await?;
    let registry = service.registry().await?;
    let controller = registry.resolve(registry.initial_session_id()).await?;
    let snapshot = controller.observer.snapshot()?;
    assert_eq!(snapshot.session.model, "mock-model");
    assert_eq!(snapshot.session.effort, "medium");
    assert_eq!(snapshot.session.context_window.as_deref(), Some("default"));
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
