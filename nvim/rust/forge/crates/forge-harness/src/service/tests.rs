mod bootstrap;
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
