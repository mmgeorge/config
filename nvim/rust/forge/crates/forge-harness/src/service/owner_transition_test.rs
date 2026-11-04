/// Select the point at which the second configuration must collect its predecessor.
#[derive(Clone, Copy)]
enum SessionToolToggleBoundary {
    Settled,
    Prepared,
    IdentityBeforeResult,
    VirginPreparedResume,
}

/// Keep real owner admission and held generic provider work under deterministic test control.
struct OwnerTransitionFixture {
    root: tempfile::TempDir,
    service: HarnessService,
    controller: Arc<SessionController>,
    session: String,
    provider: Arc<crate::provider::Provider>,
    engine: Arc<crate::provider::session_execution::SessionExecution>,
    sink: MessageSender,
    reader: tokio::task::JoinHandle<Result<()>>,
    original: String,
}

/// Start one held root invocation without using retired restart or cancellation flags.
async fn owner_transition_fixture() -> Result<OwnerTransitionFixture> {
    use crate::provider::mock_response::MockResponseCommand;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session).await?;
    controller.trace.configure_default(true)?;
    controller.task_controller.change(
        &mut SqliteStore::open(&controller.data_root)?,
        None,
        crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
            "Preserve the original task".into(),
            Default::default(),
            1,
        ))),
        1,
    )?;
    let provider = controller.provider_handle();
    let script = provider
        .mock_response_control()
        .context("fixture controls missing")?;
    let hold = script
        .dispatch(
            &session,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok(())
    });
    controller.attach_input(&sink).await?;
    service.dispatch(Some(session.clone()), Request { id: 2, method: "session.input.apply".into(),
        params: json!({"id":"original-owner-input","action":{"kind":"submit","message":{"id":"original-owner-input","text":"Keep this transcript through configuration","intent":"auto","attachments":[]}}}) }, &sink).await?;
    tokio::time::timeout(
        Duration::from_secs(5),
        script.dispatch(
            &session,
            MockResponseCommand::Wait {
                id: hold["id"].as_str().unwrap().into(),
            },
        ),
    )
    .await??;
    let original = SqliteStore::read_only(&controller.data_root)?
        .latest_exchange(&session)?
        .unwrap()
        .id;
    let execution_registry = controller.broker.lock().await.execution_registry();
    let engine = execution_registry
        .owner(crate::provider::ExecutionOwner::root(&session))
        .await?;
    Ok(OwnerTransitionFixture {
        root,
        service,
        controller,
        session,
        provider,
        engine,
        sink,
        reader,
        original,
    })
}

/// Preserve failed race fixtures and their final owner trace instead of losing timeout evidence.
fn retain_owner_transition_failure(
    fixture: OwnerTransitionFixture,
    error: anyhow::Error,
) -> anyhow::Error {
    let trace_path = fixture
        .controller
        .trace
        .session_status(&fixture.session)
        .path;
    let trace = std::fs::read_to_string(&trace_path)
        .unwrap_or_else(|failure| format!("trace unavailable: {failure}"));
    let tail = trace
        .lines()
        .rev()
        .take(80)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect::<Vec<_>>()
        .join("\n");
    let root = fixture.root.keep();
    error.context(format!(
        "retained owner fixture: {}; trace: {trace_path}\n{tail}",
        root.display()
    ))
}

/// Retain a custom provider fixture with the stage that stopped progressing.
fn retain_cleanup_failure(
    root: tempfile::TempDir,
    controller: &SessionController,
    error: anyhow::Error,
) -> anyhow::Error {
    let session = controller.observer.session_id();
    let trace_path = controller.trace.session_status(session).path;
    let trace = std::fs::read_to_string(&trace_path)
        .unwrap_or_else(|failure| format!("trace unavailable: {failure}"));
    let tail = trace
        .lines()
        .rev()
        .take(80)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect::<Vec<_>>()
        .join("\n");
    let retained = root.keep();
    error.context(format!(
        "retained cleanup fixture: {}; trace: {trace_path}\n{tail}",
        retained.display()
    ))
}
/// Keep invocation tool changes on the initialized binding through collection and later input.
#[tokio::test]
async fn owner_session_tool_toggles_preserve_binding_and_observation() -> Result<()> {
    session_tool_toggle_boundary(SessionToolToggleBoundary::Settled).await
}

/// Collect a retained prepared continuation through another configuration without a separate cancel.
#[tokio::test]
async fn owner_session_tool_toggles_collect_prepared_continuation_and_reuse_binding() -> Result<()>
{
    session_tool_toggle_boundary(SessionToolToggleBoundary::Prepared).await
}

/// Certify a same-binding history replacement that the real owner persists before configure returns.
#[tokio::test]
async fn owner_configuration_commits_ordered_identity_before_result_and_reuses_binding() -> Result<()> {
    session_tool_toggle_boundary(SessionToolToggleBoundary::IdentityBeforeResult).await
}

/// Retained input interrupted before native handoff must not require an unused native history on resume.
#[tokio::test]
async fn owner_prepared_configuration_close_resumes_retained_unconsumed_input() -> Result<()> {
    session_tool_toggle_boundary(SessionToolToggleBoundary::VirginPreparedResume).await
}

/// Exercise committed off/on selections across one exact native generation boundary.
async fn session_tool_toggle_boundary(boundary: SessionToolToggleBoundary) -> Result<()> {
    use crate::exchange::command::{CommandOutcome, ConfigurationOutcome};
    use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionResult, ExecutionUpdate};
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let invoked = Arc::new(tokio::sync::Notify::new());
    let opened = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let closed = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let dispatched = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let observations = Arc::new(std::sync::Mutex::new(Vec::new()));
    let (prepared, mut preparation) = tokio::sync::mpsc::unbounded_channel();
    let identity_controller: Arc<std::sync::Mutex<std::sync::Weak<SessionController>>> = Arc::default();
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(descriptor, {
        let invoked = invoked.clone();
        let dispatched = dispatched.clone();
        let observations = observations.clone();
        let identity_controller = identity_controller.clone();
        move |request, sink, mut cancelled| {
            let invoked = invoked.clone();
            let identity_controller = identity_controller.clone();
            let first = dispatched.fetch_add(1, Ordering::AcqRel) == 0;
            observations.lock().unwrap().push((
                sink.binding_id().clone(),
                request.input.clone(),
                request
                    .tools
                    .iter()
                    .map(|tool| tool.name)
                    .collect::<Vec<_>>(),
            ));
            async move {
                let thread_id = identity_controller.lock().unwrap().upgrade()
                    .expect("native execution retains its session owner")
                    .observer.snapshot().expect("fixture session snapshot remains readable")
                    .session.provider_session_id.expect("opened fixture history is observed before input");
                let address = crate::provider::TurnAddress {
                    scope: request.scope.clone(),
                    thread_id,
                    turn_id: request.scope.invocation_id.as_str().into(),
                };
                sink.send_wait(crate::provider::ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(crate::provider::ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
                })
                .await?;
                if first {
                    invoked.notify_one();
                    while !*cancelled.borrow_and_update() {
                        cancelled
                            .changed()
                            .await
                            .map_err(|_| crate::provider::ProviderError::Cancelled)?;
                    }
                } else {
                    for event in crate::provider::test_support::message(
                        &address,
                        "tool-toggle-answer",
                        Some(crate::provider::MessageRole::Response),
                        "Binding remains usable",
                    ) {
                        sink.send_wait(event).await?;
                    }
                }
                sink.send_wait(crate::provider::ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: if first {
                            forge_provider_api::turn::TurnOutcome::Cancelled
                        } else {
                            forge_provider_api::turn::TurnOutcome::Completed
                        },
                        completed_at_ms: 2,
                    },
                })
                .await?;
                let completion = forge_provider_api::completion::ProviderCompletion {
                    provider_session_id: Some(address.thread_id.clone().try_into().unwrap()),
                    ..Default::default()
                }.into();
                sink.send_wait(crate::provider::ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope,
                        ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 2,
                            result: if first {
                                ExecutionResult::Cancelled { completion }
                            } else {
                                ExecutionResult::Completed { completion }
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        }
    });
    if matches!(boundary, SessionToolToggleBoundary::Prepared | SessionToolToggleBoundary::VirginPreparedResume) {
        let preparations = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        native.with_pre_dispatch(move |request| {
            let ordinal = preparations.fetch_add(1, Ordering::AcqRel);
            let hold = matches!(boundary, SessionToolToggleBoundary::VirginPreparedResume) || ordinal == 1;
            let prepared = prepared.clone();
            async move {
                if hold {
                    prepared
                        .send((
                            request.scope,
                            request
                                .tools
                                .iter()
                                .map(|tool| tool.name)
                                .collect::<Vec<_>>(),
                        ))
                        .expect("preparation observer stays attached");
                    return std::future::pending().await;
                }
                Ok(())
            }
        });
    }
    if matches!(boundary, SessionToolToggleBoundary::IdentityBeforeResult) {
        let native = Arc::downgrade(&native);
        let controller = identity_controller.clone();
        let configured = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        native.upgrade().unwrap().with_session_configure(move |binding, _| {
            let native = native.upgrade().expect("configuration retains its adapter");
            let controller = controller.lock().unwrap().upgrade().expect("configuration retains its session owner");
            let history = format!("configured-tool-history-{}", configured.fetch_add(1, Ordering::AcqRel));
            async move {
                let owner = crate::provider::ExecutionOwner::root(controller.observer.session_id());
                let sink = native.event_sink(&owner).await?;
                if sink.binding_id() != &binding { return Err(crate::provider::ProviderError::DeliveryUncertain); }
                sink.send_wait(crate::provider::ProviderEvent::SessionIdentity {
                    scope: crate::provider::EventScope::Binding { owner },
                    provider_session_id: history.clone().try_into().unwrap(),
                }).await?;
                tokio::time::timeout(Duration::from_secs(5), async {
                    loop {
                        let snapshot = controller.observer.snapshot().map_err(|_| crate::provider::ProviderError::DeliveryUncertain)?;
                        if snapshot.session.provider_session_id.as_deref() == Some(history.as_str()) { break Ok::<(), crate::provider::ProviderError>(()); }
                        tokio::task::yield_now().await;
                    }
                }).await.map_err(|_| crate::provider::ProviderError::DeliveryUncertain)??;
                Ok(())
            }
        });
    }
    native.with_session_open({
        let opened = opened.clone();
        move |_| {
            opened.fetch_add(1, Ordering::AcqRel);
            async { Ok(()) }
        }
    });
    native.with_session_close({
        let closed = closed.clone();
        move |_| {
            closed.fetch_add(1, Ordering::AcqRel);
            async { Ok(()) }
        }
    });
    let provider_fixture =
        crate::provider::test_support::ProviderTestSession::from_native(native.clone())?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let controller = SessionController::new(broker)?;
    *identity_controller.lock().unwrap() = Arc::downgrade(&controller);
    controller.trace.configure_default(true)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let scenario = async {
        controller.start_provider(&sink).await;
        tokio::time::timeout(Duration::from_secs(5), async {
            while !controller.provider_ready().await { tokio::task::yield_now().await; }
        }).await.context("tool toggle provider did not become ready")?;
        controller.attach_input(&sink).await?;
        prompt::apply(&controller, Request { id: 70, method: "session.prompt.submit".into(),
            params: json!({"operation_id":"tool-toggle-original","text":"Hold this initialized invocation"}),
        }, &sink).await?;
        if matches!(boundary, SessionToolToggleBoundary::VirginPreparedResume) {
            tokio::time::timeout(Duration::from_secs(5), preparation.recv()).await?
                .context("original input did not reach retained preparation")?;
            ensure!(controller.observer.snapshot()?.session.provider_session_id.is_some(), "fixture did not retain its unused native history");
        } else {
            tokio::time::timeout(Duration::from_secs(5), invoked.notified()).await?;
        }
        let owner = crate::provider::ExecutionOwner::root(controller.observer.session_id());
        let original = native.event_sink(&owner).await?;
        let binding = original.binding_id().clone();
        let inbox = controller.observer.session_events();
        for (enabled, operation, text) in [
            (false, "tool-toggle-off", "Follow up without session tools"),
            (true, "tool-toggle-on", "Follow up with session tools"),
        ] {
            let command = native_settings::decode_command(&Request { id: 71,
                method: "session.command.apply".into(),
                params: json!({"operation_id":operation,"action":"configure","config":{"session_tools":enabled}}),
            })?;
            let completed = tokio::time::timeout(Duration::from_secs(10),
                controller.exchange_owner.execute_identified(command, sink.clone()),
            ).await.context("tool toggle did not commit")??;
            ensure!(matches!(completed.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings { .. })), "tool toggle lost its committed result");
            ensure!(controller.observer.snapshot()?.session_tools == enabled, "tool toggle did not publish committed settings");
            ensure!(!controller.task_controller.configuration_is_uncertain(), "successful tool toggle fenced configuration");
            ensure!(!inbox.is_closed() && Arc::ptr_eq(&inbox, &controller.observer.session_events()), "tool toggle replaced or closed observation");
            ensure!(native.event_sink(&owner).await?.binding_id() == &binding, "tool toggle replaced native binding");
            ensure!(opened.load(Ordering::Acquire) == 1 && closed.load(Ordering::Acquire) == 0, "tool toggle opened or closed native resources");
            if matches!(boundary, SessionToolToggleBoundary::VirginPreparedResume) {
                tokio::time::timeout(Duration::from_secs(5), preparation.recv()).await?
                    .context("configured unconsumed continuation did not reach preparation")?;
                ensure!(dispatched.load(Ordering::Acquire) == 0, "virgin input crossed native handoff");
                let retained = SqliteStore::read_only(&controller.data_root)?.list_exchange(owner.session_id.as_str())?;
                ensure!(!retained.is_empty() && retained.iter().all(|exchange| exchange.input_message_id.is_some() && exchange.turn.is_empty()), "retained input lost its journal linkage or acquired native history");
                return Ok::<(), anyhow::Error>(());
            }
            if matches!(boundary, SessionToolToggleBoundary::IdentityBeforeResult) {
                let history = if enabled { "configured-tool-history-1" } else { "configured-tool-history-0" };
                ensure!(controller.observer.snapshot()?.session.provider_session_id.as_deref() == Some(history), "ordered native identity was not committed");
                ensure!(SqliteStore::read_only(&controller.data_root)?.load_session(controller.observer.session_id())?.unwrap().provider_session_id.as_deref() == Some(history), "configured history was not durable");
            }
            if matches!(boundary, SessionToolToggleBoundary::Prepared) && !enabled {
                let (scope, tools) = tokio::time::timeout(Duration::from_secs(5), preparation.recv())
                    .await?.context("configured continuation did not reach retained preparation")?;
                ensure!(scope.owner == owner && !tools.contains(&"harness_session_list"), "retained preparation used stale tool configuration");
                ensure!(dispatched.load(Ordering::Acquire) == 1, "prepared continuation reached native handoff before collection");
                continue;
            }
            if matches!(boundary, SessionToolToggleBoundary::Prepared) {
                ensure!(observations.lock().unwrap().iter().all(|(_, _, tools)| tools.contains(&"harness_session_list")), "collected middle generation reached native execution");
            }
            prompt::apply(&controller, Request { id: 72, method: "session.prompt.submit".into(),
                params: json!({"operation_id":format!("{operation}-followup"),"text":text}),
            }, &sink).await?;
            tokio::time::timeout(Duration::from_secs(5), async {
                loop {
                    let observed = observations.lock().unwrap().iter().find(|(_, input, _)| {
                        matches!(input, forge_provider_api::input::PromptInput::Text(value) if value.contains(text))
                    }).cloned();
                    if let Some((observed_binding, _, tools)) = observed {
                        ensure!(observed_binding == binding, "follow-up input used another binding");
                        ensure!(tools.contains(&"harness_session_list") == enabled, "follow-up input retained obsolete session tool authorization");
                        break Ok::<(), anyhow::Error>(());
                    }
                    tokio::task::yield_now().await;
                }
            }).await.context("follow-up input did not reuse configured binding")??;
        }
        Ok::<(), anyhow::Error>(())
    }.await;
    if let Err(error) = scenario {
        return Err(retain_cleanup_failure(root, &controller, error));
    }
    let cleanup = async {
        controller.input_stopping.store(true, Ordering::Release);
        controller.coordinator.submissions.changed.notify_one();
        tokio::time::timeout(Duration::from_secs(5), controller.finish_exchange_owner()).await
            .context("tool toggle owner shutdown did not settle")??;
        provider_fixture.provider_handle().shutdown().await.context("tool toggle provider shutdown failed")?;
        controller.finish_session_events().await.context("tool toggle observation shutdown failed")?;
        if matches!(boundary, SessionToolToggleBoundary::VirginPreparedResume) {
        let session_id = controller.observer.session_id().to_owned();
        let before = SqliteStore::read_only(&controller.data_root)?.list_exchange(&session_id)?;
        ensure!(!before.is_empty(), "close removed the retained exchange");
        controller.broker.lock().await.release_lease()?;
        let mut request = initialize(&root, "mock");
        request.session_id = Some(session_id.clone());
        let resumed = HarnessBroker::initialize(request)?;
        let snapshot = resumed.snapshot()?;
        ensure!(snapshot.session.provider_session_id.is_none(), "positive nonconsumption retained an unusable native history");
        ensure!(!snapshot.session_tools, "resume lost committed tool configuration");
        let after = SqliteStore::read_only(resumed.data_root())?.list_exchange(&session_id)?;
        ensure!(before.len() == after.len() && before.iter().zip(&after).all(|(before, after)| before.id == after.id && before.prompt == after.prompt && before.input_message_id == after.input_message_id && before.checkpoint_before == after.checkpoint_before), "native identity reset changed retained host history");
        }
        Ok::<(), anyhow::Error>(())
    }.await;
    if let Err(error) = cleanup {
        return Err(retain_cleanup_failure(root, &controller, error));
    }
    reader.abort();
    Ok(())
}

/// Observe independent FIFO configuration completions before queued pause collects the replacement.
#[tokio::test]
async fn owner_model_changes_precede_pause_and_preserve_transcript() -> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandRequest, InputControl, TaskCommand, TaskSelection,
    };
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    let script = fixture.provider.mock_response_control().unwrap();
    for _ in 0..2 {
        script
            .dispatch(
                &fixture.session,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
    }
    let request = |id: &str, model: &str| {
        native_settings::decode_command(&Request {
            id: 3,
            method: "session.command.apply".into(),
            params: json!({"operation_id":id,"action":"configure","config":{"model":model}}),
        })
    };
    let mut first = Box::pin(fixture.controller.exchange_owner.execute_identified(
        request("owner-model-b", "mock-model")?,
        fixture.sink.clone(),
    ));
    assert!(futures_util::poll!(first.as_mut()).is_pending());
    let mut second = Box::pin(
        fixture
            .controller
            .exchange_owner
            .execute_identified(request("owner-model-c", "default")?, fixture.sink.clone()),
    );
    assert!(futures_util::poll!(second.as_mut()).is_pending());
    let pause = CommandRequest {
        id: CommandId::new("owner-after-models-pause")?,
        command: Command::Task(TaskCommand::Pause(TaskSelection::capture(
            &fixture.controller.task_controller.current()?.unwrap(),
        ))),
        input_control: InputControl::AtExecution,
    };
    let mut paused = Box::pin(
        fixture
            .controller
            .exchange_owner
            .execute_identified(pause, fixture.sink.clone()),
    );
    assert!(futures_util::poll!(paused.as_mut()).is_pending());
    let completed = tokio::time::timeout(Duration::from_secs(10), async {
        tokio::join!(first, second, paused)
    })
    .await
    .map_err(anyhow::Error::from)
    .and_then(|(first, second, paused)| Ok((first?, second?, paused?)));
    let (first, second, paused) = match completed {
        Ok(completed) => completed,
        Err(error) => return Err(retain_owner_transition_failure(fixture, error)),
    };
    assert!(first.sequence < second.sequence && second.sequence < paused.sequence);
    assert_eq!(
        fixture.controller.observer.snapshot()?.session.model,
        "default"
    );
    assert_eq!(
        fixture
            .controller
            .task_controller
            .current()?
            .unwrap()
            .status(),
        crate::task::TaskStatus::Paused
    );
    assert!(fixture.engine.active_invocation_snapshot()?.is_none());
    let store = SqliteStore::read_only(&fixture.controller.data_root)?;
    let original = store.load_exchange(&fixture.original)?.unwrap();
    assert!(!original.turn.is_empty());
    assert!(original.prompt.contains("Keep this transcript"));
    assert_eq!(
        store.latest_exchange(&fixture.session)?.unwrap().id,
        fixture.original
    );
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}

/// Two queued model changes must finish locally before cancellation settles their retained replacement.
#[tokio::test]
async fn owner_two_model_changes_then_cancel_settles_latest_retained_work_once() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, CancellationOutcome, Command, CommandId, CommandOutcome,
        CommandRequest, ConfigurationOutcome, InputControl,
    };
    use crate::exchange::journal::CommandState;
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    fixture.controller.trace.configure(true)?;
    let ids = [
        "owner-cancel-model-b",
        "owner-cancel-model-c",
        "owner-cancel-after-models",
    ];
    let scenario = async {
        let script = fixture.provider.mock_response_control().context("mock control missing")?;
        for _ in 0..2 {
            script.dispatch(&fixture.session, MockResponseCommand::Hold { mode: None, any_session: false }).await?;
        }
        let request = |id: &str, model: &str| native_settings::decode_command(&Request {
            id: 42,
            method: "session.command.apply".into(),
            params: json!({"operation_id":id,"action":"configure","config":{"model":model}}),
        });
        let barrier = fixture.controller.coordinator.control.clone().acquire_owned().await?;
        let mut first = Box::pin(fixture.controller.exchange_owner.execute_identified(request(ids[0], "mock-model")?, fixture.sink.clone()));
        assert!(futures_util::poll!(first.as_mut()).is_pending());
        let mut second = Box::pin(fixture.controller.exchange_owner.execute_identified(request(ids[1], "default")?, fixture.sink.clone()));
        assert!(futures_util::poll!(second.as_mut()).is_pending());
        let mut cancellation = Box::pin(fixture.controller.exchange_owner.execute_identified(CommandRequest {
            id: CommandId::new(ids[2])?,
            command: Command::CancelExchange(CancelExchangeRequest::default()),
            input_control: InputControl::AtExecution,
        }, fixture.sink.clone()));
        assert!(futures_util::poll!(cancellation.as_mut()).is_pending());
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let first = fixture.controller.exchange_owner.command_status(CommandId::new(ids[0])?).await?;
                let second = fixture.controller.exchange_owner.command_status(CommandId::new(ids[1])?).await?;
                let cancellation = fixture.controller.exchange_owner.command_status(CommandId::new(ids[2])?).await?;
                if matches!(first.as_ref().map(|record| &record.state), Some(CommandState::Active { .. }))
                    && matches!(second.as_ref().map(|record| &record.state), Some(CommandState::Queued { .. }))
                    && matches!(cancellation.as_ref().map(|record| &record.state), Some(CommandState::Queued { .. })) {
                    break Ok::<_, anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        }).await.context("configuration/cancel intents did not remain in FIFO order")??;
        drop(barrier);
        let (first, second, cancellation) = tokio::time::timeout(Duration::from_secs(10), async {
            tokio::join!(first, second, cancellation)
        }).await.context("two model changes then cancel waited for held generation acknowledgments")?;
        let (first, second, cancellation) = (first?, second?, cancellation?);
        ensure!(first.sequence < second.sequence && second.sequence < cancellation.sequence, "configuration and cancel completion receipts lost FIFO order");
        ensure!(matches!(first.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings { .. })), "first model change lost its own result");
        ensure!(matches!(second.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings { .. })), "second model change lost its own result");
        ensure!(matches!(cancellation.outcome, CommandOutcome::Cancelled(CancellationOutcome::Cancelled { ref exchange }) if exchange.as_str() == fixture.original), "cancel did not settle the replacement in the original exchange");
        ensure!(fixture.controller.observer.snapshot()?.session.model == "default", "cancel reverted the second committed model");
        ensure!(fixture.engine.active_invocation_snapshot()?.is_none(), "cancel left a successor invocation running");
        ensure!(fixture.controller.coordinator.submissions.snapshot()?.suspended, "cancel did not suspend further input");
        let store = SqliteStore::read_only(&fixture.controller.data_root)?;
        let exchange = store.load_exchange(&fixture.original)?.context("configuration lost original history")?;
        ensure!(exchange.state() == crate::exchange::ExchangeState::Cancelled && !exchange.turn.is_empty(), "cancel lost terminal state or accepted transcript");
        ensure!(exchange.prompt.contains("Keep this transcript"), "configuration replaced the original prompt");
        for id in ids {
            let record = fixture.controller.exchange_owner.command_status(CommandId::new(id)?).await?.context("command receipt disappeared")?;
            ensure!(matches!(record.state, CommandState::Finished { outcome: Ok(_), .. }), "returned command result was not durable");
        }
        let trace = fixture.controller.trace.clone();
        let path = trace.session_status(&fixture.session).path;
        let records = tokio::task::spawn_blocking(move || -> Result<Vec<Value>> {
            trace.flush()?;
            std::fs::read_to_string(path)?.lines().map(|line| serde_json::from_str(line).map_err(Into::into)).collect()
        }).await??;
        for id in ids {
            let count = |event| records.iter().filter(|record| record["event"].as_str() == Some(event)
                && record["payload"]["command_id"].as_str() == Some(id)).count();
            ensure!(count("exchange.command.completed") == 1 && count("exchange.command.response") == 1, "command {id} completed or responded more than once");
        }
        Ok::<_, anyhow::Error>(())
    }.await;
    if let Err(error) = scenario {
        return Err(retain_owner_transition_failure(fixture, error));
    }
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}

/// Resolve configuration at retained scheduling so later FIFO work can cancel a prepared successor.
#[tokio::test]
async fn owner_model_changes_then_cancel_collects_undispatched_replacement() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, CancellationOutcome, Command, CommandId, CommandOutcome,
        CommandRequest, ConfigurationOutcome, InputControl, InvocationDisposition,
    };
    use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionResult, ExecutionUpdate};
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let invoked = Arc::new(tokio::sync::Notify::new());
    let dispatched = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let preparations = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let (prepared, mut preparation) = tokio::sync::mpsc::unbounded_channel();
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(descriptor, {
        let invoked = invoked.clone();
        let dispatched = dispatched.clone();
        move |request, sink, mut cancelled| {
            let invoked = invoked.clone();
            let dispatched = dispatched.clone();
            async move {
                dispatched.fetch_add(1, Ordering::AcqRel);
                let address = crate::provider::TurnAddress {
                    scope: request.scope.clone(),
                    thread_id: "retained-thread".into(),
                    turn_id: "retained-turn".into(),
                };
                sink.send_wait(crate::provider::ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(crate::provider::ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
                })
                .await?;
                invoked.notify_one();
                while !*cancelled.borrow_and_update() {
                    cancelled
                        .changed()
                        .await
                        .map_err(|_| crate::provider::ProviderError::Cancelled)?;
                }
                sink.send_wait(crate::provider::ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Cancelled,
                        completed_at_ms: 2,
                    },
                })
                .await?;
                sink.send_wait(crate::provider::ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id.clone()],
                            completed_at_ms: 2,
                            result: ExecutionResult::Cancelled {
                                completion:
                                    forge_provider_api::completion::ProviderCompletion::default()
                                        .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        }
    });
    native.with_models(vec![crate::provider::ProviderModel {
        id: "mock-model".into(),
        service_tiers: Vec::new(),
        reasoning: vec!["low".into(), "medium".into(), "high".into()],
        default_reasoning: Some("medium".into()),
        selected_reasoning: None,
        context_window: vec![forge_provider_api::model::ProviderContextWindow {
            id: "default".into(),
            token_limit: Some(100_000),
        }],
        default_context_window: Some("default".into()),
        selected_context_window: None,
        vision: false,
        description: None,
        is_default: true,
    }]);
    native.with_pre_dispatch({
        let preparations = preparations.clone();
        move |request| {
            let first = preparations.fetch_add(1, Ordering::AcqRel) == 0;
            let prepared = prepared.clone();
            async move {
                if first {
                    return Ok(());
                }
                prepared
                    .send(request.scope)
                    .expect("preparation observer stays attached");
                std::future::pending().await
            }
        }
    });
    let provider_fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let controller = SessionController::new(broker)?;
    controller.trace.configure_default(true)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let scenario = async {
        controller.start_provider(&sink).await;
        tokio::time::timeout(Duration::from_secs(5), async {
            while !controller.provider_ready().await { tokio::task::yield_now().await; }
        }).await.context("provider readiness timed out")?;
        controller.attach_input(&sink).await?;
        controller.task_controller.change(&mut SqliteStore::open(&controller.data_root)?, None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Continue through retained preparation".into(), Default::default(), 1,
            ))), 1)?;
        prompt::apply(&controller, Request { id: 50, method: "session.prompt.submit".into(),
            params: json!({"operation_id":"retained-original","text":"Keep original accepted history"}),
        }, &sink).await?;
        tokio::time::timeout(Duration::from_secs(5), invoked.notified()).await?;
        let session = controller.observer.session_id();
        let original = SqliteStore::read_only(&controller.data_root)?.latest_exchange(session)?.unwrap().id;
        let configure = |id: &str, model: &str| native_settings::decode_command(&Request {
            id: 51, method: "session.command.apply".into(),
            params: json!({"operation_id":id,"action":"configure","config":{"model":model}}),
        });
        let first = tokio::time::timeout(Duration::from_secs(5),
            controller.exchange_owner.execute_identified(configure("retained-model-a", "mock-model")?, sink.clone()),
        ).await.context("model A waited for replacement handoff")??;
        let held = tokio::time::timeout(Duration::from_secs(5), preparation.recv()).await?
            .context("configured successor never reached held preparation")?;
        ensure!(held.exchange_id.as_str() == original, "preparation changed exchange identity");
        ensure!(matches!(first.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings {
            disposition: InvocationDisposition::Committed, ..
        })), "model A did not complete at committed scheduling");
        let mut second = Box::pin(controller.exchange_owner.execute_identified(
            configure("retained-model-b", "default")?, sink.clone()));
        assert!(futures_util::poll!(second.as_mut()).is_pending());
        let cancel = controller.exchange_owner.execute_identified(CommandRequest {
            id: CommandId::new("retained-cancel")?,
            command: Command::CancelExchange(CancelExchangeRequest::default()),
            input_control: InputControl::AtExecution,
        }, sink.clone());
        let (second, cancel) = tokio::time::timeout(Duration::from_secs(10), async {
            tokio::join!(second, cancel)
        }).await.context("queued configuration/cancel waited for undispatched replacement")?;
        let (second, cancel) = (second?, cancel?);
        ensure!(first.sequence < second.sequence && second.sequence < cancel.sequence, "retained work lost FIFO order");
        ensure!(matches!(second.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings {
            disposition: InvocationDisposition::Committed, ..
        })), "model B lost committed scheduling result");
        ensure!(matches!(cancel.outcome, CommandOutcome::Cancelled(CancellationOutcome::Cancelled {
            ref exchange,
        }) if exchange.as_str() == original), "cancel did not settle retained original exchange");
        ensure!(dispatched.load(Ordering::Acquire) == 1, "a held successor reached native handoff");
        ensure!(controller.observer.snapshot()?.session.model == "default", "cancel reverted model B");
        let engine = controller.broker.lock().await.execution_registry()
            .existing_owner(&crate::provider::ExecutionOwner::root(session))?.context("execution owner missing")?;
        ensure!(engine.active_invocation_snapshot()?.is_none(), "cancel left pending native work");
        let exchange = SqliteStore::read_only(&controller.data_root)?.load_exchange(&original)?.unwrap();
        ensure!(exchange.state() == crate::exchange::ExchangeState::Cancelled
            && exchange.prompt.contains("Keep original accepted history"), "cancel lost original accepted history");
        Ok::<(), anyhow::Error>(())
    }.await;
    if let Err(error) = scenario {
        return Err(retain_cleanup_failure(root, &controller, error));
    }
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    tokio::time::timeout(Duration::from_secs(5), controller.finish_exchange_owner()).await??;
    provider_fixture.provider_handle().shutdown().await?;
    controller.finish_session_events().await?;
    reader.abort();
    Ok(())
}

/// Reject unavailable selections before touching active work and leave later commands usable.
#[tokio::test]
async fn owner_invalid_model_rejects_before_collection_and_preserves_queue() -> Result<()> {
    use crate::exchange::command::{CommandFailure, ConfigurationRejection, ExecuteCommandError};
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    let rejected = native_settings::decode_command(&Request {
        id: 30,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"owner-invalid-model","action":"configure","config":{"model":"missing-owner-model"}}),
    })?;
    let outcome = tokio::time::timeout(
        Duration::from_secs(5),
        fixture
            .controller
            .exchange_owner
            .execute_identified(rejected, fixture.sink.clone()),
    )
    .await?;
    assert_eq!(
        outcome
            .as_ref()
            .err()
            .context("unavailable model unexpectedly succeeded")?
            .code(),
        "configuration_model_unavailable"
    );
    assert!(matches!(
        outcome,
        Err(ExecuteCommandError::Failed(
            CommandFailure::ConfigurationRejected(ConfigurationRejection::ModelUnavailable)
        ))
    ));
    assert!(
        fixture.engine.active_invocation_snapshot()?.is_some(),
        "invalid model preflight must not collect the original invocation"
    );
    assert_eq!(
        SqliteStore::read_only(&fixture.controller.data_root)?
            .latest_exchange(&fixture.session)?
            .unwrap()
            .id,
        fixture.original
    );
    fixture
        .provider
        .mock_response_control()
        .unwrap()
        .dispatch(
            &fixture.session,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let valid = native_settings::decode_command(&Request {
        id: 31,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"owner-valid-model-after-rejection","action":"configure","config":{"model":"mock-model"}}),
    })?;
    tokio::time::timeout(
        Duration::from_secs(10),
        fixture
            .controller
            .exchange_owner
            .execute_identified(valid, fixture.sink.clone()),
    )
    .await??;
    assert_eq!(
        fixture.controller.observer.snapshot()?.session.model,
        "mock-model"
    );
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}

/// Permission changes collect child work as well as the root before the replacement can run.
#[tokio::test]
async fn owner_permission_change_collects_child_before_replacement() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = owner_transition_fixture().await?;
    let child_provider = crate::provider::test_support::build_mock(
        fixture.controller.data_root.join("child-provider"),
        fixture.controller.permission.clone(),
        fixture.controller.trace.clone(),
    )?;
    fixture
        .controller
        .broker
        .lock()
        .await
        .agent_provider_factory
        .register(child_provider.clone());
    let script = child_provider.mock_response_control().unwrap();
    let child_hold = script
        .dispatch(
            &fixture.session,
            MockResponseCommand::Hold {
                mode: None,
                any_session: true,
            },
        )
        .await?;
    fixture.controller.attach_agents(&fixture.sink).await?;
    let spawned = fixture
        .controller
        .agent_coordinator
        .sender()
        .request(crate::agent::AgentRequest {
            operation_id: "owner-child-for-permission".into(),
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
                profile: Some("worker".into()),
                text: "Wait for collection".into(),
            }),
        })
        .await?;
    let child = spawned["agent"]["id"]
        .as_str()
        .context("spawned child identity missing")?
        .to_owned();
    let arrived = tokio::time::timeout(
        Duration::from_secs(5),
        script.dispatch(
            &fixture.session,
            MockResponseCommand::Wait {
                id: child_hold["id"].as_str().unwrap().into(),
            },
        ),
    )
    .await
    .map_err(anyhow::Error::from)
    .and_then(|result| result);
    let arrived = match arrived {
        Ok(arrived) => arrived,
        Err(error) => {
            return Err(retain_owner_transition_failure(
                fixture,
                error.context("registered child provider did not reach its hold"),
            ));
        }
    };
    assert_eq!(
        arrived["session_id"],
        crate::provider::ExecutionOwner {
            session_id: fixture.session.clone().into(),
            agent_id: child.into()
        }
        .runtime_key()
    );
    assert!(fixture.controller.agent_coordinator.active_count() > 0);
    let command = native_permission::decode_command(&Request {
        id: 4,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"owner-collect-child","action":"permission","change":{"kind":"set_approval","mode":"deny"}}),
    })?;
    let completed = tokio::time::timeout(
        Duration::from_secs(10),
        fixture
            .controller
            .exchange_owner
            .execute_identified(command, fixture.sink.clone()),
    )
    .await
    .map_err(anyhow::Error::from)
    .and_then(|result| result.map_err(anyhow::Error::from));
    if let Err(error) = completed {
        return Err(retain_owner_transition_failure(
            fixture,
            error.context("permission command did not collect the held child"),
        ));
    }
    assert_eq!(fixture.controller.agent_coordinator.active_count(), 0);
    assert!(!fixture.controller.agent_coordinator.is_quiescing());
    assert_eq!(
        fixture
            .controller
            .observer
            .snapshot()?
            .session
            .approval_mode,
        crate::session::ApprovalMode::Deny
    );
    assert!(
        SqliteStore::read_only(&fixture.controller.data_root)?
            .load_exchange(&fixture.original)?
            .is_some()
    );
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}

/// A timed-out cleanup keeps its actual worker and leaves the requested policy uncommitted.
#[tokio::test]
async fn owner_permission_timeout_retains_cleanup_and_prior_policy() -> Result<()> {
    use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionResult, ExecutionUpdate};
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let invoked = Arc::new(tokio::sync::Notify::new());
    let started = Arc::new(tokio::sync::Notify::new());
    let release = Arc::new(tokio::sync::Semaphore::new(0));
    let collected = Arc::new(AtomicBool::new(false));
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(descriptor, {
        let invoked = invoked.clone();
        move |request, sink, mut cancelled| {
            let invoked = invoked.clone();
            async move {
                let address = crate::provider::TurnAddress {
                    scope: request.scope.clone(),
                    thread_id: "cleanup-thread".into(),
                    turn_id: "cleanup-turn".into(),
                };
                sink.send_wait(crate::provider::ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(crate::provider::ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
                })
                .await?;
                invoked.notify_one();
                while !*cancelled.borrow_and_update() {
                    cancelled
                        .changed()
                        .await
                        .map_err(|_| crate::provider::ProviderError::Cancelled)?;
                }
                sink.send_wait(crate::provider::ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Cancelled,
                        completed_at_ms: 2,
                    },
                })
                .await?;
                sink.send_wait(crate::provider::ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id.clone()],
                            completed_at_ms: 2,
                            result: ExecutionResult::Cancelled {
                                completion:
                                    forge_provider_api::completion::ProviderCompletion::default()
                                        .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        }
    });
    native.with_cleanup({
        let started = started.clone();
        let release = release.clone();
        let collected = collected.clone();
        move |_| {
            let started = started.clone();
            let release = release.clone();
            let collected = collected.clone();
            async move {
                if !collected.load(Ordering::Acquire) {
                    started.notify_one();
                    release.acquire().await.unwrap().forget();
                    collected.store(true, Ordering::Release);
                }
                Ok(())
            }
        }
    });
    let provider_fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let controller = SessionController::new(broker)?;
    controller.trace.configure_default(true)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    if let Err(error) = tokio::time::timeout(Duration::from_secs(5), async {
        while !controller.provider_ready().await {
            tokio::task::yield_now().await;
        }
    })
    .await
    {
        return Err(retain_cleanup_failure(
            root,
            &controller,
            anyhow::Error::from(error).context("provider readiness timed out"),
        ));
    }
    controller.attach_input(&sink).await?;
    prompt::apply(
        &controller,
        Request {
            id: 50,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"cleanup-live-input","text":"Wait for cleanup"}),
        },
        &sink,
    )
    .await?;
    if let Err(error) = tokio::time::timeout(Duration::from_secs(5), invoked.notified()).await {
        return Err(retain_cleanup_failure(
            root,
            &controller,
            anyhow::Error::from(error).context("fixture invocation never started"),
        ));
    }
    let before = controller.observer.snapshot()?.session.access;
    let command = native_permission::decode_command(&Request {
        id: 51,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"owner-cleanup-timeout","action":"permission","change":{"kind":"toggle_write_enabled"}}),
    })?;
    let mut operation = Box::pin(
        controller
            .exchange_owner
            .execute_identified(command, sink.clone()),
    );
    let entered = tokio::time::timeout(Duration::from_secs(5), async {
        tokio::select! {
            result = &mut operation => Err(anyhow::anyhow!("cleanup unexpectedly completed before barrier: {result:?}")),
            () = started.notified() => Ok(()),
        }
    }).await.map_err(anyhow::Error::from).and_then(|result| result);
    if let Err(error) = entered {
        return Err(retain_cleanup_failure(
            root,
            &controller,
            error.context("permission collection did not enter cleanup"),
        ));
    }
    tokio::time::pause();
    tokio::time::advance(Duration::from_secs(31)).await;
    let result = operation.await;
    tokio::time::resume();
    assert!(
        matches!(
            &result,
            Err(crate::exchange::command::ExecuteCommandError::Failed(
                crate::exchange::command::CommandFailure::CollectionTimedOut
            ))
        ),
        "{result:?}"
    );
    assert_eq!(controller.observer.snapshot()?.session.access, before);
    assert!(!collected.load(Ordering::Acquire));
    assert_eq!(release.available_permits(), 0);
    release.add_permits(1);
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    let finished = tokio::time::timeout(Duration::from_secs(5), controller.finish_exchange_owner())
        .await
        .map_err(anyhow::Error::from)
        .and_then(|result| result);
    if let Err(error) = finished {
        return Err(retain_cleanup_failure(
            root,
            &controller,
            error.context("owner did not finish after cleanup release"),
        ));
    }
    assert!(collected.load(Ordering::Acquire));
    provider_fixture.provider_handle().shutdown().await?;
    controller.finish_session_events().await?;
    reader.abort();
    Ok(())
}

/// Conversation rollback collects a live selected exchange before publishing removed history.
#[tokio::test]
async fn owner_conversation_rollback_collects_selected_live_work() -> Result<()> {
    use crate::exchange::{
        command::{Command, CommandId, CommandRequest, InputControl},
        rollback::{PreparedRollbackToken, RollbackCommand, RollbackSelection},
    };
    let fixture = owner_transition_fixture().await?;
    let file = fixture.root.path().join("rollback-must-preserve.txt");
    std::fs::write(&file, "unchanged")?;
    let rollback =
        fixture
            .controller
            .broker
            .lock()
            .await
            .capture_rollback(&RollbackCommand::Apply(
                PreparedRollbackToken::Conversation {
                    selection: RollbackSelection::new(
                        fixture.original.clone(),
                        fixture.original.clone(),
                    ),
                },
            ))?;
    let request = CommandRequest {
        id: CommandId::new("owner-conversation-rollback")?,
        command: Command::Rollback(rollback),
        input_control: InputControl::AtExecution,
    };
    let mut result = Box::pin(
        fixture
            .controller
            .exchange_owner
            .execute_identified(request, fixture.sink.clone()),
    );
    assert!(futures_util::poll!(result.as_mut()).is_pending());
    tokio::time::timeout(Duration::from_secs(10), result).await??;
    assert!(fixture.engine.active_invocation_snapshot()?.is_none());
    assert_eq!(
        SqliteStore::read_only(&fixture.controller.data_root)?
            .load_exchange(&fixture.original)?
            .unwrap()
            .disposition(),
        crate::exchange::HistoryDisposition::RolledBack
    );
    assert_eq!(std::fs::read_to_string(file)?, "unchanged");
    if let Err(error) = fixture.service.shutdown(Duration::from_secs(5)).await {
        return Err(retain_owner_transition_failure(
            fixture,
            error.context("rollback session shutdown failed"),
        ));
    }
    fixture.reader.abort();
    Ok(())
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
        .owner(crate::provider::ExecutionOwner::root(&sibling_record.id))
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
    let result = tokio::time::timeout(Duration::from_secs(5), async {
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
    })
    .await
    .map_err(anyhow::Error::from)
    .and_then(|result| result);
    if let Err(error) = result {
        return Err(retain_owner_transition_failure(
            fixture,
            error.context("shared sibling did not survive owner close"),
        ));
    }
    if let Err(error) = fixture.service.shutdown(Duration::from_secs(5)).await {
        return Err(retain_owner_transition_failure(
            fixture,
            error.context("shared runtime shutdown failed"),
        ));
    }
    fixture.reader.abort();
    Ok(())
}
/// Saturated presentation output cannot delay owner shutdown or discard its retained prompt response.
#[tokio::test]
async fn owner_shutdown_collects_while_prompt_ui_output_is_saturated() -> Result<()> {
    let fixture = owner_transition_fixture().await?;
    let (output, mut receiver) = forge_protocol::outbound::channel();
    for sequence in 0..forge_protocol::MAX_PENDING_FRAMES - forge_protocol::RESERVED_CONTROL_RECORDS
    {
        output.send(sequence)?;
    }
    for sequence in 0..forge_protocol::RESERVED_CONTROL_RECORDS {
        output.send_control(sequence)?;
    }
    let attempting_output = Arc::new(tokio::sync::Notify::new());
    let observed = attempting_output.clone();
    let output = output.observe_responses(move |response| {
        if response.id == 9001 {
            observed.notify_one();
        }
        Ok(())
    });
    let controller = fixture.controller.clone();
    let prompt_output = output.clone();
    let prompt = tokio::spawn(async move {
        prompt::apply(&controller, Request { id: 9001, method: "session.prompt.submit".into(),
            params: json!({"operation_id":"saturated-prompt","input_id":"saturated-prompt-input","text":"Retain this accepted input","queued":true}) }, &prompt_output).await
    });
    tokio::time::timeout(Duration::from_secs(5), attempting_output.notified())
        .await
        .context("prompt never reached its saturated UI output boundary")?;
    assert!(
        !output.is_closed(),
        "fixture must retain a live backpressured observer"
    );
    assert!(fixture.engine.active_invocation_snapshot()?.is_some());
    let shutdown = tokio::time::timeout(
        Duration::from_secs(5),
        fixture.service.shutdown(Duration::from_secs(5)),
    )
    .await
    .context("UI backpressure prevented lifecycle shutdown collection")
    .and_then(|result| result);
    if let Err(error) = shutdown {
        return Err(retain_owner_transition_failure(fixture, error));
    }
    assert!(fixture.engine.active_invocation_snapshot()?.is_none());
    tokio::time::timeout(Duration::from_secs(1), prompt)
        .await
        .context(
            "presentation observer retained the service activity after input acceptance",
        )???;
    let response = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            let frame = receiver
                .recv()
                .await?
                .context("retained UI output closed before prompt response")?;
            let value: Value = serde_json::from_slice(frame.bytes())?;
            if value.get("id").and_then(Value::as_u64) == Some(9001) {
                return Ok::<_, anyhow::Error>(value);
            }
        }
    })
    .await
    .context("retained UI output was lost during shutdown")??;
    assert_eq!(response["result"]["accepted"], true);
    assert_eq!(response["result"]["input_id"], "saturated-prompt-input");
    fixture.reader.abort();
    Ok(())
}

/// Real owner traces must show FIFO starts on distinct updates, after preceding durable completion.
#[tokio::test]
async fn owner_starts_one_fifo_command_per_update_after_committed_completion() -> Result<()> {
    use crate::exchange::command::{
        CancelExchangeRequest, Command, CommandId, CommandOutcome, CommandRequest,
        ConfigurationChange, ConfigurationOutcome, InputControl, PermissionChange,
    };
    use crate::exchange::journal::CommandState;
    let fixture = owner_transition_fixture().await?;
    fixture.controller.trace.configure(true)?;
    let ids = [
        "step-budget-cancel",
        "step-budget-checks-on",
        "step-budget-checks-off",
    ];
    let scenario = async {
        // Hold asynchronous collection while all three caller intents reach durable FIFO admission.
        let collection_barrier = fixture.controller.coordinator.control.clone().acquire_owned().await?;
        let request = |id: &str, command| -> Result<CommandRequest> {
            Ok(CommandRequest { id: CommandId::new(id)?, command, input_control: InputControl::AtExecution })
        };
        let mut cancelled = Box::pin(fixture.controller.execute_command(
            request(ids[0], Command::CancelExchange(CancelExchangeRequest::default()))?, &fixture.sink));
        assert!(futures_util::poll!(cancelled.as_mut()).is_pending());
        let mut enabled = Box::pin(fixture.controller.execute_command(
            request(ids[1], Command::ConfigureSession(ConfigurationChange::Permission(
                PermissionChange::SetCheckEditing { enabled: true })))?, &fixture.sink));
        assert!(futures_util::poll!(enabled.as_mut()).is_pending());
        let mut disabled = Box::pin(fixture.controller.execute_command(
            request(ids[2], Command::ConfigureSession(ConfigurationChange::Permission(
                PermissionChange::SetCheckEditing { enabled: false })))?, &fixture.sink));
        assert!(futures_util::poll!(disabled.as_mut()).is_pending());
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let first = fixture.controller.exchange_owner.command_status(CommandId::new(ids[0])?).await?;
                let second = fixture.controller.exchange_owner.command_status(CommandId::new(ids[1])?).await?;
                let third = fixture.controller.exchange_owner.command_status(CommandId::new(ids[2])?).await?;
                if matches!(first.as_ref().map(|record| &record.state), Some(CommandState::Active { .. }))
                    && matches!(second.as_ref().map(|record| &record.state), Some(CommandState::Queued { .. }))
                    && matches!(third.as_ref().map(|record| &record.state), Some(CommandState::Queued { .. })) {
                    break Ok::<_, anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        }).await.context("owner did not retain asynchronous collection ahead of queued local commands")??;
        drop(collection_barrier);
        let (cancelled, enabled, disabled) = tokio::time::timeout(Duration::from_secs(10), async {
            tokio::join!(cancelled, enabled, disabled)
        }).await.context("owner did not finish queued commands after collection was released")?;
        let (cancelled, enabled, disabled) = (cancelled?, enabled?, disabled?);
        ensure!(cancelled.sequence < enabled.sequence && enabled.sequence < disabled.sequence, "completion receipts lost FIFO sequence");
        ensure!(matches!(cancelled.outcome, CommandOutcome::Cancelled(_)), "collection did not finish cancellation");
        ensure!(matches!(enabled.outcome, CommandOutcome::Configured(ConfigurationOutcome::CheckEditing { enabled: true })), "first local command lost its own result");
        ensure!(matches!(disabled.outcome, CommandOutcome::Configured(ConfigurationOutcome::CheckEditing { enabled: false })), "second local command lost its own result");
        ensure!(!fixture.controller.observer.snapshot()?.check_editing, "final configuration did not follow FIFO order");
        for id in ids {
            let record = fixture.controller.exchange_owner.command_status(CommandId::new(id)?).await?.context("completed command disappeared")?;
            ensure!(matches!(record.state, CommandState::Finished { outcome: Ok(_), .. }), "caller completion was not durable");
        }

        // Read the emitted owner contract only after the real trace writer has drained.
        let trace = fixture.controller.trace.clone();
        let path = trace.session_status(&fixture.session).path;
        let records = tokio::task::spawn_blocking(move || -> Result<Vec<Value>> {
            trace.flush()?;
            std::fs::read_to_string(path)?.lines().map(|line| serde_json::from_str(line).map_err(Into::into)).collect()
        }).await??;
        let mut starts = Vec::new();
        let mut completions = Vec::new();
        for (position, record) in records.iter().enumerate() {
            let Some(id) = record["payload"]["command_id"].as_str() else { continue; };
            if !ids.contains(&id) { continue; }
            match record["event"].as_str() {
                Some("exchange.command.started") => starts.push((id, position,
                    record["payload"]["update_step"].as_u64().context("start trace omitted its update step")?)),
                Some("exchange.command.completed") => completions.push((id, position,
                    record["payload"]["update_step"].as_u64().context("completion trace omitted its update step")?)),
                _ => {}
            }
        }
        ensure!(starts.len() == ids.len() && completions.len() == ids.len(), "each admitted command must start and finish exactly once: starts={starts:?}, completions={completions:?}");
        ensure!(starts.iter().map(|start| start.0).collect::<Vec<_>>() == ids, "trace starts were not FIFO: {starts:?}");
        ensure!(completions.iter().map(|completion| completion.0).collect::<Vec<_>>() == ids, "trace completions were not FIFO: {completions:?}");
        for index in 0..ids.len() {
            ensure!(starts[index].1 < completions[index].1, "completion preceded its own start");
            if index + 1 < ids.len() {
                ensure!(starts[index].2 < starts[index + 1].2, "two commands started in one owner update: {starts:?}");
                ensure!(completions[index].1 < starts[index + 1].1, "successor started before preceding completion committed: starts={starts:?}, completions={completions:?}");
            }
        }
        ensure!(starts[0].2 < completions[0].2, "held collection did not span owner updates");
        Ok::<_, anyhow::Error>(())
    }.await;
    if let Err(error) = scenario {
        return Err(retain_owner_transition_failure(fixture, error));
    }
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    Ok(())
}
