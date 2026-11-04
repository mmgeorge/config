/// Keep real owner admission and held generic provider work under deterministic test control.
struct OwnerTransitionFixture {
    root: tempfile::TempDir,
    service: HarnessService,
    controller: Arc<SessionController>,
    session: String,
    provider: Arc<crate::provider::Provider>,
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
            false,
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
    Ok(OwnerTransitionFixture {
        root,
        service,
        controller,
        session,
        provider,
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
    assert!(!fixture.provider.has_active_turn(&fixture.session).await);
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
        fixture.provider.has_active_turn(&fixture.session).await,
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
    assert!(!fixture.provider.has_active_turn(&fixture.session).await);
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
            fixture.provider.has_active_turn(&sibling_record.id).await,
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
        while fixture.provider.has_active_turn(&sibling_record.id).await
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
    assert!(fixture.provider.has_active_turn(&fixture.session).await);
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
    assert!(!fixture.provider.has_active_turn(&fixture.session).await);
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
