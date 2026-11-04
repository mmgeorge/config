use super::*;
use crate::provider::submission::{SubmissionState, SubmissionStore};
use crate::provider::{PromptMode, ProviderError, ProviderInput, ServiceTier};
use crate::session::ApprovalMode;

#[derive(Clone, Copy)]
enum BackgroundFinish {
    Shutdown,
    IdentityLookup,
    ForeignOwner,
    PriorTask,
    NoisyTimeout,
}

#[derive(Clone)]
struct BackgroundObservation {
    scope: crate::provider::InvocationScope,
    address: crate::provider::ToolAddress,
}

fn background_provider(
    finish: BackgroundFinish,
    output_count: usize,
    shutdown_called: Arc<std::sync::atomic::AtomicBool>,
) -> Arc<crate::provider::test_support::EventFixtureProvider> {
    let pending = Arc::new(std::sync::Mutex::new(None::<BackgroundObservation>));
    let prompt_pending = pending.clone();
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
    )
    .expect("mock fixture descriptor");
    let provider = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, mut cancelled| {
            let pending = prompt_pending.clone();
            async move {
                let scope = request.scope.clone();
                let turn = crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "fixture-thread".into(),
                    turn_id: "fixture-turn".into(),
                };
                let address = crate::provider::ToolAddress {
                    turn: turn.clone(),
                    tool_id: "background-tool".into(),
                };
                let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
                    scope.clone(),
                    forge_provider_api::lifecycle::ExecutionUpdate::Started {
                        address: turn.clone(),
                    },
                )?;
                sink.send_wait(ProviderEvent::Execution { observation })
                    .await?;
                for update in [
                    crate::provider::ToolUpdate::Requested {
                        invocation: crate::provider::ToolInvocation {
                            semantics: Default::default(),
                            kind: crate::provider::ToolActivityKind::Command,
                            name: "exec".into(),
                            title: "Background command".into(),
                            arguments: serde_json::json!({}),
                        },
                    },
                    crate::provider::ToolUpdate::Started,
                    crate::provider::ToolUpdate::Output {
                        update: crate::provider::TextUpdate::Append("BEGIN\n".into()),
                    },
                    crate::provider::ToolUpdate::Backgrounded,
                ] {
                    sink.send_wait(ProviderEvent::Tool {
                        address: address.clone(),
                        update,
                    })
                    .await?;
                }
                *pending.lock().unwrap() = Some(BackgroundObservation {
                    scope: scope.clone(),
                    address: address.clone(),
                });
                if matches!(finish, BackgroundFinish::NoisyTimeout) {
                    loop {
                        tokio::select! {
                            changed = cancelled.changed() => {
                                if changed.is_err() || *cancelled.borrow() {
                                    sink.send_wait(ProviderEvent::Execution {
                                        observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                                            scope.clone(),
                                            forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                                                address: turn.clone(), covered_submissions: vec![request.submission_id.clone()],
                                                completed_at_ms: 1,
                                                result: forge_provider_api::lifecycle::ExecutionResult::Cancelled {
                                                    completion: forge_provider_api::completion::ProviderCompletion::default().into(),
                                                },
                                            },
                                        )?,
                                    }).await?;
                                    return Ok(());
                                }
                            }
                            _ = tokio::time::sleep(std::time::Duration::from_millis(20)) => {
                                sink.send_wait(ProviderEvent::Tool {
                                    address: address.clone(),
                                    update: crate::provider::ToolUpdate::Output {
                                        update: crate::provider::TextUpdate::Append("still running\n".into()),
                                    },
                                }).await?;
                            }
                        }
                    }
                }
                Ok(())
            }
        },
    );
    let active_provider = Arc::downgrade(&provider);
    let active_pending = pending.clone();
    provider.with_active_session_id(move |owner| {
        let provider = active_provider
            .upgrade()
            .expect("fixture remains alive during session lookup");
        let pending = active_pending.clone();
        async move {
            if matches!(finish, BackgroundFinish::IdentityLookup) {
                publish_background_terminal(
                    &provider,
                    &pending,
                    finish,
                    output_count,
                    crate::provider::ToolOutcome::Succeeded,
                )
                .await?;
            }
            if owner.session_id.as_str().is_empty() {
                return Err(fixture_provider_error(
                    forge_provider_api::ProviderFailureKind::Rejected,
                ));
            }
            Ok(Some(
                "fixture-session"
                    .to_owned()
                    .try_into()
                    .expect("fixture session ID is nonempty"),
            ))
        }
    });
    let shutdown_provider = Arc::downgrade(&provider);
    let shutdown_pending = pending.clone();
    provider.with_shutdown(move || {
        let provider = shutdown_provider
            .upgrade()
            .expect("fixture remains alive during shutdown");
        let pending = shutdown_pending.clone();
        let shutdown_called = shutdown_called.clone();
        async move {
            shutdown_called.store(true, std::sync::atomic::Ordering::Release);
            if matches!(
                finish,
                BackgroundFinish::Shutdown | BackgroundFinish::PriorTask
            ) {
                publish_background_terminal(
                    &provider,
                    &pending,
                    finish,
                    output_count,
                    crate::provider::ToolOutcome::Interrupted,
                )
                .await?;
            }
            if matches!(finish, BackgroundFinish::ForeignOwner) {
                reject_foreign_background_scope(&provider, &pending).await?;
            }
            Ok(())
        }
    });
    provider
}

async fn publish_background_terminal(
    provider: &crate::provider::test_support::EventFixtureProvider,
    pending: &std::sync::Mutex<Option<BackgroundObservation>>,
    finish: BackgroundFinish,
    output_count: usize,
    outcome: crate::provider::ToolOutcome,
) -> std::result::Result<(), ProviderError> {
    let Some(mut observed) = pending.lock().unwrap().take() else {
        return Ok(());
    };
    if matches!(finish, BackgroundFinish::PriorTask) {
        observed.scope.exchange_id = "prior-child-task".into();
        observed.address.turn.scope = observed.scope.clone();
    }
    let sink = provider.invocation_sink(&observed.scope).await?;
    for sequence in 0..output_count {
        sink.send_wait(ProviderEvent::Tool {
            address: observed.address.clone(),
            update: crate::provider::ToolUpdate::Output {
                update: crate::provider::TextUpdate::Append(format!("END-{sequence}\n")),
            },
        })
        .await?;
    }
    sink.send_wait(ProviderEvent::Tool {
        address: observed.address,
        update: crate::provider::ToolUpdate::Finished { outcome },
    })
    .await?;
    Ok(())
}

async fn reject_foreign_background_scope(
    provider: &crate::provider::test_support::EventFixtureProvider,
    pending: &std::sync::Mutex<Option<BackgroundObservation>>,
) -> std::result::Result<(), ProviderError> {
    let Some(observed) = pending.lock().unwrap().take() else {
        return Ok(());
    };
    let session_sink = provider.event_sink(&observed.scope.owner).await?;
    let mut foreign = observed.scope;
    foreign.owner.agent_id = "foreign-child".into();
    session_sink.bind_scope(forge_provider_api::EventScope::from(foreign))?;
    Err(fixture_provider_error(
        forge_provider_api::ProviderFailureKind::Protocol,
    ))
}

fn fixture_provider_error(kind: forge_provider_api::ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::NativeTransport,
        kind,
    ))
}
async fn collect_background_child(
    finish: BackgroundFinish,
    output_count: usize,
) -> Result<Vec<ProviderEvent>> {
    let fixture = tempfile::tempdir()?;
    let trace = Arc::new(crate::trace::TraceStore::open(fixture.path())?);
    let store = SubmissionStore::open(fixture.path(), trace.clone())?;
    let coordinator = crate::agent::AgentCoordinator::new("session".into());
    let (_cancel, cancellation) = watch::channel(false);
    let owner = ExecutionOwner {
        session_id: "session".into(),
        agent_id: "child".into(),
    };
    let shutdown_called = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let native = background_provider(finish, output_count, shutdown_called.clone());
    let test_provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let provider = test_provider.provider_handle();
    let runtime = AgentRuntime {
        execution_registry: Arc::new(
            crate::provider::session_execution::SessionExecutionRegistry::new(
                provider.clone(),
                trace.clone(),
            ),
        ),
        provider: provider.clone(),
        tool_timeout: if matches!(finish, BackgroundFinish::NoisyTimeout) {
            crate::session::InputTimeout {
                amount: 1,
                unit: crate::session::TimeoutUnit::S,
            }
        } else {
            Default::default()
        },
        trace,
        submission_store: store,
        permission: crate::provider::approval::PermissionCoordinator::transient(
            fixture.path().to_str().unwrap(),
        )?,
        request: ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: Some(owner.clone()),
            harness_session_id: "session".into(),
            workspace: fixture.path().to_string_lossy().into_owned(),
            input: ProviderInput::from_text("Start one background command"),
            mode: PromptMode::Chat,
            model: "fixture".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        },
        task_id: "child-task".into(),
        cancellation,
        sender: coordinator.sender(),
        lsp_lease: None,
    };
    let running = tokio::spawn(runtime.run());
    let events = tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let mut events = Vec::new();
        loop {
            match coordinator.try_receive() {
                Some(AgentCommand::Event {
                    owner: event_owner,
                    task_id,
                    event,
                }) => {
                    assert_eq!(event_owner, owner);
                    assert_eq!(task_id, "child-task");
                    assert_eq!(event.scope().owner(), &owner);
                    if let Some(invocation) = event.scope().invocation() {
                        assert_eq!(invocation.exchange_id.as_str(), "child-task");
                    }
                    events.push(*event);
                }
                Some(AgentCommand::Completed {
                    owner: completed_owner,
                    task_id,
                    output,
                    cleanup_error,
                }) => {
                    assert_eq!(completed_owner, owner);
                    assert_eq!(task_id, "child-task");
                    if matches!(
                        finish,
                        BackgroundFinish::ForeignOwner | BackgroundFinish::PriorTask
                    ) {
                        assert!(
                            output.is_err() || cleanup_error.is_some(),
                            "invalid session scope was not reported"
                        );
                    } else if matches!(finish, BackgroundFinish::NoisyTimeout) {
                        let error = output.unwrap_err();
                        assert!(
                            error.to_string().contains("timeout"),
                            "child failure: {error:#}"
                        );
                        assert!(cleanup_error.is_none(), "{cleanup_error:?}");
                    } else {
                        assert!(output.is_ok());
                        assert!(cleanup_error.is_none(), "{cleanup_error:?}");
                    }
                    return Ok::<_, anyhow::Error>(events);
                }
                Some(AgentCommand::Request { .. }) => {
                    anyhow::bail!("fixture unexpectedly issued a control request")
                }
                None => tokio::task::yield_now().await,
            }
        }
    })
    .await
    .context("child cleanup blocked while observation queues filled")??;
    running.await??;
    assert!(
        shutdown_called.load(std::sync::atomic::Ordering::Acquire),
        "scope failure skipped provider cleanup"
    );
    assert!(
        coordinator.try_receive().is_none(),
        "event arrived after child completion"
    );
    Ok(events)
}

fn assert_background_completion(
    events: &[ProviderEvent],
    outcome: crate::provider::ToolOutcome,
    output_count: usize,
) {
    let mut text = String::new();
    let mut terminal_count = 0;
    for event in events {
        if let ProviderEvent::Tool { address, update } = event {
            assert_eq!(address.tool_id.as_str(), "background-tool");
            assert_eq!(address.turn.scope.exchange_id.as_str(), "child-task");
            match update {
                crate::provider::ToolUpdate::Output {
                    update: crate::provider::TextUpdate::Append(part),
                } => {
                    assert_eq!(terminal_count, 0, "output followed terminal settlement");
                    text.push_str(part);
                }
                crate::provider::ToolUpdate::Finished { outcome: received } => {
                    terminal_count += 1;
                    assert_eq!(received, &outcome);
                }
                crate::provider::ToolUpdate::Output {
                    update: crate::provider::TextUpdate::Replace(part),
                } => {
                    assert_eq!(terminal_count, 0, "output followed terminal settlement");
                    text = part.clone();
                }
                crate::provider::ToolUpdate::Requested { .. }
                | crate::provider::ToolUpdate::TitleChanged { .. }
                | crate::provider::ToolUpdate::Input { .. }
                | crate::provider::ToolUpdate::Started
                | crate::provider::ToolUpdate::Backgrounded
                | crate::provider::ToolUpdate::Changes { .. }
                | crate::provider::ToolUpdate::SkillRead { .. } => {}
            }
        }
    }
    assert_eq!(
        terminal_count, 1,
        "final background state did not precede Completed"
    );
    let expected = format!(
        "BEGIN\n{}",
        (0..output_count)
            .map(|sequence| format!("END-{sequence}\n"))
            .collect::<String>()
    );
    assert_eq!(text, expected);
}

/// Continuous shell output cannot defer a child deadline or prevent cleanup.
#[tokio::test]
async fn noisy_child_tool_timeout_stops_execution_and_cleans_up() -> Result<()> {
    let events = collect_background_child(BackgroundFinish::NoisyTimeout, 0).await?;
    assert!(
        events
            .iter()
            .any(|event| matches!(event, ProviderEvent::Tool {
        update: crate::provider::ToolUpdate::Finished {
            outcome: crate::provider::ToolOutcome::Failed { diagnostic },
        }, ..
    } if diagnostic.kind == forge_provider_api::ProviderFailureKind::Timeout))
    );
    Ok(())
}

/// Final native cleanup evidence must reach the child owner before managed completion.
#[tokio::test]
async fn background_shutdown_settlement_precedes_child_completion() -> Result<()> {
    let events = collect_background_child(BackgroundFinish::Shutdown, 1).await?;
    assert_background_completion(&events, crate::provider::ToolOutcome::Interrupted, 1);
    Ok(())
}

/// A natural completion during identity lookup remains successful through later shutdown.
#[tokio::test]
async fn natural_background_completion_during_identity_lookup_is_preserved() -> Result<()> {
    let events = collect_background_child(BackgroundFinish::IdentityLookup, 1).await?;
    assert_background_completion(&events, crate::provider::ToolOutcome::Succeeded, 1);
    Ok(())
}

/// Drain more than the existing provider queue capacity while cleanup is still publishing.
#[tokio::test]
async fn background_shutdown_drains_full_provider_queue_before_child_completion() -> Result<()> {
    let output_count = forge_protocol::MAX_PENDING_FRAMES + 1;
    let events = collect_background_child(BackgroundFinish::Shutdown, output_count).await?;
    assert_background_completion(
        &events,
        crate::provider::ToolOutcome::Interrupted,
        output_count,
    );
    Ok(())
}

/// Reject a foreign-owner normalized event scope before child forwarding, then collect cleanup.
#[tokio::test]
async fn foreign_owner_event_scope_fails_child_without_forwarding_or_skipping_cleanup() -> Result<()>
{
    let events = collect_background_child(BackgroundFinish::ForeignOwner, 1).await?;
    assert!(
        !events.iter().any(|event| matches!(
            event,
            ProviderEvent::Tool {
                update: crate::provider::ToolUpdate::Finished { .. },
                ..
            }
        )),
        "foreign terminal event reached the child coordinator"
    );
    Ok(())
}

/// Same-owner observations from a prior task must not be relabelled as the current task.
#[tokio::test]
async fn prior_task_session_event_fails_child_without_forwarding_or_skipping_cleanup() -> Result<()>
{
    let events = collect_background_child(BackgroundFinish::PriorTask, 1).await?;
    assert!(
        !events.iter().any(|event| matches!(
            event,
            ProviderEvent::Tool {
                update: crate::provider::ToolUpdate::Finished { .. },
                ..
            }
        )),
        "prior-task terminal event reached the current child coordinator"
    );
    Ok(())
}

#[derive(Clone, Copy)]
enum SubmissionBoundary {
    Prepared,
    Sending,
    Accepted,
}

/// Provider cancellation excludes only unsent child input while preserving uncertain or accepted delivery.
#[tokio::test]
async fn independent_provider_cancellation_preserves_child_submission_boundaries() -> Result<()> {
    for boundary in [
        SubmissionBoundary::Prepared,
        SubmissionBoundary::Sending,
        SubmissionBoundary::Accepted,
    ] {
        let fixture = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(fixture.path())?);
        let store = SubmissionStore::open(fixture.path(), trace.clone())?;
        let coordinator = crate::agent::AgentCoordinator::new("session".into());
        let (_cancel, cancellation) = watch::channel(false);
        let mut request = ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: Some(ExecutionOwner {
                session_id: "session".into(),
                agent_id: "child".into(),
            }),
            harness_session_id: "session".into(),
            workspace: fixture.path().to_string_lossy().into_owned(),
            input: ProviderInput::from_text("child request"),
            mode: PromptMode::Chat,
            model: "fixture".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let mut descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
        )
        .expect("mock fixture descriptor");
        descriptor.capability.integration.tracked_submission = true;
        descriptor.capability.integration.local_acceptance = false;
        let native =
            crate::provider::test_support::EventFixtureProvider::new(descriptor, |_, _, _| async {
                Err(ProviderError::Cancelled)
            });
        if matches!(boundary, SubmissionBoundary::Prepared) {
            native.with_pre_dispatch(|_| async { Err(ProviderError::Cancelled) });
        }
        if matches!(boundary, SubmissionBoundary::Sending) {
            native.with_admission(|_| Err(ProviderError::DeliveryUncertain));
        }
        let test_provider =
            crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let provider = test_provider.provider_handle();
        let submission = store.prepare(&request, provider.descriptor().kind)?;
        request.submission = Some(submission.clone());
        AgentRuntime {
            execution_registry: Arc::new(
                crate::provider::session_execution::SessionExecutionRegistry::new(
                    provider.clone(),
                    trace.clone(),
                ),
            ),
            provider,
            tool_timeout: Default::default(),
            trace,
            submission_store: store,
            permission: crate::provider::approval::PermissionCoordinator::transient(
                &request.workspace,
            )?,
            request,
            task_id: "child-task".into(),
            cancellation,
            sender: coordinator.sender(),
            lsp_lease: None,
        }
        .run()
        .await?;
        let state = submission.record()?.delivery;
        match boundary {
            SubmissionBoundary::Prepared => assert_eq!(state, SubmissionState::Cancelled),
            SubmissionBoundary::Sending => {
                assert!(matches!(state, SubmissionState::Sending { .. }))
            }
            SubmissionBoundary::Accepted => {
                assert!(matches!(state, SubmissionState::Accepted { .. }))
            }
        }
        let command = coordinator
            .try_receive()
            .context("child completion was not published")?;
        let AgentCommand::Completed {
            output,
            cleanup_error,
            ..
        } = command
        else {
            anyhow::bail!("unexpected child command")
        };
        let error = (*output).unwrap_err();
        assert!(
            error.chain().any(|cause| matches!(
                cause.downcast_ref::<ProviderError>(),
                Some(
                    ProviderError::Observation(_)
                        | ProviderError::DeliveryUncertain
                        | ProviderError::Cancelled
                )
            )),
            "an uncertified native cancellation must remain uncertain: {error:#}"
        );
        assert!(cleanup_error.is_none());
    }
    Ok(())
}
