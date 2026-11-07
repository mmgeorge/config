use super::*;
use crate::provider::submission::{SubmissionState, SubmissionStore};
use crate::provider::{PromptMode, ProviderError, ProviderInput, ServiceTier};
use crate::session::ApprovalMode;

#[derive(Clone, Copy)]
enum BackgroundFinish {
    Shutdown,
    LateIdentityObservation,
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
        move |request, sink, start, mut cancelled| {
            let pending = prompt_pending.clone();
            async move {
                let scope = request.scope.clone();
                let turn = crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: request.thread_id.clone(),
                    turn_id: request.turn_id.clone(),
                };
                let address = crate::provider::ToolAddress {
                    turn,
                    item_id: forge_provider_api::ItemId::new("background-tool").unwrap(),
                };
                start.observed(1).await?;
                use forge_provider_api::message::{ItemEvent, ItemStart};
                use forge_provider_api::tool::ToolItemEvent;
                let descriptor = forge_provider_api::tool::ToolDescriptor {
                    semantics: Default::default(),
                    name: "exec".into(),
                    title: "Background command".into(),
                    arguments: forge_provider_api::tool::InspectableArguments::new(
                        serde_json::json!({}),
                    ),
                };
                for event in [
                    ItemEvent::Started(ItemStart::CommandExecution(descriptor)),
                    ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted,
                    },
                    ItemEvent::Tool {
                        event: ToolItemEvent::Output(crate::provider::TextUpdate::Append(
                            "BEGIN\n".into(),
                        )),
                    },
                    ItemEvent::Tool {
                        event: ToolItemEvent::Backgrounded,
                    },
                ] {
                    sink.send_wait(tool_activity(&address, event)).await?;
                }
                *pending.lock().unwrap() = Some(BackgroundObservation {
                    scope,
                    address: address.clone(),
                });
                if matches!(finish, BackgroundFinish::NoisyTimeout) {
                    loop {
                        tokio::select! {
                            changed = cancelled.changed() => {
                                if changed.is_err() || *cancelled.borrow() {
                                    return Ok(crate::provider::test_support::native_result(&request, forge_provider_api::turn::TurnOutcome::Interrupted));
                                }
                            }
                            _ = tokio::time::sleep(std::time::Duration::from_millis(20)) => {
                                sink.send_wait(tool_activity(&address, ItemEvent::Tool { event: ToolItemEvent::Output(crate::provider::TextUpdate::Append("still running\n".into())) })).await?;
                            }
                        }
                    }
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
    let active_provider = Arc::downgrade(&provider);
    let active_pending = pending.clone();
    provider.with_session_identity(move |owner| {
        let provider = active_provider
            .upgrade()
            .expect("fixture remains alive during automatic identity publication");
        let pending = active_pending.clone();
        async move {
            if matches!(finish, BackgroundFinish::LateIdentityObservation) {
                publish_background_terminal(
                    &provider,
                    &pending,
                    finish,
                    output_count,
                    crate::provider::ToolOutcome::Succeeded,
                )
                .await?;
            }
            if owner.session_id().as_str().is_empty() {
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
    let close_provider = Arc::downgrade(&provider);
    let close_pending = pending.clone();
    provider.with_session_close(move |_| {
        let provider = close_provider
            .upgrade()
            .expect("fixture remains alive through full binding collection");
        let pending = close_pending.clone();
        async move {
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
    provider.with_shutdown(move || {
        let shutdown_called = shutdown_called.clone();
        async move {
            shutdown_called.store(true, std::sync::atomic::Ordering::Release);
            Ok(())
        }
    });
    provider
}

/// Create canonical original-owner activity for the background fixture.
fn tool_activity(
    address: &crate::provider::ToolAddress,
    event: forge_provider_api::message::ItemEvent,
) -> forge_provider_api::AdapterActivity {
    forge_provider_api::AdapterActivity::Item {
        thread_id: address.turn.thread_id.clone(),
        turn_id: address.turn.turn_id.clone(),
        item_id: forge_provider_api::ItemId::new(address.item_id.as_str()).unwrap(),
        event,
    }
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
        sink.send_wait(tool_activity(
            &observed.address,
            forge_provider_api::message::ItemEvent::Tool {
                event: forge_provider_api::tool::ToolItemEvent::Output(
                    crate::provider::TextUpdate::Append(format!("END-{sequence}\n")),
                ),
            },
        ))
        .await?;
    }
    sink.send_wait(tool_activity(
        &observed.address,
        forge_provider_api::message::ItemEvent::Completed(
            forge_provider_api::message::ItemCompletion::CommandExecution(
                forge_provider_api::tool::ToolCompletion {
                    outcome,
                    descriptor: None,
                    output: None,
                    changes: Default::default(),
                },
            ),
        ),
    ))
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
    foreign.owner = ThreadOwner::new(
        foreign.owner.session_id().clone(),
        forge_provider_api::AgentId::new("foreign-child").unwrap(),
    )
    .expect("fixture identities are nonempty");
    session_sink.invocation_writer(&foreign)?;
    Err(fixture_provider_error(
        forge_provider_api::ProviderFailureKind::Protocol,
    ))
}

fn fixture_provider_error(kind: forge_provider_api::ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::TurnStart,
        kind,
    ))
}
async fn collect_background_child(
    finish: BackgroundFinish,
    output_count: usize,
) -> Result<Vec<OwnerObservation>> {
    let fixture = tempfile::tempdir()?;
    let trace = Arc::new(crate::trace::TraceStore::open(fixture.path())?);
    let store = SubmissionStore::open(fixture.path(), trace.clone())?;
    let coordinator = crate::agent::AgentCoordinator::new("session".into());
    let (_cancel, cancellation) = watch::channel(false);
    let owner = ThreadOwner::new(
        "session".into(),
        forge_provider_api::AgentId::new("child").unwrap(),
    )
    .expect("registered session and agent identities are nonempty");
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
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
        },
        task_id: "child-task".into(),
        cancellation,
        sender: coordinator.sender(),
        lsp_lease: None,
    };
    // Waiting on the send revision instead of yielding lets a paused clock auto-advance
    // while the child runtime idles, so deadline fixtures expire without real waits.
    let mut revision = coordinator.subscribe();
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
                    ..
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
                        assert!(output.is_ok(), "child foreground failed: {output:?}");
                        assert!(cleanup_error.is_none(), "{cleanup_error:?}");
                    }
                    return Ok::<_, anyhow::Error>(events);
                }
                Some(AgentCommand::Request { .. }) => {
                    anyhow::bail!("fixture unexpectedly issued a control request")
                }
                None => revision
                    .changed()
                    .await
                    .context("agent coordinator closed before child completion")?,
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
    events: &[OwnerObservation],
    outcome: crate::provider::ToolOutcome,
    output_count: usize,
) {
    let mut text = String::new();
    let mut terminal_count = 0;
    for observation in events {
        if let OwnerObservation::Event(HarnessEvent::Native {
            scope,
            event: boxed_native_event,
            ..
        }) = observation
            && let forge_provider_api::ProviderEvent::Item { item_id, event, .. } =
                boxed_native_event.as_ref()
        {
            assert_eq!(item_id.as_str(), "background-tool");
            assert_eq!(
                scope.invocation().unwrap().exchange_id.as_str(),
                "child-task"
            );
            use forge_provider_api::message::{ItemCompletion, ItemEvent};
            match event {
                ItemEvent::Tool {
                    event:
                        forge_provider_api::tool::ToolItemEvent::Output(
                            crate::provider::TextUpdate::Append(part),
                        ),
                } => {
                    assert_eq!(terminal_count, 0, "output followed terminal settlement");
                    text.push_str(part);
                }
                ItemEvent::Tool {
                    event:
                        forge_provider_api::tool::ToolItemEvent::Output(
                            crate::provider::TextUpdate::Replace(part),
                        ),
                } => {
                    assert_eq!(terminal_count, 0, "output followed terminal settlement");
                    text = part.clone();
                }
                ItemEvent::Completed(ItemCompletion::CommandExecution(completion)) => {
                    terminal_count += 1;
                    assert_eq!(completion.outcome, outcome);
                }
                ItemEvent::Started(_)
                | ItemEvent::Message { .. }
                | ItemEvent::Tool { .. }
                | ItemEvent::Completed(_) => {}
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
///
/// The paused clock advances through the fixture's output ticks to the tool deadline, so
/// the timeout expires in virtual time rather than after a real second.
#[tokio::test(start_paused = true)]
async fn noisy_child_tool_timeout_stops_execution_and_cleans_up() -> Result<()> {
    let events = collect_background_child(BackgroundFinish::NoisyTimeout, 0).await?;
    assert!(events.iter().any(|event| matches!(event, OwnerObservation::Event(HarnessEvent::Host {
        event: crate::provider::HostEvent::Failure { failure, .. }, .. }) if failure.kind() == forge_provider_api::ProviderFailureKind::Timeout)));
    Ok(())
}

/// Final native cleanup evidence must reach the child owner before managed completion.
#[tokio::test]
async fn background_shutdown_settlement_precedes_child_completion() -> Result<()> {
    let events = collect_background_child(BackgroundFinish::Shutdown, 1).await?;
    assert_background_completion(&events, crate::provider::ToolOutcome::Interrupted, 1);
    Ok(())
}

/// Natural tool completion during automatic identity publication survives later binding collection.
#[tokio::test]
async fn natural_background_completion_during_identity_lookup_is_preserved() -> Result<()> {
    let events = collect_background_child(BackgroundFinish::LateIdentityObservation, 1).await?;
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
           OwnerObservation::Event(HarnessEvent::Native {
               event: boxed_native_event,
               ..
           })
        if matches!(boxed_native_event.as_ref(), forge_provider_api::ProviderEvent::Item {
                   event: forge_provider_api::message::ItemEvent::Completed(_),
                   ..
               }))),
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
           OwnerObservation::Event(HarnessEvent::Native {
               event: boxed_native_event,
               ..
           })
        if matches!(boxed_native_event.as_ref(), forge_provider_api::ProviderEvent::Item {
                   event: forge_provider_api::message::ItemEvent::Completed(_),
                   ..
               }))),
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
            execution_owner: Some(
                ThreadOwner::new(
                    "session".into(),
                    forge_provider_api::AgentId::new("child").unwrap(),
                )
                .expect("registered session and agent identities are nonempty"),
            ),
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
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
        };
        let mut descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
        )
        .expect("mock fixture descriptor");
        descriptor.capability.integration.tracked_submission = true;
        descriptor.capability.integration.local_acceptance = false;
        let dispatched = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let handler_dispatches = dispatched.clone();
        let native = crate::provider::test_support::EventFixtureProvider::new(
            descriptor,
            move |_request, _sink, _start, _| {
                handler_dispatches.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
                async move { Err(ProviderError::Cancelled) }
            },
        );
        let collected = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let close_collected = collected.clone();
        native.with_session_close(move |_| {
            let collected = close_collected.clone();
            async move {
                collected.store(true, std::sync::atomic::Ordering::Release);
                Ok(())
            }
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
                assert!(
                    matches!(state, SubmissionState::Sending { target: Some(_) }),
                    "uncertain input lost its retained recovery receipt"
                )
            }
            SubmissionBoundary::Accepted => {
                assert!(matches!(state, SubmissionState::Accepted { .. }))
            }
        }
        let expected_owner = ThreadOwner::new(
            "session".into(),
            forge_provider_api::AgentId::new("child").unwrap(),
        )
        .expect("registered session and agent identities are nonempty");
        let (output, cleanup_error) = loop {
            match coordinator
                .try_receive()
                .context("child completion was not published")?
            {
                AgentCommand::Event {
                    owner,
                    task_id,
                    event,
                } => {
                    assert_eq!(owner, expected_owner);
                    assert_eq!(task_id, "child-task");
                    assert_eq!(event.scope().owner(), &expected_owner);
                    if let Some(invocation) = event.scope().invocation() {
                        assert_eq!(invocation.exchange_id.as_str(), "child-task");
                    }
                }
                AgentCommand::Completed {
                    owner,
                    task_id,
                    output,
                    cleanup_error,
                    ..
                } => {
                    assert_eq!(owner, expected_owner);
                    assert_eq!(task_id, "child-task");
                    break (output, cleanup_error);
                }
                AgentCommand::Request { .. } => {
                    anyhow::bail!("cancelled child unexpectedly requested host control")
                }
            }
        };
        assert!(
            coordinator.try_receive().is_none(),
            "observation followed child completion"
        );
        let error = (*output).unwrap_err();
        if matches!(boundary, SubmissionBoundary::Accepted) {
            assert!(
                error.is::<crate::broker::TurnCancelled>(),
                "certified child cancellation lost its terminal proof: {error:#}"
            );
        } else {
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
        }
        assert_eq!(
            dispatched.load(std::sync::atomic::Ordering::Acquire),
            usize::from(matches!(boundary, SubmissionBoundary::Accepted)),
            "child cancellation dispatched another native input"
        );
        if matches!(boundary, SubmissionBoundary::Sending) {
            assert!(
                error.chain().any(|cause| matches!(
                    cause.downcast_ref::<ProviderError>(),
                    Some(
                        ProviderError::DeliveryUncertain
                            | ProviderError::Observation(
                                forge_provider_api::ObservationFailure::DeliveryUncertain
                            )
                    )
                )),
                "resource collection replaced the primary input uncertainty: {error:#}"
            );
        }
        assert!(
            collected.load(std::sync::atomic::Ordering::Acquire),
            "child completed before positive full-binding collection"
        );
        assert!(cleanup_error.is_none(), "{cleanup_error:?}");
    }
    Ok(())
}

/// Persist actual child mutation facts separately when the canonical native outcome is failure.
#[tokio::test]
async fn committed_child_file_change_survives_native_error() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let path = directory.path().join("changed.rs");
    std::fs::write(&path, "old")?;
    let context = crate::tools::ControlTurnContext::inactive(PromptMode::Chat);
    let slot = context.provider_completion.clone();
    let retained = slot.clone();
    let changed_path = path.clone();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().unwrap(),
        )?,
        move |request, _writer, start, _cancel| {
            let slot = retained.clone();
            let path = changed_path.clone();
            async move {
                start.observed(1).await?;
                std::fs::write(&path, "new").map_err(crate::provider::test_support::io_failure)?;
                let mut completion = ProviderCompletion::default();
                completion
                    .host_file_changes
                    .push(crate::tools::lsp::HostFileChange {
                        scope: request.scope.clone(),
                        call_id: forge_provider_api::control::ToolCallId::new("committed-call")
                            .unwrap(),
                        native_item_id: None,
                        changes: forge_provider_api::ProviderChangeSet {
                            file: vec![forge_provider_api::ProviderFileChange {
                                path: path.to_string_lossy().into_owned(),
                                move_path: None,
                                kind: forge_provider_api::ProviderChangeKind::Update,
                                diff: "@@ -1 +1 @@\n-old\n+new\n".into(),
                            }],
                        },
                    });
                slot.publish(completion)?;
                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Failed {
                        error: forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Rejected,
                        ),
                        retry: forge_provider_api::turn::RetryDecision::NotAllowed,
                    },
                ))
            }
        },
    );
    let session = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let provider = session.provider_handle();
    let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
    let coordinator = crate::agent::AgentCoordinator::new("session".into());
    let (_cancel, cancellation) = watch::channel(false);
    let owner = ThreadOwner::new("session".into(), forge_provider_api::AgentId::new("child")?)?;
    AgentRuntime {
        execution_registry: Arc::new(
            crate::provider::session_execution::SessionExecutionRegistry::new(
                provider.clone(),
                trace.clone(),
            ),
        ),
        provider,
        tool_timeout: Default::default(),
        trace: trace.clone(),
        submission_store: SubmissionStore::open(directory.path(), trace)?,
        permission: crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
        request: ProviderRequest {
            dispatch: Default::default(),
            execution_owner: Some(owner.clone()),
            harness_session_id: "session".into(),
            workspace: directory.path().to_string_lossy().into_owned(),
            input: ProviderInput::from_text("change"),
            mode: PromptMode::Chat,
            model: "fixture".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: Some(context),
        },
        task_id: "child-task".into(),
        cancellation,
        sender: coordinator.sender(),
        lsp_lease: None,
    }
    .run()
    .await?;
    let (output, changes) = loop {
        match coordinator
            .try_receive()
            .context("child completion missing")?
        {
            AgentCommand::Completed {
                output,
                host_file_changes,
                ..
            } => break (output, host_file_changes),
            AgentCommand::Event { .. } => {}
            AgentCommand::Request { .. } => anyhow::bail!("fixture requested unrelated work"),
        }
    };
    assert!(output.is_err());
    assert_eq!(changes.len(), 1);
    assert_eq!(changes[0].scope.owner, owner);
    assert_eq!(changes[0].scope.exchange_id.as_str(), "child-task");
    assert_eq!(changes[0].call_id.as_str(), "committed-call");
    assert!(changes[0].native_item_id.is_none());
    assert_eq!(std::fs::read_to_string(path)?, "new");
    assert!(slot.preview()?.batches.is_empty());
    assert_eq!(slot.collected_file_changes(), changes);
    Ok(())
}
