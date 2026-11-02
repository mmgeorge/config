use super::*;
use crate::provider::submission::{SubmissionState, SubmissionStore, SubmissionTarget};
use crate::provider::{
    PromptMode, ProviderDescriptor, ProviderError, ProviderEventSink, ProviderInput, ProviderKind,
    ServiceTier,
};
use crate::session::ApprovalMode;

#[derive(Clone, Copy)]
enum BackgroundFinish {
    Shutdown,
    IdentityLookup,
    ForeignOwner,
    PriorTask,
    NoisyTimeout,
}

/// Retain the actual session delivery lane until the simulated native command settles.
struct BackgroundProvider {
    fixture_events: std::sync::Arc<crate::provider::test_support::SessionEvents>,
    finish: BackgroundFinish,
    output_count: usize,
    session_sink: tokio::sync::Mutex<Option<ProviderEventSink>>,
    background: tokio::sync::Mutex<Option<(ProviderEventSink, crate::provider::ToolAddress)>>,
    shutdown_called: std::sync::atomic::AtomicBool,
}

impl BackgroundProvider {
    async fn finish_background(&self) -> Result<()> {
        let pending = self.background.lock().await.take();
        if let Some((sink, address)) = pending {
            for sequence in 0..self.output_count {
                sink.send_wait(ProviderEvent::Tool {
                    address: address.clone(),
                    update: crate::provider::ToolUpdate::Output {
                        update: crate::provider::TextUpdate::Append(format!("END-{sequence}\n")),
                    },
                })
                .await?;
            }
            sink.send_wait(ProviderEvent::Tool {
                address,
                update: crate::provider::ToolUpdate::Finished {
                    outcome: match self.finish {
                        BackgroundFinish::Shutdown
                        | BackgroundFinish::ForeignOwner
                        | BackgroundFinish::PriorTask
                        | BackgroundFinish::NoisyTimeout => {
                            crate::provider::ToolOutcome::Interrupted
                        }
                        BackgroundFinish::IdentityLookup => crate::provider::ToolOutcome::Succeeded,
                    },
                },
            })
            .await?;
        }
        Ok(())
    }
}

#[async_trait::async_trait]
impl crate::provider::ProviderAdapter for BackgroundProvider {
    type Host = crate::provider::HarnessProviderHost;
    fn descriptor(&self) -> ProviderDescriptor {
        ProviderDescriptor {
            kind: ProviderKind::Mock,
            label: "background child fixture".into(),
            capability: Default::default(),
        }
    }

    async fn session_events(
        &self,
        owner: crate::provider::ExecutionOwner,
    ) -> std::result::Result<
        crate::provider::events::ProviderEventStream,
        crate::provider::ProviderError,
    > {
        let stream = self.fixture_events.subscribe(owner.clone())?;
        *self.session_sink.lock().await = Some(self.fixture_events.session_sink(&owner)?);
        Ok(stream)
    }

    async fn session_prompt(
        self: std::sync::Arc<Self>,
        request: crate::provider::ProviderRequest,
        scope: crate::provider::InvocationScope,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let events = self.fixture_events.clone();
        events
            .submit(
                request,
                scope,
                crate::provider::TerminalBoundary::LocalFixture,
                move |request, sink| async move { self.run_native(request, sink).await },
            )
            .await
    }

    async fn active_session_id_for(
        &self,
        _: &str,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>> {
        if matches!(self.finish, BackgroundFinish::IdentityLookup) {
            self.finish_background()
                .await
                .expect("fixture session lane must remain open during identity lookup");
        }
        Ok(Some("child-thread".to_owned().try_into()?))
    }

    async fn shutdown(&self) -> Result<()> {
        self.shutdown_called
            .store(true, std::sync::atomic::Ordering::Release);
        self.finish_background().await?;
        self.session_sink.lock().await.take();
        Ok(())
    }
    async fn cleanup_execution(&self, _: &str) -> Result<()> {
        Ok(())
    }
    async fn fence_session(&self, owner: &ExecutionOwner) -> Result<()> {
        self.fixture_events.cancel(owner).await.map_err(Into::into)
    }
    async fn cancel_session(&self, _: &str) -> Result<()> {
        Ok(())
    }
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }
}

impl BackgroundProvider {
    async fn run_native(
        &self,
        _: ProviderRequest,
        sink: ProviderEventSink,
    ) -> Result<ProviderCompletion, ProviderError> {
        let scope = sink.invocation_scope()?.clone();
        let address = crate::provider::ToolAddress {
            turn: crate::provider::TurnAddress {
                scope: scope.clone(),
                thread_id: "child-thread".into(),
                turn_id: "child-turn".into(),
            },
            tool_id: "background-tool".into(),
        };
        sink.send_wait(ProviderEvent::Execution {
            observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                scope.clone(),
                forge_provider_api::lifecycle::ExecutionUpdate::Started {
                    address: address.turn.clone(),
                },
            )
            .expect("execution scope is derived from its native address"),
        })
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
        let mut observed_scope = scope;
        if matches!(self.finish, BackgroundFinish::NoisyTimeout) {
            loop {
                sink.send_wait(ProviderEvent::Tool {
                    address: address.clone(),
                    update: crate::provider::ToolUpdate::Output {
                        update: crate::provider::TextUpdate::Append("still running\n".into()),
                    },
                })
                .await?;
                tokio::time::sleep(std::time::Duration::from_millis(20)).await;
            }
        }
        match self.finish {
            BackgroundFinish::ForeignOwner => {
                observed_scope.owner.agent_id = "foreign-child".into()
            }
            BackgroundFinish::PriorTask => observed_scope.exchange_id = "prior-child-task".into(),
            BackgroundFinish::Shutdown
            | BackgroundFinish::IdentityLookup
            | BackgroundFinish::NoisyTimeout => {}
        }
        let mut observed_address = address;
        observed_address.turn.scope = observed_scope.clone();
        let observer = self
            .session_sink
            .lock()
            .await
            .as_ref()
            .context("runtime did not subscribe to child session events")?
            .invocation_sink(&observed_scope)?;
        *self.background.lock().await = Some((observer, observed_address));
        Ok(ProviderCompletion::default())
    }
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
    let provider = Arc::new(BackgroundProvider {
        fixture_events: Default::default(),
        finish,
        output_count,
        session_sink: Default::default(),
        background: Default::default(),
        shutdown_called: Default::default(),
    });
    let runtime = AgentRuntime {
        execution_registry: Arc::new(
            crate::provider::session_execution::SessionExecutionRegistry::new(provider.clone()),
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
        provider
            .shutdown_called
            .load(std::sync::atomic::Ordering::Acquire),
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
    } if diagnostic.code.as_deref() == Some("tool_timeout")))
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

/// Reject a well-formed foreign-owner stream at the runtime boundary while still cleaning up.
#[tokio::test]
async fn foreign_owner_session_event_fails_child_without_forwarding_or_skipping_cleanup()
-> Result<()> {
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

struct ProviderCancelledProvider {
    fixture_events: std::sync::Arc<crate::provider::test_support::SessionEvents>,
    boundary: SubmissionBoundary,
}

#[async_trait::async_trait]
impl crate::provider::ProviderAdapter for ProviderCancelledProvider {
    type Host = crate::provider::HarnessProviderHost;
    /// Use a tracked provider kind without launching an external provider.
    fn descriptor(&self) -> ProviderDescriptor {
        ProviderDescriptor {
            kind: ProviderKind::Codex,
            label: "cancelled child".into(),
            capability: {
                let mut capability = crate::provider::mock_capability();
                capability.integration.tracked_submission = true;
                capability.integration.local_acceptance = false;
                capability
            },
        }
    }
    /// Complete fixture cleanup without resources.
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
    /// Leave cancellation cleanup successful.
    async fn cleanup_execution(&self, _: &str) -> Result<()> {
        Ok(())
    }
    /// Leave cancellation cleanup successful.
    async fn fence_session(&self, owner: &ExecutionOwner) -> Result<()> {
        self.fixture_events.cancel(owner).await.map_err(Into::into)
    }
    async fn cancel_session(&self, _: &str) -> Result<()> {
        Ok(())
    }
    /// No native turn remains after the fixture response.
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }
    /// Do not invent a native conversation after cancellation.
    async fn active_session_id_for(
        &self,
        _: &str,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>> {
        Ok(None)
    }
    /// Transfer the fixture's owned observation stream to its execution engine.
    async fn session_events(
        &self,
        owner: crate::provider::ExecutionOwner,
    ) -> std::result::Result<
        crate::provider::events::ProviderEventStream,
        crate::provider::ProviderError,
    > {
        let stream = self.fixture_events.subscribe(owner.clone())?;
        Ok(stream)
    }

    async fn session_prompt(
        self: std::sync::Arc<Self>,
        request: crate::provider::ProviderRequest,
        scope: crate::provider::InvocationScope,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let events = self.fixture_events.clone();
        events
            .submit(
                request,
                scope,
                crate::provider::TerminalBoundary::LocalFixture,
                move |request, sink| async move { self.run_native(request, sink).await },
            )
            .await
    }
}

impl ProviderCancelledProvider {
    async fn run_native(
        &self,
        request: ProviderRequest,
        _: ProviderEventSink,
    ) -> Result<ProviderCompletion, ProviderError> {
        let submission = request.submission.as_ref().unwrap();
        match self.boundary {
            SubmissionBoundary::Prepared => {}
            SubmissionBoundary::Sending => submission
                .sending(SubmissionTarget::native(
                    "codex",
                    "thread",
                    serde_json::json!({}),
                ))
                .map_err(ProviderError::from)?,
            SubmissionBoundary::Accepted => {
                submission
                    .sending(SubmissionTarget::native(
                        "codex",
                        "thread",
                        serde_json::json!({}),
                    ))
                    .map_err(ProviderError::from)?;
                submission
                    .accepted("accepted".into())
                    .map_err(ProviderError::from)?;
            }
        }
        Err(ProviderError::Cancelled)
    }
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
        let submission = store.prepare(&request, ProviderKind::Codex)?;
        request.submission = Some(submission.clone());
        let provider: Arc<Provider> = Arc::new(ProviderCancelledProvider {
            fixture_events: Default::default(),
            boundary,
        });
        AgentRuntime {
            execution_registry: Arc::new(
                crate::provider::session_execution::SessionExecutionRegistry::new(provider.clone()),
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
                Some(ProviderError::Observation(_) | ProviderError::DeliveryUncertain { .. })
            )),
            "an uncertified native cancellation must remain uncertain: {error:#}"
        );
        assert!(cleanup_error.is_none());
    }
    Ok(())
}
