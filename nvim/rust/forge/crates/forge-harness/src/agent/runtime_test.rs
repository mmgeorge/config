use super::*;
use crate::backend::submission::{SubmissionState, SubmissionStore, SubmissionTarget};
use crate::backend::{
    BackendDescriptor, BackendError, BackendEventSink, BackendInput, BackendKind, PromptMode,
    ServiceTier,
};
use crate::session::ApprovalMode;

#[derive(Clone, Copy)]
enum BackgroundFinish {
    Shutdown,
    IdentityLookup,
    ForeignOwner,
    PriorTask,
}

/// Retain the actual session delivery lane until the simulated native command settles.
struct BackgroundBackend {
    finish: BackgroundFinish,
    output_count: usize,
    session_sink: tokio::sync::Mutex<Option<BackendEventSink>>,
    background: tokio::sync::Mutex<Option<(BackendEventSink, crate::backend::ToolAddress)>>,
    shutdown_called: std::sync::atomic::AtomicBool,
}

impl BackgroundBackend {
    async fn finish_background(&self) -> Result<()> {
        let pending = self.background.lock().await.take();
        if let Some((sink, address)) = pending {
            for sequence in 0..self.output_count {
                sink.send_wait(BackendEvent::Tool {
                    address: address.clone(),
                    update: crate::backend::ToolUpdate::Output {
                        update: crate::backend::TextUpdate::Append(format!("END-{sequence}\n")),
                    },
                })
                .await?;
            }
            sink.send_wait(BackendEvent::Tool {
                address,
                update: crate::backend::ToolUpdate::Finished {
                    outcome: match self.finish {
                        BackgroundFinish::Shutdown
                        | BackgroundFinish::ForeignOwner
                        | BackgroundFinish::PriorTask => crate::backend::ToolOutcome::Interrupted,
                        BackgroundFinish::IdentityLookup => crate::backend::ToolOutcome::Succeeded,
                    },
                },
            })
            .await?;
        }
        Ok(())
    }
}

#[async_trait::async_trait]
impl Backend for BackgroundBackend {
    fn descriptor(&self) -> BackendDescriptor {
        BackendDescriptor {
            kind: BackendKind::Mock,
            label: "background child fixture".into(),
            capability: Default::default(),
        }
    }

    async fn session_event_stream(
        &self,
        mut owner: ExecutionOwner,
    ) -> Result<Option<crate::backend::events::BackendEventStream>> {
        if matches!(self.finish, BackgroundFinish::ForeignOwner) {
            owner.agent_id = "foreign-child".into();
        }
        let (sink, stream) =
            crate::backend::events::channel(crate::backend::EventScope::SessionOperation {
                owner,
                operation_id: "background-observation".into(),
            });
        *self.session_sink.lock().await = Some(sink);
        Ok(Some(stream))
    }

    async fn prompt_stream(
        &self,
        _: BackendRequest,
        sink: BackendEventSink,
    ) -> Result<BackendCompletion, BackendError> {
        let scope = sink.invocation_scope()?.clone();
        let address = crate::backend::ToolAddress {
            turn: crate::backend::TurnAddress {
                scope: scope.clone(),
                thread_id: "child-thread".into(),
                turn_id: "child-turn".into(),
            },
            tool_id: "background-tool".into(),
        };
        for update in [
            crate::backend::ToolUpdate::Requested {
                invocation: crate::backend::ToolInvocation {
                    kind: crate::backend::ToolActivityKind::Command,
                    name: "exec".into(),
                    title: "Background command".into(),
                    arguments: serde_json::json!({}),
                },
            },
            crate::backend::ToolUpdate::Started,
            crate::backend::ToolUpdate::Output {
                update: crate::backend::TextUpdate::Append("BEGIN\n".into()),
            },
            crate::backend::ToolUpdate::Backgrounded,
        ] {
            sink.send_wait(BackendEvent::Tool {
                address: address.clone(),
                update,
            })
            .await?;
        }
        let mut observed_scope = scope;
        match self.finish {
            BackgroundFinish::ForeignOwner => {
                observed_scope.owner.agent_id = "foreign-child".into()
            }
            BackgroundFinish::PriorTask => observed_scope.exchange_id = "prior-child-task".into(),
            BackgroundFinish::Shutdown | BackgroundFinish::IdentityLookup => {}
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
        Ok(BackendCompletion::default())
    }

    async fn active_session_id_for(&self, _: &str) -> Option<String> {
        if matches!(self.finish, BackgroundFinish::IdentityLookup) {
            self.finish_background()
                .await
                .expect("fixture session lane must remain open during identity lookup");
        }
        Some("child-thread".into())
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
    async fn cancel_session(&self, _: &str) -> Result<()> {
        Ok(())
    }
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }
}

async fn collect_background_child(
    finish: BackgroundFinish,
    output_count: usize,
) -> Result<Vec<BackendEvent>> {
    let fixture = tempfile::tempdir()?;
    let trace = Arc::new(crate::trace::TraceStore::open(fixture.path())?);
    let store = SubmissionStore::open(fixture.path(), trace.clone())?;
    let coordinator = crate::agent::AgentCoordinator::new("session".into());
    let (_cancel, cancellation) = watch::channel(false);
    let owner = ExecutionOwner {
        session_id: "session".into(),
        agent_id: "child".into(),
    };
    let backend = Arc::new(BackgroundBackend {
        finish,
        output_count,
        session_sink: Default::default(),
        background: Default::default(),
        shutdown_called: Default::default(),
    });
    let runtime = AgentRuntime {
        backend: backend.clone(),
        trace,
        submission_store: store,
        permission: crate::backend::approval::PermissionCoordinator::transient(
            fixture.path().to_str().unwrap(),
        )?,
        request: BackendRequest {
            execution_owner: Some(owner.clone()),
            harness_session_id: "session".into(),
            workspace: fixture.path().to_string_lossy().into_owned(),
            input: BackendInput::from_text("Start one background command"),
            mode: PromptMode::Chat,
            model: "fixture".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: None,
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
        backend
            .shutdown_called
            .load(std::sync::atomic::Ordering::Acquire),
        "scope failure skipped backend cleanup"
    );
    assert!(
        coordinator.try_receive().is_none(),
        "event arrived after child completion"
    );
    Ok(events)
}

fn assert_background_completion(
    events: &[BackendEvent],
    outcome: crate::backend::ToolOutcome,
    output_count: usize,
) {
    let mut text = String::new();
    let mut terminal_count = 0;
    for event in events {
        if let BackendEvent::Tool { address, update } = event {
            assert_eq!(address.tool_id.as_str(), "background-tool");
            assert_eq!(address.turn.scope.exchange_id.as_str(), "child-task");
            match update {
                crate::backend::ToolUpdate::Output {
                    update: crate::backend::TextUpdate::Append(part),
                } => {
                    assert_eq!(terminal_count, 0, "output followed terminal settlement");
                    text.push_str(part);
                }
                crate::backend::ToolUpdate::Finished { outcome: received } => {
                    terminal_count += 1;
                    assert_eq!(received, &outcome);
                }
                crate::backend::ToolUpdate::Output {
                    update: crate::backend::TextUpdate::Replace(part),
                } => {
                    assert_eq!(terminal_count, 0, "output followed terminal settlement");
                    text = part.clone();
                }
                crate::backend::ToolUpdate::Requested { .. }
                | crate::backend::ToolUpdate::TitleChanged { .. }
                | crate::backend::ToolUpdate::Input { .. }
                | crate::backend::ToolUpdate::Started
                | crate::backend::ToolUpdate::Backgrounded
                | crate::backend::ToolUpdate::Changes { .. }
                | crate::backend::ToolUpdate::SkillRead { .. } => {}
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

/// Final native cleanup evidence must reach the child owner before managed completion.
#[tokio::test]
async fn background_shutdown_settlement_precedes_child_completion() -> Result<()> {
    let events = collect_background_child(BackgroundFinish::Shutdown, 1).await?;
    assert_background_completion(&events, crate::backend::ToolOutcome::Interrupted, 1);
    Ok(())
}

/// A natural completion during identity lookup remains successful through later shutdown.
#[tokio::test]
async fn natural_background_completion_during_identity_lookup_is_preserved() -> Result<()> {
    let events = collect_background_child(BackgroundFinish::IdentityLookup, 1).await?;
    assert_background_completion(&events, crate::backend::ToolOutcome::Succeeded, 1);
    Ok(())
}

/// Drain more than the existing provider queue capacity while cleanup is still publishing.
#[tokio::test]
async fn background_shutdown_drains_full_provider_queue_before_child_completion() -> Result<()> {
    let output_count = forge_protocol::MAX_PENDING_FRAMES + 1;
    let events = collect_background_child(BackgroundFinish::Shutdown, output_count).await?;
    assert_background_completion(
        &events,
        crate::backend::ToolOutcome::Interrupted,
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
            BackendEvent::Tool {
                update: crate::backend::ToolUpdate::Finished { .. },
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
            BackendEvent::Tool {
                update: crate::backend::ToolUpdate::Finished { .. },
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

struct ProviderCancelledBackend {
    boundary: SubmissionBoundary,
}

#[async_trait::async_trait]
impl Backend for ProviderCancelledBackend {
    /// Use a tracked provider kind without launching an external provider.
    fn descriptor(&self) -> BackendDescriptor {
        BackendDescriptor {
            kind: BackendKind::Codex,
            label: "cancelled child".into(),
            capability: crate::backend::mock_capability(),
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
    async fn cancel_session(&self, _: &str) -> Result<()> {
        Ok(())
    }
    /// No native turn remains after the fixture response.
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }
    /// Do not invent a native conversation after cancellation.
    async fn active_session_id_for(&self, _: &str) -> Option<String> {
        None
    }
    /// Cancel after the selected durable submission boundary without changing the watch channel.
    async fn prompt_stream(
        &self,
        request: BackendRequest,
        _: BackendEventSink,
    ) -> Result<BackendCompletion, BackendError> {
        let submission = request.submission.as_ref().unwrap();
        match self.boundary {
            SubmissionBoundary::Prepared => {}
            SubmissionBoundary::Sending => submission
                .sending(SubmissionTarget::Codex {
                    thread_id: "thread".into(),
                })
                .map_err(BackendError::from)?,
            SubmissionBoundary::Accepted => {
                submission
                    .sending(SubmissionTarget::Codex {
                        thread_id: "thread".into(),
                    })
                    .map_err(BackendError::from)?;
                submission
                    .accepted("accepted".into())
                    .map_err(BackendError::from)?;
            }
        }
        Err(BackendError::Cancelled)
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
        let mut request = BackendRequest {
            execution_owner: Some(ExecutionOwner {
                session_id: "session".into(),
                agent_id: "child".into(),
            }),
            harness_session_id: "session".into(),
            workspace: fixture.path().to_string_lossy().into_owned(),
            input: BackendInput::from_text("child request"),
            mode: PromptMode::Chat,
            model: "fixture".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let submission = store.prepare(&request, BackendKind::Codex)?;
        request.submission = Some(submission.clone());
        AgentRuntime {
            backend: Arc::new(ProviderCancelledBackend { boundary }),
            trace,
            submission_store: store,
            permission: crate::backend::approval::PermissionCoordinator::transient(
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
        assert!((*output).unwrap_err().is::<crate::broker::TurnCancelled>());
        assert!(cleanup_error.is_none());
    }
    Ok(())
}
