#[derive(Clone, Copy)]
enum ChildBackgroundCompletion {
    Shutdown,
    Natural,
}

struct ChildBackgroundProvider {
    fixture_events: std::sync::Arc<crate::provider::test_support::SessionEvents>,
    completion: ChildBackgroundCompletion,
    observer: std::sync::Mutex<Option<ProviderEventSink>>,
    final_event: std::sync::Mutex<Vec<ProviderEvent>>,
}

impl ChildBackgroundProvider {
    /// Publish the retained native tool address through the session observation channel.
    async fn publish_final(&self) -> Result<()> {
        let events = std::mem::take(&mut *self.final_event.lock().unwrap());
        for event in events {
            let sink = self
                .observer
                .lock()
                .unwrap()
                .clone()
                .context("missing child observer")?;
            let scope = event
                .scope()
                .invocation()
                .context("missing original invocation")?
                .clone();
            sink.invocation_sink(&scope)?.send_wait(event).await?;
        }
        Ok(())
    }
}

#[async_trait::async_trait]
impl crate::provider::ProviderAdapter for ChildBackgroundProvider {
    type Host = crate::provider::HarnessProviderHost;
    /// Select the registered test provider without starting an external process.
    fn descriptor(&self) -> crate::provider::ProviderDescriptor {
        crate::provider::ProviderDescriptor {
            kind: crate::provider::ProviderKind::Mock,
            label: "child background fixture".into(),
            capability: crate::provider::mock_capability(),
        }
    }

    /// Finish the background tool before closing the observer consumed by child shutdown.
    async fn shutdown(&self) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            self.publish_final().await?;
            self.observer.lock().unwrap().take();
            self.fixture_events.shutdown().await?;
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::Shutdown,
                source,
            )
        })
    }

    /// Keep provider cancellation independent of the test's natural prompt completion.
    async fn cancel_session(
        &self,
        runtime_key: &str,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            self.fixture_events.cancel_runtime(runtime_key).await?;
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CancelSession,
                source,
            )
        })
    }

    /// Leave cleanup to the observer-producing shutdown implementation.
    async fn cleanup_execution(
        &self,
        runtime_key: &str,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            self.fixture_events.cancel_runtime(runtime_key).await?;
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CleanupExecution,
                source,
            )
        })
    }

    /// Collect this owner's invocation tasks while preserving final session observations.
    async fn fence_session(
        &self,
        owner: &crate::provider::ExecutionOwner,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            self.fixture_events.cancel(owner).await?;
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::FenceSession,
                source,
            )
        })
    }

    /// Report no foreground execution after prompt completion.
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }

    /// Preserve the native conversation used by the original tool address.
    async fn active_session_id_for(
        &self,
        _: &str,
    ) -> anyhow::Result<Option<forge_provider_api::ProviderSessionId>> {
        Ok(Some("child-background-thread".to_owned().try_into()?))
    }

    /// Bind session observations to the actual admitted managed child owner.
    /// Admit a real background tool with partial output, then finish only its foreground turn.
    async fn session_events(
        &self,
        owner: crate::provider::ExecutionOwner,
    ) -> std::result::Result<
        crate::provider::events::ProviderEventStream,
        crate::provider::ProviderError,
    > {
        let stream = self.fixture_events.subscribe(owner.clone())?;
        *self.observer.lock().unwrap() = Some(self.fixture_events.session_sink(&owner)?);
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
                crate::provider::test_support::TerminalBoundary::LocalFixture,
                move |request, sink| async move { self.run_native(request, sink).await },
            )
            .await
    }
}

impl ChildBackgroundProvider {
    async fn run_native(
        &self,
        _: ProviderRequest,
        sink: ProviderEventSink,
    ) -> Result<crate::provider::ProviderCompletion, crate::provider::ProviderError> {
        let result: Result<_> = async {
            // Capture the original invocation before either event stream publishes completion.
            let scope = sink.invocation_scope()?.clone();
            let turn = crate::provider::test_support::address(
                &scope,
                "child-background-thread",
                "original-turn",
            );
            let address = crate::provider::ToolAddress {
                turn: turn.clone(),
                tool_id: "original-background-tool".into(),
            };
            let outcome = match self.completion {
                ChildBackgroundCompletion::Shutdown => crate::provider::ToolOutcome::Interrupted,
                ChildBackgroundCompletion::Natural => crate::provider::ToolOutcome::Succeeded,
            };
            *self.final_event.lock().unwrap() = vec![
                ProviderEvent::Tool {
                    address: address.clone(),
                    update: crate::provider::ToolUpdate::Output {
                        update: crate::provider::TextUpdate::Append("shutdown-tail\n".into()),
                    },
                },
                ProviderEvent::Tool {
                    address: address.clone(),
                    update: crate::provider::ToolUpdate::Finished { outcome },
                },
            ];
            // Admit foreground ownership before handing the tool to background observation.
            sink.send_wait(ProviderEvent::Execution {
                observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                    scope.clone(),
                    forge_provider_api::lifecycle::ExecutionUpdate::Started {
                        address: turn.clone(),
                    },
                )?,
            })
            .await?;
            sink.send_wait(ProviderEvent::Turn {
                address: turn.clone(),
                update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
            })
            .await?;
            for update in [
                crate::provider::ToolUpdate::Requested {
                    invocation: crate::provider::ToolInvocation {
                        semantics: Default::default(),
                        kind: crate::provider::ToolActivityKind::Command,
                        name: "background-command".into(),
                        title: "Original child background command".into(),
                        arguments: json!({"command":"fixture"}),
                    },
                },
                crate::provider::ToolUpdate::Started,
                crate::provider::ToolUpdate::Output {
                    update: crate::provider::TextUpdate::Append("initial-output\n".into()),
                },
                crate::provider::ToolUpdate::Backgrounded,
            ] {
                sink.send_wait(ProviderEvent::Tool {
                    address: address.clone(),
                    update,
                })
                .await?;
            }
            sink.send_wait(ProviderEvent::Turn {
                address: turn,
                update: crate::provider::TurnUpdate::Finished {
                    outcome: crate::turn::TurnOutcome::Completed,
                    completed_at_ms: 2,
                },
            })
            .await?;
            if matches!(self.completion, ChildBackgroundCompletion::Natural) {
                self.publish_final().await?;
            }
            Ok(crate::provider::ProviderCompletion {
                native: forge_provider_api::completion::ProviderCompletion {
                    provider_session_id: Some(
                        "child-background-thread".to_owned().try_into().unwrap(),
                    ),
                    capability: self.descriptor().capability,
                    ..Default::default()
                },
                ..Default::default()
            })
        }
        .await;
        result.map_err(crate::provider::ProviderError::from)
    }
}

/// Persist shutdown observations before the coordinator can complete the managed child.
#[tokio::test]
async fn managed_child_shutdown_persists_original_background_tool_before_completion() -> Result<()>
{
    assert_managed_child_background_completion(ChildBackgroundCompletion::Shutdown).await
}

/// Preserve a natural completion racing foreground delivery without replacing its native owner.
#[tokio::test]
async fn managed_child_natural_background_completion_preserves_original_tool() -> Result<()> {
    assert_managed_child_background_completion(ChildBackgroundCompletion::Natural).await
}

/// Drive real admission, runtime delivery, broker reduction, and durable child completion.
async fn assert_managed_child_background_completion(
    completion: ChildBackgroundCompletion,
) -> Result<()> {
    // Admit the child through the production coordinator and provider factory.
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker
        .agent_provider_factory
        .register(Arc::new(ChildBackgroundProvider {
            fixture_events: Default::default(),
            completion,
            observer: Default::default(),
            final_event: Default::default(),
        }));
    let (admitted, _) = broker
        .start_agent(json!({"profile":"worker","text":"Background fixture"}))
        .await?;
    let task_id = admitted["task"]["id"]
        .as_str()
        .context("missing admitted task")?
        .to_owned();
    let mut changed = broker.agent_coordinator.subscribe();
    let mut events = Vec::new();
    // Reduce runtime commands in production order while retaining the completion boundary.
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        loop {
            while let Some(command) = broker.agent_coordinator.try_receive() {
                match command {
                    crate::agent::AgentCommand::Event {
                        owner,
                        task_id,
                        event,
                    } => {
                        broker
                            .observe_managed_agent(owner, &task_id, *event, &mut events)
                            .await?;
                    }
                    crate::agent::AgentCommand::Completed {
                        owner,
                        task_id,
                        output,
                        cleanup_error,
                    } => {
                        // Inspect durable evidence before applying the child terminal transition.
                        let exchange = broker
                            .store
                            .load_exchange(&task_id)?
                            .context("missing child exchange")?;
                        let (turn, tool) = exchange
                            .tools()
                            .next()
                            .with_context(|| format!("background tool was not persisted; child_failed={}, cleanup_error={cleanup_error:?}, exchange={}", output.is_err(), serde_json::to_string(&exchange).unwrap_or_default()))?;
                        let expected = match completion {
                            ChildBackgroundCompletion::Shutdown => {
                                crate::turn::ToolState::Interrupted
                            }
                            ChildBackgroundCompletion::Natural => crate::turn::ToolState::Completed,
                        };
                        assert_eq!(tool.id, "original-background-tool");
                        assert_eq!(turn.provider().scope.owner, owner);
                        assert_eq!(turn.provider().thread_id, "child-background-thread");
                        assert_eq!(tool.state(), expected);
                        let (bytes, _) = broker.store.tool_output_bytes(
                            &task_id,
                            turn.id(),
                            &tool.id,
                            0,
                            u64::MAX,
                        )?;
                        assert_eq!(bytes, b"initial-output\nshutdown-tail\n");
                        let pending_task = broker.agent_coordinator.task(&task_id).unwrap();
                        assert_eq!(pending_task.state, ExchangeState::Running);
                        assert!(pending_task.completed_at_ms.is_none());
                        broker
                            .complete_managed_agent(
                                owner,
                                task_id,
                                *output,
                                cleanup_error,
                                &mut events,
                            )
                            .await?;
                        return Ok::<_, anyhow::Error>(());
                    }
                    crate::agent::AgentCommand::Request { .. } => {
                        anyhow::bail!("unexpected child control request")
                    }
                }
            }
            changed
                .changed()
                .await
                .context("child command channel closed")?;
        }
    })
    .await
    .context("child background delivery did not finish")??;
    // Verify the terminal transaction did not move child evidence into the root history.
    let task = broker
        .agent_coordinator
        .task(&task_id)
        .context("completed task disappeared")?;
    assert_eq!(task.state, ExchangeState::Complete);
    assert!(task.error.is_none());
    assert_eq!(
        broker.store.load_exchange(&task_id)?.unwrap().state(),
        ExchangeState::Complete
    );
    assert!(
        broker
            .store
            .list_agent_exchange(&broker.session.primary_agent_id)?
            .iter()
            .all(|exchange| exchange.tools().next().is_none())
    );
    Ok(())
}
