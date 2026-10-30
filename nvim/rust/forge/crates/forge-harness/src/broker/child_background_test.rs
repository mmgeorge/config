#[derive(Clone, Copy)]
enum ChildBackgroundCompletion {
    Shutdown,
    Natural,
}

struct ChildBackgroundBackend {
    completion: ChildBackgroundCompletion,
    observer: std::sync::Mutex<Option<BackendEventSink>>,
    final_event: std::sync::Mutex<Vec<BackendEvent>>,
}

impl ChildBackgroundBackend {
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
impl Backend for ChildBackgroundBackend {
    /// Select the registered test provider without starting an external process.
    fn descriptor(&self) -> crate::backend::BackendDescriptor {
        crate::backend::BackendDescriptor {
            kind: crate::backend::BackendKind::Mock,
            label: "child background fixture".into(),
            capability: crate::backend::mock_capability(),
        }
    }

    /// Finish the background tool before closing the observer consumed by child shutdown.
    async fn shutdown(&self) -> Result<()> {
        self.publish_final().await?;
        self.observer.lock().unwrap().take();
        Ok(())
    }

    /// Keep provider cancellation independent of the test's natural prompt completion.
    async fn cancel_session(&self, _: &str) -> Result<()> {
        Ok(())
    }

    /// Leave cleanup to the observer-producing shutdown implementation.
    async fn cleanup_execution(&self, _: &str) -> Result<()> {
        Ok(())
    }

    /// Report no foreground execution after prompt completion.
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }

    /// Preserve the native conversation used by the original tool address.
    async fn active_session_id_for(&self, _: &str) -> Option<String> {
        Some("child-background-thread".into())
    }

    /// Bind session observations to the actual admitted managed child owner.
    async fn session_event_stream(
        &self,
        owner: crate::backend::ExecutionOwner,
    ) -> Result<Option<crate::backend::events::BackendEventStream>> {
        let (sink, stream) =
            crate::backend::events::channel(crate::backend::EventScope::SessionOperation {
                owner,
                operation_id: "child-background-observer".into(),
            });
        *self.observer.lock().unwrap() = Some(sink);
        Ok(Some(stream))
    }

    /// Admit a real background tool with partial output, then finish only its foreground turn.
    async fn prompt_stream(
        &self,
        _: BackendRequest,
        sink: BackendEventSink,
    ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
        let result: Result<_> = async {
            // Capture the original invocation before either event stream publishes completion.
            let scope = sink.invocation_scope()?.clone();
            let turn = crate::backend::test_support::address(
                &scope,
                "child-background-thread",
                "original-turn",
            );
            let address = crate::backend::ToolAddress {
                turn: turn.clone(),
                tool_id: "original-background-tool".into(),
            };
            let outcome = match self.completion {
                ChildBackgroundCompletion::Shutdown => crate::backend::ToolOutcome::Interrupted,
                ChildBackgroundCompletion::Natural => crate::backend::ToolOutcome::Succeeded,
            };
            *self.final_event.lock().unwrap() = vec![
                BackendEvent::Tool {
                    address: address.clone(),
                    update: crate::backend::ToolUpdate::Output {
                        update: crate::backend::TextUpdate::Append("shutdown-tail\n".into()),
                    },
                },
                BackendEvent::Tool {
                    address: address.clone(),
                    update: crate::backend::ToolUpdate::Finished { outcome },
                },
            ];
            // Admit foreground ownership before handing the tool to background observation.
            sink.send_wait(BackendEvent::Turn {
                address: turn.clone(),
                update: crate::backend::TurnUpdate::Started { started_at_ms: 1 },
            })
            .await?;
            for update in [
                crate::backend::ToolUpdate::Requested {
                    invocation: crate::backend::ToolInvocation {
                        kind: crate::backend::ToolActivityKind::Command,
                        name: "background-command".into(),
                        title: "Original child background command".into(),
                        arguments: json!({"command":"fixture"}),
                    },
                },
                crate::backend::ToolUpdate::Started,
                crate::backend::ToolUpdate::Output {
                    update: crate::backend::TextUpdate::Append("initial-output\n".into()),
                },
                crate::backend::ToolUpdate::Backgrounded,
            ] {
                sink.send_wait(BackendEvent::Tool {
                    address: address.clone(),
                    update,
                })
                .await?;
            }
            sink.send_wait(BackendEvent::Turn {
                address: turn,
                update: crate::backend::TurnUpdate::Finished {
                    outcome: crate::turn::TurnOutcome::Completed,
                    completed_at_ms: 2,
                },
            })
            .await?;
            if matches!(self.completion, ChildBackgroundCompletion::Natural) {
                self.publish_final().await?;
            }
            Ok(crate::backend::BackendCompletion {
                backend_session_id: Some("child-background-thread".into()),
                capability: self.descriptor().capability,
                ..Default::default()
            })
        }
        .await;
        result.map_err(crate::backend::BackendError::from)
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
    // Admit the child through the production coordinator and backend factory.
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker
        .agent_backend_factory
        .register(Arc::new(ChildBackgroundBackend {
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
                            .context("background tool was not persisted")?;
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
