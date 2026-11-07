use super::super::ExchangeState;
use super::test_support::{planning_question_broker, repository};
use anyhow::{Context, Result};
use serde_json::json;
use std::sync::Arc;

#[derive(Clone, Copy)]
enum ChildBackgroundCompletion {
    Shutdown,
    Natural,
}

/// Retain command output beyond foreground collection until the actual background task ends.
fn child_background_fixture(
    completion: ChildBackgroundCompletion,
) -> Arc<crate::provider::test_support::EventFixtureProvider> {
    type Background = (
        forge_provider_api::BindingId,
        tokio::sync::watch::Sender<bool>,
        tokio::task::JoinHandle<Result<(), forge_provider_api::ProviderError>>,
    );
    let work: Arc<tokio::sync::Mutex<Vec<Background>>> = Arc::default();
    let cleanup = work.clone();
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture provider identity"),
        label: "Normalized child background fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, writer, start, _cancelled| {
            let work = work.clone();
            async move {
                use forge_provider_api::{
                    AdapterActivity, InspectableArguments, ItemCompletion, ItemEvent, ItemStart,
                    TextUpdate, ToolCompletion, ToolDescriptor, ToolItemEvent, ToolOutcome,
                };
                start.observed(1).await?;
                let thread_id = request.thread_id.clone();
                let turn_id = request.turn_id.clone();
                let item_id = forge_provider_api::ItemId::new("original-background-tool").unwrap();
                let publish = |event| AdapterActivity::Item {
                    thread_id: thread_id.clone(),
                    turn_id: turn_id.clone(),
                    item_id: item_id.clone(),
                    event,
                };
                writer
                    .send_wait(AdapterActivity::Thread {
                        thread_id: thread_id.clone(),
                        event: forge_provider_api::ThreadEvent::IdentityResolved(
                            "child-background-thread".try_into().unwrap(),
                        ),
                    })
                    .await?;
                for event in [
                    ItemEvent::Started(ItemStart::CommandExecution(ToolDescriptor {
                        name: "background-command".into(),
                        title: "Original child background command".into(),
                        semantics: Default::default(),
                        arguments: InspectableArguments::new(json!({"command":"fixture"})),
                    })),
                    ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted,
                    },
                    ItemEvent::Tool {
                        event: ToolItemEvent::Output(TextUpdate::Append("initial-output\n".into())),
                    },
                    ItemEvent::Tool {
                        event: ToolItemEvent::Backgrounded,
                    },
                ] {
                    writer.send_wait(publish(event)).await?;
                }
                let (stop, mut stopping) = tokio::sync::watch::channel(false);
                let binding = writer.binding_id().clone();
                let task = tokio::spawn(async move {
                    if matches!(completion, ChildBackgroundCompletion::Shutdown) {
                        while !*stopping.borrow_and_update() {
                            if stopping.changed().await.is_err() {
                                break;
                            }
                        }
                    }
                    let outcome = match completion {
                        ChildBackgroundCompletion::Shutdown => ToolOutcome::Interrupted,
                        ChildBackgroundCompletion::Natural => ToolOutcome::Succeeded,
                    };
                    for event in [
                        ItemEvent::Tool {
                            event: ToolItemEvent::Output(TextUpdate::Append(
                                "shutdown-tail\n".into(),
                            )),
                        },
                        ItemEvent::Completed(ItemCompletion::CommandExecution(ToolCompletion {
                            outcome,
                            descriptor: None,
                            output: None,
                            changes: Default::default(),
                        })),
                    ] {
                        writer
                            .send_wait(AdapterActivity::Item {
                                thread_id: thread_id.clone(),
                                turn_id: turn_id.clone(),
                                item_id: item_id.clone(),
                                event,
                            })
                            .await?;
                    }
                    Ok(())
                });
                work.lock().await.push((binding, stop, task));
                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                ))
            }
        },
    )
    .with_session_close(move |binding| {
        let work = cleanup.clone();
        async move {
            let targets = {
                let mut work = work.lock().await;
                let mut targets = Vec::new();
                let mut index = 0;
                while index < work.len() {
                    if work[index].0 == binding {
                        targets.push(work.remove(index));
                    } else {
                        index += 1;
                    }
                }
                targets
            };
            for (_, stop, task) in targets {
                stop.send_replace(true);
                task.await.map_err(|_| {
                    forge_provider_api::ProviderError::Observation(
                        forge_provider_api::ObservationFailure::LifecycleUnavailable,
                    )
                })??;
            }
            Ok(())
        }
    })
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
    let native = child_background_fixture(completion);
    let fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    broker
        .agent_provider_factory
        .register(fixture.provider_handle());
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
                        host_file_changes,
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
                        assert_eq!(turn.provider().thread_id, turn.provider().scope.owner.thread_id());
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
                            .complete_managed_agent(owner, task_id, *output, host_file_changes, cleanup_error, &mut events)
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
