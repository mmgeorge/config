use super::super::{ExchangeState, ProviderEvent};
use super::test_support::{planning_question_broker, repository};
use anyhow::{Context, Result};
use serde_json::json;
use std::sync::Arc;

#[derive(Clone, Copy)]
enum ChildBackgroundCompletion {
    Shutdown,
    Natural,
}

fn child_background_fixture(
    completion: ChildBackgroundCompletion,
) -> Arc<crate::provider::test_support::EventFixtureProvider> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: crate::provider::ProviderId::new("test-child-background")
            .expect("fixture ID is valid"),
        label: "Normalized child background fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, mut cancelled| async move {
            let scope = request.scope.clone();
            let thread_id = "child-background-thread";
            let turn = crate::provider::TurnAddress {
                scope: scope.clone(),
                thread_id: thread_id.into(),
                turn_id: "original-turn".into(),
            };
            let tool = crate::provider::ToolAddress {
                turn: turn.clone(),
                tool_id: "original-background-tool".into(),
            };
            let native_completion = forge_provider_api::completion::ProviderCompletion {
                provider_session_id: Some(
                    thread_id
                        .to_owned()
                        .try_into()
                        .expect("fixture ID is valid"),
                ),
                capability: crate::provider::test_support::fixture_capability(),
                ..Default::default()
            };
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
                    address: tool.clone(),
                    update,
                })
                .await?;
            }
            sink.send_wait(ProviderEvent::Turn {
                address: turn.clone(),
                update: crate::provider::TurnUpdate::Finished {
                    outcome: crate::turn::TurnOutcome::Completed,
                    completed_at_ms: 2,
                },
            })
            .await?;
            sink.send_wait(ProviderEvent::Execution {
                observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                    scope.clone(),
                    forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                        address: turn.clone(),
                        covered_submissions: vec![request.submission_id],
                        completed_at_ms: 2,
                        result: forge_provider_api::lifecycle::ExecutionResult::Completed {
                            completion: native_completion.into(),
                        },
                    },
                )?,
            })
            .await?;
            if matches!(completion, ChildBackgroundCompletion::Shutdown) {
                loop {
                    if cancelled.changed().await.is_err() || *cancelled.borrow() {
                        break;
                    }
                }
            }
            let outcome = match completion {
                ChildBackgroundCompletion::Shutdown => crate::provider::ToolOutcome::Interrupted,
                ChildBackgroundCompletion::Natural => crate::provider::ToolOutcome::Succeeded,
            };
            for update in [
                crate::provider::ToolUpdate::Output {
                    update: crate::provider::TextUpdate::Append("shutdown-tail\n".into()),
                },
                crate::provider::ToolUpdate::Finished { outcome },
            ] {
                sink.send_wait(ProviderEvent::Tool {
                    address: tool.clone(),
                    update,
                })
                .await?;
            }
            Ok(())
        },
    )
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
