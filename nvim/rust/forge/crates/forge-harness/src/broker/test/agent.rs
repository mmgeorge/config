use super::super::{
    ApprovalMode, Exchange, ExchangeAdmission, ExchangeState, HarnessBroker, InitializeRequest,
    PromptMode, ProviderEvent, ProviderInput, ProviderLaunch,
};
use super::test_support::{
    admit_child_fixture, child_address, child_completed, child_event, completed_message,
    planning_question_broker, repository, settle_managed_children,
};
use crate::provider::ScopeOwner;
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::sync::Arc;
use std::sync::atomic::Ordering;
use uuid::Uuid;

type TestReceiptWaiters = Arc<std::sync::Mutex<Vec<(String, tokio::sync::oneshot::Sender<()>)>>>;

struct RoutingProvider {
    kind: crate::provider::ProviderId,
    turn: std::sync::atomic::AtomicUsize,
    request: Arc<std::sync::Mutex<Vec<forge_provider_api::PromptRequest>>>,
    reject_steering: bool,
}

fn admitted_request_fixture(
    broker: &HarnessBroker,
    agent: crate::agent::Agent,
) -> Result<crate::agent::AgentWork> {
    let coordinator = crate::agent::AgentCoordinator::new(agent.session_id.clone());
    let task = broker
        .store
        .list_agent_task(&agent.session_id)?
        .into_iter()
        .find(|task| task.agent_id == agent.id)
        .context("fixture agent has no task")?;
    coordinator.retain(agent.clone(), task)?;
    let operation = crate::agent::AgentRequest {
        operation_id: "request-fixture".into(),
        source: crate::agent::AgentSource::User,
        operation: crate::agent::AgentOperation::Followup {
            target: agent.id.clone(),
            message: "request fixture".into(),
        },
    };
    match coordinator
        .reserve_followup(&operation, &agent.id, "request fixture", "parent", 1)?
        .commit()?
    {
        crate::agent::AgentCommit::Admitted(work) => Ok(work),
        crate::agent::AgentCommit::Existing(_) => anyhow::bail!("fixture unexpectedly reused work"),
    }
}

fn child_message(
    address: &crate::provider::TurnAddress,
    id: impl Into<String>,
    role: crate::provider::TestMessageContent,
    text: impl Into<String>,
) -> Vec<ProviderEvent> {
    completed_message(address, id, role, text)
}

async fn query_child(
    broker: &mut HarnessBroker,
    operation: crate::agent::AgentOperation,
) -> Result<Value> {
    broker
        .handle_agent_request(
            crate::agent::AgentRequest {
                operation_id: Uuid::new_v4().to_string(),
                source: crate::agent::AgentSource::User,
                operation,
            },
            None,
            &mut Vec::new(),
        )
        .await
}

#[tokio::test]
async fn child_approval_events_reach_the_wire_and_interrupt_preserves_sibling_approval()
-> Result<()> {
    use crate::permissions::matcher::{PermissionRequest, PermissionTarget};
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let first = admit_child_fixture(&mut broker, "approval-first")?;
    let sibling = admit_child_fixture(&mut broker, "approval-sibling")?;
    let mut approval_streams = Vec::new();
    let mut waiters = Vec::new();
    let mut event = Vec::new();
    for admission in [&first, &sibling] {
        let owner = crate::provider::ThreadOwner::new(
            (broker.session.id.clone()).into(),
            forge_provider_api::agent::AgentId::new(admission.agent.id.clone()).unwrap(),
        )
        .expect("registered session and agent identities are nonempty");
        broker
            .observe_child_fixture(
                owner.clone(),
                &admission.task.id,
                child_event(
                    &child_address(&(owner.clone()), &admission.task.id),
                    crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                        started_at_ms: 0,
                    }),
                ),
                &mut event,
            )
            .await?;
        let permission = broker.permission_coordinator.clone();
        let scope = crate::provider::InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(owner.clone()),
            exchange_id: admission.task.id.clone().into(),
            invocation_id: format!("fixture:{}", admission.task.id).into(),
        };
        let (sink, mut stream) = crate::provider::events::channel(scope.clone());
        waiters.push(tokio::spawn(async move {
            permission
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "approval".into(),
                        provider: forge_provider_api::ProviderId::new("fixture")
                            .expect("fixture identity is valid"),
                        subject: forge_provider_api::permissions::PermissionSubject::Unattached,
                        reason: None,
                        target_list: vec![PermissionTarget::Tool {
                            target: "probe_task".into(),
                        }],
                    },
                    Some(crate::provider::TurnAddress {
                        thread_id: (scope).owner().thread_id(),
                        turn_id: (scope).turn_id().unwrap(),
                        scope,
                    }),
                    Some(&sink),
                )
                .await
        }));
        let requested = stream.recv().await?.context("approval event missing")?;
        approval_streams.push(stream);
        let owner = crate::provider::ThreadOwner::new(
            (broker.session.id.clone()).into(),
            forge_provider_api::agent::AgentId::new(admission.agent.id.clone()).unwrap(),
        )
        .expect("registered session and agent identities are nonempty");
        broker
            .observe_child_fixture(owner.clone(), &admission.task.id, requested, &mut event)
            .await?;
        let activity = query_child(
            &mut broker,
            crate::agent::AgentOperation::Activity {
                target: admission.agent.id.clone(),
                limit: None,
            },
        )
        .await?;
        assert_eq!(activity["awaiting_approval"], true);
        assert!(
            event
                .iter()
                .any(|event| event.payload["kind"] == "approval_requested"
                    && event.payload["data"]["scope"]["Turn"]["turn_id"]
                        == child_address(&owner, &admission.task.id).turn_id.as_str()
                    && event.payload["data"]["execution_owner"]["agent_id"] == admission.agent.id)
        );
    }
    broker
        .interrupt_managed_agent(&first.agent.id, &mut event)
        .await?;
    assert_eq!(
        waiters.remove(0).await??,
        crate::provider::approval::ApprovalResolution::Cancel
    );
    assert!(!waiters[0].is_finished());
    assert_eq!(broker.permission_coordinator.pending_list()?.len(), 1);
    let first_activity = query_child(
        &mut broker,
        crate::agent::AgentOperation::Activity {
            target: first.agent.id.clone(),
            limit: None,
        },
    )
    .await?;
    let sibling_activity = query_child(
        &mut broker,
        crate::agent::AgentOperation::Activity {
            target: sibling.agent.id.clone(),
            limit: None,
        },
    )
    .await?;
    assert_eq!(first_activity["awaiting_approval"], false);
    assert_eq!(sibling_activity["awaiting_approval"], true);
    assert!(
        event
            .iter()
            .any(|event| event.payload["kind"] == "approval_cancelled"
                && event.payload["data"]["execution_owner"]["agent_id"] == first.agent.id)
    );
    let owner = crate::provider::ThreadOwner::new(
        (broker.session.id.clone()).into(),
        forge_provider_api::agent::AgentId::new(sibling.agent.id.clone()).unwrap(),
    )
    .expect("registered session and agent identities are nonempty");
    broker
        .complete_managed_agent(
            owner,
            sibling.task.id.clone(),
            Err(anyhow::anyhow!("provider failed")),
            Vec::new(),
            None,
            &mut event,
        )
        .await?;
    assert_eq!(
        waiters.remove(0).await??,
        crate::provider::approval::ApprovalResolution::Cancel
    );
    assert!(broker.permission_coordinator.pending_list()?.is_empty());
    assert!(
        event
            .iter()
            .any(|event| event.payload["kind"] == "agent_updated"
                && event.payload["data"]["summary"][&sibling.agent.id]["state"] == "failed")
    );
    Ok(())
}

#[tokio::test]
async fn interrupted_child_retains_conversation_and_all_children_guard_workspace_rollback()
-> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let admission = admit_child_fixture(&mut broker, "retained")?;
    let owner = crate::provider::ThreadOwner::new(
        (broker.session.id.clone()).into(),
        forge_provider_api::agent::AgentId::new(admission.agent.id.clone()).unwrap(),
    )
    .expect("registered session and agent identities are nonempty");
    let mut event = Vec::new();
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_event(
                &child_address(&(owner.clone()), &admission.task.id),
                crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                    started_at_ms: 0,
                }),
            ),
            &mut event,
        )
        .await?;
    assert!(
        broker
            .prepare_rollback(json!({"exchange_id":"unrelated"}))
            .await
            .unwrap_err()
            .to_string()
            .contains("child execution")
    );
    assert!(
        broker
            .apply_rollback(json!({"preview_id":"unrelated"}))
            .await
            .unwrap_err()
            .to_string()
            .contains("child execution")
    );
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            ProviderEvent::Thread {
                thread_id: owner.thread_id(),
                event: forge_provider_api::ThreadEvent::IdentityResolved(
                    "durable-child-conversation".to_owned().try_into()?,
                ),
            },
            &mut event,
        )
        .await?;
    broker
        .interrupt_managed_agent(&admission.agent.id, &mut event)
        .await?;
    broker
        .complete_managed_agent(
            owner,
            admission.task.id.clone(),
            Err(anyhow::anyhow!("interrupted")),
            Vec::new(),
            None,
            &mut event,
        )
        .await?;
    let retained = broker.agent_coordinator.resolve(&admission.agent.id)?;
    assert_eq!(
        retained.provider_thread_id.as_deref(),
        Some("durable-child-conversation")
    );
    let request = broker.agent_provider_factory.request(
        &broker.provider_request(ProviderInput::from_user_text(""), PromptMode::Chat)?,
        &admitted_request_fixture(&broker, retained.clone())?,
        forge_provider_api::forge::AgentTask {
            task_id: "follow-up".into(),
            from: forge_provider_api::forge::ROOT_AGENT_PATH.into(),
            text: "Follow-up".into(),
            messages: Vec::new(),
        },
    )?;
    assert_eq!(
        request.provider_session_id.clone().map(String::from),
        retained.provider_thread_id
    );
    // The parent's task reaches the child as a `new_task` message rather than user text, and the
    // child learns its own path and profile from the standing `<agent>` section.
    assert!(matches!(
        request.input.turn().kind(),
        forge_provider_api::forge::TurnKind::AgentTask { task } if task.text == "Follow-up"
    ));
    assert_eq!(request.input.turn().user_input(), None);
    let section = request
        .child_agent
        .as_ref()
        .context("child request names its agent")?;
    assert_eq!(section.path, retained.task_path);
    assert_eq!(section.parent, forge_provider_api::forge::ROOT_AGENT_PATH);
    assert_eq!(
        section.profile_instructions,
        retained
            .selection
            .as_ref()
            .context("fixture child has a selection")?
            .instructions
    );
    assert_eq!(request.standing_sections().agent.as_ref(), Some(section));
    broker
        .store
        .interrupt_detached_execution(&broker.session.id)?;
    assert_eq!(
        broker
            .store
            .list_agent_run(&broker.session.id)?
            .into_iter()
            .find(|agent| agent.id == retained.id)
            .unwrap()
            .provider_thread_id,
        retained.provider_thread_id
    );
    Ok(())
}

#[tokio::test]
async fn managed_child_replays_and_late_failures_preserve_sibling_and_followup_history()
-> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut parent = Exchange::agent_admission(&broker.session.id, 1, "Admission".into(), 100);
    let selection = crate::agent::AgentSelection {
        profile: "worker".into(),
        instructions: String::new(),
        provider: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        model: "mock-model".into(),
        reasoning_effort: "low".into(),
        context_window: None,
        service_tier: Default::default(),
        access: broker.session.access.clone(),
        approval_mode: broker.session.approval_mode,
    };
    let mut admission = Vec::new();
    for name in ["first", "sibling"] {
        let spawn = crate::agent::SpawnAgentRequest {
            profile: Some(name.into()),
            text: "Inspect".into(),
        };
        let request = crate::agent::AgentRequest {
            operation_id: name.into(),
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::Spawn(spawn.clone()),
        };
        let admitted = broker.agent_coordinator.reserve_spawn(
            &request,
            &spawn,
            selection.clone(),
            &parent.id,
            100,
        )?;
        parent.append_agent_task(&admitted.task)?;
        broker.store.admit_agent_task(
            &admitted.agent,
            &admitted.task,
            &parent,
            admitted.continuation_authorization(),
        )?;
        let admitted = match admitted.commit()? {
            crate::agent::AgentCommit::Admitted(work)
            | crate::agent::AgentCommit::Existing(work) => work,
        };
        broker.agent_registry.insert(admitted.agent.clone());
        admission.push(admitted);
    }
    let mut event = Vec::new();
    for admitted in &admission {
        let owner = crate::provider::ThreadOwner::new(
            (broker.session.id.clone()).into(),
            forge_provider_api::agent::AgentId::new(admitted.agent.id.clone()).unwrap(),
        )
        .expect("registered session and agent identities are nonempty");
        let update = child_event(
            &child_address(&owner, &admitted.task.id),
            crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                started_at_ms: 0,
            }),
        );
        broker
            .observe_child_fixture(owner.clone(), &admitted.task.id, update, &mut event)
            .await?;
    }
    let first = &admission[0];
    let owner = crate::provider::ThreadOwner::new(
        (broker.session.id.clone()).into(),
        forge_provider_api::agent::AgentId::new(first.agent.id.clone()).unwrap(),
    )
    .expect("registered session and agent identities are nonempty");
    let mut update = child_message(
        &child_address(&(owner.clone()), &first.task.id),
        "thought",
        crate::provider::TestMessageContent::Commentary,
        "First child thought",
    );
    broker
        .observe_child_fixture(owner.clone(), &first.task.id, update.clone(), &mut event)
        .await?;
    broker
        .observe_child_fixture(owner.clone(), &first.task.id, update.clone(), &mut event)
        .await?;
    let retained = broker.store.load_exchange(&first.task.id)?.unwrap();
    assert_eq!(
        retained
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .count(),
        1
    );
    assert_eq!(retained.turn[0].messages()[0].text(), "First child thought");
    assert!(
        broker
            .store
            .load_exchange(&admission[1].task.id)?
            .unwrap()
            .turn[0]
            .messages()
            .is_empty()
    );
    let mut foreign = update.clone();
    if let ProviderEvent::Item { thread_id, .. } = &mut foreign[0] {
        *thread_id = crate::provider::ThreadOwner::new(
            broker.session.id.clone().into(),
            forge_provider_api::agent::AgentId::new(admission[1].agent.id.clone())?,
        )
        .unwrap()
        .thread_id();
    }
    assert!(
        broker
            .observe_child_fixture(owner.clone(), &first.task.id, foreign.remove(0), &mut event)
            .await
            .is_err()
    );
    let (_routing, routing_fixture) = routing_fixture(
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        true,
    )?;
    broker
        .child_provider_by_agent
        .insert(first.agent.id.clone(), routing_fixture.provider_handle());
    let message = broker
        .handle_agent_request(
            crate::agent::AgentRequest {
                operation_id: "steering-race".into(),
                source: crate::agent::AgentSource::User,
                operation: crate::agent::AgentOperation::Message {
                    target: first.agent.id.clone(),
                    message: "Retain after rejected steering".into(),
                },
            },
            None,
            &mut event,
        )
        .await?;
    assert_eq!(message["delivery"], "queued");
    assert_eq!(
        broker.agent_coordinator.resolve(&first.agent.id)?.inbox[0].message,
        "Retain after rejected steering"
    );
    update = child_message(
        &child_address(&(owner.clone()), &first.task.id),
        "final",
        crate::provider::TestMessageContent::Response,
        "Child final response",
    );
    broker
        .observe_child_fixture(owner.clone(), &first.task.id, update.clone(), &mut event)
        .await?;
    broker
        .complete_managed_agent(
            owner.clone(),
            first.task.id.clone(),
            Err(anyhow::anyhow!("provider failed")),
            Vec::new(),
            None,
            &mut event,
        )
        .await?;
    assert_eq!(
        broker
            .agent_coordinator
            .task(&first.task.id)
            .unwrap()
            .final_response
            .as_deref(),
        Some("Child final response")
    );
    let request = crate::agent::AgentRequest {
        operation_id: "followup-event-test".into(),
        source: crate::agent::AgentSource::User,
        operation: crate::agent::AgentOperation::Followup {
            target: first.agent.id.clone(),
            message: "Follow-up".into(),
        },
    };
    let next = broker.agent_coordinator.reserve_followup(
        &request,
        &first.agent.id,
        "Follow-up",
        &parent.id,
        101,
    )?;
    parent.append_agent_task(&next.task)?;
    broker.store.admit_agent_task(
        &next.agent,
        &next.task,
        &parent,
        next.continuation_authorization(),
    )?;
    let next = match next.commit()? {
        crate::agent::AgentCommit::Admitted(work) | crate::agent::AgentCommit::Existing(work) => {
            work
        }
    };
    update = vec![child_event(
        &child_address(&(owner.clone()), &next.task.id),
        crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
            started_at_ms: 0,
        }),
    )];
    broker
        .observe_child_fixture(owner.clone(), &next.task.id, update.clone(), &mut event)
        .await?;
    update = child_message(
        &child_address(&(owner.clone()), &next.task.id),
        "next-thought",
        crate::provider::TestMessageContent::Commentary,
        "New task thought",
    );
    broker
        .observe_child_fixture(owner.clone(), &next.task.id, update.clone(), &mut event)
        .await?;
    assert_eq!(
        broker.store.load_exchange(&next.task.id)?.unwrap().turn[0].messages()[0].text(),
        "New task thought"
    );
    let saved_next = serde_json::to_value(broker.store.load_exchange(&next.task.id)?)?;
    update = child_message(
        &child_address(&(owner.clone()), &first.task.id),
        "late-old-task",
        crate::provider::TestMessageContent::Commentary,
        "LATE OLD TASK",
    );
    broker
        .observe_child_fixture(owner.clone(), &first.task.id, update, &mut event)
        .await?;
    broker
        .complete_managed_agent(
            owner,
            first.task.id.clone(),
            Err(anyhow::anyhow!("duplicate failure")),
            Vec::new(),
            None,
            &mut event,
        )
        .await?;
    assert_eq!(
        serde_json::to_value(broker.store.load_exchange(&next.task.id)?)?,
        saved_next
    );
    assert_eq!(
        broker.child_exchange_runtime_by_agent[&first.agent.id]
            .exchange
            .id,
        next.task.id
    );
    assert_eq!(
        broker
            .agent_coordinator
            .task(&first.task.id)
            .unwrap()
            .error
            .as_deref(),
        Some("provider failed")
    );
    assert_eq!(broker.agent_coordinator.active_count(), 2);
    assert!(
        broker
            .store
            .list_agent_exchange(&broker.session.primary_agent_id)?
            .iter()
            .all(|exchange| exchange.turn.is_empty())
    );
    Ok(())
}

impl RoutingProvider {
    fn fixture(self: &Arc<Self>) -> Arc<crate::provider::test_support::EventFixtureProvider> {
        let descriptor = crate::provider::ProviderDescriptor {
            kind: self.kind.clone(),
            label: self.kind.as_str().into(),
            capability: crate::provider::test_support::fixture_capability(),
        };
        let turns = Arc::new(std::sync::atomic::AtomicUsize::new(
            self.turn.load(Ordering::Acquire),
        ));
        let captured = self.request.clone();
        let handler = crate::provider::test_support::EventFixtureProvider::new(
            descriptor.clone(),
            move |request, sink, start, _cancelled| {
                let turns = turns.clone();
                let captured = captured.clone();

                async move {
                    start.observed(0).await?;

                    captured.lock().unwrap().push(request.clone());
                    let ordinal = turns.fetch_add(1, Ordering::AcqRel);
                    let root = request.scope.owner().is_root();
                    let text = crate::provider::test_support::input_text(&request.input);
                    let response = if root {
                        if ordinal == 0 {
                            crate::provider::test_support::call_tool(
                                &request,
                                "agent_spawn",
                                json!({"profile":"routing-child","text":"CHILD_RESULT"}),
                            )
                            .await?;
                            "Waiting"
                        } else {
                            if !text.contains("CHILD_RESULT") {
                                return Err(crate::provider::ProviderError::Failure(
                                    forge_provider_api::ProviderFailure::new(
                                        forge_provider_api::ProviderOperation::TurnStart,
                                        forge_provider_api::ProviderFailureKind::Protocol,
                                    ),
                                ));
                            }
                            "SYNTHESIZED_RESULT"
                        }
                    } else {
                        if !text.ends_with("CHILD_RESULT") {
                            return Err(crate::provider::ProviderError::Failure(
                                forge_provider_api::ProviderFailure::new(
                                    forge_provider_api::ProviderOperation::TurnStart,
                                    forge_provider_api::ProviderFailureKind::Protocol,
                                ),
                            ));
                        }
                        "CHILD_RESULT"
                    };
                    let scope = request.scope.clone();
                    let address = crate::provider::TurnAddress {
                        thread_id: (scope.clone()).owner().thread_id(),
                        turn_id: (scope.clone()).turn_id().unwrap(),
                        scope: scope.clone(),
                    };

                    for event in completed_message(
                        &address,
                        "routing-response",
                        crate::provider::TestMessageContent::Response,
                        response,
                    ) {
                        sink.send_wait(crate::provider::test_support::activity(event)?)
                            .await?;
                    }
                    if let Some(history) = Some(
                        if root {
                            "root-history"
                        } else {
                            "child-history"
                        }
                        .to_owned()
                        .try_into()
                        .expect("fixture ID is valid"),
                    ) {
                        sink.send_wait(forge_provider_api::AdapterActivity::Thread {
                            thread_id: request.thread_id.clone(),
                            event: forge_provider_api::ThreadEvent::IdentityResolved(history),
                        })
                        .await?;
                    }
                    let native_outcome = crate::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    };
                    let native_completed_at_ms = 2;

                    Ok(forge_provider_api::TurnResult::new(
                        native_outcome,
                        native_completed_at_ms,
                        None,
                        request.input_coverage(),
                        Vec::new(),
                        None,
                    )
                    .expect("fixture native coverage is valid"))
                }
            },
        );
        let model = forge_provider_api::ProviderModel {
            id: "fixture-model".into(),
            label: None,
            service_tiers: vec![
                forge_provider_api::ServiceTier::Standard,
                forge_provider_api::ServiceTier::Fast,
            ],
            reasoning: vec!["low".into()],
            default_reasoning: Some("low".into()),
            selected_reasoning: None,
            context_window: vec![forge_provider_api::ProviderContextWindow {
                id: "extended".into(),
                token_limit: Some(200_000),
            }],
            default_context_window: None,
            selected_context_window: None,
            vision: false,
            description: None,
            is_default: true,
        };
        let provider = handler.with_models(vec![model]);
        let reject_steering = self.reject_steering;
        provider.with_steering(move |_request| async move {
            if reject_steering {
                Err(crate::provider::ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Rejected,
                    ),
                ))
            } else {
                Ok(())
            }
        });
        provider
    }
}

fn routing_fixture(
    kind: crate::provider::ProviderId,
    reject_steering: bool,
) -> Result<(
    Arc<RoutingProvider>,
    crate::provider::test_support::ProviderTestSession,
)> {
    let native = Arc::new(RoutingProvider {
        kind,
        turn: std::sync::atomic::AtomicUsize::new(0),
        request: Default::default(),
        reject_steering,
    });
    let fixture =
        crate::provider::test_support::ProviderTestSession::from_native(native.fixture())?;
    Ok((native, fixture))
}

#[tokio::test]
async fn durable_operation_receipts_prevent_duplicate_child_tasks_before_and_after_completion()
-> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let request = json!({"profile":"worker","text":"FIRST","operation_id":"spawn-receipt"});
    let first = broker.start_agent(request.clone()).await?.0;
    let repeated = broker.start_agent(request.clone()).await?.0;
    assert_eq!(first["agent"]["id"], repeated["agent"]["id"]);
    assert_eq!(first["task"]["id"], repeated["task"]["id"]);
    settle_managed_children(&mut broker).await?;
    let completed = broker.start_agent(request.clone()).await?.0;
    assert_eq!(first["task"]["id"], completed["task"]["id"]);
    assert_eq!(broker.store.list_agent_task(&broker.session.id)?.len(), 1);
    assert_eq!(broker.store.list_exchange(&broker.session.id)?.len(), 1);
    let mut changed = request;
    changed["text"] = json!("DIFFERENT");
    assert!(broker.start_agent(changed).await.is_err());
    let session_id = broker.session.id.clone();
    broker.provider.shutdown().await?;
    drop(broker);
    let mut recovered = HarnessBroker::initialize(InitializeRequest {
        background: None,
        provider_launch_by_provider: Default::default(),
        config_dir: None,
        data_root: data.path().to_string_lossy().into_owned(),
        permission_file: None,
        workspace: repository.path().to_string_lossy().into_owned(),
        client_id: "receipt-reopen".into(),
        provider: ProviderLaunch {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            command: vec!["mock".into()],
        },
        model: "mock-model".into(),
        effort: "low".into(),
        session_id: Some(session_id),
        new_session_name: None,
        goal_max_turns: 20,
        lease_conflict_action: None,
    })?;
    let replayed = recovered
        .start_agent(json!({"profile":"worker","text":"FIRST","operation_id":"spawn-receipt"}))
        .await?
        .0;
    assert_eq!(first["task"]["id"], replayed["task"]["id"]);
    assert_eq!(recovered.agent_coordinator.active_count(), 0);
    assert_eq!(
        recovered
            .store
            .list_agent_task(&recovered.session.id)?
            .len(),
        1
    );
    Ok(())
}

#[derive(Default)]
struct ResultReceivingProvider {
    turn: Arc<std::sync::atomic::AtomicUsize>,
    incoming: Arc<tokio::sync::Notify>,
    receipt: TestReceiptWaiters,
    message: Arc<std::sync::Mutex<Vec<String>>>,
    input: Arc<std::sync::Mutex<Vec<String>>>,
}

impl ResultReceivingProvider {
    fn fixture(self: &Arc<Self>) -> Arc<crate::provider::test_support::EventFixtureProvider> {
        let descriptor = crate::provider::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id().expect("fixture identity exists"),
            label: "Normalized child result receiver".into(),
            capability: crate::provider::test_support::fixture_capability(),
        };
        let turn = self.turn.clone();
        let incoming = self.incoming.clone();
        let receipt = self.receipt.clone();
        let message = self.message.clone();
        let input = self.input.clone();
        let provider = crate::provider::test_support::EventFixtureProvider::new(
            descriptor,
            move |request, sink, start, _cancelled| {
                let turn = turn.clone();
                let incoming = incoming.clone();
                let receipt = receipt.clone();
                let message = message.clone();
                let input = input.clone();
                async move {
                    start.observed(0).await?;

                    turn.fetch_add(1, Ordering::AcqRel);
                    let text = crate::provider::test_support::input_text(&request.input);
                    input.lock().unwrap().push(text.clone());
                    // Attached child results render before the user's words, so match anywhere.
                    if !text.contains("EXPLICIT_NEXT_TURN") {
                        for task in ["first", "second"] {
                            crate::provider::test_support::call_tool(
                                &request,
                                "agent_spawn",
                                json!({
                                    "profile":"worker", "text":format!("CHILD_REPORT_{task}"),
                                }),
                            )
                            .await?;
                        }
                        loop {
                            let notified = incoming.notified();
                            for (report, accepted) in receipt.lock().unwrap().drain(..) {
                                message.lock().unwrap().push(report);
                                let _ = accepted.send(());
                            }
                            if message.lock().unwrap().len() == 2 {
                                break;
                            }
                            notified.await;
                        }
                    }
                    let scope = request.scope.clone();
                    let address = crate::provider::TurnAddress {
                        thread_id: (scope.clone()).owner().thread_id(),
                        turn_id: (scope.clone()).turn_id().unwrap(),
                        scope: scope.clone(),
                    };

                    for event in completed_message(
                        &address,
                        "synthesized-response",
                        crate::provider::TestMessageContent::Response,
                        "SYNTHESIZED_RESULT",
                    ) {
                        sink.send_wait(crate::provider::test_support::activity(event)?)
                            .await?;
                    }
                    if let Some(history) = Some(
                        "result-parent-history"
                            .to_owned()
                            .try_into()
                            .expect("fixture ID is valid"),
                    ) {
                        sink.send_wait(forge_provider_api::AdapterActivity::Thread {
                            thread_id: request.thread_id.clone(),
                            event: forge_provider_api::ThreadEvent::IdentityResolved(history),
                        })
                        .await?;
                    }
                    let native_outcome = crate::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    };
                    let native_completed_at_ms = 1;

                    Ok(forge_provider_api::TurnResult::new(
                        native_outcome,
                        native_completed_at_ms,
                        None,
                        request.input_coverage(),
                        Vec::new(),
                        None,
                    )
                    .expect("fixture native coverage is valid"))
                }
            },
        );
        let incoming = self.incoming.clone();
        let receipt = self.receipt.clone();
        provider.with_steering(move |request| {
            let incoming = incoming.clone();
            let receipt = receipt.clone();
            async move {
                let text = crate::provider::test_support::input_text(&request.input);
                let (accepted, wait) = tokio::sync::oneshot::channel();
                receipt.lock().unwrap().push((text, accepted));
                incoming.notify_one();
                wait.await
                    .map_err(|_| crate::provider::ProviderError::DeliveryUncertain)
            }
        })
    }
}
#[tokio::test]
async fn child_results_reach_active_parent_once_without_wait() -> Result<()> {
    {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let provider = Arc::new(ResultReceivingProvider::default());
        let fixture =
            crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
        broker.provider = fixture.provider_handle();
        broker.agent_coordinator.set_enabled(true);
        broker
            .run_interaction(
                "Research".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Research".into())),
            )
            .await?;
        assert_eq!(
            provider.turn.load(Ordering::Acquire),
            1,
            "accepted delivery started a duplicate parent turn"
        );
        let message = provider.message.lock().unwrap();
        assert_eq!(message.len(), 2);
        for (task, result) in broker
            .store
            .list_agent_task(&broker.session.id)?
            .iter()
            .zip(["first", "second"])
        {
            assert!(task.parent_reported);
            assert_eq!(
                message
                    .iter()
                    .filter(|message| message.contains(&task.id))
                    .count(),
                1
            );
            assert!(
                message
                    .iter()
                    .any(|message| message.contains(&format!("CHILD_REPORT_{result}")))
            );
        }
        // Each report is a typed `final_answer` `<agent_message>` carrying only the child's identity, status, and
        // outcome, never Harness bookkeeping such as `parent_reported`.
        for message in message.iter() {
            assert!(
                message.contains("<agent_message type=\"final_answer\" from=\"/root/")
                    && message.contains(" task=\"")
                    && message.contains(" status=\""),
                "{message}"
            );
            assert!(!message.contains("parent_reported"), "{message}");
        }
        let exchanges = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchanges.len(), 1);
        assert_eq!(exchanges[0].state(), ExchangeState::Complete);
        assert!(
            !exchanges[0]
                .turn
                .iter()
                .flat_map(|turn| turn.messages())
                .any(|message| message.text().contains("CHILD_REPORT_")),
            "child result was rendered as a parent message"
        );
    }
    Ok(())
}

#[tokio::test]
async fn idle_child_result_is_retained_until_one_explicit_parent_turn() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = Arc::new(ResultReceivingProvider::default());
    let fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
    broker.provider = fixture.provider_handle();
    let admission = admit_child_fixture(&mut broker, "idle-result")?;
    let owner = crate::provider::ThreadOwner::new(
        (broker.session.id.clone()).into(),
        forge_provider_api::agent::AgentId::new(admission.agent.id.clone()).unwrap(),
    )
    .expect("registered session and agent identities are nonempty");
    let mut event = Vec::new();
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_event(
                &child_address(&(owner.clone()), &admission.task.id),
                crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                    started_at_ms: 0,
                }),
            ),
            &mut event,
        )
        .await?;
    let response = child_message(
        &child_address(&(owner.clone()), &admission.task.id),
        "result",
        crate::provider::TestMessageContent::Response,
        "RETAINED_CHILD_REPORT",
    );
    broker
        .observe_child_fixture(owner.clone(), &admission.task.id, response, &mut event)
        .await?;
    let finished = child_completed(
        &child_address(&(owner.clone()), &admission.task.id),
        crate::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        },
        100,
    );
    broker
        .observe_child_fixture(owner.clone(), &admission.task.id, finished, &mut event)
        .await?;
    broker
        .complete_managed_agent(
            owner.clone(),
            admission.task.id.clone(),
            Ok(Default::default()),
            Vec::new(),
            None,
            &mut event,
        )
        .await?;
    broker
        .complete_managed_agent(
            owner,
            admission.task.id.clone(),
            Err(anyhow::anyhow!("duplicate")),
            Vec::new(),
            None,
            &mut event,
        )
        .await?;
    assert_eq!(provider.turn.load(Ordering::Acquire), 0);
    assert!(!broker.store.list_agent_task(&broker.session.id)?[0].parent_reported);
    for _ in 0..2 {
        broker
            .run_interaction(
                "EXPLICIT_NEXT_TURN".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("EXPLICIT_NEXT_TURN".into())),
            )
            .await?;
    }
    let input = provider.input.lock().unwrap();
    assert!(input[0].contains("RETAINED_CHILD_REPORT"));
    assert_eq!(input[0].matches("RETAINED_CHILD_REPORT").count(), 1);
    assert!(!input[1].contains("RETAINED_CHILD_REPORT"));
    assert!(broker.store.list_agent_task(&broker.session.id)?[0].parent_reported);
    Ok(())
}

/// Admit one model-delegated child task under `parent`, persisting the delegation as the broker does.
fn admit_model_task(
    broker: &mut HarnessBroker,
    parent: &mut Exchange,
    operation: crate::agent::AgentOperation,
) -> Result<crate::agent::AgentWork> {
    let request = crate::agent::AgentRequest {
        operation_id: Uuid::new_v4().to_string(),
        source: crate::agent::AgentSource::Model,
        operation: operation.clone(),
    };
    let admission = match &operation {
        crate::agent::AgentOperation::Spawn(spawn) => broker.agent_coordinator.reserve_spawn(
            &request,
            spawn,
            crate::agent::AgentSelection {
                profile: "worker".into(),
                instructions: String::new(),
                provider: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                model: "mock".into(),
                reasoning_effort: "low".into(),
                context_window: None,
                service_tier: Default::default(),
                access: broker.session.access.clone(),
                approval_mode: broker.session.approval_mode,
            },
            &parent.id,
            100,
        )?,
        crate::agent::AgentOperation::Followup { target, message } => broker
            .agent_coordinator
            .reserve_followup(&request, target, message, &parent.id, 100)?,
        _ => anyhow::bail!("fixture admits only spawns and follow-ups"),
    };
    parent.append_agent_task(&admission.task)?;
    broker.store.admit_agent_task(
        &admission.agent,
        &admission.task,
        parent,
        admission.continuation_authorization(),
    )?;
    let work = match admission.commit()? {
        crate::agent::AgentCommit::Admitted(work) => work,
        crate::agent::AgentCommit::Existing(_) => anyhow::bail!("fixture reused a child admission"),
    };
    broker.agent_registry.insert(work.agent.clone());
    Ok(work)
}

/// A child stop that requires authorization settles every follow-up queued behind it, a model
/// retry is rejected without new work, and the parent's settlement and next prompt proceed.
///
/// The queued follow-up previously moved to the launch queue, where it waited forever for a
/// grant; checkpoint settlement then rejected the parent on every later prompt.
#[tokio::test]
async fn stopped_child_retry_rejects_without_blocking_parent_save() -> Result<()> {
    let stops: [fn() -> anyhow::Error; 2] = [
        || anyhow::anyhow!("provider failed"),
        || anyhow::Error::new(crate::broker::TurnCancelled),
    ];
    for (stop, expected) in stops
        .into_iter()
        .zip([ExchangeState::Failed, ExchangeState::Cancelled])
    {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let provider = Arc::new(ResultReceivingProvider::default());
        let fixture =
            crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
        broker.provider = fixture.provider_handle();
        broker.agent_coordinator.set_enabled(true);
        let mut parent = Exchange::agent_admission(
            &broker.session.id,
            broker.store.next_exchange_ordinal(&broker.session.id)?,
            "Delegating parent".into(),
            100,
        );
        let first = admit_model_task(
            &mut broker,
            &mut parent,
            crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
                profile: Some("explorer".into()),
                text: "Inspect".into(),
            }),
        )?;
        let followup = admit_model_task(
            &mut broker,
            &mut parent,
            crate::agent::AgentOperation::Followup {
                target: first.agent.id.clone(),
                message: "Inspect more".into(),
            },
        )?;
        assert!(
            !followup.launch,
            "the follow-up waits behind the running task"
        );
        let owner = crate::provider::ThreadOwner::new(
            broker.session.id.clone().into(),
            forge_provider_api::agent::AgentId::new(first.agent.id.clone())?,
        )?;
        let mut event = Vec::new();
        broker
            .complete_managed_agent(
                owner,
                first.task.id.clone(),
                Err(stop()),
                Vec::new(),
                None,
                &mut event,
            )
            .await?;

        let tasks = broker.store.list_agent_task(&broker.session.id)?;
        let stopped = tasks.iter().find(|task| task.id == first.task.id).unwrap();
        assert_eq!(stopped.state, expected);
        let cancelled = tasks
            .iter()
            .find(|task| task.id == followup.task.id)
            .unwrap();
        assert_eq!(cancelled.state, ExchangeState::Cancelled);
        assert!(cancelled.completed_at_ms.is_some());
        assert!(cancelled.parent_reported);
        assert!(
            cancelled
                .error
                .as_deref()
                .is_some_and(|error| error.contains(&first.task.id))
        );
        assert_eq!(
            broker
                .store
                .load_exchange(&followup.task.id)?
                .context("follow-up exchange retained")?
                .state(),
            ExchangeState::Cancelled
        );
        assert_eq!(broker.agent_coordinator.active_count(), 0);
        assert!(!broker.agent_coordinator.collection_count().has_work());

        let retry = broker
            .handle_agent_request(
                crate::agent::AgentRequest {
                    operation_id: "model-retry".into(),
                    source: crate::agent::AgentSource::Model,
                    operation: crate::agent::AgentOperation::Followup {
                        target: first.agent.id.clone(),
                        message: "Retry".into(),
                    },
                },
                Some(&mut parent),
                &mut event,
            )
            .await
            .err()
            .context("a model retry of a stopped child must fail")?;
        let required = retry
            .downcast_ref::<crate::agent::ContinuationRequired>()
            .context("the retry error names the stopped task")?;
        assert_eq!(required.stopped_task_id.as_str(), first.task.id);
        assert_eq!(broker.store.list_agent_task(&broker.session.id)?.len(), 2);
        assert_eq!(broker.agent_coordinator.collection_count().queued, 0);

        // The parent's delegation tree is settled, so its pending finalization commits.
        parent.phase =
            crate::exchange::ExchangePhase::Finalizing(crate::exchange::ExchangeOutcome::Complete);
        broker.store.save_exchange(&parent)?;
        let mut prior = broker
            .begin_prior_finalization()?
            .context("the parent remains finalizing")?;
        assert!(matches!(
            broker.advance_prior_finalization(&mut prior, &mut event)?,
            super::super::prior_finalization::PriorFinalizationAction::Ready
        ));
        let saved = broker
            .store
            .load_exchange(&parent.id)?
            .context("parent retained")?;
        assert!(saved.is_finished());
        assert_eq!(saved.finalization_error, None);

        broker
            .run_interaction(
                "EXPLICIT_NEXT_TURN".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("EXPLICIT_NEXT_TURN".into())),
            )
            .await?;
        assert_eq!(provider.turn.load(Ordering::Acquire), 1);
        assert!(
            provider.input.lock().unwrap()[0].contains(&first.task.id),
            "the stopped report travels with the next user input"
        );
    }
    Ok(())
}

/// Start one fixture child and settle it with a retained process cleanup obligation.
async fn child_with_cleanup_obligation(
    broker: &mut HarnessBroker,
    name: &str,
    event: &mut Vec<super::super::SessionEvent>,
) -> Result<crate::agent::AgentWork> {
    let admission = admit_child_fixture(broker, name)?;
    let owner = crate::provider::ThreadOwner::new(
        broker.session.id.clone().into(),
        forge_provider_api::agent::AgentId::new(admission.agent.id.clone())?,
    )
    .expect("registered session and agent identities are nonempty");
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_event(
                &child_address(&owner, &admission.task.id),
                crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                    started_at_ms: 0,
                }),
            ),
            event,
        )
        .await?;
    broker
        .complete_managed_agent(
            owner,
            admission.task.id.clone(),
            Err(anyhow::anyhow!("provider failed")),
            Vec::new(),
            Some("native process outlived its turn".into()),
            event,
        )
        .await?;
    assert!(
        broker
            .agent_coordinator
            .cleanup_task(&admission.agent.id)
            .is_some()
    );
    Ok(admission)
}

#[tokio::test]
async fn interrupt_all_retries_every_child_cleanup_after_one_fails() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut event = Vec::new();
    child_with_cleanup_obligation(&mut broker, "cleanup-first", &mut event).await?;
    child_with_cleanup_obligation(&mut broker, "cleanup-second", &mut event).await?;
    let ordered = broker
        .agent_coordinator
        .snapshot(None)
        .into_iter()
        .map(|(agent, _)| agent.id)
        .collect::<Vec<_>>();
    let (failing_native, _) = routing_fixture(
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        false,
    )?;
    let failing = crate::provider::test_support::ProviderTestSession::from_native(
        failing_native.fixture().with_shutdown(|| async {
            Err(crate::provider::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::ThreadClose,
                    forge_provider_api::ProviderFailureKind::Transport,
                ),
            ))
        }),
    )?;
    let (_succeeding_native, succeeding) = routing_fixture(
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        false,
    )?;
    broker
        .child_provider_by_agent
        .insert(ordered[0].clone(), failing.provider_handle());
    broker
        .child_provider_by_agent
        .insert(ordered[1].clone(), succeeding.provider_handle());
    let interrupted = broker
        .handle_agent_request(
            crate::agent::AgentRequest {
                operation_id: "interrupt-all".into(),
                source: crate::agent::AgentSource::User,
                operation: crate::agent::AgentOperation::InterruptAll,
            },
            None,
            &mut event,
        )
        .await;
    assert!(interrupted.is_err(), "the failed cleanup must be reported");
    assert!(
        broker.agent_coordinator.cleanup_task(&ordered[0]).is_some(),
        "the failed cleanup remains retryable"
    );
    assert!(
        broker.agent_coordinator.cleanup_task(&ordered[1]).is_none(),
        "a later child's cleanup runs after an earlier one fails"
    );
    assert_eq!(
        broker.agent_coordinator.resolve(&ordered[1])?.state,
        crate::agent::AgentState::Ready
    );
    // A successful retry clears only the collection obligation; the turn's own failure and the
    // stop policy bound to it survive.
    let collected = broker
        .store
        .list_agent_task(&broker.session.id)?
        .into_iter()
        .find(|task| task.agent_id == ordered[1])
        .context("collected child task retained")?;
    assert_eq!(collected.error.as_deref(), Some("provider failed"));
    assert_eq!(collected.state, ExchangeState::Failed);
    assert_eq!(
        broker
            .store
            .load_exchange(&collected.id)?
            .context("collected child exchange retained")?
            .finalization_error,
        None
    );
    assert!(matches!(
        broker.agent_coordinator.resolve(&ordered[1])?.continuation,
        crate::agent::AgentContinuationPolicy::RequiresAuthorization { ref task_id }
            if task_id.as_str() == collected.id
    ));
    Ok(())
}
#[tokio::test]
async fn idle_drain_launches_children_queued_for_capacity() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut admitted = Vec::new();
    while admitted
        .last()
        .is_none_or(|work: &crate::agent::AgentWork| work.launch)
    {
        let name = format!("capacity-{}", admitted.len());
        admitted.push(admit_child_fixture(&mut broker, &name)?);
    }
    let queued = admitted.pop().context("fixture queued no child")?;
    let running = admitted.len();
    assert_eq!(broker.agent_coordinator.active_count(), running);
    let first = &admitted[0];
    let owner = crate::provider::ThreadOwner::new(
        broker.session.id.clone().into(),
        forge_provider_api::agent::AgentId::new(first.agent.id.clone())?,
    )
    .expect("registered session and agent identities are nonempty");
    let mut event = Vec::new();
    broker
        .complete_managed_agent(
            owner,
            first.task.id.clone(),
            Err(anyhow::anyhow!("provider failed")),
            Vec::new(),
            None,
            &mut event,
        )
        .await?;
    assert_eq!(broker.agent_coordinator.active_count(), running - 1);
    let mut drained = Vec::new();
    while broker.drain_next_agent_command(None, &mut drained).await {}
    assert_eq!(
        broker.agent_coordinator.active_count(),
        running,
        "the idle drain must launch the child that waited for capacity"
    );
    assert!(
        broker
            .agent_coordinator
            .snapshot(None)
            .into_iter()
            .any(|(agent, _)| agent.id == queued.agent.id)
    );
    Ok(())
}

/// A failed child outcome still records committed edits before releasing writer exclusion.
#[tokio::test]
async fn child_file_change_reservation_requires_durable_completion() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let work = admit_child_fixture(&mut broker, "rename-child")?;
    let owner = crate::provider::ThreadOwner::new(
        broker.session.id.clone().into(),
        forge_provider_api::agent::AgentId::new(work.agent.id.clone())?,
    )?;
    let address = child_address(&owner, &work.task.id);
    broker
        .observe_child_fixture(
            owner.clone(),
            &work.task.id,
            child_event(
                &address,
                crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                    started_at_ms: 100,
                }),
            ),
            &mut Vec::new(),
        )
        .await?;
    let change = crate::tools::lsp::HostFileChange {
        reservation: Some(crate::checks::MutationReservation::acquire(
            &(repository.path()).join("harness-state"),
            repository.path(),
        )?),
        scope: address.scope,
        call_id: forge_provider_api::control::ToolCallId::new("child-rename")?,
        native_item_id: None,
        changes: Default::default(),
    };
    let database = crate::storage::test::raw_connection(data.path())?;
    database.execute_batch(
        "CREATE TRIGGER refuse_child_change BEFORE UPDATE ON exchange_record
         BEGIN SELECT RAISE(ABORT,'child change refused'); END;",
    )?;
    let error = broker
        .apply_managed_agent_completion(
            owner.clone(),
            work.task.id.clone(),
            Err(anyhow::anyhow!("native turn failed")),
            vec![change.clone()],
            None,
        )
        .err()
        .context("failed child storage acknowledged the mutation")?;
    assert!(format!("{error:#}").contains("child change refused"));
    assert!(
        broker
            .store
            .load_exchange(&work.task.id)?
            .context("child exchange disappeared")?
            .host_file_changes
            .is_empty()
    );
    assert!(
        crate::checks::CheckReservation::acquire(
            &(repository.path()).join("harness-state"),
            repository.path(),
            crate::checks::CheckPurpose::Verify
        )
        .is_err()
    );
    database.execute_batch("DROP TRIGGER refuse_child_change;")?;
    broker.apply_managed_agent_completion(
        owner,
        work.task.id.clone(),
        Err(anyhow::anyhow!("native turn failed")),
        vec![change.clone()],
        None,
    )?;
    let recorded = broker
        .store
        .load_exchange(&work.task.id)?
        .context("child completion disappeared")?;
    assert_eq!(recorded.host_file_changes, vec![change]);
    assert_eq!(recorded.state(), ExchangeState::Failed);
    let _verification = crate::checks::CheckReservation::acquire(
        &(repository.path()).join("harness-state"),
        repository.path(),
        crate::checks::CheckPurpose::Verify,
    )?;
    Ok(())
}

/// Build a root provider that admits one child and then fails before that child can finish.
///
/// The child runs on the configured mock adapter, so it can still be starting or running when
/// the root's failure reaches finalization.
fn failing_root_after_spawn() -> Arc<crate::provider::test_support::EventFixtureProvider> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity exists"),
        label: "Root failure after child admission".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, _sink, start, _cancelled| async move {
            start.observed(0).await?;
            crate::provider::test_support::call_tool(
                &request,
                "agent_spawn",
                json!({"profile":"worker","text":"DELEGATED_BEFORE_FAILURE"}),
            )
            .await?;
            Err(crate::provider::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                ),
            ))
        },
    )
}

/// A root turn that fails while its child is unfinished settles that child before finalizing.
///
/// Failure finalization previously stopped servicing child completions and went straight to the
/// checkpoint, whose descendant guard rejected the unfinished child. The parent then stayed
/// finalizing with that rejection, and the child's completion was never applied.
#[tokio::test]
async fn root_failure_settles_unfinished_children_before_finalizing() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let fixture = crate::provider::test_support::ProviderTestSession::from_native(
        failing_root_after_spawn(),
    )?;
    broker.provider = fixture.provider_handle();
    broker.agent_coordinator.set_enabled(true);
    // The interaction's own completion is the record this test needs. A wall-clock bound
    // here fired under load before finalization finished; nextest's limit still ends a hang.
    let outcome = broker
        .run_interaction(
            "Delegate".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Delegate".into())),
        )
        .await;
    assert!(outcome.is_err(), "the root provider failure must surface");
    let tasks = broker.store.list_agent_task(&broker.session.id)?;
    assert_eq!(tasks.len(), 1);
    assert!(tasks[0].completed_at_ms.is_some(), "{:?}", tasks[0]);
    let root = broker
        .store
        .load_exchange(&tasks[0].parent_exchange_id)?
        .context("root exchange retained")?;
    assert_eq!(
        (root.state(), root.finalization_error.as_deref()),
        (ExchangeState::Failed, None)
    );
    Ok(())
}

/// A user grant reserved against one stop cannot commit after a newer stop became durable.
///
/// The coordinator reserves a user continuation against the stop it observed. Storage rechecks
/// the persisted policy inside the admission transaction, so a newer stop rejects the grant and
/// the failed admission leaves no task, delegation, policy change, or queued reservation behind.
#[tokio::test]
async fn continuation_authorization_cannot_clear_newer_stop() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.agent_coordinator.set_enabled(true);
    let mut parent = Exchange::agent_admission(
        &broker.session.id,
        broker.store.next_exchange_ordinal(&broker.session.id)?,
        "Delegating parent".into(),
        100,
    );
    let first = admit_model_task(
        &mut broker,
        &mut parent,
        crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
            profile: Some("explorer".into()),
            text: "Inspect".into(),
        }),
    )?;
    let queued = admit_model_task(
        &mut broker,
        &mut parent,
        crate::agent::AgentOperation::Followup {
            target: first.agent.id.clone(),
            message: "Inspect more".into(),
        },
    )?;
    let owner = crate::provider::ThreadOwner::new(
        broker.session.id.clone().into(),
        forge_provider_api::agent::AgentId::new(first.agent.id.clone())?,
    )?;
    let mut event = Vec::new();
    broker
        .complete_managed_agent(
            owner,
            first.task.id.clone(),
            Err(anyhow::anyhow!("provider failed")),
            Vec::new(),
            None,
            &mut event,
        )
        .await?;
    // Persist a newer stop on the follow-up the first stop cancelled, as a concurrent owner would.
    let mut newer = broker.agent_coordinator.resolve(&first.agent.id)?;
    newer.continuation = crate::agent::AgentContinuationPolicy::RequiresAuthorization {
        task_id: queued.task.id.as_str().into(),
    };
    broker.store.save_agent_run(&newer)?;
    let tasks_before = broker.store.list_agent_task(&broker.session.id)?.len();
    let delegation_before = parent.clone();

    let request = crate::agent::AgentRequest {
        operation_id: "stale-grant".into(),
        source: crate::agent::AgentSource::User,
        operation: crate::agent::AgentOperation::Followup {
            target: first.agent.id.clone(),
            message: "Continue".into(),
        },
    };
    let admission = broker.agent_coordinator.reserve_followup(
        &request,
        &first.agent.id,
        "Continue",
        &parent.id,
        200,
    )?;
    assert_eq!(
        admission
            .continuation_authorization()
            .map(|grant| grant.as_str()),
        Some(first.task.id.as_str()),
        "the coordinator granted the stop it observed"
    );
    let mut staged = parent.clone();
    staged.append_agent_task(&admission.task)?;
    let rejected = broker
        .store
        .admit_agent_task(
            &admission.agent,
            &admission.task,
            &staged,
            admission.continuation_authorization(),
        )
        .err()
        .context("a grant for an older stop must not commit")?;
    assert!(format!("{rejected:#}").contains("stale"), "{rejected:#}");
    drop(admission);

    assert_eq!(
        broker.store.list_agent_task(&broker.session.id)?.len(),
        tasks_before
    );
    let stored_parent = broker
        .store
        .load_exchange(&parent.id)?
        .context("parent retained")?;
    assert_eq!(
        stored_parent.node_list.len(),
        delegation_before.node_list.len(),
        "the rejected grant left a delegation reference"
    );
    let durable = broker
        .store
        .list_agent_run(&broker.session.id)?
        .into_iter()
        .find(|agent| agent.id == first.agent.id)
        .context("child retained")?;
    assert_eq!(durable.continuation, newer.continuation);
    let count = broker.agent_coordinator.collection_count();
    assert_eq!((count.running, count.queued), (0, 0));
    Ok(())
}

/// Recovery terminalizes a crashed parent's children without replay, then the parent settles;
/// a delegation whose child exchange vanished is reported by identity instead.
#[tokio::test]
async fn recovery_settles_children_before_parent_checkpoint() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.agent_coordinator.set_enabled(true);
    let mut parent = Exchange::agent_admission(
        &broker.session.id,
        broker.store.next_exchange_ordinal(&broker.session.id)?,
        "Delegating parent".into(),
        100,
    );
    let running = admit_model_task(
        &mut broker,
        &mut parent,
        crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
            profile: Some("explorer".into()),
            text: "Inspect".into(),
        }),
    )?;
    let queued = admit_model_task(
        &mut broker,
        &mut parent,
        crate::agent::AgentOperation::Followup {
            target: running.agent.id.clone(),
            message: "Inspect more".into(),
        },
    )?;
    // The host stopped while the parent was finalizing and both child tasks were unfinished.
    parent.phase =
        crate::exchange::ExchangePhase::Finalizing(crate::exchange::ExchangeOutcome::Complete);
    broker.store.save_exchange(&parent)?;

    broker
        .store
        .interrupt_detached_execution(&broker.session.id)?;
    for task in broker.store.list_agent_task(&broker.session.id)? {
        assert_eq!(task.state, ExchangeState::Interrupted, "{task:?}");
        assert!(task.completed_at_ms.is_some() && task.parent_reported);
        assert!(
            broker
                .store
                .load_exchange(&task.id)?
                .context("child exchange retained")?
                .is_finished(),
            "recovery left child exchange {} open",
            task.id
        );
    }
    for task in [&running.task, &queued.task] {
        assert!(
            broker
                .store
                .list_agent_task(&broker.session.id)?
                .iter()
                .any(|recovered| recovered.id == task.id),
            "recovery dropped task {}",
            task.id
        );
    }
    assert_eq!(broker.store.list_agent_task(&broker.session.id)?.len(), 2);
    let mut event = Vec::new();
    let mut prior = broker
        .begin_prior_finalization()?
        .context("the recovered parent remains finalizing")?;
    assert!(matches!(
        broker.advance_prior_finalization(&mut prior, &mut event)?,
        super::super::prior_finalization::PriorFinalizationAction::Ready
    ));
    let settled = broker
        .store
        .load_exchange(&parent.id)?
        .context("parent retained")?;
    assert!(settled.is_finished());
    assert_eq!(settled.finalization_error, None);

    // A delegation whose child exchange is missing fails with that child's identity.
    let mut orphaned = Exchange::agent_admission(
        &broker.session.id,
        broker.store.next_exchange_ordinal(&broker.session.id)?,
        "Orphaned parent".into(),
        200,
    );
    let missing = admit_model_task(
        &mut broker,
        &mut orphaned,
        crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
            profile: Some("explorer".into()),
            text: "Inspect elsewhere".into(),
        }),
    )?;
    broker.store.delete_exchange(&missing.task.id)?;
    let error = broker
        .store
        .require_settled_descendants(&orphaned)
        .err()
        .context("a missing child exchange cannot settle its parent")?;
    let unsettled = error
        .downcast_ref::<crate::storage::DescendantUnsettled>()
        .context("the guard names the missing child")?;
    assert_eq!(
        (unsettled.agent_id.as_str(), unsettled.exchange_id.as_str()),
        (missing.agent.id.as_str(), missing.task.id.as_str())
    );
    assert_eq!(format!("{:?}", unsettled.obligation), "Missing");
    Ok(())
}

/// A write-off child selection stays write-off at dispatch: its request denies writes and takes
/// the shared repository reservation, while an inherited selection keeps the root's writer.
#[tokio::test]
async fn write_off_child_dispatch_denies_writes_under_a_shared_reservation() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.agent_coordinator.set_enabled(true);
    assert_ne!(
        broker.session.access.write_access,
        crate::session::WriteScope::Off,
        "the root must hold write scope for this case to distinguish the child"
    );
    let mut parent = Exchange::agent_admission(
        &broker.session.id,
        broker.store.next_exchange_ordinal(&broker.session.id)?,
        "Delegating parent".into(),
        100,
    );
    let admitted = admit_model_task(
        &mut broker,
        &mut parent,
        crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
            profile: Some("explorer".into()),
            text: "Inspect".into(),
        }),
    )?;
    let root = broker.provider_request(ProviderInput::from_user_text(""), PromptMode::Chat)?;
    for (write_access, repository_access) in [
        (None, crate::repository_access::RepositoryAccess::Write),
        (
            Some(crate::session::WriteScope::Off),
            crate::repository_access::RepositoryAccess::Read,
        ),
    ] {
        let mut agent = admitted.agent.clone();
        if let Some(write_access) = write_access {
            agent
                .selection
                .as_mut()
                .context("admitted child has a selection")?
                .access
                .write_access = write_access;
        }
        let request = broker.agent_provider_factory.request(
            &root,
            &admitted_request_fixture(&broker, agent)?,
            forge_provider_api::forge::AgentTask {
                task_id: "dispatch".into(),
                from: forge_provider_api::forge::ROOT_AGENT_PATH.into(),
                text: "Inspect".into(),
                messages: Vec::new(),
            },
        )?;
        assert_eq!(
            crate::agent::repository_access(&request.access),
            repository_access
        );
        let scope = crate::provider::InvocationScope::new(request.owner()?, "child-dispatch");
        let _directories = broker
            .permission_coordinator
            .directories(None, &request, &scope)?;
        assert_eq!(
            broker
                .permission_coordinator
                .writes_disabled(Some(&scope))?,
            write_access.is_some(),
            "dispatched write policy diverged from the child selection"
        );
    }
    Ok(())
}

/// Cancelling admitted work that holds no execution slot terminalizes it durably, so the parent
/// settles and the next prompt starts.
///
/// Collection once skipped child interruption whenever no execution slot was occupied, so a
/// launch queued behind a fence stayed admitted and the parent's checkpoint guard rejected it.
#[tokio::test]
async fn cancel_collects_pending_children_without_execution_slots() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = Arc::new(ResultReceivingProvider::default());
    let fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
    broker.provider = fixture.provider_handle();
    broker.agent_coordinator.set_enabled(true);
    let mut parent = Exchange::agent_admission(
        &broker.session.id,
        broker.store.next_exchange_ordinal(&broker.session.id)?,
        "Delegating parent".into(),
        100,
    );
    let spawn = crate::agent::SpawnAgentRequest {
        profile: Some("explorer".into()),
        text: "Inspect".into(),
    };
    let request = crate::agent::AgentRequest {
        operation_id: "queued-spawn".into(),
        source: crate::agent::AgentSource::Model,
        operation: crate::agent::AgentOperation::Spawn(spawn.clone()),
    };
    let admission = broker.agent_coordinator.reserve_spawn(
        &request,
        &spawn,
        crate::agent::AgentSelection {
            profile: "worker".into(),
            instructions: String::new(),
            provider: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            model: "mock".into(),
            reasoning_effort: "low".into(),
            context_window: None,
            service_tier: Default::default(),
            access: broker.session.access.clone(),
            approval_mode: broker.session.approval_mode,
        },
        &parent.id,
        100,
    )?;
    // A launch fence that begins between reservation and commit queues the admitted task.
    let quiescence = broker.agent_coordinator.begin_quiescence();
    parent.append_agent_task(&admission.task)?;
    broker.store.admit_agent_task(
        &admission.agent,
        &admission.task,
        &parent,
        admission.continuation_authorization(),
    )?;
    let queued = match admission.commit()? {
        crate::agent::AgentCommit::Admitted(work) => work,
        crate::agent::AgentCommit::Existing(_) => anyhow::bail!("fixture reused a child admission"),
    };
    broker.agent_registry.insert(queued.agent.clone());
    assert!(!queued.launch);
    let count = broker.agent_coordinator.collection_count();
    assert_eq!((count.running, count.queued), (0, 1));

    // The cancel transition's collection sends exactly this request when work is admitted.
    let mut event = Vec::new();
    broker
        .handle_agent_request(
            crate::agent::AgentRequest {
                operation_id: "cancel:collect-children".into(),
                source: crate::agent::AgentSource::User,
                operation: crate::agent::AgentOperation::InterruptAll,
            },
            None,
            &mut event,
        )
        .await?;
    drop(quiescence);
    assert!(!broker.agent_coordinator.collection_count().has_work());
    assert!(broker.agent_coordinator.ready_launch()?.is_none());
    let task = broker
        .store
        .list_agent_task(&broker.session.id)?
        .into_iter()
        .find(|task| task.id == queued.task.id)
        .context("queued task retained")?;
    assert_eq!(task.state, ExchangeState::Cancelled);
    assert!(task.completed_at_ms.is_some());
    assert_eq!(
        broker
            .store
            .load_exchange(&queued.task.id)?
            .context("queued exchange retained")?
            .state(),
        ExchangeState::Cancelled
    );

    parent.phase =
        crate::exchange::ExchangePhase::Finalizing(crate::exchange::ExchangeOutcome::Cancelled);
    broker.store.save_exchange(&parent)?;
    let mut prior = broker
        .begin_prior_finalization()?
        .context("the cancelled parent remains finalizing")?;
    assert!(matches!(
        broker.advance_prior_finalization(&mut prior, &mut event)?,
        super::super::prior_finalization::PriorFinalizationAction::Ready
    ));
    broker
        .run_interaction(
            "EXPLICIT_NEXT_TURN".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("EXPLICIT_NEXT_TURN".into())),
        )
        .await?;
    assert_eq!(provider.turn.load(Ordering::Acquire), 1);
    Ok(())
}

/// Show an admitted child in status until it settles, then only in its parent's Agents summary.
#[tokio::test]
async fn running_children_move_from_status_to_the_agents_summary() -> Result<()> {
    use crate::timeline::TimelineEntry;
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let work = admit_child_fixture(&mut broker, "status-child")?;
    let label = work.agent.task_name.clone();
    let status_agents = |timeline: &[TimelineEntry]| {
        timeline
            .iter()
            .find_map(|entry| match entry {
                TimelineEntry::Status { agents, .. } => Some(
                    agents
                        .iter()
                        .map(|agent| agent.label.clone())
                        .collect::<Vec<_>>(),
                ),
                _ => None,
            })
            .unwrap_or_default()
    };
    let transcript = |timeline: &[TimelineEntry]| -> Result<Vec<String>> {
        let mut rows = Vec::new();
        for entry in timeline {
            rows.extend(crate::buffer::projection::tests::project_rows(entry)?);
        }
        Ok(rows)
    };
    let timeline = broker.snapshot()?.timeline;
    assert_eq!(status_agents(&timeline), std::slice::from_ref(&label));
    let rows = transcript(&timeline)?;
    assert!(
        rows.iter()
            .any(|row| row.starts_with(&format!("◇ Spawned {label} · "))),
        "{rows:#?}"
    );
    assert!(
        !rows.iter().any(|row| row.starts_with("Agents · ")),
        "a running child has no summary yet: {rows:#?}"
    );

    let owner = crate::provider::ThreadOwner::new(
        broker.session.id.clone().into(),
        forge_provider_api::agent::AgentId::new(work.agent.id.clone())?,
    )
    .expect("registered session and agent identities are nonempty");
    let mut event = Vec::new();
    broker
        .complete_managed_agent(
            owner,
            work.task.id.clone(),
            Err(anyhow::anyhow!("provider failed")),
            Vec::new(),
            None,
            &mut event,
        )
        .await?;
    let timeline = broker.snapshot()?.timeline;
    assert!(status_agents(&timeline).is_empty());
    let rows = transcript(&timeline)?;
    assert!(
        rows.iter()
            .any(|row| row.starts_with("Agents · 0/1 completed")),
        "{rows:#?}"
    );
    assert!(
        rows.iter()
            .any(|row| row.starts_with(&format!("✗ {label}")) && row.contains(" · Failed ")),
        "{rows:#?}"
    );
    Ok(())
}
