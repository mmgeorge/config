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
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "probe task".into(),
                            shell: None,
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
        &broker.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?,
        &admitted_request_fixture(&broker, retained.clone())?,
        "Follow-up".into(),
    )?;
    assert_eq!(
        request.provider_session_id.map(String::from),
        retained.provider_thread_id
    );
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
        // Each report is a typed `<agent_result>` carrying only the child's identity, status, and
        // outcome, never Harness bookkeeping such as `parent_reported`.
        for message in message.iter() {
            assert!(
                message.contains("<agent_result agent=\"")
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
