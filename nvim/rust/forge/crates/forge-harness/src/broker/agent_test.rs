use super::super::{
    AppliedProviderEvent, ApprovalMode, Exchange, ExchangeAdmission, ExchangeState, HarnessBroker,
    InitializeRequest, PlanExecutionState, PromptMode, ProviderEvent, ProviderInput,
    ProviderLaunch, SessionUpdate, TurnCancelled,
};
use super::test_support::{
    admit_child_fixture, check_fixture, child_address, child_completed, child_event, child_tool,
    completed_message, planning_question_broker, repository, settle_managed_children,
};
use crate::provider::ScopeOwner;
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use uuid::Uuid;

type TestReceiptWaiters = Arc<std::sync::Mutex<Vec<(String, tokio::sync::oneshot::Sender<()>)>>>;

fn save_child_profile(
    broker: &HarnessBroker,
    name: &str,
    profile: crate::agent::AgentProfile,
) -> Result<()> {
    let snapshot = broker.agent_provider_factory.profiles.snapshot()?;
    let mut document = snapshot.document;
    document.profiles.insert(name.into(), profile);
    broker
        .agent_provider_factory
        .profiles
        .save(&snapshot.revision, document)?;
    Ok(())
}

struct RoutingProvider {
    kind: crate::provider::ProviderId,
    turn: std::sync::atomic::AtomicUsize,
    request: Arc<std::sync::Mutex<Vec<forge_provider_api::PromptRequest>>>,
    reject_steering: bool,
}

#[tokio::test]
async fn profile_toggles_gate_fresh_and_retained_child_admission() -> Result<()> {
    use crate::agent::{
        AgentOperation, AgentProfile, AgentRequest, AgentSource, SpawnAgentRequest,
    };
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.agent_coordinator.set_enabled(true);
    save_child_profile(&broker, "reviewer", AgentProfile::default())?;
    let (admitted, _) = broker
        .start_agent(json!({"profile":"reviewer","text":"First review"}))
        .await?;
    let target = admitted["agent"]["id"]
        .as_str()
        .context("missing child identity")?
        .to_owned();
    settle_managed_children(&mut broker).await?;
    let original = broker.agent_coordinator.resolve(&target)?;
    let task_count = broker.store.list_agent_task(&broker.session.id)?.len();
    let profiles = crate::agent::AgentProfileStore::new(
        broker
            .agent_provider_factory
            .profiles
            .snapshot()?
            .path
            .into(),
    );
    let snapshot = profiles.snapshot()?;
    let mut document = snapshot.document;
    document.profiles.get_mut("reviewer").unwrap().enabled = false;
    profiles.save(&snapshot.revision, document)?;
    for source in [AgentSource::User, AgentSource::Model] {
        for operation in [
            AgentOperation::Spawn(SpawnAgentRequest {
                profile: Some("reviewer".into()),
                text: "New review".into(),
            }),
            AgentOperation::Followup {
                target: target.clone(),
                message: "Another review".into(),
            },
        ] {
            let error = broker
                .handle_agent_request(
                    AgentRequest {
                        operation_id: Uuid::new_v4().to_string(),
                        source,
                        operation,
                    },
                    None,
                    &mut Vec::new(),
                )
                .await
                .unwrap_err();
            assert!(
                error
                    .to_string()
                    .contains("subagent profile is disabled: reviewer"),
                "{error:#}"
            );
            assert_eq!(
                broker.store.list_agent_task(&broker.session.id)?.len(),
                task_count
            );
            assert_eq!(broker.agent_coordinator.active_count(), 0);
        }
    }
    let snapshot = profiles.snapshot()?;
    let mut document = snapshot.document;
    let profile = document.profiles.get_mut("reviewer").unwrap();
    profile.enabled = true;
    profile.instructions = "New spawns use these instructions".into();
    profiles.save(&snapshot.revision, document)?;
    let result = broker
        .handle_agent_request(
            AgentRequest {
                operation_id: Uuid::new_v4().to_string(),
                source: AgentSource::Model,
                operation: AgentOperation::Followup {
                    target: target.clone(),
                    message: "Review after enabling".into(),
                },
            },
            None,
            &mut Vec::new(),
        )
        .await?;
    assert_eq!(result["agent"]["id"], target);
    assert_eq!(
        result["agent"]["selection"],
        serde_json::to_value(&original.selection)?
    );
    settle_managed_children(&mut broker).await?;
    assert_eq!(
        broker.store.list_agent_task(&broker.session.id)?.len(),
        task_count + 1
    );
    let (fresh, _) = broker
        .start_agent(json!({"profile":"reviewer","text":"Fresh enabled review"}))
        .await?;
    assert_ne!(fresh["agent"]["id"], target);
    assert_eq!(
        fresh["agent"]["selection"]["instructions"],
        "New spawns use these instructions"
    );
    settle_managed_children(&mut broker).await?;
    Ok(())
}

#[tokio::test]
async fn profile_selection_captures_tuning_and_instructions_without_widening_permissions()
-> Result<()> {
    use crate::agent::{AgentProfile, SpawnAgentRequest};
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (_routing, routing_fixture) =
        routing_fixture(forge_providers::registry::default_id(), false)?;
    broker
        .agent_provider_factory
        .register(routing_fixture.provider_handle());
    let mut root =
        broker.provider_request(ProviderInput::from_text("parent secret"), PromptMode::Chat)?;
    root.model = "parent-model".into();
    root.effort = "high".into();
    root.approval_mode = ApprovalMode::Ask;
    let inherited = broker
        .agent_provider_factory
        .resolve(
            &root,
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            &SpawnAgentRequest {
                profile: None,
                text: "task".into(),
            },
        )
        .await?;
    assert_eq!(inherited.model, "parent-model");
    assert_eq!(inherited.reasoning_effort, "high");
    save_child_profile(
        &broker,
        "review",
        AgentProfile {
            provider: forge_providers::registry::default_id().to_string(),
            model: "default".into(),
            reasoning_effort: "default".into(),
            service_tier: "default".into(),
            approval_mode: "ask".into(),
            instructions: "First instructions".into(),
            ..Default::default()
        },
    )?;
    let spawn = SpawnAgentRequest {
        profile: Some("review".into()),
        text: "task".into(),
    };
    let selected = broker
        .agent_provider_factory
        .resolve(
            &root,
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            &spawn,
        )
        .await?;
    assert_eq!(selected.provider, forge_providers::registry::default_id());
    assert_eq!(selected.model, "fixture-model");
    assert_eq!(selected.reasoning_effort, "low");
    assert_eq!(selected.approval_mode, ApprovalMode::Ask);
    let mut child = admit_child_fixture(&mut broker, "resolved-profile")?;
    child.agent.selection = Some(selected);
    save_child_profile(
        &broker,
        "review",
        AgentProfile {
            instructions: "Changed instructions".into(),
            ..Default::default()
        },
    )?;
    root.model = "changed-parent-model".into();
    root.approval_mode = ApprovalMode::Allow;
    let request = broker
        .agent_provider_factory
        .request(&root, &child, "follow-up".into())?;
    assert_eq!(request.model, "fixture-model");
    assert_eq!(request.approval_mode, ApprovalMode::Ask);
    assert!(
        matches!(request.input,ProviderInput::Text {text, ..} if text.contains("First instructions")
        && !text.contains("Changed instructions") && !text.contains("parent secret"))
    );
    assert!(request.provider_session_id.is_none());
    assert!(request.control_context.is_none());
    let next = broker
        .agent_provider_factory
        .resolve(
            &root,
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            &spawn,
        )
        .await?;
    assert_eq!(next.model, "changed-parent-model");
    assert_eq!(next.instructions, "Changed instructions");
    save_child_profile(
        &broker,
        "review",
        AgentProfile {
            provider: forge_providers::registry::default_id().to_string(),
            ..Default::default()
        },
    )?;
    assert!(
        broker
            .agent_provider_factory
            .resolve(
                &root,
                forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                &spawn
            )
            .await
            .is_err(),
        "a foreign provider must reject an inherited unavailable model"
    );
    root.approval_mode = ApprovalMode::Ask;
    child.agent.selection.as_mut().unwrap().approval_mode = ApprovalMode::Allow;
    assert_eq!(
        broker
            .agent_provider_factory
            .request(&root, &child, "task".into())?
            .approval_mode,
        ApprovalMode::Ask
    );
    Ok(())
}

/// Keep binding status out of task transitions while validating the exact child owner.
#[test]
fn child_terminal_inventory_does_not_require_or_mutate_an_invocation() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let admission = admit_child_fixture(&mut broker, "terminal-status")?;
    let owner = crate::provider::ThreadOwner::new(
        broker.session.id.clone().into(),
        forge_provider_api::agent::AgentId::new(admission.agent.id.clone()).unwrap(),
    )
    .expect("registered session and agent identities are nonempty");
    let status = |target: crate::provider::ThreadOwner| {
        crate::provider::session_execution::OwnerObservation::Event(
            crate::provider::test_support::native_event(
                forge_provider_api::EventScope::Binding {
                    thread_id: forge_provider_api::ThreadId::from(target.clone()),
                },
                ProviderEvent::Thread {
                    thread_id: target.thread_id(),
                    event: forge_provider_api::ThreadEvent::TerminalsUpdated(Ok(
                        forge_provider_api::terminal::TerminalInventory::observed(Vec::new()),
                    )),
                },
            ),
        )
    };
    let before = broker
        .agent_coordinator
        .task(&admission.task.id)
        .unwrap()
        .state;
    assert!(matches!(
        broker.apply_managed_agent_observation(
            owner.clone(),
            &admission.task.id,
            status(owner.clone()),
            &mut Vec::new()
        )?,
        AppliedProviderEvent::None
    ));
    assert_eq!(
        broker
            .agent_coordinator
            .task(&admission.task.id)
            .unwrap()
            .state,
        before
    );
    let foreign = crate::provider::ThreadOwner::root("foreign");
    assert!(
        broker
            .apply_managed_agent_observation(
                owner.clone(),
                &admission.task.id,
                status(foreign),
                &mut Vec::new()
            )
            .is_err()
    );
    let diagnostic = crate::provider::session_execution::OwnerObservation::Event(
        crate::provider::test_support::native_event(
            forge_provider_api::EventScope::Binding {
                thread_id: forge_provider_api::ThreadId::from(owner.clone()),
            },
            ProviderEvent::Thread {
                thread_id: owner.thread_id(),
                event: forge_provider_api::ThreadEvent::TerminalsUpdated(Err(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TerminalList,
                        forge_provider_api::ProviderFailureKind::Runtime,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Observation,
                        forge_provider_api::FailureDisposition::Unchanged,
                    ),
                )),
            },
        ),
    );
    assert!(matches!(
        broker.apply_managed_agent_observation(
            owner,
            &admission.task.id,
            diagnostic,
            &mut Vec::new()
        )?,
        AppliedProviderEvent::Update(SessionUpdate::Diagnostic { .. })
    ));
    Ok(())
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
async fn agent_activity_omits_output_and_pages_stable_owned_calls_across_followups() -> Result<()> {
    use crate::agent::AgentOperation;
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let admission = admit_child_fixture(&mut broker, "inspection")?;
    let sibling = admit_child_fixture(&mut broker, "sibling-inspection")?;
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
            &mut Vec::new(),
        )
        .await?;
    for ordinal in 0..6 {
        let thought = child_message(
            &child_address(&(owner.clone()), &admission.task.id),
            format!("thought-{ordinal}"),
            crate::provider::TestMessageContent::Commentary,
            if ordinal == 5 {
                "思".repeat(4001)
            } else {
                format!("Thought {ordinal}")
            },
        );
        broker
            .observe_child_fixture(owner.clone(), &admission.task.id, thought, &mut Vec::new())
            .await?;
    }
    let source = format!("{}\0tail", "output-é😀".repeat(9000));
    let mut update = child_tool(
        &child_address(&(owner.clone()), &admission.task.id),
        source.clone(),
        crate::provider::ToolStatus::Running,
    );
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            update.clone(),
            &mut Vec::new(),
        )
        .await?;
    let activity = query_child(
        &mut broker,
        AgentOperation::Activity {
            target: admission.agent.id.clone(),
            limit: None,
        },
    )
    .await?;
    assert_eq!(activity["entries"].as_array().unwrap().len(), 5);
    assert_eq!(activity["has_more"], true);
    let thought = &activity["entries"][3];
    assert_eq!(thought["text"].as_str().unwrap().chars().count(), 4000);
    assert_eq!(thought["truncated"], true);
    let tool = &activity["entries"][4];
    assert_eq!(tool["status"], "running");
    assert!(tool.get("output").is_none());
    assert!(!activity.to_string().contains("output-é"));
    let call_ref = tool["call_ref"].as_str().unwrap().to_owned();
    let sibling_owner = crate::provider::ThreadOwner::new(
        (broker.session.id.clone()).into(),
        forge_provider_api::agent::AgentId::new(sibling.agent.id.clone()).unwrap(),
    )
    .expect("registered session and agent identities are nonempty");
    broker
        .observe_child_fixture(
            sibling_owner.clone(),
            &sibling.task.id,
            child_event(
                &child_address(&(sibling_owner.clone()), &sibling.task.id),
                crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                    started_at_ms: 0,
                }),
            ),
            &mut Vec::new(),
        )
        .await?;
    let sibling_tool = child_tool(
        &child_address(&(sibling_owner), &sibling.task.id),
        "SIBLING OUTPUT".into(),
        crate::provider::ToolStatus::Running,
    );
    broker
        .observe_child_fixture(
            sibling_owner.clone(),
            &sibling.task.id,
            sibling_tool,
            &mut Vec::new(),
        )
        .await?;
    let sibling_activity = query_child(
        &mut broker,
        AgentOperation::Activity {
            target: sibling.agent.id.clone(),
            limit: Some(1),
        },
    )
    .await?;
    let sibling_ref = sibling_activity["entries"][0]["call_ref"]
        .as_str()
        .unwrap()
        .to_owned();
    assert_ne!(
        sibling_ref, call_ref,
        "identical native call IDs must remain child-scoped"
    );
    let sibling_page = query_child(
        &mut broker,
        AgentOperation::ToolOutput {
            call_ref: sibling_ref,
            offset: 0,
            limit: 8000,
        },
    )
    .await?;
    assert_eq!(sibling_page["output"], "SIBLING OUTPUT");
    let page = query_child(
        &mut broker,
        AgentOperation::ToolOutput {
            call_ref: call_ref.clone(),
            offset: 65530,
            limit: 17,
        },
    )
    .await?;
    assert_eq!(
        page["output"],
        source.chars().skip(65530).take(17).collect::<String>()
    );
    assert_eq!(page["next_offset"], 65547);
    assert_eq!(page["total_chars"], source.chars().count());
    let tail = query_child(
        &mut broker,
        AgentOperation::ToolOutput {
            call_ref: call_ref.clone(),
            offset: source.chars().count() - 5,
            limit: 10,
        },
    )
    .await?;
    assert_eq!(tail["output"], "\0tail");
    assert!(tail["next_offset"].is_null());
    let child_turn = child_address(&owner, &admission.task.id);
    update = vec![ProviderEvent::Item {
        item_id: forge_provider_api::ItemId::new(format!(
            "{}:same-provider-call",
            child_turn.turn_id.as_str()
        ))
        .unwrap(),
        thread_id: child_turn.thread_id,
        turn_id: child_turn.turn_id,
        event: forge_provider_api::ItemEvent::Tool {
            event: forge_provider_api::ToolItemEvent::Output(crate::provider::TextUpdate::Append(
                "APPENDED".into(),
            )),
        },
    }];
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            update.clone(),
            &mut Vec::new(),
        )
        .await?;
    let appended = query_child(
        &mut broker,
        AgentOperation::ToolOutput {
            call_ref: call_ref.clone(),
            offset: source.chars().count(),
            limit: 8000,
        },
    )
    .await?;
    assert_eq!(appended["output"], "APPENDED");
    assert_eq!(appended["status"], "running");
    assert!(appended["next_offset"].is_null());
    let last_activity = broker
        .agent_coordinator
        .task(&admission.task.id)
        .unwrap()
        .last_activity_at_ms;
    let repeated = query_child(
        &mut broker,
        AgentOperation::Activity {
            target: admission.agent.id.clone(),
            limit: Some(1),
        },
    )
    .await?;
    assert_eq!(
        repeated["last_activity_at_ms"],
        json!(last_activity),
        "inspection must not count as child activity"
    );

    let compact = query_child(
        &mut broker,
        AgentOperation::Activity {
            target: admission.agent.id.clone(),
            limit: Some(2),
        },
    )
    .await?;
    assert_eq!(compact["entries"].as_array().unwrap().len(), 2);
    let expanded = query_child(
        &mut broker,
        AgentOperation::Activity {
            target: admission.agent.id.clone(),
            limit: Some(20),
        },
    )
    .await?;
    assert_eq!(expanded["entries"].as_array().unwrap().len(), 7);
    assert_eq!(expanded["has_more"], false);
    let default = query_child(
        &mut broker,
        AgentOperation::Activity {
            target: admission.agent.id.clone(),
            limit: None,
        },
    )
    .await?;
    assert_eq!(default["entries"].as_array().unwrap().len(), 5);
    assert!(
        query_child(
            &mut broker,
            AgentOperation::Activity {
                target: admission.agent.id.clone(),
                limit: Some(21)
            }
        )
        .await
        .is_err()
    );
    assert!(
        query_child(
            &mut broker,
            AgentOperation::Activity {
                target: admission.agent.id.clone(),
                limit: Some(0)
            }
        )
        .await
        .is_err()
    );

    let followup_request = crate::agent::AgentRequest {
        operation_id: "inspection-followup".into(),
        source: crate::agent::AgentSource::User,
        operation: AgentOperation::Followup {
            target: admission.agent.id.clone(),
            message: "Next task".into(),
        },
    };
    let followup = broker.agent_coordinator.reserve_followup(
        &followup_request,
        &admission.agent.id,
        "Next task",
        &admission.task.parent_exchange_id,
        101,
    )?;
    let mut parent = broker
        .store
        .load_exchange(&admission.task.parent_exchange_id)?
        .unwrap();
    parent.append_agent_task(&followup.task)?;
    broker.store.admit_agent_task(
        &followup.agent,
        &followup.task,
        &parent,
        followup.continuation_authorization(),
    )?;
    followup.commit()?;
    let active = query_child(
        &mut broker,
        AgentOperation::Activity {
            target: admission.agent.id.clone(),
            limit: Some(1),
        },
    )
    .await?;
    assert_eq!(
        active["task_id"], admission.task.id,
        "queued follow-up must not hide active progress"
    );

    update = child_tool(
        &child_address(&(owner.clone()), &admission.task.id),
        "failure details".into(),
        crate::provider::ToolStatus::Failed,
    );
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            update.clone(),
            &mut Vec::new(),
        )
        .await?;
    let final_message = child_message(
        &child_address(&(owner.clone()), &admission.task.id),
        "final",
        crate::provider::TestMessageContent::Response,
        "FINAL ANSWER MUST NOT ENTER ACTIVITY",
    );
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            final_message,
            &mut Vec::new(),
        )
        .await?;
    let before_completion = query_child(
        &mut broker,
        AgentOperation::Activity {
            target: admission.agent.id.clone(),
            limit: Some(20),
        },
    )
    .await?;
    assert!(
        !before_completion
            .to_string()
            .contains("FINAL ANSWER MUST NOT ENTER ACTIVITY")
    );
    broker
        .complete_managed_agent(
            owner.clone(),
            admission.task.id.clone(),
            Err(anyhow::anyhow!("failed")),
            Vec::new(),
            None,
            &mut Vec::new(),
        )
        .await?;
    let old_page = query_child(
        &mut broker,
        AgentOperation::ToolOutput {
            call_ref: call_ref.clone(),
            offset: 0,
            limit: 8000,
        },
    )
    .await?;
    assert_eq!(old_page["status"], "failed");
    assert_eq!(old_page["failed"], true);
    assert_eq!(old_page["output"], "failure details");

    let foreign = crate::agent::AgentToolReference {
        session_id: "other-session".into(),
        agent_id: admission.agent.id.clone(),
        exchange_id: admission.task.id.clone(),
        turn_id: child_address(&owner, &admission.task.id)
            .turn_id
            .as_str()
            .to_owned(),
        tool_id: "same-provider-call".into(),
    };
    let forged = format!("agent-tool:{}", hex::encode(serde_json::to_vec(&foreign)?));
    assert!(
        query_child(
            &mut broker,
            AgentOperation::ToolOutput {
                call_ref: forged,
                offset: 0,
                limit: 8
            }
        )
        .await
        .is_err()
    );
    let crossed = crate::agent::AgentToolReference {
        session_id: broker.session.id.clone(),
        agent_id: sibling.agent.id.clone(),
        ..foreign
    };
    let forged = format!("agent-tool:{}", hex::encode(serde_json::to_vec(&crossed)?));
    assert!(
        query_child(
            &mut broker,
            AgentOperation::ToolOutput {
                call_ref: forged,
                offset: 0,
                limit: 8
            }
        )
        .await
        .is_err()
    );
    assert!(
        query_child(
            &mut broker,
            AgentOperation::ToolOutput {
                call_ref,
                offset: 999999,
                limit: 8
            }
        )
        .await
        .is_err()
    );
    Ok(())
}

#[cfg(windows)]
#[tokio::test]
async fn child_execution_blocks_automatic_verification_without_mutating_the_goal() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = check_fixture(
        repository.path(),
        data.path(),
        vec!["Write-Output verified".into()],
        false,
    )
    .await;
    let admission = admit_child_fixture(&mut broker, "verification-child")?;
    assert!(
        broker
            .run_checks()
            .await
            .unwrap_err()
            .to_string()
            .contains("child execution")
    );
    let execution = broker
        .store
        .load_plan_execution("check-execution")?
        .unwrap();
    assert!(execution.check_runs.is_empty());
    assert_eq!(execution.state, PlanExecutionState::Active);
    assert!(broker.task_controller.current()?.is_some());
    let owner = crate::provider::ThreadOwner::new(
        (broker.session.id.clone()).into(),
        forge_provider_api::agent::AgentId::new(admission.agent.id).unwrap(),
    )
    .expect("registered session and agent identities are nonempty");
    broker
        .complete_managed_agent(
            owner,
            admission.task.id,
            Err(anyhow::anyhow!("stopped")),
            Vec::new(),
            None,
            &mut Vec::new(),
        )
        .await?;
    broker.run_checks().await?;
    assert_eq!(
        broker
            .store
            .load_plan_execution("check-execution")?
            .unwrap()
            .state,
        PlanExecutionState::Complete
    );
    Ok(())
}

#[tokio::test]
async fn child_summary_preserves_running_exchange_when_followup_is_queued() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let admission = admit_child_fixture(&mut broker, "queued-followup")?;
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
                &child_address(&(owner), &admission.task.id),
                crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                    started_at_ms: 0,
                }),
            ),
            &mut Vec::new(),
        )
        .await?;
    let request = crate::agent::AgentRequest {
        operation_id: "queued".into(),
        source: crate::agent::AgentSource::User,
        operation: crate::agent::AgentOperation::Followup {
            target: admission.agent.id.clone(),
            message: "Next".into(),
        },
    };
    let queued = broker.agent_coordinator.reserve_followup(
        &request,
        &admission.agent.id,
        "Next",
        &admission.task.parent_exchange_id,
        101,
    )?;
    let mut parent = broker
        .store
        .load_exchange(&admission.task.parent_exchange_id)?
        .unwrap();
    parent.append_agent_task(&queued.task)?;
    broker.store.admit_agent_task(
        &queued.agent,
        &queued.task,
        &parent,
        queued.continuation_authorization(),
    )?;
    queued.commit()?;
    let snapshot = broker.snapshot()?;
    let summary = &snapshot.agent.summary[&admission.agent.id];
    assert_eq!(summary["state"], "running");
    assert_eq!(summary["latest_exchange_id"], admission.task.id);
    assert_eq!(
        summary["target"]["turn_id"],
        json!(child_address(&owner, &admission.task.id).turn_id)
    );
    assert_eq!(summary["exchange_count"], 2);
    Ok(())
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
        let requested = tokio::time::timeout(crate::hang_guard::HANG_GUARD, stream.recv())
            .await??
            .context("approval event missing")?;
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
async fn child_admission_and_followup_disable_unsupported_root_sandbox() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let admission = admit_child_fixture(&mut broker, "policy")?;
    let mut root = broker.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?;
    root.access.sandbox = true;
    assert!(root.access.sandbox);
    for provider in forge_providers::registry::descriptors()
        .into_iter()
        .filter(|entry| entry.selectable && entry.id != forge_providers::registry::default_id())
        .map(|entry| entry.id)
    {
        let (_routing, routing_fixture) = routing_fixture(provider.clone(), false)?;
        broker
            .agent_provider_factory
            .register(routing_fixture.provider_handle());
        save_child_profile(
            &broker,
            "unsandboxed",
            crate::agent::AgentProfile {
                provider: provider.as_str().into(),
                model: "fixture-model".into(),
                reasoning_effort: "low".into(),
                ..Default::default()
            },
        )?;
        let spawn = crate::agent::SpawnAgentRequest {
            profile: Some("unsandboxed".into()),
            text: "Inspect".into(),
        };
        let selected = broker
            .agent_provider_factory
            .resolve(&root, forge_providers::registry::default_id(), &spawn)
            .await?;
        assert!(!selected.access.sandbox);
        let mut agent = admission.agent.clone();
        agent.selection.as_mut().unwrap().provider = provider;
        assert!(
            !broker
                .agent_provider_factory
                .request(
                    &root,
                    &admitted_request_fixture(&broker, agent)?,
                    "Follow-up".into()
                )?
                .access
                .sandbox
        );
        assert!(root.access.sandbox);
    }
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
                    let text = match &request.input {
                        forge_provider_api::PromptInput::Text(text) => text.clone(),
                        _ => String::new(),
                    };
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
async fn cross_provider_children_resolve_profile_model_reasoning_and_service_tier() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (_routing, routing_fixture) =
        routing_fixture(forge_providers::registry::default_id(), false)?;
    broker
        .agent_provider_factory
        .register(routing_fixture.provider_handle());
    save_child_profile(
        &broker,
        "configured-child",
        crate::agent::AgentProfile {
            provider: forge_providers::registry::default_id().to_string(),
            model: "fixture-model".into(),
            reasoning_effort: "low".into(),
            service_tier: "fast".into(),
            ..Default::default()
        },
    )?;
    let root = broker.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?;
    let spawn = crate::agent::SpawnAgentRequest {
        profile: Some("configured-child".into()),
        text: "Inspect".into(),
    };
    let selection = broker
        .agent_provider_factory
        .resolve(
            &root,
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            &spawn,
        )
        .await?;
    assert_eq!(selection.model, "fixture-model");
    assert_eq!(selection.reasoning_effort, "low");
    assert_eq!(selection.context_window, None);
    assert_eq!(selection.service_tier, crate::provider::ServiceTier::Fast);
    assert_eq!(selection.access.sandbox, root.access.sandbox);
    assert_eq!(selection.approval_mode, root.approval_mode);
    Ok(())
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

#[tokio::test]
async fn rejected_followup_preserves_inbox_and_current_root_policy() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    // Each child shuts its adapter down when it settles, so every child gets a fresh adapter
    // over the same routing behavior. The adapter rejects the follow-up before sending it, so
    // the child turn fails without consuming input or invalidating its native binding.
    let retained_sessions = Arc::new(std::sync::Mutex::new(Vec::new()));
    broker.agent_provider_factory.register_builder(
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        Arc::new(move || {
            let native = Arc::new(RoutingProvider {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                turn: std::sync::atomic::AtomicUsize::new(0),
                request: Default::default(),
                reject_steering: false,
            });
            let rejecting = native.fixture().with_pre_dispatch(|request| async move {
                match &request.input {
                    forge_provider_api::PromptInput::Text(text)
                        if text.ends_with("CHILD_RESULT") =>
                    {
                        Ok(())
                    }
                    _ => Err(crate::provider::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Rejected,
                        ),
                    )),
                }
            });
            let session =
                crate::provider::test_support::ProviderTestSession::from_native(rejecting)?;
            let handle = session.provider_handle();
            retained_sessions.lock().unwrap().push(session);
            Ok(handle)
        }),
    );
    let admitted = broker
        .start_agent(json!({"profile":"worker","text":"CHILD_RESULT"}))
        .await?
        .0;
    let target = admitted["agent"]["id"].as_str().unwrap().to_owned();
    settle_managed_children(&mut broker).await?;
    broker
        .handle_agent_request(
            crate::agent::AgentRequest {
                operation_id: "inbox".into(),
                source: crate::agent::AgentSource::User,
                operation: crate::agent::AgentOperation::Message {
                    target: target.clone(),
                    message: "Retain this information".into(),
                },
            },
            None,
            &mut Vec::new(),
        )
        .await?;
    let mut agent = broker.agent_coordinator.resolve(&target)?;
    agent.selection.as_mut().unwrap().approval_mode = ApprovalMode::Allow;
    agent.selection.as_mut().unwrap().access.sandbox = false;
    let root = broker.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?;
    let request = broker.agent_provider_factory.request(
        &root,
        &admitted_request_fixture(&broker, agent.clone())?,
        "NEXT".into(),
    )?;
    assert_eq!(request.approval_mode, root.approval_mode);
    assert_eq!(request.access.sandbox, root.access.sandbox);
    assert_eq!(
        request.provider_session_id.map(String::from),
        agent.provider_thread_id
    );
    broker
        .handle_agent_request(
            crate::agent::AgentRequest {
                operation_id: "followup".into(),
                source: crate::agent::AgentSource::User,
                operation: crate::agent::AgentOperation::Followup {
                    target: target.clone(),
                    message: "NEXT".into(),
                },
            },
            None,
            &mut Vec::new(),
        )
        .await?;
    settle_managed_children(&mut broker).await?;
    let agent = broker.agent_coordinator.resolve(&target)?;
    assert_eq!(
        agent.inbox[0].delivery,
        crate::agent::MessageDelivery::Queued
    );
    assert_eq!(agent.inbox[0].task_id, None);
    assert_eq!(
        broker
            .store
            .list_agent_task(&broker.session.id)?
            .last()
            .unwrap()
            .state,
        ExchangeState::Failed
    );
    assert_eq!(broker.agent_coordinator.active_count(), 0);
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
                    let text = match &request.input {
                        forge_provider_api::PromptInput::Text(text) => text.clone(),
                        _ => String::new(),
                    };
                    input.lock().unwrap().push(text.clone());
                    if !text.starts_with("EXPLICIT_NEXT_TURN") {
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
                let text = match request.input {
                    forge_provider_api::PromptInput::Text(text) => text,
                    forge_provider_api::PromptInput::Skill { name, arguments } => {
                        format!("/{name} {arguments}")
                    }
                    forge_provider_api::PromptInput::QuestionReply(reply) => reply.question,
                };
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
        tokio::time::timeout(
            crate::hang_guard::HANG_GUARD,
            broker.run_interaction(
                "Research".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Research".into())),
            ),
        )
        .await??;
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
        for message in message.iter() {
            let payload: Value = serde_json::from_str(message.split_once('\n').unwrap().1)?;
            assert_eq!(payload.as_object().unwrap().len(), 5);
            assert!(
                payload.get("instructions").is_none() && payload.get("parent_reported").is_none()
            );
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

struct SuspendedParentProvider {
    blocked: AtomicBool,
    root_turn: std::sync::atomic::AtomicUsize,
    root_started: tokio::sync::Semaphore,
    child_started: tokio::sync::Semaphore,
    child_release: [tokio::sync::Semaphore; 2],
    input: std::sync::Mutex<Vec<String>>,
}

impl Default for SuspendedParentProvider {
    fn default() -> Self {
        Self {
            blocked: Default::default(),
            root_turn: Default::default(),
            root_started: tokio::sync::Semaphore::new(0),
            child_started: tokio::sync::Semaphore::new(0),
            child_release: std::array::from_fn(|_| tokio::sync::Semaphore::new(0)),
            input: Default::default(),
        }
    }
}

/// Give every child a fresh adapter over the shared fixture state, as production does, because a
/// child shuts its adapter down when it settles.
fn register_child_adapters(
    broker: &mut HarnessBroker,
    provider: &Arc<SuspendedParentProvider>,
    root: Arc<crate::provider::Provider>,
) {
    let provider = provider.clone();
    let retained = Arc::new(std::sync::Mutex::new(Vec::new()));
    broker.agent_provider_factory.register_builder(
        root.descriptor().kind,
        Arc::new(move || {
            let session = crate::provider::test_support::ProviderTestSession::from_native(
                provider.fixture(),
            )?;
            let handle = session.provider_handle();
            retained.lock().unwrap().push(session);
            Ok(handle)
        }),
    );
}

impl SuspendedParentProvider {
    fn fixture(self: &Arc<Self>) -> Arc<crate::provider::test_support::EventFixtureProvider> {
        let mut capability = crate::provider::test_support::fixture_capability();
        capability.native_steer = false;
        let descriptor = crate::provider::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            label: "Suspended parent".into(),
            capability,
        };
        let state = self.clone();
        crate::provider::test_support::EventFixtureProvider::new(
            descriptor.clone(),
            move |request, sink, start, mut cancelled| {
                let state = state.clone();

                async move {
                    start.observed(0).await?;

                    let text = match &request.input {
                        forge_provider_api::PromptInput::Text(text) => text.clone(),
                        _ => String::new(),
                    };
                    let response = if request.scope.owner().is_root() {
                        state.input.lock().unwrap().push(text.clone());
                        let ordinal = state.root_turn.fetch_add(1, Ordering::AcqRel);
                        if ordinal == 0 {
                            for name in ["ONE", "TWO"] {
                                crate::provider::test_support::call_tool(
                                    &request,
                                    "agent_spawn",
                                    json!({"profile":"worker","text":format!("CHILD_{name}")}),
                                )
                                .await?;
                            }
                        }
                        state.root_started.add_permits(1);
                        if state.blocked.load(Ordering::Acquire) && text == "Research both" {
                            crate::provider::test_support::call_tool(
                            &request,
                            "question_ask",
                            json!({"wait":true,"questions":[{"header":"Research scope","question":"Which research scope?","options":[],"allow_freeform":true}]}),
                        ).await?;
                        }
                        "PARENT_RESPONSE".to_owned()
                    } else {
                        let index = usize::from(text.ends_with("CHILD_TWO"));
                        state.child_started.add_permits(1);
                        tokio::select! {
                            _ = state.child_release[index].acquire() => {},
                            // A native interrupt reports a terminal outcome, as Codex does.
                            _ = cancelled.changed() => {
                                return Ok(forge_provider_api::TurnResult::new(
                                    forge_provider_api::turn::TurnOutcome::Interrupted,
                                    2,
                                    None,
                                    request.input_coverage(),
                                    Vec::new(),
                                    None,
                                )
                                .expect("fixture native coverage is valid"));
                            }
                        }
                        format!("RESULT_{}", if index == 0 { "ONE" } else { "TWO" })
                    };
                    let scope = request.scope.clone();

                    let address = crate::provider::TurnAddress {
                        thread_id: (scope.clone()).owner().thread_id(),
                        turn_id: (scope.clone()).turn_id().unwrap(),
                        scope: scope.clone(),
                    };

                    for event in completed_message(
                        &address,
                        "suspended-response",
                        crate::provider::TestMessageContent::Response,
                        response,
                    ) {
                        sink.send_wait(crate::provider::test_support::activity(event)?)
                            .await?;
                    }
                    let native_outcome = forge_provider_api::turn::TurnOutcome::Completed {
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
        )
    }
}

#[tokio::test]
async fn suspended_parent_joins_children_before_resuming() -> Result<()> {
    assert_suspended_parent_joins_children(false).await
}

#[tokio::test]
async fn user_input_wakes_suspended_parent_without_native_steering() -> Result<()> {
    assert_suspended_parent_joins_children(true).await
}

/// Suspends the parent behind two running children and checks that it resumes only after both
/// settle, or earlier through user input that cannot use native steering.
async fn assert_suspended_parent_joins_children(user_input: bool) -> Result<()> {
    {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let provider = Arc::new(SuspendedParentProvider::default());
        let fixture =
            crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
        broker.provider = fixture.provider_handle();
        register_child_adapters(&mut broker, &provider, fixture.provider_handle());
        broker.agent_coordinator.set_enabled(true);
        let coordinator = broker.agent_coordinator.clone();
        let input = broker.parent_input.clone();
        // Accepted steering must carry the journal receipt that production input always holds.
        let correction = if user_input {
            let catalog = broker.provider_catalog_request()?;
            Some(broker.submission_store.prepare_input(
                &catalog,
                ProviderInput::from_text("USER_CORRECTION"),
                broker.provider.descriptor().kind,
                Some("user-correction".into()),
            )?)
        } else {
            None
        };
        let task = tokio::spawn(async move {
            let result = broker
                .run_interaction(
                    "Research both".into(),
                    PromptMode::Chat,
                    Some(ExchangeAdmission::chat("Research both".into())),
                )
                .await;
            (broker, result)
        });
        tokio::time::timeout(
            crate::hang_guard::HANG_GUARD,
            provider.child_started.acquire_many(2),
        )
        .await??
        .forget();
        provider.root_started.acquire().await?.forget();
        provider.child_release[0].add_permits(1);
        tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
            while coordinator.active_count() != 1 {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        assert_eq!(
            provider.root_turn.load(Ordering::Acquire),
            1,
            "first child resumed the idle parent before its sibling settled"
        );
        if let Some(submission) = correction {
            tokio::time::timeout(
                crate::hang_guard::HANG_GUARD,
                input.steer(crate::provider::SteeringInput::User {
                    text: "USER_CORRECTION".into(),
                    attachments: Vec::new(),
                    submission: Some(submission),
                }),
            )
            .await??;
            tokio::time::timeout(
                crate::hang_guard::HANG_GUARD,
                provider.root_started.acquire(),
            )
            .await??
            .forget();
            assert_eq!(provider.root_turn.load(Ordering::Acquire), 2);
            assert_eq!(
                coordinator.active_count(),
                1,
                "user input cancelled the remaining child"
            );
        }
        provider.child_release[1].add_permits(1);
        let (broker, result) = tokio::time::timeout(crate::hang_guard::HANG_GUARD, task).await??;
        let (_, events) = result?;
        let statuses = events
            .iter()
            .filter_map(|event| event.payload.get("status"))
            .filter_map(|status| status.get("kind").and_then(Value::as_str))
            .collect::<Vec<_>>();
        assert!(statuses.contains(&"waiting_for_agent"), "{statuses:?}");
        let waiting = statuses
            .iter()
            .position(|status| *status == "waiting_for_agent")
            .unwrap();
        assert!(statuses[waiting + 1..].contains(&"working"), "{statuses:?}");
        assert_eq!(
            provider.root_turn.load(Ordering::Acquire),
            if user_input { 3 } else { 2 }
        );
        let inputs = provider.input.lock().unwrap();
        for result in ["RESULT_ONE", "RESULT_TWO"] {
            assert_eq!(
                inputs
                    .iter()
                    .map(|input| input.matches(result).count())
                    .sum::<usize>(),
                1
            );
        }
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].state(), ExchangeState::Complete);
        assert_eq!(exchange[0].node_list.iter().filter(|node|matches!(node,
            crate::exchange::ExchangeNode::ExchangeInput {prompt} if prompt.text=="USER_CORRECTION")).count(),usize::from(user_input));
        assert!(
            broker
                .store
                .list_agent_task(&broker.session.id)?
                .iter()
                .all(|task| task.parent_reported)
        );
    }
    Ok(())
}

#[tokio::test]
async fn parent_cancellation_prevents_child_continuation() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = Arc::new(SuspendedParentProvider::default());
    let fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
    broker.provider = fixture.provider_handle();
    register_child_adapters(&mut broker, &provider, fixture.provider_handle());
    broker.agent_coordinator.set_enabled(true);
    let cancellation = broker.turn_cancellation();
    let coordinator = broker.agent_coordinator.clone();
    let task = tokio::spawn(async move {
        let result = broker
            .run_interaction(
                "Research both".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Research both".into())),
            )
            .await;
        (broker, result)
    });
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        provider.child_started.acquire_many(2),
    )
    .await??
    .forget();
    provider.root_started.acquire().await?.forget();
    let quiescence = coordinator.begin_quiescence();
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        coordinator.sender().request(crate::agent::AgentRequest {
            operation_id: "cancel-children".into(),
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::InterruptAll,
        }),
    )
    .await??;
    assert_eq!(provider.root_turn.load(Ordering::Acquire), 1);
    cancellation.request();
    let (mut broker, result) = tokio::time::timeout(crate::hang_guard::HANG_GUARD, task).await??;
    let error = result.unwrap_err();
    assert!(error.is::<TurnCancelled>(), "{error:#}");
    provider.child_release[0].add_permits(1);
    provider.child_release[1].add_permits(1);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while broker.agent_coordinator.active_count() != 0 {
            broker.drain_agent_commands(None).await?;
            tokio::task::yield_now().await;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await??;
    assert_eq!(provider.root_turn.load(Ordering::Acquire), 1);
    assert!(!broker.parent_input.is_active());
    assert_eq!(broker.agent_coordinator.unreported_result().len(), 2);
    drop(quiescence);
    Ok(())
}

/// A waiting question holds the parent's tool call, so children that settle meanwhile must not
/// resume the parent. The answer releases the call and the parent then receives each child
/// result exactly once.
#[tokio::test]
async fn user_decision_holds_parent_until_answered_before_delivering_child_results() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = Arc::new(SuspendedParentProvider::default());
    provider.blocked.store(true, Ordering::Release);
    let fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
    broker.provider = fixture.provider_handle();
    register_child_adapters(&mut broker, &provider, fixture.provider_handle());
    broker.agent_coordinator.set_enabled(true);
    let coordinator = broker.agent_coordinator.clone();
    let questions = broker.questions.clone();
    let task = tokio::spawn(async move {
        let result = broker
            .run_interaction(
                "Research both".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Research both".into())),
            )
            .await;
        (broker, result)
    });
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        provider.child_started.acquire_many(2),
    )
    .await??
    .forget();
    provider.root_started.acquire().await?.forget();
    provider.child_release[0].add_permits(1);
    provider.child_release[1].add_permits(1);
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while coordinator.active_count() != 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert_eq!(
        provider.root_turn.load(Ordering::Acquire),
        1,
        "settled children resumed a parent that was waiting for the user"
    );
    assert!(
        !task.is_finished(),
        "parent finished while its question was unanswered"
    );
    let pending = tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            let snapshot = questions.snapshot()?;
            if let Some(question) = snapshot
                .questions
                .iter()
                .find(|question| question.pending())
            {
                return Ok::<_, anyhow::Error>((question.id.clone(), question.content.revision));
            }
            tokio::task::yield_now().await;
        }
    })
    .await??;
    questions.apply(crate::question::QuestionCommand {
        operation_id: "answer-scope".into(),
        question_id: pending.0,
        revision: pending.1,
        action: crate::question::QuestionAction::Answer {
            answer: crate::question::QuestionAnswer::Other {
                text: "Both".into(),
            },
        },
    })?;
    let (broker, result) = tokio::time::timeout(crate::hang_guard::HANG_GUARD, task).await??;
    result?;
    let inputs = provider.input.lock().unwrap();
    for result in ["RESULT_ONE", "RESULT_TWO"] {
        assert_eq!(
            inputs
                .iter()
                .map(|input| input.matches(result).count())
                .sum::<usize>(),
            1,
            "{result} was not delivered to the parent exactly once"
        );
    }
    assert!(
        broker
            .store
            .list_agent_task(&broker.session.id)?
            .iter()
            .all(|task| task.parent_reported)
    );
    Ok(())
}

/// Interrupting one child while its parent waits keeps the parent open until its sibling settles.
#[tokio::test]
async fn interrupted_child_does_not_finalize_parent_before_running_sibling_settles() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = Arc::new(SuspendedParentProvider::default());
    let fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
    broker.provider = fixture.provider_handle();
    register_child_adapters(&mut broker, &provider, fixture.provider_handle());
    broker.agent_coordinator.set_enabled(true);
    let coordinator = broker.agent_coordinator.clone();
    let task = tokio::spawn(async move {
        let result = broker
            .run_interaction(
                "Research both".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Research both".into())),
            )
            .await;
        (broker, result)
    });
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        provider.child_started.acquire_many(2),
    )
    .await??
    .forget();
    provider.root_started.acquire().await?.forget();
    let first_child = coordinator
        .snapshot(None)
        .into_iter()
        .find(|(_, task)| {
            task.as_ref()
                .is_some_and(|task| task.message.ends_with("CHILD_ONE"))
        })
        .map(|(agent, _)| agent.id)
        .context("first child was not admitted")?;
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        coordinator.sender().request(crate::agent::AgentRequest {
            operation_id: "interrupt-first-child".into(),
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::Interrupt {
                target: first_child.clone(),
            },
        }),
    )
    .await??;
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while coordinator.active_count() != 1 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert!(
        !task.is_finished(),
        "parent finalized before its sibling settled"
    );
    provider.child_release[0].add_permits(1);
    provider.child_release[1].add_permits(1);
    let (broker, result) = tokio::time::timeout(crate::hang_guard::HANG_GUARD, task).await??;
    result?;
    let tasks = broker.store.list_agent_task(&broker.session.id)?;
    assert_eq!(tasks.len(), 2);
    assert!(
        tasks
            .iter()
            .any(|task| task.agent_id != first_child && task.state == ExchangeState::Complete),
        "{tasks:?}"
    );
    let exchange = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].state(), ExchangeState::Complete);
    Ok(())
}

/// Count the reducer's deferred native-result reads retained in the local trace.
fn native_ready_reads(broker_data: &std::path::Path, trace: &crate::trace::TraceStore) -> usize {
    trace.flush().unwrap();
    std::fs::read_to_string(
        crate::storage::state::directory(broker_data).join("harness-trace.jsonl"),
    )
    .unwrap_or_default()
    .lines()
    .filter(|line| line.contains("\"exchange.event.read\"") && line.contains("\"NativeReady\""))
    .count()
}

/// A root result deferred by child quiescence stays parked until quiescence ends, then completes.
/// Two workers keep a regressed hot reducer from starving this observer into a hang.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn quiescence_parks_the_deferred_root_result_until_release() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = Arc::new(SuspendedParentProvider::default());
    let fixture =
        crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
    broker.provider = fixture.provider_handle();
    register_child_adapters(&mut broker, &provider, fixture.provider_handle());
    broker.agent_coordinator.set_enabled(true);
    broker.trace.configure(true)?;
    let trace = broker.trace.clone();
    let coordinator = broker.agent_coordinator.clone();
    let quiescence = coordinator.begin_quiescence();
    let task = tokio::spawn(async move {
        let result = broker
            .run_interaction(
                "Research both".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Research both".into())),
            )
            .await;
        (broker, result)
    });
    provider.root_started.acquire().await?.forget();
    let parked = tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            let reads = native_ready_reads(data.path(), &trace);
            if reads > 0 {
                return reads;
            }
            tokio::task::yield_now().await;
        }
    })
    .await?;
    for _ in 0..200 {
        tokio::task::yield_now().await;
    }
    assert_eq!(
        native_ready_reads(data.path(), &trace),
        parked,
        "the deferred root result was re-read while quiescence persisted"
    );
    assert!(!task.is_finished());
    // Quiescence rejects the spawns, so release alone must let the parked root finalize.
    drop(quiescence);
    let (broker, result) = tokio::time::timeout(crate::hang_guard::HANG_GUARD, task).await??;
    result?;
    let exchange = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].state(), ExchangeState::Complete);
    Ok(())
}
