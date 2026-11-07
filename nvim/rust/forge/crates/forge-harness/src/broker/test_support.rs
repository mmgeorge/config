use super::super::{
    Exchange, GoalRecord, GoalState, HarnessBroker, InitializeRequest, PlanExecutionRecord,
    PlanState, ProviderEvent, ProviderLaunch, SessionEvent,
};
use super::{FixedClock, PlanFixtureBehavior, git, plan_fixture_provider, save_check_exchange};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::Ordering;
use tokio::sync::Notify;

/// Control native preparation and rejection barriers without timing-dependent assertions.
pub(super) struct PreparationProbe {
    pub(super) started: Notify,
    pub(super) release: Notify,
    pub(super) fail: bool,
}

/// Create normalized completed message observations for a supplied turn.
pub(super) fn completed_message(
    turn: &crate::provider::TurnAddress,
    id: impl Into<String>,
    role: crate::provider::TestMessageContent,
    text: impl Into<String>,
) -> Vec<ProviderEvent> {
    crate::provider::test_support::message(
        turn,
        format!("{}:{}", turn.turn_id.as_str(), id.into()),
        Some(role),
        text,
    )
}

/// Create native input with independently controlled checkpoint, execution, and cleanup barriers.
pub(super) fn checkpoint_fixture(
    workspace: PathBuf,
    preparation: Option<Arc<PreparationProbe>>,
    started: Arc<tokio::sync::Notify>,
    tool_ran: Arc<std::sync::atomic::AtomicBool>,
    active: Arc<std::sync::atomic::AtomicUsize>,
    native_rejection: Option<Arc<PreparationProbe>>,
    cleanup_error: bool,
) -> Result<crate::provider::test_support::ProviderTestSession> {
    struct ActiveProbe(Arc<std::sync::atomic::AtomicUsize>);
    impl Drop for ActiveProbe {
        fn drop(&mut self) {
            self.0.fetch_sub(1, Ordering::SeqCst);
        }
    }
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized checkpoint fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(descriptor.clone(), {
        let started = started.clone();
        let tool_ran = tool_ran.clone();
        let active = active.clone();
        move |request, _writer, start, _cancelled| {
            let started = started.clone();
            let tool_ran = tool_ran.clone();
            let active = active.clone();
            let workspace = workspace.clone();
            async move {
                active.fetch_add(1, Ordering::SeqCst);
                let _active = ActiveProbe(active);
                start.observed(0).await?;
                started.notify_one();
                if let Err(error) = request.turn_control.before_send(&request.scope).await {
                    if matches!(error, crate::provider::ProviderError::Cancelled) {
                        return Ok(forge_provider_api::TurnResult::new(
                            forge_provider_api::turn::TurnOutcome::Interrupted,
                            1,
                            None,
                            vec![forge_provider_api::CoveredInput::new(
                                request.submission_id,
                                request.attempt_id,
                            )],
                            Vec::new(),
                            Some(
                                forge_provider_api::CheckpointId::new("fixture-native-boundary")
                                    .unwrap(),
                            ),
                        )
                        .expect("fixture has one admitted input and no conflicting accounting"));
                    }
                    return Err(error);
                }
                tool_ran.store(true, Ordering::SeqCst);
                std::fs::write(workspace.join("seed.txt"), "tool changed workspace\n").map_err(
                    |_| {
                        crate::provider::ProviderError::Failure(
                            forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::TurnStart,
                                forge_provider_api::ProviderFailureKind::Unknown,
                            ),
                        )
                    },
                )?;
                Ok(forge_provider_api::TurnResult::new(
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                    2,
                    None,
                    vec![forge_provider_api::CoveredInput::new(
                        request.submission_id,
                        request.attempt_id,
                    )],
                    Vec::new(),
                    None,
                )
                .expect("fixture has one admitted input and no conflicting accounting"))
            }
        }
    });
    let fixture = fixture.with_pre_dispatch(move |_request| {
        let preparation = preparation.clone();
        let native_rejection = native_rejection.clone();
        async move {
            let (probe, kind) = match (preparation, native_rejection) {
                (Some(probe), _) => (
                    Some(probe),
                    Some(forge_provider_api::ProviderFailureKind::Runtime),
                ),
                (_, Some(probe)) => (
                    Some(probe),
                    Some(forge_provider_api::ProviderFailureKind::Rejected),
                ),
                _ => (None, None),
            };
            if let (Some(probe), Some(kind)) = (probe, kind) {
                probe.started.notify_one();
                probe.release.notified().await;
                if probe.fail || kind == forge_provider_api::ProviderFailureKind::Rejected {
                    return Err(crate::provider::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            kind,
                        ),
                    ));
                }
            }
            Ok(())
        }
    });
    let fixture = if cleanup_error {
        fixture.with_cleanup(|_| async {
            Err(crate::provider::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnInterrupt,
                    forge_provider_api::ProviderFailureKind::Transport,
                ),
            ))
        })
    } else {
        fixture
    };
    crate::provider::test_support::ProviderTestSession::from_native(fixture)
}

/// Drain managed child commands through the coordinator completion notification.
pub(super) async fn settle_managed_children(
    broker: &mut HarnessBroker,
) -> Result<Vec<SessionEvent>> {
    let mut revision = broker.agent_coordinator.subscribe();
    let mut event = Vec::new();
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            event.extend(broker.drain_agent_commands(None).await?);
            if broker.agent_coordinator.active_count() == 0 {
                return Ok::<_, anyhow::Error>(());
            }
            revision.changed().await.context("child lane closed")?;
        }
    })
    .await
    .context("managed child did not settle within five seconds")??;
    Ok(event)
}

/// Create an accepted execution workflow with persisted check definitions and baseline evidence.
pub(super) async fn check_fixture(
    repository: &Path,
    data: &Path,
    commands: Vec<String>,
    manual: bool,
) -> HarnessBroker {
    let mut broker = planning_question_broker(repository, data, false);
    let mut document = crate::plan::test_fixture("check-plan", "Check fixture");
    let mut design = crate::plan::DeclarationDesign::default();
    design.document.objective = "Check accepted commands".into();
    design.document.background = "A repository fixture".into();
    design.document.design = "Harness owns the command gate".into();
    let config = crate::checks::CheckConfig {
        version: 2,
        definitions: commands
            .into_iter()
            .enumerate()
            .map(|(index, command)| crate::checks::CheckDefinition {
                auto: true,
                id: format!("check-{index}"),
                name: format!("Check {index}"),
                category: crate::checks::CheckCategory::Check,
                purpose: crate::checks::CheckPurpose::Verify,
                enabled: true,
                area: "Fixture".into(),
                trigger: crate::checks::CheckTrigger::AnyChange,
                scope: Default::default(),
                directory: ".".into(),
                all: crate::checks::CheckOperation {
                    input: false,
                    args: None,
                    inputs: None,
                    command,
                },
                files: None,
                cases: None,
                depends_on: Vec::new(),
            })
            .collect(),
    };
    std::fs::write(
        repository.join(".forge.json"),
        serde_json::to_vec(&json!({"checks":config})).unwrap(),
    )
    .unwrap();
    design.document.verification = if manual {
        vec!["Observe manual result".into()]
    } else {
        Vec::new()
    };
    design
        .proposed
        .insert(".gitignore".into(), "/target/\n".into());
    document.design = Some(design);
    let mut plan = broker
        .plan_file
        .prepare_entry(&broker.session.id, "Run checks".into(), Some(document), 1)
        .unwrap()
        .record;
    let (_, _, accepted_digest) = broker
        .plan_file
        .submit_document_revision(&broker.session.id, "check-plan", 1, 1)
        .unwrap();
    plan.state = PlanState::Accepted;
    plan.model_revision = 1;
    plan.accepted_revision = Some(1);
    plan.review_digest = Some(accepted_digest.clone());
    plan.accepted_digest = Some(accepted_digest);
    plan.submitted_version = Some(plan.document_version);
    broker.store.save_plan(&plan).unwrap();
    std::fs::write(repository.join(".gitignore"), "/target/\n").unwrap();
    let goal = GoalRecord {
        id: "check-goal".into(),
        session_id: broker.session.id.clone(),
        objective: "Run checks".into(),
        state: GoalState::Active,
        continuation: Default::default(),
        created_at_ms: 1,
        updated_at_ms: 1,
    };
    broker.save_session().unwrap();
    let baseline = broker.prepare_workspace_capture().run().await.unwrap();
    broker.store.save_checkpoint(&baseline).unwrap();
    std::fs::write(
        repository.join(".gate-input"),
        "Exercise the automated gate\n",
    )
    .unwrap();
    let execution:PlanExecutionRecord=serde_json::from_value(json!({
        "baseline_checkpoint":baseline.id,
        "manual_verification":if manual { vec!["Observe manual result"] } else { Vec::new() },
        "id":"check-execution","session_id":broker.session.id,"plan_id":"check-plan","goal_id":goal.id,
        "state":"active","phase":"checking","original_revision":1,"revision":1,"generation":0,
        "lifecycle":[],"findings":[],"progress":crate::plan::execution::SemanticProgress::default(),
        "verification":[],"check_runs":[],"revision_history":[],"resume_after_review":false,
        "observed_source":{},"observed_document_version":0,"observed_check":[],"created_at_ms":1
    })).unwrap();
    broker.store.save_plan_execution(&execution).unwrap();
    let task = crate::task::Task::execute(
        "Run checks".into(),
        execution,
        crate::session::continuation::ContinuationBudget::new(20, 2),
        1,
    )
    .unwrap();
    broker
        .task_controller
        .change(
            &mut broker.store,
            None,
            crate::task::TaskChange::Install(Box::new(task)),
            1,
        )
        .unwrap();
    save_check_exchange(&mut broker, "check-exchange", true).unwrap();
    broker
}

/// Create a committed Git repository with deterministic fixture identity and line endings.
pub(in crate::broker) fn repository() -> tempfile::TempDir {
    let temporary = tempfile::tempdir().unwrap();
    git(temporary.path(), &["init", "-q"]);
    git(temporary.path(), &["config", "core.autocrlf", "false"]);
    git(
        temporary.path(),
        &["config", "user.email", "harness@example.invalid"],
    );
    git(temporary.path(), &["config", "user.name", "Harness Test"]);
    std::fs::write(temporary.path().join("seed.txt"), "seed\n").unwrap();
    git(temporary.path(), &["add", "."]);
    git(temporary.path(), &["commit", "-qm", "seed"]);
    temporary
}

/// Create a persisted broker with normalized planning and question behavior.
pub(in crate::broker) fn planning_question_broker(
    repository: &Path,
    data_root: &Path,
    structured: bool,
) -> HarnessBroker {
    crate::checks::configure_fixture(repository);
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data_root.to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.to_string_lossy().into_owned(),
            client_id: "planning-question-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    broker.provider = plan_fixture_provider(&broker, PlanFixtureBehavior::Questions { structured });
    broker
}

/// Commit one child admission and its durable parent reference.
pub(super) fn admit_child_fixture(
    broker: &mut HarnessBroker,
    name: &str,
) -> Result<crate::agent::AgentWork> {
    let mut parent = Exchange::agent_admission(
        &broker.session.id,
        broker.store.next_exchange_ordinal(&broker.session.id)?,
        "Admission".into(),
        100,
    );
    let spawn = crate::agent::SpawnAgentRequest {
        profile: Some(name.into()),
        text: "Inspect".into(),
    };
    let request = crate::agent::AgentRequest {
        operation_id: name.into(),
        source: crate::agent::AgentSource::User,
        operation: crate::agent::AgentOperation::Spawn(spawn.clone()),
    };
    let selection = crate::agent::AgentSelection {
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
    };
    let admission = broker
        .agent_coordinator
        .reserve_spawn(&request, &spawn, selection, &parent.id, 100)?;
    parent.append_agent_task(&admission.task)?;
    broker.store.admit_agent_task(
        &admission.agent,
        &admission.task,
        &parent,
        admission.continuation_authorization(),
    )?;
    let admission = match admission.commit()? {
        crate::agent::AgentCommit::Admitted(work) => work,
        crate::agent::AgentCommit::Existing(_) => anyhow::bail!("fixture reused a child admission"),
    };
    broker.agent_registry.insert(admission.agent.clone());
    Ok(admission)
}

/// Admit one fixture's original child owner and invocation before producing native facts.
pub(super) fn child_address(
    owner: &crate::provider::ThreadOwner,
    task_id: &str,
) -> crate::provider::TurnAddress {
    crate::provider::test_support::address(&crate::provider::InvocationScope {
        thread_id: forge_provider_api::ThreadId::from(owner.clone()),
        exchange_id: task_id.into(),
        invocation_id: format!("fixture:{task_id}").into(),
    })
}

/// Create a child turn observation for the shared fixture address.
pub(super) fn child_event(
    address: &crate::provider::TurnAddress,
    update: crate::provider::TurnEvent,
) -> ProviderEvent {
    ProviderEvent::Turn {
        thread_id: address.thread_id.clone(),
        turn_id: address.turn_id.clone(),
        event: update,
    }
}

/// Collect a native child outcome through the shared runner for reducer fixtures.
pub(super) fn child_completed(
    address: &crate::provider::TurnAddress,
    outcome: forge_provider_api::turn::TurnOutcome,
    completed_at_ms: i64,
) -> ProviderEvent {
    let result = forge_provider_api::TurnResult::new(
        outcome,
        completed_at_ms,
        None,
        Vec::new(),
        Vec::new(),
        None,
    )
    .expect("fixture has no conflicting input or accounting records");
    let completed = crate::provider::test_support::completed_turn(address.scope.clone(), result)
        .expect("deterministic fixture native resources collect successfully");
    child_event(address, crate::provider::TurnEvent::Completed(completed))
}

/// Create a child tool lifecycle sequence for the shared fixture address.
pub(super) fn child_tool(
    address: &crate::provider::TurnAddress,
    output: String,
    status: crate::provider::ToolStatus,
) -> Vec<ProviderEvent> {
    let start =
        forge_provider_api::ItemStart::CommandExecution(forge_provider_api::ToolDescriptor {
            semantics: Default::default(),
            name: "inspect".into(),
            title: "Inspect compilation diagnostics".into(),
            arguments: forge_provider_api::InspectableArguments::new(Value::Null),
        });
    let updates =
        crate::turn::test_support::tool_updates(&start, status, Some(output), Default::default());
    crate::provider::test_support::tool(
        address,
        format!("{}:same-provider-call", address.turn_id.as_str()),
        start,
        updates,
    )
}

impl HarnessBroker {
    /// Bind normalized fixture content to the admitted child before applying its observations.
    pub(super) async fn observe_child_fixture(
        &mut self,
        owner: crate::provider::ThreadOwner,
        task_id: &str,
        batch: impl Into<crate::provider::test_support::FixtureBatch>,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        use crate::provider::test_support::FixtureBatch;
        let scope = crate::provider::InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(owner.clone()),
            exchange_id: task_id.into(),
            invocation_id: format!("fixture:{task_id}").into(),
        };
        let updates = match batch.into() {
            FixtureBatch::Event(event) => {
                vec![crate::provider::session_execution::OwnerObservation::Event(
                    crate::provider::test_support::native_event(scope.clone(), *event),
                )]
            }
            FixtureBatch::Sequence(events) => events
                .into_iter()
                .map(|event| {
                    crate::provider::session_execution::OwnerObservation::Event(
                        crate::provider::test_support::native_event(scope.clone(), event),
                    )
                })
                .collect(),
            FixtureBatch::Observation(event) => vec![*event],
            FixtureBatch::Observations(events) => events,
        };
        for update in updates {
            self.observe_managed_agent(owner.clone(), task_id, update, event)
                .await?;
        }
        Ok(())
    }
}
