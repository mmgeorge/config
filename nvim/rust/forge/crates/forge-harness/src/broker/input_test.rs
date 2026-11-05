use super::super::{
    ChecklistTracker, ExchangeAdmission, ExchangeNode, ExchangeRuntime, ExchangeState,
    ForegroundAction, ForegroundCollection, ForegroundDispatch, ForegroundJob, ForegroundSignal,
    HarnessBroker, NativeForegroundSignal, PromptMode, ProviderAttempt, ProviderCompletion,
    ProviderError, ProviderEvent, ProviderInput, Request, SessionEventKind, SettlementRequirement,
    SqliteStore, TimelineEntry, publication,
};
use super::test_support::{completed_message, planning_question_broker, repository};
use anyhow::{Context, Result};
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::Ordering;

#[tokio::test]
async fn ordinary_input_does_not_activate_a_pending_plan_entry() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let prepared =
        broker
            .plan_file
            .prepare_entry(&broker.session.id, "Pending plan".into(), None, 1)?;
    let mut task = crate::task::Task::plan(prepared.record, 1)?;
    task.request_entry(prepared.instruction, 1)?;
    let pending = broker
        .task_controller
        .change(
            &mut broker.store,
            None,
            crate::task::TaskChange::Install(Box::new(task)),
            1,
        )?
        .unwrap();
    let response = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Explain the current approach"}),
        })
        .await;
    assert!(
        response.response.error().is_none(),
        "{:?}",
        response.response.error()
    );
    let current = broker.task_controller.current()?.unwrap();
    assert_eq!(current.revision, pending.revision);
    assert!(!current.entry_accepted());
    let crate::task::WorkflowSnapshot::Plan { plan } = current.workflow.clone() else {
        unreachable!()
    };
    assert_eq!(plan.artifact.state, crate::plan::PlanState::Generating);
    let exchange = broker.store.latest_exchange(&broker.session.id)?.unwrap();
    assert_eq!(exchange.prompt, "Explain the current approach");
    assert_eq!(exchange.origin, crate::exchange::ExchangeOrigin::User);
    Ok(())
}

/// Reject new steering as soon as the owner reads native completion, before checkpoint settlement.
#[tokio::test]
async fn native_ready_closes_foreground_steering_admission() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.checkpoint = None;
    broker.turn_cancellation.arm();
    let mut staged = broker.stage_foreground_interaction(
        "finish",
        PromptMode::Chat,
        Some(&ExchangeAdmission::chat("finish".into())),
        ProviderInput::from_text("finish"),
    )?;
    broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
    let engine = broker
        .execution_registry()
        .owner(crate::provider::ExecutionOwner::root(&broker.session.id))
        .await?;
    let mut transaction = broker.construct_foreground_transaction(&staged, engine)?;
    transaction.scan_enabled = false;
    transaction.question_enabled = false;
    transaction.agent_command_active = true;
    transaction.execution_open = false;
    transaction.native.pending_submission = Some(Box::pin(async {
        Ok(ProviderAttempt::Ready(Box::new(Ok(
            ProviderCompletion::default(),
        ))))
    }));
    assert!(transaction.accepts_steering());
    transaction.native.authorize_submission();
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            if matches!(
                transaction.next().await?,
                ForegroundSignal::Native(NativeForegroundSignal::NativeReady(_))
            ) {
                break Ok::<(), anyhow::Error>(());
            }
        }
    })
    .await??;
    assert!(!transaction.accepts_steering());
    // A result owned by a steering effect must settle before terminal evidence is re-exposed.
    let flight = transaction.steering.pending_deliveries.retain();
    transaction.pending_native_ready = Some(Ok(ProviderCompletion::default()));
    assert!(futures_util::FutureExt::now_or_never(transaction.next()).is_none());
    drop(flight);
    assert!(matches!(
        tokio::time::timeout(std::time::Duration::from_secs(1), transaction.next()).await??,
        ForegroundSignal::Native(NativeForegroundSignal::NativeReady(_))
    ));
    transaction.acknowledge_native_result();
    transaction.collection = Some(ForegroundCollection::Restart);
    let (sink, _stream) = publication::channel();
    let ForegroundDispatch::Admitting(admission) = broker.begin_foreground_dispatch(
        Request {
            id: 9,
            method: "session.prompt.submit".into(),
            params: json!({"text":"finish"}),
        },
        sink,
    )?
    else {
        unreachable!()
    };
    let mut dispatch = ForegroundDispatch::Running {
        transaction: Box::new(transaction),
        admission,
    };
    assert!(matches!(
        broker.step_foreground(
            &mut dispatch,
            ForegroundSignal::EffectFailed(
                ProviderError::PreparationFailed(Box::new(ProviderError::Cancelled)).into(),
            )
        )?,
        ForegroundAction::Wait
    ));
    let ForegroundDispatch::Running { transaction, .. } = &dispatch else {
        unreachable!()
    };
    assert!(!transaction.failure_cleanup_active);
    assert!(matches!(
        transaction.pending_native_ready,
        Some(Err(ProviderError::Cancelled))
    ));
    let _quiescence = broker.agent_coordinator.begin_quiescence();
    broker.step_foreground(
        &mut dispatch,
        ForegroundSignal::Native(NativeForegroundSignal::NativeReady(ProviderAttempt::Ready(
            Box::new(Ok(ProviderCompletion::default())),
        ))),
    )?;
    assert!(matches!(dispatch, ForegroundDispatch::Finalizing { .. }));
    Ok(())
}

/// Require native settlement unless collected receipt evidence proves the input was never sent.
#[tokio::test]
async fn collected_undispatched_settlement_preserves_missing_ticket_failure() -> Result<()> {
    for (requirement, expected_success) in [
        (SettlementRequirement::Native, false),
        (SettlementRequirement::CollectedUndispatched, true),
    ] {
        let cell = Arc::new(crate::provider::session_execution::SettlementCell::default());
        cell.seal_effects(&[])?;
        let signal = ForegroundJob::SettlementCommit {
            cell,
            requirement,
            undispatched: Vec::new(),
        }
        .run()
        .await?;
        let ForegroundSignal::SettlementCommitted(result) = signal else {
            unreachable!()
        };
        assert_eq!(result.is_ok(), expected_success);
    }
    Ok(())
}

struct BoundarySteeringProvider {
    capability: crate::provider::ProviderCapability,
    initial_started: Arc<tokio::sync::Semaphore>,
    initial_release: Arc<tokio::sync::Semaphore>,
    steering_deferred: Arc<tokio::sync::Semaphore>,
    continuation_started: Arc<tokio::sync::Semaphore>,
    continuation_accept: Arc<tokio::sync::Semaphore>,
    continuation_release: Arc<tokio::sync::Semaphore>,
    input: Arc<std::sync::Mutex<Vec<String>>>,
    fail_response: Arc<std::sync::atomic::AtomicBool>,
}

fn task_start_fixture(
    conflicting_replay: bool,
) -> Arc<crate::provider::test_support::EventFixtureProvider> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: crate::provider::ProviderId::new("test-task-start").expect("fixture ID is valid"),
        label: "Normalized task-start fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, _cancelled| async move {
            let scope = request.scope.clone();
            let address = crate::provider::TurnAddress {
                scope: scope.clone(),
                thread_id: "task-start-thread".into(),
                turn_id: "task-start-turn".into(),
            };
            let completion = forge_provider_api::completion::ProviderCompletion {
                provider_session_id: Some(
                    "task-start-thread"
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
                        address: address.clone(),
                    },
                )?,
            })
            .await?;
            sink.send_wait(ProviderEvent::Turn {
                address: address.clone(),
                update: crate::provider::TurnUpdate::Started { started_at_ms: 0 },
            })
            .await?;
            let start = forge_provider_api::input::TaskStart {
                kind: forge_provider_api::input::TaskKind::Goal,
                title: "Reduce startup time".into(),
            };
            let mut acknowledgement = forge_provider_api::InputAcknowledgement::TaskStarted {
                input_id: Some("task-entry".into()),
                start,
                prompt: "Complete startup work.\nVerify the result.".into(),
            };
            sink.send_wait(ProviderEvent::InputAccepted {
                scope: scope.clone(),
                acknowledgement: acknowledgement.clone(),
            })
            .await?;
            if conflicting_replay {
                if let forge_provider_api::InputAcknowledgement::TaskStarted { prompt, .. } =
                    &mut acknowledgement
                {
                    *prompt = "Conflicting instruction".into();
                }
                sink.send_wait(ProviderEvent::InputAccepted {
                    scope: scope.clone(),
                    acknowledgement,
                })
                .await?;
            }
            sink.send_wait(ProviderEvent::Turn {
                address: address.clone(),
                update: crate::provider::TurnUpdate::Finished {
                    outcome: crate::turn::TurnOutcome::Completed,
                    completed_at_ms: 1,
                },
            })
            .await?;
            sink.send_wait(ProviderEvent::Execution {
                observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                    scope,
                    forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                        address,
                        covered_submissions: vec![request.submission_id],
                        completed_at_ms: 1,
                        result: forge_provider_api::lifecycle::ExecutionResult::Completed {
                            completion: completion.into(),
                        },
                    },
                )?,
            })
            .await?;
            Ok(())
        },
    )
}
#[tokio::test]
async fn acknowledged_task_start_is_retained_once_inside_its_exchange() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let native = task_start_fixture(false);
    let fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    broker.provider = fixture.provider_handle();
    tokio::time::timeout(
        std::time::Duration::from_secs(10),
        broker.run_interaction(
            "Inspect startup".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Inspect startup".into())),
        ),
    )
    .await
    .context("task-start fixture did not settle")??;
    let snapshot = broker.snapshot()?;
    assert_eq!(snapshot.exchange.len(), 1);
    assert_eq!(
        snapshot.exchange[0].origin,
        crate::exchange::ExchangeOrigin::User
    );
    assert_eq!(snapshot.exchange[0].prompt, "Inspect startup");
    let TimelineEntry::Exchange { exchange, .. } = &snapshot.timeline[0] else {
        panic!("task start created a separate timeline entry")
    };
    let events = exchange
        .node_list
        .iter()
        .filter_map(|node| match node {
            ExchangeNode::TimelineEvent { event } => match event.as_ref() {
                crate::timeline::TimelineEvent::Session { event } => Some(event),
                _ => None,
            },
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(events.len(), 1);
    assert_eq!(events[0].owner.exchange_id(), Some(exchange.id.as_str()));
    assert!(
        matches!(&events[0].detail, SessionEventKind::TaskStarted { start, prompt }
        if start.title == "Reduce startup time" && prompt == "Complete startup work.\nVerify the result.")
    );
    assert!(
        !exchange
            .node_list
            .iter()
            .any(|node| matches!(node, ExchangeNode::ExchangeInput { .. }))
    );
    let restored = SqliteStore::open(data.path())?;
    let persisted = restored
        .load_session_event(
            &broker.session.id,
            &format!("{}:task-start:task-entry", broker.session.id),
        )?
        .unwrap();
    assert_eq!(persisted.owner, events[0].owner);
    Ok(())
}

/// Keep foreground state owned across service turns and reject foreign invocation events.
#[tokio::test]
async fn owned_foreground_applies_events_and_rejects_foreign_scope() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (exchange, _, _) = broker
        .test_stage_interaction_for_turn("Inspect", true, 1)
        .await?;
    broker.store.save_exchange(&exchange)?;
    let scope = crate::provider::InvocationScope::new(
        crate::provider::ExecutionOwner::root(&broker.session.id),
        exchange.id.clone(),
    );
    broker.exchange_runtime = Some(ExchangeRuntime {
        task_permit: None,
        checkpoint: None,
        tool_gate: None,
        exchange_id: exchange.id.clone(),
        checklist: ChecklistTracker::default(),
        retraction_eligible: true,
        active_wait: None,
    });
    let mut transaction = broker.begin_owned_foreground(exchange, scope.clone())?;
    assert!(broker.exchange_runtime.is_none());
    let event = |scope| ProviderEvent::InputAccepted {
        scope,
        acknowledgement: crate::provider::InputAcknowledgement::Steering {
            input_id: Some("input".into()),
            text: "continue".into(),
        },
    };
    let foreign = crate::provider::InvocationScope::new(scope.owner.clone(), "another-exchange");
    assert!(
        broker
            .apply_owned_foreground(&mut transaction, event(foreign))
            .is_err()
    );
    let (effects, events) = broker.apply_owned_foreground(&mut transaction, event(scope))?;
    assert!(events.is_empty());
    broker.prepare_provider_delivery(effects)?.deliver().await?;
    let exchange = broker.finish_owned_foreground(&mut transaction)?;
    assert!(exchange.node_list.iter().any(
        |node| matches!(node, ExchangeNode::ExchangeInput { prompt } if prompt.id == "input")
    ));
    assert!(broker.exchange_runtime.is_some());
    assert!(broker.finish_owned_foreground(&mut transaction).is_err());
    Ok(())
}

#[tokio::test]
async fn task_start_updates_live_presentation_before_the_turn_finishes() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (mut exchange, _, mut notifications) = broker
        .test_stage_interaction_for_turn("Inspect startup", true, 1)
        .await?;
    broker.store.save_exchange(&exchange)?;
    broker.reconcile_live_interaction(Some(&exchange), None)?;
    let document = forge_buffer::identity::DocumentId("task-start-live".into());
    broker.presentation.lock().unwrap().open(
        document.clone(),
        forge_buffer::identity::ViewId("task-start-view".into()),
        forge_buffer::width::WidthProfile::default(),
    )?;
    let mut runtime = ExchangeRuntime {
        task_permit: None,
        checkpoint: None,
        tool_gate: None,
        exchange_id: exchange.id.clone(),

        checklist: ChecklistTracker::default(),
        retraction_eligible: true,
        active_wait: None,
    };
    let task_event = ProviderEvent::InputAccepted {
        scope: crate::provider::InvocationScope::new(
            crate::provider::ExecutionOwner::root(&broker.session.id),
            exchange.id.clone(),
        ),
        acknowledgement: crate::provider::InputAcknowledgement::TaskStarted {
            input_id: Some("live-input".into()),
            start: crate::exchange::TaskStart {
                kind: crate::task::TaskKind::Goal,
                title: "Reduce startup time".into(),
            },
            prompt: "Complete startup work.\nVerify the result.".into(),
        },
    };
    broker
        .test_apply_provider_event(
            &mut runtime,
            &mut exchange,
            task_event.clone(),
            &mut notifications,
        )
        .await?;
    let assert_visible = |broker: &HarnessBroker| -> Result<()> {
        let snapshot = broker.presentation.lock().unwrap().snapshot(&document)?;
        let text = snapshot
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert_eq!(
            text.matches("Goal started: Reduce startup time").count(),
            1,
            "{text}"
        );
        assert!(text.contains("Reduce startup time"), "{text}");
        Ok(())
    };
    assert_visible(&broker)?;
    broker
        .test_apply_provider_event(&mut runtime, &mut exchange, task_event, &mut notifications)
        .await?;
    broker.reconcile_live_interaction(Some(&exchange), None)?;
    assert_visible(&broker)?;
    assert_eq!(exchange.state(), ExchangeState::Running);
    assert!(
        broker
            .store
            .load_exchange(&exchange.id)?
            .unwrap()
            .node_list
            .is_empty(),
        "presentation event was copied into the persisted provider content"
    );
    Ok(())
}

#[tokio::test]
async fn task_start_replay_rejects_reused_identity_with_different_content() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let native = task_start_fixture(true);
    let fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    broker.provider = fixture.provider_handle();
    let result = tokio::time::timeout(
        std::time::Duration::from_secs(10),
        broker.run_interaction(
            "Inspect startup".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Inspect startup".into())),
        ),
    )
    .await
    .context("conflicting task-start fixture did not settle")?;
    assert!(
        format!("{:#}", result.unwrap_err())
            .contains("task entry identity was reused for different content")
    );
    let retained = broker
        .store
        .load_session_event(
            &broker.session.id,
            &format!("{}:task-start:task-entry", broker.session.id),
        )?
        .unwrap();
    assert!(
        matches!(retained.detail, SessionEventKind::TaskStarted { prompt, .. } if prompt == "Complete startup work.\nVerify the result.")
    );
    Ok(())
}

impl BoundarySteeringProvider {
    fn new() -> Self {
        Self {
            capability: crate::provider::test_support::fixture_capability(),
            initial_started: Arc::new(tokio::sync::Semaphore::new(0)),
            initial_release: Arc::new(tokio::sync::Semaphore::new(0)),
            steering_deferred: Arc::new(tokio::sync::Semaphore::new(0)),
            continuation_started: Arc::new(tokio::sync::Semaphore::new(0)),
            continuation_accept: Arc::new(tokio::sync::Semaphore::new(0)),
            continuation_release: Arc::new(tokio::sync::Semaphore::new(0)),
            input: Default::default(),
            fail_response: Default::default(),
        }
    }
}

async fn await_boundary_barrier(semaphore: &tokio::sync::Semaphore, name: &str) -> Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(3), semaphore.acquire())
        .await
        .with_context(|| format!("boundary fixture timed out waiting for {name}"))??
        .forget();
    Ok(())
}

impl BoundarySteeringProvider {
    fn fixture(self: &Arc<Self>) -> Arc<crate::provider::test_support::EventFixtureProvider> {
        let mut capability = self.capability.clone();
        capability.integration.tracked_submission = true;
        capability.integration.local_acceptance = false;
        let descriptor = crate::provider::ProviderDescriptor {
            kind: crate::provider::ProviderId::new("test-boundary-steering")
                .expect("fixture ID is valid"),
            label: "Normalized boundary steering fixture".into(),
            capability: capability.clone(),
        };
        let initial_started = self.initial_started.clone();
        let initial_release = self.initial_release.clone();
        let continuation_release = self.continuation_release.clone();
        let input = self.input.clone();
        let fail_response = self.fail_response.clone();
        let native_steer = capability.native_steer;
        let provider = crate::provider::test_support::EventFixtureProvider::new(
            descriptor,
            move |request, sink, _cancelled| {
                let initial_started = initial_started.clone();
                let initial_release = initial_release.clone();
                let continuation_release = continuation_release.clone();
                let input = input.clone();
                let fail_response = fail_response.clone();
                let capability = capability.clone();
                async move {
                    let text = match &request.input {
                        forge_provider_api::PromptInput::Text(text) => text.clone(),
                        forge_provider_api::PromptInput::Skill { name, arguments } => {
                            format!("/{name} {arguments}")
                        }
                        forge_provider_api::PromptInput::QuestionReply(reply) => {
                            reply.question.clone()
                        }
                    };
                    input.lock().unwrap().push(text.clone());
                    let continuation = text == "boundary correction";
                    let address = crate::provider::TurnAddress {
                        scope: request.scope.clone(),
                        thread_id: "boundary-thread".into(),
                        turn_id: if continuation {
                            "continuation-turn"
                        } else {
                            "initial-turn"
                        }
                        .into(),
                    };
                    sink.send_wait(ProviderEvent::SessionIdentity {
                        scope: request.scope.clone().into(),
                        provider_session_id: "boundary-history"
                            .to_owned()
                            .try_into()
                            .expect("fixture history is nonempty"),
                    })
                    .await?;
                    sink.send_wait(ProviderEvent::Execution {
                        observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                            request.scope.clone(),
                            forge_provider_api::lifecycle::ExecutionUpdate::Started {
                                address: address.clone(),
                            },
                        )?,
                    })
                    .await?;
                    sink.send_wait(ProviderEvent::Turn {
                        address: address.clone(),
                        update: crate::provider::TurnUpdate::Started { started_at_ms: 0 },
                    })
                    .await?;
                    if continuation {
                        crate::provider::test_support::wait_gate(&continuation_release).await?;
                    } else {
                        initial_started.add_permits(1);
                        crate::provider::test_support::wait_gate(&initial_release).await?;
                    }
                    let cancelled =
                        fail_response.load(Ordering::Acquire) && (continuation || !native_steer);
                    let (outcome, result) = if cancelled {
                        (
                            crate::turn::TurnOutcome::Cancelled,
                            forge_provider_api::lifecycle::ExecutionResult::Cancelled {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    provider_session_id: Some(
                                        "boundary-history"
                                            .to_owned()
                                            .try_into()
                                            .expect("fixture ID is valid"),
                                    ),
                                    capability: capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        )
                    } else {
                        for event in completed_message(
                            &address,
                            "handled-response",
                            crate::provider::MessageRole::Response,
                            format!("handled {text}"),
                        ) {
                            sink.send_wait(event).await?;
                        }
                        (
                            crate::turn::TurnOutcome::Completed,
                            forge_provider_api::lifecycle::ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    provider_session_id: Some(
                                        "boundary-history"
                                            .to_owned()
                                            .try_into()
                                            .expect("fixture ID is valid"),
                                    ),
                                    capability: capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        )
                    };
                    sink.send_wait(ProviderEvent::Turn {
                        address: address.clone(),
                        update: crate::provider::TurnUpdate::Finished {
                            outcome,
                            completed_at_ms: 1,
                        },
                    })
                    .await?;
                    sink.send_wait(ProviderEvent::Execution {
                        observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                            request.scope.clone(),
                            forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                                address,
                                covered_submissions: vec![request.submission_id],
                                completed_at_ms: 1,
                                result,
                            },
                        )?,
                    })
                    .await?;
                    Ok(())
                }
            },
        );
        let continuation_started = self.continuation_started.clone();
        let continuation_accept = self.continuation_accept.clone();
        provider.with_pre_dispatch(move |request| {
            let continuation_started = continuation_started.clone();
            let continuation_accept = continuation_accept.clone();
            async move {
                if matches!(&request.input, forge_provider_api::PromptInput::Text(text) if text == "boundary correction") {
                    continuation_started.add_permits(1);
                    crate::provider::test_support::wait_gate(&continuation_accept).await?;
                }
                Ok(())
            }
        });
        let deferred = self.steering_deferred.clone();
        provider.with_steering(move |_request| {
            let deferred = deferred.clone();
            async move {
                deferred.add_permits(1);
                Err(crate::provider::ProviderError::NotConsumed)
            }
        })
    }
}
#[tokio::test]
async fn deferred_steering_acknowledges_only_after_provider_acceptance() -> Result<()> {
    assert_deferred_steering_delivery(false).await
}

/// Retaining a cancelled foreground must release unsent commands and their durable attempts.
#[tokio::test]
async fn cancelled_terminal_releases_undispatched_steering_before_retention() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut fixture = BoundarySteeringProvider::new();
    fixture.capability.native_steer = false;
    fixture.fail_response.store(true, Ordering::Release);
    let provider = Arc::new(fixture);
    let session =
        crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
    broker.provider = session.provider_handle();
    let lane = broker.parent_input.clone();
    let catalog = broker.provider_catalog_request()?;
    broker.turn_cancellation().arm();
    let submission = broker.submission_store.prepare_input(
        &catalog,
        ProviderInput::from_text("Retain café 東京"),
        broker.provider.descriptor().kind,
        Some("cancelled-followup".into()),
    )?;
    let running = tokio::spawn(async move {
        let result = broker
            .run_interaction(
                "initial request".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("initial request".into())),
            )
            .await;
        (broker, result)
    });
    await_boundary_barrier(&provider.initial_started, "initial native start").await?;
    let pending = lane.steer(crate::provider::SteeringInput::User {
        text: "Retain café 東京".into(),
        attachments: Vec::new(),
        submission: Some(submission.clone()),
    });
    tokio::pin!(pending);
    assert!(futures_util::poll!(pending.as_mut()).is_pending());
    provider.initial_release.add_permits(1);
    let (broker, result) =
        tokio::time::timeout(std::time::Duration::from_secs(5), running).await??;
    assert!(result.is_err());
    assert!(
        !broker.retained_foregrounds.is_empty(),
        "the regression requires retained foreground ownership"
    );
    let rejected = tokio::time::timeout(std::time::Duration::from_secs(1), pending)
        .await?
        .unwrap_err();
    assert!(matches!(
        rejected.downcast_ref::<crate::provider::ProviderError>(),
        Some(crate::provider::ProviderError::Cancelled)
    ));
    assert_eq!(
        submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Cancelled
    );
    assert_eq!(
        provider.input.lock().unwrap().as_slice(),
        ["initial request"]
    );
    Ok(())
}

#[tokio::test]
async fn accepted_steering_survives_response_failure() -> Result<()> {
    assert_deferred_steering_delivery(true).await
}

#[tokio::test]
async fn deferred_steering_cancellation_preserves_acceptance_boundary() -> Result<()> {
    for accept_before_cancel in [false, true] {
        tokio::time::timeout(std::time::Duration::from_secs(10), async {
            let repository = repository();
            let data = tempfile::tempdir()?;
            let mut broker = planning_question_broker(repository.path(), data.path(), false);
            let provider = Arc::new(BoundarySteeringProvider::new());
            let session = crate::provider::test_support::ProviderTestSession::from_native(
                provider.fixture(),
            )?;
            broker.provider = session.provider_handle();
            let provider_id = broker.provider.descriptor().kind;
            let lane = broker.parent_input.clone();
            let catalog = broker.provider_catalog_request()?;
            let cancellation = broker.turn_cancellation();
            cancellation.arm();
            let journal = crate::provider::submission::SubmissionStore::open(
                data.path(),
                broker.trace.clone(),
            )?;
            let running = tokio::spawn(async move {
                let result = broker
                    .run_interaction(
                        "initial request".into(),
                        PromptMode::Chat,
                        Some(ExchangeAdmission::chat("initial request".into())),
                    )
                    .await;
                (broker, result)
            });
            await_boundary_barrier(&provider.initial_started, "initial native start").await?;
            let submission = journal.prepare_input(
                &catalog,
                ProviderInput::from_text("boundary correction"),
                provider_id,
                Some("cancel-boundary-message".into()),
            )?;
            let mut steering = Box::pin(lane.steer(crate::provider::SteeringInput::User {
                text: "boundary correction".into(),
                attachments: Vec::new(),
                submission: Some(submission.clone()),
            }));
            assert!(futures_util::poll!(steering.as_mut()).is_pending());
            await_boundary_barrier(&provider.steering_deferred, "native steering deferral").await?;
            provider.initial_release.add_permits(1);
            await_boundary_barrier(&provider.continuation_started, "continuation preparation")
                .await?;
            if accept_before_cancel {
                provider.continuation_accept.add_permits(1);
                tokio::time::timeout(std::time::Duration::from_secs(3), &mut steering)
                    .await
                    .context("boundary fixture timed out waiting for continuation acceptance")??;
            }
            cancellation.request();
            let (broker, result) = tokio::time::timeout(std::time::Duration::from_secs(3), running)
                .await
                .context("boundary fixture timed out collecting cancelled invocation")??;
            assert!(result.is_err());
            if !accept_before_cancel {
                assert!(
                    tokio::time::timeout(std::time::Duration::from_secs(3), steering)
                        .await
                        .context("boundary fixture timed out resolving unaccepted input")?
                        .is_err(),
                    "unaccepted steering was acknowledged after cancellation"
                );
            }
            assert_eq!(
                matches!(
                    submission.record()?.delivery,
                    crate::provider::submission::SubmissionState::Accepted { .. }
                ),
                accept_before_cancel
            );
            let exchange = broker.store.list_exchange(&broker.session.id)?;
            assert_eq!(exchange.len(), 1);
            assert_eq!(
                exchange[0]
                    .node_list
                    .iter()
                    .filter(|node| matches!(node,
                crate::exchange::ExchangeNode::ExchangeInput { prompt }
                    if prompt.id == "cancel-boundary-message"))
                    .count(),
                usize::from(accept_before_cancel)
            );
            Ok::<(), anyhow::Error>(())
        })
        .await??;
    }
    Ok(())
}

async fn assert_deferred_steering_delivery(fail_response: bool) -> Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let provider = Arc::new(BoundarySteeringProvider::new());
        provider.fail_response.store(fail_response, std::sync::atomic::Ordering::Release);
        let session = crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
        broker.provider = session.provider_handle();
        let provider_id = broker.provider.descriptor().kind;
        let lane = broker.parent_input.clone();
        let catalog = broker.provider_catalog_request()?;
        let journal = crate::provider::submission::SubmissionStore::open(data.path(), broker.trace.clone())?;
        let mut running = tokio::spawn(async move {
            let result = broker.run_interaction("initial request".into(), PromptMode::Chat,
                Some(ExchangeAdmission::chat("initial request".into()))).await;
            (broker, result)
        });
        tokio::select! {
            started = await_boundary_barrier(&provider.initial_started, "initial native start") => started?,
            finished = &mut running => {
                let (_, result) = finished?;
                anyhow::bail!("boundary foreground ended before initial native start: {result:?}");
            }
        }
        let submission = journal.prepare_input(&catalog,
            ProviderInput::from_text("boundary correction"), provider_id, Some("boundary-message".into()))?;
        let mut steering = Box::pin(lane.steer(crate::provider::SteeringInput::User {
            text: "boundary correction".into(), attachments: Vec::new(), submission: Some(submission.clone()),
        }));
        assert!(futures_util::poll!(steering.as_mut()).is_pending());
        await_boundary_barrier(&provider.steering_deferred, "native steering deferral").await?;
        provider.initial_release.add_permits(1);
        await_boundary_barrier(&provider.continuation_started, "continuation preparation").await?;
        assert!(matches!(submission.record()?.delivery, crate::provider::submission::SubmissionState::Prepared));
        assert!(futures_util::poll!(steering.as_mut()).is_pending(),
            "a continuation was acknowledged while its journal still allowed requeueing");
        provider.continuation_accept.add_permits(1);
        tokio::time::timeout(std::time::Duration::from_secs(3), steering).await.context("boundary fixture timed out waiting for continuation acceptance")??;
        assert!(matches!(submission.record()?.delivery, crate::provider::submission::SubmissionState::Accepted { .. }));
        assert!(!running.is_finished(), "steering acknowledgement waited for the full provider response");
        provider.continuation_release.add_permits(1);
        let (broker, result) = tokio::time::timeout(std::time::Duration::from_secs(3), running).await.context("boundary fixture timed out collecting continuation")??;
        if fail_response { assert!(result.is_err()); } else { result?; }
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchange.len(), 1, "steering created a second exchange");
        assert_eq!(exchange[0].node_list.iter().filter(|node| matches!(node,
            crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.text == "boundary correction" && prompt.id == "boundary-message")).count(), 1);
        assert_eq!(*provider.input.lock().unwrap(), ["initial request", "boundary correction"]);
        Ok::<(), anyhow::Error>(())
    }).await?
}

/// Hold native steering admission while the original provider turn keeps running.
struct SlowSteeringProvider {
    inner: Arc<BoundarySteeringProvider>,
    steering_started: Arc<tokio::sync::Semaphore>,
    steering_release: Arc<tokio::sync::Semaphore>,
}
#[tokio::test]
async fn slow_native_steering_keeps_its_delivery_until_acknowledged() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = Arc::new(SlowSteeringProvider {
        inner: Arc::new(BoundarySteeringProvider::new()),
        steering_started: Arc::new(tokio::sync::Semaphore::new(0)),
        steering_release: Arc::new(tokio::sync::Semaphore::new(0)),
    });
    let started = provider.steering_started.clone();
    let release = provider.steering_release.clone();
    let native = provider.inner.fixture().with_steering(move |_request| {
        let started = started.clone();
        let release = release.clone();
        async move {
            started.add_permits(1);
            crate::provider::test_support::wait_gate(&release).await?;
            Ok(())
        }
    });
    let fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    broker.provider = fixture.provider_handle();
    let provider_id = broker.provider.descriptor().kind;
    let lane = broker.parent_input.clone();
    let catalog = broker.provider_catalog_request()?;
    let cancellation = broker.turn_cancellation();
    let journal =
        crate::provider::submission::SubmissionStore::open(data.path(), broker.trace.clone())?;
    let mut running = tokio::spawn(async move {
        broker
            .run_interaction(
                "initial request".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("initial request".into())),
            )
            .await
    });
    tokio::select! {
        started = await_boundary_barrier(&provider.inner.initial_started, "initial native start") => started?,
        finished = &mut running => {
            anyhow::bail!("slow steering foreground ended before initial native start: {:?}", finished?);
        }
    }
    let submission = journal.prepare_input(
        &catalog,
        ProviderInput::from_text("keep working"),
        provider_id,
        Some("slow-steering".into()),
    )?;
    let mut steering = std::pin::pin!(lane.steer(crate::provider::SteeringInput::User {
        text: "keep working".into(),
        attachments: Vec::new(),
        submission: Some(submission.clone()),
    }));
    assert!(futures_util::poll!(steering.as_mut()).is_pending());
    tokio::select! {
        started = await_boundary_barrier(&provider.steering_started, "native steering admission") => started?,
        finished = &mut running => {
            anyhow::bail!("slow steering foreground ended before native steering admission: {:?}", finished?);
        }
    }
    assert!(
        matches!(
            submission.record()?.delivery,
            crate::provider::submission::SubmissionState::Sending { .. }
        ),
        "native steering admission did not preserve its unacknowledged transport boundary"
    );

    tokio::time::pause();
    tokio::time::advance(std::time::Duration::from_secs(29)).await;
    tokio::task::yield_now().await;
    tokio::task::yield_now().await;
    assert!(futures_util::poll!(steering.as_mut()).is_pending());
    tokio::time::resume();
    provider.steering_release.add_permits(1);
    let delivered =
        tokio::time::timeout(std::time::Duration::from_secs(1), steering.as_mut()).await;
    cancellation.request();
    provider.inner.initial_release.add_permits(1);
    let _cancelled = tokio::time::timeout(std::time::Duration::from_secs(10), running)
        .await
        .context("steering fixture execution did not collect after cancellation")??;

    delivered.context("steering was abandoned before its acknowledgment deadline")??;
    assert!(matches!(
        submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Accepted { .. }
    ));
    assert_eq!(*provider.inner.input.lock().unwrap(), ["initial request"]);
    Ok(())
}
