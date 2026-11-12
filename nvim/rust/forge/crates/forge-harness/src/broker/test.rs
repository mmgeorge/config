use super::*;
use crate::provider::ScopeOwner;

use crate::session::state_machine::SessionPhase;

mod admission;
mod agent;
mod input;
mod test_support;

use test_support::{
    PreparationProbe, checkpoint_fixture, completed_message, settle_managed_children,
};
pub(super) use test_support::{git_repository, planning_question_broker, repository};

/// A failed pre-baseline owner releases its own capture while an unrelated Git editor stays admitted.
#[tokio::test]
async fn prebaseline_failure_settles_while_shared_git_admission_remains_held() -> Result<()> {
    for native_rejection in [false, true] {
        let repository = git_repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let rejection = Arc::new(PreparationProbe {
            started: Notify::new(),
            release: Notify::new(),
            fail: true,
        });
        let tool_ran = Arc::new(AtomicBool::new(false));
        let active = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let provider = checkpoint_fixture(
            repository.path().to_owned(),
            (!native_rejection).then(|| rejection.clone()),
            Arc::new(Notify::new()),
            tool_ran.clone(),
            active.clone(),
            native_rejection.then(|| rejection.clone()),
            false,
        )?;
        broker.provider = provider.provider_handle();
        let repositories = broker.repositories.clone();
        let handle = repositories
            .open(repository.path().to_owned())
            .await?
            .context("fixture repository missing")?;
        let operation = repositories.writes.admit(
            vec![forge_git::mutation::MutationScope::WorktreeFiles(
                handle
                    .identity
                    .worktree
                    .clone()
                    .context("fixture worktree missing")?,
            )],
            0,
        )?;
        let held = repositories
            .writes
            .start(operation)?
            .context("external Git mutation was not admitted")?;
        let reject = async {
            rejection.started.notified().await;
            while repositories.writes.usage().operations < 2 {
                tokio::task::yield_now().await;
            }
            rejection.release.notify_one();
        };
        let prompt = "Fail before the baseline while Git commit owns the workspace";
        let (result, ()) = async {
            tokio::join!(
                broker.run_interaction(
                    prompt.into(),
                    PromptMode::Chat,
                    Some(ExchangeAdmission::chat(prompt.into()))
                ),
                reject
            )
        }
        .await;
        let error = result.expect_err("pre-baseline failure became successful execution");
        let expected = if native_rejection {
            "Rejected"
        } else {
            "Runtime"
        };
        assert!(format!("{error:#}").contains(expected), "{error:#}");
        assert!(!error.is::<TurnCancelled>() && !error.is::<TurnRetracted>());
        assert!(!tool_ran.load(Ordering::Acquire) && active.load(Ordering::Acquire) == 0);
        assert!(broker.host_settlement().is_none() && broker.exchange_runtime.is_none());
        assert_eq!(
            repositories.writes.state(operation),
            Some(forge_git::coordinator::OperationState::Running {
                cancellation_requested: false
            })
        );
        assert_eq!(
            repositories.writes.usage().operations,
            1,
            "aborted baseline retained mutation admission"
        );
        let exchanges = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchanges.len(), 1);
        assert_eq!(exchanges[0].state(), ExchangeState::Failed);
        assert!(
            exchanges[0].checkpoint_before.is_none() && exchanges[0].checkpoint_after.is_none()
        );
        assert!(
            exchanges[0]
                .finalization_error
                .as_deref()
                .is_some_and(|error| error.contains(expected))
        );
        let query =
            broker.provider_request(ProviderInput::from_user_text(prompt), PromptMode::Chat)?;
        let pending = broker.submission_store.pending(
            &query,
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            None,
        )?;
        assert_eq!(pending.len(), 1);
        assert!(matches!(
            pending[0].delivery,
            crate::provider::submission::SubmissionState::Prepared
        ));
        assert_eq!(
            std::fs::read_to_string(repository.path().join("seed.txt"))?,
            "seed\n"
        );
        held.finish(forge_git::mutation::OperationCompletion::Completed)?;
        repositories.writes.take_receipt(operation);
    }
    Ok(())
}

#[tokio::test]
async fn native_abort_completion_rewinds_before_pending_checkpoint_finalization() -> Result<()> {
    // Direct cancellation through `run_interaction` always asks to restore an unconsumed prompt;
    // the retaining policy comes from an owner command, so it is not reachable here.
    for (cleanup_before_completion, visible_output) in
        [(true, false), (false, false), (true, true), (false, true)]
    {
        let repository = git_repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (provider, signals) = cleanup_retry_provider(&broker, visible_output, false);
        broker.provider = provider.clone();
        broker.capability.native_turn_rollback = true;
        broker.trace.configure(crate::trace::LogLevel::Trace)?;
        let execution_registry = broker.execution_registry();
        let repositories = Arc::clone(&broker.repositories);
        let handle = repositories
            .open(repository.path().to_owned())
            .await?
            .context("fixture repository")?;
        let operation = repositories.writes.admit(
            vec![forge_git::mutation::MutationScope::WorktreeFiles(
                handle
                    .identity
                    .worktree
                    .clone()
                    .context("fixture worktree")?,
            )],
            0,
        )?;
        let held = repositories
            .writes
            .start(operation)?
            .context("fixture mutation admission")?;
        let cancellation = broker.turn_cancellation();
        cancellation.arm();
        let session_id = broker.session.id.clone();
        let cancel = async {
            signals.started.notified().await;
            if cleanup_before_completion {
                cancellation.block_submissions();
                let scope = signals
                    .scope
                    .lock()
                    .unwrap()
                    .clone()
                    .context("fixture invocation scope missing")?;
                provider.cancel_invocation(scope).await?;
            } else {
                signals.complete.notify_one();
                signals.settled.notified().await;
                tokio::task::yield_now().await;
                cancellation.block_submissions();
            }
            cancellation.request();
            Ok::<_, anyhow::Error>(())
        };
        let execution = broker.run_interaction(
            "Cancel after native dispatch".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat(
                "Cancel after native dispatch".into(),
            )),
        );
        let result = tokio::join!(execution, cancel);
        held.finish(forge_git::mutation::OperationCompletion::Completed)?;
        repositories.writes.take_receipt(operation);
        let (result, cancelled) = result;
        cancelled?;
        let error = result.unwrap_err();
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        if !visible_output {
            assert!(
                error.is::<TurnRetracted>(),
                "cleanup_before_completion={cleanup_before_completion}, visible_output={visible_output}; {error:#}; trace={}",
                std::fs::read_to_string(broker.trace.session_status(&broker.session.id).path)
                    .unwrap_or_else(|error| format!("read failed: {error}"))
            );
            assert!(exchange.is_empty());
        } else {
            assert!(error.is::<TurnCancelled>(), "{error:#}");
            assert_eq!(exchange.len(), 1);
            assert_eq!(exchange[0].state(), ExchangeState::Cancelled);
            assert!(exchange[0].checkpoint_before.is_none());
            assert!(exchange[0].checkpoint_after.is_none());
            assert!(exchange[0].finalization_error.is_none());
            if visible_output {
                assert!(exchange[0].turn.iter().any(|turn| {
                    turn.messages()
                        .iter()
                        .any(|message| message.text() == "Partial response")
                }));
            }
        }
        assert!(broker.exchange_runtime.is_none());
        // Cancellation collects only the exchange's native turn through its scope, so the
        // session owner stays available for the next prompt.
        assert!(
            execution_registry
                .owner(crate::provider::ThreadOwner::root(&session_id))
                .await?
                .is_healthy()
                .await,
            "scoped cancellation closed the session owner; cleanup_before_completion={cleanup_before_completion}, visible_output={visible_output}"
        );
    }
    Ok(())
}

#[tokio::test]
async fn checkpoint_does_not_delay_prompt_admission_but_blocks_first_tool() -> Result<()> {
    let repository = git_repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let started = Arc::new(tokio::sync::Notify::new());
    let tool_ran = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let document = forge_buffer::identity::DocumentId("checkpoint-transcript".into());
    {
        let mut presentation = broker.presentation.lock().unwrap();
        presentation.open(
            document.clone(),
            forge_buffer::identity::ViewId("checkpoint-view".into()),
            forge_buffer::width::WidthProfile::default(),
        )?;
        presentation.begin_submission(&document, 1, "Capture before my first tool")?;
    }
    broker.prompt_admission = Some((Some(document), 1));
    let (sink, mut stream) = publication::channel();
    broker.event_sink = Some(sink);
    let provider = checkpoint_fixture(
        repository.path().to_owned(),
        None,
        started.clone(),
        tool_ran.clone(),
        Default::default(),
        None,
        false,
    )?;
    broker.provider = provider.provider_handle();
    let repositories = broker.repositories.clone();
    let handle = repositories
        .open(repository.path().to_owned())
        .await?
        .context("fixture repository")?;
    let operation = repositories.writes.admit(
        vec![forge_git::mutation::MutationScope::WorktreeFiles(
            handle
                .identity
                .worktree
                .clone()
                .context("fixture worktree")?,
        )],
        0,
    )?;
    let held = repositories
        .writes
        .start(operation)?
        .context("fixture mutation admission")?;
    let session_id = broker.session.id.clone();
    let observe = async {
        started.notified().await;
        let mut accepted = false;
        let mut projected = false;
        async {
            while !accepted || !projected {
                let event = stream
                    .recv()
                    .await?
                    .context("admission publication closed")?;
                accepted |= matches!(
                    &event,
                    SessionUpdate::Submission {
                        transition: publication::SubmissionUpdate {
                            state: publication::SubmissionState::Accepted,
                            ..
                        }
                    }
                );
                projected |= matches!(&event, SessionUpdate::DocumentChanged { .. });
            }
            Ok::<(), anyhow::Error>(())
        }
        .await?;
        assert!(
            accepted,
            "composer acceptance waited for checkpoint capture"
        );
        assert!(
            projected,
            "timeline did not project before checkpoint capture"
        );
        let store = SqliteStore::open(data.path())?;
        let exchange = store.list_exchange(&session_id)?;
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].prompt, "Capture before my first tool");
        assert!(exchange[0].checkpoint_before.is_none());
        assert!(!tool_ran.load(Ordering::SeqCst));
        assert_eq!(
            std::fs::read_to_string(repository.path().join("seed.txt"))?,
            "seed\n"
        );
        held.finish(forge_git::mutation::OperationCompletion::Completed)?;
        repositories.writes.take_receipt(operation);
        Ok::<_, anyhow::Error>(())
    };
    let (result, observed) = async {
        tokio::join!(
            broker.run_interaction(
                "Capture before my first tool".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat(
                    "Capture before my first tool".into()
                ))
            ),
            observe
        )
    }
    .await;
    observed?;
    result?;
    assert!(tool_ran.load(Ordering::SeqCst));
    let exchange = broker
        .store
        .list_exchange(&session_id)?
        .pop()
        .context("completed fixture exchange")?;
    let before = broker
        .store
        .load_checkpoint(exchange.checkpoint_before.as_deref().context("before id")?)?
        .context("before checkpoint")?;
    assert!(!before.file.iter().any(|file| file.path == "seed.txt"));
    let after = broker
        .store
        .load_checkpoint(exchange.checkpoint_after.as_deref().context("after id")?)?
        .context("after checkpoint")?;
    assert!(after.file.iter().any(|file| file.path == "seed.txt"));
    Ok(())
}

#[tokio::test]
async fn manual_child_admission_streams_and_followup_reuses_only_child_history() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session.provider_session_id = Some("root-history".into());
    assert!(!broker.agent_coordinator.enabled());
    let (admitted, _) = broker
        .start_agent(json!({"profile":"worker","text":"CHILD_FIRST"}))
        .await?;
    let agent_id = admitted["agent"]["id"]
        .as_str()
        .context("admission has no child identity")?
        .to_owned();
    assert_eq!(admitted["status"], "admitted");
    let event = settle_managed_children(&mut broker).await?;
    assert!(
        event.iter().any(|event| event.event == "document_changed"
            || event.payload["kind"] == "document_changed"),
        "child events did not update the shared document: {:?}",
        event
            .iter()
            .map(|event| (&event.event, &event.payload["kind"]))
            .collect::<Vec<_>>()
    );
    let child = broker.agent_coordinator.resolve(&agent_id)?;
    assert!(child.provider_thread_id.is_some());
    assert_ne!(child.provider_thread_id.as_deref(), Some("root-history"));
    assert_eq!(
        broker.session.provider_session_id.as_deref(),
        Some("root-history")
    );
    let exchange = broker.store.list_agent_exchange(&agent_id)?;
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].state(), ExchangeState::Complete);
    assert!(
        exchange[0]
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .any(|message| message.text().contains("CHILD_FIRST"))
    );
    let sender = broker.agent_coordinator.sender();
    let request = crate::agent::AgentRequest {
        operation_id: "followup".into(),
        source: crate::agent::AgentSource::User,
        operation: crate::agent::AgentOperation::Followup {
            target: agent_id.clone(),
            message: "CHILD_SECOND".into(),
        },
    };
    let response = tokio::spawn(async move { sender.request(request).await });
    tokio::task::yield_now().await;
    let mut drained = Vec::new();
    while broker.drain_next_agent_command(None, &mut drained).await {}
    assert_eq!(response.await??["agent"]["id"], agent_id);
    settle_managed_children(&mut broker).await?;
    let exchange = broker.store.list_agent_exchange(&agent_id)?;
    assert_eq!(exchange.len(), 2);
    assert!(
        exchange
            .iter()
            .all(|exchange| exchange.state() == ExchangeState::Complete)
    );
    assert_eq!(broker.agent_coordinator.snapshot(None).len(), 1);
    assert_eq!(broker.store.list_agent_task(&broker.session.id)?.len(), 2);
    Ok(())
}

fn delegating_root_fixture() -> Result<(
    Arc<AtomicU64>,
    crate::provider::test_support::ProviderTestSession,
)> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized mock".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let turn = Arc::new(AtomicU64::new(0));
    let provider = crate::provider::test_support::EventFixtureProvider::new(descriptor.clone(), {
        let turn = turn.clone();
        move |request, sink, start, _cancelled| {
            let turn = turn.clone();

            async move {
                start.observed(0).await?;

                let ordinal = turn.fetch_add(1, Ordering::AcqRel);
                let text = crate::provider::test_support::input_text(&request.input);
                if ordinal == 0 {
                    let result = crate::provider::test_support::call_tool(
                        &request,
                        "agent_spawn",
                        json!({"profile":"worker","text":"CHILD_RESULT"}),
                    )
                    .await?;
                    assert!(result.to_string().contains("admitted"));
                } else {
                    assert!(ordinal == 1, "parent continuation duplicated");
                    assert!(
                        text.contains("CHILD_RESULT")
                            && text.contains("<agent_message type=\"final_answer\" "),
                        "parent continuation omitted child result: {text}"
                    );
                }
                let scope = request.scope.clone();
                let address = crate::provider::TurnAddress {
                    thread_id: (scope.clone()).owner().thread_id(),
                    turn_id: (scope.clone()).turn_id().unwrap(),
                    scope: scope.clone(),
                };

                let response = if ordinal == 0 {
                    "Waiting for child"
                } else {
                    "SYNTHESIZED_RESULT"
                };
                for event in completed_message(
                    &address,
                    "root-response",
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
        }
    });
    let provider = provider.with_session_identity(|_| async {
        Ok(Some(
            "parent-history"
                .to_owned()
                .try_into()
                .expect("fixture ID is valid"),
        ))
    });
    Ok((
        turn,
        crate::provider::test_support::ProviderTestSession::from_native(provider)?,
    ))
}

#[tokio::test]
async fn model_child_completes_through_the_active_root_lane_and_continues_the_same_exchange()
-> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (turn, provider) = delegating_root_fixture()?;
    broker.provider = provider.provider_handle();
    broker.agent_coordinator.set_enabled(true);
    broker
        .run_interaction(
            "Root request".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Root request".into())),
        )
        .await?;
    assert_eq!(turn.load(Ordering::Acquire), 2);
    let exchange = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].state(), ExchangeState::Complete);
    assert!(
        exchange[0]
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .any(|message| message.text().contains("SYNTHESIZED_RESULT"))
    );
    let task = broker.store.list_agent_task(&broker.session.id)?;
    assert_eq!(task.len(), 1);
    assert!(task[0].parent_reported);
    assert_eq!(task[0].parent_exchange_id, exchange[0].id);
    Ok(())
}

#[tokio::test]
async fn yolo_toggles_sandbox_in_direct_and_admitted_transitions() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    for registration in forge_providers::registry::descriptors() {
        let sandbox_supported = registration.capability.supports_sandbox();
        broker.session.provider = registration.id.to_string();
        // The broker validates access against its bound provider's capability.
        broker.capability = registration.capability.clone();
        for admitted in [false, true] {
            broker
                .configure_session(json!({"access":crate::session::AccessPolicy {
                sandbox: sandbox_supported, ..Default::default()
            }, "approval_mode":"ask"}))
                .await
                .unwrap();
            for (sandbox, approval) in [
                (false, "allow"),
                (sandbox_supported, "ask"),
                (false, "allow"),
            ] {
                let mut action = json!({"change":{"kind":"enable_yolo"}});
                if admitted {
                    action["permission_selection"] = json!({
                        "sandbox":sandbox,"read_access":"full","write_access":"full","approval_mode":approval
                    });
                }
                broker
                    .apply_configuration(action, configuration::ConfigurationCommand::Permission)
                    .await
                    .unwrap();
                let stored = broker
                    .store
                    .load_session(&broker.session.id)
                    .unwrap()
                    .unwrap();
                assert_eq!(stored.access.sandbox, sandbox);
                assert_eq!(stored.access.abbreviation(), "rxwx");
                assert_eq!(stored.approval_mode.label().to_lowercase(), approval);
                assert!(stored.active_task.is_none());
            }
        }
    }
}

#[tokio::test]
async fn access_configuration_is_atomic_persistent_and_independent_of_approvals() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let writable = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let policy = crate::session::AccessPolicy {
        sandbox: false,
        writable_directory: vec![writable.path().to_string_lossy().into_owned()],
        ..Default::default()
    };
    broker
        .configure_session(json!({"access":policy}))
        .await
        .unwrap();
    let stored = broker
        .store
        .load_session(&broker.session.id)
        .unwrap()
        .unwrap();
    assert!(!stored.access.sandbox);
    assert_eq!(stored.access.writable_directory.len(), 1);
    assert_eq!(stored.approval_mode, ApprovalMode::Ask);
    let preference = broker
        .store
        .load_preference(&stored.workspace, &stored.provider)
        .unwrap()
        .unwrap();
    assert_eq!(preference.access, stored.access);
    broker
        .configure_session(json!({"approval_mode":"allow"}))
        .await
        .unwrap();
    assert_eq!(broker.session.access, stored.access);
    assert!(
        broker
            .configure_session(json!({"approval_mode":"allow","effort":""}))
            .await
            .is_err()
    );
    assert_eq!(broker.session.approval_mode, ApprovalMode::Allow);
    assert!(broker.configure_session(json!({"access":{"sandbox":true,"write_access":"workspace","writable_directory":["relative"],"isolation_mode":"elevated"}})).await.is_err());
    assert_eq!(broker.session.access, stored.access);
}

fn workflow_fixture_bridge(
    broker: &HarnessBroker,
    native: std::sync::Arc<crate::provider::test_support::EventFixtureProvider>,
) -> Arc<Provider> {
    Arc::new(Provider::new(
        native.registration(),
        broker.permission_coordinator.clone(),
        broker.trace.clone(),
        broker.data_root.clone(),
    ))
}

pub(super) fn completed_interaction(
    id: &str,
    session_id: &str,
    node_list: Vec<ExchangeNode>,
) -> Exchange {
    Exchange {
        approval_mode: None,
        finalization_error: None,
        check_owner: None,
        output_store: None,
        archived_output: Default::default(),
        input_message_id: None,
        provider_retry: None,
        agent_id: HarnessSession::primary_agent_id(session_id),
        id: id.into(),
        session_id: session_id.into(),
        ordinal: 1,
        prompt: "test interaction".into(),
        origin: crate::exchange::ExchangeOrigin::User,

        phase: crate::exchange::ExchangePhase::fixture(ExchangeState::Complete, Some(2), None),
        checkpoint_before: None,
        checkpoint_after: None,
        attributed_diff_text: None,
        checkpoint_diff_text: None,
        attributed_matches_checkpoint: false,
        provider_checkpoint_id: None,
        approval_resolution: Vec::new(),
        host_file_changes: Vec::new(),
        check_setup: None,
        turn: Vec::new(),
        created_at_ms: 1,
        entry: Vec::new(),
        tool_counter: Default::default(),
        node_list,
        metrics: crate::exchange::ExchangeMetrics::default(),
        comment: Vec::new(),
        checklist: None,

        question: crate::exchange::ExchangeQuestion::None,
        clock: crate::exchange::ExchangeClock::Paused { elapsed_ms: 1 },
    }
}

fn saturating_provider(
    broker: &HarnessBroker,
    completed: bool,
    wait_for_capacity: bool,
    cancelled: Arc<AtomicBool>,
    // When set, each delta is one Markdown paragraph and the provider sends the next only after
    // the reader signals that it has seen the previous one, as a model streams to an editor.
    paragraph_pace: Option<Arc<Notify>>,
) -> Arc<Provider> {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Event delivery fixture".into(),
        capability: Default::default(),
    };
    let cleanup_cancelled = cancelled.clone();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, mut cancellation| {
            let cancelled = cancelled.clone();
            let paragraph_pace = paragraph_pace.clone();
            async move {
                let native_outcome = forge_provider_api::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                };
                let native_completed_at_ms = 0;
                start.observed(0).await?;

                let turn = crate::provider::TurnAddress {
                    thread_id: (request.scope.clone()).owner().thread_id(),
                    turn_id: (request.scope.clone()).turn_id().unwrap(),
                    scope: request.scope.clone(),
                };

                let address = (
                    turn.clone(),
                    forge_provider_api::ItemId::new(format!(
                        "{}:saturated-message",
                        turn.turn_id.as_str()
                    ))
                    .unwrap(),
                );
                let mut text = String::new();
                // A waiting burst proves bounded delivery drains. The non-waiting variant
                // sends one event and then injects the overflow a full outbound queue
                // reports, instead of filling that queue by volume.
                let event_count = match (&paragraph_pace, wait_for_capacity) {
                    (Some(_), _) => 12,
                    (None, true) => 384,
                    (None, false) => 1,
                };
                for sequence in 0..event_count {
                    let delta = match &paragraph_pace {
                        Some(_) => format!("paragraph {sequence}\n\n"),
                        None => sequence.to_string(),
                    };
                    text.push_str(&delta);
                    let update = if sequence == 0 {
                        forge_provider_api::ItemEvent::Started(
                            forge_provider_api::ItemStart::AgentMessage(
                                forge_provider_api::AgentMessageStart {
                                    // A provider that streams prose before it knows whether
                                    // a tool call follows leaves the message unclassified.
                                    classification: if paragraph_pace.is_some() {
                                        forge_provider_api::MessageClassification::Unclassified
                                    } else {
                                        forge_provider_api::MessageClassification::Known(
                                            forge_provider_api::AgentMessageRole::Response,
                                        )
                                    },
                                    text: delta,
                                },
                            ),
                        )
                    } else {
                        forge_provider_api::ItemEvent::Message {
                            event: forge_provider_api::MessageItemEvent::Delta(delta),
                        }
                    };
                    let event = ProviderEvent::Item {
                        thread_id: turn.thread_id.clone(),
                        turn_id: turn.turn_id.clone(),
                        item_id: address.1.clone(),
                        event: update,
                    };
                    if wait_for_capacity {
                        sink.send_wait(crate::provider::test_support::activity(event)?)
                            .await?;
                        if let Some(pace) = &paragraph_pace {
                            pace.notified().await;
                        }
                    } else {
                        sink.send(crate::provider::test_support::activity(event)?)?;
                    }
                }
                if !wait_for_capacity {
                    // The sink retains the overflow as its terminal failure while the
                    // native turn keeps running, so the host must cancel or collect it.
                    sink.fail(crate::provider::events::EventDeliveryError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::Overflow,
                    ));
                }
                if wait_for_capacity {
                    sink.send_wait(crate::provider::test_support::activity(
                        ProviderEvent::Item {
                            thread_id: (address.clone()).0.thread_id.clone(),
                            turn_id: (address.clone()).0.turn_id.clone(),
                            item_id: (address.clone()).1.clone(),
                            event: forge_provider_api::ItemEvent::Completed(
                                forge_provider_api::ItemCompletion::AgentMessage(
                                    forge_provider_api::MessageCompletion {
                                        text,
                                        outcome: forge_provider_api::MessageOutcome::Completed,
                                    },
                                ),
                            ),
                        },
                    )?)
                    .await?;
                }
                if completed {
                    return Ok::<_, forge_provider_api::ProviderError>(
                        forge_provider_api::TurnResult::new(
                            native_outcome,
                            native_completed_at_ms,
                            None,
                            request.input_coverage(),
                            Vec::new(),
                            None,
                        )
                        .expect("fixture coverage is valid"),
                    );
                }
                if cancellation.changed().await.is_ok() {
                    cancelled.store(true, Ordering::Release);
                }
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
    .with_cleanup_request(move |_binding| {
        let cancelled = cleanup_cancelled.clone();
        async move {
            cancelled.store(true, Ordering::Release);
            Ok(())
        }
    });
    workflow_fixture_bridge(broker, native)
}
#[tokio::test]
async fn event_saturation_cancels_pending_and_completed_provider_turns() {
    for completed in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                provider_launch_by_provider: Default::default(),
                config_dir: None,
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "saturation-test".into(),
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
            Box::new(FixedClock(200)),
        )
        .unwrap();
        let cancelled = Arc::new(AtomicBool::new(false));
        let fixture_provider =
            saturating_provider(&broker, completed, false, cancelled.clone(), None);
        broker.provider = fixture_provider;
        let admitted = broker
            .dispatch(Request {
                id: 1,
                method: "session.goal.set".into(),
                params: json!({ "objective": "test bounded provider delivery" }),
            })
            .await;
        assert!(admitted.response.error().is_none());
        let error = dispatch_pending_task_input(&mut broker)
            .await
            .expect_err("saturation must not report success");
        // Dispatch reports the failure as a response, so only its rendered
        // classification crosses the boundary.
        let overflow = forge_provider_api::lifecycle::ObservationFailure::Overflow.to_string();
        assert!(format!("{error:#}").contains(&overflow), "{error:?}");
        // Only a still-running native turn needs interruption; a completed turn has
        // nothing left to cancel, yet its exchange still fails with the lost events.
        if !completed {
            assert!(cancelled.load(Ordering::Acquire));
        }
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(interaction.len(), 1);
        assert_eq!(interaction[0].state(), ExchangeState::Failed);
    }
}

/// Create a broker whose fixture provider streams a burst of 384 text deltas per prompt.
fn burst_broker(
    repository: &tempfile::TempDir,
    data: &tempfile::TempDir,
    cancelled: Arc<AtomicBool>,
) -> HarnessBroker {
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "burst-test".into(),
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
        Box::new(FixedClock(200)),
    )
    .unwrap();
    broker.provider = saturating_provider(&broker, true, true, cancelled, None);
    broker
}

/// A streamed save that fails after its text was shown fences the session at the next save.
#[tokio::test]
async fn failed_write_behind_save_fails_the_turn_instead_of_healing() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = burst_broker(&repository, &data, Arc::new(AtomicBool::new(false)));
    let database = crate::storage::test::raw_connection(data.path()).unwrap();
    // Refuse the exchange once its streamed text reaches the sixteenth digit of the burst.
    database
        .execute_batch(
            "CREATE TRIGGER refuse_streamed_text BEFORE UPDATE ON exchange_record
             WHEN instr(NEW.payload,'0123456789101112')>0
             BEGIN SELECT RAISE(ABORT,'streamed text refused'); END;",
        )
        .unwrap();
    let (sink, mut stream) = publication::channel();
    let dispatch = broker.dispatch_stream(
        Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "burst then final answer" }),
        },
        sink,
    );
    let consume = async move { while stream.recv().await.unwrap().is_some() {} };
    let (result, ()) = tokio::join!(dispatch, consume);
    assert!(
        result.response.error().is_some(),
        "the turn completed although its shown text was never saved"
    );
    let failure = broker
        .store
        .write_behind_failure(&broker.session.id)
        .expect("the failed save fences the session");
    assert!(failure.to_string().contains("streamed text refused"));
}

/// Each streamed paragraph of a response reaches the open transcript while the turn is still
/// running. A transcript that shows the first delta and then nothing until completion fails.
#[tokio::test]
async fn streamed_response_paragraphs_reach_the_open_transcript_while_the_turn_runs() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = burst_broker(&repository, &data, Arc::new(AtomicBool::new(false)));
    let pace = Arc::new(Notify::new());
    broker.provider = saturating_provider(
        &broker,
        true,
        true,
        Arc::new(AtomicBool::new(false)),
        Some(pace.clone()),
    );
    let document = forge_buffer::identity::DocumentId("streaming-transcript".into());
    let presentation = broker.presentation();
    let opened = presentation
        .lock()
        .unwrap()
        .open(
            document.clone(),
            forge_buffer::identity::ViewId("streaming-view".into()),
            forge_buffer::width::WidthProfile::default(),
        )
        .unwrap();
    let (sink, mut stream) = publication::channel();
    let dispatch = broker.dispatch_stream(
        Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "stream twelve paragraphs" }),
        },
        sink,
    );
    let consume = async move {
        let mut revision = opened.transcript.revision;
        // The newest paragraph the transcript had shown at each synchronization.
        let mut newest_shown = std::collections::BTreeSet::new();
        let mut newest = None;
        while let Some(event) = stream.recv().await.unwrap() {
            if !matches!(event, SessionUpdate::DocumentChanged { .. }) {
                continue;
            }
            let sync = presentation
                .lock()
                .unwrap()
                .sync(&document, revision)
                .unwrap();
            let mut rows = Vec::new();
            if let Some(snapshot) = &sync.snapshot {
                revision = snapshot.revision;
                for block in &snapshot.block {
                    rows.extend(block.text.wire_rows().into_iter().map(str::to_owned));
                }
            }
            for patch in &sync.patch {
                revision = patch.next;
                for edit in &patch.text_edit {
                    rows.extend(edit.text.wire_rows().into_iter().map(str::to_owned));
                }
            }
            newest = rows
                .iter()
                .filter_map(|row| row.trim().strip_prefix("paragraph ")?.parse::<usize>().ok())
                .max()
                .max(newest);
            newest_shown.extend(newest);
            pace.notify_one();
        }
        newest_shown
    };
    let (result, newest_shown) = tokio::join!(dispatch, consume);
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    assert!(
        newest_shown.len() >= 6,
        "the transcript advanced only at these paragraphs of 12: {newest_shown:?}"
    );
}

#[tokio::test]
async fn burst_delivery_drains_both_queues_and_keeps_the_next_prompt_usable() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let cancelled = Arc::new(AtomicBool::new(false));
    let mut broker = burst_broker(&repository, &data, cancelled.clone());
    for request_id in 1..=2 {
        let session_id = broker.session.id.clone();
        let (sink, mut stream) = publication::channel();
        let dispatch = broker.dispatch_stream(
            Request {
                id: request_id,
                method: "session.prompt.submit".into(),
                params: json!({ "text": "burst then final answer" }),
            },
            sink,
        );
        tokio::pin!(dispatch);
        // Hold the consumer until the producer reaches bounded backpressure.
        assert!(
            std::future::poll_fn(|context| {
                std::task::Poll::Ready(std::future::Future::poll(dispatch.as_mut(), context))
            })
            .await
            .is_pending()
        );
        let consume = async move {
            let mut notifications = 0;
            while let Some(event) = stream.recv().await.unwrap() {
                if matches!(event, SessionUpdate::DocumentChanged { .. }) {
                    notifications += 1;
                }
                let wire = forge_protocol::message::Message::Event(
                    publication::encode_session_event(&session_id, &event).unwrap(),
                );
                serde_json::to_value(wire)
                    .expect("broker notifications must satisfy the editor wire contract");
            }
            assert!(notifications > 0);
            // The consumer was held while the burst queued, so adjacent deltas merged.
            assert!(
                notifications < 384,
                "{notifications} publications for 384 deltas"
            );
        };
        let (result, ()) = async { tokio::join!(dispatch, consume) }.await;
        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
    }
    assert!(!cancelled.load(Ordering::Acquire));
    let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(exchange.len(), 2);
    assert!(
        exchange
            .iter()
            .all(|exchange| exchange.state() == ExchangeState::Complete)
    );
    assert!(exchange.iter().all(|exchange| {
        exchange
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .any(|message| {
                message.text == (0..384).map(|delta| delta.to_string()).collect::<String>()
            })
    }));
}

/// Coordinate the fixture's native completion and retryable cleanup barriers.
struct CleanupSignals {
    scope: std::sync::Mutex<Option<crate::provider::InvocationScope>>,
    started: Notify,
    complete: Notify,
    settled: Notify,
    native_settled: AtomicBool,
    attempt: std::sync::atomic::AtomicUsize,
}

fn cleanup_retry_provider(
    broker: &HarnessBroker,
    visible_output: bool,
    fail_first_cleanup: bool,
) -> (Arc<Provider>, Arc<CleanupSignals>) {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Cleanup lifecycle fixture".into(),
        capability: crate::provider::ProviderCapability {
            native_turn_rollback: true,
            ..Default::default()
        },
    };
    let signals = Arc::new(CleanupSignals {
        scope: std::sync::Mutex::new(None),
        started: Notify::new(),
        complete: Notify::new(),
        settled: Notify::new(),
        native_settled: AtomicBool::new(false),
        attempt: std::sync::atomic::AtomicUsize::new(usize::from(!fail_first_cleanup)),
    });
    let handler_signals = signals.clone();
    let cleanup_signals = signals.clone();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, _cancelled| {
            let signals = handler_signals.clone();
            async move {
                // The test cancels the turn, so the native turn ends interrupted at its checkpoint.
                // Native rollback only accepts an interrupted turn.
                let native_outcome = forge_provider_api::turn::TurnOutcome::Interrupted;
                let native_completed_at_ms = 0;
                start.observed(0).await?;

                use forge_provider_api::MessageClassification;

                let address = crate::provider::TurnAddress {
                    thread_id: (request.scope.clone()).owner().thread_id(),
                    turn_id: (request.scope.clone()).turn_id().unwrap(),
                    scope: request.scope.clone(),
                };
                let checkpoint = format!(
                    "cleanup-fixture:{}:{}",
                    request.submission_id.as_str(),
                    request.attempt_id.as_str()
                );

                if visible_output {
                    sink.send_wait(crate::provider::test_support::activity(
                        ProviderEvent::Item {
                            thread_id: (
                                address.clone(),
                                forge_provider_api::ItemId::new("partial-response").unwrap(),
                            )
                                .0
                                .thread_id
                                .clone(),
                            turn_id: (
                                address.clone(),
                                forge_provider_api::ItemId::new("partial-response").unwrap(),
                            )
                                .0
                                .turn_id
                                .clone(),
                            item_id: (
                                address.clone(),
                                forge_provider_api::ItemId::new("partial-response").unwrap(),
                            )
                                .1
                                .clone(),
                            event: forge_provider_api::ItemEvent::Started(
                                forge_provider_api::ItemStart::AgentMessage(
                                    forge_provider_api::AgentMessageStart {
                                        text: "Partial response".into(),
                                        classification: MessageClassification::Known(
                                            forge_provider_api::AgentMessageRole::Response,
                                        ),
                                    },
                                ),
                            ),
                        },
                    )?)
                    .await?;
                }
                *signals.scope.lock().unwrap() = Some(request.scope.clone());
                signals.started.notify_one();
                signals.complete.notified().await;

                signals.native_settled.store(true, Ordering::Release);
                signals.settled.notify_waiters();
                Ok::<_, forge_provider_api::ProviderError>(
                    forge_provider_api::TurnResult::new(
                        native_outcome,
                        native_completed_at_ms,
                        None,
                        request.input_coverage(),
                        Vec::new(),
                        Some(forge_provider_api::CheckpointId::new(checkpoint).unwrap()),
                    )
                    .expect("fixture coverage is valid"),
                )
            }
        },
    )
    .with_cleanup_request(move |_binding| {
        let signals = cleanup_signals.clone();
        async move {
            if signals.attempt.fetch_add(1, Ordering::SeqCst) == 0 {
                return Err(forge_provider_api::ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnInterrupt,
                        forge_provider_api::ProviderFailureKind::Timeout,
                    ),
                ));
            }
            signals.complete.notify_one();
            loop {
                let settled = signals.settled.notified();
                tokio::pin!(settled);
                settled.as_mut().enable();
                if signals.native_settled.load(Ordering::Acquire) {
                    break;
                }
                settled.await;
            }
            Ok(())
        }
    });
    (workflow_fixture_bridge(broker, native), signals)
}

/// Without native rewind, conversation rollback reports unsupported and changes nothing.
#[tokio::test]
async fn conversation_rollback_is_unsupported_without_native_rewind() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.capability.native_conversation_rewind = false;
    let first = completed_interaction("retained", &broker.session.id, Vec::new());
    let mut second = completed_interaction("removed", &broker.session.id, Vec::new());
    second.ordinal = 2;
    broker.store.save_exchange(&first)?;
    broker.store.save_exchange(&second)?;
    broker.session.provider_session_id = Some("retained-provider-history".into());
    broker.save_session()?;
    std::fs::write(
        repository.path().join("seed.txt"),
        "Keep these file changes\n",
    )?;
    let params =
        json!({"scope":"conversation","exchange_id":"removed","latest_exchange_id":"removed"});
    let error = broker
        .apply_rollback(params)
        .await
        .expect_err("rollback without native rewind must be rejected");
    assert!(format!("{error:#}").contains("unsupported"), "{error:#}");
    assert_eq!(
        broker.store.list_exchange(&broker.session.id)?[1].disposition(),
        crate::exchange::HistoryDisposition::Current
    );
    let session = broker.store.load_session(&broker.session.id)?.unwrap();
    assert_eq!(
        session.provider_session_id,
        Some("retained-provider-history".into())
    );
    assert_eq!(
        std::fs::read_to_string(repository.path().join("seed.txt"))?,
        "Keep these file changes\n"
    );
    Ok(())
}

fn failing_provider_fixture(
    workspace: PathBuf,
) -> Result<crate::provider::test_support::ProviderTestSession> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized failure fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, _sink, start, _cancelled| {
            let workspace = workspace.clone();

            async move {
                start.observed(0).await?;

                let nested = workspace.join("failed/deep/write.txt");
                std::fs::create_dir_all(nested.parent().expect("nested path has parent")).map_err(
                    |_| {
                        crate::provider::ProviderError::Failure(
                            forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::TurnStart,
                                forge_provider_api::ProviderFailureKind::Unknown,
                            ),
                        )
                    },
                )?;
                std::fs::write(&nested, "written before provider failure\n").map_err(|_| {
                    crate::provider::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Unknown,
                        ),
                    )
                })?;
                let native_outcome = forge_provider_api::turn::TurnOutcome::Failed {
                    error: forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Unknown,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Observation,
                        forge_provider_api::FailureDisposition::Uncertain,
                    ),
                    retry: forge_provider_api::turn::RetryDecision::NotAllowed,
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
    crate::provider::test_support::ProviderTestSession::from_native(fixture)
}

#[derive(Clone, Copy)]
enum PlanFixtureBehavior {
    Questions { structured: bool },
}

/// Read the active plan's ID and version from the identity line Harness prepends to planning
/// prompts.
fn plan_document_in_request(request: &forge_provider_api::PromptRequest) -> Option<Value> {
    let mut text = request.user_message_block();
    if let Some(forge_provider_api::input::PromptInput::Text(input)) = request.input.user_input() {
        text.push('\n');
        text.push_str(input);
    }
    for recovered in &request.recovered_inputs {
        if let Some(forge_provider_api::input::PromptInput::Text(input)) =
            recovered.input.user_input()
        {
            text.push('\n');
            text.push_str(input);
        }
    }
    let marker = "Active plan `";
    let start = text.find(marker)? + marker.len();
    let (plan_id, rest) = text[start..].split_once("` is at version ")?;
    let version = rest
        .split(|character: char| !character.is_ascii_digit())
        .next()?
        .parse::<u64>()
        .ok()?;
    Some(json!({"plan_id": plan_id, "version": version}))
}

async fn plan_fixture_tool(
    request: &forge_provider_api::PromptRequest,
    submit: bool,
) -> Result<(), forge_provider_api::ProviderError> {
    let Some(document) = plan_document_in_request(request) else {
        return Ok(());
    };
    let Some(plan_id) = document.get("plan_id").and_then(Value::as_str) else {
        return Ok(());
    };
    let read = crate::provider::test_support::call_tool(
        request,
        "plan_read",
        json!({"plan_id":plan_id,"path":"plan:overview.md"}),
    )
    .await?;
    let read = read.as_str().unwrap_or_default();
    let version = shared_version(read)
        .or_else(|| document.get("version").and_then(Value::as_u64))
        .unwrap_or(1);
    // Fill only the sections submission requires, so overview text a revision already reviewed
    // keeps its exact content.
    let current = read
        .lines()
        .skip(2)
        .map(|line| line.split_once(": ").map_or("", |(_, text)| text))
        .collect::<Vec<_>>();
    let mut filled = Vec::new();
    for (index, line) in current.iter().enumerate() {
        filled.push((*line).to_owned());
        let fallback = match *line {
            "## Objective" => "Implement the requested migration.",
            "## Background" => "Preserve the existing caller contract.",
            "## Design" => "Keep ownership explicit and preserve compatibility.",
            _ => continue,
        };
        if current
            .get(index + 1)
            .is_none_or(|next| next.trim().is_empty() || next.starts_with("## "))
        {
            filled.push(fallback.to_owned());
        }
    }
    if !request.tools.iter().any(|tool| tool.name == "plan_edit") {
        return Err(forge_provider_api::ProviderError::Failure(
            forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Authorization,
            ),
        ));
    }
    let patch = format!(
        "*** Begin Patch\n*** Add File: plan:overview.md\n{}\n*** End Patch",
        filled
            .iter()
            .map(|line| format!("+{line}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    let edited = request
        .provider_tools
        .call(
            &request.scope,
            forge_provider_api::control::ToolCall {
                id: forge_provider_api::control::ToolCallId::new("fixture-call-1")
                    .expect("fixture call identity is nonempty"),
                name: "plan_edit".into(),
                arguments: json!({
                    "plan_id":plan_id,
                    "expected_version":version,
                    "patch":patch
                }),
            },
        )
        .await?;
    if submit {
        let version = shared_version(&edited).unwrap_or(version);
        if !request.tools.iter().any(|tool| tool.name == "plan_submit") {
            return Err(forge_provider_api::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Authorization,
                ),
            ));
        }
        request
            .provider_tools
            .call(
                &request.scope,
                forge_provider_api::control::ToolCall {
                    id: forge_provider_api::control::ToolCallId::new("fixture-call-2")
                        .expect("fixture call identity is nonempty"),
                    name: "plan_submit".into(),
                    arguments: json!({"plan_id":plan_id,"expected_version":version}),
                },
            )
            .await?;
    }
    Ok(())
}

/// Read the shared plan version from a plan_read header or a plan_edit confirmation.
fn shared_version(text: &str) -> Option<u64> {
    let (_, rest) = text
        .split_once("· version ")
        .or_else(|| text.split_once("version is "))?;
    rest.chars()
        .take_while(char::is_ascii_digit)
        .collect::<String>()
        .parse()
        .ok()
}

fn plan_fixture_provider(broker: &HarnessBroker, behavior: PlanFixtureBehavior) -> Arc<Provider> {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Plan workflow fixture".into(),
        capability: Default::default(),
    };
    let turn = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, start, _cancelled| {
            let turn = turn.clone();
            async move {
                start.observed(0).await?;

                let index = turn.fetch_add(1, Ordering::SeqCst);
                match behavior {
                    PlanFixtureBehavior::Questions { structured: true } if index == 0 => {
                        let _question = crate::provider::test_support::call_tool(&request, "question_ask", json!({
                            "questions":[{"header":"Migration","question":"Which migration strategy should the plan use?","options":[
                                {"label":"Staged","description":"Support both formats temporarily.","recommended":false},
                                {"label":"Immediate","description":"Remove the old format now.","recommended":false}
                            ],"allow_freeform":true}]
                        })).await?;
                    }
                    PlanFixtureBehavior::Questions { .. }
                        if index > 0
                            && request.execution == forge_provider_api::ExecutionMode::Planning =>
                    {
                        plan_fixture_tool(&request, true).await?;
                    }
                    _ => {}
                }
                let scope = request.scope.clone();
                let address = crate::provider::TurnAddress {
                    thread_id: (scope.clone()).owner().thread_id(),
                    turn_id: (scope.clone()).turn_id().unwrap(),
                    scope: scope.clone(),
                };

                for event in completed_message(
                    &address,
                    format!("plan-response-{index}"),
                    crate::provider::TestMessageContent::Response,
                    if index == 0 {
                        "Which migration strategy should the plan use?"
                    } else {
                        "The plan is ready for review."
                    },
                ) {
                    sink.send_wait(crate::provider::test_support::activity(event)?)
                        .await?;
                }
                let native_outcome = forge_provider_api::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                };
                let native_completed_at_ms = index as i64 + 1;

                Ok::<_, forge_provider_api::ProviderError>(
                    forge_provider_api::TurnResult::new(
                        native_outcome,
                        native_completed_at_ms,
                        None,
                        request.input_coverage(),
                        Vec::new(),
                        None,
                    )
                    .expect("fixture coverage is valid"),
                )
            }
        },
    );
    workflow_fixture_bridge(broker, fixture)
}
fn nested_write_provider(broker: &HarnessBroker, workspace: &Path) -> Arc<Provider> {
    let workspace = workspace.to_owned();
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Nested write fixture".into(),
        capability: Default::default(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, start, _cancelled| {
            let workspace = workspace.clone();
            async move {
                start.observed(0).await?;

                use forge_provider_api::ProviderEvent;
                use forge_provider_api::tool::ToolOutcome;

                let scope = request.scope.clone();
                let turn = crate::provider::TurnAddress {
                    thread_id: (scope.clone()).owner().thread_id(),
                    turn_id: (scope.clone()).turn_id().unwrap(),
                    scope: scope.clone(),
                };

                for event in completed_message(
                    &turn,
                    "nested-commentary",
                    crate::provider::TestMessageContent::Commentary,
                    "Creating the nested module.",
                ) {
                    sink.send_wait(crate::provider::test_support::activity(event)?)
                        .await?;
                }

                let file_changes = forge_provider_api::ProviderChangeSet {
                    file: vec![forge_provider_api::ProviderFileChange {
                        path: "seed.txt".into(),
                        move_path: None,
                        kind: forge_provider_api::ProviderChangeKind::Update,
                        diff: "@@ -1 +1 @@\n-seed\n+provider edit".into(),
                    }],
                };
                let file_call = (
                    turn.clone(),
                    forge_provider_api::ItemId::new("provider-file-change").unwrap(),
                );
                let file_invocation = forge_provider_api::ToolDescriptor {
                    semantics: Default::default(),
                    name: "fixture".into(),
                    title: "file changes".into(),
                    arguments: forge_provider_api::InspectableArguments::new(Value::Null),
                };
                sink.send_wait(crate::provider::test_support::activity({
                    let invocation = file_invocation;
                    let descriptor = invocation;
                    ProviderEvent::Item {
                        thread_id: (file_call.clone()).0.thread_id.clone(),
                        turn_id: (file_call.clone()).0.turn_id.clone(),
                        item_id: (file_call.clone()).1.clone(),
                        event: forge_provider_api::ItemEvent::Started(
                            forge_provider_api::ItemStart::FileChange(descriptor),
                        ),
                    }
                })?)
                .await?;
                sink.send_wait(crate::provider::test_support::activity(
                    ProviderEvent::Item {
                        thread_id: (file_call.clone()).0.thread_id.clone(),
                        turn_id: (file_call.clone()).0.turn_id.clone(),
                        item_id: (file_call.clone()).1.clone(),
                        event: forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::ExecutionStarted,
                        },
                    },
                )?)
                .await?;
                sink.send_wait(crate::provider::test_support::activity(
                    ProviderEvent::Item {
                        thread_id: (file_call.clone()).0.thread_id.clone(),
                        turn_id: (file_call.clone()).0.turn_id.clone(),
                        item_id: (file_call.clone()).1.clone(),
                        event: forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::Changes(file_changes.clone()),
                        },
                    },
                )?)
                .await?;
                // Native edits wait on the host barrier, which opens once the baseline
                // checkpoint is durable, as a provider's pre-tool hook does.
                request.permissions.before_tool(&request.scope).await?;
                std::fs::write(workspace.join("seed.txt"), "provider edit\n")
                    .map_err(crate::provider::test_support::io_failure)?;
                sink.send_wait(crate::provider::test_support::activity(
                    ProviderEvent::Item {
                        thread_id: (file_call).0.thread_id.clone(),
                        turn_id: (file_call).0.turn_id.clone(),
                        item_id: (file_call).1.clone(),
                        event: {
                            let completion = forge_provider_api::ToolCompletion {
                                outcome: ToolOutcome::Succeeded,
                                descriptor: None,
                                output: None,
                                changes: Default::default(),
                            };
                            forge_provider_api::ItemEvent::Completed(
                                forge_provider_api::ItemCompletion::FileChange(completion),
                            )
                        },
                    },
                )?)
                .await?;

                let command_call = (
                    turn.clone(),
                    forge_provider_api::ItemId::new("nested-command").unwrap(),
                );
                sink.send_wait(crate::provider::test_support::activity({
                    let invocation = forge_provider_api::ToolDescriptor {
                        semantics: Default::default(),
                        name: "fixture".into(),
                        title: "create nested module".into(),
                        arguments: forge_provider_api::InspectableArguments::new(Value::Null),
                    };
                    let descriptor = invocation;
                    ProviderEvent::Item {
                        thread_id: (command_call.clone()).0.thread_id.clone(),
                        turn_id: (command_call.clone()).0.turn_id.clone(),
                        item_id: (command_call.clone()).1.clone(),
                        event: forge_provider_api::ItemEvent::Started(
                            forge_provider_api::ItemStart::CommandExecution(descriptor),
                        ),
                    }
                })?)
                .await?;
                sink.send_wait(crate::provider::test_support::activity(
                    ProviderEvent::Item {
                        thread_id: (command_call.clone()).0.thread_id.clone(),
                        turn_id: (command_call.clone()).0.turn_id.clone(),
                        item_id: (command_call.clone()).1.clone(),
                        event: forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::ExecutionStarted,
                        },
                    },
                )?)
                .await?;
                let nested = workspace.join("apps/new/deep/module");
                std::fs::create_dir_all(&nested)
                    .map_err(crate::provider::test_support::io_failure)?;
                std::fs::write(nested.join("lib.rs"), "pub fn nested() {}\n")
                    .map_err(crate::provider::test_support::io_failure)?;
                let ignored = workspace.join("target/generated/deep");
                std::fs::create_dir_all(&ignored)
                    .map_err(crate::provider::test_support::io_failure)?;
                std::fs::write(ignored.join("artifact.txt"), "ignored\n")
                    .map_err(crate::provider::test_support::io_failure)?;
                sink.send_wait(crate::provider::test_support::activity(
                    ProviderEvent::Item {
                        thread_id: (command_call.clone()).0.thread_id.clone(),
                        turn_id: (command_call.clone()).0.turn_id.clone(),
                        item_id: (command_call.clone()).1.clone(),
                        event: forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::Output(
                                forge_provider_api::tool::TextUpdate::Append(
                                    "created nested module".into(),
                                ),
                            ),
                        },
                    },
                )?)
                .await?;
                sink.send_wait(crate::provider::test_support::activity(
                    ProviderEvent::Item {
                        thread_id: (command_call).0.thread_id.clone(),
                        turn_id: (command_call).0.turn_id.clone(),
                        item_id: (command_call).1.clone(),
                        event: {
                            let completion = forge_provider_api::ToolCompletion {
                                outcome: ToolOutcome::Succeeded,
                                descriptor: None,
                                output: None,
                                changes: Default::default(),
                            };
                            forge_provider_api::ItemEvent::Completed(
                                forge_provider_api::ItemCompletion::CommandExecution(completion),
                            )
                        },
                    },
                )?)
                .await?;
                for event in completed_message(
                    &turn,
                    "nested-response",
                    crate::provider::TestMessageContent::Response,
                    "The nested module is ready.",
                ) {
                    sink.send_wait(crate::provider::test_support::activity(event)?)
                        .await?;
                }
                let native_outcome = forge_provider_api::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                };
                let native_completed_at_ms = 1;

                Ok::<_, forge_provider_api::ProviderError>(
                    forge_provider_api::TurnResult::new(
                        native_outcome,
                        native_completed_at_ms,
                        None,
                        request.input_coverage(),
                        Vec::new(),
                        None,
                    )
                    .expect("fixture coverage is valid"),
                )
            }
        },
    );
    workflow_fixture_bridge(broker, fixture)
}

struct FixedClock(i64);
impl Clock for FixedClock {
    fn now_ms(&self) -> i64 {
        self.0
    }
}

fn git(workspace: &Path, args: &[&str]) {
    let status = std::process::Command::new("git")
        .args(args)
        .current_dir(workspace)
        .status()
        .unwrap();
    assert!(status.success());
}

async fn dispatch_pending_task_input(
    broker: &mut HarnessBroker,
) -> Result<(Value, Vec<SessionEvent>)> {
    let task = broker
        .task_controller
        .current()?
        .context("task is missing")?;
    let (instruction, purpose) = if let Some(entry) = task.entry.as_ref()
        && entry.delivery == crate::task::TaskInputDelivery::Pending
    {
        anyhow::ensure!(
            broker.task_controller.acknowledge_entry(
                &mut broker.store,
                &task.revision,
                &entry.id,
                broker.clock.now_ms()
            )?,
            "task entry was replaced"
        );
        (
            entry.instruction.clone(),
            crate::input::InputPurpose::TaskEntry {
                revision: task.revision.clone(),
                id: entry.id.clone(),
                start: (!entry.resumed).then(|| crate::exchange::TaskStart {
                    kind: task.kind(),
                    title: task.title().to_owned(),
                }),
            },
        )
    } else {
        let turn = task.turn.as_ref().context("task turn is missing")?;
        let continuation = turn
            .continuation
            .as_ref()
            .context("task continuation is missing")?;
        anyhow::ensure!(
            continuation.delivery == crate::task::TaskInputDelivery::Pending,
            "task continuation was already delivered"
        );
        anyhow::ensure!(
            broker.task_controller.acknowledge_continuation(
                &mut broker.store,
                &task.revision,
                &continuation.id,
                broker.clock.now_ms()
            )?,
            "task continuation was replaced"
        );
        (
            continuation.work.instruction().unwrap().clone(),
            crate::input::InputPurpose::TaskContinuation {
                revision: task.revision.clone(),
                id: continuation.id.clone(),
                completed_turn_id: turn.id.clone(),
                reason: continuation.reason.clone(),
            },
        )
    };
    broker.task_controller.admit(task.kind())?;
    broker.input_purpose = purpose;
    // The submitted text is the entry's label; dispatch reads the typed input from the task.
    let result = broker
        .submit_prompt(json!({"text": instruction.label()}))
        .await;
    broker.input_purpose = crate::input::InputPurpose::User;
    result
}

fn mutable_question_fixture() -> Arc<crate::provider::test_support::EventFixtureProvider> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized question fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let turns = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, start, _cancelled| {
            let turns = turns.clone();

            async move {
                start.observed(0).await?;

                let turn = turns.fetch_add(1, Ordering::SeqCst);
                let scope = request.scope.clone();
                if turn == 0 {
                    crate::provider::test_support::call_tool(&request, "question_ask", json!({
                    "questions":[{"header":"Migration","question":"Which migration should the implementation use?","options":[
                        {"label":"Staged","description":"Preserve compatibility temporarily.","recommended":false},
                        {"label":"Immediate","description":"Replace the old format now.","recommended":false}
                    ],"allow_freeform":true}]
                })).await?;
                }
                let address = crate::provider::TurnAddress {
                    thread_id: (scope.clone()).owner().thread_id(),
                    turn_id: (scope.clone()).turn_id().unwrap(),
                    scope: scope.clone(),
                };

                for event in completed_message(
                    &address,
                    "question-response",
                    crate::provider::TestMessageContent::Response,
                    if turn == 0 {
                        "Which migration strategy should the plan use?"
                    } else {
                        "The migration strategy is recorded."
                    },
                ) {
                    sink.send_wait(crate::provider::test_support::activity(event)?)
                        .await?;
                }
                let native_outcome = forge_provider_api::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                };
                let native_completed_at_ms = turn as i64 + 1;

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

fn mutable_question_broker(repository: &Path, data_root: &Path) -> HarnessBroker {
    let mut broker = planning_question_broker(repository, data_root, false);
    let native = mutable_question_fixture();
    broker.provider = Arc::new(crate::provider::Provider::new(
        native.registration(),
        broker.permission_coordinator.clone(),
        broker.trace.clone(),
        broker.data_root.clone(),
    ));
    broker
}

#[test]
fn question_discussion_is_read_only_without_changing_session_access() {
    let repository = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session.access.sandbox = false;
    broker.session.access.write_access = crate::session::WriteScope::Full;
    broker.question_discussion = true;
    let request = broker
        .provider_request(
            ProviderInput::from_user_text("Explain this choice"),
            PromptMode::Chat,
        )
        .unwrap();
    assert!(!request.access.sandbox);
    assert_eq!(request.access.write_access, crate::session::WriteScope::Off);
    assert!(request.access.writable_directory.is_empty());
    assert!(!broker.session.access.sandbox);
    assert_eq!(
        broker.session.access.write_access,
        crate::session::WriteScope::Full
    );
}

#[tokio::test]
async fn session_mutations_enforce_target_ownership() -> Result<()> {
    use std::fs;

    let source_workspace = tempfile::tempdir()?;
    let target_workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut source = mutable_question_broker(source_workspace.path(), data.path());
    let target = mutable_question_broker(target_workspace.path(), data.path());
    let target_id = target.session.id.clone();
    let target_name = target.session.name.clone();
    let plan_directory = crate::storage::state::directory(data.path())
        .join("plans")
        .join(&target_id);
    fs::create_dir_all(&plan_directory)?;
    let plan = plan_directory.join("preserved.md");
    fs::write(&plan, "preserve active plan")?;
    for (id, method, params) in [
        (
            1,
            "session.rename",
            json!({"session_id":target_id,"name":"invalid"}),
        ),
        (2, "session.delete", json!({"session_id":target_id})),
    ] {
        let response = source
            .dispatch(Request {
                id,
                method: method.into(),
                params,
            })
            .await;
        assert!(
            response.response.error().is_some(),
            "{method} accepted a foreign owner"
        );
        assert_eq!(
            source.store.load_session(&target_id)?.unwrap().name,
            target_name
        );
        assert_eq!(fs::read_to_string(&plan)?, "preserve active plan");
    }
    let source_id = source.session.id.clone();
    let rename = source
        .dispatch(Request {
            id: 3,
            method: "session.rename".into(),
            params: json!({"session_id":source_id,"name":"owned rename"}),
        })
        .await;
    assert!(rename.response.error().is_none());
    assert_eq!(source.session.name, "owned rename");
    drop(target);
    let deletion = source
        .dispatch(Request {
            id: 4,
            method: "session.delete".into(),
            params: json!({"session_id":target_id}),
        })
        .await;
    assert!(deletion.response.error().is_none());
    assert!(source.store.load_session(&target_id)?.is_none());
    assert!(!plan_directory.exists());
    Ok(())
}

/// Persist late background output and completion only on the admitted historical owner.
#[tokio::test]
async fn background_tool_completes_on_settled_owner_without_touching_current_turn() -> Result<()> {
    use forge_provider_api::{
        InspectableArguments, ItemCompletion, ItemEvent, ItemStart, TextUpdate, ToolCompletion,
        ToolDescriptor, ToolItemEvent, ToolOutcome,
    };
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut settled = completed_interaction("background-owner", &broker.session.id, Vec::new());
    let provider = crate::turn::test_support::address(&settled, "background-provider-turn");
    let mut turn =
        crate::turn::test_support::TestTurn::new("background-turn".into(), provider.clone(), 1);
    for event in [
        ItemEvent::Started(ItemStart::CommandExecution(ToolDescriptor {
            name: "exec".into(),
            title: "Background command".into(),
            semantics: Default::default(),
            arguments: InspectableArguments::new(Value::Null),
        })),
        ItemEvent::Tool {
            event: ToolItemEvent::ExecutionStarted,
        },
        ItemEvent::Tool {
            event: ToolItemEvent::Output(TextUpdate::Append("START\n".into())),
        },
        ItemEvent::Tool {
            event: ToolItemEvent::Backgrounded,
        },
    ] {
        turn.record_tool("background", &event, 2)?;
    }
    turn.finish(
        crate::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        },
        3,
    )?;
    let recorded = turn.into_exchange();
    settled.turn = recorded.turn;
    settled.node_list = recorded.node_list;
    settled.entry = recorded.entry;
    settled.rebuild_tool_counter();
    broker.store.save_exchange(&settled)?;

    let mut current = completed_interaction("foreground-owner", &broker.session.id, Vec::new());
    current.ordinal = 2;
    broker.store.save_exchange(&current)?;
    let current_before = serde_json::to_value(&current)?;
    let item_id = forge_provider_api::ItemId::new("background")?;
    let completion = |outcome| {
        ItemEvent::Completed(ItemCompletion::CommandExecution(ToolCompletion {
            outcome,
            descriptor: None,
            output: None,
            changes: Default::default(),
        }))
    };
    let mut event = Vec::new();
    for update in [
        ItemEvent::Tool {
            event: ToolItemEvent::Output(TextUpdate::Append("END\n".into())),
        },
        completion(ToolOutcome::Succeeded),
    ] {
        assert!(
            broker
                .test_apply_settled_parent_event(
                    &current,
                    &crate::turn::test_support::item_event(&provider, item_id.as_str(), update),
                    &mut event
                )
                .await?
        );
    }
    let saved = broker.store.load_exchange(&settled.id)?.unwrap();
    let tool = saved.tools().next().unwrap().1;
    assert_eq!(tool.state(), crate::turn::ToolState::Completed);
    assert_eq!(tool.output, "START\nEND\n");
    assert_eq!(saved.state(), ExchangeState::Complete);
    assert_eq!(
        serde_json::to_value(broker.store.load_exchange(&current.id)?.unwrap())?,
        current_before
    );
    assert!(!event.is_empty());
    let unknown = forge_provider_api::ItemId::new("unknown")?;
    assert!(!saved.accepts_settled_tool_update(
        &provider,
        &unknown,
        &completion(ToolOutcome::Succeeded)
    ));
    assert!(!saved.accepts_settled_tool_update(
        &provider,
        &item_id,
        &completion(ToolOutcome::Failed {
            diagnostic: crate::provider::ToolDiagnostic {
                kind: forge_provider_api::ProviderFailureKind::Unknown,
                message: forge_provider_api::SanitizedMessage::new("late contradiction")?
            },
        })
    ));
    Ok(())
}

#[tokio::test]
async fn auto_compaction_requires_fresh_usage_and_runs_once_per_turn() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "data_root":data.path(),"workspace":repository.path(),"client_id":"automatic-compaction",
        "provider":{"kind":"mock","command":[]},"model":"mock-model","effort":"low"
    }))?)?;
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"First turn"}),
        })
        .await;
    assert!(result.response.error().is_none());
    broker.session.auto_compaction = crate::session::AutoCompaction {
        enabled: true,
        threshold_tokens: 40_000,
    };
    broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
    broker.context_usage_fresh = false;
    broker.auto_compact().await?;
    assert!(
        broker
            .store
            .list_session_event(&broker.session.id)?
            .is_empty()
    );
    broker.context_usage_fresh = true;
    broker.auto_compact().await?;
    let records = broker.store.list_session_event(&broker.session.id)?;
    assert_eq!(records.len(), 1);
    let SessionEventKind::Compaction { compaction } = &records[0].detail else {
        panic!("missing compaction")
    };
    assert!(!compaction.manual);
    broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
    broker.auto_compact().await?;
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        1
    );
    let result = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Second turn"}),
        })
        .await;
    assert!(result.response.error().is_none());
    assert!(
        !broker.context_usage_fresh,
        "new input reused prior-turn context usage"
    );
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        1,
        "a turn without fresh usage triggered compaction"
    );
    broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
    broker.context_usage_fresh = true;
    broker.auto_compact().await?;
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        2
    );
    let result = broker
        .dispatch(Request {
            id: 3,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Third turn"}),
        })
        .await;
    assert!(result.response.error().is_none());
    assert!(!broker.context_usage_fresh);
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        2
    );
    broker.store.save_session_event(&SessionEventRecord {
        id: "native-running".into(),
        session_id: broker.session.id.clone(),
        owner: crate::timeline::TimelineOwner::session(&broker.session.id),
        created_at_ms: 1,
        detail: SessionEventKind::Compaction {
            compaction: (crate::provider::Compaction {
                id: "native-running".into(),
                thread_id: crate::provider::ThreadOwner::root(&broker.session.id)
                    .thread_id()
                    .as_str()
                    .to_owned(),
                state: crate::provider::CompactionState::Running,
                manual: false,
                started_at_ms: Some(1),
                completed_at_ms: None,
                result: Default::default(),
            })
            .into(),
        },
    })?;
    broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
    broker.context_usage_fresh = true;
    broker.auto_compact().await?;
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        3,
        "native compaction must exclude another request"
    );
    Ok(())
}

#[test]
fn explicit_session_initialization_validates_before_leasing_and_resumes_exact_id() {
    let workspace = repository();
    let other_workspace = repository();
    let data = tempfile::tempdir().unwrap();
    let mut request = InitializeRequest {
        background: None,
        provider_launch_by_provider: Default::default(),
        config_dir: None,
        data_root: data.path().to_string_lossy().into_owned(),
        permission_file: None,
        workspace: workspace.path().to_string_lossy().into_owned(),
        client_id: "source-client".into(),
        provider: ProviderLaunch {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            command: vec!["mock".into()],
        },
        model: "mock-model".into(),
        effort: "low".into(),
        session_id: None,
        new_session_name: Some("source".into()),
        goal_max_turns: 20,
        lease_conflict_action: None,
    };
    let mut source =
        HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(100))).unwrap();
    source.release_lease().unwrap();
    request.client_id = "destination-client".into();
    request.new_session_name = Some(String::new());
    let newest =
        HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(110))).unwrap();
    assert_ne!(source.session.id, newest.session.id);
    request.new_session_name = None;
    request.session_id = Some(source.session.id.clone());

    let mut incompatible = request.clone();
    incompatible.workspace = other_workspace.path().to_string_lossy().into_owned();
    let failure = HarnessBroker::initialize_with_clock(incompatible, Box::new(FixedClock(120)))
        .err()
        .unwrap();
    assert!(failure.to_string().contains("different workspace"));
    assert!(
        source
            .store
            .load_session(&source.session.id)
            .unwrap()
            .unwrap()
            .lease_owner
            .is_none()
    );

    let mut stored = source
        .store
        .load_session(&source.session.id)
        .unwrap()
        .unwrap();
    stored.provider = forge_providers::registry::alternate_id(&request.provider.kind)
        .expect("alternate registered provider")
        .to_string();
    source.store.save_session(&stored).unwrap();
    let failure = HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(130)))
        .err()
        .unwrap();
    assert!(
        failure
            .to_string()
            .contains("different configured provider")
    );
    assert!(
        source
            .store
            .load_session(&source.session.id)
            .unwrap()
            .unwrap()
            .lease_owner
            .is_none()
    );
    stored.provider = "mock".into();
    stored.approval_mode = ApprovalMode::Ask;
    stored.workspace = format!("{}//./", stored.workspace);
    source.store.save_session(&stored).unwrap();

    let resumed = HarnessBroker::initialize_with_clock(request, Box::new(FixedClock(140))).unwrap();
    assert_eq!(resumed.session.id, source.session.id);
    assert_eq!(resumed.session.name, "source");
    assert_eq!(resumed.session.approval_mode, ApprovalMode::Ask);
    assert_eq!(resumed.store.list_session(None).unwrap().len(), 2);
}

#[tokio::test]
async fn forks_a_live_session_without_taking_its_lease() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let initialize = |client_id: &str, lease_conflict_action: Option<String>| InitializeRequest {
        background: None,
        provider_launch_by_provider: Default::default(),
        config_dir: None,
        data_root: data.path().to_string_lossy().into_owned(),
        permission_file: None,
        workspace: repository.path().to_string_lossy().into_owned(),
        client_id: client_id.into(),
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
        lease_conflict_action,
    };

    let mut source = HarnessBroker::initialize_with_clock(
        initialize("source-client", None),
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let source_session_id = source.session.id.clone();
    source
        .dispatch(Request {
            id: 0,
            method: "session.permission.set".into(),
            params: json!({ "change":{"kind":"set_approval","mode":"ask"} }),
        })
        .await;
    let prompt = source
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "preserve this interaction" }),
        })
        .await;
    assert!(prompt.response.error().is_none());
    assert!(source.session.native_fork);
    source.session.context_usage = ContextUsage::reported(50, 200);
    source.save_session().unwrap();

    let conflict = match HarnessBroker::initialize_with_clock(
        initialize("fork-client", None),
        Box::new(FixedClock(110)),
    ) {
        Ok(_) => panic!("second controller should encounter the live lease"),
        Err(error) => error,
    };
    let conflict = conflict
        .downcast_ref::<crate::session::SessionLeaseConflict>()
        .expect("structured lease conflict");
    assert_eq!(conflict.session_id, source_session_id);
    assert!(conflict.native_fork);

    let fork_controller = HarnessBroker::initialize_with_clock(
        initialize("fork-client", Some("new".into())),
        Box::new(FixedClock(110)),
    )
    .unwrap();
    let preparation = prepare_provider_fork(
        data.path(),
        "fork-client",
        "mock",
        &fork_controller.capability,
        &fork_controller.session.id,
        &json!({ "session_id": source_session_id, "name": "fork investigation" }),
        crate::broker::ForkOptions {
            purpose: crate::session::SessionPurpose::Conversation,
            source_is_executing: &|| false,
        },
    )
    .unwrap();
    assert!(matches!(
        preparation.child.provider_fork_state,
        ProviderForkState::Preparing { .. }
    ));
    let provider_request = preparation
        .provider_request
        .expect("an idle source forks its native conversation");
    assert_eq!(provider_request.checkpoint_id.as_deref(), None);
    let child_session_id = preparation.child.id.clone();
    let inherited_source = source
        .snapshot()
        .unwrap()
        .timeline
        .into_iter()
        .filter(|entry| !matches!(entry, TimelineEntry::Status { .. }))
        .map(|entry| serde_json::to_value(entry).unwrap())
        .collect::<Vec<_>>();
    let provider_fork = fork_controller
        .provider
        .thread_fork(provider_request)
        .await
        .unwrap();
    let mut child_initialize = initialize("fork-client", None);
    child_initialize.session_id = Some(child_session_id.clone());
    let mut child_controller = HarnessBroker::initialize_with_clock(
        child_initialize,
        Box::new(FixedClock(preparation.child.created_at_ms + 1)),
    )
    .unwrap();
    let ready_event = child_controller
        .complete_provider_fork(Ok(provider_fork))
        .unwrap();
    assert_eq!(ready_event.event, "session_fork_ready");
    let snapshot = serde_json::to_value(child_controller.snapshot().unwrap()).unwrap();
    assert_ne!(snapshot["session"]["id"], source_session_id);
    assert_eq!(snapshot["session"]["name"], "fork investigation");
    assert_eq!(snapshot["session"]["approval_mode"], "ask");
    assert_eq!(snapshot["session"]["provider_fork_state"]["state"], "ready");
    assert_eq!(snapshot["session"]["context_usage"]["used"], 50);
    assert_eq!(snapshot["session"]["context_usage"]["size"], 200);
    assert_eq!(
        snapshot["session"]["context_usage"]["remaining_percent"],
        75
    );
    assert!(snapshot.get("exchange").is_none() && snapshot.get("timeline").is_none());
    let timeline = child_controller.snapshot().unwrap().timeline;
    assert_eq!(timeline.len(), inherited_source.len() + 2);
    let inherited = timeline
        .iter()
        .filter_map(|entry| match entry {
            TimelineEntry::Inherited { entry, .. } => Some(serde_json::to_value(entry).unwrap()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(inherited, inherited_source);
    assert!(
        inherited
            .iter()
            .any(|entry| entry["exchange"]["prompt"] == "preserve this interaction")
    );
    assert!(
        child_controller
            .store
            .list_exchange(&child_session_id)
            .unwrap()
            .is_empty()
    );
    assert!(
        child_controller
            .task_controller
            .current()
            .unwrap()
            .is_none()
    );
    assert!(matches!(
        &timeline[inherited.len()],
        TimelineEntry::SessionEvent { .. }
    ));
    assert!(matches!(
        timeline.last().unwrap(),
        TimelineEntry::Status {
            status: SessionPhase::Idle,
            ..
        }
    ));
    let failed_preparation = prepare_provider_fork(
        data.path(),
        "fork-client",
        "mock",
        &fork_controller.capability,
        &fork_controller.session.id,
        &json!({ "session_id": source_session_id }),
        crate::broker::ForkOptions {
            purpose: crate::session::SessionPurpose::Conversation,
            source_is_executing: &|| false,
        },
    )
    .unwrap();
    let mut failed_initialize = initialize("fork-client", None);
    failed_initialize.session_id = Some(failed_preparation.child.id);
    let mut failed_controller =
        HarnessBroker::initialize_with_clock(failed_initialize, Box::new(FixedClock(130))).unwrap();
    let failed_event = failed_controller
        .complete_provider_fork(Err("native fork failed".into()))
        .unwrap();
    assert_eq!(failed_event.event, "session_fork_failed");
    assert!(matches!(
        failed_controller.provider_fork_state(),
        ProviderForkState::Failed { message, .. } if message == "native fork failed"
    ));
    assert!(
        failed_controller
            .snapshot()
            .unwrap()
            .timeline
            .iter()
            .any(|entry| matches!(entry, TimelineEntry::Inherited { .. }))
    );

    let later_source = source
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "source-only later message" }),
        })
        .await;
    assert!(later_source.response.error().is_none());
    let child_prompt = child_controller
        .dispatch(Request {
            id: 3,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "child-only new message" }),
        })
        .await;
    assert!(child_prompt.response.error().is_none());
    let child_timeline = child_controller.snapshot().unwrap().timeline;
    let child_inherited = child_timeline
        .iter()
        .filter_map(|entry| match entry {
            TimelineEntry::Inherited { entry, .. } => Some(serde_json::to_value(entry).unwrap()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(child_inherited, inherited_source);
    assert!(child_timeline.iter().skip(inherited_source.len() + 1).any(
        |entry| matches!(entry, TimelineEntry::Exchange { exchange, .. }
            if exchange.prompt == "child-only new message")
    ));
    let preview = source
        .dispatch(Request {
            id: 4,
            method: "session.preview".into(),
            params: json!({ "session_id": child_session_id }),
        })
        .await;
    assert!(preview.response.error().is_none());
    assert!(
        serde_json::to_string(preview.response.result().unwrap())
            .unwrap()
            .contains("preserve this interaction")
    );

    let grandchild = prepare_provider_fork(
        data.path(),
        "fork-client",
        "mock",
        &child_controller.capability,
        &child_session_id,
        &json!({ "session_id": child_session_id }),
        crate::broker::ForkOptions {
            purpose: crate::session::SessionPurpose::Conversation,
            source_is_executing: &|| false,
        },
    )
    .unwrap();
    let grandchild_history = child_controller
        .store
        .with_fork_history(
            &grandchild.child.id,
            Vec::new(),
            &child_controller.plan_file,
        )
        .unwrap();
    assert_eq!(grandchild_history.len(), child_timeline.len() - 1);
    for (inherited, source) in grandchild_history.iter().zip(&child_timeline) {
        let TimelineEntry::Inherited { entry, .. } = inherited else {
            panic!("grandchild history must be presentation-only");
        };
        assert!(!matches!(entry.as_ref(), TimelineEntry::Inherited { .. }));
        let source = match source {
            TimelineEntry::Inherited { entry, .. } => entry.as_ref(),
            source => source,
        };
        assert_eq!(
            serde_json::to_value(entry).unwrap(),
            serde_json::to_value(source).unwrap()
        );
    }
    let persisted_source = child_controller
        .store
        .load_session(&source_session_id)
        .unwrap()
        .expect("source session");
    assert_eq!(
        persisted_source.lease_owner.as_deref(),
        Some("source-client")
    );
}

fn preference_provider() -> anyhow::Result<crate::provider::test_support::ProviderTestSession> {
    use forge_provider_api::{ProviderFailure, ProviderFailureKind, ProviderOperation};
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::default_id(),
        label: "Preference fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        |_request, _sink, _start, _cancelled| async {
            Err(crate::provider::ProviderError::Failure(
                ProviderFailure::new(ProviderOperation::TurnStart, ProviderFailureKind::Rejected),
            ))
        },
    )
    .with_models(vec![
        forge_provider_api::ProviderModel {
            id: "gpt-5.6".into(),
            label: None,
            service_tiers: vec![
                forge_provider_api::ServiceTier::Standard,
                forge_provider_api::ServiceTier::Fast,
                forge_provider_api::ServiceTier::Ultrafast,
            ],
            reasoning: vec!["low".into(), "medium".into(), "high".into()],
            default_reasoning: Some("medium".into()),
            selected_reasoning: None,
            context_window: vec![
                forge_provider_api::ProviderContextWindow {
                    id: "default".into(),
                    token_limit: None,
                },
                forge_provider_api::ProviderContextWindow {
                    id: "long_context".into(),
                    token_limit: None,
                },
            ],
            default_context_window: Some("default".into()),
            selected_context_window: None,
            vision: false,
            description: None,
            is_default: true,
        },
        forge_provider_api::ProviderModel {
            id: "gpt-5.4".into(),
            label: None,
            service_tiers: vec![
                forge_provider_api::ServiceTier::Standard,
                forge_provider_api::ServiceTier::Fast,
                forge_provider_api::ServiceTier::Ultrafast,
            ],
            reasoning: vec!["low".into(), "medium".into(), "high".into()],
            default_reasoning: Some("medium".into()),
            selected_reasoning: None,
            context_window: vec![
                forge_provider_api::ProviderContextWindow {
                    id: "default".into(),
                    token_limit: None,
                },
                forge_provider_api::ProviderContextWindow {
                    id: "long_context".into(),
                    token_limit: None,
                },
            ],
            default_context_window: Some("default".into()),
            selected_context_window: None,
            vision: false,
            description: None,
            is_default: false,
        },
    ]);
    crate::provider::test_support::ProviderTestSession::from_native(fixture)
}

#[tokio::test]
async fn carries_preferences_into_a_new_session_and_mode_into_a_resume() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "settings-client".into(),
            provider: crate::provider::test_support::default_launch(),
            model: "default".into(),
            effort: "medium".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let preference_provider = self::preference_provider().unwrap();
    broker.provider = preference_provider.provider_handle();
    let selected = broker
        .dispatch(Request {
            id: 0,
            method: "session.permission.set".into(),
            params: json!({ "change":{"kind":"enable_yolo"} }),
        })
        .await;
    assert!(selected.response.error().is_none());
    let configured = broker
        .dispatch(Request {
            id: 1,
            method: "session.configure".into(),
            params: json!({
                "model": "gpt-5.6",
                "effort": "low",
                "context_window": "default",
                "service_tier": "ultrafast"
            }),
        })
        .await;
    assert!(
        configured.response.error().is_none(),
        "{:?}",
        configured.response.error()
    );
    for invalid in [json!("warp"), json!(true), json!(null)] {
        assert!(
            broker
                .configure_session(json!({"service_tier":invalid}))
                .await
                .is_err()
        );
        assert_eq!(
            broker.session.service_tier,
            crate::provider::ServiceTier::Ultrafast
        );
    }
    broker.capability.ultrafast_mode = false;
    assert!(
        broker
            .configure_session(json!({"service_tier":"ultrafast"}))
            .await
            .is_err()
    );
    assert_eq!(
        broker.session.service_tier,
        crate::provider::ServiceTier::Ultrafast
    );
    broker.capability.ultrafast_mode = true;

    broker
        .dispatch(Request {
            id: 11,
            method: "session.configure".into(),
            params: json!({
                "model": "gpt-5.4",
                "effort": "high",
                "context_window": "long_context"
            }),
        })
        .await;
    broker
        .dispatch(Request {
            id: 12,
            method: "session.configure".into(),
            params: json!({
                "model": "gpt-5.6",
                "effort": "low",
                "context_window": "default"
            }),
        })
        .await;
    let preference = broker
        .store
        .load_preference(&broker.session.workspace, &broker.session.provider)
        .unwrap()
        .unwrap();
    assert_eq!(
        preference.model_setting["gpt-5.4"]
            .context_window
            .as_deref(),
        Some("long_context")
    );
    assert_eq!(
        preference.model_setting["gpt-5.6"].reasoning.as_deref(),
        Some("low")
    );
    let created = broker
        .dispatch(Request {
            id: 2,
            method: "session.new".into(),
            params: json!({}),
        })
        .await;
    let session = created.response.result().unwrap()["session"].clone();
    assert_eq!(session["name"], "");
    assert_eq!(session["model"], "gpt-5.6");
    assert_eq!(session["effort"], "low");
    assert_eq!(session["context_window"], "default");
    assert_eq!(session["service_tier"], "ultrafast");
    assert_eq!(session["approval_mode"], "ask");
    drop(broker);
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "settings-client".into(),
            provider: crate::provider::test_support::default_launch(),
            model: "default".into(),
            effort: "medium".into(),
            session_id: Some(session["id"].as_str().expect("child session id").to_owned()),
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let preference_provider = self::preference_provider().unwrap();
    broker.provider = preference_provider.provider_handle();
    let selected = broker
        .dispatch(Request {
            id: 21,
            method: "session.permission.set".into(),
            params: json!({ "change":{"kind":"set_approval","mode":"ask"} }),
        })
        .await;
    assert!(selected.response.error().is_none());
    let renamed = broker
        .dispatch(Request {
            id: 3,
            method: "session.rename".into(),
            params: json!({ "session_id": session["id"], "name": "  Architecture review  " }),
        })
        .await;
    assert_eq!(
        renamed.response.result().unwrap()["name"],
        "Architecture review"
    );
    assert!(!renamed.event.iter().any(|event| {
        event.event == "provider_event"
            && event.payload["kind"] == "timeline_session_event"
            && event.payload["data"]["event"]["name"] == "Architecture review"
    }));
    let stale = broker
        .observer
        .rename_session(json!({ "session_id": session["id"],
        "name": "Generated name", "expected_name": "Previous name" }));
    assert!(
        stale
            .unwrap_err()
            .to_string()
            .contains("changed during generation")
    );
    assert_eq!(
        broker
            .store
            .load_session(session["id"].as_str().unwrap())
            .unwrap()
            .unwrap()
            .name,
        "Architecture review"
    );
    drop(broker);

    let resumed_session_id = session["id"].as_str().expect("new session id").to_owned();
    let restarted = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "restarted-settings-client".into(),
            provider: crate::provider::test_support::default_launch(),
            model: "default".into(),
            effort: "medium".into(),
            session_id: Some(resumed_session_id),
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(200)),
    )
    .unwrap();
    assert_eq!(restarted.session.model, "gpt-5.6");
    assert_eq!(restarted.session.effort, "low");
    assert_eq!(
        restarted.session.service_tier,
        crate::provider::ServiceTier::Ultrafast
    );
    assert_eq!(restarted.session.approval_mode, ApprovalMode::Ask);
    assert!(
        !restarted
            .snapshot()
            .unwrap()
            .timeline
            .iter()
            .any(|entry| matches!(
                entry,
                TimelineEntry::SessionEvent { event, .. }
                    if matches!(
                        &event.detail,
                        SessionEventKind::Renamed { .. }
                    )
            ))
    );
}

#[tokio::test]
async fn resumes_latest_repository_session_after_broker_restart() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "first-client".into(),
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
    let session_id = broker.session.id.clone();
    broker.session = broker
        .store
        .rename_owned_session(
            &broker.session.id,
            &broker.client_id,
            "Persistent analysis",
            None,
            100,
        )
        .unwrap();
    let (mut exchange, _, _) = broker
        .test_stage_interaction_for_turn("interrupted request", true, 100)
        .await
        .unwrap();
    exchange.resume(100).unwrap();
    exchange
        .start_turn(
            crate::provider::TurnAddress {
                thread_id: (crate::turn::test_support::address(&exchange, "unfinished-turn").scope)
                    .owner()
                    .thread_id(),
                turn_id: (crate::turn::test_support::address(&exchange, "unfinished-turn").scope)
                    .turn_id()
                    .unwrap(),
                scope: crate::turn::test_support::address(&exchange, "unfinished-turn").scope,
            },
            100,
        )
        .unwrap();
    broker.store.save_exchange(&exchange).unwrap();
    let child = crate::agent::Agent::pending(&session_id, "reviewer", "review", 100);
    broker.store.save_agent_run(&child).unwrap();
    drop(broker);

    let restarted = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "second-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "different-model".into(),
            effort: "high".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(200)),
    )
    .unwrap();

    assert_eq!(restarted.session.id, session_id);
    assert_eq!(restarted.session.name, "Persistent analysis");
    assert_eq!(restarted.session.model, "mock-model");
    assert_eq!(restarted.session.effort, "low");
    let snapshot = restarted.snapshot().unwrap();
    assert_eq!(snapshot.exchange.len(), 1);
    let recovered = &snapshot.exchange[0];
    assert_eq!(recovered.id, exchange.id);
    assert_eq!(recovered.state(), ExchangeState::Interrupted);
    assert_eq!(recovered.elapsed(90_000), 0);
    assert_eq!(
        recovered.turn[0].state(),
        crate::turn::TurnState::Finished {
            outcome: crate::turn::TurnOutcome::Interrupted,
        }
    );
    let child = restarted
        .store
        .list_agent_run(&session_id)
        .unwrap()
        .pop()
        .unwrap();
    assert_eq!(child.state, crate::agent::AgentState::Closed);
    assert_eq!(
        restarted
            .store
            .list_session(Some(&restarted.session.workspace))
            .unwrap()
            .len(),
        1
    );
}

#[tokio::test]
async fn pauses_an_active_goal_after_a_provider_failure() {
    let repository = git_repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "failure-test".into(),
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
        Box::new(FixedClock(200)),
    )
    .unwrap();
    let provider = failing_provider_fixture(repository.path().to_owned()).unwrap();
    broker.provider = provider.provider_handle();
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.goal.set".into(),
            params: json!({ "objective": "finish the work" }),
        })
        .await;
    assert!(result.response.error().is_none());
    assert!(dispatch_pending_task_input(&mut broker).await.is_err());
    assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
    let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(interaction.len(), 1);
    assert_eq!(interaction[0].state(), ExchangeState::Failed);
    assert!(interaction[0].checkpoint_after.is_some());
    assert!(
        interaction[0]
            .checkpoint_diff_text
            .as_deref()
            .is_some_and(|diff| diff.contains("failed/deep/write.txt"))
    );
}

#[tokio::test]
async fn non_git_exchanges_save_reported_changes_without_initializing_repository() {
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: workspace.path().to_string_lossy().into_owned(),
            client_id: "reported-edits-test".into(),
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
        Box::new(FixedClock(300)),
    )
    .unwrap();
    broker.provider = nested_write_provider(&broker, Path::new(&broker.session.workspace));
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"create a module"}),
        })
        .await;
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
    let exchange = &exchanges[0];
    assert_eq!(exchange.state(), ExchangeState::Complete);
    assert!(exchange.checkpoint_before.is_none() && exchange.checkpoint_after.is_none());
    assert!(exchange.checkpoint_diff_text.is_none());
    assert!(!exchange.attributed_matches_checkpoint);
    let diff = exchange
        .attributed_diff_text
        .as_deref()
        .expect("persisted reported edits");
    assert!(diff.contains("seed.txt") && diff.contains("+provider edit"));
    assert!(
        !diff.contains("apps/new/deep/module/lib.rs"),
        "command edits have no provider provenance"
    );
    assert!(!workspace.path().join(".git").exists());
}

#[tokio::test]
async fn provider_and_command_changes_use_separate_diff_sources() {
    let repository = git_repository();
    std::fs::write(repository.path().join(".gitignore"), "target/\n").unwrap();
    git(repository.path(), &["add", ".gitignore"]);
    git(repository.path(), &["commit", "-qm", "ignore build output"]);
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "interaction-diff-test".into(),
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
        Box::new(FixedClock(300)),
    )
    .unwrap();
    broker.provider = nested_write_provider(&broker, Path::new(&broker.session.workspace));
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "create a nested module" }),
        })
        .await;
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(interaction.len(), 1);
    assert_eq!(interaction[0].turn.len(), 1);
    assert_eq!(
        interaction[0].turn[0].messages()[0].text(),
        "Creating the nested module."
    );
    assert_eq!(
        interaction[0]
            .tools_for_turn(interaction[0].turn[0].id())
            .count(),
        2
    );
    assert!(
        interaction[0]
            .checkpoint_diff_text
            .as_deref()
            .is_some_and(|diff| diff.contains("apps/new/deep/module/lib.rs"))
    );
    let attributed_diff = interaction[0]
        .attributed_diff_text
        .as_deref()
        .expect("attributed interaction diff");
    assert!(attributed_diff.contains("seed.txt"));
    assert!(!attributed_diff.contains("apps/new/deep/module/lib.rs"));
    assert!(!interaction[0].attributed_matches_checkpoint);
    let interaction_diff = interaction[0].checkpoint_diff_text.as_deref().unwrap();
    assert!(interaction_diff.contains("seed.txt"));
    assert!(!interaction_diff.contains("target/generated/deep/artifact.txt"));
    assert!(
        interaction[0].turn[0]
            .messages()
            .iter()
            .any(|message| message.text() == "The nested module is ready.")
    );
}
