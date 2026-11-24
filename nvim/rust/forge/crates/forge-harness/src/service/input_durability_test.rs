use super::*;

/// Recover a completed tool check into durable input without launching check commands.
#[tokio::test]
async fn completed_check_is_delivered_once_after_owner_restart() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"check-results",
        "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session_record = broker.snapshot()?.session;
    let session = session_record.id.clone();
    let mut exchange = crate::storage::test::running_exchange("check-owner");
    exchange.session_id = session.clone();
    exchange.agent_id = session_record.primary_agent_id;
    exchange.finish(crate::exchange::ExchangeOutcome::Complete, 100)?;
    SqliteStore::open(&broker.data_root)?.save_exchange(&exchange)?;
    let checks = broker.checks();
    let run: crate::checks::CheckRun = serde_json::from_value(json!({
        "id":"completed-check","owner":{"kind":"session","session_id":session},"revision":0,
        "shell":"system","executable":null,"workspace":root.path(),"environment_digest":"fixture",
        "started_at_ms":1,"completed_at_ms":2,"state":"failed",
        "progress":crate::plan::execution::SemanticProgress::default(),"workspace_identity":null,"workspace_digest":{},
        "commands":[{"id":"command","command":"fixture","state":"failed","selected":true,
            "started_at_ms":1,"completed_at_ms":2,"exit_code":1,"error":"fixture failure",
            "stdout":"unused","stderr":"unused","output":"unused","output_bytes":0,"preview":""}]
    }))?;
    let record = checks.root.join("completed-check");
    std::fs::create_dir_all(&record)?;
    std::fs::write(
        record.join("record.json"),
        serde_json::to_vec(&json!({"version":crate::storage::state::VERSION,"run":run}))?,
    )?;
    let receipts = checks.root.join("owner-receipts");
    std::fs::create_dir_all(&receipts)?;
    let scope = crate::provider::InvocationScope::new(
        crate::provider::ThreadOwner::root(&session),
        exchange.id.as_str(),
    );
    std::fs::write(
        receipts.join("fixture.json"),
        serde_json::to_vec(&json!({
            "scope":scope,"call_id":"fixture-call","request":crate::checks::RunRequest::default(),
            "run_id":run.id,"state":"admitted"
        }))?,
    )?;
    drop(checks);
    broker.observer().input.suspend(true, None)?;
    drop(broker);
    let controller = receipt_test_controller_with_session(root.path(), &session).await?;
    let input = &controller.observer.check_inbox;
    loop {
        let changed = input.changed.notified();
        if input
            .admission_receipt("check-result:completed-check")?
            .is_some()
        {
            break;
        }
        changed.await;
    }
    let snapshot = input.snapshot()?;
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty(),
        "check completion leaked into the user queue"
    );
    let message = snapshot
        .messages
        .iter()
        .find(|message| message.id == "check-result:completed-check")
        .context("result input")?;
    assert!(
        controller.coordinator.submissions.snapshot()?.suspended,
        "completion resumed a paused queue"
    );
    assert!(!snapshot.suspended, "user pause reached the internal inbox");
    assert!(
        matches!(&message.purpose, crate::input::InputPurpose::CheckResult { exchange_id, run_id } if exchange_id == "check-owner" && run_id == "completed-check")
    );
    assert!(
        matches!(message.turn().unwrap().kind(), forge_provider_api::forge::TurnKind::CheckResult { result } if result.checks[0].error.as_deref() == Some("fixture failure"))
    );
    let claimed = input
        .claim(
            &message.id,
            message.revision,
            crate::input::InputRoute::Turn,
        )?
        .context("internal claim")?;
    input.delivery_if_current(
        &claimed.id,
        crate::input::InputDeliveryGuard::Claimed(claimed.revision),
        None,
        None,
        None,
    )?;
    controller.finish_exchange_owner().await?;
    drop(controller);
    let reopened = receipt_test_controller_with_session(root.path(), &session).await?;
    reopened
        .exchange_owner
        .command_status(crate::exchange::command::CommandId::new(
            "recovery-barrier",
        )?)
        .await?;
    assert!(
        reopened
            .observer
            .check_inbox
            .admission_receipt("check-result:completed-check")?
            .is_some()
    );
    assert!(
        reopened
            .observer
            .check_inbox
            .snapshot()?
            .messages
            .is_empty()
    );
    reopened.finish_exchange_owner().await?;
    Ok(())
}

/// Recovered receipts deduplicate completed runs without publishing them as user input.
#[tokio::test]
async fn check_result_replay_preserves_receipts_and_delivers_later_runs() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"check-replay",
        "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session = broker.snapshot()?.session;
    let mut exchange = crate::storage::test::running_exchange("check-owner");
    exchange.session_id = session.id.clone();
    exchange.agent_id = session.primary_agent_id;
    exchange.finish(crate::exchange::ExchangeOutcome::Complete, 100)?;
    SqliteStore::open(&broker.data_root)?.save_exchange(&exchange)?;
    let checks_root = broker.checks().root.clone();
    let scope = crate::provider::InvocationScope::new(
        crate::provider::ThreadOwner::root(&session.id),
        exchange.id.as_str(),
    );
    seed_completed_check(&checks_root, &session.id, &scope, "first")?;
    broker.observer().input.suspend(true, None)?;
    drop(broker);
    let controller = receipt_test_controller_with_session(root.path(), &session.id).await?;
    let input = &controller.observer.check_inbox;
    loop {
        let changed = input.changed.notified();
        if input.admission_receipt("check-result:first")?.is_some() {
            break;
        }
        changed.await;
    }
    controller.finish_exchange_owner().await?;
    drop(controller);
    seed_completed_check(&checks_root, &session.id, &scope, "second")?;
    seed_completed_check(&checks_root, &session.id, &scope, "third")?;
    let reopened = receipt_test_controller_with_session(root.path(), &session.id).await?;
    let input = &reopened.observer.check_inbox;
    reopened
        .exchange_owner
        .command_status(crate::exchange::command::CommandId::new("replay-barrier")?)
        .await?;
    assert!(
        input.admission_receipt("check-result:second")?.is_some(),
        "replayed first result blocked the second result"
    );
    let messages = input.snapshot()?.messages;
    assert_eq!(messages.len(), 2);
    for id in ["first", "second"] {
        assert_eq!(
            messages
                .iter()
                .filter(|message| message.id == format!("check-result:{id}"))
                .count(),
            1
        );
    }
    let grouped = messages
        .iter()
        .find(|message| message.id == "check-result:second")
        .unwrap();
    assert_eq!(
        grouped
            .turn()
            .unwrap()
            .check_results()
            .map(|result| result.run_id.as_str())
            .collect::<Vec<_>>(),
        ["second", "third"]
    );
    let publications = reopened
        .publications
        .after(None, std::num::NonZeroU16::new(u16::MAX).unwrap())?;
    let check_publications = publications
        .iter()
        .filter(|publication| {
            publication.event().event == "input_changed"
                && publication.event().payload["operation_id"]
                    .as_str()
                    .is_some_and(|id| id.starts_with("check-result:"))
        })
        .collect::<Vec<_>>();
    assert_eq!(
        check_publications.len(),
        0,
        "internal results were published as user input"
    );
    assert!(
        reopened
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    reopened.finish_exchange_owner().await?;
    Ok(())
}

/// Deliver recovered evidence through MockProvider even while user follow-ups remain paused.
#[tokio::test]
async fn check_result_reaches_mock_provider_while_user_queue_is_paused() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"internal-result",
        "provider":{"kind":"mock","command":[]},
    }))?)?;
    let session = broker.snapshot()?.session;
    let mut exchange = crate::storage::test::running_exchange("check-owner");
    exchange.session_id = session.id.clone();
    exchange.agent_id = session.primary_agent_id;
    let address = crate::turn::test_support::address(&exchange, "initial");
    exchange.start_turn(address.clone(), 1)?;
    exchange.observe_event(
        &crate::turn::test_support::message(
            &address,
            "initial-commentary",
            Some(forge_provider_api::AgentMessageRole::Commentary),
            "Starting checks.",
        ),
        1,
    )?;
    exchange.observe_event(
        &crate::turn::test_support::message(
            &address,
            "pending-response",
            Some(forge_provider_api::AgentMessageRole::Response),
            "Checks are pending.",
        ),
        2,
    )?;
    // A response renders only once the provider completes it.
    exchange.observe_event(
        &crate::turn::test_support::message_completed(
            &address,
            "pending-response",
            "Checks are pending.",
        ),
        2,
    )?;
    exchange.observe_event(
        &crate::turn::test_support::completed_event(
            &address,
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            3,
        ),
        3,
    )?;
    exchange.finish(crate::exchange::ExchangeOutcome::Complete, 100)?;
    SqliteStore::open(&broker.data_root)?.save_exchange(&exchange)?;
    seed_completed_check(
        &broker.checks().root,
        &session.id,
        &crate::provider::InvocationScope::new(
            crate::provider::ThreadOwner::root(&session.id),
            "check-owner",
        ),
        "recovered",
    )?;
    broker.observer().input.suspend(true, None)?;
    drop(broker);
    let controller = receipt_test_controller_with_session(root.path(), &session.id).await?;
    let control = controller
        .provider_handle()
        .mock_response_control()
        .context("mock control")?;
    let hold = control
        .dispatch(
            &session.id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    control
        .dispatch(
            &session.id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![
                        TestPromptStep::Message {
                            content: TestMessageContent::Commentary,
                            text: "Reviewing the check result.".into(),
                        },
                        TestPromptStep::Message {
                            content: TestMessageContent::Response,
                            text: "Checks passed.".into(),
                        },
                    ],
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<_, anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let delivered = control
        .dispatch(
            &session.id,
            MockResponseCommand::Wait {
                id: hold["id"].as_str().unwrap().into(),
            },
        )
        .await?;
    assert!(delivered["prompt"].as_str().unwrap().contains("recovered"));
    assert!(controller.coordinator.submissions.snapshot()?.suspended);
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    control
        .dispatch(
            &session.id,
            MockResponseCommand::Release {
                id: hold["id"].as_str().unwrap().into(),
            },
        )
        .await?;
    controller.finish_exchange_owner().await?;
    let completed = crate::storage::HistoryReader::current(&controller.data_root)?
        .latest_exchange(&session.id)?
        .context("completed exchange")?;
    let projected = crate::buffer::projection::project(
        crate::buffer::projection::ProjectionSource::Exchange(&completed),
        &forge_buffer::width::WidthProfile::default(),
        100,
        false,
        &std::collections::HashMap::new(),
        &std::collections::HashMap::new(),
        &mut crate::buffer::identity::DisplayRegistry::default(),
    )?;
    let blocks = &projected.entry.block;
    let summary = blocks
        .iter()
        .find(|block| block.id.0 == "check-owner:summary")
        .context("activity summary")?;
    let activity_end = blocks
        .iter()
        .position(|block| block.id == summary.metadata.fold[0].end.block)
        .context("activity fold end")?;
    for text in [
        "Checks are pending.",
        "Reviewing the check result.",
        "Checks passed.",
    ] {
        let position = blocks
            .iter()
            .position(|block| block.text.wire_rows().join("\n").contains(text))
            .with_context(|| format!("missing rendered message: {text}"))?;
        if text == "Checks passed." {
            assert!(
                position > activity_end,
                "final response belongs outside the activity fold"
            );
        } else {
            assert!(
                position <= activity_end,
                "continued activity escaped the Thought fold: {text}"
            );
            assert_eq!(blocks[position].metadata.layout.as_ref().unwrap().indent, 4);
        }
    }
    drop(sink);
    drop(controller);
    reader.abort();
    Ok(())
}

/// A cancellation can collect an exchange whose turn already completed natively, as an interrupt
/// does when it reaches a turn that is still finalizing. Natural completion wins, so the exchange
/// stays complete and a check result its turn started continues it. The collection's engine
/// fence must not then refuse that continuation's prepared submission, which once failed the
/// result's turn as `provider_cancelled` with no cancellation in flight.
#[tokio::test]
async fn check_result_continues_an_exchange_collected_after_its_turn_completed() -> Result<()> {
    use crate::provider::ScopeOwner;
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let completed_script = |text: &str| MockResponseCommand::Script {
        script: TestPromptScript {
            acceptance: TestInputAcceptance::BeforeResponse,
            steps: vec![TestPromptStep::Message {
                content: TestMessageContent::Response,
                text: text.into(),
            }],
            terminal: TestPromptTerminal::Completed,
        },
    };
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let session = controller.observer.session_id().to_owned();
    let control = controller
        .provider_handle()
        .mock_response_control()
        .context("mock control")?;
    control
        .dispatch(&session, completed_script("Started the check."))
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<_, anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    queued_offer_input(&controller, "user", "Start the check", None)?;
    let submissions = &controller.coordinator.submissions;
    loop {
        let changed = submissions.changed.notified();
        if submissions.snapshot()?.messages.is_empty() {
            break;
        }
        changed.await;
    }
    let started = crate::storage::HistoryReader::current(&controller.data_root)?
        .latest_exchange(&session)?
        .context("started exchange")?;
    assert_eq!(started.state(), crate::exchange::ExchangeState::Complete);
    // The interrupt pinned the finalizing turn and collects its exchange, as
    // `transition::collect_resources` does for the scope it captured.
    let scope = started
        .turn
        .last()
        .context("started turn")?
        .provider()
        .scope
        .clone();
    let engine = controller
        .broker
        .lock()
        .await
        .execution_registry()
        .existing_owner(&scope.owner())?
        .context("root execution owner")?;
    engine.cancel_exchange(scope).await?;
    control
        .dispatch(&session, completed_script("Reviewed the check result."))
        .await?;
    let inbox = &controller.observer.check_inbox;
    submit_check_result(&controller, &started.id, "interrupted-run")?;
    controller.exchange_owner.input_ready(sink.clone()).await?;
    loop {
        let changed = inbox.changed.notified();
        // The result leaves the inbox once its turn settles; a failed turn leaves it failed.
        if inbox.snapshot()?.messages.iter().all(|message| {
            matches!(
                message.delivery(),
                crate::input::InputDelivery::Failed | crate::input::InputDelivery::Unknown
            )
        }) {
            break;
        }
        changed.await;
    }
    let remaining = inbox.snapshot()?.messages;
    assert!(
        remaining.is_empty(),
        "check result did not settle: {:?}",
        remaining
            .iter()
            .map(|message| (message.delivery(), message.error()))
            .collect::<Vec<_>>()
    );
    let continued = crate::storage::HistoryReader::current(&controller.data_root)?
        .load_exchange(&started.id)?
        .context("continued exchange")?;
    assert_eq!(
        continued.state(),
        crate::exchange::ExchangeState::Complete,
        "check result continuation failed: {:?}",
        continued.finalization_error
    );
    assert_eq!(continued.turn.len(), 2, "check result never reached a turn");
    assert!(
        matches!(
            continued.turn[1].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Completed { .. }
            }
        ),
        "check result turn ended as {:?}",
        continued.turn[1].state()
    );
    let cancelled_report = controller
        .publications
        .after(None, std::num::NonZeroU16::new(u16::MAX).unwrap())?
        .into_iter()
        .find(|publication| {
            publication
                .event()
                .payload
                .to_string()
                .contains("provider execution cancelled")
        });
    assert!(
        cancelled_report.is_none(),
        "the continuation reported a cancellation: {:?}",
        cancelled_report.map(|publication| publication.event().clone())
    );
    controller.finish_exchange_owner().await?;
    drop(sink);
    drop(controller);
    reader.abort();
    Ok(())
}

/// Seed collected check evidence and its admission receipt without running a command.
fn seed_completed_check(
    root: &std::path::Path,
    session: &str,
    scope: &crate::provider::InvocationScope,
    id: &str,
) -> Result<()> {
    let record = root.join(id);
    std::fs::create_dir_all(&record)?;
    let run: crate::checks::CheckRun = serde_json::from_value(json!({
        "id":id,"owner":{"kind":"session","session_id":session},"revision":0,
        "shell":"system","executable":null,"workspace":".","environment_digest":"fixture",
        "started_at_ms":1,"completed_at_ms":2,"state":"passed",
        "progress":crate::plan::execution::SemanticProgress::default(),"workspace_identity":null,"workspace_digest":{},
        "commands":[]
    }))?;
    std::fs::write(
        record.join("record.json"),
        serde_json::to_vec(&json!({
            "version":crate::storage::state::VERSION,"run":run
        }))?,
    )?;
    let receipts = root.join("owner-receipts");
    std::fs::create_dir_all(&receipts)?;
    std::fs::write(
        receipts.join(format!("{id}.json")),
        serde_json::to_vec(&json!({
            "scope":scope,"call_id":id,"request":crate::checks::RunRequest::default(),
            "run_id":id,"state":"admitted"
        }))?,
    )?;
    Ok(())
}

/// Reopen the same mock session so the real owner recovers its persisted check result.
async fn receipt_test_controller_with_session(
    root: &std::path::Path,
    session: &str,
) -> Result<Arc<SessionController>> {
    SessionController::new(HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":root,"data_root":root.join("data"),"client_id":"check-results","session_id":session,
        "provider":{"kind":"mock","command":[]},
    }))?)?).await
}

/// Answer admission must remain automatic when no UI exists and legacy control/broker resources are held.
#[tokio::test]
async fn question_input_owner_admits_headlessly_without_waiting_for_legacy_locks() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller.coordinator.control.acquire().await?;
    let broker = controller.broker.lock().await;
    let questions = controller.questions.ask(
        "headless-question",
        "question-exchange",
        crate::question::QuestionOrigin::Discussion(None),
        vec![crate::question::QuestionDraft {
            header: "Scope".into(),
            question: "Which scope?".into(),
            requirement: crate::question::QuestionRequirement::Optional,
            options: Vec::new(),
        }],
    )?;
    controller
        .questions
        .apply(crate::question::QuestionCommand {
            operation_id: "headless-answer".into(),
            question_id: questions[0].clone(),
            revision: 1,
            action: crate::question::QuestionAction::Answer {
                answer: crate::question::QuestionAnswer::Other {
                    text: "Workspace".into(),
                },
            },
        })?;
    let message = controller.questions.outbox()?[0].clone();
    let receipt = async {
        loop {
            let receipt = controller
                .coordinator
                .submissions
                .admission_receipt(&format!("question:{}", message.id))?;
            if let Some(receipt) = receipt
                && controller.questions.outbox()?.is_empty()
            {
                break Ok::<_, anyhow::Error>(receipt);
            }
            tokio::task::yield_now().await;
        }
    }
    .await?;
    assert!(receipt.error_code.is_none());
    let queued = controller.coordinator.submissions.snapshot()?;
    assert_eq!(queued.messages.len(), 1);
    assert_eq!(queued.messages[0].id, message.id);
    assert!(
        matches!(&queued.messages[0].purpose, crate::input::InputPurpose::Question { question_id, reply, .. } if question_id == &questions[0] && reply == &message.reply)
    );
    assert!(controller.coordinator.execution.try_acquire().is_err());
    assert!(
        controller
            .coordinator
            .control
            .clone()
            .try_acquire_owned()
            .is_err()
    );
    drop(broker);
    drop(control);
    drop(execution);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Policy updates neither wait for native lanes nor let a replay overwrite a later committed policy.
#[tokio::test]
async fn task_policy_commits_with_control_and_execution_held_without_replaying_later_policy()
-> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandOutcome, CommandRequest, InputControl, PlanAcceptancePolicy,
        TaskCommand, TaskCommandOutcome, TaskSelection,
    };
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let plan: crate::plan::PlanRecord = serde_json::from_value(json!({
        "id":"owned-policy-plan","session_id":controller.observer.session_id(),"request":"Plan a change",
        "state":"configuring_checks","working_path":"working.json","model_revision":0,"user_revision":0,"created_at_ms":1,"updated_at_ms":1,
    }))?;
    let task = controller
        .task_controller
        .change(
            &mut SqliteStore::open(&controller.data_root)?,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::plan(plan, 1)?)),
            1,
        )?
        .unwrap();
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let request = CommandRequest {
        id: CommandId::new("accept-policy-on")?,
        command: Command::Task(TaskCommand::SetAutoAccept(PlanAcceptancePolicy {
            target: TaskSelection::capture(&task),
            enabled: true,
        })),
        input_control: InputControl::AtExecution,
    };
    let committed = controller
        .exchange_owner
        .execute_identified(request.clone(), sink.clone())
        .await?;
    assert!(matches!(
        committed.outcome,
        CommandOutcome::Task(TaskCommandOutcome::AutoAcceptChanged { enabled: true, .. })
    ));
    controller
        .exchange_owner
        .execute_command(Command::Task(TaskCommand::SetAutoAccept(
            PlanAcceptancePolicy {
                target: TaskSelection::capture(&task),
                enabled: false,
            },
        )))
        .await?;
    let replay = controller
        .exchange_owner
        .execute_identified(request, sink)
        .await?;
    assert_eq!(replay.sequence, committed.sequence);
    assert!(
        !controller
            .task_controller
            .current()?
            .unwrap()
            .auto_accept_plan
    );
    assert_eq!(
        controller.task_controller.current()?.unwrap().revision,
        task.revision
    );
    assert_eq!(controller.coordinator.control.available_permits(), 0);
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    drop(control);
    drop(execution);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Readiness selects current input, bypasses queued drafts, and retains sends after a waiter drops.
#[tokio::test]
async fn input_readiness_owns_steering_burst_without_waiting_for_acknowledgments() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    controller.coordinator.submissions.apply(serde_json::from_value(json!({
        "id":"submit-draft","action":{"kind":"submit","message":{"id":"draft","text":"Wait for a new turn","intent":"queue"}},
    }))?)?;
    queued_offer_input(&controller, "first", "First immediate input", None)?;
    queued_offer_input(&controller, "second", "Second immediate input", None)?;
    let execution = controller.coordinator.execution.acquire().await?;
    let mut active = controller.parent_input.activate(None)?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let mut readiness = Box::pin(controller.exchange_owner.input_ready(sink.clone()));
    assert!(futures_util::poll!(readiness.as_mut()).is_pending());
    drop(readiness);
    let first = active
        .receive()
        .await
        .context("dropped readiness waiter cancelled its admitted send")?;
    assert_eq!(first.text, "First immediate input");
    let first_receipt = first
        .submission
        .as_ref()
        .context("first input has no receipt")?
        .clone();
    async {
        loop {
            let changed = controller.coordinator.submissions.changed.notified();
            match controller.exchange_owner.input_ready(sink.clone()).await? {
                crate::exchange::InputReadiness::Started => return Ok::<(), anyhow::Error>(()),
                crate::exchange::InputReadiness::Waiting => changed.await,
                crate::exchange::InputReadiness::WaitExecution => {
                    anyhow::bail!("second immediate input waited for native acknowledgment")
                }
            }
        }
    }
    .await?;
    let second = active
        .receive()
        .await
        .context("second immediate input was not handed off")?;
    assert_eq!(second.text, "Second immediate input");
    let second_receipt = second
        .submission
        .as_ref()
        .context("second input has no receipt")?
        .clone();
    assert_eq!(
        first_receipt.record()?.delivery,
        crate::provider::submission::SubmissionState::Prepared
    );
    assert_ne!(first_receipt.receipt_id(), second_receipt.receipt_id());
    second_receipt.sending(None)?;
    second_receipt.accepted(None)?;
    second.complete(Ok(())).await;
    first_receipt.sending(None)?;
    first_receipt.accepted(None)?;
    first.complete(Ok(())).await;
    async {
        loop {
            let changed = controller.coordinator.submissions.changed.notified();
            let queue = controller.coordinator.submissions.snapshot()?;
            if queue.messages.len() == 1 {
                assert_eq!(queue.messages[0].id, "draft");
                assert_eq!(
                    queue.messages[0].delivery(),
                    crate::input::InputDelivery::Queued
                );
                return Ok::<(), anyhow::Error>(());
            }
            changed.await;
        }
    }
    .await?;
    drop(active);
    drop(execution);
    controller.finish_exchange_owner().await?;
    let registry = controller.broker.lock().await.execution_registry();
    registry.close().await?;
    Ok(())
}

/// Steering remains owner-controlled after its offer waiter drops, and handoff precedes acknowledgment.
#[tokio::test]
async fn steering_owner_retains_dropped_offer_and_settles_receipt_after_handoff() -> Result<()> {
    #[derive(Debug)]
    enum ControlChange {
        Unchanged,
        Cancellation,
    }
    for change in [ControlChange::Unchanged, ControlChange::Cancellation] {
        let root = tempfile::tempdir()?;
        let controller = receipt_test_controller(root.path()).await?;
        let input_id = "owned-steering";
        let input = queued_offer_input(&controller, input_id, "Keep the captured target", None)?;
        let execution = controller.coordinator.execution.acquire().await?;
        let mut active = controller.parent_input.activate(None)?;
        let ordering = Arc::new(tokio::sync::Semaphore::new(1));
        let control = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        let order = ordering.clone().acquire_owned().await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let mut offered = Box::pin(controller.exchange_owner.offer_steering(
            crate::exchange::SteeringDeliveryOffer {
                input,
                control,
                order,
            },
            sink.clone(),
        ));
        assert!(futures_util::poll!(offered.as_mut()).is_pending());
        drop(offered);
        let command = active
            .receive()
            .await
            .context("owned steering was not delivered")?;
        let receipt = command
            .submission
            .as_ref()
            .context("steering has no durable receipt")?
            .clone();
        assert_eq!(
            receipt.record()?.input_message_id.as_deref(),
            Some(input_id)
        );
        let handoff = ordering.clone().acquire_owned().await?;
        assert_eq!(
            receipt.record()?.delivery,
            crate::provider::submission::SubmissionState::Prepared
        );
        drop(handoff);
        receipt.sending(None)?;
        receipt.accepted(None)?;
        async {
            loop {
                let changed = controller.coordinator.submissions.changed.notified();
                if controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .iter()
                    .any(|message| {
                        message.id == input_id
                            && message.delivery() == crate::input::InputDelivery::Accepted
                    })
                {
                    return Ok::<(), anyhow::Error>(());
                }
                changed.await;
            }
        }
        .await?;
        // The original receipt remains observable after dispatch authority changes.
        match change {
            ControlChange::Unchanged => {}
            ControlChange::Cancellation => controller.cancellation.request_native_cancel(),
        }
        command.complete(Ok(())).await;
        async {
            loop {
                let changed = controller.coordinator.submissions.changed.notified();
                let snapshot = controller.coordinator.submissions.snapshot()?;
                assert!(
                    snapshot
                        .messages
                        .iter()
                        .all(|message| message.delivery() != crate::input::InputDelivery::Failed),
                    "accepted receipt completion failed its input"
                );
                if snapshot.messages.is_empty() {
                    return Ok::<(), anyhow::Error>(());
                }
                changed.await;
            }
        }
        .await?;
        controller.cancellation.arm();

        // Reuse the same captured lane after receipt settlement, rather than recovering a fenced owner.
        let input = queued_offer_input(
            &controller,
            "reused-steering",
            "Use the same owner again",
            None,
        )?;
        assert!(
            controller
                .exchange_owner
                .offer_steering(
                    crate::exchange::SteeringDeliveryOffer {
                        input,
                        control: controller
                            .coordinator
                            .control
                            .clone()
                            .acquire_owned()
                            .await?,
                        order: ordering.clone().acquire_owned().await?,
                    },
                    sink.clone()
                )
                .await?
        );
        let reused = active
            .receive()
            .await
            .context("receipt settlement fenced subsequent steering")?;
        let reused_receipt = reused
            .submission
            .as_ref()
            .context("reused steering has no receipt")?
            .clone();
        reused_receipt.sending(None)?;
        reused_receipt.accepted(None)?;
        controller
            .exchange_owner
            .observe_input_receipt(
                crate::exchange::InputReceiptFact::Acceptance {
                    submission: reused_receipt.receipt_id().clone(),
                },
                None,
            )
            .await?;
        reused.complete(Ok(())).await;
        async {
            loop {
                let changed = controller.coordinator.submissions.changed.notified();
                let snapshot = controller.coordinator.submissions.snapshot()?;
                assert!(
                    snapshot
                        .messages
                        .iter()
                        .all(|message| message.delivery() != crate::input::InputDelivery::Failed)
                );
                if snapshot.messages.is_empty() {
                    return Ok::<(), anyhow::Error>(());
                }
                changed.await;
            }
        }
        .await?;
        controller
            .exchange_owner
            .command_status(crate::exchange::command::CommandId::new(
                "settled-receipt-barrier",
            )?)
            .await?;
        while let Ok(frame) = output.try_recv() {
            if let forge_protocol::message::Message::Event(event) =
                serde_json::from_slice(frame.bytes())?
            {
                assert!(
                    !(event.event == "provider_event" && event.payload["kind"] == "error"),
                    "successful receipt completion published failure: {:?}",
                    event.payload
                );
            }
        }
        assert!(
            SqliteStore::read_only(&controller.data_root)?
                .latest_exchange(controller.observer.session_id())?
                .is_none(),
            "steering must not manufacture a new foreground exchange"
        );
        drop(active);
        drop(execution);
        controller.finish_exchange_owner().await?;
        let registry = controller.broker.lock().await.execution_registry();
        registry.close().await?;
    }
    Ok(())
}

/// Recover acceptance committed after the old owner stopped without replaying the stored prompt.
#[tokio::test]
async fn owner_recovers_stored_acceptance_without_dispatching_input() -> Result<()> {
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let session = controller.observer.session_id().to_owned();
    let (_, submission) =
        registered_receipt(&controller, "recover", crate::input::InputPurpose::User).await?;
    controller.finish_exchange_owner().await?;
    submission.sending(None)?;
    submission.accepted(None)?;
    drop(submission);
    drop(controller);
    let controller =
        SessionController::new(HarnessBroker::initialize(serde_json::from_value(json!({
            "workspace":root.path(),"data_root":root.path().join("data"),
            "session_id":session,"client_id":"receipt-recovery",
            "provider":{"kind":"mock","command":[]},
        }))?)?)
        .await?;
    controller
        .exchange_owner
        .command_status(crate::exchange::command::CommandId::new(
            "receipt-recovery-barrier",
        )?)
        .await?;
    let input = controller.coordinator.submissions.snapshot()?;
    assert!(input.suspended);
    assert!(input.messages.is_empty());
    assert!(
        SqliteStore::read_only(&controller.data_root)?
            .latest_exchange(&session)?
            .is_none()
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Recover the original text when native acceptance outlives its transcript append.
#[tokio::test]
async fn accepted_steering_reopens_in_its_exchange_without_replay() -> Result<()> {
    use crate::input::{InputDelivery, InputRoute};
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let session = controller.broker.lock().await.snapshot()?.session;
    let mut exchange = crate::storage::test::running_exchange("steering-owner");
    exchange.session_id = session.id.clone();
    exchange.agent_id = session.primary_agent_id;
    let scope = crate::provider::InvocationScope::new(
        crate::provider::ThreadOwner::root(&session.id),
        exchange.id.clone(),
    );
    exchange.finish(crate::exchange::ExchangeOutcome::Complete, 100)?;
    SqliteStore::open(&controller.data_root)?.save_exchange(&exchange)?;

    let (input, submission) = registered_receipt_on_route(
        &controller,
        "accepted-steer",
        crate::input::InputPurpose::User,
        InputRoute::Steering,
    )
    .await?;
    submission.bind_steering_scope(submission.record()?.attempt_id, scope)?;
    submission.sending(None)?;
    submission.accepted(None)?;
    let receipt = submission.receipt_id().clone();
    controller
        .exchange_owner
        .observe_input_receipt(
            crate::exchange::InputReceiptFact::Settled {
                submission: receipt.clone(),
                result: crate::exchange::InputExecutionResult::Completed,
                activity: crate::exchange::InputDispatchActivity::Active,
            },
            None,
        )
        .await?;
    assert_eq!(
        controller.coordinator.submissions.snapshot()?.messages[0].delivery(),
        InputDelivery::Accepted,
        "settlement dropped the only copy before the transcript append"
    );
    controller.finish_exchange_owner().await?;
    drop(submission);
    drop(controller);

    for _ in 0..2 {
        let reopened = receipt_test_controller_with_session(root.path(), &session.id).await?;
        let history = SqliteStore::read_only(&reopened.data_root)?.list_exchange(&session.id)?;
        assert_eq!(
            history.len(),
            1,
            "acceptance was replayed as a new exchange"
        );
        let prompts: Vec<_> = history[0]
            .node_list
            .iter()
            .filter_map(|node| match node {
                crate::exchange::ExchangeNode::ExchangeInput { prompt }
                    if prompt.submission_id.as_ref() == Some(&receipt) =>
                {
                    Some(prompt)
                }
                _ => None,
            })
            .collect();
        let [prompt] = prompts.as_slice() else {
            anyhow::bail!("the original exchange must show the accepted steer once");
        };
        assert_eq!(prompt.id, input.id.as_str());
        assert_eq!(prompt.text, "Owned receipt");
        assert_eq!(
            prompt.consumption,
            crate::exchange::InputConsumption::Unconfirmed
        );
        assert!(
            reopened
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty()
        );
        reopened.finish_exchange_owner().await?;
    }
    Ok(())
}

/// Close the exchange owner while it waits for catalog evidence for an unapplied queued choice.
#[tokio::test]
async fn closing_owner_keeps_queued_settings_after_catalog_returns() -> Result<()> {
    use crate::input::InputDelivery;
    let root = tempfile::tempdir()?;
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().context("fixture provider missing")?,
    )
    .context("fixture descriptor missing")?;
    let queried = Arc::new(tokio::sync::Notify::new());
    let release = Arc::new(tokio::sync::Notify::new());
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        |_request, _sink, _start, _cancelled| async {
            Err(crate::provider::ProviderError::Cancelled)
        },
    )
    .with_models(vec![serde_json::from_value(json!({
        "id":"fixture-model", "reasoning":["low","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?])
    .with_model_list_answer({
        let queried = queried.clone();
        let release = release.clone();
        move || {
            let queried = queried.clone();
            let release = release.clone();
            async move {
                queried.notify_one();
                release.notified().await;
                Ok(())
            }
        }
    });
    let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"closing-settings",
        "provider":{"kind":"mock","command":[]},
    }))?)?;
    broker.set_provider_for_test(provider.provider_handle());
    let initial_model = broker.snapshot()?.session.model;
    let controller = SessionController::new(broker).await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let input = queued_offer_input(
        &controller,
        "queued-settings",
        "",
        Some(json!({
            "model":"fixture-model", "effort":"high",
        })),
    )?;
    controller.attach_input(&sink).await?;
    queried.notified().await;
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    controller.exchange_owner.close().await;
    while controller
        .exchange_owner
        .input_ready(sink.clone())
        .await
        .is_ok()
    {}
    release.notify_one();
    controller.finish_exchange_owner().await?;

    let snapshot = controller.coordinator.submissions.snapshot()?;
    let [message] = snapshot.messages.as_slice() else {
        anyhow::bail!("shutdown consumed queued settings");
    };
    assert_eq!(message.id, input.id.as_str());
    assert_eq!(message.delivery(), InputDelivery::Queued);
    assert!(snapshot.suspended);
    assert_eq!(
        controller.broker.lock().await.snapshot()?.session.model,
        initial_model
    );
    provider.provider_handle().shutdown().await?;
    Ok(())
}

/// Retire a staged steer while its captured foreground receiver is still open.
#[tokio::test]
async fn stopped_staged_steering_returns_to_the_paused_queue() -> Result<()> {
    use crate::input::InputDelivery;
    let root = tempfile::tempdir()?;
    let controller = receipt_test_controller(root.path()).await?;
    let input = queued_offer_input(&controller, "staged-steer", "Preserve this text", None)?;
    let execution = controller.coordinator.execution.acquire().await?;
    let mut active = controller.parent_input.activate(None)?;
    let (sink, _output) = forge_protocol::outbound::channel();
    assert!(
        controller
            .exchange_owner
            .offer_steering(
                crate::exchange::SteeringDeliveryOffer {
                    input: input.clone(),
                    control: controller
                        .coordinator
                        .control
                        .clone()
                        .acquire_owned()
                        .await?,
                    order: Arc::new(tokio::sync::Semaphore::new(1))
                        .acquire_owned()
                        .await?,
                },
                sink,
            )
            .await?
    );
    let command = active
        .receive()
        .await
        .context("steering was never staged")?;
    let queue = &controller.coordinator.submissions;
    queue.suspend(true, None)?;
    // The foreground effect gate drops this command before running it and waits for the
    // provider's separate collection to close the lane.
    drop(command);
    loop {
        let changed = queue.changed.notified();
        let snapshot = queue.snapshot()?;
        let [message] = snapshot.messages.as_slice() else {
            anyhow::bail!("stopping removed the unsent steer");
        };
        if message.delivery() != InputDelivery::Delivering {
            assert_eq!(message.id, input.id.as_str());
            assert_eq!(message.text, "Preserve this text");
            assert_eq!(message.delivery(), InputDelivery::Queued);
            assert!(snapshot.suspended);
            break;
        }
        changed.await;
    }
    drop(active);
    drop(execution);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Accept a draft into the actual queue without starting its legacy readiness listener.
fn queued_offer_input(
    controller: &SessionController,
    id: &str,
    text: &str,
    config: Option<Value>,
) -> Result<crate::exchange::command::InputRevision> {
    controller.coordinator.submissions.apply(serde_json::from_value(json!({
        "id":format!("submit-{id}"),"action":{"kind":"submit","message":{"id":id,"text":text,"intent":"auto","config":config}},
    }))?)?;
    let message = controller
        .coordinator
        .submissions
        .snapshot()?
        .messages
        .into_iter()
        .find(|message| message.id == id)
        .unwrap();
    Ok(crate::exchange::command::InputRevision {
        id: crate::exchange::command::InputId::new(id)?,
        revision: message.revision,
    })
}

/// Create the real session owner without starting input dispatch or a live native process.
async fn receipt_test_controller(root: &std::path::Path) -> Result<Arc<SessionController>> {
    SessionController::new(HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":root,"data_root":root.join("data"),"client_id":"owner-receipts",
        "provider":{"kind":"mock","command":[]},
    }))?)?)
    .await
}

/// Register the real input journal and receipt binding used by owner observation tests.
async fn registered_receipt(
    controller: &SessionController,
    id: &str,
    purpose: crate::input::InputPurpose,
) -> Result<(
    crate::exchange::command::InputRevision,
    Arc<crate::provider::submission::Submission>,
)> {
    registered_receipt_on_route(controller, id, purpose, crate::input::InputRoute::Turn).await
}

/// Register one real receipt on the route whose recovery authority the test exercises.
async fn registered_receipt_on_route(
    controller: &SessionController,
    id: &str,
    purpose: crate::input::InputPurpose,
    route: crate::input::InputRoute,
) -> Result<(
    crate::exchange::command::InputRevision,
    Arc<crate::provider::submission::Submission>,
)> {
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    controller.coordinator.submissions.apply(InputOperation {
        id: format!("submit-{id}"),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: id.into(),
                text: "Owned receipt".into(),
                purpose: purpose.clone(),
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Auto,
            })),
        },
    })?;
    let message = controller
        .coordinator
        .submissions
        .claim(id, 1, route)?
        .unwrap();
    let catalog = controller.catalog_request.read().await.clone();
    let provider = controller.provider_handle().descriptor().kind;
    let input = crate::provider::ProviderInput::from_user_text(message.text);
    let submission = match purpose {
        crate::input::InputPurpose::TaskEntry { id, .. }
        | crate::input::InputPurpose::TaskContinuation { id, .. } => controller
            .submission_store
            .prepare_task_input(&catalog, input, provider, &id)?,
        crate::input::InputPurpose::Clarification { .. }
        | crate::input::InputPurpose::User
        | crate::input::InputPurpose::CheckResult { .. }
        | crate::input::InputPurpose::Question { .. }
        | crate::input::InputPurpose::QuestionContinuation { .. }
        | crate::input::InputPurpose::WorkflowContinuation { .. } => controller
            .submission_store
            .prepare_input(&catalog, input, provider, Some(id.into()))?,
    };
    let selected = crate::exchange::command::InputRevision {
        id: crate::exchange::command::InputId::new(id)?,
        revision: message.revision,
    };
    controller
        .exchange_owner
        .observe_input_receipt(
            crate::exchange::InputReceiptFact::Prepared {
                input: selected.clone(),
                submission: submission.clone(),
            },
            None,
        )
        .await?;
    Ok((selected, submission))
}

/// Restore server-acknowledged drafts through the real controller constructor without dispatch.
#[tokio::test]
async fn controller_reopens_acknowledged_queue_suspended() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let initialize: InitializeRequest = serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"input-durability", "provider":{"kind":"mock","command":[]},
    }))?;
    let controller = SessionController::new(HarnessBroker::initialize(initialize.clone())?).await?;
    let session_id = controller.observer.session_id().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    // Typed input commands resolve each attachment before acknowledgment, so the fixture
    // imports a real retained file with a non-ASCII name instead of an untyped placeholder.
    let source = fixture.path().join("資料.txt");
    std::fs::write(&source, "retained notes")?;
    let attachment =
        crate::attachment::Attachment::import(&controller.data_root, &source)?.selection(1)?;
    for (request_id, message_id) in [(1, "first"), (2, "second")] {
        controller.input_request(Request {
            id: request_id, method: "session.input.apply".into(),
            params: json!({"id":format!("submit-{message_id}"), "action":{"kind":"submit","message":{
                "id":message_id,"text":format!("{} draft {message_id}", attachment.label),"intent":"queue",
                "attachments":[attachment],"config":{"model":"mock"}
            }}}),
        }, RuntimeMethod::SessionInputApply, &sink).await?;
        loop {
            let frame = output
                .recv()
                .await?
                .context("input acknowledgment channel closed")?;
            let response: Value = serde_json::from_slice(frame.bytes())?;
            if response["id"] == request_id {
                assert!(response["error"].is_null());
                break;
            }
        }
    }
    let before = controller.coordinator.submissions.snapshot()?;
    controller.finish_exchange_owner().await?;
    drop(controller);
    let mut reopened = initialize;
    reopened.session_id = Some(session_id);
    let controller = SessionController::new(HarnessBroker::initialize(reopened)?).await?;
    let restored = controller.coordinator.submissions.snapshot()?;
    assert!(restored.suspended);
    assert_eq!(
        serde_json::to_value(restored.messages)?,
        serde_json::to_value(before.messages)?
    );
    assert!(
        controller
            .coordinator
            .submissions
            .operation("submit-first")?
            .is_some()
    );
    assert!(
        controller
            .coordinator
            .submissions
            .claim("first", 1, crate::input::InputRoute::Turn)?
            .is_none()
    );
    Ok(())
}

/// Keep a retracted prompt in the durable queue until an editor takes it back or retries it.
///
/// Interrupting a prompt before any model output retracts it and deletes its exchange. The
/// editor that would restore the text can exit before it hears of the retraction, so the host
/// must hold the text as an unsent entry instead of trusting that restoration. Reopening pauses
/// the queue, and an explicit retry of the entry must still send it.
#[tokio::test]
async fn retracted_prompt_survives_restart_and_runs_after_retry() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let controller = receipt_test_controller(root.path()).await?;
    let session = controller.observer.session_id().to_owned();
    let control = controller
        .provider_handle()
        .mock_response_control()
        .context("mock control")?;
    // The mock holds the turn before any output, which is where an interrupt may retract it.
    let hold = control
        .dispatch(
            &session,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let (frames, mut received) = tokio::sync::mpsc::unbounded_channel::<Value>();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let _ = frames.send(serde_json::from_slice(frame.bytes())?);
        }
        Ok::<_, anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    controller
        .input_request(
            Request {
                id: 1,
                method: "session.input.apply".into(),
                params: json!({"id":"submit-retracted","action":{"kind":"submit","message":{
                    "id":"retracted-prompt","text":"Explain ring buffers at length","intent":"auto"
                }}}),
            },
            RuntimeMethod::SessionInputApply,
            &sink,
        )
        .await?;
    control
        .dispatch(
            &session,
            MockResponseCommand::Wait {
                id: hold["id"].as_str().context("hold identity")?.into(),
            },
        )
        .await?;
    cancellation::apply(
        &controller,
        &Request {
            id: 2,
            method: "session.turn.cancel".into(),
            params: json!({"operation_id":"cancel-retracted","restore_prompt_if_no_output":true}),
        },
        &sink,
    )
    .await?;
    loop {
        let frame = received
            .recv()
            .await
            .context("input observation channel closed before the retraction")?;
        if frame["event"] == "input_changed" && frame["payload"]["retracted"] == "retracted-prompt"
        {
            break;
        }
    }
    assert!(
        SqliteStore::read_only(&controller.data_root)?
            .list_exchange(&session)?
            .is_empty(),
        "retraction kept the provisional exchange"
    );
    // No editor acknowledges the retraction: the owner stops as it does when Neovim exits.
    controller.finish_exchange_owner().await?;
    drop(sink);
    drop(controller);
    reader.abort();
    let reopened = receipt_test_controller_with_session(root.path(), &session).await?;
    let restored = reopened.coordinator.submissions.snapshot()?;
    let [unsent] = restored.messages.as_slice() else {
        anyhow::bail!(
            "retracted prompt left the durable queue: {:?}",
            restored.messages
        );
    };
    assert_eq!(unsent.text, "Explain ring buffers at length");
    assert_eq!(unsent.delivery(), crate::input::InputDelivery::Failed);
    assert_eq!(
        unsent.submission_id(),
        None,
        "the unsent entry kept the retracted provider receipt"
    );
    assert_ne!(
        unsent.id, "retracted-prompt",
        "the unsent entry reused an identity whose provider record was accepted"
    );
    assert!(
        restored.suspended,
        "recovered unsent input resumed delivery"
    );
    // Retrying the unsent entry is the user's request to send it, so it must reach the provider
    // although the restart left the queue paused.
    let control = reopened
        .provider_handle()
        .mock_response_control()
        .context("mock control")?;
    let hold = control
        .dispatch(
            &session,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<_, anyhow::Error>(())
    });
    reopened.attach_input(&sink).await?;
    reopened
        .input_request(
            Request {
                id: 3,
                method: "session.input.apply".into(),
                params: json!({"id":"retry-unsent","action":{
                    "kind":"retry","id":unsent.id,"revision":unsent.revision,
                }}),
            },
            RuntimeMethod::SessionInputApply,
            &sink,
        )
        .await?;
    assert!(
        !reopened.coordinator.submissions.snapshot()?.suspended,
        "the retry left the recovered queue paused"
    );
    let delivered = control
        .dispatch(
            &session,
            MockResponseCommand::Wait {
                id: hold["id"].as_str().context("hold identity")?.into(),
            },
        )
        .await?;
    assert!(
        delivered["prompt"]
            .as_str()
            .is_some_and(|prompt| prompt.contains("Explain ring buffers at length")),
        "the retried entry never reached the provider: {delivered}"
    );
    control
        .dispatch(
            &session,
            MockResponseCommand::Release {
                id: hold["id"].as_str().context("hold identity")?.into(),
            },
        )
        .await?;
    reopened.finish_exchange_owner().await?;
    drop(sink);
    reader.abort();
    Ok(())
}

/// Reuse preparation after a crash before the queue can link its submission ID.
#[tokio::test]
async fn queue_claim_preparation_gap_reuses_only_one_native_submission() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let initialize: InitializeRequest = serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"input-gap", "provider":{"kind":"mock","command":[]},
    }))?;
    let controller = SessionController::new(HarnessBroker::initialize(initialize.clone())?).await?;
    let session_id = controller.observer.session_id().to_owned();
    controller.coordinator.submissions.apply(serde_json::from_value(json!({
        "id":"submit-gap", "action":{"kind":"submit","message":{"id":"gap","text":"once","intent":"queue"}}
    }))?)?;
    controller
        .coordinator
        .submissions
        .claim("gap", 1, crate::input::InputRoute::Turn)?
        .context("claim failed")?;
    let catalog = controller.catalog_request.read().await.clone();
    let journal = controller.submission_store.clone();
    let prepared = journal.prepare_input(
        &catalog,
        crate::provider::ProviderInput::from_user_text("once"),
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        Some("gap".into()),
    )?;
    controller.finish_exchange_owner().await?;
    drop(controller);
    let mut resumed = initialize;
    resumed.session_id = Some(session_id);
    let controller = SessionController::new(HarnessBroker::initialize(resumed)?).await?;
    assert!(controller.coordinator.submissions.snapshot()?.suspended);
    let again = journal.prepare_input(
        &catalog,
        crate::provider::ProviderInput::from_user_text("once"),
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        Some("gap".into()),
    )?;
    assert_eq!(prepared.id(), again.id());
    let edited = journal.prepare_input(
        &catalog,
        crate::provider::ProviderInput::from_user_text("edited"),
        forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        Some("gap".into()),
    )?;
    assert_ne!(prepared.id(), edited.id());
    assert!(matches!(
        prepared.record()?.delivery,
        crate::provider::submission::SubmissionState::Cancelled
    ));
    assert_eq!(
        journal
            .pending(
                &edited.record()?.request,
                forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                None
            )?
            .len(),
        1
    );
    Ok(())
}

/// Own one mock-provider session with the agent tools enabled, for prompts that arrive while
/// the parent exchange delegates to children.
struct DelegatingSession {
    controller: Arc<SessionController>,
    session: String,
    sink: MessageSender,
    reader: tokio::task::JoinHandle<Result<()>>,
    // Declared last: fields drop in order, and Windows cannot delete files still open.
    _root: tempfile::TempDir,
}

impl DelegatingSession {
    async fn start(client_id: &str) -> Result<Self> {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let controller =
            SessionController::new(HarnessBroker::initialize(serde_json::from_value(json!({
                "workspace":root.path(),"data_root":root.path().join("data"),"client_id":client_id,
                "provider":{"kind":"mock","command":[]},
            }))?)?)
            .await?;
        let session = controller.observer.session_id().to_owned();
        controller.agent_coordinator.set_enabled(true);
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<_, anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        Ok(Self {
            controller,
            session,
            sink,
            reader,
            _root: root,
        })
    }

    async fn control(
        &self,
        command: crate::provider::mock_response::MockResponseCommand,
    ) -> Result<Value> {
        self.controller
            .provider_handle()
            .mock_response_control()
            .context("mock control")?
            .dispatch(&self.session, command)
            .await
    }

    async fn script(&self, script: forge_provider_api::testing::TestPromptScript) -> Result<()> {
        self.control(crate::provider::mock_response::MockResponseCommand::Script { script })
            .await?;
        Ok(())
    }

    /// Queue one user prompt, as the editor's composer does.
    fn submit(&self, id: &str, text: &str) -> Result<()> {
        use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
        self.controller
            .coordinator
            .submissions
            .apply(InputOperation {
                id: format!("submit-{id}"),
                control_revision: 0,
                after_control: None,
                action: InputAction::Submit {
                    message: Box::new(InputMessage::queued(crate::input::InputDraft {
                        id: id.into(),
                        text: text.into(),
                        purpose: crate::input::InputPurpose::User,
                        attachments: Vec::new(),
                        config: None,
                        intent: InputIntent::Auto,
                    })),
                },
            })?;
        Ok(())
    }

    /// Wait until execution is idle, the queue is empty, and at least `count` exchanges exist,
    /// all of them settled.
    async fn settled_exchanges(&self, count: usize) -> Result<Vec<crate::exchange::Exchange>> {
        loop {
            if self.controller.coordinator.execution.available_permits() == 1
                && self
                    .controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .is_empty()
            {
                let exchanges = crate::storage::HistoryReader::current(&self.controller.data_root)?
                    .list_exchange(&self.session)?;
                if exchanges.len() >= count
                    && exchanges.iter().all(|exchange| exchange.is_finished())
                {
                    return Ok(exchanges);
                }
            }
            tokio::task::yield_now().await;
        }
    }

    async fn finish(self) -> Result<()> {
        self.controller.finish_exchange_owner().await?;
        drop(self.sink);
        self.reader.abort();
        Ok(())
    }
}

/// Commit a completed run at the same inbox boundary used by check collection.
fn submit_check_result(controller: &SessionController, owner: &str, run: &str) -> Result<()> {
    use crate::input::{
        InputAction, InputDraft, InputIntent, InputMessage, InputOperation, InputPurpose,
    };
    use forge_provider_api::forge::{TurnInput, TurnKind};
    let id = format!("check-result:{run}");
    let result = serde_json::from_value(json!({
        "run_id":run,"exchange_id":owner,"state":"failed","complete":true,"checks":[]
    }))?;
    let turn = TurnInput::new(TurnKind::CheckResult { result });
    let message = InputMessage::queued(InputDraft {
        id: id.clone(),
        text: turn.label(),
        purpose: InputPurpose::CheckResult {
            exchange_id: owner.to_owned(),
            run_id: run.to_owned(),
        },
        attachments: Vec::new(),
        config: None,
        intent: InputIntent::Auto,
    })
    .with_turn(turn);
    controller.observer.check_inbox.apply(InputOperation {
        id,
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(message),
        },
    })?;
    Ok(())
}

/// Late results steer their resumed owner even after a newer conversation exchange settled.
#[tokio::test]
async fn check_results_steer_resumed_exchange_below_newer_history() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = DelegatingSession::start("resumed-check-owner").await?;
    fixture.submit("original", "Set up the checks")?;
    let owner = fixture.settled_exchanges(1).await?[0].id.clone();
    fixture.submit("question", "Which command ran?")?;
    fixture.settled_exchanges(2).await?;
    let hold = fixture
        .control(MockResponseCommand::Hold {
            mode: None,
            any_session: false,
        })
        .await?;
    let barrier = hold["id"].as_str().context("mock hold")?.to_owned();
    let inbox = &fixture.controller.observer.check_inbox;
    submit_check_result(&fixture.controller, &owner, "first")?;
    fixture
        .controller
        .exchange_owner
        .input_ready(fixture.sink.clone())
        .await?;
    fixture
        .control(MockResponseCommand::Wait {
            id: barrier.clone(),
        })
        .await?;
    submit_check_result(&fixture.controller, &owner, "second")?;
    fixture
        .controller
        .exchange_owner
        .input_ready(fixture.sink.clone())
        .await?;
    let steered = tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let changed = inbox.changed.notified();
            if inbox.snapshot()?.messages.iter().all(|message| {
                message.id != "check-result:second"
                    || message.delivery() == crate::input::InputDelivery::Accepted
            }) {
                return Ok::<_, anyhow::Error>(());
            }
            changed.await;
        }
    })
    .await;
    fixture
        .control(MockResponseCommand::Release { id: barrier })
        .await?;
    steered.context("second result never reached the resumed turn")??;
    let exchanges = fixture.settled_exchanges(2).await?;
    assert_eq!(
        exchanges
            .iter()
            .find(|exchange| exchange.id == owner)
            .context("check owner")?
            .turn
            .len(),
        2
    );
    assert_eq!(
        exchanges
            .iter()
            .find(|exchange| exchange.id != owner)
            .context("later exchange")?
            .turn
            .len(),
        1
    );
    fixture.finish().await
}

/// A result for an older exchange waits while another exchange owns the provider receiver.
#[tokio::test]
async fn check_result_waits_for_its_own_exchange() -> Result<()> {
    use crate::exchange::command::{InputId, InputRevision};
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = DelegatingSession::start("other-check-owner").await?;
    fixture.submit("original", "Start checks")?;
    let origin = fixture.settled_exchanges(1).await?[0].id.clone();
    let hold = fixture
        .control(MockResponseCommand::Hold {
            mode: None,
            any_session: false,
        })
        .await?;
    let barrier = hold["id"].as_str().context("mock hold")?.to_owned();
    fixture.submit("question", "What command ran?")?;
    fixture
        .control(MockResponseCommand::Wait {
            id: barrier.clone(),
        })
        .await?;
    let controller = &fixture.controller;
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    submit_check_result(controller, &origin, "deferred")?;
    let inbox = &controller.observer.check_inbox;
    let message = inbox.snapshot()?.messages.remove(0);
    let offered = controller
        .exchange_owner
        .offer_steering(
            crate::exchange::SteeringDeliveryOffer {
                input: InputRevision {
                    id: InputId::new(message.id)?,
                    revision: message.revision,
                },
                control,
                order: Arc::new(tokio::sync::Semaphore::new(1))
                    .acquire_owned()
                    .await?,
            },
            fixture.sink.clone(),
        )
        .await?;
    assert!(!offered, "another exchange accepted the result");
    assert_eq!(
        inbox.snapshot()?.messages[0].delivery(),
        crate::input::InputDelivery::Queued
    );
    fixture
        .control(MockResponseCommand::Release { id: barrier })
        .await?;
    loop {
        let changed = inbox.changed.notified();
        if inbox.snapshot()?.messages.is_empty() {
            break;
        }
        changed.await;
    }
    let exchanges = fixture.settled_exchanges(2).await?;
    assert_eq!(
        exchanges
            .iter()
            .find(|exchange| exchange.id == origin)
            .context("check owner")?
            .turn
            .len(),
        2
    );
    fixture.finish().await
}

/// A prompt submitted while the parent continues after its children steers that continuation.
///
/// The child's result continues the parent under a successor invocation of the same exchange, and
/// the steer's acceptance must be applied to that exchange rather than refused as belonging to
/// another invocation, which failed the exchange and lost the prompt.
#[tokio::test]
async fn prompt_during_the_continuation_after_children_steers_it() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let fixture = DelegatingSession::start("continuation-steer").await?;
    fixture
        .script(TestPromptScript {
            acceptance: TestInputAcceptance::BeforeResponse,
            steps: vec![
                TestPromptStep::ToolCall {
                    name: "agent_spawn".into(),
                    arguments: json!({"profile":"worker","text":"Inspect the sources"}),
                },
                TestPromptStep::Message {
                    content: TestMessageContent::Response,
                    text: "Waiting for the child".into(),
                },
            ],
            terminal: TestPromptTerminal::Completed,
        })
        .await?;
    // The hold takes the parent's next turn, which is the continuation the child's result starts.
    let hold = fixture
        .control(MockResponseCommand::Hold {
            mode: None,
            any_session: false,
        })
        .await?;
    let hold_id = hold["id"].as_str().context("hold identity")?.to_owned();
    fixture.submit("delegate", "Delegate an inspection")?;
    fixture
        .control(MockResponseCommand::Wait {
            id: hold_id.clone(),
        })
        .await?;
    fixture.submit("steer", "Also name the exported types")?;
    // Release the continuation only once the provider holds the steer, so the turn cannot end
    // before the steer reaches it and send the prompt as a later follow-up instead.
    loop {
        let changed = fixture
            .controller
            .coordinator
            .submissions
            .changed
            .notified();
        if fixture
            .controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .iter()
            .all(|message| {
                message.id != "steer" || message.delivery() == crate::input::InputDelivery::Accepted
            })
        {
            break;
        }
        changed.await;
    }
    fixture
        .control(MockResponseCommand::Release { id: hold_id })
        .await?;
    let exchanges = fixture.settled_exchanges(1).await?;
    ensure!(
        exchanges.len() == 1,
        "the steer started a separate exchange instead of steering the continuation"
    );
    let exchange = &exchanges[0];
    ensure!(
        exchange.state() == crate::exchange::ExchangeState::Complete && exchange.turn.len() == 2,
        "the steered continuation settled {:?} after {} turns: {:?}",
        exchange.state(),
        exchange.turn.len(),
        exchange.finalization_error
    );
    ensure!(
        serde_json::to_string(exchange)?.contains("Also name the exported types"),
        "the steered prompt is not in its exchange"
    );
    fixture.finish().await
}

/// A prompt submitted while the parent waits for its children is never lost and never fails the
/// exchange, and the parent's report survives.
///
/// The prompt waits while the parent is parked and then reaches the parent with the children's
/// results as the prompt of a successor invocation. That invocation's acceptance travels under the
/// exchange's first invocation, which the foreground once refused as crossing invocations.
#[tokio::test]
async fn prompt_while_the_parent_waits_for_children_reaches_the_parent() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let fixture = DelegatingSession::start("parked-steer").await?;
    fixture
        .script(TestPromptScript {
            acceptance: TestInputAcceptance::BeforeResponse,
            steps: vec![
                TestPromptStep::Barrier {
                    id: forge_provider_api::testing::TestHoldId::new("before-spawn")?,
                },
                TestPromptStep::Message {
                    content: TestMessageContent::Response,
                    text: "Waiting for the child".into(),
                },
            ],
            terminal: TestPromptTerminal::Completed,
        })
        .await?;
    fixture
        .script(TestPromptScript {
            acceptance: TestInputAcceptance::BeforeResponse,
            steps: vec![TestPromptStep::Message {
                content: TestMessageContent::Response,
                text: "The child inspected the sources".into(),
            }],
            terminal: TestPromptTerminal::Completed,
        })
        .await?;
    fixture.submit("delegate", "Delegate an inspection")?;
    fixture
        .control(MockResponseCommand::Wait {
            id: "before-spawn".into(),
        })
        .await?;
    // Reserve the child for this exchange, then hold its launch while the parent settles.
    // Releasing the coordinator's hold admits the real mock child and its result.
    let mut parent = crate::storage::HistoryReader::current(&fixture.controller.data_root)?
        .latest_exchange(&fixture.session)?
        .context("delegating exchange")?;
    let coordinator = &fixture.controller.agent_coordinator;
    let spawn = crate::agent::SpawnAgentRequest {
        profile: Some("worker".into()),
        access: None,
        text: "Inspect the sources".into(),
    };
    let reservation = coordinator.reserve_spawn(
        &crate::agent::AgentRequest {
            operation_id: "inspect-sources".into(),
            source: crate::agent::AgentSource::Model,
            operation: crate::agent::AgentOperation::Spawn(spawn.clone()),
        },
        &spawn,
        crate::agent::AgentSelection {
            profile: "worker".into(),
            instructions: String::new(),
            provider: fixture.controller.provider_handle().descriptor().kind,
            model: "default".into(),
            reasoning_effort: "low".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
        },
        &parent.id,
        1,
    )?;
    parent.append_agent_task(&reservation.task)?;
    crate::storage::SqliteStore::open(&fixture.controller.data_root)?.admit_agent_task(
        &reservation.agent,
        &reservation.task,
        &parent,
        reservation.continuation_authorization(),
    )?;
    let launch_hold = coordinator.hold_launches()?;
    ensure!(
        matches!(reservation.commit()?, crate::agent::AgentCommit::Admitted(work) if !work.launch),
        "child must be queued"
    );
    fixture
        .control(MockResponseCommand::Release {
            id: "before-spawn".into(),
        })
        .await?;
    loop {
        let parked = coordinator.has_active_task(&parent.id)
            && crate::storage::HistoryReader::current(&fixture.controller.data_root)?
                .list_exchange(&fixture.session)?
                .first()
                .and_then(|exchange| exchange.turn.last())
                .is_some_and(|turn| {
                    matches!(turn.state(), crate::turn::TurnState::Finished { .. })
                });
        if parked {
            break;
        }
        tokio::task::yield_now().await;
    }
    fixture.submit("steer", "Also name the exported types")?;
    drop(launch_hold);
    let exchanges = fixture.settled_exchanges(1).await?;
    ensure!(
        exchanges
            .iter()
            .all(|exchange| exchange.state() == crate::exchange::ExchangeState::Complete),
        "an exchange did not complete: {:?}",
        exchanges
            .iter()
            .map(|exchange| (exchange.state(), exchange.finalization_error.clone()))
            .collect::<Vec<_>>()
    );
    let transcript = serde_json::to_string(&exchanges)?;
    ensure!(
        transcript.contains("Also name the exported types"),
        "the prompt submitted during the wait was lost"
    );
    ensure!(
        transcript.contains("The child inspected the sources"),
        "the parent's report after its children is missing"
    );
    fixture.finish().await
}

/// A prompt submitted after an exchange failed runs as the next exchange.
///
/// The failed prompt stays in the queue as a failed entry and pauses it. A new prompt resumes the
/// queue, as it does after an interrupt, so it must not wait behind the failed entry, which only an
/// explicit retry sends again.
#[tokio::test]
async fn prompt_after_a_failed_exchange_runs_and_leaves_the_failure_to_retry() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{TestInputAcceptance, TestPromptScript, TestPromptTerminal};
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let controller = receipt_test_controller(root.path()).await?;
    let session = controller.observer.session_id().to_owned();
    let control = controller
        .provider_handle()
        .mock_response_control()
        .context("mock control")?;
    control
        .dispatch(
            &session,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![],
                    terminal: TestPromptTerminal::RejectedBeforeDispatch(
                        forge_provider_api::ProviderFailureKind::Runtime,
                    ),
                },
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<_, anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let submit = |request: u64, id: &str, text: &str, control_revision: u64| Request {
        id: request,
        method: "session.input.apply".into(),
        params: json!({"id":format!("submit-{id}"),"control_revision":control_revision,"action":{
            "kind":"submit","message":{"id":id,"text":text,"intent":"auto"}
        }}),
    };
    controller
        .input_request(
            submit(1, "failed-prompt", "Prompt whose exchange fails", 0),
            RuntimeMethod::SessionInputApply,
            &sink,
        )
        .await
        .context("first submission")?;
    let paused = tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let snapshot = controller.coordinator.submissions.snapshot()?;
            if snapshot.messages.iter().any(|message| {
                message.id == "failed-prompt"
                    && message.delivery() == crate::input::InputDelivery::Failed
            }) {
                break Ok::<_, anyhow::Error>(snapshot);
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("the scripted failure never settled as a failed entry")??;
    assert!(paused.suspended, "the failed prompt left the queue running");
    controller
        .input_request(
            submit(
                2,
                "next-prompt",
                "Prompt after the failure",
                paused.control_revision,
            ),
            RuntimeMethod::SessionInputApply,
            &sink,
        )
        .await
        .context("second submission")?;
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let finished = SqliteStore::read_only(&controller.data_root)?
                .list_exchange(&session)?
                .iter()
                .any(|exchange| {
                    exchange.input_message_id.as_deref() == Some("next-prompt")
                        && exchange.is_finished()
                });
            if finished {
                break Ok::<_, anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("the prompt after the failure waited behind the failed entry")??;
    let snapshot = controller.coordinator.submissions.snapshot()?;
    assert!(
        snapshot
            .messages
            .iter()
            .any(|message| message.id == "failed-prompt"
                && message.delivery() == crate::input::InputDelivery::Failed),
        "the failed prompt left the queue instead of waiting for its retry: {:?}",
        snapshot.messages
    );
    controller.finish_exchange_owner().await?;
    drop(sink);
    reader.abort();
    Ok(())
}
