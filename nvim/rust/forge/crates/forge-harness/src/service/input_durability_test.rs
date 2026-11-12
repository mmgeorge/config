use super::*;

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
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation, InputRoute};
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
        .claim(id, 1, InputRoute::Turn)?
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
