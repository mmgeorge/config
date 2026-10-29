use super::*;

fn submit(id: &str) -> InputOperation {
    InputOperation {
        id: format!("submit-{id}"),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage {
                id: id.into(),
                revision: 0,
                text: format!("message {id}"),
                purpose: crate::input::InputPurpose::User,
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Queue,
                delivery: InputDelivery::Queued,
                route: None,
                reservation: None,
                submission_id: None,
                error: None,
            }),
        },
    }
}

#[test]
fn submission_after_pause_resumes_only_that_control_operation() -> Result<()> {
    let input = SessionInputQueue::default();
    input.suspend(true, Some("first-pause"))?;
    let mut after_pause = submit("after-pause");
    after_pause.after_control = Some("first-pause".into());
    if let InputAction::Submit { message } = &mut after_pause.action {
        message.intent = InputIntent::Auto;
    }
    input.apply(after_pause)?;
    assert!(!input.snapshot()?.suspended);
    input.suspend(true, Some("second-pause"))?;
    let mut delayed = submit("delayed");
    delayed.after_control = Some("first-pause".into());
    if let InputAction::Submit { message } = &mut delayed.action {
        message.intent = InputIntent::Auto;
    }
    input.apply(delayed)?;
    assert!(input.snapshot()?.suspended);
    let mut queued = submit("explicit-queue");
    queued.after_control = Some("second-pause".into());
    input.apply(queued)?;
    assert!(input.snapshot()?.suspended);
    Ok(())
}

#[test]
fn operations_preserve_delivery_and_edit_boundaries() -> Result<()> {
    let input = SessionInputQueue::default();
    let first = input.apply(submit("first"))?;
    assert_eq!(input.apply(submit("first"))?.revision, first.revision);
    input.apply(submit("second"))?;
    let reserved = input.apply(InputOperation {
        id: "reserve".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Reserve {
            id: "first".into(),
            revision: 1,
            reservation: "editor".into(),
        },
    })?;
    assert!(reserved.error.is_none());
    assert!(input.claim("first", 1, InputRoute::Turn)?.is_none());
    assert!(input.claim("second", 1, InputRoute::Turn)?.is_none());
    let rejected = input.apply(InputOperation {
        id: "stale-edit".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Commit {
            reservation: "editor".into(),
            edits: vec![InputEdit {
                id: "first".into(),
                revision: 1,
                text: "stale".into(),
                attachments: vec![],
                config: None,
            }],
        },
    })?;
    assert!(rejected.error.is_some());
    assert_eq!(input.snapshot()?.messages[0].text, "message first");
    let committed = input.apply(InputOperation {
        id: "commit".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Commit {
            reservation: "editor".into(),
            edits: vec![InputEdit {
                id: "first".into(),
                revision: 2,
                text: "edited".into(),
                attachments: vec![],
                config: None,
            }],
        },
    })?;
    assert!(committed.error.is_none());
    input.suspend(true, None)?;
    assert!(input.claim("first", 3, InputRoute::Turn)?.is_none());
    input.suspend(false, None)?;
    assert!(input.claim("first", 3, InputRoute::Turn)?.is_some());
    let missed_edit = input.apply(InputOperation {
        id: "late-edit".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Reserve {
            id: "first".into(),
            revision: 4,
            reservation: "other-editor".into(),
        },
    })?;
    assert!(missed_edit.error.is_some());
    input.delivery("first", None, None, None)?;
    assert!(input.claim("second", 1, InputRoute::Turn)?.is_some());
    input.delivery(
        "second",
        Some(InputDelivery::Unknown),
        Some("receipt-pending".into()),
        Some("lost acknowledgement".into()),
    )?;
    let restored = input;
    assert_eq!(
        restored.snapshot()?.messages[0].delivery,
        InputDelivery::Unknown
    );
    assert!(restored.claim("second", 3, InputRoute::Turn)?.is_none());
    assert_eq!(
        restored.operation("submit-first")?.unwrap().revision,
        restored.snapshot()?.revision
    );
    restored.suspend(true, None)?;
    let mut delayed = submit("captured-before-pause");
    if let InputAction::Submit { message } = &mut delayed.action {
        message.intent = InputIntent::Auto;
    }
    restored.apply(delayed)?;
    assert!(
        restored.snapshot()?.suspended,
        "late input reopened a paused queue"
    );
    let control_revision = restored.snapshot()?.control_revision;
    let resumed = restored.apply(InputOperation {
        id: "resume-current-control".into(),
        control_revision,
        after_control: None,
        action: InputAction::Resume,
    })?;
    assert!(resumed.error.is_none() && !restored.snapshot()?.suspended);
    let independent = submit("after-task-change");
    restored.apply(independent)?;
    assert!(
        restored
            .claim("after-task-change", 1, InputRoute::Turn)?
            .is_none(),
        "changing tasks bypassed unresolved input delivery"
    );
    std::thread::scope(|scope| {
        let worker = |session_id: &str| -> Result<()> {
            let store = SessionInputQueue::default();
            for index in 0..32 {
                let id = format!("{session_id}-{index}");
                let result = store.apply(submit(&id))?;
                assert!(result.error.is_none(), "{:?}", result.error);
            }
            Ok(())
        };
        let first = scope.spawn(move || worker("concurrent-first"));
        let second = scope.spawn(move || worker("concurrent-second"));
        first.join().unwrap()?;
        second.join().unwrap()?;
        Ok::<(), anyhow::Error>(())
    })?;
    let fresh = SessionInputQueue::default();
    assert!(
        fresh.snapshot()?.messages.is_empty(),
        "a new session retained an old queue"
    );
    assert!(fresh.operation("submit-first")?.is_none());
    let failed = SessionInputQueue::default();
    failed.apply(submit("failed"))?;
    failed.apply(submit("following"))?;
    failed.delivery(
        "failed",
        Some(InputDelivery::Failed),
        None,
        Some("pre-delivery failure".into()),
    )?;
    assert!(
        failed.claim("following", 1, InputRoute::Turn)?.is_none(),
        "a follow-up bypassed an unresolved failure"
    );
    failed.apply(InputOperation {
        id: "discard-failure".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Remove {
            id: "failed".into(),
            revision: 2,
        },
    })?;
    assert!(failed.claim("following", 1, InputRoute::Turn)?.is_some());
    Ok(())
}

#[test]
fn user_input_precedes_generated_continuation() -> Result<()> {
    let input = SessionInputQueue::default();
    let mut continuation = submit("continuation");
    if let InputAction::Submit { message } = &mut continuation.action {
        message.purpose = InputPurpose::TaskContinuation {
            revision: crate::task::TaskRevision::fresh(),
            id: crate::task::TaskInputId::fresh(),
            completed_turn_id: crate::task::TaskTurnId {
                exchange_ordinal: 1,
                turn_ordinal: 1,
            },
            reason: "More work remains".into(),
        };
        message.intent = InputIntent::Auto;
    }
    input.apply(continuation)?;
    input.apply(submit("user"))?;
    assert!(input.claim("continuation", 1, InputRoute::Turn)?.is_none());
    assert!(
        input
            .claim("continuation", 1, InputRoute::Steering)?
            .is_none()
    );
    assert!(input.claim("user", 1, InputRoute::Turn)?.is_some());
    input.delivery("user", None, None, None)?;
    assert!(input.claim("continuation", 1, InputRoute::Turn)?.is_some());
    Ok(())
}

#[test]
fn legacy_task_bound_input_becomes_session_input() -> Result<()> {
    let message: InputMessage = serde_json::from_value(serde_json::json!({
        "id":"queued-before-replacement", "text":"Keep this message", "task_id":"replaced-task"
    }))?;
    assert!(matches!(message.purpose, InputPurpose::User));
    let input = SessionInputQueue::default();
    input.apply(InputOperation {
        id: "submit-before-replacement".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(message),
        },
    })?;
    assert!(input.has_pending()?.1);
    let admitted = input
        .claim("queued-before-replacement", 1, InputRoute::Turn)?
        .unwrap();
    assert_eq!(admitted.text, "Keep this message");
    assert!(serde_json::to_value(admitted)?.get("task_id").is_none());
    Ok(())
}

#[test]
fn steering_bypasses_only_explicit_queued_messages() -> Result<()> {
    let input = SessionInputQueue::default();
    input.apply(submit("first-queued"))?;
    input.apply(submit("second-queued"))?;
    for id in ["first-steering", "second-steering"] {
        let mut operation = submit(id);
        if let InputAction::Submit { message } = &mut operation.action {
            message.intent = InputIntent::Auto;
        }
        input.apply(operation)?;
    }
    assert_eq!(
        input
            .snapshot()?
            .messages
            .iter()
            .map(|message| message.id.as_str())
            .collect::<Vec<_>>(),
        [
            "first-steering",
            "second-steering",
            "first-queued",
            "second-queued"
        ]
    );
    assert!(
        input
            .claim("first-queued", 1, InputRoute::Steering)?
            .is_none()
    );
    assert!(
        input
            .claim("second-steering", 1, InputRoute::Steering)?
            .is_none()
    );
    input.suspend(true, None)?;
    assert!(
        input
            .claim("first-steering", 1, InputRoute::Steering)?
            .is_none()
    );
    input.suspend(false, None)?;
    assert!(
        input
            .claim("first-steering", 1, InputRoute::Steering)?
            .is_some()
    );
    input.delivery("first-steering", None, None, None)?;
    assert!(
        input
            .claim("second-steering", 1, InputRoute::Steering)?
            .is_some()
    );
    input.delivery("second-steering", None, None, None)?;
    assert!(input.claim("second-queued", 1, InputRoute::Turn)?.is_none());
    assert!(input.claim("first-queued", 1, InputRoute::Turn)?.is_some());
    input.delivery("first-queued", None, None, None)?;
    assert!(input.claim("second-queued", 1, InputRoute::Turn)?.is_some());
    Ok(())
}

#[test]
fn steering_respects_uncertain_delivery_and_configuration_barriers() -> Result<()> {
    let input = SessionInputQueue::default();
    input.apply(submit("uncertain"))?;
    input.delivery("uncertain", Some(InputDelivery::Unknown), None, None)?;
    let mut steering = submit("steering");
    if let InputAction::Submit { message } = &mut steering.action {
        message.intent = InputIntent::Auto;
    }
    input.apply(steering.clone())?;
    assert!(input.claim("steering", 1, InputRoute::Steering)?.is_none());
    input.delivery("uncertain", None, None, None)?;
    assert!(input.claim("steering", 1, InputRoute::Steering)?.is_some());
    let mut configuration = submit("configuration");
    if let InputAction::Submit { message } = &mut configuration.action {
        message.intent = InputIntent::Auto;
        message.config = Some(serde_json::json!({"model":"next-model"}));
    }
    let configured = SessionInputQueue::default();
    configured.apply(configuration)?;
    configured.apply(steering)?;
    assert!(
        configured
            .claim("configuration", 1, InputRoute::Steering)?
            .is_none()
    );
    assert!(
        configured
            .claim("steering", 1, InputRoute::Steering)?
            .is_none()
    );
    Ok(())
}

#[test]
fn accepted_input_never_returns_as_failed_and_retry_preserves_identity() -> Result<()> {
    let input = SessionInputQueue::default();
    input.apply(submit("message"))?;
    let claimed = input.claim("message", 1, InputRoute::Turn)?.unwrap();
    assert!(
        input
            .claim("message", claimed.revision, InputRoute::Turn)?
            .is_none()
    );
    let failed = input.delivery(
        "message",
        Some(InputDelivery::Failed),
        Some("attempt-one".into()),
        Some("not sent".into()),
    )?;
    let retry = InputOperation {
        id: "retry".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Retry {
            id: "message".into(),
            revision: failed.upsert[0].revision,
        },
    };
    let retried = input.apply(retry.clone())?;
    assert!(retried.error.is_none());
    assert_eq!(retried.upsert[0].id, "message");
    assert!(retried.upsert[0].submission_id.is_none());
    assert!(
        input
            .claim("message", retried.upsert[0].revision, InputRoute::Turn)?
            .is_some()
    );
    input.delivery(
        "message",
        Some(InputDelivery::Accepted),
        Some("attempt-two".into()),
        None,
    )?;
    input.delivery(
        "message",
        Some(InputDelivery::Failed),
        Some("attempt-one".into()),
        Some("late failure".into()),
    )?;
    assert_eq!(
        input.snapshot()?.messages[0].delivery,
        InputDelivery::Accepted
    );
    input.delivery("message", None, None, None)?;
    input.delivery(
        "message",
        Some(InputDelivery::Failed),
        None,
        Some("late cancellation".into()),
    )?;
    assert!(input.snapshot()?.messages.is_empty());
    let replay = input.apply(retry)?;
    assert!(
        replay.snapshot.unwrap().messages.is_empty(),
        "old acknowledgement resurrected a delivered message"
    );
    Ok(())
}
