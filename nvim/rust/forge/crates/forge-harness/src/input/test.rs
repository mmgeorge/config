use super::*;

fn submit(id: &str) -> InputOperation {
    InputOperation {
        id: format!("submit-{id}"),
        control_revision: 0,
        action: InputAction::Submit {
            message: InputMessage {
                id: id.into(),
                revision: 0,
                text: format!("message {id}"),
                task_id: None,
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Queue,
                delivery: InputDelivery::Queued,
                reservation: None,
                submission_id: None,
                error: None,
            },
        },
    }
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
    input.suspend(true)?;
    assert!(input.claim("first", 3, InputRoute::Turn)?.is_none());
    input.suspend(false)?;
    assert!(input.claim("first", 3, InputRoute::Turn)?.is_some());
    let missed_edit = input.apply(InputOperation {
        id: "late-edit".into(),
        control_revision: 0,
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
        first.revision
    );
    restored.suspend(true)?;
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
        action: InputAction::Resume,
    })?;
    assert!(resumed.error.is_none() && !restored.snapshot()?.suspended);
    let mut independent = submit("different-task");
    if let InputAction::Submit { message } = &mut independent.action {
        message.task_id = Some("new-task".into());
    }
    restored.apply(independent)?;
    assert!(
        restored
            .claim("different-task", 1, InputRoute::Turn)?
            .is_some(),
        "an old task's queue blocked a different task"
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
        failed.claim("following", 1, InputRoute::Turn)?.is_some(),
        "failed input blocked the runnable queue"
    );
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
    assert!(
        input
            .claim("first-steering", 1, InputRoute::Turn)?
            .is_none()
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
    input.suspend(true)?;
    assert!(
        input
            .claim("first-steering", 1, InputRoute::Steering)?
            .is_none()
    );
    input.suspend(false)?;
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
