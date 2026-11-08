use super::*;

fn submit(id: &str) -> InputOperation {
    InputOperation {
        id: format!("submit-{id}"),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: id.into(),
                text: format!("message {id}"),
                purpose: crate::input::InputPurpose::User,
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Queue,
            })),
        },
    }
}

/// Preserve reserved and edited input when an earlier local directive completes late.
#[test]
fn local_input_completion_cannot_remove_an_edited_or_reserved_revision() -> Result<()> {
    let input = SessionInputQueue::default();
    input.apply(submit("message"))?;
    let original = input.snapshot()?.messages[0].revision;
    input.apply(InputOperation {
        id: "edit-message".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Reserve {
            id: "message".into(),
            revision: original,
            reservation: "editor".into(),
        },
    })?;
    let reserved = serde_json::to_value(input.snapshot()?)?;
    assert!(
        input
            .delivery_if_current(
                "message",
                InputDeliveryGuard::Queued(input.snapshot()?.messages[0].revision),
                None,
                None,
                None
            )?
            .is_none()
    );
    assert!(
        input
            .delivery_if_current(
                "message",
                InputDeliveryGuard::Queued(original),
                None,
                None,
                None
            )?
            .is_none()
    );
    assert_eq!(serde_json::to_value(input.snapshot()?)?, reserved);
    input.apply(InputOperation {
        id: "commit-message".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Commit {
            reservation: "editor".into(),
            edits: vec![InputEdit {
                id: "message".into(),
                revision: input.snapshot()?.messages[0].revision,
                text: "New content must survive".into(),
                attachments: Vec::new(),
                config: None,
            }],
        },
    })?;
    assert!(
        input
            .delivery_if_current(
                "message",
                InputDeliveryGuard::Queued(original),
                Some(InputDelivery::Failed),
                None,
                Some("stale local result".into())
            )?
            .is_none()
    );
    assert_eq!(
        input.snapshot()?.messages[0].delivery(),
        InputDelivery::Queued
    );
    assert_eq!(
        input.snapshot()?.messages[0].text,
        "New content must survive"
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
    assert!(retried.upsert[0].submission_id().is_none());
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
        input.snapshot()?.messages[0].delivery(),
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
