use super::*;
use crate::provider::submission::SubmissionStore;

#[derive(Clone, Copy, Debug)]
enum DispatchCut {
    NoSubmission,
    Prepared,
    Sending,
    Accepted,
    Cancelled,
}

/// Create one queue operation with content that exercises full draft persistence.
fn queued_operation(id: &str) -> InputOperation {
    InputOperation {
        id: format!("operation-{id}"),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: id.into(),
                text: format!("draft {id}"),
                purpose: InputPurpose::User,
                attachments: vec![serde_json::json!({"id":"attachment","name":"資料.txt"})],
                config: Some(serde_json::json!({"model":"fixture","effort":"low"})),
                intent: InputIntent::Queue,
            })),
        },
    }
}

/// Create the real provider journal used to reconcile queue dispatch evidence.
fn submission_store(root: &std::path::Path) -> Result<Arc<SubmissionStore>> {
    SubmissionStore::open(root, Arc::new(crate::trace::TraceStore::open(root)?))
}

/// Preserve drafts and receipts while releasing edit locks owned by a dead editor.
#[test]
fn accepted_queue_reopens_suspended_with_order_and_idempotency() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    queue.apply(queued_operation("first"))?;
    queue.apply(queued_operation("second"))?;
    queue.apply(InputOperation {
        id: "reserve".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Reserve {
            id: "first".into(),
            revision: 1,
            reservation: "editor".into(),
        },
    })?;
    let before = queue.snapshot()?;
    drop(queue);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    let restored = queue.snapshot()?;
    assert!(restored.suspended);
    assert_eq!(restored.messages[0].text, before.messages[0].text);
    assert_eq!(
        restored.messages[0].attachments,
        before.messages[0].attachments
    );
    assert_eq!(restored.messages[0].config, before.messages[0].config);
    assert!(restored.messages[0].reservation().is_none());
    assert_eq!(
        restored.messages[0].revision,
        before.messages[0].revision + 1
    );
    assert_eq!(restored.messages[1].id, "second");
    queue.apply(queued_operation("first"))?;
    assert_eq!(queue.snapshot()?.messages.len(), 2);
    let mut duplicate = queued_operation("first");
    duplicate.id = "different-operation".into();
    assert!(queue.apply(duplicate)?.error.is_some());
    assert_eq!(queue.snapshot()?.messages.len(), 2);
    assert!(queue.claim("second", 1, InputRoute::Turn)?.is_none());
    Ok(())
}

/// Classify every crash cut without ever replaying uncertain or accepted provider input.
#[test]
fn queue_restart_reconciles_dispatch_cuts_without_replay() -> Result<()> {
    for boundary in [
        DispatchCut::NoSubmission,
        DispatchCut::Prepared,
        DispatchCut::Sending,
        DispatchCut::Accepted,
        DispatchCut::Cancelled,
    ] {
        let root = tempfile::tempdir()?;
        let submissions = submission_store(root.path())?;
        let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
        queue.apply(queued_operation("message"))?;
        queue
            .claim("message", 1, InputRoute::Turn)?
            .context("claim failed")?;
        if !matches!(boundary, DispatchCut::NoSubmission) {
            let request = crate::provider::ProviderRequest {
                dispatch: forge_provider_api::submission::DispatchTracker::default(),
                execution_owner: None,
                harness_session_id: "session".into(),
                workspace: "workspace".into(),
                input: crate::provider::ProviderInput::from_text("draft message"),
                mode: crate::provider::PromptMode::Chat,
                model: "fixture".into(),
                effort: "low".into(),
                context_window: None,
                service_tier: Default::default(),
                access: Default::default(),
                approval_mode: crate::session::ApprovalMode::Ask,
                provider_session_id: None,
                submission: None,
                acknowledged_child_reports: Vec::new(),
                recovered_input: Vec::new(),
                control_context: None,
            };
            let submission =
                submissions.prepare(&request, forge_providers::registry::default_id())?;
            if matches!(boundary, DispatchCut::Sending | DispatchCut::Accepted) {
                submission.sending(Some(forge_provider_api::RecoveryToken::new(
                    b"fixture-sending".to_vec(),
                )?))?;
            }
            if matches!(boundary, DispatchCut::Accepted) {
                submission.accepted(Some(forge_provider_api::RecoveryToken::new(
                    b"fixture-receipt".to_vec(),
                )?))?;
            }
            if matches!(boundary, DispatchCut::Cancelled) {
                submission.cancel()?;
            }
            queue.delivery(
                "message",
                Some(InputDelivery::Delivering),
                Some(submission.id().into()),
                None,
            )?;
        }
        drop(queue);
        let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
        let restored = queue.snapshot()?;
        assert_eq!(
            restored.suspended,
            !matches!(boundary, DispatchCut::Accepted | DispatchCut::Cancelled)
        );
        if matches!(boundary, DispatchCut::Accepted | DispatchCut::Cancelled) {
            assert!(restored.messages.is_empty());
        } else {
            let expected = match boundary {
                DispatchCut::NoSubmission => InputDelivery::Queued,
                DispatchCut::Sending => InputDelivery::Unknown,
                DispatchCut::Prepared => InputDelivery::Failed,
                DispatchCut::Accepted | DispatchCut::Cancelled => {
                    unreachable!("settled input was removed")
                }
            };
            assert_eq!(restored.messages[0].delivery(), expected, "{boundary:?}");
            assert!(
                queue
                    .claim("message", restored.messages[0].revision, InputRoute::Turn)?
                    .is_none()
            );
        }
    }
    Ok(())
}

/// Roll back memory and duplicate bookkeeping if the durable transaction cannot commit.
#[test]
fn queue_commit_failure_does_not_publish_candidate_state() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    queue.apply(queued_operation("first"))?;
    let before = serde_json::to_value(queue.snapshot()?)?;
    let database = rusqlite::Connection::open(crate::storage::state::database(root.path()))?;
    database.execute_batch("CREATE TRIGGER reject_queue_write BEFORE UPDATE ON input_session BEGIN SELECT RAISE(ABORT, 'fixture disk failure'); END;")?;
    assert!(queue.apply(queued_operation("second")).is_err());
    assert_eq!(serde_json::to_value(queue.snapshot()?)?, before);
    assert!(queue.operation("operation-second")?.is_none());
    database.execute_batch("DROP TRIGGER reject_queue_write;")?;
    queue.apply(queued_operation("second"))?;
    assert_eq!(queue.snapshot()?.messages.len(), 2);
    Ok(())
}

/// Reject corrupted persistent state rather than silently losing acknowledged input.
#[test]
fn queue_reopen_rejects_corrupt_snapshot() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    queue.apply(queued_operation("message"))?;
    drop(queue);
    let database = rusqlite::Connection::open(crate::storage::state::database(root.path()))?;
    database.execute("UPDATE input_session SET payload='{}'", [])?;
    assert!(SessionInputQueue::open(root.path(), "session", &submissions).is_err());
    Ok(())
}
