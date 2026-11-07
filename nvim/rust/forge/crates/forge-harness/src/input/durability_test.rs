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

/// Recover the same pending task entry even when a stale queue item was already discarded.
#[test]
fn pending_task_input_rebinds_after_restart_without_losing_its_identity() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    let InputAction::Submit { message } = queued_operation("entry").action else {
        unreachable!()
    };
    let mut message = *message;
    let id = crate::task::TaskInputId::fresh();
    message.id = id.as_str().into();
    message.purpose = InputPurpose::TaskEntry {
        revision: crate::task::TaskRevision::fresh(),
        id: id.clone(),
        start: crate::exchange::TaskStart {
            kind: crate::task::TaskKind::Plan,
            title: "Plan".into(),
        },
    };
    assert!(queue.reconcile_task_input(message.clone())?.is_some());
    assert!(queue.reconcile_task_input(message.clone())?.is_none());
    drop(queue);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    assert!(queue.snapshot()?.suspended);
    let revision = crate::task::TaskRevision::fresh();
    message.purpose = InputPurpose::TaskEntry {
        revision: revision.clone(),
        id,
        start: crate::exchange::TaskStart {
            kind: crate::task::TaskKind::Plan,
            title: "Plan".into(),
        },
    };
    queue.reconcile_task_input(message.clone())?;
    let restored = queue.snapshot()?;
    assert!(
        restored.suspended,
        "reconciliation must not implicitly resume the queue"
    );
    assert_eq!(restored.messages.len(), 1);
    assert!(
        matches!(&restored.messages[0].purpose, InputPurpose::TaskEntry { revision: current, .. } if current == &revision)
    );
    queue.delivery(&message.id, None, None, None)?;
    assert!(queue.snapshot()?.messages.is_empty());
    queue.reconcile_task_input(message.clone())?;
    assert_eq!(queue.snapshot()?.messages.len(), 1);
    assert_eq!(queue.snapshot()?.messages[0].id, message.id);
    Ok(())
}

/// Keep uncertain provider deliveries behind reconciliation rather than requeueing them on resume.
#[test]
fn task_input_reconciliation_preserves_uncertain_delivery() -> Result<()> {
    let queue = SessionInputQueue::default();
    let InputAction::Submit { message } = queued_operation("entry").action else {
        unreachable!()
    };
    let mut message = *message;
    let id = crate::task::TaskInputId::fresh();
    message.id = id.as_str().into();
    message.purpose = InputPurpose::TaskEntry {
        revision: crate::task::TaskRevision::fresh(),
        id: id.clone(),
        start: crate::exchange::TaskStart {
            kind: crate::task::TaskKind::Goal,
            title: "Goal".into(),
        },
    };
    queue.reconcile_task_input(message.clone())?;
    queue.claim(&message.id, 1, InputRoute::Turn)?.unwrap();
    queue.delivery(
        &message.id,
        Some(InputDelivery::Unknown),
        Some("provider-submission".into()),
        Some("Acceptance is unknown".into()),
    )?;
    message.purpose = InputPurpose::TaskEntry {
        revision: crate::task::TaskRevision::fresh(),
        id,
        start: crate::exchange::TaskStart {
            kind: crate::task::TaskKind::Goal,
            title: "Goal".into(),
        },
    };
    assert!(queue.reconcile_task_input(message)?.is_none());
    let retained = queue.snapshot()?;
    assert_eq!(retained.messages[0].delivery(), InputDelivery::Unknown);
    assert_eq!(
        retained.messages[0].submission_id(),
        Some("provider-submission")
    );
    Ok(())
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

/// Delete one closed session's full journal without disturbing another session's input.
#[test]
fn queue_deletion_removes_only_selected_session_receipts() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let first = SessionInputQueue::open(root.path(), "first-session", &submissions)?;
    let second = SessionInputQueue::open(root.path(), "second-session", &submissions)?;
    first.apply(queued_operation("first"))?;
    second.apply(queued_operation("second"))?;
    drop(first);
    drop(second);
    SessionInputQueue::delete_persisted(root.path(), "first-session")?;
    let first = SessionInputQueue::open(root.path(), "first-session", &submissions)?;
    let second = SessionInputQueue::open(root.path(), "second-session", &submissions)?;
    assert!(first.snapshot()?.messages.is_empty());
    assert!(first.operation("operation-first")?.is_none());
    assert!(first.apply(queued_operation("first"))?.error.is_none());
    assert_eq!(second.snapshot()?.messages[0].id, "second");
    assert!(second.operation("operation-second")?.is_some());
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

/// Retain the retry receipt and message tombstone after reconciling provider acceptance.
#[test]
fn accepted_retry_removal_and_receipt_survive_reopen() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    queue.apply(queued_operation("message"))?;
    let retry = InputOperation {
        id: "reconciled-retry".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Retry {
            id: "message".into(),
            revision: 1,
        },
    };
    queue.acknowledge_retry(retry.clone())?;
    drop(queue);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    assert!(queue.snapshot()?.messages.is_empty());
    assert!(queue.acknowledge_retry(retry)?.error.is_none());
    let mut duplicate = queued_operation("message");
    duplicate.id = "different-operation".into();
    assert!(queue.apply(duplicate)?.error.is_some());
    Ok(())
}

/// Preserve empty-session delivery readiness while retaining an intentional pause.
#[test]
fn empty_queue_reopen_preserves_readiness_and_explicit_suspension() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    queue.apply(queued_operation("finished"))?;
    queue.delivery("finished", None, None, None)?;
    drop(queue);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    assert!(!queue.snapshot()?.suspended);
    let mut next = queued_operation("next");
    if let InputAction::Submit { message } = &mut next.action {
        message.intent = InputIntent::Auto;
    }
    queue.apply(next)?;
    assert!(queue.claim("next", 1, InputRoute::Turn)?.is_some());
    queue.delivery("next", None, None, None)?;
    queue.suspend(true, Some("explicit-pause"))?;
    drop(queue);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    assert!(queue.snapshot()?.suspended);
    Ok(())
}

/// Commit proven non-delivery and retry eligibility without exposing an intermediate state.
#[test]
fn reconciled_retry_commits_one_revision_and_preserves_receipt_identity() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions)?;
    queue.apply(queued_operation("message"))?;
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
    let submission = submissions.prepare(&request, forge_providers::registry::default_id())?;
    submission.sending(None)?;
    queue.claim("message", 1, InputRoute::Turn)?.unwrap();
    queue.delivery(
        "message",
        Some(InputDelivery::Unknown),
        Some(submission.id().into()),
        Some("Acceptance is unknown".into()),
    )?;
    let before = queue.snapshot()?;
    let retry = InputOperation {
        id: "reconciled-retry".into(),
        control_revision: before.control_revision,
        after_control: None,
        action: InputAction::Retry {
            id: "message".into(),
            revision: before.messages[0].revision,
        },
    };
    let rejected = queue.retry_reconciled(retry.clone(), &submission.record()?)?;
    assert_eq!(
        rejected.error_code,
        Some(InputRejection::DeliveryUnconfirmed)
    );
    assert_eq!(
        queue.snapshot()?.messages[0].delivery(),
        InputDelivery::Unknown
    );
    submission.rejected("native history proved non-delivery".into())?;
    let mut safe_retry = retry;
    safe_retry.id = "safe-reconciled-retry".into();
    let revision = queue.snapshot()?.revision;
    let accepted = queue.retry_reconciled(safe_retry.clone(), &submission.record()?)?;
    assert!(accepted.error_code.is_none());
    assert_eq!(accepted.revision, revision + 1);
    let message = &accepted.upsert[0];
    assert_eq!(message.delivery(), InputDelivery::Queued);
    assert!(message.submission_id().is_none());
    assert_eq!(message.revision, before.messages[0].revision + 1);
    assert!(
        queue
            .retry_reconciled(safe_retry, &submission.record()?)?
            .error_code
            .is_none()
    );
    Ok(())
}
