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

/// Commit internal evidence independently of user publication and retain its receipt after delivery.
#[test]
fn check_inbox_commits_without_user_publication_and_preserves_receipt() -> Result<()> {
    let root = tempfile::tempdir()?;
    use crate::session::SessionStore;
    crate::storage::SqliteStore::open(root.path())?
        .save_session(&crate::storage::test::session("session", "."))?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open_channel(
        root.path(),
        "session",
        &submissions,
        InputChannel::CheckResults,
    )?;
    let mut operation = queued_operation("check");
    if let InputAction::Submit { message } = &mut operation.action {
        message.purpose = InputPurpose::CheckResult {
            exchange_id: "exchange".into(),
            run_id: "run".into(),
        };
        message.config = None;
        message.attachments.clear();
        let result = serde_json::from_value(serde_json::json!({
            "run_id":"run","exchange_id":"exchange","state":"passed","complete":true,"checks":[]
        }))?;
        **message = message
            .as_ref()
            .clone()
            .with_turn(forge_provider_api::forge::TurnInput::new(
                forge_provider_api::forge::TurnKind::CheckResult { result },
            ));
    }
    let database = crate::storage::test::raw_connection(root.path())?;
    database.execute_batch(
        "CREATE TRIGGER reject_check_publication BEFORE INSERT ON owner_publication BEGIN SELECT RAISE(ABORT,'publication unavailable'); END;"
    )?;
    assert!(queue.apply(operation.clone())?.error.is_none());
    database.execute_batch("DROP TRIGGER reject_check_publication;")?;
    queue.apply(operation.clone())?;
    queue.apply(operation)?;
    let publications =
        crate::exchange::publication::SessionPublications::open(root.path(), "session")?;
    assert_eq!(
        publications
            .after(None, std::num::NonZeroU16::new(10).unwrap())?
            .len(),
        0
    );
    assert_eq!(queue.check_result_receipts()?.len(), 1);
    let revision = queue.snapshot()?.messages[0].revision;
    let rejected = queue.apply(InputOperation {
        id: "remove-check".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Remove {
            id: "check".into(),
            revision,
        },
    })?;
    assert_eq!(rejected.error_code, Some(InputRejection::WrongChannel));
    let claimed = queue
        .claim("check", revision, InputRoute::Turn)?
        .context("claim internal result")?;
    queue.delivery_if_current(
        "check",
        InputDeliveryGuard::Claimed(claimed.revision),
        None,
        None,
        None,
    )?;
    assert_eq!(queue.check_result_receipts()?.len(), 1);
    assert_eq!(
        publications
            .after(None, std::num::NonZeroU16::new(10).unwrap())?
            .len(),
        0
    );
    Ok(())
}

/// Recover an unclaimed provider handoff without changing the result identity.
#[test]
fn check_inbox_recovers_unsubmitted_delivery() -> Result<()> {
    check_inbox_restart_case(DispatchCut::NoSubmission)
}

/// Retry only the prepared delivery whose journal proves no native send occurred.
#[test]
fn check_inbox_recovers_prepared_delivery() -> Result<()> {
    check_inbox_restart_case(DispatchCut::Prepared)
}

/// Keep a potentially sent result fenced instead of replaying it after restart.
#[test]
fn check_inbox_retains_uncertain_delivery() -> Result<()> {
    check_inbox_restart_case(DispatchCut::Sending)
}

/// Remove already accepted delivery while retaining its deduplication receipt.
#[test]
fn check_inbox_retires_accepted_delivery() -> Result<()> {
    check_inbox_restart_case(DispatchCut::Accepted)
}

/// Recover a cancelled preparation whose native send never started.
#[test]
fn check_inbox_recovers_cancelled_preparation() -> Result<()> {
    check_inbox_restart_case(DispatchCut::Cancelled)
}

/// Reopen ordinary production state at one explicit provider-delivery boundary.
fn check_inbox_restart_case(boundary: DispatchCut) -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open_channel(
        root.path(),
        "session",
        &submissions,
        InputChannel::CheckResults,
    )?;
    let result = serde_json::from_value(serde_json::json!({
        "run_id":"run","exchange_id":"exchange","state":"passed","complete":true,"checks":[]
    }))?;
    let turn = forge_provider_api::forge::TurnInput::new(
        forge_provider_api::forge::TurnKind::CheckResult { result },
    );
    let message = InputMessage::queued(InputDraft {
        id: "result".into(),
        text: turn.label(),
        purpose: InputPurpose::CheckResult {
            exchange_id: "exchange".into(),
            run_id: "run".into(),
        },
        attachments: Vec::new(),
        config: None,
        intent: InputIntent::Auto,
    })
    .with_turn(turn);
    queue.apply(InputOperation {
        id: "result".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(message.clone()),
        },
    })?;
    queue
        .claim("result", 1, InputRoute::Turn)?
        .context("claim result")?;
    if !matches!(boundary, DispatchCut::NoSubmission) {
        let mut request = fixture_request();
        request.input = crate::provider::ProviderInput::from_message(&message);
        let submission = submissions.prepare_input_request(
            &request,
            forge_providers::registry::default_id(),
            crate::provider::submission::SubmissionOrigin::QueuedInput(
                crate::exchange::command::InputId::new("result")?,
            ),
        )?;
        match boundary {
            DispatchCut::Sending => submission.sending(None)?,
            DispatchCut::Accepted => {
                submission.sending(None)?;
                submission.accepted(None)?;
            }
            DispatchCut::Cancelled => submission.cancel()?,
            DispatchCut::Prepared | DispatchCut::NoSubmission => {}
        }
        queue.delivery(
            "result",
            Some(InputDelivery::Delivering),
            Some(submission.id().into()),
            None,
        )?;
    }
    drop(queue);
    let recovered = SessionInputQueue::open_channel(
        root.path(),
        "session",
        &submissions,
        InputChannel::CheckResults,
    )?;
    let snapshot = recovered.snapshot()?;
    assert!(!snapshot.suspended);
    assert_eq!(recovered.check_result_receipts()?.len(), 1);
    match boundary {
        DispatchCut::Accepted => assert!(snapshot.messages.is_empty()),
        DispatchCut::Sending => {
            assert_eq!(snapshot.messages[0].delivery(), InputDelivery::Unknown);
            assert!(
                recovered
                    .claim("result", snapshot.messages[0].revision, InputRoute::Turn)?
                    .is_none()
            );
        }
        DispatchCut::NoSubmission | DispatchCut::Prepared | DispatchCut::Cancelled => {
            assert_eq!(snapshot.messages[0].delivery(), InputDelivery::Queued);
            assert!(snapshot.messages[0].submission_id().is_none());
            assert!(
                recovered
                    .claim("result", snapshot.messages[0].revision, InputRoute::Turn)?
                    .is_some()
            );
        }
    }
    Ok(())
}

/// Preserve drafts and receipts while releasing edit locks owned by a dead editor.
#[test]
fn accepted_queue_reopens_suspended_with_order_and_idempotency() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
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
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
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

/// Build the provider request journaled for the queued fixture message.
fn fixture_request() -> crate::provider::ProviderRequest {
    crate::provider::ProviderRequest {
        dispatch: forge_provider_api::submission::DispatchTracker::default(),
        execution_owner: None,
        harness_session_id: "session".into(),
        workspace: "workspace".into(),
        input: crate::provider::ProviderInput::from_user_text("draft message"),
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
        child_agent: None,
    }
}

/// Claim the fixture message and bind it to a fresh provider journal record.
fn claim_with_submission(
    queue: &SessionInputQueue,
    submissions: &Arc<SubmissionStore>,
) -> Result<Arc<crate::provider::submission::Submission>> {
    queue.apply(queued_operation("message"))?;
    queue
        .claim("message", 1, InputRoute::Turn)?
        .context("claim failed")?;
    let submission =
        submissions.prepare(&fixture_request(), forge_providers::registry::default_id())?;
    queue.delivery(
        "message",
        Some(InputDelivery::Delivering),
        Some(submission.id().into()),
        None,
    )?;
    Ok(submission)
}

/// A prompt that failed before dispatch keeps blocking follow-ups after the host restarts.
///
/// Settlement retires the unsent record to Cancelled while the queue shows the entry Failed;
/// reopening must not read that retirement as a user retraction and drop the entry.
#[test]
fn failed_prompt_survives_restart_and_blocks_follow_ups() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
    let submission = claim_with_submission(&queue, &submissions)?;
    submission.cancel()?;
    queue.delivery(
        "message",
        Some(InputDelivery::Failed),
        None,
        Some("Provider did not confirm message acceptance".into()),
    )?;
    queue.apply(queued_operation("follow-up"))?;
    drop(queue);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
    let restored = queue.snapshot()?;
    assert_eq!(restored.messages[0].id, "message");
    assert_eq!(restored.messages[0].delivery(), InputDelivery::Failed);
    assert_eq!(
        restored.messages[0].error(),
        Some("Provider did not confirm message acceptance")
    );
    queue.apply(InputOperation {
        id: "resume".into(),
        control_revision: restored.control_revision,
        after_control: None,
        action: InputAction::Resume,
    })?;
    let follow_up = &restored.messages[1];
    assert!(
        queue
            .claim(&follow_up.id, follow_up.revision, InputRoute::Turn)?
            .is_none(),
        "a follow-up drained past the failed prompt"
    );
    Ok(())
}

/// A prompt interrupted before dispatch whose exchange the transcript already shows settles
/// there at restore: it leaves the queue, cannot be retried, and does not pause later input.
#[test]
fn interrupted_prompt_with_exchange_settles_at_restore() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
    let submission = claim_with_submission(&queue, &submissions)?;
    drop(queue);
    let exchanged = std::collections::HashSet::from(["message".to_owned()]);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &exchanged)?;
    let restored = queue.snapshot()?;
    assert!(restored.messages.is_empty(), "{:?}", restored.messages);
    assert!(!restored.suspended, "a settled prompt paused the queue");
    assert!(matches!(
        submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Cancelled
    ));
    Ok(())
}

/// A prompt interrupted before dispatch without an exchange stays as the failed entry the user
/// retries explicitly, and the queue reopens paused behind it.
#[test]
fn interrupted_prompt_without_exchange_stays_failed_and_paused() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
    claim_with_submission(&queue, &submissions)?;
    drop(queue);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
    let restored = queue.snapshot()?;
    assert_eq!(restored.messages.len(), 1);
    assert_eq!(restored.messages[0].delivery(), InputDelivery::Failed);
    assert!(restored.suspended);
    Ok(())
}

/// Retrying the failed entry of a paused queue keeps each prompt the queue held as its own
/// turn, so none of them steers into the retried turn out of submission order.
#[test]
fn retrying_a_paused_queue_keeps_held_prompts_as_turns() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
    claim_with_submission(&queue, &submissions)?;
    drop(queue);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
    let mut held = queued_operation("held");
    let InputAction::Submit { message } = &mut held.action else {
        unreachable!("queued operations submit")
    };
    message.intent = InputIntent::Auto;
    // The stale control revision submits behind the pause without resuming it.
    queue.apply(held)?;
    let paused = queue.snapshot()?;
    assert!(paused.suspended);
    let retried = queue.apply(InputOperation {
        id: "retry".into(),
        control_revision: paused.control_revision,
        after_control: None,
        action: InputAction::Retry {
            id: "message".into(),
            revision: paused
                .messages
                .iter()
                .find(|message| message.id == "message")
                .context("failed prompt missing")?
                .revision,
        },
    })?;
    assert!(retried.error.is_none(), "{:?}", retried.error);
    let resumed = queue.snapshot()?;
    assert!(!resumed.suspended);
    let held = resumed
        .messages
        .iter()
        .find(|message| message.id == "held")
        .context("held prompt missing")?;
    assert_eq!(held.intent, InputIntent::Queue);
    Ok(())
}

/// Removing uncertain input discards its record without replay and unblocks the queue.
#[test]
fn removing_uncertain_input_discards_its_record_without_replay() -> Result<()> {
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
    let submission = claim_with_submission(&queue, &submissions)?;
    submission.sending(Some(forge_provider_api::RecoveryToken::new(
        b"fixture-sending".to_vec(),
    )?))?;
    let unknown = queue.delivery(
        "message",
        Some(InputDelivery::Unknown),
        None,
        Some("Delivery unconfirmed. Reconcile before retrying.".into()),
    )?;
    let revision = unknown.upsert[0].revision;
    queue.apply(queued_operation("follow-up"))?;
    let follow_up = queue.snapshot()?.messages[1].clone();
    assert!(
        queue
            .claim(&follow_up.id, follow_up.revision, InputRoute::Turn)?
            .is_none(),
        "uncertain input must block later claims until it is discarded"
    );
    let removed = queue.apply(InputOperation {
        id: "discard".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Remove {
            id: "message".into(),
            revision,
        },
    })?;
    assert!(removed.error.is_none(), "{:?}", removed.error);
    assert_eq!(removed.removed, vec!["message".to_owned()]);
    assert_eq!(
        submissions.submission(submission.id())?.record()?.delivery,
        crate::provider::submission::SubmissionState::Discarded
    );
    assert!(
        submissions
            .pending(
                &fixture_request(),
                forge_providers::registry::default_id(),
                None
            )?
            .is_empty(),
        "a discarded record must not be recovered into a later prompt"
    );
    assert!(
        submission.begin_prompt().is_err(),
        "a discarded record must never dispatch again"
    );
    assert!(
        queue
            .claim(&follow_up.id, follow_up.revision, InputRoute::Turn)?
            .is_some()
    );
    drop(queue);
    let queue = SessionInputQueue::open(root.path(), "session", &submissions, &Default::default())?;
    assert!(
        queue
            .snapshot()?
            .messages
            .iter()
            .all(|message| message.id != "message")
    );
    Ok(())
}

/// Classify every crash cut without ever replaying uncertain or accepted provider input.
#[test]
fn queue_restart_reconciles_dispatch_cuts_without_replay() -> Result<()> {
    // One store serves every cut under its own session, because creating and syncing a fresh
    // set of databases per cut made the test's duration follow disk load rather than its logic.
    let root = tempfile::tempdir()?;
    let submissions = submission_store(root.path())?;
    for boundary in [
        DispatchCut::NoSubmission,
        DispatchCut::Prepared,
        DispatchCut::Sending,
        DispatchCut::Accepted,
        DispatchCut::Cancelled,
    ] {
        let session = format!("session-{boundary:?}");
        let queue =
            SessionInputQueue::open(root.path(), &session, &submissions, &Default::default())?;
        queue.apply(queued_operation("message"))?;
        queue
            .claim("message", 1, InputRoute::Turn)?
            .context("claim failed")?;
        if !matches!(boundary, DispatchCut::NoSubmission) {
            let request = crate::provider::ProviderRequest {
                dispatch: forge_provider_api::submission::DispatchTracker::default(),
                execution_owner: None,
                harness_session_id: session.clone(),
                workspace: "workspace".into(),
                input: crate::provider::ProviderInput::from_user_text("draft message"),
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
                child_agent: None,
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
        let queue =
            SessionInputQueue::open(root.path(), &session, &submissions, &Default::default())?;
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
