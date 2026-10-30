use super::*;

/// Restore server-acknowledged drafts through the real controller constructor without dispatch.
#[tokio::test]
async fn controller_reopens_acknowledged_queue_suspended() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let initialize: InitializeRequest = serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"input-durability", "backend":{"kind":"mock","command":[]},
    }))?;
    let controller = SessionController::new(HarnessBroker::initialize(initialize.clone())?)?;
    let session_id = controller.observer.session_id().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    for (request_id, message_id) in [(1, "first"), (2, "second")] {
        controller.input_request(Request {
            id: request_id, method: "session.input.apply".into(),
            params: json!({"id":format!("submit-{message_id}"), "action":{"kind":"submit","message":{
                "id":message_id,"text":format!("draft {message_id}"),"intent":"queue",
                "attachments":[{"id":"attachment","name":"資料.txt"}],"config":{"model":"mock"}
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
    drop(controller);
    let mut reopened = initialize;
    reopened.session_id = Some(session_id);
    let controller = SessionController::new(HarnessBroker::initialize(reopened)?)?;
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

/// Retry pending deletion cleanup, including an unloaded queue, before removing its session.
#[test]
fn interrupted_session_deletion_retries_queue_cleanup() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let initialize: InitializeRequest = serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"input-deletion", "backend":{"kind":"mock","command":[]},
    }))?;
    let controller = SessionController::new(HarnessBroker::initialize(initialize)?)?;
    let session_id = controller.observer.session_id().to_owned();
    let root = controller.data_root.clone();
    let submissions = controller.submission_store.clone();
    controller.coordinator.submissions.apply(serde_json::from_value(json!({
        "id":"queued-before-deletion", "action":{"kind":"submit","message":{"id":"delete-message","text":"retained","intent":"queue"}}
    }))?)?;
    drop(controller);
    let mut store = SqliteStore::open(&root)?;
    assert!(
        store
            .delete_session_with(&session_id, || anyhow::bail!("interrupted cleanup"))
            .is_err()
    );
    store.recover_session_deletions()?;
    assert!(store.load_session(&session_id)?.is_none());
    let queue = crate::input::SessionInputQueue::open(&root, &session_id, &submissions)?;
    assert!(queue.snapshot()?.messages.is_empty());
    assert!(queue.operation("queued-before-deletion")?.is_none());
    Ok(())
}

/// Reuse preparation after a crash before the queue can link its submission ID.
#[tokio::test]
async fn queue_claim_preparation_gap_reuses_only_one_native_submission() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let initialize: InitializeRequest = serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"input-gap", "backend":{"kind":"mock","command":[]},
    }))?;
    let controller = SessionController::new(HarnessBroker::initialize(initialize.clone())?)?;
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
        crate::backend::BackendInput::from_text("once"),
        crate::backend::BackendKind::Mock,
        Some("gap".into()),
    )?;
    drop(controller);
    let mut resumed = initialize;
    resumed.session_id = Some(session_id);
    let controller = SessionController::new(HarnessBroker::initialize(resumed)?)?;
    assert!(controller.coordinator.submissions.snapshot()?.suspended);
    let again = journal.prepare_input(
        &catalog,
        crate::backend::BackendInput::from_text("once"),
        crate::backend::BackendKind::Mock,
        Some("gap".into()),
    )?;
    assert_eq!(prepared.id(), again.id());
    let edited = journal.prepare_input(
        &catalog,
        crate::backend::BackendInput::from_text("edited"),
        crate::backend::BackendKind::Mock,
        Some("gap".into()),
    )?;
    assert_ne!(prepared.id(), edited.id());
    assert!(matches!(
        prepared.record()?.delivery,
        crate::backend::submission::SubmissionState::Cancelled
    ));
    assert_eq!(
        journal
            .pending(
                &edited.record()?.request,
                crate::backend::BackendKind::Mock,
                None
            )?
            .len(),
        1
    );
    Ok(())
}
