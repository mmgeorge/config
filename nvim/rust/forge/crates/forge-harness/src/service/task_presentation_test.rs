/// Retire review status when cancellation finishes without another provider turn.
#[tokio::test]
async fn plan_cancellation_retires_open_status_and_reopens_idle() -> Result<()> {
    use forge_buffer::identity::{DocumentId, ViewId};
    use forge_buffer::width::WidthProfile;

    let fixture = tempfile::tempdir()?;
    crate::checks::configure_fixture(fixture.path());
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let reviewed = reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
    let document = DocumentId("cancel-review-status".into());
    let opened = controller.presentation.lock().unwrap().open(
        document.clone(),
        ViewId("review-status-view".into()),
        WidthProfile::default(),
    )?;
    assert!(
        !opened.status.block.is_empty(),
        "completed planning lost its review status"
    );
    let cancellation = Request {
        id: 32,
        method: "session.plan.cancel".into(),
        params: json!({"operation_id":"cancel-review-status", "plan_id":reviewed["plan_id"], "digest":reviewed["digest"]}),
    };
    service
        .dispatch(Some(session_id.clone()), cancellation.clone(), &sink)
        .await?;
    assert!(controller.task_controller.current()?.is_none());
    let snapshot = controller.observer.snapshot()?;
    assert!(matches!(
        snapshot.status,
        crate::session::state_machine::SessionPhase::Idle
    ));
    assert!(snapshot.active_plan.is_none() && snapshot.active_elicitation.is_none());
    let cancelled_status = {
        let mut presentation = controller.presentation.lock().unwrap();
        let settled = presentation.sync(&document, opened.transcript.revision)?;
        assert!(
            settled.status.block.is_empty(),
            "cancelled plan retained actionable review status"
        );
        assert!(settled.status.revision > opened.status.revision);
        settled.status
    };
    service
        .dispatch(Some(session_id), cancellation, &sink)
        .await?;
    assert!(controller.task_controller.current()?.is_none());
    {
        let mut presentation = controller.presentation.lock().unwrap();
        let replayed = presentation.sync(&document, opened.transcript.revision)?;
        assert_eq!(
            replayed.status, cancelled_status,
            "cancellation replay republished or revived the retired status"
        );
        presentation.close(&document)?;
        let reopened = presentation.open(
            document.clone(),
            ViewId("reopened-status".into()),
            WidthProfile::default(),
        )?;
        assert!(
            reopened.status.block.is_empty(),
            "reopening restored cancelled review status"
        );
        presentation.close(&document)?;
    }
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    Ok(())
}

/// Publish goal removal to an existing status document without a provider callback.
#[tokio::test]
async fn goal_clear_retires_open_paused_status_without_another_turn() -> Result<()> {
    use crate::task::{ActiveTask, TaskChange};
    use forge_buffer::identity::{DocumentId, ViewId};
    use forge_buffer::width::WidthProfile;

    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let mut store = SqliteStore::open(&controller.data_root)?;
    let task = ActiveTask::goal(
        "Retain paused status until cleared".into(),
        Default::default(),
        false,
        1,
    );
    controller
        .task_controller
        .change(&mut store, None, TaskChange::Install(Box::new(task)), 1)?;
    let current = controller.task_controller.current()?.unwrap();
    controller
        .task_controller
        .change(&mut store, Some(&current.revision), TaskChange::Pause, 2)?;
    let snapshot = controller.observer.snapshot()?;
    let document = DocumentId("clear-goal-status".into());
    let opened = {
        let mut presentation = controller.presentation.lock().unwrap();
        presentation.reconcile(snapshot.timeline)?;
        presentation.open(
            document.clone(),
            ViewId("goal-status-view".into()),
            WidthProfile::default(),
        )?
    };
    assert!(!opened.status.block.is_empty());
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    service
        .dispatch(
            Some(session_id),
            Request {
                id: 33,
                method: "session.goal.clear".into(),
                params: json!({"operation_id":"clear-goal-status"}),
            },
            &sink,
        )
        .await?;
    assert!(controller.task_controller.current()?.is_none());
    {
        let mut presentation = controller.presentation.lock().unwrap();
        let settled = presentation.sync(&document, opened.transcript.revision)?;
        assert!(
            settled.status.block.is_empty(),
            "cleared goal retained paused status"
        );
        presentation.close(&document)?;
    }
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    Ok(())
}
