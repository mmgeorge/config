use super::bootstrap::{initialize, service};
use crate::storage::SqliteStore;
use anyhow::Result;
use forge_protocol::message::Request;
use serde_json::json;
use std::time::Duration;

/// Publish goal removal to an existing status document without a provider callback.
#[tokio::test]
async fn goal_clear_retires_open_paused_status_without_another_turn() -> Result<()> {
    use crate::task::{Task, TaskChange};
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
    let task = Task::goal(
        "Retain paused status until cleared".into(),
        Default::default(),
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
    reader.await??;
    Ok(())
}
