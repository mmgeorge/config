use super::*;

/// A failure after history replacement cannot retire only the task or only its history.
#[test]
fn rollback_task_history_and_session_commit_atomically() -> Result<()> {
    let (_root, mut store, controller) = crate::task::SessionTaskController::test_goal();
    for (id, ordinal) in [("rollback-root", 1), ("rollback-later", 2)] {
        let exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":id, "session_id":"session",
            "agent_id":crate::session::HarnessSession::primary_agent_id("session"),
            "ordinal":ordinal, "prompt":"Completed work", "state":"complete",
            "created_at_ms":ordinal, "node_list":[], "attributed_matches_checkpoint":false
        }))?;
        store.save_exchange(&exchange)?;
    }
    let before_session = store
        .load_session("session")?
        .context("fixture session missing")?;
    let before_task = controller.current()?.context("fixture task missing")?;
    let before_history = serde_json::to_value(store.list_exchange("session")?)?;
    let before_events = serde_json::to_value(store.list_session_event("session")?)?;
    let mut replacement = before_session.clone();
    replacement.provider_session_id = Some("restored-provider-session".into());
    replacement.updated_at_ms = 50;
    let roots = vec!["rollback-root".into(), "rollback-later".into()];

    // The rollback writes history first and preserves the active task until the
    // final session write. Fail there to exercise rollback of those earlier writes.
    store.connection.execute_batch(
        "CREATE TRIGGER reject_rollback_task_retirement BEFORE UPDATE ON session_record
         WHEN NEW.id='session' AND json_extract(NEW.payload,'$.session.active_task') IS NULL
         BEGIN SELECT RAISE(ABORT,'injected rollback task retirement failure'); END;",
    )?;
    let failure = controller.clear_for_rollback(
        &mut store,
        Some(&before_task.revision),
        &replacement,
        &roots,
        50,
    );
    assert!(failure.is_err(), "the injected final write must fail");
    assert_eq!(
        serde_json::to_value(store.load_session("session")?)?,
        serde_json::to_value(Some(&before_session))?
    );
    assert_eq!(
        serde_json::to_value(controller.current()?)?,
        serde_json::to_value(Some(&before_task))?
    );
    assert_eq!(
        serde_json::to_value(store.list_exchange("session")?)?,
        before_history
    );
    assert_eq!(
        serde_json::to_value(store.list_session_event("session")?)?,
        before_events
    );

    store
        .connection
        .execute_batch("DROP TRIGGER reject_rollback_task_retirement;")?;
    controller.clear_for_rollback(
        &mut store,
        Some(&before_task.revision),
        &replacement,
        &roots,
        50,
    )?;
    assert!(controller.current()?.is_none());
    let restored = store
        .load_session("session")?
        .context("restored session missing")?;
    assert!(restored.active_task.is_none());
    assert_eq!(
        restored.provider_session_id,
        replacement.provider_session_id
    );
    assert_eq!(restored.name, before_session.name);
    assert_eq!(
        store.load_exchange("rollback-root")?.unwrap().disposition(),
        crate::exchange::HistoryDisposition::RolledBack
    );
    assert_eq!(
        store
            .load_exchange("rollback-later")?
            .unwrap()
            .disposition(),
        crate::exchange::HistoryDisposition::Superseded
    );
    Ok(())
}
