use super::*;
use crate::{
    backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind},
    exchange::{Exchange, ExchangeState},
};

fn output_exchange(text: &str) -> Result<(Exchange, BackendEvent)> {
    let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
        "id":"output","session_id":"session","agent_id":"session:agent:primary","ordinal":1,
        "prompt":"run","kind":"chat","state":"running","turn":[],"node_list":[],"created_at_ms":1,
        "attributed_matches_checkpoint":false
    }))?;
    let address = ProviderAddress {
        owner: None,
        thread_id: "thread".into(),
        turn_id: "turn".into(),
    };
    exchange.start_turn(address.clone(), 1)?;
    let event = BackendEvent {
        received_at_ms: None,
        address: Some(address),
        turn_boundary: None,
        kind: "tool".into(),
        text: None,
        data: serde_json::json!({}),
        summary: None,
        task_update: None,
        activity: Some(ToolActivity {
            id: "call".into(),
            kind: ToolActivityKind::Command,
            title: "read".into(),
            output: Some(text.into()),
            status: Some("inProgress".into()),
            change: Default::default(),
            output_delta: false,
        }),
    };
    exchange.observe_turn(&event, 2)?;
    Ok((exchange, event))
}

#[test]
fn output_saves_pages_and_migration_preserve_content_without_history_reads() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut store = SqliteStore::open(root.path())?;
    store.save_session(&test::session("session", "workspace"))?;
    let source = "λ🙂\0\r\ntext".repeat(12_000);
    let (mut exchange, mut event) = output_exchange(&source)?;
    store.save_exchange(&exchange)?;
    let turn = exchange.turn[0].id().to_owned();
    store.connection.execute_batch("CREATE TRIGGER no_output_rewrite BEFORE INSERT ON tool_output_chunk BEGIN SELECT RAISE(ABORT,'unchanged output rewritten'); END;")?;
    let writes = store.connection.total_changes();
    store.save_exchange(&exchange)?;
    assert_eq!(store.connection.total_changes() - writes, 1);
    assert_eq!(
        store
            .tool_output_page("output", &turn, "call", 11, 17)?
            .output,
        source.chars().skip(11).take(17).collect::<String>()
    );
    store
        .connection
        .execute_batch("DROP TRIGGER no_output_rewrite")?;
    event.activity.as_mut().unwrap().output_delta = true;
    event.activity.as_mut().unwrap().output = Some("追加🙂".into());
    exchange.observe_turn(&event, 3)?;
    store.connection.execute_batch("CREATE TRIGGER reject_summary BEFORE UPDATE ON tool_output_summary BEGIN SELECT RAISE(ABORT,'failed commit'); END;")?;
    assert!(store.save_tool_output_delta(&exchange, &event).is_err());
    assert_eq!(
        store
            .tool_output_page("output", &turn, "call", 0, 1)?
            .total_chars,
        source.chars().count()
    );
    store
        .connection
        .execute_batch("DROP TRIGGER reject_summary")?;
    store.save_tool_output_delta(&exchange, &event)?;
    let expected = format!("{source}追加🙂");
    assert_eq!(
        store
            .tool_output_page("output", &turn, "call", source.chars().count(), 10)?
            .output,
        "追加🙂"
    );
    assert_eq!(
        store
            .tool_output_bytes("output", &turn, "call", (source.len() - 7) as u64, 30)?
            .0,
        expected.as_bytes()[source.len() - 7..]
    );
    let offset: i64 = store.connection.query_row(
        "SELECT max(byte_offset) FROM tool_output_chunk",
        [],
        |row| row.get(0),
    )?;
    store.connection.execute(
        "UPDATE tool_output_chunk SET output=CAST(x'80' AS TEXT) WHERE byte_offset=?1",
        [offset],
    )?;
    assert_eq!(
        store
            .tool_output_page("output", &turn, "call", 0, 2)?
            .output,
        expected.chars().take(2).collect::<String>()
    );
    assert!(
        store
            .tool_output_page("output", &turn, "call", source.chars().count(), 10)
            .is_err()
    );
    store.connection.execute(
        "UPDATE tool_output_chunk SET output='追加🙂' WHERE byte_offset=?1",
        [offset],
    )?;
    store.connection.execute_batch("DROP TABLE tool_output_position; DROP TABLE tool_output_summary; PRAGMA user_version=2; CREATE TRIGGER reject_migration BEFORE INSERT ON tool_output_chunk BEGIN SELECT RAISE(ABORT,'migration failure'); END;")?;
    assert!(SqliteStore::open(root.path()).is_err());
    assert_eq!(
        store
            .connection
            .pragma_query_value(None, "user_version", |row| row.get::<_, i64>(0))?,
        2
    );
    store
        .connection
        .execute_batch("DROP TRIGGER reject_migration")?;
    let reopened = SqliteStore::open(root.path())?;
    assert_eq!(
        reopened
            .tool_output_page("output", &turn, "call", 0, expected.chars().count() + 1)?
            .output,
        expected
    );
    assert_eq!(
        reopened
            .load_exchange("output")?
            .unwrap()
            .tools()
            .map(|(_, tool)| tool)
            .next()
            .unwrap()
            .output,
        expected
    );
    Ok(())
}

#[test]
fn selected_exchange_queries_ignore_unrelated_malformed_history_and_use_indexes() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut store = SqliteStore::open(root.path())?;
    store.save_session(&test::session("session", "workspace"))?;
    let (mut exchange, _) = output_exchange("retained")?;
    store.save_exchange(&exchange)?;
    exchange.id = "latest".into();
    exchange.ordinal = 2;
    exchange.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Finalizing,
        exchange.completed_at_ms(),
        exchange.finalization_outcome(),
    );
    exchange.execution_id = Some("execution".into());
    store.save_exchange(&exchange)?;
    store.connection.execute(
        "INSERT INTO agent_run_record VALUES('child','session','{}')",
        [],
    )?;
    let mut child = exchange.clone();
    child.id = "child-exchange".into();
    child.agent_id = "child".into();
    child.ordinal = 100;
    store.save_exchange(&child)?;
    assert_eq!(
        store
            .latest_execution_exchange("session", "execution")?
            .unwrap()
            .id,
        "latest"
    );
    store.connection.execute("UPDATE exchange_record SET payload=json_set(payload,'$.turn','malformed history') WHERE id='output'",[])?;
    assert!(store.list_exchange("session").is_err());
    assert_eq!(store.latest_exchange("session")?.unwrap().id, "latest");
    assert_eq!(
        store
            .exchange_in_state("session", ExchangeState::Finalizing)?
            .unwrap()
            .id,
        "latest"
    );
    assert!(
        store
            .exchange_in_state("session", ExchangeState::Complete)?
            .is_none()
    );
    for (sql, index) in [
        (
            "SELECT payload FROM exchange_record WHERE session_id='session' AND agent_id='agent' ORDER BY ordinal",
            "exchange_session_agent_order",
        ),
        (
            "SELECT payload FROM exchange_record WHERE agent_id='agent' ORDER BY ordinal",
            "exchange_agent_order",
        ),
        (
            "SELECT payload FROM exchange_record WHERE session_id='session' AND agent_id='agent' AND json_extract(payload,'$.state')='finalizing' ORDER BY ordinal LIMIT 1",
            "exchange_state_order",
        ),
    ] {
        let mut statement = store
            .connection
            .prepare(&format!("EXPLAIN QUERY PLAN {sql}"))?;
        let plan = statement
            .query_map([], |row| row.get::<_, String>(3))?
            .collect::<rusqlite::Result<Vec<_>>>()?
            .join("\n");
        assert!(plan.contains(index), "{plan}");
        assert!(!plan.contains("TEMP B-TREE"), "{plan}");
    }
    Ok(())
}

#[test]
fn runtime_projection_tracks_commits_without_database_reads() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut owner = SqliteStore::open(root.path())?;
    let mut session = test::session("session", "workspace");
    owner.save_session(&session)?;
    let projection = owner.session_admission(&session.id)?;
    let mut writer = SqliteStore::open(root.path())?;
    let mut task = crate::task::TaskRecord::new(
        session.id.clone(),
        "original".into(),
        crate::task::TaskKind::Plan,
        session.access.clone(),
        session.approval_mode,
        1,
    );
    writer.save_task(&task)?;
    session.current_task_id = Some(task.id.clone());
    writer.save_session(&session)?;
    assert_eq!(
        projection.runtime()?.task.as_ref().unwrap().title,
        "original"
    );
    writer.connection.execute_batch("CREATE TRIGGER fail_task BEFORE UPDATE ON task_record BEGIN SELECT RAISE(ABORT,'failed task write'); END;")?;
    task.title = "committed".into();
    assert!(writer.save_task(&task).is_err());
    assert_eq!(
        projection.runtime()?.task.as_ref().unwrap().title,
        "original"
    );
    writer.connection.execute_batch("DROP TRIGGER fail_task")?;
    writer.save_task(&task)?;
    assert_eq!(
        projection.runtime()?.task.as_ref().unwrap().title,
        "committed"
    );
    let mut operation: crate::task::TaskOperation = serde_json::from_value(serde_json::json!({
        "id":"configure","session_id":"session","action":{"action":"configure"},"state":"admitted",
        "permission_selection":null,"error":null,"created_at_ms":1
    }))?;
    writer.admit_task_operation(&operation)?;
    assert!(projection.runtime()?.configuring);
    operation.state = "completed".into();
    writer.save_task_operation(&mut operation)?;
    assert!(!projection.runtime()?.configuring);
    let plan: crate::plan::PlanRecord = serde_json::from_value(serde_json::json!({
        "id":"plan","session_id":"session","request":"plan","state":"awaiting_review",
        "working_path":"","model_revision":1,"user_revision":0,"created_at_ms":0,"updated_at_ms":0
    }))?;
    writer.save_plan(&plan)?;
    task.plan_id = Some(plan.id.clone());
    writer.save_task(&task)?;
    assert_eq!(
        projection.runtime()?.plan.as_ref().unwrap().state,
        crate::plan::PlanState::AwaitingReview
    );
    let mut execution: crate::plan::PlanExecutionRecord = serde_json::from_value(
        serde_json::json!({
            "id":"execution","session_id":"session","plan_id":"plan","goal_id":"goal",
            "state":"active","phase":"implement","original_revision":1,"revision":1,"generation":1,
            "lifecycle":[],"findings":[],"progress":crate::plan::execution::SemanticProgress::default(),
            "verification":[],"revision_history":[],"review_paused":false,"observed_source":{},
            "observed_generation":0,"observed_check":[],"created_at_ms":0
        }),
    )?;
    writer.save_plan_execution(&execution)?;
    assert_eq!(
        projection.runtime()?.execution.as_ref().unwrap().phase,
        crate::plan::PlanPhase::Implement
    );
    let (exchange, _) = output_exchange("")?;
    writer.save_exchange(&exchange)?;
    let goal = crate::goal::GoalRecord {
        id: "goal".into(),
        session_id: "session".into(),
        objective: "run".into(),
        state: crate::goal::GoalState::Active,
        continuation: Default::default(),
        native: false,
        created_at_ms: 0,
        updated_at_ms: 0,
    };
    execution.phase = crate::plan::PlanPhase::Verify;
    writer.save_execution_transition(&execution, &goal, None, Some(&exchange), None)?;
    assert_eq!(
        projection.runtime()?.execution.as_ref().unwrap().phase,
        crate::plan::PlanPhase::Verify
    );
    owner.connection.execute_batch("ALTER TABLE task_record RENAME TO hidden_task; ALTER TABLE task_operation RENAME TO hidden_operation;")?;
    assert_eq!(
        projection.runtime()?.task.as_ref().unwrap().title,
        "committed"
    );
    assert!(!projection.runtime()?.configuring);
    Ok(())
}

#[test]
fn legacy_empty_output_can_append_after_migration() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut store = SqliteStore::open(root.path())?;
    store.save_session(&test::session("session", "workspace"))?;
    let (mut exchange, mut event) = output_exchange("")?;
    store.save_exchange(&exchange)?;
    store.connection.execute_batch("DELETE FROM tool_output_owner; UPDATE exchange_record SET payload=json_remove(payload,'$.tool_output_format'); PRAGMA user_version=0;")?;
    drop(store);
    let mut store = SqliteStore::open(root.path())?;
    event.activity.as_mut().unwrap().output_delta = true;
    event.activity.as_mut().unwrap().output = Some("first λ".into());
    exchange.observe_turn(&event, 3)?;
    store.save_tool_output_delta(&exchange, &event)?;
    assert_eq!(
        store
            .tool_output_page("output", exchange.turn[0].id(), "call", 0, 100)?
            .output,
        "first λ"
    );
    Ok(())
}
