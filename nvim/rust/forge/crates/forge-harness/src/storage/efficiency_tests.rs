use super::*;
use crate::{
    exchange::{Exchange, ExchangeState},
    provider::{ProviderEvent, TextUpdate, ToolActivityKind, ToolInvocation, ToolUpdate},
};

fn output_exchange(text: &str) -> Result<(Exchange, ProviderEvent)> {
    let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
        "id":"output","session_id":"session","agent_id":"session:agent:primary","ordinal":1,
        "prompt":"run","kind":"chat","state":"running","turn":[],"node_list":[],"created_at_ms":1,
        "attributed_matches_checkpoint":false
    }))?;
    let address = crate::turn::test_support::address(&exchange, "turn");
    exchange.start_turn(address.clone(), 1)?;
    crate::turn::test_support::tool(
        &mut exchange,
        &address,
        "call",
        ToolInvocation {
            semantics: Default::default(),
            kind: ToolActivityKind::Command,
            name: "read".into(),
            title: "read".into(),
            arguments: serde_json::json!({}),
        },
        [ToolUpdate::Started],
        2,
    )?;
    let event = ProviderEvent::Tool {
        address: crate::provider::ToolAddress {
            turn: address,
            tool_id: "call".into(),
        },
        update: ToolUpdate::Output {
            update: TextUpdate::Replace(text.into()),
        },
    };
    exchange.observe_turn(&event, 2)?;
    Ok((exchange, event))
}

#[test]
fn output_saves_and_pages_preserve_content_without_history_reads() -> Result<()> {
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
    if let ProviderEvent::Tool { update, .. } = &mut event {
        *update = ToolUpdate::Output {
            update: TextUpdate::Append("追加🙂".into()),
        };
    }
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
    drop(store);
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
    assert_eq!(store.latest_exchange("session")?.unwrap().id, "latest");
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
    let session = test::session("session", "workspace");
    owner.save_session(&session)?;
    let projection = owner.session_admission(&session.id)?;
    let mut writer = SqliteStore::open(root.path())?;
    let task = crate::task::Task::goal("original".into(), Default::default(), 1);
    writer.commit_task_change(&session.id, None, 1, |active| {
        *active = Some(task.clone());
        Ok(())
    })?;
    assert_eq!(
        projection.runtime()?.active_task.as_ref().unwrap().title(),
        "original"
    );
    writer.connection.execute_batch("CREATE TRIGGER fail_task BEFORE UPDATE ON session_record BEGIN SELECT RAISE(ABORT,'failed task write'); END;")?;
    let mut replacement = crate::task::Task::goal("committed".into(), Default::default(), 2);
    assert!(
        writer
            .commit_task_change(&session.id, Some(&task.revision), 2, |active| {
                *active = Some(replacement.clone());
                Ok(())
            })
            .is_err()
    );
    assert_eq!(
        projection.runtime()?.active_task.as_ref().unwrap().title(),
        "original"
    );
    writer.connection.execute_batch("DROP TRIGGER fail_task")?;
    writer.commit_task_change(&session.id, Some(&task.revision), 2, |active| {
        *active = Some(replacement.clone());
        Ok(())
    })?;
    assert_eq!(
        projection.runtime()?.active_task.as_ref().unwrap().title(),
        "committed"
    );
    let plan: crate::plan::PlanRecord = serde_json::from_value(serde_json::json!({
        "id":"plan","session_id":"session","request":"plan","state":"awaiting_review",
        "working_path":"","model_revision":1,"submitted_version":1,"review_digest":"submitted","user_revision":0,"created_at_ms":0,"updated_at_ms":0
    }))?;
    let plan_task = crate::task::Task::plan(plan.clone(), 3)?;
    writer.commit_task_change(&session.id, Some(&replacement.revision), 3, |active| {
        *active = Some(plan_task.clone());
        Ok(())
    })?;
    replacement = plan_task;
    assert_eq!(
        projection.runtime()?.plan.as_ref().unwrap().state,
        crate::plan::PlanState::AwaitingReview
    );
    let mut execution: crate::plan::PlanExecutionRecord = serde_json::from_value(
        serde_json::json!({
            "id":"execution","session_id":"session","plan_id":"plan",
            "state":"active","phase":"implement","baseline_checkpoint":"captured","original_revision":1,"revision":1,"generation":1,
            "lifecycle":[],"findings":[],"progress":crate::plan::execution::SemanticProgress::default(),
            "verification":[],"revision_history":[],"resume_after_review":false,"observed_source":{},
            "observed_document_version":0,"observed_check":[],"created_at_ms":0
        }),
    )?;
    let execute_task =
        crate::task::Task::execute("committed".into(), execution.clone(), Default::default(), 4)?;
    writer.commit_task_change(&session.id, Some(&replacement.revision), 4, |active| {
        *active = Some(execute_task.clone());
        Ok(())
    })?;
    assert_eq!(
        match &projection.runtime()?.active_task.as_ref().unwrap().workflow {
            crate::task::WorkflowSnapshot::Execute { execution } => execution.record.phase,
            _ => panic!("expected execution"),
        },
        crate::plan::PlanPhase::Implement
    );
    let (exchange, _) = output_exchange("")?;
    writer.save_exchange(&exchange)?;
    execution.phase = crate::plan::PlanPhase::Checking;
    writer.commit_task_change(&session.id, Some(&execute_task.revision), 5, |active| {
        active.as_mut().unwrap().edit(|draft| {
            let crate::task::WorkflowSnapshot::Execute { execution: current } = &mut draft.workflow
            else {
                unreachable!()
            };
            current.record = execution.clone();
            Ok(())
        })
    })?;
    assert_eq!(
        match &projection.runtime()?.active_task.as_ref().unwrap().workflow {
            crate::task::WorkflowSnapshot::Execute { execution } => execution.record.phase,
            _ => panic!("expected execution"),
        },
        crate::plan::PlanPhase::Checking
    );
    owner
        .connection
        .execute_batch("ALTER TABLE task_record RENAME TO hidden_task;")?;
    assert_eq!(
        projection.runtime()?.active_task.as_ref().unwrap().title(),
        "committed"
    );
    Ok(())
}

#[test]
fn empty_output_can_append_after_reopening() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut store = SqliteStore::open(root.path())?;
    store.save_session(&test::session("session", "workspace"))?;
    let (mut exchange, mut event) = output_exchange("")?;
    store.save_exchange(&exchange)?;
    drop(store);
    let mut store = SqliteStore::open(root.path())?;
    if let ProviderEvent::Tool { update, .. } = &mut event {
        *update = ToolUpdate::Output {
            update: TextUpdate::Append("first λ".into()),
        };
    }
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
