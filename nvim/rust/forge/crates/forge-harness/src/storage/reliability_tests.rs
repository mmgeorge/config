use super::{SqliteStore, test::session};
use crate::session::SessionStore;
use anyhow::Result;
use rusqlite::TransactionBehavior;

#[test]
fn current_schema_reads_do_not_wait_for_another_writer() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut owner = SqliteStore::open(root.path())?;
    owner.save_session(&session("session", "workspace"))?;
    let write = owner
        .connection
        .transaction_with_behavior(TransactionBehavior::Immediate)?;
    write.execute("UPDATE session_record SET updated_at_ms=2", [])?;
    let reader = SqliteStore::open(root.path())?;
    assert_eq!(reader.load_session("session")?.unwrap().updated_at_ms, 1);
    let observer = SqliteStore::read_only(root.path())?;
    assert_eq!(observer.load_session("session")?.unwrap().updated_at_ms, 1);
    assert_eq!(
        reader
            .connection
            .pragma_query_value(None, "user_version", |row| row.get::<_, i64>(0))?,
        super::schema::VERSION
    );
    Ok(())
}

#[test]
fn receipt_upgrade_does_not_rescan_existing_conversations() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut store = SqliteStore::open(root.path())?;
    store.save_session(&session("session", "workspace"))?;
    store.connection.execute_batch("DROP TABLE command_receipt; PRAGMA user_version=1; CREATE TRIGGER prevent_scan BEFORE UPDATE ON exchange_record BEGIN SELECT RAISE(ABORT,'unexpected conversation rewrite'); END;")?;
    let upgraded = SqliteStore::open(root.path())?;
    assert_eq!(
        upgraded
            .connection
            .pragma_query_value(None, "user_version", |row| row.get::<_, i64>(0))?,
        super::schema::VERSION
    );
    assert_eq!(
        upgraded
            .connection
            .query_row("SELECT count(*) FROM command_receipt", [], |row| row
                .get::<_, i64>(0))?,
        0
    );
    Ok(())
}

#[test]
fn schema_upgrade_rolls_back_and_newer_databases_are_preserved() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut store = SqliteStore::open(root.path())?;
    store.save_session(&session("session", "workspace"))?;
    let payload = r#"{"turn":[{"id":"turn","tool":{"item":{"call":{"output":"retained"}}}}]}"#;
    store.connection.execute(
        "INSERT INTO exchange_record(id,session_id,ordinal,payload) VALUES('old','session',1,?1)",
        [payload],
    )?;
    store.connection.execute_batch("PRAGMA user_version=0; CREATE TRIGGER refuse_conversion BEFORE UPDATE ON exchange_record BEGIN SELECT RAISE(ABORT,'conversion failed'); END;")?;
    assert!(SqliteStore::open(root.path()).is_err());
    assert_eq!(
        store
            .connection
            .pragma_query_value(None, "user_version", |row| row.get::<_, i64>(0))?,
        0
    );
    assert_eq!(
        store
            .connection
            .query_row("SELECT COUNT(*) FROM tool_output_chunk", [], |row| row
                .get::<_, i64>(0))?,
        0
    );
    store
        .connection
        .execute_batch("DROP TRIGGER refuse_conversion")?;
    let migrated = SqliteStore::open(root.path())?;
    assert_eq!(
        migrated.tool_output_bytes("old", "turn", "call", 0, 100)?.0,
        b"retained"
    );
    store
        .connection
        .pragma_update(None, "user_version", super::schema::VERSION + 1)?;
    let error = SqliteStore::open(root.path())
        .err()
        .expect("newer schema must fail");
    assert!(format!("{error:#}").contains("newer than supported"));
    assert!(
        store
            .connection
            .query_row(
                "SELECT payload FROM exchange_record WHERE id='old'",
                [],
                |row| row.get::<_, String>(0)
            )?
            .contains("tool_output_format")
    );
    Ok(())
}

#[test]
fn partial_deletion_recovers_without_blocking_unrelated_writes() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut store = SqliteStore::open(root.path())?;
    store.save_session(&session("deleted", "workspace"))?;
    store.save_session(&session("survivor", "workspace"))?;
    let plans = root.path().join("plans/deleted");
    std::fs::create_dir_all(&plans)?;
    std::fs::write(plans.join("first"), "first")?;
    std::fs::write(plans.join("second"), "second")?;
    let error = store
        .delete_session_with("deleted", || {
            std::fs::remove_file(plans.join("first"))?;
            let independent = SqliteStore::open(root.path())?;
            independent.connection.execute(
                "UPDATE session_record SET updated_at_ms=3 WHERE id='survivor'",
                [],
            )?;
            anyhow::bail!("interrupted cleanup")
        })
        .unwrap_err();
    assert!(format!("{error:#}").contains("deletion remains pending"));
    assert!(plans.join("second").exists());
    assert_eq!(store.list_session(None)?.len(), 1);
    assert!(store.acquire_session_lease("deleted", "other").is_err());
    drop(store);
    let mut reopened = SqliteStore::open(root.path())?;
    reopened.recover_session_deletions()?;
    assert!(!plans.exists());
    assert!(reopened.load_session("deleted")?.is_none());
    assert!(reopened.load_session("survivor")?.is_some());
    Ok(())
}

#[test]
fn sqlite_contention_produces_an_actionable_error() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut first = SqliteStore::open(root.path())?;
    let second = SqliteStore::open(root.path())?;
    second.connection.busy_timeout(std::time::Duration::ZERO)?;
    let _write = first
        .connection
        .transaction_with_behavior(TransactionBehavior::Immediate)?;
    let error: anyhow::Error = second
        .connection
        .execute(
            "INSERT INTO prompt_history_record(text,created_at_ms) VALUES('blocked',0)",
            [],
        )
        .unwrap_err()
        .into();
    let message = super::error_message(&error.context("save prompt history"));
    assert!(message.contains("SQLite contention"));
    assert!(message.contains("5 seconds"));
    assert!(message.contains("save prompt history"));
    assert_eq!(
        second
            .connection
            .query_row("SELECT COUNT(*) FROM prompt_history_record", [], |row| row
                .get::<_, i64>(
                0
            ))?,
        0
    );
    Ok(())
}

#[test]
fn stored_output_is_deferred_and_remains_complete_when_opened() -> Result<()> {
    use crate::backend::{TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome, ToolUpdate};
    let root = tempfile::tempdir()?;
    let mut store = SqliteStore::open(root.path())?;
    store.save_session(&session("session", "workspace"))?;
    let mut exchange: crate::exchange::Exchange = serde_json::from_value(serde_json::json!({
        "id":"exchange","session_id":"session","agent_id":"session:agent:primary","ordinal":1,
        "prompt":"run","kind":"chat","state":"running","turn":[],"node_list":[],"created_at_ms":1,
        "attributed_matches_checkpoint":false
    }))?;
    let address = crate::turn::test_support::address(&exchange, "turn");
    exchange.start_turn(address.clone(), 1)?;
    let source = "λ unicode output\n".repeat(100_000);
    crate::turn::test_support::tool(
        &mut exchange,
        &address,
        "call",
        ToolInvocation {
            kind: ToolActivityKind::Command,
            name: "read".into(),
            title: "read".into(),
            arguments: serde_json::json!({}),
        },
        [
            ToolUpdate::Started,
            ToolUpdate::Output {
                update: TextUpdate::Replace(source.clone()),
            },
            ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded,
            },
        ],
        2,
    )?;
    store.save_exchange(&exchange)?;
    let metadata = store.list_exchange_metadata("session")?;
    assert!(
        metadata[0]
            .tools_for_turn(metadata[0].turn[0].id())
            .next()
            .unwrap()
            .output
            .is_empty()
    );
    let turn = metadata[0].turn[0].id();
    let view = crate::buffer::tool::ToolOutputView::stored(
        crate::buffer::identity::DisplayId::test(),
        root.path(),
        "exchange",
        turn,
        "call",
    );
    let snapshot = view.complete_snapshot()?;
    assert_eq!(
        snapshot.retained_bytes(),
        0,
        "opening a deferred document must not load output"
    );
    let mut output = crate::buffer::output::OutputDocument::new(
        forge_buffer::identity::DocumentId("deferred-output".into()),
        snapshot,
    )?;
    assert!(output.snapshot().block.is_empty());
    loop {
        let delivery = output.demand(output.snapshot().revision)?;
        if !delivery.more {
            break;
        }
    }
    let path = output.export(
        root.path(),
        std::sync::Arc::new(crate::trace::TraceStore::open(root.path())?),
        "session",
    )?;
    assert_eq!(std::fs::read_to_string(path)?, source);
    let mut inherited = serde_json::to_value(&exchange)?;
    super::archive::encode(&mut inherited, &store.objects)?;
    let key = format!("{turn}:call");
    let object = inherited["archived_output"][&key]
        .as_str()
        .unwrap()
        .to_owned();
    store.delete_session("session")?;
    assert_eq!(store.objects.get(&object)?, source.as_bytes());
    Ok(())
}

#[test]
fn simultaneous_initialization_commits_one_compatible_schema() -> Result<()> {
    let root = tempfile::tempdir()?;
    let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
    let workers = (0..2)
        .map(|_| {
            let path = root.path().to_owned();
            let barrier = barrier.clone();
            std::thread::spawn(move || -> Result<()> {
                barrier.wait();
                let store = SqliteStore::open(&path)?;
                assert_eq!(
                    store
                        .connection
                        .pragma_query_value(None, "user_version", |row| row.get::<_, i64>(0))?,
                    super::schema::VERSION
                );
                Ok(())
            })
        })
        .collect::<Vec<_>>();
    for worker in workers {
        worker.join().expect("initialization worker panicked")?;
    }
    Ok(())
}
