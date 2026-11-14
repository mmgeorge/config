use super::{SqliteStore, test::session};
use crate::session::SessionStore;
use anyhow::Result;

/// A matching version cannot certify altered table constraints or a missing required index.
#[test]
fn matching_version_rejects_changed_structure() -> Result<()> {
    for alteration in [
        "DROP INDEX session_workspace_activity",
        "DROP TABLE setting_application; CREATE TABLE setting_application(session_id TEXT,payload TEXT)",
    ] {
        let root = tempfile::tempdir()?;
        let connection = super::database::HistoryConnection::open(root.path())?;
        connection.execute_batch(alteration)?;
        drop(connection);
        for result in [
            super::database::HistoryConnection::open(root.path()),
            super::database::HistoryConnection::read_only(root.path()),
        ] {
            let error = result.err().expect("changed schema must fail");
            assert!(format!("{error:#}").contains("incompatible"));
        }
    }
    Ok(())
}

/// Both readers reject an older or newer version without modifying its records.
#[test]
fn mismatched_versions_reject_both_open_modes() -> Result<()> {
    for version in [super::state::VERSION - 1, super::state::VERSION + 1] {
        let root = tempfile::tempdir()?;
        let connection = super::database::HistoryConnection::open(root.path())?;
        connection.pragma_update(None, "user_version", version)?;
        drop(connection);
        assert!(super::database::HistoryConnection::open(root.path()).is_err());
        assert!(super::database::HistoryConnection::read_only(root.path()).is_err());
        assert_eq!(
            super::test::raw_connection(root.path())?.pragma_query_value(
                None,
                "user_version",
                |row| row.get::<_, u32>(0)
            )?,
            version
        );
    }
    Ok(())
}

/// A version marker cannot certify an incomplete schema or authorize repairing it in place.
#[test]
fn matching_version_missing_table_is_rejected_on_every_open() -> Result<()> {
    let root = tempfile::tempdir()?;
    let connection = super::database::HistoryConnection::open(root.path())?;
    connection.execute_batch("DROP TABLE terminal_request; DROP TABLE owner_cancellation; DROP TABLE owner_command_join; DROP TABLE session_question_state;")?;
    drop(connection);

    for result in [
        super::database::HistoryConnection::open(root.path()),
        super::database::HistoryConnection::read_only(root.path()),
    ] {
        let error = result.err().expect("incomplete current schema must fail");
        let message = format!("{error:#}");
        assert!(
            [
                "terminal_request",
                "owner_cancellation",
                "owner_command_join",
                "session_question_state"
            ]
            .iter()
            .all(|table| message.contains(table)),
            "{message}"
        );
    }
    let connection = super::test::raw_connection(root.path())?;
    assert_eq!(
        connection.pragma_query_value(None, "user_version", |row| row.get::<_, i64>(0))?,
        super::schema::VERSION,
    );
    let count: i64 = connection.query_row(
        "SELECT COUNT(*) FROM sqlite_master WHERE name='terminal_request'",
        [],
        |row| row.get(0),
    )?;
    assert_eq!(
        count, 0,
        "opening must not repair incompatible disposable state"
    );
    Ok(())
}

#[test]
fn newer_databases_are_preserved_and_obsolete_history_is_deleted() -> Result<()> {
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
    drop(store);
    std::fs::rename(
        super::state::directory(root.path()),
        root.path().join("state/v0"),
    )?;
    let migrated = SqliteStore::open(root.path())?;
    assert!(migrated.load_exchange("old")?.is_none());
    let mut store = migrated;
    store.save_session(&session("current", "workspace"))?;
    store
        .connection
        .pragma_update(None, "user_version", super::schema::VERSION + 1)?;
    let error = SqliteStore::open(root.path())
        .err()
        .expect("newer schema must fail");
    assert!(format!("{error:#}").contains("newer than supported"));
    assert!(store.load_session("current")?.is_some());
    Ok(())
}
