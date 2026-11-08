use super::{SqliteStore, test::session};
use crate::session::SessionStore;
use anyhow::Result;

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
