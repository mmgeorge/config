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

#[test]
fn partial_deletion_recovers_without_blocking_unrelated_writes() -> Result<()> {
    let root = tempfile::tempdir()?;
    let mut store = SqliteStore::open(root.path())?;
    store.save_session(&session("deleted", "workspace"))?;
    store.save_session(&session("survivor", "workspace"))?;
    let plans = crate::storage::state::directory(root.path()).join("plans/deleted");
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
