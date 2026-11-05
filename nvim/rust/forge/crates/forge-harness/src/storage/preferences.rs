use std::path::Path;

use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, OpenFlags};

/// Decode compatible setting fields without interpreting historical session payloads.
#[derive(serde::Deserialize)]
struct PreferenceImport {
    format_version: u32,
    preference: crate::session::HarnessPreference,
}

/// Open durable workspace preferences and model pins independently of session state.
pub(super) fn open(root: &Path) -> Result<Connection> {
    std::fs::create_dir_all(root)?;
    let _bootstrap = super::bootstrap_lock(root, "preferences")?;
    let mut connection = Connection::open(root.join("preferences.sqlite3"))?;
    connection.busy_timeout(std::time::Duration::from_secs(5))?;
    connection.pragma_update(None, "synchronous", "FULL")?;
    let version: i64 = connection.pragma_query_value(None, "user_version", |row| row.get(0))?;
    ensure!(
        (0..=1).contains(&version),
        "unsupported preferences schema {version}"
    );
    if version == 0 {
        let transaction = connection.transaction()?;
        transaction.execute_batch(
            "CREATE TABLE preference_record(workspace TEXT NOT NULL,backend TEXT NOT NULL,payload TEXT NOT NULL,PRIMARY KEY(workspace,backend));
             CREATE TABLE model_pin(backend TEXT NOT NULL,model TEXT NOT NULL,PRIMARY KEY(backend,model));
             CREATE TABLE preference_operation(id TEXT PRIMARY KEY);"
        )?;
        let previous = root.join("harness.sqlite3");
        if previous.try_exists()? {
            import(&transaction, &previous)
                .context("import persistent settings from unversioned history")?;
        }
        transaction.pragma_update(None, "user_version", 1)?;
        transaction.commit()?;
    }
    Ok(connection)
}

/// Attach preferences for independent reads and writes outside session transactions.
pub(super) fn attach(connection: &Connection, root: &Path) -> Result<()> {
    drop(open(root)?);
    connection.execute(
        "ATTACH DATABASE ?1 AS preferences",
        [root.join("preferences.sqlite3").to_string_lossy().as_ref()],
    )?;
    Ok(())
}

/// Apply a recovered preference once, including a receipt in the same persistent transaction.
pub(super) fn apply_configuration(
    root: &Path,
    intent: &super::configuration::ConfigurationIntent,
    preference: &crate::session::HarnessPreference,
) -> Result<()> {
    let mut connection = open(root)?;
    let transaction =
        connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
    if transaction.execute(
        "INSERT OR IGNORE INTO preference_operation(id) VALUES(?1)",
        [&intent.id],
    )? != 0
    {
        let payload = serde_json::to_string(&serde_json::json!({
            "format_version": super::PREFERENCE_FORMAT_VERSION, "preference": preference,
        }))?;
        transaction.execute(
            "INSERT INTO preference_record(workspace,backend,payload) VALUES(?1,?2,?3)
             ON CONFLICT(workspace,backend) DO UPDATE SET payload=excluded.payload",
            rusqlite::params![intent.workspace, intent.provider, payload],
        )?;
    }
    transaction.commit()?;
    Ok(())
}

/// Copy compatible settings once without accessing session tables or modifying the source.
fn import(destination: &Connection, path: &Path) -> Result<()> {
    let source = Connection::open_with_flags(path, OpenFlags::SQLITE_OPEN_READ_ONLY)?;
    source.busy_timeout(std::time::Duration::from_secs(5))?;
    source.execute_batch("BEGIN")?;
    if table_exists(&source, "preference_record")? {
        let mut query =
            source.prepare("SELECT workspace,backend,payload FROM preference_record")?;
        let mut rows = query.query([])?;
        while let Some(row) = rows.next()? {
            let workspace: String = row.get(0)?;
            let provider: String = row.get(1)?;
            let previous: PreferenceImport = serde_json::from_str(&row.get::<_, String>(2)?)
                .with_context(|| format!("decode {provider} preferences for {workspace}"))?;
            ensure!(
                previous.format_version > 0
                    && previous.format_version <= super::PREFERENCE_FORMAT_VERSION,
                "unsupported imported preference format {}",
                previous.format_version
            );
            let payload = serde_json::to_string(&serde_json::json!({
                "format_version": super::PREFERENCE_FORMAT_VERSION,
                "preference": previous.preference,
            }))?;
            destination.execute(
                "INSERT INTO preference_record VALUES(?1,?2,?3)",
                rusqlite::params![workspace, provider, payload],
            )?;
        }
    }
    if table_exists(&source, "model_pin")? {
        let mut query = source.prepare("SELECT backend,model FROM model_pin")?;
        let mut rows = query.query([])?;
        while let Some(row) = rows.next()? {
            destination.execute(
                "INSERT INTO model_pin VALUES(?1,?2)",
                rusqlite::params![row.get::<_, String>(0)?, row.get::<_, String>(1)?],
            )?;
        }
    }
    source.execute_batch("COMMIT")?;
    Ok(())
}

/// Detect optional legacy settings tables without touching obsolete session schemas.
fn table_exists(connection: &Connection, name: &str) -> rusqlite::Result<bool> {
    connection.query_row(
        "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name=?1)",
        [name],
        |row| row.get(0),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Import settings once, then preserve edits and pins across disposable-state deletion.
    #[test]
    fn preferences_import_once_and_survive_state_clear() -> Result<()> {
        let root = tempfile::tempdir()?;
        let old = Connection::open(root.path().join("harness.sqlite3"))?;
        old.execute_batch(
            "CREATE TABLE preference_record(workspace TEXT,backend TEXT,payload TEXT);
            CREATE TABLE model_pin(backend TEXT,model TEXT);
            CREATE TABLE session_record(payload TEXT);
            INSERT INTO session_record VALUES('unreadable obsolete session');
            INSERT INTO model_pin VALUES('mock','chosen-model');",
        )?;
        old.execute("INSERT INTO preference_record VALUES('workspace','mock',?1)", [
            serde_json::json!({"format_version":43,"preference":{"model":"chosen-model","effort":"low"}}).to_string()
        ])?;
        let mut store = crate::storage::SqliteStore::open(root.path())?;
        let mut preference = store.load_preference("workspace", "mock")?.unwrap();
        assert_eq!(preference.model, "chosen-model");
        preference.model = "edited-model".into();
        store.save_preference("workspace", "mock", &preference)?;
        drop(store);
        assert!(crate::storage::state::clear(root.path())?.current_cleared);
        let reopened = crate::storage::SqliteStore::open(root.path())?;
        assert_eq!(
            reopened
                .load_preference("workspace", "mock")?
                .unwrap()
                .model,
            "edited-model"
        );
        assert_eq!(
            crate::storage::model_pin::ModelPinStore::open(root.path())?.load("mock")?,
            ["chosen-model"]
        );
        Ok(())
    }

    /// Roll back failed imports without marking unsupported settings as migrated.
    #[test]
    fn preferences_failed_import_remains_retryable() -> Result<()> {
        let root = tempfile::tempdir()?;
        let old = Connection::open(root.path().join("harness.sqlite3"))?;
        old.execute_batch(
            "CREATE TABLE preference_record(workspace TEXT,backend TEXT,payload TEXT);
            INSERT INTO preference_record VALUES('workspace','mock','{}');",
        )?;
        assert!(open(root.path()).is_err());
        old.execute("UPDATE preference_record SET payload=?1", [
            serde_json::json!({"format_version":44,"preference":{"model":"repaired","effort":"low"}}).to_string()
        ])?;
        assert!(open(root.path()).is_ok());
        Ok(())
    }
}
