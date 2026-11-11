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
    #[expect(
        clippy::disallowed_methods,
        reason = "preferences live in their own database outside versioned state"
    )]
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
///
/// A record whose payload no longer decodes into the current preference schema, or whose
/// format version is outside the supported range, stays behind in the read-only source.
/// Obsolete private formats are hidden rather than upgraded, so one stale record must not
/// prevent the remaining compatible settings from importing or the store from opening.
/// SQLite failures still abort the import, leaving the destination schema unversioned.
fn import(destination: &Connection, path: &Path) -> Result<()> {
    #[expect(
        clippy::disallowed_methods,
        reason = "this reads a legacy preference database once, to import it"
    )]
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
            let Ok(previous) = serde_json::from_str::<PreferenceImport>(&row.get::<_, String>(2)?)
            else {
                continue;
            };
            if previous.format_version == 0
                || previous.format_version > super::PREFERENCE_FORMAT_VERSION
            {
                continue;
            }
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
