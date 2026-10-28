use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, TransactionBehavior};

pub(super) const VERSION: i64 = 3;
const APPLICATION: i64 = 0x464f5247;

/// Serializes bootstrap pragmas across processes before SQLite can acquire its own write lock.
pub(crate) fn bootstrap_lock(root: &std::path::Path, name: &str) -> Result<std::fs::File> {
    let path = root.join(format!("{name}-schema.lock"));
    let lock = std::fs::OpenOptions::new()
        .create(true)
        .truncate(false)
        .read(true)
        .write(true)
        .open(&path)?;
    let started = std::time::Instant::now();
    loop {
        match lock.try_lock() {
            Ok(()) => return Ok(lock),
            Err(std::fs::TryLockError::WouldBlock)
                if started.elapsed() < std::time::Duration::from_secs(5) =>
            {
                std::thread::sleep(std::time::Duration::from_millis(10));
            }
            Err(std::fs::TryLockError::WouldBlock) => anyhow::bail!(
                "database schema initialization lock {} remained busy after 5 seconds",
                path.display()
            ),
            Err(error) => return Err(error).context("acquire database schema initialization lock"),
        }
    }
}

/// Rejects foreign and newer database formats before any data or schema is changed.
pub(super) fn validate(connection: &Connection) -> Result<i64> {
    let application: i64 =
        connection.pragma_query_value(None, "application_id", |row| row.get(0))?;
    let version: i64 = connection.pragma_query_value(None, "user_version", |row| row.get(0))?;
    ensure!(
        application == 0 || application == APPLICATION,
        "database belongs to another application"
    );
    ensure!(
        version <= VERSION,
        "Harness database schema {version} is newer than supported schema {VERSION}; update Harness"
    );
    Ok(version)
}

/// Upgrades recognized legacy databases once and commits their version with the converted records.
pub(super) fn initialize(
    connection: &mut Connection,
    objects: &super::objects::ObjectStore,
    root: &std::path::Path,
) -> Result<()> {
    if validate(connection)? == VERSION {
        return Ok(());
    }
    let _bootstrap = bootstrap_lock(root, "harness")?;
    if validate(connection)? == VERSION {
        return Ok(());
    }
    connection.pragma_update(None, "journal_mode", "WAL")?;
    let transaction = connection
        .transaction_with_behavior(TransactionBehavior::Immediate)
        .context("acquire Harness schema migration write lock (SQLite wait limit: 5 seconds)")?;
    if validate(&transaction)? == VERSION {
        return Ok(());
    }
    let previous = validate(&transaction)?;
    if previous > 0 {
        if previous == 1 {
            initialize_commands(&transaction)?;
        }
        initialize_query_indexes(&transaction)?;
        super::tool_output::initialize_positions(&transaction)?;
        transaction.pragma_update(None, "user_version", VERSION)?;
        transaction.commit()?;
        return Ok(());
    }
    let foreign: bool = transaction.query_row(
        "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%') AND NOT EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name IN ('session_record','preference_record','model_pin'))",
        [], |row| row.get(0),
    )?;
    ensure!(
        !foreign,
        "unrecognized unversioned Harness database; existing data was preserved"
    );
    transaction.execute_batch(
        r#"
            CREATE TABLE IF NOT EXISTS preference_record (
                workspace TEXT NOT NULL,
                backend TEXT NOT NULL,
                payload TEXT NOT NULL,
                PRIMARY KEY(workspace, backend)
            );
            "#,
    )?;
    transaction.execute_batch(
        r#"
            CREATE TABLE IF NOT EXISTS session_record (
                id TEXT PRIMARY KEY,
                workspace TEXT NOT NULL,
                updated_at_ms INTEGER NOT NULL,
                payload TEXT NOT NULL
            );
            CREATE TABLE IF NOT EXISTS task_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS task_operation (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE INDEX IF NOT EXISTS session_workspace_activity
                ON session_record(workspace, updated_at_ms DESC);
            CREATE TABLE IF NOT EXISTS exchange_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                agent_id TEXT,
                ordinal INTEGER NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE,
                FOREIGN KEY(agent_id) REFERENCES agent_run_record(id) ON DELETE CASCADE
            );
            CREATE UNIQUE INDEX IF NOT EXISTS exchange_session_ordinal
                ON exchange_record(session_id, COALESCE(agent_id, ''), ordinal);
            CREATE TABLE IF NOT EXISTS exchange_comment (
                id TEXT PRIMARY KEY,
                interaction_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(interaction_id) REFERENCES exchange_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS plan_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS plan_lifecycle_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS plan_execution_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS plan_deviation_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS plan_audit_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS plan_resolution_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS goal_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS checkpoint_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS prompt_history_record (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                text TEXT NOT NULL,
                created_at_ms INTEGER NOT NULL
            );
            CREATE TABLE IF NOT EXISTS prompt_attachment_record (
                history_id INTEGER PRIMARY KEY,
                payload TEXT NOT NULL,
                FOREIGN KEY(history_id) REFERENCES prompt_history_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS agent_run_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS agent_task_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                agent_id TEXT NOT NULL,
                operation_id TEXT NOT NULL,
                ordinal INTEGER NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE,
                FOREIGN KEY(agent_id) REFERENCES agent_run_record(id) ON DELETE CASCADE,
                FOREIGN KEY(id) REFERENCES exchange_record(id) ON DELETE CASCADE,
                UNIQUE(session_id, operation_id),
                UNIQUE(agent_id, ordinal)
            );

            CREATE TABLE IF NOT EXISTS session_event_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE TABLE IF NOT EXISTS fork_history_record (
                session_id TEXT NOT NULL,
                ordinal INTEGER NOT NULL,
                payload TEXT NOT NULL,
                PRIMARY KEY(session_id, ordinal),
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            "#,
    )?;

    transaction.execute_batch(
        "CREATE TABLE IF NOT EXISTS model_pin (backend TEXT NOT NULL, model TEXT NOT NULL, PRIMARY KEY(backend,model));
         CREATE TABLE IF NOT EXISTS session_deletion (session_id TEXT PRIMARY KEY REFERENCES session_record(id) ON DELETE CASCADE);
         CREATE INDEX IF NOT EXISTS task_session ON task_record(session_id);
         CREATE INDEX IF NOT EXISTS task_operation_session ON task_operation(session_id);
         CREATE INDEX IF NOT EXISTS comment_exchange ON exchange_comment(interaction_id);
         CREATE INDEX IF NOT EXISTS plan_session ON plan_record(session_id);
         CREATE INDEX IF NOT EXISTS lifecycle_session ON plan_lifecycle_record(session_id);
         CREATE INDEX IF NOT EXISTS execution_session ON plan_execution_record(session_id);
         CREATE INDEX IF NOT EXISTS deviation_session ON plan_deviation_record(session_id);
         CREATE INDEX IF NOT EXISTS audit_session ON plan_audit_record(session_id);
         CREATE INDEX IF NOT EXISTS resolution_session ON plan_resolution_record(session_id);
         CREATE INDEX IF NOT EXISTS goal_session ON goal_record(session_id);
         CREATE INDEX IF NOT EXISTS checkpoint_session ON checkpoint_record(session_id);
         CREATE INDEX IF NOT EXISTS agent_session ON agent_run_record(session_id);
         CREATE INDEX IF NOT EXISTS agent_task_session ON agent_task_record(session_id);
         CREATE INDEX IF NOT EXISTS event_session ON session_event_record(session_id);"
    )?;
    super::tool_output::initialize(&transaction)?;
    initialize_query_indexes(&transaction)?;
    super::archive::migrate(&transaction, objects)?;
    initialize_commands(&transaction)?;
    transaction.pragma_update(None, "application_id", APPLICATION)?;
    transaction.pragma_update(None, "user_version", VERSION)?;
    transaction.commit()?;
    Ok(())
}

fn initialize_query_indexes(connection: &Connection) -> Result<()> {
    connection.execute_batch(
        "CREATE INDEX IF NOT EXISTS exchange_session_agent_order ON exchange_record(session_id,agent_id,ordinal);
         CREATE INDEX IF NOT EXISTS exchange_agent_order ON exchange_record(agent_id,ordinal);
         CREATE INDEX IF NOT EXISTS exchange_state_order ON exchange_record(session_id,agent_id,json_extract(payload,'$.state'),ordinal);
         CREATE INDEX IF NOT EXISTS execution_plan_order ON plan_execution_record(session_id,json_extract(payload,'$.plan_id'));
         CREATE INDEX IF NOT EXISTS exchange_execution_order ON exchange_record(session_id,agent_id,json_extract(payload,'$.execution_id'),ordinal);
         CREATE INDEX IF NOT EXISTS task_session_updated ON task_record(session_id,json_extract(payload,'$.updated_at_ms'));")?;
    Ok(())
}

fn initialize_commands(connection: &Connection) -> Result<()> {
    connection.execute_batch(
        "CREATE TABLE IF NOT EXISTS command_receipt (
        session_id TEXT NOT NULL REFERENCES session_record(id) ON DELETE CASCADE,
        id TEXT NOT NULL,
        request TEXT NOT NULL,
        result TEXT NOT NULL,
        PRIMARY KEY(session_id,id)
    );",
    )?;
    Ok(())
}
