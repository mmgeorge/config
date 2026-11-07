use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, TransactionBehavior};

pub(super) const VERSION: i64 = super::state::VERSION as i64;
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
    if application == 0 {
        let foreign: bool = connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%') AND NOT EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name='session_record')",
            [],
            |row| row.get(0),
        )?;
        ensure!(!foreign, "database belongs to another application");
    }
    Ok(version)
}

/// Create the exact schema for this executable's disposable-state version.
pub(super) fn initialize(connection: &mut Connection) -> Result<()> {
    connection.pragma_update(None, "journal_mode", "WAL")?;
    let transaction = connection
        .transaction_with_behavior(TransactionBehavior::Immediate)
        .context("acquire Harness schema initialization write lock")?;
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
            CREATE TABLE IF NOT EXISTS observed_binding_unavailable (
                binding_id TEXT PRIMARY KEY
            );
            CREATE TABLE IF NOT EXISTS observed_agent_record (
                binding_id TEXT NOT NULL,
                agent_id TEXT NOT NULL,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                PRIMARY KEY(binding_id,agent_id),
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );            CREATE TABLE IF NOT EXISTS agent_task_record (
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
        "CREATE TABLE IF NOT EXISTS session_deletion (session_id TEXT PRIMARY KEY REFERENCES session_record(id) ON DELETE CASCADE);
         CREATE INDEX IF NOT EXISTS task_session ON task_record(session_id);
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
    initialize_commands(&transaction)?;
    initialize_owner_commands(&transaction)?;
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
         DROP INDEX IF EXISTS exchange_execution_order;
         DROP INDEX IF EXISTS task_session_updated;")?;
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

/// Keep command outcomes and accepted input in one SQLite commit boundary.
fn initialize_owner_commands(connection: &Connection) -> Result<()> {
    connection.execute_batch(
        "CREATE TABLE IF NOT EXISTS owner_command (
            session_id TEXT NOT NULL,
            id TEXT NOT NULL,
            sequence INTEGER NOT NULL CHECK(sequence > 0),
            request TEXT NOT NULL,
            state TEXT NOT NULL,
            PRIMARY KEY(session_id,id),
            UNIQUE(session_id,sequence)
        );
        CREATE TABLE IF NOT EXISTS owner_core_log(
            session_id TEXT NOT NULL REFERENCES session_record(id) ON DELETE CASCADE,
            sequence INTEGER NOT NULL CHECK(sequence > 0),
            record TEXT NOT NULL,
            PRIMARY KEY(session_id,sequence)
        );
        CREATE TABLE IF NOT EXISTS owner_publication(
            session_id TEXT NOT NULL REFERENCES session_record(id) ON DELETE CASCADE,
            sequence INTEGER NOT NULL CHECK(sequence > 0),
            event TEXT NOT NULL,state TEXT NOT NULL,
            PRIMARY KEY(session_id,sequence)
        );
        CREATE TABLE IF NOT EXISTS owner_publication_origin(
            session_id TEXT NOT NULL,origin TEXT NOT NULL,sequence INTEGER NOT NULL,
            PRIMARY KEY(session_id,origin),
            FOREIGN KEY(session_id,sequence) REFERENCES owner_publication(session_id,sequence) ON DELETE CASCADE
        );
        CREATE TABLE IF NOT EXISTS input_session(session_id TEXT PRIMARY KEY,payload TEXT NOT NULL);
        CREATE TABLE IF NOT EXISTS input_operation(
            session_id TEXT NOT NULL,id TEXT NOT NULL,request TEXT NOT NULL,payload TEXT NOT NULL,
            PRIMARY KEY(session_id,id)
        );
        CREATE TABLE IF NOT EXISTS input_submitted(
            session_id TEXT NOT NULL,id TEXT NOT NULL,PRIMARY KEY(session_id,id)
        );",
    )?;
    Ok(())
}
