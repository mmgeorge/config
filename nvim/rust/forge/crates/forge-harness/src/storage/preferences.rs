use anyhow::{Context, Result, ensure};
use rusqlite::Connection;
use rusqlite::OptionalExtension;
use std::path::Path;

const VERSION: i64 = 2;
const SCHEMA: &str = "
CREATE TABLE setting_revision (id INTEGER PRIMARY KEY CHECK(id=1), revision INTEGER NOT NULL);
INSERT INTO setting_revision VALUES (1,0);
CREATE TABLE persistent_setting_layer (scope TEXT NOT NULL, identity TEXT NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(scope,identity));
CREATE TABLE setting_catalog (id INTEGER PRIMARY KEY CHECK(id=1), root TEXT NOT NULL);
CREATE TABLE setting_operation (id TEXT PRIMARY KEY, session_id TEXT NOT NULL, payload TEXT NOT NULL);
CREATE TABLE setting_resource (id TEXT PRIMARY KEY, scope TEXT NOT NULL, identity TEXT NOT NULL, payload TEXT NOT NULL);
CREATE TABLE preference_record(workspace TEXT NOT NULL,backend TEXT NOT NULL,payload TEXT NOT NULL,PRIMARY KEY(workspace,backend));
CREATE TABLE model_pin(backend TEXT NOT NULL,model TEXT NOT NULL,PRIMARY KEY(backend,model));
CREATE TABLE preference_operation(id TEXT PRIMARY KEY);
CREATE TABLE host_preference(name TEXT PRIMARY KEY,payload TEXT NOT NULL);";

/// Create canonical settings only when no earlier schema is present.
pub(super) fn initialize(root: &Path) -> Result<()> {
    #[expect(
        clippy::disallowed_methods,
        reason = "canonical settings initialization owns this connection"
    )]
    let mut connection = Connection::open(root.join("settings.db"))?;
    let version =
        connection.pragma_query_value(None, "user_version", |row| row.get::<_, i64>(0))?;
    if version == VERSION {
        return validate(&connection);
    }
    let count: i64 = connection.query_row(
        "SELECT count(*) FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'",
        [],
        |row| row.get(0),
    )?;
    ensure!(
        version == 0 && count == 0,
        "settings initialization refuses existing schema {version}"
    );
    connection.pragma_update(None, "journal_mode", "WAL")?;
    let transaction = connection.transaction()?;
    transaction.execute_batch(SCHEMA)?;
    transaction.pragma_update(None, "user_version", VERSION)?;
    transaction.commit()?;
    validate(&connection)
}

/// Reject incompatible or incomplete settings before reading a saved choice.
pub(crate) fn validate(connection: &Connection) -> Result<()> {
    let version =
        connection.pragma_query_value(None, "user_version", |row| row.get::<_, i64>(0))?;
    ensure!(
        version == VERSION,
        "unsupported settings schema {version}; expected {VERSION}"
    );
    for query in [
        "SELECT id,revision FROM setting_revision WHERE id=1",
        "SELECT scope,identity,payload FROM persistent_setting_layer LIMIT 0",
        "SELECT id,root FROM setting_catalog LIMIT 0",
        "SELECT id,session_id,payload FROM setting_operation LIMIT 0",
        "SELECT id,scope,identity,payload FROM setting_resource LIMIT 0",
        "SELECT workspace,backend,payload FROM preference_record LIMIT 0",
        "SELECT backend,model FROM model_pin LIMIT 0",
        "SELECT id FROM preference_operation LIMIT 0",
        "SELECT name,payload FROM host_preference LIMIT 0",
    ] {
        connection
            .prepare(query)
            .context("incomplete settings schema")?;
    }
    connection.query_row(
        "SELECT revision FROM setting_revision WHERE id=1",
        [],
        |row| row.get::<_, i64>(0),
    )?;
    validate_structure(connection)?;
    Ok(())
}

/// Reject altered constraints as well as missing columns before accepting settings.
fn validate_structure(connection: &Connection) -> Result<()> {
    static EXPECTED: std::sync::OnceLock<Vec<(String, String)>> = std::sync::OnceLock::new();
    let expected = EXPECTED.get_or_init(|| {
        #[expect(clippy::disallowed_methods, reason = "derive settings validation from its compiled schema without opening user state")]
        let reference = Connection::open_in_memory().expect("in-memory settings connection must open");
        reference.execute_batch(SCHEMA).expect("compiled settings schema must initialize");
        schema_objects(&reference).expect("compiled settings schema must be readable")
    });
    let actual = schema_objects(connection)?;
    ensure!(
        expected.iter().all(|required| actual.contains(required)),
        "incompatible settings schema structure; saved settings have been preserved"
    );
    Ok(())
}

/// Read canonical table definitions without decoding user choices.
fn schema_objects(connection: &Connection) -> Result<Vec<(String, String)>> {
    Ok(connection.prepare("SELECT name,sql FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name")?
        .query_map([], |row| Ok((row.get(0)?, row.get::<_, String>(1)?.split_whitespace().collect::<String>())))?
        .collect::<rusqlite::Result<Vec<_>>>()?)
}

/// Open persistent choices without importing a retired representation.
pub(crate) fn open(root: &Path) -> Result<Connection> {
    super::layout::prepare(root)?;
    #[expect(
        clippy::disallowed_methods,
        reason = "canonical persistent settings open through this boundary"
    )]
    let connection = Connection::open_with_flags(
        root.join("settings.db"),
        rusqlite::OpenFlags::SQLITE_OPEN_READ_WRITE,
    )?;
    connection.busy_timeout(std::time::Duration::from_secs(5))?;
    connection.pragma_update(None, "synchronous", "FULL")?;
    validate(&connection)?;
    Ok(connection)
}

/// Read a typed host preference without opening disposable history.
pub(crate) fn host_get<Value: serde::de::DeserializeOwned>(
    root: &Path,
    name: &str,
) -> Result<Option<Value>> {
    let connection = open(root)?;
    let payload: Option<String> = connection
        .query_row(
            "SELECT payload FROM host_preference WHERE name=?1",
            [name],
            |row| row.get(0),
        )
        .optional()?;
    payload
        .map(|payload| {
            serde_json::from_str(&payload)
                .with_context(|| format!("decode saved {name} preference"))
        })
        .transpose()
}

/// Persist a typed host preference atomically without opening disposable history.
pub(crate) fn host_set<Value: serde::Serialize>(
    root: &Path,
    name: &str,
    value: &Value,
) -> Result<()> {
    open(root)?.execute("INSERT INTO host_preference VALUES(?1,?2) ON CONFLICT(name) DO UPDATE SET payload=excluded.payload", rusqlite::params![name, serde_json::to_string(value)?])?;
    Ok(())
}

/// Attach canonical preferences read-only for history transactions that read a user choice.
///
/// SQLite's `BEGIN IMMEDIATE` takes the write lock of every writable attached database in
/// attachment order. A writable attachment would make every history write also lock the
/// settings database that every other host's settings writer locks, and opposite lock orders
/// between hosts then wait each other out until the busy timeout fails both.
pub(super) fn attach(connection: &Connection, root: &Path) -> Result<()> {
    drop(open(root)?);
    connection.execute(
        "ATTACH DATABASE ?1 AS preferences",
        [read_only_uri(&root.join("settings.db"))],
    )?;
    Ok(())
}

/// Attach canonical preferences writable after the history main database.
///
/// The settings store writes both databases in one transaction. Opening history as the main
/// database gives it index 0, so SQLite locks history before settings, the same order as every
/// history transaction.
pub(super) fn attach_writable(connection: &Connection, root: &Path) -> Result<()> {
    drop(open(root)?);
    connection.execute(
        "ATTACH DATABASE ?1 AS preferences",
        [root.join("settings.db").to_string_lossy().as_ref()],
    )?;
    connection.pragma_update(Some("preferences"), "synchronous", "FULL")?;
    Ok(())
}

/// Build a read-only SQLite URI for `path`, escaping the characters URI syntax reserves.
fn read_only_uri(path: &Path) -> String {
    let path = dunce::simplified(path).to_string_lossy().replace('\\', "/");
    let mut encoded = String::with_capacity(path.len());
    for character in path.chars() {
        match character {
            '%' => encoded.push_str("%25"),
            '?' => encoded.push_str("%3f"),
            '#' => encoded.push_str("%23"),
            character => encoded.push(character),
        }
    }
    let authority = if encoded.starts_with('/') {
        "file://"
    } else if encoded.as_bytes().get(1) == Some(&b':') {
        "file:///"
    } else {
        "file:"
    };
    format!("{authority}{encoded}?mode=ro")
}

/// Commit a recovered preference and its replay receipt in one persistent transaction.
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
        let payload = serde_json::to_string(
            &serde_json::json!({"format_version": super::PREFERENCE_FORMAT_VERSION,"preference": preference}),
        )?;
        transaction.execute("INSERT INTO preference_record(workspace,backend,payload) VALUES(?1,?2,?3) ON CONFLICT(workspace,backend) DO UPDATE SET payload=excluded.payload", rusqlite::params![intent.workspace,intent.provider,payload])?;
    }
    transaction.commit()?;
    Ok(())
}
