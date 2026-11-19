use std::{
    ops::{Deref, DerefMut},
    path::Path,
    time::Duration,
};

use rusqlite::{Connection, OpenFlags};

/// Distinguish state ownership, SQLite, and schema failures.
#[derive(Debug, thiserror::Error)]
pub enum DatabaseError {
    #[error("Forge history SQLite operation failed")]
    Sqlite(#[from] rusqlite::Error),
    #[error("Forge history initialization failed: {0:#}")]
    Initialization(#[source] anyhow::Error),
}

/// Keep the state version alive until SQLite closes.
pub(crate) struct HistoryConnection {
    connection: Connection,
    _lease: super::state::StateLease,
}

impl HistoryConnection {
    /// Open only this executable's history version, leaving older hosts undisturbed.
    pub(crate) fn open(root: &Path) -> Result<Self, DatabaseError> {
        Self::open_attaching(root, super::preferences::attach)
    }

    /// Open history as the main database with settings attached writable after it.
    ///
    /// Only the settings store writes both databases in one transaction; attachment order makes
    /// its transactions lock history before settings, as every history transaction does.
    pub(crate) fn open_settings_writer(root: &Path) -> Result<Self, DatabaseError> {
        Self::open_attaching(root, super::preferences::attach_writable)
    }

    fn open_attaching(
        root: &Path,
        attach: fn(&Connection, &Path) -> anyhow::Result<()>,
    ) -> Result<Self, DatabaseError> {
        let lease = super::state::StateLease::open(root).map_err(DatabaseError::Initialization)?;
        let directory = super::state::directory(root);
        let _bootstrap = super::schema::bootstrap_lock(&directory, "harness")
            .map_err(DatabaseError::Initialization)?;
        let mut connection = connection_open(root, OpenFlags::default())?;
        let version =
            super::schema::validate(&connection).map_err(DatabaseError::Initialization)?;
        if version == 0 {
            let populated: bool = connection.query_row(
                "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%')",
                [], |row| row.get(0),
            )?;
            if populated {
                return Err(DatabaseError::Initialization(anyhow::anyhow!(
                    "unversioned schema in {}; clear this state version with ForgeHistoryClear",
                    directory.display()
                )));
            }
            super::schema::initialize(&mut connection).map_err(DatabaseError::Initialization)?;
        } else if version != super::schema::VERSION {
            return Err(DatabaseError::Initialization(anyhow::anyhow!(
                "unexpected schema {version} in {}; clear this state version with ForgeHistoryClear",
                directory.display()
            )));
        }
        attach(&connection, root).map_err(DatabaseError::Initialization)?;
        Ok(Self {
            connection,
            _lease: lease,
        })
    }

    /// Read current history without admitting schema writes.
    pub(crate) fn read_only(root: &Path) -> Result<Self, DatabaseError> {
        let lease = super::state::StateLease::open(root).map_err(DatabaseError::Initialization)?;
        let connection = connection_open(
            root,
            OpenFlags::SQLITE_OPEN_READ_ONLY | OpenFlags::SQLITE_OPEN_URI,
        )?;
        if super::schema::validate(&connection).map_err(DatabaseError::Initialization)?
            != super::schema::VERSION
        {
            return Err(DatabaseError::Initialization(anyhow::anyhow!(
                "history requires initialization"
            )));
        }
        super::preferences::attach(&connection, root).map_err(DatabaseError::Initialization)?;
        Ok(Self {
            connection,
            _lease: lease,
        })
    }
}

impl Deref for HistoryConnection {
    type Target = Connection;
    /// Borrow SQLite while retaining its version lease.
    fn deref(&self) -> &Connection {
        &self.connection
    }
}

impl DerefMut for HistoryConnection {
    /// Borrow SQLite mutably while retaining its version lease.
    fn deref_mut(&mut self) -> &mut Connection {
        &mut self.connection
    }
}

/// Report whether `error` is SQLite contention whose transaction committed nothing.
///
/// SQLite reports `SQLITE_BUSY` or `SQLITE_LOCKED` before a write transaction holds its lock or
/// when its commit could not take it, and in both cases the transaction rolled back.
pub(crate) fn storage_busy(error: &anyhow::Error) -> bool {
    error.chain().any(|cause| {
        cause
            .downcast_ref::<rusqlite::Error>()
            .and_then(rusqlite::Error::sqlite_error_code)
            .is_some_and(|code| {
                matches!(
                    code,
                    rusqlite::ErrorCode::DatabaseBusy | rusqlite::ErrorCode::DatabaseLocked
                )
            })
    })
}

/// Classify the SQLite cause of a storage failure without its message text.
///
/// Trace records keep only allowlisted metadata, because error text can quote user content;
/// this stable code still tells a reader why a commit failed.
pub(crate) fn storage_cause(error: &anyhow::Error) -> Option<&'static str> {
    error.chain().find_map(|cause| {
        if let Some(error) = cause.downcast_ref::<rusqlite::Error>() {
            return Some(match error.sqlite_error_code() {
                Some(rusqlite::ErrorCode::DatabaseBusy) => "sqlite_busy",
                Some(rusqlite::ErrorCode::DatabaseLocked) => "sqlite_locked",
                Some(rusqlite::ErrorCode::ReadOnly) => "sqlite_read_only",
                Some(rusqlite::ErrorCode::DiskFull) => "sqlite_disk_full",
                Some(rusqlite::ErrorCode::SystemIoFailure) => "sqlite_io",
                Some(rusqlite::ErrorCode::DatabaseCorrupt) => "sqlite_corrupt",
                Some(rusqlite::ErrorCode::ConstraintViolation) => "sqlite_constraint",
                Some(_) => "sqlite_other",
                None => "sqlite_api",
            });
        }
        cause.downcast_ref::<std::io::Error>().map(|_| "io")
    })
}

/// Repeat a rolled-back write while another writer holds the lock, until `deadline` passes.
///
/// Each attempt already waits up to the busy timeout inside SQLite before it reports contention,
/// and the writes retried here begin immediate transactions, which never report a stale WAL
/// snapshot at once, so the loop needs no pause of its own.
pub(crate) fn retry_while_busy<Value>(
    deadline: Option<std::time::Instant>,
    mut attempt: impl FnMut() -> anyhow::Result<Value>,
) -> anyhow::Result<Value> {
    loop {
        match attempt() {
            Err(error)
                if storage_busy(&error)
                    && deadline.is_some_and(|deadline| std::time::Instant::now() < deadline) =>
            {
                continue;
            }
            result => return result,
        }
    }
}

/// Wait this long for a competing SQLite writer before reporting contention.
const BUSY_TIMEOUT: Duration = Duration::from_secs(5);

/// Apply one contention and durability policy to every history handle.
fn connection_open(root: &Path, flags: OpenFlags) -> Result<Connection, rusqlite::Error> {
    #[expect(
        clippy::disallowed_methods,
        reason = "this is the one place a history connection is opened"
    )]
    let connection = Connection::open_with_flags(super::state::database(root), flags)?;
    connection.busy_timeout(BUSY_TIMEOUT)?;
    connection.pragma_update(None, "synchronous", "FULL")?;
    connection.pragma_update(None, "foreign_keys", "ON")?;
    Ok(connection)
}
