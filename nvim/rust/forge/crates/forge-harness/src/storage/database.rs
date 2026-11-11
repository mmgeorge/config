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
    pub(in crate::storage) fn open(root: &Path) -> Result<Self, DatabaseError> {
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
        super::preferences::attach(&connection, root).map_err(DatabaseError::Initialization)?;
        Ok(Self {
            connection,
            _lease: lease,
        })
    }

    /// Read current history without admitting schema writes.
    pub(in crate::storage) fn read_only(root: &Path) -> Result<Self, DatabaseError> {
        let lease = super::state::StateLease::open(root).map_err(DatabaseError::Initialization)?;
        let connection = connection_open(root, OpenFlags::SQLITE_OPEN_READ_ONLY)?;
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
