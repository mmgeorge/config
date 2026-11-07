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
    pub(crate) fn read_only(root: &Path) -> Result<Self, DatabaseError> {
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

#[cfg(test)]
thread_local! {
    static BUSY_TIMEOUT_OVERRIDE: std::cell::Cell<Option<Duration>> =
        const { std::cell::Cell::new(None) };
}

/// Restore the production busy timeout when a contention test releases its override.
#[cfg(test)]
pub(crate) struct BusyTimeoutOverride {
    previous: Option<Duration>,
}

#[cfg(test)]
impl Drop for BusyTimeoutOverride {
    fn drop(&mut self) {
        BUSY_TIMEOUT_OVERRIDE.with(|slot| slot.set(self.previous));
    }
}

/// Make history handles opened on this thread report contention after `timeout`.
///
/// Contention tests use a zero timeout so a held external lock fails the write at once instead
/// of waiting for the production timeout to elapse.
#[cfg(test)]
pub(crate) fn override_busy_timeout(timeout: Duration) -> BusyTimeoutOverride {
    BusyTimeoutOverride {
        previous: BUSY_TIMEOUT_OVERRIDE.with(|slot| slot.replace(Some(timeout))),
    }
}

fn busy_timeout() -> Duration {
    #[cfg(test)]
    if let Some(timeout) = BUSY_TIMEOUT_OVERRIDE.with(std::cell::Cell::get) {
        return timeout;
    }
    BUSY_TIMEOUT
}

/// Apply one contention and durability policy to every history handle.
fn connection_open(root: &Path, flags: OpenFlags) -> Result<Connection, rusqlite::Error> {
    let connection = Connection::open_with_flags(super::state::database(root), flags)?;
    connection.busy_timeout(busy_timeout())?;
    connection.pragma_update(None, "synchronous", "FULL")?;
    connection.pragma_update(None, "foreign_keys", "ON")?;
    Ok(connection)
}
