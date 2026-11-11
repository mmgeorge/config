//! Read history through standing read-only handles.
//!
//! Opening a history handle takes the state lease, opens the database file, validates the
//! schema, and attaches the preference store. A reader that opened one handle per operation
//! paid that cost on every snapshot. This module keeps the handles of a root idle between
//! reads. The pool belongs to the root's history writer, so the handles close, and release
//! their state lease, when the last writer handle drops.

use std::{
    path::Path,
    sync::{Arc, Mutex},
};

use anyhow::Result;

use super::{SqliteStore, writer::HistoryWriter};

/// Lend standing read-only handles for one state root.
pub(crate) struct HistoryReader {
    writer: Option<HistoryWriter>,
}

impl HistoryReader {
    /// Get the reader for a root. A root whose writer has not started has no pool, and each
    /// read then opens its own handle.
    pub(crate) fn for_root(root: &Path) -> Self {
        Self {
            writer: HistoryWriter::started(root),
        }
    }

    /// Borrow a handle that sees every write queued so far, including streamed text that is
    /// shown before it is saved.
    pub(crate) fn current(root: &Path) -> Result<PooledStore> {
        let reader = Self::for_root(root);
        if let Some(writer) = &reader.writer {
            super::writer::off_worker(|| writer.barrier());
        }
        reader.checkout(root)
    }

    fn checkout(&self, root: &Path) -> Result<PooledStore> {
        let Some(writer) = &self.writer else {
            return Ok(PooledStore {
                #[expect(
                    clippy::disallowed_methods,
                    reason = "a root without a running writer has no pool to borrow from"
                )]
                store: Some(SqliteStore::read_only(root)?),
                pool: None,
            });
        };
        let pool = writer.readers();
        let idle = pool
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .pop();
        let store = match idle {
            Some(store) => store,
            #[expect(
                clippy::disallowed_methods,
                reason = "this opens the handle that the pool then keeps"
            )]
            None => SqliteStore::read_only(root)?,
        };
        Ok(PooledStore {
            store: Some(store),
            pool: Some(pool),
        })
    }
}

/// A read-only handle that returns to its root's pool when dropped.
pub(crate) struct PooledStore {
    store: Option<SqliteStore>,
    pool: Option<Arc<Mutex<Vec<SqliteStore>>>>,
}

impl std::ops::Deref for PooledStore {
    type Target = SqliteStore;
    fn deref(&self) -> &SqliteStore {
        self.store
            .as_ref()
            .expect("a live checkout holds its handle")
    }
}

impl Drop for PooledStore {
    fn drop(&mut self) {
        // A handle left inside a transaction by a failed read is closed, not reused. So is a
        // handle whose writer has already dropped the pool.
        if let (Some(store), Some(pool)) = (self.store.take(), self.pool.take())
            && store.is_idle()
            && Arc::strong_count(&pool) > 1
        {
            pool.lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(store);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::SessionStore;
    use crate::storage::test::session;

    /// A pooled handle is reused, sees queued writes, and closes with the writer.
    #[test]
    fn pooled_reads_see_queued_writes_and_release_the_root() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&session("session", "D:/work"))?;
        drop(store);
        assert!(HistoryReader::for_root(root.path()).writer.is_none());

        let writer = HistoryWriter::for_root(root.path())?;
        let renamed = writer.enqueue(
            crate::storage::writer::WriteScope::Session("session".into()),
            |transaction| {
                transaction.execute(
                    "UPDATE session_record SET workspace='D:/renamed' WHERE id='session'",
                    [],
                )?;
                Ok(())
            },
        )?;
        let workspace: Option<String> = HistoryReader::current(root.path())?
            .connection()
            .query_row(
                "SELECT workspace FROM session_record WHERE id='session'",
                [],
                |row| row.get(0),
            )?;
        assert!(writer.wait_committed(&renamed).is_ok());
        assert_eq!(workspace.as_deref(), Some("D:/renamed"));
        assert_eq!(writer.readers().lock().unwrap().len(), 1);
        drop(HistoryReader::current(root.path())?);
        assert_eq!(
            writer.readers().lock().unwrap().len(),
            1,
            "the handle was not reused"
        );

        drop(writer);
        assert!(HistoryReader::for_root(root.path()).writer.is_none());
        Ok(())
    }
}
