use anyhow::{Context, Result, ensure};
use rusqlite::{TransactionBehavior, params};

use super::{SqliteStore, decode_current_session, encode, encode_session};
use crate::session::HarnessSession;
use crate::timeline::{SessionEventKind, SessionEventRecord, TimelineOwner};

/// Grants metadata access only while the original runtime owner remains alive.
pub(crate) struct SessionMetadataWriter {
    data_root: std::path::PathBuf,
    session_id: String,
    client_id: String,
    owner: std::sync::Weak<std::fs::File>,
}

impl SessionMetadataWriter {
    /// Pins runtime ownership while committing an independent name update.
    pub(crate) fn rename(
        &self,
        name: &str,
        expected_name: Option<&str>,
        now_ms: i64,
    ) -> Result<HarnessSession> {
        let owner = self
            .owner
            .upgrade()
            .context("session runtime ownership ended")?;
        let mut store = SqliteStore::open(&self.data_root)?;
        store.session_lock.insert(self.session_id.clone(), owner);
        store.rename_owned_session(
            &self.session_id,
            &self.client_id,
            name,
            expected_name,
            now_ms,
        )
    }
}

impl SqliteStore {
    /// Names a previously unnamed session without replacing a concurrent user rename.
    pub(crate) fn initialize_session_name(
        &mut self,
        session_id: &str,
        client_id: &str,
        name: &str,
    ) -> Result<HarnessSession> {
        ensure!(
            self.session_lock.contains_key(session_id),
            "session naming requires runtime ownership"
        );
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let payload: String = transaction.query_row(
            "SELECT payload FROM session_record WHERE id=?1",
            [session_id],
            |row| row.get(0),
        )?;
        let mut session = decode_current_session(&payload)?.context("session format changed")?;
        ensure!(
            session.lease_owner.as_deref() == Some(client_id),
            "Harness session lease ownership changed"
        );
        if session.name.trim().is_empty() {
            session.name = name.to_owned();
            transaction.execute(
                "UPDATE session_record SET payload=?2 WHERE id=?1",
                params![session_id, encode_session(&session)?],
            )?;
        }
        transaction.commit()?;
        Ok(session)
    }

    /// Captures revocable metadata access without extending the runtime owner's lifetime.
    pub(crate) fn session_metadata_writer(
        &self,
        session_id: &str,
        client_id: &str,
    ) -> Result<SessionMetadataWriter> {
        let owner = self
            .session_lock
            .get(session_id)
            .context("session requires runtime ownership")?;
        Ok(SessionMetadataWriter {
            data_root: self.data_root.clone(),
            session_id: session_id.to_owned(),
            client_id: client_id.to_owned(),
            owner: std::sync::Arc::downgrade(owner),
        })
    }

    /// Holds target ownership across cleanup and deletion. Cleanup errors retain the database record.
    pub(crate) fn delete_session_with(
        &mut self,
        session_id: &str,
        cleanup: impl FnOnce() -> Result<()>,
    ) -> Result<()> {
        ensure!(
            !self.session_lock.contains_key(session_id),
            "cannot delete an owned session"
        );
        let _owner = self.acquire_runtime_lock(session_id)?;
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        transaction.execute("DELETE FROM session_record WHERE id=?1", [session_id])?;
        cleanup()?;
        transaction.commit()?;
        Ok(())
    }

    /// Commits a name and its timeline event under the current owner and expected-name checks.
    pub(crate) fn rename_owned_session(
        &mut self,
        session_id: &str,
        client_id: &str,
        name: &str,
        expected_name: Option<&str>,
        now_ms: i64,
    ) -> Result<HarnessSession> {
        ensure!(
            self.session_lock.contains_key(session_id),
            "session rename requires runtime ownership"
        );
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let payload: String = transaction.query_row(
            "SELECT payload FROM session_record WHERE id=?1",
            [session_id],
            |row| row.get(0),
        )?;
        let mut session = decode_current_session(&payload)?.context("session format changed")?;
        ensure!(
            session.lease_owner.as_deref() == Some(client_id),
            "Harness session lease ownership changed"
        );
        if let Some(expected_name) = expected_name {
            ensure!(
                session.name == expected_name,
                "Session name changed during generation"
            );
        }
        session.name = name.to_owned();
        session.updated_at_ms = now_ms;
        let event = SessionEventRecord {
            id: uuid::Uuid::new_v4().to_string(),
            session_id: session.id.clone(),
            owner: TimelineOwner::session(&session.id),
            created_at_ms: now_ms,
            detail: SessionEventKind::Renamed {
                name: session.name.clone(),
            },
        };
        transaction.execute(
            "UPDATE session_record SET payload=?2,updated_at_ms=?3 WHERE id=?1",
            params![session_id, encode_session(&session)?, now_ms],
        )?;
        transaction.execute(
            "INSERT INTO session_event_record(id,session_id,payload) VALUES(?1,?2,?3)",
            params![event.id, session_id, encode(&event)?],
        )?;
        transaction.commit()?;
        Ok(session)
    }
}
