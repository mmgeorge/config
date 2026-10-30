use super::{InputSnapshot, InputState, InputUpdate};
use anyhow::{Result, ensure};
use rusqlite::{Connection, OptionalExtension, params};

/// Persist active input separately from immutable idempotency receipts.
pub(super) struct InputJournal {
    connection: Connection,
    session_id: String,
}

impl InputJournal {
    /// Delete this owner's active queue and every idempotency record atomically.
    pub(super) fn delete(&mut self) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        for table in ["input_session", "input_operation", "input_submitted"] {
            transaction.execute(
                &format!("DELETE FROM {table} WHERE session_id=?1"),
                [&self.session_id],
            )?;
        }
        transaction.commit()?;
        Ok(())
    }

    /// Open the queue journal using the same durable SQLite policy as provider submissions.
    pub(super) fn open(root: &std::path::Path, session_id: &str) -> Result<Self> {
        std::fs::create_dir_all(root)?;
        let connection = Connection::open(root.join("input-queue.sqlite3"))?;
        connection.busy_timeout(std::time::Duration::from_secs(5))?;
        let _bootstrap = crate::storage::bootstrap_lock(root, "input-queue")?;
        let version: i64 = connection.query_row("PRAGMA user_version", [], |row| row.get(0))?;
        ensure!(
            matches!(version, 0 | 1),
            "unsupported input journal version {version}"
        );
        connection.execute_batch("PRAGMA journal_mode=WAL; PRAGMA synchronous=FULL;
            CREATE TABLE IF NOT EXISTS input_session(session_id TEXT PRIMARY KEY, payload TEXT NOT NULL);
            CREATE TABLE IF NOT EXISTS input_operation(session_id TEXT NOT NULL,id TEXT NOT NULL,request TEXT NOT NULL,payload TEXT NOT NULL,PRIMARY KEY(session_id,id));
            CREATE TABLE IF NOT EXISTS input_submitted(session_id TEXT NOT NULL,id TEXT NOT NULL,PRIMARY KEY(session_id,id));
            PRAGMA user_version=1;")?;
        Ok(Self {
            connection,
            session_id: session_id.into(),
        })
    }

    /// Load active queue metadata and durable duplicate-detection records for one owner.
    pub(super) fn load(&self) -> Result<(InputState, bool)> {
        let snapshot: Option<String> = self
            .connection
            .query_row(
                "SELECT payload FROM input_session WHERE session_id=?1",
                [&self.session_id],
                |row| row.get(0),
            )
            .optional()?;
        let restored = snapshot.is_some();
        let mut state = InputState {
            snapshot: snapshot
                .map(|payload| serde_json::from_str::<InputSnapshot>(&payload))
                .transpose()?
                .unwrap_or_default(),
            ..Default::default()
        };
        let mut query = self
            .connection
            .prepare("SELECT id,request,payload FROM input_operation WHERE session_id=?1")?;
        for row in query.query_map([&self.session_id], |row| {
            Ok((
                row.get::<_, String>(0)?,
                row.get::<_, String>(1)?,
                row.get::<_, String>(2)?,
            ))
        })? {
            let (id, request, payload) = row?;
            let operation: super::InputOperation = serde_json::from_str(&request)?;
            let receipt: InputUpdate = serde_json::from_str(&payload)?;
            ensure!(
                !id.is_empty()
                    && operation.id == id
                    && receipt.operation_id.as_deref() == Some(id.as_str()),
                "input receipt identity is corrupt"
            );
            ensure!(
                receipt.snapshot.is_none(),
                "input receipt contains an unexpected snapshot"
            );
            state.operation.insert(id, (request, receipt));
        }
        let mut query = self
            .connection
            .prepare("SELECT id FROM input_submitted WHERE session_id=?1")?;
        for id in query.query_map([&self.session_id], |row| row.get::<_, String>(0))? {
            let id = id?;
            ensure!(!id.is_empty(), "input message identity is corrupt");
            state.submitted.insert(id);
        }
        ensure!(
            restored || state.operation.is_empty() && state.submitted.is_empty(),
            "input journal snapshot is missing"
        );
        let mut identities = std::collections::HashSet::new();
        for message in &state.snapshot.messages {
            ensure!(
                !message.id.is_empty() && identities.insert(&message.id),
                "input queue identities are corrupt"
            );
            ensure!(
                state.submitted.contains(&message.id) && message.revision > 0,
                "input queue admission record is missing"
            );
            ensure!(
                !message.text.trim().is_empty() || message.config.is_some(),
                "input queue content is empty"
            );
            ensure!(
                message.reservation.as_ref().is_none_or(|id| !id.is_empty())
                    && message
                        .submission_id
                        .as_ref()
                        .is_none_or(|id| !id.is_empty()),
                "input queue reference is corrupt"
            );
            ensure!(
                !matches!(
                    message.delivery,
                    super::InputDelivery::Accepted | super::InputDelivery::Unknown
                ) || message.submission_id.is_some(),
                "input delivery evidence is missing"
            );
        }
        for (request, receipt) in state.operation.values() {
            let operation: super::InputOperation = serde_json::from_str(request)?;
            if receipt.error.is_none()
                && let super::InputAction::Submit { message } = operation.action
            {
                ensure!(
                    state.submitted.contains(&message.id),
                    "input receipt has no admission tombstone"
                );
            }
        }
        Ok((state, restored))
    }

    /// Commit only active state and newly admitted receipts, never rewrite historical receipts.
    pub(super) fn save(&mut self, previous: &InputState, next: &InputState) -> Result<()> {
        let snapshot = serde_json::to_string(&next.snapshot)?;
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        transaction.execute("INSERT INTO input_session(session_id,payload) VALUES(?1,?2) ON CONFLICT(session_id) DO UPDATE SET payload=excluded.payload",params![self.session_id,snapshot])?;
        for (id, (request, outcome)) in &next.operation {
            if !previous.operation.contains_key(id) {
                transaction.execute("INSERT INTO input_operation(session_id,id,request,payload) VALUES(?1,?2,?3,?4)",params![self.session_id,id,request,serde_json::to_string(outcome)?])?;
            }
        }
        for id in next.submitted.difference(&previous.submitted) {
            transaction.execute(
                "INSERT INTO input_submitted(session_id,id) VALUES(?1,?2)",
                params![self.session_id, id],
            )?;
        }
        transaction.commit()?;
        Ok(())
    }
}
