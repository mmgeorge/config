use super::{InputSnapshot, InputState, InputUpdate};
use anyhow::{Context, Result, ensure};
use rusqlite::{OptionalExtension, params};

/// Persist active input separately from immutable idempotency receipts.
pub(super) struct InputJournal {
    /// The state root's history writer; every queue statement runs on its connection.
    writer: crate::storage::writer::HistoryWriter,
    session_id: String,
}

impl InputJournal {
    /// Delete this owner's active queue and every idempotency record atomically.
    pub(super) fn delete(&mut self) -> Result<()> {
        let mut connection = self.writer.connection()?;
        let transaction =
            connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        for table in [
            "input_session",
            "input_operation",
            "input_submitted",
            "owner_command",
        ] {
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
        let writer = crate::storage::writer::HistoryWriter::for_root(root)?;
        Ok(Self {
            writer,
            session_id: session_id.into(),
        })
    }

    /// Load active queue metadata and durable duplicate-detection records for one owner.
    pub(super) fn load(&self) -> Result<(InputState, bool)> {
        let connection = self.writer.connection()?;
        let snapshot: Option<String> = connection
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
        let mut query = connection
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
        let mut query = connection.prepare("SELECT id FROM input_submitted WHERE session_id=?1")?;
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
        let mut connection = self.writer.connection()?;
        let snapshot = serde_json::to_string(&next.snapshot)?;
        let transaction =
            connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        transaction.execute("INSERT INTO input_session(session_id,payload) VALUES(?1,?2) ON CONFLICT(session_id) DO UPDATE SET payload=excluded.payload",params![self.session_id,snapshot])?;
        for (id, (request, outcome)) in &next.operation {
            if !previous.operation.contains_key(id) {
                transaction.execute("INSERT INTO input_operation(session_id,id,request,payload) VALUES(?1,?2,?3,?4)",params![self.session_id,id,request,serde_json::to_string(outcome)?])?;
                crate::exchange::journal::commit_input_command(
                    &transaction,
                    &self.session_id,
                    id,
                    outcome,
                )?;
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

/// Reserve a workflow input in the transaction that commits its source and owner checkpoint.
pub(crate) fn reserve_workflow_input(
    transaction: &rusqlite::Transaction<'_>,
    session: &str,
    message: &super::InputMessage,
) -> Result<()> {
    ensure!(
        !message.id.is_empty() && !message.text.trim().is_empty(),
        "workflow input identity and text are required"
    );
    let payload: Option<String> = transaction
        .query_row(
            "SELECT payload FROM input_session WHERE session_id=?1",
            [session],
            |row| row.get(0),
        )
        .optional()?;
    let mut snapshot: super::InputSnapshot = payload
        .map(|payload| serde_json::from_str(&payload))
        .transpose()?
        .unwrap_or_default();
    ensure!(
        !snapshot.messages.iter().any(|input| input.id == message.id),
        "workflow input was already reserved"
    );
    let mut message = message.clone();
    message.revision = 1;
    snapshot.messages.push(message.clone());
    snapshot.revision = snapshot
        .revision
        .checked_add(1)
        .context("input revision exhausted")?;
    transaction.execute("INSERT INTO input_session(session_id,payload) VALUES(?1,?2) ON CONFLICT(session_id) DO UPDATE SET payload=excluded.payload", params![session, serde_json::to_string(&snapshot)?])?;
    let operation = super::InputOperation {
        id: message.id.clone(),
        control_revision: snapshot.control_revision,
        after_control: None,
        action: super::InputAction::Submit {
            message: Box::new(message.clone()),
        },
    };
    let receipt = super::InputUpdate {
        operation_id: Some(operation.id.clone()),
        revision: snapshot.revision,
        upsert: vec![message.clone()],
        suspended: snapshot.suspended,
        control_revision: snapshot.control_revision,
        ..Default::default()
    };
    transaction.execute(
        "INSERT INTO input_operation(session_id,id,request,payload) VALUES(?1,?2,?3,?4)",
        params![
            session,
            operation.id,
            serde_json::to_string(&operation)?,
            serde_json::to_string(&receipt)?
        ],
    )?;
    transaction.execute(
        "INSERT INTO input_submitted(session_id,id) VALUES(?1,?2)",
        params![session, message.id],
    )?;
    Ok(())
}
