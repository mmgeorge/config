use super::{InputSnapshot, InputState, InputUpdate};
use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, OptionalExtension, params};

/// Persist active input separately from immutable idempotency receipts.
pub(super) struct InputJournal {
    connection: Connection,
    session_id: String,
    root: std::path::PathBuf,
}

impl InputJournal {
    /// Delete this owner's active queue and every idempotency record atomically.
    pub(super) fn delete(&mut self) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
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
        std::fs::create_dir_all(root)?;
        // Bootstrap the shared schema before opening this domain's independent handle.
        // Input receipt and owner-command completion now commit in the same database.
        drop(crate::storage::SqliteStore::open(root)?);
        let mut connection = Connection::open(root.join("harness.sqlite3"))?;
        connection.busy_timeout(std::time::Duration::from_secs(5))?;
        connection.pragma_update(None, "synchronous", "FULL")?;
        migrate_legacy(&mut connection, root, session_id)?;
        Ok(Self {
            connection,
            session_id: session_id.into(),
            root: root.to_owned(),
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
                .map(|payload| self.decode::<InputSnapshot>(&payload))
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
            let operation: super::InputOperation = self.decode(&request)?;
            let receipt: InputUpdate = self.decode(&payload)?;
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
            let operation: super::InputOperation = self.decode(request)?;
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

    /// Upgrade saved question messages before decoding the current typed queue schema.
    fn decode<T: serde::de::DeserializeOwned>(&self, payload: &str) -> Result<T> {
        let mut value: serde_json::Value = serde_json::from_str(payload)?;
        if let Some(messages) = value
            .get_mut("messages")
            .and_then(serde_json::Value::as_array_mut)
        {
            for message in messages {
                self.restore_question_message(message)?;
            }
        }
        if let Some(messages) = value
            .get_mut("upsert")
            .and_then(serde_json::Value::as_array_mut)
        {
            for message in messages {
                self.restore_question_message(message)?;
            }
        }
        if let Some(messages) = value
            .pointer_mut("/snapshot/messages")
            .and_then(serde_json::Value::as_array_mut)
        {
            for message in messages {
                self.restore_question_message(message)?;
            }
        }
        if value
            .pointer("/action/kind")
            .and_then(serde_json::Value::as_str)
            == Some("submit")
            && let Some(message) = value.pointer_mut("/action/message")
        {
            self.restore_question_message(message)?;
        }
        Ok(serde_json::from_value(value)?)
    }

    /// Rebuild legacy replies from durable question history, never their native text envelope.
    fn restore_question_message(&self, message: &mut serde_json::Value) -> Result<()> {
        let Some(purpose) = message.get("purpose") else {
            return Ok(());
        };
        if purpose.get("kind").and_then(serde_json::Value::as_str) != Some("question")
            || purpose.get("reply").is_some()
        {
            return Ok(());
        }
        let question_id =
            serde_json::from_value(purpose.get("question_id").cloned().ok_or_else(|| {
                anyhow::anyhow!("saved question input has no question identity")
            })?)?;
        let kind = serde_json::from_value(
            purpose
                .get("message_kind")
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("saved question input has no action"))?,
        )?;
        let message_id = message
            .get("id")
            .and_then(serde_json::Value::as_str)
            .ok_or_else(|| anyhow::anyhow!("saved question input has no message identity"))?;
        let reply = crate::question::restore_input_reply(
            &self.root,
            &self.session_id,
            &question_id,
            message_id,
            kind,
        )?;
        let text = match &reply.response {
            forge_provider_api::QuestionReplyResponse::Answer { answer, .. }
            | forge_provider_api::QuestionReplyResponse::Correction { answer, .. } => answer,
            forge_provider_api::QuestionReplyResponse::Clarification { text, .. } => text,
        };
        message["text"] = serde_json::Value::String(text.clone());
        message["purpose"]["reply"] = serde_json::to_value(reply)?;
        Ok(())
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

/// Copy one legacy session once, retaining the source for rollback and inspection.
/// The migration marker commits with all records, preventing old queues from returning
/// after deletion or a later host restart. Existing shared-database input wins.
fn migrate_legacy(
    connection: &mut Connection,
    root: &std::path::Path,
    session: &str,
) -> Result<()> {
    let transaction =
        connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
    let migrated: bool = transaction.query_row(
        "SELECT EXISTS(SELECT 1 FROM input_migration WHERE session_id=?1)",
        [session],
        |row| row.get(0),
    )?;
    if migrated {
        return Ok(());
    }
    let present: bool = transaction.query_row(
        "SELECT EXISTS(SELECT 1 FROM input_session WHERE session_id=?1)",
        [session],
        |row| row.get(0),
    )?;
    let legacy_path = root.join("input-queue.sqlite3");
    if !present && legacy_path.is_file() {
        let legacy =
            Connection::open_with_flags(legacy_path, rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY)?;
        legacy.busy_timeout(std::time::Duration::from_secs(5))?;
        let version: i64 = legacy.pragma_query_value(None, "user_version", |row| row.get(0))?;
        ensure!(
            matches!(version, 0 | 1),
            "unsupported legacy input journal version {version}"
        );
        let snapshot: Option<String> = legacy
            .query_row(
                "SELECT payload FROM input_session WHERE session_id=?1",
                [session],
                |row| row.get(0),
            )
            .optional()?;
        if let Some(snapshot) = snapshot {
            transaction.execute(
                "INSERT INTO input_session(session_id,payload) VALUES(?1,?2)",
                params![session, snapshot],
            )?;
        }
        let mut query =
            legacy.prepare("SELECT id,request,payload FROM input_operation WHERE session_id=?1")?;
        for row in query.query_map([session], |row| {
            Ok((
                row.get::<_, String>(0)?,
                row.get::<_, String>(1)?,
                row.get::<_, String>(2)?,
            ))
        })? {
            let (id, request, payload) = row?;
            transaction.execute(
                "INSERT INTO input_operation(session_id,id,request,payload) VALUES(?1,?2,?3,?4)",
                params![session, id, request, payload],
            )?;
        }
        let mut query = legacy.prepare("SELECT id FROM input_submitted WHERE session_id=?1")?;
        for row in query.query_map([session], |row| row.get::<_, String>(0))? {
            transaction.execute(
                "INSERT INTO input_submitted(session_id,id) VALUES(?1,?2)",
                params![session, row?],
            )?;
        }
    }
    transaction.execute(
        "INSERT INTO input_migration(session_id) VALUES(?1)",
        [session],
    )?;
    transaction.commit()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Copy legacy state once and retain the import tombstone across explicit deletion.
    #[test]
    fn legacy_queue_migrates_once_and_cannot_resurrect_after_delete() -> Result<()> {
        let root = tempfile::tempdir()?;
        let legacy = Connection::open(root.path().join("input-queue.sqlite3"))?;
        legacy.execute_batch("CREATE TABLE input_session(session_id TEXT PRIMARY KEY,payload TEXT NOT NULL);
            CREATE TABLE input_operation(session_id TEXT NOT NULL,id TEXT NOT NULL,request TEXT NOT NULL,payload TEXT NOT NULL,PRIMARY KEY(session_id,id));
            CREATE TABLE input_submitted(session_id TEXT NOT NULL,id TEXT NOT NULL,PRIMARY KEY(session_id,id)); PRAGMA user_version=1;")?;
        let input = crate::input::SessionInputQueue::default();
        let operation = crate::input::InputOperation {
            id: "legacy-submit".into(),
            control_revision: 0,
            after_control: None,
            action: crate::input::InputAction::Submit {
                message: Box::new(crate::input::InputMessage {
                    id: "legacy-input".into(),
                    revision: 0,
                    text: "saved draft".into(),
                    purpose: crate::input::InputPurpose::User,
                    attachments: Vec::new(),
                    config: None,
                    intent: crate::input::InputIntent::Queue,
                    delivery: crate::input::InputDelivery::Queued,
                    route: None,
                    reservation: None,
                    submission_id: None,
                    error: None,
                }),
            },
        };
        let mut update = input.apply(operation.clone())?;
        update.snapshot = None;
        legacy.execute(
            "INSERT INTO input_session(session_id,payload) VALUES('session',?1)",
            [serde_json::to_string(&input.snapshot()?)?],
        )?;
        legacy.execute(
            "INSERT INTO input_operation(session_id,id,request,payload) VALUES('session',?1,?2,?3)",
            params![
                operation.id,
                serde_json::to_string(&operation)?,
                serde_json::to_string(&update)?
            ],
        )?;
        legacy.execute(
            "INSERT INTO input_submitted(session_id,id) VALUES('session','legacy-input')",
            [],
        )?;
        let mut current = InputJournal::open(root.path(), "session")?;
        let (state, restored) = current.load()?;
        assert!(restored);
        assert_eq!(state.snapshot.messages[0].text, "saved draft");
        assert!(state.operation.contains_key("legacy-submit"));
        current.delete()?;
        drop(current);
        let (state, restored) = InputJournal::open(root.path(), "session")?.load()?;
        assert!(!restored);
        assert!(state.snapshot.messages.is_empty() && state.operation.is_empty());
        assert_eq!(
            legacy.query_row("SELECT count(*) FROM input_session", [], |row| row
                .get::<_, i64>(0))?,
            1
        );
        Ok(())
    }
    /// Rolling back a workflow transaction must remove its input and immutable receipt together.
    #[test]
    fn workflow_reservation_rolls_back_and_recovers_its_original_receipt() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut journal = InputJournal::open(root.path(), "session")?;
        let message = super::super::InputMessage {
            id: "workflow-input".into(),
            revision: 0,
            text: "Continue the captured request".into(),
            purpose: super::super::InputPurpose::User,
            attachments: Vec::new(),
            config: None,
            intent: super::super::InputIntent::Auto,
            delivery: super::super::InputDelivery::Queued,
            route: None,
            reservation: None,
            submission_id: None,
            error: None,
        };
        {
            let transaction = journal.connection.transaction()?;
            reserve_workflow_input(&transaction, "session", &message)?;
            transaction.rollback()?;
        }
        let (state, _) = journal.load()?;
        assert!(state.snapshot.messages.is_empty());
        assert!(state.operation.is_empty());
        {
            let transaction = journal.connection.transaction()?;
            reserve_workflow_input(&transaction, "session", &message)?;
            transaction.commit()?;
        }
        let (state, _) = journal.load()?;
        assert_eq!(state.snapshot.messages[0].text, message.text);
        let (_, receipt) = state.operation.get(&message.id).unwrap();
        assert_eq!(receipt.upsert[0].id, message.id);
        assert_eq!(receipt.upsert[0].revision, 1);
        assert!(receipt.snapshot.is_none());
        Ok(())
    }
}
