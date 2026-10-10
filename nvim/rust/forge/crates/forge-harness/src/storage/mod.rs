pub mod objects;
mod ownership;
mod recovery;
mod tool_output;

use crate::agent::Agent;
use crate::checkpoint::CheckpointRecord;
use crate::exchange::{Exchange, ExchangeComment};
use crate::goal::GoalRecord;
use crate::plan::{PlanExecutionRecord, PlanLifecycleRecord, PlanRecord};
use crate::session::{HarnessPreference, HarnessSession, SessionStore};
use crate::timeline::SessionEventRecord;
use anyhow::{Context, Result};
use rusqlite::{Connection, OptionalExtension, TransactionBehavior, params};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;
use std::time::Duration;

const SESSION_FORMAT_VERSION: u32 = 39;

/// Stores one session with the exact durable format that produced it.
#[derive(Deserialize, Serialize)]
struct SessionEnvelope {
    format_version: u32,
    session: HarnessSession,
}

/// Owns SQLite metadata and content-addressed objects for the Harness broker.
pub struct SqliteStore {
    connection: Connection,
    session_lock: std::collections::HashMap<String, fs::File>,
    data_root: std::path::PathBuf,
    pub objects: objects::ObjectStore,
}

impl SqliteStore {
    /// Holds an operating-system lock so lease expiry cannot admit a second live runtime.
    pub(crate) fn lock_session(&mut self, session_id: &str) -> Result<()> {
        if self.session_lock.contains_key(session_id) { return Ok(()); }
        let directory = self.data_root.join("session-owner");
        fs::create_dir_all(&directory)?;
        let path = directory.join(format!("{}.lock", crate::plan::digest(session_id.as_bytes())));
        let owner = fs::OpenOptions::new().create(true).truncate(false).read(true).write(true).open(&path)?;
        match owner.try_lock() {
            Ok(()) => {},
            Err(fs::TryLockError::WouldBlock) => return Err(crate::session::SessionLeaseConflict {
                session_id: session_id.to_owned(), native_fork: self.load_session(session_id)?.is_some_and(|session| session.native_fork),
            }.into()),
            Err(error) => return Err(anyhow::anyhow!("acquire session runtime ownership: {error}")),
        }
        self.session_lock.insert(session_id.to_owned(), owner);
        Ok(())
    }
    /// Loads workflow history for a single conversation.
    pub fn list_task(&self, session_id: &str) -> Result<Vec<crate::task::TaskRecord>> {
        self.list_payload("SELECT payload FROM task_record WHERE session_id=?1 ORDER BY json_extract(payload,'$.updated_at_ms') DESC, rowid DESC", [session_id])
    }

    /// Persists a workflow checkpoint without changing the selected conversation.
    pub fn save_task(&mut self, task: &crate::task::TaskRecord) -> Result<()> {
        self.save_scoped_payload("task_record", &task.id, &task.session_id, task)
    }

    /// Commits task selection and its lifecycle checkpoint under the session lease.
    pub fn select_task(&mut self, session: &HarnessSession, task: Option<&crate::task::TaskRecord>, goal: Option<&GoalRecord>, plan: Option<&PlanRecord>, client_id: &str) -> Result<()> {
        let transaction = self.connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let payload: String = transaction.query_row("SELECT payload FROM session_record WHERE id=?1", [&session.id], |row| row.get(0))?;
        let owner = decode_current_session(&payload)?.context("session format changed")?;
        anyhow::ensure!(owner.lease_owner.as_deref() == Some(client_id), "task selection lost session ownership");
        if let Some(task) = task {
            anyhow::ensure!(task.session_id == session.id, "task belongs to another conversation");
            transaction.execute("INSERT INTO task_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload", params![task.id, session.id, encode(task)?])?;
        }
        if let Some(goal) = goal {
            anyhow::ensure!(goal.session_id == session.id, "goal belongs to another conversation");
            transaction.execute("INSERT INTO goal_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload", params![goal.id, session.id, encode(goal)?])?;
        }
        if let Some(plan) = plan {
            anyhow::ensure!(plan.session_id == session.id, "plan belongs to another conversation");
            let mut payload = serde_json::to_value(plan)?;
            payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
            transaction.execute("INSERT INTO plan_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload", params![plan.id, session.id, encode(&payload)?])?;
        }
        transaction.execute("UPDATE session_record SET payload=?2,updated_at_ms=?3 WHERE id=?1", params![session.id,encode_session(session)?,session.updated_at_ms])?;
        transaction.commit()?;
        Ok(())
    }

    /// Publishes acceptance and every execution owner in one commit before provider dispatch.
    pub fn accept_task(&mut self, session: &HarnessSession, task: &crate::task::TaskRecord,
        goal: &GoalRecord, plan: &PlanRecord, execution: &PlanExecutionRecord,
        lifecycle: &PlanLifecycleRecord, client_id: &str) -> Result<()> {
        let transaction = self.connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let payload: String = transaction.query_row("SELECT payload FROM session_record WHERE id=?1", [&session.id], |row| row.get(0))?;
        let owner = decode_current_session(&payload)?.context("session format changed")?;
        anyhow::ensure!(owner.lease_owner.as_deref() == Some(client_id), "plan acceptance lost session ownership");
        for (table, id, mut payload, schema) in [
            ("task_record", task.id.as_str(), serde_json::to_value(task)?, false),
            ("goal_record", goal.id.as_str(), serde_json::to_value(goal)?, false),
            ("plan_record", plan.id.as_str(), serde_json::to_value(plan)?, true),
            ("plan_execution_record", execution.id.as_str(), serde_json::to_value(execution)?, true),
            ("plan_lifecycle_record", lifecycle.id.as_str(), serde_json::to_value(lifecycle)?, true),
        ] {
            if schema { payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION); }
            transaction.execute(&format!("INSERT INTO {table}(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload"), params![id, session.id, encode(&payload)?])?;
        }
        transaction.execute("UPDATE session_record SET payload=?2,updated_at_ms=?3 WHERE id=?1", params![session.id,encode_session(session)?,session.updated_at_ms])?;
        transaction.commit()?;
        Ok(())
    }

    /// Records an intent before waiting for execution ownership. Duplicate IDs never replay.
    pub fn admit_task_operation(&mut self, operation: &crate::task::TaskOperation) -> Result<bool> {
        let changed = self.connection.execute(
            "INSERT OR IGNORE INTO task_operation(id,session_id,payload) VALUES(?1,?2,?3)",
            params![operation.id, operation.session_id, encode(operation)?])?;
        if changed == 0 {
            let previous = self.load_task_operation(&operation.session_id, &operation.id)?.context("operation ID belongs to another conversation")?;
            anyhow::ensure!(previous.action == operation.action, "operation ID was reused for a different action");
        }
        Ok(changed != 0)
    }

    /// Claims one admitted intent without allowing duplicate requests to execute it again.
    pub fn claim_task_operation(&mut self, session_id: &str, id: &str) -> Result<bool> {
        Ok(self.connection.execute(
            "UPDATE task_operation SET payload=json_set(payload,'$.state','accepted') WHERE session_id=?1 AND id=?2 AND json_extract(payload,'$.state')='admitted'",
            params![session_id, id])? == 1)
    }

    /// Queries an intent by its original identity after uncertain delivery.
    pub fn load_task_operation(&self, session_id: &str, id: &str) -> Result<Option<crate::task::TaskOperation>> {
        self.load_payload("SELECT payload FROM task_operation WHERE session_id=?1 AND id=?2", params![session_id,id])
    }

    /// Publishes a settled intent without dispatching its action again.
    pub fn save_task_operation(&mut self, operation: &crate::task::TaskOperation) -> Result<()> {
        self.save_scoped_payload("task_operation", &operation.id, &operation.session_id, operation)
    }

    /// Tests whether a later intent has superseded this pending activation.
    pub fn latest_task_operation(&self, session_id: &str) -> Result<Option<crate::task::TaskOperation>> {
        self.load_payload("SELECT payload FROM task_operation WHERE session_id=?1 ORDER BY rowid DESC LIMIT 1", [session_id])
    }

    /// Open durable storage for the current Harness format.
    pub fn open(data_root: &Path) -> Result<Self> {
        fs::create_dir_all(data_root)
            .with_context(|| format!("create Harness data directory {}", data_root.display()))?;
        let objects = objects::ObjectStore::open(data_root)?;
        let mut connection = Connection::open(data_root.join("harness.sqlite3"))?;
        connection.busy_timeout(Duration::from_secs(5))?;
        connection.pragma_update(None, "journal_mode", "WAL")?;
        connection.pragma_update(None, "synchronous", "FULL")?;
        connection.pragma_update(None, "foreign_keys", "ON")?;
        connection.execute_batch(
            r#"
            CREATE TABLE IF NOT EXISTS preference_record (
                workspace TEXT NOT NULL,
                backend TEXT NOT NULL,
                payload TEXT NOT NULL,
                PRIMARY KEY(workspace, backend)
            );
            "#,
        )?;
        connection.execute_batch(
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
            CREATE TABLE IF NOT EXISTS task_operation (
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
            CREATE TABLE IF NOT EXISTS agent_run_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );

            CREATE TABLE IF NOT EXISTS session_event_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            "#,
        )?;
        tool_output::initialize(&mut connection)?;
        Ok(Self {
            connection,
            session_lock: Default::default(),
            data_root: data_root.to_owned(),
            objects,
        })
    }

    /// Load the last model controls selected for one backend and workspace.
    pub fn load_preference(
        &self,
        workspace: &str,
        backend: &str,
    ) -> Result<Option<HarnessPreference>> {
        let payload: Option<serde_json::Value> = self.load_payload(
            "SELECT payload FROM preference_record WHERE workspace = ?1 AND backend = ?2",
            params![workspace, backend],
        )?;
        let Some(payload) = payload.filter(|payload| payload["format_version"] == SESSION_FORMAT_VERSION) else {
            return Ok(None);
        };
        Ok(Some(serde_json::from_value(payload["preference"].clone())?))
    }

    /// Persist the last model controls selected for one backend and workspace.
    pub fn save_preference(
        &mut self,
        workspace: &str,
        backend: &str,
        preference: &HarnessPreference,
    ) -> Result<()> {
        self.connection.execute(
            "INSERT INTO preference_record(workspace, backend, payload) VALUES(?1, ?2, ?3) \
             ON CONFLICT(workspace, backend) DO UPDATE SET payload=excluded.payload",
            params![workspace, backend, encode(&serde_json::json!({"format_version":SESSION_FORMAT_VERSION,"preference":preference}))?],
        )?;
        Ok(())
    }

    /// Acquire one session lease inside an immediate SQLite transaction.
    pub fn acquire_session_lease(
        &mut self,
        session_id: &str,
        client_id: &str,
        now_ms: i64,
    ) -> Result<HarnessSession> {
        self.lock_session(session_id)?;
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let payload: String = transaction
            .query_row(
                "SELECT payload FROM session_record WHERE id=?1",
                [session_id],
                |row| row.get(0),
            )
            .with_context(|| format!("load Harness session lease {session_id}"))?;
        let mut session = decode_current_session(&payload)?
            .with_context(|| format!("Harness session {session_id} uses an outdated format"))?;
        discard_incompatible_plan_state(&transaction, &mut session)?;
        session.lease_owner = None;
        session.lease_expires_at_ms = None;
        session.acquire_lease(client_id, now_ms)?;
        transaction.execute(
            "UPDATE session_record SET payload=?2 WHERE id=?1",
            params![session_id, encode_session(&session)?],
        )?;
        transaction.commit()?;
        Ok(session)
    }

    /// Release one session lease only when the requesting client still owns it.
    pub fn release_session_lease(&mut self, session_id: &str, client_id: &str) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let payload: Option<String> = transaction
            .query_row(
                "SELECT payload FROM session_record WHERE id=?1",
                [session_id],
                |row| row.get(0),
            )
            .optional()?;
        if let Some(payload) = payload
            && let Some(mut session) = decode_current_session(&payload)?
            && session.lease_owner.as_deref() == Some(client_id)
        {
            session.lease_owner = None;
            session.lease_expires_at_ms = None;
            transaction.execute(
                "UPDATE session_record SET payload=?2 WHERE id=?1",
                params![session_id, encode_session(&session)?],
            )?;
        }
        transaction.commit()?;
        self.session_lock.remove(session_id);
        Ok(())
    }

    /// Renew one lease only while the requesting client remains its owner.
    pub fn renew_session_lease(
        &mut self,
        session_id: &str,
        client_id: &str,
        now_ms: i64,
    ) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let payload: String = transaction.query_row(
            "SELECT payload FROM session_record WHERE id=?1",
            [session_id],
            |row| row.get(0),
        )?;
        let mut session = decode_current_session(&payload)?
            .with_context(|| format!("Harness session {session_id} uses an outdated format"))?;
        anyhow::ensure!(
            session.lease_owner.as_deref() == Some(client_id),
            "Harness session lease ownership changed"
        );
        session.lease_expires_at_ms = Some(now_ms + 30_000);
        transaction.execute(
            "UPDATE session_record SET payload=?2 WHERE id=?1",
            params![session_id, encode_session(&session)?],
        )?;
        transaction.commit()?;
        Ok(())
    }

    /// Save active session state only while the requesting client owns its lease.
    pub fn save_owned_session(&mut self, session: &HarnessSession, client_id: &str) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let payload: Option<String> = transaction
            .query_row(
                "SELECT payload FROM session_record WHERE id=?1",
                [&session.id],
                |row| row.get::<_, String>(0),
            )
            .optional()?;
        let owner = match payload {
            Some(payload) => {
                decode_current_session(&payload)?.and_then(|stored| stored.lease_owner)
            }
            None => None,
        };
        anyhow::ensure!(
            owner.as_deref() == Some(client_id),
            "Harness session lease ownership changed"
        );
        transaction.execute(
            "UPDATE session_record SET workspace=?2, updated_at_ms=?3, payload=?4 WHERE id=?1",
            params![
                session.id,
                session.workspace,
                session.updated_at_ms,
                encode_session(session)?
            ],
        )?;
        transaction.commit()?;
        Ok(())
    }

    /// Persist an exchange and admit its delegated child requests atomically.
    pub fn save_exchange(&mut self, interaction: &Exchange) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let belongs: bool = transaction.query_row(
            "SELECT EXISTS(SELECT 1 FROM agent_run_record WHERE id=?1 AND session_id=?2)",
            params![interaction.agent_id, interaction.session_id],
            |row| row.get(0),
        )?;
        anyhow::ensure!(belongs, "exchange agent does not belong to its session");
        let written = transaction.execute(
            "INSERT INTO exchange_record(id, session_id, agent_id, ordinal, payload) VALUES(?1, ?2, ?3, ?4, ?5)\
             ON CONFLICT(id) DO UPDATE SET ordinal=excluded.ordinal, payload=excluded.payload \
             WHERE exchange_record.session_id=excluded.session_id AND exchange_record.agent_id IS excluded.agent_id",
            params![interaction.id, interaction.session_id, interaction.agent_id, interaction.ordinal as i64, tool_output::encode(interaction)?],
        )?;
        anyhow::ensure!(
            written == 1,
            "an exchange cannot change its owning session or agent"
        );
        tool_output::synchronize(&transaction, interaction)?;
        for node in &interaction.node_list {
            let crate::exchange::ExchangeNode::AgentReference { agent } = node else {
                continue;
            };
            let existing: Option<(String, String)> = transaction
                .query_row(
                    "SELECT session_id, agent_id FROM exchange_record WHERE id=?1",
                    [&agent.child_exchange_id],
                    |row| Ok((row.get(0)?, row.get(1)?)),
                )
                .optional()?;
            if let Some((session_id, agent_id)) = existing {
                anyhow::ensure!(
                    session_id == interaction.session_id && agent_id == agent.child_agent_id,
                    "delegation cannot change its child exchange owner"
                );
                continue;
            }
            let belongs: bool = transaction.query_row(
                "SELECT EXISTS(SELECT 1 FROM agent_run_record WHERE id=?1 AND session_id=?2)",
                params![agent.child_agent_id, interaction.session_id],
                |row| row.get(0),
            )?;
            anyhow::ensure!(belongs, "delegated agent does not belong to its session");
            let ordinal: i64 = transaction.query_row(
                "SELECT COALESCE(MAX(ordinal),0)+1 FROM exchange_record WHERE agent_id=?1",
                [&agent.child_agent_id],
                |row| row.get(0),
            )?;
            let mut child = Exchange::delegated(&interaction.session_id, agent, ordinal as u64);
            child.mode = interaction.mode;
            transaction.execute(
                "INSERT INTO exchange_record(id, session_id, agent_id, ordinal, payload) VALUES(?1,?2,?3,?4,?5)",
                params![child.id, child.session_id, child.agent_id, ordinal, encode(&child)?],
            )?;
        }
        transaction.commit()?;
        Ok(())
    }

    /// Remove one provisional interaction after its provider turn is retracted.
    pub fn delete_exchange(&mut self, interaction_id: &str) -> Result<()> {
        self.connection
            .execute("DELETE FROM exchange_record WHERE id=?1", [interaction_id])?;
        Ok(())
    }

    /// Load interactions in their admitted user-action order.
    pub fn list_exchange(&self, session_id: &str) -> Result<Vec<Exchange>> {
        self.list_exchange_payload(
            "SELECT payload FROM exchange_record WHERE session_id=?1 AND agent_id=?2 ORDER BY ordinal",
            params![session_id, HarnessSession::primary_agent_id(session_id)],
        )
    }

    /// Load the latest interaction ordinal for one session.
    pub fn next_exchange_ordinal(&self, session_id: &str) -> Result<u64> {
        let ordinal: Option<i64> = self.connection.query_row(
            "SELECT MAX(ordinal) FROM exchange_record WHERE session_id=?1 AND agent_id=?2",
            params![session_id, HarnessSession::primary_agent_id(session_id)],
            |row| row.get(0),
        )?;
        Ok(ordinal.unwrap_or(0) as u64 + 1)
    }

    /// Write a diff annotation for later request-changes prompts.
    pub fn save_exchange_comment(&mut self, comment: &ExchangeComment) -> Result<()> {
        self.connection.execute(
            "INSERT INTO exchange_comment(id, interaction_id, payload) VALUES(?1, ?2, ?3)\
             ON CONFLICT(id) DO UPDATE SET payload=excluded.payload",
            params![comment.id, comment.exchange_id, encode(comment)?],
        )?;
        Ok(())
    }

    /// Load annotations for one historical interaction review.
    pub fn list_exchange_comment(&self, exchange_id: &str) -> Result<Vec<ExchangeComment>> {
        self.list_payload(
            "SELECT payload FROM exchange_comment WHERE interaction_id=?1 ORDER BY rowid",
            [exchange_id],
        )
    }

    /// Commit execution state, its goal, and an optional accepted revision together.
    pub fn save_execution_transition(&mut self, execution: &PlanExecutionRecord, goal: &GoalRecord, plan: Option<&PlanRecord>) -> Result<()> {
        let mut execution_payload = serde_json::to_value(execution)?;
        execution_payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
        let mut rows = vec![("plan_execution_record", execution.id.as_str(), execution_payload),
            ("goal_record", goal.id.as_str(), serde_json::to_value(goal)?)];
        if let Some(plan) = plan {
            let mut payload = serde_json::to_value(plan)?;
            payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
            rows.push(("plan_record", plan.id.as_str(), payload));
        }
        let transaction = self.connection.transaction()?;
        for (table, id, payload) in rows {
            transaction.execute(&format!("INSERT INTO {table}(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload"), params![id,execution.session_id,encode(&payload)?])?;
        }
        transaction.commit()?;
        Ok(())
    }

    /// Commit an approved scope change and its reconciled scheduler together.
    pub fn save_plan_transition(
        &mut self,
        deviation: &crate::plan::PlanDeviation,
        execution: &PlanExecutionRecord,
    ) -> Result<()> {
        let mut deviation_payload = serde_json::to_value(deviation)?;
        let mut execution_payload = serde_json::to_value(execution)?;
        for payload in [&mut deviation_payload, &mut execution_payload] {
            payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
        }
        let transaction = self.connection.transaction()?;
        for (table, id, payload) in [
            ("plan_deviation_record", &deviation.id, deviation_payload),
            ("plan_execution_record", &execution.id, execution_payload),
        ] {
            transaction.execute(&format!("INSERT INTO {table}(id, session_id, payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload"),
                params![id, execution.session_id, encode(&payload)?])?;
        }
        transaction.commit()?;
        Ok(())
    }

    fn save_plan_payload<T: Serialize>(
        &mut self,
        table: &str,
        id: &str,
        session_id: &str,
        value: &T,
    ) -> Result<()> {
        let mut payload = serde_json::to_value(value)?;
        payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
        self.save_scoped_payload(table, id, session_id, &payload)
    }

    /// Write a plan lifecycle record.
    pub fn save_plan(&mut self, plan: &PlanRecord) -> Result<()> {
        self.save_plan_payload("plan_record", &plan.id, &plan.session_id, plan)
    }

    /// Load a plan by stable Harness identifier.
    pub fn load_plan(&self, plan_id: &str) -> Result<Option<PlanRecord>> {
        self.load_payload("SELECT payload FROM plan_record WHERE id=?1 AND json_extract(payload, '$.schema_version')=8", [plan_id])
    }

    /// Load every plan artifact for one session in creation order.
    pub fn list_plan(&self, session_id: &str) -> Result<Vec<PlanRecord>> {
        self.list_payload(
            "SELECT payload FROM plan_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=8 ORDER BY rowid",
            [session_id],
        )
    }

    /// Remove one retracted canonical plan record.
    pub fn delete_plan(&mut self, plan_id: &str) -> Result<()> {
        self.connection
            .execute("DELETE FROM plan_record WHERE id=?1", [plan_id])?;
        Ok(())
    }

    /// Write one immutable plan lifecycle event.
    pub fn save_plan_lifecycle(&mut self, lifecycle: &PlanLifecycleRecord) -> Result<()> {
        self.save_plan_payload(
            "plan_lifecycle_record",
            &lifecycle.id,
            &lifecycle.session_id,
            lifecycle,
        )
    }

    /// Load plan lifecycle events in their insertion order.
    pub fn list_plan_lifecycle(&self, session_id: &str) -> Result<Vec<PlanLifecycleRecord>> {
        self.list_payload(
            "SELECT payload FROM plan_lifecycle_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=8 ORDER BY rowid",
            [session_id],
        )
    }

    /// Write one immutable session-level timeline event.
    pub fn save_session_event(&mut self, event: &SessionEventRecord) -> Result<()> {
        self.save_scoped_payload("session_event_record", &event.id, &event.session_id, event)
    }

    /// Load session-level timeline events in their insertion order.
    pub fn list_session_event(&self, session_id: &str) -> Result<Vec<SessionEventRecord>> {
        self.list_payload(
            "SELECT payload FROM session_event_record WHERE session_id=?1 ORDER BY rowid",
            [session_id],
        )
    }

    /// Remove one provisional lifecycle event after its owning backend turn fails.
    pub fn delete_plan_lifecycle(&mut self, lifecycle_id: &str) -> Result<()> {
        self.connection.execute(
            "DELETE FROM plan_lifecycle_record WHERE id=?1",
            [lifecycle_id],
        )?;
        Ok(())
    }

    /// Write one accepted-plan execution record.
    pub fn save_plan_execution(&mut self, execution: &PlanExecutionRecord) -> Result<()> {
        self.save_plan_payload(
            "plan_execution_record",
            &execution.id,
            &execution.session_id,
            execution,
        )
    }

    /// Load one accepted-plan execution by stable identifier.
    pub fn load_plan_execution(&self, execution_id: &str) -> Result<Option<PlanExecutionRecord>> {
        self.load_payload(
            "SELECT payload FROM plan_execution_record WHERE id=?1 AND json_extract(payload, '$.schema_version')=8",
            [execution_id],
        )
    }

    /// Load every accepted-plan execution for one session.
    pub fn list_plan_execution(&self, session_id: &str) -> Result<Vec<PlanExecutionRecord>> {
        self.list_payload(
            "SELECT payload FROM plan_execution_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=8 ORDER BY rowid",
            [session_id],
        )
    }

    /// Write one execution-time plan deviation.
    pub fn save_plan_deviation(
        &mut self,
        session_id: &str,
        deviation: &crate::plan::PlanDeviation,
    ) -> Result<()> {
        self.save_plan_payload(
            "plan_deviation_record",
            &deviation.id,
            session_id,
            deviation,
        )
    }

    /// Load every plan deviation for one session in chronological order.
    pub fn list_plan_deviation(&self, session_id: &str) -> Result<Vec<crate::plan::PlanDeviation>> {
        self.list_payload(
            "SELECT payload FROM plan_deviation_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=8 ORDER BY rowid",
            [session_id],
        )
    }

    /// Write one final accepted-plan audit.
    pub fn save_plan_audit(
        &mut self,
        session_id: &str,
        audit: &crate::plan::PlanAudit,
    ) -> Result<()> {
        self.save_plan_payload("plan_audit_record", &audit.id, session_id, audit)
    }

    /// Load every plan audit for one session in chronological order.
    pub fn list_plan_audit(&self, session_id: &str) -> Result<Vec<crate::plan::PlanAudit>> {
        self.list_payload(
            "SELECT payload FROM plan_audit_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=8 ORDER BY rowid",
            [session_id],
        )
    }

    /// Write one chronological terminal plan resolution.
    pub fn save_plan_resolution(
        &mut self,
        resolution: &crate::plan::PlanResolutionRecord,
    ) -> Result<()> {
        self.save_plan_payload(
            "plan_resolution_record",
            &resolution.id,
            &resolution.session_id,
            resolution,
        )
    }

    /// Load every terminal plan resolution for one session in chronological order.
    pub fn list_plan_resolution(
        &self,
        session_id: &str,
    ) -> Result<Vec<crate::plan::PlanResolutionRecord>> {
        self.list_payload(
            "SELECT payload FROM plan_resolution_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=8 ORDER BY rowid",
            [session_id],
        )
    }

    /// Write a goal lifecycle record.
    pub fn save_goal(&mut self, goal: &GoalRecord) -> Result<()> {
        self.save_scoped_payload("goal_record", &goal.id, &goal.session_id, goal)
    }

    /// Load a goal by stable Harness identifier.
    pub fn load_goal(&self, goal_id: &str) -> Result<Option<GoalRecord>> {
        self.load_payload("SELECT payload FROM goal_record WHERE id=?1", [goal_id])
    }

    /// Remove one provisional goal created by a retracted prompt.
    pub fn delete_goal(&mut self, goal_id: &str) -> Result<()> {
        self.connection
            .execute("DELETE FROM goal_record WHERE id=?1", [goal_id])?;
        Ok(())
    }

    /// Write one immutable workspace checkpoint record.
    pub fn save_checkpoint(&mut self, checkpoint: &CheckpointRecord) -> Result<()> {
        self.save_scoped_payload(
            "checkpoint_record",
            &checkpoint.id,
            &checkpoint.session_id,
            checkpoint,
        )
    }

    /// Publish a checkpoint and its owning exchange as one durable transition.
    pub fn save_checkpoint_exchange(
        &mut self,
        checkpoint: &CheckpointRecord,
        exchange: &Exchange,
    ) -> Result<()> {
        anyhow::ensure!(
            checkpoint.session_id == exchange.session_id,
            "checkpoint and exchange belong to different sessions"
        );
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let belongs: bool = transaction.query_row(
            "SELECT EXISTS(SELECT 1 FROM agent_run_record WHERE id=?1 AND session_id=?2)",
            params![exchange.agent_id, exchange.session_id],
            |row| row.get(0),
        )?;
        anyhow::ensure!(belongs, "exchange agent does not belong to its session");
        transaction.execute(
            "INSERT INTO checkpoint_record(id, session_id, payload) VALUES(?1, ?2, ?3)\
             ON CONFLICT(id) DO UPDATE SET payload=excluded.payload \
             WHERE checkpoint_record.session_id=excluded.session_id",
            params![checkpoint.id, checkpoint.session_id, encode(checkpoint)?],
        )?;
        let written = transaction.execute(
            "INSERT INTO exchange_record(id, session_id, agent_id, ordinal, payload) VALUES(?1, ?2, ?3, ?4, ?5)\
             ON CONFLICT(id) DO UPDATE SET ordinal=excluded.ordinal, payload=excluded.payload \
             WHERE exchange_record.session_id=excluded.session_id AND exchange_record.agent_id IS excluded.agent_id",
            params![exchange.id, exchange.session_id, exchange.agent_id, exchange.ordinal as i64, tool_output::encode(exchange)?],
        )?;
        anyhow::ensure!(
            written == 1,
            "an exchange cannot change its owning session or agent"
        );
        tool_output::synchronize(&transaction, exchange)?;
        transaction.commit()?;
        Ok(())
    }

    /// Load one workspace checkpoint by content digest.
    pub fn load_checkpoint(&self, checkpoint_id: &str) -> Result<Option<CheckpointRecord>> {
        self.load_payload(
            "SELECT payload FROM checkpoint_record WHERE id=?1",
            [checkpoint_id],
        )
    }

    /// Record one globally shared prompt and retain only the newest bounded history.
    pub fn record_prompt_history(&mut self, text: &str, created_at_ms: i64) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        transaction.execute(
            "INSERT INTO prompt_history_record(text, created_at_ms) VALUES(?1, ?2)",
            params![text, created_at_ms],
        )?;
        transaction.execute(
            "DELETE FROM prompt_history_record WHERE id NOT IN \
             (SELECT id FROM prompt_history_record ORDER BY id DESC LIMIT 100)",
            [],
        )?;
        transaction.commit()?;
        Ok(())
    }

    /// Load globally shared prompts from newest to oldest.
    pub fn list_prompt_history(&self) -> Result<Vec<String>> {
        let mut statement = self
            .connection
            .prepare("SELECT text FROM prompt_history_record ORDER BY id DESC LIMIT 100")?;
        let row_list = statement.query_map([], |row| row.get::<_, String>(0))?;
        row_list
            .collect::<rusqlite::Result<Vec<_>>>()
            .map_err(Into::into)
    }

    /// Write one concrete child-agent run into its owning Harness session.
    pub fn save_agent_run(&mut self, run: &Agent) -> Result<()> {
        self.save_scoped_payload("agent_run_record", &run.id, &run.session_id, run)
    }

    /// Load child-agent runs in creation order for one Harness session.
    pub fn list_agent_run(&self, session_id: &str) -> Result<Vec<Agent>> {
        self.list_payload(
            "SELECT payload FROM agent_run_record WHERE session_id=?1 ORDER BY rowid",
            [session_id],
        )
    }

    /// Load exchanges in admission order for one child agent.
    pub fn list_agent_exchange(&self, run_id: &str) -> Result<Vec<Exchange>> {
        self.list_exchange_payload(
            "SELECT payload FROM exchange_record WHERE agent_id=?1 ORDER BY ordinal",
            [run_id],
        )
    }

    fn save_scoped_payload<T: Serialize>(
        &mut self,
        table: &str,
        id: &str,
        session_id: &str,
        value: &T,
    ) -> Result<()> {
        let sql = format!(
            "INSERT INTO {table}(id, session_id, payload) VALUES(?1, ?2, ?3) \
             ON CONFLICT(id) DO UPDATE SET session_id=excluded.session_id, payload=excluded.payload"
        );
        self.connection
            .execute(&sql, params![id, session_id, encode(value)?])?;
        Ok(())
    }

    fn load_payload<T: DeserializeOwned, P: rusqlite::Params>(
        &self,
        sql: &str,
        params: P,
    ) -> Result<Option<T>> {
        let payload: Option<String> = self
            .connection
            .query_row(sql, params, |row| row.get(0))
            .optional()?;
        payload.map(|value| decode(&value)).transpose()
    }

    fn list_payload<T: DeserializeOwned, P: rusqlite::Params>(
        &self,
        sql: &str,
        params: P,
    ) -> Result<Vec<T>> {
        let mut statement = self.connection.prepare(sql)?;
        let rows = statement.query_map(params, |row| row.get::<_, String>(0))?;
        rows.map(|row| decode(&row?)).collect()
    }

    fn list_current_session<P: rusqlite::Params>(
        &self,
        sql: &str,
        params: P,
    ) -> Result<Vec<HarnessSession>> {
        let mut statement = self.connection.prepare(sql)?;
        let rows = statement.query_map(params, |row| row.get::<_, String>(0))?;
        let mut session_list = Vec::new();
        for row in rows {
            if let Some(mut session) = decode_current_session(&row?)? {
                discard_incompatible_plan_state(&self.connection, &mut session)?;
                session_list.push(session);
            }
        }
        Ok(session_list)
    }
}

impl SessionStore for SqliteStore {
    fn save_session(&mut self, session: &HarnessSession) -> Result<()> {
        let primary = Agent::primary(&session.id, session.created_at_ms);
        anyhow::ensure!(
            session.primary_agent_id == primary.id,
            "session primary agent identity does not belong to this session"
        );
        let transaction = self.connection.transaction()?;
        transaction.execute(
            "INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES(?1, ?2, ?3, ?4)\
             ON CONFLICT(id) DO UPDATE SET workspace=excluded.workspace, updated_at_ms=excluded.updated_at_ms, payload=excluded.payload",
            params![
                session.id,
                session.workspace,
                session.updated_at_ms,
                encode_session(session)?
            ],
        )?;
        transaction.execute(
            "INSERT INTO agent_run_record(id, session_id, payload) VALUES(?1, ?2, ?3) ON CONFLICT(id) DO NOTHING",
            params![primary.id, primary.session_id, encode(&primary)?],
        )?;
        transaction.commit()?;
        Ok(())
    }

    fn load_session(&self, session_id: &str) -> Result<Option<HarnessSession>> {
        let payload: Option<String> = self
            .connection
            .query_row(
                "SELECT payload FROM session_record WHERE id=?1",
                [session_id],
                |row| row.get(0),
            )
            .optional()?;
        match payload {
            Some(value) => {
                let mut session = decode_current_session(&value)?;
                if let Some(session) = session.as_mut() {
                    discard_incompatible_plan_state(&self.connection, session)?;
                }
                Ok(session)
            }
            None => Ok(None),
        }
    }

    fn list_session(&self, workspace: Option<&str>) -> Result<Vec<HarnessSession>> {
        let mut sessions = self.list_current_session(
            "SELECT payload FROM session_record ORDER BY updated_at_ms DESC", [],
        )?;
        if let Some(path) = workspace {
            sessions.retain(|session| crate::workspace::same(&session.workspace, path));
        }
        Ok(sessions)
    }

    fn delete_session(&mut self, session_id: &str) -> Result<()> {
        self.connection
            .execute("DELETE FROM session_record WHERE id=?1", [session_id])?;
        Ok(())
    }
}

fn encode<T: Serialize>(value: &T) -> Result<String> {
    serde_json::to_string(value).context("encode Harness storage payload")
}

fn decode<T: DeserializeOwned>(value: &str) -> Result<T> {
    serde_json::from_str(value).context("decode Harness storage payload")
}

fn discard_incompatible_plan_state(
    connection: &Connection,
    session: &mut HarnessSession,
) -> Result<()> {
    if let Some(plan_id) = session.active_plan_id.as_ref() {
        let supported: bool = connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM plan_record WHERE id=?1 AND json_extract(payload, '$.schema_version')=8)",
            [plan_id], |row| row.get(0))?;
        if !supported {
            session.active_plan_id = None;
        }
    }

    if let Some(goal_id) = session.goal_id.as_ref() {
        let incompatible: bool = connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM plan_execution_record WHERE session_id=?1 AND json_extract(payload, '$.goal_id')=?2 AND coalesce(json_extract(payload, '$.schema_version'),0)<>8)",
            params![session.id, goal_id], |row| row.get(0))?;
        if incompatible {
            session.goal_id = None;
        }
    }
    Ok(())
}

fn encode_session(session: &HarnessSession) -> Result<String> {
    encode(&SessionEnvelope {
        format_version: SESSION_FORMAT_VERSION,
        session: session.clone(),
    })
}

fn decode_current_session(value: &str) -> Result<Option<HarnessSession>> {
    let payload: serde_json::Value = decode(value)?;
    let format_version = payload
        .get("format_version")
        .and_then(serde_json::Value::as_u64);
    if format_version != Some(u64::from(SESSION_FORMAT_VERSION)) {
        return Ok(None);
    }
    let envelope: SessionEnvelope =
        serde_json::from_value(payload).context("decode current Harness session payload")?;
    Ok(Some(envelope.session))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn streamed_output_reopens_migrates_and_commits_completion_atomically() -> Result<()> {
        use crate::backend::{BackendEvent,ProviderAddress,ToolActivity,ToolActivityKind};
        let directory = tempfile::tempdir()?;
        let mut store = SqliteStore::open(directory.path())?;
        store.save_session(&session("session","D:/work"))?;
        let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":"stream","session_id":"session","agent_id":"session:agent:primary",
            "ordinal":1,"prompt":"read","kind":"chat","state":"running",
            "disposition":"current","turn":[],"node_list":[],"created_at_ms":1,
            "awaiting_input":false,"duration_ms":0,"metrics":{},"comment":[],
            "attributed_matches_checkpoint":false
        }))?;
        let address = ProviderAddress { thread_id:"thread".into(),turn_id:"provider-turn".into() };
        exchange.start_turn(address.clone(),1)?;
        let mut event = BackendEvent {
            received_at_ms: None, address:Some(address),turn_boundary:None,kind:"tool".into(),
            text:None,data:serde_json::json!({}),summary:None,task_update:None,
            activity:Some(ToolActivity { id:"call".into(),kind:ToolActivityKind::Command,
                title:"read".into(),output:None,status:Some("inProgress".into()),
                change:Default::default(),output_delta:false }) };
        exchange.observe_turn(&event,2)?;
        store.save_exchange(&exchange)?;
        event.activity.as_mut().unwrap().output_delta = true;
        for chunk in ["\u{1b}[31", "mλ\r\n", "tail"] {
            event.activity.as_mut().unwrap().output = Some(chunk.into());
            exchange.observe_turn(&event,3)?;
            store.save_tool_output_delta(&exchange,&event)?;
        }
        let expected = "\u{1b}[31mλ\r\ntail";
        assert_eq!(store.list_exchange("session")?[0].turn[0].tools().next().unwrap().output,expected);
        let payload: String = store.connection.query_row("SELECT payload FROM exchange_record WHERE id='stream'",[],|row|row.get(0))?;
        assert!(!payload.contains("tail"));
        drop(store);
        let store = SqliteStore::open(directory.path())?;
        assert_eq!(store.list_exchange("session")?[0].turn[0].tools().next().unwrap().output,expected);
        store.connection.execute("UPDATE exchange_record SET payload=?1 WHERE id='stream'",[encode(&exchange)?])?;
        drop(store);
        let mut store = SqliteStore::open(directory.path())?;
        assert_eq!(store.list_exchange("session")?[0].turn[0].tools().next().unwrap().output,expected);
        event.activity.as_mut().unwrap().output_delta = false;
        event.activity.as_mut().unwrap().output = Some("authoritative λ\n".into());
        event.activity.as_mut().unwrap().status = Some("completed".into());
        exchange.observe_turn(&event,4)?;
        store.connection.execute_batch("CREATE TRIGGER reject_chunk BEFORE INSERT ON tool_output_chunk BEGIN SELECT RAISE(ABORT,'storage failure'); END;")?;
        assert!(store.save_exchange(&exchange).is_err());
        let recovered = store.list_exchange("session")?;
        assert_eq!(recovered[0].turn[0].tools().next().unwrap().output,expected);
        assert_eq!(recovered[0].turn[0].tools().next().unwrap().state(),crate::turn::ToolState::Running);
        store.connection.execute_batch("DROP TRIGGER reject_chunk")?;
        store.save_exchange(&exchange)?;
        assert_eq!(store.list_exchange("session")?[0].turn[0].tools().next().unwrap().output,"authoritative λ\n");
        Ok(())
    }

    #[test]
    fn outdated_plans_do_not_restore_execution_or_block_the_session() {
        let temporary = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(temporary.path()).unwrap();
        let mut stored = session("current", "D:/work");
        stored.active_plan_id = Some("old-plan".into());
        stored.goal_id = Some("old-goal".into());
        store.save_session(&stored).unwrap();
        store
            .connection
            .execute(
                "INSERT INTO plan_record(id,session_id,payload) VALUES('old-plan','current',?1)",
                [r#"{"id":"old-plan","schema_version":4}"#],
            )
            .unwrap();
        store.connection.execute("INSERT INTO plan_execution_record(id,session_id,payload) VALUES('old-execution','current',?1)",
            [r#"{"id":"old-execution","goal_id":"old-goal","schema_version":4}"#]).unwrap();
        assert!(store.list_plan("current").unwrap().is_empty());
        assert!(store.list_plan_execution("current").unwrap().is_empty());
        let reopened = store.load_session("current").unwrap().unwrap();
        assert!(reopened.active_plan_id.is_none() && reopened.goal_id.is_none());
        assert_eq!(reopened.name, "current");
        let leased = store
            .acquire_session_lease("current", "client", 10)
            .unwrap();
        assert!(leased.active_plan_id.is_none() && leased.goal_id.is_none());
    }

    fn session(id: &str, workspace: &str) -> HarnessSession {
        HarnessSession {
            id: id.into(),
            primary_agent_id: HarnessSession::primary_agent_id(id),
            name: id.into(),
            workspace: workspace.into(),
            backend: "copilot".into(),
            backend_session_id: None,
            provider_checkpoint_id: None,
            provider_fork_state: crate::session::ProviderForkState::Ready,
            model: "default".into(),
            provider_label: "Copilot CLI".into(),
            resolved_model: None,
            effort: "medium".into(),
            plan_executor: Default::default(),
            plan_compact: false,
            plan_auto_approve_revisions: true,
            context_window: None,
            service_tier: crate::backend::ServiceTier::Standard,
            access: Default::default(),
            execution_mode: crate::session::PermissionMode::Read,
            current_task_id: None, default_write_permission: crate::session::PermissionMode::Write, plan_permission: None,
            created_at_ms: 1,
            updated_at_ms: 1,
            active_plan_id: None,
            goal_id: None,
            lease_owner: None,
            lease_expires_at_ms: None,
            native_fork: false,
            native_compact: false,
            context_usage: None,
        }
    }

    #[test]
    fn filters_sessions_by_exact_workspace() {
        let temporary = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(temporary.path()).unwrap();
        for (id, workspace) in [("one", "D:/one"), ("two", "D:/two")] {
            store.save_session(&session(id, workspace)).unwrap();
        }
        assert_eq!(store.list_session(Some("D:/one")).unwrap().len(), 1);
        assert_eq!(store.list_session(None).unwrap().len(), 2);
        let workspace = tempfile::tempdir().unwrap();
        let path = workspace.path().to_string_lossy();
        store.save_session(&session("alternate", &format!("{path}//./"))).unwrap();
        assert_eq!(store.list_session(Some(&path)).unwrap()[0].id, "alternate");
    }

    #[test]
    fn shares_only_the_newest_hundred_prompts() {
        let temporary = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(temporary.path()).unwrap();
        for index in 0..105 {
            store
                .record_prompt_history(&format!("prompt-{index}"), index)
                .unwrap();
        }

        let history = store.list_prompt_history().unwrap();
        assert_eq!(history.len(), 100);
        assert_eq!(history.first().map(String::as_str), Some("prompt-104"));
        assert_eq!(history.last().map(String::as_str), Some("prompt-5"));

        let reopened = SqliteStore::open(temporary.path()).unwrap();
        assert_eq!(reopened.list_prompt_history().unwrap(), history);
    }

    #[test]
    fn recovery_is_atomic_and_preserves_waiting_exchanges() {
        use crate::exchange::{ExchangeKind, ExchangeState, HistoryDisposition};
        let temporary = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(temporary.path()).unwrap();
        store.save_session(&session("session", "D:/work")).unwrap();
        store.save_agent_run(&Agent::primary("session", 0)).unwrap();
        let mut exchange = Exchange {
            lifecycle: None,
            finalization_error: None,
            finalization_outcome: None,
            agent_id: "session:agent:primary".into(),
            id: "active".into(),
            session_id: "session".into(),
            ordinal: 1,
            prompt: "work".into(),
            kind: ExchangeKind::Chat,
            mode: None,
            state: ExchangeState::Running,
            plan_id: None,
            execution_id: None,
            goal_id: None,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            disposition: HistoryDisposition::Current,
            turn: Vec::new(),
            created_at_ms: 10,
            completed_at_ms: None,
            node_list: Vec::new(),
            awaiting_input: false,
            elicitation: None,
            duration_ms: 20,
            execution_started_at_ms: Some(30),
            metrics: crate::exchange::ExchangeMetrics::default(),
            comment: Vec::new(),
            task: None,
        };
        exchange.observe_blocker("approval:recovery".into(), true, 70);
        store.save_exchange(&exchange).unwrap();
        exchange.id = "waiting".into();
        exchange.ordinal = 2;
        exchange.awaiting_input = true;
        exchange.execution_started_at_ms = None;
        store.save_exchange(&exchange).unwrap();
        let child = Agent::pending("session", "reviewer", "review", 10);
        store.save_agent_run(&child).unwrap();
        store
            .connection
            .execute(
                "UPDATE agent_run_record SET payload='invalid' WHERE id=?1",
                [&child.id],
            )
            .unwrap();
        assert!(store.interrupt_detached_execution("session").is_err());
        let unchanged = store.list_exchange("session").unwrap();
        assert!(
            unchanged
                .iter()
                .all(|exchange| exchange.state == ExchangeState::Running)
        );
        assert_eq!(unchanged[0].execution_started_at_ms, Some(30));
        store.save_agent_run(&child).unwrap();
        store.interrupt_detached_execution("session").unwrap();
        let recovered = store.list_exchange("session").unwrap();
        assert_eq!(recovered[0].state, ExchangeState::Interrupted);
        assert_eq!(recovered[0].elapsed(90_000), 60);
        assert_eq!(recovered[0].completed_at_ms, Some(70));
        assert_eq!(recovered[1].state, ExchangeState::Running);
        assert!(recovered[1].awaiting_input);
        assert_eq!(recovered[1].elapsed(90_000), 20);
        let snapshot = serde_json::to_value(&recovered).unwrap();
        store.interrupt_detached_execution("session").unwrap();
        assert_eq!(
            serde_json::to_value(store.list_exchange("session").unwrap()).unwrap(),
            snapshot
        );
    }

    #[test]
    fn persists_canonical_exchanges_with_immutable_agent_ownership() {
        let temporary = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(temporary.path()).unwrap();
        store.save_session(&session("session", "D:/work")).unwrap();
        store.save_agent_run(&Agent::primary("session", 0)).unwrap();
        let first = Agent::pending("session", "explorer", "inspect Bevy", 1);
        let second = Agent::pending("session", "explorer", "inspect physics", 2);
        store.save_agent_run(&first).unwrap();
        store.save_agent_run(&second).unwrap();
        let mut interaction = Exchange {
            lifecycle: None,
            finalization_error: None,
            finalization_outcome: None,
            agent_id: "session:agent:primary".into(),
            id: "interaction".into(),
            session_id: "session".into(),
            ordinal: 1,
            prompt: "report".into(),
            kind: crate::exchange::ExchangeKind::Chat,
            mode: None,
            state: crate::exchange::ExchangeState::Complete,
            plan_id: None,
            execution_id: None,
            goal_id: None,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            disposition: crate::exchange::HistoryDisposition::Current,
            turn: Vec::new(),
            created_at_ms: 3,
            completed_at_ms: Some(4),
            node_list: Vec::new(),
            awaiting_input: false,
            elicitation: None,
            duration_ms: 1,
            execution_started_at_ms: None,
            metrics: crate::exchange::ExchangeMetrics::default(),
            comment: Vec::new(),
            task: None,
        };
        interaction.agent_id = first.id.clone();
        store.save_exchange(&interaction).unwrap();

        assert_eq!(store.list_agent_run("session").unwrap().len(), 3);
        assert_eq!(store.list_agent_exchange(&first.id).unwrap().len(), 1);
        assert!(store.list_agent_exchange(&second.id).unwrap().is_empty());
        assert!(store.list_exchange("session").unwrap().is_empty());
        let mut primary = interaction.clone();
        primary.agent_id = "session:agent:primary".into();
        primary.id = "primary".into();
        store.save_exchange(&primary).unwrap();
        assert_eq!(store.list_exchange("session").unwrap().len(), 1);
        assert_eq!(store.next_exchange_ordinal("session").unwrap(), 2);
        interaction.agent_id = second.id.clone();
        assert!(
            store.save_exchange(&interaction).is_err(),
            "exchange owner changed"
        );
        assert!(store.list_agent_exchange(&second.id).unwrap().is_empty());
        interaction.id = "foreign-owner".into();
        interaction.session_id = "another-session".into();
        store
            .save_session(&session("another-session", "D:/work"))
            .unwrap();
        assert!(
            store.save_exchange(&interaction).is_err(),
            "cross-session child accepted"
        );
    }

    #[test]
    fn lease_transactions_reject_stale_session_writers() {
        let temporary = tempfile::tempdir().unwrap();
        let mut first = SqliteStore::open(temporary.path()).unwrap();
        first.save_session(&session("session", "D:/work")).unwrap();
        let owned = first
            .acquire_session_lease("session", "client-one", 10)
            .unwrap();
        let mut second = SqliteStore::open(temporary.path()).unwrap();
        assert!(
            second
                .acquire_session_lease("session", "client-two", 20)
                .is_err()
        );
        first
            .renew_session_lease("session", "client-one", 25)
            .unwrap();
        assert!(second.acquire_session_lease("session", "client-two", 30_026).is_err(),
            "lease expiry must not replace a live runtime owner");
        first.release_session_lease("session", "client-one").unwrap();
        let replacement = second
            .acquire_session_lease("session", "client-two", 30_026)
            .unwrap();
        assert_eq!(replacement.lease_owner.as_deref(), Some("client-two"));
        assert!(first.save_owned_session(&owned, "client-one").is_err());
        first
            .release_session_lease("session", "client-one")
            .unwrap();
        assert_eq!(
            second
                .load_session("session")
                .unwrap()
                .unwrap()
                .lease_owner
                .as_deref(),
            Some("client-two")
        );
    }

    #[test]
    fn reopens_current_sessions_with_ordered_plan_deviations() {
        let temporary = tempfile::tempdir().unwrap();
        {
            let mut store = SqliteStore::open(temporary.path()).unwrap();
            store.save_session(&session("current", "D:/work")).unwrap();
            let document = crate::plan::test_fixture("plan", "Overview");
            let entity = document.entity_changes[0].clone();
            store
                .save_plan_deviation(
                    "current",
                    &crate::plan::PlanDeviation {
                        id: "deviation".into(),
                        plan_id: "plan".into(),
                        execution_id: "execution".into(),
                        kind: crate::plan::PlanDeviationKind::Scope,
                        disposition: crate::plan::PlanDeviationDisposition::AutoApproved,
                        summary: "Extend the accepted entity.".into(),
                        reason: "Repository evidence requires the additional change.".into(),
                        task_path: None,
                        subtask_path: None,
                        affected_paths: Vec::new(),
                        proposed_changes: crate::plan::PlanMutation {
                            set: Some(crate::plan::PlanResourceSet {
                                entity_changes: Some(vec![entity]),
                                ..Default::default()
                            }),
                            ..Default::default()
                        },
                        created_at_ms: 1,
                        resolved_at_ms: Some(1),
                    },
                )
                .unwrap();
        }

        let store = SqliteStore::open(temporary.path()).unwrap();
        assert_eq!(
            store.load_session("current").unwrap().unwrap().id,
            "current"
        );
        let deviation_list = store.list_plan_deviation("current").unwrap();
        let entry_list = deviation_list[0]
            .proposed_changes
            .set
            .as_ref()
            .and_then(|set| set.entity_changes.as_ref())
            .unwrap();
        assert!(!entry_list[0].name.is_empty());
    }

    #[test]
    fn hides_outdated_sessions_without_deleting_preferences() {
        let temporary = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(temporary.path()).unwrap();
        let preference = HarnessPreference {
            access: Default::default(),
            default_write_permission: crate::session::PermissionMode::Write,
            plan_permission: None,
            model: "remembered-model".into(),
            effort: "low".into(),
            model_setting: Default::default(),
            service_tier: crate::backend::ServiceTier::Fast,
            plan_executor: Default::default(),
            plan_compact: false,
            plan_auto_approve_revisions: true,
        };
        store
            .save_preference("D:/work", "codex", &preference)
            .unwrap();
        store
            .connection
            .execute(
                "INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES(?1, ?2, ?3, ?4)",
                params![
                    "outdated",
                    "D:/work",
                    1,
                    encode(&serde_json::json!({
                        "format_version": SESSION_FORMAT_VERSION - 1,
                        "session": session("outdated", "D:/work")
                    }))
                    .unwrap()
                ],
            )
            .unwrap();
        store
            .connection
            .execute(
                "INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES(?1, ?2, ?3, ?4)",
                params![
                    "future",
                    "D:/work",
                    2,
                    encode(&serde_json::json!({
                        "format_version": SESSION_FORMAT_VERSION + 1,
                        "session": session("future", "D:/work")
                    }))
                    .unwrap()
                ],
            )
            .unwrap();
        store
            .connection
            .execute(
                "INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES(?1, ?2, ?3, ?4)",
                params![
                    "unversioned",
                    "D:/work",
                    3,
                    encode(&session("unversioned", "D:/work")).unwrap()
                ],
            )
            .unwrap();
        store.save_session(&session("current", "D:/work")).unwrap();

        assert!(store.load_session("outdated").unwrap().is_none());
        assert!(store.load_session("future").unwrap().is_none());
        assert!(store.load_session("unversioned").unwrap().is_none());
        assert_eq!(
            store
                .list_session(Some("D:/work"))
                .unwrap()
                .into_iter()
                .map(|session| session.id)
                .collect::<Vec<_>>(),
            vec!["current"]
        );
        assert_eq!(
            store
                .load_preference("D:/work", "codex")
                .unwrap()
                .unwrap()
                .model,
            "remembered-model"
        );
        let outdated_count: u32 = store
            .connection
            .query_row(
                "SELECT COUNT(*) FROM session_record WHERE id='outdated'",
                [],
                |row| row.get(0),
            )
            .unwrap();
        assert_eq!(outdated_count, 1);
    }

    #[test]
    fn reopens_sessions_written_by_the_current_format() {
        let temporary = tempfile::tempdir().unwrap();
        {
            let mut store = SqliteStore::open(temporary.path()).unwrap();
            store.save_session(&session("current", "D:/work")).unwrap();
        }

        let store = SqliteStore::open(temporary.path()).unwrap();
        assert_eq!(
            store.load_session("current").unwrap().unwrap().id,
            "current"
        );
    }
}
