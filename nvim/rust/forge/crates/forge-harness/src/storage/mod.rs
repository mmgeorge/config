use crate::provider::ScopeOwner;
pub(crate) mod admission;
mod agent_policy;
mod archive;
pub(crate) mod configuration;
mod current;
pub mod database;
pub(crate) mod inspection;
pub(crate) mod lifecycle;
pub(crate) mod model_pin;
pub mod objects;
mod observed_agent;
mod ownership;
mod preferences;
mod question;
mod recovery;
mod schema;
pub mod state;
mod task;
pub(crate) use schema::bootstrap_lock;
pub(crate) use task::{PlanArtifactGuard, TaskEffects};
mod tool_output;

#[cfg(test)]
mod concurrency_tests;
#[cfg(test)]
mod reliability_tests;

use crate::agent::Agent;
use crate::checkpoint::CheckpointRecord;
use crate::exchange::{Exchange, ExchangeComment};
use crate::plan::{PlanExecutionRecord, PlanLifecycleRecord, PlanRecord};
use crate::session::{HarnessPreference, HarnessSession, SessionStore};
use crate::timeline::SessionEventRecord;
use anyhow::{Context, Result};
use rusqlite::{Connection, OptionalExtension, TransactionBehavior, params};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

const SESSION_FORMAT_VERSION: u32 = 65;
const PREFERENCE_FORMAT_VERSION: u32 = 44;

/// Keeps storage failures explicit at the host boundary, including the configured contention wait.
pub fn error_message(error: &anyhow::Error) -> String {
    if let Some(provider) = error
        .chain()
        .find_map(|cause| cause.downcast_ref::<crate::provider::ProviderError>())
    {
        // Normalized provider errors already render their operation and cause.
        // Expanding their source chain repeats that same failure at each layer.
        return provider.to_string();
    }
    let sqlite = error
        .chain()
        .find_map(|cause| cause.downcast_ref::<rusqlite::Error>());
    if let Some(rusqlite::Error::SqliteFailure(code, _)) = sqlite {
        if matches!(
            code.code,
            rusqlite::ErrorCode::DatabaseBusy | rusqlite::ErrorCode::DatabaseLocked
        ) {
            return format!(
                "SQLite contention prevented this operation (configured lock wait: 5 seconds). The operation failed; it was not silently retried. {error:#}"
            );
        }
        return format!("SQLite storage failure: {error:#}");
    }
    distinct_cause_message(error)
}

/// Render an error chain without repeating a cause that an outer layer already displays.
/// Several typed wrappers both print and expose their cause, so `{:#}` alone renders it twice.
pub fn distinct_cause_message(error: &anyhow::Error) -> String {
    let mut message = String::new();
    for cause in error.chain() {
        let text = cause.to_string();
        if message.contains(&text) {
            continue;
        }
        if !message.is_empty() {
            message.push_str(": ");
        }
        message.push_str(&text);
    }
    message
}

/// Stores one session with the exact durable format that produced it.
#[derive(Deserialize, Serialize)]
struct SessionEnvelope {
    format_version: u32,
    session: HarnessSession,
}

/// Owns SQLite metadata and content-addressed objects for the Harness broker.
pub struct SqliteStore {
    connection: database::HistoryConnection,
    admission: std::sync::Arc<admission::AdmissionRegistry>,
    current_exchange: std::cell::RefCell<Option<current::CurrentExchangeCache>>,
    session_lock: std::collections::HashMap<String, std::sync::Arc<fs::File>>,
    data_root: std::path::PathBuf,
    pub objects: objects::ObjectStore,
}

impl SqliteStore {
    /// Keeps related observations on one committed SQLite snapshot while writers continue.
    pub(crate) fn read_snapshot<Value>(
        &self,
        read: impl FnOnce() -> Result<Value>,
    ) -> Result<Value> {
        let transaction = self.connection.unchecked_transaction()?;
        let value = read()?;
        transaction.commit()?;
        Ok(value)
    }

    /// Holds an operating-system lock for the lifetime of one session owner.
    pub(crate) fn lock_session(&mut self, session_id: &str) -> Result<()> {
        if self.session_lock.contains_key(session_id) {
            return Ok(());
        }
        let owner = self.acquire_runtime_lock(session_id)?;
        self.session_lock
            .insert(session_id.to_owned(), std::sync::Arc::new(owner));
        Ok(())
    }

    /// Acquires an independent lock handle for runtime ownership or session deletion.
    fn acquire_runtime_lock(&self, session_id: &str) -> Result<fs::File> {
        let directory = state::directory(&self.data_root).join("session-owner");
        fs::create_dir_all(&directory)?;
        let path = directory.join(format!(
            "{}.lock",
            crate::plan::digest(session_id.as_bytes())
        ));
        let owner = fs::OpenOptions::new()
            .create(true)
            .truncate(false)
            .read(true)
            .write(true)
            .open(&path)?;
        match owner.try_lock() {
            Ok(()) => {}
            Err(fs::TryLockError::WouldBlock) => {
                return Err(crate::session::SessionLeaseConflict {
                    session_id: session_id.to_owned(),
                    native_fork: self
                        .load_session(session_id)?
                        .is_some_and(|session| session.native_fork),
                }
                .into());
            }
            Err(error) => {
                return Err(anyhow::anyhow!(
                    "acquire session runtime ownership: {error}"
                ));
            }
        }
        Ok(owner)
    }
    /// Open current history, replacing an incompatible database without migrating sessions.
    pub fn open(data_root: &Path) -> Result<Self> {
        fs::create_dir_all(data_root)
            .with_context(|| format!("create Harness data directory {}", data_root.display()))?;
        let objects = objects::ObjectStore::open(data_root)?;
        let connection = database::HistoryConnection::open(data_root)?;
        Ok(Self {
            connection,
            admission: admission::registry(data_root)?,
            current_exchange: Default::default(),
            session_lock: Default::default(),
            data_root: data_root.to_owned(),
            objects,
        })
    }

    /// Open initialized storage without schema writes.
    pub(crate) fn read_only(data_root: &Path) -> Result<Self> {
        let connection = database::HistoryConnection::read_only(data_root)?;
        Ok(Self {
            connection,
            admission: admission::registry(data_root)?,
            current_exchange: Default::default(),
            session_lock: Default::default(),
            data_root: data_root.to_owned(),
            objects: objects::ObjectStore::open(data_root)?,
        })
    }

    /// Load the last model controls selected for one provider and workspace.
    pub fn load_preference(
        &self,
        workspace: &str,
        provider: &str,
    ) -> Result<Option<HarnessPreference>> {
        let payload: Option<serde_json::Value> = self.load_payload(
            "SELECT payload FROM preferences.preference_record WHERE workspace = ?1 AND backend = ?2",
            params![workspace, provider],
        )?;
        let Some(payload) =
            payload.filter(|payload| payload["format_version"] == PREFERENCE_FORMAT_VERSION)
        else {
            return Ok(None);
        };
        let Some(preference) = payload
            .get("preference")
            .filter(|preference| preference.is_object())
        else {
            return Ok(None);
        };
        Ok(Some(serde_json::from_value(preference.clone())?))
    }

    /// Persist the last model controls selected for one provider and workspace.
    pub fn save_preference(
        &mut self,
        workspace: &str,
        provider: &str,
        preference: &HarnessPreference,
    ) -> Result<()> {
        self.connection.execute(
            "INSERT INTO preferences.preference_record(workspace, backend, payload) VALUES(?1, ?2, ?3) \
             ON CONFLICT(workspace, backend) DO UPDATE SET payload=excluded.payload",
            params![workspace, provider, encode(&serde_json::json!({"format_version":PREFERENCE_FORMAT_VERSION,"preference":preference}))?],
        )?;
        Ok(())
    }

    /// Acquires durable session ownership while holding its process lock.
    pub fn acquire_session_lease(
        &mut self,
        session_id: &str,
        client_id: &str,
    ) -> Result<HarnessSession> {
        self.lock_session(session_id)?;
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        anyhow::ensure!(
            !transaction.query_row(
                "SELECT EXISTS(SELECT 1 FROM session_deletion WHERE session_id=?1)",
                [session_id],
                |row| row.get::<_, bool>(0)
            )?,
            "Harness session deletion is pending; resume cleanup before opening the session"
        );
        let payload: String = transaction
            .query_row(
                "SELECT payload FROM session_record WHERE id=?1",
                [session_id],
                |row| row.get(0),
            )
            .with_context(|| format!("load Harness session lease {session_id}"))?;
        let mut session = decode_current_session(&payload)?
            .with_context(|| format!("Harness session {session_id} uses an outdated format"))?;
        validate_session_task_artifacts(&transaction, &session)?;
        session.lease_owner = None;
        session.acquire_lease(client_id)?;
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
            transaction.execute(
                "UPDATE session_record SET payload=?2 WHERE id=?1",
                params![session_id, encode_session(&session)?],
            )?;
        }
        transaction.commit()?;
        self.session_lock.remove(session_id);
        Ok(())
    }

    /// Save active session state only while the requesting client owns its lease.
    pub fn save_owned_session(&mut self, session: &HarnessSession, client_id: &str) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)
            .context("begin session ownership write")?;
        Self::ensure_no_configuration_intent(&transaction, &session.id)?;
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
            "UPDATE session_record SET workspace=?2, updated_at_ms=?3, payload=json_set(?4,'$.session.name',json_extract(payload,'$.session.name'),'$.session.active_task',json_extract(payload,'$.session.active_task')) WHERE id=?1",
            params![
                session.id,
                session.workspace,
                session.updated_at_ms,
                encode_session(session)?
            ],
        )?;
        transaction.commit()?;
        self.publish_admission(&session.id)
    }

    /// Persist an exchange and admit its delegated child requests atomically.
    pub fn save_exchange(&mut self, interaction: &Exchange) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)
            .context("begin exchange write")?;
        Self::write_exchange(&transaction, interaction).context("write exchange")?;
        transaction.commit().context("commit exchange write")?;
        self.publish_admission_exchange(
            &interaction.session_id,
            &interaction.id,
            Some(interaction.awaiting_input() && interaction.elicitation().is_some()),
        )
    }

    fn write_exchange(
        transaction: &rusqlite::Transaction<'_>,
        interaction: &Exchange,
    ) -> Result<()> {
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
            params![interaction.id, interaction.session_id, interaction.agent_id, interaction.ordinal as i64, question::preserve_answers(transaction, "exchange_record", &interaction.id, tool_output::encode(interaction)?)?],
        )?;
        anyhow::ensure!(
            written == 1,
            "an exchange cannot change its owning session or agent"
        );
        tool_output::synchronize(transaction, interaction)?;
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
            child.approval_mode = interaction.approval_mode;
            transaction.execute(
                "INSERT INTO exchange_record(id, session_id, agent_id, ordinal, payload) VALUES(?1,?2,?3,?4,?5)",
                params![child.id, child.session_id, child.agent_id, ordinal, encode(&child)?],
            )?;
        }
        Ok(())
    }

    /// Commit a child identity, causal delegation, exchange, and task before dispatch.
    pub fn admit_agent_task(
        &mut self,
        agent: &Agent,
        task: &crate::agent::AgentTask,
        parent: &Exchange,
        authorization: Option<&forge_provider_api::ExchangeId>,
    ) -> Result<()> {
        anyhow::ensure!(
            agent.session_id == task.session_id && parent.session_id == task.session_id,
            "agent admission crosses session ownership"
        );
        anyhow::ensure!(
            agent.id == task.agent_id && parent.id == task.parent_exchange_id,
            "agent admission has inconsistent causal ownership"
        );
        anyhow::ensure!(
            !agent.is_primary()
                && parent.agent_id
                    == crate::session::HarnessSession::primary_agent_id(&parent.session_id),
            "only the root can delegate child work"
        );
        anyhow::ensure!(parent.node_list.iter().any(|node| matches!(node,
            crate::exchange::ExchangeNode::AgentReference { agent: delegation }
                if delegation.child_agent_id == agent.id && delegation.child_exchange_id == task.id)),
            "agent task has no matching parent delegation");
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        agent_policy::admit(&transaction, agent, task, authorization)?;
        let conflict: bool = transaction.query_row(
            "SELECT EXISTS(SELECT 1 FROM agent_run_record WHERE session_id=?1 AND id!=?2 AND json_extract(payload,'$.task_name')=?3)",
            params![agent.session_id, agent.id, agent.task_name], |row| row.get(0))?;
        anyhow::ensure!(
            !conflict,
            "task name is already admitted in this session: {}",
            agent.task_name
        );
        let written = transaction.execute(
            "INSERT INTO agent_run_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload WHERE agent_run_record.session_id=excluded.session_id",
            params![agent.id, agent.session_id, encode(agent)?])?;
        anyhow::ensure!(written == 1, "agent cannot change its owning session");
        Self::write_exchange(&transaction, parent)?;
        Self::write_agent_task(&transaction, task)?;
        transaction.commit()?;
        self.publish_admission_exchange(
            &parent.session_id,
            &parent.id,
            Some(parent.awaiting_input() && parent.elicitation().is_some()),
        )
    }

    /// Commits one child's durable outcome without exposing a partially completed task.
    pub fn complete_agent_task(
        &mut self,
        agent: &Agent,
        task: &crate::agent::AgentTask,
        exchange: &Exchange,
    ) -> Result<()> {
        anyhow::ensure!(
            agent.session_id == task.session_id
                && exchange.session_id == task.session_id
                && agent.id == task.agent_id
                && exchange.agent_id == task.agent_id
                && exchange.id == task.id,
            "child completion crosses execution ownership"
        );
        anyhow::ensure!(
            task.completed_at_ms.is_some() && exchange.state() == task.state,
            "child completion has inconsistent task and exchange outcomes"
        );
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        Self::write_exchange(&transaction, exchange)?;
        Self::write_agent_task(&transaction, task)?;
        let written = transaction.execute(
            "UPDATE agent_run_record SET payload=?1 WHERE id=?2 AND session_id=?3",
            params![encode(agent)?, agent.id, agent.session_id],
        )?;
        anyhow::ensure!(written == 1, "child completion has no admitted identity");
        transaction.commit()?;
        self.publish_admission_exchange(
            &exchange.session_id,
            &exchange.id,
            Some(exchange.awaiting_input() && exchange.elicitation().is_some()),
        )
    }

    fn write_agent_task(
        connection: &rusqlite::Connection,
        task: &crate::agent::AgentTask,
    ) -> Result<()> {
        let belongs: bool = connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM exchange_record WHERE id=?1 AND session_id=?2 AND agent_id=?3)",
            params![task.id, task.session_id, task.agent_id], |row| row.get(0))?;
        anyhow::ensure!(
            belongs,
            "task exchange does not belong to its agent and session"
        );
        let written = connection.execute(
            "INSERT INTO agent_task_record(id,session_id,agent_id,operation_id,ordinal,payload) VALUES(?1,?2,?3,?4,?5,?6) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload WHERE agent_task_record.session_id=excluded.session_id AND agent_task_record.agent_id=excluded.agent_id AND agent_task_record.operation_id=excluded.operation_id AND agent_task_record.ordinal=excluded.ordinal",
            params![task.id, task.session_id, task.agent_id, task.operation_id, task.ordinal as i64, encode(task)?])?;
        anyhow::ensure!(
            written == 1,
            "task cannot change its execution ownership or admission identity"
        );
        Ok(())
    }

    /// Persist a task outcome without changing its admission identity.
    pub fn save_agent_task(&mut self, task: &crate::agent::AgentTask) -> Result<()> {
        Self::write_agent_task(&self.connection, task)
    }

    /// Restore ordered task outcomes within one visible root session.
    pub fn list_agent_task(&self, session_id: &str) -> Result<Vec<crate::agent::AgentTask>> {
        self.list_payload(
            "SELECT payload FROM agent_task_record WHERE session_id=?1 ORDER BY rowid",
            [session_id],
        )
    }

    /// Remove one provisional interaction after its provider turn is retracted.
    pub fn delete_exchange(&mut self, interaction_id: &str) -> Result<()> {
        let session_id: Option<String> = self
            .connection
            .query_row(
                "SELECT session_id FROM exchange_record WHERE id=?1",
                [interaction_id],
                |row| row.get(0),
            )
            .optional()?;
        self.connection
            .execute("DELETE FROM exchange_record WHERE id=?1", [interaction_id])?;
        if let Some(session_id) = session_id {
            self.publish_admission_exchange(&session_id, interaction_id, None)?;
        }
        Ok(())
    }

    /// Load one exchange with its retained tool output.
    pub fn load_exchange(&self, id: &str) -> Result<Option<Exchange>> {
        Ok(self
            .list_exchange_payload("SELECT payload FROM exchange_record WHERE id=?1", [id])?
            .pop())
    }

    /// Load the owner of a provider turn without hydrating unrelated exchange bodies.
    pub(crate) fn load_provider_exchange(
        &self,
        session_id: &str,
        address: &crate::provider::TurnAddress,
    ) -> Result<Option<Exchange>> {
        Ok(self
            .list_exchange_payload(
                "SELECT payload FROM exchange_record WHERE session_id=?1 AND id=?4 AND agent_id=?5 AND EXISTS(\
                SELECT 1 FROM json_each(exchange_record.payload, '$.turn') AS provider_turn \
                WHERE json_extract(provider_turn.value, '$.provider.thread_id')=?2 \
                AND json_extract(provider_turn.value, '$.provider.turn_id')=?3 \
                AND json_extract(provider_turn.value, '$.provider.scope.invocation_id')=?6 \
                AND json_extract(provider_turn.value, '$.provider.scope.thread_id')=?7) LIMIT 1",
                params![session_id, address.thread_id.as_str(), address.turn_id.as_str(),
                    address.scope.exchange_id.as_str(), address.scope.owner().agent_id().as_str(),
                    address.scope.invocation_id.as_str(), address.scope.thread_id.as_str()],
            )?
            .pop())
    }

    /// Loads a compaction owner across agents without hydrating unrelated exchanges.
    pub(crate) fn load_compaction_exchange(
        &self,
        session_id: &str,
        compaction: &crate::provider::Compaction,
    ) -> Result<Option<Exchange>> {
        Ok(self.list_exchange_payload(
            "SELECT payload FROM exchange_record WHERE session_id=?1 AND EXISTS(SELECT 1 FROM json_each(exchange_record.payload,'$.node_list') AS node WHERE json_extract(node.value,'$.kind')='compaction' AND json_extract(node.value,'$.compaction.id')=?2 AND json_extract(node.value,'$.compaction.thread_id')=?3) ORDER BY (agent_id=?4) DESC,ordinal LIMIT 1",
            params![session_id,compaction.id,compaction.thread_id,HarnessSession::primary_agent_id(session_id)],
        )?.pop())
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
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        for (table, id, payload) in [
            ("plan_deviation_record", &deviation.id, deviation_payload),
            ("plan_execution_record", &execution.id, execution_payload),
        ] {
            transaction.execute(&format!("INSERT INTO {table}(id, session_id, payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload"),
                params![id, execution.session_id, encode(&payload)?])?;
        }

        transaction.commit()?;
        self.publish_admission(&execution.session_id)
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
        // Acquire the file gate before opening SQLite's write transaction.
        let artifacts =
            crate::plan::PlanFileStore::new(&self.data_root, std::path::PathBuf::new()).unwrap();
        let owns_artifacts = artifacts.claim_plan_artifacts(plan)?;
        let committed = (|| -> Result<()> {
            let transaction = self
                .connection
                .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
            let mut payload = serde_json::to_value(plan)?;
            payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
            let payload = question::preserve_answers(
                &transaction,
                "plan_record",
                &plan.id,
                encode(&payload)?,
            )?;
            let written = transaction.execute(
                "INSERT INTO plan_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload WHERE plan_record.session_id=excluded.session_id",
                params![plan.id, plan.session_id, payload],
            )?;
            anyhow::ensure!(written == 1, "plan cannot change its owning session");
            transaction.commit()?;
            Ok(())
        })();
        // Reconcile only after the transaction has released its database lock.
        if owns_artifacts && let Some(persisted) = self.load_plan(&plan.id)? {
            artifacts.reconcile_plan_artifacts(&persisted)?;
        }
        committed?;
        self.publish_admission(&plan.session_id)
    }

    /// Load a plan by stable Harness identifier.
    pub fn load_plan(&self, plan_id: &str) -> Result<Option<PlanRecord>> {
        if let Some(plan) = self.active_plan_view(plan_id)? {
            return Ok(Some(plan));
        }
        self.load_payload("SELECT payload FROM plan_record WHERE id=?1 AND json_extract(payload, '$.schema_version')=?2", params![plan_id, crate::plan::PLAN_SCHEMA_VERSION])
    }

    /// Load every plan artifact for one session in creation order.
    pub fn list_plan(&self, session_id: &str) -> Result<Vec<PlanRecord>> {
        self.list_payload(
            "SELECT payload FROM plan_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=?2 ORDER BY rowid",
            params![session_id, crate::plan::PLAN_SCHEMA_VERSION],
        )
    }

    /// Remove one retracted canonical plan record.
    pub fn delete_plan(&mut self, plan_id: &str) -> Result<()> {
        let session_id: Option<String> = self
            .connection
            .query_row(
                "SELECT session_id FROM plan_record WHERE id=?1",
                [plan_id],
                |row| row.get(0),
            )
            .optional()?;
        self.connection
            .execute("DELETE FROM plan_record WHERE id=?1", [plan_id])?;
        if let Some(session_id) = session_id {
            self.publish_admission(&session_id)?;
        }
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
            "SELECT payload FROM plan_lifecycle_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=?2 ORDER BY rowid",
            params![session_id, crate::plan::PLAN_SCHEMA_VERSION],
        )
    }

    /// Persist a timeline event without changing its admitted owner.
    pub fn save_session_event(&mut self, event: &SessionEventRecord) -> Result<()> {
        anyhow::ensure!(
            event.owner.session_id() == event.session_id,
            "timeline event crosses session ownership"
        );
        let transaction = rusqlite::Transaction::new_unchecked(
            &self.connection,
            rusqlite::TransactionBehavior::Immediate,
        )?;
        self.validate_timeline_owner(&event.owner)?;
        let existing: Option<String> = transaction
            .query_row(
                "SELECT payload FROM session_event_record WHERE id=?1",
                [&event.id],
                |row| row.get(0),
            )
            .optional()?;
        if let Some(existing) = existing {
            let existing: SessionEventRecord = decode(&existing)?;
            anyhow::ensure!(
                existing.session_id == event.session_id && existing.owner == event.owner,
                "timeline event ownership cannot change"
            );
        }
        transaction.execute(
            "INSERT INTO session_event_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload",
            params![event.id, event.session_id, encode(event)?],
        )?;
        transaction.commit()?;
        Ok(())
    }

    pub(crate) fn load_session_event(
        &self,
        session_id: &str,
        id: &str,
    ) -> Result<Option<SessionEventRecord>> {
        let event: Option<SessionEventRecord> =
            self.load_payload("SELECT payload FROM session_event_record WHERE id=?1", [id])?;
        if let Some(event) = &event {
            anyhow::ensure!(
                event.session_id == session_id,
                "timeline event belongs to another session"
            );
        }
        Ok(event)
    }

    /// Load session-level timeline events in their insertion order.
    pub fn list_session_event(&self, session_id: &str) -> Result<Vec<SessionEventRecord>> {
        self.list_payload(
            "SELECT payload FROM session_event_record WHERE session_id=?1 ORDER BY rowid",
            [session_id],
        )
    }

    /// Preserve native identity whenever this session inherited content or a fork boundary.
    pub(crate) fn has_fork_lineage(&self, session_id: &str) -> Result<bool> {
        Ok(self.connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM fork_history_record WHERE session_id=?1)
                OR EXISTS(SELECT 1 FROM session_event_record, json_tree(session_event_record.payload) AS entry
                    WHERE session_id=?1 AND entry.key='kind' AND entry.value='forked')",
            [session_id], |row| row.get(0),
        )?)
    }

    /// Prepend the immutable fork snapshot without exposing it to child execution queries.
    pub(crate) fn with_fork_history(
        &self,
        session_id: &str,
        timeline: Vec<crate::timeline::TimelineEntry>,
        plan_file: &crate::plan::PlanFileStore,
    ) -> Result<Vec<crate::timeline::TimelineEntry>> {
        let mut history: Vec<crate::timeline::TimelineEntry> = self.list_payload(
            "SELECT payload FROM fork_history_record WHERE session_id=?1 ORDER BY ordinal",
            [session_id],
        )?;
        for entry in &mut history {
            archive::attach(entry, &self.data_root);
            entry.restore_reports(plan_file);
        }
        history.extend(timeline);
        Ok(history)
    }

    /// Commit the child's inherited timeline and lineage event together.
    pub(crate) fn save_fork_history(
        &mut self,
        session: &HarnessSession,
        history: &[crate::timeline::TimelineEntry],
        event: &SessionEventRecord,
    ) -> Result<()> {
        anyhow::ensure!(
            event.session_id == session.id
                && event.owner == crate::timeline::TimelineOwner::session(&session.id),
            "fork history event must belong to its destination session"
        );
        let payload = history
            .iter()
            .map(|entry| {
                let mut value = crate::codec::to_value(entry)?;
                archive::encode(&mut value, &self.objects)?;
                encode(&value)
            })
            .collect::<Result<Vec<_>>>()?;
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let primary = Agent::primary(&session.id, session.created_at_ms);
        anyhow::ensure!(
            session.primary_agent_id == primary.id,
            "session primary agent identity does not belong to this session"
        );
        transaction.execute(
            "INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES(?1, ?2, ?3, ?4)",
            params![session.id, session.workspace, session.updated_at_ms, encode_session(session)?],
        )?;
        transaction.execute(
            "INSERT INTO agent_run_record(id, session_id, payload) VALUES(?1, ?2, ?3)",
            params![primary.id, primary.session_id, encode(&primary)?],
        )?;
        for (ordinal, entry) in payload.iter().enumerate() {
            transaction.execute(
                "INSERT INTO fork_history_record(session_id, ordinal, payload) VALUES(?1, ?2, ?3)",
                params![session.id, ordinal as i64, entry],
            )?;
        }
        transaction.execute(
            "INSERT INTO session_event_record(id, session_id, payload) VALUES(?1, ?2, ?3)",
            params![event.id, event.session_id, encode(event)?],
        )?;
        transaction.commit()?;
        Ok(())
    }

    /// Remove one provisional lifecycle event after its owning provider turn fails.
    pub fn delete_plan_lifecycle(&mut self, lifecycle_id: &str) -> Result<()> {
        self.connection.execute(
            "DELETE FROM plan_lifecycle_record WHERE id=?1",
            [lifecycle_id],
        )?;
        Ok(())
    }

    /// Write one accepted-plan execution record.
    pub fn save_plan_execution(&mut self, execution: &PlanExecutionRecord) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let mut payload = serde_json::to_value(execution)?;
        payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
        let written = transaction.execute("INSERT INTO plan_execution_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload WHERE plan_execution_record.session_id=excluded.session_id", params![execution.id,execution.session_id,encode(&payload)?])?;
        anyhow::ensure!(written == 1, "execution cannot change its owning session");

        transaction.commit()?;
        self.publish_admission(&execution.session_id)
    }

    /// Load one accepted-plan execution by stable identifier.
    pub fn load_plan_execution(&self, execution_id: &str) -> Result<Option<PlanExecutionRecord>> {
        if let Some(execution) = self.active_execution_view(execution_id)? {
            return Ok(Some(execution));
        }
        self.load_payload(
            "SELECT payload FROM plan_execution_record WHERE id=?1 AND json_extract(payload, '$.schema_version')=?2",
            params![execution_id, crate::plan::PLAN_SCHEMA_VERSION],
        )
    }

    /// Load every accepted-plan execution for one session.
    pub fn list_plan_execution(&self, session_id: &str) -> Result<Vec<PlanExecutionRecord>> {
        self.list_payload(
            "SELECT payload FROM plan_execution_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=?2 ORDER BY rowid",
            params![session_id, crate::plan::PLAN_SCHEMA_VERSION],
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
            "SELECT payload FROM plan_deviation_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=?2 ORDER BY rowid",
            params![session_id, crate::plan::PLAN_SCHEMA_VERSION],
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
            "SELECT payload FROM plan_audit_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=?2 ORDER BY rowid",
            params![session_id, crate::plan::PLAN_SCHEMA_VERSION],
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
            "SELECT payload FROM plan_resolution_record WHERE session_id=?1 AND json_extract(payload, '$.schema_version')=?2 ORDER BY rowid",
            params![session_id, crate::plan::PLAN_SCHEMA_VERSION],
        )
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
        self.publish_admission_exchange(
            &exchange.session_id,
            &exchange.id,
            Some(exchange.awaiting_input() && exchange.elicitation().is_some()),
        )
    }

    /// Load one workspace checkpoint by content digest.
    pub fn load_checkpoint(&self, checkpoint_id: &str) -> Result<Option<CheckpointRecord>> {
        self.load_payload(
            "SELECT payload FROM checkpoint_record WHERE id=?1",
            [checkpoint_id],
        )
    }

    /// Record one globally shared prompt and retain only the newest bounded history.
    pub fn record_prompt_history(
        &mut self,
        draft: &crate::attachment::PromptDraft,
        created_at_ms: i64,
    ) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        transaction.execute(
            "INSERT INTO prompt_history_record(text, created_at_ms) VALUES(?1, ?2)",
            params![draft.text, created_at_ms],
        )?;
        transaction.execute(
            "INSERT INTO prompt_attachment_record(history_id,payload) VALUES(?1,?2)",
            params![
                transaction.last_insert_rowid(),
                serde_json::to_string(&draft.attachments)?
            ],
        )?;
        transaction.commit()?;
        Ok(())
    }

    /// Load globally shared prompts from newest to oldest.
    pub fn list_prompt_history(&self) -> Result<Vec<crate::attachment::PromptDraft>> {
        let mut statement = self
            .connection
            .prepare("SELECT h.text, COALESCE(a.payload, '[]') FROM prompt_history_record h LEFT JOIN prompt_attachment_record a ON a.history_id=h.id ORDER BY h.id DESC")?;
        let row_list = statement.query_map([], |row| {
            Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?))
        })?;
        row_list
            .map(|row| {
                let (text, attachments) = row?;
                Ok(crate::attachment::PromptDraft {
                    text,
                    attachments: serde_json::from_str(&attachments)?,
                })
            })
            .collect()
    }

    /// Write one concrete child-agent run into its owning Harness session.
    pub fn save_agent_run(&mut self, run: &Agent) -> Result<()> {
        self.save_scoped_payload("agent_run_record", &run.id, &run.session_id, run)
    }

    /// Load child-agent runs in creation order for one Harness session.
    pub fn list_agent_run(&self, session_id: &str) -> Result<Vec<Agent>> {
        let mut statement = self.connection.prepare(
            "SELECT id,payload FROM agent_run_record WHERE session_id=?1 ORDER BY rowid",
        )?;
        statement
            .query_map([session_id], |row| {
                Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?))
            })?
            .map(|row| {
                let (id, payload) = row?;
                let agent: Agent = decode(&payload)?;
                anyhow::ensure!(
                    agent.id == id && agent.session_id == session_id,
                    "stored child identity changes its persisted owner"
                );
                agent_policy::validate(&self.connection, &agent)?;
                Ok(agent)
            })
            .collect()
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
        if matches!(
            table,
            "task_record" | "plan_record" | "plan_execution_record"
        ) {
            self.publish_admission(session_id)?;
        }
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
            if let Some(session) = decode_current_session(&row?)? {
                validate_session_task_artifacts(&self.connection, &session)?;
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
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        Self::ensure_no_configuration_intent(&transaction, &session.id)?;
        transaction.execute(
            "INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES(?1, ?2, ?3, ?4)\
             ON CONFLICT(id) DO UPDATE SET workspace=excluded.workspace, updated_at_ms=excluded.updated_at_ms, payload=json_set(excluded.payload,'$.session.name',json_extract(session_record.payload,'$.session.name'),'$.session.active_task',json_extract(session_record.payload,'$.session.active_task'))",
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
        self.publish_admission(&session.id)
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
                anyhow::ensure!(
                    session.is_some(),
                    "This saved Harness session uses an unsupported session contract. Create a new session; saved history has been retained."
                );
                if let Some(session) = session.as_mut() {
                    validate_session_task_artifacts(&self.connection, session)?;
                }
                Ok(session)
            }
            None => Ok(None),
        }
    }

    fn list_session(&self, workspace: Option<&str>) -> Result<Vec<HarnessSession>> {
        let mut sessions = self.list_current_session(
            "SELECT payload FROM session_record WHERE NOT EXISTS(SELECT 1 FROM session_deletion WHERE session_id=session_record.id) ORDER BY updated_at_ms DESC",
            [],
        )?;
        if let Some(path) = workspace {
            sessions.retain(|session| crate::workspace::same(&session.workspace, path));
        }
        Ok(sessions)
    }

    fn delete_session(&mut self, session_id: &str) -> Result<()> {
        self.delete_session_with(session_id, || Ok(()))
    }
}

fn encode<T: Serialize>(value: &T) -> Result<String> {
    crate::codec::to_string(value).context("encode Harness storage payload")
}

fn decode<T: DeserializeOwned>(value: &str) -> Result<T> {
    crate::codec::from_str(value).context("decode Harness storage payload")
}

fn validate_session_task_artifacts(
    connection: &Connection,
    session: &HarnessSession,
) -> Result<()> {
    if let Some(active) = &session.active_task {
        task::validate_task(connection, &session.id, active)?;
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
pub(crate) mod test {
    use super::*;

    /// Reject unsupported contracts before changed fields decode while preserving new-session resume.
    #[test]
    fn session_contract_rejects_old_payload_before_decode_and_resumes_new_history() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let mut store = SqliteStore::open(directory.path())?;
        for (id, version) in [
            ("older", Some(SESSION_FORMAT_VERSION - 1)),
            ("missing", None),
        ] {
            let mut payload = serde_json::json!({"session":{"provider_session_id":17,"access":"invalid-old-shape"}});
            if let Some(version) = version {
                payload["format_version"] = version.into();
            }
            store.connection.execute(
                "INSERT INTO session_record(id,workspace,updated_at_ms,payload) VALUES(?1,'D:/work',1,?2)",
                params![id, encode(&payload)?],
            )?;
            let message = store.load_session(id).unwrap_err().to_string();
            assert!(message.contains("Create a new session"), "{message}");
            assert!(
                !message.contains("decode current"),
                "old payload was decoded: {message}"
            );
        }
        let mut current = session("current-contract", "D:/work");
        current.provider_session_id = Some("new-contract-history".into());
        store.save_session(&current)?;
        drop(store);
        let reopened = SqliteStore::open(directory.path())?;
        let resumed = reopened
            .load_session(&current.id)?
            .expect("current session remains resumable");
        assert_eq!(resumed.provider_session_id, current.provider_session_id);
        assert_eq!(reopened.list_session(None)?.len(), 1);
        let retained: i64 =
            reopened
                .connection
                .query_row("SELECT COUNT(*) FROM session_record", [], |row| row.get(0))?;
        assert_eq!(retained, 3);
        Ok(())
    }

    pub(crate) fn session(id: &str, workspace: &str) -> HarnessSession {
        HarnessSession {
            purpose: crate::session::SessionPurpose::Conversation,
            id: id.into(),
            primary_agent_id: HarnessSession::primary_agent_id(id),
            name: id.into(),
            workspace: workspace.into(),
            provider: "fixture-beta".into(),
            provider_session_id: None,
            provider_checkpoint_id: None,
            provider_fork_state: crate::session::ProviderForkState::Ready,
            model: "default".into(),
            model_label: None,
            provider_label: "Fixture Beta".into(),
            resolved_model: None,
            effort: "medium".into(),
            plan_executor: Default::default(),
            plan_compact: false,
            approval_timeout: Default::default(),
            tool_timeout: Default::default(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: Default::default(),
            shell: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            active_task: None,
            created_at_ms: 1,
            updated_at_ms: 1,
            lease_owner: None,
            native_fork: false,
            native_compact: false,
            context_usage: None,
            background: None,
            auto_compaction: Default::default(),
            last_compaction_turn: None,
        }
    }

    #[test]
    fn recovery_is_atomic_and_preserves_waiting_exchanges() {
        use crate::exchange::ExchangeState;
        let temporary = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(temporary.path()).unwrap();
        store.save_session(&session("session", "D:/work")).unwrap();
        store.save_agent_run(&Agent::primary("session", 0)).unwrap();
        let mut exchange = Exchange {
            finalization_error: None,
            check_owner: None,
            output_store: None,
            archived_output: Default::default(),
            input_message_id: None,
            provider_retry: None,
            agent_id: "session:agent:primary".into(),
            id: "active".into(),
            session_id: "session".into(),
            ordinal: 1,
            prompt: "work".into(),
            origin: crate::exchange::ExchangeOrigin::User,
            approval_mode: None,
            phase: crate::exchange::ExchangePhase::Running,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            approval_resolution: Vec::new(),
            host_file_changes: Vec::new(),
            check_setup: None,
            turn: Vec::new(),
            created_at_ms: 10,
            entry: Vec::new(),
            tool_counter: Default::default(),
            node_list: Vec::new(),
            metrics: crate::exchange::ExchangeMetrics::default(),
            comment: Vec::new(),
            checklist: None,

            question: crate::exchange::ExchangeQuestion::None,
            clock: crate::exchange::ExchangeClock::Running {
                elapsed_ms: 20,
                started_at_ms: 30,
            },
        };
        exchange.observe_blocker("approval:recovery".into(), true, 70);
        store.save_exchange(&exchange).unwrap();
        exchange.id = "waiting".into();
        exchange.ordinal = 2;
        exchange.set_awaiting_input(true);
        exchange.clock =
            crate::exchange::ExchangeClock::fixture(20, exchange.execution_started_at_ms());
        exchange.clock = crate::exchange::ExchangeClock::Paused {
            elapsed_ms: exchange.duration_ms(),
        };
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
                .all(|exchange| exchange.state() == ExchangeState::Running)
        );
        assert_eq!(unchanged[0].execution_started_at_ms(), None);
        assert_eq!(unchanged[0].duration_ms(), 60);
        store.save_agent_run(&child).unwrap();
        store.interrupt_detached_execution("session").unwrap();
        let recovered = store.list_exchange("session").unwrap();
        assert_eq!(recovered[0].state(), ExchangeState::Interrupted);
        assert_eq!(recovered[0].elapsed(90_000), 60);
        assert_eq!(recovered[0].completed_at_ms(), Some(70));
        assert_eq!(recovered[1].state(), ExchangeState::Running);
        assert!(recovered[1].awaiting_input());
        assert_eq!(recovered[1].elapsed(90_000), 20);
        let snapshot = serde_json::to_value(&recovered).unwrap();
        store.interrupt_detached_execution("session").unwrap();
        assert_eq!(
            serde_json::to_value(store.list_exchange("session").unwrap()).unwrap(),
            snapshot
        );
    }
}
