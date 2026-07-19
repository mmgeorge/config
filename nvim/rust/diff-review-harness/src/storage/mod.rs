use crate::agent::{AgentRun, AgentTurnRecord};
use crate::checkpoint::CheckpointRecord;
use crate::goal::GoalRecord;
use crate::interaction::{InteractionComment, InteractionRecord};
use crate::plan::{PlanExecutionRecord, PlanLifecycleRecord, PlanRecord};
use crate::session::{HarnessPreference, HarnessSession, SessionStore};
use crate::timeline::SessionEventRecord;
use anyhow::{Context, Result};
use rusqlite::{Connection, OptionalExtension, TransactionBehavior, params};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

const SESSION_FORMAT_VERSION: u32 = 8;

/// Stores one session with the exact durable format that produced it.
#[derive(Deserialize, Serialize)]
struct SessionEnvelope {
    format_version: u32,
    session: HarnessSession,
}

/// Owns SQLite metadata and content-addressed objects for the Harness broker.
pub struct SqliteStore {
    connection: Connection,
    object_root: PathBuf,
}

impl SqliteStore {
    /// Open durable storage for the current Harness format.
    pub fn open(data_root: &Path) -> Result<Self> {
        fs::create_dir_all(data_root)
            .with_context(|| format!("create Harness data directory {}", data_root.display()))?;
        let object_root = data_root.join("objects").join("sha256");
        fs::create_dir_all(&object_root).with_context(|| {
            format!("create Harness object directory {}", object_root.display())
        })?;
        let connection = Connection::open(data_root.join("harness.sqlite3"))?;
        connection.busy_timeout(Duration::from_secs(5))?;
        connection.pragma_update(None, "journal_mode", "WAL")?;
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
            CREATE INDEX IF NOT EXISTS session_workspace_activity
                ON session_record(workspace, updated_at_ms DESC);
            CREATE TABLE IF NOT EXISTS interaction_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                ordinal INTEGER NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            CREATE UNIQUE INDEX IF NOT EXISTS interaction_session_ordinal
                ON interaction_record(session_id, ordinal);
            CREATE TABLE IF NOT EXISTS interaction_comment (
                id TEXT PRIMARY KEY,
                interaction_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(interaction_id) REFERENCES interaction_record(id) ON DELETE CASCADE
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
            CREATE TABLE IF NOT EXISTS agent_turn_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                agent_run_id TEXT NOT NULL,
                ordinal INTEGER NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE,
                FOREIGN KEY(agent_run_id) REFERENCES agent_run_record(id) ON DELETE CASCADE
            );
            CREATE UNIQUE INDEX IF NOT EXISTS agent_turn_run_ordinal
                ON agent_turn_record(agent_run_id, ordinal);
            CREATE TABLE IF NOT EXISTS session_event_record (
                id TEXT PRIMARY KEY,
                session_id TEXT NOT NULL,
                payload TEXT NOT NULL,
                FOREIGN KEY(session_id) REFERENCES session_record(id) ON DELETE CASCADE
            );
            "#,
        )?;
        Ok(Self {
            connection,
            object_root,
        })
    }

    /// Load the last model controls selected for one backend and workspace.
    pub fn load_preference(
        &self,
        workspace: &str,
        backend: &str,
    ) -> Result<Option<HarnessPreference>> {
        self.load_payload(
            "SELECT payload FROM preference_record WHERE workspace = ?1 AND backend = ?2",
            params![workspace, backend],
        )
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
            params![workspace, backend, encode(preference)?],
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

    /// Write one interaction record into its session timeline.
    pub fn save_interaction(&mut self, interaction: &InteractionRecord) -> Result<()> {
        self.connection.execute(
            "INSERT INTO interaction_record(id, session_id, ordinal, payload) VALUES(?1, ?2, ?3, ?4)\
             ON CONFLICT(id) DO UPDATE SET ordinal=excluded.ordinal, payload=excluded.payload",
            params![interaction.id, interaction.session_id, interaction.ordinal as i64, encode(interaction)?],
        )?;
        Ok(())
    }

    /// Remove one provisional interaction after its provider turn is retracted.
    pub fn delete_interaction(&mut self, interaction_id: &str) -> Result<()> {
        self.connection.execute(
            "DELETE FROM interaction_record WHERE id=?1",
            [interaction_id],
        )?;
        Ok(())
    }

    /// Load interactions in their admitted user-action order.
    pub fn list_interaction(&self, session_id: &str) -> Result<Vec<InteractionRecord>> {
        self.list_payload(
            "SELECT payload FROM interaction_record WHERE session_id=?1 ORDER BY ordinal",
            [session_id],
        )
    }

    /// Load the latest interaction ordinal for one session.
    pub fn next_interaction_ordinal(&self, session_id: &str) -> Result<u64> {
        let ordinal: Option<i64> = self.connection.query_row(
            "SELECT MAX(ordinal) FROM interaction_record WHERE session_id=?1",
            [session_id],
            |row| row.get(0),
        )?;
        Ok(ordinal.unwrap_or(0) as u64 + 1)
    }

    /// Write a diff annotation for later request-changes prompts.
    pub fn save_interaction_comment(&mut self, comment: &InteractionComment) -> Result<()> {
        self.connection.execute(
            "INSERT INTO interaction_comment(id, interaction_id, payload) VALUES(?1, ?2, ?3)\
             ON CONFLICT(id) DO UPDATE SET payload=excluded.payload",
            params![comment.id, comment.interaction_id, encode(comment)?],
        )?;
        Ok(())
    }

    /// Load annotations for one historical interaction review.
    pub fn list_interaction_comment(
        &self,
        interaction_id: &str,
    ) -> Result<Vec<InteractionComment>> {
        self.list_payload(
            "SELECT payload FROM interaction_comment WHERE interaction_id=?1 ORDER BY rowid",
            [interaction_id],
        )
    }

    /// Write a plan lifecycle record.
    pub fn save_plan(&mut self, plan: &PlanRecord) -> Result<()> {
        self.save_scoped_payload("plan_record", &plan.id, &plan.session_id, plan)
    }

    /// Load a plan by stable Harness identifier.
    pub fn load_plan(&self, plan_id: &str) -> Result<Option<PlanRecord>> {
        self.load_payload("SELECT payload FROM plan_record WHERE id=?1", [plan_id])
    }

    /// Load every plan artifact for one session in creation order.
    pub fn list_plan(&self, session_id: &str) -> Result<Vec<PlanRecord>> {
        self.list_payload(
            "SELECT payload FROM plan_record WHERE session_id=?1 ORDER BY rowid",
            [session_id],
        )
    }

    /// Write one immutable plan lifecycle event.
    pub fn save_plan_lifecycle(&mut self, lifecycle: &PlanLifecycleRecord) -> Result<()> {
        self.save_scoped_payload(
            "plan_lifecycle_record",
            &lifecycle.id,
            &lifecycle.session_id,
            lifecycle,
        )
    }

    /// Load plan lifecycle events in their insertion order.
    pub fn list_plan_lifecycle(&self, session_id: &str) -> Result<Vec<PlanLifecycleRecord>> {
        self.list_payload(
            "SELECT payload FROM plan_lifecycle_record WHERE session_id=?1 ORDER BY rowid",
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
        self.save_scoped_payload(
            "plan_execution_record",
            &execution.id,
            &execution.session_id,
            execution,
        )
    }

    /// Load one accepted-plan execution by stable identifier.
    pub fn load_plan_execution(&self, execution_id: &str) -> Result<Option<PlanExecutionRecord>> {
        self.load_payload(
            "SELECT payload FROM plan_execution_record WHERE id=?1",
            [execution_id],
        )
    }

    /// Load every accepted-plan execution for one session.
    pub fn list_plan_execution(&self, session_id: &str) -> Result<Vec<PlanExecutionRecord>> {
        self.list_payload(
            "SELECT payload FROM plan_execution_record WHERE session_id=?1 ORDER BY rowid",
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

    /// Load one workspace checkpoint by content digest.
    pub fn load_checkpoint(&self, checkpoint_id: &str) -> Result<Option<CheckpointRecord>> {
        self.load_payload(
            "SELECT payload FROM checkpoint_record WHERE id=?1",
            [checkpoint_id],
        )
    }

    /// Write a content-addressed object once and return its SHA-256 identifier.
    pub fn put_object(&self, content: &[u8]) -> Result<String> {
        let object_id = crate::plan::digest(content);
        let directory = self.object_root.join(&object_id[0..2]);
        fs::create_dir_all(&directory)?;
        let path = directory.join(&object_id[2..]);
        if !path.exists() {
            fs::write(&path, content)
                .with_context(|| format!("write Harness object {}", path.display()))?;
        }
        Ok(object_id)
    }

    /// Read a content-addressed object and verify its expected location exists.
    pub fn get_object(&self, object_id: &str) -> Result<Vec<u8>> {
        anyhow::ensure!(
            object_id.len() == 64 && object_id.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "invalid Harness object identifier"
        );
        let path = self
            .object_root
            .join(&object_id[0..2])
            .join(&object_id[2..]);
        fs::read(&path).with_context(|| format!("read Harness object {}", path.display()))
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
    pub fn save_agent_run(&mut self, run: &AgentRun) -> Result<()> {
        self.save_scoped_payload("agent_run_record", &run.id, &run.session_id, run)
    }

    /// Load child-agent runs in creation order for one Harness session.
    pub fn list_agent_run(&self, session_id: &str) -> Result<Vec<AgentRun>> {
        self.list_payload(
            "SELECT payload FROM agent_run_record WHERE session_id=?1 ORDER BY rowid",
            [session_id],
        )
    }

    /// Write one child-agent turn while preserving its run-local ordinal.
    pub fn save_agent_turn(&mut self, turn: &AgentTurnRecord) -> Result<()> {
        self.connection.execute(
            "INSERT INTO agent_turn_record(id, session_id, agent_run_id, ordinal, payload) \
             VALUES(?1, ?2, ?3, ?4, ?5) ON CONFLICT(id) DO UPDATE SET \
             ordinal=excluded.ordinal, payload=excluded.payload",
            params![
                turn.id,
                turn.session_id,
                turn.agent_run_id,
                turn.ordinal as i64,
                encode(turn)?
            ],
        )?;
        Ok(())
    }

    /// Load child-agent turns in their run-local interaction order.
    pub fn list_agent_turn(&self, run_id: &str) -> Result<Vec<AgentTurnRecord>> {
        self.list_payload(
            "SELECT payload FROM agent_turn_record WHERE agent_run_id=?1 ORDER BY ordinal",
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
            if let Some(session) = decode_current_session(&row?)? {
                session_list.push(session);
            }
        }
        Ok(session_list)
    }
}

impl SessionStore for SqliteStore {
    fn save_session(&mut self, session: &HarnessSession) -> Result<()> {
        self.connection.execute(
            "INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES(?1, ?2, ?3, ?4)\
             ON CONFLICT(id) DO UPDATE SET workspace=excluded.workspace, updated_at_ms=excluded.updated_at_ms, payload=excluded.payload",
            params![
                session.id,
                session.workspace,
                session.updated_at_ms,
                encode_session(session)?
            ],
        )?;
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
            Some(value) => decode_current_session(&value),
            None => Ok(None),
        }
    }

    fn list_session(&self, workspace: Option<&str>) -> Result<Vec<HarnessSession>> {
        match workspace {
            Some(path) => self.list_current_session(
                "SELECT payload FROM session_record WHERE workspace=?1 ORDER BY updated_at_ms DESC",
                [path],
            ),
            None => self.list_current_session(
                "SELECT payload FROM session_record ORDER BY updated_at_ms DESC",
                [],
            ),
        }
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

    fn session(id: &str, workspace: &str) -> HarnessSession {
        HarnessSession {
            id: id.into(),
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
            context_window: None,
            fast_mode: false,
            execution_mode: crate::session::ExecutionMode::Read,
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
    fn persists_repeated_agent_runs_and_run_local_turns() {
        let temporary = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(temporary.path()).unwrap();
        store.save_session(&session("session", "D:/work")).unwrap();
        let first = AgentRun::pending("session", "explorer", "inspect Bevy", 1);
        let second = AgentRun::pending("session", "explorer", "inspect physics", 2);
        store.save_agent_run(&first).unwrap();
        store.save_agent_run(&second).unwrap();
        let interaction = InteractionRecord {
            id: "interaction".into(),
            session_id: "session".into(),
            ordinal: 1,
            prompt: "report".into(),
            kind: crate::interaction::InteractionKind::Chat,
            state: crate::interaction::InteractionState::Complete,
            plan_id: None,
            execution_id: None,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            created_at_ms: 3,
            completed_at_ms: Some(4),
            node_list: vec![crate::interaction::InteractionNode::MainSegment {
                segment: Box::new(crate::interaction::MainSegment {
                    id: "interaction:segment:1".into(),
                    state: crate::interaction::SegmentState::Complete,
                    started_at_ms: 3,
                    completed_at_ms: Some(4),
                    duration_ms: 1,
                    token_count: None,
                    spawned_agent_count: 0,
                    thought: Vec::new(),
                    active: None,
                    response: Some("done".into()),
                }),
            }],
            awaiting_input: false,
            elicitation: None,
            duration_ms: 1,
            token_count: None,
            comment: Vec::new(),
            task: None,
        };
        store
            .save_agent_turn(&AgentTurnRecord {
                id: "turn".into(),
                session_id: "session".into(),
                agent_run_id: first.id.clone(),
                ordinal: 1,
                interaction,
            })
            .unwrap();

        assert_eq!(store.list_agent_run("session").unwrap().len(), 2);
        assert_eq!(store.list_agent_turn(&first.id).unwrap().len(), 1);
        assert!(store.list_agent_turn(&second.id).unwrap().is_empty());
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
    fn hides_outdated_sessions_without_deleting_preferences() {
        let temporary = tempfile::tempdir().unwrap();
        let mut store = SqliteStore::open(temporary.path()).unwrap();
        let preference = HarnessPreference {
            model: "remembered-model".into(),
            effort: "low".into(),
            model_setting: Default::default(),
            fast_mode: true,
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
