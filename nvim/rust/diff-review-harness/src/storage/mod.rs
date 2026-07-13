use crate::checkpoint::CheckpointRecord;
use crate::goal::GoalRecord;
use crate::interaction::{InteractionComment, InteractionRecord};
use crate::plan::{PlanExecutionRecord, PlanLifecycleRecord, PlanRecord};
use crate::session::{HarnessPreference, HarnessSession, SessionStore};
use anyhow::{Context, Result};
use rusqlite::{Connection, OptionalExtension, TransactionBehavior, params};
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// Owns SQLite metadata and content-addressed objects for the Harness broker.
pub struct SqliteStore {
    connection: Connection,
    object_root: PathBuf,
}

impl SqliteStore {
    /// Open durable storage and apply idempotent schema migrations.
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
        let schema_version: u32 =
            connection.pragma_query_value(None, "user_version", |row| row.get(0))?;
        if schema_version < 2 {
            connection.execute_batch(
                r#"
                PRAGMA foreign_keys=OFF;
                DROP TABLE IF EXISTS transcript_event;
                DROP TABLE IF EXISTS interaction_comment;
                DROP TABLE IF EXISTS checkpoint_record;
                DROP TABLE IF EXISTS goal_record;
                DROP TABLE IF EXISTS plan_record;
                DROP TABLE IF EXISTS interaction_record;
                DROP TABLE IF EXISTS session_record;
                PRAGMA foreign_keys=ON;
                "#,
            )?;
        }
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
            PRAGMA user_version=4;
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
        let mut session: HarnessSession = decode(&payload)?;
        session.acquire_lease(client_id, now_ms)?;
        transaction.execute(
            "UPDATE session_record SET payload=?2 WHERE id=?1",
            params![session_id, encode(&session)?],
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
        if let Some(payload) = payload {
            let mut session: HarnessSession = decode(&payload)?;
            if session.lease_owner.as_deref() == Some(client_id) {
                session.lease_owner = None;
                session.lease_expires_at_ms = None;
                transaction.execute(
                    "UPDATE session_record SET payload=?2 WHERE id=?1",
                    params![session_id, encode(&session)?],
                )?;
            }
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
        let mut session: HarnessSession = decode(&payload)?;
        anyhow::ensure!(
            session.lease_owner.as_deref() == Some(client_id),
            "Harness session lease ownership changed"
        );
        session.lease_expires_at_ms = Some(now_ms + 30_000);
        transaction.execute(
            "UPDATE session_record SET payload=?2 WHERE id=?1",
            params![session_id, encode(&session)?],
        )?;
        transaction.commit()?;
        Ok(())
    }

    /// Save active session state only while the requesting client owns its lease.
    pub fn save_owned_session(&mut self, session: &HarnessSession, client_id: &str) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let owner: Option<String> = transaction
            .query_row(
                "SELECT payload FROM session_record WHERE id=?1",
                [&session.id],
                |row| row.get::<_, String>(0),
            )
            .optional()?
            .map(|payload| decode::<HarnessSession>(&payload))
            .transpose()?
            .and_then(|stored| stored.lease_owner);
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
                encode(session)?
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
}

impl SessionStore for SqliteStore {
    fn save_session(&mut self, session: &HarnessSession) -> Result<()> {
        self.connection.execute(
            "INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES(?1, ?2, ?3, ?4)\
             ON CONFLICT(id) DO UPDATE SET workspace=excluded.workspace, updated_at_ms=excluded.updated_at_ms, payload=excluded.payload",
            params![session.id, session.workspace, session.updated_at_ms, encode(session)?],
        )?;
        Ok(())
    }

    fn load_session(&self, session_id: &str) -> Result<Option<HarnessSession>> {
        self.load_payload(
            "SELECT payload FROM session_record WHERE id=?1",
            [session_id],
        )
    }

    fn list_session(&self, workspace: Option<&str>) -> Result<Vec<HarnessSession>> {
        match workspace {
            Some(path) => self.list_payload(
                "SELECT payload FROM session_record WHERE workspace=?1 ORDER BY updated_at_ms DESC",
                [path],
            ),
            None => self.list_payload(
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::session::WriteMode;

    fn session(id: &str, workspace: &str) -> HarnessSession {
        HarnessSession {
            id: id.into(),
            name: id.into(),
            workspace: workspace.into(),
            backend: "acp".into(),
            backend_session_id: None,
            model: "default".into(),
            provider_label: "ACP agent".into(),
            resolved_model: None,
            effort: "medium".into(),
            fast_mode: false,
            trust_profile: "workspace".into(),
            write_mode: WriteMode::Read,
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
    fn timeline_migration_preserves_preferences_and_discards_legacy_sessions() {
        let temporary = tempfile::tempdir().unwrap();
        let database_path = temporary.path().join("harness.sqlite3");
        let connection = Connection::open(&database_path).unwrap();
        connection
            .execute_batch(
                r#"
                CREATE TABLE preference_record (
                    workspace TEXT NOT NULL,
                    backend TEXT NOT NULL,
                    payload TEXT NOT NULL,
                    PRIMARY KEY(workspace, backend)
                );
                CREATE TABLE session_record (
                    id TEXT PRIMARY KEY,
                    workspace TEXT NOT NULL,
                    updated_at_ms INTEGER NOT NULL,
                    payload TEXT NOT NULL
                );
                CREATE TABLE transcript_event (
                    sequence INTEGER PRIMARY KEY AUTOINCREMENT,
                    session_id TEXT NOT NULL,
                    payload TEXT NOT NULL
                );
                PRAGMA user_version=1;
                "#,
            )
            .unwrap();
        let preference = HarnessPreference {
            model: "remembered-model".into(),
            effort: "low".into(),
            fast_mode: true,
        };
        connection
            .execute(
                "INSERT INTO preference_record(workspace, backend, payload) VALUES(?1, ?2, ?3)",
                params!["D:/work", "codex", encode(&preference).unwrap()],
            )
            .unwrap();
        connection
            .execute(
                "INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES('old', 'D:/work', 1, '{}')",
                [],
            )
            .unwrap();
        drop(connection);

        let store = SqliteStore::open(temporary.path()).unwrap();
        let restored = store.load_preference("D:/work", "codex").unwrap().unwrap();
        assert_eq!(restored.model, "remembered-model");
        assert_eq!(restored.effort, "low");
        assert!(restored.fast_mode);
        assert!(store.list_session(None).unwrap().is_empty());
        let legacy_table: Option<String> = store
            .connection
            .query_row(
                "SELECT name FROM sqlite_master WHERE type='table' AND name='transcript_event'",
                [],
                |row| row.get(0),
            )
            .optional()
            .unwrap();
        assert!(legacy_table.is_none());
    }

    #[test]
    fn version_two_migration_preserves_durable_sessions() {
        let temporary = tempfile::tempdir().unwrap();
        {
            let mut store = SqliteStore::open(temporary.path()).unwrap();
            store
                .save_session(&session("preserved", "D:/work"))
                .unwrap();
            store
                .connection
                .pragma_update(None, "user_version", 2)
                .unwrap();
        }
        let store = SqliteStore::open(temporary.path()).unwrap();
        assert!(store.load_session("preserved").unwrap().is_some());
        let schema_version: u32 = store
            .connection
            .pragma_query_value(None, "user_version", |row| row.get(0))
            .unwrap();
        assert_eq!(schema_version, 4);
    }
}
