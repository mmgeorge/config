//! Committed admission state shared by every storage handle in one host.
//!
//! Bind once while opening a session. Admission reads this projection without SQLite I/O.
//! Writers publish after committing and before returning. Publication failure invalidates the
//! projection, so input never proceeds using a silently stale task or question state.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, Mutex, OnceLock, Weak},
};

use anyhow::{Context, Result};
use rusqlite::OptionalExtension;
use serde::Deserialize;

use super::SqliteStore;

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
pub(crate) struct AdmissionSession {
    pub id: String,
    pub current_task_id: Option<String>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
pub(crate) struct AdmissionTask {
    pub id: String,
    pub status: crate::task::TaskStatus,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
pub(crate) struct AdmissionSnapshot {
    pub session: AdmissionSession,
    pub task: Option<AdmissionTask>,
    pub operation_state: Option<String>,
    pub has_elicitation: bool,
}

struct PublishedAdmission {
    snapshot: Result<Arc<AdmissionSnapshot>, String>,
    exchange: HashMap<String, bool>,
}

/// Read capability for a committed session projection. It cannot mutate session state.
pub(crate) struct SessionAdmission {
    state: Mutex<PublishedAdmission>,
    pub changed: Arc<tokio::sync::Notify>,
}

impl SessionAdmission {
    pub(crate) fn snapshot(&self) -> Result<Arc<AdmissionSnapshot>> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("admission state lock poisoned"))?;
        state
            .snapshot
            .as_ref()
            .cloned()
            .map_err(|error| anyhow::anyhow!("session admission unavailable: {error}"))
    }

    pub(crate) fn owns_exchange(&self, id: &str) -> Result<bool> {
        Ok(self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("admission state lock poisoned"))?
            .exchange
            .contains_key(id))
    }
}

/// One registry per canonical database directory, shared across independent connections.
pub(super) struct AdmissionRegistry {
    session: Mutex<HashMap<String, Weak<SessionAdmission>>>,
}

pub(super) fn registry(root: &Path) -> Result<Arc<AdmissionRegistry>> {
    static REGISTRY: OnceLock<Mutex<HashMap<PathBuf, Weak<AdmissionRegistry>>>> = OnceLock::new();
    let root = std::fs::canonicalize(root)?;
    let mut registry = REGISTRY
        .get_or_init(Default::default)
        .lock()
        .map_err(|_| anyhow::anyhow!("admission registry lock poisoned"))?;
    registry.retain(|_, entry| entry.strong_count() > 0);
    if let Some(existing) = registry.get(&root).and_then(Weak::upgrade) {
        return Ok(existing);
    }
    let created = Arc::new(AdmissionRegistry {
        session: Mutex::new(HashMap::new()),
    });
    registry.insert(root, Arc::downgrade(&created));
    Ok(created)
}

impl AdmissionRegistry {
    fn bind(&self, store: &SqliteStore, session_id: &str) -> Result<Arc<SessionAdmission>> {
        let mut session = self
            .session
            .lock()
            .map_err(|_| anyhow::anyhow!("admission registry lock poisoned"))?;
        session.retain(|_, entry| entry.strong_count() > 0);
        if let Some(existing) = session.get(session_id).and_then(Weak::upgrade) {
            return Ok(existing);
        }
        let (snapshot, exchange) = store.read_snapshot(|| {
            Ok((
                store.read_admission(session_id)?,
                store.admission_exchange_ids(session_id)?,
            ))
        })?;
        let created = Arc::new(SessionAdmission {
            state: Mutex::new(PublishedAdmission {
                snapshot: Ok(Arc::new(snapshot)),
                exchange,
            }),
            changed: Arc::new(tokio::sync::Notify::new()),
        });
        session.insert(session_id.to_owned(), Arc::downgrade(&created));
        Ok(created)
    }

    fn publish(
        &self,
        store: &SqliteStore,
        session_id: &str,
        membership: Option<(&str, Option<bool>)>,
        refresh_membership: bool,
    ) -> Result<()> {
        let session = self
            .session
            .lock()
            .map_err(|_| anyhow::anyhow!("admission registry lock poisoned"))?
            .get(session_id)
            .and_then(Weak::upgrade);
        let Some(session) = session else {
            return Ok(());
        };
        let mut current = session
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("admission state lock poisoned"))?;
        if !refresh_membership
            && current.snapshot.is_ok()
            && membership.is_some_and(|(id, waiting)| current.exchange.get(id).copied() == waiting)
        {
            return Ok(());
        }
        let observed = store.read_snapshot(|| {
            Ok((
                store.read_admission(session_id)?,
                membership.map(|(id, _)| {
                    store.connection.query_row(
                        "SELECT coalesce(json_extract(payload,'$.awaiting_input')=1 AND json_type(payload,'$.elicitation')='object',0) FROM exchange_record WHERE session_id=?1 AND id=?2",
                        rusqlite::params![session_id, id], |row| row.get::<_, bool>(0),
                    ).optional()
                }).transpose()?,
                refresh_membership
                    .then(|| store.admission_exchange_ids(session_id))
                    .transpose()?,
            ))
        });
        match observed {
            Ok((snapshot, committed_membership, refreshed)) => {
                let changed = current
                    .snapshot
                    .as_ref()
                    .map_or(true, |previous| previous.as_ref() != &snapshot);
                if changed {
                    current.snapshot = Ok(Arc::new(snapshot));
                }
                if let Some(exchange) = refreshed {
                    current.exchange = exchange;
                }
                if let (Some((id, _)), Some(waiting)) = (membership, committed_membership) {
                    if let Some(waiting) = waiting {
                        current.exchange.insert(id.to_owned(), waiting);
                    } else {
                        current.exchange.remove(id);
                    }
                }
                if changed {
                    session.changed.notify_one();
                }
                Ok(())
            }
            Err(error) => {
                current.snapshot = Err(format!("{error:#}"));
                session.changed.notify_one();
                Err(error.context("publish committed session admission state"))
            }
        }
    }
}

impl SqliteStore {
    pub(crate) fn session_admission(&self, session_id: &str) -> Result<Arc<SessionAdmission>> {
        self.admission.bind(self, session_id)
    }

    /// Complete a relevant write by publishing its committed projection to the runtime owner.
    /// Call only after the transaction commits. The returned error leaves admission blocked.
    pub(super) fn publish_admission(&self, session_id: &str) -> Result<()> {
        self.admission.publish(self, session_id, None, false)
    }

    pub(super) fn publish_admission_exchange(
        &self,
        session_id: &str,
        id: &str,
        waiting: Option<bool>,
    ) -> Result<()> {
        self.admission
            .publish(self, session_id, Some((id, waiting)), false)
    }

    pub(super) fn refresh_admission_membership(&self, session_id: &str) -> Result<()> {
        self.admission.publish(self, session_id, None, true)
    }

    fn read_admission(&self, session_id: &str) -> Result<AdmissionSnapshot> {
        let encoded: Option<String> = self.connection.query_row(
            "SELECT json_object(
                'session',json_object('id',s.id,'current_task_id',json_extract(s.payload,'$.session.current_task_id')),
                'task',CASE WHEN t.id IS NULL THEN NULL ELSE json_object('id',t.id,'status',json_extract(t.payload,'$.status')) END,
                'operation_state',(SELECT json_extract(o.payload,'$.state') FROM task_operation o WHERE o.session_id=s.id ORDER BY o.rowid DESC LIMIT 1),
                'has_elicitation',json(CASE WHEN (
                    (coalesce(json_extract(t.payload,'$.status'),'')!='paused' AND
                     (json_type(p.payload,'$.acceptance')='object' OR json_type(p.payload,'$.elicitation')='object'))
                    OR EXISTS(SELECT 1 FROM exchange_record e WHERE e.session_id=s.id AND e.agent_id=?2
                        AND json_extract(e.payload,'$.awaiting_input')=1 AND json_type(e.payload,'$.elicitation')='object')
                ) THEN 'true' ELSE 'false' END))
             FROM session_record s
             LEFT JOIN task_record t ON t.id=json_extract(s.payload,'$.session.current_task_id')
             LEFT JOIN plan_record p ON p.id=json_extract(t.payload,'$.plan_id') AND json_extract(p.payload,'$.schema_version')=?3
             WHERE s.id=?1 AND json_extract(s.payload,'$.format_version')=?4",
            rusqlite::params![session_id, crate::session::HarnessSession::primary_agent_id(session_id), crate::plan::PLAN_SCHEMA_VERSION, super::SESSION_FORMAT_VERSION],
            |row| row.get(0),
        ).optional()?;
        Ok(serde_json::from_str(&encoded.context(
            "admission session is missing or incompatible",
        )?)?)
    }

    fn admission_exchange_ids(&self, session_id: &str) -> Result<HashMap<String, bool>> {
        let mut statement = self
            .connection
            .prepare("SELECT id,coalesce(json_extract(payload,'$.awaiting_input')=1 AND json_type(payload,'$.elicitation')='object',0) FROM exchange_record WHERE session_id=?1")?;
        Ok(statement
            .query_map([session_id], |row| Ok((row.get(0)?, row.get(1)?)))?
            .collect::<rusqlite::Result<_>>()?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        session::SessionStore,
        task::{TaskKind, TaskRecord, TaskStatus},
    };

    #[test]
    fn committed_admission_tracks_independent_writers_and_preserves_failed_writes() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut owner = SqliteStore::open(root.path())?;
        let mut session = super::super::test::session("session", "workspace");
        owner.save_session(&session)?;
        let admission = owner.session_admission(&session.id)?;
        let mut writer = SqliteStore::open(root.path())?;
        assert!(Arc::ptr_eq(
            &admission,
            &writer.session_admission(&session.id)?
        ));
        let mut task = TaskRecord::new(
            session.id.clone(),
            "task".into(),
            TaskKind::Plan,
            session.access.clone(),
            session.approval_mode,
            1,
        );
        writer.save_task(&task)?;
        session.current_task_id = Some(task.id.clone());
        writer.save_session(&session)?;
        assert_eq!(
            admission.snapshot()?.task.as_ref().unwrap().status,
            TaskStatus::Ready
        );
        writer.connection.execute_batch("CREATE TRIGGER reject_task BEFORE UPDATE ON task_record BEGIN SELECT RAISE(ABORT,'failed commit'); END;")?;
        task.status = TaskStatus::Paused;
        assert!(writer.save_task(&task).is_err());
        assert_eq!(
            admission.snapshot()?.task.as_ref().unwrap().status,
            TaskStatus::Ready
        );
        writer
            .connection
            .execute_batch("DROP TRIGGER reject_task")?;
        writer.save_task(&task)?;
        assert_eq!(
            admission.snapshot()?.task.as_ref().unwrap().status,
            TaskStatus::Paused
        );
        let previous = admission.snapshot()?;
        task.title = "renamed".into();
        writer.save_task(&task)?;
        assert!(Arc::ptr_eq(&previous, &admission.snapshot()?));
        Ok(())
    }

    #[test]
    fn admission_reads_no_database_and_refresh_failure_blocks_until_repaired() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut owner = SqliteStore::open(root.path())?;
        let session = super::super::test::session("session", "workspace");
        owner.save_session(&session)?;
        let admission = owner.session_admission(&session.id)?;
        owner
            .connection
            .execute_batch("ALTER TABLE task_operation RENAME TO unavailable_operation")?;
        assert_eq!(admission.snapshot()?.session.id, session.id);
        assert!(owner.publish_admission(&session.id).is_err());
        assert!(admission.snapshot().is_err());
        owner
            .connection
            .execute_batch("ALTER TABLE unavailable_operation RENAME TO task_operation")?;
        owner.publish_admission(&session.id)?;
        assert_eq!(admission.snapshot()?.session.id, session.id);
        Ok(())
    }

    #[test]
    fn question_and_exchange_membership_updates_are_isolated_between_sessions() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut owner = SqliteStore::open(root.path())?;
        for id in ["session", "other"] {
            owner.save_session(&super::super::test::session(id, "workspace"))?;
        }
        let admission = owner.session_admission("session")?;
        let other = owner.session_admission("other")?;
        let unchanged = other.snapshot()?;
        let mut exchange: crate::exchange::Exchange = serde_json::from_value(serde_json::json!({
            "id":"exchange","session_id":"session","agent_id":"session:agent:primary","ordinal":1,
            "prompt":"question","kind":"chat","state":"complete","turn":[],"node_list":[],
            "created_at_ms":1,"attributed_matches_checkpoint":false,"awaiting_input":true
        }))?;
        exchange.elicitation = Some(crate::plan::PlanElicitation::new(serde_json::from_value(
            serde_json::json!({
                "id":"set","questions":[{"id":"question","header":"Choice","question":"Which?",
                    "options":[{"label":"First","description":"First choice","recommended":true}],"allow_freeform":true}]
            }),
        )?));
        let mut writer = SqliteStore::open(root.path())?;
        writer.save_exchange(&exchange)?;
        assert!(admission.snapshot()?.has_elicitation);
        assert!(admission.owns_exchange("exchange")?);
        assert!(!other.owns_exchange("exchange")?);
        assert!(Arc::ptr_eq(&unchanged, &other.snapshot()?));
        exchange.elicitation = None;
        exchange.awaiting_input = false;
        writer.save_exchange(&exchange)?;
        assert!(!admission.snapshot()?.has_elicitation);
        writer.delete_exchange("exchange")?;
        assert!(!admission.owns_exchange("exchange")?);
        owner.publish_admission_exchange("session", "exchange", Some(false))?;
        assert!(
            !admission.owns_exchange("exchange")?,
            "a delayed publisher restored deleted ownership"
        );
        Ok(())
    }
}
