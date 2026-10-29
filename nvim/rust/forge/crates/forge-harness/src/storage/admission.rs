//! Committed admission state shared by every storage handle in one host.
//!
//! Bind once while opening a session. Admission reads this projection without SQLite I/O.
//! Writers publish after committing and before returning. Publication failure invalidates the
//! projection, so input never proceeds using a silently stale task or question state.
//! The same commit boundary owns active task, plan status, execution status, and configuration
//! state. Rendering and dispatch read these values without reopening storage. Historical content
//! remains in SQLite and is loaded only by explicit history queries.

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
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
pub(crate) struct AdmissionTask {
    pub id: String,
    pub status: crate::task::TaskStatus,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
pub(crate) struct AdmissionSnapshot {
    pub session: AdmissionSession,
    pub task_revision: Option<crate::task::TaskRevision>,
    pub task: Option<AdmissionTask>,
    pub operation_state: Option<String>,
    pub operation_id: Option<String>,
    pub operation_action: Option<String>,
    pub has_elicitation: bool,
}

struct PublishedAdmission {
    snapshot: Result<Arc<AdmissionSnapshot>, String>,
    exchange: HashMap<String, bool>,
    runtime: Arc<RuntimeSnapshot>,
}

#[derive(Clone, Debug)]
/// Active lifecycle values published atomically with admission, excluding historical output.
pub(crate) struct RuntimeSnapshot {
    pub active_task: Option<crate::task::ActiveTask>,
    pub plan: Option<crate::session::state_machine::PlanStatus>,
    pub configuring: bool,
}

/// Read capability for a committed session projection. It cannot mutate session state.
pub(crate) struct SessionAdmission {
    state: Mutex<PublishedAdmission>,
    pub changed: Arc<tokio::sync::Notify>,
}

impl SessionAdmission {
    pub(crate) fn runtime(&self) -> Result<Arc<RuntimeSnapshot>> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("admission state lock poisoned"))?;
        state
            .snapshot
            .as_ref()
            .map_err(|error| anyhow::anyhow!("session admission unavailable: {error}"))?;
        Ok(state.runtime.clone())
    }
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
        let (snapshot, exchange, runtime) = store.read_snapshot(|| {
            let snapshot = store.read_admission(session_id)?;
            let runtime = store.read_runtime(&snapshot)?;
            Ok((snapshot, store.admission_exchange_ids(session_id)?, runtime))
        })?;
        let created = Arc::new(SessionAdmission {
            state: Mutex::new(PublishedAdmission {
                snapshot: Ok(Arc::new(snapshot)),
                exchange,
                runtime: Arc::new(runtime),
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
            let snapshot = store.read_admission(session_id)?;
            let runtime = store.read_runtime(&snapshot)?;
            Ok((
                snapshot,
                membership.map(|(id, _)| {
                    store.connection.query_row(
                        "SELECT coalesce(json_extract(payload,'$.awaiting_input')=1 AND json_type(payload,'$.elicitation')='object',0) FROM exchange_record WHERE session_id=?1 AND id=?2",
                        rusqlite::params![session_id, id], |row| row.get::<_, bool>(0),
                    ).optional()
                }).transpose()?,
                refresh_membership
                    .then(|| store.admission_exchange_ids(session_id))
                    .transpose()?,
                runtime,
            ))
        });
        match observed {
            Ok((snapshot, committed_membership, refreshed, runtime)) => {
                current.runtime = Arc::new(runtime);
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
    fn read_runtime(&self, snapshot: &AdmissionSnapshot) -> Result<RuntimeSnapshot> {
        let session_id = &snapshot.session.id;
        let active_task = self.load_active_task(session_id)?;
        let plan = match active_task.as_ref().map(|task| &task.workflow) {
            Some(crate::task::TaskWorkflow::Plan { plan }) => Some((&plan.artifact).into()),
            Some(crate::task::TaskWorkflow::Execute { execution }) => self
                .load_plan(&execution.record.plan_id)?
                .as_ref()
                .map(Into::into),
            _ => None,
        };
        let configuring = snapshot.operation_action.as_deref() == Some("configure")
            && matches!(
                snapshot.operation_state.as_deref(),
                Some("admitted" | "accepted" | "stopping" | "running" | "applying" | "verifying")
            );
        Ok(RuntimeSnapshot {
            active_task,
            plan,
            configuring,
        })
    }
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
                'session',json_object('id',s.id),
                'task_revision',json_extract(s.payload,'$.session.active_task.revision'),
                'task',NULL,
                'operation_state',json_extract(o.payload,'$.state'),
                'operation_id',o.id,
                'operation_action',json_extract(o.payload,'$.action.action'),
                'has_elicitation',json('false'))
             FROM session_record s
             LEFT JOIN task_operation o ON o.rowid=(SELECT rowid FROM task_operation WHERE session_id=s.id ORDER BY rowid DESC LIMIT 1)
             WHERE s.id=?1 AND json_extract(s.payload,'$.format_version')=?2",
            rusqlite::params![session_id, super::SESSION_FORMAT_VERSION],
            |row| row.get(0),
        ).optional()?;
        let mut snapshot: AdmissionSnapshot = serde_json::from_str(
            &encoded.context("admission session is missing or incompatible")?,
        )?;
        let active = self.load_active_task(session_id)?;
        snapshot.task = active.as_ref().map(|task| AdmissionTask {
            id: task.revision.as_str().into(),
            status: task.status(),
        });
        let plan_wait = active.as_ref().is_some_and(|task| match &task.workflow {
            crate::task::TaskWorkflow::Plan { plan } => {
                !plan.paused
                    && (plan.artifact.acceptance.is_some() || plan.artifact.elicitation.is_some())
            }
            _ => false,
        });
        let exchange_wait: bool = self.connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM exchange_record WHERE session_id=?1 AND agent_id=?2 AND json_extract(payload,'$.awaiting_input')=1 AND json_type(payload,'$.elicitation')='object')",
            rusqlite::params![session_id, crate::session::HarnessSession::primary_agent_id(session_id)], |row| row.get(0),
        )?;
        snapshot.has_elicitation = plan_wait || exchange_wait;
        Ok(snapshot)
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
        task::{ActiveTask, TaskStatus},
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
        let mut task = ActiveTask::goal("task".into(), Default::default(), false, 1);
        writer.commit_task_change(&session.id, None, 1, |active| {
            *active = Some(task.clone());
            Ok(())
        })?;
        assert_eq!(
            admission.snapshot()?.task.as_ref().unwrap().status,
            TaskStatus::Running
        );
        writer.connection.execute_batch("CREATE TRIGGER reject_task BEFORE UPDATE ON session_record BEGIN SELECT RAISE(ABORT,'failed commit'); END;")?;
        let previous_revision = task.revision.clone();
        if let crate::task::TaskWorkflow::Goal { goal } = &mut task.workflow {
            goal.state = crate::goal::GoalState::Paused;
        }
        task.renew(2);
        assert!(
            writer
                .commit_task_change(&session.id, Some(&previous_revision), 2, |active| {
                    *active = Some(task.clone());
                    Ok(())
                })
                .is_err()
        );
        assert_eq!(
            admission.snapshot()?.task.as_ref().unwrap().status,
            TaskStatus::Running
        );
        writer
            .connection
            .execute_batch("DROP TRIGGER reject_task")?;
        writer.commit_task_change(&session.id, Some(&previous_revision), 2, |active| {
            *active = Some(task.clone());
            Ok(())
        })?;
        assert_eq!(
            admission.snapshot()?.task.as_ref().unwrap().status,
            TaskStatus::Paused
        );
        let previous = admission.snapshot()?;
        session.model = "metadata-change".into();
        writer.save_session(&session)?;
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
        exchange.set_elicitation(Some(crate::plan::PlanElicitation::new(serde_json::from_value(
            serde_json::json!({
                "id":"set","questions":[{"id":"question","header":"Choice","question":"Which?",
                    "options":[{"label":"First","description":"First choice","recommended":true}],"allow_freeform":true}]
            }),
        )?)));
        let mut writer = SqliteStore::open(root.path())?;
        writer.save_exchange(&exchange)?;
        assert!(admission.snapshot()?.has_elicitation);
        assert!(admission.owns_exchange("exchange")?);
        assert!(!other.owns_exchange("exchange")?);
        assert!(Arc::ptr_eq(&unchanged, &other.snapshot()?));
        exchange.set_elicitation(None);
        exchange.set_awaiting_input(false);
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
