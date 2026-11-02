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
    task_generation: u64,
}

#[derive(Clone, Debug)]
/// Active lifecycle values published atomically with admission, excluding historical output.
pub(crate) struct RuntimeSnapshot {
    pub active_task: Option<crate::task::Task>,
    pub plan: Option<crate::session::state_machine::PlanStatus>,
    pub configuring: bool,
}

/// Read capability for a committed session projection. It cannot mutate session state.
pub(crate) struct SessionAdmission {
    task_mutation: Mutex<()>,
    native_goal_lane: Arc<tokio::sync::Mutex<()>>,
    native_goal_epoch: std::sync::atomic::AtomicU64,
    native_goal_acknowledged: std::sync::atomic::AtomicU64,
    native_goal_uncertain: std::sync::atomic::AtomicBool,
    native_goal_reconciled: tokio::sync::Notify,
    state: Mutex<PublishedAdmission>,
    pub changed: Arc<tokio::sync::Notify>,
}

impl SessionAdmission {
    /// Share one native goal transport lane among all controllers of this session.
    pub(crate) fn native_goal_lane(&self) -> Arc<tokio::sync::Mutex<()>> {
        self.native_goal_lane.clone()
    }

    /// Invalidate any cached native goal acknowledgment after a transport write.
    pub(crate) fn native_goal_written(&self) -> Result<u64> {
        let previous = match self.native_goal_epoch.try_update(
            std::sync::atomic::Ordering::AcqRel,
            std::sync::atomic::Ordering::Acquire,
            |epoch| epoch.checked_add(1),
        ) {
            Ok(previous) => previous,
            Err(_) => {
                self.mark_native_goal_uncertain();
                return Err(crate::provider::ProviderError::DeliveryUncertain { message: "native goal authority epoch exhausted; provider generation must be replaced".into() }.into());
            }
        };
        let epoch = previous + 1;
        self.changed.notify_one();
        Ok(epoch)
    }

    pub(crate) fn native_goal_epoch(&self) -> u64 {
        self.native_goal_epoch
            .load(std::sync::atomic::Ordering::Acquire)
    }

    /// Block native goal writes until the old provider generation is explicitly replaced.
    pub(crate) fn mark_native_goal_uncertain(&self) {
        self.native_goal_uncertain
            .store(true, std::sync::atomic::Ordering::Release);
        self.changed.notify_one();
    }

    pub(crate) fn ensure_native_goal_certain(&self) -> Result<()> {
        if self
            .native_goal_uncertain
            .load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(crate::provider::ProviderError::DeliveryUncertain {
                message: "native goal transport is uncertain; replace the provider generation before retrying".into(),
            }.into());
        }
        Ok(())
    }

    pub(crate) fn native_goal_is_uncertain(&self) -> bool {
        self.native_goal_uncertain
            .load(std::sync::atomic::Ordering::Acquire)
    }

    /// Call only after provider reset and owner generation replacement have completed.
    pub(crate) fn reset_native_goal_generation(&self) -> Result<()> {
        self.native_goal_uncertain
            .store(false, std::sync::atomic::Ordering::Release);
        self.native_goal_written()?;
        Ok(())
    }

    /// Publish a canonical provider acknowledgment only after the task has been rechecked.
    pub(crate) fn acknowledge_native_goal(&self, epoch: u64) {
        self.native_goal_acknowledged
            .fetch_max(epoch, std::sync::atomic::Ordering::AcqRel);
        self.native_goal_reconciled.notify_waiters();
    }

    pub(crate) async fn wait_native_goal_acknowledged(&self, epoch: u64) {
        loop {
            let notified = self.native_goal_reconciled.notified();
            tokio::pin!(notified);
            notified.as_mut().enable();
            if self
                .native_goal_acknowledged
                .load(std::sync::atomic::Ordering::Acquire)
                >= epoch
            {
                return;
            }
            notified.await;
        }
    }
    /// Serialize task authority changes with their dependent filesystem commits.
    ///
    /// Acquire this gate before SQLite write admission and retain it through publication.
    /// The closure must be synchronous and must not enter another task mutation.
    pub(crate) fn with_task_mutation<Output>(
        &self,
        commit: impl FnOnce() -> Result<Output>,
    ) -> Result<Output> {
        // Lock order is task mutation, SQLite transaction, then published admission state.
        let _guard = self
            .task_mutation
            .lock()
            .map_err(|_| anyhow::anyhow!("task mutation gate poisoned"))?;
        commit()
    }

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

    /// Capture task identity and its monotonic replacement generation under the mutation gate.
    pub(crate) fn callback_snapshot(&self) -> Result<(u64, Option<crate::task::Task>)> {
        self.with_task_mutation(|| {
            let state = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("admission state lock poisoned"))?;
            state
                .snapshot
                .as_ref()
                .map_err(|error| anyhow::anyhow!("session admission unavailable: {error}"))?;
            Ok((state.task_generation, state.runtime.active_task.clone()))
        })
    }

    /// Validate captured authority and run only a short synchronous mutation.
    pub(crate) fn with_callback_authority<T>(
        &self,
        generation: u64,
        permit: Option<&crate::task::TaskPermit>,
        commit: impl FnOnce() -> Result<T>,
    ) -> Result<T> {
        self.with_task_mutation(|| {
            {
                let state = self
                    .state
                    .lock()
                    .map_err(|_| anyhow::anyhow!("admission state lock poisoned"))?;
                state
                    .snapshot
                    .as_ref()
                    .map_err(|error| anyhow::anyhow!("session admission unavailable: {error}"))?;
                if state.task_generation != generation {
                    return Err(crate::task::StaleTask.into());
                }
                match permit {
                    Some(permit) => permit.validate(state.runtime.active_task.as_ref())?,
                    None if state.runtime.active_task.is_some() => {
                        return Err(crate::task::StaleTask.into());
                    }
                    None => {}
                }
            }
            commit()
        })
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
            task_mutation: Mutex::new(()),
            native_goal_lane: Arc::new(tokio::sync::Mutex::new(())),
            native_goal_epoch: std::sync::atomic::AtomicU64::new(0),
            native_goal_acknowledged: std::sync::atomic::AtomicU64::new(0),
            native_goal_uncertain: std::sync::atomic::AtomicBool::new(false),
            native_goal_reconciled: tokio::sync::Notify::new(),
            state: Mutex::new(PublishedAdmission {
                snapshot: Ok(Arc::new(snapshot)),
                exchange,
                runtime: Arc::new(runtime),
                task_generation: 0,
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
                let previous_id = current
                    .runtime
                    .active_task
                    .as_ref()
                    .map(|task| task.id.clone());
                let next_id = runtime.active_task.as_ref().map(|task| task.id.clone());
                if previous_id != next_id {
                    let Some(generation) = current.task_generation.checked_add(1) else {
                        current.snapshot = Err("task callback generation exhausted".into());
                        session.changed.notify_one();
                        anyhow::bail!("task callback generation exhausted");
                    };
                    current.task_generation = generation;
                }
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
            Some(crate::task::WorkflowSnapshot::Plan { plan }) => Some((&plan.artifact).into()),
            Some(crate::task::WorkflowSnapshot::Execute { execution }) => self
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
            crate::task::WorkflowSnapshot::Plan { plan } => {
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
        task::{Task, TaskStatus},
    };

    /// Independent controllers share the goal lane, and stale acknowledgments cannot clear a newer write.
    #[tokio::test]
    async fn native_goal_lane_fences_poison_and_requires_current_acknowledgment() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        let session = super::super::test::session("goal-lane", "workspace");
        store.save_session(&session)?;
        let first = store.session_admission(&session.id)?;
        let second = SqliteStore::open(root.path())?.session_admission(&session.id)?;
        let first_lane = first.native_goal_lane();
        let second_lane = second.native_goal_lane();
        let held = first_lane.lock().await;
        assert!(second_lane.try_lock().is_err());
        drop(held);
        assert!(second_lane.try_lock().is_ok());
        let old = first.native_goal_written()?;
        first.acknowledge_native_goal(old);
        let current = second.native_goal_written()?;
        assert!(
            tokio::time::timeout(
                std::time::Duration::from_millis(10),
                second.wait_native_goal_acknowledged(current)
            )
            .await
            .is_err()
        );
        second.acknowledge_native_goal(current);
        second.wait_native_goal_acknowledged(current).await;
        first.mark_native_goal_uncertain();
        let error = second.ensure_native_goal_certain().unwrap_err();
        assert!(matches!(
            error.downcast_ref::<crate::provider::ProviderError>(),
            Some(crate::provider::ProviderError::DeliveryUncertain { .. })
        ));
        second.reset_native_goal_generation()?;
        first.ensure_native_goal_certain()?;
        first
            .native_goal_epoch
            .store(u64::MAX, std::sync::atomic::Ordering::Release);
        assert!(first.native_goal_written().is_err());
        assert!(first.native_goal_is_uncertain());
        Ok(())
    }

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
        let mut task = Task::goal("task".into(), Default::default(), false, 1);
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
        task.edit(|draft| {
            if let crate::task::WorkflowSnapshot::Goal { goal } = &mut draft.workflow {
                goal.state = crate::goal::GoalState::Paused;
            }
            Ok(())
        })?;
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
