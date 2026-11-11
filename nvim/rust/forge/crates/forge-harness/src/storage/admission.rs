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
}

/// Read capability for a committed session projection. It cannot mutate session state.
pub(crate) struct SessionAdmission {
    task_mutation: Mutex<()>,
    configuration_lane: Arc<tokio::sync::Mutex<()>>,
    configuration_uncertain: std::sync::atomic::AtomicBool,
    state: Mutex<PublishedAdmission>,
    pub changed: Arc<tokio::sync::Notify>,
}

impl SessionAdmission {
    /// Share one configuration write lane among all controllers of this session.
    pub(crate) fn configuration_lane(&self) -> Arc<tokio::sync::Mutex<()>> {
        self.configuration_lane.clone()
    }

    /// Block configuration writes until the old provider generation is explicitly replaced.
    pub(crate) fn mark_configuration_uncertain(&self) {
        self.configuration_uncertain
            .store(true, std::sync::atomic::Ordering::Release);
        self.changed.notify_one();
    }

    pub(crate) fn ensure_configuration_certain(&self) -> Result<()> {
        if self
            .configuration_uncertain
            .load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(crate::provider::ProviderError::DeliveryUncertain.into());
        }
        Ok(())
    }

    pub(crate) fn configuration_is_uncertain(&self) -> bool {
        self.configuration_uncertain
            .load(std::sync::atomic::Ordering::Acquire)
    }

    /// Call only after provider reset and owner generation replacement have completed.
    pub(crate) fn reset_configuration_generation(&self) -> Result<()> {
        self.configuration_uncertain
            .store(false, std::sync::atomic::Ordering::Release);
        self.changed.notify_one();
        Ok(())
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
        // The writer thread publishes admission after its commits. Wait for this handle's
        // queued writes before taking a registry lock, or the two threads wait on each other.
        store.settle_write_behind();
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
            configuration_lane: Arc::new(tokio::sync::Mutex::new(())),
            configuration_uncertain: std::sync::atomic::AtomicBool::new(false),
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
        // The writer thread publishes admission after its commits. Wait for this handle's
        // queued writes before taking a registry lock, or the two threads wait on each other.
        store.settle_write_behind();
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
        Ok(RuntimeSnapshot { active_task, plan })
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
        let encoded: Option<String> = self
            .connection
            .query_row(
                "SELECT json_object(
                'session',json_object('id',s.id),
                'task_revision',json_extract(s.payload,'$.session.active_task.revision'),
                'task',NULL,
                'has_elicitation',json('false'))
             FROM session_record s
             WHERE s.id=?1 AND json_extract(s.payload,'$.format_version')=?2",
                rusqlite::params![session_id, super::SESSION_FORMAT_VERSION],
                |row| row.get(0),
            )
            .optional()?;
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
                    && (plan
                        .artifact
                        .review_questions()
                        .is_some_and(|questions| questions.has_pending_decisions())
                        || plan.artifact.elicitation.is_some())
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
