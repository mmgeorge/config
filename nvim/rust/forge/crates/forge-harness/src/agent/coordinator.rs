use std::{
    collections::{HashMap, VecDeque},
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use tokio::sync::{mpsc, oneshot, watch};

use super::{Agent, AgentSelection, AgentSource, AgentTask};
use crate::{
    exchange::ExchangeState,
    provider::{ExecutionOwner, ProviderCompletion, ProviderEvent},
};

pub const MAX_ACTIVE_CHILDREN: usize = 8;
pub const MAX_PENDING_TASKS: usize = 1024;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Describes a fresh child without inheriting parent transcript or workflow state.
pub struct SpawnAgentRequest {
    pub profile: Option<String>,
    pub text: String,
}

#[derive(Clone, Debug)]
/// Identifies an authenticated operation independently of model-supplied arguments.
pub struct AgentRequest {
    pub operation_id: String,
    pub source: AgentSource,
    pub operation: AgentOperation,
}

#[derive(Clone, Debug)]
/// Shares one contract between manual commands and authenticated root tools.
pub enum AgentOperation {
    Spawn(SpawnAgentRequest),
    Message {
        target: String,
        message: String,
    },
    Followup {
        target: String,
        message: String,
    },
    Interrupt {
        target: String,
    },
    InterruptAll,
    List {
        prefix: Option<String>,
    },
    Activity {
        target: String,
        limit: Option<usize>,
    },
    ToolOutput {
        call_ref: String,
        offset: usize,
        limit: usize,
    },
}

/// Retain the exact child and latest task selected before FIFO admission.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "StoredAgentTarget", into = "StoredAgentTarget")]
pub struct AgentTarget {
    agent_id: String,
    task_id: Option<String>,
}

impl AgentTarget {
    /// Get the stable child identity used by the coordinator.
    pub(crate) fn agent_id(&self) -> &str {
        &self.agent_id
    }
}

/// Reject malformed child identities when command journals or callers decode a target.
#[derive(Debug, thiserror::Error)]
#[error("child target requires nonempty agent and task identities")]
pub struct AgentTargetDecodeError;

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct StoredAgentTarget {
    agent_id: String,
    task_id: Option<String>,
}

impl TryFrom<StoredAgentTarget> for AgentTarget {
    type Error = AgentTargetDecodeError;
    /// Validate every persisted identity before allowing target use.
    fn try_from(stored: StoredAgentTarget) -> std::result::Result<Self, Self::Error> {
        let StoredAgentTarget { agent_id, task_id } = stored;
        if agent_id.trim().is_empty() || task_id.as_ref().is_some_and(|id| id.trim().is_empty()) {
            return Err(AgentTargetDecodeError);
        }
        Ok(Self { agent_id, task_id })
    }
}

impl From<AgentTarget> for StoredAgentTarget {
    /// Preserve a validated target without selecting a newer task.
    fn from(target: AgentTarget) -> Self {
        let AgentTarget { agent_id, task_id } = target;
        Self { agent_id, task_id }
    }
}

/// Serializes durable mutations without reacquiring an in-flight prompt's broker lock.
pub enum AgentCommand {
    Request {
        request: Box<AgentRequest>,
        authority: Option<AgentRequestAuthority>,
        target: Option<AgentTarget>,
        response: oneshot::Sender<Result<serde_json::Value>>,
        task_admission: Option<tokio::sync::OwnedSemaphorePermit>,
    },
    Event {
        owner: ExecutionOwner,
        task_id: String,
        event: Box<ProviderEvent>,
    },
    Completed {
        owner: ExecutionOwner,
        task_id: String,
        output: Box<Result<ProviderCompletion>>,
        cleanup_error: Option<String>,
    },
}

/// Retains authenticated turn ownership until the broker admits a model operation.
pub struct AgentRequestAuthority {
    pub owner: ExecutionOwner,
    pub parent_exchange_id: String,
    pub expired: watch::Receiver<bool>,
    pub(crate) task: Option<crate::tools::TaskAuthority>,
}

#[derive(Clone)]
/// Publishes bounded requests and wakes either the idle controller or active prompt.
pub struct AgentCommandSender {
    sender: mpsc::Sender<AgentCommand>,
    revision: watch::Sender<u64>,
    task_slots: Arc<tokio::sync::Semaphore>,
    state: Arc<Mutex<AgentStateStore>>,
}

impl AgentCommandSender {
    /// Reserve child task capacity before a public command enters its parent's FIFO.
    /// Existing operation identities retain immediate observation. Dropping this await or its
    /// acquired permit before command admission releases capacity without creating child work.
    pub(crate) async fn acquire_task_admission(
        &self,
        operation: &str,
    ) -> Result<Option<tokio::sync::OwnedSemaphorePermit>> {
        let mut revision = self.revision.subscribe();
        let acquire = self.task_slots.clone().acquire_owned();
        tokio::pin!(acquire);
        loop {
            if self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("agent state poisoned"))?
                .admission_by_operation
                .contains_key(operation)
            {
                return Ok(None);
            }
            tokio::select! {
                permit = &mut acquire => return Ok(Some(permit.context("child task admission is closed")?)),
                _ = self.sender.closed() => anyhow::bail!("agent coordinator is closed"),
                changed = revision.changed() => { changed.context("agent coordinator is closed")?; }
            }
        }
    }

    pub async fn send(&self, mut command: AgentCommand) -> Result<()> {
        if let AgentCommand::Request {
            request,
            authority,
            response,
            task_admission,
            ..
        } = &mut command
        {
            let creates_task = match &request.operation {
                AgentOperation::Spawn(_) | AgentOperation::Followup { .. } => true,
                AgentOperation::Message { .. }
                | AgentOperation::Interrupt { .. }
                | AgentOperation::InterruptAll
                | AgentOperation::List { .. }
                | AgentOperation::Activity { .. }
                | AgentOperation::ToolOutput { .. } => false,
            };
            let retained = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("agent state poisoned"))?
                .admission_by_operation
                .contains_key(&request.operation_id);
            if creates_task && !retained && task_admission.is_none() {
                let mut expired = authority
                    .as_ref()
                    .map(|authority| authority.expired.clone());
                let permit = tokio::select! {
                    permit = self.acquire_task_admission(&request.operation_id) => permit?,
                    _ = response.closed() => anyhow::bail!("child task caller is closed"),
                    _ = async {
                        match &mut expired {
                            Some(expired) => { let _ = expired.wait_for(|expired| *expired).await; }
                            None => std::future::pending().await,
                        }
                    } => anyhow::bail!("child task authority expired before admission"),
                };
                *task_admission = permit;
            }
        }
        self.sender
            .send(command)
            .await
            .context("agent coordinator is closed")?;
        self.revision
            .send_modify(|revision| *revision = revision.wrapping_add(1));
        Ok(())
    }

    pub async fn request(&self, request: AgentRequest) -> Result<serde_json::Value> {
        let (response, receiver) = oneshot::channel();
        self.send(AgentCommand::Request {
            request: Box::new(request),
            authority: None,
            target: None,
            response,
            task_admission: None,
        })
        .await?;
        receiver
            .await
            .context("agent operation ended without durable acknowledgment")?
    }

    /// Submit a parent-owned command with its captured child scope.
    pub(crate) async fn request_captured(
        &self,
        request: AgentRequest,
        target: Option<AgentTarget>,
        task_admission: Option<tokio::sync::OwnedSemaphorePermit>,
    ) -> Result<serde_json::Value> {
        let (response, receiver) = oneshot::channel();
        self.send(AgentCommand::Request {
            request: Box::new(request),
            authority: None,
            target,
            response,
            task_admission,
        })
        .await?;
        receiver
            .await
            .context("agent operation ended without durable acknowledgment")?
    }

    pub async fn request_authenticated(
        &self,
        request: AgentRequest,
        authority: AgentRequestAuthority,
    ) -> Result<serde_json::Value> {
        let (response, receiver) = oneshot::channel();
        self.send(AgentCommand::Request {
            request: Box::new(request),
            authority: Some(authority),
            target: None,
            response,
            task_admission: None,
        })
        .await?;
        receiver
            .await
            .context("agent operation ended without durable acknowledgment")?
    }
}

#[derive(Debug)]
/// Owns committed child work with immutable session, agent, task, and exchange identities.
pub struct AgentWork {
    pub agent: Agent,
    pub task: AgentTask,
    pub launch: bool,
    execution: AdmittedExecution,
}

#[derive(Debug)]
/// Exposes candidate data while only the coordinator can admit its execution.
pub struct ReservedAgentWork {
    pub agent: Agent,
    pub task: AgentTask,
    pub launch: bool,
}

#[derive(Clone, Debug)]
struct AdmittedExecution {
    owner: ExecutionOwner,
    task: crate::provider::TaskId,
    exchange: crate::provider::ExchangeId,
}

impl AgentWork {
    pub fn owner(&self) -> &ExecutionOwner {
        &self.execution.owner
    }
    pub fn task_id(&self) -> &crate::provider::TaskId {
        &self.execution.task
    }
    pub fn exchange_id(&self) -> &crate::provider::ExchangeId {
        &self.execution.exchange
    }
}

impl ReservedAgentWork {
    fn committed(&self) -> AgentWork {
        AgentWork {
            agent: self.agent.clone(),
            task: self.task.clone(),
            launch: self.launch,
            execution: AdmittedExecution {
                owner: ExecutionOwner {
                    session_id: self.agent.session_id.clone().into(),
                    agent_id: self.agent.id.clone().into(),
                },
                task: self.task.id.clone().into(),
                exchange: self.task.id.clone().into(),
            },
        }
    }
}

#[must_use]
/// Owns reserved capacity until commit, cancellation, or the last handle is dropped.
pub struct AgentAdmission {
    owner: Arc<Mutex<AgentStateStore>>,
    work: Arc<ReservedAgentWork>,
}

pub enum AgentCommit {
    Admitted(AgentWork),
    Existing(AgentWork),
}

enum AdmissionRelease {
    Cancel,
    LastHandle,
}

impl std::ops::Deref for AgentAdmission {
    type Target = ReservedAgentWork;

    fn deref(&self) -> &Self::Target {
        &self.work
    }
}

impl AgentAdmission {
    pub fn commit(self) -> Result<AgentCommit> {
        let mut state = self
            .owner
            .lock()
            .map_err(|_| anyhow::anyhow!("agent state poisoned"))?;
        let admission = &self.work;
        if state.task_by_id.contains_key(&admission.task.id) {
            let mut work = admission.committed();
            work.launch = state
                .execution_by_agent
                .get(&admission.agent.id)
                .is_some_and(|execution| execution.active_task_id == admission.task.id);
            return Ok(AgentCommit::Existing(work));
        }
        ensure!(
            state
                .admission_by_operation
                .get(&admission.task.operation_id)
                .is_some_and(|reserved| Arc::ptr_eq(reserved, admission)),
            "child reservation was cancelled"
        );
        let launch = !state.execution_by_agent.contains_key(&admission.agent.id)
            && !state.pending_launches.iter().any(|id| {
                state
                    .task_by_id
                    .get(id)
                    .is_some_and(|task| task.agent_id == admission.agent.id)
            })
            && state.pending_launches.is_empty()
            && state.execution_by_agent.len() < MAX_ACTIVE_CHILDREN
            && state.quiescence_count == 0;
        if launch {
            let (cancellation, _) = watch::channel(false);
            state.execution_by_agent.insert(
                admission.agent.id.clone(),
                ChildExecution {
                    active_task_id: admission.task.id.clone(),
                    pending: VecDeque::new(),
                    cancellation,
                    cleanup_failed: false,
                },
            );
        } else if let Some(execution) = state.execution_by_agent.get_mut(&admission.agent.id) {
            execution.pending.push_back(admission.task.id.clone());
        } else {
            state.pending_launches.push_back(admission.task.id.clone());
        }
        state
            .agent_by_id
            .insert(admission.agent.id.clone(), admission.agent.clone());
        state
            .task_by_id
            .insert(admission.task.id.clone(), admission.task.clone());
        let mut work = admission.committed();
        work.launch = launch;
        Ok(AgentCommit::Admitted(work))
    }

    pub fn cancel(self) -> Result<()> {
        self.release(AdmissionRelease::Cancel)
    }

    fn release(&self, mode: AdmissionRelease) -> Result<()> {
        let mut state = self
            .owner
            .lock()
            .map_err(|_| anyhow::anyhow!("agent state poisoned"))?;
        if matches!(mode, AdmissionRelease::LastHandle) && Arc::strong_count(&self.work) != 2 {
            return Ok(());
        }
        if !state.task_by_id.contains_key(&self.work.task.id)
            && state
                .admission_by_operation
                .get(&self.work.task.operation_id)
                .is_some_and(|reserved| Arc::ptr_eq(reserved, &self.work))
        {
            state
                .admission_by_operation
                .remove(&self.work.task.operation_id);
        }
        Ok(())
    }
}

impl Drop for AgentAdmission {
    fn drop(&mut self) {
        let _ = self.release(AdmissionRelease::LastHandle);
    }
}

struct ChildExecution {
    active_task_id: String,
    pending: VecDeque<String>,
    cancellation: watch::Sender<bool>,
    cleanup_failed: bool,
}

#[derive(Default)]
struct AgentStateStore {
    quiescence_count: usize,
    agent_by_id: HashMap<String, Agent>,
    task_by_id: HashMap<String, AgentTask>,
    admission_by_operation: HashMap<String, Arc<ReservedAgentWork>>,
    execution_by_agent: HashMap<String, ChildExecution>,
    pending_launches: VecDeque<String>,
    pending_task_slots: HashMap<String, tokio::sync::OwnedSemaphorePermit>,
}

/// Owns one root's identities, admission capacity, task queues, and cancellation.
pub struct AgentCoordinator {
    session_id: String,
    enabled: AtomicBool,
    state: Arc<Mutex<AgentStateStore>>,
    receiver: Mutex<mpsc::Receiver<AgentCommand>>,
    sender: AgentCommandSender,
    activity: watch::Sender<u64>,
    profiles: Mutex<Option<Arc<super::AgentProfileStore>>>,
}

#[must_use]
/// Rejects new child work until its owning cancellation or policy transition finishes.
pub struct AgentQuiescence {
    coordinator: Arc<AgentCoordinator>,
}

impl Drop for AgentQuiescence {
    fn drop(&mut self) {
        self.coordinator
            .state
            .lock()
            .expect("agent state poisoned")
            .quiescence_count -= 1;
        self.coordinator
            .sender
            .revision
            .send_modify(|revision| *revision = revision.wrapping_add(1));
    }
}

impl AgentCoordinator {
    pub fn new(session_id: String) -> Arc<Self> {
        let (sender, receiver) = mpsc::channel(64);
        let (revision, _) = watch::channel(0);
        let (activity, _) = watch::channel(0);
        let state = Arc::new(Mutex::new(AgentStateStore::default()));
        Arc::new(Self {
            session_id,
            enabled: AtomicBool::new(false),
            state: state.clone(),
            receiver: Mutex::new(receiver),
            sender: AgentCommandSender {
                sender,
                revision,
                task_slots: Arc::new(tokio::sync::Semaphore::new(MAX_PENDING_TASKS)),
                state,
            },
            activity,
            profiles: Mutex::new(None),
        })
    }

    pub fn sender(&self) -> AgentCommandSender {
        self.sender.clone()
    }

    /// Transfer an intake permit only to durably committed work still awaiting execution.
    /// Rejected work and active executions release intake capacity immediately. Keep this
    /// transfer after durable commit even if a later publication or response fails.
    pub(crate) fn retain_task_admission(
        &self,
        operation: &str,
        admission: Option<tokio::sync::OwnedSemaphorePermit>,
    ) {
        let Some(admission) = admission else {
            return;
        };
        {
            let mut state = self.state.lock().expect("agent state poisoned");
            if let Some(reserved) = state.admission_by_operation.get(operation) {
                let task = reserved.task.id.clone();
                if state.task_by_id.contains_key(&task)
                    && !state
                        .execution_by_agent
                        .get(&reserved.agent.id)
                        .is_some_and(|execution| execution.active_task_id == task)
                {
                    state.pending_task_slots.entry(task).or_insert(admission);
                }
            }
        }
        self.sender
            .revision
            .send_modify(|revision| *revision = revision.wrapping_add(1));
    }

    pub fn subscribe(&self) -> watch::Receiver<u64> {
        self.sender.revision.subscribe()
    }

    pub fn set_enabled(&self, enabled: bool) {
        self.enabled.store(enabled, Ordering::Release);
    }

    pub fn enabled(&self) -> bool {
        self.enabled.load(Ordering::Acquire)
    }

    pub fn set_profiles(&self, profiles: Arc<super::AgentProfileStore>) {
        *self.profiles.lock().expect("agent profiles poisoned") = Some(profiles);
    }

    pub fn profile_schema(&self) -> Option<serde_json::Value> {
        let profiles = self
            .profiles
            .lock()
            .expect("agent profiles poisoned")
            .clone()?;
        let snapshot = profiles.snapshot().ok()?;
        let names: Vec<_> = snapshot
            .document
            .profiles
            .iter()
            .filter(|(_, profile)| profile.enabled)
            .map(|(name, _)| name)
            .collect();
        let description = snapshot
            .document
            .profiles
            .iter()
            .filter(|(_, profile)| profile.enabled)
            .map(|(name, profile)| format!("{name}: {}", profile.description))
            .collect::<Vec<_>>()
            .join("\n");
        Some(serde_json::json!({"type":"string", "enum":names,
            "description":format!("Default: {}.\n{description}",snapshot.document.default_profile)}))
    }

    pub fn owns(&self, owner: &ExecutionOwner) -> bool {
        owner.session_id == forge_provider_api::SessionId::from(self.session_id.as_str())
    }

    pub fn activity(&self) -> watch::Receiver<u64> {
        self.activity.subscribe()
    }

    /// Returns the revision of committed child activity for incremental waits.
    pub fn activity_revision(&self) -> u64 {
        *self.activity.borrow()
    }

    /// Wakes waiters after a child event has reached durable state.
    pub fn notify_activity(&self) {
        self.activity
            .send_modify(|revision| *revision = revision.wrapping_add(1));
    }

    /// Returns completed results not yet delivered to the authenticated root.
    pub fn unreported_result(&self) -> Vec<AgentTask> {
        let state = self.state.lock().expect("agent state poisoned");
        let mut result: Vec<_> = state
            .task_by_id
            .values()
            .filter(|task| task.completed_at_ms.is_some() && !task.parent_reported)
            .cloned()
            .collect();
        result.sort_by_key(|task| (task.completed_at_ms, task.created_at_ms, task.ordinal));
        result
    }

    pub fn begin_quiescence(self: &Arc<Self>) -> AgentQuiescence {
        self.state
            .lock()
            .expect("agent state poisoned")
            .quiescence_count += 1;
        self.sender
            .revision
            .send_modify(|revision| *revision = revision.wrapping_add(1));
        AgentQuiescence {
            coordinator: Arc::clone(self),
        }
    }

    /// Prevents parent continuation while cancellation or a policy transition owns child cleanup.
    pub(crate) fn is_quiescing(&self) -> bool {
        self.state
            .lock()
            .expect("agent state poisoned")
            .quiescence_count
            > 0
    }

    pub fn try_receive(&self) -> Option<AgentCommand> {
        self.receiver
            .lock()
            .expect("agent request receiver poisoned")
            .try_recv()
            .ok()
    }

    pub fn reserve_spawn(
        &self,
        request: &AgentRequest,
        spawn: &SpawnAgentRequest,
        selection: AgentSelection,
        parent_exchange_id: &str,
        now_ms: i64,
    ) -> Result<AgentAdmission> {
        validate_message(&spawn.text)?;
        ensure!(
            matches!(request.source, AgentSource::User | AgentSource::Model),
            "only root callers can admit children"
        );
        let mut state = self.state.lock().expect("agent state poisoned");
        if let Some(existing) = state.admission_by_operation.get(&request.operation_id) {
            ensure!(
                existing.agent.definition == selection.profile
                    && existing.task.message == spawn.text
                    && existing.task.source == request.source
                    && existing.task.parent_exchange_id == parent_exchange_id,
                "operation identity was reused for different child work"
            );
            return Ok(AgentAdmission {
                owner: self.state.clone(),
                work: existing.clone(),
            });
        }
        ensure!(
            state.quiescence_count == 0,
            "child admission is closed during cancellation or a policy transition"
        );
        let reservation_count = state
            .admission_by_operation
            .values()
            .filter(|admission| {
                admission.launch
                    && !state.execution_by_agent.contains_key(&admission.agent.id)
                    && !state.task_by_id.contains_key(&admission.task.id)
            })
            .count();
        let launch = state.execution_by_agent.len() + reservation_count < MAX_ACTIVE_CHILDREN
            && state.pending_launches.is_empty();
        ensure!(
            pending_task_count(&state) < MAX_PENDING_TASKS || launch,
            "child task queue already has 1024 pending tasks"
        );
        let mut ordinal = 1;
        let task_name = loop {
            let candidate = format!("{}-{ordinal}", selection.profile);
            if !state
                .agent_by_id
                .values()
                .any(|agent| agent.task_name == candidate)
                && !state
                    .admission_by_operation
                    .values()
                    .any(|admission| admission.agent.task_name == candidate)
            {
                break candidate;
            }
            ordinal += 1;
        };
        let mut agent = Agent::pending(&self.session_id, &selection.profile, &spawn.text, now_ms);
        agent.task_path = format!("/root/{task_name}");
        agent.task_name = task_name;
        agent.source = request.source;
        agent.selection = Some(selection);
        let task = AgentTask {
            id: uuid::Uuid::new_v4().to_string(),
            session_id: self.session_id.clone(),
            agent_id: agent.id.clone(),
            operation_id: request.operation_id.clone(),
            ordinal: 1,
            source: request.source,
            parent_exchange_id: parent_exchange_id.into(),
            message: spawn.text.clone(),
            state: ExchangeState::Queued,
            final_response: None,
            error: None,
            parent_reported: false,
            created_at_ms: now_ms,
            last_activity_at_ms: None,
            completed_at_ms: None,
        };
        let admission = Arc::new(ReservedAgentWork {
            agent,
            task,
            launch,
        });
        state
            .admission_by_operation
            .insert(request.operation_id.clone(), admission.clone());
        Ok(AgentAdmission {
            owner: self.state.clone(),
            work: admission,
        })
    }

    pub fn reserve_followup(
        &self,
        request: &AgentRequest,
        target: &str,
        message: &str,
        parent_exchange_id: &str,
        now_ms: i64,
    ) -> Result<AgentAdmission> {
        validate_message(message)?;
        let mut state = self.state.lock().expect("agent state poisoned");
        let agent = resolve_agent(&state, &self.session_id, target)?.clone();
        if let Some(existing) = state.admission_by_operation.get(&request.operation_id) {
            ensure!(
                existing.agent.id == agent.id
                    && existing.task.message == message
                    && existing.task.source == request.source
                    && existing.task.parent_exchange_id == parent_exchange_id,
                "operation identity was reused for different child work"
            );
            return Ok(AgentAdmission {
                owner: self.state.clone(),
                work: existing.clone(),
            });
        }
        ensure!(
            state.quiescence_count == 0,
            "child admission is closed during cancellation or a policy transition"
        );
        let reserved_active = state.admission_by_operation.values().any(|admission| {
            admission.agent.id == agent.id
                && admission.launch
                && !state.task_by_id.contains_key(&admission.task.id)
        });
        ensure!(
            !reserved_active,
            "child has an admission awaiting durable commit"
        );
        let reserved_launches = state
            .admission_by_operation
            .values()
            .filter(|admission| {
                admission.launch && !state.task_by_id.contains_key(&admission.task.id)
            })
            .count();
        let launch = !state.execution_by_agent.contains_key(&agent.id)
            && state.execution_by_agent.len() + reserved_launches < MAX_ACTIVE_CHILDREN
            && state.pending_launches.is_empty();
        if let Some(execution) = state.execution_by_agent.get(&agent.id) {
            ensure!(
                !*execution.cancellation.borrow(),
                "child interruption has not settled"
            );
            ensure!(
                pending_task_count(&state) < MAX_PENDING_TASKS,
                "child task queue already has 1024 pending tasks"
            );
        } else {
            let reserved = state
                .admission_by_operation
                .values()
                .filter(|admission| {
                    admission.launch && !state.task_by_id.contains_key(&admission.task.id)
                })
                .count();
            ensure!(
                pending_task_count(&state) < MAX_PENDING_TASKS
                    || state.execution_by_agent.len() + reserved < MAX_ACTIVE_CHILDREN,
                "child task queue already has 1024 pending tasks"
            );
        }
        let ordinal = state
            .task_by_id
            .values()
            .filter(|task| task.agent_id == agent.id)
            .map(|task| task.ordinal)
            .chain(
                state
                    .admission_by_operation
                    .values()
                    .filter(|admission| admission.agent.id == agent.id)
                    .map(|admission| admission.task.ordinal),
            )
            .max()
            .unwrap_or(0)
            + 1;
        let task = AgentTask {
            id: uuid::Uuid::new_v4().to_string(),
            session_id: self.session_id.clone(),
            agent_id: agent.id.clone(),
            operation_id: request.operation_id.clone(),
            ordinal,
            source: request.source,
            parent_exchange_id: parent_exchange_id.into(),
            message: message.into(),
            state: ExchangeState::Queued,
            final_response: None,
            error: None,
            parent_reported: false,
            created_at_ms: now_ms,
            last_activity_at_ms: None,
            completed_at_ms: None,
        };
        let admission = Arc::new(ReservedAgentWork {
            agent,
            task,
            launch,
        });
        state
            .admission_by_operation
            .insert(request.operation_id.clone(), admission.clone());
        Ok(AgentAdmission {
            owner: self.state.clone(),
            work: admission,
        })
    }

    pub fn cancellation(&self, agent_id: &str, task_id: &str) -> Result<watch::Receiver<bool>> {
        let state = self.state.lock().expect("agent state poisoned");
        let execution = state
            .execution_by_agent
            .get(agent_id)
            .context("child has no active execution")?;
        ensure!(
            execution.active_task_id == task_id,
            "child task no longer owns execution"
        );
        Ok(execution.cancellation.subscribe())
    }

    /// Capture one child identity and its latest task under the coordinator lock.
    pub(crate) fn capture_target(&self, target: &str) -> Result<AgentTarget> {
        let state = self.state.lock().expect("agent state poisoned");
        let agent = resolve_agent(&state, &self.session_id, target)?;
        ensure!(
            !agent.is_primary(),
            "the root is not a child command target"
        );
        let task_id = state
            .task_by_id
            .values()
            .filter(|task| task.agent_id == agent.id)
            .max_by_key(|task| task.ordinal)
            .map(|task| task.id.clone());
        Ok(AgentTarget {
            agent_id: agent.id.clone(),
            task_id,
        })
    }

    /// Reject a queued command if another child task replaced its captured scope.
    pub(crate) fn validate_target(&self, target: &AgentTarget) -> Result<()> {
        ensure!(
            self.capture_target(&target.agent_id)? == *target,
            "the selected child task changed before the command applied"
        );
        Ok(())
    }

    /// Observe collection without mistaking a replacement task for the selected work.
    pub(crate) fn target_collected(&self, target: &AgentTarget) -> Result<bool> {
        self.validate_target(target)?;
        let state = self.state.lock().expect("agent state poisoned");
        Ok(!state.execution_by_agent.contains_key(&target.agent_id)
            && !state.pending_launches.iter().any(|id| {
                state
                    .task_by_id
                    .get(id)
                    .is_some_and(|task| task.agent_id == target.agent_id)
            }))
    }

    pub fn resolve(&self, target: &str) -> Result<Agent> {
        Ok(resolve_agent(
            &self.state.lock().expect("agent state poisoned"),
            &self.session_id,
            target,
        )?
        .clone())
    }

    /// Selects the executing run before queued follow-ups, or the last run when idle.
    pub(crate) fn inspection(&self, target: &str) -> Result<(Agent, AgentTask)> {
        let state = self.state.lock().expect("agent state poisoned");
        let agent = resolve_agent(&state, &self.session_id, target)?.clone();
        let active = state
            .execution_by_agent
            .get(&agent.id)
            .and_then(|execution| state.task_by_id.get(&execution.active_task_id));
        let task = active
            .or_else(|| {
                state
                    .task_by_id
                    .values()
                    .filter(|task| task.agent_id == agent.id)
                    .max_by_key(|task| task.ordinal)
            })
            .context("child has no admitted task")?
            .clone();
        Ok((agent, task))
    }

    pub fn interrupt(&self, target: &str) -> Result<Vec<AgentTask>> {
        let mut state = self.state.lock().expect("agent state poisoned");
        let agent = resolve_agent(&state, &self.session_id, target)?.clone();
        let queued: Vec<_> = state
            .pending_launches
            .iter()
            .filter_map(|id| {
                state
                    .task_by_id
                    .get(id)
                    .filter(|task| task.agent_id == agent.id)
                    .cloned()
            })
            .collect();
        let queued_ids: std::collections::HashSet<_> =
            queued.iter().map(|task| task.id.clone()).collect();
        state.pending_launches.retain(|id| !queued_ids.contains(id));
        for id in &queued_ids {
            state.pending_task_slots.remove(id);
        }
        let Some(execution) = state.execution_by_agent.get_mut(&agent.id) else {
            return Ok(queued);
        };
        execution.cancellation.send_replace(true);
        let pending: Vec<_> = execution.pending.drain(..).collect();
        for id in &pending {
            state.pending_task_slots.remove(id);
        }
        Ok(queued
            .into_iter()
            .chain(
                pending
                    .into_iter()
                    .filter_map(|id| state.task_by_id.get(&id).cloned()),
            )
            .collect())
    }

    pub fn retain(&self, agent: Agent, task: AgentTask) -> Result<()> {
        ensure!(
            agent.session_id == self.session_id
                && task.session_id == self.session_id
                && task.agent_id == agent.id,
            "child state crosses root ownership"
        );
        let mut state = self.state.lock().expect("agent state poisoned");
        state.agent_by_id.insert(agent.id.clone(), agent);
        state.task_by_id.insert(task.id.clone(), task);
        self.activity
            .send_modify(|revision| *revision = revision.wrapping_add(1));
        Ok(())
    }

    /// Identifies a retained process cleanup obligation after its task has ended.
    pub fn cleanup_task(&self, agent_id: &str) -> Option<AgentTask> {
        let state = self.state.lock().expect("agent state poisoned");
        let execution = state.execution_by_agent.get(agent_id)?;
        if !execution.cleanup_failed {
            return None;
        }
        state.task_by_id.get(&execution.active_task_id).cloned()
    }

    pub fn pending_result(&self, parent_exchange_id: &str) -> Vec<AgentTask> {
        let state = self.state.lock().expect("agent state poisoned");
        let mut result: Vec<_> = state
            .task_by_id
            .values()
            .filter(|task| {
                task.source == AgentSource::Model
                    && task.parent_exchange_id == parent_exchange_id
                    && task.completed_at_ms.is_some()
                    && !task.parent_reported
            })
            .cloned()
            .collect();
        result.sort_by_key(|task| (task.completed_at_ms, task.created_at_ms, task.ordinal));
        result
    }

    /// Reports unfinished model-delegated work owned by this exchange.
    pub fn has_active_task(&self, parent_exchange_id: &str) -> bool {
        self.state
            .lock()
            .expect("agent state poisoned")
            .task_by_id
            .values()
            .any(|task| {
                task.source == AgentSource::Model
                    && task.parent_exchange_id == parent_exchange_id
                    && task.completed_at_ms.is_none()
            })
    }

    pub fn has_unresolved_task(&self, parent_exchange_id: &str) -> bool {
        self.state
            .lock()
            .expect("agent state poisoned")
            .task_by_id
            .values()
            .any(|task| {
                task.source == AgentSource::Model
                    && task.parent_exchange_id == parent_exchange_id
                    && !task.parent_reported
            })
    }

    pub fn settled(
        &self,
        agent_id: &str,
        task_id: &str,
        cleanup_failed: bool,
    ) -> Result<Option<AgentWork>> {
        let mut state = self.state.lock().expect("agent state poisoned");
        let Some(execution) = state.execution_by_agent.get_mut(agent_id) else {
            return Ok(None);
        };
        ensure!(
            execution.active_task_id == task_id,
            "stale task cannot settle a newer child turn"
        );
        if cleanup_failed {
            execution.cleanup_failed = true;
            self.activity
                .send_modify(|revision| *revision = revision.wrapping_add(1));
            return Ok(None);
        }
        let next = if *execution.cancellation.borrow() {
            None
        } else {
            execution.pending.pop_front()
        };
        if let Some(next) = next {
            execution.active_task_id = next.clone();
            execution.cleanup_failed = false;
            state.pending_task_slots.remove(&next);
            let agent = state
                .agent_by_id
                .get(agent_id)
                .context("child identity missing")?
                .clone();
            let task = state
                .task_by_id
                .get(&next)
                .context("queued child task missing")?
                .clone();
            Ok(Some(AgentWork {
                execution: AdmittedExecution {
                    owner: ExecutionOwner {
                        session_id: self.session_id.clone().into(),
                        agent_id: agent.id.clone().into(),
                    },
                    task: task.id.clone().into(),
                    exchange: task.id.clone().into(),
                },
                agent,
                task,
                launch: true,
            }))
        } else {
            state.execution_by_agent.remove(agent_id);
            self.activity
                .send_modify(|revision| *revision = revision.wrapping_add(1));
            Ok(None)
        }
    }

    pub fn active_count(&self) -> usize {
        self.state
            .lock()
            .expect("agent state poisoned")
            .execution_by_agent
            .len()
    }

    /// Claim the next queued child for the exchange owner's launch path after a capacity wake.
    /// Quiescence retains queued work without launching it. Its release wakes the owner, which
    /// must poll this method again; no worker independently changes the launch lifecycle.
    pub fn ready_launch(&self) -> Result<Option<AgentWork>> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("agent state poisoned"))?;
        if state.quiescence_count > 0 || state.execution_by_agent.len() >= MAX_ACTIVE_CHILDREN {
            return Ok(None);
        }
        let Some(index) = state.pending_launches.iter().position(|id| {
            state
                .task_by_id
                .get(id)
                .is_some_and(|task| !state.execution_by_agent.contains_key(&task.agent_id))
        }) else {
            return Ok(None);
        };
        let task_id = state.pending_launches[index].clone();
        let task = state
            .task_by_id
            .get(&task_id)
            .context("queued child task missing")?
            .clone();
        let agent = state
            .agent_by_id
            .get(&task.agent_id)
            .context("queued child identity missing")?
            .clone();
        let (cancellation, _) = watch::channel(false);
        let mut pending = VecDeque::new();
        state.pending_launches.remove(index);
        state.pending_task_slots.remove(&task_id);
        let followups: Vec<_> = state
            .pending_launches
            .iter()
            .filter(|id| {
                state
                    .task_by_id
                    .get(*id)
                    .is_some_and(|task| task.agent_id == agent.id)
            })
            .cloned()
            .collect();
        for id in followups {
            state.pending_launches.retain(|candidate| candidate != &id);
            pending.push_back(id);
        }
        state.execution_by_agent.insert(
            agent.id.clone(),
            ChildExecution {
                active_task_id: task.id.clone(),
                pending,
                cancellation,
                cleanup_failed: false,
            },
        );
        Ok(Some(AgentWork {
            execution: AdmittedExecution {
                owner: ExecutionOwner {
                    session_id: self.session_id.clone().into(),
                    agent_id: agent.id.clone().into(),
                },
                task: task.id.clone().into(),
                exchange: task.id.clone().into(),
            },
            agent,
            task,
            launch: true,
        }))
    }

    pub fn snapshot(&self, prefix: Option<&str>) -> Vec<(Agent, Option<AgentTask>)> {
        let state = self.state.lock().expect("agent state poisoned");
        let mut result: Vec<_> = state
            .agent_by_id
            .values()
            .filter(|agent| prefix.is_none_or(|prefix| agent.task_path.starts_with(prefix)))
            .map(|agent| {
                (
                    agent.clone(),
                    state
                        .task_by_id
                        .values()
                        .filter(|task| task.agent_id == agent.id)
                        .max_by_key(|task| task.ordinal)
                        .cloned(),
                )
            })
            .collect();
        result.sort_by(|left, right| {
            left.0
                .created_at_ms
                .cmp(&right.0.created_at_ms)
                .then_with(|| left.0.task_path.cmp(&right.0.task_path))
        });
        result
    }

    pub fn task(&self, id: &str) -> Option<AgentTask> {
        self.state
            .lock()
            .expect("agent state poisoned")
            .task_by_id
            .get(id)
            .cloned()
    }
}

/// Count queued tasks and uncommitted reservations without charging live executions twice.
fn pending_task_count(state: &AgentStateStore) -> usize {
    state.pending_launches.len()
        + state
            .execution_by_agent
            .values()
            .map(|execution| execution.pending.len())
            .sum::<usize>()
        + state
            .admission_by_operation
            .values()
            .filter(|admission| {
                !admission.launch && !state.task_by_id.contains_key(&admission.task.id)
            })
            .count()
}

fn validate_message(message: &str) -> Result<()> {
    ensure!(
        !message.trim().is_empty(),
        "child message must not be empty"
    );
    Ok(())
}

fn resolve_agent<'state>(
    state: &'state AgentStateStore,
    session_id: &str,
    target: &str,
) -> Result<&'state Agent> {
    let mut matched = state.agent_by_id.values().filter(|agent| {
        agent.session_id == session_id
            && !agent.is_primary()
            && (agent.id == target || agent.task_path == target || agent.task_name == target)
    });
    let agent = matched
        .next()
        .context("target is not a child of this root session")?;
    ensure!(matched.next().is_none(), "child target is ambiguous");
    Ok(agent)
}
