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
    backend::{BackendCompletion, BackendEvent, ExecutionOwner},
    exchange::ExchangeState,
};

pub const MAX_ACTIVE_CHILDREN: usize = 8;
pub const MAX_PENDING_TASKS: usize = 32;

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

/// Serializes durable mutations without reacquiring an in-flight prompt's broker lock.
pub enum AgentCommand {
    Request {
        request: Box<AgentRequest>,
        authority: Option<AgentRequestAuthority>,
        response: oneshot::Sender<Result<serde_json::Value>>,
    },
    Event {
        owner: ExecutionOwner,
        task_id: String,
        event: Box<BackendEvent>,
    },
    Completed {
        owner: ExecutionOwner,
        task_id: String,
        output: Box<Result<BackendCompletion>>,
        cleanup_error: Option<String>,
    },
}

/// Retains authenticated turn ownership until the broker admits a model operation.
pub struct AgentRequestAuthority {
    pub owner: ExecutionOwner,
    pub parent_exchange_id: String,
    pub expired: watch::Receiver<bool>,
}

#[derive(Clone)]
/// Publishes bounded requests and wakes either the idle controller or active prompt.
pub struct AgentCommandSender {
    sender: mpsc::Sender<AgentCommand>,
    revision: watch::Sender<u64>,
}

impl AgentCommandSender {
    pub async fn send(&self, command: AgentCommand) -> Result<()> {
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
            response,
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
            response,
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
    task: crate::backend::TaskId,
    exchange: crate::backend::ExchangeId,
}

impl AgentWork {
    pub fn owner(&self) -> &ExecutionOwner {
        &self.execution.owner
    }
    pub fn task_id(&self) -> &crate::backend::TaskId {
        &self.execution.task
    }
    pub fn exchange_id(&self) -> &crate::backend::ExchangeId {
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
            return Ok(AgentCommit::Existing(admission.committed()));
        }
        ensure!(
            state
                .admission_by_operation
                .get(&admission.task.operation_id)
                .is_some_and(|reserved| Arc::ptr_eq(reserved, admission)),
            "child reservation was cancelled"
        );
        if admission.launch {
            ensure!(
                !state.execution_by_agent.contains_key(&admission.agent.id),
                "child already owns an active provider turn"
            );
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
        } else {
            state
                .execution_by_agent
                .get_mut(&admission.agent.id)
                .context("queued task lost its active child")?
                .pending
                .push_back(admission.task.id.clone());
        }
        state
            .agent_by_id
            .insert(admission.agent.id.clone(), admission.agent.clone());
        state
            .task_by_id
            .insert(admission.task.id.clone(), admission.task.clone());
        Ok(AgentCommit::Admitted(admission.committed()))
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
        Arc::new(Self {
            session_id,
            enabled: AtomicBool::new(false),
            state: Arc::new(Mutex::new(AgentStateStore::default())),
            receiver: Mutex::new(receiver),
            sender: AgentCommandSender { sender, revision },
            activity,
            profiles: Mutex::new(None),
        })
    }

    pub fn sender(&self) -> AgentCommandSender {
        self.sender.clone()
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
        owner.session_id == self.session_id
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
        ensure!(
            state.execution_by_agent.len() + reservation_count < MAX_ACTIVE_CHILDREN,
            "this session already has eight active children"
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
            launch: true,
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
        let launch = !state.execution_by_agent.contains_key(&agent.id);
        if let Some(execution) = state.execution_by_agent.get(&agent.id) {
            ensure!(
                !*execution.cancellation.borrow(),
                "child interruption has not settled"
            );
            let reserved = state
                .admission_by_operation
                .values()
                .filter(|admission| {
                    admission.agent.id == agent.id
                        && !state.task_by_id.contains_key(&admission.task.id)
                })
                .count();
            ensure!(
                execution.pending.len() + reserved < MAX_PENDING_TASKS,
                "child follow-up queue already has 32 pending tasks"
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
                state.execution_by_agent.len() + reserved < MAX_ACTIVE_CHILDREN,
                "this session already has eight active children"
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
        let Some(execution) = state.execution_by_agent.get_mut(&agent.id) else {
            return Ok(Vec::new());
        };
        execution.cancellation.send_replace(true);
        let pending: Vec<_> = execution.pending.drain(..).collect();
        Ok(pending
            .into_iter()
            .filter_map(|id| state.task_by_id.get(&id).cloned())
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
