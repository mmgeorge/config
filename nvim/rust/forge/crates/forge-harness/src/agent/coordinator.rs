use std::{
    collections::{HashMap, VecDeque},
    num::NonZeroUsize,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use tokio::sync::{mpsc, oneshot, watch};

use super::{Agent, AgentContinuationPolicy, AgentSelection, AgentSource, AgentTask};
use crate::{
    exchange::ExchangeState,
    provider::{ProviderCompletion, ThreadOwner},
};

pub const MAX_PENDING_TASKS: usize = 1024;

/// Maximum number of managed children that may hold execution slots in one root session.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(transparent)]
pub struct SubagentLimit(NonZeroUsize);

impl Default for SubagentLimit {
    /// Get the capacity for sessions without a saved limit.
    fn default() -> Self {
        Self(NonZeroUsize::new(8).expect("the default subagent limit is positive"))
    }
}

/// Access requested for a single newly spawned child.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SpawnAccess {
    /// Remove the child's write authority.
    Read,
    /// Require the parent's existing write authority within the selected profile.
    Write,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Describes a fresh child without inheriting parent transcript or workflow state.
pub struct SpawnAgentRequest {
    pub profile: Option<String>,
    /// Use the profile's access mode when omitted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub access: Option<SpawnAccess>,
    pub text: String,
}

impl SpawnAgentRequest {
    /// Check whether retained child work satisfies this spawn request.
    pub(crate) fn matches_admission(&self, agent: &Agent, task: &AgentTask) -> bool {
        task.ordinal == 1
            && task.message == self.text
            && self
                .profile
                .as_deref()
                .is_none_or(|profile| profile == agent.definition)
            && self.access.is_none_or(|access| {
                agent.selection.as_ref().is_some_and(|selection| {
                    let write_scope = selection.access.write_access;
                    match access {
                        SpawnAccess::Read => write_scope == crate::session::WriteScope::Off,
                        SpawnAccess::Write => write_scope != crate::session::WriteScope::Off,
                    }
                })
            })
    }
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
        owner: ThreadOwner,
        task_id: String,
        event: Box<crate::provider::session_execution::OwnerObservation>,
    },
    Completed {
        owner: ThreadOwner,
        task_id: String,
        output: Box<Result<ProviderCompletion>>,
        host_file_changes: Vec<crate::tools::lsp::HostFileChange>,
        cleanup_error: Option<String>,
    },
}

/// Decide scheduling separately from actual resource collection and native completion.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AgentSettlementDisposition {
    Ready,
    RequiresAuthorization,
    CleanupFailed,
}

/// Retains authenticated turn ownership until the broker admits a model operation.
pub struct AgentRequestAuthority {
    pub owner: ThreadOwner,
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
    /// Admit only the still-current queued task before it starts a provider process.
    ///
    /// Each child holds a shared source lock until its task result is durably published.
    /// `access` limits the operations the child can borrow from that reservation.
    pub(crate) async fn repository_admit(
        &self,
        owner: &ThreadOwner,
        task_id: &str,
        root: &std::path::Path,
        workspace: &std::path::Path,
        access: crate::repository_access::RepositoryAccess,
        mut cancelled: watch::Receiver<bool>,
    ) -> Result<Arc<crate::repository_access::RepositoryReservation>> {
        loop {
            ensure!(
                !*cancelled.borrow(),
                "child task cancelled before repository admission"
            );
            {
                let state = self
                    .state
                    .lock()
                    .map_err(|_| anyhow::anyhow!("agent state poisoned"))?;
                let execution = state
                    .execution_by_agent
                    .get(owner.agent_id().as_str())
                    .context("child execution was retired")?;
                ensure!(
                    execution.active_task_id == task_id,
                    "child repository admission is stale"
                );
                if let Some(repository) = &execution.repository {
                    return Ok(repository.clone());
                }
            }
            // No coordinator or broker lock is held while inspecting repository ownership.
            match crate::repository_access::RepositoryReservation::acquire_for_child(
                root, workspace, access,
            ) {
                Ok(repository) => {
                    let repository = Arc::new(repository);
                    let mut state = self
                        .state
                        .lock()
                        .map_err(|_| anyhow::anyhow!("agent state poisoned"))?;
                    let execution = state
                        .execution_by_agent
                        .get_mut(owner.agent_id().as_str())
                        .context("child execution was retired")?;
                    ensure!(
                        execution.active_task_id == task_id && !*execution.cancellation.borrow(),
                        "child repository admission was cancelled or replaced"
                    );
                    ensure!(
                        execution.repository.is_none(),
                        "child repository ownership was already admitted"
                    );
                    execution.repository = Some(repository.clone());
                    return Ok(repository);
                }
                Err(crate::repository_access::RepositoryAccessError::Busy(_)) => {
                    tokio::select! {
                        _ = cancelled.wait_for(|cancelled| *cancelled) => anyhow::bail!("child task cancelled while waiting for repository ownership"),
                        _ = tokio::time::sleep(std::time::Duration::from_millis(50)) => {}
                    }
                }
                Err(error) => return Err(error.into()),
            }
        }
    }

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
            // A user's interrupt already stopped its child when the editor's command was
            // captured (`signal_user_interrupt`); a model's interrupt waits for the broker, which
            // authenticates it first.
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
    continuation_authorization: Option<forge_provider_api::ExchangeId>,
}

#[derive(Clone, Debug)]
struct AdmittedExecution {
    owner: ThreadOwner,
    task: crate::provider::TaskId,
    exchange: crate::provider::ExchangeId,
}

impl AgentWork {
    pub fn owner(&self) -> &ThreadOwner {
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
    /// Get the exact stopped task acknowledged by this explicit user admission.
    pub fn continuation_authorization(&self) -> Option<&forge_provider_api::ExchangeId> {
        self.continuation_authorization.as_ref()
    }

    fn committed(&self) -> AgentWork {
        AgentWork {
            agent: self.agent.clone(),
            task: self.task.clone(),
            launch: self.launch,
            execution: AdmittedExecution {
                owner: ThreadOwner::new(
                    self.agent.session_id.clone().into(),
                    forge_provider_api::AgentId::new(self.agent.id.clone())
                        .expect("registered agent identity is nonempty"),
                )
                .expect("registered session and agent identities are nonempty"),
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
        if let Some(current) = state.agent_by_id.get(&admission.agent.id) {
            match &admission.continuation_authorization {
                Some(stopped) => ensure!(
                    admission.task.source == AgentSource::User
                        && matches!(&current.continuation, AgentContinuationPolicy::RequiresAuthorization { task_id } if task_id == stopped),
                    "child continuation authorization is stale"
                ),
                None => ensure!(
                    current.continuation == admission.agent.continuation,
                    "child continuation policy changed during admission"
                ),
            }
        }
        ensure!(
            state
                .admission_by_operation
                .get(&admission.task.operation_id)
                .is_some_and(|reserved| Arc::ptr_eq(reserved, admission)),
            "child reservation was cancelled"
        );
        let launch = matches!(admission.agent.continuation, AgentContinuationPolicy::Ready)
            && !state.execution_by_agent.contains_key(&admission.agent.id)
            && !state.pending_launches.iter().any(|id| {
                state
                    .task_by_id
                    .get(id)
                    .is_some_and(|task| task.agent_id == admission.agent.id)
            })
            && state.pending_launches.is_empty()
            && state.has_capacity(0)
            && state.quiescence_count == 0
            && state.launch_hold_count == 0;
        if launch {
            let (cancellation, _) = watch::channel(false);
            state.execution_by_agent.insert(
                admission.agent.id.clone(),
                ChildExecution {
                    active_task_id: admission.task.id.clone(),
                    pending: VecDeque::new(),
                    cancellation,
                    interrupted_by: None,
                    cleanup_failed: false,
                    repository: None,
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
    /// Who requested the active task's interruption, so its report can name the cancel.
    interrupted_by: Option<AgentSource>,
    cleanup_failed: bool,
    repository: Option<Arc<crate::repository_access::RepositoryReservation>>,
}

#[derive(Default)]
struct AgentStateStore {
    subagent_limit: SubagentLimit,
    quiescence_count: usize,
    launch_hold_count: usize,
    foreground_claim_count: usize,
    agent_by_id: HashMap<String, Agent>,
    task_by_id: HashMap<String, AgentTask>,
    admission_by_operation: HashMap<String, Arc<ReservedAgentWork>>,
    execution_by_agent: HashMap<String, ChildExecution>,
    pending_launches: VecDeque<String>,
    pending_task_slots: HashMap<String, tokio::sync::OwnedSemaphorePermit>,
}

impl AgentStateStore {
    /// Check execution capacity while accounting for slots reserved by pending admissions.
    fn has_capacity(&self, reserved_launches: usize) -> bool {
        self.execution_by_agent.len() + reserved_launches < self.subagent_limit.0.get()
    }
}

/// Owns one root's identities, admission capacity, task queues, and cancellation.
pub struct AgentCoordinator {
    session_id: String,
    enabled: AtomicBool,
    state: Arc<Mutex<AgentStateStore>>,
    receiver: Mutex<mpsc::Receiver<AgentCommand>>,
    foreground_deferred: Mutex<VecDeque<AgentCommand>>,
    /// Set while the broker holds back the agent snapshot behind still-queued child commands.
    agent_publication_deferred: AtomicBool,
    sender: AgentCommandSender,
    activity: watch::Sender<u64>,
    profiles: Mutex<Option<Arc<super::AgentProfileStore>>>,
}

/// Split admitted child work by whether a collection can still wait for it to settle.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AgentCollectionCount {
    /// Executions whose turn has not settled.
    pub running: usize,
    /// Executions whose turn ended but whose process cleanup failed and stays retained.
    pub cleanup_failed: usize,
    /// Admitted tasks waiting for an execution slot; collection cancels them durably.
    pub queued: usize,
}

impl AgentCollectionCount {
    /// Report whether any admitted work, running or queued, remains for collection.
    pub fn has_work(&self) -> bool {
        self.running + self.cleanup_failed + self.queued > 0
    }
}

#[must_use]
/// Rejects new child work until its owning cancellation or policy transition finishes.
pub struct AgentQuiescence {
    coordinator: Arc<AgentCoordinator>,
}

/// Prevent child launches while an idle root runs checks, without parking its response.
#[must_use]
pub(crate) struct AgentLaunchHold {
    coordinator: Arc<AgentCoordinator>,
}

impl Drop for AgentLaunchHold {
    fn drop(&mut self) {
        self.coordinator
            .state
            .lock()
            .expect("agent state poisoned")
            .launch_hold_count -= 1;
        self.coordinator
            .sender
            .revision
            .send_modify(|revision| *revision = revision.wrapping_add(1));
    }
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

#[must_use]
/// Reserves queued agent commands for the active root turn, which authenticates model requests
/// against its own exchange. Dropping the claim wakes the idle drain for any commands left behind.
pub struct AgentForegroundClaim {
    coordinator: Arc<AgentCoordinator>,
}

impl Drop for AgentForegroundClaim {
    fn drop(&mut self) {
        self.coordinator
            .state
            .lock()
            .expect("agent state poisoned")
            .foreground_claim_count -= 1;
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
            foreground_deferred: Mutex::new(VecDeque::new()),
            agent_publication_deferred: AtomicBool::new(false),
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

    /// Get the current capacity for managed child execution.
    pub(crate) fn subagent_limit(&self) -> SubagentLimit {
        self.state
            .lock()
            .expect("agent state poisoned")
            .subagent_limit
    }

    /// Apply a new capacity and wake the owner to reconsider queued launches.
    pub(crate) fn set_subagent_limit(&self, limit: SubagentLimit) {
        self.state
            .lock()
            .expect("agent state poisoned")
            .subagent_limit = limit;
        self.sender
            .revision
            .send_modify(|revision| *revision = revision.wrapping_add(1));
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

    pub fn owns(&self, owner: &ThreadOwner) -> bool {
        *owner.session_id() == crate::provider::SessionId::from(self.session_id.as_str())
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

    /// Atomically require idle children and fence launches until the returned hold drops.
    pub(crate) fn hold_launches(self: &Arc<Self>) -> Result<AgentLaunchHold> {
        let mut state = self.state.lock().expect("agent state poisoned");
        ensure!(
            state.execution_by_agent.is_empty(),
            "checks cannot start while delegated agents are active"
        );
        state.launch_hold_count += 1;
        Ok(AgentLaunchHold {
            coordinator: Arc::clone(self),
        })
    }

    /// Routes queued commands to the active root turn until the returned claim drops.
    pub(crate) fn claim_foreground(self: &Arc<Self>) -> AgentForegroundClaim {
        self.state
            .lock()
            .expect("agent state poisoned")
            .foreground_claim_count += 1;
        AgentForegroundClaim {
            coordinator: Arc::clone(self),
        }
    }

    /// Reports whether an active root turn owns the command queue.
    pub(crate) fn foreground_claimed(&self) -> bool {
        self.state
            .lock()
            .expect("agent state poisoned")
            .foreground_claim_count
            > 0
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

    /// Receives the next command for the active root turn, starting with requests the idle
    /// drain handed over.
    pub(crate) fn try_receive_foreground(&self) -> Option<AgentCommand> {
        let deferred = self
            .foreground_deferred
            .lock()
            .expect("deferred agent commands poisoned")
            .pop_front();
        deferred.or_else(|| self.try_receive())
    }

    /// Receives the next command for the idle drain. Handed-over requests return to it only
    /// after the root turn released its claim, so it can reject them as unauthenticated.
    pub(crate) fn try_receive_idle(&self) -> Option<AgentCommand> {
        if !self.foreground_claimed() {
            let deferred = self
                .foreground_deferred
                .lock()
                .expect("deferred agent commands poisoned")
                .pop_front();
            if deferred.is_some() {
                return deferred;
            }
        }
        self.try_receive()
    }

    /// Report whether a child command still waits for the broker, in the channel or handed to
    /// the active root turn.
    pub(crate) fn has_queued_commands(&self) -> bool {
        !self
            .receiver
            .lock()
            .expect("agent request receiver poisoned")
            .is_empty()
            || !self
                .foreground_deferred
                .lock()
                .expect("deferred agent commands poisoned")
                .is_empty()
    }

    /// Record that the broker applied a child observation without publishing the agent
    /// snapshot, because a command still queued behind it reaches the broker afterwards.
    pub(crate) fn defer_agent_publication(&self) {
        self.agent_publication_deferred
            .store(true, Ordering::Release);
    }

    /// Clear a deferred agent snapshot, reporting whether one was pending.
    pub(crate) fn take_deferred_agent_publication(&self) -> bool {
        self.agent_publication_deferred
            .swap(false, Ordering::AcqRel)
    }

    /// Hands a request to the active root turn and wakes its agent wait.
    pub(crate) fn defer_to_foreground(&self, command: AgentCommand) {
        self.foreground_deferred
            .lock()
            .expect("deferred agent commands poisoned")
            .push_back(command);
        self.sender
            .revision
            .send_modify(|revision| *revision = revision.wrapping_add(1));
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
                    && spawn.matches_admission(&existing.agent, &existing.task)
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
            state.quiescence_count == 0 && state.launch_hold_count == 0,
            "child admission is closed during checks, cancellation, or a policy transition"
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
        let launch = state.has_capacity(reservation_count) && state.pending_launches.is_empty();
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
        agent.task_path = format!("{}/{task_name}", forge_provider_api::forge::ROOT_AGENT_PATH);
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
            cancelled_by: None,
            created_at_ms: now_ms,
            last_activity_at_ms: None,
            completed_at_ms: None,
        };
        let admission = Arc::new(ReservedAgentWork {
            agent,
            task,
            launch,
            continuation_authorization: None,
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
        let mut agent = resolve_agent(&state, &self.session_id, target)?.clone();
        let continuation_authorization = if request.source == AgentSource::User {
            match agent.continuation.clone() {
                AgentContinuationPolicy::RequiresAuthorization { task_id } => {
                    agent.authorize_continuation(&task_id)?;
                    Some(task_id)
                }
                AgentContinuationPolicy::Ready => None,
            }
        } else {
            None
        };
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
        // Only an explicit user grant clears a stopped task; queued work behind the gate could
        // never launch and would leave its parent's delegation tree permanently unsettled.
        if continuation_authorization.is_none()
            && let Some(required) = super::ContinuationRequired::for_agent(&agent)
        {
            return Err(required.into());
        }
        ensure!(
            state.quiescence_count == 0 && state.launch_hold_count == 0,
            "child admission is closed during checks, cancellation, or a policy transition"
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
        let launch = matches!(agent.continuation, AgentContinuationPolicy::Ready)
            && !state.execution_by_agent.contains_key(&agent.id)
            && state.has_capacity(reserved_launches)
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
            ensure!(
                pending_task_count(&state) < MAX_PENDING_TASKS
                    || state.has_capacity(reserved_launches),
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
            cancelled_by: None,
            created_at_ms: now_ms,
            last_activity_at_ms: None,
            completed_at_ms: None,
        };
        let admission = Arc::new(ReservedAgentWork {
            agent,
            task,
            launch,
            continuation_authorization,
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

    /// Stop the captured child task for a user's interrupt before the request is queued.
    ///
    /// The request reaches the broker only after the parent's command FIFO and the child command
    /// channel, which also carries every child observation, so the child is signalled here and
    /// the broker still cancels its queued tasks and records the interrupt on arrival. A target
    /// whose child has since started another task signals nothing.
    pub(crate) fn signal_user_interrupt(&self, target: &AgentTarget) {
        let mut state = self.state.lock().expect("agent state poisoned");
        let latest_task_id = state
            .task_by_id
            .values()
            .filter(|task| task.agent_id == target.agent_id)
            .max_by_key(|task| task.ordinal)
            .map(|task| task.id.clone());
        if latest_task_id == target.task_id
            && let Some(execution) = state.execution_by_agent.get_mut(&target.agent_id)
        {
            execution.interrupted_by.get_or_insert(AgentSource::User);
            execution.cancellation.send_replace(true);
        }
    }

    /// Stop every running child for a user's interrupt of all children before it is queued.
    pub(crate) fn signal_user_interrupt_all(&self) {
        let mut state = self.state.lock().expect("agent state poisoned");
        for execution in state.execution_by_agent.values_mut() {
            execution.interrupted_by.get_or_insert(AgentSource::User);
            execution.cancellation.send_replace(true);
        }
    }

    /// Report who interrupted the child's active task, or `None` when no interrupt was requested.
    pub fn interruption(&self, agent_id: &str, task_id: &str) -> Option<AgentSource> {
        let state = self.state.lock().expect("agent state poisoned");
        state
            .execution_by_agent
            .get(agent_id)
            .filter(|execution| execution.active_task_id == task_id)
            .and_then(|execution| execution.interrupted_by)
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

    /// Interrupt a child's active task and cancel its queued tasks on behalf of `source`.
    pub fn interrupt(&self, target: &str, source: AgentSource) -> Result<Vec<AgentTask>> {
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
        execution.interrupted_by.get_or_insert(source);
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
        if let AgentContinuationPolicy::RequiresAuthorization { task_id } = &agent.continuation {
            let stopped = if task.id == task_id.as_str() {
                &task
            } else {
                state
                    .task_by_id
                    .get(task_id.as_str())
                    .context("child continuation policy points to an unknown task")?
            };
            ensure!(
                stopped.agent_id == agent.id
                    && stopped.session_id == agent.session_id
                    && stopped.completed_at_ms.is_some(),
                "child continuation policy requires its original collected task"
            );
        }

        state.agent_by_id.insert(agent.id.clone(), agent);
        state.task_by_id.insert(task.id.clone(), task);
        self.activity
            .send_modify(|revision| *revision = revision.wrapping_add(1));
        Ok(())
    }

    /// Restore every stored child task before validating continuation policies.
    ///
    /// A policy names the child's latest stopped task, which storage may list after the child's
    /// earlier tasks, so validation waits until the whole set is present. A policy whose task is
    /// missing or never collected is disposable evidence from an interrupted host; it is reset to
    /// `Ready` and returned so the caller persists the repair instead of failing startup.
    pub fn restore(
        &self,
        agents: impl IntoIterator<Item = Agent>,
        tasks: impl IntoIterator<Item = AgentTask>,
    ) -> Result<Vec<Agent>> {
        let mut state = self.state.lock().expect("agent state poisoned");
        let mut owners = HashMap::new();
        for agent in agents {
            ensure!(
                agent.session_id == self.session_id,
                "child state crosses root ownership"
            );
            owners.insert(agent.id.clone(), agent);
        }
        for task in tasks {
            ensure!(
                task.session_id == self.session_id && owners.contains_key(&task.agent_id),
                "stored task has no owning child"
            );
            state.task_by_id.insert(task.id.clone(), task);
        }
        let mut repaired = Vec::new();
        for agent in owners.values_mut() {
            if let AgentContinuationPolicy::RequiresAuthorization { task_id } = &agent.continuation
                && !state
                    .task_by_id
                    .get(task_id.as_str())
                    .is_some_and(|stopped| {
                        stopped.agent_id == agent.id && stopped.completed_at_ms.is_some()
                    })
            {
                agent.continuation = AgentContinuationPolicy::Ready;
                repaired.push(agent.clone());
            }
        }
        let owned: std::collections::HashSet<_> = state
            .task_by_id
            .values()
            .map(|task| task.agent_id.clone())
            .collect();
        for (id, agent) in owners {
            if owned.contains(&id) {
                state.agent_by_id.insert(id, agent);
            }
        }
        self.activity
            .send_modify(|revision| *revision = revision.wrapping_add(1));
        Ok(repaired)
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
    ///
    /// Admission rejects work behind a stopped child's authorization gate, and a stop
    /// terminalizes the follow-ups already queued behind it, so every unfinished task counted
    /// here can still run to a durable outcome.
    pub fn has_active_task(&self, parent_exchange_id: &str) -> bool {
        let state = self.state.lock().expect("agent state poisoned");
        state.task_by_id.values().any(|task| {
            task.source == AgentSource::Model
                && task.parent_exchange_id == parent_exchange_id
                && task.completed_at_ms.is_none()
        })
    }

    /// Reports model-delegated work of this exchange whose result the parent has yet to receive.
    pub fn has_unresolved_task(&self, parent_exchange_id: &str) -> bool {
        let state = self.state.lock().expect("agent state poisoned");
        state.task_by_id.values().any(|task| {
            task.source == AgentSource::Model
                && task.parent_exchange_id == parent_exchange_id
                && !task.parent_reported
        })
    }

    /// List each child with an unfinished task delegated by this exchange, from any source.
    ///
    /// Checkpoint settlement requires every task in the exchange's delegation tree to be
    /// terminal, including user-started ones, so collection targets all of them.
    pub fn unfinished_task_agents(&self, parent_exchange_id: &str) -> Vec<String> {
        let state = self.state.lock().expect("agent state poisoned");
        let mut agents = state
            .task_by_id
            .values()
            .filter(|task| {
                task.parent_exchange_id == parent_exchange_id && task.completed_at_ms.is_none()
            })
            .map(|task| task.agent_id.clone())
            .collect::<Vec<_>>();
        agents.sort();
        agents.dedup();
        agents
    }

    /// List the admitted, undispatched follow-ups queued behind this child's running task.
    pub fn queued_followups(&self, agent_id: &str) -> Vec<AgentTask> {
        let state = self.state.lock().expect("agent state poisoned");
        state
            .execution_by_agent
            .get(agent_id)
            .into_iter()
            .flat_map(|execution| execution.pending.iter())
            .chain(state.pending_launches.iter())
            .filter_map(|id| state.task_by_id.get(id))
            .filter(|task| task.agent_id == agent_id && task.completed_at_ms.is_none())
            .cloned()
            .collect()
    }

    pub fn settled(
        &self,
        agent_id: &str,
        task_id: &str,
        disposition: AgentSettlementDisposition,
    ) -> Result<Option<AgentWork>> {
        let mut state = self.state.lock().expect("agent state poisoned");
        let authorization_required = match &state
            .agent_by_id
            .get(agent_id)
            .context("child identity missing")?
            .continuation
        {
            AgentContinuationPolicy::Ready => {
                ensure!(
                    disposition != AgentSettlementDisposition::RequiresAuthorization,
                    "stopped child policy must be durably retained before releasing execution"
                );
                false
            }
            AgentContinuationPolicy::RequiresAuthorization { task_id: stopped } => {
                ensure!(
                    stopped.as_str() == task_id,
                    "child continuation policy belongs to another task"
                );
                true
            }
        };
        let Some(execution) = state.execution_by_agent.get_mut(agent_id) else {
            return Ok(None);
        };
        ensure!(
            execution.active_task_id == task_id,
            "stale task cannot settle a newer child turn"
        );
        if disposition == AgentSettlementDisposition::CleanupFailed {
            execution.cleanup_failed = true;
            self.activity
                .send_modify(|revision| *revision = revision.wrapping_add(1));
            return Ok(None);
        }
        if let Some(repository) = &execution.repository {
            repository.published()?;
        }
        execution.repository.take();
        if authorization_required {
            let pending = std::mem::take(&mut execution.pending);
            // The broker terminalizes queued follow-ups in the stop's completion transaction;
            // a follow-up still unfinished here would be stranded behind the stopped task.
            if pending.iter().any(|id| {
                state
                    .task_by_id
                    .get(id.as_str())
                    .is_none_or(|task| task.completed_at_ms.is_none())
            }) {
                if let Some(execution) = state.execution_by_agent.get_mut(agent_id) {
                    execution.pending = pending;
                }
                anyhow::bail!("stopped child still has an unsettled queued follow-up");
            }
            state.execution_by_agent.remove(agent_id);
            for id in &pending {
                state.pending_task_slots.remove(id);
            }
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
                    owner: ThreadOwner::new(
                        self.session_id.clone().into(),
                        forge_provider_api::AgentId::new(agent.id.clone())
                            .expect("registered agent identity is nonempty"),
                    )
                    .expect("registered session and agent identities are nonempty"),
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

    /// Count child executions still running, and those whose turn ended with failed cleanup.
    ///
    /// A cleanup-failed execution keeps its slot in `active_count` for its retained cleanup
    /// obligation, but no further event settles it, so a collection waiting for children treats
    /// it as settled with an error rather than waiting for the count to reach zero.
    pub fn collection_count(&self) -> AgentCollectionCount {
        let state = self.state.lock().expect("agent state poisoned");
        let cleanup_failed = state
            .execution_by_agent
            .values()
            .filter(|execution| execution.cleanup_failed)
            .count();
        AgentCollectionCount {
            running: state.execution_by_agent.len() - cleanup_failed,
            cleanup_failed,
            queued: state.pending_launches.len(),
        }
    }

    /// Claim the next queued child for the exchange owner's launch path after a capacity wake.
    /// Quiescence retains queued work without launching it. Its release wakes the owner, which
    /// must poll this method again; no worker independently changes the launch lifecycle.
    pub fn ready_launch(&self) -> Result<Option<AgentWork>> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("agent state poisoned"))?;
        if state.quiescence_count > 0 || state.launch_hold_count > 0 || !state.has_capacity(0) {
            return Ok(None);
        }
        let Some(index) = state.pending_launches.iter().position(|id| {
            state.task_by_id.get(id).is_some_and(|task| {
                !state.execution_by_agent.contains_key(&task.agent_id)
                    && state.agent_by_id.get(&task.agent_id).is_some_and(|agent| {
                        matches!(agent.continuation, AgentContinuationPolicy::Ready)
                    })
            })
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
                interrupted_by: None,
                cleanup_failed: false,
                repository: None,
            },
        );
        Ok(Some(AgentWork {
            execution: AdmittedExecution {
                owner: ThreadOwner::new(
                    self.session_id.clone().into(),
                    forge_provider_api::AgentId::new(agent.id.clone())
                        .expect("registered agent identity is nonempty"),
                )
                .expect("registered session and agent identities are nonempty"),
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
/// Report a task queued for launch whose child must be authorized by the user before it runs.
///
/// `ready_launch` skips such a task, so nothing launches it until an explicit user admission
/// acknowledges the child's stopped task.
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
