mod fork;
mod new_session;
mod replanning;

pub use fork::{ForkPreparation, prepare_provider_fork};
pub use new_session::prepare_new_session;

use crate::agent::{
    Agent, AgentDefinition, AgentExecutionState, AgentLifecycleEvent, AgentRegistry, AgentState,
    load_codex_agent_catalog,
};
#[cfg(test)]
use crate::backend::BackendForkRequest;
use crate::backend::approval::{ApprovalRequestView, PermissionCoordinator};
use crate::backend::{
    Backend, BackendCapability, BackendCatalogRequest, BackendEvent, BackendEventSink,
    BackendInput, BackendLaunch, BackendRequest, PromptMode,
};
use crate::buffer::session::SessionPresentation;
use crate::checkpoint::{GitCheckpoint, checkpoint_diff, checkpoint_diff_for_paths};
use crate::exchange::{
    ActiveWait, Exchange, ExchangeComment, ExchangeKind, ExchangeNode, ExchangeState,
    ProviderChangeIndex, TaskTracker,
};
use crate::goal::{ContinuationDecision, GoalRecord, GoalState};
use crate::permissions::store::PermissionStore;
use crate::plan::state_machine::{PlanEvent, PlanStateMachine};
use crate::plan::{
    ArtifactSummary, ContextChoice, PlanAcceptance, PlanCallable, PlanDeviation,
    PlanDeviationDisposition, PlanDeviationKind, PlanDocument, PlanElicitation,
    PlanExecutionLifecycleEvent, PlanExecutionPromptKind, PlanExecutionRecord, PlanExecutionState,
    PlanFileStore, PlanLifecycleKind, PlanLifecycleRecord, PlanPrompt, PlanQuestionAnswer,
    PlanQuestionResponse, PlanQuestionSet, PlanQuestionWithdrawal, PlanRecord, PlanResolutionKind,
    PlanState, ScopeDeviationReview, digest as plan_digest, execution_prompt,
};
use crate::protocol::HarnessMethod;
use crate::rustdoc::{RustdocResolver, RustdocResolverConfig, validate_plan_rust_api};
use crate::session::{
    ContextUsage, ExecutionMode, HarnessMode, HarnessPreference, HarnessSession, ModelSetting,
    ProviderForkState, SessionStore,
};
use crate::storage::SqliteStore;
use crate::timeline::stream::TimelinePatch;
use crate::timeline::{
    SessionEventKind, SessionEventRecord, TimelineEntry, TimelineProjection, TimelineProjector,
};
use crate::trace::TraceStore;
use crate::workspace::WorkspaceKind;
use anyhow::{Context, Result};
use forge_protocol::message::{Request, Response, SessionEvent};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{
    Arc, RwLock,
    atomic::{AtomicBool, Ordering},
};
use std::time::{SystemTime, UNIX_EPOCH};
use tokio::sync::Notify;
use uuid::Uuid;

/// Defines wall-clock access consumed by leases and durable timelines.
pub trait Clock: Send + Sync {
    /// Resolve the current Unix time in milliseconds.
    fn now_ms(&self) -> i64;
}

/// Provides system wall-clock time for the production broker.
pub struct SystemClock;

impl Clock for SystemClock {
    fn now_ms(&self) -> i64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as i64
    }
}

/// Represents initialization state supplied by the Neovim client.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InitializeRequest {
    pub data_root: String,
    #[serde(default)]
    pub permission_file: Option<String>,
    pub workspace: String,
    pub client_id: String,
    pub backend: BackendLaunch,
    #[serde(default = "default_model")]
    pub model: String,
    #[serde(default = "default_effort")]
    pub effort: String,
    pub session_id: Option<String>,
    #[serde(default)]
    pub new_session_name: Option<String>,
    #[serde(default = "default_goal_max_turns")]
    pub goal_max_turns: u32,
    #[serde(default)]
    pub lease_conflict_action: Option<String>,
}

/// Represents the complete client-visible state for one active session.
#[derive(Clone, Debug, Serialize)]
pub struct BrokerSnapshot {
    pub session: HarnessSession,
    pub exchange: Vec<Exchange>,
    pub capability: BackendCapability,
    pub no_checkpoint: bool,
    pub goal: Option<GoalRecord>,
    pub active_plan: Option<PlanRecord>,
    pub active_elicitation: Option<ActiveElicitation>,
    pub artifact: Vec<ArtifactSummary>,
    pub timeline: Vec<TimelineEntry>,
    pub timeline_revision: u64,
    pub goal_execution: Option<PlanExecutionRecord>,
    pub active_wait: Option<ActiveWait>,
    pub prompt_history: Vec<String>,
    pub agent: AgentSnapshot,
    pub approval: Vec<ApprovalRequestView>,
}

/// Represents selectable definitions and durable child timelines for one session.
#[derive(Clone, Debug, Serialize)]
pub struct AgentSnapshot {
    pub definition: Vec<AgentDefinition>,
    pub run: Vec<Agent>,
    pub exchange: Vec<Exchange>,
}

/// Represents a stored session timeline projected without acquiring its lease.
#[derive(Clone, Debug, Serialize)]
pub struct SessionPreview {
    pub session: HarnessSession,
    pub exchange: Vec<Exchange>,
    pub timeline: Vec<TimelineEntry>,
    pub agent: AgentSnapshot,
}

/// Represents the durable owner and question state presented by the Harness question UI.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ElicitationOwner {
    Plan,
    Interaction,
    PlanAcceptance,
}

/// Represents the durable owner and question state presented by the Harness question UI.
#[derive(Clone, Debug, Serialize)]
pub struct ActiveElicitation {
    pub owner: ElicitationOwner,
    pub plan_id: Option<String>,
    pub exchange_id: Option<String>,
    pub elicitation: PlanElicitation,
}

struct ExchangeRuntime {
    exchange_id: String,
    synthetic_turn: Option<crate::backend::ProviderAddress>,
    task: TaskTracker,
    retraction_eligible: bool,
    active_wait: Option<ActiveWait>,
}

struct ChildExchangeRuntime {
    exchange: Exchange,
    synthetic_turn: Option<crate::backend::ProviderAddress>,
    task: TaskTracker,
}

struct RustdocTarget {
    plan_id: String,
    expected_version: u64,
    selection: String,
    receiver: String,
    receiver_package: String,
    receiver_version: String,
    callable: Option<PlanCallable>,
    package_list: Vec<(String, String)>,
}

fn rustdoc_callable(callable: Option<&PlanCallable>) -> Result<&PlanCallable> {
    callable.context("selected flow edge has no Rust callable")
}

/// Represents one request result plus asynchronous events emitted before its response.
pub struct DispatchResult {
    pub response: Response,
    pub event: Vec<SessionEvent>,
}

/// Owns provider, permission, and analysis infrastructure shared by every session controller.
pub struct BrokerRuntime {
    repositories: Arc<forge_git::store::RepositoryStore>,
    diff: Arc<forge_diff::engine::DiffEngine>,
    workspace_kind: WorkspaceKind,
    backend: Arc<dyn Backend>,
    permission_store: Arc<RwLock<PermissionStore>>,
    permission_coordinator: Arc<PermissionCoordinator>,
    trace: Arc<TraceStore>,
    rustdoc: Arc<RustdocResolver>,
}

impl BrokerRuntime {
    /// Builds provider infrastructure for a workspace already resolved by the caller.
    /// Session controllers reuse its workspace, permission scope, and caller-owned analysis engine.
    pub fn initialize(
        request: &InitializeRequest,
        workspace_kind: WorkspaceKind,
        repositories: Arc<forge_git::store::RepositoryStore>,
        diff: Arc<forge_diff::engine::DiffEngine>,
    ) -> Result<Arc<Self>> {
        let data_root = PathBuf::from(&request.data_root);
        let workspace = match &workspace_kind {
            WorkspaceKind::Git(path) | WorkspaceKind::Untracked(path) => {
                path.to_string_lossy().into_owned()
            }
        };
        let permission_file = request
            .permission_file
            .as_deref()
            .map(PathBuf::from)
            .unwrap_or_else(|| data_root.join("permissions.json"));
        let permission_store = Arc::new(RwLock::new(PermissionStore::load(
            permission_file,
            &workspace,
        )?));
        let permission_coordinator =
            Arc::new(PermissionCoordinator::new(Arc::clone(&permission_store)));
        let trace = Arc::new(TraceStore::open(&data_root)?);
        let rustdoc = Arc::new(RustdocResolver::new(RustdocResolverConfig::production(
            &data_root,
        )?)?);
        let backend = Arc::<dyn Backend>::from(crate::backend::build(
            request.backend.clone(),
            Arc::clone(&permission_coordinator),
            Arc::clone(&trace),
        )?);
        Ok(Arc::new(Self {
            repositories,
            diff,
            workspace_kind,
            backend,
            permission_store,
            permission_coordinator,
            trace,
            rustdoc,
        }))
    }

    /// Share the process-wide provider host with concurrent session controllers.
    pub fn backend_handle(&self) -> Arc<dyn Backend> {
        Arc::clone(&self.backend)
    }

    pub fn trace(&self) -> Arc<TraceStore> {
        Arc::clone(&self.trace)
    }
}

/// Coordinates an out-of-band cancellation request with the active backend turn.
#[derive(Clone, Debug, Default)]
enum ExecutionCleanup {
    #[default]
    Idle,
    Pending,
    Failed(String),
}

/// Coordinates execution interruption without discarding pending provider evidence.
pub struct TurnCancellation {
    requested: Arc<AtomicBool>,
    restore_prompt: AtomicBool,
    restart: AtomicBool,
    cleanup: tokio::sync::watch::Sender<ExecutionCleanup>,
    retraction_allowed: AtomicBool,
    notify: Notify,
}

impl TurnCancellation {
    fn new() -> Self {
        Self {
            requested: Arc::new(AtomicBool::new(false)),
            restore_prompt: AtomicBool::new(false),
            restart: AtomicBool::new(false),
            cleanup: tokio::sync::watch::channel(ExecutionCleanup::Idle).0,
            retraction_allowed: AtomicBool::new(false),
            notify: Notify::new(),
        }
    }

    /// Clear cancellation state before dispatching the next broker request.
    pub fn arm(&self, retraction_allowed: bool) {
        self.requested.store(false, Ordering::Release);
        self.restore_prompt.store(false, Ordering::Release);
        self.restart.store(false, Ordering::Release);
        self.cleanup.send_replace(ExecutionCleanup::Idle);
        self.retraction_allowed
            .store(retraction_allowed, Ordering::Release);
    }

    /// Request cancellation without waiting for the broker's serialized request lane.
    pub fn request(&self, restore_prompt: bool) {
        self.restore_prompt.store(restore_prompt, Ordering::Release);
        self.requested.store(true, Ordering::Release);
        self.notify.notify_waiters();
    }

    /// Interrupt provider execution while retaining the current exchange for resumption.
    pub fn request_restart(&self) {
        self.restart.store(true, Ordering::Release);
        self.request(false);
    }

    /// Retain provider delivery while cancellation waits for terminal execution evidence.
    pub(crate) fn begin_cleanup(&self, restart: bool, restore_prompt: bool) -> Result<()> {
        anyhow::ensure!(
            matches!(*self.cleanup.borrow(), ExecutionCleanup::Idle)
                || self.restart.load(Ordering::Acquire) == restart,
            "pending cleanup cannot change between cancellation and restart"
        );
        self.restart.store(restart, Ordering::Release);
        self.restore_prompt.store(restore_prompt, Ordering::Release);
        self.cleanup.send_replace(ExecutionCleanup::Pending);
        Ok(())
    }

    /// Publish a retryable cleanup failure without dropping the provider consumer.
    pub(crate) fn fail_cleanup(&self, error: &anyhow::Error) {
        self.cleanup
            .send_replace(ExecutionCleanup::Failed(format!("{error:#}")));
    }

    fn restores_prompt(&self) -> bool {
        self.restore_prompt.load(Ordering::Acquire)
            && self.retraction_allowed.load(Ordering::Acquire)
    }

    async fn cancelled(&self) {
        loop {
            let notified = self.notify.notified();
            if self.requested.load(Ordering::Acquire) {
                return;
            }
            notified.await;
        }
    }
}

#[derive(Debug)]
struct TurnCancelled;

impl std::fmt::Display for TurnCancelled {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("Turn cancelled by user")
    }
}

impl std::error::Error for TurnCancelled {}

#[derive(Debug)]
struct TurnRetracted {
    prompt: String,
}

impl std::fmt::Display for TurnRetracted {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("Output-free turn retracted")
    }
}

impl std::error::Error for TurnRetracted {}

struct ExchangeAdmission {
    plan_event_id: Option<String>,
    prompt: String,
    kind: ExchangeKind,
    plan_id: Option<String>,
    execution_id: Option<String>,
    goal_id: Option<String>,
    agent_run_id: Option<String>,
}

struct PromptControlSnapshot {
    session: HarnessSession,
    goal: Option<GoalRecord>,
    execution: Vec<PlanExecutionRecord>,
    plan: Vec<PlanRecord>,
}

impl ExchangeAdmission {
    fn chat(prompt: String) -> Self {
        Self {
            plan_event_id: None,
            prompt,
            kind: ExchangeKind::Chat,
            plan_id: None,
            execution_id: None,
            goal_id: None,
            agent_run_id: None,
        }
    }

    fn agent(prompt: String, agent_run_id: String) -> Self {
        Self {
            plan_event_id: None,
            prompt,
            kind: ExchangeKind::Chat,
            plan_id: None,
            execution_id: None,
            goal_id: None,
            agent_run_id: Some(agent_run_id),
        }
    }

    fn plan(prompt: String, plan_id: Option<String>, revision: bool) -> Self {
        Self {
            plan_event_id: None,
            prompt,
            kind: if revision {
                ExchangeKind::PlanRevision
            } else {
                ExchangeKind::PlanDraft
            },
            plan_id,
            execution_id: None,
            goal_id: None,
            agent_run_id: None,
        }
    }

    fn execution(prompt: String, plan_id: String, execution_id: String, goal_id: String) -> Self {
        Self {
            plan_event_id: None,
            prompt,
            kind: ExchangeKind::PlanExecution,
            plan_id: Some(plan_id),
            execution_id: Some(execution_id),
            goal_id: Some(goal_id),
            agent_run_id: None,
        }
    }

    fn goal(prompt: String, goal_id: String) -> Self {
        Self {
            plan_event_id: None,
            prompt,
            kind: ExchangeKind::Chat,
            plan_id: None,
            execution_id: None,
            goal_id: Some(goal_id),
            agent_run_id: None,
        }
    }
}

/// Coordinates backend turns, durable state, plan review, goals, and interaction checkpoints.
pub struct HarnessBroker {
    repositories: Arc<forge_git::store::RepositoryStore>,
    diff: Arc<forge_diff::engine::DiffEngine>,
    store: SqliteStore,
    plan_file: PlanFileStore,
    workspace_kind: WorkspaceKind,
    data_root: PathBuf,
    client_id: String,
    backend_launch: BackendLaunch,
    backend: Arc<dyn Backend>,
    session: HarnessSession,
    capability: BackendCapability,
    goal_max_turns: u32,
    scope_deviation_review: ScopeDeviationReview,
    permission_store: Arc<RwLock<PermissionStore>>,
    permission_coordinator: Arc<PermissionCoordinator>,
    trace: Arc<TraceStore>,
    rustdoc: Arc<RustdocResolver>,
    event_sink: Option<BackendEventSink>,
    clock: Box<dyn Clock>,
    exchange_runtime: Option<ExchangeRuntime>,
    agent_registry: AgentRegistry,
    child_exchange_runtime_by_agent: HashMap<String, ChildExchangeRuntime>,
    presentation: Arc<std::sync::Mutex<SessionPresentation>>,
    composer_admission: Option<(forge_buffer::identity::DocumentId, u64)>,
    active_wait_projection: Option<ActiveWait>,
    timeline_reconciled_after_dispatch: bool,
    turn_cancellation: Arc<TurnCancellation>,
}

impl HarnessBroker {
    /// Initialize the broker and acquire one writable session lease.
    pub fn initialize(request: InitializeRequest) -> Result<Self> {
        Self::initialize_with_clock(request, Box::new(SystemClock))
    }

    /// Initialize the broker with an injected clock for deterministic lease tests.
    pub fn initialize_with_clock(
        request: InitializeRequest,
        clock: Box<dyn Clock>,
    ) -> Result<Self> {
        let workspace_kind = crate::workspace::resolve(Path::new(&request.workspace))?;
        let runtime = BrokerRuntime::initialize(
            &request,
            workspace_kind,
            Arc::new(forge_git::store::RepositoryStore::default()),
            forge_diff::engine::DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4),
        )?;
        Self::initialize_with_runtime_and_clock(request, runtime, clock)
    }

    /// Initialize one session controller against shared provider infrastructure.
    pub fn initialize_with_runtime(
        request: InitializeRequest,
        runtime: Arc<BrokerRuntime>,
    ) -> Result<Self> {
        Self::initialize_with_runtime_and_clock(request, runtime, Box::new(SystemClock))
    }

    fn initialize_with_runtime_and_clock(
        request: InitializeRequest,
        runtime: Arc<BrokerRuntime>,
        clock: Box<dyn Clock>,
    ) -> Result<Self> {
        let data_root = PathBuf::from(&request.data_root);
        let mut store = SqliteStore::open(&data_root)?;
        let workspace_kind = runtime.workspace_kind.clone();
        let workspace = match &workspace_kind {
            WorkspaceKind::Git(path) | WorkspaceKind::Untracked(path) => {
                path.to_string_lossy().into_owned()
            }
        };
        let now_ms = clock.now_ms();
        let permission_store = Arc::clone(&runtime.permission_store);
        let permission_coordinator = Arc::clone(&runtime.permission_coordinator);
        let backend = Arc::clone(&runtime.backend);
        let trace = runtime.trace();
        let backend_descriptor = backend.descriptor();
        let requested_new_session_name = request.new_session_name.as_deref().map(str::trim);
        let force_new_session = requested_new_session_name.is_some()
            || request.lease_conflict_action.as_deref() == Some("new");
        let mut session = match request.session_id.as_deref().filter(|_| !force_new_session) {
            Some(session_id) => {
                store.acquire_session_lease(session_id, &request.client_id, now_ms)?
            }
            None => {
                let latest_session = if force_new_session {
                    None
                } else {
                    store
                        .list_session(Some(&workspace))?
                        .into_iter()
                        .find(|session| session.backend == request.backend.kind)
                };
                match latest_session {
                    Some(session) => {
                        store.acquire_session_lease(&session.id, &request.client_id, now_ms)?
                    }
                    None => {
                        let preference =
                            store.load_preference(&workspace, &request.backend.kind)?;
                        let session_id = Uuid::new_v4().to_string();
                        let mut session = HarnessSession {
                            primary_agent_id: HarnessSession::primary_agent_id(&session_id),
                            id: session_id,
                            name: requested_new_session_name.unwrap_or_default().to_owned(),
                            workspace,
                            backend: request.backend.kind.clone(),
                            backend_session_id: None,
                            provider_checkpoint_id: None,
                            provider_fork_state: ProviderForkState::Ready,
                            model: preference
                                .as_ref()
                                .map_or(request.model, |value| value.model.clone()),
                            provider_label: backend_descriptor.label.clone(),
                            resolved_model: None,
                            effort: preference
                                .as_ref()
                                .map_or(request.effort, |value| value.effort.clone()),
                            context_window: None,
                            fast_mode: preference.as_ref().is_some_and(|value| value.fast_mode),
                            execution_mode: ExecutionMode::Read,
                            mode: HarnessMode::Read,
                            created_at_ms: now_ms,
                            updated_at_ms: now_ms,
                            active_plan_id: None,
                            goal_id: None,
                            lease_owner: None,
                            lease_expires_at_ms: None,
                            native_fork: false,
                            native_compact: backend_descriptor.capability.native_compact,
                            context_usage: None,
                        };
                        session.context_window = preference.as_ref().and_then(|preference| {
                            preference
                                .model_setting
                                .get(&session.model)
                                .and_then(|setting| setting.context_window.clone())
                        });
                        acquire_lease(&mut session, &request.client_id, now_ms)?;
                        session
                    }
                }
            }
        };
        if session.provider_label.is_empty() {
            session.provider_label = backend_descriptor.label.clone();
        }
        if session.resolved_model.is_none() && session.model != "default" {
            session.resolved_model = Some(session.model.clone());
        }
        session.updated_at_ms = now_ms;
        store.save_session(&session)?;
        if store
            .list_agent_run(&session.id)?
            .iter()
            .all(|agent| agent.id != session.primary_agent_id)
        {
            store.save_agent_run(&Agent::primary(&session.id, now_ms))?;
        }
        let previous_preference = store.load_preference(&session.workspace, &session.backend)?;
        store.save_preference(
            &session.workspace,
            &session.backend,
            &preference_for_session(&session, previous_preference),
        )?;
        let mut capability = backend_descriptor.capability;
        capability.native_fork = session.native_fork || capability.native_fork;
        capability.native_compact = session.native_compact || capability.native_compact;
        store.interrupt_detached_execution(&session.id)?;
        let agent_registry = load_agent_registry(&store, &session.id)?;
        let presentation = Arc::new(std::sync::Mutex::new(SessionPresentation::new(
            session.id.clone(),
        )));
        let broker = Self {
            repositories: Arc::clone(&runtime.repositories),
            diff: Arc::clone(&runtime.diff),
            store,
            plan_file: PlanFileStore::new(&data_root, &session.workspace),
            workspace_kind,
            data_root,
            client_id: request.client_id,
            backend_launch: request.backend,
            backend,
            session,
            capability,
            goal_max_turns: request.goal_max_turns,
            scope_deviation_review: ScopeDeviationReview::Auto,
            permission_store,
            permission_coordinator,
            trace,
            rustdoc: Arc::clone(&runtime.rustdoc),
            event_sink: None,
            clock,
            exchange_runtime: None,
            agent_registry,
            child_exchange_runtime_by_agent: HashMap::new(),
            presentation,
            composer_admission: None,
            active_wait_projection: None,
            timeline_reconciled_after_dispatch: false,
            turn_cancellation: Arc::new(TurnCancellation::new()),
        };
        let initial_timeline = broker.snapshot()?.timeline;
        broker
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .initialize(initial_timeline)?;
        Ok(broker)
    }

    /// Share the out-of-band cancellation signal with the broker transport loop.
    pub fn turn_cancellation(&self) -> Arc<TurnCancellation> {
        Arc::clone(&self.turn_cancellation)
    }

    /// Clone the backend control boundary for out-of-band active-turn requests.
    pub fn backend_handle(&self) -> Arc<dyn Backend> {
        Arc::clone(&self.backend)
    }

    /// Build the stable provider-catalog identity used outside serialized broker dispatch.
    pub fn backend_catalog_request(&self) -> BackendCatalogRequest {
        BackendCatalogRequest {
            harness_session_id: self.session.id.clone(),
            workspace: self.session.workspace.clone(),
            execution_mode: self.session.execution_mode,
            backend_session_id: self.session.backend_session_id.clone(),
        }
    }

    fn control_turn_context(
        &self,
        mode: PromptMode,
    ) -> Option<crate::control_tools::ControlTurnContext> {
        let plan = self
            .session
            .active_plan_id
            .as_deref()
            .and_then(|plan_id| self.store.load_plan(plan_id).ok().flatten());
        let document = plan.as_ref().and_then(|plan| {
            self.plan_file
                .read_working_document(&self.session.id, &plan.id)
                .ok()
        });
        Some(crate::control_tools::ControlTurnContext {
            mode,
            planning_feedback: mode == PromptMode::Plan
                && plan
                    .as_ref()
                    .is_some_and(|plan| plan.state == crate::plan::PlanState::AwaitingInput),
            plan_state: plan.as_ref().map(|plan| plan.state),
            plan_document: document,
            resolved_question_digest_set: plan
                .as_ref()
                .map(|plan| {
                    plan.question_ledger
                        .resolution
                        .iter()
                        .map(|item| item.content_digest.clone())
                        .collect()
                })
                .unwrap_or_default(),
            has_active_elicitation: plan.as_ref().is_some_and(|plan| plan.elicitation.is_some()),
            has_active_execution: mode == PromptMode::ExecutePlan,
            has_active_goal: self.session.goal_id.is_some(),
            workspace_root: Some(PathBuf::from(&self.session.workspace)),
            rustdoc: Some(Arc::clone(&self.rustdoc)),
            repository: None,
        })
    }

    /// Share the out-of-band permission lane with the broker transport loop.
    pub fn permission_coordinator(&self) -> Arc<PermissionCoordinator> {
        Arc::clone(&self.permission_coordinator)
    }

    /// Build the initial snapshot returned by the initialize response.
    pub fn snapshot(&self) -> Result<BrokerSnapshot> {
        let goal = self
            .session
            .goal_id
            .as_deref()
            .map(|id| self.store.load_goal(id))
            .transpose()?
            .flatten();
        let active_plan = self
            .session
            .active_plan_id
            .as_deref()
            .map(|id| self.store.load_plan(id))
            .transpose()?
            .flatten();
        if let Some(plan) = active_plan.as_ref() {
            self.record_plan_trace("snapshot_plan_state", plan_trace_fields(plan));
        }
        let interaction = self.store.list_exchange(&self.session.id)?;
        let active_elicitation = active_plan
            .as_ref()
            .and_then(|plan| {
                plan.acceptance
                    .as_ref()
                    .map(|acceptance| ActiveElicitation {
                        owner: ElicitationOwner::PlanAcceptance,
                        plan_id: Some(plan.id.clone()),
                        exchange_id: None,
                        elicitation: acceptance.elicitation.clone(),
                    })
            })
            .or_else(|| {
                active_plan.as_ref().and_then(|plan| {
                    plan.elicitation
                        .clone()
                        .map(|elicitation| ActiveElicitation {
                            owner: ElicitationOwner::Plan,
                            plan_id: Some(plan.id.clone()),
                            exchange_id: None,
                            elicitation,
                        })
                })
            })
            .or_else(|| {
                interaction.iter().rev().find_map(|interaction| {
                    interaction
                        .elicitation
                        .clone()
                        .map(|elicitation| ActiveElicitation {
                            owner: ElicitationOwner::Interaction,
                            plan_id: interaction.plan_id.clone(),
                            exchange_id: Some(interaction.id.clone()),
                            elicitation,
                        })
                })
            });
        let plan_list = self.store.list_plan(&self.session.id)?;
        let lifecycle_list = self.store.list_plan_lifecycle(&self.session.id)?;
        let execution_list = self.store.list_plan_execution(&self.session.id)?;
        let goal_execution = goal.as_ref().and_then(|goal| {
            execution_list
                .iter()
                .rev()
                .find(|execution| execution.goal_id == goal.id)
                .cloned()
        });
        let active_wait = self.active_wait_projection.clone();
        let status = crate::session::state_machine::SessionPhase::resolve(
            active_plan.as_ref(),
            active_wait.as_ref(),
            interaction.last(),
        );
        self.record_plan_trace(
            "status_projected",
            json!({ "session_id": self.session.id, "status": status }),
        );
        let agent_run_list = self.agent_registry.list();
        let mut agent_exchange_list = Vec::new();
        for run in &agent_run_list {
            agent_exchange_list.extend(self.store.list_agent_exchange(&run.id)?);
        }
        let mut timeline = TimelineProjector::build(TimelineProjection {
            interaction_list: interaction.clone(),
            plan_list: &plan_list,
            lifecycle_list,
            execution_list,
            deviation_list: self.store.list_plan_deviation(&self.session.id)?,
            audit_list: self.store.list_plan_audit(&self.session.id)?,
            resolution_list: self.store.list_plan_resolution(&self.session.id)?,
            agent_run_list: agent_run_list.clone(),
            agent_exchange_list: agent_exchange_list.clone(),
            session_event_list: self.store.list_session_event(&self.session.id)?,
            plan_file: &self.plan_file,
        })?;
        if status.visible() {
            timeline.push(TimelineEntry::Status {
                id: format!("{}:status", self.session.id),
                created_at_ms: 0,
                status: status.clone(),
            });
        }
        let approval = self
            .permission_coordinator
            .pending_list()?
            .into_iter()
            .filter(|approval| {
                interaction
                    .iter()
                    .chain(agent_exchange_list.iter())
                    .any(|exchange| approval.exchange_id.as_deref() == Some(exchange.id.as_str()))
            })
            .collect();
        Ok(BrokerSnapshot {
            session: self.session.clone(),
            exchange: interaction,
            capability: self.capability.clone(),
            no_checkpoint: matches!(self.workspace_kind, WorkspaceKind::Untracked(_)),
            goal,
            active_plan,
            active_elicitation,
            artifact: plan_list
                .iter()
                .filter(|plan| !plan.working_path.is_empty())
                .map(ArtifactSummary::from)
                .collect(),
            timeline,
            timeline_revision: self
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .revision(),
            goal_execution,
            active_wait,
            prompt_history: self.store.list_prompt_history()?,
            agent: AgentSnapshot {
                definition: if self.capability.agent.catalog {
                    load_codex_agent_catalog(Path::new(&self.session.workspace))?
                } else {
                    Vec::new()
                },
                run: agent_run_list,
                exchange: agent_exchange_list,
            },
            approval,
        })
    }

    pub(crate) fn capture_plan_review(
        &self,
        plan_id: &str,
        expected_digest: &str,
    ) -> Result<crate::plan::review_source::PlanReviewSource> {
        anyhow::ensure!(
            !plan_id.is_empty() && plan_id.len() <= 256,
            "invalid plan identity"
        );
        let plan = self
            .store
            .load_plan(plan_id)?
            .context("plan review record is missing")?;
        anyhow::ensure!(
            plan.session_id == self.session.id,
            "plan belongs to another session"
        );
        anyhow::ensure!(
            plan.review_digest.as_deref() == Some(expected_digest),
            "plan review digest changed"
        );
        self.plan_file.capture_review_source(
            &self.session.id,
            &plan.id,
            plan.model_revision,
            expected_digest,
        )
    }

    /// Route one client request through the owning feature boundary.
    pub async fn dispatch(&mut self, request: Request) -> DispatchResult {
        let id = request.id;
        let method = match HarnessMethod::decode(&request.method) {
            Ok(method) => method,
            Err(error) => {
                return DispatchResult {
                    response: Response::failure(id, "unknown_method", error.to_string()),
                    event: Vec::new(),
                };
            }
        };
        self.timeline_reconciled_after_dispatch = false;
        self.trace.record(
            &self.session.id,
            "lua.rpc.received",
            json!({
                "id": id,
                "method": request.method,
                "params": request.params.clone(),
            }),
        );
        self.record_plan_trace(
            "request_received",
            json!({ "request_id": id, "method": request.method }),
        );
        let outcome = self.dispatch_inner(method, request.params).await;
        match outcome {
            Ok((value, mut event)) => {
                self.active_wait_projection = None;
                let timeline_patch = match self.reconcile_after_dispatch() {
                    Ok(timeline_patch) => timeline_patch,
                    Err(error) => {
                        return DispatchResult {
                            response: Response::failure(
                                id,
                                "timeline_projection_failed",
                                error.to_string(),
                            ),
                            event: Vec::new(),
                        };
                    }
                };
                if !timeline_patch.is_empty() {
                    match serde_json::to_value(timeline_patch)
                        .map_err(Into::into)
                        .and_then(|payload| self.event("timeline_patch", payload))
                    {
                        Ok(timeline_event) => event.push(timeline_event),
                        Err(error) => {
                            return DispatchResult {
                                response: Response::failure(
                                    id,
                                    "timeline_patch_failed",
                                    error.to_string(),
                                ),
                                event: Vec::new(),
                            };
                        }
                    }
                }
                match Response::success(id, value) {
                    Ok(response) => {
                        self.trace.record(
                            &self.session.id,
                            "lua.rpc.completed",
                            json!({
                                "id": id,
                                "response": response.clone(),
                                "event": event.clone(),
                            }),
                        );
                        self.record_plan_trace(
                            "request_succeeded",
                            json!({ "request_id": id, "event_count": event.len() }),
                        );
                        DispatchResult { response, event }
                    }
                    Err(error) => DispatchResult {
                        response: Response::failure(id, "encode_response", error.to_string()),
                        event: Vec::new(),
                    },
                }
            }
            Err(error) => {
                self.active_wait_projection = None;
                let mut event = Vec::new();
                if let Ok(timeline_patch) = self.reconcile_after_dispatch()
                    && !timeline_patch.is_empty()
                    && let Ok(payload) = serde_json::to_value(timeline_patch)
                    && let Ok(timeline_event) = self.event("timeline_patch", payload)
                {
                    event.push(timeline_event);
                }
                if let Some(retracted) = error.downcast_ref::<TurnRetracted>() {
                    return DispatchResult {
                        response: Response::failure_with_data(
                            id,
                            "turn_retracted",
                            retracted.to_string(),
                            json!({ "prompt": retracted.prompt }),
                        ),
                        event,
                    };
                }
                let code = if error.downcast_ref::<TurnCancelled>().is_some() {
                    "turn_cancelled"
                } else {
                    "request_failed"
                };
                self.record_plan_trace("request_failed", json!({ "request_id": id, "code": code }));
                self.trace.record(
                    &self.session.id,
                    "lua.rpc.failed",
                    json!({
                        "id": id,
                        "code": code,
                        "error": format!("{error:#}"),
                        "event": event.clone(),
                    }),
                );
                DispatchResult {
                    response: Response::failure(id, code, format!("{error:#}")),
                    event,
                }
            }
        }
    }

    /// Route one client request while forwarding provider updates as they arrive.
    pub async fn dispatch_stream(
        &mut self,
        request: Request,
        event_sink: BackendEventSink,
    ) -> DispatchResult {
        self.event_sink = Some(event_sink);
        let result = self.dispatch(request).await;
        self.event_sink = None;
        result
    }

    async fn dispatch_inner(
        &mut self,
        method: HarnessMethod,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        if !matches!(
            method,
            HarnessMethod::StateGet | HarnessMethod::SessionList | HarnessMethod::SessionPreview
        ) {
            self.refresh_lease()?;
        }
        match method {
            HarnessMethod::StateGet => Ok((serde_json::to_value(self.snapshot()?)?, Vec::new())),
            HarnessMethod::TraceStatus => {
                Ok((serde_json::to_value(self.trace.status())?, Vec::new()))
            }
            HarnessMethod::TraceConfigure => self.configure_trace(params),
            HarnessMethod::TraceToggle => {
                Ok((serde_json::to_value(self.trace.toggle()?)?, Vec::new()))
            }
            HarnessMethod::TraceClear => {
                Ok((serde_json::to_value(self.trace.clear()?)?, Vec::new()))
            }
            HarnessMethod::BackendModels => self.list_backend_model().await,
            HarnessMethod::AgentList => {
                Ok((serde_json::to_value(self.snapshot()?.agent)?, Vec::new()))
            }
            HarnessMethod::AgentStart => self.start_agent(params).await,
            HarnessMethod::PermissionsOpen => self.open_permission_document(),
            HarnessMethod::PermissionsSave => self.save_permission_document(params),
            HarnessMethod::SessionExecutionMode => self.select_execution_mode(params),
            HarnessMethod::SessionMode => self.select_harness_mode(params),
            HarnessMethod::ExchangeResume => self.resume_exchange(params).await,
            HarnessMethod::PromptSubmit => self.submit_prompt(params).await,
            HarnessMethod::HistoryRecord => self.record_prompt_history(params),
            HarnessMethod::QueueEditLast => {
                anyhow::bail!("prompt queue ownership lives in the Neovim client")
            }
            HarnessMethod::PlanAccept => self.accept_plan(params).await,
            HarnessMethod::PlanAcceptanceBegin => self.begin_plan_acceptance(params),
            HarnessMethod::PlanAcceptanceCancel => self.cancel_plan_acceptance(),
            HarnessMethod::PlanEntityRename => self.rename_plan_entity(params),
            HarnessMethod::PlanRustdocHover => self.rustdoc_hover(params).await,
            HarnessMethod::PlanRustdocSource => self.rustdoc_source(params).await,
            HarnessMethod::PlanRequestChanges => self.request_plan_changes(params).await,
            HarnessMethod::PlanCancel => self.cancel_plan(),
            HarnessMethod::PlanActivate => self.activate_plan(params),
            HarnessMethod::PlanList => self.list_replanning_choices(),
            HarnessMethod::PlanScopeDeviationReview => self.select_scope_deviation_review(params),
            HarnessMethod::PlanDeviationResolve => self.resolve_plan_deviation(params).await,
            HarnessMethod::QuestionAnswer => self.answer_question(params),
            HarnessMethod::QuestionSkip => self.skip_question(params),
            HarnessMethod::QuestionAsk => self.ask_question(params).await,
            HarnessMethod::QuestionContinue => self.continue_question().await,
            HarnessMethod::GoalSet => self.set_goal(params).await,
            HarnessMethod::GoalPause => self.pause_goal().await,
            HarnessMethod::GoalResume => self.resume_goal().await,
            HarnessMethod::GoalClear => self.clear_goal().await,
            HarnessMethod::GoalContinue => self.continue_goal().await,
            HarnessMethod::ExchangeList => Ok((
                serde_json::to_value(self.list_exchange_review()?)?,
                Vec::new(),
            )),
            HarnessMethod::ExchangeCommentSave => self.save_exchange_comment(params),
            HarnessMethod::ExchangeRequestChanges => self.request_exchange_changes(params).await,
            HarnessMethod::ExchangeRollback => self.rollback_exchange(params).await,
            HarnessMethod::SessionNew | HarnessMethod::SessionClear => {
                self.new_session(params).await
            }
            HarnessMethod::SessionList => self.list_session(params),
            HarnessMethod::SessionPreview => self.preview_session(params),
            HarnessMethod::SessionResume => self.resume_session(params).await,
            HarnessMethod::SessionRename => self.rename_session(params),
            HarnessMethod::SessionConfigure => self.configure_session(params).await,
            HarnessMethod::SessionCompact => self.compact_session().await,
            HarnessMethod::SessionDelete => self.delete_session(params),
            HarnessMethod::SessionFork => anyhow::bail!(
                "session.fork must route through the process-wide session coordinator"
            ),
            HarnessMethod::Shutdown => {
                self.release_lease()?;
                Ok((json!({ "shutdown": true }), Vec::new()))
            }
            HarnessMethod::TurnCancel => self.retry_finalization().await,
            HarnessMethod::TurnRestart
            | HarnessMethod::TurnSteer
            | HarnessMethod::ApprovalResolve
            | HarnessMethod::BackendSkills
            | HarnessMethod::BackendSkillsSetEnabled
            | HarnessMethod::BackendMcp
            | HarnessMethod::BackendMcpSetEnabled => {
                anyhow::bail!("Harness control method requires the process coordinator")
            }
        }
    }

    async fn rustdoc_hover(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let target = self
            .resolve_rustdoc_target(&params, "plan.rustdoc.hover")
            .await?;
        let hover = if target.selection == "receiver" {
            self.rustdoc
                .type_hover(
                    &target.receiver_package,
                    &target.receiver_version,
                    &target.receiver,
                )
                .await?
        } else {
            let callable = rustdoc_callable(target.callable.as_ref())?;
            self.rustdoc
                .callable_hover(
                    &target.package_list,
                    &target.receiver_package,
                    &target.receiver_version,
                    &target.receiver,
                    &callable.name,
                    callable.kind,
                )
                .await?
        };
        let mut width = forge_buffer::width::WidthProfile::default();
        width.columns = 60;
        let content = format!("```rust\n{}\n```\n\n{}", hover.signature, hover.docs);
        let rendered = forge_buffer::markdown::MarkdownRenderer::render(
            forge_buffer::identity::BlockId("rustdoc".into()),
            &content,
            &width,
        )?;
        let document = forge_buffer::document::BufferDocument::new(
            forge_buffer::identity::DocumentId(format!("plan:rustdoc:{}", uuid::Uuid::new_v4())),
            vec![rendered.block],
        )?;
        let mut result = serde_json::to_value(hover)?;
        result["document"] = serde_json::to_value(document.snapshot())?;
        Ok((result, Vec::new()))
    }

    async fn rustdoc_source(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let target = self
            .resolve_rustdoc_target(&params, "plan.rustdoc.source")
            .await?;
        let location = if target.selection == "receiver" {
            self.rustdoc
                .type_source(
                    &target.receiver_package,
                    &target.receiver_version,
                    &target.receiver,
                )
                .await?
        } else {
            let callable = rustdoc_callable(target.callable.as_ref())?;
            self.rustdoc
                .callable_source(
                    &target.package_list,
                    &target.receiver_package,
                    &target.receiver_version,
                    &target.receiver,
                    &callable.name,
                    callable.kind,
                )
                .await?
        };
        let current = self
            .plan_file
            .read_working_document(&self.session.id, &target.plan_id)?;
        anyhow::ensure!(
            current.version == target.expected_version,
            "plan version changed before Rust source resolved"
        );
        Ok((serde_json::to_value(location)?, Vec::new()))
    }

    async fn resolve_rustdoc_target(
        &mut self,
        params: &Value,
        method: &str,
    ) -> Result<RustdocTarget> {
        let plan_id = params
            .get("plan_id")
            .and_then(Value::as_str)
            .with_context(|| format!("{method} requires plan_id"))?
            .to_owned();
        let expected_version = params
            .get("expected_version")
            .and_then(Value::as_u64)
            .with_context(|| format!("{method} requires expected_version"))?;
        let json_path = params
            .get("json_path")
            .and_then(Value::as_str)
            .with_context(|| format!("{method} requires json_path"))?;
        let selection = params
            .get("selection")
            .and_then(Value::as_str)
            .with_context(|| format!("{method} requires selection"))?
            .to_owned();
        anyhow::ensure!(
            matches!(selection.as_str(), "receiver" | "callable"),
            "{method} selection must be receiver or callable"
        );
        let mut document = self
            .plan_file
            .read_working_document(&self.session.id, &plan_id)?;
        anyhow::ensure!(
            document.version == expected_version,
            "plan version changed before Rust documentation resolved"
        );
        let document_value = serde_json::to_value(&document)?;
        let edge = document_value
            .pointer(json_path)
            .context("Rust documentation flow edge no longer exists")
            .and_then(|value| {
                serde_json::from_value::<crate::plan::PlanFlowEdge>(value.clone())
                    .context("Rust documentation path does not identify a flow edge")
            })?;
        let crate::plan::EntityReference::ExternalEntity {
            name: receiver,
            dependency: Some(receiver_dependency),
            ..
        } = &edge.target
        else {
            anyhow::bail!("Rust documentation requires an external typed receiver");
        };
        let receiver = receiver.clone();
        let receiver_dependency = receiver_dependency.clone();
        let callable = edge.callable;
        let dependency_index = document
            .dependencies
            .iter()
            .position(|dependency| dependency.name == receiver_dependency)
            .context("Rust receiver dependency is not declared by this plan")?;
        let dependency_index_list = if selection == "callable" {
            (0..document.dependencies.len()).collect::<Vec<_>>()
        } else {
            vec![dependency_index]
        };
        for index in dependency_index_list {
            if document.dependencies[index].resolved_version.is_some() {
                continue;
            }
            let dependency_name = document.dependencies[index].name.clone();
            let dependency_requirement = document.dependencies[index].version.clone();
            let version = self
                .rustdoc
                .resolve_version(&dependency_name, &dependency_requirement)
                .await?;
            document.dependencies[index].resolved_version = Some(version);
        }
        let receiver_package = document.dependencies[dependency_index].name.clone();
        let receiver_version = document.dependencies[dependency_index]
            .resolved_version
            .clone()
            .context("Rust receiver dependency version was not resolved")?;
        let package_list = document
            .dependencies
            .iter()
            .filter_map(|dependency| {
                dependency
                    .resolved_version
                    .as_ref()
                    .map(|version| (dependency.name.clone(), version.clone()))
            })
            .collect();
        Ok(RustdocTarget {
            plan_id,
            expected_version,
            selection,
            receiver,
            receiver_package,
            receiver_version,
            callable,
            package_list,
        })
    }

    fn open_permission_document(&self) -> Result<(Value, Vec<SessionEvent>)> {
        let (path, source) = self
            .permission_store
            .read()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .open();
        Ok((json!({ "path": path, "source": source }), Vec::new()))
    }

    fn save_permission_document(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let source = required_text(&params, "source")?;
        self.permission_store
            .write()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .save(&source)?;
        self.open_permission_document()
    }

    fn select_execution_mode(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let mode = serde_json::from_value::<ExecutionMode>(
            params.get("mode").cloned().context("mode is required")?,
        )?;
        anyhow::ensure!(
            self.capability.execution_mode_list.contains(&mode),
            "execution mode {} is unavailable for this backend",
            mode.label()
        );
        self.session.execution_mode = mode;
        self.session.mode = mode.into();
        self.save_session()?;
        let session = serde_json::to_value(&self.session)?;
        Ok((
            session.clone(),
            vec![self.event("execution_mode_changed", session)?],
        ))
    }

    fn select_harness_mode(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let mode = serde_json::from_value::<HarnessMode>(
            params.get("mode").cloned().context("mode is required")?,
        )?;
        if let Some(execution_mode) = mode.execution_mode() {
            anyhow::ensure!(
                self.capability
                    .execution_mode_list
                    .contains(&execution_mode),
                "execution mode {} is unavailable for this backend",
                execution_mode.label()
            );
            self.session.execution_mode = execution_mode;
        }
        self.session.mode = mode;
        self.save_session()?;
        let session = serde_json::to_value(&self.session)?;
        Ok((session.clone(), vec![self.event("mode_changed", session)?]))
    }

    fn select_scope_deviation_review(
        &mut self,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        self.scope_deviation_review = serde_json::from_value(
            params
                .get("policy")
                .cloned()
                .context("scope deviation review policy is required")?,
        )?;
        Ok((json!({ "policy": self.scope_deviation_review }), Vec::new()))
    }

    async fn resume_exchange(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let text = required_text(&params, "text")?;
        let interaction = self
            .store
            .list_exchange(&self.session.id)?
            .into_iter()
            .last()
            .context("no Harness interaction is available to resume")?;
        anyhow::ensure!(
            interaction.state == ExchangeState::Running
                && interaction.execution_started_at_ms.is_none()
                && !interaction.awaiting_input
                && interaction
                    .turn
                    .iter()
                    .all(|turn| turn.state() != crate::turn::TurnState::Running),
            "the latest Harness exchange is not paused for resumption"
        );
        let mode = match interaction.kind {
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision => PromptMode::Plan,
            ExchangeKind::PlanExecution => PromptMode::ExecutePlan,
            ExchangeKind::Chat if interaction.goal_id.is_some() => PromptMode::GoalContinuation,
            ExchangeKind::Chat => PromptMode::Chat,
        };
        if mode == PromptMode::Plan {
            self.run_planning_interaction(text, None).await
        } else {
            self.run_interaction(text, mode, None).await
        }
    }

    async fn start_agent(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            self.capability.agent.spawn == crate::agent::AgentControlMode::ParentMediated,
            "the current backend does not support spawning child agents"
        );
        let definition = required_text(&params, "definition")?;
        let task = required_text(&params, "task")?;
        anyhow::ensure!(
            load_codex_agent_catalog(Path::new(&self.session.workspace))?
                .iter()
                .any(|candidate| candidate.name == definition),
            "unknown agent definition: {definition}"
        );
        let run = Agent::pending(&self.session.id, &definition, &task, self.clock.now_ms());
        self.store.save_agent_run(&run)?;
        self.agent_registry.insert(run.clone());
        self.agent_registry.execution_mut(&run.id).task = task.clone();
        let prompt = format!(
            "Call the subagent spawn tool exactly once from this parent turn. Set its agent type exactly to `{definition}` and give that child the task below. Do not spawn a default, intermediary, or coordinator agent. Wait for the selected child to finish, then synthesize its result for the user.\n\n{task}"
        );
        let result = self
            .run_interaction(
                prompt,
                PromptMode::Chat,
                Some(ExchangeAdmission::agent(
                    format!("/spawn {definition} {task}"),
                    run.id.clone(),
                )),
            )
            .await;
        let mut run = self.agent_registry.get(&run.id).cloned().unwrap_or(run);
        if run.provider_thread_id.is_none() {
            run.state = crate::agent::AgentState::Closed;
            run.updated_at_ms = self.clock.now_ms();
            self.store.save_agent_run(&run)?;
            self.agent_registry.insert(run.clone());
        }
        let (_, mut event) = result?;
        event.push(self.event(
            "agent_updated",
            serde_json::to_value(self.snapshot()?.agent)?,
        )?);
        Ok((serde_json::to_value(run)?, event))
    }

    async fn route_agent_backend_event(
        &mut self,
        mut parent: Option<&mut Exchange>,
        backend_event: &BackendEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        let thread_id = backend_event
            .address
            .as_ref()
            .map(|address| address.thread_id.as_str())
            .or_else(|| {
                backend_event
                    .data
                    .pointer("/params/threadId")
                    .or_else(|| backend_event.data.pointer("/params/thread_id"))
                    .or_else(|| backend_event.data.pointer("/params/turn/threadId"))
                    .or_else(|| backend_event.data.pointer("/params/turn/thread_id"))
                    .and_then(Value::as_str)
            });
        let Some(run_id) = thread_id
            .and_then(|thread_id| self.agent_registry.get_by_thread(thread_id))
            .map(|run| run.id.clone())
        else {
            return Ok(false);
        };
        let parent_agent_id = self
            .agent_registry
            .execution(&run_id)
            .and_then(|execution| execution.parent_thread_id.as_deref())
            .and_then(|thread| self.agent_registry.get_by_thread(thread))
            .map(|agent| agent.id.clone());
        if let Some(parent_agent_id) = parent_agent_id
            && parent.as_ref().map(|exchange| exchange.agent_id.as_str())
                != Some(parent_agent_id.as_str())
        {
            let mut owner = self
                .child_exchange_runtime_by_agent
                .remove(&parent_agent_id)
                .context("nested child event has no active parent exchange")?;
            let result = Box::pin(self.route_agent_backend_event(
                Some(&mut owner.exchange),
                backend_event,
                event,
            ))
            .await;
            self.child_exchange_runtime_by_agent
                .insert(parent_agent_id, owner);
            return result;
        }
        let now_ms = self.clock.now_ms();
        if backend_event.kind == "turn_started" {
            if let Some(address) = &backend_event.address {
                let previous = self.store.list_agent_exchange(&run_id)?;
                if previous.iter().any(|record| {
                    record.turn.iter().any(|turn| {
                        turn.provider() == address
                            && turn.state() != crate::turn::TurnState::Running
                    })
                }) {
                    return Ok(true);
                }
            }
            let turn_id = backend_event
                .address
                .as_ref()
                .map(|address| address.turn_id.clone())
                .or_else(|| {
                    backend_event
                        .data
                        .pointer("/params/turn/id")
                        .and_then(Value::as_str)
                        .map(str::to_owned)
                });
            if let Some(parent) = parent.as_deref_mut() {
                let latest = parent.node_list.iter().rev().find_map(|node| match node {
                    ExchangeNode::AgentReference { agent } if agent.child_agent_id == run_id => {
                        Some(agent)
                    }
                    _ => None,
                });
                let needs_delegation = match latest {
                    None => true,
                    Some(delegation) => {
                        self.store
                            .list_agent_exchange(&run_id)?
                            .iter()
                            .any(|record| {
                                record.id == delegation.child_exchange_id
                                    && record.completed_at_ms.is_some()
                            })
                    }
                };
                if needs_delegation {
                    let invocation_id = turn_id
                        .as_deref()
                        .context("child start requires a provider turn identity")?;
                    let task = self
                        .agent_registry
                        .execution(&run_id)
                        .map(|execution| execution.task.clone())
                        .context("child start references an unknown agent execution")?;
                    parent.append_delegation(&run_id, invocation_id, &task, now_ms)?;
                    self.store.save_exchange(parent)?;
                }
            }
            self.agent_registry.execution_mut(&run_id).active_turn_id = turn_id;
            if let Some(run) = self.agent_registry.get_mut(&run_id) {
                run.state = AgentState::Ready;
                run.updated_at_ms = now_ms;
                self.store.save_agent_run(run)?;
            }
        }
        if backend_event.kind == "turn_completed" {
            if let Some(address) = &backend_event.address {
                if self
                    .agent_registry
                    .execution(&run_id)
                    .and_then(|execution| execution.active_turn_id.as_deref())
                    != Some(address.turn_id.as_str())
                {
                    return Ok(true);
                }
            }
            if let Some(mut runtime) = self.child_exchange_runtime_by_agent.remove(&run_id) {
                runtime.exchange.observe_turn(backend_event, now_ms)?;
                let outcome = match backend_event.turn_boundary {
                    Some(crate::backend::TurnBoundary::Finished { outcome }) => outcome,
                    _ => crate::turn::TurnOutcome::Completed,
                };
                finish_synthetic_turn(
                    &mut runtime.exchange,
                    runtime.synthetic_turn.as_ref(),
                    outcome,
                    now_ms,
                )?;
                let state = match outcome {
                    crate::turn::TurnOutcome::Completed => ExchangeState::Complete,
                    crate::turn::TurnOutcome::Failed => ExchangeState::Failed,
                    crate::turn::TurnOutcome::Cancelled => ExchangeState::Cancelled,
                    crate::turn::TurnOutcome::Interrupted => ExchangeState::Interrupted,
                };
                runtime.exchange.finish(state, now_ms)?;
                runtime.exchange.duration_ms =
                    now_ms.saturating_sub(runtime.exchange.created_at_ms) as u64;
                self.store.save_exchange(&runtime.exchange)?;
            }
            self.agent_registry.execution_mut(&run_id).active_turn_id = None;
            if let Some(run) = self.agent_registry.get_mut(&run_id) {
                run.updated_at_ms = now_ms;
                self.store.save_agent_run(run)?;
            }
            self.emit_live(
                BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "agent_updated".into(),
                    text: None,
                    data: serde_json::to_value(self.snapshot()?.agent)?,
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                event,
            )
            .await?;
            return Ok(true);
        }
        let mut runtime = match self.child_exchange_runtime_by_agent.remove(&run_id) {
            Some(runtime) => runtime,
            None => {
                if backend_event.kind != "turn_started" {
                    return Ok(true);
                }
                anyhow::ensure!(
                    self.agent_registry.get(&run_id).is_some(),
                    "child event references an unknown agent run"
                );
                let delegation = parent
                    .and_then(|parent| {
                        parent.node_list.iter().rev().find_map(|node| match node {
                            ExchangeNode::AgentReference { agent }
                                if agent.child_agent_id == run_id =>
                            {
                                Some(agent)
                            }
                            _ => None,
                        })
                    })
                    .context("child execution has no owning delegation")?;
                let mut interaction = self
                    .store
                    .list_agent_exchange(&run_id)?
                    .into_iter()
                    .find(|exchange| exchange.id == delegation.child_exchange_id)
                    .context("delegation has no durable child exchange")?;
                anyhow::ensure!(
                    interaction.state == ExchangeState::Queued,
                    "child execution requires its admitted queued exchange"
                );
                interaction.state = ExchangeState::Running;
                interaction.resume(now_ms)?;
                ChildExchangeRuntime {
                    synthetic_turn: None,
                    task: TaskTracker::default(),
                    exchange: interaction,
                }
            }
        };
        let mut canonical_event = backend_event.clone();
        admit_addressless_turn(
            &mut runtime.exchange,
            &mut runtime.synthetic_turn,
            &mut canonical_event,
            now_ms,
        )?;
        if !runtime.exchange.observe_turn(&canonical_event, now_ms)? {
            self.child_exchange_runtime_by_agent.insert(run_id, runtime);
            return Ok(true);
        }
        if let Some(update) = canonical_event.task_update.as_ref() {
            runtime.task.replace(update);
            runtime.exchange.task = Some(runtime.task.snapshot().clone());
        }
        if canonical_event.address.is_some() {
            attribute_provider_tool(&mut runtime.exchange, &mut runtime.task, &canonical_event)?;
        }
        self.store.save_exchange(&runtime.exchange)?;
        self.emit_live(
            BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "agent_timeline_updated".into(),
                text: None,
                data: json!({
                    "run_id": run_id,
                    "interaction": runtime.exchange,
                    "active": null,
                }),
                activity: None,
                summary: None,
                task_update: None,
            },
            event,
        )
        .await?;
        self.child_exchange_runtime_by_agent.insert(run_id, runtime);
        Ok(true)
    }

    async fn apply_agent_lifecycle(
        &mut self,
        backend_event: &BackendEvent,
        parent_exchange_id: Option<&str>,
        event: &mut Vec<SessionEvent>,
    ) -> Result<Option<Agent>> {
        let lifecycle: AgentLifecycleEvent = serde_json::from_value(backend_event.data.clone())?;
        let spawn_operation = lifecycle.operation.eq_ignore_ascii_case("spawnAgent");
        let activity_operation = lifecycle.operation.eq_ignore_ascii_case("agentActivity")
            || lifecycle.operation.eq_ignore_ascii_case("subAgentActivity");
        let existing_id = lifecycle
            .provider_thread_id
            .as_deref()
            .and_then(|thread_id| self.agent_registry.get_by_thread(thread_id))
            .map(|run| run.id.clone())
            .or_else(|| {
                if !spawn_operation && !activity_operation {
                    return None;
                }
                self.agent_registry
                    .resolve_unbound(
                        parent_exchange_id,
                        lifecycle.parent_thread_id.as_deref(),
                        lifecycle.turn_id.as_deref(),
                    )
                    .map(|run| run.id.clone())
            });
        let can_create =
            (spawn_operation || activity_operation) && lifecycle.provider_thread_id.is_some();
        if existing_id.is_none() && !can_create {
            return Ok(None);
        }
        let mut run = existing_id
            .as_deref()
            .and_then(|run_id| self.agent_registry.get(run_id))
            .cloned()
            .unwrap_or_else(|| {
                Agent::pending(
                    &self.session.id,
                    lifecycle.definition.as_deref().unwrap_or("default"),
                    lifecycle.task.as_deref().unwrap_or_default(),
                    self.clock.now_ms(),
                )
            });
        let parent_thread_id = lifecycle.parent_thread_id.clone();
        let task = lifecycle.task.clone().filter(|task| !task.is_empty());
        run.provider_thread_id = lifecycle.provider_thread_id.or(run.provider_thread_id);
        if let Some(definition) = lifecycle.definition {
            run.definition = definition;
        }
        run.nickname = lifecycle.nickname.or(run.nickname);
        run.state = match lifecycle.status {
            AgentExecutionState::Starting => AgentState::Starting,
            AgentExecutionState::Closed => AgentState::Closed,
            _ => AgentState::Ready,
        };
        run.updated_at_ms = self.clock.now_ms();
        self.store.save_agent_run(&run)?;
        self.agent_registry.insert(run.clone());
        let execution = self.agent_registry.execution_mut(&run.id);
        if execution.parent_exchange_id.is_none() {
            execution.parent_exchange_id = parent_exchange_id.map(str::to_owned);
        }
        if execution.parent_thread_id.is_none() {
            execution.parent_thread_id = parent_thread_id;
        }
        if let Some(task) = task {
            execution.task = task;
        }
        let queued_outcome = match lifecycle.status {
            AgentExecutionState::Completed => Some(ExchangeState::Complete),
            AgentExecutionState::Failed => Some(ExchangeState::Failed),
            AgentExecutionState::Interrupted | AgentExecutionState::Closed => {
                Some(ExchangeState::Interrupted)
            }
            _ => None,
        };
        if let Some(outcome) = queued_outcome {
            for mut exchange in self.store.list_agent_exchange(&run.id)? {
                if exchange.state == ExchangeState::Queued {
                    exchange.finish(outcome, self.clock.now_ms())?;
                    self.store.save_exchange(&exchange)?;
                }
            }
        }
        self.emit_live(
            BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "agent_updated".into(),
                text: None,
                data: serde_json::to_value(self.snapshot()?.agent)?,
                activity: None,
                summary: None,
                task_update: None,
            },
            event,
        )
        .await?;
        Ok(Some(run))
    }

    fn record_prompt_history(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let text = required_text(&params, "text")?;
        self.store
            .record_prompt_history(&text, self.clock.now_ms())?;
        Ok((
            serde_json::to_value(self.store.list_prompt_history()?)?,
            Vec::new(),
        ))
    }

    async fn list_backend_model(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut model_list = self
            .backend
            .model_list(self.backend_request(BackendInput::from_text(""), PromptMode::Chat))
            .await?;
        let preference = self
            .store
            .load_preference(&self.session.workspace, &self.session.backend)?;
        for model in &mut model_list {
            let setting = preference
                .as_ref()
                .and_then(|preference| preference.model_setting.get(&model.id));
            model.selected_reasoning = setting
                .and_then(|setting| setting.reasoning.clone())
                .or_else(|| model.default_reasoning.clone());
            model.selected_context_window = setting
                .and_then(|setting| setting.context_window.clone())
                .or_else(|| model.default_context_window.clone());
        }
        if self.session.model == "default"
            && let Some(model) = model_list
                .iter()
                .find(|model| model.is_default)
                .or_else(|| model_list.first())
        {
            self.session.resolved_model = Some(model.id.clone());
            self.save_session()?;
            return Ok((
                serde_json::to_value(model_list)?,
                vec![self.event("session_configured", serde_json::to_value(&self.session)?)?],
            ));
        }
        Ok((serde_json::to_value(model_list)?, Vec::new()))
    }

    async fn submit_prompt(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let snapshot = PromptControlSnapshot {
            session: self.session.clone(),
            goal: self.current_goal()?,
            execution: self.store.list_plan_execution(&self.session.id)?,
            plan: self.store.list_plan(&self.session.id)?,
        };
        self.composer_admission = params
            .get("_composer_admission")
            .map(|value| -> Result<_> {
                Ok((
                    serde_json::from_value(
                        value
                            .get("document")
                            .cloned()
                            .context("composer admission document missing")?,
                    )?,
                    value
                        .get("token")
                        .and_then(Value::as_u64)
                        .context("composer admission token missing")?,
                ))
            })
            .transpose()?;
        let submitted_composer = self.composer_admission.clone();
        let mut result = self.submit_prompt_inner(params).await;
        let mut composer_event = Vec::new();
        self.settle_composer_admission(result.is_ok(), &mut composer_event)
            .await?;
        if let Ok((_, event)) = &mut result {
            event.append(&mut composer_event);
        }
        if result
            .as_ref()
            .is_err_and(|error| error.downcast_ref::<TurnRetracted>().is_some())
        {
            self.restore_retracted_control_state(snapshot).await?;
            if let Some((document, token)) = submitted_composer {
                let patch = self
                    .presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .retract_submission(&document, token)?;
                if let Some(patch) = patch {
                    self.emit_backend_event(
                        BackendEvent {
                            address: None,
                            turn_boundary: None,
                            kind: "composer_patch".into(),
                            text: None,
                            data: serde_json::to_value(patch)?,
                            activity: None,
                            summary: None,
                            task_update: None,
                        },
                        &mut composer_event,
                    )
                    .await?;
                }
            }
        }
        result
    }

    async fn settle_composer_admission(
        &mut self,
        admitted: bool,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let Some((document, token)) = self.composer_admission.take() else {
            return Ok(());
        };
        let patch = self
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .complete_submission(&document, token, admitted)?;
        if let Some(patch) = patch {
            self.emit_backend_event(
                BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "composer_patch".into(),
                    text: None,
                    data: serde_json::to_value(patch)?,
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                event,
            )
            .await?;
        }
        Ok(())
    }

    async fn submit_prompt_inner(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let text = required_text(&params, "text")?;
        if text == "/read" {
            return self.select_execution_mode(json!({ "mode": "read" }));
        }
        if text == "/write" {
            return self.select_execution_mode(json!({ "mode": "write" }));
        }
        if text == "/full" {
            return self.select_execution_mode(json!({ "mode": "full" }));
        }
        if text == "/yolo" {
            return self.select_execution_mode(json!({ "mode": "yolo" }));
        }
        if text == "/clear" {
            return self.new_session(Value::Null).await;
        }
        if text == "/plan cancel" {
            return self.cancel_plan();
        }
        if text == "/plan retry" {
            return self.retry_plan().await;
        }
        if let Some(selection) = text.strip_prefix("/replan ") {
            return self.replan(selection).await;
        }
        let failed_plan_active = self
            .session
            .active_plan_id
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| plan.state == PlanState::Failed);
        anyhow::ensure!(
            !failed_plan_active,
            "plan generation stopped; run /plan retry or /plan cancel"
        );
        anyhow::ensure!(text != "/plan", "usage: /plan <prompt>");
        anyhow::ensure!(text != "/replan", "select a plan and revision with the /replan picker");
        let explicit_plan_request = text.strip_prefix("/plan ");
        let active_plan_controls = self
            .session
            .active_plan_id
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| {
                matches!(
                    plan.state,
                    PlanState::Generating
                        | PlanState::Revising
                        | PlanState::AwaitingInput
                        | PlanState::AwaitingReview
                )
            });
        let plan_request = explicit_plan_request.or_else(|| {
            (self.session.mode == HarnessMode::Plan
                && !active_plan_controls
                && !text.starts_with('/'))
            .then_some(text.as_str())
        });
        if let Some(request) = plan_request {
            anyhow::ensure!(!request.trim().is_empty(), "usage: /plan <prompt>");
            let plan_id = Uuid::new_v4().to_string();
            let document = PlanDocument {
                schema_version: crate::plan::PLAN_SCHEMA_VERSION,
                version: 1,
                plan_id: plan_id.clone(),
                title: crate::plan::PROVISIONAL_PLAN_TITLE.into(),
                prompt: request.to_owned(),
                overview: "Planning in progress.".into(),
                usage: None,
                entity_changes: Vec::new(),
                dependencies: Vec::new(),
                flows: Vec::new(),
                tasks: Vec::new(),
                assumptions: Vec::new(),
            };
            return self.start_plan(request.to_owned(), text.clone(), document).await;
        }
        match text.as_str() {
            "/goal pause" => return self.pause_goal().await,
            "/goal resume" => return self.resume_goal().await,
            "/goal clear" => return self.clear_goal().await,
            _ => {}
        }
        if let Some(objective) = text.strip_prefix("/goal ") {
            return self.set_goal(json!({ "objective": objective })).await;
        }
        if let Some(plan) = self
            .session
            .active_plan_id
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
            .filter(|plan| plan.state == PlanState::AwaitingInput)
        {
            let question_id = plan
                .elicitation
                .as_ref()
                .map(|elicitation| {
                    elicitation
                        .current_question()
                        .map(|question| question.id.clone())
                        .unwrap_or_else(|| elicitation.question_set.id.clone())
                })
                .context("active plan has no elicitation state")?;
            return self
                .ask_plan_question(json!({ "question_id": question_id, "text": text }))
                .await;
        }
        if let Some(interaction) = self.find_active_interaction_elicitation()? {
            let question_id = interaction
                .elicitation
                .as_ref()
                .and_then(|elicitation| {
                    elicitation
                        .current_question()
                        .map(|question| question.id.clone())
                        .or_else(|| Some(elicitation.question_set.id.clone()))
                })
                .context("active interaction has no elicitation state")?;
            return self
                .ask_question(json!({ "question_id": question_id, "text": text }))
                .await;
        }
        let active_plan = self
            .session
            .active_plan_id
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten();
        let prompt = match active_plan.as_ref() {
            Some(plan) if !plan.working_path.is_empty() => {
                let document = self
                    .plan_file
                    .read_working_document(&self.session.id, &plan.id)?;
                PlanPrompt::with_active_document(text.clone(), &document.model_json()?)
            }
            _ => text.clone(),
        };
        let input = match BackendInput::parse(&text) {
            BackendInput::Skill {
                name,
                mut arguments,
            } => {
                if let Some(context) = prompt
                    .strip_suffix(&text)
                    .filter(|context| !context.is_empty())
                {
                    arguments = format!("{context}{arguments}");
                }
                BackendInput::Skill { name, arguments }
            }
            BackendInput::Text { .. } => BackendInput::from_text(prompt.clone()),
        };
        let prompt_mode = if self.session.mode == HarnessMode::Plan
            && active_plan.as_ref().is_some_and(|plan| {
                matches!(plan.state, PlanState::Generating | PlanState::Revising)
            }) {
            PromptMode::Plan
        } else {
            PromptMode::Chat
        };
        self.run_interaction_input(
            prompt,
            prompt_mode,
            Some(ExchangeAdmission::chat(text)),
            input,
        )
        .await
    }

    async fn restore_retracted_control_state(
        &mut self,
        snapshot: PromptControlSnapshot,
    ) -> Result<()> {
        let current_goal = self.current_goal()?;
        let goal_changed = match (&current_goal, &snapshot.goal) {
            (Some(current), Some(restored)) => {
                current.id != restored.id
                    || current.objective != restored.objective
                    || current.state != restored.state
            }
            (None, None) => false,
            _ => true,
        };
        let current_goal_id = self.session.goal_id.clone();
        let restored_goal_id = snapshot.goal.as_ref().map(|goal| goal.id.as_str());
        if current_goal_id.as_deref() != restored_goal_id
            && let Some(goal_id) = current_goal_id.as_deref()
        {
            self.store.delete_goal(goal_id)?;
        }
        if let Some(goal) = snapshot.goal.as_ref() {
            self.store.save_goal(goal)?;
        }
        for execution in &snapshot.execution {
            self.store.save_plan_execution(execution)?;
        }
        let restored_plan_id = snapshot
            .plan
            .iter()
            .map(|plan| plan.id.as_str())
            .collect::<HashSet<_>>();
        for plan in self.store.list_plan(&self.session.id)? {
            if !restored_plan_id.contains(plan.id.as_str()) {
                self.store.delete_plan(&plan.id)?;
                self.plan_file.delete_plan(&self.session.id, &plan.id)?;
            }
        }
        for plan in &snapshot.plan {
            self.store.save_plan(plan)?;
        }
        self.session = snapshot.session;
        self.save_session()?;

        if goal_changed && self.capability.native_goal && self.session.backend_session_id.is_some()
        {
            let request = self.backend_request(BackendInput::from_text(""), PromptMode::Chat);
            match snapshot.goal {
                Some(goal) => {
                    let status = goal_state_name(goal.state);
                    self.backend
                        .goal_status(request, Some(goal.objective), status)
                        .await?;
                }
                None => self.backend.goal_status(request, None, "cleared").await?,
            }
        }
        Ok(())
    }

    fn answer_plan_question_choice(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let mut plan = self.active_elicitation_plan()?;
        let question_id = required_text(&params, "question_id")?;
        let response: PlanQuestionResponse = serde_json::from_value(
            params
                .get("response")
                .cloned()
                .context("response is required")?,
        )?;
        plan.elicitation
            .as_mut()
            .context("active plan has no elicitation state")?
            .answer(&question_id, response)?;
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        Ok((
            serde_json::to_value(&plan)?,
            vec![self.event("plan_question_updated", json!({ "plan": plan }))?],
        ))
    }

    fn answer_plan_acceptance_choice(
        &mut self,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let mut plan = self.active_plan_acceptance()?;
        let question_id = required_text(&params, "question_id")?;
        let response: PlanQuestionResponse = serde_json::from_value(
            params
                .get("response")
                .cloned()
                .context("response is required")?,
        )?;
        plan.acceptance
            .as_mut()
            .context("active plan has no acceptance state")?
            .elicitation
            .answer(&question_id, response)?;
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        let snapshot = self.snapshot()?;
        Ok((
            serde_json::to_value(&snapshot)?,
            vec![self.event(
                "plan_acceptance_updated",
                serde_json::to_value(&snapshot.active_elicitation)?,
            )?],
        ))
    }

    fn answer_question(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        if self.active_plan_has_acceptance()? {
            return self.answer_plan_acceptance_choice(params);
        }
        if self.active_plan_awaits_input()? {
            let (_, event) = self.answer_plan_question_choice(params)?;
            return Ok((serde_json::to_value(self.snapshot()?)?, event));
        }
        let question_id = required_text(&params, "question_id")?;
        let response: PlanQuestionResponse = serde_json::from_value(
            params
                .get("response")
                .cloned()
                .context("response is required")?,
        )?;
        let mut interaction = self.active_interaction_elicitation()?;
        interaction
            .elicitation
            .as_mut()
            .context("active interaction has no elicitation state")?
            .answer(&question_id, response)?;
        self.store.save_exchange(&interaction)?;
        let snapshot = self.snapshot()?;
        Ok((
            serde_json::to_value(&snapshot)?,
            vec![self.event(
                "question_updated",
                serde_json::to_value(&snapshot.active_elicitation)?,
            )?],
        ))
    }

    /// Record one explicit conversational answer against the durable elicitation owner.
    fn answer_active_elicitation_from_model(
        &mut self,
        answer: PlanQuestionAnswer,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let has_pending_question;
        if self.active_plan_awaits_input()? {
            let mut plan = self.active_elicitation_plan()?;
            let elicitation = plan
                .elicitation
                .as_mut()
                .context("active plan has no elicitation state")?;
            elicitation.answer_from_model(&answer.question_id, answer.response)?;
            has_pending_question = elicitation.current_question().is_some();
            plan.updated_at_ms = self.clock.now_ms();
            self.store.save_plan(&plan)?;
        } else {
            let mut interaction = self.active_interaction_elicitation()?;
            let elicitation = interaction
                .elicitation
                .as_mut()
                .context("active interaction has no elicitation state")?;
            elicitation.answer_from_model(&answer.question_id, answer.response)?;
            has_pending_question = elicitation.current_question().is_some();
            self.store.save_exchange(&interaction)?;
        }
        if has_pending_question {
            event.push(self.event(
                "question_updated",
                serde_json::to_value(self.snapshot()?.active_elicitation)?,
            )?);
        }
        Ok(())
    }

    /// Remove the durable elicitation only when the provider reports that no decision remains.
    fn withdraw_active_elicitation(
        &mut self,
        withdrawal: PlanQuestionWithdrawal,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let reason = withdrawal.reason.trim();
        anyhow::ensure!(!reason.is_empty(), "question withdrawal reason is required");
        if self.active_plan_awaits_input()? {
            let mut plan = self.active_elicitation_plan()?;
            let elicitation = plan
                .elicitation
                .take()
                .context("active plan has no elicitation state")?;
            let now_ms = self.clock.now_ms();
            for question in &elicitation.question_set.questions {
                plan.question_ledger.resolve(question, None, now_ms);
            }
            PlanStateMachine::apply(&mut plan, PlanEvent::FeedbackConsumed, now_ms)?;
            plan.generation.reset_no_progress();
            self.store.save_plan(&plan)?;
            let lifecycle = PlanLifecycleRecord {
                title: plan.title.clone(),
                anchor: Some(self.plan_exchange_anchor(&plan.id)?),
                id: Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                plan_id: plan.id.clone(),
                kind: PlanLifecycleKind::QuestionWithdrawn,
                model_revision: plan.model_revision,
                user_revision: plan.user_revision,
                overall_comment: None,
                annotation: Vec::new(),
                question: Some(elicitation.question_set),
                answer: Some(reason.to_owned()),
                created_at_ms: self.clock.now_ms(),
            };
            self.store.save_plan_lifecycle(&lifecycle)?;
            event.push(self.event(
                "plan_question_withdrawn",
                json!({ "plan": plan, "lifecycle": lifecycle }),
            )?);
            return Ok(());
        }
        let mut interaction = self.active_interaction_elicitation()?;
        interaction.awaiting_input = false;
        interaction.elicitation = None;
        self.store.save_exchange(&interaction)?;
        event.push(self.event(
            "question_withdrawn",
            json!({ "exchange_id": interaction.id, "reason": reason }),
        )?);
        Ok(())
    }

    /// Replace the pending owner question set when a clarification turn asks again.
    fn replace_active_elicitation(
        &mut self,
        question: PlanQuestionSet,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        if self.active_plan_awaits_input()? {
            let mut plan = self.active_elicitation_plan()?;
            let Some(question) = plan.question_ledger.unresolved(question) else {
                self.record_plan_trace(
                    "resolved_plan_question_suppressed",
                    plan_trace_fields(&plan),
                );
                return Ok(true);
            };
            plan.elicitation
                .as_mut()
                .context("active plan has no elicitation state")?
                .replace_question_set(question.clone());
            plan.updated_at_ms = self.clock.now_ms();
            self.store.save_plan(&plan)?;
            let lifecycle = PlanLifecycleRecord {
                title: plan.title.clone(),
                anchor: Some(self.plan_exchange_anchor(&plan.id)?),
                id: Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                plan_id: plan.id.clone(),
                kind: PlanLifecycleKind::QuestionAsked,
                model_revision: plan.model_revision,
                user_revision: plan.user_revision,
                overall_comment: None,
                annotation: Vec::new(),
                question: Some(question.clone()),
                answer: None,
                created_at_ms: self.clock.now_ms(),
            };
            self.store.save_plan_lifecycle(&lifecycle)?;
            event.push(self.event(
                "plan_question",
                json!({ "plan": plan, "lifecycle": lifecycle, "question": question }),
            )?);
            return Ok(true);
        }
        let Some(mut owner) = self.find_active_interaction_elicitation()? else {
            return Ok(false);
        };
        owner
            .elicitation
            .as_mut()
            .context("active interaction has no elicitation state")?
            .replace_question_set(question);
        self.store.save_exchange(&owner)?;
        event.push(self.event(
            "question",
            json!({
                "owner": "interaction",
                "plan_id": owner.plan_id,
                "exchange_id": owner.id,
                "elicitation": owner.elicitation,
            }),
        )?);
        Ok(true)
    }

    fn skip_question(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        self.answer_question(json!({
            "question_id": required_text(&params, "question_id")?,
            "response": { "kind": "skipped" }
        }))
    }

    fn active_plan_awaits_input(&self) -> Result<bool> {
        Ok(self
            .session
            .active_plan_id
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| {
                plan.state == PlanState::AwaitingInput && plan.elicitation.is_some()
            }))
    }

    fn active_plan_has_acceptance(&self) -> Result<bool> {
        Ok(self
            .session
            .active_plan_id
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| {
                plan.state == PlanState::AwaitingReview && plan.acceptance.is_some()
            }))
    }

    fn active_plan_acceptance(&self) -> Result<PlanRecord> {
        let plan_id = self
            .session
            .active_plan_id
            .as_deref()
            .context("no active plan")?;
        let plan = self
            .store
            .load_plan(plan_id)?
            .context("active plan record is missing")?;
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview && plan.acceptance.is_some(),
            "active plan has no pending acceptance"
        );
        Ok(plan)
    }

    fn active_interaction_elicitation(&self) -> Result<Exchange> {
        self.find_active_interaction_elicitation()?
            .context("no interaction awaits user input")
    }

    fn find_active_interaction_elicitation(&self) -> Result<Option<Exchange>> {
        Ok(self
            .store
            .list_exchange(&self.session.id)?
            .into_iter()
            .rev()
            .find(|interaction| interaction.awaiting_input && interaction.elicitation.is_some()))
    }

    /// Capture the owning planning exchange for a user review or question action.
    fn plan_exchange_anchor(&self, plan_id: &str) -> Result<crate::plan::ExchangeAnchor> {
        let exchange = self.store.list_exchange(&self.session.id)?.into_iter().rev()
            .find(|exchange| exchange.plan_id.as_deref() == Some(plan_id))
            .context("planning event has no owning exchange")?;
        Ok(crate::plan::ExchangeAnchor::capture(&exchange))
    }

    /// Record planning feedback only on the live exchange that owns the plan.
    fn append_plan_feedback(&mut self, plan_id: &str, text: String, intent: crate::exchange::InputIntent, question: Option<crate::exchange::QuestionInput>) -> Result<()> {
        let mut exchange = self
            .store
            .list_exchange(&self.session.id)?
            .pop()
            .context("planning feedback has no owning exchange")?;
        anyhow::ensure!(
            exchange.plan_id.as_deref() == Some(plan_id)
                && exchange.state == ExchangeState::Running,
            "planning feedback does not target the active exchange"
        );
        if let Some(question) = question {
            exchange.append_question_input(intent, text, question, self.clock.now_ms())?;
        } else {
            exchange.append_input(intent, text, self.clock.now_ms())?;
        }
        self.store.save_exchange(&exchange)
    }

    async fn ask_plan_question(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let mut plan = self.active_elicitation_plan()?;
        let question_id = required_text(&params, "question_id")?;
        let text = required_text(&params, "text")?;
        let elicitation = plan
            .elicitation
            .as_mut()
            .context("active plan has no elicitation state")?;
        anyhow::ensure!(
            elicitation.question_set.id == question_id
                || elicitation.question(&question_id).is_some(),
            "clarification does not target a pending planning question"
        );
        elicitation.begin_clarification(&question_id)?;
        let elicitation_json = serde_json::to_string_pretty(&elicitation)?;
        let target = crate::exchange::QuestionInput {
            set_id: elicitation.question_set.id.clone(),
            question_id: Some(elicitation.question(&question_id).or_else(|| elicitation.current_question())
                .context("clarification has no pending question")?.id.clone()),
            answer: Vec::new(),
        };
        plan.updated_at_ms = self.clock.now_ms();
        self.append_plan_feedback(&plan.id, text.clone(), crate::exchange::InputIntent::Clarification, Some(target))?;
        self.store.save_plan(&plan)?;
        let (mut value, mut event) = self
            .run_interaction(
                PlanPrompt::clarification(&plan.request, &elicitation_json, &text),
                PromptMode::Chat,
                None,
            )
            .await?;
        let current_plan = self
            .store
            .load_plan(&plan.id)?
            .context("active plan record is missing after clarification")?;
        if current_plan.elicitation.is_none()
            && matches!(
                current_plan.state,
                PlanState::Generating | PlanState::Revising
            )
        {
            let withdrawal = self
                .store
                .list_plan_lifecycle(&self.session.id)?
                .into_iter()
                .rev()
                .find(|lifecycle| {
                    lifecycle.plan_id == plan.id
                        && lifecycle.kind == PlanLifecycleKind::QuestionWithdrawn
                })
                .context("withdrawn planning question has no lifecycle record")?;
            let feedback = format!(
                "Pending planning questions withdrawn: {}",
                withdrawal
                    .answer
                    .as_deref()
                    .unwrap_or("no material decision remains")
            );
            let continuation = self
                .run_planning_interaction(PlanPrompt::feedback(&plan.request, &feedback), None)
                .await;
            match continuation {
                Ok((next_value, mut next_event)) => {
                    value = next_value;
                    event.append(&mut next_event);
                }
                Err(error) => {
                    return Err(error).context("continue planning after question withdrawal");
                }
            }
        } else if current_plan
            .elicitation
            .as_ref()
            .is_some_and(|elicitation| elicitation.current_question().is_none())
        {
            let (next_value, mut next_event) = self.continue_plan_question().await?;
            value = next_value;
            event.append(&mut next_event);
        }
        Ok((value, event))
    }

    async fn ask_question(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            !self.active_plan_has_acceptance()?,
            "plan acceptance questions do not support clarification turns"
        );
        if self.active_plan_awaits_input()? {
            let (_, event) = self.ask_plan_question(params).await?;
            return Ok((serde_json::to_value(self.snapshot()?)?, event));
        }
        let question_id = required_text(&params, "question_id")?;
        let text = required_text(&params, "text")?;
        let mut interaction = self.active_interaction_elicitation()?;
        let elicitation = interaction
            .elicitation
            .as_mut()
            .context("active interaction has no elicitation state")?;
        anyhow::ensure!(
            elicitation.question_set.id == question_id
                || elicitation.question(&question_id).is_some(),
            "clarification does not target a pending Harness question"
        );
        elicitation.begin_clarification(&question_id)?;
        let elicitation_json = serde_json::to_string_pretty(elicitation)?;
        let target = crate::exchange::QuestionInput {
            set_id: elicitation.question_set.id.clone(),
            question_id: Some(elicitation.question(&question_id).or_else(|| elicitation.current_question())
                .context("clarification has no pending question")?.id.clone()),
            answer: Vec::new(),
        };
        interaction.append_question_input(
            crate::exchange::InputIntent::Clarification, text.clone(), target, self.clock.now_ms(),
        )?;
        self.store.save_exchange(&interaction)?;
        let (mut value, mut event) = self
            .run_interaction(
                PlanPrompt::question_follow_up(&elicitation_json, &text),
                PromptMode::Chat,
                None,
            )
            .await?;
        if self
            .find_active_interaction_elicitation()?
            .and_then(|interaction| interaction.elicitation)
            .is_some_and(|elicitation| elicitation.current_question().is_none())
        {
            let (next_value, mut next_event) = self.continue_question().await?;
            value = next_value;
            event.append(&mut next_event);
        }
        Ok((value, event))
    }

    async fn continue_plan_question(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut plan = self.active_elicitation_plan()?;
        self.record_plan_trace("feedback_continuation_started", plan_trace_fields(&plan));
        let elicitation = plan
            .elicitation
            .take()
            .context("active plan has no elicitation state")?;
        let answer = elicitation.feedback();
        let question = elicitation.question_set.clone();
        self.append_plan_feedback(&plan.id, answer.clone(), crate::exchange::InputIntent::Answer, Some(crate::exchange::QuestionInput {
            set_id: question.id.clone(), question_id: None, answer: elicitation.answer.clone(),
        }))?;
        let now_ms = self.clock.now_ms();
        for pending_question in &elicitation.question_set.questions {
            let response = Some(
                elicitation
                    .answer
                    .iter()
                    .find(|item| item.question_id == pending_question.id)
                    .map(|item| item.response.clone())
                    .unwrap_or(PlanQuestionResponse::Skipped),
            );
            plan.question_ledger
                .resolve(pending_question, response, now_ms);
        }
        PlanStateMachine::apply(&mut plan, PlanEvent::FeedbackConsumed, now_ms)?;
        plan.generation.reset_no_progress();
        self.store.save_plan(&plan)?;
        self.record_plan_trace("feedback_consumed", plan_trace_fields(&plan));
        let lifecycle = PlanLifecycleRecord {
            title: plan.title.clone(),
            anchor: Some(self.plan_exchange_anchor(&plan.id)?),
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            plan_id: plan.id.clone(),
            kind: PlanLifecycleKind::QuestionAnswered,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            overall_comment: None,
            annotation: Vec::new(),
            question: Some(question.clone()),
            answer: Some(answer.clone()),
            created_at_ms: self.clock.now_ms(),
        };
        self.store.save_plan_lifecycle(&lifecycle)?;
        let mut leading_event = vec![self.event(
            "plan_question_answered",
            json!({ "plan": plan, "lifecycle": lifecycle }),
        )?];
        let result = self
            .run_planning_interaction(PlanPrompt::feedback(&plan.request, &answer), None)
            .await;
        match result {
            Ok((value, mut event)) => {
                self.record_plan_trace("feedback_continuation_finished", plan_trace_fields(&plan));
                leading_event.append(&mut event);
                Ok((value, leading_event))
            }
            Err(error) => {
                let failed_plan = self
                    .store
                    .load_plan(&plan.id)?
                    .context("active plan record is missing after continuation failure")?;
                self.record_plan_trace(
                    "feedback_continuation_failed",
                    plan_trace_fields(&failed_plan),
                );
                Err(error).context("continue planning after user feedback")
            }
        }
    }

    async fn continue_question(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        if self.active_plan_has_acceptance()? {
            return self.continue_plan_acceptance().await;
        }
        if self.active_plan_awaits_input()? {
            let (_, event) = self.continue_plan_question().await?;
            return Ok((serde_json::to_value(self.snapshot()?)?, event));
        }
        let mut interaction = self.active_interaction_elicitation()?;
        let elicitation = interaction
            .elicitation
            .take()
            .context("active interaction has no elicitation state")?;
        let feedback = elicitation.feedback();
        interaction.awaiting_input = false;
        for node in &mut interaction.node_list {
            if let ExchangeNode::QuestionPresented { question, answer, .. } = node {
                if question.id == elicitation.question_set.id {
                    *answer = Some(feedback.clone());
                }
            }
        }
        interaction.append_question_input(
            crate::exchange::InputIntent::Answer,
            feedback.clone(),
            crate::exchange::QuestionInput {
                set_id: elicitation.question_set.id.clone(), question_id: None, answer: elicitation.answer.clone(),
            },
            self.clock.now_ms(),
        )?;
        self.store.save_exchange(&interaction)?;
        let (value, mut event) = self
            .run_interaction(
                format!(
                    "The user answered the pending Harness questions. Continue the original request using these responses.\n\n{feedback}"
                ),
                PromptMode::Chat,
                None,
            )
            .await?;
        event.insert(
            0,
            self.event(
                "question_answered",
                json!({
                    "exchange_id": interaction.id,
                    "question": elicitation.question_set,
                    "answer": elicitation.answer,
                }),
            )?,
        );
        Ok((value, event))
    }

    fn active_elicitation_plan(&self) -> Result<PlanRecord> {
        let plan_id = self
            .session
            .active_plan_id
            .as_deref()
            .context("no active plan")?;
        let plan = self
            .store
            .load_plan(plan_id)?
            .context("active plan record is missing")?;
        anyhow::ensure!(
            plan.state == PlanState::AwaitingInput && plan.elicitation.is_some(),
            "active plan is not awaiting planning input"
        );
        Ok(plan)
    }

    async fn run_interaction(
        &mut self,
        text: String,
        mode: PromptMode,
        admission: Option<ExchangeAdmission>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let input = BackendInput::from_text(text.clone());
        self.run_interaction_input(text, mode, admission, input)
            .await
    }

    async fn run_planning_interaction(
        &mut self,
        mut prompt: String,
        mut admission: Option<ExchangeAdmission>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let mut accumulated_event = Vec::new();
        loop {
            let plan_id = self
                .session
                .active_plan_id
                .clone()
                .context("planning continuation has no active plan")?;
            let before_document = self
                .plan_file
                .read_working_document(&self.session.id, &plan_id)?;
            let before_digest = plan_digest(&serde_json::to_vec(&before_document)?);
            let result = self
                .run_interaction(prompt, PromptMode::Plan, admission.take())
                .await;
            let (value, mut event) = match result {
                Ok(result) => result,
                Err(error) => {
                    if error.downcast_ref::<TurnCancelled>().is_some()
                        && self.turn_cancellation.restart.load(Ordering::Acquire)
                    {
                        return Err(error);
                    }
                    let mut plan = self
                        .store
                        .load_plan(&plan_id)?
                        .context("planning continuation lost its plan record")?;
                    if matches!(
                        plan.state,
                        PlanState::Generating | PlanState::Revising | PlanState::AwaitingInput
                    ) {
                        PlanStateMachine::apply(
                            &mut plan,
                            PlanEvent::TransitionFailed,
                            self.clock.now_ms(),
                        )?;
                        self.store.save_plan(&plan)?;
                    }
                    return Err(error);
                }
            };
            accumulated_event.append(&mut event);
            let mut plan = self
                .store
                .load_plan(&plan_id)?
                .context("planning continuation lost its plan record")?;
            let after_document = self
                .plan_file
                .read_working_document(&self.session.id, &plan_id)?;
            let canonical_progress =
                plan_digest(&serde_json::to_vec(&after_document)?) != before_digest;
            if canonical_progress {
                plan.generation.canonical_revision =
                    plan.generation.canonical_revision.saturating_add(1);
            }
            let continuation_allowed = plan.generation.observe(canonical_progress);
            self.store.save_plan(&plan)?;
            if matches!(
                plan.state,
                PlanState::AwaitingReview | PlanState::AwaitingInput
            ) {
                return Ok((value, accumulated_event));
            }
            if !matches!(plan.state, PlanState::Generating | PlanState::Revising) {
                return Ok((value, accumulated_event));
            }

            if !continuation_allowed {
                PlanStateMachine::apply(
                    &mut plan,
                    PlanEvent::TransitionFailed,
                    self.clock.now_ms(),
                )?;
                self.store.save_plan(&plan)?;
                self.record_plan_trace("plan_generation_exhausted", plan_trace_fields(&plan));
                accumulated_event.push(self.event(
                    "plan_generation_failed",
                    json!({
                        "plan": plan,
                        "reason": "planning stopped without submitting the canonical plan"
                    }),
                )?);
                return Ok((serde_json::to_value(self.snapshot()?)?, accumulated_event));
            }
            self.record_plan_trace(
                "plan_generation_retry",
                json!({
                    "plan": plan_trace_fields(&plan),
                    "turn": plan.generation.budget.turn_count + 1,
                    "max_turn": plan.generation.budget.max_turn_count,
                    "canonical_progress": canonical_progress,
                }),
            );
            accumulated_event.push(self.event(
                "plan_generation_retry",
                json!({
                    "plan_id": plan.id,
                    "turn": plan.generation.budget.turn_count + 1,
                    "max_turn": plan.generation.budget.max_turn_count,
                    "canonical_progress": canonical_progress,
                }),
            )?);
            prompt = format!(
                "Continue the active planning task. The previous provider turn ended without a \
terminal planning action. Update the canonical PlanDocument if needed, then call \
harness_plan_submit. Ask only a genuinely new unresolved question with \
harness_question_ask. Do not repeat resolved questions or return prose.\n\n\
Planning continuation: turn {} of {}.",
                plan.generation.budget.turn_count + 1,
                plan.generation.budget.max_turn_count
            );
        }
    }

    async fn run_interaction_input(
        &mut self,
        text: String,
        mode: PromptMode,
        admission: Option<ExchangeAdmission>,
        input: BackendInput,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        match self
            .run_interaction_input_inner(text, mode, admission, input)
            .await
        {
            Ok(result) => Ok(result),
            Err(error) if self.exchange_runtime.is_some() => {
                match self.finalize_interaction_after_processing_failure().await {
                    Ok(()) => Err(error),
                    Err(finalization_error) => Err(error.context(format!(
                        "also failed to finalize the interaction: {finalization_error:#}"
                    ))),
                }
            }
            Err(error) => Err(error),
        }
    }

    async fn run_interaction_input_inner(
        &mut self,
        text: String,
        mode: PromptMode,
        admission: Option<ExchangeAdmission>,
        mut input: BackendInput,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            self.store
                .list_exchange(&self.session.id)?
                .iter()
                .all(|exchange| exchange.state != ExchangeState::Finalizing),
            "an exchange is still finalizing; retry cleanup before starting more work"
        );
        let now_ms = self.clock.now_ms();
        let admitted_prompt = admission
            .as_ref()
            .map(|value| value.prompt.as_str())
            .unwrap_or(&text);
        let (mut interaction, new_interaction) = self
            .interaction_for_turn(admitted_prompt, admission.is_some(), now_ms)
            .await?;
        interaction.mode = Some(self.session.mode);
        if new_interaction && let Some(admission) = admission.as_ref() {
            interaction.kind = admission.kind;
            interaction.plan_id.clone_from(&admission.plan_id);
            interaction.execution_id.clone_from(&admission.execution_id);
            interaction.goal_id.clone_from(&admission.goal_id);
            if let Some(run_id) = admission.agent_run_id.as_deref()
                && let Some(run) = self.agent_registry.get(run_id).cloned()
            {
                self.agent_registry.execution_mut(run_id).parent_exchange_id =
                    Some(interaction.id.clone());
                self.store.save_agent_run(&run)?;
                self.agent_registry.insert(run);
            }
            if interaction.task.is_none()
                && let Some(execution_id) = interaction.execution_id.as_deref()
            {
                interaction.task = self
                    .store
                    .list_exchange(&self.session.id)?
                    .into_iter()
                    .rev()
                    .find(|previous| previous.execution_id.as_deref() == Some(execution_id))
                    .and_then(|previous| previous.task);
            }
        }
        let mut event = Vec::new();
        if new_interaction {
            self.start_exchange_runtime(&mut interaction, now_ms)
                .await?;
        } else if self
            .exchange_runtime
            .as_ref()
            .is_none_or(|runtime| runtime.exchange_id != interaction.id)
        {
            self.resume_exchange_runtime(&interaction)?;
        }
        if new_interaction {
            if let Some(event_id) = admission.as_ref().and_then(|admission| admission.plan_event_id.as_deref()) {
                let mut lifecycle = self.store.list_plan_lifecycle(&self.session.id)?.into_iter()
                    .find(|record| record.id == event_id).context("admitted planning event is missing")?;
                lifecycle.anchor = Some(crate::plan::ExchangeAnchor::capture(&interaction));
                self.store.save_plan_lifecycle(&lifecycle)?;
            }
            if let Some(execution_id) = interaction.execution_id.as_deref() {
                if let Some(mut execution) = self.store.load_plan_execution(execution_id)? {
                    for record in &mut execution.lifecycle {
                        if admission.as_ref().is_some_and(|admission| admission.plan_event_id.is_some()) {
                            record.anchor = Some(crate::plan::ExchangeAnchor::capture(&interaction));
                        }
                    }
                    self.store.save_plan_execution(&execution)?;
                }
            }
        }
        interaction.resume(self.clock.now_ms())?;
        self.store.save_exchange(&interaction)?;
        self.settle_composer_admission(true, &mut event).await?;
        self.emit_live_interaction(
            BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "timeline_exchange_started".into(),
                text: None,
                data: serde_json::to_value(&interaction)?,
                activity: None,
                summary: None,
                task_update: None,
            },
            &interaction,
            &mut event,
        )
        .await?;

        if mode == PromptMode::Plan
            || admission.as_ref().is_some_and(|admission| admission.plan_event_id.is_some())
        {
            let patch = self.reconcile_timeline()?;
            self.emit_timeline_patch(patch, &mut event).await?;
        }

        let goal = self.session.goal_id.as_deref()
            .map(|id| self.store.load_goal(id)).transpose()?.flatten();
        let goal_execution = self.store.list_plan_execution(&self.session.id)?
            .into_iter().rev().find(|execution| goal.as_ref().is_some_and(|goal| goal.id == execution.goal_id));
        self.emit_backend_event(BackendEvent {
            address: None, turn_boundary: None, kind: "execution_state".into(), text: None,
            data: json!({"session":self.session,"goal":goal,"goal_execution":goal_execution}),
            activity: None, summary: None, task_update: None,
        }, &mut event).await?;

        if mode == PromptMode::Plan && !input.text().contains("Active canonical PlanDocument:") {
            let plan_id = self
                .session
                .active_plan_id
                .as_deref()
                .context("planning turn has no active plan")?;
            let document = self
                .plan_file
                .read_working_document(&self.session.id, plan_id)?;
            let context = PlanPrompt::with_active_document(String::new(), &document.model_json()?);
            input = match input {
                BackendInput::Text { text } => BackendInput::from_text(format!("{context}{text}")),
                BackendInput::Skill { name, arguments } => BackendInput::Skill {
                    name,
                    arguments: format!("{context}{arguments}"),
                },
            };
        }

        let mut backend_request = self.backend_request(input, mode);
        let policy = self
            .permission_store
            .read()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .compiled()
            .clone();
        let (repository_scope, repository_lease) =
            crate::control_tools::repository::RepositoryToolScope::open(
                self.session.id.clone(),
                interaction.id.clone(),
                PathBuf::from(&self.session.workspace),
                policy,
                self.session.execution_mode,
                Arc::clone(&self.turn_cancellation.requested),
                Arc::clone(&self.repositories),
                Arc::clone(&self.diff),
            )?;
        if let Some(context) = &mut backend_request.control_context {
            context.repository = Some(repository_scope);
        }
        self.trace.record(
            &self.session.id,
            "model.requested",
            serde_json::to_value(&backend_request)?,
        );
        let output = self
            .prompt_with_timeline(backend_request, &mut interaction, &mut event)
            .await;
        drop(repository_lease);
        let mut output = match output {
            Ok(output) => output,
            Err(error) => {
                self.trace.record(
                    &self.session.id,
                    "model.failed",
                    json!({ "error": format!("{error:#}") }),
                );
                let cancelled = error.downcast_ref::<TurnCancelled>().is_some();
                if cancelled
                    && let Some(backend_session_id) =
                        self.backend.active_session_id_for(&self.session.id).await
                {
                    self.session.backend_session_id = Some(backend_session_id);
                }
                if cancelled && self.turn_cancellation.restart.load(Ordering::Acquire) {
                    interaction.pause_for_restart(self.clock.now_ms())?;
                    self.store.save_exchange(&interaction)?;
                    self.exchange_runtime = None;
                    self.active_wait_projection = None;
                    self.save_session()?;
                    return Err(error);
                }
                let outcome = if cancelled {
                    ExchangeState::Cancelled
                } else {
                    ExchangeState::Failed
                };
                let final_checkpoint_id = self
                    .capture_final_checkpoint(&mut interaction, outcome)
                    .await?;
                let workspace_unchanged = interaction.checkpoint_before.is_some()
                    && interaction.checkpoint_before == final_checkpoint_id;
                let retraction_eligible = cancelled
                    && self.turn_cancellation.restores_prompt()
                    && self.capability.native_turn_rollback
                    && workspace_unchanged
                    && self
                        .exchange_runtime
                        .as_ref()
                        .is_some_and(|runtime| runtime.retraction_eligible);
                if retraction_eligible {
                    match self
                        .backend
                        .rollback_cancelled_turn_for(&self.session.id)
                        .await
                    {
                        Ok(true) => {
                            self.exchange_runtime.take();
                            self.store.delete_exchange(&interaction.id)?;
                            self.emit_live(
                                BackendEvent {
                                    address: None,
                                    turn_boundary: None,
                                    kind: "timeline_exchange_retracted".into(),
                                    text: None,
                                    data: json!({
                                        "exchange_id": interaction.id,
                                        "prompt": interaction.prompt.clone(),
                                    }),
                                    activity: None,
                                    summary: None,
                                    task_update: None,
                                },
                                &mut event,
                            )
                            .await?;
                            return Err(anyhow::Error::new(TurnRetracted {
                                prompt: interaction.prompt,
                            }));
                        }
                        Ok(false) => {}
                        Err(rollback_error) => {
                            self.emit_live(
                                BackendEvent {
                                    address: None,
                                    turn_boundary: None,
                                    kind: "error".into(),
                                    text: Some(format!(
                                        "Cancelled turn could not be retracted: {rollback_error:#}"
                                    )),
                                    data: Value::Null,
                                    activity: None,
                                    summary: None,
                                    task_update: None,
                                },
                                &mut event,
                            )
                            .await?;
                        }
                    }
                }
                if let Some(runtime) = self.exchange_runtime.take() {
                    finish_synthetic_turn(
                        &mut interaction,
                        runtime.synthetic_turn.as_ref(),
                        if cancelled {
                            crate::turn::TurnOutcome::Cancelled
                        } else {
                            crate::turn::TurnOutcome::Failed
                        },
                        self.clock.now_ms(),
                    )?;
                }
                let outcome = if cancelled {
                    ExchangeState::Cancelled
                } else {
                    ExchangeState::Failed
                };
                interaction.finish(outcome, self.clock.now_ms())?;
                self.store.save_exchange(&interaction)?;
                self.pause_goal_after_turn_failure().await?;
                self.save_session()?;
                if cancelled {
                    self.emit_live_interaction(
                        BackendEvent {
                            address: None,
                            turn_boundary: None,
                            kind: "timeline_exchange_cancelled".into(),
                            text: None,
                            data: serde_json::to_value(&interaction)?,
                            activity: None,
                            summary: None,
                            task_update: None,
                        },
                        &interaction,
                        &mut event,
                    )
                    .await?;
                    return Err(error);
                }
                return Err(error).context("backend turn failed");
            }
        };
        self.trace.record(
            &self.session.id,
            "model.completed",
            serde_json::to_value(&output)?,
        );
        let token_count = output.metrics.token_count;
        interaction.pause(self.clock.now_ms());
        interaction.token_count = token_count.or(interaction.token_count);
        interaction.record_latest_turn_usage(token_count);
        self.capability = output.capability.clone();
        self.session.native_fork = output.capability.native_fork;
        self.session.native_compact = output.capability.native_compact;
        if let Some(context_usage) = output.metrics.context_usage.clone() {
            self.session.context_usage = Some(context_usage);
        }
        self.session.backend_session_id = output.backend_session_id.clone();
        self.session.provider_checkpoint_id = output.provider_checkpoint_id.clone();
        if !output.runtime.provider.is_empty() {
            self.session.provider_label = output.runtime.provider.clone();
        }
        self.session.resolved_model = output.runtime.model.clone();
        output.event.clear();

        if let Some(control_error) = output.control_error.take() {
            anyhow::bail!(control_error);
        }

        let elicitation_control_count = usize::from(output.plan_question.is_some())
            + usize::from(output.question_answer.is_some())
            + usize::from(output.question_withdrawal.is_some());
        let refresh_elicitation =
            interaction.elicitation.is_some() && elicitation_control_count > 0;
        anyhow::ensure!(
            elicitation_control_count <= 1,
            "backend turn emitted multiple Harness question control actions"
        );
        if mode == PromptMode::Plan {
            anyhow::ensure!(
                output.question_answer.is_none() && output.question_withdrawal.is_none(),
                "planning generation cannot answer or withdraw a pending question"
            );
        }

        if mode == PromptMode::Plan {
            let mut plan = self
                .session
                .active_plan_id
                .as_deref()
                .map(|plan_id| self.store.load_plan(plan_id))
                .transpose()?
                .flatten()
                .filter(|plan| {
                    matches!(
                        plan.state,
                        PlanState::Generating | PlanState::Revising | PlanState::AwaitingInput
                    )
                })
                .context("planning turn has no active canonical plan")?;
            self.record_plan_trace(
                "plan_output_received",
                json!({
                    "plan": plan_trace_fields(&plan),
                    "edit_count": output.plan_edit.len(),
                    "read": output.plan_read.is_some(),
                    "submitted": output.plan_submit.is_some(),
                    "questioned": output.plan_question.is_some(),
                }),
            );

            for edit_request in std::mem::take(&mut output.plan_edit) {
                anyhow::ensure!(
                    edit_request.plan_id == plan.id,
                    "edited plan id does not match active plan"
                );
                let result = self
                    .plan_file
                    .edit_working_document(&self.session.id, edit_request)?;
                plan.document_version = result.version;
                plan.title = result.document.title;
                plan.updated_at_ms = self.clock.now_ms();
            }
            if let Some(read_plan_id) = output.plan_read.take() {
                anyhow::ensure!(read_plan_id == plan.id, "requested plan is not active");
            }

            if let Some(submission) = output.plan_submit.take() {
                anyhow::ensure!(
                    submission.plan_id == plan.id,
                    "submitted plan id does not match active plan"
                );
                let mut validated_document = self
                    .plan_file
                    .read_working_document(&self.session.id, &plan.id)?;
                anyhow::ensure!(
                    validated_document.version == submission.expected_version,
                    "plan version changed before Rust API validation"
                );
                let validation_warning =
                    match validate_plan_rust_api(&self.rustdoc, &mut validated_document).await {
                        Ok(report) => report.warning,
                        Err(error) => error.violation,
                    };
                let previous_markdown = if plan.model_revision == 0 {
                    String::new()
                } else {
                    crate::plan::render_plan_at(
                        &self.plan_file.read_submitted_document(
                            &self.session.id,
                            &plan.id,
                            plan.model_revision,
                        )?,
                        Path::new(&self.session.workspace),
                    )?
                    .markdown
                };
                let lifecycle_kind = if plan.model_revision == 0 {
                    PlanLifecycleKind::Created
                } else {
                    PlanLifecycleKind::RevisionCreated
                };
                plan.model_revision += 1;
                let (document, rendered, digest) =
                    self.plan_file.submit_validated_document_revision(
                        &self.session.id,
                        &plan.id,
                        plan.model_revision,
                        submission.expected_version,
                        validated_document,
                    )?;
                plan.document_version = document.version;
                plan.submitted_version = Some(document.version);
                plan.title = document.title.clone();
                plan.review_digest = Some(digest);
                plan.validation_warning = validation_warning;
                PlanStateMachine::apply(&mut plan, PlanEvent::PlanSubmitted, self.clock.now_ms())?;
                self.store.save_plan(&plan)?;
                self.record_plan_trace("plan_submitted", plan_trace_fields(&plan));
                if lifecycle_kind == PlanLifecycleKind::RevisionCreated {
                    let lifecycle = self
                        .store
                        .list_plan_lifecycle(&self.session.id)?
                        .into_iter()
                        .rev()
                        .find(|lifecycle| {
                            lifecycle.plan_id == plan.id
                                && lifecycle.kind == PlanLifecycleKind::ChangesRequested
                                && lifecycle.user_revision == plan.user_revision
                        });
                    if let Some(lifecycle) =
                        lifecycle.filter(|lifecycle| !lifecycle.annotation.is_empty())
                    {
                        interaction
                            .node_list
                            .push(ExchangeNode::PlanCommentResolution {
                                resolution: crate::exchange::PlanCommentResolution {
                                    id: format!(
                                        "{}:resolved-comments:{}",
                                        interaction.id, plan.user_revision
                                    ),
                                    annotation: lifecycle.annotation,
                                    created_at_ms: self.clock.now_ms(),
                                },
                            });
                    }
                }
                interaction.node_list.push(ExchangeNode::ArtifactChange {
                    change: crate::exchange::ArtifactChange {
                        id: format!("{}:artifact:{}", interaction.id, plan.model_revision),
                        path: plan.working_path.clone(),
                        diff_text: crate::plan::render_plan_delta(
                            &plan.working_path,
                            &previous_markdown,
                            &rendered.markdown,
                        ),
                        created_at_ms: self.clock.now_ms(),
                    },
                });
                let lifecycle = PlanLifecycleRecord {
                    title: plan.title.clone(),
                    anchor: Some(crate::plan::ExchangeAnchor::capture(&interaction)),
                    id: Uuid::new_v4().to_string(),
                    session_id: self.session.id.clone(),
                    plan_id: plan.id.clone(),
                    kind: lifecycle_kind,
                    model_revision: plan.model_revision,
                    user_revision: plan.user_revision,
                    overall_comment: None,
                    annotation: Vec::new(),
                    question: None,
                    answer: None,
                    created_at_ms: self.clock.now_ms(),
                };
                self.store.save_plan_lifecycle(&lifecycle)?;
                event.push(self.event(
                    if lifecycle_kind == PlanLifecycleKind::Created {
                        "plan_created"
                    } else {
                        "plan_revision_created"
                    },
                    json!({
                        "plan": plan,
                        "lifecycle": lifecycle,
                        "document": document,
                        "content": rendered.markdown,
                        "navigation": rendered.navigation
                    }),
                )?);
            } else if let Some(question) = output.plan_question.take() {
                let question = question.normalize()?;
                if let Some(question) = plan.question_ledger.unresolved(question) {
                    match plan.elicitation.as_mut() {
                        Some(elicitation) => elicitation.replace_question_set(question.clone()),
                        None => plan.elicitation = Some(PlanElicitation::new(question.clone())),
                    }
                    PlanStateMachine::apply(
                        &mut plan,
                        PlanEvent::QuestionAsked,
                        self.clock.now_ms(),
                    )?;
                    self.store.save_plan(&plan)?;
                    self.record_plan_trace("plan_question_asked", plan_trace_fields(&plan));
                    interaction.awaiting_input = true;
                    let lifecycle = PlanLifecycleRecord {
                        title: plan.title.clone(),
                        anchor: Some(crate::plan::ExchangeAnchor::capture(&interaction)),
                        id: Uuid::new_v4().to_string(),
                        session_id: self.session.id.clone(),
                        plan_id: plan.id.clone(),
                        kind: PlanLifecycleKind::QuestionAsked,
                        model_revision: plan.model_revision,
                        user_revision: plan.user_revision,
                        overall_comment: None,
                        annotation: Vec::new(),
                        question: Some(question.clone()),
                        answer: None,
                        created_at_ms: self.clock.now_ms(),
                    };
                    self.store.save_plan_lifecycle(&lifecycle)?;
                    event.push(self.event(
                        "plan_question",
                        json!({ "plan": plan, "lifecycle": lifecycle, "question": question }),
                    )?);
                } else {
                    plan.updated_at_ms = self.clock.now_ms();
                    self.store.save_plan(&plan)?;
                    self.record_plan_trace(
                        "resolved_plan_question_suppressed",
                        plan_trace_fields(&plan),
                    );
                }
            } else {
                plan.updated_at_ms = self.clock.now_ms();
                self.store.save_plan(&plan)?;
                self.record_plan_trace(
                    "plan_turn_without_terminal_control",
                    plan_trace_fields(&plan),
                );
            }
        } else {
            if let Some(question) = output.plan_question.take() {
                let question = question.normalize()?;
                if !self.active_plan_awaits_input()? {
                    interaction.node_list.push(ExchangeNode::QuestionPresented {
                        id: format!("{}:question:{}", interaction.id, Uuid::new_v4()),
                        question: question.clone(),
                        answer: None,
                    });
                }
                if !self.replace_active_elicitation(question.clone(), &mut event)? {
                    interaction.awaiting_input = true;
                    interaction.elicitation = Some(PlanElicitation::new(question.clone()));
                    self.store.save_exchange(&interaction)?;
                    event.push(self.event(
                        "question",
                        json!({
                            "owner": "interaction",
                            "plan_id": interaction.plan_id,
                            "exchange_id": interaction.id,
                            "elicitation": interaction.elicitation,
                        }),
                    )?);
                }
            } else if let Some(answer) = output.question_answer.take() {
                self.answer_active_elicitation_from_model(answer, &mut event)?;
            } else if let Some(withdrawal) = output.question_withdrawal.take() {
                self.withdraw_active_elicitation(withdrawal, &mut event)?;
            }
        }

        if refresh_elicitation {
            let owner = self
                .store
                .list_exchange(&self.session.id)?
                .into_iter()
                .find(|owner| owner.id == interaction.id)
                .context("question control lost its owning exchange")?;
            interaction.elicitation = owner.elicitation;
            interaction.awaiting_input = owner.awaiting_input;
        }
        let execution_paused =
            self.apply_plan_execution_control(&mut output, &mut event, &interaction)?;
        self.session.updated_at_ms = self.clock.now_ms();
        self.save_session()?;
        let continuing = if execution_paused {
            false
        } else {
            self.apply_goal_evidence(output.evidence, &mut event)?
        };
        let mut runtime = self
            .exchange_runtime
            .take()
            .context("active interaction timeline is missing")?;
        finish_synthetic_turn(
            &mut interaction,
            runtime.synthetic_turn.as_ref(),
            crate::turn::TurnOutcome::Completed,
            self.clock.now_ms(),
        )?;
        runtime.synthetic_turn = None;
        let planning_open = interaction
            .plan_id
            .as_deref()
            .map(|id| self.store.load_plan(id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| {
                matches!(
                    plan.state,
                    PlanState::Generating | PlanState::Revising | PlanState::AwaitingInput
                )
            });
        let goal_state = interaction.goal_id.as_deref()
            .map(|goal_id| self.store.load_goal(goal_id)).transpose()?.flatten()
            .map(|goal| goal.state);
        let goal_suspended = matches!(goal_state,
            Some(GoalState::Paused | GoalState::UsageLimited | GoalState::BudgetLimited));
        let remains_open = continuing || goal_suspended || interaction.awaiting_input || planning_open;
        if remains_open {
            self.exchange_runtime = Some(runtime);
        } else {
            let outcome = if goal_state == Some(GoalState::Cleared) {
                ExchangeState::Cancelled
            } else {
                ExchangeState::Complete
            };
            self.capture_final_checkpoint(&mut interaction, outcome)
                .await?;
            interaction.finish(outcome, self.clock.now_ms())?;
        }
        self.store.save_exchange(&interaction)?;
        let can_finalize_incrementally = !continuing
            && mode == PromptMode::Chat
            && self.session.active_plan_id.is_none()
            && interaction.plan_id.is_none()
            && interaction.execution_id.is_none()
            && interaction.elicitation.is_none()
            && !interaction.awaiting_input
            && !self.agent_registry.list().iter().any(|run| {
                self.agent_registry
                    .execution(&run.id)
                    .and_then(|execution| execution.parent_exchange_id.as_deref())
                    == Some(interaction.id.as_str())
            });
        if can_finalize_incrementally {
            self.active_wait_projection = None;
            let timeline_patch = self.reconcile_live_interaction(Some(&interaction), None)?;
            self.emit_timeline_patch(timeline_patch, &mut event).await?;
            self.timeline_reconciled_after_dispatch = true;
        }
        event.push(self.event(
            if remains_open {
                "exchange_updated"
            } else {
                "exchange_complete"
            },
            serde_json::to_value(&interaction)?,
        )?);
        Ok((
            json!({ "exchange": interaction, "session": self.session, "capability": self.capability }),
            event,
        ))
    }

    async fn finalize_interaction_after_processing_failure(&mut self) -> Result<()> {
        let Some(runtime) = self.exchange_runtime.take() else {
            return Ok(());
        };
        let mut interaction = self
            .store
            .list_exchange(&self.session.id)?
            .into_iter()
            .find(|interaction| interaction.id == runtime.exchange_id)
            .context("failed interaction runtime has no durable interaction")?;
        if interaction.state != ExchangeState::Running {
            return Ok(());
        }

        let now_ms = self.clock.now_ms();
        let mut event = Vec::new();
        finish_synthetic_turn(
            &mut interaction,
            runtime.synthetic_turn.as_ref(),
            crate::turn::TurnOutcome::Failed,
            now_ms,
        )?;
        self.capture_final_checkpoint(&mut interaction, ExchangeState::Failed)
            .await?;
        interaction.finish(ExchangeState::Failed, now_ms)?;
        self.store.save_exchange(&interaction)?;
        self.pause_goal_after_turn_failure().await?;
        self.save_session()?;
        self.active_wait_projection = None;
        self.emit_live_interaction(
            BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "timeline_exchange_failed".into(),
                text: None,
                data: serde_json::to_value(&interaction)?,
                activity: None,
                summary: None,
                task_update: None,
            },
            &interaction,
            &mut event,
        )
        .await?;
        Ok(())
    }

    async fn start_exchange_runtime(
        &mut self,
        interaction: &mut Exchange,
        now_ms: i64,
    ) -> Result<()> {
        if let WorkspaceKind::Git(workspace) = &self.workspace_kind {
            let checkpoint = GitCheckpoint::new(workspace)
                .capture(
                    &self.store.objects,
                    &self.repositories,
                    &self.session.id,
                    now_ms,
                )
                .await?;
            self.store.save_checkpoint(&checkpoint)?;
            interaction.checkpoint_before = Some(checkpoint.id.clone());
        }
        self.active_wait_projection = None;
        self.exchange_runtime = Some(ExchangeRuntime {
            exchange_id: interaction.id.clone(),
            synthetic_turn: None,
            task: interaction
                .task
                .clone()
                .map(TaskTracker::from_snapshot)
                .unwrap_or_default(),
            retraction_eligible: true,
            active_wait: None,
        });
        Ok(())
    }

    fn resume_exchange_runtime(&mut self, interaction: &Exchange) -> Result<()> {
        self.exchange_runtime = Some(ExchangeRuntime {
            exchange_id: interaction.id.clone(),
            synthetic_turn: None,
            task: interaction
                .task
                .clone()
                .map(TaskTracker::from_snapshot)
                .unwrap_or_default(),
            retraction_eligible: false,
            active_wait: None,
        });
        Ok(())
    }

    async fn prompt_with_timeline(
        &mut self,
        request: BackendRequest,
        interaction: &mut Exchange,
        event: &mut Vec<SessionEvent>,
    ) -> Result<crate::backend::BackendOutput> {
        let mut runtime = self
            .exchange_runtime
            .take()
            .context("active interaction timeline is missing")?;
        let backend = Arc::clone(&self.backend);
        let cancellation = Arc::clone(&self.turn_cancellation);
        let mut cleanup = cancellation.cleanup.subscribe();
        let (backend_event_sink, mut backend_event_stream) = crate::backend::events::channel();
        let mut stream_open = true;
        let delivery_sink = self.event_sink.clone();
        let outcome = {
            let prompt = async move {
                backend
                    .prompt_stream(request, Some(backend_event_sink))
                    .await
            };
            tokio::pin!(prompt);
            loop {
                tokio::select! {
                    changed = cleanup.changed() => {
                        if changed.is_ok() {
                            let state = cleanup.borrow_and_update().clone();
                            if !matches!(state, ExecutionCleanup::Idle) {
                                if !cancellation.restart.load(Ordering::Acquire) {
                                    interaction.begin_finalization(ExchangeState::Cancelled, self.clock.now_ms())?;
                                }
                                interaction.finalization_error = match state {
                                    ExecutionCleanup::Failed(error) => Some(error),
                                    _ => None,
                                };
                                self.store.save_exchange(interaction)?;
                                let patch = self.reconcile_live_interaction(Some(interaction), None)?;
                                self.emit_timeline_patch(patch, event).await?;
                            }
                        }
                    }
                    backend_event = backend_event_stream.recv(), if stream_open => {
                        let backend_event = match backend_event {
                            Ok(Some(event)) => event,
                            Ok(None) => { stream_open = false; continue; },
                            Err(error) => break Err(error.into()),
                        };
                        if let Err(error) = self
                            .process_backend_event(&mut runtime, interaction, backend_event, event)
                            .await
                        {
                            break Err(error);
                        }
                    }
                    error = crate::backend::events::failed(delivery_sink.as_ref()) => break Err(error.into()),
                    result = &mut prompt => break result,
                    () = cancellation.cancelled() => break Err(anyhow::Error::new(TurnCancelled)),
                }
            }
        };
        let mut outcome = if !matches!(*cancellation.cleanup.borrow(), ExecutionCleanup::Idle) {
            Err(anyhow::Error::new(TurnCancelled))
        } else {
            outcome
        };
        while let Ok(backend_event) = backend_event_stream.try_recv() {
            if let Err(error) = self
                .process_backend_event(&mut runtime, interaction, backend_event, event)
                .await
            {
                if outcome.is_ok() {
                    outcome = Err(error);
                }
            }
        }
        if outcome.is_ok()
            && let Err(error) = backend_event_stream.check()
        {
            outcome = Err(error.into());
        }
        if outcome.is_err()
            && let Err(error) = self
                .permission_coordinator
                .cancel_all(self.event_sink.as_ref())
                .await
        {
            outcome = Err(error).context("clear pending approval requests");
        }
        if outcome.as_ref().is_err_and(|error| {
            error.downcast_ref::<TurnCancelled>().is_some()
                || error
                    .downcast_ref::<crate::backend::events::EventDeliveryFailure>()
                    .is_some()
        }) && let Err(error) = self.backend.cancel_session(&self.session.id).await
        {
            outcome = Err(error).context("stop cancelled backend transport");
        }
        self.exchange_runtime = Some(runtime);
        outcome
    }

    /// Retry finalization without submitting provider input or replaying tool execution.
    async fn retry_finalization(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let Some(mut exchange) = self
            .store
            .list_exchange(&self.session.id)?
            .into_iter()
            .find(|exchange| exchange.state == ExchangeState::Finalizing)
        else {
            return Ok((json!({"cancel_requested": true}), Vec::new()));
        };
        let descendants: Vec<_> = referenced_child_exchange_list(&exchange)
            .into_iter()
            .map(|(_, id)| id)
            .collect();
        if let Err(error) = self
            .store
            .require_settled_exchange_tree(&self.session.id, &descendants)
        {
            exchange.finalization_error = Some(format!("{error:#}"));
            self.store.save_exchange(&exchange)?;
            return Err(error);
        }
        let outcome = exchange
            .finalization_outcome
            .context("finalization outcome is missing")?;
        self.capture_final_checkpoint(&mut exchange, outcome)
            .await?;
        exchange.finish(outcome, self.clock.now_ms())?;
        self.store.save_exchange(&exchange)?;
        self.exchange_runtime = None;
        self.save_session()?;
        Ok((
            json!({"cancel_requested": true, "finalized_exchange_id": exchange.id}),
            vec![self.event("exchange_complete", serde_json::to_value(&exchange)?)?],
        ))
    }

    async fn capture_final_checkpoint(
        &mut self,
        interaction: &mut Exchange,
        outcome: ExchangeState,
    ) -> Result<Option<String>> {
        interaction.begin_finalization(outcome, self.clock.now_ms())?;
        self.store.save_exchange(interaction)?;
        let result: Result<Option<String>> = async {
            let descendants: Vec<_> = referenced_child_exchange_list(interaction)
                .into_iter()
                .map(|(_, id)| id)
                .collect();
            self.store
                .require_settled_exchange_tree(&self.session.id, &descendants)?;

            let WorkspaceKind::Git(workspace) = &self.workspace_kind else {
                interaction.finish(outcome, self.clock.now_ms())?;
                return Ok(None);
            };
            let checkpoint = GitCheckpoint::new(workspace)
                .capture(
                    &self.store.objects,
                    &self.repositories,
                    &self.session.id,
                    self.clock.now_ms(),
                )
                .await?;
            let before = interaction
                .checkpoint_before
                .as_deref()
                .map(|id| self.store.load_checkpoint(id))
                .transpose()?
                .flatten()
                .context("interaction before checkpoint is missing")?;
            self.populate_interaction_change_diffs(interaction, &before, &checkpoint)
                .await?;
            let mut settled = interaction.clone();
            settled.checkpoint_after = Some(checkpoint.id.clone());
            settled.finalization_error = None;
            settled.finish(outcome, self.clock.now_ms())?;
            self.store.save_checkpoint_exchange(&checkpoint, &settled)?;
            *interaction = settled;
            Ok(Some(checkpoint.id))
        }
        .await;
        interaction.finalization_error = result.as_ref().err().map(|error| format!("{error:#}"));
        self.store.save_exchange(interaction)?;
        result
    }

    async fn populate_interaction_change_diffs(
        &mut self,
        interaction: &mut Exchange,
        before: &crate::checkpoint::CheckpointRecord,
        after: &crate::checkpoint::CheckpointRecord,
    ) -> Result<()> {
        let change_index = self.interaction_provider_change_index(interaction)?;
        let checkpoint_diff_text = checkpoint_diff(
            &self.store.objects,
            &self.repositories.reads,
            &self.diff,
            before,
            after,
        )
        .await?;
        let attributed_diff_text = if change_index.is_empty() {
            None
        } else {
            Some(
                checkpoint_diff_for_paths(
                    &self.store.objects,
                    &self.repositories.reads,
                    &self.diff,
                    before,
                    after,
                    change_index.paths(),
                )
                .await?,
            )
        };
        interaction.attributed_matches_checkpoint = attributed_diff_text
            .as_deref()
            .is_some_and(|diff_text| diff_text == checkpoint_diff_text);
        interaction.attributed_diff_text = attributed_diff_text;
        interaction.checkpoint_diff_text = Some(checkpoint_diff_text);
        Ok(())
    }

    fn interaction_provider_change_index(
        &self,
        interaction: &Exchange,
    ) -> Result<ProviderChangeIndex> {
        let mut index = ProviderChangeIndex::default();
        index.record(interaction);
        let mut pending = referenced_child_exchange_list(interaction);
        let mut visited = HashSet::new();
        while let Some((agent_id, exchange_id)) = pending.pop() {
            if !visited.insert(exchange_id.clone()) {
                continue;
            }
            if let Some(record) = self
                .store
                .list_agent_exchange(&agent_id)?
                .into_iter()
                .find(|record| record.id == exchange_id)
            {
                index.record(&record);
                pending.extend(referenced_child_exchange_list(&record));
            }
        }
        Ok(index)
    }

    async fn process_backend_event(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        mut backend_event: BackendEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        if backend_event.kind == "approval_requested" {
            let owner = if let Some(address) = backend_event.address.as_ref() {
                if interaction
                    .turn
                    .iter()
                    .any(|turn| turn.provider() == address)
                {
                    interaction
                } else {
                    &self
                        .child_exchange_runtime_by_agent
                        .values()
                        .find(|child| {
                            child
                                .exchange
                                .turn
                                .iter()
                                .any(|turn| turn.provider() == address)
                        })
                        .context("approval refers to an unknown child execution")?
                        .exchange
                }
            } else {
                interaction
            };
            let turn = owner.turn.iter().find(|turn| {
                turn.state() == crate::turn::TurnState::Running
                    && backend_event
                        .address
                        .as_ref()
                        .is_none_or(|address| turn.provider() == address)
            });
            let id = backend_event
                .data
                .get("id")
                .and_then(Value::as_str)
                .context("approval request omitted its id")?;
            let approval = self.permission_coordinator.bind_owner(
                id,
                &owner.id,
                turn.map(|turn| turn.id()),
            )?;
            backend_event.data = serde_json::to_value(approval)?;
            self.emit_live(backend_event, event).await?;
            return Ok(());
        }
        if backend_event.kind == "parent_boundary" {
            let boundary = backend_event
                .data
                .get("boundary")
                .and_then(Value::as_str)
                .unwrap_or_default();
            let now_ms = self.clock.now_ms();
            match boundary {
                "wait_started" => {
                    interaction.close_running_messages();
                    runtime.active_wait = Some(ActiveWait {
                        exchange_id: interaction.id.clone(),
                        started_at_ms: now_ms,
                        agent_count: backend_event
                            .data
                            .get("agent_count")
                            .and_then(Value::as_u64)
                            .unwrap_or_default() as usize,
                    });
                    self.store.save_exchange(interaction)?;
                }
                "wait_updated" => {
                    if let Some(wait) = runtime.active_wait.as_mut() {
                        wait.agent_count = backend_event
                            .data
                            .get("agent_count")
                            .and_then(Value::as_u64)
                            .unwrap_or_default()
                            as usize;
                    }
                }
                "wait_ended" => runtime.active_wait = None,
                _ => return Ok(()),
            }
            self.emit_active_wait(interaction, runtime, event).await?;
            return Ok(());
        }
        if backend_event.kind == "agent_lifecycle" {
            runtime.retraction_eligible = false;
            let lifecycle: AgentLifecycleEvent =
                serde_json::from_value(backend_event.data.clone())?;
            let parent_agent_id = lifecycle
                .parent_thread_id
                .as_deref()
                .and_then(|thread| self.agent_registry.get_by_thread(thread))
                .map(|agent| agent.id.clone());
            if lifecycle.starts_child
                && let Some(parent_agent_id) = parent_agent_id
            {
                let mut owner = self
                    .child_exchange_runtime_by_agent
                    .remove(&parent_agent_id)
                    .context("nested delegation has no active parent exchange")?;
                let result: Result<()> = async {
                    if let Some(child) = self
                        .apply_agent_lifecycle(&backend_event, Some(&owner.exchange.id), event)
                        .await?
                    {
                        let task = self
                            .agent_registry
                            .execution(&child.id)
                            .map(|execution| execution.task.clone())
                            .unwrap_or_default();
                        owner.exchange.append_delegation(
                            &child.id,
                            "spawn",
                            &task,
                            self.clock.now_ms(),
                        )?;
                        self.store.save_exchange(&owner.exchange)?;
                    }
                    Ok(())
                }
                .await;
                self.child_exchange_runtime_by_agent
                    .insert(parent_agent_id, owner);
                result?;
                return Ok(());
            }
            if lifecycle.starts_child {
                let now_ms = self.clock.now_ms();
                if let Some(run) = self
                    .apply_agent_lifecycle(&backend_event, Some(&interaction.id), event)
                    .await?
                {
                    let task = self
                        .agent_registry
                        .execution(&run.id)
                        .map(|execution| execution.task.clone())
                        .unwrap_or_default();
                    let appended =
                        interaction.append_delegation(&run.id, "spawn", &task, now_ms)?;
                    if appended {
                        self.store.save_exchange(interaction)?;
                        self.emit_live_interaction(
                            BackendEvent {
                                address: None,
                                turn_boundary: None,
                                kind: "timeline_node_updated".into(),
                                text: None,
                                data: json!({
                                    "exchange_id": interaction.id,
                                    "node": interaction.node_list.last(),
                                }),
                                activity: None,
                                summary: None,
                                task_update: None,
                            },
                            interaction,
                            event,
                        )
                        .await?;
                    }
                }
            } else {
                self.apply_agent_lifecycle(&backend_event, Some(&interaction.id), event)
                    .await?;
            }
            return Ok(());
        }
        if self
            .route_agent_backend_event(Some(interaction), &backend_event, event)
            .await?
        {
            runtime.retraction_eligible = false;
            return Ok(());
        }
        admit_addressless_turn(
            interaction,
            &mut runtime.synthetic_turn,
            &mut backend_event,
            self.clock.now_ms(),
        )?;
        if !interaction.observe_turn(&backend_event, self.clock.now_ms())? {
            self.trace.record(
                &self.session.id,
                "provider.event.rejected",
                json!({
                    "exchange_id": interaction.id,
                    "thread_id": backend_event.address.as_ref().map(|address| &address.thread_id),
                    "turn_id": backend_event.address.as_ref().map(|address| &address.turn_id),
                    "event_type": backend_event.kind,
                    "code": "execution_unknown_or_settled",
                }),
            );
            return Ok(());
        }
        if backend_event.turn_boundary.is_some() {
            self.store.save_exchange(interaction)?;
            if backend_event.kind == "turn_started" {
                self.emit_backend_event(backend_event, event).await?;
            }
            return Ok(());
        }
        if backend_event.kind == "steering_input" {
            runtime.retraction_eligible = false;
            let now_ms = self.clock.now_ms();
            runtime.active_wait = None;
            interaction.append_input(
                crate::exchange::InputIntent::Steering,
                backend_event.text.unwrap_or_default(),
                now_ms,
            )?;
            self.store.save_exchange(interaction)?;
            self.emit_live_interaction(
                BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "timeline_node_updated".into(),
                    text: None,
                    data: json!({
                        "exchange_id": interaction.id,
                        "node": interaction.node_list.last(),
                    }),
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                interaction,
                event,
            )
            .await?;
            self.emit_active_wait(interaction, runtime, event).await?;
            return Ok(());
        }
        if backend_event.task_update.is_some()
            || backend_event.activity.is_some()
            || (backend_event.kind == "assistant_message"
                && backend_event
                    .text
                    .as_deref()
                    .is_some_and(|text| !text.is_empty()))
            || backend_event.kind == "error"
        {
            runtime.retraction_eligible = false;
        }
        if backend_event.kind == "context_usage" {
            let context_usage: ContextUsage = serde_json::from_value(backend_event.data.clone())?;
            self.session.context_usage = Some(context_usage);
            self.emit_backend_event(backend_event, event).await?;
            return Ok(());
        }
        if let Some(update) = backend_event.task_update.as_ref() {
            runtime.task.replace(update);
            interaction.task = Some(runtime.task.snapshot().clone());
            self.store.save_exchange(interaction)?;
            self.emit_live_interaction(
                BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "timeline_task_updated".into(),
                    text: None,
                    data: serde_json::to_value(runtime.task.snapshot())?,
                    activity: None,
                    summary: None,
                    task_update: Some(update.clone()),
                },
                interaction,
                event,
            )
            .await?;
        }
        if backend_event.address.is_some() {
            attribute_provider_tool(interaction, &mut runtime.task, &backend_event)?;
            self.store.save_exchange(interaction)?;
            self.emit_live_interaction(backend_event, interaction, event)
                .await?;
            return Ok(());
        }
        Ok(())
    }

    async fn emit_active_wait(
        &mut self,
        interaction: &Exchange,
        runtime: &ExchangeRuntime,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        self.active_wait_projection = runtime.active_wait.clone();
        self.emit_live_interaction(
            BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "timeline_wait_updated".into(),
                text: None,
                data: json!({
                    "exchange_id": interaction.id,
                    "wait": runtime.active_wait,
                }),
                activity: None,
                summary: None,
                task_update: None,
            },
            interaction,
            event,
        )
        .await
    }

    async fn emit_live(
        &mut self,
        backend_event: BackendEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        self.emit_backend_event(backend_event, event).await?;
        let timeline_patch = self.reconcile_timeline()?;
        self.emit_timeline_patch(timeline_patch, event).await
    }

    async fn emit_live_interaction(
        &mut self,
        backend_event: BackendEvent,
        interaction: &Exchange,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        self.emit_backend_event(backend_event, event).await?;
        let timeline_patch = self.reconcile_live_interaction(Some(interaction), None)?;
        self.emit_timeline_patch(timeline_patch, event).await
    }

    async fn emit_backend_event(
        &mut self,
        backend_event: BackendEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        self.trace.record(
            &self.session.id,
            "lua.event.emitted",
            serde_json::to_value(&backend_event)?,
        );
        if let Some(event_sink) = self.event_sink.as_ref() {
            event_sink.send_wait(backend_event).await?;
        } else {
            event.push(self.event("backend_event", serde_json::to_value(backend_event)?)?);
        }
        Ok(())
    }

    async fn emit_timeline_patch(
        &mut self,
        timeline_patch: TimelinePatch,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        if timeline_patch.is_empty() {
            return Ok(());
        }
        self.record_plan_trace(
            "timeline_patch_emitted",
            json!({
                "session_id": timeline_patch.session_id,
                "base_revision": timeline_patch.base_revision,
                "revision": timeline_patch.revision,
                "operation_count": timeline_patch.operation.len(),
            }),
        );
        if let Some(event_sink) = self.event_sink.as_ref() {
            event_sink
                .send_wait(BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "timeline_patch".into(),
                    text: None,
                    data: serde_json::to_value(timeline_patch)?,
                    activity: None,
                    summary: None,
                    task_update: None,
                })
                .await?;
        } else {
            event.push(self.event("timeline_patch", serde_json::to_value(timeline_patch)?)?);
        }
        Ok(())
    }

    pub(crate) fn presentation(&self) -> Arc<std::sync::Mutex<SessionPresentation>> {
        Arc::clone(&self.presentation)
    }

    /// Reconcile durable state into the next session-local timeline revision.
    fn reconcile_timeline(&mut self) -> Result<TimelinePatch> {
        let timeline = self.snapshot()?.timeline;
        self.presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .reconcile(timeline)
    }

    fn reconcile_after_dispatch(&mut self) -> Result<TimelinePatch> {
        if self.timeline_reconciled_after_dispatch {
            self.timeline_reconciled_after_dispatch = false;
            let revision = self
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .revision();
            return Ok(TimelinePatch {
                session_id: self.session.id.clone(),
                base_revision: revision,
                revision,
                operation: Vec::new(),
            });
        }
        self.reconcile_timeline()
    }

    fn reconcile_live_interaction(
        &mut self,
        interaction: Option<&Exchange>,
        removed_exchange_id: Option<&str>,
    ) -> Result<TimelinePatch> {
        let active_plan = self.session.active_plan_id.as_deref()
            .map(|id| self.store.load_plan(id)).transpose()?.flatten();
        let status = crate::session::state_machine::SessionPhase::resolve(
            active_plan.as_ref(),
            self.active_wait_projection.as_ref(),
            interaction,
        );
        self.presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .update_live(interaction, removed_exchange_id, status)
    }

    async fn interaction_for_turn(
        &mut self,
        text: &str,
        admit_user_action: bool,
        now_ms: i64,
    ) -> Result<(Exchange, bool)> {
        let mut interaction_list = self.store.list_exchange(&self.session.id)?;
        if !admit_user_action {
            let mut interaction = interaction_list
                .pop()
                .context("goal continuation has no originating user interaction")?;
            interaction.resume(now_ms)?;
            return Ok((interaction, false));
        }
        if interaction_list.is_empty() && self.session.name.trim().is_empty() {
            self.session.name = text.split_whitespace().collect::<Vec<_>>().join(" ")
                .chars().take(120).collect();
            self.save_session()?;
        }
        if let Some(previous) = interaction_list.last_mut()
            && previous.state == ExchangeState::Running
        {
            if previous.checkpoint_before.is_some() {
                self.capture_final_checkpoint(previous, ExchangeState::Complete)
                    .await?;
            }
            previous.finish(ExchangeState::Complete, now_ms)?;
            self.store.save_exchange(previous)?;
        }
        Ok((
            Exchange {
                finalization_error: None,
                finalization_outcome: None,
                agent_id: self.session.primary_agent_id.clone(),
                id: Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                ordinal: self.store.next_exchange_ordinal(&self.session.id)?,
                prompt: text.to_owned(),
                kind: ExchangeKind::Chat,
                mode: Some(self.session.mode),
                plan_id: None,
                execution_id: None,
                goal_id: None,
                state: ExchangeState::Running,
                checkpoint_before: None,
                checkpoint_after: None,
                attributed_diff_text: None,
                checkpoint_diff_text: None,
                attributed_matches_checkpoint: false,
                disposition: crate::exchange::HistoryDisposition::Current,
                turn: Vec::new(),
                created_at_ms: now_ms,
                completed_at_ms: None,
                node_list: Vec::new(),
                awaiting_input: false,
                elicitation: None,
                duration_ms: 0,
                execution_started_at_ms: Some(now_ms),
                token_count: None,
                comment: Vec::new(),
                task: None,
            },
            true,
        ))
    }

    fn begin_plan_acceptance(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let plan_id = params
            .get("plan_id")
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or_else(|| self.session.active_plan_id.clone())
            .context("no plan awaits review")?;
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("active plan record is missing")?;
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview,
            "plan does not await review"
        );
        let review_digest = plan
            .review_digest
            .clone()
            .context("submitted plan digest is missing")?;
        if let Some(expected) = params.get("digest").and_then(Value::as_str) {
            anyhow::ensure!(
                expected == review_digest,
                "plan changed after the accept action began"
            );
        }
        let reviewed = self.plan_file.capture_review_source(
            &self.session.id,
            &plan.id,
            plan.model_revision,
            &review_digest,
        )?;
        let mut acceptance =
            PlanAcceptance::new(review_digest, &self.capability.execution_mode_list)?;
        if let Some(expected) = params.get("saved_source_digest").and_then(Value::as_str) {
            anyhow::ensure!(
                expected == reviewed.saved_digest,
                "saved plan source changed after the review action"
            );
        }
        acceptance.saved_source_digest = Some(reviewed.saved_digest);
        plan.acceptance = Some(acceptance);
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        self.session.active_plan_id = Some(plan.id.clone());
        self.save_session()?;
        let snapshot = self.snapshot()?;
        Ok((
            serde_json::to_value(&snapshot)?,
            vec![self.event(
                "plan_acceptance_started",
                serde_json::to_value(&snapshot.active_elicitation)?,
            )?],
        ))
    }

    fn cancel_plan_acceptance(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut plan = self.active_plan_acceptance()?;
        plan.acceptance = None;
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        let snapshot = self.snapshot()?;
        Ok((
            serde_json::to_value(&snapshot)?,
            vec![self.event("plan_acceptance_cancelled", json!({ "plan": plan }))?],
        ))
    }

    fn rename_plan_entity(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let plan_id = params
            .get("plan_id")
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or_else(|| self.session.active_plan_id.clone())
            .context("no plan awaits review")?;
        let entity_name = required_text(&params, "entity_name")?;
        let new_name = required_text(&params, "name")?;
        anyhow::ensure!(!new_name.trim().is_empty(), "entity name cannot be empty");
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("active plan record is missing")?;
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview,
            "plan does not await review"
        );
        let document = self
            .plan_file
            .read_working_document(&self.session.id, &plan.id)?;
        if let Some(expected_version) = params.get("expected_version").and_then(Value::as_u64) {
            anyhow::ensure!(
                document.version == expected_version,
                "plan version changed before entity rename"
            );
        }
        let previous_name = document
            .entity_changes
            .iter()
            .find(|entity| entity.name == entity_name)
            .map(|entity| entity.name.clone())
            .with_context(|| format!("program entity `{entity_name}` does not exist"))?;
        anyhow::ensure!(
            previous_name != new_name,
            "new entity name matches current name"
        );
        let result = self.plan_file.rename_added_entity(
            &self.session.id,
            &plan.id,
            &entity_name,
            new_name.clone(),
        )?;
        plan.model_revision = plan.model_revision.saturating_add(1);
        plan.user_revision = plan.user_revision.saturating_add(1);
        let (document, rendered, digest) = self.plan_file.submit_document_revision(
            &self.session.id,
            &plan.id,
            plan.model_revision,
            result.version,
        )?;
        plan.document_version = result.version;
        plan.submitted_version = Some(result.version);
        plan.title.clone_from(&document.title);
        plan.review_digest = Some(digest);
        plan.acceptance = None;
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        self.session.active_plan_id = Some(plan.id.clone());
        self.save_session()?;
        let lifecycle = PlanLifecycleRecord {
            title: plan.title.clone(),
            anchor: Some(self.plan_exchange_anchor(&plan.id)?),
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            plan_id: plan.id.clone(),
            kind: PlanLifecycleKind::RevisionCreated,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            overall_comment: Some(format!("Renamed {previous_name} to {new_name}")),
            annotation: Vec::new(),
            question: None,
            answer: None,
            created_at_ms: self.clock.now_ms(),
        };
        self.store.save_plan_lifecycle(&lifecycle)?;
        let payload = json!({
            "plan": &plan,
            "lifecycle": &lifecycle,
            "content": &rendered.markdown,
            "document": &document,
            "previous_name": previous_name,
            "name": new_name,
        });
        Ok((
            payload.clone(),
            vec![self.event("plan_entity_renamed", payload)?],
        ))
    }

    async fn continue_plan_acceptance(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let plan = self.active_plan_acceptance()?;
        let acceptance = plan
            .acceptance
            .as_ref()
            .context("active plan has no acceptance state")?;
        let context_choice = acceptance.context_choice()?;
        let execution_mode = acceptance.execution_mode()?;
        self.accept_plan(json!({
            "plan_id": plan.id,
            "digest": acceptance.review_digest,
            "saved_source_digest": acceptance.saved_source_digest,
            "fresh_context": context_choice == ContextChoice::Fresh,
            "execution_mode": execution_mode,
        }))
        .await
    }

    async fn accept_plan(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            !self
                .store
                .list_plan_execution(&self.session.id)?
                .iter()
                .any(|execution| execution.state == PlanExecutionState::Active),
            "another plan execution is already active"
        );
        let execution_mode = serde_json::from_value::<ExecutionMode>(
            params
                .get("execution_mode")
                .cloned()
                .context("execution_mode is required")?,
        )?;
        anyhow::ensure!(
            self.capability
                .execution_mode_list
                .contains(&execution_mode),
            "execution mode {} is unavailable for this backend",
            execution_mode.label()
        );
        let plan_id = params
            .get("plan_id")
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or_else(|| self.session.active_plan_id.clone())
            .context("no plan awaits review")?;
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("active plan record is missing")?;
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview,
            "plan does not await review"
        );
        let digest = plan
            .review_digest
            .clone()
            .context("submitted plan digest is missing")?;
        if let Some(expected) = params.get("digest").and_then(Value::as_str) {
            anyhow::ensure!(
                expected == digest,
                "plan changed after the accept action began"
            );
        }
        let reviewed = self.plan_file.capture_review_source(
            &self.session.id,
            &plan.id,
            plan.model_revision,
            &digest,
        )?;
        let accepted_document = reviewed.document;
        let accepted_render = reviewed.rendered;
        if let Some(expected) = params.get("saved_source_digest").and_then(Value::as_str) {
            anyhow::ensure!(
                expected == reviewed.saved_digest,
                "saved plan source changed during acceptance"
            );
        }
        plan.user_revision += 1;
        plan.accepted_digest = Some(digest);
        plan.accepted_revision = Some(plan.model_revision);
        plan.acceptance = None;
        PlanStateMachine::apply(&mut plan, PlanEvent::Accepted, self.clock.now_ms())?;
        self.store.save_plan(&plan)?;
        self.session.active_plan_id = Some(plan.id.clone());
        let objective = format!("Complete accepted plan: {}", plan.title);
        let goal = self.create_goal(objective, false)?;
        let lifecycle = PlanLifecycleRecord {
            title: plan.title.clone(),
            anchor: Some(self.plan_exchange_anchor(&plan.id)?),
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            plan_id: plan.id.clone(),
            kind: PlanLifecycleKind::Accepted,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            overall_comment: None,
            annotation: Vec::new(),
            question: None,
            answer: None,
            created_at_ms: self.clock.now_ms(),
        };
        self.store.save_plan_lifecycle(&lifecycle)?;
        let fresh_context = params
            .get("fresh_context")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        let planning_backend_session_id = self.session.backend_session_id.clone();
        if fresh_context {
            self.session.backend_session_id = None;
            self.session.provider_checkpoint_id = None;
            self.session.context_usage = None;
        }
        self.session.execution_mode = execution_mode;
        self.session.mode = HarnessMode::from(self.session.execution_mode);
        self.save_session()?;
        let execution_created_at_ms = self.clock.now_ms();
        let mut scheduler = crate::plan::PlanScheduler::activate(&accepted_document);
        let active_task = scheduler.next_task(&accepted_document, execution_created_at_ms);
        let mut execution_record = PlanExecutionRecord {
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            plan_id: plan.id.clone(),
            goal_id: goal.id.clone(),
            state: PlanExecutionState::Active,
            planning_backend_session_id,
            execution_backend_session_id: self.session.backend_session_id.clone(),
            scheduler,
            lifecycle: Vec::new(),
            created_at_ms: execution_created_at_ms,
            completed_at_ms: None,
        };
        if let Some(active_task) = active_task {
            let task_path = execution_record
                .scheduler
                .task
                .iter()
                .find(|task| task.state == crate::plan::PlanTaskState::Active)
                .map(|task| task.task_path.clone())
                .context("activated task path is missing")?;
            execution_record.append_lifecycle(
                lifecycle.anchor.clone().context("plan acceptance has no owning exchange")?,
                execution_created_at_ms,
                PlanExecutionLifecycleEvent::TaskStarted {
                    task_path,
                    ordinal: 1,
                    total: accepted_document.tasks.len(),
                    title: active_task.title.clone(),
                },
            );
        }
        self.store.save_plan_execution(&execution_record)?;
        let mut pre_execution_event = Vec::new();
        self.emit_live(
            BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "timeline_plan_lifecycle".into(),
                text: None,
                data: json!({
                    "kind": "plan_lifecycle",
                    "id": lifecycle.id,
                    "created_at_ms": lifecycle.created_at_ms,
                    "plan": &plan,
                    "lifecycle": &lifecycle,
                    "content": &accepted_render.markdown,
                    "document": &accepted_document
                }),
                activity: None,
                summary: None,
                task_update: None,
            },
            &mut pre_execution_event,
        )
        .await?;
        let execution_prompt = execution_prompt(
            PlanExecutionPromptKind::Start,
            &execution_record.id,
            active_task,
            &accepted_document,
        )?;
        let mut admission = ExchangeAdmission::execution(
            format!("Accept plan: {}", plan.request), plan.id.clone(),
            execution_record.id.clone(), goal.id.clone());
        admission.plan_event_id = Some(lifecycle.id.clone());
        let execution = self.run_interaction(execution_prompt, PromptMode::ExecutePlan, Some(admission))
            .await;
        let execution_succeeded = execution.is_ok();
        execution_record = self
            .store
            .load_plan_execution(&execution_record.id)?
            .unwrap_or(execution_record);
        execution_record.execution_backend_session_id = self.session.backend_session_id.clone();
        self.store.save_plan_execution(&execution_record)?;
        let (result, mut event) = match execution {
            Ok(result) => result,
            Err(error) => {
                if error.downcast_ref::<TurnCancelled>().is_some()
                    && self.turn_cancellation.restart.load(Ordering::Acquire)
                {
                    return Err(error);
                }
                let mut paused_goal = self.active_goal()?;
                paused_goal.state = GoalState::Paused;
                paused_goal.updated_at_ms = self.clock.now_ms();
                self.store.save_goal(&paused_goal)?;
                execution_record.state = PlanExecutionState::Paused;
                self.store.save_plan_execution(&execution_record)?;
                let backend_event = BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "error".into(),
                    text: Some(format!(
                        "The plan was accepted, but its first execution turn failed: {error:#}. Use /goal resume to retry."
                    )),
                    data: Value::Null,
                    activity: None,
                    summary: None,
                    task_update: None,
                };
                (
                    json!({
                        "session": self.session,
                        "capability": self.capability,
                        "execution_error": format!("{error:#}")
                    }),
                    vec![
                        self.event("backend_event", serde_json::to_value(backend_event)?)?,
                        self.event("goal_changed", serde_json::to_value(paused_goal)?)?,
                    ],
                )
            }
        };
        event.splice(0..0, pre_execution_event);
        if execution_succeeded {
            event.insert(0, self.event("goal_changed", serde_json::to_value(goal)?)?);
        }
        event.insert(
            0,
            self.event(
                "plan_accepted",
                json!({
                    "plan": plan,
                    "lifecycle": lifecycle,
                    "execution": execution_record,
                    "content": accepted_render.markdown,
                    "document": accepted_document,
                    "fresh_context": fresh_context
                }),
            )?,
        );
        Ok((result, event))
    }

    async fn request_plan_changes(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let plan_id = params
            .get("plan_id")
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or_else(|| self.session.active_plan_id.clone())
            .context("no plan awaits review")?;
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("active plan record is missing")?;
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview,
            "plan does not await review"
        );
        let expected_digest = params
            .get("digest")
            .and_then(Value::as_str)
            .or(plan.review_digest.as_deref())
            .context("submitted plan digest is missing")?;
        let reviewed = self.capture_plan_review(&plan.id, expected_digest)?;
        if let Some(expected) = params.get("saved_source_digest").and_then(Value::as_str) {
            anyhow::ensure!(
                expected == reviewed.saved_digest,
                "saved plan source changed after the review action"
            );
        }
        plan.user_revision += 1;
        let document = reviewed.document;
        let rendered = reviewed.rendered;
        let annotation_input: Vec<crate::plan::PlanAnnotationInput> = serde_json::from_value(
            params
                .get("annotations")
                .cloned()
                .unwrap_or_else(|| json!([])),
        )?;
        let annotation = crate::plan::resolve_annotations(&rendered, annotation_input)?;
        let overall_comment = params
            .get("comment")
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|comment| !comment.is_empty())
            .map(str::to_owned);
        anyhow::ensure!(
            overall_comment
                .as_ref()
                .is_none_or(|comment| comment.len() <= 65536),
            "overall plan review comment exceeds 64 KiB"
        );
        plan.acceptance = None;
        PlanStateMachine::apply(&mut plan, PlanEvent::ChangesRequested, self.clock.now_ms())?;
        plan.generation.reset();
        self.store.save_plan(&plan)?;
        self.session.active_plan_id = Some(plan.id.clone());
        self.session.mode = HarnessMode::Plan;
        self.save_session()?;
        let lifecycle = PlanLifecycleRecord {
            title: plan.title.clone(),
            anchor: Some(self.plan_exchange_anchor(&plan.id)?),
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            plan_id: plan.id.clone(),
            kind: PlanLifecycleKind::ChangesRequested,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            overall_comment: overall_comment.clone(),
            annotation: annotation.clone(),
            question: None,
            answer: None,
            created_at_ms: self.clock.now_ms(),
        };
        self.store.save_plan_lifecycle(&lifecycle)?;
        let document_json = document.model_json()?;
        let instruction = PlanPrompt::revision(
            &document_json,
            &serde_json::to_string_pretty(&annotation)?,
            overall_comment.as_deref(),
        );
        let leading_event = self.event(
            "plan_changes_requested",
            json!({ "plan": &plan, "lifecycle": &lifecycle }),
        )?;
        let review_prompt = overall_comment
            .as_deref()
            .map(|comment| format!("Request plan changes: {comment}"))
            .unwrap_or_else(|| "Request plan changes".into());
        let mut admission = ExchangeAdmission::plan(review_prompt, Some(plan.id.clone()), true);
        admission.plan_event_id = Some(lifecycle.id.clone());
        match self.run_planning_interaction(instruction, Some(admission))
            .await
        {
            Ok((result, mut event)) => {
                event.insert(0, leading_event);
                Ok((result, event))
            }
            Err(error) => Err(error).context("revise reviewed plan"),
        }
    }

    fn cancel_plan(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let plan_id = self
            .session
            .active_plan_id
            .clone()
            .context("no plan awaits review")?;
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("active plan record is missing")?;
        PlanStateMachine::apply(&mut plan, PlanEvent::Cancelled, self.clock.now_ms())?;
        plan.acceptance = None;
        self.store.save_plan(&plan)?;
        let lifecycle = PlanLifecycleRecord {
            title: plan.title.clone(),
            anchor: Some(self.plan_exchange_anchor(&plan.id)?),
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            plan_id: plan.id.clone(),
            kind: PlanLifecycleKind::Cancelled,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            overall_comment: None,
            annotation: Vec::new(),
            question: None,
            answer: None,
            created_at_ms: self.clock.now_ms(),
        };
        self.store.save_plan_lifecycle(&lifecycle)?;
        for mut exchange in self.store.list_exchange(&self.session.id)? {
            if exchange.plan_id.as_deref() == Some(&plan.id) && exchange.completed_at_ms.is_none() {
                exchange.awaiting_input = false;
                exchange.elicitation = None;
                exchange.finish(ExchangeState::Cancelled, self.clock.now_ms())?;
                self.store.save_exchange(&exchange)?;
            }
        }
        self.session.active_plan_id = None;
        self.session.mode = self.session.execution_mode.into();
        self.save_session()?;
        Ok((
            serde_json::to_value(&plan)?,
            vec![self.event(
                "plan_cancelled",
                json!({ "plan": plan, "lifecycle": lifecycle }),
            )?],
        ))
    }

    async fn retry_plan(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let plan_id = self
            .session
            .active_plan_id
            .clone()
            .context("no failed plan can be retried")?;
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("failed plan record is missing")?;
        anyhow::ensure!(
            plan.state == PlanState::Failed,
            "/plan retry requires a failed plan"
        );
        plan.generation.reset();
        PlanStateMachine::apply(&mut plan, PlanEvent::RetryRequested, self.clock.now_ms())?;
        self.store.save_plan(&plan)?;
        let document = self
            .plan_file
            .read_working_document(&self.session.id, &plan.id)?;
        self.run_planning_interaction(
            PlanPrompt::with_active_document(
                format!(
                    "Resume the failed planning task using the existing canonical document and \
resolved decisions. Complete and submit the plan for this request:\n\n{}",
                    plan.request
                ),
                &document.model_json()?,
            ),
            Some(ExchangeAdmission::plan(
                "/plan retry".into(),
                Some(plan.id),
                plan.model_revision > 0,
            )),
        )
        .await
    }

    /// Read a submitted revision after validating its owning session and revision range.
    pub(crate) fn capture_plan_revision(
        &self,
        plan_id: &str,
        revision: u32,
    ) -> Result<crate::plan::review_source::PlanReviewSource> {
        let plan = self.store.load_plan(plan_id)?.context("plan artifact not found")?;
        anyhow::ensure!(plan.session_id == self.session.id, "plan belongs to another session");
        anyhow::ensure!(revision > 0 && revision <= plan.model_revision, "plan revision is unavailable");
        self.plan_file.capture_revision_source(&self.session.id, &plan.id, revision)
    }

    fn activate_plan(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let plan_id = required_text(&params, "plan_id")?;
        let plan = self
            .store
            .load_plan(&plan_id)?
            .context("plan artifact not found")?;
        anyhow::ensure!(
            plan.session_id == self.session.id,
            "plan belongs to another session"
        );
        anyhow::ensure!(
            !plan.working_path.is_empty(),
            "plan artifact is not ready for review"
        );
        if let Some(revision) = params.get("revision").filter(|value| !value.is_null()) {
            let revision: u32 = serde_json::from_value(revision.clone())?;
            if revision != plan.model_revision || plan.state != PlanState::AwaitingReview {
                let source = self.capture_plan_revision(&plan.id, revision)?;
                let mut historical = serde_json::to_value(&plan)?;
                historical["title"] = serde_json::json!(source.document.title);
                historical["working_path"] = serde_json::json!(source.path);
                historical["document_version"] = serde_json::json!(source.document.version);
                historical["model_revision"] = serde_json::json!(revision);
                historical["historical_revision"] = serde_json::json!(revision);
                historical["review_digest"] = serde_json::json!(crate::plan::digest(&serde_json::to_vec(&source.document)?));
                return Ok((historical, Vec::new()));
            }
        }
        self.session.active_plan_id = Some(plan.id.clone());
        self.save_session()?;
        Ok((
            serde_json::to_value(&plan)?,
            vec![self.event("plan_activated", serde_json::to_value(plan)?)?],
        ))
    }

    async fn set_goal(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let objective = required_text(&params, "objective")?;
        self.pause_current_goal().await?;
        let goal = self.create_goal(objective.clone(), self.capability.native_goal)?;
        let prompt = if goal.native {
            format!("/goal {objective}")
        } else {
            format!(
                "Goal: {objective}\nContinue working until the goal is complete. Use harness_goal_complete or harness_goal_blocked to report terminal state."
            )
        };
        let (result, mut event) = self
            .run_interaction(
                prompt,
                PromptMode::GoalContinuation,
                Some(ExchangeAdmission::goal(
                    format!("/goal {objective}"),
                    goal.id.clone(),
                )),
            )
            .await?;
        event.insert(0, self.event("goal_changed", serde_json::to_value(goal)?)?);
        Ok((result, event))
    }

    fn create_goal(&mut self, objective: String, native: bool) -> Result<GoalRecord> {
        let now_ms = self.clock.now_ms();
        let goal = GoalRecord {
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            objective,
            state: GoalState::Active,
            continuation: crate::session::continuation::ContinuationBudget::new(
                self.goal_max_turns,
                2,
            ),
            native,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
        };
        self.store.save_goal(&goal)?;
        self.session.goal_id = Some(goal.id.clone());
        self.save_session()?;
        Ok(goal)
    }

    async fn pause_goal(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut goal = self.active_goal()?;
        if goal.state == GoalState::Paused {
            return Ok((serde_json::to_value(&goal)?,
                vec![self.event("goal_changed", serde_json::to_value(goal)?)?]));
        }
        anyhow::ensure!(goal.state == GoalState::Active, "goal is not active");
        self.sync_native_goal(&goal, "paused").await?;
        goal.state = GoalState::Paused;
        goal.updated_at_ms = self.clock.now_ms();
        self.store.save_goal(&goal)?;
        self.sync_plan_execution(&goal)?;
        Ok((
            serde_json::to_value(&goal)?,
            vec![self.event("goal_changed", serde_json::to_value(goal)?)?],
        ))
    }

    async fn resume_goal(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut goal = self.active_goal()?;
        anyhow::ensure!(
            matches!(
                goal.state,
                GoalState::Paused | GoalState::Stalled | GoalState::Blocked
                    | GoalState::UsageLimited | GoalState::BudgetLimited
            ),
            "goal cannot resume from its current state"
        );
        goal.resume(self.clock.now_ms());
        self.store.save_goal(&goal)?;
        self.sync_plan_execution(&goal)?;
        let plan_prompt =
            self.plan_goal_prompt(&goal, PlanExecutionPromptKind::ResumeAfterInterruption)?;
        let mode = if plan_prompt.is_some() {
            PromptMode::ExecutePlan
        } else {
            PromptMode::GoalContinuation
        };
        let prompt = plan_prompt
            .unwrap_or_else(|| {
                if goal.native {
                    "/goal resume".to_owned()
                } else {
                    format!(
                        "/goal resume\nContinue working toward this goal: {}",
                        goal.objective
                    )
                }
            });
        let admission = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .find(|execution| execution.goal_id == goal.id)
            .map(|execution| {
                ExchangeAdmission::execution(
                    "/goal resume".into(),
                    execution.plan_id,
                    execution.id,
                    goal.id.clone(),
                )
            })
            .unwrap_or_else(|| ExchangeAdmission::goal("/goal resume".into(), goal.id.clone()));
        match self
            .run_interaction(prompt, mode, Some(admission))
            .await
        {
            Ok((result, mut event)) => {
                event.insert(0, self.event("goal_changed", serde_json::to_value(goal)?)?);
                Ok((result, event))
            }
            Err(error) => {
                if error.downcast_ref::<TurnCancelled>().is_some()
                    && self.turn_cancellation.restart.load(Ordering::Acquire)
                {
                    return Err(error);
                }
                goal.state = GoalState::Paused;
                goal.updated_at_ms = self.clock.now_ms();
                self.store.save_goal(&goal)?;
                self.sync_plan_execution(&goal)?;
                let _ = self.sync_native_goal(&goal, "paused").await;
                Err(error).context("resume Harness goal")
            }
        }
    }

    async fn clear_goal(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut goal = self.active_goal()?;
        self.sync_native_goal(&goal, "cleared").await?;
        goal.state = GoalState::Cleared;
        goal.updated_at_ms = self.clock.now_ms();
        self.store.save_goal(&goal)?;
        let mut event = Vec::new();
        for mut exchange in self.store.list_exchange(&self.session.id)? {
            if exchange.goal_id.as_deref() != Some(goal.id.as_str())
                || !matches!(exchange.state, ExchangeState::Running | ExchangeState::Finalizing)
            {
                continue;
            }
            let outcome = exchange.finalization_outcome.unwrap_or(ExchangeState::Cancelled);
            self.capture_final_checkpoint(&mut exchange, outcome).await?;
            if self.exchange_runtime.as_ref().is_some_and(|runtime| runtime.exchange_id == exchange.id) {
                self.exchange_runtime = None;
            }
            event.push(self.event("exchange_complete", serde_json::to_value(exchange)?)?);
        }
        self.sync_plan_execution(&goal)?;
        self.session.goal_id = None;
        self.save_session()?;
        event.push(self.event("goal_changed", serde_json::to_value(&goal)?)?);
        Ok((serde_json::to_value(goal)?, event))
    }

    async fn continue_goal(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let goal = self.active_goal()?;
        anyhow::ensure!(goal.state == GoalState::Active, "goal is not active");
        let plan_prompt = self.plan_goal_prompt(&goal, PlanExecutionPromptKind::Continue)?;
        let mode = if plan_prompt.is_some() {
            PromptMode::ExecutePlan
        } else {
            PromptMode::GoalContinuation
        };
        let prompt = plan_prompt
            .unwrap_or_else(|| {
                if goal.native {
                    "/goal resume".to_owned()
                } else {
                    format!("Continue working toward this goal: {}", goal.objective)
                }
            });
        self.run_interaction(prompt, mode, None)
            .await
    }

    fn plan_goal_prompt(
        &self,
        goal: &GoalRecord,
        kind: PlanExecutionPromptKind,
    ) -> Result<Option<String>> {
        let Some(execution) = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .find(|execution| execution.goal_id == goal.id)
        else {
            return Ok(None);
        };
        let plan = self
            .store
            .load_plan(&execution.plan_id)?
            .context("accepted plan record is missing")?;
        let accepted_revision = plan
            .accepted_revision
            .context("accepted plan revision is missing")?;
        let accepted = self.plan_file.read_submitted_document(
            &self.session.id,
            &plan.id,
            accepted_revision,
        )?;
        let deviation_list = self
            .store
            .list_plan_deviation(&self.session.id)?
            .into_iter()
            .filter(|deviation| deviation.execution_id == execution.id)
            .collect::<Vec<_>>();
        let effective = crate::plan::build_effective_plan(&accepted, &deviation_list)?;
        let active_task_index = execution
            .scheduler
            .task
            .iter()
            .enumerate()
            .find(|(_, task)| task.state == crate::plan::PlanTaskState::Active)
            .map(|(task_index, _)| task_index);
        let active_task =
            active_task_index.and_then(|task_index| effective.document.tasks.get(task_index));
        Ok(Some(execution_prompt(
            kind,
            &execution.id,
            active_task,
            &effective.document,
        )?))
    }

    fn apply_goal_evidence(
        &mut self,
        evidence: crate::goal::TurnEvidence,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        let Some(goal_id) = self.session.goal_id.clone() else {
            return Ok(false);
        };
        let mut goal = self
            .store
            .load_goal(&goal_id)?
            .context("active goal record is missing")?;
        let continuing = match goal.observe(evidence, self.clock.now_ms()) {
            ContinuationDecision::Continue | ContinuationDecision::RetryNoProgress => {
                event.push(self.event("goal_continue_requested", serde_json::to_value(&goal)?)?);
                true
            }
            ContinuationDecision::Complete
            | ContinuationDecision::Blocked
            | ContinuationDecision::Stalled
            | ContinuationDecision::Settled => {
                event.push(self.event("goal_changed", serde_json::to_value(&goal)?)?);
                false
            }
            ContinuationDecision::Stop => return Ok(false),
        };
        self.store.save_goal(&goal)?;
        self.sync_plan_execution(&goal)?;
        let resolved_execution = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .find(|execution| execution.goal_id == goal.id);
        let resolution_list = self.store.list_plan_resolution(&self.session.id)?;
        if let Some(resolution) = resolved_execution.and_then(|execution| {
            resolution_list
                .into_iter()
                .rev()
                .find(|resolution| resolution.execution_id == execution.id
                    && Some(resolution.resolved_at_ms) == execution.completed_at_ms)
        }) {
            let deviations = self
                .store
                .list_plan_deviation(&self.session.id)?
                .into_iter()
                .filter(|deviation| deviation.execution_id == resolution.execution_id)
                .collect::<Vec<_>>();
            let audit = self
                .store
                .list_plan_audit(&self.session.id)?
                .into_iter()
                .find(|audit| audit.id == resolution.audit_id);
            event.push(self.event(
                "plan_resolution",
                json!({ "resolution": resolution, "deviations": deviations, "audit": audit }),
            )?);
        }
        Ok(continuing)
    }

    fn apply_plan_execution_control(
        &mut self,
        output: &mut crate::backend::BackendOutput,
        event: &mut Vec<SessionEvent>,
        exchange: &Exchange,
    ) -> Result<bool> {
        if output.plan_deviation.is_empty() && output.plan_task_report.is_empty() {
            return Ok(false);
        }
        let mut execution = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .rev()
            .find(|execution| execution.state == PlanExecutionState::Active)
            .context("plan execution control requires an active execution")?;
        let plan = self
            .store
            .load_plan(&execution.plan_id)?
            .context("accepted plan record is missing")?;
        let accepted_revision = plan
            .accepted_revision
            .context("accepted plan revision is missing")?;
        let accepted = self.plan_file.read_submitted_document(
            &self.session.id,
            &plan.id,
            accepted_revision,
        )?;
        let mut deviation_list = self.store.list_plan_deviation(&self.session.id)?;
        let effective_before_deviation =
            crate::plan::build_effective_plan(&accepted, &deviation_list)?;
        let mut pause_for_review = false;
        for request in std::mem::take(&mut output.plan_deviation) {
            anyhow::ensure!(
                request.plan_id == plan.id,
                "deviation plan id does not match execution"
            );
            let effective_value = serde_json::to_value(&effective_before_deviation.document)?;
            for pointer in [
                request.task_path.as_deref(),
                request.subtask_path.as_deref(),
            ]
            .into_iter()
            .flatten()
            {
                anyhow::ensure!(
                    effective_value.pointer(pointer).is_some(),
                    "deviation references a path outside the effective plan"
                );
            }
            let disposition = match request.kind {
                PlanDeviationKind::Informational => PlanDeviationDisposition::Recorded,
                PlanDeviationKind::Scope
                    if self.scope_deviation_review == ScopeDeviationReview::Auto =>
                {
                    PlanDeviationDisposition::AutoApproved
                }
                PlanDeviationKind::Scope => {
                    pause_for_review = true;
                    PlanDeviationDisposition::Pending
                }
            };
            let deviation_created_at_ms = self.clock.now_ms();
            let deviation = PlanDeviation {
                id: Uuid::new_v4().to_string(),
                plan_id: plan.id.clone(),
                execution_id: execution.id.clone(),
                kind: request.kind,
                disposition,
                summary: request.summary,
                reason: request.reason,
                task_path: request.task_path,
                subtask_path: request.subtask_path,
                affected_paths: request.affected_paths,
                proposed_changes: request.proposed_changes,
                created_at_ms: deviation_created_at_ms,
                resolved_at_ms: (disposition != PlanDeviationDisposition::Pending)
                    .then(|| self.clock.now_ms()),
            };
            deviation.validate()?;
            self.store
                .save_plan_deviation(&self.session.id, &deviation)?;
            event.push(self.event(
                if disposition == PlanDeviationDisposition::Pending {
                    "plan_deviation_review"
                } else {
                    "plan_deviation_recorded"
                },
                serde_json::to_value(&deviation)?,
            )?);
            execution.append_lifecycle(
                crate::plan::ExchangeAnchor::capture(exchange),
                deviation_created_at_ms,
                PlanExecutionLifecycleEvent::DeviationRecorded {
                    deviation_id: deviation.id.clone(),
                    summary: deviation.summary.clone(),
                },
            );
            deviation_list.push(deviation);
        }
        let effective = crate::plan::build_effective_plan(&accepted, &deviation_list)?;
        if pause_for_review {
            anyhow::ensure!(
                output.plan_task_report.is_empty(),
                "task cannot finish while a scope deviation awaits review"
            );
            let mut goal = self.active_goal()?;
            goal.state = GoalState::Paused;
            goal.updated_at_ms = self.clock.now_ms();
            self.store.save_goal(&goal)?;
            execution.state = PlanExecutionState::Paused;
            self.store.save_plan_execution(&execution)?;
            event.push(self.event("goal_changed", serde_json::to_value(goal)?)?);
            return Ok(true);
        }
        for report in std::mem::take(&mut output.plan_task_report) {
            anyhow::ensure!(
                report.execution_id == execution.id,
                "task report execution id does not match active execution"
            );
            let task_index = execution
                .scheduler
                .task
                .iter()
                .position(|task| task.task_path == report.task_path)
                .context("scheduled task not found")?;
            let task = effective
                .document
                .tasks
                .get(task_index)
                .context("canonical task not found")?;
            let task_path = report.task_path.clone();
            let task_title = task.title.clone();
            let report_state = report.state;
            let transition_at_ms = self.clock.now_ms();
            let prior_exchanges = self.store.list_exchange(&self.session.id)?;
            let elapsed_ms = execution.task_duration_ms(&task_path,
                prior_exchanges.iter().filter(|prior| prior.id != exchange.id)
                    .chain(std::iter::once(exchange)), transition_at_ms);
            let next_task =
                execution
                    .scheduler
                    .apply_report(&effective.document, report, transition_at_ms)?;
            if report_state == crate::plan::PlanTaskState::Complete {
                execution.append_lifecycle(
                    crate::plan::ExchangeAnchor::capture(exchange),
                    transition_at_ms,
                    PlanExecutionLifecycleEvent::TaskCompleted {
                        task_path,
                        ordinal: task_index + 1,
                        total: execution.scheduler.task.len(),
                        title: task_title,
                        elapsed_ms,
                    },
                );
            }
            if let Some(next_task) = next_task {
                let next_index = execution
                    .scheduler
                    .task
                    .iter()
                    .position(|task| task.state == crate::plan::PlanTaskState::Active)
                    .context("activated task not found")?;
                let next_task_path = execution.scheduler.task[next_index].task_path.clone();
                execution.append_lifecycle(
                    crate::plan::ExchangeAnchor::capture(exchange),
                    transition_at_ms,
                    PlanExecutionLifecycleEvent::TaskStarted {
                        task_path: next_task_path,
                        ordinal: next_index + 1,
                        total: execution.scheduler.task.len(),
                        title: next_task.title.clone(),
                    },
                );
            }
            event.push(self.event(
                "plan_task_updated",
                json!({ "execution": &execution, "scheduler": &execution.scheduler }),
            )?);
        }
        if output.evidence.structured_complete {
            anyhow::ensure!(
                execution.scheduler.is_complete(),
                "plan cannot complete before every scheduled task completes"
            );
        }
        if execution
            .scheduler
            .task
            .iter()
            .any(|task| task.state == crate::plan::PlanTaskState::Blocked)
        {
            output.evidence.structured_blocked = true;
        }
        self.store.save_plan_execution(&execution)?;
        Ok(false)
    }

    async fn resolve_plan_deviation(
        &mut self,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let deviation_id = required_text(&params, "deviation_id")?;
        let approved = params
            .get("approved")
            .and_then(Value::as_bool)
            .context("approved is required")?;
        let mut deviation = self
            .store
            .list_plan_deviation(&self.session.id)?
            .into_iter()
            .find(|deviation| deviation.id == deviation_id)
            .context("plan deviation not found")?;
        anyhow::ensure!(
            deviation.disposition == PlanDeviationDisposition::Pending,
            "plan deviation does not await review"
        );
        deviation.disposition = if approved {
            PlanDeviationDisposition::UserApproved
        } else {
            PlanDeviationDisposition::Rejected
        };
        deviation.resolved_at_ms = Some(self.clock.now_ms());
        self.store
            .save_plan_deviation(&self.session.id, &deviation)?;
        let mut event =
            vec![self.event("plan_deviation_resolved", serde_json::to_value(&deviation)?)?];
        if approved {
            let (result, mut resume_event) = self.resume_goal().await?;
            event.append(&mut resume_event);
            Ok((result, event))
        } else {
            let mut goal = self.active_goal()?;
            goal.state = GoalState::Blocked;
            goal.updated_at_ms = self.clock.now_ms();
            self.store.save_goal(&goal)?;
            self.sync_plan_execution(&goal)?;
            event.push(self.event("goal_changed", serde_json::to_value(&goal)?)?);
            Ok((serde_json::to_value(goal)?, event))
        }
    }

    fn sync_plan_execution(&mut self, goal: &GoalRecord) -> Result<()> {
        let Some(mut execution) = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .find(|execution| execution.goal_id == goal.id)
        else {
            return Ok(());
        };
        if goal.state == GoalState::Active && execution.state != PlanExecutionState::Active {
            execution.scheduler.resume_blocked_task();
        }
        execution.state = match goal.state {
            GoalState::Active => PlanExecutionState::Active,
            GoalState::Paused | GoalState::UsageLimited | GoalState::BudgetLimited => PlanExecutionState::Paused,
            GoalState::Cleared => PlanExecutionState::Cancelled,
            GoalState::Complete => PlanExecutionState::Complete,
            GoalState::Blocked => PlanExecutionState::Blocked,
            GoalState::Stalled => PlanExecutionState::Stalled,
        };
        if matches!(
            execution.state,
            PlanExecutionState::Complete
                | PlanExecutionState::Blocked
                | PlanExecutionState::Stalled
                | PlanExecutionState::Cancelled
        ) {
            execution.completed_at_ms.get_or_insert(goal.updated_at_ms);
        } else {
            execution.completed_at_ms = None;
        }
        self.store.save_plan_execution(&execution)?;
        let resolution_kind = match execution.state {
            PlanExecutionState::Complete => Some(PlanResolutionKind::Completed),
            PlanExecutionState::Blocked | PlanExecutionState::Stalled => {
                Some(PlanResolutionKind::Blocked)
            }
            PlanExecutionState::Cancelled => Some(PlanResolutionKind::Cancelled),
            PlanExecutionState::Active | PlanExecutionState::Paused => None,
        };
        if let Some(kind) = resolution_kind {
            self.finalize_plan_resolution(&execution, kind)?;
        }
        Ok(())
    }

    fn finalize_plan_resolution(
        &mut self,
        execution: &PlanExecutionRecord,
        kind: PlanResolutionKind,
    ) -> Result<()> {
        let now_ms = execution.completed_at_ms.context("plan resolution requires a settled execution")?;
        if self
            .store
            .list_plan_resolution(&self.session.id)?
            .iter()
            .any(|resolution| resolution.execution_id == execution.id
                && resolution.kind == kind && resolution.resolved_at_ms == now_ms)
        {
            return Ok(());
        }
        let plan = self
            .store
            .load_plan(&execution.plan_id)?
            .context("resolved plan record is missing")?;
        let accepted_revision = plan
            .accepted_revision
            .context("resolved plan accepted revision is missing")?;
        let accepted = self.plan_file.read_submitted_document(
            &self.session.id,
            &plan.id,
            accepted_revision,
        )?;
        let deviation_list = self
            .store
            .list_plan_deviation(&self.session.id)?
            .into_iter()
            .filter(|deviation| deviation.execution_id == execution.id)
            .collect::<Vec<_>>();
        let effective = crate::plan::build_effective_plan(&accepted, &deviation_list)?;
        let audit = crate::plan::build_plan_audit(
            Uuid::new_v4().to_string(),
            execution.id.clone(),
            &effective.document,
            &execution.scheduler,
            &deviation_list,
            now_ms,
        );
        self.store.save_plan_audit(&self.session.id, &audit)?;
        let mut resolution = crate::plan::build_plan_resolution(
            Uuid::new_v4().to_string(),
            self.session.id.clone(),
            accepted_revision,
            kind,
            crate::plan::PlanResolutionEvidence {
                scheduler: &execution.scheduler,
                deviation_list: &deviation_list,
                audit: &audit,
            },
            now_ms,
        )?;
        resolution.anchor = Some(crate::plan::ExchangeAnchor::capture(
            &self.store.list_exchange(&self.session.id)?.into_iter().rev()
                .find(|exchange| exchange.execution_id.as_deref() == Some(&execution.id))
                .context("plan resolution has no owning exchange")?));
        self.store.save_plan_resolution(&resolution)
    }

    fn active_goal(&self) -> Result<GoalRecord> {
        let goal_id = self.session.goal_id.as_deref().context("no active goal")?;
        self.store
            .load_goal(goal_id)?
            .context("active goal record is missing")
    }

    fn current_goal(&self) -> Result<Option<GoalRecord>> {
        self.session
            .goal_id
            .as_deref()
            .map(|goal_id| self.store.load_goal(goal_id))
            .transpose()
            .map(Option::flatten)
    }

    async fn pause_goal_after_turn_failure(&mut self) -> Result<()> {
        let Some(mut goal) = self.current_goal()? else {
            return Ok(());
        };
        if goal.state != GoalState::Active {
            return Ok(());
        }
        let _ = self.sync_native_goal(&goal, "paused").await;
        goal.state = GoalState::Paused;
        goal.updated_at_ms = self.clock.now_ms();
        self.store.save_goal(&goal)?;
        self.sync_plan_execution(&goal)
    }

    async fn sync_native_goal(&mut self, goal: &GoalRecord, status: &str) -> Result<()> {
        if !goal.native || self.session.backend_session_id.is_none() {
            return Ok(());
        }
        self.backend
            .goal_status(
                self.backend_request(BackendInput::from_text(""), PromptMode::GoalContinuation),
                Some(goal.objective.clone()),
                status,
            )
            .await
    }

    fn save_exchange_comment(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let mut comment: ExchangeComment = serde_json::from_value(params)?;
        anyhow::ensure!(
            self.store
                .list_exchange(&self.session.id)?
                .iter()
                .any(|interaction| interaction.id == comment.exchange_id),
            "interaction comment does not belong to the active session"
        );
        comment.created_at_ms = self.clock.now_ms();
        self.store.save_exchange_comment(&comment)?;
        Ok((serde_json::to_value(comment)?, Vec::new()))
    }

    fn list_exchange_review(&self) -> Result<Vec<Exchange>> {
        let mut interaction_list = self.store.list_exchange(&self.session.id)?;
        for interaction in &mut interaction_list {
            interaction.comment = self.store.list_exchange_comment(&interaction.id)?;
        }
        Ok(interaction_list)
    }

    async fn request_exchange_changes(
        &mut self,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let exchange_id = required_text(&params, "exchange_id")?;
        let interaction = self
            .store
            .list_exchange(&self.session.id)?
            .into_iter()
            .find(|item| item.id == exchange_id)
            .context("interaction not found")?;
        let comment = self.store.list_exchange_comment(&interaction.id)?;
        let prompt = format!(
            "Address this review of interaction {}. Apply every requested change while preserving unrelated work.\n\nOriginal prompt:\n{}\n\nRecorded diff:\n{}\n\nReview comments:\n{}",
            interaction.ordinal,
            interaction.prompt,
            interaction.checkpoint_diff_text.unwrap_or_default(),
            serde_json::to_string_pretty(&comment)?
        );
        self.run_interaction(
            prompt,
            PromptMode::RequestChanges,
            Some(ExchangeAdmission::chat(format!(
                "Request changes for interaction {}",
                interaction.ordinal
            ))),
        )
        .await
    }

    async fn rollback_exchange(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let exchange_id = required_text(&params, "exchange_id")?;
        let exchange_id = self
            .store
            .checkpoint_owner(&self.session.id, &exchange_id)?;
        let mut interaction = self.store.list_exchange(&self.session.id)?;
        let target_index = interaction
            .iter()
            .position(|item| item.id == exchange_id)
            .context("interaction not found")?;
        anyhow::ensure!(
            interaction[target_index].disposition == crate::exchange::HistoryDisposition::Current,
            "selected exchange no longer belongs to the current workspace history"
        );
        anyhow::ensure!(
            interaction
                .iter()
                .skip(target_index)
                .all(|exchange| exchange.completed_at_ms.is_some()),
            "rollback requires settled exchanges"
        );
        self.store.require_settled_exchange_tree(
            &self.session.id,
            &interaction
                .iter()
                .skip(target_index)
                .map(|exchange| exchange.id.clone())
                .collect::<Vec<_>>(),
        )?;
        let expected_id = interaction
            .iter()
            .rev()
            .find(|item| item.disposition != crate::exchange::HistoryDisposition::Superseded)
            .and_then(|item| match item.disposition {
                crate::exchange::HistoryDisposition::Current => item.checkpoint_after.as_deref(),
                crate::exchange::HistoryDisposition::RolledBack => {
                    item.checkpoint_before.as_deref()
                }
                crate::exchange::HistoryDisposition::Superseded => None,
            })
            .context("latest interaction has no checkpoint")?;
        let target_id = interaction[target_index]
            .checkpoint_before
            .as_deref()
            .context("selected interaction has no checkpoint")?;
        let expected = self
            .store
            .load_checkpoint(expected_id)?
            .context("latest checkpoint is missing")?;
        let target = self
            .store
            .load_checkpoint(target_id)?
            .context("target checkpoint is missing")?;
        let WorkspaceKind::Git(workspace) = &self.workspace_kind else {
            anyhow::bail!("rollback is unavailable because this session has NO CHECKPOINT")
        };
        GitCheckpoint::new(workspace)
            .admit_restore(Arc::clone(&self.repositories), expected, target)
            .await?
            .restore(self.store.objects.clone())
            .await?;
        self.store.record_rollback(
            &self.session.id,
            &interaction
                .iter()
                .skip(target_index)
                .map(|exchange| exchange.id.clone())
                .collect::<Vec<_>>(),
        )?;
        interaction = self.store.list_exchange(&self.session.id)?;
        if self.session.goal_id.is_some() {
            let mut goal = self.active_goal()?;
            self.sync_native_goal(&goal, "cleared").await?;
            goal.state = GoalState::Cleared;
            goal.updated_at_ms = self.clock.now_ms();
            self.store.save_goal(&goal)?;
            self.session.goal_id = None;
        }
        self.save_session()?;
        Ok((
            json!({ "rolled_back": interaction[target_index].id }),
            vec![self.event(
                "interaction_rolled_back",
                serde_json::to_value(&interaction[target_index])?,
            )?],
        ))
    }

    async fn new_session(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let now_ms = self.clock.now_ms();
        let child = prepare_new_session(
            &self.data_root,
            &self.client_id,
            &self.backend_launch.kind,
            self.backend.descriptor().capability.native_compact,
            &self.session.id,
            &params,
            now_ms,
        )?;
        let snapshot = self.snapshot_for_session(child.clone())?;
        Ok((
            serde_json::to_value(snapshot)?,
            vec![SessionEvent {
                session_id: child.id.clone(),
                event: "session_created".into(),
                payload: serde_json::to_value(child)?,
            }],
        ))
    }

    fn list_session(&self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let scope = params
            .get("scope")
            .and_then(Value::as_str)
            .unwrap_or("repo");
        let session = if scope == "all" {
            self.store.list_session(None)?
        } else {
            let workspace = params
                .get("workspace")
                .and_then(Value::as_str)
                .map(Path::new)
                .map(crate::workspace::resolve)
                .transpose()?
                .map(|kind| match kind {
                    WorkspaceKind::Git(path) | WorkspaceKind::Untracked(path) => {
                        path.to_string_lossy().into_owned()
                    }
                })
                .unwrap_or_else(|| self.session.workspace.clone());
            self.store.list_session(Some(&workspace))?
        };
        Ok((serde_json::to_value(session)?, Vec::new()))
    }

    fn preview_session(&self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        let preview_session = self
            .store
            .load_session(&session_id)?
            .context("session not found")?;
        let interaction = self.store.list_exchange(&session_id)?;
        let plan_list = self.store.list_plan(&session_id)?;
        let agent_run_list = self
            .store
            .list_agent_run(&session_id)?
            .into_iter()
            .filter(|agent| !agent.is_primary())
            .collect::<Vec<_>>();
        let mut agent_exchange_list = Vec::new();
        for run in &agent_run_list {
            agent_exchange_list.extend(self.store.list_agent_exchange(&run.id)?);
        }
        let timeline = TimelineProjector::build(TimelineProjection {
            interaction_list: interaction.clone(),
            plan_list: &plan_list,
            lifecycle_list: self.store.list_plan_lifecycle(&session_id)?,
            execution_list: self.store.list_plan_execution(&session_id)?,
            deviation_list: self.store.list_plan_deviation(&session_id)?,
            audit_list: self.store.list_plan_audit(&session_id)?,
            resolution_list: self.store.list_plan_resolution(&session_id)?,
            agent_run_list: agent_run_list.clone(),
            agent_exchange_list: agent_exchange_list.clone(),
            session_event_list: self.store.list_session_event(&session_id)?,
            plan_file: &self.plan_file,
        })?;
        let preview = SessionPreview {
            session: preview_session,
            exchange: interaction,
            timeline,
            agent: AgentSnapshot {
                definition: Vec::new(),
                run: agent_run_list,
                exchange: agent_exchange_list,
            },
        };
        Ok((serde_json::to_value(preview)?, Vec::new()))
    }

    async fn resume_session(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        let mut session = self
            .store
            .load_session(&session_id)?
            .context("session not found")?;
        anyhow::ensure!(
            session.workspace == self.session.workspace,
            "cross-worktree resume requires native fork"
        );
        anyhow::ensure!(
            session.backend == self.backend_launch.kind,
            "session uses a different configured backend"
        );
        if session.id == self.session.id {
            return Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()));
        }
        self.pause_current_goal().await?;
        session =
            self.store
                .acquire_session_lease(&session.id, &self.client_id, self.clock.now_ms())?;
        self.release_lease()?;
        if session.provider_label.is_empty() {
            session.provider_label = self.backend.descriptor().label;
        }
        self.store.save_session(&session)?;
        self.exchange_runtime = None;
        self.store.interrupt_detached_execution(&session.id)?;
        self.session = session;
        self.agent_registry = load_agent_registry(&self.store, &self.session.id)?;
        self.child_exchange_runtime_by_agent.clear();
        self.capability.native_fork = self.session.native_fork;
        self.capability.native_compact = self.session.native_compact;
        self.capability = self.backend.descriptor().capability;
        self.capability.native_fork = self.session.native_fork || self.capability.native_fork;
        self.capability.native_compact =
            self.session.native_compact || self.capability.native_compact;
        Ok((
            serde_json::to_value(self.snapshot()?)?,
            vec![self.event("session_changed", serde_json::to_value(&self.session)?)?],
        ))
    }

    async fn pause_current_goal(&mut self) -> Result<()> {
        let Some(goal_id) = self.session.goal_id.clone() else {
            return Ok(());
        };
        let Some(mut goal) = self.store.load_goal(&goal_id)? else {
            return Ok(());
        };
        if goal.state != GoalState::Active {
            return Ok(());
        }
        self.sync_native_goal(&goal, "paused").await?;
        goal.state = GoalState::Paused;
        goal.updated_at_ms = self.clock.now_ms();
        self.store.save_goal(&goal)
    }

    fn rename_session(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        let name = required_text(&params, "name")?.trim().to_owned();
        let mut session = self
            .store
            .load_session(&session_id)?
            .context("session not found")?;
        if let Some(expected) = params.get("expected_name").and_then(Value::as_str) {
            anyhow::ensure!(session.name == expected, "Session name changed during generation");
        }
        session.name = name;
        let created_at_ms = self.clock.now_ms();
        session.updated_at_ms = created_at_ms;
        self.store.save_session(&session)?;
        let timeline_event = SessionEventRecord {
            id: Uuid::new_v4().to_string(),
            session_id: session.id.clone(),
            created_at_ms,
            detail: SessionEventKind::Renamed {
                name: session.name.clone(),
            },
        };
        self.store.save_session_event(&timeline_event)?;
        if self.session.id == session.id {
            self.session = session.clone();
        }
        Ok((serde_json::to_value(session)?, Vec::new()))
    }

    async fn configure_session(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        if params.get("fast_mode").and_then(Value::as_bool).is_some() {
            anyhow::ensure!(
                self.capability.native_goal,
                "fast mode requires the Codex backend"
            );
        }
        let requested_model = params.get("model").and_then(Value::as_str);
        let requested_effort = params.get("effort").and_then(Value::as_str);
        let requested_context_window = params.get("context_window").and_then(Value::as_str);
        let validate_selection = params.get("validate").and_then(Value::as_bool) == Some(true);
        if validate_selection
            && (requested_model.is_some()
                || requested_effort.is_some()
                || requested_context_window.is_some())
        {
            let model_list = self
                .backend
                .model_list(self.backend_request(BackendInput::from_text(""), PromptMode::Chat))
                .await?;
            let target_model = requested_model
                .and_then(|model| model_list.iter().find(|candidate| candidate.id == model))
                .or_else(|| {
                    let active_model = self
                        .session
                        .resolved_model
                        .as_deref()
                        .unwrap_or(&self.session.model);
                    model_list
                        .iter()
                        .find(|candidate| candidate.id == active_model)
                })
                .or_else(|| model_list.iter().find(|candidate| candidate.is_default));
            if let Some(model) = requested_model {
                anyhow::ensure!(
                    target_model.is_some_and(|candidate| candidate.id == model),
                    "model {model} is unavailable for this backend"
                );
            }
            if let Some(effort) = requested_effort {
                let model =
                    target_model.context("the active model is unavailable for this backend")?;
                if !model.reasoning.is_empty() {
                    anyhow::ensure!(
                        model.reasoning.iter().any(|candidate| candidate == effort),
                        "reasoning effort {effort} is unavailable for model {}",
                        model.id
                    );
                }
            }
            if let Some(context_window) = requested_context_window {
                let model =
                    target_model.context("the active model is unavailable for this backend")?;
                if !model.context_window.is_empty() {
                    anyhow::ensure!(
                        model
                            .context_window
                            .iter()
                            .any(|candidate| candidate.id == context_window),
                        "context window {context_window} is unavailable for model {}",
                        model.id
                    );
                }
            }
        }
        if let Some(model) = params.get("model").and_then(Value::as_str) {
            anyhow::ensure!(!model.trim().is_empty(), "model cannot be empty");
            self.session.model = model.to_owned();
            self.session.resolved_model = (model != "default").then(|| model.to_owned());
        }
        if let Some(effort) = params.get("effort").and_then(Value::as_str) {
            anyhow::ensure!(!effort.trim().is_empty(), "effort cannot be empty");
            self.session.effort = effort.to_owned();
        }
        if params.get("context_window").is_some() {
            self.session.context_window = params
                .get("context_window")
                .and_then(Value::as_str)
                .map(str::to_owned);
        }
        if let Some(fast_mode) = params.get("fast_mode").and_then(Value::as_bool) {
            self.session.fast_mode = fast_mode;
        }
        self.save_session()?;
        self.save_preference()?;
        Ok((
            serde_json::to_value(&self.session)?,
            vec![self.event("session_configured", serde_json::to_value(&self.session)?)?],
        ))
    }

    fn delete_session(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        anyhow::ensure!(
            session_id != self.session.id,
            "cannot delete the active session"
        );
        let session = self
            .store
            .load_session(&session_id)?
            .context("session not found")?;
        self.plan_file.delete_session(&session.id)?;
        self.store.delete_session(&session_id)?;
        Ok((
            json!({ "deleted": session_id, "provider_session_preserved": true }),
            Vec::new(),
        ))
    }

    async fn compact_session(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            self.capability.native_compact,
            "current backend does not support context compaction"
        );
        anyhow::ensure!(
            self.session.backend_session_id.is_some(),
            "current session has no provider conversation to compact"
        );
        let output = self
            .backend
            .compact(self.backend_request(BackendInput::from_text(""), PromptMode::Chat))
            .await?;
        self.capability = output.capability.clone();
        self.session.native_fork = output.capability.native_fork;
        self.session.native_compact = output.capability.native_compact;
        if let Some(backend_session_id) = output.backend_session_id {
            self.session.backend_session_id = Some(backend_session_id);
        }
        if let Some(context_usage) = output.metrics.context_usage {
            self.session.context_usage = Some(context_usage);
        }
        if !output.runtime.provider.is_empty() {
            self.session.provider_label = output.runtime.provider;
        }
        if output.runtime.model.is_some() {
            self.session.resolved_model = output.runtime.model;
        }
        self.save_session()?;
        let snapshot = self.snapshot()?;
        Ok((
            serde_json::to_value(&snapshot)?,
            vec![self.event("context_compacted", serde_json::to_value(snapshot)?)?],
        ))
    }

    fn snapshot_for_session(&mut self, session: HarnessSession) -> Result<BrokerSnapshot> {
        let parent_session = std::mem::replace(&mut self.session, session);
        let child_workspace = crate::workspace::resolve(Path::new(&self.session.workspace))?;
        let parent_workspace = std::mem::replace(&mut self.workspace_kind, child_workspace);
        let child_registry = load_agent_registry(&self.store, &self.session.id)?;
        let parent_registry = std::mem::replace(&mut self.agent_registry, child_registry);
        let parent_runtime = self.exchange_runtime.take();
        let snapshot = self.snapshot();
        self.session = parent_session;
        self.workspace_kind = parent_workspace;
        self.agent_registry = parent_registry;
        self.exchange_runtime = parent_runtime;
        snapshot
    }

    fn backend_request(&self, input: BackendInput, mode: PromptMode) -> BackendRequest {
        BackendRequest {
            harness_session_id: self.session.id.clone(),
            workspace: self.session.workspace.clone(),
            input,
            mode,
            model: self.session.model.clone(),
            effort: self.session.effort.clone(),
            context_window: self.session.context_window.clone(),
            fast_mode: self.session.fast_mode,
            execution_mode: self.session.execution_mode,
            backend_session_id: self.session.backend_session_id.clone(),
            control_context: self.control_turn_context(mode),
        }
    }

    fn event(&self, name: &str, payload: Value) -> Result<SessionEvent> {
        Ok(SessionEvent {
            session_id: self.session.id.clone(),
            event: name.into(),
            payload,
        })
    }

    fn save_session(&mut self) -> Result<()> {
        let now_ms = self.clock.now_ms();
        self.session.acquire_lease(&self.client_id, now_ms)?;
        self.session.updated_at_ms = now_ms;
        self.store
            .save_owned_session(&self.session, &self.client_id)
    }

    fn save_preference(&mut self) -> Result<()> {
        let previous = self
            .store
            .load_preference(&self.session.workspace, &self.session.backend)?;
        self.store.save_preference(
            &self.session.workspace,
            &self.session.backend,
            &preference_for_session(&self.session, previous),
        )
    }

    fn refresh_lease(&mut self) -> Result<()> {
        let now_ms = self.clock.now_ms();
        let persisted =
            self.store
                .acquire_session_lease(&self.session.id, &self.client_id, now_ms)?;
        self.session.lease_owner = persisted.lease_owner;
        self.session.lease_expires_at_ms = persisted.lease_expires_at_ms;
        self.save_session()
    }

    pub(crate) fn release_lease(&mut self) -> Result<()> {
        self.store
            .release_session_lease(&self.session.id, &self.client_id)
    }

    /// Resolve the durable data root for diagnostics and manual verification.
    pub fn data_root(&self) -> &Path {
        &self.data_root
    }

    /// Record plan lifecycle metadata without persisting prompts or plan content.
    fn record_plan_trace(&self, event: &str, fields: Value) {
        self.trace.record(
            &self.session.id,
            event,
            json!({
                "active_plan_id": self.session.active_plan_id,
                "fields": fields,
            }),
        );
    }

    fn configure_trace(&self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let enabled = params
            .get("enabled")
            .and_then(Value::as_bool)
            .context("trace enabled is required")?;
        Ok((
            serde_json::to_value(self.trace.configure(enabled)?)?,
            Vec::new(),
        ))
    }

    /// Resolve durable lease identity for the broker heartbeat.
    pub fn lease_identity(&self) -> (PathBuf, String, String) {
        (
            self.data_root.clone(),
            self.session.id.clone(),
            self.client_id.clone(),
        )
    }
}

impl Drop for HarnessBroker {
    fn drop(&mut self) {
        let _ = self.release_lease();
    }
}

fn acquire_lease(session: &mut HarnessSession, client_id: &str, now_ms: i64) -> Result<()> {
    session.acquire_lease(client_id, now_ms)
}

/// Attribute a native tool once to the single active task in its owning exchange.
fn attribute_provider_tool(
    exchange: &mut Exchange,
    task: &mut TaskTracker,
    event: &BackendEvent,
) -> Result<()> {
    let (Some(address), Some(activity), Some(task_id)) = (
        event.address.as_ref(),
        event.activity.as_ref(),
        task.attribution_target().map(str::to_owned),
    ) else {
        return Ok(());
    };
    if exchange
        .turn_mut(address)?
        .attribute_tool(&activity.id, &task_id)?
    {
        task.mark_attributed(&task_id);
        exchange.task = Some(task.snapshot().clone());
    }
    Ok(())
}

fn admit_addressless_turn(
    exchange: &mut Exchange,
    synthetic_turn: &mut Option<crate::backend::ProviderAddress>,
    event: &mut BackendEvent,
    now_ms: i64,
) -> Result<()> {
    let owns_content = event.activity.is_some()
        || matches!(event.kind.as_str(), "assistant_message" | "reasoning")
            && event.text.as_deref().is_some_and(|text| !text.is_empty());
    if event.address.is_some() || !owns_content {
        return Ok(());
    }
    let address = synthetic_turn
        .get_or_insert_with(|| crate::backend::ProviderAddress {
            thread_id: format!("harness:{}", exchange.agent_id),
            turn_id: format!("{}:provider:{}", exchange.id, exchange.turn.len() + 1),
        })
        .clone();
    if exchange.turn.iter().all(|turn| turn.provider() != &address) {
        exchange.start_turn(address.clone(), now_ms)?;
    }
    event.address = Some(address);
    Ok(())
}

fn finish_synthetic_turn(
    exchange: &mut Exchange,
    synthetic_turn: Option<&crate::backend::ProviderAddress>,
    outcome: crate::turn::TurnOutcome,
    now_ms: i64,
) -> Result<()> {
    if let Some(address) = synthetic_turn {
        exchange.turn_mut(address)?.finish(outcome, now_ms)?;
    }
    Ok(())
}

/// Collect exact child exchanges for recursive change attribution.
fn referenced_child_exchange_list(interaction: &Exchange) -> Vec<(String, String)> {
    interaction
        .node_list
        .iter()
        .filter_map(|node| match node {
            ExchangeNode::AgentReference { agent } => Some((
                agent.child_agent_id.clone(),
                agent.child_exchange_id.clone(),
            )),
            _ => None,
        })
        .collect()
}

fn load_agent_registry(store: &SqliteStore, session_id: &str) -> Result<AgentRegistry> {
    Ok(AgentRegistry::from_run_list(
        store.list_agent_run(session_id)?,
    ))
}

fn preference_for_session(
    session: &HarnessSession,
    previous: Option<HarnessPreference>,
) -> HarnessPreference {
    let mut model_setting = previous
        .map(|preference| preference.model_setting)
        .unwrap_or_default();
    model_setting.insert(
        session.model.clone(),
        ModelSetting {
            reasoning: Some(session.effort.clone()),
            context_window: session.context_window.clone(),
        },
    );
    HarnessPreference {
        model: session.model.clone(),
        effort: session.effort.clone(),
        model_setting,
        fast_mode: session.fast_mode,
    }
}

fn required_text(value: &Value, field: &str) -> Result<String> {
    value
        .get(field)
        .and_then(Value::as_str)
        .map(str::to_owned)
        .with_context(|| format!("{field} is required"))
}

fn plan_trace_fields(plan: &PlanRecord) -> Value {
    json!({
        "plan_id": plan.id,
        "state": format!("{:?}", plan.state),
        "document_version": plan.document_version,
        "submitted_version": plan.submitted_version,
        "model_revision": plan.model_revision,
        "user_revision": plan.user_revision,
        "has_elicitation": plan.elicitation.is_some(),
        "resolved_question_count": plan.question_ledger.resolution.len(),
        "generation_turn": plan.generation.budget.turn_count,
        "generation_max_turn": plan.generation.budget.max_turn_count,
        "generation_no_progress": plan.generation.budget.consecutive_no_progress,
        "generation_max_no_progress": plan.generation.budget.max_consecutive_no_progress,
        "canonical_revision": plan.generation.canonical_revision,
    })
}

fn resolve_fork_name(
    params: &Value,
    source_name: &str,
    existing_name_set: &HashSet<String>,
) -> String {
    params
        .get("name")
        .and_then(Value::as_str)
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(str::to_owned)
        .unwrap_or_else(|| {
            let base_name = if source_name.is_empty() {
                "fork".to_owned()
            } else {
                format!("{source_name}-fork")
            };
            (0_u64..)
                .map(|ordinal| format!("{base_name}-{ordinal}"))
                .find(|candidate| !existing_name_set.contains(candidate))
                .expect("fork ordinal space is unbounded")
        })
}

fn goal_state_name(state: GoalState) -> &'static str {
    match state {
        GoalState::Active => "active",
        GoalState::Paused => "paused",
        GoalState::Complete => "complete",
        GoalState::Blocked => "blocked",
        GoalState::UsageLimited => "usage_limited",
        GoalState::BudgetLimited => "budget_limited",
        GoalState::Stalled => "stalled",
        GoalState::Cleared => "cleared",
    }
}

fn default_model() -> String {
    "default".into()
}
fn default_effort() -> String {
    "medium".into()
}
fn default_goal_max_turns() -> u32 {
    20
}

#[cfg(test)]
mod test {
    use super::*;

    /// Render persisted timeline state through the native document projection.
    fn timeline_text(snapshot: &BrokerSnapshot) -> String {
        snapshot.timeline.iter().flat_map(|entry| {
            let rendered = crate::buffer::projection::project(entry, &Default::default(), false, &Default::default()).unwrap();
            rendered.entry.block.into_iter().flat_map(|block|
                (0..block.text.row_count()).map(|row| block.text.row(row).unwrap().to_owned()).collect::<Vec<_>>()
            ).collect::<Vec<_>>()
        }).collect::<Vec<_>>().join("\n")
    }

    #[test]
    fn resolves_explicit_named_and_unnamed_fork_names() {
        let existing_name_set = HashSet::from(["review-fork-0".to_owned()]);
        assert_eq!(
            resolve_fork_name(
                &json!({ "name": "  investigation  " }),
                "review",
                &existing_name_set,
            ),
            "investigation"
        );
        assert_eq!(
            resolve_fork_name(&json!({}), "review", &existing_name_set),
            "review-fork-1"
        );
        assert_eq!(resolve_fork_name(&json!({}), "", &HashSet::new()), "fork-0");
    }

    fn completed_interaction(id: &str, session_id: &str, node_list: Vec<ExchangeNode>) -> Exchange {
        Exchange {
            mode: None,
            finalization_error: None,
            finalization_outcome: None,
            agent_id: HarnessSession::primary_agent_id(session_id),
            id: id.into(),
            session_id: session_id.into(),
            ordinal: 1,
            prompt: "test interaction".into(),
            kind: ExchangeKind::Chat,
            plan_id: None,
            execution_id: None,
            goal_id: None,
            state: ExchangeState::Complete,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            disposition: crate::exchange::HistoryDisposition::Current,
            turn: Vec::new(),
            created_at_ms: 1,
            completed_at_ms: Some(2),
            node_list,
            awaiting_input: false,
            elicitation: None,
            duration_ms: 1,
            execution_started_at_ms: None,
            token_count: None,
            comment: Vec::new(),
            task: None,
        }
    }

    struct SaturatingBackend {
        completed: bool,
        wait_for_capacity: bool,
        cancelled: Arc<AtomicBool>,
    }

    #[async_trait::async_trait]
    impl Backend for SaturatingBackend {
        async fn prompt_stream(
            &self,
            _request: BackendRequest,
            event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let event_sink = event_sink.unwrap();
            for sequence in 0..384 {
                let event = BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "assistant_message".into(),
                    text: Some(sequence.to_string()),
                    data: Value::Null,
                    activity: None,
                    summary: None,
                    task_update: None,
                };
                if self.wait_for_capacity {
                    event_sink.send_wait(event).await?;
                } else {
                    let _ = event_sink.send(event);
                }
            }
            if self.completed {
                Ok(crate::backend::BackendOutput::default())
            } else {
                std::future::pending().await
            }
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            self.cancelled.store(true, Ordering::Release);
            Ok(())
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("fork unavailable")
        }
    }

    #[tokio::test]
    async fn event_saturation_cancels_pending_and_completed_provider_turns() {
        for completed in [false, true] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = HarnessBroker::initialize_with_clock(
                InitializeRequest {
                    data_root: data.path().to_string_lossy().into_owned(),
                    permission_file: None,
                    workspace: repository.path().to_string_lossy().into_owned(),
                    client_id: "saturation-test".into(),
                    backend: BackendLaunch {
                        kind: "mock".into(),
                        command: vec!["mock".into()],
                    },
                    model: "mock-model".into(),
                    effort: "low".into(),
                    session_id: None,
                    new_session_name: None,
                    goal_max_turns: 20,
                    lease_conflict_action: None,
                },
                Box::new(FixedClock(200)),
            )
            .unwrap();
            let cancelled = Arc::new(AtomicBool::new(false));
            broker.backend = Arc::new(SaturatingBackend {
                completed,
                wait_for_capacity: false,
                cancelled: Arc::clone(&cancelled),
            });
            let result = tokio::time::timeout(
                std::time::Duration::from_secs(5),
                broker.dispatch(Request {
                    id: 1,
                    method: "goal.set".into(),
                    params: json!({ "objective": "test bounded provider delivery" }),
                }),
            )
            .await
            .expect("saturation must terminate the affected turn");
            let error = result
                .response
                .error()
                .expect("saturation must not report success");
            assert!(error.message.contains("event delivery failed"), "{error:?}");
            assert!(cancelled.load(Ordering::Acquire));
            assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
            let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
            assert_eq!(interaction.len(), 1);
            assert_eq!(interaction[0].state, ExchangeState::Failed);
        }
    }

    #[tokio::test]
    async fn burst_delivery_drains_both_queues_and_keeps_the_next_prompt_usable() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "burst-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(200)),
        )
        .unwrap();
        let cancelled = Arc::new(AtomicBool::new(false));
        broker.backend = Arc::new(SaturatingBackend {
            completed: true,
            wait_for_capacity: true,
            cancelled: Arc::clone(&cancelled),
        });
        for request_id in 1..=2 {
            let (sink, mut stream) = crate::backend::events::channel();
            let dispatch = broker.dispatch_stream(
                Request {
                    id: request_id,
                    method: "prompt.submit".into(),
                    params: json!({ "text": "burst then final answer" }),
                },
                sink,
            );
            tokio::pin!(dispatch);
            // Hold the consumer until the producer reaches bounded backpressure.
            assert!(
                std::future::poll_fn(|context| {
                    std::task::Poll::Ready(std::future::Future::poll(dispatch.as_mut(), context))
                })
                .await
                .is_pending()
            );
            let consume = async move {
                let mut sequence = 0;
                while let Some(event) = stream.recv().await.unwrap() {
                    if event.kind == "assistant_message" {
                        assert_eq!(event.text, Some(sequence.to_string()));
                        sequence += 1;
                    }
                }
                assert_eq!(sequence, 384);
            };
            let (result, ()) = tokio::time::timeout(std::time::Duration::from_secs(10), async {
                tokio::join!(dispatch, consume)
            })
            .await
            .expect("bounded delivery must make progress");
            assert!(
                result.response.error().is_none(),
                "{:?}",
                result.response.error()
            );
        }
        assert!(!cancelled.load(Ordering::Acquire));
        let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchange.len(), 2);
        assert!(
            exchange
                .iter()
                .all(|exchange| exchange.state == ExchangeState::Complete)
        );
        assert!(exchange.iter().all(|exchange| {
            exchange
                .turn
                .iter()
                .flat_map(|turn| turn.messages())
                .any(|message| message.text.ends_with("383"))
        }));
    }

    struct RestartBackend {
        cancellation: Arc<TurnCancellation>,
        invocation: std::sync::atomic::AtomicUsize,
    }

    #[async_trait::async_trait]
    impl Backend for RestartBackend {
        async fn prompt_stream(
            &self,
            _request: BackendRequest,
            event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let invocation = self.invocation.fetch_add(1, Ordering::SeqCst);
            let mut event = BackendEvent {
                address: Some(crate::backend::ProviderAddress {
                    thread_id: "restart-thread".into(),
                    turn_id: format!("turn-{invocation}"),
                }),
                turn_boundary: Some(crate::backend::TurnBoundary::Started),
                kind: "turn_started".into(),
                text: None,
                data: Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            };
            let sink = event_sink.context("restart test requires event delivery")?;
            sink.send(event.clone())?;
            if invocation == 0 {
                self.cancellation.request_restart();
                return std::future::pending().await;
            }
            event.kind = "turn_completed".into();
            event.turn_boundary = Some(crate::backend::TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Completed,
            });
            sink.send(event)?;
            Ok(crate::backend::BackendOutput::default())
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("restart test does not fork")
        }
    }

    #[tokio::test]
    async fn mode_restart_preserves_exchange_and_checkpoint_with_distinct_turns() {
        let repository = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.backend = Arc::new(RestartBackend {
            cancellation: Arc::clone(&broker.turn_cancellation),
            invocation: std::sync::atomic::AtomicUsize::new(0),
        });
        let result = broker
            .run_interaction(
                "Original request".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Original request".into())),
            )
            .await;
        assert!(result.is_err());
        let before = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(before.len(), 1);
        assert_eq!(before[0].state, ExchangeState::Running);
        assert_eq!(before[0].turn.len(), 1);
        assert_eq!(
            before[0].turn[0].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Interrupted,
            }
        );
        assert!(before[0].checkpoint_after.is_none());
        broker.turn_cancellation.arm(false);
        broker
            .resume_exchange(json!({"text": "Continue after changing mode"}))
            .await
            .unwrap();
        let after = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(after.len(), 1);
        assert_eq!(after[0].id, before[0].id);
        assert_eq!(after[0].prompt, "Original request");
        assert_eq!(after[0].checkpoint_before, before[0].checkpoint_before);
        assert_eq!(after[0].turn.len(), 2);
        assert_eq!(after[0].turn[0].state(), before[0].turn[0].state());
        assert_eq!(after[0].state, ExchangeState::Complete);
    }

    #[tokio::test]
    async fn planning_restart_retains_the_plan_and_resumes_its_existing_exchange() {
        let repository = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), true);
        broker.backend = Arc::new(RestartBackend {
            cancellation: broker.turn_cancellation(),
            invocation: std::sync::atomic::AtomicUsize::new(0),
        });
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({"text":"/plan Design the change"}),
            })
            .await;
        assert_eq!(result.response.error().unwrap().code, "turn_cancelled");
        let before = broker.snapshot().unwrap();
        assert_eq!(
            before.active_plan.as_ref().unwrap().state,
            PlanState::Generating
        );
        assert_eq!(before.exchange.len(), 1);
        assert_eq!(before.exchange[0].kind, ExchangeKind::PlanDraft);
        broker.backend = Arc::new(PlanningQuestionBackend::new(true));
        broker.turn_cancellation.arm(false);
        broker
            .resume_exchange(json!({"text":"Continue planning"}))
            .await
            .unwrap();
        let after = broker.snapshot().unwrap();
        assert_eq!(after.exchange.len(), 1);
        assert_eq!(after.exchange[0].id, before.exchange[0].id);
        assert_eq!(after.exchange[0].plan_id, before.exchange[0].plan_id);
        assert_eq!(
            after.exchange[0].checkpoint_before,
            before.exchange[0].checkpoint_before
        );
        assert_eq!(after.active_plan.unwrap().state, PlanState::AwaitingInput);
    }

    struct CleanupRetryBackend {
        started: Notify,
        complete: Notify,
        settled: Notify,
        attempt: std::sync::atomic::AtomicUsize,
    }

    #[async_trait::async_trait]
    impl Backend for CleanupRetryBackend {
        async fn prompt_stream(
            &self,
            _request: BackendRequest,
            sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let sink = sink.context("cleanup test requires event delivery")?;
            let mut event: BackendEvent = serde_json::from_value(json!({
                "kind":"turn_started", "text":null, "data":null,
                "address":{"thread_id":"cleanup-thread", "turn_id":"cleanup-turn"},
                "turn_boundary":{"kind":"started"}
            }))?;
            event.turn_boundary = Some(crate::backend::TurnBoundary::Started);
            sink.send(event.clone())?;
            self.started.notify_one();
            self.complete.notified().await;
            event.kind = "turn_completed".into();
            event.turn_boundary = Some(crate::backend::TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Interrupted,
            });
            sink.send(event)?;
            self.settled.notify_one();
            Ok(crate::backend::BackendOutput::default())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            if self.attempt.fetch_add(1, Ordering::SeqCst) == 0 {
                anyhow::bail!("cleanup deadline expired");
            }
            self.complete.notify_one();
            self.settled.notified().await;
            Ok(())
        }
    }

    #[tokio::test]
    async fn cleanup_failure_retains_provider_delivery_until_retry_settles_execution() {
        let repository = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let backend = Arc::new(CleanupRetryBackend {
            started: Notify::new(),
            complete: Notify::new(),
            settled: Notify::new(),
            attempt: std::sync::atomic::AtomicUsize::new(0),
        });
        broker.backend = backend.clone();
        let cancellation = broker.turn_cancellation();
        let session_id = broker.session.id.clone();
        let control = async {
            backend.started.notified().await;
            cancellation.begin_cleanup(false, false).unwrap();
            let error = backend.cleanup_execution(&session_id).await.unwrap_err();
            cancellation.fail_cleanup(&error);
            tokio::time::timeout(std::time::Duration::from_secs(2), async {
                loop {
                    let store = SqliteStore::open(data.path()).unwrap();
                    let exchange = store.list_exchange(&session_id).unwrap();
                    if exchange[0].state == ExchangeState::Finalizing
                        && exchange[0].finalization_error.is_some()
                    {
                        assert!(exchange[0].checkpoint_after.is_none());
                        break;
                    }
                    tokio::task::yield_now().await;
                }
            })
            .await
            .expect("cleanup failure should persist without losing provider delivery");
            cancellation.begin_cleanup(false, false).unwrap();
            backend.cleanup_execution(&session_id).await.unwrap();
            cancellation.request(false);
        };
        let execution = broker.run_interaction(
            "Stop this request".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Stop this request".into())),
        );
        let (result, ()) = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::join!(execution, control)
        })
        .await
        .expect("retry should settle the original execution");
        assert!(result.is_err());
        let exchange = broker.store.list_exchange(&session_id).unwrap();
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].state, ExchangeState::Cancelled);
        assert!(exchange[0].finalization_error.is_none());
        assert_eq!(
            exchange[0].turn[0].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Interrupted,
            }
        );
    }

    #[tokio::test]
    async fn first_prompt_names_unnamed_sessions_without_overwriting_explicit_names() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.session.name.clear();
        broker.interaction_for_turn("  Explain\n this repository  ", true, 100).await.unwrap();
        assert_eq!(broker.session.name, "Explain this repository");
        assert_eq!(broker.store.load_session(&broker.session.id).unwrap().unwrap().name,
            "Explain this repository");
        broker.interaction_for_turn("Another prompt", true, 101).await.unwrap();
        assert_eq!(broker.session.name, "Explain this repository");
        broker.session.name = "My explicit name".into();
        broker.interaction_for_turn("Do not replace this name", true, 102).await.unwrap();
        assert_eq!(broker.session.name, "My explicit name");
    }

    #[tokio::test]
    async fn consecutive_rollbacks_follow_restored_workspace_history() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut exchange_id = Vec::new();
        for content in ["first\n", "second\n"] {
            let (mut exchange, _) = broker
                .interaction_for_turn(content, true, 100)
                .await
                .unwrap();
            broker
                .start_exchange_runtime(&mut exchange, 100)
                .await
                .unwrap();
            std::fs::write(repository.path().join("seed.txt"), content).unwrap();
            broker
                .capture_final_checkpoint(&mut exchange, ExchangeState::Complete)
                .await
                .unwrap();
            exchange_id.push(exchange.id);
            broker.exchange_runtime = None;
        }
        broker
            .rollback_exchange(json!({"exchange_id":exchange_id[1]}))
            .await
            .unwrap();
        assert_eq!(
            std::fs::read_to_string(repository.path().join("seed.txt")).unwrap(),
            "first\n"
        );
        assert!(
            broker
                .rollback_exchange(json!({"exchange_id":exchange_id[1]}))
                .await
                .is_err()
        );
        broker
            .rollback_exchange(json!({"exchange_id":exchange_id[0]}))
            .await
            .unwrap();
        assert_eq!(
            std::fs::read_to_string(repository.path().join("seed.txt")).unwrap(),
            "seed\n"
        );
        let history = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(history[0].state, ExchangeState::Complete);
        assert_eq!(
            history[0].disposition,
            crate::exchange::HistoryDisposition::RolledBack
        );
        assert_eq!(
            history[1].disposition,
            crate::exchange::HistoryDisposition::Superseded
        );
    }

    #[test]
    fn delegation_admission_survives_restart_before_the_child_turn_starts() {
        let repository = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let child = Agent::pending(&broker.session.id, "reviewer", "Review", 1);
        broker.store.save_agent_run(&child).unwrap();
        let mut parent = completed_interaction("parent", &broker.session.id, Vec::new());
        parent.state = ExchangeState::Running;
        parent.completed_at_ms = None;
        parent
            .append_delegation(&child.id, "spawn", "Review", 1)
            .unwrap();
        broker.store.save_exchange(&parent).unwrap();
        let queued = broker.store.list_agent_exchange(&child.id).unwrap();
        assert_eq!(queued.len(), 1);
        assert_eq!(queued[0].state, ExchangeState::Queued);
        assert_eq!(queued[0].ordinal, 1);
        parent
            .append_delegation("missing-agent", "bad", "Invalid", 2)
            .unwrap();
        assert!(broker.store.save_exchange(&parent).is_err());
        assert_eq!(
            broker.store.list_exchange(&broker.session.id).unwrap()[0]
                .node_list
                .len(),
            1
        );
        let mut reopened = SqliteStore::open(data.path()).unwrap();
        reopened
            .interrupt_detached_execution(&broker.session.id)
            .unwrap();
        let recovered = reopened.list_agent_exchange(&child.id).unwrap();
        assert_eq!(recovered[0].state, ExchangeState::Interrupted);
        assert!(recovered[0].turn.is_empty());
        reopened
            .require_settled_exchange_tree(&broker.session.id, &[parent.id])
            .unwrap();
    }

    struct FailingBackend;

    #[async_trait::async_trait]
    impl Backend for FailingBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let nested = Path::new(&request.workspace).join("failed/deep/write.txt");
            std::fs::create_dir_all(nested.parent().context("failed write parent")?)?;
            std::fs::write(&nested, "written before provider failure\n")?;
            anyhow::bail!("synthetic provider failure")
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("synthetic provider failure")
        }
    }

    struct InvalidControlOutputBackend;

    #[async_trait::async_trait]
    impl Backend for InvalidControlOutputBackend {
        async fn prompt_stream(
            &self,
            _request: BackendRequest,
            event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            if let Some(event_sink) = event_sink {
                let _ = event_sink.send(BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "assistant_message".into(),
                    text: Some("Validated the corrected plan.".into()),
                    data: Value::Null,
                    activity: None,
                    summary: None,
                    task_update: None,
                });
            }
            Ok(crate::backend::BackendOutput {
                plan_read: Some("rejected-plan".into()),
                ..crate::backend::BackendOutput::default()
            })
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("fork unavailable")
        }
    }

    struct AdvancingClock(std::sync::atomic::AtomicI64);

    impl Clock for AdvancingClock {
        fn now_ms(&self) -> i64 {
            self.0.fetch_add(10, std::sync::atomic::Ordering::Relaxed)
        }
    }

    struct ParentBoundaryBackend;

    #[async_trait::async_trait]
    impl Backend for ParentBoundaryBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let event_list = vec![
                BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "assistant_message".into(),
                    text: Some("An intermediate answer.".into()),
                    data: Value::Null,
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "parent_boundary".into(),
                    text: None,
                    data: json!({ "boundary": "wait_started", "agent_count": 1 }),
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "parent_boundary".into(),
                    text: None,
                    data: json!({ "boundary": "wait_ended", "agent_count": 0 }),
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "assistant_message".into(),
                    text: Some("A final synthesis.".into()),
                    data: Value::Null,
                    activity: None,
                    summary: None,
                    task_update: None,
                },
            ];
            if let Some(event_sink) = event_sink {
                for event in event_list {
                    let _ = event_sink.send(event);
                }
            }
            Ok(crate::backend::BackendOutput {
                backend_session_id: Some("parent-boundary".into()),
                runtime: crate::backend::BackendRuntime {
                    provider: "Parent boundary test".into(),
                    model: Some(request.model),
                },
                ..crate::backend::BackendOutput::default()
            })
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("parent boundary backend does not fork")
        }
    }

    struct PlanningQuestionBackend {
        turn: std::sync::atomic::AtomicUsize,
        structured: bool,
    }

    struct MutableQuestionBackend {
        fail_withdrawal_continuation: bool,
    }

    struct RepeatedQuestionBackend {
        turn: std::sync::atomic::AtomicUsize,
    }

    struct RetryPlanBackend {
        turn: std::sync::atomic::AtomicUsize,
    }

    struct RepeatedPlanEditBackend;

    fn submit_test_plan(
        request: &BackendRequest,
        output: &mut crate::backend::BackendOutput,
        overview: &str,
    ) {
        let request_text = request.input.text();
        let document = crate::backend::mock_plan_document_from_prompt(&request_text)
            .unwrap_or_else(|| {
                panic!("planning prompt should include the canonical document: {request_text}")
            });
        let mut mutation = crate::plan::PlanMutation {
            plan: Some(crate::plan::PlanFieldPatch {
                title: Some("Migration plan".into()),
                overview: Some(overview.into()),
                ..Default::default()
            }),
            ..Default::default()
        };
        if document.tasks.is_empty() {
            mutation.set = Some(crate::plan::PlanResourceSet {
                entity_changes: Some(vec![crate::plan::ProgramEntityChange {
                        action: crate::plan::EntityChangeAction::Modify,
                        kind: crate::plan::EntityKind::Function,
                        renamed_from: None,
                        name: "migrate".into(),
                        description: "Apply the selected strategy.".into(),
                        path: "src/migration.rs".into(),
                        members: Vec::new(),
                        variants: Vec::new(),
                        extends: None,
                        conforms_to: Vec::new(),
                }]),
                flows: Some(vec![crate::plan::PlanFlow {
                        title: "Migration".into(),
                        description: "Start from the selected migration and produce updated persisted state. Keep migration decisions separate from storage ownership.".into(),
                        source: crate::plan::EntityReference::PlannedEntity {
                            entity: "migrate".into(),
                        },
                        edges: vec![crate::plan::PlanFlowEdge {
                                relation: crate::plan::PlanFlowRelation::Write,
                                target: crate::plan::EntityReference::ExternalEntity {
                                    entity_kind: crate::plan::ReferencedEntityKind::Type,
                                    name: "MigrationStore".into(),
                                    dependency: None,
                                },
                                callable: Some(crate::plan::PlanCallable {
                                    kind: crate::plan::PlanCallableKind::Method,
                                    name: "persist".into(),
                                }),
                                payload_type: None,
                                return_type: Some(crate::plan::PlanFlowReturnType {
                                    value_type: "PersistedMigration".into(),
                                    error_type: None,
                                }),
                                expansion: Vec::new(),
                                branches: Vec::new(),
                        }],
                }]),
                tasks: Some(vec![crate::plan::PlanTask {
                        title: "Implement the migration".into(),
                        description: "Apply the selected strategy at its owner.".into(),
                        files: vec![crate::plan::PlanFile {
                            change: crate::plan::PlanFileChange::Modify {
                                path: "src/migration.rs".into(),
                            },
                            subtasks: vec![
                                crate::plan::PlanSubtask::Work(
                                    crate::plan::PlanWorkSubtask {
                                        action: crate::plan::SubtaskAction::Create,
                                        description: "Change the persisted format.".into(),
                                        entities: vec!["migrate".into()],
                                    },
                                ),
                                crate::plan::PlanSubtask::Test(
                                    crate::plan::PlanTestSubtask {
                                        operation: crate::plan::TestSubtaskOperation::Test,
                                        action: crate::plan::ChangeAction::Add,
                                        renamed_from: None,
                                        name: "verify_migration".into(),
                                        category: crate::plan::TestCategory::Unit,
                                        behavior:
                                            "The selected strategy preserves valid state.".into(),
                                        covers_entities: vec!["migrate".into()],
                                    },
                                ),
                            ],
                        }],
                }]),
                ..Default::default()
            });
        }
        let edit_request = crate::plan::PlanEditRequest {
            plan_id: document.plan_id.clone(),
            expected_version: document.version,
            mutation,
        };
        let expected_version = crate::plan::apply_plan_edit(&document, edit_request.clone())
            .expect("test plan edit should validate")
            .version;
        output.plan_edit.push(edit_request);
        output.plan_submit = Some(crate::backend::PlanSubmitRequest {
            plan_id: document.plan_id.clone(),
            expected_version,
        });
    }

    #[async_trait::async_trait]
    impl Backend for MutableQuestionBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let question_set = || PlanQuestionSet {
                id: "migration-set".into(),
                questions: vec![crate::plan::PlanQuestion {
                    id: "migration".into(),
                    header: "Migration".into(),
                    question: "Which migration should the implementation use?".into(),
                    options: vec![
                        crate::plan::PlanQuestionOption {
                            label: "Staged".into(),
                            description: "Preserve compatibility temporarily.".into(),
                        },
                        crate::plan::PlanQuestionOption {
                            label: "Immediate".into(),
                            description: "Replace the old format now.".into(),
                        },
                    ],
                    allow_freeform: true,
                }],
            };
            let request_text = request.input.text();
            let mut output = crate::backend::BackendOutput {
                backend_session_id: Some("mutable-question".into()),
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Mutable question test".into(),
                    model: Some(request.model.clone()),
                },
                ..crate::backend::BackendOutput::default()
            };
            if request_text.contains("User follow-up:\nreplace the options") {
                let mut replacement = question_set();
                replacement.questions[0].options = vec![
                    crate::plan::PlanQuestionOption {
                        label: "Safe".into(),
                        description: "Retain compatibility.".into(),
                    },
                    crate::plan::PlanQuestionOption {
                        label: "Fast".into(),
                        description: "Prefer immediate cleanup.".into(),
                    },
                ];
                output.plan_question = Some(replacement);
            } else if request_text.contains("User follow-up:\nuse staged") {
                output.question_answer = Some(PlanQuestionAnswer {
                    question_id: "migration".into(),
                    response: PlanQuestionResponse::Selected {
                        option: "Staged".into(),
                        feedback: None,
                    },
                });
            } else if request_text.contains("User follow-up:\nthis decision is not material") {
                output.question_withdrawal = Some(PlanQuestionWithdrawal {
                    reason: "Repository policy determines the migration.".into(),
                });
            } else if request_text.contains("Pending planning questions withdrawn")
                || request_text.contains("Planning feedback:\n- Migration: Staged")
                || request_text.contains("The user answered the pending Harness questions")
            {
                if self.fail_withdrawal_continuation
                    && request_text.contains("Pending planning questions withdrawn")
                {
                    anyhow::bail!("synthetic withdrawal continuation failure");
                }
                if request.mode == PromptMode::Plan {
                    submit_test_plan(&request, &mut output, "Follow repository policy.");
                }
            } else {
                output.plan_question = Some(question_set());
            }
            Ok(output)
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            Ok(crate::backend::BackendForkResult::unprofiled(
                "mutable-question-fork",
            ))
        }
    }

    #[async_trait::async_trait]
    impl Backend for RepeatedQuestionBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let turn = self.turn.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            let mut output = crate::backend::BackendOutput {
                backend_session_id: Some("repeated-question".into()),
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Repeated question test".into(),
                    model: Some(request.model.clone()),
                },
                ..crate::backend::BackendOutput::default()
            };
            if turn < 2 {
                output.plan_question = Some(PlanQuestionSet {
                    id: format!("provider-set-{turn}"),
                    questions: vec![crate::plan::PlanQuestion {
                        id: format!("provider-question-{turn}"),
                        header: "Migration".into(),
                        question: "Which migration should the implementation use?".into(),
                        options: vec![
                            crate::plan::PlanQuestionOption {
                                label: "Staged".into(),
                                description: "Preserve compatibility temporarily.".into(),
                            },
                            crate::plan::PlanQuestionOption {
                                label: "Immediate".into(),
                                description: "Replace the old format now.".into(),
                            },
                        ],
                        allow_freeform: true,
                    }],
                });
            } else {
                submit_test_plan(
                    &request,
                    &mut output,
                    "Use the consumed migration decision.",
                );
            }
            Ok(output)
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            Ok(crate::backend::BackendForkResult::unprofiled(
                "repeated-question-fork",
            ))
        }
    }

    #[async_trait::async_trait]
    impl Backend for RetryPlanBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let turn = self.turn.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            let mut output = crate::backend::BackendOutput {
                backend_session_id: Some("retry-plan".into()),
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Retry plan test".into(),
                    model: Some(request.model.clone()),
                },
                ..crate::backend::BackendOutput::default()
            };
            if turn >= 2 {
                submit_test_plan(&request, &mut output, "Submitted after an explicit retry.");
            }
            Ok(output)
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            Ok(crate::backend::BackendForkResult::unprofiled(
                "retry-plan-fork",
            ))
        }
    }

    #[async_trait::async_trait]
    impl Backend for RepeatedPlanEditBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let mut output = crate::backend::BackendOutput {
                backend_session_id: Some("repeated-plan-document".into()),
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Repeated plan document test".into(),
                    model: Some(request.model.clone()),
                },
                ..crate::backend::BackendOutput::default()
            };
            submit_test_plan(&request, &mut output, "The same canonical plan.");
            output.plan_submit = None;
            Ok(output)
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            Ok(crate::backend::BackendForkResult::unprofiled(
                "repeated-plan-document-fork",
            ))
        }
    }

    impl PlanningQuestionBackend {
        fn new(structured: bool) -> Self {
            Self {
                turn: std::sync::atomic::AtomicUsize::new(0),
                structured,
            }
        }
    }

    #[async_trait::async_trait]
    impl Backend for PlanningQuestionBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let turn = self.turn.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            let text = if turn == 0 {
                "Which migration strategy should the plan use?"
            } else {
                "The plan is ready for review."
            };
            let event = BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "assistant_message".into(),
                text: Some(text.into()),
                data: Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            };
            if let Some(event_sink) = event_sink {
                let _ = event_sink.send(event.clone());
            }
            let plan_question = (turn == 0 && self.structured).then(|| PlanQuestionSet {
                id: String::new(),
                questions: vec![crate::plan::PlanQuestion {
                    id: String::new(),
                    header: "Migration".into(),
                    question: text.into(),
                    options: vec![
                        crate::plan::PlanQuestionOption {
                            label: "Staged".into(),
                            description: "Support both formats temporarily.".into(),
                        },
                        crate::plan::PlanQuestionOption {
                            label: "Immediate".into(),
                            description: "Remove the old format now.".into(),
                        },
                    ],
                    allow_freeform: true,
                }],
            });
            let mut output = crate::backend::BackendOutput {
                backend_session_id: Some("planning-question".into()),
                event: vec![event],
                plan_question,
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Planning question test".into(),
                    model: Some(request.model.clone()),
                },
                ..crate::backend::BackendOutput::default()
            };
            if turn > 0 && request.mode == PromptMode::Plan {
                submit_test_plan(
                    &request,
                    &mut output,
                    "Use the selected migration strategy.",
                );
            }
            Ok(output)
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            Ok(crate::backend::BackendForkResult::unprofiled(
                "planning-question-fork",
            ))
        }
    }

    struct NestedWriteBackend;

    #[async_trait::async_trait]
    impl Backend for NestedWriteBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let commentary = BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "assistant_message".into(),
                text: Some("Creating the nested module.".into()),
                data: Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            };
            let provider_change = crate::backend::ProviderChangeSet {
                file: vec![crate::backend::ProviderFileChange {
                    path: "seed.txt".into(),
                    move_path: None,
                    kind: crate::backend::ProviderChangeKind::Update,
                    diff: "@@ -1 +1 @@\n-seed\n+provider edit".into(),
                }],
            };
            let file_started = BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "tool".into(),
                text: None,
                data: Value::Null,
                activity: Some(crate::backend::ToolActivity {
                    id: "provider-file-change".into(),
                    kind: crate::backend::ToolActivityKind::FileChange,
                    title: "file changes".into(),
                    output: None,
                    status: Some("inProgress".into()),
                    change: provider_change.clone(),
                    output_delta: false,
                }),
                summary: None,
                task_update: None,
            };
            let started = BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "tool".into(),
                text: None,
                data: Value::Null,
                activity: Some(crate::backend::ToolActivity {
                    id: "nested-command".into(),
                    kind: crate::backend::ToolActivityKind::Command,
                    title: "create nested module".into(),
                    output: None,
                    status: Some("inProgress".into()),
                    change: crate::backend::ProviderChangeSet::default(),
                    output_delta: false,
                }),
                summary: None,
                task_update: None,
            };
            if let Some(event_sink) = event_sink.as_ref() {
                let _ = event_sink.send(commentary.clone());
                let _ = event_sink.send(file_started.clone());
            }
            std::fs::write(
                Path::new(&request.workspace).join("seed.txt"),
                "provider edit\n",
            )?;
            let file_completed = BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "tool".into(),
                text: None,
                data: Value::Null,
                activity: Some(crate::backend::ToolActivity {
                    id: "provider-file-change".into(),
                    kind: crate::backend::ToolActivityKind::FileChange,
                    title: "file changes".into(),
                    output: None,
                    status: Some("completed".into()),
                    change: provider_change,
                    output_delta: false,
                }),
                summary: None,
                task_update: None,
            };
            if let Some(event_sink) = event_sink.as_ref() {
                let _ = event_sink.send(file_completed.clone());
                let _ = event_sink.send(started.clone());
            }
            let nested = Path::new(&request.workspace).join("apps/new/deep/module");
            std::fs::create_dir_all(&nested)?;
            std::fs::write(nested.join("lib.rs"), "pub fn nested() {}\n")?;
            let ignored = Path::new(&request.workspace).join("target/generated/deep");
            std::fs::create_dir_all(&ignored)?;
            std::fs::write(ignored.join("artifact.txt"), "ignored\n")?;
            let completed = BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "tool".into(),
                text: None,
                data: Value::Null,
                activity: Some(crate::backend::ToolActivity {
                    id: "nested-command".into(),
                    kind: crate::backend::ToolActivityKind::Command,
                    title: "create nested module".into(),
                    output: Some("created nested module".into()),
                    status: Some("completed".into()),
                    change: crate::backend::ProviderChangeSet::default(),
                    output_delta: false,
                }),
                summary: None,
                task_update: None,
            };
            let response = BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "assistant_message".into(),
                text: Some("The nested module is ready.".into()),
                data: Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            };
            if let Some(event_sink) = event_sink {
                let _ = event_sink.send(completed.clone());
                let _ = event_sink.send(response.clone());
            }
            Ok(crate::backend::BackendOutput {
                backend_session_id: Some("nested-write".into()),
                event: vec![
                    commentary,
                    file_started,
                    file_completed,
                    started,
                    completed,
                    response,
                ],
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Nested test".into(),
                    model: Some(request.model),
                },
                ..crate::backend::BackendOutput::default()
            })
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            Ok(crate::backend::BackendForkResult::unprofiled(
                "nested-write-fork",
            ))
        }
    }

    struct TaskUpdateBackend;

    #[async_trait::async_trait]
    impl Backend for TaskUpdateBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            let task_event = BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "plan".into(),
                text: None,
                data: Value::Null,
                activity: None,
                summary: None,
                task_update: Some(crate::backend::ProviderTaskUpdate {
                    scope_id: "ordinary-turn".into(),
                    name: None,
                    complete: false,
                    replace_entries: true,
                    entry_list: vec![crate::backend::ProviderTaskEntry {
                        provider_id: None,
                        content: "Inspect ordinary chat".into(),
                        priority: None,
                        status: crate::backend::TaskStatus::InProgress,
                        provider_ordinal: 0,
                    }],
                }),
            };
            let response = BackendEvent {
                address: None,
                turn_boundary: None,
                kind: "assistant_message".into(),
                text: Some("Ordinary chat completed.".into()),
                data: Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            };
            if let Some(event_sink) = event_sink {
                let _ = event_sink.send(task_event.clone());
                let _ = event_sink.send(response.clone());
            }
            Ok(crate::backend::BackendOutput {
                backend_session_id: Some("task-update".into()),
                event: vec![task_event, response],
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Task test".into(),
                    model: Some(request.model),
                },
                ..crate::backend::BackendOutput::default()
            })
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            Ok(crate::backend::BackendForkResult::unprofiled(
                "task-update-fork",
            ))
        }
    }

    struct FixedClock(i64);
    impl Clock for FixedClock {
        fn now_ms(&self) -> i64 {
            self.0
        }
    }

    #[test]
    fn rejects_a_second_live_session_controller() {
        let mut session = HarnessSession {
            id: "session".into(),
            primary_agent_id: HarnessSession::primary_agent_id("session"),
            name: "name".into(),
            workspace: "work".into(),
            backend: "mock".into(),
            backend_session_id: None,
            provider_checkpoint_id: None,
            provider_fork_state: ProviderForkState::Ready,
            model: "model".into(),
            provider_label: "Mock backend".into(),
            resolved_model: Some("model".into()),
            effort: "low".into(),
            context_window: None,
            fast_mode: false,
            execution_mode: ExecutionMode::Read,
            mode: HarnessMode::Read,
            created_at_ms: 0,
            updated_at_ms: 0,
            active_plan_id: None,
            goal_id: None,
            lease_owner: Some("one".into()),
            lease_expires_at_ms: Some(20),
            native_fork: false,
            native_compact: false,
            context_usage: None,
        };
        assert!(acquire_lease(&mut session, "two", 10).is_err());
        assert!(acquire_lease(&mut session, "two", 21).is_ok());
        assert_eq!(FixedClock(5).now_ms(), 5);

        let mut model_setting = std::collections::BTreeMap::new();
        model_setting.insert(
            "other-model".into(),
            ModelSetting {
                reasoning: Some("high".into()),
                context_window: Some("long_context".into()),
            },
        );
        let preference = preference_for_session(
            &session,
            Some(HarnessPreference {
                model: "other-model".into(),
                effort: "high".into(),
                model_setting,
                fast_mode: false,
            }),
        );
        assert_eq!(preference.model_setting.len(), 2);
        assert_eq!(
            preference.model_setting["other-model"]
                .context_window
                .as_deref(),
            Some("long_context")
        );
        assert_eq!(
            preference.model_setting["model"].reasoning.as_deref(),
            Some("low")
        );
    }

    fn git(workspace: &Path, args: &[&str]) {
        let status = std::process::Command::new("git")
            .args(args)
            .current_dir(workspace)
            .status()
            .unwrap();
        assert!(status.success());
    }

    fn repository() -> tempfile::TempDir {
        let temporary = tempfile::tempdir().unwrap();
        git(temporary.path(), &["init", "-q"]);
        git(
            temporary.path(),
            &["config", "user.email", "harness@example.invalid"],
        );
        git(temporary.path(), &["config", "user.name", "Harness Test"]);
        std::fs::write(temporary.path().join("seed.txt"), "seed\n").unwrap();
        git(temporary.path(), &["add", "."]);
        git(temporary.path(), &["commit", "-qm", "seed"]);
        temporary
    }

    fn planning_question_broker(
        repository: &Path,
        data_root: &Path,
        structured: bool,
    ) -> HarnessBroker {
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data_root.to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.to_string_lossy().into_owned(),
                client_id: "planning-question-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        broker.backend = Arc::new(PlanningQuestionBackend::new(structured));
        broker
    }

    fn mutable_question_broker(repository: &Path, data_root: &Path) -> HarnessBroker {
        let mut broker = planning_question_broker(repository, data_root, true);
        broker.backend = Arc::new(MutableQuestionBackend {
            fail_withdrawal_continuation: false,
        });
        broker
    }

    #[tokio::test]
    async fn unknown_non_spawn_lifecycle_does_not_create_an_agent_run() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut event = Vec::new();
        let lifecycle = AgentLifecycleEvent {
            operation: "wait".into(),
            starts_child: false,
            parent_thread_id: Some("parent-thread".into()),
            provider_thread_id: Some("unknown-child".into()),
            turn_id: None,
            definition: None,
            nickname: None,
            task: None,
            status: AgentExecutionState::Completed,
        };

        broker
            .apply_agent_lifecycle(
                &BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "agent_lifecycle".into(),
                    text: None,
                    data: serde_json::to_value(lifecycle).unwrap(),
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                None,
                &mut event,
            )
            .await
            .unwrap();

        assert!(broker.agent_registry.list().is_empty());
        assert!(event.is_empty());
    }

    #[tokio::test]
    async fn subagent_activity_binds_the_pending_explicit_run() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let pending = Agent::pending(&broker.session.id, "explorer", "inspect Bevy", 100);
        broker.store.save_agent_run(&pending).unwrap();
        broker.agent_registry.insert(pending.clone());
        broker
            .agent_registry
            .execution_mut(&pending.id)
            .parent_exchange_id = Some("parent-interaction".into());
        let mut event = Vec::new();
        let lifecycle = AgentLifecycleEvent {
            operation: "subAgentActivity".into(),
            starts_child: true,
            parent_thread_id: Some("parent-thread".into()),
            provider_thread_id: Some("child-thread".into()),
            turn_id: Some("parent-turn".into()),
            definition: None,
            nickname: None,
            task: None,
            status: AgentExecutionState::Running,
        };

        broker
            .apply_agent_lifecycle(
                &BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "agent_lifecycle".into(),
                    text: None,
                    data: serde_json::to_value(lifecycle).unwrap(),
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                Some("parent-interaction"),
                &mut event,
            )
            .await
            .unwrap();

        let run_list = broker.agent_registry.list();
        assert_eq!(run_list.len(), 1);
        assert_eq!(run_list[0].id, pending.id);
        assert_eq!(run_list[0].definition, "explorer");
        assert_eq!(
            broker
                .agent_registry
                .execution(&run_list[0].id)
                .and_then(|execution| execution.parent_exchange_id.as_deref()),
            Some("parent-interaction")
        );
        assert_eq!(
            run_list[0].provider_thread_id.as_deref(),
            Some("child-thread")
        );
    }

    #[tokio::test]
    async fn child_turn_completion_preserves_history_without_accepting_input() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut run = Agent::pending(&broker.session.id, "explorer", "inspect Bevy", 100);
        run.provider_thread_id = Some("child-thread".into());
        run.state = AgentState::Ready;
        broker.store.save_agent_run(&run).unwrap();
        broker.agent_registry.insert(run.clone());
        let mut event = Vec::new();

        let routed = broker
            .route_agent_backend_event(
                None,
                &BackendEvent {
                    address: None,
                    turn_boundary: None,
                    kind: "turn_completed".into(),
                    text: None,
                    data: json!({
                        "params": {
                            "threadId": "child-thread",
                            "turn": { "id": "child-turn", "status": "completed" }
                        }
                    }),
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                &mut event,
            )
            .await
            .unwrap();

        let completed = broker.agent_registry.get(&run.id).unwrap();
        assert!(routed);
        assert_eq!(completed.state, AgentState::Ready);
        assert!(
            broker
                .agent_registry
                .execution(&run.id)
                .and_then(|execution| execution.active_turn_id.as_deref())
                .is_none()
        );
    }

    #[tokio::test]
    async fn native_provider_content_never_creates_presentation_segments() {
        use crate::backend::{ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary};
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut exchange = completed_interaction("native-runtime", &broker.session.id, Vec::new());
        exchange.state = ExchangeState::Running;
        exchange.completed_at_ms = None;
        let mut runtime = ExchangeRuntime {
            exchange_id: exchange.id.clone(),
            synthetic_turn: None,
            task: TaskTracker::default(),
            retraction_eligible: true,
            active_wait: None,
        };
        let mut provider = BackendEvent {
            address: Some(ProviderAddress {
                thread_id: "parent".into(),
                turn_id: "native".into(),
            }),
            turn_boundary: Some(TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        let mut events = Vec::new();
        broker
            .process_backend_event(&mut runtime, &mut exchange, provider.clone(), &mut events)
            .await
            .unwrap();
        provider.turn_boundary = None;
        provider.kind = "assistant_message".into();
        provider.text = Some("Inspecting".into());
        broker
            .process_backend_event(&mut runtime, &mut exchange, provider.clone(), &mut events)
            .await
            .unwrap();
        provider.text = None;
        provider.kind = "tool".into();
        provider.activity = Some(ToolActivity {
            id: "tool".into(),
            kind: ToolActivityKind::Command,
            title: "inspect".into(),
            output: Some("result".into()),
            status: Some("completed".into()),
            output_delta: false,
            change: Default::default(),
        });
        broker
            .process_backend_event(&mut runtime, &mut exchange, provider, &mut events)
            .await
            .unwrap();
        assert_eq!(exchange.turn[0].messages()[0].text(), "Inspecting");
        assert_eq!(exchange.turn[0].tools().count(), 1);
        assert!(
            exchange
                .node_list
                .iter()
                .all(|node| matches!(node, ExchangeNode::TurnContent { .. }))
        );
        assert!(runtime.synthetic_turn.is_none());
        let persisted = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(persisted[0].node_list.len(), 2);
        assert!(!runtime.retraction_eligible);
    }

    #[tokio::test]
    async fn failed_checkpoint_finalization_persists_the_failure_and_blocks_admission() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut exchange = completed_interaction("unfinished", &broker.session.id, Vec::new());
        exchange.state = ExchangeState::Running;
        exchange.completed_at_ms = None;
        exchange.checkpoint_before = Some("missing-checkpoint".into());
        broker.store.save_exchange(&exchange).unwrap();
        assert!(
            broker
                .capture_final_checkpoint(&mut exchange, ExchangeState::Failed)
                .await
                .is_err()
        );
        let persisted = broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .pop()
            .unwrap();
        assert_eq!(persisted.state, ExchangeState::Finalizing);
        assert!(
            broker
                .snapshot()
                .unwrap()
                .timeline
                .iter()
                .any(|entry| matches!(
                    entry,
                    TimelineEntry::Status {
                        status: crate::session::state_machine::SessionPhase::Finalizing { .. },
                        ..
                    }
                ))
        );
        assert!(
            persisted
                .finalization_error
                .as_deref()
                .unwrap()
                .contains("before checkpoint is missing")
        );
        let rejected = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({"text":"new request"}),
            })
            .await;
        assert!(rejected.response.error().is_some());
        assert_eq!(
            broker
                .store
                .list_exchange(&broker.session.id)
                .unwrap()
                .len(),
            1
        );
        let before = GitCheckpoint::new(repository.path())
            .capture(
                &broker.store.objects,
                &broker.repositories,
                &broker.session.id,
                100,
            )
            .await
            .unwrap();
        broker.store.save_checkpoint(&before).unwrap();
        exchange.checkpoint_before = Some(before.id);
        broker.store.save_exchange(&exchange).unwrap();
        let nodes_before = serde_json::to_value(&exchange.node_list).unwrap();
        let retried = broker
            .dispatch(Request {
                id: 2,
                method: "turn.cancel".into(),
                params: json!({}),
            })
            .await;
        assert!(
            retried.response.error().is_none(),
            "{:?}",
            retried.response.error()
        );
        let settled = broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .pop()
            .unwrap();
        assert_eq!(settled.state, ExchangeState::Failed);
        assert!(settled.finalization_error.is_none());
        assert_eq!(
            serde_json::to_value(&settled.node_list).unwrap(),
            nodes_before
        );
        assert_eq!(settled.turn.len(), exchange.turn.len());
        assert_eq!(settled.duration_ms, exchange.duration_ms);
        let settled_json = serde_json::to_value(&settled).unwrap();
        let repeated = broker
            .dispatch(Request {
                id: 3,
                method: "turn.cancel".into(),
                params: json!({}),
            })
            .await;
        assert!(repeated.response.error().is_none());
        assert_eq!(
            serde_json::to_value(
                broker
                    .store
                    .list_exchange(&broker.session.id)
                    .unwrap()
                    .pop()
                    .unwrap()
            )
            .unwrap(),
            settled_json
        );
    }

    #[tokio::test]
    async fn nested_child_start_uses_its_parent_exchange_delegation() {
        use crate::backend::{ProviderAddress, TurnBoundary};
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut child = Agent::pending(&broker.session.id, "explorer", "inspect", 1);
        child.provider_thread_id = Some("child-thread".into());
        let mut grandchild = Agent::pending(&broker.session.id, "reviewer", "review", 2);
        grandchild.provider_thread_id = Some("grandchild-thread".into());
        for agent in [&child, &grandchild] {
            broker.store.save_agent_run(agent).unwrap();
            broker.agent_registry.insert(agent.clone());
        }
        broker
            .agent_registry
            .execution_mut(&grandchild.id)
            .parent_thread_id = child.provider_thread_id.clone();
        broker.agent_registry.execution_mut(&child.id).task = "inspect".into();
        broker.agent_registry.execution_mut(&grandchild.id).task = "review".into();
        let mut primary = completed_interaction("primary", &broker.session.id, Vec::new());
        primary.state = ExchangeState::Running;
        primary.completed_at_ms = None;
        let mut event = BackendEvent {
            address: Some(ProviderAddress {
                thread_id: "child-thread".into(),
                turn_id: "child-turn".into(),
            }),
            turn_boundary: Some(TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: json!({}),
            activity: None,
            summary: None,
            task_update: None,
        };
        let mut emitted = Vec::new();
        broker
            .route_agent_backend_event(Some(&mut primary), &event, &mut emitted)
            .await
            .unwrap();
        let primary_before = serde_json::to_value(&primary).unwrap();
        event.address = Some(ProviderAddress {
            thread_id: "grandchild-thread".into(),
            turn_id: "grandchild-turn".into(),
        });
        broker
            .route_agent_backend_event(Some(&mut primary), &event, &mut emitted)
            .await
            .unwrap();
        assert_eq!(serde_json::to_value(&primary).unwrap(), primary_before);
        let owner = &broker.child_exchange_runtime_by_agent[&child.id].exchange;
        let links = referenced_child_exchange_list(owner);
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].0, grandchild.id);
        let record = broker
            .store
            .list_agent_exchange(&grandchild.id)
            .unwrap()
            .pop()
            .unwrap();
        assert_eq!(record.id, links[0].1);
        assert_eq!(record.prompt, "review");
        assert!(record.checkpoint_before.is_none());
        assert!(
            broker
                .store
                .require_settled_exchange_tree(&broker.session.id, &[primary.id.clone()])
                .is_err()
        );
        let mut settled_primary = primary.clone();
        settled_primary
            .finish(ExchangeState::Complete, 300)
            .unwrap();
        broker.store.save_exchange(&settled_primary).unwrap();
        assert!(
            broker
                .store
                .require_settled_exchange_tree(&broker.session.id, &[primary.id.clone()])
                .is_err()
        );
        for agent_id in [&child.id, &grandchild.id] {
            let mut exchange = broker
                .store
                .list_agent_exchange(agent_id)
                .unwrap()
                .pop()
                .unwrap();
            exchange.finish(ExchangeState::Cancelled, 300).unwrap();
            broker.store.save_exchange(&exchange).unwrap();
        }
        broker
            .store
            .require_settled_exchange_tree(&broker.session.id, &[primary.id.clone()])
            .unwrap();

        assert_eq!(
            broker
                .store
                .checkpoint_owner(&broker.session.id, &record.id)
                .unwrap(),
            primary.id
        );
        assert!(
            broker
                .store
                .checkpoint_owner("another-session", &record.id)
                .is_err()
        );
        broker
            .store
            .record_rollback(&broker.session.id, &[primary.id.clone()])
            .unwrap();
        for agent_id in [&child.id, &grandchild.id] {
            let exchange = broker
                .store
                .list_agent_exchange(agent_id)
                .unwrap()
                .pop()
                .unwrap();
            assert_eq!(
                exchange.disposition,
                crate::exchange::HistoryDisposition::RolledBack
            );
            assert_eq!(exchange.state, ExchangeState::Cancelled);
        }
        let stored_owner = broker
            .store
            .list_agent_exchange(&child.id)
            .unwrap()
            .pop()
            .unwrap();
        assert_eq!(referenced_child_exchange_list(&stored_owner), links);
        broker.child_exchange_runtime_by_agent.remove(&child.id);
        event.address.as_mut().unwrap().turn_id = "missing-parent".into();
        assert!(
            broker
                .route_agent_backend_event(Some(&mut primary), &event, &mut emitted)
                .await
                .is_err()
        );
        assert_eq!(serde_json::to_value(&primary).unwrap(), primary_before);
    }

    #[tokio::test]
    async fn child_reuse_creates_a_new_exchange_and_replayed_start_cannot_reopen_history() {
        use crate::backend::{ProviderAddress, TurnBoundary};
        use crate::turn::TurnOutcome;
        for (outcome, state) in [
            (TurnOutcome::Completed, ExchangeState::Complete),
            (TurnOutcome::Failed, ExchangeState::Failed),
            (TurnOutcome::Cancelled, ExchangeState::Cancelled),
            (TurnOutcome::Interrupted, ExchangeState::Interrupted),
        ] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = planning_question_broker(repository.path(), data.path(), false);
            let mut child = Agent::pending(&broker.session.id, "explorer", "first task", 1);
            child.provider_thread_id = Some("child-thread".into());
            broker.store.save_agent_run(&child).unwrap();
            broker.agent_registry.insert(child.clone());
            let mut parent = completed_interaction("parent", &broker.session.id, Vec::new());
            parent.state = ExchangeState::Running;
            parent.completed_at_ms = None;
            parent
                .append_delegation(&child.id, "spawn", "first task", 1)
                .unwrap();
            broker.store.save_exchange(&parent).unwrap();
            let mut event = BackendEvent {
                address: Some(ProviderAddress {
                    thread_id: "child-thread".into(),
                    turn_id: "first".into(),
                }),
                turn_boundary: Some(TurnBoundary::Started),
                kind: "turn_started".into(),
                text: None,
                data: json!({}),
                activity: None,
                summary: None,
                task_update: None,
            };
            let mut emitted = Vec::new();
            broker
                .route_agent_backend_event(Some(&mut parent), &event, &mut emitted)
                .await
                .unwrap();
            broker
                .route_agent_backend_event(Some(&mut parent), &event, &mut emitted)
                .await
                .unwrap();
            assert_eq!(parent.node_list.len(), 1);
            event.kind = "turn_completed".into();
            event.turn_boundary = Some(TurnBoundary::Finished { outcome });
            broker
                .route_agent_backend_event(Some(&mut parent), &event, &mut emitted)
                .await
                .unwrap();
            let first = broker.store.list_agent_exchange(&child.id).unwrap()[0].clone();
            assert_eq!(
                first.state, state,
                "child exchange must retain its provider outcome"
            );
            assert_eq!(
                first.turn[0].state(),
                crate::turn::TurnState::Finished { outcome }
            );
            event.kind = "turn_started".into();
            event.turn_boundary = Some(TurnBoundary::Started);
            broker
                .route_agent_backend_event(Some(&mut parent), &event, &mut emitted)
                .await
                .unwrap();
            assert_eq!(
                parent.node_list.len(),
                1,
                "replayed terminal turn created a new delegation"
            );
            broker.agent_registry.execution_mut(&child.id).task = "second task".into();
            event.address.as_mut().unwrap().turn_id = "second".into();
            broker
                .route_agent_backend_event(Some(&mut parent), &event, &mut emitted)
                .await
                .unwrap();
            let records = broker.store.list_agent_exchange(&child.id).unwrap();
            assert_eq!(records.len(), 2);
            assert_ne!(records[0].id, records[1].id);
            assert_eq!(records[0].completed_at_ms, first.completed_at_ms);
            assert_eq!(records[1].prompt, "second task");
            let links = referenced_child_exchange_list(&parent);
            assert_eq!(links.len(), 2);
            assert_eq!(links[0].1, records[0].id);
            assert_eq!(links[1].1, records[1].id);
        }
    }

    #[tokio::test]
    async fn shares_prompt_history_across_repository_sessions() {
        let first_repository = repository();
        let second_repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut first = planning_question_broker(first_repository.path(), data.path(), false);
        let recorded = first
            .dispatch(Request {
                id: 1,
                method: "history.record".into(),
                params: json!({ "text": "inspect the architecture" }),
            })
            .await;
        assert!(recorded.response.error().is_none());
        drop(first);

        let second = planning_question_broker(second_repository.path(), data.path(), false);
        assert_eq!(
            second.snapshot().unwrap().prompt_history,
            vec!["inspect the architecture"]
        );
    }

    #[tokio::test]
    async fn pauses_and_resumes_structured_questions_during_ordinary_chat() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), true);

        let prompted = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "prompt me with a multiple-choice question" }),
            })
            .await;
        assert!(prompted.response.error().is_none());
        assert!(prompted.event.iter().any(|event| event.event == "question"));
        let initial = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(initial.len(), 1);
        let exchange_id = initial[0].id.clone();
        let checkpoint_before = initial[0].checkpoint_before.clone();
        assert!(initial[0].checkpoint_after.is_none());
        assert!(initial[0].completed_at_ms.is_none());
        let pending = broker.snapshot().unwrap().active_elicitation.unwrap();
        assert_eq!(pending.owner, ElicitationOwner::Interaction);
        assert_eq!(
            pending.elicitation.current_question().unwrap().header,
            "Migration"
        );

        let answered = broker
            .dispatch(Request {
                id: 2,
                method: "question.answer".into(),
                params: json!({
                    "question_id": pending.elicitation.current_question().unwrap().id,
                    "response": { "kind": "selected", "option": "Staged" }
                }),
            })
            .await;
        assert!(answered.response.error().is_none());

        let continued = broker
            .dispatch(Request {
                id: 3,
                method: "question.continue".into(),
                params: json!({}),
            })
            .await;
        assert!(continued.response.error().is_none());
        assert!(continued.event.iter().filter(|event| event.event == "timeline_patch")
            .any(|event| event.payload.to_string().contains("\"kind\":\"working\"")),
            "resuming a chat clarification must restore the Working footer in live patches");
        assert!(broker.snapshot().unwrap().active_elicitation.is_none());
        let completed = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(completed.len(), 1);
        assert_eq!(completed[0].id, exchange_id);
        assert_eq!(completed[0].checkpoint_before, checkpoint_before);
        assert!(completed[0].checkpoint_after.is_some());
        assert_eq!(completed[0].state, ExchangeState::Complete);
        assert!(completed[0].node_list.iter().any(|node| matches!(node,
            ExchangeNode::QuestionPresented { question, answer: Some(answer), .. }
                if question.questions[0].header == "Migration" && answer.contains("Staged"))));
        assert!(completed[0].node_list.iter().any(|node| matches!(node,
            ExchangeNode::ExchangeInput { prompt } if prompt.intent == crate::exchange::InputIntent::Answer)));
        let rendered = timeline_text(&broker.snapshot().unwrap());
        assert!(rendered.contains("Questions · 1/1 answered"));
        assert!(rendered.contains("You answered: Staged"));
    }

    #[tokio::test]
    async fn compacts_provider_context_without_creating_an_interaction() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "compact-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let prompted = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "establish provider context" }),
            })
            .await;
        assert!(prompted.response.error().is_none());
        let interaction_count = broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .len();

        let compacted = broker
            .dispatch(Request {
                id: 2,
                method: "session.compact".into(),
                params: json!({}),
            })
            .await;

        assert!(compacted.response.error().is_none());
        assert_eq!(
            broker
                .session
                .context_usage
                .as_ref()
                .unwrap()
                .remaining_percent,
            80
        );
        assert_eq!(
            broker
                .store
                .list_exchange(&broker.session.id)
                .unwrap()
                .len(),
            interaction_count
        );
        assert_eq!(compacted.event[0].event, "context_compacted");
    }

    #[tokio::test]
    async fn initializes_one_named_session_without_resuming_a_leased_session() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let initialize = |client_id: &str, name: &str| InitializeRequest {
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: client_id.into(),
            backend: BackendLaunch {
                kind: "mock".into(),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: Some(name.into()),
            goal_max_turns: 20,
            lease_conflict_action: None,
        };

        let first = HarnessBroker::initialize_with_clock(
            initialize("first-client", "first"),
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let second = HarnessBroker::initialize_with_clock(
            initialize("second-client", "  second  "),
            Box::new(FixedClock(110)),
        )
        .unwrap();

        assert_ne!(first.session.id, second.session.id);
        assert_eq!(first.session.name, "first");
        assert_eq!(second.session.name, "second");
        assert_eq!(second.store.list_session(None).unwrap().len(), 2);
    }

    #[tokio::test]
    async fn forks_a_live_session_without_taking_its_lease() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let initialize =
            |client_id: &str, lease_conflict_action: Option<String>| InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: client_id.into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action,
            };

        let mut source = HarnessBroker::initialize_with_clock(
            initialize("source-client", None),
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let source_session_id = source.session.id.clone();
        source
            .dispatch(Request {
                id: 0,
                method: "session.execution_mode".into(),
                params: json!({ "mode": "full" }),
            })
            .await;
        let prompt = source
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "preserve this interaction" }),
            })
            .await;
        assert!(prompt.response.error().is_none());
        assert!(source.session.native_fork);
        source.session.context_usage = ContextUsage::reported(50, 200);
        source.save_session().unwrap();

        let conflict = match HarnessBroker::initialize_with_clock(
            initialize("fork-client", None),
            Box::new(FixedClock(110)),
        ) {
            Ok(_) => panic!("second controller should encounter the live lease"),
            Err(error) => error,
        };
        let conflict = conflict
            .downcast_ref::<crate::session::SessionLeaseConflict>()
            .expect("structured lease conflict");
        assert_eq!(conflict.session_id, source_session_id);
        assert!(conflict.native_fork);

        let fork_controller = HarnessBroker::initialize_with_clock(
            initialize("fork-client", Some("new".into())),
            Box::new(FixedClock(110)),
        )
        .unwrap();
        let preparation = prepare_provider_fork(
            data.path(),
            "fork-client",
            "mock",
            &fork_controller.capability,
            &fork_controller.session.id,
            &json!({ "session_id": source_session_id, "name": "fork investigation" }),
        )
        .unwrap();
        assert!(matches!(
            preparation.child.provider_fork_state,
            ProviderForkState::Preparing { .. }
        ));
        assert_eq!(
            preparation.backend_request.checkpoint_id.as_deref(),
            Some("mock-checkpoint")
        );
        let child_session_id = preparation.child.id.clone();
        let backend_fork = fork_controller
            .backend
            .fork(preparation.backend_request)
            .await
            .unwrap();
        let mut child_initialize = initialize("fork-client", None);
        child_initialize.session_id = Some(child_session_id);
        let mut child_controller =
            HarnessBroker::initialize_with_clock(child_initialize, Box::new(FixedClock(120)))
                .unwrap();
        let ready_event = child_controller
            .complete_provider_fork(Ok(backend_fork))
            .unwrap();
        assert_eq!(ready_event.event, "session_fork_ready");
        let snapshot = serde_json::to_value(child_controller.snapshot().unwrap()).unwrap();
        assert_ne!(snapshot["session"]["id"], source_session_id);
        assert_eq!(snapshot["session"]["name"], "fork investigation");
        assert_eq!(snapshot["session"]["execution_mode"], "read");
        assert_eq!(snapshot["session"]["provider_fork_state"]["state"], "ready");
        assert_eq!(snapshot["session"]["context_usage"]["used"], 50);
        assert_eq!(snapshot["session"]["context_usage"]["size"], 200);
        assert_eq!(
            snapshot["session"]["context_usage"]["remaining_percent"],
            75
        );
        assert_eq!(snapshot["exchange"].as_array().map(Vec::len), Some(0));
        assert_eq!(snapshot["timeline"].as_array().map(Vec::len), Some(1));
        assert_eq!(snapshot["timeline"][0]["kind"], "session_event");
        assert_eq!(snapshot["timeline"][0]["event"]["kind"], "forked");
        assert_eq!(
            snapshot["timeline"][0]["event"]["source_session_id"],
            source_session_id
        );
        let failed_preparation = prepare_provider_fork(
            data.path(),
            "fork-client",
            "mock",
            &fork_controller.capability,
            &fork_controller.session.id,
            &json!({ "session_id": source_session_id }),
        )
        .unwrap();
        let mut failed_initialize = initialize("fork-client", None);
        failed_initialize.session_id = Some(failed_preparation.child.id);
        let mut failed_controller =
            HarnessBroker::initialize_with_clock(failed_initialize, Box::new(FixedClock(130)))
                .unwrap();
        let failed_event = failed_controller
            .complete_provider_fork(Err("native fork failed".into()))
            .unwrap();
        assert_eq!(failed_event.event, "session_fork_failed");
        assert!(matches!(
            failed_controller.provider_fork_state(),
            ProviderForkState::Failed { message, .. } if message == "native fork failed"
        ));
        let persisted_source = child_controller
            .store
            .load_session(&source_session_id)
            .unwrap()
            .expect("source session");
        assert_eq!(
            persisted_source.lease_owner.as_deref(),
            Some("source-client")
        );
    }

    #[tokio::test]
    async fn carries_preferences_into_a_new_session_and_mode_into_a_resume() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "settings-client".into(),
                backend: BackendLaunch {
                    kind: "codex".into(),
                    command: vec!["codex".into(), "app-server".into()],
                },
                model: "default".into(),
                effort: "medium".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let selected = broker
            .dispatch(Request {
                id: 0,
                method: "session.execution_mode".into(),
                params: json!({ "mode": "yolo" }),
            })
            .await;
        assert!(selected.response.error().is_none());
        let configured = broker
            .dispatch(Request {
                id: 1,
                method: "session.configure".into(),
                params: json!({
                    "model": "gpt-5.6",
                    "effort": "low",
                    "context_window": "default",
                    "fast_mode": true
                }),
            })
            .await;
        assert!(configured.response.error().is_none());
        broker
            .dispatch(Request {
                id: 11,
                method: "session.configure".into(),
                params: json!({
                    "model": "gpt-5.4",
                    "effort": "high",
                    "context_window": "long_context"
                }),
            })
            .await;
        broker
            .dispatch(Request {
                id: 12,
                method: "session.configure".into(),
                params: json!({
                    "model": "gpt-5.6",
                    "effort": "low",
                    "context_window": "default"
                }),
            })
            .await;
        let preference = broker
            .store
            .load_preference(&broker.session.workspace, &broker.session.backend)
            .unwrap()
            .unwrap();
        assert_eq!(
            preference.model_setting["gpt-5.4"]
                .context_window
                .as_deref(),
            Some("long_context")
        );
        assert_eq!(
            preference.model_setting["gpt-5.6"].reasoning.as_deref(),
            Some("low")
        );
        let created = broker
            .dispatch(Request {
                id: 2,
                method: "session.new".into(),
                params: json!({}),
            })
            .await;
        let session = created.response.result().unwrap()["session"].clone();
        assert_eq!(session["name"], "");
        assert_eq!(session["model"], "gpt-5.6");
        assert_eq!(session["effort"], "low");
        assert_eq!(session["context_window"], "default");
        assert_eq!(session["fast_mode"], true);
        assert_eq!(session["execution_mode"], "read");
        drop(broker);
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "settings-client".into(),
                backend: BackendLaunch {
                    kind: "codex".into(),
                    command: vec!["codex".into(), "app-server".into()],
                },
                model: "default".into(),
                effort: "medium".into(),
                session_id: Some(session["id"].as_str().expect("child session id").to_owned()),
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let selected = broker
            .dispatch(Request {
                id: 21,
                method: "session.execution_mode".into(),
                params: json!({ "mode": "full" }),
            })
            .await;
        assert!(selected.response.error().is_none());
        let renamed = broker
            .dispatch(Request {
                id: 3,
                method: "session.rename".into(),
                params: json!({ "session_id": session["id"], "name": "  Architecture review  " }),
            })
            .await;
        assert_eq!(
            renamed.response.result().unwrap()["name"],
            "Architecture review"
        );
        assert!(!renamed.event.iter().any(|event| {
            event.event == "backend_event"
                && event.payload["kind"] == "timeline_session_event"
                && event.payload["data"]["event"]["name"] == "Architecture review"
        }));
        let stale = broker.rename_session(json!({ "session_id": session["id"],
            "name": "Generated name", "expected_name": "Previous name" }));
        assert!(stale.unwrap_err().to_string().contains("changed during generation"));
        assert_eq!(broker.store.load_session(session["id"].as_str().unwrap()).unwrap().unwrap().name, "Architecture review");
        drop(broker);

        let resumed_session_id = session["id"].as_str().expect("new session id").to_owned();
        let restarted = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "restarted-settings-client".into(),
                backend: BackendLaunch {
                    kind: "codex".into(),
                    command: vec!["codex".into(), "app-server".into()],
                },
                model: "default".into(),
                effort: "medium".into(),
                session_id: Some(resumed_session_id),
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(200)),
        )
        .unwrap();
        assert_eq!(restarted.session.model, "gpt-5.6");
        assert_eq!(restarted.session.effort, "low");
        assert!(restarted.session.fast_mode);
        assert_eq!(restarted.session.execution_mode, ExecutionMode::Full);
        assert!(
            !restarted
                .snapshot()
                .unwrap()
                .timeline
                .iter()
                .any(|entry| matches!(
                    entry,
                    TimelineEntry::SessionEvent { event, .. }
                        if matches!(
                            &event.detail,
                            SessionEventKind::Renamed { .. }
                        )
                ))
        );
    }

    #[tokio::test]
    async fn rejects_inline_model_effort_and_mode_values_outside_backend_capabilities() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "inline-config-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "medium".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();

        let invalid_model = broker
            .dispatch(Request {
                id: 1,
                method: "session.configure".into(),
                params: json!({ "model": "sol", "validate": true }),
            })
            .await;
        assert!(invalid_model.response.error().is_some());
        assert_eq!(broker.session.model, "mock-model");

        let invalid_effort = broker
            .dispatch(Request {
                id: 2,
                method: "session.configure".into(),
                params: json!({ "effort": "xhigh", "validate": true }),
            })
            .await;
        assert!(invalid_effort.response.error().is_some());
        assert_eq!(broker.session.effort, "medium");

        let invalid_mode = broker
            .dispatch(Request {
                id: 3,
                method: "session.execution_mode".into(),
                params: json!({ "mode": "invalid" }),
            })
            .await;
        assert!(invalid_mode.response.error().is_some());
        assert_eq!(broker.session.execution_mode, ExecutionMode::Read);
    }

    #[tokio::test]
    async fn previews_a_stored_session_without_acquiring_its_lease() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "preview-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let source_session_id = broker.session.id.clone();
        let prompt = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "preserve preview history" }),
            })
            .await;
        assert!(prompt.response.error().is_none());
        let created = broker
            .dispatch(Request {
                id: 2,
                method: "session.new".into(),
                params: json!({}),
            })
            .await;
        assert!(created.response.error().is_none());
        let mut source_session = broker
            .store
            .load_session(&source_session_id)
            .unwrap()
            .expect("source session");
        source_session.lease_owner = Some("foreign-client".into());
        source_session.lease_expires_at_ms = Some(500);
        broker.store.save_session(&source_session).unwrap();

        let preview = broker
            .dispatch(Request {
                id: 3,
                method: "session.preview".into(),
                params: json!({ "session_id": source_session_id }),
            })
            .await;
        assert!(preview.response.error().is_none());
        let result = preview.response.result().expect("session preview");
        assert_eq!(result["session"]["id"], source_session_id);
        assert_eq!(result["exchange"].as_array().map(Vec::len), Some(1));
        assert_eq!(result["exchange"][0]["prompt"], "preserve preview history");
        assert_eq!(result["timeline"].as_array().map(Vec::len), Some(1));
        let persisted = broker
            .store
            .load_session(&source_session_id)
            .unwrap()
            .expect("persisted source session");
        assert_eq!(persisted.lease_owner.as_deref(), Some("foreign-client"));
        assert_eq!(persisted.lease_expires_at_ms, Some(500));
    }

    #[tokio::test]
    async fn resumes_latest_repository_session_after_broker_restart() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "first-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let session_id = broker.session.id.clone();
        broker.session.name = "Persistent analysis".into();
        broker.store.save_session(&broker.session).unwrap();
        let (mut exchange, _) = broker
            .interaction_for_turn("interrupted request", true, 100)
            .await
            .unwrap();
        exchange.resume(100).unwrap();
        exchange
            .start_turn(
                crate::backend::ProviderAddress {
                    thread_id: "main-thread".into(),
                    turn_id: "unfinished-turn".into(),
                },
                100,
            )
            .unwrap();
        broker.store.save_exchange(&exchange).unwrap();
        let child = crate::agent::Agent::pending(&session_id, "reviewer", "review", 100);
        broker.store.save_agent_run(&child).unwrap();
        drop(broker);

        let restarted = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "second-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "different-model".into(),
                effort: "high".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(200)),
        )
        .unwrap();

        assert_eq!(restarted.session.id, session_id);
        assert_eq!(restarted.session.name, "Persistent analysis");
        assert_eq!(restarted.session.model, "mock-model");
        assert_eq!(restarted.session.effort, "low");
        let snapshot = restarted.snapshot().unwrap();
        assert_eq!(snapshot.exchange.len(), 1);
        let recovered = &snapshot.exchange[0];
        assert_eq!(recovered.id, exchange.id);
        assert_eq!(recovered.state, ExchangeState::Interrupted);
        assert_eq!(recovered.elapsed(90_000), 0);
        assert_eq!(
            recovered.turn[0].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Interrupted,
            }
        );
        let child = restarted
            .store
            .list_agent_run(&session_id)
            .unwrap()
            .pop()
            .unwrap();
        assert_eq!(child.state, crate::agent::AgentState::Closed);
        assert_eq!(
            restarted
                .store
                .list_session(Some(&restarted.session.workspace))
                .unwrap()
                .len(),
            1
        );
    }

    #[tokio::test]
    async fn pauses_for_structured_feedback_then_submits_the_same_plan() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), true);
        broker.trace.configure(true).unwrap();

        let paused = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert!(
            paused.response.error().is_none(),
            "{:?}",
            paused.response.error()
        );
        assert!(
            paused
                .event
                .iter()
                .any(|event| event.event == "plan_question")
        );
        let paused_snapshot = broker.snapshot().unwrap();
        let pending_plan = paused_snapshot.active_plan.expect("active paused plan");
        assert_eq!(pending_plan.state, PlanState::AwaitingInput);
        let elicitation = pending_plan.elicitation.expect("durable elicitation");
        assert_eq!(elicitation.question_set.questions[0].header, "Migration");
        assert_eq!(paused_snapshot.artifact.len(), 1);
        assert!(paused_snapshot.exchange.last().unwrap().awaiting_input);

        let original_exchange = paused_snapshot.exchange.last().unwrap().clone();
        assert!(original_exchange.execution_started_at_ms.is_none());
        assert!(
            broker
                .append_plan_feedback("unrelated-plan", "wrong owner".into(), crate::exchange::InputIntent::Clarification, None)
                .is_err()
        );
        assert_eq!(
            broker.store.list_exchange(&broker.session.id).unwrap()[0]
                .node_list
                .len(),
            original_exchange.node_list.len()
        );
        let question_id = elicitation.question_set.questions[0].id.clone();
        let clarified = broker
            .dispatch(Request {
                id: 2,
                method: "question.ask".into(),
                params: json!({
                    "question_id": question_id,
                    "text": "What compatibility cost does staged migration add?"
                }),
            })
            .await;
        assert!(clarified.response.error().is_none());
        let clarified_snapshot = broker.snapshot().unwrap();
        assert_eq!(clarified_snapshot.exchange.len(), 1);
        let clarified_exchange = &clarified_snapshot.exchange[0];
        assert_eq!(clarified_exchange.id, original_exchange.id);
        assert!(clarified_exchange.checkpoint_after.is_none());
        assert!(clarified_exchange.execution_started_at_ms.is_none());
        let clarified_plan = clarified_snapshot.active_plan.unwrap();
        assert_eq!(clarified_plan.state, PlanState::AwaitingInput);
        assert!(
            clarified_plan
                .elicitation
                .as_ref()
                .unwrap()
                .clarification_active
        );
        let follow_up = broker
            .dispatch(Request {
                id: 3,
                method: "prompt.submit".into(),
                params: json!({ "text": "How long should compatibility remain?" }),
            })
            .await;
        assert!(follow_up.response.error().is_none());
        assert_eq!(
            broker.snapshot().unwrap().active_plan.unwrap().state,
            PlanState::AwaitingInput
        );
        let answered = broker
            .dispatch(Request {
                id: 4,
                method: "question.answer".into(),
                params: json!({
                    "question_id": question_id,
                    "response": {
                        "kind": "selected",
                        "option": "Staged",
                        "feedback": "Preserve compatibility for one release"
                    }
                }),
            })
            .await;
        assert!(answered.response.error().is_none());
        let completed = broker
            .dispatch(Request {
                id: 5,
                method: "question.continue".into(),
                params: json!({}),
            })
            .await;
        assert!(completed.response.error().is_none());
        assert!(
            completed
                .event
                .iter()
                .any(|event| event.event == "plan_question_answered")
        );
        assert!(completed.event.iter().filter(|event| event.event == "timeline_patch")
            .any(|event| {
                let payload = event.payload.to_string();
                payload.contains("\"kind\":\"working\"")
                    || payload.contains("\"kind\":\"retrying_plan_generation\"")
            }),
            "resuming plan feedback must restore an active footer in live patches");
        assert!(
            completed
                .event
                .iter()
                .any(|event| event.event == "plan_created")
        );
        let completed_snapshot = broker.snapshot().unwrap();
        let rendered = timeline_text(&completed_snapshot);
        assert_eq!(rendered.matches("You answered: Staged").count(), 1);
        assert!(!rendered.contains("QuestionAsked"));
        let plan = completed_snapshot.active_plan.expect("submitted plan");
        assert_eq!(plan.state, PlanState::AwaitingReview);
        assert!(plan.elicitation.is_none());
        assert!(completed_snapshot.timeline.iter().any(|entry| matches!(
            entry,
            TimelineEntry::Status {
                status: crate::session::state_machine::SessionPhase::AwaitingPlanReview { .. },
                ..
            }
        )));
        assert_eq!(completed_snapshot.artifact.len(), 1);
        assert!(Path::new(&plan.working_path).exists());
        assert_eq!(completed_snapshot.exchange.len(), 1);
        let completed_exchange = &completed_snapshot.exchange[0];
        assert_eq!(completed_exchange.id, original_exchange.id);
        assert_eq!(
            completed_exchange.checkpoint_before,
            original_exchange.checkpoint_before
        );
        assert!(original_exchange.checkpoint_after.is_none());
        assert!(completed_exchange.checkpoint_after.is_some());
        assert!(
            broker
                .append_plan_feedback(&plan.id, "late feedback".into(), crate::exchange::InputIntent::Clarification, None)
                .is_err()
        );
        assert_eq!(completed_snapshot.exchange[0].plan_id, Some(plan.id));
        assert!(completed_snapshot.timeline.iter().all(|entry| matches!(entry,
            TimelineEntry::Exchange { .. } | TimelineEntry::Status { .. })));
        let lifecycle = broker
            .store
            .list_plan_lifecycle(&broker.session.id)
            .unwrap();
        assert_eq!(
            lifecycle
                .iter()
                .map(|record| record.kind)
                .collect::<Vec<_>>(),
            [
                PlanLifecycleKind::QuestionAsked,
                PlanLifecycleKind::QuestionAnswered,
                PlanLifecycleKind::Created
            ]
        );
        let trace = std::fs::read_to_string(data.path().join("harness-trace.jsonl")).unwrap();
        assert!(trace.contains("feedback_consumed"));
        assert!(trace.contains("plan_submitted"));
        assert!(trace.contains("snapshot_plan_state"));
    }

    #[tokio::test]
    async fn consumed_question_content_cannot_reenter_awaiting_input() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), true);
        broker.backend = Arc::new(RepeatedQuestionBackend {
            turn: std::sync::atomic::AtomicUsize::new(0),
        });

        let paused = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert!(paused.response.error().is_none());
        let pending = broker.snapshot().unwrap().active_plan.unwrap();
        let question_id = pending.elicitation.as_ref().unwrap().question_set.questions[0]
            .id
            .clone();
        broker
            .dispatch(Request {
                id: 2,
                method: "question.answer".into(),
                params: json!({
                    "question_id": question_id,
                    "response": {
                        "kind": "selected",
                        "option": "Staged",
                        "feedback": null
                    }
                }),
            })
            .await;
        let completed = broker
            .dispatch(Request {
                id: 3,
                method: "question.continue".into(),
                params: json!({}),
            })
            .await;
        assert!(completed.response.error().is_none());

        let snapshot = broker.snapshot().unwrap();
        let plan = snapshot.active_plan.unwrap();
        assert_eq!(plan.state, PlanState::AwaitingReview);
        assert!(plan.elicitation.is_none());
        assert_eq!(plan.question_ledger.resolution.len(), 1);
        assert_eq!(
            snapshot.exchange.len(),
            1,
            "planning answers and automatic retries must reuse the original exchange"
        );
        assert_eq!(
            broker
                .store
                .list_plan_lifecycle(&broker.session.id)
                .unwrap()
                .iter()
                .filter(|entry| entry.kind == PlanLifecycleKind::QuestionAsked)
                .count(),
            1
        );
        assert_eq!(
            broker
                .store
                .list_plan_lifecycle(&broker.session.id)
                .unwrap()
                .iter()
                .filter(|entry| entry.kind == PlanLifecycleKind::QuestionAnswered)
                .count(),
            1
        );
        assert!(
            completed
                .event
                .iter()
                .any(|event| event.event == "plan_generation_retry")
        );
    }

    #[tokio::test]
    async fn retry_resumes_the_same_failed_plan_with_a_fresh_budget() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.backend = Arc::new(RetryPlanBackend {
            turn: std::sync::atomic::AtomicUsize::new(0),
        });

        let failed = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert!(failed.response.error().is_none());
        let failed_plan = broker.snapshot().unwrap().active_plan.unwrap();
        let plan_id = failed_plan.id.clone();
        assert_eq!(failed_plan.state, PlanState::Failed);
        assert_eq!(failed_plan.generation.budget.turn_count, 2);
        let rejected = broker
            .dispatch(Request {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "keep going" }),
            })
            .await;
        assert_eq!(
            rejected
                .response
                .error()
                .map(|error| error.message.as_str()),
            Some("plan generation stopped; run /plan retry or /plan cancel")
        );

        let retried = broker
            .dispatch(Request {
                id: 3,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan retry" }),
            })
            .await;
        assert!(retried.response.error().is_none());
        let submitted = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(submitted.id, plan_id);
        assert_eq!(submitted.state, PlanState::AwaitingReview);
        assert_eq!(submitted.generation.budget.turn_count, 1);
    }

    #[tokio::test]
    async fn abort_plan_clears_review_and_question_waits_and_restores_execution_mode() {
        for structured in [false, true] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = planning_question_broker(repository.path(), data.path(), structured);
            broker.session.execution_mode = ExecutionMode::Full;
            let planned = broker.dispatch(Request { id:1, method:"prompt.submit".into(),
                params:json!({"text":"/plan migrate"}) }).await;
            assert!(planned.response.error().is_none());
            let plan = broker.snapshot().unwrap().active_plan.unwrap();
            assert!(matches!(plan.state, PlanState::AwaitingInput | PlanState::AwaitingReview));
            let cancelled = broker.dispatch(Request { id:2, method:"plan.cancel".into(), params:json!({}) }).await;
            assert!(cancelled.response.error().is_none());
            let snapshot = broker.snapshot().unwrap();
            assert!(snapshot.active_plan.is_none());
            assert_eq!(broker.session.mode, HarnessMode::Full);
            assert!(snapshot.active_elicitation.is_none());
            assert!(!snapshot.timeline.iter().any(|entry| matches!(entry, TimelineEntry::Status { .. })));
            assert!(broker.store.list_exchange(&broker.session.id).unwrap().iter()
                .filter(|exchange| exchange.plan_id.as_deref() == Some(&plan.id))
                .all(|exchange| exchange.completed_at_ms.is_some() && !exchange.awaiting_input));
        }
    }

    #[tokio::test]
    async fn replan_uses_selected_snapshot_and_reports_completed_execution_only() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.dispatch(Request { id:1, method:"prompt.submit".into(), params:json!({"text":"/plan migrate"}) }).await;
        let mut source = broker.snapshot().unwrap().active_plan.unwrap();
        let original = broker.plan_file.read_submitted_document(&broker.session.id, &source.id, 1).unwrap();
        let mut revised = original.clone();
        revised.version += 1;
        revised.assumptions = vec!["Only revision two contains this assumption".into()];
        broker.plan_file.write_working_document(&broker.session.id, &source.id, &revised).unwrap();
        let (_, _, checksum) = broker.plan_file.submit_document_revision(&broker.session.id, &source.id, 2, revised.version).unwrap();
        source.model_revision = 2;
        source.document_version = revised.version;
        source.review_digest = Some(checksum);
        broker.store.save_plan(&source).unwrap();
        let (choices, _) = broker.list_replanning_choices().unwrap();
        assert_eq!(choices[0]["revision_count"], 2);
        assert_eq!(choices[0]["implemented"], false);
        assert!(broker.replan(&format!("{} 99", source.id)).await.is_err());
        assert_eq!(broker.session.active_plan_id.as_deref(), Some(source.id.as_str()));
        broker.replan(&format!("{} 1", source.id)).await.unwrap();
        let next = broker.snapshot().unwrap().active_plan.unwrap();
        assert_ne!(next.id, source.id);
        assert_eq!(next.model_revision, 1);
        let seeded = broker.plan_file.read_submitted_document(&broker.session.id, &next.id, 1).unwrap();
        assert_eq!(seeded.assumptions, original.assumptions);
        assert_eq!(broker.plan_file.read_submitted_document(&broker.session.id, &source.id, 1).unwrap(), original);
        broker.store.save_plan_execution(&PlanExecutionRecord {
            id:"completed-source".into(), session_id:broker.session.id.clone(), plan_id:source.id.clone(),
            goal_id:"goal".into(), state:PlanExecutionState::Complete, planning_backend_session_id:None,
            execution_backend_session_id:None, scheduler:Default::default(), lifecycle:Vec::new(),
            created_at_ms:0, completed_at_ms:Some(1),
        }).unwrap();
        let (choices, _) = broker.list_replanning_choices().unwrap();
        assert!(choices.as_array().unwrap().iter().any(|choice| choice["id"] == source.id && choice["implemented"] == true));
        assert!(choices.as_array().unwrap().iter().any(|choice| choice["id"] == next.id && choice["implemented"] == false));
    }

    #[tokio::test]
    async fn cancel_terminates_a_failed_plan_without_reopening_input() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.backend = Arc::new(RetryPlanBackend {
            turn: std::sync::atomic::AtomicUsize::new(0),
        });
        broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert_eq!(
            broker.snapshot().unwrap().active_plan.unwrap().state,
            PlanState::Failed
        );

        let cancelled = broker
            .dispatch(Request {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan cancel" }),
            })
            .await;
        assert!(cancelled.response.error().is_none());
        assert!(broker.snapshot().unwrap().active_plan.is_none());
        assert!(
            broker
                .store
                .list_plan_lifecycle(&broker.session.id)
                .unwrap()
                .iter()
                .any(|entry| entry.kind == PlanLifecycleKind::Cancelled)
        );
    }

    #[tokio::test]
    async fn identical_plan_writes_do_not_evade_the_no_progress_guard() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.backend = Arc::new(RepeatedPlanEditBackend);

        let result = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert!(result.response.error().is_none());
        let plan = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(plan.state, PlanState::Failed);
        assert_eq!(plan.generation.budget.turn_count, 3);
        assert_eq!(plan.generation.canonical_revision, 1);
        assert_eq!(plan.generation.budget.consecutive_no_progress, 2);
    }

    #[tokio::test]
    async fn replaces_unsubmitted_questions_and_preserves_editable_answers() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), true);
        broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let initial = broker.snapshot().unwrap().active_plan.unwrap();
        let question = initial.elicitation.as_ref().unwrap().question_set.questions[0].clone();
        broker
            .dispatch(Request {
                id: 2,
                method: "question.answer".into(),
                params: json!({
                    "question_id": question.id,
                    "response": { "kind": "selected", "option": "Staged", "feedback": "one release" }
                }),
            })
            .await;
        let replacement = PlanQuestionSet {
            id: "replacement".into(),
            questions: vec![question],
        }
        .normalize()
        .unwrap();
        let mut event = Vec::new();
        assert!(
            broker
                .replace_active_elicitation(replacement, &mut event)
                .unwrap()
        );

        let snapshot = broker.snapshot().unwrap();
        let plan = snapshot.active_plan.unwrap();
        let elicitation = plan.elicitation.unwrap();
        assert_eq!(elicitation.revision, 2);
        assert_eq!(elicitation.answer.len(), 1);
        assert_eq!(snapshot.active_elicitation.unwrap().plan_id, Some(plan.id));
        assert!(event.iter().any(|event| event.event == "plan_question"));
    }

    #[tokio::test]
    async fn clarification_after_review_reopens_the_answer_without_continuing() {
        for prompt in ["ask which migration to use", "/plan migrate the event format"] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = mutable_question_broker(repository.path(), data.path());
            let result = broker.dispatch(Request { id: 1, method: "prompt.submit".into(),
                params: json!({"text": prompt}) }).await;
            assert!(result.response.error().is_none());
            let question_id = broker.snapshot().unwrap().active_elicitation.unwrap()
                .elicitation.question_set.questions[0].id.clone();
            let result = broker.dispatch(Request { id: 2, method: "question.answer".into(),
                params: json!({"question_id":question_id, "response":{"kind":"selected","option":"Staged"}}) }).await;
            assert!(result.response.error().is_none());
            assert!(broker.snapshot().unwrap().active_elicitation.unwrap().elicitation.current_question().is_none());
            let result = broker.dispatch(Request { id: 3, method: "question.ask".into(),
                params: json!({"question_id":question_id,"text":"what do you mean?"}) }).await;
            assert!(result.response.error().is_none(), "{:?}", result.response.error());
            let snapshot = broker.snapshot().unwrap();
            let elicitation = snapshot.active_elicitation.unwrap().elicitation;
            assert!(elicitation.answer.is_empty());
            assert_eq!(elicitation.current_question().unwrap().id, question_id);
            assert_eq!(snapshot.exchange.len(), 1);
            assert!(snapshot.exchange[0].completed_at_ms.is_none());
            let input = snapshot.exchange[0].node_list.iter().find_map(|node| match node {
                ExchangeNode::ExchangeInput { prompt } if prompt.intent == crate::exchange::InputIntent::Clarification => Some(prompt),
                _ => None,
            }).unwrap();
            let target = input.question.as_ref().expect("clarification must retain its question target");
            assert_eq!(target.question_id.as_deref(),Some(question_id.as_str()));
            assert_eq!(target.set_id,elicitation.question_set.id);
            assert!(!result.event.iter().any(|event| event.event == "question_answered"));
        }
    }

    #[tokio::test]
    async fn clarification_replaces_the_complete_pending_question_set() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = mutable_question_broker(repository.path(), data.path());
        broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "replace the options" }),
            })
            .await;
        assert!(result.response.error().is_none());
        let elicitation = broker
            .snapshot()
            .unwrap()
            .active_plan
            .unwrap()
            .elicitation
            .unwrap();
        assert_eq!(elicitation.revision, 2);
        assert_eq!(
            elicitation.question_set.questions[0].options[0].label,
            "Safe"
        );
    }

    #[tokio::test]
    async fn explicit_chat_answer_resumes_an_ordinary_request() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = mutable_question_broker(repository.path(), data.path());
        broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "ask which migration to use" }),
            })
            .await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "use staged" }),
            })
            .await;
        assert!(result.response.error().is_none());
        assert!(broker.snapshot().unwrap().active_elicitation.is_none());
        assert!(
            result
                .event
                .iter()
                .any(|event| event.event == "question_answered")
        );
    }

    #[tokio::test]
    async fn explicit_chat_answer_resumes_a_plan_when_no_decisions_remain() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = mutable_question_broker(repository.path(), data.path());
        broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "use staged" }),
            })
            .await;
        assert!(result.response.error().is_none());
        let snapshot = broker.snapshot().unwrap();
        assert_eq!(
            snapshot.active_plan.unwrap().state,
            PlanState::AwaitingReview
        );
        assert!(snapshot.active_elicitation.is_none());
        assert!(
            result
                .event
                .iter()
                .any(|event| event.event == "plan_question_answered")
        );
    }

    #[tokio::test]
    async fn withdrawn_plan_question_resumes_planning_and_records_its_reason() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = mutable_question_broker(repository.path(), data.path());
        broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "this decision is not material" }),
            })
            .await;
        assert!(result.response.error().is_none());
        let snapshot = broker.snapshot().unwrap();
        assert_eq!(
            snapshot.active_plan.unwrap().state,
            PlanState::AwaitingReview
        );
        assert!(snapshot.active_elicitation.is_none());
        let lifecycle = broker
            .store
            .list_plan_lifecycle(&broker.session.id)
            .unwrap();
        assert!(lifecycle.iter().any(|entry| {
            entry.kind == PlanLifecycleKind::QuestionWithdrawn
                && entry.answer.as_deref() == Some("Repository policy determines the migration.")
        }));
        assert!(
            result
                .event
                .iter()
                .any(|event| event.event == "plan_question_withdrawn")
        );
    }

    #[tokio::test]
    async fn failed_withdrawal_continuation_keeps_the_question_consumed() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = mutable_question_broker(repository.path(), data.path());
        broker.backend = Arc::new(MutableQuestionBackend {
            fail_withdrawal_continuation: true,
        });
        broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "this decision is not material" }),
            })
            .await;
        assert!(result.response.error().is_some());
        let plan = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(plan.state, PlanState::Failed);
        assert!(plan.elicitation.is_none());
        assert_eq!(plan.question_ledger.resolution.len(), 1);
        assert_eq!(
            plan.question_ledger.resolution[0].kind,
            crate::plan::PlanQuestionResolutionKind::Withdrawn
        );
        assert!(
            broker
                .store
                .list_plan_lifecycle(&broker.session.id)
                .unwrap()
                .iter()
                .any(|entry| entry.kind == PlanLifecycleKind::QuestionWithdrawn)
        );
    }

    #[tokio::test]
    async fn postprocessing_failure_finalizes_the_interaction_timeline() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.backend = Arc::new(InvalidControlOutputBackend);
        broker.clock = Box::new(AdvancingClock(std::sync::atomic::AtomicI64::new(100)));

        let result = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;

        assert!(result.response.error().is_some());
        let interaction = broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .into_iter()
            .last()
            .expect("failed planning interaction");
        assert_eq!(interaction.state, ExchangeState::Failed);
        assert!(interaction.completed_at_ms.is_some());
        assert!(interaction.duration_ms > 0);
        assert!(
            interaction
                .turn
                .iter()
                .all(|turn| { !matches!(turn.state(), crate::turn::TurnState::Running) })
        );
    }

    #[tokio::test]
    async fn retries_an_unstructured_planning_response_until_submission() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);

        let paused = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert!(paused.response.error().is_none());
        let plan = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(plan.state, PlanState::AwaitingReview);
        assert!(plan.elicitation.is_none());
    }

    #[tokio::test]
    async fn renames_only_added_plan_entities_and_publishes_a_fresh_review_revision() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let planned = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert!(planned.response.error().is_none());
        let plan = broker.snapshot().unwrap().active_plan.unwrap();

        let rejected = broker
            .dispatch(Request {
                id: 2,
                method: "plan.entity.rename".into(),
                params: json!({
                    "plan_id": plan.id,
                    "entity_name": "migrate",
                    "name": "MigrationRunner",
                }),
            })
            .await;
        assert!(
            rejected
                .response
                .error()
                .is_some_and(|error| error.message.contains("only newly added"))
        );

        let mut document = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)
            .unwrap();
        document.entity_changes[0].action = crate::plan::EntityChangeAction::Add;
        document.overview = "migrate coordinates the migration.".into();
        broker
            .plan_file
            .write_working_document(&broker.session.id, &plan.id, &document)
            .unwrap();
        let renamed = broker
            .dispatch(Request {
                id: 3,
                method: "plan.entity.rename".into(),
                params: json!({
                    "plan_id": plan.id,
                    "entity_name": "migrate",
                    "name": "MigrationRunner",
                    "expected_version": document.version,
                }),
            })
            .await;
        assert!(
            renamed.response.error().is_none(),
            "{:?}",
            renamed.response.error()
        );
        assert!(
            renamed
                .event
                .iter()
                .any(|event| event.event == "plan_entity_renamed")
        );

        let renamed_snapshot = broker.snapshot().unwrap();
        let rendered = timeline_text(&renamed_snapshot);
        assert!(rendered.contains("Renamed migrate to MigrationRunner"));
        assert!(rendered.contains("Plan revised"));
        let updated_plan = renamed_snapshot.active_plan.unwrap();
        let updated_document = broker
            .plan_file
            .read_working_document(&broker.session.id, &updated_plan.id)
            .unwrap();
        assert_eq!(updated_document.entity_changes[0].name, "MigrationRunner");
        assert_eq!(
            updated_document.tasks[0].files[0].subtasks[0].owned_entities(),
            ["MigrationRunner"]
        );
        assert_eq!(
            updated_document.overview,
            "MigrationRunner coordinates the migration."
        );
        let submitted_document = broker
            .plan_file
            .read_submitted_document(
                &broker.session.id,
                &updated_plan.id,
                updated_plan.model_revision,
            )
            .unwrap();
        assert_eq!(submitted_document, updated_document);
        assert_eq!(
            updated_plan.submitted_version,
            Some(updated_document.version)
        );
        let active_before = broker.session.active_plan_id.clone();
        let (historical, events) = broker.activate_plan(json!({
            "plan_id": plan.id, "revision": plan.model_revision,
        })).unwrap();
        assert_eq!(historical["historical_revision"], plan.model_revision);
        assert!(events.is_empty());
        assert_eq!(broker.session.active_plan_id, active_before);
        assert!(historical["working_path"].as_str().unwrap().contains("submitted-"));
        let original = broker.capture_plan_revision(&plan.id, plan.model_revision).unwrap();
        assert_eq!(original.document.entity_changes[0].name, "migrate");
        assert!(broker.activate_plan(json!({"plan_id": plan.id, "revision": 999})).is_err());
        let (current, _) = broker.activate_plan(json!({
            "plan_id": plan.id, "revision": updated_plan.model_revision,
        })).unwrap();
        assert!(current.get("historical_revision").is_none());
        assert_eq!(current["model_revision"], updated_plan.model_revision);
    }

    #[tokio::test]
    async fn reviews_revises_and_accepts_a_mock_plan_before_execution() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "test-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let planned = broker
            .dispatch_stream(
                Request {
                    id: 1,
                    method: "prompt.submit".into(),
                    params: json!({ "text": "/plan build the feature" }),
                },
                event_sink,
            )
            .await;
        assert!(planned.response.error().is_none());
        assert_eq!(
            event_stream.try_recv().unwrap().kind,
            "timeline_exchange_started",
            "the durable interaction must reach the live stream before provider progress"
        );
        assert_eq!(
            event_stream.try_recv().unwrap().kind,
            "timeline_patch",
            "the canonical interaction revision must follow its provider lifecycle source"
        );
        let execution_state = event_stream.try_recv().unwrap();
        assert_eq!(execution_state.kind, "execution_state");
        assert_eq!(execution_state.data["session"]["execution_mode"], "read");
        assert_eq!(
            event_stream.try_recv().unwrap().kind,
            "assistant_message",
            "provider progress must reach the live stream before the final response is rendered"
        );
        assert_eq!(
            event_stream.try_recv().unwrap().kind,
            "timeline_patch",
            "the canonical provider-progress revision must follow its lifecycle source"
        );
        let review = planned
            .event
            .iter()
            .find(|event| event.event == "plan_created")
            .unwrap();
        let path = review
            .payload
            .pointer("/plan/working_path")
            .and_then(Value::as_str)
            .unwrap();
        assert!(Path::new(path).exists());
        assert_eq!(broker.session.execution_mode, ExecutionMode::Read);
        let planned_snapshot = broker.snapshot().unwrap();
        assert_eq!(planned_snapshot.artifact.len(), 1);
        assert!(planned_snapshot.timeline.iter().any(|entry| {
            matches!(
                entry,
                TimelineEntry::Exchange { exchange: interaction, .. }
                    if interaction.node_list.iter().any(|node| matches!(
                        node,
                        ExchangeNode::ArtifactChange { .. }
                    ))
            )
        }));

        let navigation: Value = serde_json::from_str(
            &std::fs::read_to_string(
                Path::new(path)
                    .parent()
                    .expect("plan directory")
                    .join("working.index.json"),
            )
            .unwrap(),
        )
        .unwrap();
        let overview_anchor = navigation["anchor"]
            .as_array()
            .unwrap()
            .iter()
            .find(|anchor| {
                anchor.pointer("/target/target_type") == Some(&json!("section"))
                    && anchor.pointer("/target/section") == Some(&json!("overview"))
            })
            .expect("overview navigation anchor");
        let overview_line = overview_anchor["line"].as_u64().unwrap();
        let overview_path = overview_anchor["json_path"].as_str().unwrap();
        let following_anchor = navigation["anchor"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|anchor| {
                anchor["line"]
                    .as_u64()
                    .is_some_and(|line| line > overview_line)
                    && anchor["json_path"].as_str() != Some(overview_path)
            })
            .min_by_key(|anchor| anchor["line"].as_u64())
            .expect("anchor after overview");
        let following_line = following_anchor["line"].as_u64().unwrap();
        let following_label = following_anchor["label"].as_str().unwrap().to_owned();

        let revised = broker
            .dispatch(Request {
                id: 2,
                method: "plan.request_changes".into(),
                params: json!({
                    "comment": "Name every dependency explicitly",
                    "annotations": [{
                        "start_line": overview_line,
                        "end_line": following_line,
                        "body": "Keep the reader boundary narrow"
                    }]
                }),
            })
            .await;
        assert!(
            revised.response.error().is_none(),
            "{:?}",
            revised.response.error()
        );
        let revised_snapshot = broker.snapshot().unwrap();
        assert!(revised_snapshot.timeline.iter().all(|entry| matches!(entry,
            TimelineEntry::Exchange { .. } | TimelineEntry::Status { .. })));
        let rendered = timeline_text(&revised_snapshot);
        assert!(rendered.contains("Plan changes requested"));
        assert!(rendered.find("Keep the reader boundary narrow").unwrap()
            < rendered.find("Resolved Overview").unwrap());
        let revised_interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(
            revised_interaction[1].prompt,
            "Request plan changes: Name every dependency explicitly"
        );
        assert!(
            revised_interaction[1].node_list.iter().any(
                |node| matches!(node, ExchangeNode::PlanCommentResolution { resolution }
                    if resolution.annotation[0].body == "Keep the reader boundary narrow"
                        && resolution.annotation[0].label == format!("Overview through {following_label}")
                        && resolution.annotation[0].subject.len() == 2)
            ),
            "a submitted revision should attach resolved inline comments before its artifact delta"
        );
        assert!(
            revised_interaction[1]
                .node_list
                .iter()
                .any(|node| matches!(node, ExchangeNode::ArtifactChange { .. }))
        );

        let reviewed_markdown = std::fs::read(&path).unwrap();
        std::fs::write(&path, "# Accepted plan\n\n1. Finish everything.\n").unwrap();
        let changed_review = broker
            .dispatch(Request {
                id: 3,
                method: "plan.acceptance.begin".into(),
                params: json!({}),
            })
            .await;
        assert!(changed_review.response.error().is_some());
        std::fs::write(&path, reviewed_markdown).unwrap();
        let acceptance = broker
            .dispatch(Request {
                id: 3,
                method: "plan.acceptance.begin".into(),
                params: json!({}),
            })
            .await;
        assert!(
            acceptance.response.error().is_none(),
            "{:?}",
            acceptance.response.error()
        );
        assert_eq!(
            acceptance.response.result().unwrap()["active_elicitation"]["owner"],
            "plan_acceptance"
        );
        for (id, question_id, option) in [
            (4, "acceptance-context", "Continue context"),
            (
                5,
                "acceptance-execution-mode",
                "Write workspace (Recommended)",
            ),
        ] {
            let answer = broker
                .dispatch(Request {
                    id,
                    method: "question.answer".into(),
                    params: json!({
                        "question_id": question_id,
                        "response": { "kind": "selected", "option": option, "feedback": null }
                    }),
                })
                .await;
            assert!(
                answer.response.error().is_none(),
                "{:?}",
                answer.response.error()
            );
        }
        let accepted = broker
            .dispatch(Request {
                id: 6,
                method: "question.continue".into(),
                params: json!({}),
            })
            .await;
        assert!(
            accepted.response.error().is_none(),
            "{:?}",
            accepted.response.error()
        );
        assert_eq!(broker.session.execution_mode, ExecutionMode::Write);
        let goal = broker.active_goal().unwrap();
        assert_eq!(
            goal.objective,
            "Complete accepted plan: Implement the requested change"
        );
        assert_eq!(goal.state, GoalState::Complete);
        let execution = broker
            .snapshot()
            .unwrap()
            .goal_execution
            .expect("completed goal should retain its plan execution");
        assert_eq!(execution.state, PlanExecutionState::Complete);
        let Some(PlanExecutionLifecycleEvent::TaskStarted {
            title: started_title,
            ..
        }) = execution.lifecycle.first().map(|record| &record.event)
        else {
            panic!("accepted execution should start its first canonical task");
        };
        let Some(PlanExecutionLifecycleEvent::TaskCompleted {
            title: completed_title,
            ..
        }) = execution.lifecycle.last().map(|record| &record.event)
        else {
            panic!("completed execution should close its canonical task");
        };
        assert!(!started_title.is_empty());
        assert_eq!(started_title, completed_title);
        let resolution = broker
            .store
            .list_plan_resolution(&broker.session.id)
            .unwrap();
        assert_eq!(resolution.len(), 1);
        assert_eq!(resolution[0].kind, PlanResolutionKind::Completed);
        let duplicate_acceptance = broker
            .dispatch(Request {
                id: 20,
                method: "plan.accept".into(),
                params: json!({ "execution_mode": "write" }),
            })
            .await;
        assert!(duplicate_acceptance.response.error().is_some());
        let plan = broker
            .store
            .load_plan(broker.session.active_plan_id.as_deref().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(plan.state, PlanState::Accepted);
        assert!(plan.accepted_digest.is_some());
        let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(interaction[0].prompt, "/plan build the feature");
        assert_eq!(
            interaction[1].prompt,
            "Request plan changes: Name every dependency explicitly"
        );
        assert_eq!(interaction[2].prompt, "Accept plan: build the feature");
    }

    #[tokio::test]
    async fn rejected_agent_spawns_preserve_names_and_close_unbound_identities() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let definition_directory = repository.path().join(".codex/agents");
        std::fs::create_dir_all(&definition_directory).unwrap();
        std::fs::write(
            definition_directory.join("manual-code-explorer.toml"),
            "description = \"Read-only test explorer\"\n",
        ).unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "agent-spawn-test".into(),
                backend: BackendLaunch { kind: "mock".into(), command: vec!["mock".into()] },
                model: "mock-model".into(),
                effort: "medium".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        ).unwrap();
        let params = json!({"definition": "manual-code-explorer", "task": "Inspect tests"});
        broker.capability.agent.spawn = crate::agent::AgentControlMode::ParentMediated;
        let (run, event) = broker.start_agent(params.clone()).await.unwrap();
        assert_eq!(run["state"], "closed");
        assert!(event.iter().any(|event| event.event == "agent_updated"));
        let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
        let rendered = serde_json::to_string(&exchange).unwrap();
        assert!(rendered.contains("type exactly to `manual-code-explorer`"));
        assert!(rendered.contains("/spawn manual-code-explorer Inspect tests"));
        assert!(!rendered.contains("manual_code_explorer"));
        let recovered_backend = broker.backend.clone();
        broker.backend = Arc::new(FailingBackend);
        assert!(broker.start_agent(params).await.is_err());
        let stored = broker.store.list_agent_run(&broker.session.id).unwrap();
        let registry = crate::agent::AgentRegistry::from_run_list(stored);
        assert!(registry.list().iter().all(|agent| agent.state == crate::agent::AgentState::Closed));
        assert!(registry.resolve_unbound(None, None, None).is_none());
        assert!(broker.store.list_exchange(&broker.session.id).unwrap().iter()
            .flat_map(|exchange| &exchange.turn)
            .all(|turn| !matches!(turn.state(), crate::turn::TurnState::Running)));
        broker.backend = recovered_backend;
        let recovery = broker.dispatch(Request {
            id: 2,
            method: "prompt.submit".into(),
            params: json!({ "text": "Continue after the failed child request" }),
        }).await;
        assert!(recovery.response.error().is_none(), "{:?}", recovery.response.error());
        let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchange.last().unwrap().state, ExchangeState::Complete);
    }

    struct DeferredExecutionBackend {
        inner: Arc<dyn Backend>,
        started: Notify,
        release: Notify,
    }

    #[async_trait::async_trait]
    impl Backend for DeferredExecutionBackend {
        async fn prompt_stream(&self, request: BackendRequest, event_sink: Option<BackendEventSink>) -> Result<crate::backend::BackendOutput> {
            self.started.notify_one();
            self.release.notified().await;
            self.inner.prompt_stream(request, event_sink).await
        }

        async fn fork(&self, request: BackendForkRequest) -> Result<crate::backend::BackendForkResult> {
            self.inner.fork(request).await
        }
    }

    #[tokio::test]
    async fn execution_state_streams_before_goal_resume_and_plan_provider_completion() {
        for planning in [false, true] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = HarnessBroker::initialize_with_clock(InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(), permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(), client_id: "execution-state-test".into(),
                backend: BackendLaunch { kind: "mock".into(), command: vec!["mock".into()] },
                model: "mock-model".into(), effort: "medium".into(), session_id: None,
                new_session_name: None, goal_max_turns: 20, lease_conflict_action: None,
            }, Box::new(FixedClock(100))).unwrap();
            if planning {
                let result = broker.dispatch(Request { id: 1, method: "prompt.submit".into(), params: json!({"text":"/plan build the feature"}) }).await;
                assert!(result.response.error().is_none());
            } else {
                broker.set_goal(json!({"objective":"continue"})).await.unwrap();
                broker.pause_goal().await.unwrap();
            }
            let backend = Arc::new(DeferredExecutionBackend {
                inner: broker.backend.clone(), started: Notify::new(), release: Notify::new(),
            });
            broker.backend = backend.clone();
            let (sink, mut stream) = crate::backend::events::channel();
            let operation = tokio::spawn(async move {
                broker.dispatch_stream(Request { id: 2,
                    method: if planning { "plan.accept" } else { "goal.resume" }.into(),
                    params: if planning { json!({"execution_mode":"write"}) } else { json!({}) },
                }, sink).await
            });
            let mut admitted_plan_events = false;
            let state = tokio::time::timeout(std::time::Duration::from_secs(2), async {
                loop {
                    let event = stream.recv().await.unwrap().unwrap();
                    if event.kind == "timeline_patch" {
                        if let Some(operations) = event.data["operation"].as_array() {
                            admitted_plan_events |= operations.iter().any(|operation| {
                                let exchange = &operation["entry"]["exchange"];
                                exchange["kind"] == "plan_execution" && exchange["node_list"].as_array().is_some_and(|nodes|
                                    nodes.iter().any(|node| node["event"]["content"]["lifecycle"]["kind"] == "accepted")
                                    && nodes.iter().any(|node| node["event"]["content"]["event"]["kind"] == "task_started"))
                            });
                        }
                    }
                    if event.kind == "execution_state" { break event.data; }
                }
            }).await.unwrap();
            tokio::time::timeout(std::time::Duration::from_secs(2), backend.started.notified()).await.unwrap();
            assert!(!operation.is_finished(), "state arrived only after provider completion");
            assert_eq!(state["goal"]["state"], "active");
            if planning {
                assert!(admitted_plan_events, "planning events arrived only after provider completion");
                assert_eq!(state["session"]["execution_mode"], "write");
                assert_eq!(state["goal_execution"]["state"], "active");
            } else {
                assert!(state["goal_execution"].is_null());
            }
            backend.release.notify_one();
            let drain = tokio::spawn(async move { while stream.recv().await.unwrap().is_some() {} });
            let result = tokio::time::timeout(std::time::Duration::from_secs(2), operation).await.unwrap().unwrap();
            assert!(result.response.error().is_none(), "{:?}", result.response.error());
            drain.await.unwrap();
        }
    }

    struct NativeSettlementBackend {
        inner: Arc<dyn Backend>,
        state: GoalState,
    }

    #[async_trait::async_trait]
    impl Backend for NativeSettlementBackend {
        async fn prompt_stream(&self, request: BackendRequest, event_sink: Option<BackendEventSink>) -> Result<crate::backend::BackendOutput> {
            let mut output = self.inner.prompt_stream(request, event_sink).await?;
            output.evidence.native_state = Some(self.state);
            output.evidence.tool_called = true;
            Ok(output)
        }

        async fn fork(&self, request: BackendForkRequest) -> Result<crate::backend::BackendForkResult> {
            self.inner.fork(request).await
        }
    }

    #[tokio::test]
    async fn native_goal_settlement_persists_without_requesting_continuation() {
        for state in [GoalState::Paused, GoalState::Complete, GoalState::Blocked,
            GoalState::UsageLimited, GoalState::BudgetLimited, GoalState::Cleared] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let initialize = || InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(), permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "native-settlement-test".into(),
                backend: BackendLaunch { kind: "mock".into(), command: vec!["mock".into()] },
                model: "mock-model".into(), effort: "medium".into(), session_id: None,
                new_session_name: None, goal_max_turns: 20, lease_conflict_action: None,
            };
            let mut broker = HarnessBroker::initialize_with_clock(initialize(), Box::new(FixedClock(100))).unwrap();
            broker.backend = Arc::new(NativeSettlementBackend { inner: broker.backend.clone(), state });
            let (_, event) = broker.set_goal(json!({"objective":"native settlement fixture"})).await.unwrap();
            assert!(!event.iter().any(|event| event.event == "goal_continue_requested"));
            assert!(event.iter().any(|event| event.event == "goal_changed"));
            assert_eq!(broker.active_goal().unwrap().state, state);
            assert_eq!(broker.active_goal().unwrap().continuation.turn_count, 0);
            let exchange = broker.store.list_exchange(&broker.session.id).unwrap().pop().unwrap();
            let suspended = matches!(state, GoalState::Paused | GoalState::UsageLimited | GoalState::BudgetLimited);
            assert_eq!(exchange.state, if suspended { ExchangeState::Running }
                else if state == GoalState::Cleared { ExchangeState::Cancelled }
                else { ExchangeState::Complete });
            assert_eq!(exchange.checkpoint_after.is_none(), suspended);
            if state == GoalState::Paused {
                broker.clock = Box::new(FixedClock(500));
                broker.pause_goal().await.unwrap();
                assert_eq!(broker.active_goal().unwrap().updated_at_ms, 100);
            }
            let goal = serde_json::to_value(broker.active_goal().unwrap()).unwrap();
            drop(broker);
            let mut reopened = HarnessBroker::initialize_with_clock(initialize(), Box::new(FixedClock(900))).unwrap();
            assert_eq!(serde_json::to_value(reopened.active_goal().unwrap()).unwrap(), goal);
            if matches!(state, GoalState::Paused | GoalState::Blocked | GoalState::UsageLimited | GoalState::BudgetLimited) {
                reopened.backend = Arc::new(NativeSettlementBackend {
                    inner: reopened.backend.clone(), state: GoalState::Complete,
                });
                reopened.resume_goal().await.unwrap();
                assert_eq!(reopened.active_goal().unwrap().state, GoalState::Complete);
            }
        }
    }

    struct ContinuingPlanBackend {
        inner: Arc<dyn Backend>,
        invocation: std::sync::atomic::AtomicUsize,
        block_first: bool,
    }

    #[async_trait::async_trait]
    impl Backend for ContinuingPlanBackend {
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: Option<BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            assert_eq!(request.mode, PromptMode::ExecutePlan);
            let context = request.control_context.clone().unwrap();
            assert!(context.has_active_execution);
            let mut runtime = crate::control_tools::ControlToolRuntime::new(context);
            let mut output = self.inner.prompt_stream(request, event_sink).await?;
            let invocation = self.invocation.fetch_add(1, Ordering::SeqCst);
            if self.block_first && invocation == 0 {
                let report = output.plan_task_report.first_mut().unwrap();
                report.state = crate::plan::PlanTaskState::Blocked;
                report.blocking_reason = Some("awaiting explicit resume".into());
                output.evidence.structured_complete = false;
                output.evidence.structured_blocked = true;
            }
            let report = output.plan_task_report.first().unwrap();
            runtime
                .invoke(crate::control_tools::ControlToolInvocation {
                    name: "harness_plan_task_report".into(),
                    arguments: serde_json::to_value(report)?,
                })
                .await?;
            if invocation < 3 && !(self.block_first && invocation == 0) {
                output.plan_task_report.clear();
                output.evidence.structured_complete = false;
                output.evidence.native_state = None;
                output.evidence.tool_called = true;
            }
            Ok(output)
        }

        async fn fork(&self, request: BackendForkRequest) -> Result<crate::backend::BackendForkResult> {
            self.inner.fork(request).await
        }
    }

    #[tokio::test]
    async fn resumed_plan_records_completion_after_a_blocked_resolution() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(InitializeRequest {
            data_root: data.path().to_string_lossy().into_owned(), permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(), client_id: "resolution-test".into(),
            backend: BackendLaunch { kind: "mock".into(), command: vec!["mock".into()] },
            model: "mock-model".into(), effort: "medium".into(), session_id: None,
            new_session_name: None, goal_max_turns: 20, lease_conflict_action: None,
        }, Box::new(FixedClock(100))).unwrap();
        let planned = broker.dispatch(Request { id: 1, method: "prompt.submit".into(),
            params: json!({"text":"/plan build the feature"}) }).await;
        assert!(planned.response.error().is_none());
        let backend = Arc::new(ContinuingPlanBackend {
            inner: broker.backend.clone(), invocation: std::sync::atomic::AtomicUsize::new(0),
            block_first: true,
        });
        broker.backend = backend.clone();
        broker.accept_plan(json!({"execution_mode":"write"})).await.unwrap();
        assert_eq!(broker.snapshot().unwrap().goal_execution.unwrap().scheduler.task[0].state,
            crate::plan::PlanTaskState::Blocked);
        let blocked = broker.store.list_plan_resolution(&broker.session.id).unwrap();
        assert_eq!(blocked.len(), 1);
        assert_eq!(blocked[0].kind, PlanResolutionKind::Blocked);
        broker.clock = Box::new(FixedClock(150));
        let (_, continued) = broker.resume_goal().await.unwrap();
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Active);
        assert!(!continued.iter().any(|event| event.event == "plan_resolution"),
            "active execution republished an earlier blocked resolution");
        broker.clock = Box::new(FixedClock(175));
        broker.apply_goal_evidence(crate::goal::TurnEvidence {
            structured_blocked: true, ..Default::default()
        }, &mut Vec::new()).unwrap();
        broker.clock = Box::new(FixedClock(200));
        backend.invocation.store(3, Ordering::SeqCst);
        let (_, events) = broker.resume_goal().await.unwrap();
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Complete);
        let resolutions = broker.store.list_plan_resolution(&broker.session.id).unwrap();
        assert_eq!(resolutions.len(), 3, "resumption retained only the obsolete blocked resolution");
        assert_eq!(resolutions[0].id, blocked[0].id);
        assert_eq!(resolutions[1].kind, PlanResolutionKind::Blocked);
        assert_eq!(resolutions[2].kind, PlanResolutionKind::Completed);
        let delivered: Vec<_> = events.iter().filter(|event| event.event == "plan_resolution").collect();
        assert_eq!(delivered.len(), 1);
        assert_eq!(delivered[0].payload["resolution"]["id"], resolutions[2].id);
        broker.clock = Box::new(FixedClock(900));
        broker.sync_plan_execution(&broker.active_goal().unwrap()).unwrap();
        assert_eq!(broker.store.list_plan_resolution(&broker.session.id).unwrap().len(), 3);
        let snapshot = broker.snapshot().unwrap();
        let projected: Vec<_> = snapshot.timeline.iter().flat_map(|entry| match entry {
            TimelineEntry::Exchange { exchange, .. } => exchange.node_list.iter().filter_map(|node| match node {
                ExchangeNode::PlanEvent { event } => match &event.content {
                    crate::plan::PlanEventContent::Resolution { resolution, .. } => Some(resolution.kind),
                    _ => None,
                },
                _ => None,
            }).collect::<Vec<_>>(),
            _ => Vec::new(),
        }).collect();
        assert_eq!(projected, vec![PlanResolutionKind::Blocked, PlanResolutionKind::Blocked,
            PlanResolutionKind::Completed]);
    }

    #[tokio::test]
    async fn plan_continuation_and_resumption_retain_execution_controls() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "plan-continuation-test".into(),
                backend: BackendLaunch { kind: "mock".into(), command: vec!["mock".into()] },
                model: "mock-model".into(),
                effort: "medium".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        ).unwrap();
        let planned = broker.dispatch(Request {
            id: 1,
            method: "prompt.submit".into(),
            params: json!({ "text": "/plan build the feature" }),
        }).await;
        assert!(planned.response.error().is_none(), "{:?}", planned.response.error());
        assert!(broker.capability.native_goal);
        let backend = Arc::new(ContinuingPlanBackend {
            inner: Arc::clone(&broker.backend),
            invocation: std::sync::atomic::AtomicUsize::new(0),
            block_first: false,
        });
        broker.backend = backend.clone();
        broker.accept_plan(json!({ "execution_mode": "write" })).await.unwrap();
        assert!(!broker.active_goal().unwrap().native);
        let execution_exchange = broker.store.list_exchange(&broker.session.id).unwrap().pop().unwrap();
        assert_eq!(execution_exchange.state, ExchangeState::Running);
        assert!(execution_exchange.checkpoint_after.is_none());

        let prompt = broker.plan_goal_prompt(
            &broker.active_goal().unwrap(), PlanExecutionPromptKind::Continue,
        ).unwrap().unwrap();
        broker.resume_exchange(json!({ "text": prompt })).await.unwrap();
        broker.continue_goal().await.unwrap();
        let continued = broker.store.list_exchange(&broker.session.id).unwrap().pop().unwrap();
        assert_eq!(continued.id, execution_exchange.id);
        assert_eq!(continued.state, ExchangeState::Running);
        assert!(continued.checkpoint_after.is_none());
        broker.pause_goal().await.unwrap();
        broker.resume_goal().await.unwrap();
        assert_eq!(backend.invocation.load(Ordering::SeqCst), 4);
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Complete);
        assert_eq!(broker.snapshot().unwrap().goal_execution.unwrap().state, PlanExecutionState::Complete);
        assert_eq!(broker.store.list_exchange(&broker.session.id).unwrap().last().unwrap().state, ExchangeState::Complete);
        let settled_goal = serde_json::to_value(broker.active_goal().unwrap()).unwrap();
        let settled_execution = serde_json::to_value(broker.snapshot().unwrap().goal_execution).unwrap();
        let settled_resolution = serde_json::to_value(broker.store.list_plan_resolution(&broker.session.id).unwrap()).unwrap();
        let settled_audit = serde_json::to_value(broker.store.list_plan_audit(&broker.session.id).unwrap()).unwrap();
        broker.clock = Box::new(FixedClock(900));
        let mut event = Vec::new();
        assert!(!broker.apply_goal_evidence(crate::goal::TurnEvidence::default(), &mut event).unwrap());
        assert!(event.is_empty());
        broker.sync_plan_execution(&broker.active_goal().unwrap()).unwrap();
        assert_eq!(serde_json::to_value(broker.active_goal().unwrap()).unwrap(), settled_goal);
        assert_eq!(serde_json::to_value(broker.snapshot().unwrap().goal_execution).unwrap(), settled_execution);
        assert_eq!(serde_json::to_value(broker.store.list_plan_resolution(&broker.session.id).unwrap()).unwrap(), settled_resolution);
        assert_eq!(serde_json::to_value(broker.store.list_plan_audit(&broker.session.id).unwrap()).unwrap(), settled_audit);
    }

    #[tokio::test]
    async fn clearing_goal_settles_its_exchange_before_reopen() {
        for (scenario, terminal_outcome) in [
            ("active", None),
            ("paused", None),
            ("retry", None),
            ("complete", Some(ExchangeState::Complete)),
            ("failed", Some(ExchangeState::Failed)),
        ] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let initialize = || InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "clear-goal-test".into(),
                backend: BackendLaunch { kind: "mock".into(), command: vec!["mock".into()] },
                model: "mock-model".into(),
                effort: "medium".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            };
            let mut broker = HarnessBroker::initialize_with_clock(initialize(), Box::new(FixedClock(100))).unwrap();
            broker.set_goal(json!({ "objective": "continue this work" })).await.unwrap();
            let before = broker.store.list_exchange(&broker.session.id).unwrap().pop().unwrap();
            assert_eq!(before.state, ExchangeState::Running);
            assert!(before.checkpoint_before.is_some());
            assert!(before.checkpoint_after.is_none());
            if scenario == "paused" {
                broker.pause_goal().await.unwrap();
            }
            broker.clock = Box::new(FixedClock(500));
            let terminal_before = if let Some(outcome) = terminal_outcome {
                let mut exchange = before.clone();
                broker.capture_final_checkpoint(&mut exchange, outcome).await.unwrap();
                broker.exchange_runtime = None;
                Some(serde_json::to_value(exchange).unwrap())
            } else {
                None
            };
            if scenario == "retry" {
                let mut damaged = before.clone();
                damaged.checkpoint_before = Some("missing-checkpoint".into());
                broker.store.save_exchange(&damaged).unwrap();
                assert!(broker.clear_goal().await.is_err());
                assert_eq!(broker.active_goal().unwrap().state, GoalState::Cleared);
                let mut failed = broker.store.list_exchange(&broker.session.id).unwrap().pop().unwrap();
                assert_eq!(failed.state, ExchangeState::Finalizing);
                assert!(failed.finalization_error.is_some());
                assert_eq!(failed.finalization_outcome, Some(ExchangeState::Cancelled));
                failed.checkpoint_before = before.checkpoint_before.clone();
                broker.store.save_exchange(&failed).unwrap();
            }
            let (_, events) = broker.clear_goal().await.unwrap();
            let settled = broker.store.list_exchange(&broker.session.id).unwrap().pop().unwrap();
            assert_eq!(settled.state, terminal_outcome.unwrap_or(ExchangeState::Cancelled));
            assert!(settled.checkpoint_after.is_some());
            assert_eq!(settled.turn.len(), before.turn.len());
            assert_eq!(settled.turn[0].state(), before.turn[0].state());
            assert_eq!(settled.elapsed(90_000), before.elapsed(500));
            assert_eq!(events.iter().any(|event| event.event == "exchange_complete"), terminal_outcome.is_none());
            if let Some(terminal_before) = terminal_before {
                assert_eq!(serde_json::to_value(&settled).unwrap(), terminal_before);
            }
            assert!(broker.session.goal_id.is_none());
            assert!(broker.exchange_runtime.is_none());
            let session_id = broker.session.id.clone();
            drop(broker);
            let reopened = HarnessBroker::initialize_with_clock(initialize(), Box::new(FixedClock(900))).unwrap();
            assert_eq!(reopened.session.id, session_id);
            let restored = reopened.store.list_exchange(&session_id).unwrap().pop().unwrap();
            assert_eq!(serde_json::to_value(restored).unwrap(), serde_json::to_value(settled).unwrap());
        }
    }

    #[tokio::test]
    async fn pauses_an_active_goal_after_a_backend_failure() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "failure-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(200)),
        )
        .unwrap();
        broker.backend = Arc::new(FailingBackend);
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "goal.set".into(),
                params: json!({ "objective": "finish the work" }),
            })
            .await;
        assert!(result.response.error().is_some());
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(interaction.len(), 1);
        assert_eq!(interaction[0].state, ExchangeState::Failed);
        assert!(interaction[0].checkpoint_after.is_some());
        assert!(
            interaction[0]
                .checkpoint_diff_text
                .as_deref()
                .is_some_and(|diff| diff.contains("failed/deep/write.txt"))
        );
    }

    #[tokio::test]
    async fn stores_provider_tasks_for_an_ordinary_chat_interaction() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "task-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(250)),
        )
        .unwrap();
        broker.backend = Arc::new(TaskUpdateBackend);
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "answer an ordinary question" }),
            })
            .await;
        assert!(result.response.error().is_none());
        let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        let task = interaction[0]
            .task
            .as_ref()
            .expect("ordinary task snapshot");
        assert_eq!(task.current[0].title, "Inspect ordinary chat");
        assert_eq!(interaction[0].kind, ExchangeKind::Chat);
    }

    #[tokio::test]
    async fn removes_transient_wait_without_persisting_a_timeline_node() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "parent-boundary-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(500)),
        )
        .unwrap();
        broker.backend = Arc::new(ParentBoundaryBackend);

        let result = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "wait for the explorer" }),
            })
            .await;
        assert!(result.response.error().is_none());
        let wait_update_list = result
            .event
            .iter()
            .filter(|event| {
                event.event == "backend_event"
                    && event.payload.get("kind").and_then(Value::as_str)
                        == Some("timeline_wait_updated")
            })
            .collect::<Vec<_>>();
        assert_eq!(wait_update_list.len(), 2);
        assert!(
            wait_update_list[0]
                .payload
                .pointer("/data/wait")
                .is_some_and(Value::is_object)
        );
        assert!(
            wait_update_list[1]
                .payload
                .pointer("/data/wait")
                .is_some_and(Value::is_null)
        );

        let snapshot = broker.snapshot().unwrap();
        assert!(snapshot.active_wait.is_none());
        assert!(
            snapshot.exchange[0]
                .node_list
                .iter()
                .all(|node| !node.id().ends_with(":wait"))
        );
        let message_list = snapshot.exchange[0].turn[0].messages();
        assert_eq!(message_list.len(), 2);
        assert_eq!(message_list[0].text(), "An intermediate answer.");
        assert_eq!(message_list[1].text(), "A final synthesis.");
    }

    #[tokio::test]
    async fn provider_and_command_changes_use_separate_diff_sources() {
        let repository = repository();
        std::fs::write(repository.path().join(".gitignore"), "target/\n").unwrap();
        git(repository.path(), &["add", ".gitignore"]);
        git(repository.path(), &["commit", "-qm", "ignore build output"]);
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "interaction-diff-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        broker.backend = Arc::new(NestedWriteBackend);
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "create a nested module" }),
            })
            .await;
        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
        let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(interaction.len(), 1);
        assert_eq!(interaction[0].turn.len(), 1);
        assert_eq!(
            interaction[0].turn[0].messages()[0].text(),
            "Creating the nested module."
        );
        assert_eq!(interaction[0].turn[0].tools().count(), 2);
        assert!(
            interaction[0]
                .checkpoint_diff_text
                .as_deref()
                .is_some_and(|diff| diff.contains("apps/new/deep/module/lib.rs"))
        );
        let attributed_diff = interaction[0]
            .attributed_diff_text
            .as_deref()
            .expect("attributed interaction diff");
        assert!(attributed_diff.contains("seed.txt"));
        assert!(!attributed_diff.contains("apps/new/deep/module/lib.rs"));
        assert!(!interaction[0].attributed_matches_checkpoint);
        let interaction_diff = interaction[0].checkpoint_diff_text.as_deref().unwrap();
        assert!(interaction_diff.contains("seed.txt"));
        assert!(!interaction_diff.contains("target/generated/deep/artifact.txt"));
        assert!(
            interaction[0].turn[0]
                .messages()
                .iter()
                .any(|message| message.text() == "The nested module is ready.")
        );
    }

    #[tokio::test]
    async fn matching_provider_and_checkpoint_changes_share_one_canonical_diff() {
        let repository = repository();
        std::fs::write(repository.path().join(".gitignore"), "apps/\ntarget/\n").unwrap();
        git(repository.path(), &["add", ".gitignore"]);
        git(
            repository.path(),
            &["commit", "-qm", "ignore generated output"],
        );
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "matching-interaction-diff-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        broker.backend = Arc::new(NestedWriteBackend);

        let occupied: Vec<_> = (0..4)
            .map(|_| broker.repositories.reads.submit(0, |_| Ok(())).unwrap())
            .collect();
        let (mut rejected, _) = broker
            .interaction_for_turn("not started", true, 300)
            .await
            .unwrap();
        let error = broker
            .start_exchange_runtime(&mut rejected, 300)
            .await
            .unwrap_err();
        assert!(error.to_string().contains("capacity is full"));
        assert!(rejected.checkpoint_before.is_none());
        assert!(broker.exchange_runtime.is_none());
        assert!(
            broker
                .store
                .list_exchange(&broker.session.id)
                .unwrap()
                .is_empty()
        );
        for task in occupied {
            task.finish().await.unwrap();
        }

        let result = broker
            .dispatch(Request {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "change the seed" }),
            })
            .await;

        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
        let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(interaction.len(), 1);
        assert!(interaction[0].attributed_matches_checkpoint);
        assert_eq!(broker.diff.usage().cache.cached_entries, 1);
        assert_eq!(
            interaction[0].attributed_diff_text,
            interaction[0].checkpoint_diff_text
        );
    }

    #[test]
    fn provider_change_index_includes_referenced_child_turns() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "child-attribution-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        let child_run_id = "child-run";
        let child_exchange = |id: &str, path: &str, ordinal: u64| {
            let mut exchange = completed_interaction(id, &broker.session.id, Vec::new());
            exchange.ordinal = ordinal;
            let mut turn = crate::turn::Turn::new(
                format!("{id}:turn:1"),
                crate::backend::ProviderAddress {
                    thread_id: child_run_id.into(),
                    turn_id: format!("{id}:provider"),
                },
                1,
            );
            turn.record_tool(&crate::backend::ToolActivity {
                id: format!("{id}:change"),
                kind: crate::backend::ToolActivityKind::FileChange,
                title: "file changes".into(),
                output: None,
                output_delta: false,
                status: Some("completed".into()),
                change: crate::backend::ProviderChangeSet {
                    file: vec![crate::backend::ProviderFileChange {
                        path: path.into(),
                        move_path: None,
                        kind: crate::backend::ProviderChangeKind::Add,
                        diff: "child".into(),
                    }],
                },
            })
            .unwrap();
            turn.finish(crate::turn::TurnOutcome::Completed, 2).unwrap();
            exchange.turn.push(turn);
            exchange
        };
        let mut child_interaction = child_exchange("child-interaction", "child.rs", 1);
        let mut unrelated = child_exchange("unrelated-child-exchange", "unrelated.rs", 2);
        let mut child_run = Agent::pending(&broker.session.id, "explorer", "edit child", 1);
        child_run.id = child_run_id.into();
        broker.store.save_agent_run(&child_run).unwrap();
        unrelated.agent_id = child_run_id.into();
        broker.store.save_exchange(&unrelated).unwrap();

        child_interaction.agent_id = child_run_id.into();
        broker.store.save_exchange(&child_interaction).unwrap();
        let parent_interaction = completed_interaction(
            "parent-interaction",
            &broker.session.id,
            vec![ExchangeNode::AgentReference {
                agent: crate::agent::Delegation {
                    id: "child-reference".into(),
                    parent_exchange_id: "parent-interaction".into(),
                    parent_turn_id: None,
                    task: "inspect".into(),
                    child_agent_id: child_run_id.into(),
                    child_exchange_id: "child-interaction".into(),
                    created_at_ms: 1,
                },
            }],
        );

        let index = broker
            .interaction_provider_change_index(&parent_interaction)
            .unwrap();

        assert_eq!(
            index.paths().iter().map(String::as_str).collect::<Vec<_>>(),
            vec!["child.rs"]
        );
    }

    #[tokio::test]
    async fn rejects_resuming_a_session_owned_by_another_backend() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "backend-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        let active_id = broker.session.id.clone();
        let mut incompatible = broker.session.clone();
        incompatible.id = "copilot-session".into();
        incompatible.primary_agent_id = HarnessSession::primary_agent_id(&incompatible.id);
        incompatible.backend = "copilot".into();
        incompatible.lease_owner = None;
        incompatible.lease_expires_at_ms = None;
        broker.store.save_session(&incompatible).unwrap();
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "session.resume".into(),
                params: json!({ "session_id": incompatible.id }),
            })
            .await;
        assert!(result.response.error().is_some());
        assert_eq!(broker.session.id, active_id);
    }
}
