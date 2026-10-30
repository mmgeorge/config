mod acceptance;
mod admission;
#[cfg(test)]
mod change_results_test;
mod checks;
mod configuration;
mod execution;
mod fork;
mod interaction;
pub mod publication;
use publication::{ConfigurationProgressState, SessionUpdate, TimelineUpdate};
mod new_session;
mod observation;
pub(crate) mod plan_entry;
mod project_checks;
mod review_question;
pub(crate) use observation::BrokerObserver;

pub use fork::{ForkPreparation, prepare_provider_fork};
pub use new_session::prepare_new_session;

use crate::agent::{Agent, AgentLifecycleEvent, AgentRegistry, AgentState};
use crate::backend::approval::{ApprovalRequestView, PermissionCoordinator};
use crate::backend::{
    Backend, BackendCapability, BackendCatalogRequest, BackendEvent, BackendInput, BackendLaunch,
    BackendRequest, PromptMode,
};
#[cfg(test)]
use crate::backend::{BackendEventSink, BackendForkRequest};
use crate::buffer::session::SessionPresentation;
use crate::checkpoint::{GitCheckpoint, checkpoint_diff, checkpoint_diff_for_paths};
use crate::exchange::{
    ActiveWait, ChecklistTracker, Exchange, ExchangeComment, ExchangeNode, ExchangeState,
    ProviderChangeIndex,
};
use crate::goal::{ContinuationDecision, GoalRecord, GoalState};
use crate::permissions::store::PermissionStore;
use crate::plan::state_machine::{PlanEvent, PlanStateMachine};
use crate::plan::{
    ArtifactSummary, PlanAcceptance, PlanElicitation, PlanExecutionLifecycleEvent,
    PlanExecutionPromptKind, PlanExecutionRecord, PlanExecutionState, PlanFileStore,
    PlanLifecycleKind, PlanLifecycleRecord, PlanPrompt, PlanQuestionAnswer, PlanQuestionResponse,
    PlanQuestionSet, PlanQuestionWithdrawal, PlanRecord, PlanState, digest as plan_digest,
};
use crate::protocol::RuntimeMethod;
use crate::session::{
    ApprovalMode, ContextUsage, HarnessPreference, HarnessSession, ModelSetting, PlanExecutor,
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
    atomic::{AtomicBool, AtomicU64, Ordering},
};
use std::time::{SystemTime, UNIX_EPOCH};
use tokio::sync::Notify;
use uuid::Uuid;

/// Defines wall-clock access consumed by durable timelines.
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
    /// Creates or resumes only a tool-free Background conversation when present.
    #[serde(default)]
    pub background: Option<crate::background::BackgroundIdentity>,
    #[serde(default)]
    /// Resolves the dotfiles-owned global Rulesync catalog independently of workspace.
    pub config_dir: Option<String>,
    pub data_root: String,
    #[serde(default)]
    pub permission_file: Option<String>,
    pub workspace: String,
    pub client_id: String,
    pub backend: BackendLaunch,
    #[serde(default)]
    /// Supplies explicit launch descriptors for cross-provider children.
    pub backend_launch_by_provider: HashMap<String, BackendLaunch>,
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

fn agent_summaries(
    runs: &[Agent],
    exchanges: &[Exchange],
) -> std::collections::HashMap<String, Value> {
    runs.iter().map(|run| {
        let owned: Vec<_> = exchanges.iter().filter(|exchange| exchange.agent_id == run.id).collect();
        let tool_count: usize = owned.iter().map(|exchange| exchange.tool_counter.total).sum();
        let failed_count: usize = owned.iter().map(|exchange| exchange.tool_counter.failed).sum();
        let latest = owned.iter().rev().find(|exchange| exchange.state() == ExchangeState::Running)
            .or_else(|| owned.last());
        let target = latest.and_then(|exchange| exchange.turn.iter().rev().find(|turn| matches!(turn.state(),crate::turn::TurnState::Running)))
            .map(|turn| serde_json::to_value(turn.provider()).expect("provider address"));
        (run.id.clone(), json!({"state":latest.map(|exchange| serde_json::to_value(exchange.state()).expect("exchange state")).unwrap_or_else(|| serde_json::to_value(run.state).expect("agent state")),
            "tool_count":tool_count,"failed_count":failed_count,"target":target,
            "exchange_count":owned.len(),"latest_exchange_id":latest.map(|exchange|exchange.id.as_str())}))
    }).collect()
}

fn preview_lines(timeline: &[TimelineEntry]) -> Result<Vec<String>> {
    let mut display = crate::buffer::identity::DisplayRegistry::default();
    let width = forge_buffer::width::WidthProfile {
        columns: 100,
        ..Default::default()
    };
    let mut lines = Vec::new();
    for (index, entry) in timeline.iter().rev().take(20).rev().enumerate() {
        let projected = crate::buffer::projection::project(
            entry,
            &width,
            index > 0,
            &Default::default(),
            &Default::default(),
            &mut display,
        )?;
        for block in crate::buffer::sections::preview(projected.entry)?.block {
            for row in block.text.wire_rows() {
                if lines.len() >= 200 {
                    return Ok(lines);
                }
                lines.push(row.chars().take(1000).collect());
            }
        }
    }
    Ok(lines)
}

/// Represents the complete client-visible state for one active session.
#[derive(Clone, Debug, Serialize)]
pub struct BrokerSnapshot {
    pub ask_model: crate::settings::AskModel,
    /// Confirmed access label computed by the authoritative filesystem policy.
    pub access_abbreviation: String,
    pub subagent_tools: bool,
    pub lsp_tools: bool,
    pub session_tools: bool,
    pub status_thoughts: bool,
    pub check_editing: bool,
    pub runtime_epoch: String,
    pub snapshot_revision: u64,
    pub checks_running: bool,
    pub check_timing: crate::checks::CheckTiming,
    /// Effective completion-check enablement, independent of manual and plan checks.
    pub automatic_checks: bool,
    pub checks_deferred: bool,
    pub active_check_run: Option<String>,
    pub task_operation: Option<Value>,
    pub session: HarnessSession,
    #[serde(skip)]
    pub exchange: Vec<Exchange>,
    pub capability: BackendCapability,
    pub no_checkpoint: bool,
    pub restore_recovery: Option<Value>,
    pub active_plan: Option<PlanRecord>,
    pub active_elicitation: Option<ActiveElicitation>,
    pub artifact: Vec<ArtifactSummary>,
    #[serde(skip)]
    pub timeline: Vec<TimelineEntry>,
    pub timeline_revision: u64,
    pub status: crate::session::state_machine::SessionPhase,
    pub active_wait: Option<ActiveWait>,
    pub prompt_history: Vec<crate::attachment::PromptDraft>,
    pub agent: AgentSnapshot,
    pub approval: Vec<ApprovalRequestView>,
}

/// Retains reusable child identities and their durable timelines inside one root.
#[derive(Clone, Debug, Serialize)]
pub struct AgentSnapshot {
    pub run: Vec<Agent>,
    pub summary: std::collections::HashMap<String, Value>,
    #[serde(skip)]
    pub exchange: Vec<Exchange>,
}

/// Represents a stored session timeline projected without acquiring its lease.
#[derive(Clone, Debug, Serialize)]
pub struct SessionPreview {
    pub lines: Vec<String>,
    pub session: HarnessSession,
    #[serde(skip)]
    pub exchange: Vec<Exchange>,
    #[serde(skip)]
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

struct ProviderSteering<'turn> {
    active: &'turn mut crate::backend::ActiveSteering,
    deferred: &'turn mut std::collections::VecDeque<crate::backend::SteerCommand>,
    continuation: Option<crate::backend::SteerCommand>,
}

struct ExchangeRuntime {
    task_permit: Option<crate::task::TaskPermit>,
    checkpoint: Option<crate::checkpoint::CheckpointTask>,
    tool_gate: Option<crate::checkpoint::ToolGate>,
    exchange_id: String,
    checklist: ChecklistTracker,
    retraction_eligible: bool,
    active_wait: Option<ActiveWait>,
}

impl ExchangeRuntime {
    fn retractable_before_tools(&self) -> bool {
        self.retraction_eligible && self.tools_blocked()
    }

    fn tools_blocked(&self) -> bool {
        self.tool_gate.as_ref().is_some_and(|gate| !gate.ready())
    }
}

impl Drop for ExchangeRuntime {
    fn drop(&mut self) {
        if let Some(gate) = &self.tool_gate {
            let _ = gate.complete(Err("exchange ended before checkpoint persistence".into()));
        }
    }
}

struct ChildExchangeRuntime {
    exchange: Exchange,
    checklist: ChecklistTracker,
}

/// Represents one request result plus asynchronous events emitted before its response.
pub struct DispatchResult {
    pub response: Response,
    pub event: Vec<SessionEvent>,
}

/// Owns provider, permission, and analysis infrastructure shared by every session controller.
pub struct BrokerRuntime {
    pub(crate) lsp: Arc<crate::control_tools::lsp::LspService>,
    repositories: Arc<forge_git::store::RepositoryStore>,
    diff: Arc<forge_diff::engine::DiffEngine>,
    workspace_kind: WorkspaceKind,
    backend: Arc<dyn Backend>,
    pub(crate) permission_store: Arc<RwLock<PermissionStore>>,
    pub(crate) profiles: Arc<crate::agent::AgentProfileStore>,
    permission_coordinator: Arc<PermissionCoordinator>,
    trace: Arc<TraceStore>,
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
        let profiles = Arc::new(crate::agent::AgentProfileStore::new(
            permission_file
                .parent()
                .unwrap_or(&data_root)
                .join("subagents.json"),
        ));
        let permission_store = Arc::new(RwLock::new(PermissionStore::load(
            permission_file,
            &workspace,
        )?));
        let permission_coordinator =
            Arc::new(PermissionCoordinator::new(Arc::clone(&permission_store)));
        let trace = Arc::new(TraceStore::open(&data_root)?);
        let backend = Arc::<dyn Backend>::from(crate::backend::build(
            request.backend.clone(),
            Arc::clone(&permission_coordinator),
            Arc::clone(&trace),
            data_root.clone(),
        )?);
        Ok(Arc::new(Self {
            lsp: Arc::new(Default::default()),
            repositories,
            diff,
            workspace_kind,
            backend,
            permission_store,
            profiles,
            permission_coordinator,
            trace,
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
    /// Cancellation revision captured by queued requests before waiting for admission.
    pub(crate) request_revision: AtomicU64,
    requested: Arc<AtomicBool>,
    submission_cancelled: Arc<AtomicBool>,
    restore_prompt: AtomicBool,
    restart: AtomicBool,
    cleanup: tokio::sync::watch::Sender<ExecutionCleanup>,
    retraction_allowed: AtomicBool,
    notify: Notify,
    state: tokio::sync::watch::Sender<bool>,
}

impl TurnCancellation {
    fn new() -> Self {
        Self {
            request_revision: AtomicU64::new(0),
            requested: Arc::new(AtomicBool::new(false)),
            submission_cancelled: Arc::new(AtomicBool::new(false)),
            restore_prompt: AtomicBool::new(false),
            restart: AtomicBool::new(false),
            cleanup: tokio::sync::watch::channel(ExecutionCleanup::Idle).0,
            retraction_allowed: AtomicBool::new(false),
            notify: Notify::new(),
            state: tokio::sync::watch::channel(false).0,
        }
    }

    /// Clear cancellation state before dispatching the next broker request.
    pub fn arm(&self, retraction_allowed: bool) {
        self.requested.store(false, Ordering::Release);
        self.submission_cancelled.store(false, Ordering::Release);
        self.restore_prompt.store(false, Ordering::Release);
        self.restart.store(false, Ordering::Release);
        self.cleanup.send_replace(ExecutionCleanup::Idle);
        self.retraction_allowed
            .store(retraction_allowed, Ordering::Release);
        self.state.send_replace(false);
    }

    /// Request cancellation without waiting for the broker's serialized request lane.
    pub fn request(&self, restore_prompt: bool) {
        self.request_revision.fetch_add(1, Ordering::AcqRel);
        self.submission_cancelled.store(true, Ordering::Release);
        self.restore_prompt.store(restore_prompt, Ordering::Release);
        self.requested.store(true, Ordering::Release);
        self.state.send_replace(true);
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
        self.submission_cancelled.store(true, Ordering::Release);
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

    /// Observe cancellation and rearming without acquiring execution ownership.
    pub(crate) fn subscribe(&self) -> tokio::sync::watch::Receiver<bool> {
        self.state.subscribe()
    }
}

#[derive(Debug)]
/// Identifies user cancellation across broker and task-coordinator error boundaries.
pub(crate) struct TurnCancelled;

impl std::fmt::Display for TurnCancelled {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("Turn cancelled by user")
    }
}

impl std::error::Error for TurnCancelled {}

#[derive(Debug)]
/// Rejects a request before it owns or mutates the active execution.
pub struct RequestNotAdmitted(
    /// Describes the unavailable execution or cleanup owner.
    pub &'static str,
);

impl std::fmt::Display for RequestNotAdmitted {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.0)
    }
}

impl std::error::Error for RequestNotAdmitted {}

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
    origin: crate::exchange::ExchangeOrigin,
    check_owner: Option<String>,
    automatic_owner: Option<String>,
    plan_event_id: Option<String>,
    prompt: String,
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
            check_owner: None,
            automatic_owner: None,
            prompt,
            origin: crate::exchange::ExchangeOrigin::User,
            agent_run_id: None,
        }
    }
}

/// Coordinates backend turns, durable state, plan review, goals, and interaction checkpoints.
pub struct HarnessBroker {
    pub(crate) task_controller: Arc<crate::task::SessionTaskController>,
    context_usage_fresh: bool,
    pub(crate) lsp: Arc<crate::control_tools::lsp::LspService>,
    checkpoint: Option<GitCheckpoint>,
    active_operation_id: Option<String>,
    observer: Arc<observation::BrokerObserver>,
    repositories: Arc<forge_git::store::RepositoryStore>,
    diff: Arc<forge_diff::engine::DiffEngine>,
    store: SqliteStore,
    pub(crate) submission_store: Arc<crate::backend::submission::SubmissionStore>,
    plan_file: Arc<PlanFileStore>,
    check_runner: Arc<crate::checks::CheckSupervisor>,
    check_setup: bool,
    /// Shares admitted review documents between view requests and serialized question turns.
    pub(crate) plan_review: Arc<crate::plan::review_document::PlanReviewStore>,
    pub(crate) revision_review: Arc<crate::plan::revision_review::RevisionReview>,
    workspace_kind: WorkspaceKind,
    pub(crate) data_root: PathBuf,
    client_id: String,
    backend_launch: BackendLaunch,
    backend: Arc<dyn Backend>,
    session: HarnessSession,
    capability: BackendCapability,
    pub(crate) goal_max_turns: u32,
    permission_store: Arc<RwLock<PermissionStore>>,
    permission_coordinator: Arc<PermissionCoordinator>,
    pub(crate) trace: Arc<TraceStore>,
    event_sink: Option<publication::SessionUpdateSink>,
    clock: Box<dyn Clock>,
    exchange_runtime: Option<ExchangeRuntime>,
    agent_registry: AgentRegistry,
    agent_coordinator: Arc<crate::agent::AgentCoordinator>,
    pub(crate) agent_backend_factory: crate::agent::AgentBackendFactory,
    child_backend_by_agent: HashMap<String, Arc<dyn Backend>>,
    diagnostic_native_thread: HashSet<String>,
    mcp_notice: HashMap<String, String>,
    child_exchange_runtime_by_agent: HashMap<String, ChildExchangeRuntime>,
    presentation: Arc<std::sync::Mutex<SessionPresentation>>,
    prompt_admission: Option<(Option<forge_buffer::identity::DocumentId>, u64)>,
    pub(crate) input_submission: Option<Arc<crate::backend::submission::Submission>>,
    pub(crate) input_purpose: crate::input::InputPurpose,
    pub(crate) input_wakeup: Option<Arc<tokio::sync::Notify>>,
    submitted_attachments: Vec<crate::attachment::InputAttachment>,
    active_wait_projection: Option<ActiveWait>,
    pub(crate) parent_input: crate::backend::SteeringLane,
    timeline_reconciled_after_dispatch: bool,
    turn_cancellation: Arc<TurnCancellation>,
    pub(crate) startup_timing: std::collections::BTreeMap<&'static str, f64>,
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
        let startup_started = std::time::Instant::now();
        let mut startup_timing = std::collections::BTreeMap::new();
        let data_root = PathBuf::from(&request.data_root);
        let mut store = SqliteStore::open(&data_root)?;
        startup_timing.insert(
            "database_open_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        store.recover_session_deletions()?;
        startup_timing.insert(
            "deletion_recovery_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
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
        let mut created_session = false;
        let mut session = match request.session_id.as_deref().filter(|_| !force_new_session) {
            Some(session_id) => {
                let selected = store
                    .load_session(session_id)?
                    .context("session not found")?;
                anyhow::ensure!(
                    selected.background.is_some() == request.background.is_some(),
                    "Background and interactive sessions cannot share a provider conversation"
                );
                anyhow::ensure!(
                    crate::workspace::same(&selected.workspace, &workspace),
                    "session uses a different workspace"
                );
                anyhow::ensure!(
                    selected.backend == request.backend.kind,
                    "session uses a different configured backend"
                );
                store.acquire_session_lease(session_id, &request.client_id)?
            }
            None => {
                let latest_session = if force_new_session {
                    None
                } else {
                    store
                        .list_session(Some(&workspace))?
                        .into_iter()
                        .find(|session| {
                            session.background.is_none()
                                && session.purpose == crate::session::SessionPurpose::Conversation
                                && session.backend == request.backend.kind
                        })
                };
                match latest_session {
                    Some(session) => {
                        store.acquire_session_lease(&session.id, &request.client_id)?
                    }
                    None => {
                        created_session = true;
                        let preference = if request.background.is_some() {
                            None
                        } else {
                            store.load_preference(&workspace, &request.backend.kind)?
                        };
                        let session_id = Uuid::new_v4().to_string();
                        let mut session = HarnessSession {
                            purpose: crate::session::SessionPurpose::Conversation,
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
                            plan_executor: preference
                                .as_ref()
                                .map_or_else(Default::default, |value| value.plan_executor.clone()),
                            plan_compact: preference
                                .as_ref()
                                .is_some_and(|value| value.plan_compact),
                            plan_auto_approve_revisions: preference
                                .as_ref()
                                .is_none_or(|value| value.plan_auto_approve_revisions),
                            approval_timeout: Default::default(),
                            answer_timeout: Default::default(),
                            context_window: None,
                            service_tier: preference
                                .as_ref()
                                .map(|value| value.service_tier)
                                .unwrap_or_default(),
                            access: preference
                                .as_ref()
                                .map(|value| value.access.clone())
                                .unwrap_or_default(),
                            shell: preference
                                .as_ref()
                                .map_or(Default::default(), |preference| preference.shell),
                            approval_mode: preference
                                .as_ref()
                                .map_or(ApprovalMode::Ask, |value| value.default_approval),
                            active_task: None,
                            default_approval: preference
                                .as_ref()
                                .map_or(ApprovalMode::Ask, |value| value.default_approval),
                            plan_approval: preference
                                .as_ref()
                                .and_then(|value| value.plan_approval),
                            created_at_ms: now_ms,
                            updated_at_ms: now_ms,
                            lease_owner: None,
                            native_fork: false,
                            native_compact: backend_descriptor.capability.native_compact,
                            context_usage: None,
                            background: request.background.clone(),
                            auto_compaction: Default::default(),
                            last_compaction_turn: None,
                        };
                        session.context_window = preference.as_ref().and_then(|preference| {
                            preference
                                .model_setting
                                .get(&session.model)
                                .and_then(|setting| setting.context_window.clone())
                        });
                        session.acquire_lease(&request.client_id)?;
                        session
                    }
                }
            }
        };
        startup_timing.insert(
            "session_selected_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        if session.provider_label.is_empty() {
            session.provider_label = backend_descriptor.label.clone();
        }
        if session.resolved_model.is_none() && session.model != "default" {
            session.resolved_model = Some(session.model.clone());
        }
        session.updated_at_ms = now_ms;
        let shared = crate::settings::SettingsStore::open(&data_root)?.get(
            &session.workspace,
            &session.id,
            &session.backend,
        )?;
        shared
            .desired
            .apply(&mut session, backend_descriptor.capability.native_compact)?;
        if created_session && request.background.is_none() {
            shared.apply_provider_defaults(&mut session)?;
            session.approval_mode = session.default_approval;
        }
        if request.background.is_some() {
            session.access.write_access = crate::session::WriteScope::Off;
        }
        if session.model == "default" && session.resolved_model.is_none() {
            session.resolved_model =
                store.latest_resolved_default_model(&session.workspace, &session.backend)?;
        }
        permission_coordinator.configure_input_timeouts(
            &session.id,
            session.approval_timeout,
            session.answer_timeout,
        )?;
        startup_timing.insert(
            "settings_applied_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        store.lock_session(&session.id)?;
        store.save_session(&session)?;
        if store
            .list_agent_run(&session.id)?
            .iter()
            .all(|agent| agent.id != session.primary_agent_id)
        {
            store.save_agent_run(&Agent::primary(&session.id, now_ms))?;
        }
        let previous_preference = store.load_preference(&session.workspace, &session.backend)?;
        if previous_preference.is_none() && request.background.is_none() {
            store.save_preference(
                &session.workspace,
                &session.backend,
                &preference_for_session(&session, None),
            )?;
        }
        let mut capability = backend_descriptor.capability;
        capability.native_fork = session.native_fork || capability.native_fork;
        capability.native_compact = session.native_compact || capability.native_compact;
        store.interrupt_detached_execution(&session.id)?;
        session = store
            .load_session(&session.id)?
            .context("session disappeared during recovery")?;
        let agent_registry = load_agent_registry(&store, &session.id)?;
        startup_timing.insert(
            "session_recovered_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        let agent_coordinator = crate::agent::AgentCoordinator::new(session.id.clone());
        agent_coordinator.set_enabled(shared.desired.subagent_tools.unwrap_or(false));
        runtime
            .lsp
            .set_enabled(&session.id, shared.desired.lsp_tools.unwrap_or(false));
        for task in store.list_agent_task(&session.id)? {
            let agent = agent_registry
                .get(&task.agent_id)
                .cloned()
                .context("stored task has no owning child")?;
            agent_coordinator.retain(agent, task)?;
        }
        let mut agent_backend_factory = crate::agent::AgentBackendFactory::new(
            request.backend.clone(),
            request.backend_launch_by_provider.clone(),
            permission_coordinator.clone(),
            trace.clone(),
            data_root.clone(),
        );
        agent_backend_factory.profiles = runtime.profiles.clone();
        agent_coordinator.set_profiles(agent_backend_factory.profiles.clone());
        let presentation = Arc::new(std::sync::Mutex::new(SessionPresentation::new(
            session.id.clone(),
        )));
        let check_runner = crate::checks::CheckSupervisor::open(
            crate::checks::CheckSupervisorContext {
                session_id: session.id.clone(),
                data_root: data_root.clone(),
                workspace: PathBuf::from(&session.workspace),
                root: data_root.join(&session.id).join("checks"),
            },
            Arc::clone(&presentation),
            store.objects.clone(),
            Arc::clone(&runtime.repositories),
            trace.clone(),
        )?;
        let plan_file = Arc::new(PlanFileStore::new(&data_root, &session.workspace));
        let observer = Arc::new(observation::BrokerObserver {
            admission: store.session_admission(&session.id)?,
            input_schedule: tokio::sync::watch::channel(Default::default()).0,
            lsp: runtime.lsp.clone(),
            status_thoughts: AtomicBool::new(shared.desired.status_thoughts.unwrap_or(true)),
            check_editing: Arc::new(AtomicBool::new(
                shared.desired.check_editing.unwrap_or(false),
            )),
            completion_queue: Default::default(),
            data_root: data_root.clone(),
            session_id: session.id.clone(),
            runtime_epoch: Uuid::new_v4().to_string(),
            session_metadata: store.session_metadata_writer(&session.id, &request.client_id)?,
            snapshot_revision: AtomicU64::new(0),
            metadata: RwLock::new(observation::ObservationMetadata {
                revision: 0,
                capability: capability.clone(),
                active_wait: None,
                no_checkpoint: matches!(workspace_kind, WorkspaceKind::Untracked(_)),
            }),
            plan_file: Arc::clone(&plan_file),
            permission_coordinator: Arc::clone(&permission_coordinator),
            permission_store: Arc::clone(&permission_store),
            agent_coordinator: Arc::clone(&agent_coordinator),
            check_runner: Arc::clone(&check_runner),
            trace: Arc::clone(&trace),
        });
        startup_timing.insert(
            "services_created_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        let mut broker = Self {
            task_controller: Arc::new(crate::task::SessionTaskController::new(
                observer.admission.clone(),
            )?),
            submission_store: crate::backend::submission::SubmissionStore::open(
                &data_root,
                trace.clone(),
            )?,
            context_usage_fresh: false,
            lsp: runtime.lsp.clone(),
            active_operation_id: None,
            observer,
            repositories: Arc::clone(&runtime.repositories),
            checkpoint: match &workspace_kind {
                WorkspaceKind::Git(workspace) if request.background.is_none() => {
                    Some(GitCheckpoint::new(workspace))
                }
                _ => None,
            },
            diff: Arc::clone(&runtime.diff),
            store,
            plan_file,
            check_runner,
            check_setup: false,
            workspace_kind,
            data_root,
            client_id: request.client_id,
            backend_launch: request.backend,
            backend,
            session,
            capability,
            goal_max_turns: request.goal_max_turns,
            permission_store,
            permission_coordinator,
            trace,
            plan_review: Arc::new(crate::plan::review_document::PlanReviewStore::default()),
            revision_review: Arc::new(crate::plan::revision_review::RevisionReview::default()),
            event_sink: None,
            clock,
            exchange_runtime: None,
            agent_registry,
            agent_coordinator,
            agent_backend_factory,
            child_backend_by_agent: HashMap::new(),
            diagnostic_native_thread: HashSet::new(),
            mcp_notice: HashMap::new(),
            child_exchange_runtime_by_agent: HashMap::new(),
            presentation,
            prompt_admission: None,
            input_submission: None,
            input_purpose: crate::input::InputPurpose::User,
            input_wakeup: None,
            active_wait_projection: None,
            parent_input: Default::default(),
            submitted_attachments: Vec::new(),
            timeline_reconciled_after_dispatch: false,
            turn_cancellation: Arc::new(TurnCancellation::new()),
            startup_timing,
        };
        broker.store.current_exchange_state(&broker.session.id)?;
        broker.refresh_input_schedule()?;
        let initial_timeline = broker.snapshot()?.timeline;
        broker.startup_timing.insert(
            "timeline_loaded_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        broker
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .initialize(initial_timeline)?;
        broker.startup_timing.insert(
            "presentation_initialized_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
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

    pub(crate) fn checks(&self) -> Arc<crate::checks::CheckSupervisor> {
        Arc::clone(&self.check_runner)
    }

    /// Share root-owned admission without requiring the active provider's broker lock.
    pub(crate) fn agent_coordinator(&self) -> Arc<crate::agent::AgentCoordinator> {
        self.agent_coordinator.clone()
    }

    /// Build the stable provider-catalog identity used outside serialized broker dispatch.
    pub fn backend_catalog_request(&self) -> BackendCatalogRequest {
        BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: self.session.id.clone(),
            workspace: self.session.workspace.clone(),
            access: self.session.access.clone(),
            approval_mode: self.session.approval_mode,
            backend_session_id: self.session.backend_session_id.clone(),
        }
    }

    /// Capture interactive startup settings without creating an exchange or task.
    pub(crate) fn initialization_request(&self) -> Option<BackendRequest> {
        (self.session.background.is_none()
            && self.session.purpose == crate::session::SessionPurpose::Conversation
            && self.provider_fork_state() == crate::session::ProviderForkState::Ready)
            .then(|| self.backend_request(BackendInput::from_text(""), PromptMode::Chat))
    }

    /// Retain provider history only while the captured session settings remain current.
    pub(crate) fn retain_initialized_session(
        &mut self,
        request: &BackendRequest,
        provider_id: String,
    ) -> Result<Option<BrokerSnapshot>> {
        if self.session.backend_session_id.as_deref() == Some(&provider_id) {
            return Ok(None);
        }
        if self.session.id != request.harness_session_id
            || self.session.backend_session_id != request.backend_session_id
            || self.session.model != request.model
            || self.session.effort != request.effort
            || self.session.access != request.access
            || self.session.approval_mode != request.approval_mode
        {
            return Ok(None);
        }
        self.session.backend_session_id = Some(provider_id);
        self.save_session()?;
        Ok(Some(self.snapshot()?))
    }

    fn control_turn_context(
        &self,
        mode: PromptMode,
    ) -> Option<crate::control_tools::ControlTurnContext> {
        if mode == PromptMode::Background {
            return Some(crate::control_tools::ControlTurnContext::inactive(mode));
        }
        let plan = self
            .current_plan_id()
            .ok()
            .flatten()
            .as_deref()
            .and_then(|plan_id| self.store.load_plan(plan_id).ok().flatten());
        let document = plan.as_ref().and_then(|plan| {
            self.plan_file
                .read_working_document(&self.session.id, &plan.id)
                .ok()
        });
        Some(crate::control_tools::ControlTurnContext {
            task_controller: Some(self.task_controller.clone()),
            native_goal: if mode == PromptMode::GoalContinuation {
                self.task_controller
                    .current()
                    .ok()
                    .flatten()
                    .and_then(|task| match task.workflow {
                        crate::task::TaskWorkflow::Goal { goal } if goal.native => {
                            Some(goal.objective)
                        }
                        _ => None,
                    })
            } else {
                None
            },
            task_permit: self
                .task_controller
                .current()
                .ok()
                .flatten()
                .map(|task| task.permit()),
            submission_cancelled: Some(Arc::clone(&self.turn_cancellation.submission_cancelled)),
            checkpoint: self
                .exchange_runtime
                .as_ref()
                .and_then(|runtime| runtime.tool_gate.clone()),
            agent: None,
            checks: None,
            check_shell: self.session.shell,
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
            has_active_execution: self.current_execution().ok()?.is_some_and(|execution| {
                !matches!(
                    execution.state,
                    PlanExecutionState::Complete | PlanExecutionState::Cancelled
                )
            }),
            workspace_root: Some(PathBuf::from(&self.session.workspace)),
            repository: None,
            harness: None,
            lsp: None,
            execution: None,
        })
    }

    /// Share the out-of-band permission lane with the broker transport loop.
    pub fn permission_coordinator(&self) -> Arc<PermissionCoordinator> {
        Arc::clone(&self.permission_coordinator)
    }

    /// Build the initial snapshot returned by the initialize response.
    pub fn snapshot(&self) -> Result<BrokerSnapshot> {
        self.observer.publish(self)
    }

    fn session_view(&self) -> Result<HarnessSession> {
        let mut session = self.session.clone();
        let committed = self
            .store
            .load_session(&session.id)?
            .context("observed session is missing")?;
        session.name = committed.name;
        session.active_task = committed.active_task;
        Ok(session)
    }

    /// Generate one tool-free Background exchange using its durable provider conversation.
    pub(crate) async fn generate_background(
        &mut self,
        input: String,
        identity: crate::background::BackgroundIdentity,
        config: crate::service::TextGenerationConfig,
        policy: crate::session::AutoCompaction,
        sink: publication::SessionUpdateSink,
    ) -> Result<String> {
        let crate::service::TextGenerationConfig {
            model,
            reasoning_effort,
            service_tier,
            ..
        } = config;
        let effort = reasoning_effort.unwrap_or_else(|| "low".into());
        anyhow::ensure!(
            self.session.background.is_some(),
            "Background request requires a Background session"
        );
        service_tier.validate(&self.capability)?;
        if let Some(previous) = &self.session.background
            && !previous.reason.is_empty()
            && self
                .store
                .current_exchange_state(&self.session.id)?
                .latest
                .is_none()
        {
            self.store
                .save_session_event(&crate::timeline::SessionEventRecord {
                    id: uuid::Uuid::new_v4().to_string(),
                    session_id: self.session.id.clone(),
                    owner: crate::timeline::TimelineOwner::session(&self.session.id),
                    created_at_ms: self.clock.now_ms(),
                    detail: crate::timeline::SessionEventKind::Configuration {
                        operation_id: uuid::Uuid::new_v4().to_string(),
                        state: "completed".into(),
                        field: vec!["background_session".into()],
                        reason: Some(previous.reason.clone()),
                    },
                })?;
        }
        self.session.background = Some(identity);
        self.session.model = model;
        self.session.effort = effort;
        self.session.context_window = None;
        self.session.service_tier = service_tier;
        self.session.access.write_access = crate::session::WriteScope::Off;
        self.session.auto_compaction = policy;
        self.checkpoint = None;
        self.event_sink = Some(sink);
        self.save_session()?;
        let result = self
            .run_interaction_input(
                input.clone(),
                PromptMode::Background,
                Some(ExchangeAdmission::chat(input.clone())),
                BackendInput::from_text(input),
            )
            .await;
        self.event_sink = None;
        result?;
        self.save_session()?;
        let exchange = self
            .store
            .latest_exchange(&self.session.id)?
            .context("Background exchange is missing")?;
        let text = exchange
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .filter(|message| {
                message.kind() == crate::turn::MessageKind::Assistant
                    && message.delivery() == crate::turn::MessageDelivery::Final
            })
            .map(|message| message.text())
            .collect::<Vec<_>>()
            .join("\n");
        Ok(text)
    }

    /// Retain a Background failure even when no assistant response can be published.
    pub(crate) fn background_failure(&mut self, stage: &str, reason: String) -> Result<()> {
        self.store
            .save_session_event(&crate::timeline::SessionEventRecord {
                id: uuid::Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                owner: crate::timeline::TimelineOwner::session(&self.session.id),
                created_at_ms: self.clock.now_ms(),
                detail: crate::timeline::SessionEventKind::Configuration {
                    operation_id: uuid::Uuid::new_v4().to_string(),
                    state: "failed".into(),
                    field: vec![stage.into()],
                    reason: Some(reason),
                },
            })?;
        Ok(())
    }

    /// Compact once per newly completed provider turn while retaining the full timeline.
    pub(crate) async fn auto_compact(&mut self) -> Result<()> {
        let policy = &self.session.auto_compaction;
        if !policy.enabled || !self.context_usage_fresh {
            return Ok(());
        }
        anyhow::ensure!(
            self.capability.native_compact,
            "This backend does not support automatic compaction"
        );
        policy.validate(self.session.context_usage.as_ref())?;
        if self
            .store
            .list_session_event(&self.session.id)?
            .iter()
            .any(|event| {
                matches!(&event.detail, SessionEventKind::Compaction { compaction }
                if compaction.state == crate::backend::CompactionState::Running
                    && Some(&compaction.thread_id) == self.session.backend_session_id.as_ref())
            })
        {
            return Ok(());
        }
        if !policy.due(
            self.session.context_usage.as_ref(),
            self.capability.native_compact,
        ) {
            return Ok(());
        }
        let latest_exchange = self.store.latest_exchange(&self.session.id)?;
        let latest = latest_exchange
            .as_ref()
            .and_then(|exchange| exchange.turn.last())
            .map(|turn| turn.id().to_owned());
        let native_compacted = latest_exchange.as_ref().is_some_and(|exchange| exchange.node_list.iter().any(|node|
            matches!(node, crate::exchange::ExchangeNode::Compaction { compaction, .. }
                if matches!(compaction.state, crate::backend::CompactionState::Completed | crate::backend::CompactionState::Running))));
        if latest.is_none() || latest == self.session.last_compaction_turn {
            return Ok(());
        }
        self.session.last_compaction_turn = latest;
        self.save_session()?;
        if native_compacted {
            return Ok(());
        }
        self.compact_session(false).await?;
        Ok(())
    }

    /// Shares the read-only session boundary with out-of-band controls.
    pub(crate) fn observer(&self) -> Arc<observation::BrokerObserver> {
        Arc::clone(&self.observer)
    }

    /// Route one client request through the owning feature boundary.
    pub async fn dispatch(&mut self, request: Request) -> DispatchResult {
        let id = request.id;
        let method = match RuntimeMethod::decode(&request.method) {
            Ok(method) => method,
            Err(error) => {
                return DispatchResult {
                    response: Response::failure(id, "unknown_method", error.to_string()),
                    event: Vec::new(),
                };
            }
        };
        let edit_trace = matches!(
            method,
            RuntimeMethod::SessionPlanEntityRename
                | RuntimeMethod::SessionPlanTestDelete
                | RuntimeMethod::SessionPlanCheckUpdate
        )
        .then(|| {
            crate::plan::edit_trace::PlanEditTrace::new(
                Arc::clone(&self.trace),
                &self.session.id,
                &request.method,
                &request.params,
            )
        });
        let dispatch_stage = edit_trace
            .as_ref()
            .map(|trace| trace.stage("broker.dispatch"));
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
        if method == RuntimeMethod::SessionCommandApply {
            self.active_operation_id = request
                .params
                .get("operation_id")
                .and_then(Value::as_str)
                .map(str::to_owned);
        } else {
            self.active_operation_id = None;
        }
        let mutation_stage = edit_trace
            .as_ref()
            .map(|trace| trace.stage("broker.mutation"));
        let command = (|| -> Result<_> {
            let mut candidate = request.clone();
            if candidate.params.get("operation_id").is_none()
                && let Some(id) = candidate
                    .params
                    .pointer("/_command/id")
                    .and_then(Value::as_str)
                    .map(str::to_owned)
            {
                candidate.params["operation_id"] = json!(id);
            }
            if method == RuntimeMethod::SessionQuestionContinue {
                let prior = candidate.params["operation_id"]
                    .as_str()
                    .map(|id| self.store.load_task_operation(&self.session.id, id))
                    .transpose()?
                    .flatten();
                let target = self
                    .task_controller
                    .current()?
                    .and_then(|task| match task.workflow {
                        crate::task::TaskWorkflow::Plan { plan } => plan
                            .artifact
                            .acceptance
                            .map(|acceptance| (plan.artifact.id, acceptance.review_digest)),
                        _ => None,
                    });
                crate::task::command::SessionCommand::question_continue(
                    &candidate,
                    prior.as_ref(),
                    target,
                )
            } else {
                crate::task::command::SessionCommand::parse_request(&candidate)
            }
        })();
        let mut outcome = match command {
            Ok(Some(command)) => self.commit_session_command(command),
            Err(error) => Err(error),
            Ok(None) => {
                match crate::attachment::resolve_input(&self.data_root, &request.params).await {
                    Ok(attachments) => {
                        self.submitted_attachments = attachments;
                        let outcome = self.dispatch_inner(method, request.params).await;
                        self.submitted_attachments.clear();
                        outcome
                    }
                    Err(error) => Err(error),
                }
            }
        };
        if let Some(stage) = mutation_stage {
            stage.complete(json!({"status":if outcome.is_ok() { "completed" } else { "failed" }}));
        }
        if let Err(error) = self.refresh_input_schedule() {
            outcome = Err(error.context("publish input deadline"));
        }
        match outcome {
            Ok((value, mut event)) => {
                if method.settles_task() {
                    self.active_wait_projection = None;
                }
                let reconcile_stage = edit_trace
                    .as_ref()
                    .map(|trace| trace.stage("broker.timeline_reconcile"));
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
                if let Some(stage) = reconcile_stage {
                    stage.complete(json!({}));
                }
                if !timeline_patch.is_empty() {
                    match self.event("document_changed", timeline_patch.notification()) {
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
                let response_stage = edit_trace
                    .as_ref()
                    .map(|trace| trace.stage("broker.response_and_trace"));
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
                        if let Some(stage) = response_stage {
                            stage.complete(json!({"count":event.len()}));
                        }
                        if let Some(stage) = dispatch_stage {
                            stage.complete(json!({"status":"completed"}));
                        }
                        DispatchResult { response, event }
                    }
                    Err(error) => DispatchResult {
                        response: Response::failure(id, "encode_response", error.to_string()),
                        event: Vec::new(),
                    },
                }
            }
            Err(error) => {
                if method.settles_task() {
                    self.active_wait_projection = None;
                }
                let mut event = Vec::new();
                if let Ok(timeline_patch) = self.reconcile_after_dispatch()
                    && !timeline_patch.is_empty()
                    && let Ok(timeline_event) =
                        self.event("document_changed", timeline_patch.notification())
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
                let code = if error.is::<RequestNotAdmitted>() {
                    "not_admitted"
                } else if error.downcast_ref::<TurnCancelled>().is_some() {
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
                    response: Response::failure(id, code, crate::storage::error_message(&error)),
                    event,
                }
            }
        }
    }

    /// Route one client request while forwarding provider updates as they arrive.
    pub async fn dispatch_stream(
        &mut self,
        request: Request,
        event_sink: publication::SessionUpdateSink,
    ) -> DispatchResult {
        self.event_sink = Some(event_sink);
        let result = Box::pin(self.dispatch(request)).await;
        self.event_sink = None;
        result
    }

    // Select the handler before polling so unrelated workflow futures do not share a stack frame.
    fn dispatch_inner(
        &mut self,
        method: RuntimeMethod,
        params: Value,
    ) -> futures_util::future::BoxFuture<'_, Result<(Value, Vec<SessionEvent>)>> {
        match method {
            RuntimeMethod::SessionCheckComplete => Box::pin(async move {
                let result = (serde_json::to_value(self.snapshot()?)?, Vec::new());
                self.settle_automatic_checks(result, PromptMode::Chat).await
            }),
            RuntimeMethod::SessionCheckList
            | RuntimeMethod::SessionCheckOverride
            | RuntimeMethod::SessionCheckRun
            | RuntimeMethod::SessionCheckStatus
            | RuntimeMethod::SessionCheckCancel => Box::pin(self.project_checks(method, params)),
            RuntimeMethod::SessionCommandApply => self.transition_task(params),
            RuntimeMethod::SessionProviderModelList => Box::pin(self.list_backend_model()),
            RuntimeMethod::SessionAgentStart => Box::pin(self.start_agent(params)),
            RuntimeMethod::SessionPromptSubmit => Box::pin(self.submit_prompt(params)),
            RuntimeMethod::SessionPlanAccept => Box::pin(std::future::ready(
                self.dispatch_session_command("session.plan.accept", params),
            )),
            RuntimeMethod::SessionPlanAcceptanceBegin => Box::pin(std::future::ready(
                self.dispatch_session_command("session.plan.acceptance.begin", params),
            )),
            RuntimeMethod::SessionPlanCheckUpdate => Box::pin(self.update_plan_checks(params)),
            RuntimeMethod::SessionPlanChangesRequest => Box::pin(std::future::ready(
                self.dispatch_session_command("session.plan.changes.request", params),
            )),
            RuntimeMethod::SessionPlanQuestionAnswer => Box::pin(async {
                anyhow::bail!("plan review questions must route through the session coordinator")
            }),
            RuntimeMethod::SessionQuestionAsk => Box::pin(self.ask_question(params)),
            RuntimeMethod::SessionQuestionContinue => Box::pin(async move {
                if let Some(target) = params.get("target") {
                    let active = self
                        .snapshot()?
                        .active_elicitation
                        .context("question set is no longer active")?;
                    let owner_id = active.exchange_id.as_ref().or(active.plan_id.as_ref());
                    anyhow::ensure!(
                        serde_json::to_value(active.owner)? == target["owner"]
                            && owner_id.map(String::as_str) == target["id"].as_str()
                            && Some(active.elicitation.question_set.id.as_str())
                                == target["set_id"].as_str()
                            && Some(u64::from(active.elicitation.revision))
                                == target["revision"].as_u64(),
                        "question target changed before continuation"
                    );
                    anyhow::ensure!(
                        active
                            .elicitation
                            .question_set
                            .questions
                            .iter()
                            .all(|question| active
                                .elicitation
                                .answer
                                .iter()
                                .any(|answer| answer.question_id == question.id)),
                        "question answers have not all committed"
                    );
                }
                self.continue_question().await
            }),
            RuntimeMethod::SessionApprovalContinue => {
                Box::pin(self.continue_after_approval_timeout())
            }
            RuntimeMethod::SessionGoalSet => Box::pin(std::future::ready(
                self.dispatch_session_command("session.goal.set", params),
            )),
            RuntimeMethod::SessionGoalPause => Box::pin(std::future::ready(
                self.dispatch_session_command("session.goal.pause", json!({})),
            )),
            RuntimeMethod::SessionGoalResume => Box::pin(std::future::ready(
                self.dispatch_session_command("session.goal.resume", json!({})),
            )),
            RuntimeMethod::SessionGoalClear => Box::pin(std::future::ready(
                self.dispatch_session_command("session.goal.clear", json!({})),
            )),
            RuntimeMethod::SessionExchangeRollbackPrepare => {
                Box::pin(self.prepare_rollback(params))
            }
            RuntimeMethod::SessionExchangeRollbackApply => Box::pin(self.apply_rollback(params)),
            RuntimeMethod::SessionExchangeRecovery => Box::pin(self.restore_recovery(params)),
            RuntimeMethod::SessionResume => Box::pin(self.resume_session(params)),
            RuntimeMethod::SessionConfigure => Box::pin(self.configure_session(params)),
            RuntimeMethod::SessionCompact => Box::pin(self.compact_session(true)),
            RuntimeMethod::SessionTurnCancel => Box::pin(self.cancel_settled_execution()),
            _ => Box::pin(self.dispatch_local(method, params)),
        }
    }

    async fn dispatch_local(
        &mut self,
        method: RuntimeMethod,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        if !matches!(
            method,
            RuntimeMethod::SessionStateGet
                | RuntimeMethod::SessionList
                | RuntimeMethod::SessionPreview
        ) {
            self.save_session()?;
        }
        match method {
            RuntimeMethod::SessionCheckEditingSet => Ok((
                self.observer.set_check_editing(
                    params["enabled"]
                        .as_bool()
                        .context("check editing requires enabled")?,
                )?,
                Vec::new(),
            )),
            RuntimeMethod::SessionTaskGet => Ok((
                serde_json::to_value(self.task_controller.current()?)?,
                Vec::new(),
            )),
            RuntimeMethod::SessionLspAuthorize
            | RuntimeMethod::SessionAttachmentImport
            | RuntimeMethod::SessionLspRespond
            | RuntimeMethod::SessionLspAnalyze
            | RuntimeMethod::SessionCommandOperationGet
            | RuntimeMethod::SessionHealthGet => {
                anyhow::bail!("control requires the service coordinator")
            }
            RuntimeMethod::SessionStateGet => {
                Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()))
            }
            RuntimeMethod::HostTraceStatus => {
                Ok((serde_json::to_value(self.trace.status())?, Vec::new()))
            }
            RuntimeMethod::HostTraceConfigure => self.configure_trace(params),
            RuntimeMethod::SessionTraceClear => Ok((
                serde_json::to_value(self.trace.clear_session(&self.session.id)?)?,
                Vec::new(),
            )),
            RuntimeMethod::ProviderModelPinList | RuntimeMethod::ProviderModelPinSet => {
                anyhow::bail!("model pins require the service coordinator")
            }
            RuntimeMethod::SessionAgentMessageSend
            | RuntimeMethod::SessionAgentFollowup
            | RuntimeMethod::SessionAgentInterrupt
            | RuntimeMethod::SessionAgentInterruptAll => {
                let target = || required_text(&params, "target");
                let operation = match method {
                    RuntimeMethod::SessionAgentMessageSend => {
                        crate::agent::AgentOperation::Message {
                            target: target()?,
                            message: required_text(&params, "message")?,
                        }
                    }
                    RuntimeMethod::SessionAgentFollowup => crate::agent::AgentOperation::Followup {
                        target: target()?,
                        message: required_text(&params, "message")?,
                    },
                    RuntimeMethod::SessionAgentInterrupt => {
                        crate::agent::AgentOperation::Interrupt { target: target()? }
                    }
                    _ => crate::agent::AgentOperation::InterruptAll,
                };
                let mut event = Vec::new();
                let result = self
                    .handle_agent_request(
                        crate::agent::AgentRequest {
                            operation_id: Uuid::new_v4().to_string(),
                            source: crate::agent::AgentSource::User,
                            operation,
                        },
                        None,
                        &mut event,
                    )
                    .await?;
                Ok((result, event))
            }
            RuntimeMethod::SessionAgentModelList => {
                let provider =
                    crate::backend::BackendKind::parse(&required_text(&params, "provider")?)?;
                let model = self
                    .agent_backend_factory
                    .catalog(
                        provider,
                        self.backend_request(
                            BackendInput::from_text(String::new()),
                            PromptMode::Chat,
                        ),
                    )
                    .await?;
                Ok((serde_json::to_value(model)?, Vec::new()))
            }
            RuntimeMethod::AgentProfileGet => Ok((
                serde_json::to_value(self.agent_backend_factory.profiles.snapshot()?)?,
                Vec::new(),
            )),
            RuntimeMethod::AgentProfileSave => {
                let revision = required_text(&params, "expected_revision")?;
                let document = serde_json::from_value(params["document"].clone())?;
                Ok((
                    serde_json::to_value(
                        self.agent_backend_factory
                            .profiles
                            .save(&revision, document)?,
                    )?,
                    Vec::new(),
                ))
            }
            RuntimeMethod::PermissionsDocumentOpen => self.observer.open_permission_document(),
            RuntimeMethod::PermissionsDocumentSave => {
                self.observer.save_permission_document(params)
            }
            RuntimeMethod::SessionPermissionSet => {
                let mut action = params;
                action["action"] = json!("permission");
                self.transition_task(action).await
            }
            RuntimeMethod::SessionPromptHistoryRecord => self
                .observer
                .record_prompt_history(params, self.clock.now_ms()),
            RuntimeMethod::SessionPlanAcceptanceCancel => {
                self.dispatch_session_command("session.plan.acceptance.cancel", json!({}))
            }
            RuntimeMethod::SessionPlanEntityRename => self.rename_plan_entity(params),
            RuntimeMethod::SessionPlanTestDelete => self.delete_plan_tests(params),
            RuntimeMethod::SessionPlanCancel => {
                self.dispatch_session_command("session.plan.cancel", params)
            }
            RuntimeMethod::SessionPlanGet => Ok((self.observer.plan(params)?, Vec::new())),
            RuntimeMethod::SessionPlanDeclarationGet => self.observer.read_plan_declaration(params),
            RuntimeMethod::SessionPlanList => self.observer.list_replanning_choices(),
            RuntimeMethod::SessionQuestionAnswer => self.answer_question(params),
            RuntimeMethod::SessionQuestionSkip => self.skip_question(params),
            RuntimeMethod::SessionInputTimeoutConfigure => {
                anyhow::bail!("input timeout settings require the service coordinator")
            }
            RuntimeMethod::SessionExchangeList => Ok((
                serde_json::to_value(self.observer.list_exchange_review()?)?,
                Vec::new(),
            )),
            RuntimeMethod::SessionExchangeCommentSave => {
                self.observer.save_exchange_comment(params)
            }
            RuntimeMethod::SessionExchangeRollbackPreview => Ok((
                self.observer
                    .query(RuntimeMethod::SessionExchangeRollbackPreview, params)?
                    .context("rollback preview observation is unavailable")?,
                Vec::new(),
            )),
            RuntimeMethod::SessionNew => self.new_session(params).await,
            RuntimeMethod::SessionList => self.observer.list_session(params),
            RuntimeMethod::SessionPreview => self.observer.preview_session(params),
            RuntimeMethod::SessionRename => {
                let renamed = self.observer.rename_session(params)?;
                self.session.name = required_text(&renamed.0, "name")?;
                Ok(renamed)
            }
            RuntimeMethod::SessionDelete => self.observer.delete_session(params),
            RuntimeMethod::SessionFork => anyhow::bail!(
                "session.fork must route through the process-wide session coordinator"
            ),
            RuntimeMethod::HostShutdown => {
                self.release_lease()?;
                Ok((json!({ "host.shutdown": true }), Vec::new()))
            }
            RuntimeMethod::SessionTurnRestart
            | RuntimeMethod::SessionInputApply
            | RuntimeMethod::SessionInputGet
            | RuntimeMethod::SessionInputOperationGet
            | RuntimeMethod::SessionTurnSteer
            | RuntimeMethod::SessionApprovalResolve
            | RuntimeMethod::SessionProviderSkillList
            | RuntimeMethod::SettingsSkillSetEnabled
            | RuntimeMethod::SessionProviderMcpList
            | RuntimeMethod::SessionProviderCapabilitiesGet
            | RuntimeMethod::SessionProviderAuthenticate
            | RuntimeMethod::SessionProviderSetup
            | RuntimeMethod::SettingsGet
            | RuntimeMethod::SettingsUpdate
            | RuntimeMethod::SettingsImportPreview
            | RuntimeMethod::SettingsImportApply
            | RuntimeMethod::SettingsMcpSetEnabled => {
                anyhow::bail!("Harness control method requires the process coordinator")
            }
            _ => anyhow::bail!("method requires asynchronous handler routing"),
        }
    }

    fn current_plan_id(&self) -> Result<Option<String>> {
        Ok(self
            .task_controller
            .current()?
            .and_then(|task| match task.workflow {
                crate::task::TaskWorkflow::Plan { plan } => Some(plan.artifact.id),
                crate::task::TaskWorkflow::Execute { execution } => Some(execution.record.plan_id),
                crate::task::TaskWorkflow::Goal { .. } => None,
            }))
    }

    fn publish_plan(&mut self, plan: &PlanRecord) -> Result<()> {
        let current = self.task_controller.current()?;
        if let Some(task) = current.filter(|task| {
            matches!(&task.workflow,
            crate::task::TaskWorkflow::Plan { plan: active } if active.artifact.id == plan.id)
        }) {
            self.task_controller.update(
                &mut self.store,
                &task.permit(),
                self.clock.now_ms(),
                |task| {
                    let crate::task::TaskWorkflow::Plan { plan: active } = &mut task.workflow
                    else {
                        return Err(crate::task::StaleTask.into());
                    };
                    active.artifact = plan.clone();
                    Ok(())
                },
            )
        } else {
            self.store.save_plan(plan)
        }
    }

    fn task_prompt_mode(&self) -> Result<PromptMode> {
        let Some(task) = self.task_controller.current()? else {
            return Ok(PromptMode::Chat);
        };
        if task.status() == crate::task::TaskStatus::Paused || !task.entry_accepted() {
            return Ok(PromptMode::Chat);
        }
        Ok(match task.workflow {
            crate::task::TaskWorkflow::Goal { .. } => PromptMode::GoalContinuation,
            crate::task::TaskWorkflow::Execute { .. } => PromptMode::ExecutePlan,
            crate::task::TaskWorkflow::Plan { plan } => {
                if plan.artifact.state == PlanState::AwaitingReview {
                    PromptMode::PlanDiscussion
                } else {
                    PromptMode::Plan
                }
            }
        })
    }

    fn current_execution(&self) -> Result<Option<PlanExecutionRecord>> {
        Ok(self.capture_execution()?.map(|capture| capture.record))
    }

    fn reserve_execution_continuation(&mut self, completed: crate::task::TaskTurnId) -> Result<()> {
        let Some(task) = self.task_controller.current()? else {
            return Ok(());
        };
        let crate::task::TaskWorkflow::Execute { execution } = &task.workflow else {
            return Ok(());
        };
        if execution.record.state != PlanExecutionState::Active {
            return Ok(());
        }
        let now_ms = self.clock.now_ms();
        if execution.record.phase == crate::plan::PlanPhase::Checking {
            self.task_controller.reserve_checks(
                &mut self.store,
                &task.permit(),
                completed,
                now_ms,
            )?;
        } else {
            let goal = self.active_goal()?;
            let prompt = self
                .plan_goal_prompt(&goal, PlanExecutionPromptKind::Continue)?
                .context("execution continuation has no accepted plan")?;
            self.task_controller.reserve_continuation(
                &mut self.store,
                &task.permit(),
                completed,
                prompt,
                "The accepted plan requires another execution phase".into(),
                now_ms,
            )?;
        }
        Ok(())
    }

    pub(crate) async fn run_task_checks(&mut self) -> Result<Vec<SessionEvent>> {
        let task = self
            .task_controller
            .current()?
            .context("check task was cleared")?;
        let turn = task
            .turn
            .as_ref()
            .context("check work has no source turn")?;
        let work = turn
            .continuation
            .as_ref()
            .context("check work was removed")?;
        anyhow::ensure!(
            work.work == crate::task::TaskContinuationWork::Checks,
            "pending work is not checks"
        );
        let source = turn.id.clone();
        let id = work.id.clone();
        let events = self.run_checks().await?;
        let Some(current) = self.task_controller.current()? else {
            return Ok(events);
        };
        if current.entry.as_ref().map(|entry| &entry.id)
            != task.entry.as_ref().map(|entry| &entry.id)
            || !matches!((&task.workflow, &current.workflow),
                (crate::task::TaskWorkflow::Execute { execution: original }, crate::task::TaskWorkflow::Execute { execution: current }) if original.record.id == current.record.id)
        {
            return Ok(events);
        }
        self.task_controller.acknowledge_continuation(
            &mut self.store,
            &current.revision,
            &id,
            self.clock.now_ms(),
        )?;
        self.reserve_execution_continuation(source)?;
        Ok(events)
    }

    fn current_task_is_plan(&self) -> Result<bool> {
        Ok(matches!(
            self.task_prompt_mode()?,
            PromptMode::Plan | PromptMode::PlanDiscussion
        ))
    }

    async fn pause_task(&mut self, clear: bool) -> Result<(Value, Vec<SessionEvent>)> {
        self.retry_finalization().await?;
        let current = self.task_controller.current()?;
        if let Some(task) = current {
            self.task_controller.change(
                &mut self.store,
                Some(&task.revision),
                if clear {
                    crate::task::TaskChange::Clear
                } else {
                    crate::task::TaskChange::Pause
                },
                self.clock.now_ms(),
            )?;
        }
        Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()))
    }

    #[cfg(test)]
    fn create_task(
        &mut self,
        kind: crate::task::TaskKind,
        title: String,
        plan_id: Option<String>,
        _goal_id: Option<String>,
    ) -> Result<()> {
        let now_ms = self.clock.now_ms();
        let task = if kind == crate::task::TaskKind::Plan {
            crate::task::ActiveTask::plan(
                self.store
                    .load_plan(&plan_id.context("plan is required")?)?
                    .context("plan is missing")?,
                now_ms,
            )
        } else {
            crate::task::ActiveTask::goal(title, Default::default(), false, now_ms)
        };
        let current = self.task_controller.current()?;
        self.task_controller.change(
            &mut self.store,
            current.as_ref().map(|task| &task.revision),
            crate::task::TaskChange::Install(Box::new(task)),
            now_ms,
        )?;
        Ok(())
    }

    /// Applies a transition only after the service has collected the previous owner.
    /// Apply one task transition without retaining unrelated operation futures while it runs.
    pub(crate) fn transition_task(
        &mut self,
        action: Value,
    ) -> futures_util::future::BoxFuture<'_, Result<(Value, Vec<SessionEvent>)>> {
        let command = match required_text(&action, "action") {
            Ok(command) => command,
            Err(error) => return Box::pin(std::future::ready(Err(error))),
        };
        match command.as_str() {
            "undo" => Box::pin(self.transition_undo(action)),
            "pause" | "clear" => Box::pin(self.transition_pause(command == "clear")),
            "permission" => Box::pin(
                self.apply_configuration(action, configuration::ConfigurationCommand::Permission),
            ),
            "configure" => Box::pin(
                self.apply_configuration(action, configuration::ConfigurationCommand::Configure),
            ),
            "compact" => Box::pin(
                self.apply_configuration(action, configuration::ConfigurationCommand::Compact),
            ),
            "resume" | "plan" | "goal" | "execute" | "attach_plan" | "fork" | "replan"
            | "continue" => Box::pin(std::future::ready(
                self.dispatch_session_command("session.command.apply", action),
            )),
            _ => Box::pin(std::future::ready(Err(anyhow::anyhow!(
                "unknown task action"
            )))),
        }
    }

    async fn transition_undo(&mut self, action: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let rollback = action
            .get("rollback")
            .cloned()
            .context("undo requires rollback selection")?;
        anyhow::ensure!(
            rollback["scope"] == "conversation",
            "undo requires conversation rollback scope"
        );
        self.pause_task(false).await?;
        self.cancel_settled_execution().await?;
        self.rollback_conversation(rollback).await
    }

    async fn transition_pause(&mut self, clear: bool) -> Result<(Value, Vec<SessionEvent>)> {
        self.dispatch_session_command(
            "session.command.apply",
            json!({"action":if clear { "clear" } else { "pause" }}),
        )
    }

    async fn resume_exchange(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let text = required_text(&params, "text")?;
        let interaction = self
            .store
            .latest_exchange(&self.session.id)?
            .context("no Harness interaction is available to resume")?;
        anyhow::ensure!(
            interaction.state() == ExchangeState::Running
                && interaction.execution_started_at_ms().is_none()
                && interaction
                    .turn
                    .iter()
                    .all(|turn| turn.state() != crate::turn::TurnState::Running),
            "the latest Harness exchange is not paused for resumption"
        );
        let pending_plan = if self.active_plan_awaits_input()? {
            Some(self.active_elicitation_plan()?)
        } else {
            None
        };
        let elicitation = pending_plan
            .as_ref()
            .and_then(|plan| plan.elicitation.as_ref())
            .or(interaction.elicitation());
        if let Some(elicitation) = elicitation {
            if !elicitation.clarification_active {
                return Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()));
            }
            let clarification = interaction
                .node_list
                .iter()
                .rev()
                .find_map(|node| match node {
                    ExchangeNode::ExchangeInput { prompt }
                        if prompt.intent == crate::exchange::InputIntent::Clarification
                            && prompt.question.as_ref().is_some_and(|target| {
                                target.set_id == elicitation.question_set.id
                            }) =>
                    {
                        Some(prompt)
                    }
                    _ => None,
                });
            let clarification = clarification
                .context("interrupted clarification has no recorded question input")?;
            let follow_up = format!("{}\n\n{text}", clarification.text);
            let elicitation_json = serde_json::to_string_pretty(elicitation)?;
            let prompt = if let Some(plan) = &pending_plan {
                PlanPrompt::clarification(&plan.request, &elicitation_json, &follow_up)
            } else {
                PlanPrompt::question_follow_up(&elicitation_json, &follow_up)
            };
            let mode = self.task_prompt_mode()?;
            let (value, event) = self.run_interaction(prompt, mode, None).await?;
            return if let Some(plan) = pending_plan {
                self.finish_plan_clarification(&plan.id, value, event).await
            } else {
                self.finish_question_clarification(value, event).await
            };
        }
        anyhow::ensure!(
            !interaction.awaiting_input(),
            "the latest Harness exchange is awaiting a user decision"
        );
        let mode = self.task_prompt_mode()?;
        if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion) {
            self.run_planning_interaction(text, None).await
        } else {
            self.run_interaction(text, mode, None).await
        }
    }

    async fn start_agent(&mut self, mut params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let operation_id = params
            .as_object_mut()
            .and_then(|params| params.remove("operation_id"))
            .and_then(|value| value.as_str().map(str::to_owned))
            .or_else(|| self.active_operation_id.clone())
            .unwrap_or_else(|| Uuid::new_v4().to_string());
        let spawn: crate::agent::SpawnAgentRequest = serde_json::from_value(params)?;
        let request = crate::agent::AgentRequest {
            operation_id,
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::Spawn(spawn),
        };
        let mut event = Vec::new();
        let result = self.handle_agent_request(request, None, &mut event).await?;
        Ok((result, event))
    }

    async fn handle_agent_request(
        &mut self,
        request: crate::agent::AgentRequest,
        parent: Option<&mut Exchange>,
        event: &mut Vec<SessionEvent>,
    ) -> Result<Value> {
        use crate::agent::{AgentOperation, AgentSource};
        anyhow::ensure!(
            request.source != AgentSource::Model || self.agent_coordinator.enabled(),
            "model-facing subagent tools are disabled"
        );
        if let Some(task) = self
            .store
            .list_agent_task(&self.session.id)?
            .into_iter()
            .find(|task| task.operation_id == request.operation_id)
        {
            let agent = self.agent_coordinator.resolve(&task.agent_id)?;
            let same_work = match &request.operation {
                AgentOperation::Spawn(spawn) => {
                    task.ordinal == 1
                        && task.message == spawn.text
                        && spawn
                            .profile
                            .as_deref()
                            .is_none_or(|profile| profile == agent.definition)
                }
                AgentOperation::Followup { target, message } => {
                    task.ordinal > 1
                        && task.message == *message
                        && self.agent_coordinator.resolve(target)?.id == agent.id
                }
                _ => false,
            };
            anyhow::ensure!(
                same_work
                    && task.source == request.source
                    && parent
                        .as_deref()
                        .is_none_or(|parent| task.parent_exchange_id == parent.id),
                "operation identity was reused for different child work"
            );
            return Ok(json!({"agent":agent,"task":task,"status":"admitted","replayed":true}));
        }
        enum ChildWork<'request> {
            Spawn(&'request crate::agent::SpawnAgentRequest),
            Followup {
                target: &'request str,
                message: &'request str,
            },
        }
        let work = match &request.operation {
            AgentOperation::Activity { target, limit } => {
                let limit = limit.unwrap_or(5);
                let (agent, task) = self.agent_coordinator.inspection(target)?;
                let saved;
                let exchange = if let Some(runtime) = self
                    .child_exchange_runtime_by_agent
                    .get(&agent.id)
                    .filter(|runtime| runtime.exchange.id == task.id)
                {
                    &runtime.exchange
                } else {
                    saved = self
                        .store
                        .load_exchange_metadata(&task.id)?
                        .context("child activity exchange missing")?;
                    &saved
                };
                let awaiting_approval =
                    self.permission_coordinator
                        .pending_list()?
                        .iter()
                        .any(|approval| {
                            approval.execution_owner.as_ref().is_some_and(|owner| {
                                owner.agent_id == agent.id && owner.session_id == self.session.id
                            })
                        });
                let result = serde_json::to_value(crate::agent::AgentActivity::capture(
                    &agent,
                    &task,
                    exchange,
                    limit,
                    self.clock.now_ms(),
                    awaiting_approval,
                )?)?;
                return Ok(result);
            }
            AgentOperation::ToolOutput {
                call_ref,
                offset,
                limit,
            } => {
                let reference = crate::agent::AgentToolReference::decode(call_ref)?;
                anyhow::ensure!(
                    reference.session_id == self.session.id,
                    "tool reference belongs to another root session"
                );
                let agent = self.agent_coordinator.resolve(&reference.agent_id)?;
                let task = self
                    .agent_coordinator
                    .task(&reference.exchange_id)
                    .context("tool reference has no admitted child run")?;
                anyhow::ensure!(
                    task.agent_id == agent.id && task.session_id == self.session.id,
                    "tool reference crosses child ownership"
                );
                let exchange = self
                    .store
                    .load_exchange_metadata(&reference.exchange_id)?
                    .context("tool reference exchange missing")?;
                anyhow::ensure!(
                    exchange.agent_id == agent.id && exchange.session_id == self.session.id,
                    "tool reference crosses exchange ownership"
                );
                let tool = exchange
                    .turn
                    .iter()
                    .find(|turn| turn.id() == reference.turn_id)
                    .and_then(|turn| {
                        exchange
                            .tools_for_turn(turn.id())
                            .find(|tool| tool.id == reference.tool_id)
                    })
                    .context("tool reference does not identify a recorded call")?;
                let page = self.store.tool_output_page(
                    &exchange.id,
                    &reference.turn_id,
                    &reference.tool_id,
                    *offset,
                    *limit,
                )?;
                return Ok(
                    json!({"call_ref":call_ref,"status":crate::agent::tool_activity_status(tool),
                    "failed":tool.failed,"output":page.output,"offset":offset,
                    "next_offset":page.next_offset,"total_chars":page.total_chars}),
                );
            }
            AgentOperation::List { prefix } => {
                let mut entry = Vec::new();
                if prefix
                    .as_deref()
                    .is_none_or(|prefix| "/root".starts_with(prefix))
                {
                    entry.push(json!({"agent":Agent::primary(&self.session.id, self.session.created_at_ms),"latest_task":null}));
                }
                entry.extend(
                    self.agent_coordinator
                        .snapshot(prefix.as_deref())
                        .into_iter()
                        .map(|(agent, task)| json!({"agent":agent,"latest_task":task})),
                );
                return Ok(json!({"agents":entry}));
            }
            AgentOperation::Message { target, message } => {
                anyhow::ensure!(
                    !message.trim().is_empty(),
                    "informational message must contain nonempty text"
                );
                let mut agent = self.agent_coordinator.resolve(target)?;
                let id = request.operation_id.clone();
                if let Some(existing) = agent.inbox.iter().find(|message| message.id == id) {
                    anyhow::ensure!(
                        existing.message == *message,
                        "operation identity was reused for a different informational message"
                    );
                    return Ok(serde_json::to_value(existing)?);
                }
                let mut delivery = crate::agent::MessageDelivery::Queued;
                if let Some(backend) = self.child_backend_by_agent.get(&agent.id)
                    && backend.descriptor().capability.native_steer
                    && backend
                        .has_active_turn(
                            &crate::backend::ExecutionOwner {
                                session_id: (self.session.id.clone()).into(),
                                agent_id: (agent.id.clone()).into(),
                            }
                            .runtime_key(),
                        )
                        .await
                {
                    match backend.steer_session(&crate::backend::ExecutionOwner {session_id: (self.session.id.clone()).into(),agent_id: (agent.id.clone()).into()}.runtime_key(), message.clone().into()).await {
                        Ok(()) => delivery = crate::agent::MessageDelivery::Steered,
                        Err(error) => self.trace.record(&self.session.id,"agent.message_queued",json!({
                            "agent_id":agent.id,"reason":"steering_not_accepted","error":format!("{error:#}")})),
                    }
                }
                let message = crate::agent::AgentMessage {
                    id,
                    ordinal: agent.inbox.last().map_or(1, |message| message.ordinal + 1),
                    message: message.clone(),
                    delivery,
                    task_id: None,
                    created_at_ms: self.clock.now_ms(),
                };
                agent.inbox.push(message.clone());
                self.store.save_agent_run(&agent)?;
                if let Some(task) = self
                    .agent_coordinator
                    .snapshot(None)
                    .into_iter()
                    .find(|(candidate, _)| candidate.id == agent.id)
                    .and_then(|(_, task)| task)
                {
                    self.agent_coordinator.retain(agent.clone(), task)?;
                }
                self.agent_registry.insert(agent);
                return Ok(serde_json::to_value(message)?);
            }
            AgentOperation::Interrupt { target } => {
                self.interrupt_managed_agent(target, event).await?;
                return Ok(json!({"status":"interrupting","target":target}));
            }
            AgentOperation::InterruptAll => {
                for (agent, _) in self.agent_coordinator.snapshot(None) {
                    self.interrupt_managed_agent(&agent.id, event).await?;
                }
                return Ok(json!({"status":"interrupting"}));
            }
            AgentOperation::Spawn(spawn) => ChildWork::Spawn(spawn),
            AgentOperation::Followup { target, message } => ChildWork::Followup { target, message },
        };
        let mut admitted_parent = if let Some(parent) = parent.as_deref() {
            parent.clone()
        } else {
            Exchange::agent_admission(
                &self.session.id,
                self.store.next_exchange_ordinal(&self.session.id)?,
                "Child task admission".into(),
                self.clock.now_ms(),
            )
        };
        let admission = match work {
            ChildWork::Spawn(spawn) => {
                let mut root =
                    self.backend_request(BackendInput::from_text(String::new()), PromptMode::Chat);
                if root.model == "default"
                    && let Some(model) = &self.session.resolved_model
                {
                    root.model = model.clone();
                }
                let selection = self
                    .agent_backend_factory
                    .resolve(
                        &root,
                        crate::backend::BackendKind::parse(&self.session.backend)?,
                        spawn,
                    )
                    .await?;
                anyhow::ensure!(
                    request.source != AgentSource::Model || self.agent_coordinator.enabled(),
                    "model-facing subagent tools are disabled"
                );
                self.agent_backend_factory
                    .profiles
                    .resolve(Some(&selection.profile))?;
                self.agent_coordinator.reserve_spawn(
                    &request,
                    spawn,
                    selection,
                    &admitted_parent.id,
                    self.clock.now_ms(),
                )?
            }
            ChildWork::Followup { target, message } => {
                let agent = self.agent_coordinator.resolve(target)?;
                self.agent_backend_factory
                    .profiles
                    .resolve(Some(&agent.definition))?;
                self.agent_coordinator.reserve_followup(
                    &request,
                    target,
                    message,
                    &admitted_parent.id,
                    self.clock.now_ms(),
                )?
            }
        };
        let persisted = (|| -> Result<()> {
            admitted_parent.append_agent_task(&admission.task)?;
            self.store
                .admit_agent_task(&admission.agent, &admission.task, &admitted_parent)
        })();
        if let Err(error) = persisted {
            return Err(match admission.cancel() {
                Ok(()) => error,
                Err(cancellation) => {
                    error.context(format!("release child reservation: {cancellation:#}"))
                }
            });
        }
        let (admission, launch) = match admission.commit()? {
            crate::agent::AgentCommit::Admitted(work) => (work, true),
            crate::agent::AgentCommit::Existing(work) => (work, false),
        };
        self.agent_registry.insert(admission.agent.clone());
        if let Some(parent) = parent {
            *parent = admitted_parent;
        } else {
            admitted_parent.finish(
                crate::exchange::ExchangeOutcome::Complete,
                self.clock.now_ms(),
            )?;
            self.store.save_exchange(&admitted_parent)?;
        }
        self.trace.record(&self.session.id,"agent.admitted",json!({"agent_id":admission.agent.id,"exchange_id":admission.task.id,"parent_exchange_id":admission.task.parent_exchange_id,"source":admission.task.source}));
        let warning = if matches!(request.operation, AgentOperation::Spawn(_)) {
            self.agent_backend_factory.profiles.snapshot()?.error
        } else {
            None
        };
        let result = json!({"agent":admission.agent,"task":admission.task,"status":"admitted","warning":warning});
        if launch && admission.launch {
            self.launch_managed_agent(admission, event).await?;
        }
        self.publish_agent_state(event).await?;
        Ok(result)
    }

    async fn publish_agent_state(&mut self, event: &mut Vec<SessionEvent>) -> Result<()> {
        let snapshot = self.snapshot()?;
        self.emit_session_update(
            SessionUpdate::Agent {
                snapshot: snapshot.agent,
            },
            event,
        )
        .await?;
        let patch = self
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .reconcile_with_status_thoughts(
                snapshot.timeline,
                self.observer.status_thoughts.load(Ordering::Relaxed),
            )?;
        self.emit_timeline_patch(patch, event).await
    }

    async fn cancel_agent_approvals(
        &mut self,
        owner: &crate::backend::ExecutionOwner,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let pending = self
            .permission_coordinator
            .pending_list()?
            .into_iter()
            .filter(|approval| approval.execution_owner.as_ref() == Some(owner))
            .collect::<Vec<_>>();
        self.permission_coordinator
            .cancel_owner(owner, None)
            .await?;
        for approval in pending {
            if let Some(runtime) = self
                .child_exchange_runtime_by_agent
                .get_mut(owner.agent_id.as_str())
            {
                runtime.exchange.observe_blocker(
                    format!("approval:{}", approval.id),
                    false,
                    self.clock.now_ms(),
                );
                self.store.save_exchange(&runtime.exchange)?;
            }
            self.emit_session_update(
                SessionUpdate::Approval {
                    update: Box::new(crate::backend::ApprovalUpdate::Cancelled(approval.clone())),
                },
                event,
            )
            .await?;
        }
        Ok(())
    }

    async fn interrupt_managed_agent(
        &mut self,
        target: &str,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let agent = self.agent_coordinator.resolve(target)?;
        let owner = crate::backend::ExecutionOwner {
            session_id: (self.session.id.clone()).into(),
            agent_id: (agent.id.clone()).into(),
        };
        let cancelled = self.agent_coordinator.interrupt(target)?;
        self.cancel_agent_approvals(&owner, event).await?;
        for mut task in cancelled {
            task.state = ExchangeState::Cancelled;
            task.completed_at_ms = Some(self.clock.now_ms());
            if let Some(mut exchange) = self.store.load_exchange(&task.id)? {
                exchange.finish(
                    crate::exchange::ExchangeOutcome::Cancelled,
                    self.clock.now_ms(),
                )?;
                self.store.complete_agent_task(&agent, &task, &exchange)?;
            }
            self.agent_coordinator.retain(agent.clone(), task)?;
        }
        if let Some(mut task) = self.agent_coordinator.cleanup_task(&agent.id) {
            let backend = self
                .child_backend_by_agent
                .get(&agent.id)
                .cloned()
                .context("child cleanup obligation has no retained provider adapter")?;
            tokio::time::timeout(std::time::Duration::from_secs(30), async {
                backend.cancel_session(&owner.runtime_key()).await?;
                backend.cleanup_execution(&owner.runtime_key()).await?;
                backend.shutdown().await
            })
            .await
            .context("child cleanup retry exceeded 30 seconds")??;
            let mut agent = agent.clone();
            agent.state = AgentState::Ready;
            agent.updated_at_ms = self.clock.now_ms();
            task.error = None;
            let mut exchange = self
                .store
                .load_exchange(&task.id)?
                .context("child cleanup exchange missing")?;
            exchange.finalization_error = None;
            self.store.complete_agent_task(&agent, &task, &exchange)?;
            self.agent_registry.insert(agent.clone());
            self.agent_coordinator.retain(agent, task.clone())?;
            self.agent_coordinator
                .settled(&owner.agent_id, &task.id, false)?;
            self.child_backend_by_agent.remove(owner.agent_id.as_str());
        }
        self.publish_agent_state(event).await?;
        Ok(())
    }

    async fn launch_managed_agent(
        &mut self,
        mut admission: crate::agent::AgentWork,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let owner = admission.owner().clone();
        anyhow::ensure!(
            owner.session_id == self.session.id,
            "child execution belongs to another session"
        );
        let prepared = (|| -> Result<_> {
            let mut message = admission.task.message.clone();
            for incoming in admission
                .agent
                .inbox
                .iter_mut()
                .filter(|message| message.delivery == crate::agent::MessageDelivery::Queued)
            {
                message.push_str(&format!(
                    "\n\nAdditional information:\n{}",
                    incoming.message
                ));
                incoming.delivery = crate::agent::MessageDelivery::Attached;
                incoming.task_id = Some(admission.task.id.clone());
            }
            let mut request = self.agent_backend_factory.request(
                &self.backend_request(BackendInput::from_text(String::new()), PromptMode::Chat),
                &admission,
                message,
            )?;
            let backend = self.agent_backend_factory.build(
                admission
                    .agent
                    .selection
                    .as_ref()
                    .context("child selection missing")?
                    .provider,
            )?;
            let cancellation = self
                .agent_coordinator
                .cancellation(&admission.agent.id, &admission.task.id)?;
            let lsp_lease = if self.lsp.enabled(&self.session.id) {
                let policy = self
                    .permission_store
                    .read()
                    .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
                    .compiled()
                    .clone();
                let (scope, lease) = self.lsp.scope(
                    self.session.id.clone(),
                    admission.task.id.clone(),
                    PathBuf::from(&self.session.workspace),
                    policy,
                    Arc::new(AtomicBool::new(false)),
                );
                let mut context =
                    crate::control_tools::ControlTurnContext::inactive(PromptMode::Chat);
                context.lsp = Some(scope);
                context.workspace_root = Some(PathBuf::from(&self.session.workspace));
                request.control_context = Some(context);
                Some(lease)
            } else {
                None
            };
            self.store.save_agent_run(&admission.agent)?;
            self.agent_registry.insert(admission.agent.clone());
            self.agent_coordinator
                .retain(admission.agent.clone(), admission.task.clone())?;
            self.child_backend_by_agent
                .insert(admission.agent.id.clone(), backend.clone());
            Ok(crate::agent::AgentRuntime {
                backend,
                trace: self.trace.clone(),
                submission_store: self.submission_store.clone(),
                permission: self.permission_coordinator.clone(),
                request,
                task_id: admission.task_id().to_string(),
                cancellation,
                sender: self.agent_coordinator.sender(),
                lsp_lease,
            })
        })();
        match prepared {
            Ok(runtime) => {
                let sender = self.agent_coordinator.sender();
                let task_id = admission.task.id;
                tokio::spawn(async move {
                    if let Err(error) = runtime.run().await {
                        let _ = sender
                            .send(crate::agent::AgentCommand::Completed {
                                owner,
                                task_id,
                                output: Box::new(Err(error)),
                                cleanup_error: Some(
                                    "child event delivery or cleanup failed".into(),
                                ),
                            })
                            .await;
                    }
                });
            }
            Err(error) => {
                self.complete_managed_agent(owner, admission.task.id, Err(error), None, event)
                    .await?
            }
        }
        Ok(())
    }

    /// Drain owned mutations through the active prompt or idle controller, never both concurrently.
    pub(crate) async fn drain_agent_commands(
        &mut self,
        mut parent: Option<&mut Exchange>,
    ) -> Result<Vec<SessionEvent>> {
        let mut event = Vec::new();
        while let Some(command) = self.agent_coordinator.try_receive() {
            match command {
                crate::agent::AgentCommand::Request {
                    request,
                    authority,
                    response,
                } => {
                    if response.is_closed() {
                        continue;
                    }
                    let authenticated = if request.source == crate::agent::AgentSource::Model {
                        authority.as_ref().is_some_and(|authority| {
                            !*authority.expired.borrow()
                                && authority.owner
                                    == crate::backend::ExecutionOwner::root(&self.session.id)
                                && parent
                                    .as_deref()
                                    .is_some_and(|parent| parent.id == authority.parent_exchange_id)
                        })
                    } else {
                        authority.is_none()
                    };
                    let result = if authenticated {
                        self.handle_agent_request(*request, parent.as_deref_mut(), &mut event)
                            .await
                    } else {
                        Err(anyhow::anyhow!(
                            "agent request has no active authenticated root authority"
                        ))
                    };
                    let _ = response.send(result);
                }
                crate::agent::AgentCommand::Event {
                    owner,
                    task_id,
                    event: backend_event,
                } => {
                    self.observe_managed_agent(owner, &task_id, *backend_event, &mut event)
                        .await?;
                }
                crate::agent::AgentCommand::Completed {
                    owner,
                    task_id,
                    output,
                    cleanup_error,
                } => {
                    Box::pin(self.complete_managed_agent(
                        owner,
                        task_id,
                        *output,
                        cleanup_error,
                        &mut event,
                    ))
                    .await?;
                }
            }
        }
        Ok(event)
    }

    async fn observe_managed_agent(
        &mut self,
        owner: crate::backend::ExecutionOwner,
        task_id: &str,
        mut backend_event: BackendEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        anyhow::ensure!(
            owner.session_id == self.session.id && !owner.is_root(),
            "child event crosses execution ownership"
        );
        let binding = backend_event.scope();
        let scope = binding
            .invocation()
            .context("child event requires invocation ownership")?;
        anyhow::ensure!(
            scope.owner == owner && scope.exchange_id == task_id,
            "child provider event has a different execution owner or task"
        );
        let mut task = self
            .agent_coordinator
            .task(task_id)
            .context("child event has no admitted task")?;
        anyhow::ensure!(
            task.agent_id == owner.agent_id,
            "child event has a different task owner"
        );
        if task.completed_at_ms.is_some() {
            return Ok(());
        }
        if let BackendEvent::Agent {
            update: crate::backend::AgentUpdate::Lifecycle { event: lifecycle },
            ..
        } = &backend_event
        {
            self.apply_agent_lifecycle(lifecycle, Some(&task.parent_exchange_id), event)
                .await?;
            return Ok(());
        }
        if backend_event
            .turn_address()
            .is_some_and(|address| self.diagnostic_native_thread.contains(&address.thread_id))
        {
            return Ok(());
        }
        let provider_thread_id = match &backend_event {
            BackendEvent::Agent {
                update: crate::backend::AgentUpdate::ConversationResolved { thread_id },
                ..
            } => Some(thread_id.as_str()),
            event => event
                .turn_address()
                .map(|address| address.thread_id.as_str()),
        };
        if let Some(thread_id) = provider_thread_id.filter(|id| !id.is_empty()) {
            let mut agent = self.agent_coordinator.resolve(&owner.agent_id)?;
            if agent.provider_thread_id.as_deref() != Some(thread_id) {
                agent.provider_thread_id = Some(thread_id.to_owned());
                self.store.save_agent_run(&agent)?;
                self.agent_registry.insert(agent.clone());
                self.agent_coordinator.retain(agent, task.clone())?;
            }
        }
        if matches!(&backend_event, BackendEvent::Agent { .. }) {
            return Ok(());
        }
        if let BackendEvent::Runtime {
            update: crate::backend::RuntimeUpdate::Resolved { runtime },
            ..
        } = &backend_event
        {
            let mut agent = self.agent_coordinator.resolve(&owner.agent_id)?;
            if let Some(model) = runtime.model.as_ref().filter(|model| !model.is_empty())
                && let Some(selection) = agent.selection.as_mut()
            {
                selection.model = model.clone();
            }
            self.store.save_agent_run(&agent)?;
            self.agent_registry.insert(agent.clone());
            self.agent_coordinator.retain(agent, task)?;
            return self.publish_agent_state(event).await;
        }
        if task.state == ExchangeState::Queued {
            task.state = ExchangeState::Running;
            self.store.save_agent_task(&task)?;
            self.agent_coordinator.retain(
                self.agent_coordinator.resolve(&owner.agent_id)?,
                task.clone(),
            )?;
        }
        anyhow::ensure!(
            self.child_exchange_runtime_by_agent
                .get(owner.agent_id.as_str())
                .is_none_or(|runtime| runtime.exchange.id == task_id),
            "late child event cannot update a newer task"
        );
        let mut runtime = match self
            .child_exchange_runtime_by_agent
            .remove(owner.agent_id.as_str())
        {
            Some(runtime) => runtime,
            None => {
                let mut exchange = self
                    .store
                    .load_exchange(task_id)?
                    .context("child task exchange missing")?;
                if exchange.is_finished() {
                    return Ok(());
                }
                exchange.phase = crate::exchange::ExchangePhase::Running;
                exchange.resume(self.clock.now_ms())?;
                ChildExchangeRuntime {
                    exchange,
                    checklist: ChecklistTracker::default(),
                }
            }
        };
        match &mut backend_event {
            BackendEvent::Approval {
                update: crate::backend::ApprovalUpdate::Requested(request),
                ..
            } => {
                let turn = runtime
                    .exchange
                    .turn
                    .iter()
                    .rev()
                    .find(|turn| turn.state() == crate::turn::TurnState::Running);
                let Some(approval) = self.permission_coordinator.bind_owner(
                    &request.id,
                    &runtime.exchange.id,
                    turn.map(|turn| turn.id()),
                )?
                else {
                    self.child_exchange_runtime_by_agent
                        .insert(owner.agent_id.to_string(), runtime);
                    return Ok(());
                };
                runtime.exchange.observe_blocker(
                    format!("approval:{}", approval.id),
                    true,
                    self.clock.now_ms(),
                );
                *request = approval;
            }
            BackendEvent::Approval { update, .. } => {
                let approval = match update {
                    crate::backend::ApprovalUpdate::Resolved(approval)
                    | crate::backend::ApprovalUpdate::Cancelled(approval) => approval,
                    crate::backend::ApprovalUpdate::Requested(_) => unreachable!(),
                };
                runtime.exchange.observe_blocker(
                    format!("approval:{}", approval.id),
                    false,
                    self.clock.now_ms(),
                );
                runtime.exchange.approval_resolution.push(approval.clone());
            }
            BackendEvent::Compaction { update, .. } => {
                runtime
                    .exchange
                    .observe_compaction(update.as_ref().clone(), self.clock.now_ms());
            }
            observation => {
                runtime
                    .exchange
                    .observe_turn(observation, self.clock.now_ms())?;
                if let BackendEvent::Checklist { update, .. } = observation {
                    runtime.checklist.replace(update);
                    runtime.exchange.checklist = Some(runtime.checklist.snapshot().clone());
                }
                attribute_provider_tool(
                    &mut runtime.exchange,
                    &mut runtime.checklist,
                    observation,
                )?;
            }
        }
        if backend_event.tool_output_delta().is_some() {
            self.store
                .save_tool_output_delta(&runtime.exchange, &backend_event)?;
        } else {
            self.store.save_exchange(&runtime.exchange)?;
        }
        task.last_activity_at_ms = Some(
            task.last_activity_at_ms
                .unwrap_or(i64::MIN)
                .max(self.clock.now_ms()),
        );
        self.store.save_agent_task(&task)?;
        self.agent_coordinator
            .retain(self.agent_coordinator.resolve(&owner.agent_id)?, task)?;
        self.child_exchange_runtime_by_agent
            .insert(owner.agent_id.to_string(), runtime);
        self.emit_observation(&backend_event, event).await?;
        self.publish_agent_state(event).await?;
        self.agent_coordinator.notify_activity();
        Ok(())
    }

    async fn complete_managed_agent(
        &mut self,
        owner: crate::backend::ExecutionOwner,
        task_id: String,
        output: Result<crate::backend::BackendCompletion>,
        cleanup_error: Option<String>,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let mut task = self
            .agent_coordinator
            .task(&task_id)
            .context("child completion has no admitted task")?;
        anyhow::ensure!(
            owner.session_id == self.session.id && task.agent_id == owner.agent_id,
            "child completion crosses execution ownership"
        );
        if task.completed_at_ms.is_some() {
            return Ok(());
        }
        self.cancel_agent_approvals(&owner, event).await?;
        let mut agent = self.agent_coordinator.resolve(&owner.agent_id)?;
        let interrupted = self
            .agent_coordinator
            .cancellation(&owner.agent_id, &task_id)
            .is_ok_and(|cancel| *cancel.borrow());
        task.state = if interrupted {
            ExchangeState::Cancelled
        } else if output.is_err() || cleanup_error.is_some() {
            ExchangeState::Failed
        } else {
            ExchangeState::Complete
        };
        task.error = cleanup_error
            .clone()
            .or_else(|| output.as_ref().err().map(|error| format!("{error:#}")));
        task.completed_at_ms = Some(self.clock.now_ms());
        if let Ok(output) = &output {
            agent.provider_thread_id = output
                .backend_session_id
                .clone()
                .or(agent.provider_thread_id);
        }
        let runtime = self
            .child_exchange_runtime_by_agent
            .remove(owner.agent_id.as_str());
        let mut exchange = if let Some(runtime) = runtime {
            runtime.exchange
        } else {
            self.store
                .load_exchange(&task_id)?
                .context("child completion exchange missing")?
        };
        exchange.finish(task.state.try_into()?, self.clock.now_ms())?;
        exchange.finalization_error = cleanup_error.clone();
        if exchange.turn.is_empty() {
            for message in agent.inbox.iter_mut().filter(|message| {
                message.task_id.as_deref() == Some(task_id.as_str())
                    && message.delivery == crate::agent::MessageDelivery::Attached
            }) {
                message.delivery = crate::agent::MessageDelivery::Queued;
                message.task_id = None;
            }
        }
        let assistant_message: Vec<_> = exchange
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .filter(|message| message.kind() == crate::turn::MessageKind::Assistant)
            .collect();
        let final_message: Vec<_> = assistant_message
            .iter()
            .filter(|message| message.delivery() == crate::turn::MessageDelivery::Final)
            .collect();
        task.final_response = Some(if final_message.is_empty() {
            assistant_message
                .last()
                .map(|message| message.text().to_owned())
                .unwrap_or_default()
        } else {
            final_message
                .iter()
                .map(|message| message.text())
                .collect::<Vec<_>>()
                .join("\n\n")
        });
        agent.state = if cleanup_error.is_some() {
            AgentState::Closing
        } else {
            AgentState::Ready
        };
        agent.updated_at_ms = self.clock.now_ms();
        self.store.complete_agent_task(&agent, &task, &exchange)?;
        self.agent_registry.insert(agent.clone());
        self.agent_coordinator.retain(agent.clone(), task.clone())?;
        let next =
            self.agent_coordinator
                .settled(&owner.agent_id, &task_id, cleanup_error.is_some())?;
        if cleanup_error.is_none() {
            self.child_backend_by_agent.remove(owner.agent_id.as_str());
        }
        self.trace.record(&self.session.id,"agent.completed",json!({"agent_id":owner.agent_id,"exchange_id":task_id,"state":task.state,"cleanup_error":cleanup_error}));
        self.publish_agent_state(event).await?;
        if let Some(next) = next {
            Box::pin(self.launch_managed_agent(next, event)).await?;
        }
        Ok(())
    }

    async fn apply_agent_lifecycle(
        &mut self,
        lifecycle: &AgentLifecycleEvent,
        _parent_exchange_id: Option<&str>,
        _event: &mut Vec<SessionEvent>,
    ) -> Result<Option<Agent>> {
        if let Some(thread) = &lifecycle.provider_thread_id {
            self.diagnostic_native_thread.insert(thread.clone());
        }
        self.trace.record(
            &self.session.id,
            "agent.native_diagnostic",
            serde_json::to_value(lifecycle)?,
        );
        Ok(None)
    }

    async fn list_backend_model(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut model_list = self
            .backend
            .model_list(self.backend_request(BackendInput::from_text(""), PromptMode::Chat))
            .await?;
        let preference = self
            .store
            .load_preference(&self.session.workspace, &self.session.backend)?;
        crate::backend::select_model_settings(&mut model_list, preference.as_ref());
        let event = self.resolve_default_model(&model_list)?;
        Ok((serde_json::to_value(model_list)?, event))
    }

    /// Publishes an unresolved default model without replacing an explicit selection.
    pub(crate) fn resolve_default_model(
        &mut self,
        model_list: &[crate::backend::BackendModel],
    ) -> Result<Vec<SessionEvent>> {
        if self.session.model == "default"
            && let Some(model) = model_list
                .iter()
                .find(|model| model.is_default)
                .or_else(|| model_list.first())
            && self.session.resolved_model.as_deref() != Some(model.id.as_str())
        {
            self.session.resolved_model = Some(model.id.clone());
            self.save_session()?;
            return Ok(vec![self.event(
                "session_configured",
                serde_json::to_value(self.session_view()?)?,
            )?]);
        }
        Ok(Vec::new())
    }

    async fn submit_prompt(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let snapshot = PromptControlSnapshot {
            session: self.session_view()?,
            goal: self.current_goal()?,
            execution: self.store.list_plan_execution(&self.session.id)?,
            plan: self.store.list_plan(&self.session.id)?,
        };
        self.prompt_admission = params
            .get("_prompt_admission")
            .map(|value| -> Result<_> {
                Ok((
                    serde_json::from_value(
                        value
                            .get("document")
                            .cloned()
                            .context("prompt admission document missing")?,
                    )?,
                    value
                        .get("token")
                        .and_then(Value::as_u64)
                        .context("prompt admission token missing")?,
                ))
            })
            .transpose()?;
        let submitted_prompt = self.prompt_admission.clone();
        let mut result = Box::pin(self.submit_prompt_inner(params)).await;
        let mut submission_event = Vec::new();
        Box::pin(self.settle_prompt_admission(result.is_ok(), &mut submission_event)).await?;
        if let Ok((_, event)) = &mut result {
            event.append(&mut submission_event);
        }
        if result
            .as_ref()
            .is_err_and(|error| error.downcast_ref::<TurnRetracted>().is_some())
        {
            Box::pin(self.restore_retracted_control_state(snapshot)).await?;
            if let Some((document, token)) = submitted_prompt {
                let transition = if let Some(document) = document {
                    self.presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                        .retract_submission(&document, token)?
                } else {
                    Some(json!({"document":null,"token":token,"state":"retracted"}))
                };
                if let Some(transition) = transition {
                    Box::pin(self.emit_session_update(
                        SessionUpdate::Submission {
                            transition: serde_json::from_value(transition)?,
                        },
                        &mut submission_event,
                    ))
                    .await?;
                }
            }
        }
        result
    }

    async fn settle_prompt_admission(
        &mut self,
        admitted: bool,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let Some((document, token)) = self.prompt_admission.take() else {
            return Ok(());
        };
        let transition = if let Some(document) = document {
            self.presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .complete_submission(&document, token, admitted)?
        } else {
            Some(
                json!({"document":null,"token":token,"state":if admitted { "accepted" } else { "rejected" }}),
            )
        };
        if let Some(transition) = transition {
            self.emit_session_update(
                SessionUpdate::Submission {
                    transition: serde_json::from_value(transition)?,
                },
                event,
            )
            .await?;
        }
        Ok(())
    }

    async fn submit_prompt_inner(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        self.require_restore_recovered()?;
        let text = required_text(&params, "text")?;
        let purpose = std::mem::take(&mut self.input_purpose);
        if matches!(purpose, crate::input::InputPurpose::User) {
            let operation_id = params
                .get("operation_id")
                .and_then(Value::as_str)
                .or(self.active_operation_id.as_deref());
            if let Some(command) =
                crate::task::command::SessionCommand::parse_text(&text, operation_id)?
            {
                return self.commit_session_command(command);
            }
        }
        anyhow::ensure!(
            self.check_runner.active()?.is_none(),
            "wait for or cancel the active check run before starting source work"
        );
        if !matches!(purpose, crate::input::InputPurpose::User) {
            let (revision, origin) = match purpose {
                crate::input::InputPurpose::TaskEntry {
                    revision, start, ..
                } => (
                    revision,
                    crate::exchange::ExchangeOrigin::TaskStarted { start },
                ),
                crate::input::InputPurpose::TaskContinuation {
                    revision, reason, ..
                } => (
                    revision,
                    crate::exchange::ExchangeOrigin::task_continuation(reason),
                ),
                crate::input::InputPurpose::User => unreachable!(),
            };
            let task = self
                .task_controller
                .current()?
                .context("task input was cleared before dispatch")?;
            anyhow::ensure!(
                task.revision == revision,
                "task input was replaced before dispatch"
            );
            if let crate::task::TaskWorkflow::Plan { plan } = &task.workflow {
                let mut admission = ExchangeAdmission::chat(text.clone());
                admission.origin = origin;
                if plan.artifact.state == PlanState::ConfiguringChecks {
                    self.observer.set_check_editing(true)?;
                    self.check_setup = true;
                    let result = self
                        .run_interaction(text, PromptMode::Chat, Some(admission))
                        .await;
                    self.check_setup = false;
                    return result;
                }
                return self.run_planning_interaction(text, Some(admission)).await;
            }
            let mode = if task.kind() == crate::task::TaskKind::Execute {
                PromptMode::ExecutePlan
            } else {
                PromptMode::GoalContinuation
            };
            let mut admission = ExchangeAdmission::chat(text.clone());
            admission.origin = origin;
            return self
                .run_interaction_input(
                    text.clone(),
                    mode,
                    Some(admission),
                    BackendInput::from_text(text),
                )
                .await;
        }
        if text.trim() == "/plan" {
            return self.observer.list_replanning_choices();
        }
        let change = match text.as_str() {
            "/read full" => Some(crate::task::PermissionChange::ToggleReadScope),
            "/write" => Some(crate::task::PermissionChange::ToggleWriteEnabled),
            "/write full" => Some(crate::task::PermissionChange::ToggleWriteScope),
            "/ask" => Some(crate::task::PermissionChange::SetApproval {
                mode: ApprovalMode::Ask,
            }),
            "/deny" => Some(crate::task::PermissionChange::SetApproval {
                mode: ApprovalMode::Deny,
            }),
            "/allow" => Some(crate::task::PermissionChange::SetApproval {
                mode: ApprovalMode::Allow,
            }),
            "/yolo" => Some(crate::task::PermissionChange::ToggleYolo),
            _ => None,
        };
        if let Some(change) = change {
            return Box::pin(self.transition_task(json!({"action":"permission","change":change})))
                .await;
        }
        anyhow::ensure!(
            !matches!(
                text.split_whitespace().next(),
                Some("/read" | "/write" | "/mode" | "/ask" | "/deny" | "/allow" | "/yolo")
            ),
            "Use /read full, /write, /write full, /ask, /deny, /allow, or /yolo"
        );
        if text == "/clear" {
            return Box::pin(self.new_session(Value::Null)).await;
        }
        if self.task_controller.current()?.is_some_and(|task| {
            task.status() == crate::task::TaskStatus::Paused || !task.entry_accepted()
        }) {
            return self
                .run_interaction_input(
                    text.clone(),
                    PromptMode::Chat,
                    Some(ExchangeAdmission::chat(text.clone())),
                    BackendInput::parse(&text),
                )
                .await;
        }
        let failed_plan_active = self
            .current_plan_id()?
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| plan.state == PlanState::Failed);
        anyhow::ensure!(
            !failed_plan_active,
            "plan generation stopped; run /plan retry or /plan cancel"
        );
        if let Some(plan) = self
            .current_plan_id()?
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
            return Box::pin(
                self.ask_plan_question(json!({ "question_id": question_id, "text": text })),
            )
            .await;
        }
        if let Some(interaction) = self.find_active_interaction_elicitation()? {
            let question_id = interaction
                .elicitation()
                .and_then(|elicitation| {
                    elicitation
                        .current_question()
                        .map(|question| question.id.clone())
                        .or_else(|| Some(elicitation.question_set.id.clone()))
                })
                .context("active interaction has no elicitation state")?;
            return Box::pin(
                self.ask_question(json!({ "question_id": question_id, "text": text })),
            )
            .await;
        }
        if self
            .current_plan_id()?
            .as_deref()
            .map(|id| self.store.load_plan(id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| plan.state == PlanState::ConfiguringChecks)
        {
            return Box::pin(self.configure_plan_checks(text)).await;
        }
        let active_plan = self
            .current_plan_id()?
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten();
        if self.current_task_is_plan()?
            && active_plan
                .as_ref()
                .is_some_and(|plan| plan.state == PlanState::AwaitingReview)
        {
            let admission = ExchangeAdmission::chat(text.clone());
            return Box::pin(self.run_planning_interaction(text, Some(admission))).await;
        }
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
                attachments,
            } => {
                if let Some(context) = prompt
                    .strip_suffix(&text)
                    .filter(|context| !context.is_empty())
                {
                    arguments = format!("{context}{arguments}");
                }
                BackendInput::Skill {
                    name,
                    arguments,
                    attachments,
                }
            }
            BackendInput::Text { .. } => BackendInput::from_text(prompt.clone()),
        };
        let prompt_mode = if self.current_task_is_plan()?
            && active_plan.as_ref().is_some_and(|plan| {
                matches!(plan.state, PlanState::Generating | PlanState::Revising)
            }) {
            PromptMode::Plan
        } else {
            PromptMode::Chat
        };
        Box::pin(self.run_interaction_input(
            prompt,
            prompt_mode,
            Some(ExchangeAdmission::chat(text)),
            input,
        ))
        .await
    }

    async fn restore_retracted_control_state(
        &mut self,
        snapshot: PromptControlSnapshot,
    ) -> Result<()> {
        let current = self.task_controller.current()?;
        let unchanged = match (&current, &snapshot.session.active_task) {
            (Some(current), Some(original)) => {
                current.revision == original.revision && current.same_contract(original)
            }
            (None, None) => true,
            _ => false,
        };
        if unchanged {
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
        }
        self.session = snapshot.session;
        self.session.active_task = current;
        self.save_session()?;
        if unchanged && self.capability.native_goal && self.session.backend_session_id.is_some() {
            let request = self.backend_request(BackendInput::from_text(""), PromptMode::Chat);
            match snapshot.goal {
                Some(goal) => {
                    self.backend
                        .goal_status(request, Some(goal.objective), goal_state_name(goal.state))
                        .await?
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
        self.publish_plan(&plan)?;
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
        self.publish_plan(&plan)?;
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
            .elicitation_mut()
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
            self.publish_plan(&plan)?;
        } else {
            let mut interaction = self.active_interaction_elicitation()?;
            let elicitation = interaction
                .elicitation_mut()
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
            self.publish_plan(&plan)?;
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
        interaction.set_awaiting_input(false);
        interaction.set_elicitation(None);
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
            self.publish_plan(&plan)?;
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
            .elicitation_mut()
            .context("active interaction has no elicitation state")?
            .replace_question_set(question);
        self.store.save_exchange(&owner)?;
        event.push(self.event(
            "question",
            json!({
                "owner": "interaction",
                "exchange_id": owner.id,
                "elicitation": owner.elicitation(),
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
            .current_plan_id()?
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
            .current_plan_id()?
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| {
                plan.state == PlanState::AwaitingReview && plan.acceptance.is_some()
            }))
    }

    fn active_plan_acceptance(&self) -> Result<PlanRecord> {
        let current_plan_id = self.current_plan_id()?;
        let plan_id = current_plan_id.as_deref().context("no active plan")?;
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
        self.store.pending_exchange(&self.session.id)
    }

    /// Capture the owning planning exchange for a user review or question action.
    fn plan_exchange_anchor(&self, plan_id: &str) -> Result<crate::plan::ExchangeAnchor> {
        if let Some(anchor) = self
            .store
            .list_plan_lifecycle(&self.session.id)?
            .into_iter()
            .rev()
            .filter(|lifecycle| lifecycle.plan_id == plan_id)
            .find_map(|lifecycle| lifecycle.anchor)
        {
            return Ok(anchor);
        }
        let exchange = self
            .store
            .latest_exchange(&self.session.id)?
            .context("planning event has no owning exchange")?;
        Ok(crate::plan::ExchangeAnchor::capture(&exchange))
    }

    /// Record planning feedback only on the live exchange that owns the plan.
    fn append_plan_feedback(
        &mut self,
        plan_id: &str,
        text: String,
        intent: crate::exchange::InputIntent,
        question: Option<crate::exchange::QuestionInput>,
    ) -> Result<()> {
        let mut exchange = self
            .store
            .latest_exchange(&self.session.id)?
            .context("planning feedback has no owning exchange")?;
        anyhow::ensure!(
            self.current_plan_id()?.as_deref() == Some(plan_id)
                && exchange.state() == ExchangeState::Running,
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
            question_id: Some(
                elicitation
                    .question(&question_id)
                    .or_else(|| elicitation.current_question())
                    .context("clarification has no pending question")?
                    .id
                    .clone(),
            ),
            answer: Vec::new(),
        };
        plan.updated_at_ms = self.clock.now_ms();
        self.append_plan_feedback(
            &plan.id,
            text.clone(),
            crate::exchange::InputIntent::Clarification,
            Some(target),
        )?;
        self.publish_plan(&plan)?;
        let (value, event) = self
            .run_interaction(
                PlanPrompt::clarification(&plan.request, &elicitation_json, &text),
                PromptMode::Chat,
                None,
            )
            .await?;
        self.finish_plan_clarification(&plan.id, value, event).await
    }

    async fn finish_plan_clarification(
        &mut self,
        plan_id: &str,
        mut value: Value,
        mut event: Vec<SessionEvent>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let current_plan = self
            .store
            .load_plan(plan_id)?
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
                    lifecycle.plan_id == plan_id
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
                .run_planning_interaction(
                    PlanPrompt::feedback(&current_plan.request, &feedback),
                    None,
                )
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
            .elicitation_mut()
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
            question_id: Some(
                elicitation
                    .question(&question_id)
                    .or_else(|| elicitation.current_question())
                    .context("clarification has no pending question")?
                    .id
                    .clone(),
            ),
            answer: Vec::new(),
        };
        interaction.append_question_input(
            crate::exchange::InputIntent::Clarification,
            text.clone(),
            target,
            self.clock.now_ms(),
        )?;
        self.store.save_exchange(&interaction)?;
        let (value, event) = self
            .run_interaction(
                PlanPrompt::question_follow_up(&elicitation_json, &text),
                self.task_prompt_mode()?,
                None,
            )
            .await?;
        self.finish_question_clarification(value, event).await
    }

    async fn finish_question_clarification(
        &mut self,
        mut value: Value,
        mut event: Vec<SessionEvent>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        if self
            .find_active_interaction_elicitation()?
            .and_then(|interaction| interaction.elicitation().cloned())
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
        self.append_plan_feedback(
            &plan.id,
            answer.clone(),
            crate::exchange::InputIntent::Answer,
            Some(crate::exchange::QuestionInput {
                set_id: question.id.clone(),
                question_id: None,
                answer: elicitation.answer.clone(),
            }),
        )?;
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
        self.publish_plan(&plan)?;
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
            .take_elicitation()
            .context("active interaction has no elicitation state")?;
        let feedback = elicitation.feedback();
        if interaction.check_setup.is_some()
            && elicitation.answer.iter().any(|answer| {
                answer.source == crate::session::ResolutionSource::User
                    && !matches!(answer.response, PlanQuestionResponse::Skipped)
            })
        {
            interaction.check_setup = Some(crate::checks::CheckSetupState::Reviewed);
        }
        interaction.set_awaiting_input(false);
        for node in &mut interaction.node_list {
            if let ExchangeNode::QuestionPresented {
                question, answer, ..
            } = node
                && question.id == elicitation.question_set.id
            {
                *answer = Some(feedback.clone());
            }
        }
        interaction.append_question_input(
            crate::exchange::InputIntent::Answer,
            feedback.clone(),
            crate::exchange::QuestionInput {
                set_id: elicitation.question_set.id.clone(),
                question_id: None,
                answer: elicitation.answer.clone(),
            },
            self.clock.now_ms(),
        )?;
        self.store.save_exchange(&interaction)?;
        let prompt = format!(
            "Harness resolved the pending questions and recorded their response sources. Do not call question_answer or question_withdraw for this set. Continue the original request using the responses and current task context.\n\n{feedback}"
        );
        let (value, mut event) = if matches!(
            self.task_prompt_mode()?,
            PromptMode::Plan | PromptMode::PlanDiscussion
        ) {
            self.run_planning_interaction(prompt, None).await?
        } else {
            self.run_interaction(prompt, PromptMode::Chat, None).await?
        };
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

    async fn continue_after_approval_timeout(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let owner = crate::backend::ExecutionOwner::root(&self.session.id);
        let Some(feedback) = self.permission_coordinator.take_timeout_feedback(&owner)? else {
            return Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()));
        };
        if let Some(task) = self.task_controller.current()? {
            if matches!(
                task.status(),
                crate::task::TaskStatus::Running | crate::task::TaskStatus::Waiting
            ) {
                let exchange = self
                    .store
                    .latest_exchange(&self.session.id)?
                    .context("task has no completed turn")?;
                self.task_controller.reserve_continuation(
                    &mut self.store,
                    &task.permit(),
                    crate::task::TaskTurnId {
                        exchange_ordinal: exchange.ordinal,
                        turn_ordinal: exchange.turn.len(),
                    },
                    feedback,
                    "An approval timed out".into(),
                    self.clock.now_ms(),
                )?;
            }
            return Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()));
        }
        let mut admission = ExchangeAdmission::chat(feedback.clone());
        admission.origin =
            crate::exchange::ExchangeOrigin::task_continuation("An approval timed out");
        self.run_interaction(feedback, PromptMode::Chat, Some(admission))
            .await
    }

    /// Publish canonical input ownership after a durable state transition.
    pub(crate) fn refresh_input_schedule(&self) -> Result<()> {
        let plan = if self.active_plan_awaits_input()? {
            Some(self.active_elicitation_plan()?)
        } else {
            None
        };
        let interaction = if plan.is_none() {
            self.find_active_interaction_elicitation()?
        } else {
            None
        };
        let input = plan
            .as_ref()
            .and_then(|plan| {
                plan.elicitation
                    .as_ref()
                    .map(|input| (format!("plan:{}", plan.id), input))
            })
            .or_else(|| {
                interaction.as_ref().and_then(|exchange| {
                    exchange
                        .elicitation()
                        .map(|input| (format!("exchange:{}", exchange.id), input))
                })
            });
        let next = crate::session::InputSchedule {
            identity: input
                .as_ref()
                .map(|(owner, input)| format!("{owner}:{}", input.question_set.id)),
            deadline: input.as_ref().and_then(|(_, input)| input.deadline.clone()),
            question_revision: input.as_ref().map(|(_, input)| input.revision),
            approval_allowed: input.is_none() && !self.active_plan_has_acceptance()?,
            ..Default::default()
        };
        self.observer
            .input_schedule
            .send_if_modified(|current| current.update(next, self.clock.now_ms()));
        Ok(())
    }

    /// Resolve expired input only while holding the owning session's execution admission.
    pub(crate) fn expire_input(
        &mut self,
        active_deadline: &mut Option<(String, crate::session::InputDeadline)>,
    ) -> Result<Option<Request>> {
        if self.turn_cancellation.requested.load(Ordering::Acquire) {
            return Ok(None);
        }
        let mut plan = if self.active_plan_awaits_input()? {
            Some(self.active_elicitation_plan()?)
        } else {
            None
        };
        let mut interaction = if plan.is_none() {
            self.find_active_interaction_elicitation()?
        } else {
            None
        };
        let owner = plan
            .as_ref()
            .map(|plan| format!("plan:{}", plan.id))
            .or_else(|| {
                interaction
                    .as_ref()
                    .map(|exchange| format!("exchange:{}", exchange.id))
            });
        let elicitation = plan
            .as_mut()
            .and_then(|plan| plan.elicitation.as_mut())
            .or_else(|| {
                interaction
                    .as_mut()
                    .and_then(|exchange| exchange.elicitation_mut())
            });
        if let Some(elicitation) = elicitation {
            if let Some(deadline) = &elicitation.deadline {
                let owner = owner.as_deref().expect("active input owner");
                let identity = format!(
                    "{owner}:{}:{}",
                    elicitation.question_set.id, deadline.expires_at_ms
                );
                if active_deadline
                    .as_ref()
                    .is_none_or(|(current, _)| current != &identity)
                {
                    let mut deadline = deadline.clone();
                    deadline.restore(self.clock.now_ms());
                    *active_deadline = Some((identity, deadline));
                }
                if active_deadline
                    .as_ref()
                    .is_some_and(|(_, deadline)| deadline.expired(self.clock.now_ms()))
                {
                    elicitation.expire()?;
                    if let Some(plan) = &plan {
                        self.publish_plan(plan)?;
                    }
                    if let Some(interaction) = &interaction {
                        self.store.save_exchange(interaction)?;
                    }
                    *active_deadline = None;
                    return Ok(Some(Request {
                        id: 0,
                        method: "session.question.continue".into(),
                        params: json!({}),
                    }));
                }
            } else {
                *active_deadline = None;
            }
            return Ok(None);
        }
        *active_deadline = None;
        let owner = crate::backend::ExecutionOwner::root(&self.session.id);
        if !self.active_plan_has_acceptance()?
            && self.permission_coordinator.has_timeout_feedback(&owner)?
        {
            return Ok(Some(Request {
                id: 0,
                method: "session.approval.continue".into(),
                params: json!({}),
            }));
        }
        Ok(None)
    }

    fn active_elicitation_plan(&self) -> Result<PlanRecord> {
        let current_plan_id = self.current_plan_id()?;
        let plan_id = current_plan_id.as_deref().context("no active plan")?;
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
        let result = self
            .run_interaction_input(text, mode, admission, input)
            .await?;
        if mode == PromptMode::Chat {
            Box::pin(self.advance_configured_plan(result)).await
        } else {
            Ok(result)
        }
    }

    async fn run_planning_interaction(
        &mut self,
        prompt: String,
        admission: Option<ExchangeAdmission>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let task = self
            .task_controller
            .current()?
            .context("planning requires an active task")?;
        let crate::task::TaskWorkflow::Plan { plan } = &task.workflow else {
            anyhow::bail!("planning requires a Plan task");
        };
        let plan_id = plan.artifact.id.clone();
        let before = self
            .plan_file
            .read_working_document(&self.session.id, &plan_id)?;
        let before_digest = plan_digest(&serde_json::to_vec(&before)?);
        let discussion = plan.artifact.state == PlanState::AwaitingReview;
        let result = self
            .run_interaction(
                prompt,
                if discussion {
                    PromptMode::PlanDiscussion
                } else {
                    PromptMode::Plan
                },
                admission,
            )
            .await;
        let current = self.task_controller.current()?;
        let Some(current) = current.filter(|current| {
            matches!(&current.workflow,
            crate::task::TaskWorkflow::Plan { plan } if plan.artifact.id == plan_id && !plan.paused)
        }) else {
            return result;
        };
        if task.permit().validate(Some(&current)).is_err()
            && !matches!(&current.workflow, crate::task::TaskWorkflow::Plan { plan }
                if matches!(plan.artifact.state, PlanState::AwaitingReview | PlanState::AwaitingInput))
        {
            return result;
        }
        let (mut value, mut events) = match result {
            Ok(result) => result,
            Err(error) => {
                if !error.is::<TurnCancelled>() {
                    let now_ms = self.clock.now_ms();
                    if let Err(persistence) = self.task_controller.update(
                        &mut self.store,
                        &current.permit(),
                        now_ms,
                        |task| {
                            let crate::task::TaskWorkflow::Plan { plan } = &mut task.workflow
                            else {
                                anyhow::bail!("plan was replaced")
                            };
                            if matches!(
                                plan.artifact.state,
                                PlanState::Generating
                                    | PlanState::Revising
                                    | PlanState::AwaitingInput
                            ) {
                                PlanStateMachine::apply(
                                    &mut plan.artifact,
                                    PlanEvent::TransitionFailed,
                                    now_ms,
                                )?;
                            }
                            Ok(())
                        },
                    ) && !persistence.is::<crate::task::StaleTask>()
                    {
                        return Err(persistence).context("persist planning failure");
                    }
                }
                return Err(error);
            }
        };
        let crate::task::TaskWorkflow::Plan { plan } = &current.workflow else {
            unreachable!()
        };
        if discussion && plan.artifact.state == PlanState::AwaitingReview {
            return Ok((value, events));
        }
        let after = self
            .plan_file
            .read_working_document(&self.session.id, &plan_id)?;
        let progress = plan_digest(&serde_json::to_vec(&after)?) != before_digest;
        let mut completed = self
            .store
            .latest_exchange(&self.session.id)?
            .context("planning progress has no completed exchange")?;
        let observed = self.task_controller.observe_plan(
            &mut self.store,
            &current.permit(),
            crate::task::TaskTurnId {
                exchange_ordinal: completed.ordinal,
                turn_ordinal: completed.turn.len(),
            },
            progress,
            self.clock.now_ms(),
        );
        let Some((plan, _)) = (match observed {
            Ok(result) => result,
            Err(error) if error.is::<crate::task::StaleTask>() => return Ok((value, events)),
            Err(error) => return Err(error),
        }) else {
            return Ok((value, events));
        };
        if !completed.awaiting_input() && !completed.is_finished() {
            self.capture_final_checkpoint(
                &mut completed,
                crate::exchange::ExchangeOutcome::Complete,
            )
            .await?;
            completed.finish(
                crate::exchange::ExchangeOutcome::Complete,
                self.clock.now_ms(),
            )?;
            self.store.save_exchange(&completed)?;
            self.exchange_runtime = None;
            value["exchange"] = json!({"id":completed.id,"state":completed.state()});
            events.push(self.event(
                "exchange_complete",
                json!({"id":completed.id,"state":completed.state()}),
            )?);
        }
        if plan.state == PlanState::Failed {
            events.push(self.event(
                "plan_generation_failed",
                json!({"plan":plan,
                "reason":"planning stopped without submitting the canonical plan"}),
            )?);
        }
        Ok((value, events))
    }

    async fn run_interaction_input(
        &mut self,
        text: String,
        mode: PromptMode,
        admission: Option<ExchangeAdmission>,
        input: BackendInput,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        self.context_usage_fresh = false;
        match Box::pin(self.run_interaction_input_inner(text, mode, admission, input)).await {
            Ok(result) => {
                let mut result = Box::pin(self.settle_automatic_checks(result, mode)).await?;
                if mode != PromptMode::Background
                    && let Err(error) = Box::pin(self.auto_compact()).await
                {
                    result.1.push(self.event(
                        "background_error",
                        json!({"error":format!("Automatic compaction failed: {error:#}")}),
                    )?);
                }
                Ok(result)
            }
            Err(error) if self.exchange_runtime.is_some() => {
                match Box::pin(self.finalize_interaction_after_processing_failure()).await {
                    Ok(()) => Err(error),
                    Err(finalization_error) => Err(error.context(format!(
                        "also failed to finalize the interaction: {finalization_error:#}"
                    ))),
                }
            }
            Err(error) => Err(error),
        }
    }

    async fn settle_automatic_checks(
        &mut self,
        mut result: (Value, Vec<SessionEvent>),
        mode: PromptMode,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        use crate::checks::{AutomaticChecks, CheckState, RunRequest};
        if !matches!(
            mode,
            PromptMode::Chat | PromptMode::RequestChanges | PromptMode::GoalContinuation
        ) || self.check_setup
            || self.agent_coordinator.active_count() != 0
            || self.find_active_interaction_elicitation()?.is_some()
        {
            return Ok(result);
        }
        let Some(interaction) = self.store.latest_exchange(&self.session.id)? else {
            return Ok(result);
        };
        if interaction.state() != ExchangeState::Complete
            || interaction.awaiting_input()
            || interaction.check_setup.is_some()
        {
            return Ok(result);
        }
        if self.task_controller.current()?.is_some() {
            return Ok(result);
        }
        let settings = crate::settings::SettingsStore::open(&self.data_root)?;
        let check_settings = settings
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.backend,
            )?
            .desired;
        if !check_settings.automatic_checks_enabled() {
            self.observer.completion_queue.finish()?;
            return Ok(result);
        }
        if crate::checks::ConfigSnapshot::load(Path::new(&self.session.workspace)).is_err() {
            let catalog = self.check_runner.catalog(None)?;
            if catalog["configured"] == false {
                return Ok(result);
            }
            anyhow::bail!(
                "Automatic checks could not load configuration: {}",
                catalog["error"]
            );
        }
        let Some(change_owner) = interaction.check_owner.clone() else {
            return Ok(result);
        };
        let owner = change_owner.clone();
        let mut automatic = AutomaticChecks::open(&self.check_runner.root, &owner)?;
        let timing = check_settings.check_timing.unwrap_or_default();
        if self
            .observer
            .completion_queue
            .defer(timing, &change_owner)?
        {
            automatic.pending = true;
            automatic.save()?;
            result.0["checks_deferred"] = json!(true);
            return Ok(result);
        }
        loop {
            if !settings
                .get(
                    &self.session.workspace,
                    &self.session.id,
                    &self.session.backend,
                )?
                .desired
                .automatic_checks_enabled()
            {
                automatic.pending = false;
                automatic.blocked = None;
                automatic.delivered.clear();
                automatic.repair_prompt = None;
                automatic.repair_exchange = None;
                automatic.save()?;
                self.observer.completion_queue.finish()?;
                return Ok(result);
            }
            if self.turn_cancellation.requested.load(Ordering::Acquire) {
                return Err(anyhow::Error::new(TurnCancelled));
            }
            if let Some(id) = self.check_runner.active()? {
                let running = self.check_runner.status(&id, 30_000, false).await?;
                if running.state == CheckState::Running {
                    continue;
                }
            }
            self.check_runner.refresh_changes().await?;
            let mut run = self.check_runner.prepare_request(
                self.store
                    .capture_timeline_owner(&self.session.id, &interaction.id)?,
                RunRequest::default(),
                None,
                self.session.shell,
                "Automatic completion checks".into(),
            )?;
            if !run
                .commands
                .iter()
                .any(|command| command.state != CheckState::Skipped)
            {
                self.observer.completion_queue.finish()?;
                automatic.pending = false;
                automatic.blocked = None;
                automatic.delivered.clear();
                automatic.repair_prompt = None;
                automatic.repair_exchange = None;
                automatic.save()?;
                return Ok(result);
            }
            automatic.pending = true;
            automatic.save()?;
            run.context.automatic = true;
            let run = self
                .check_runner
                .run(
                    run,
                    self.event_sink.clone(),
                    Some(Arc::clone(&self.turn_cancellation.requested)),
                )
                .await?;
            if run.state == CheckState::Interrupted {
                return Err(anyhow::Error::new(TurnCancelled));
            }
            if !settings
                .get(
                    &self.session.workspace,
                    &self.session.id,
                    &self.session.backend,
                )?
                .desired
                .automatic_checks_enabled()
            {
                continue;
            }
            if run.state == CheckState::Stale {
                continue;
            }
            let repair = automatic.observe(&run)?;
            if matches!(run.state, CheckState::Stopped | CheckState::ExecutionError) {
                result.0["automatic_checks"] = json!({"run_id":run.id,"state":run.state});
                return Ok(result);
            }
            if let Some(reason) = &automatic.blocked {
                self.emit_session_update(
                    SessionUpdate::Diagnostic {
                        diagnostic: crate::backend::ProviderDiagnostic {
                            code: None,
                            message: reason.clone(),
                            fatal: false,
                        },
                    },
                    &mut result.1,
                )
                .await?;
                result.0["automatic_checks"] = json!({"run_id":run.id,"blocked":reason});
                return Ok(result);
            }
            let Some(prompt) = repair else {
                self.observer.completion_queue.finish()?;
                result.0["automatic_checks"] = json!({"run_id":run.id,"state":run.state});
                return Ok(result);
            };
            let mut admission = ExchangeAdmission::chat(String::new());
            admission.check_owner = Some(change_owner.clone());
            admission.automatic_owner = Some(owner.clone());
            let repaired = Box::pin(self.run_interaction_input_inner(
                prompt.clone(),
                PromptMode::Chat,
                Some(admission),
                BackendInput::from_text(prompt),
            ))
            .await?;
            result.0 = repaired.0;
            result.1.extend(repaired.1);
            let latest = self
                .store
                .latest_exchange(&self.session.id)?
                .context("repair exchange disappeared")?;
            automatic.repair_exchange = Some(latest.id.clone());
            automatic.save()?;
            if latest.state() != ExchangeState::Complete
                || latest.awaiting_input()
                || self.find_active_interaction_elicitation()?.is_some()
                || self.agent_coordinator.active_count() != 0
            {
                return Ok(result);
            }
        }
    }

    async fn finalize_interaction_after_processing_failure(&mut self) -> Result<()> {
        let Some(runtime) = self.exchange_runtime.take() else {
            return Ok(());
        };
        let mut interaction = self
            .store
            .load_exchange(&runtime.exchange_id)?
            .context("failed interaction runtime has no durable interaction")?;
        if interaction.state() != ExchangeState::Running {
            return Ok(());
        }

        let now_ms = self.clock.now_ms();
        let mut event = Vec::new();
        self.record_execution_failure(
            &mut interaction,
            "Failed while processing the provider response".into(),
        )
        .await?;
        self.capture_final_checkpoint(&mut interaction, crate::exchange::ExchangeOutcome::Failed)
            .await?;
        interaction.finish(crate::exchange::ExchangeOutcome::Failed, now_ms)?;
        self.store.save_exchange(&interaction)?;
        self.pause_goal_after_turn_failure().await?;
        self.save_session()?;
        self.active_wait_projection = None;
        self.emit_live_interaction(
            publication::LiveUpdate::StateChanged,
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
        self.require_restore_recovered()?;
        let tool_gate = self
            .checkpoint
            .as_ref()
            .map(|_| crate::checkpoint::ToolGate::new())
            .transpose()?;
        let checkpoint = self.checkpoint.clone().map(|checkpoint| {
            let objects = self.store.objects.clone();
            let repositories = Arc::clone(&self.repositories);
            let session_id = self.session.id.clone();
            let trace = self.trace.clone();
            let exchange_id = interaction.id.clone();
            crate::checkpoint::CheckpointTask(tokio::spawn(async move {
                trace
                    .instrument(
                        &session_id,
                        &exchange_id,
                        "checkpoint.capture",
                        checkpoint.capture(&objects, &repositories, &session_id, now_ms),
                    )
                    .await
            }))
        });
        self.active_wait_projection = None;
        self.exchange_runtime = Some(ExchangeRuntime {
            task_permit: self
                .task_controller
                .current()?
                .filter(|task| task.entry_accepted())
                .map(|task| task.permit()),
            checkpoint,
            tool_gate,
            exchange_id: interaction.id.clone(),
            checklist: interaction
                .checklist
                .clone()
                .map(ChecklistTracker::from_snapshot)
                .unwrap_or_default(),
            retraction_eligible: true,
            active_wait: None,
        });
        Ok(())
    }

    fn finish_initial_checkpoint(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        result: Result<crate::checkpoint::CheckpointRecord>,
    ) -> Result<()> {
        runtime.checkpoint.take();
        let saved = result.and_then(|checkpoint| {
            let task = self.task_controller.current()?.filter(|task| runtime.task_permit.as_ref()
                .is_some_and(|permit| permit.validate(Some(task)).is_ok()));
            let execution = task.as_ref().and_then(|task| match &task.workflow {
                crate::task::TaskWorkflow::Execute { execution } => Some(execution),
                _ => None,
            });
            let queued_owner = if execution.is_none() {
                self.observer.completion_queue.owner()?
            } else {
                None
            };
            let task_owner = task.as_ref().map(|task| task.revision.as_str().to_owned());
            let owner = interaction
                .check_owner
                .as_ref()
                .or(queued_owner.as_ref())
                .or(task_owner.as_ref())
                .unwrap_or(&interaction.id)
                .clone();
            interaction.check_owner = Some(owner.clone());
            self.check_runner.begin_changes(owner, &checkpoint)?;
            interaction.checkpoint_before = Some(checkpoint.id.clone());
            self.store
                .save_checkpoint_exchange(&checkpoint, interaction)?;
            if let Some(task) = task
                && matches!(&task.workflow, crate::task::TaskWorkflow::Execute { execution } if execution.record.baseline_checkpoint.is_none())
            {
                self.task_controller.update(&mut self.store, &task.permit(), self.clock.now_ms(), |task| {
                    if let crate::task::TaskWorkflow::Execute { execution } = &mut task.workflow {
                        execution.record.baseline_checkpoint = Some(checkpoint.id);
                    }
                    Ok(())
                })?;
            }
            Ok(())
        });
        if let Some(gate) = &runtime.tool_gate {
            gate.complete(
                saved
                    .as_ref()
                    .map(|_| ())
                    .map_err(|error| format!("{error:#}")),
            )?;
        }
        self.trace.record(
            &self.session.id,
            "turn.dependency",
            json!({
                "exchange_id":interaction.id,"phase":"checkpoint.persist",
                "status":if saved.is_ok() { "ready" } else { "failed" },
            }),
        );
        saved
    }

    async fn collect_initial_checkpoint(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
    ) -> Result<()> {
        let Some(task) = runtime.checkpoint.as_mut() else {
            return Ok(());
        };
        let result = (&mut task.0)
            .await
            .context("checkpoint worker failed")
            .and_then(|result| result);
        self.finish_initial_checkpoint(runtime, interaction, result)
    }

    async fn park_exchange_for_restart(&mut self, interaction: &mut Exchange) -> Result<()> {
        if let Err(error) = self.collect_active_initial_checkpoint(interaction).await {
            interaction.begin_finalization(
                crate::exchange::ExchangeOutcome::Failed,
                self.clock.now_ms(),
            )?;
            interaction.finalization_error = Some(format!("{error:#}"));
            self.store.save_exchange(interaction)?;
            return Err(error);
        }
        interaction.pause_for_restart(self.clock.now_ms())?;
        self.store.save_exchange(interaction)?;
        self.exchange_runtime = None;
        self.active_wait_projection = None;
        self.save_session()
    }

    async fn collect_active_initial_checkpoint(
        &mut self,
        interaction: &mut Exchange,
    ) -> Result<()> {
        if !self.exchange_runtime.as_ref().is_some_and(|runtime| {
            runtime.exchange_id == interaction.id && runtime.checkpoint.is_some()
        }) {
            return Ok(());
        }
        let mut runtime = self.exchange_runtime.take().expect("checked runtime");
        let result = self
            .collect_initial_checkpoint(&mut runtime, interaction)
            .await;
        self.exchange_runtime = Some(runtime);
        result
    }

    fn resume_exchange_runtime(&mut self, interaction: &Exchange) -> Result<()> {
        self.require_restore_recovered()?;
        self.exchange_runtime = Some(ExchangeRuntime {
            task_permit: self
                .task_controller
                .current()?
                .filter(|task| task.entry_accepted())
                .map(|task| task.permit()),
            checkpoint: None,
            tool_gate: None,
            exchange_id: interaction.id.clone(),
            checklist: interaction
                .checklist
                .clone()
                .map(ChecklistTracker::from_snapshot)
                .unwrap_or_default(),
            retraction_eligible: false,
            active_wait: None,
        });
        Ok(())
    }

    fn agent_result_message(&self, task: &crate::agent::AgentTask) -> Result<String> {
        let agent = self.agent_coordinator.resolve(&task.agent_id)?;
        Ok(format!(
            "Harness child result. This is a report from a child agent, not a new user request.\n{}",
            serde_json::to_string(&json!({
                "agent":agent.task_path,"task_id":task.id,"status":task.state,
                "response":task.final_response,"error":task.error,
            }))?
        ))
    }

    fn acknowledge_agent_result(&mut self, mut task: crate::agent::AgentTask) -> Result<()> {
        task.parent_reported = true;
        self.store.save_agent_task(&task)?;
        self.agent_coordinator
            .retain(self.agent_coordinator.resolve(&task.agent_id)?, task)
    }

    fn start_agent_result_delivery(
        &self,
        attached: &[crate::agent::AgentTask],
        attempted: &mut std::collections::HashSet<String>,
        delivery: &mut tokio::task::JoinSet<(crate::agent::AgentTask, Result<()>)>,
    ) -> Result<()> {
        if !self.backend.descriptor().capability.native_steer {
            return Ok(());
        }
        for task in self.agent_coordinator.unreported_result() {
            if attached.iter().any(|attached| attached.id == task.id)
                || !attempted.insert(task.id.clone())
            {
                continue;
            }
            let message = self.agent_result_message(&task)?;
            let backend = self.backend.clone();
            let session_id = self.session.id.clone();
            delivery.spawn(async move {
                let result = tokio::time::timeout(std::time::Duration::from_secs(30), async {
                    anyhow::ensure!(
                        backend.has_active_turn(&session_id).await,
                        "parent provider turn ended before delivery"
                    );
                    backend
                        .steer_session(
                            &session_id,
                            crate::backend::SteeringInput::AgentResult(message),
                        )
                        .await
                })
                .await
                .context("parent result delivery exceeded 30 seconds")
                .and_then(|result| result);
                (task, result)
            });
        }
        Ok(())
    }

    fn finish_agent_result_delivery(
        &mut self,
        delivery: std::result::Result<
            (crate::agent::AgentTask, Result<()>),
            tokio::task::JoinError,
        >,
    ) -> Result<()> {
        let (task, result) = delivery.context("parent result delivery task failed")?;
        match result {
            Ok(()) => {
                self.trace.record(
                    &self.session.id,
                    "agent.result_delivered",
                    json!({"task_id":task.id,"delivery":"steered"}),
                );
                self.acknowledge_agent_result(task)?;
            }
            Err(error) => self.trace.record(
                &self.session.id,
                "agent.result_queued",
                json!({
                "task_id":task.id,"error":format!("{error:#}")}),
            ),
        }
        Ok(())
    }

    async fn prompt_with_timeline(
        &mut self,
        mut request: BackendRequest,
        interaction: &mut Exchange,
        event: &mut Vec<SessionEvent>,
        prepared: admission::PreparedTurn,
    ) -> Result<crate::backend::BackendCompletion> {
        prepared.consume(interaction)?;
        let mut parent_input = self.parent_input.activate(None)?;
        let mut deferred_input = std::collections::VecDeque::new();
        let mut continuation_command: Option<crate::backend::SteerCommand> = None;
        let mut continuation_count = 0;
        let mut aggregate: Option<crate::backend::BackendCompletion> = None;
        let mut continuation_result = self.agent_coordinator.unreported_result();
        if !continuation_result.is_empty() {
            let message = continuation_result
                .iter()
                .map(|task| self.agent_result_message(task))
                .collect::<Result<Vec<_>>>()?
                .join("\n\n");
            match &mut request.input {
                BackendInput::Text { text, .. } => {
                    text.push_str("\n\n");
                    text.push_str(&message);
                }
                BackendInput::Skill { arguments, .. } => {
                    arguments.push_str("\n\n");
                    arguments.push_str(&message);
                }
            }
        }
        loop {
            if self.turn_cancellation.requested.load(Ordering::Acquire) {
                return Err(anyhow::Error::new(TurnCancelled));
            }
            // Heap-owned child futures keep resumed repair polling within the worker stack.
            let output = Box::pin(self.prompt_provider_with_timeline(
                request.clone(),
                interaction,
                event,
                &continuation_result,
                ProviderSteering {
                    active: &mut parent_input,
                    deferred: &mut deferred_input,
                    continuation: continuation_command.take(),
                },
            ))
            .await?;
            for task in continuation_result.drain(..) {
                self.acknowledge_agent_result(task)?;
            }
            if self.exchange_runtime.is_some() {
                self.store.save_exchange(interaction)?;
            }
            let user_decision = output.plan_question.is_some()
                || output.question_withdrawal.is_some()
                || output.plan_submit.is_some()
                || output.control_error.is_some()
                || output.evidence.structured_blocked
                || interaction.awaiting_input();
            request.backend_session_id = output
                .backend_session_id
                .clone()
                .or(request.backend_session_id);
            aggregate = Some(if let Some(mut aggregate) = aggregate {
                aggregate.backend_session_id = output
                    .backend_session_id
                    .clone()
                    .or(aggregate.backend_session_id);
                aggregate.provider_checkpoint_id = output
                    .provider_checkpoint_id
                    .or(aggregate.provider_checkpoint_id);
                aggregate.plan_edit.extend(output.plan_edit);
                aggregate.design_patch.extend(output.design_patch);
                aggregate.plan_deviation.extend(output.plan_deviation);
                aggregate.plan_task_report.extend(output.plan_task_report);
                aggregate.plan_read = output.plan_read.or(aggregate.plan_read);
                aggregate.question_answer = output.question_answer.or(aggregate.question_answer);
                aggregate.question_withdrawal =
                    output.question_withdrawal.or(aggregate.question_withdrawal);
                aggregate.structured_plan |= output.structured_plan;
                aggregate.evidence.workspace_changed |= output.evidence.workspace_changed;
                aggregate.evidence.tool_called |= output.evidence.tool_called;
                aggregate.evidence.native_state = output
                    .evidence
                    .native_state
                    .or(aggregate.evidence.native_state);
                aggregate.evidence.structured_complete |= output.evidence.structured_complete;
                aggregate.evidence.structured_blocked |= output.evidence.structured_blocked;
                aggregate.goal_control = output.goal_control.or(aggregate.goal_control);
                aggregate.metrics.context_usage = output
                    .metrics
                    .context_usage
                    .or(aggregate.metrics.context_usage);
                aggregate.metrics.native_compact_update = output
                    .metrics
                    .native_compact_update
                    .or(aggregate.metrics.native_compact_update);
                aggregate.capability = output.capability;
                aggregate.runtime = output.runtime.clone();
                aggregate.plan_question = output.plan_question;
                aggregate.plan_submit = output.plan_submit;
                aggregate.control_error = output.control_error;
                aggregate
            } else {
                output
            });
            if user_decision {
                break;
            }
            let mut revision = self.agent_coordinator.subscribe();
            let mut continuation_input = None;
            let mut continuation_submission = None;
            loop {
                if self.turn_cancellation.requested.load(Ordering::Acquire) {
                    return Err(anyhow::Error::new(TurnCancelled));
                }
                event.extend(self.drain_agent_commands(Some(interaction)).await?);
                while let Some(input) = parent_input.try_receive() {
                    deferred_input.push_back(input);
                }
                if self.agent_coordinator.is_quiescing() {
                    tokio::select! {
                        biased;
                        _ = self.turn_cancellation.cancelled() => return Err(anyhow::Error::new(TurnCancelled)),
                        changed = revision.changed() => {
                            changed.context("agent coordinator closed during child cleanup")?;
                        }
                    }
                    continue;
                }
                if let Some(input) = deferred_input.pop_front() {
                    continuation_input = Some(
                        BackendInput::from_text(input.text.clone())
                            .with_attachments(input.attachments.clone()),
                    );
                    continuation_submission = input.submission.clone();
                    continuation_command = Some(input);
                    continuation_result = self.agent_coordinator.unreported_result();
                    if let Some(BackendInput::Text { text, .. }) = &mut continuation_input {
                        for task in &continuation_result {
                            text.push_str("\n\n");
                            text.push_str(&self.agent_result_message(task)?);
                        }
                    }
                    break;
                }
                if !self.agent_coordinator.has_unresolved_task(&interaction.id) {
                    break;
                }
                let result = self.agent_coordinator.pending_result(&interaction.id);
                if !self.agent_coordinator.has_active_task(&interaction.id) && !result.is_empty() {
                    anyhow::ensure!(
                        continuation_count < self.goal_max_turns,
                        "child-result continuation reached the session turn limit"
                    );
                    continuation_count += 1;
                    let message = result
                        .iter()
                        .map(|task| self.agent_result_message(task))
                        .collect::<Result<Vec<_>>>()?
                        .join("\n\n");
                    continuation_input = Some(BackendInput::from_text(format!(
                        "Continue this same unfinished request using the child messages below. Synthesize the results and complete the original request when its required work is settled.\n\n{message}"
                    )));
                    continuation_result = result;
                    self.trace.record(&self.session.id,"agent.parent_continuation",json!({"agent_id":self.session.primary_agent_id,"exchange_id":interaction.id,"turn":continuation_count}));
                    break;
                }
                self.set_parent_wait(interaction, true, event).await?;
                tokio::select! {
                    biased;
                    _ = self.turn_cancellation.cancelled() => return Err(anyhow::Error::new(TurnCancelled)),
                    input = parent_input.receive() => {deferred_input.push_back(input.context("parent input receiver closed")?);}
                    changed = revision.changed() => {changed.context("agent coordinator closed while parent awaits results")?;}
                }
            }
            self.set_parent_wait(interaction, false, event).await?;
            if let Some(input) = continuation_input {
                request.input = input;
                request.submission = continuation_submission;
                request.recovered_input.clear();
            } else {
                break;
            }
        }
        aggregate.context("provider returned no parent output")
    }

    async fn set_parent_wait(
        &mut self,
        interaction: &mut Exchange,
        waiting: bool,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let now_ms = self.clock.now_ms();
        let previous = self
            .active_wait_projection
            .as_ref()
            .filter(|wait| wait.exchange_id == interaction.id);
        let started_at_ms = previous.map_or(now_ms, |wait| wait.started_at_ms);
        if !waiting && previous.is_none() {
            return Ok(());
        }
        interaction.observe_blocker("agent_wait".into(), waiting, now_ms);
        self.active_wait_projection = waiting.then(|| ActiveWait {
            exchange_id: interaction.id.clone(),
            started_at_ms,
            agent_count: self.agent_coordinator.active_count(),
        });
        if let Some(runtime) = &mut self.exchange_runtime {
            runtime.active_wait = self.active_wait_projection.clone();
        }
        self.store.save_exchange(interaction)?;
        let patch = self.reconcile_live_interaction(Some(interaction), None)?;
        self.emit_timeline_patch(patch, event).await
    }

    async fn notify_unavailable_mcp(
        &mut self,
        request: &BackendCatalogRequest,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let target = match self.session.backend.as_str() {
            "codex" => "codexcli",
            "copilot" => "copilotcli",
            "claude" => "claudecode",
            "antigravity" => "antigravity-cli",
            _ => return Ok(()),
        };
        let resources = crate::settings::SessionResources::load(
            &self.data_root,
            request,
            &self.session.backend,
            target,
        )?;
        for (id, message) in &resources.launch_notice {
            if self.mcp_notice.get(id) == Some(message) {
                continue;
            }
            self.trace.record(
                &self.session.id,
                "mcp.start_skipped",
                json!({"resource_id":id,"reason":message}),
            );
            self.emit_session_update(
                SessionUpdate::Diagnostic {
                    diagnostic: crate::backend::ProviderDiagnostic {
                        code: None,
                        message: message.clone(),
                        fatal: false,
                    },
                },
                event,
            )
            .await?;
        }
        self.mcp_notice = resources.launch_notice;
        Ok(())
    }

    async fn prompt_provider_with_timeline(
        &mut self,
        mut request: BackendRequest,
        interaction: &mut Exchange,
        event: &mut Vec<SessionEvent>,
        attached_result: &[crate::agent::AgentTask],
        steering: ProviderSteering<'_>,
    ) -> Result<crate::backend::BackendCompletion> {
        let ProviderSteering {
            active: parent_input,
            deferred: deferred_input,
            continuation: continuation_command,
        } = steering;
        let turn_task_authority = request
            .control_context
            .as_ref()
            .and_then(crate::control_tools::TaskAuthority::capture);
        if request.mode != PromptMode::Background {
            self.notify_unavailable_mcp(
                &BackendCatalogRequest {
                    execution_owner: request.execution_owner.clone(),
                    harness_session_id: request.harness_session_id.clone(),
                    workspace: request.workspace.clone(),
                    access: request.access.clone(),
                    approval_mode: request.approval_mode,
                    backend_session_id: request.backend_session_id.clone(),
                },
                event,
            )
            .await?;
        }
        let (agent_scope, _agent_lease) = crate::agent::AgentToolScope::new(
            crate::backend::ExecutionOwner::root(&self.session.id),
            interaction.id.clone(),
            self.agent_coordinator.clone(),
        )?;
        if let Some(context) = &mut request.control_context {
            context.agent = Some(agent_scope);
        }
        let mut agent_revision = self.agent_coordinator.subscribe();
        let mut attempted_result = std::collections::HashSet::new();
        let mut result_delivery = tokio::task::JoinSet::new();
        let mut user_delivery = tokio::task::JoinSet::new();
        event.extend(self.drain_agent_commands(Some(interaction)).await?);
        let mut runtime = self
            .exchange_runtime
            .take()
            .context("active interaction timeline is missing")?;
        let backend = Arc::clone(&self.backend);
        let cancellation = Arc::clone(&self.turn_cancellation);
        let retry_trace = Arc::clone(&self.trace);
        let submission_store = self.submission_store.clone();
        let mut cleanup = cancellation.cleanup.subscribe();
        let scope = crate::backend::InvocationScope::new(request.owner()?, interaction.id.clone());
        let (backend_event_sink, mut backend_event_stream) =
            crate::backend::events::channel(scope.clone());
        let (execution_sender, mut execution_receiver) =
            tokio::sync::mpsc::channel::<crate::plan::execution::ExecutionControl>(1);
        let execution = self
            .current_execution()?
            .filter(|execution| execution.state == PlanExecutionState::Active);
        if request.mode == PromptMode::ExecutePlan
            && let (Some(context), Some(execution)) =
                (&mut request.control_context, execution.as_ref())
        {
            context.execution = Some(crate::plan::execution::ExecutionControlSender {
                task_controller: self.task_controller.clone(),
                phase: execution.phase,
                execution_id: execution.id.clone(),
                generation: execution.generation,
                sender: execution_sender.clone(),
            });
        }
        let (check_scope, _check_lease) = crate::checks::CheckToolScope::open(
            Arc::clone(&self.check_runner),
            crate::checks::CheckAuthority {
                data_root: self.data_root.clone(),
                backend: self.session.backend.clone(),
                task_id: self.task_controller.current()?.map(|task| {
                    task.entry
                        .as_ref()
                        .map(|entry| entry.id.as_str())
                        .unwrap_or(task.revision.as_str())
                        .to_owned()
                }),
                shell: self.session.shell,
                mode: request.mode,
                permission: self.session.approval_mode,
                permissions: Arc::clone(&self.permission_coordinator),
                scope: scope.clone(),
                authorization_sink: backend_event_sink.clone(),
                editing: Arc::clone(&self.observer.check_editing),
                setup: interaction.check_setup.is_some(),
                can_run: interaction.check_setup.is_none()
                    && self.agent_coordinator.active_count() == 0
                    && execution.is_none(),
                agent: Arc::clone(&self.agent_coordinator),
                cancelled: Arc::clone(&self.turn_cancellation.requested),
                sink: self.event_sink.clone(),
            },
        );
        if let Some(context) = &mut request.control_context {
            context.checks = Some(check_scope);
            if interaction.check_setup.is_some() && request.mode == PromptMode::Chat {
                context.resolved_question_digest_set.clear();
                context.has_active_elicitation = interaction.elicitation().is_some();
            }
        }
        let mut scan_interval = tokio::time::interval(std::time::Duration::from_secs(5));
        scan_interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
        scan_interval.tick().await;
        let mut stream_open = true;
        let mut checkpoint_failed = false;
        let mut pending_revision: Option<(
            tokio::sync::oneshot::Sender<Result<Value>>,
            crate::plan::revision_review::RevisionReviewWait,
        )> = None;
        let delivery_sink = self.event_sink.clone();
        let mut provider_task = tokio::task::JoinSet::new();
        // Provider polling starts at a task boundary instead of inside the host dispatch stack.
        provider_task.spawn(async move {
            if !backend.checkpoint_tool_gate()
                && let Some(gate) = request
                    .control_context
                    .as_ref()
                    .and_then(|context| context.checkpoint.as_ref())
            {
                gate.wait().await?;
            }
            let acceptance_sink = backend_event_sink.clone();
            let response = crate::backend::retry::prompt(
                backend.as_ref(),
                request,
                Some(backend_event_sink),
                &retry_trace,
                &submission_store,
            );
            if let Some(command) = continuation_command {
                command.complete_with(response, acceptance_sink).await
            } else {
                response.await
            }
        });
        // Event-loop state must not remain inline in the provider lifecycle future.
        let outcome = Box::pin(async {
            loop {
                tokio::select! {
                    biased;
                    () = cancellation.cancelled() => break Err(anyhow::Error::new(TurnCancelled)),
                    result = async {
                        match runtime.checkpoint.as_mut() {
                            Some(task) => (&mut task.0).await.context("checkpoint worker failed").and_then(|result|result),
                            None => std::future::pending().await,
                        }
                    } => {
                        if let Err(error) = self.finish_initial_checkpoint(&mut runtime, interaction, result) {
                            checkpoint_failed = true;
                            break Err(error);
                        }
                    }
                    input = parent_input.receive(), if user_delivery.is_empty() && deferred_input.is_empty() => {
                        let input = input.context("parent input receiver closed")?;
                        if self.backend.descriptor().capability.native_steer {
                            let backend = self.backend.clone();
                            let session_id = self.session.id.clone();
                            user_delivery.spawn(async move {
                                let result = tokio::time::timeout(std::time::Duration::from_secs(30),
                                    backend.steer_session(&session_id,input.input()))
                                    .await.context("parent input delivery exceeded 30 seconds").and_then(|result|result);
                                (input,result)
                            });
                        } else {deferred_input.push_back(input);}
                    }
                    delivery = user_delivery.join_next(), if !user_delivery.is_empty() => {
                        let (input,result) = delivery.context("parent input delivery disappeared")?.context("parent input delivery failed")?;
                        if let Some(input) = input.settle_delivery(result).await? { deferred_input.push_back(input); }
                    }
                    changed = agent_revision.changed() => {
                        changed.context("agent coordinator closed during root execution")?;
                        event.extend(self.drain_agent_commands(Some(interaction)).await?);
                        self.start_agent_result_delivery(attached_result,&mut attempted_result,&mut result_delivery)?;
                    }
                    delivery = result_delivery.join_next(), if !result_delivery.is_empty() => {
                        self.finish_agent_result_delivery(delivery.context("parent result delivery disappeared")?)?;
                    }
                    control = execution_receiver.recv() => {
                        if let Some(control) = control {
                            while let Ok(backend_event) = backend_event_stream.try_recv() {
                                self.process_backend_event(&mut runtime, interaction, backend_event, event).await?;
                            }
                            let submission = matches!(&control.operation, crate::plan::execution::ExecutionOperation::Submit { .. });
                            let result = self.handle_execution_control(&control.execution_id, control.generation, &control.task_permit, control.operation, interaction).await;
                            let committed = result.is_ok();
                            let waiting = submission && result.as_ref().is_ok_and(|response| response["status"] == "paused");
                            if waiting {
                                anyhow::ensure!(pending_revision.is_none(), "another revision review is already pending");
                                let wait = self.revision_review.open()?;
                                interaction.pause(self.clock.now_ms());
                                interaction.set_awaiting_input(true);
                                self.store.save_exchange(interaction)?;
                                pending_revision = Some((control.response, wait));
                            } else {
                                let _ = control.response.send(result);
                            }
                            if committed {
                                let patch = self.reconcile_timeline()?;
                                self.emit_timeline_patch(patch, event).await?;
                            }
                        }
                    }
                    review = async {
                        if let Some((_, wait)) = &mut pending_revision {
                            wait.receiver.recv().await
                        } else {
                            std::future::pending().await
                        }
                    } => {
                        use crate::plan::revision_review::RevisionReviewRequest;
                        match review.context("revision review request lane closed")? {
                            RevisionReviewRequest::Capture { plan_id, digest, revision, response } => {
                                if let Some(authority) = &turn_task_authority
                                    && let Err(error) = authority.validate() {
                                    let _ = response.send(Err(error));
                                    continue;
                                }
                                let result = if self.current_plan_id()?.as_deref() != Some(plan_id.as_str()) {
                                    Err(anyhow::anyhow!("review belongs to another plan"))
                                } else if let Some(revision) = revision {
                                    self.observer.capture_plan_revision(&plan_id, revision).and_then(|source| {
                                        anyhow::ensure!(crate::plan::digest(&serde_json::to_vec(&source.document)?) == digest,
                                            "plan revision changed before opening");
                                        Ok(source)
                                    })
                                } else { self.observer.capture_plan_review(&plan_id, &digest) };
                                let _ = response.send(result);
                            }
                            RevisionReviewRequest::Decide { method, params, response } => {
                                if let Some(authority) = &turn_task_authority
                                    && let Err(error) = authority.validate() {
                                    let _ = response.send(Err(error));
                                    continue;
                                }
                                let result = if params.get("plan_id").and_then(Value::as_str)
                                    .is_some_and(|id| self.current_plan_id().ok().flatten().as_deref() != Some(id)) {
                                    Err(anyhow::anyhow!("review decision belongs to another plan"))
                                } else {
                                    match method {
                                        RuntimeMethod::SessionPlanAcceptanceBegin => self.dispatch_session_command("session.plan.acceptance.begin", params),
                                        RuntimeMethod::SessionPlanChangesRequest => self.dispatch_session_command("session.plan.changes.request", params),
                                        RuntimeMethod::SessionPlanCancel => self.dispatch_session_command("session.plan.cancel", params),
                                        RuntimeMethod::SessionPlanEntityRename => self.rename_plan_entity(params),
                                        RuntimeMethod::SessionPlanTestDelete => self.delete_plan_tests(params),
                                        RuntimeMethod::SessionPlanCheckUpdate => Box::pin(self.update_plan_checks(params)).await,
                                        _ => Err(anyhow::anyhow!("unsupported revision review decision")),
                                    }
                                };
                                match result {
                                    Err(error) => { let _ = response.send(Err(error)); }
                                    Ok(result) if matches!(method, RuntimeMethod::SessionPlanEntityRename | RuntimeMethod::SessionPlanTestDelete | RuntimeMethod::SessionPlanCheckUpdate) => {
                                        let _ = response.send(Ok(result));
                                    }
                                    Ok((_, mut events)) => {
                                        interaction.set_awaiting_input(false);
                                        interaction.resume(self.clock.now_ms())?;
                                        self.store.save_exchange(interaction)?;
                                        let execution_id = self.current_execution()?.context("review has no execution")?.id;
                                        let provider_response = self.revision_review_response(&execution_id)?;
                                        events.retain(|event| event.event != "goal_continue_requested");
                                        let patch = self.reconcile_timeline()?;
                                        self.emit_timeline_patch(patch, event).await?;
                                        let snapshot = serde_json::to_value(self.snapshot()?)?;
                                        let (provider, wait) = pending_revision.take().context("revision review lost its provider wait")?;
                                        drop(wait);
                                        let _ = response.send(Ok((snapshot, events)));
                                        let _ = provider.send(Ok(provider_response));
                                    }
                                }
                            }
                        }
                    }
                    _ = scan_interval.tick(), if execution.is_some() && pending_revision.is_none() => {
                        if let Some(execution) = &execution {
                            match self.scan_execution(&execution.id).await {
                                Ok(progress) => {
                                    let patch = self.reconcile_live_interaction(Some(interaction), None)?;
                                    self.emit_timeline_patch(patch, event).await?;
                                    self.emit_session_update(SessionUpdate::PlanProgress { progress: serde_json::from_value(progress)? }, event).await?;
                                },
                                Err(error) => self.trace.record(&self.session.id, "plan.progress_failed", json!({"error":format!("{error:#}")})),
                            }
                        }
                    }
                    changed = cleanup.changed() => {
                        if changed.is_ok() {
                            let state = cleanup.borrow_and_update().clone();
                            if !matches!(state, ExecutionCleanup::Idle) {
                                if !cancellation.restart.load(Ordering::Acquire) {
                                    interaction.begin_finalization(crate::exchange::ExchangeOutcome::Cancelled, self.clock.now_ms())?;
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
                        self.start_agent_result_delivery(attached_result,&mut attempted_result,&mut result_delivery)?;
                    }
                    error = publication::failed(delivery_sink.as_ref()) => break Err(error.into()),
                    result = provider_task.join_next() => {
                        break result.context("provider task ended without an outcome")?
                            .context("provider task failed")?;
                    },
                }
            }
        }).await;
        provider_task.shutdown().await;
        let outcome = if !matches!(*cancellation.cleanup.borrow(), ExecutionCleanup::Idle) {
            Err(anyhow::Error::new(TurnCancelled))
        } else {
            outcome
        };
        if outcome.is_err()
            && let Some(gate) = &runtime.tool_gate
        {
            gate.complete(Err(
                "provider turn ended before checkpoint admission completed".into(),
            ))?;
        }
        let tools_blocked = runtime.tools_blocked();
        let cancelled_before_tools = outcome
            .as_ref()
            .is_err_and(|error| error.is::<TurnCancelled>())
            && tools_blocked;
        let outcome = if !cancelled_before_tools && let Some(task) = runtime.checkpoint.as_mut() {
            let cancelled = async {
                cancellation.cancelled().await;
                if tools_blocked {
                    return;
                }
                std::future::pending::<()>().await;
            };
            let result = tokio::select! {
                biased;
                () = cancelled => None,
                result = &mut task.0 => Some(result.context("checkpoint worker failed").and_then(|result|result)),
            };
            match result {
                Some(result) => {
                    let saved = self.finish_initial_checkpoint(&mut runtime, interaction, result);
                    checkpoint_failed |= saved.is_err();
                    saved.and(outcome)
                }
                None => Err(anyhow::Error::new(TurnCancelled)),
            }
        } else {
            outcome
        };
        while !result_delivery.is_empty() {
            tokio::select! {
                biased;
                _ = cancellation.cancelled() => {result_delivery.abort_all(); break;}
                delivery = result_delivery.join_next() => {
                    self.finish_agent_result_delivery(delivery.context("parent result delivery disappeared")?)?;
                }
            }
        }
        while !user_delivery.is_empty() {
            tokio::select! {
                biased;
                _ = cancellation.cancelled() => {user_delivery.abort_all(); break;}
                delivery = user_delivery.join_next() => {
                    let (input,result) = delivery.context("parent input delivery disappeared")?.context("parent input delivery failed")?;
                    if let Some(input) = input.settle_delivery(result).await? { deferred_input.push_back(input); }
                }
            }
        }
        drop(pending_revision.take());
        let mut outcome = if !matches!(*cancellation.cleanup.borrow(), ExecutionCleanup::Idle) {
            Err(anyhow::Error::new(TurnCancelled))
        } else {
            outcome
        };
        loop {
            match backend_event_stream.try_next() {
                Ok(Some(backend_event)) => {
                    if let Err(error) = self
                        .process_backend_event(&mut runtime, interaction, backend_event, event)
                        .await
                    {
                        outcome = Err(error);
                    }
                }
                Ok(None) => break,
                Err(error) => {
                    outcome = Err(error.into());
                    break;
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
                .cancel_owner(
                    &crate::backend::ExecutionOwner::root(&self.session.id),
                    None,
                )
                .await
        {
            outcome = Err(error).context("clear pending approval requests");
        }
        if outcome.as_ref().is_err_and(|error| {
            checkpoint_failed
                || error.downcast_ref::<TurnCancelled>().is_some()
                || error
                    .downcast_ref::<crate::backend::events::EventDeliveryError>()
                    .is_some()
        }) && let Err(error) = self.backend.cancel_session(&self.session.id).await
        {
            outcome = Err(error).context("stop cancelled backend transport");
        }
        self.exchange_runtime = Some(runtime);
        outcome
    }

    async fn retract_cancelled_interaction(
        &mut self,
        interaction: &Exchange,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        match self
            .backend
            .rollback_cancelled_turn_for(&self.session.id)
            .await
        {
            Ok(true) => {
                self.store.delete_exchange(&interaction.id)?;
                self.exchange_runtime.take();
                self.emit_live(publication::LiveUpdate::StateChanged, event)
                    .await?;
                Ok(true)
            }
            Ok(false) => Ok(false),
            Err(error) => {
                self.emit_live(
                    SessionUpdate::Diagnostic {
                        diagnostic: crate::backend::ProviderDiagnostic {
                            code: None,
                            message: format!("Cancelled turn could not be retracted: {error:#}"),
                            fatal: false,
                        },
                    },
                    event,
                )
                .await?;
                Ok(false)
            }
        }
    }

    /// Retry finalization without submitting provider input or replaying tool execution.
    async fn cancel_settled_execution(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut event = Vec::new();
        if self
            .current_goal()?
            .is_some_and(|goal| matches!(goal.state, GoalState::Active | GoalState::Paused))
        {
            let (_, goal_event) = self.dispatch_session_command("session.goal.pause", json!({}))?;
            event.extend(goal_event);
        }
        if let Some(mut exchange) = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Running)?
        {
            anyhow::ensure!(
                exchange.execution_started_at_ms().is_none()
                    && exchange
                        .turn
                        .iter()
                        .all(|turn| turn.state() != crate::turn::TurnState::Running),
                "execution must settle before idle cancellation"
            );
            exchange.begin_finalization(
                crate::exchange::ExchangeOutcome::Cancelled,
                self.clock.now_ms(),
            )?;
            self.store.save_exchange(&exchange)?;
        }
        let (result, finalization_event) = self.retry_finalization().await?;
        event.extend(finalization_event);
        Ok((result, event))
    }

    async fn retry_finalization(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let Some(mut exchange) = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Finalizing)?
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
            .finalization_outcome()
            .context("finalization outcome is missing")?;
        self.capture_final_checkpoint(&mut exchange, outcome)
            .await?;
        exchange.finish(outcome, self.clock.now_ms())?;
        self.store.save_exchange(&exchange)?;
        self.exchange_runtime = None;
        self.save_session()?;
        Ok((
            json!({"cancel_requested": true, "finalized_exchange_id": exchange.id}),
            vec![self.event(
                "exchange_complete",
                json!({"id":exchange.id,"state":exchange.state()}),
            )?],
        ))
    }

    async fn capture_final_checkpoint(
        &mut self,
        interaction: &mut Exchange,
        outcome: crate::exchange::ExchangeOutcome,
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
            if outcome == crate::exchange::ExchangeOutcome::Cancelled
                && let Some(runtime) = self.exchange_runtime.as_mut().filter(|runtime| {
                    runtime.exchange_id == interaction.id && runtime.tools_blocked()
                })
            {
                runtime
                    .tool_gate
                    .as_ref()
                    .expect("blocked tool gate")
                    .complete(Err(
                        "turn cancelled before checkpoint admission completed".into()
                    ))?;
                runtime.checkpoint.take();
                interaction.finalization_error = None;
                interaction.finish(outcome, self.clock.now_ms())?;
                return Ok(None);
            }
            self.collect_active_initial_checkpoint(interaction).await?;
            if self.checkpoint.is_none() {
                let mut changes = crate::exchange::ProviderDiffBuilder::default();
                self.visit_provider_exchanges(interaction, |exchange| changes.record(exchange))?;
                interaction.attributed_diff_text = changes.finish();
                interaction.attributed_matches_checkpoint = false;
                interaction.finish(outcome, self.clock.now_ms())?;
                return Ok(None);
            };
            let checkpoint = self
                .checkpoint
                .as_ref()
                .context("Git checkpoint adapter is missing")?
                .capture(
                    &self.store.objects,
                    &self.repositories,
                    &self.session.id,
                    self.clock.now_ms(),
                )
                .await?;
            let mut settled = interaction.clone();
            let before = if let Some(identity) = settled.checkpoint_before.as_deref() {
                self.store
                    .load_checkpoint(identity)?
                    .context("interaction before checkpoint is missing")?
            } else {
                anyhow::ensure!(
                    outcome != crate::exchange::ExchangeOutcome::Complete,
                    "completed interaction has no initial checkpoint"
                );
                // A failed initial barrier never released tools. Recovery preserves the current workspace.
                settled.checkpoint_before = Some(checkpoint.id.clone());
                checkpoint.clone()
            };
            self.populate_interaction_change_diffs(&mut settled, &before, &checkpoint)
                .await?;
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
        self.visit_provider_exchanges(interaction, |exchange| index.record(exchange))?;
        Ok(index)
    }

    fn visit_provider_exchanges(
        &self,
        interaction: &Exchange,
        mut visit: impl FnMut(&Exchange),
    ) -> Result<()> {
        visit(interaction);
        let mut pending = referenced_child_exchange_list(interaction);
        let mut visited = HashSet::new();
        visited.insert(interaction.id.clone());
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
                visit(&record);
                pending.extend(referenced_child_exchange_list(&record));
            }
        }
        Ok(())
    }

    fn process_backend_event<'event>(
        &'event mut self,
        runtime: &'event mut ExchangeRuntime,
        interaction: &'event mut Exchange,
        backend_event: BackendEvent,
        event: &'event mut Vec<SessionEvent>,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send + 'event>> {
        let scope = backend_event.scope();
        if scope.owner() != &crate::backend::ExecutionOwner::root(&self.session.id) {
            self.trace.record(
                &self.session.id,
                "provider.event.rejected",
                json!({"code":"foreign_execution_owner","scope":scope}),
            );
            return Box::pin(async { Ok(()) });
        }
        if scope.invocation().is_some_and(|scope| {
            scope.exchange_id != interaction.id
                && !interaction
                    .turn
                    .iter()
                    .any(|turn| &turn.provider().scope == scope)
        }) {
            return Box::pin(async { Ok(()) });
        }
        match backend_event {
            BackendEvent::DirectoryAccess { grant, .. } => Box::pin(async move {
                for grant in grant {
                    let directories = match grant.operation {
                        crate::permissions::directory::DirectoryOperation::Read => {
                            &mut self.session.access.readable_directory
                        }
                        crate::permissions::directory::DirectoryOperation::Write => {
                            &mut self.session.access.writable_directory
                        }
                    };
                    if !directories.contains(&grant.path) {
                        directories.push(grant.path);
                    }
                }
                self.save_session()?;
                self.session.active_task = self.task_controller.current()?;
                self.emit_session_update(
                    SessionUpdate::ExecutionState {
                        session: Box::new(self.session.clone()),
                    },
                    event,
                )
                .await
            }),
            BackendEvent::Runtime {
                update: crate::backend::RuntimeUpdate::Resolved { runtime: resolved },
                ..
            } => Box::pin(async move {
                if !resolved.provider.is_empty() {
                    self.session.provider_label = resolved.provider;
                }
                if let Some(model) = resolved.model.filter(|model| !model.is_empty()) {
                    self.session.resolved_model = Some(model);
                }
                self.save_session()?;
                self.emit_session_update(
                    SessionUpdate::Runtime {
                        session_id: self.session.id.clone(),
                        runtime: crate::backend::BackendRuntime {
                            provider: self.session.provider_label.clone(),
                            model: self.session.resolved_model.clone(),
                        },
                    },
                    event,
                )
                .await
            }),
            BackendEvent::Compaction { scope, update } => Box::pin(async move {
                let observation = BackendEvent::Compaction {
                    scope,
                    update: update.clone(),
                };
                self.process_compaction_event(runtime, interaction, observation, *update, event)
                    .await
            }),
            BackendEvent::Approval {
                scope,
                update: crate::backend::ApprovalUpdate::Requested(request),
            } => Box::pin(async move {
                if !self
                    .permission_coordinator
                    .pending_list()?
                    .iter()
                    .any(|approval| approval.id == request.id)
                {
                    return Ok(());
                }
                let turn = interaction
                    .turn
                    .iter()
                    .rev()
                    .find(|turn| turn.state() == crate::turn::TurnState::Running);
                let Some(approval) = self.permission_coordinator.bind_owner(
                    &request.id,
                    &interaction.id,
                    turn.map(|turn| turn.id()),
                )?
                else {
                    return Ok(());
                };
                interaction.observe_blocker(
                    format!("approval:{}", approval.id),
                    true,
                    self.clock.now_ms(),
                );
                self.store.save_exchange(interaction)?;
                self.emit_live(
                    SessionUpdate::Approval {
                        update: Box::new(crate::backend::ApprovalUpdate::Requested(approval)),
                    },
                    event,
                )
                .await?;
                let _ = scope;
                Ok(())
            }),
            BackendEvent::Approval { update, .. } => Box::pin(async move {
                let approval = match &update {
                    crate::backend::ApprovalUpdate::Resolved(approval)
                    | crate::backend::ApprovalUpdate::Cancelled(approval) => approval,
                    crate::backend::ApprovalUpdate::Requested(_) => unreachable!(),
                };
                interaction.observe_blocker(
                    format!("approval:{}", approval.id),
                    false,
                    self.clock.now_ms(),
                );
                interaction.approval_resolution.push(approval.clone());
                self.store.save_exchange(interaction)?;
                self.emit_live(
                    SessionUpdate::Approval {
                        update: Box::new(update),
                    },
                    event,
                )
                .await
            }),
            BackendEvent::ParentBoundary { update, .. } => Box::pin(async move {
                let now_ms = self.clock.now_ms();
                match update {
                    crate::backend::ParentBoundaryUpdate::WaitStarted { agent_count } => {
                        interaction.observe_blocker("agent_wait".into(), true, now_ms);
                        interaction.close_running_messages();
                        runtime.active_wait = Some(ActiveWait {
                            exchange_id: interaction.id.clone(),
                            started_at_ms: now_ms,
                            agent_count,
                        });
                    }
                    crate::backend::ParentBoundaryUpdate::WaitUpdated { agent_count } => {
                        if let Some(wait) = runtime.active_wait.as_mut() {
                            wait.agent_count = agent_count;
                        }
                    }
                    crate::backend::ParentBoundaryUpdate::WaitFinished => {
                        interaction.observe_blocker("agent_wait".into(), false, now_ms);
                        runtime.active_wait = None;
                    }
                }
                self.store.save_exchange(interaction)?;
                self.emit_active_wait(interaction, runtime, event).await
            }),
            BackendEvent::Agent {
                update: crate::backend::AgentUpdate::Lifecycle { event: lifecycle },
                ..
            } => Box::pin(async move {
                self.apply_agent_lifecycle(&lifecycle, Some(&interaction.id), event)
                    .await?;
                Ok(())
            }),
            BackendEvent::Agent { .. } => Box::pin(async { Ok(()) }),
            BackendEvent::Diagnostic { diagnostic, .. } => Box::pin(async move {
                self.emit_session_update(SessionUpdate::Diagnostic { diagnostic }, event)
                    .await
            }),
            content => self.process_exchange_content(runtime, interaction, content, event),
        }
    }

    fn process_exchange_content<'event>(
        &'event mut self,
        runtime: &'event mut ExchangeRuntime,
        interaction: &'event mut Exchange,
        backend_event: BackendEvent,
        event: &'event mut Vec<SessionEvent>,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send + 'event>> {
        Box::pin(async move {
            self.prepare_backend_observation(interaction, &backend_event);
            if backend_event.turn_address().is_some_and(|address| {
                self.diagnostic_native_thread.contains(&address.thread_id)
                    || self
                        .agent_registry
                        .get_by_thread(&address.thread_id)
                        .is_some_and(|agent| !agent.is_primary())
            }) {
                return Ok(());
            }
            if self
                .route_settled_parent_event(interaction, &backend_event, event)
                .await?
            {
                return Ok(());
            }
            self.admit_native_goal_continuation(runtime, interaction, &backend_event, event)
                .await?;
            if !interaction.observe_turn(&backend_event, self.clock.now_ms())? {
                self.trace.record(&self.session.id, "provider.event.rejected",
                    json!({"exchange_id":interaction.id,"code":"execution_unknown_or_settled","event":backend_event}));
                return Ok(());
            }
            match &backend_event {
                BackendEvent::Turn { address, update } => {
                    if matches!(update, crate::backend::TurnUpdate::Started { .. })
                        && self.session.backend_session_id.as_deref()
                            != Some(address.thread_id.as_str())
                    {
                        self.session.backend_session_id = Some(address.thread_id.clone());
                        self.save_session()?;
                    }
                    self.store.save_exchange(interaction)?;
                    self.emit_observation(&backend_event, event).await?;
                    return Ok(());
                }
                BackendEvent::InputAccepted {
                    acknowledgement, ..
                } => {
                    runtime.retraction_eligible = false;
                    let now_ms = self.clock.now_ms();
                    match acknowledgement {
                        crate::backend::InputAcknowledgement::TaskStarted {
                            input_id,
                            start,
                            prompt,
                        } => {
                            let input_id = input_id
                                .as_ref()
                                .context("task entry is missing its input identity")?;
                            let id = format!("{}:task-start:{input_id}", self.session.id);
                            if let Some(existing) =
                                self.store.load_session_event(&self.session.id, &id)?
                            {
                                anyhow::ensure!(
                                    matches!(existing.detail,
                                    SessionEventKind::TaskStarted { start: previous, prompt: previous_prompt }
                                    if previous == *start && previous_prompt == *prompt),
                                    "task entry identity was reused for different content"
                                );
                                return Ok(());
                            }
                            interaction.observe_blocker("agent_wait".into(), false, now_ms);
                            runtime.active_wait = None;
                            self.store.save_exchange(interaction)?;
                            let task_event = crate::timeline::SessionEventRecord {
                                id,
                                session_id: self.session.id.clone(),
                                owner: self
                                    .store
                                    .capture_timeline_owner(&self.session.id, &interaction.id)?,
                                created_at_ms: now_ms,
                                detail: SessionEventKind::TaskStarted {
                                    start: start.clone(),
                                    prompt: prompt.clone(),
                                },
                            };
                            self.store.save_session_event(&task_event)?;
                            let mut displayed = interaction.clone();
                            crate::timeline::ownership::insert_event(
                                &mut displayed,
                                crate::timeline::TimelineEvent::Session {
                                    event: Box::new(task_event),
                                },
                            )?;
                            self.emit_live_interaction(
                                SessionUpdate::Timeline {
                                    update: TimelineUpdate::Node { prompt: None },
                                },
                                &displayed,
                                event,
                            )
                            .await?;
                        }
                        crate::backend::InputAcknowledgement::Steering { input_id, text } => {
                            if input_id.as_ref().is_some_and(|id| interaction.node_list.iter().any(|node|
                                matches!(node, ExchangeNode::ExchangeInput { prompt } if prompt.id == *id))) {
                                return Ok(());
                            }
                            interaction.observe_blocker("agent_wait".into(), false, now_ms);
                            runtime.active_wait = None;
                            interaction.append_identified_input(
                                input_id.clone(),
                                crate::exchange::InputIntent::Steering,
                                text.clone(),
                                now_ms,
                            )?;
                            self.store.save_exchange(interaction)?;
                            let prompt = interaction.node_list.last().and_then(|node| match node {
                                ExchangeNode::ExchangeInput { prompt } => Some(prompt.clone()),
                                _ => None,
                            });
                            self.emit_live_interaction(
                                SessionUpdate::Timeline {
                                    update: TimelineUpdate::Node { prompt },
                                },
                                interaction,
                                event,
                            )
                            .await?;
                        }
                    }
                    self.emit_active_wait(interaction, runtime, event).await?;
                    return Ok(());
                }
                BackendEvent::Runtime {
                    update: crate::backend::RuntimeUpdate::ContextUsage { usage },
                    ..
                } => {
                    let thread_id = self.session.backend_session_id.as_deref();
                    if let Some(thread_id) = thread_id
                        && interaction.observe_compaction_usage(thread_id, usage.used) {
                        self.store.save_exchange(interaction)?;
                        let patch = self.reconcile_timeline()?;
                        self.emit_timeline_patch(patch, event).await?;
                    } else if let Some(thread_id) = thread_id
                        && !interaction.node_list.iter().any(|node| matches!(node,
                            ExchangeNode::Compaction { compaction, .. } if compaction.thread_id == thread_id))
                        && let Some(mut record) = self.store.list_session_event(&self.session.id)?.into_iter().rev()
                            .find(|record| matches!(&record.detail, SessionEventKind::Compaction { compaction }
                                if compaction.thread_id == thread_id))
                        && let SessionEventKind::Compaction { compaction } = &mut record.detail
                        && compaction.state == crate::backend::CompactionState::Completed
                        && compaction.result.context_tokens_before.is_some()
                        && compaction.result.context_tokens_after.is_none()
                        && !self.store.list_exchange_metadata(&self.session.id)?.iter().any(|exchange|
                            exchange.node_list.iter().any(|node| matches!(node,
                                ExchangeNode::Compaction { compaction, .. }
                                    if compaction.thread_id == thread_id
                                        && compaction.started_at_ms.unwrap_or(exchange.created_at_ms) >= record.created_at_ms)))
                        && compaction.observe_context_usage(usage.used) {
                        self.store.save_session_event(&record)?;
                        let patch = self.reconcile_timeline()?;
                        self.emit_timeline_patch(patch, event).await?;
                    }
                    self.session.context_usage = Some(usage.clone());
                    self.context_usage_fresh = true;
                    self.emit_observation(&backend_event, event).await?;
                    return Ok(());
                }
                BackendEvent::Checklist { update, .. } => {
                    runtime.retraction_eligible = false;
                    runtime.checklist.replace(update);
                    interaction.checklist = Some(runtime.checklist.snapshot().clone());
                    self.store.save_exchange(interaction)?;
                    self.emit_live_interaction(
                        publication::LiveUpdate::StateChanged,
                        interaction,
                        event,
                    )
                    .await?;
                }
                BackendEvent::Tool { .. } => runtime.retraction_eligible = false,
                BackendEvent::Message { update, .. }
                    if update.text().is_some_and(|text| !text.is_empty()) =>
                {
                    runtime.retraction_eligible = false
                }
                _ => {}
            }
            if backend_event.turn_address().is_some() {
                attribute_provider_tool(interaction, &mut runtime.checklist, &backend_event)?;
                if backend_event.tool_output_delta().is_some() {
                    self.store
                        .save_tool_output_delta(interaction, &backend_event)?;
                } else {
                    self.store.save_exchange(interaction)?;
                }
                self.emit_live_interaction(backend_event, interaction, event)
                    .await?;
            } else {
                self.emit_observation(&backend_event, event).await?;
            }
            Ok(())
        })
    }

    fn prepare_backend_observation(
        &mut self,
        interaction: &mut Exchange,
        backend_event: &BackendEvent,
    ) {
        if !self.capability.compaction.background
            && let BackendEvent::Turn {
                address,
                update: crate::backend::TurnUpdate::Finished { .. },
            } = backend_event
        {
            let now_ms = backend_event.observed_at_ms(self.clock.now_ms());
            interaction.unconfirm_compaction(&address.thread_id, now_ms);
            for child in self.child_exchange_runtime_by_agent.values_mut() {
                child
                    .exchange
                    .unconfirm_compaction(&address.thread_id, now_ms);
            }
        }
    }

    async fn process_compaction_event(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        backend_event: BackendEvent,
        mut compaction: crate::backend::Compaction,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        if compaction.state == crate::backend::CompactionState::Running
            && self.session.backend_session_id.as_deref() == Some(compaction.thread_id.as_str())
        {
            compaction.result.context_tokens_before = compaction
                .result
                .context_tokens_before
                .or_else(|| self.session.context_usage.as_ref().map(|usage| usage.used));
        }
        let now_ms = backend_event.observed_at_ms(self.clock.now_ms());
        let previous_owner = self.compaction_exchange_owner(&compaction)?;
        let owns_other_exchange = previous_owner.as_ref().is_some_and(|owner| {
            owner.id != interaction.id
                && !self
                    .child_exchange_runtime_by_agent
                    .values()
                    .any(|child| child.exchange.id == owner.id)
        });
        let owns_session_event =
            self.store
                .list_session_event(&self.session.id)?
                .iter()
                .any(|record| {
                    matches!(&record.detail, SessionEventKind::Compaction { compaction: existing }
                if existing.id == compaction.id && existing.thread_id == compaction.thread_id)
                });
        if owns_other_exchange || owns_session_event {
            self.retain_external_compaction(compaction, now_ms)?;
            let patch = self.reconcile_timeline()?;
            self.emit_timeline_patch(patch, event).await?;
            return Ok(());
        }
        let child_id = self
            .agent_registry
            .get_by_thread(&compaction.thread_id)
            .map(|agent| agent.id.clone())
            .filter(|id| self.child_exchange_runtime_by_agent.contains_key(id))
            .or_else(|| {
                self.child_exchange_runtime_by_agent
                    .iter()
                    .find(|(_, child)| {
                        child
                            .exchange
                            .turn
                            .iter()
                            .any(|turn| turn.provider().thread_id == compaction.thread_id)
                    })
                    .map(|(id, _)| id.clone())
            });
        if let Some(child_id) = child_id {
            let mut child = self
                .child_exchange_runtime_by_agent
                .remove(&child_id)
                .context("compaction owner disappeared")?;
            child.exchange.observe_compaction(compaction, now_ms);
            let result = async {
                self.store.save_exchange(&child.exchange)?;
                self.emit_live_interaction(backend_event, &child.exchange, event)
                    .await
            }
            .await;
            self.child_exchange_runtime_by_agent.insert(child_id, child);
            result?;
        } else if self.session.backend_session_id.as_deref() == Some(compaction.thread_id.as_str())
            || interaction
                .turn
                .iter()
                .any(|turn| turn.provider().thread_id == compaction.thread_id)
        {
            runtime.retraction_eligible = false;
            interaction.observe_compaction(compaction, now_ms);
            self.store.save_exchange(interaction)?;
            self.emit_live_interaction(backend_event, interaction, event)
                .await?;
        }
        Ok(())
    }

    async fn route_settled_parent_event(
        &mut self,
        current: &Exchange,
        backend_event: &BackendEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        let Some(address) = backend_event.turn_address() else {
            return Ok(false);
        };
        if current.turn.iter().any(|turn| turn.provider() == address)
            || !matches!(
                backend_event,
                BackendEvent::Usage { .. } | BackendEvent::Turn { .. } | BackendEvent::Tool { .. }
            )
        {
            return Ok(false);
        }
        let Some(mut owner) = self
            .store
            .load_provider_exchange(&self.session.id, address)?
        else {
            return Ok(false);
        };
        if matches!(
            backend_event,
            BackendEvent::Usage { .. }
                | BackendEvent::Tool {
                    update: crate::backend::ToolUpdate::Output { .. }
                        | crate::backend::ToolUpdate::Changes { .. }
                        | crate::backend::ToolUpdate::SkillRead { .. },
                    ..
                }
        ) {
            owner.observe_turn(backend_event, self.clock.now_ms())?;
            self.store.save_exchange(&owner)?;
            let status = self.live_interaction_status(Some(current))?;
            let patch = self
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .update_live(
                    Some(&owner),
                    None,
                    status,
                    self.observer.status_thoughts.load(Ordering::Relaxed),
                )?;
            self.emit_observation(backend_event, event).await?;
            self.emit_timeline_patch(patch, event).await?;
        }
        Ok(true)
    }

    async fn admit_native_goal_continuation(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        observation: &BackendEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let BackendEvent::Turn {
            address,
            update: crate::backend::TurnUpdate::Started { .. },
        } = observation
        else {
            return Ok(());
        };
        let Some(previous) = interaction.turn.last() else {
            return Ok(());
        };
        if previous.provider().thread_id != address.thread_id
            || interaction.turn.iter().any(|turn| {
                turn.provider() == address || turn.state() == crate::turn::TurnState::Running
            })
            || runtime.task_permit.as_ref().is_none_or(|permit| {
                permit
                    .validate(self.task_controller.current().ok().flatten().as_ref())
                    .is_err()
            })
            || !self
                .current_goal()?
                .is_some_and(|goal| goal.native && goal.state == GoalState::Active)
        {
            return Ok(());
        }
        let crate::turn::TurnState::Finished { outcome } = previous.state() else {
            return Ok(());
        };
        let outcome = match outcome {
            crate::turn::TurnOutcome::Completed => crate::exchange::ExchangeOutcome::Complete,
            crate::turn::TurnOutcome::Cancelled => crate::exchange::ExchangeOutcome::Cancelled,
            crate::turn::TurnOutcome::Interrupted => crate::exchange::ExchangeOutcome::Interrupted,
            crate::turn::TurnOutcome::Failed => crate::exchange::ExchangeOutcome::Failed,
        };
        self.collect_initial_checkpoint(runtime, interaction)
            .await?;
        self.capture_final_checkpoint(interaction, outcome).await?;
        let (mut continuation, _, boundary) = self
            .interaction_for_turn("", true, self.clock.now_ms())
            .await?;
        event.extend(boundary);
        continuation.origin = crate::exchange::ExchangeOrigin::task_continuation(
            "The native goal requires another turn",
        );
        continuation.checklist.clone_from(&interaction.checklist);
        self.start_exchange_runtime(&mut continuation, self.clock.now_ms())
            .await?;
        let mut continuation_runtime = self
            .exchange_runtime
            .take()
            .context("continuation runtime is missing")?;
        self.collect_initial_checkpoint(&mut continuation_runtime, &mut continuation)
            .await?;
        continuation.inherit_native_turn(
            address.clone(),
            observation.observed_at_ms(self.clock.now_ms()),
        )?;
        *runtime = continuation_runtime;
        runtime.retraction_eligible = false;
        self.store.save_exchange(&continuation)?;
        *interaction = continuation;
        self.emit_live_interaction(publication::LiveUpdate::StateChanged, interaction, event)
            .await
    }

    async fn emit_active_wait(
        &mut self,
        interaction: &Exchange,
        runtime: &ExchangeRuntime,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        self.active_wait_projection = runtime.active_wait.clone();
        self.emit_live_interaction(
            SessionUpdate::Timeline {
                update: TimelineUpdate::Wait {
                    exchange_id: interaction.id.clone(),
                    wait: runtime.active_wait.clone(),
                },
            },
            interaction,
            event,
        )
        .await
    }

    async fn emit_live(
        &mut self,
        update: impl Into<publication::LiveUpdate>,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        self.emit_publication(update.into(), event).await?;
        let timeline_patch = self.reconcile_timeline()?;
        self.emit_timeline_patch(timeline_patch, event).await
    }

    async fn emit_live_interaction(
        &mut self,
        update: impl Into<publication::LiveUpdate>,
        interaction: &Exchange,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let update = update.into();
        let incremental = if let publication::LiveUpdate::Observation(observation) = &update {
            if observation.tool_output_delta().is_some() {
                self.presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .append_tool_output(interaction, observation)?
            } else if observation.message_delta().is_some() {
                self.presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .update_message(interaction, observation)?
            } else {
                None
            }
        } else {
            None
        };
        self.emit_publication(update, event).await?;
        let timeline_patch = match incremental {
            Some(patch) => patch,
            None => self.reconcile_live_interaction(Some(interaction), None)?,
        };
        self.emit_timeline_patch(timeline_patch, event).await
    }

    async fn emit_publication(
        &mut self,
        update: publication::LiveUpdate,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        match update {
            publication::LiveUpdate::StateChanged => Ok(()),
            publication::LiveUpdate::Observation(observation) => {
                self.emit_observation(&observation, event).await
            }
            publication::LiveUpdate::Notification(update) => {
                self.emit_session_update(*update, event).await
            }
        }
    }

    async fn emit_observation(
        &mut self,
        observation: &BackendEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        if let Some(update) = SessionUpdate::from_observation(observation) {
            self.emit_session_update(update, event).await?;
        }
        Ok(())
    }

    async fn emit_session_update(
        &mut self,
        update: SessionUpdate,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        if update.wakes_input()
            && let Some(wakeup) = &self.input_wakeup
        {
            wakeup.notify_one();
        }
        let payload = publication::encode_session_update(&update)?;
        self.trace
            .record(&self.session.id, "lua.event.emitted", payload.clone());
        if let Some(event_sink) = self.event_sink.as_ref() {
            if let Err(error) = event_sink.send_wait(update).await {
                self.trace.record(
                    &self.session.id,
                    "presentation.delivery_failed",
                    json!({"error":error.to_string()}),
                );
                self.event_sink = None;
            }
        } else {
            event.push(self.event("backend_event", payload)?);
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
        if let Some(event_sink) = self.event_sink.as_ref() {
            let delivery = event_sink
                .send_wait(SessionUpdate::DocumentChanged {
                    patch: timeline_patch.clone(),
                })
                .await;
            if let Err(error) = delivery {
                self.trace.record(
                    &self.session.id,
                    "presentation.delivery_failed",
                    json!({"error":error.to_string()}),
                );
                self.event_sink = None;
            }
        } else {
            event.push(self.event("document_changed", timeline_patch.notification())?);
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
            .reconcile_with_status_thoughts(
                timeline,
                self.observer.status_thoughts.load(Ordering::Relaxed),
            )
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
        let status = self.live_interaction_status(interaction)?;
        self.presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .update_live(
                interaction,
                removed_exchange_id,
                status,
                self.observer.status_thoughts.load(Ordering::Relaxed),
            )
    }

    fn live_interaction_status(
        &self,
        interaction: Option<&Exchange>,
    ) -> Result<crate::session::state_machine::SessionPhase> {
        let runtime = self.store.session_admission(&self.session.id)?.runtime()?;
        let mut status = crate::session::state_machine::SessionPhase::resolve_status(
            runtime.plan.as_ref(),
            self.active_wait_projection.as_ref(),
            interaction,
        );
        if status == crate::session::state_machine::SessionPhase::Idle
            && self.agent_coordinator.active_count() > 0
        {
            status = crate::session::state_machine::SessionPhase::WaitingForAgent {
                agent_count: self.agent_coordinator.active_count(),
            };
        }
        if let Some(task) = &runtime.active_task {
            match &task.workflow {
                crate::task::TaskWorkflow::Execute { execution } => {
                    crate::session::state_machine::ExecutionStatusSnapshot::from(&execution.record)
                        .apply(&mut status)
                }
                _ if task.status() == crate::task::TaskStatus::Paused => {
                    status = crate::session::state_machine::SessionPhase::Paused
                }
                _ => {}
            }
        }
        Ok(status)
    }

    async fn interaction_for_turn(
        &mut self,
        text: &str,
        admit_exchange: bool,
        now_ms: i64,
    ) -> Result<(Exchange, bool, Vec<SessionEvent>)> {
        let current = self.store.current_exchange_state(&self.session.id)?;
        if !admit_exchange {
            let mut interaction = self
                .store
                .latest_exchange(&self.session.id)?
                .context("continuation has no originating exchange")?;
            interaction.resume(now_ms)?;
            return Ok((interaction, false, Vec::new()));
        }
        if current.latest.is_none() && self.session.name.trim().is_empty() {
            let words = text.split_whitespace().collect::<Vec<_>>();
            let title = if words.first().is_some_and(|word| word.starts_with('/')) {
                words[1..].join(" ")
            } else {
                words.join(" ")
            };
            if !title.is_empty() {
                self.session.name = self
                    .store
                    .initialize_session_name(
                        &self.session.id,
                        &self.client_id,
                        &title.chars().take(30).collect::<String>(),
                    )?
                    .name;
                self.save_session()?;
            }
        }
        let mut event = Vec::new();
        if let Some(previous) = current.latest.as_ref()
            && previous.state == ExchangeState::Running
        {
            let mut previous = self
                .store
                .load_exchange(&previous.id)?
                .context("previous running exchange disappeared")?;
            let outcome = if previous
                .turn
                .iter()
                .any(|turn| turn.state() == crate::turn::TurnState::Running)
            {
                crate::exchange::ExchangeOutcome::Interrupted
            } else {
                crate::exchange::ExchangeOutcome::Complete
            };
            if previous.checkpoint_before.is_some() {
                previous.begin_finalization(outcome, now_ms)?;
            } else {
                previous.finish(outcome, now_ms)?;
            }
            self.store.save_exchange(&previous)?;
            self.emit_live_interaction(
                publication::LiveUpdate::StateChanged,
                &previous,
                &mut event,
            )
            .await?;
        }
        Ok((
            Exchange {
                finalization_error: None,
                check_owner: None,
                output_store: None,
                archived_output: Default::default(),
                input_message_id: None,
                provider_retry: None,
                agent_id: self.session.primary_agent_id.clone(),
                id: Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                ordinal: self.store.next_exchange_ordinal(&self.session.id)?,
                prompt: text.to_owned(),
                origin: crate::exchange::ExchangeOrigin::User,
                approval_mode: Some(self.session.approval_mode),
                phase: crate::exchange::ExchangePhase::Running,
                checkpoint_before: None,
                checkpoint_after: None,
                attributed_diff_text: None,
                checkpoint_diff_text: None,
                attributed_matches_checkpoint: false,
                approval_resolution: Vec::new(),
                check_setup: None,
                turn: Vec::new(),
                created_at_ms: now_ms,
                tool_counter: Default::default(),
                entry: Vec::new(),
                node_list: Vec::new(),
                metrics: crate::exchange::ExchangeMetrics::default(),
                comment: Vec::new(),
                checklist: None,

                question: crate::exchange::ExchangeQuestion::None,
                clock: crate::exchange::ExchangeClock::Running {
                    elapsed_ms: 0,
                    started_at_ms: now_ms,
                },
            },
            true,
            event,
        ))
    }

    fn delete_plan_tests(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let trace = crate::plan::edit_trace::PlanEditTrace::new(
            Arc::clone(&self.trace),
            &self.session.id,
            "session.plan.test.delete",
            &params,
        );
        let mutation = trace.stage("delete.total");
        let plan_id = required_text(&params, "plan_id")?;
        let expected_digest = required_text(&params, "digest")?;
        let expected_version = params
            .get("expected_version")
            .and_then(Value::as_u64)
            .context("test deletion requires the reviewed plan version")?;
        let selected: Vec<crate::plan::DesignTestSelection> = serde_json::from_value(
            params
                .get("tests")
                .cloned()
                .context("test selection is missing")?,
        )?;
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("plan record is missing")?;
        anyhow::ensure!(
            plan.session_id == self.session.id,
            "plan belongs to another session"
        );
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview,
            "only plans awaiting review can have tests deleted"
        );
        anyhow::ensure!(
            plan.review_digest.as_deref() == Some(&expected_digest),
            "plan changed before test deletion"
        );
        let (document, rendered, checksum) = self.plan_file.remove_tests(
            &self.session.id,
            &plan.id,
            expected_version,
            &expected_digest,
            &selected,
            Some(&trace),
        )?;
        let records = trace.stage("delete.persist_records");
        plan.document_version = document.version;
        plan.review_digest = Some(checksum);
        plan.acceptance = None;
        let change = format!(
            "Removed planned tests: {}",
            selected
                .iter()
                .map(|test| format!("{}::{}", test.file, test.name))
                .collect::<Vec<_>>()
                .join(", ")
        );
        plan.manual_changes.push(change);
        plan.updated_at_ms = self.clock.now_ms();
        self.publish_plan(&plan)?;

        self.save_session()?;
        records.complete(json!({"count":selected.len(), "version":document.version}));
        let payload = json!({ "plan": plan, "content": rendered.markdown, "document": document });
        mutation.complete(json!({"version":document.version}));
        Ok((
            payload.clone(),
            vec![self.event("plan_tests_deleted", payload)?],
        ))
    }

    async fn update_plan_checks(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let trace = crate::plan::edit_trace::PlanEditTrace::new(
            Arc::clone(&self.trace),
            &self.session.id,
            "session.plan.check.update",
            &params,
        );
        let plan_id = required_text(&params, "plan_id")?;
        let expected_digest = required_text(&params, "digest")?;
        let expected_version = params
            .get("expected_version")
            .and_then(Value::as_u64)
            .context("check selection requires the reviewed plan version")?;
        let changes: std::collections::BTreeMap<String, bool> = serde_json::from_value(
            params
                .get("enabled")
                .cloned()
                .context("check selection is missing")?,
        )?;
        let tests: Vec<crate::plan::DesignTestSelection> =
            serde_json::from_value(params.get("tests").cloned().unwrap_or_else(|| json!([])))?;
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("plan record is missing")?;
        anyhow::ensure!(
            plan.session_id == self.session.id,
            "plan belongs to another session"
        );
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview,
            "only plans awaiting review can change checks"
        );
        anyhow::ensure!(
            plan.review_digest.as_deref() == Some(&expected_digest),
            "plan changed before check selection"
        );
        let (document, rendered, checksum) = self
            .plan_file
            .update_checks(
                &self.session.id,
                &plan.id,
                expected_version,
                &expected_digest,
                crate::plan::PlanCheckSelection {
                    enabled: &changes,
                    removed_tests: &tests,
                },
                Some(&trace),
            )
            .await?;
        plan.document_version = document.version;
        plan.review_digest = Some(checksum);
        plan.acceptance = None;
        plan.manual_changes.push(format!(
            "Updated plan checks: {}",
            changes
                .iter()
                .map(|(id, enabled)| format!(
                    "{id} {}",
                    if *enabled { "enabled" } else { "disabled" }
                ))
                .collect::<Vec<_>>()
                .join(", ")
        ));
        plan.updated_at_ms = self.clock.now_ms();
        self.publish_plan(&plan)?;
        self.save_session()?;
        let payload = json!({ "plan": plan, "content": rendered.markdown, "document": document });
        Ok((
            payload.clone(),
            vec![self.event("plan_checks_updated", payload)?],
        ))
    }

    fn rename_plan_entity(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let trace = crate::plan::edit_trace::PlanEditTrace::new(
            Arc::clone(&self.trace),
            &self.session.id,
            "session.plan.entity.rename",
            &params,
        );
        let mutation = trace.stage("rename.total");
        let preflight = trace.stage("rename.preflight");
        let plan_id = required_text(&params, "plan_id")?;
        let symbol = params.get("symbol").and_then(Value::as_str);
        let entity_name = params
            .get("entity_name")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_owned();
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
        if let Some(expected) = params.get("digest").and_then(Value::as_str) {
            anyhow::ensure!(
                plan.review_digest.as_deref() == Some(expected),
                "plan changed before rename"
            );
            anyhow::ensure!(
                crate::plan::digest(&serde_json::to_vec(&document)?) == expected,
                "working plan changed before rename"
            );
        }
        preflight.complete(json!({"version":document.version}));
        let (previous_name, document, rendered, digest) = if let Some(identity) = symbol {
            anyhow::ensure!(
                params.get("expected_version").and_then(Value::as_u64) == Some(document.version),
                "rename requires the current plan version"
            );
            self.plan_file.rename_symbol(
                &self.session.id,
                &plan.id,
                document.version,
                identity,
                &new_name,
                Some(&trace),
            )?
        } else {
            let previous = document
                .entity_changes
                .iter()
                .find(|entity| entity.name == entity_name)
                .map(|entity| entity.name.clone())
                .with_context(|| format!("program entity `{entity_name}` does not exist"))?;
            let result = self.plan_file.rename_added_entity(
                &self.session.id,
                &plan.id,
                &entity_name,
                new_name.clone(),
            )?;
            let rendered = self.plan_file.render_review(&result.document)?;
            let (document, rendered, digest) = self.plan_file.persist_review_draft(
                &self.session.id,
                crate::plan::PlanReviewRevision::new(&plan.id, result.document, rendered)?,
                Some(&trace),
            )?;
            (previous, document, rendered, digest)
        };
        let records = trace.stage("rename.persist_records");
        let version = document.version;
        plan.document_version = version;
        plan.title.clone_from(&document.title);
        plan.review_digest = Some(digest);
        plan.acceptance = None;
        let change = format!("Renamed {previous_name} to {new_name}");
        plan.manual_changes.push(change);
        plan.updated_at_ms = self.clock.now_ms();
        self.publish_plan(&plan)?;

        self.save_session()?;
        records.complete(json!({"version":version}));
        let payload = json!({
            "plan": &plan,
            "content": &rendered.markdown,
            "document": &document,
            "previous_name": previous_name,
            "name": new_name,
        });
        mutation.complete(json!({"version":version}));
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
        let approval_mode = acceptance.approval_mode()?;
        self.dispatch_session_command(
            "session.plan.accept",
            json!({
                "plan_id": plan.id,
                "digest": acceptance.review_digest,
                "saved_source_digest": acceptance.saved_source_digest,
                "approval_mode": approval_mode,
                "comment": acceptance.overall_comment,
            }),
        )
    }

    #[cfg(test)]
    fn create_goal(&mut self, objective: String, native: bool) -> Result<GoalRecord> {
        let now_ms = self.clock.now_ms();
        let mut task = crate::task::ActiveTask::goal(
            objective,
            crate::session::continuation::ContinuationBudget::new(self.goal_max_turns, 2),
            native,
            now_ms,
        );
        task.request_entry("Continue the active goal".into(), now_ms)?;
        let current = self.task_controller.current()?;
        self.task_controller.change(
            &mut self.store,
            current.as_ref().map(|task| &task.revision),
            crate::task::TaskChange::Install(Box::new(task)),
            now_ms,
        )?;
        self.session.active_task = self.task_controller.current()?;
        self.active_goal()
    }

    async fn apply_goal_evidence(
        &mut self,
        evidence: crate::goal::TurnEvidence,
        permit: Option<crate::task::TaskPermit>,
        completed_turn: crate::task::TaskTurnId,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        let Some(permit) = permit else {
            return Ok(false);
        };
        let Some(task) = self.task_controller.current()? else {
            return Ok(false);
        };
        if task.kind() == crate::task::TaskKind::Plan {
            return Ok(false);
        }
        let observed = self.task_controller.observe_goal(
            &mut self.store,
            &permit,
            completed_turn,
            evidence,
            self.clock.now_ms(),
        );
        let (observed, decision) = match observed {
            Ok(observed) => observed,
            Err(error) if error.is::<crate::task::StaleTask>() => {
                self.trace
                    .record(&self.session.id, "task.stale_goal_evidence", json!({}));
                return Ok(false);
            }
            Err(error) => return Err(error),
        };
        let goal = GoalRecord {
            id: task.revision.as_str().into(),
            session_id: self.session.id.clone(),
            objective: observed.objective,
            state: observed.state,
            continuation: observed.continuation,
            native: observed.native,
            created_at_ms: task.created_at_ms,
            updated_at_ms: self.clock.now_ms(),
        };
        let continuing = match decision {
            ContinuationDecision::Continue | ContinuationDecision::RetryNoProgress => {
                self.trace.record(
                    &self.session.id,
                    "task.continuation_reserved",
                    json!({"revision":permit.revision()}),
                );
                event.push(self.event(
                    if task.kind() == crate::task::TaskKind::Goal {
                        "goal_changed"
                    } else {
                        "goal_continue_requested"
                    },
                    serde_json::to_value(&goal)?,
                )?);
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
        Ok(continuing)
    }

    fn active_goal(&self) -> Result<GoalRecord> {
        self.current_goal()?.context("no active goal")
    }

    fn current_goal(&self) -> Result<Option<GoalRecord>> {
        if let Some(task) = self.task_controller.current()? {
            let (objective, state, continuation, native) = match &task.workflow {
                crate::task::TaskWorkflow::Goal { goal } => (
                    goal.objective.clone(),
                    goal.state,
                    goal.continuation.clone(),
                    goal.native,
                ),
                crate::task::TaskWorkflow::Execute { execution } => (
                    execution.title.clone(),
                    match execution.record.state {
                        PlanExecutionState::Active => GoalState::Active,
                        PlanExecutionState::Paused => GoalState::Paused,
                        PlanExecutionState::Complete => GoalState::Complete,
                        PlanExecutionState::Cancelled => GoalState::Cleared,
                        PlanExecutionState::Stalled => GoalState::Stalled,
                        PlanExecutionState::Blocked => GoalState::Blocked,
                    },
                    execution.continuation.clone(),
                    execution.native,
                ),
                crate::task::TaskWorkflow::Plan { .. } => return Ok(None),
            };
            return Ok(Some(GoalRecord {
                id: task.revision.as_str().into(),
                session_id: self.session.id.clone(),
                objective,
                state,
                continuation,
                native,
                created_at_ms: task.created_at_ms,
                updated_at_ms: task.updated_at_ms,
            }));
        }
        Ok(None)
    }

    async fn pause_goal_after_turn_failure(&mut self) -> Result<()> {
        if let Some(task) = self.task_controller.current()?
            && task.entry_accepted()
            && task.status() == crate::task::TaskStatus::Running
        {
            self.task_controller.change(
                &mut self.store,
                Some(&task.revision),
                crate::task::TaskChange::Pause,
                self.clock.now_ms(),
            )?;
        }
        Ok(())
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

    async fn prepare_rollback(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before workspace rollback"
        );
        let exchange_id = required_text(&params, "exchange_id")?;
        let exchange_id = self
            .store
            .checkpoint_owner(&self.session.id, &exchange_id)?;
        let interaction = self.store.list_exchange(&self.session.id)?;
        let target_index = interaction
            .iter()
            .position(|item| item.id == exchange_id)
            .context("interaction not found")?;
        anyhow::ensure!(
            interaction[target_index].disposition() == crate::exchange::HistoryDisposition::Current,
            "selected exchange no longer belongs to the current workspace history"
        );
        anyhow::ensure!(
            interaction
                .iter()
                .skip(target_index)
                .all(|exchange| exchange.is_finished()),
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
            .find(|item| item.disposition() != crate::exchange::HistoryDisposition::Superseded)
            .and_then(|item| match item.disposition() {
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
        self.require_restore_recovered()?;
        let objects = self.store.objects.clone();
        let workspace = workspace.to_owned();
        let exchange_ids = interaction
            .iter()
            .skip(target_index)
            .map(|exchange| exchange.id.clone())
            .collect();
        let preview = tokio::task::spawn_blocking(move || {
            let mut preview = crate::checkpoint::RestorePreview::prepare(
                &objects, &workspace, &expected, &target,
            )?;
            preview.exchange_ids = exchange_ids;
            preview.save(&objects)?;
            Ok::<_, anyhow::Error>(preview)
        })
        .await
        .context("restore preparation worker failed")??;
        Ok((preview.summary(0), Vec::new()))
    }

    fn require_restore_recovered(&self) -> Result<()> {
        if let Some(progress) =
            crate::checkpoint::RestoreJournal::progress(&self.store.objects, &self.session.id)?
        {
            anyhow::ensure!(
                progress.state == crate::checkpoint::RestoreState::Complete,
                "an interrupted restore requires recovery before new workspace work; open Harness undo"
            );
        }
        Ok(())
    }

    async fn apply_rollback(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        match params
            .get("scope")
            .and_then(Value::as_str)
            .unwrap_or("workspace")
        {
            "conversation" => return self.rollback_conversation(params).await,
            "workspace" => {}
            _ => anyhow::bail!("unknown rollback scope"),
        }
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before workspace rollback"
        );
        let preview_id = required_text(&params, "preview_id")?;
        let objects = self.store.objects.clone();
        let session_id = self.session.id.clone();
        let preview = tokio::task::spawn_blocking(move || {
            crate::checkpoint::RestorePreview::load(&objects, &session_id, &preview_id)
        })
        .await
        .context("restore preview read failed")??;
        anyhow::ensure!(
            crate::workspace::same(&preview.workspace, &self.session.workspace),
            "restore preview belongs to another workspace"
        );
        let exchanges = self.store.list_exchange(&self.session.id)?;
        anyhow::ensure!(
            exchanges.iter().all(|exchange| exchange.is_finished()),
            "cancel or finish the active exchange before restoring files"
        );
        if let Some(first) = preview.exchange_ids.first() {
            let position = exchanges
                .iter()
                .position(|exchange| &exchange.id == first)
                .context("restore exchange disappeared")?;
            anyhow::ensure!(
                exchanges[position..]
                    .iter()
                    .map(|exchange| &exchange.id)
                    .eq(preview.exchange_ids.iter())
                    && exchanges[position].disposition()
                        == crate::exchange::HistoryDisposition::Current,
                "restore preview is stale: exchange history changed"
            );
            self.store
                .require_settled_exchange_tree(&self.session.id, &preview.exchange_ids)?;
        }
        let journal = GitCheckpoint::new(&preview.workspace)
            .apply_preview(
                Arc::clone(&self.repositories),
                self.store.objects.clone(),
                preview,
                false,
            )
            .await?;
        self.finish_restore(journal).await
    }

    async fn finish_restore(
        &mut self,
        mut journal: crate::checkpoint::RestoreJournal,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let events = self
            .finish_conversation_rollback(&journal.preview.exchange_ids)
            .await?;
        journal.state = crate::checkpoint::RestoreState::Complete;
        journal.save(&self.store.objects)?;
        Ok((json!({"restored":true}), events))
    }

    async fn rollback_conversation(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before conversation rollback"
        );
        self.require_restore_recovered()?;
        let requested = required_text(&params, "exchange_id")?;
        let target = self.store.checkpoint_owner(&self.session.id, &requested)?;
        let expected_latest = required_text(&params, "latest_exchange_id")?;
        let exchanges = self.store.list_exchange(&self.session.id)?;
        anyhow::ensure!(
            exchanges
                .last()
                .is_some_and(|exchange| exchange.id == expected_latest),
            "conversation changed; select the rollback interaction again"
        );
        let position = exchanges
            .iter()
            .position(|exchange| exchange.id == target)
            .context("rollback interaction disappeared")?;
        anyhow::ensure!(
            exchanges[position].disposition() == crate::exchange::HistoryDisposition::Current,
            "selected interaction has already been rolled back"
        );
        anyhow::ensure!(
            exchanges.iter().all(|exchange| exchange.is_finished()),
            "cancel or finish active work before conversation rollback"
        );
        let ids = exchanges[position..]
            .iter()
            .map(|exchange| exchange.id.clone())
            .collect::<Vec<_>>();
        self.store
            .require_settled_exchange_tree(&self.session.id, &ids)?;
        let events = self.finish_conversation_rollback(&ids).await?;
        Ok((json!({"restored":true,"scope":"conversation"}), events))
    }

    async fn finish_conversation_rollback(
        &mut self,
        exchange_ids: &[String],
    ) -> Result<Vec<SessionEvent>> {
        let Some(first) = exchange_ids.first() else {
            return Ok(Vec::new());
        };
        let mut session = self.session.clone();
        if let Some(task) = self.task_controller.current()? {
            if let Some(goal) = self.current_goal()? {
                self.sync_native_goal(&goal, "cleared").await?;
            }
            self.task_controller.change(
                &mut self.store,
                Some(&task.revision),
                crate::task::TaskChange::Clear,
                self.clock.now_ms(),
            )?;
            session.active_task = None;
        }
        let replacement = crate::backend::build(
            self.backend_launch.clone(),
            self.permission_coordinator.clone(),
            self.trace.clone(),
            self.data_root.clone(),
        )?;
        self.backend.shutdown().await?;
        self.backend = Arc::from(replacement);
        session.backend_session_id = None;
        session.provider_checkpoint_id = None;
        session.provider_fork_state = ProviderForkState::Ready;
        session.context_usage = None;
        session.last_compaction_turn = None;
        session.acquire_lease(&self.client_id)?;
        session.updated_at_ms = self.clock.now_ms();
        self.store.record_rollback(&session, exchange_ids)?;
        self.session = session;
        self.context_usage_fresh = false;
        let exchange = self
            .store
            .load_exchange(first)?
            .filter(|exchange| exchange.session_id == self.session.id)
            .context("rolled-back interaction disappeared")?;
        Ok(vec![self.event(
            "interaction_rolled_back",
            serde_json::to_value(exchange)?,
        )?])
    }

    fn rollback_context(
        &self,
        current_exchange: &str,
        input: BackendInput,
    ) -> Result<BackendInput> {
        let exchanges = self.store.list_exchange(&self.session.id)?;
        if !exchanges
            .iter()
            .any(|exchange| exchange.disposition() != crate::exchange::HistoryDisposition::Current)
        {
            return Ok(input);
        }
        let retained = exchanges
            .iter()
            .filter(|exchange| {
                exchange.id != current_exchange
                    && exchange.disposition() == crate::exchange::HistoryDisposition::Current
            })
            .collect::<Vec<_>>();
        let history = serde_json::to_string(&retained)?;
        let context = format!(
            "The conversation was rolled back. Use only this retained conversation history as prior context. The current workspace files are authoritative.\n{history}\n\nCurrent request:\n"
        );
        Ok(match input {
            BackendInput::Text { text, attachments } => BackendInput::Text {
                text: format!("{context}{text}"),
                attachments,
            },
            BackendInput::Skill {
                name,
                arguments,
                attachments,
            } => BackendInput::Skill {
                name,
                arguments: format!("{context}{arguments}"),
                attachments,
            },
        })
    }

    #[cfg(test)]
    async fn rollback_exchange(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let (preview, _) = self.prepare_rollback(params).await?;
        anyhow::ensure!(
            preview["warning_count"].as_u64() == Some(0),
            "restore requires confirmation of later changes"
        );
        self.apply_rollback(json!({"preview_id":preview["preview_id"]}))
            .await
    }

    async fn restore_recovery(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let action = params
            .get("action")
            .and_then(Value::as_str)
            .unwrap_or("status");
        if action == "status" {
            let progress =
                crate::checkpoint::RestoreJournal::progress(&self.store.objects, &self.session.id)?;
            return Ok((progress.map(|progress| json!({"pending": progress.state != crate::checkpoint::RestoreState::Complete,
                "applied": progress.applied, "files": progress.files})).unwrap_or(Value::Null), Vec::new()));
        }
        enum RecoveryAction {
            Continue,
            Undo,
        }
        let action = match action {
            "continue" => RecoveryAction::Continue,
            "undo" => RecoveryAction::Undo,
            _ => anyhow::bail!("unknown restore recovery action"),
        };
        let objects = self.store.objects.clone();
        let session_id = self.session.id.clone();
        let journal = tokio::task::spawn_blocking(move || {
            crate::checkpoint::RestoreJournal::load(&objects, &session_id)
        })
        .await
        .context("restore journal read failed")??
        .context("restore journal is missing")?;
        match action {
            RecoveryAction::Continue => {
                anyhow::ensure!(
                    journal.state != crate::checkpoint::RestoreState::Complete,
                    "restore is already complete"
                );
                let exchange = self.store.list_exchange(&self.session.id)?;
                anyhow::ensure!(
                    exchange.iter().all(|exchange| exchange.is_finished()),
                    "cancel or finish active work before restore recovery"
                );
                self.store.require_settled_exchange_tree(
                    &self.session.id,
                    &exchange
                        .iter()
                        .map(|exchange| exchange.id.clone())
                        .collect::<Vec<_>>(),
                )?;
                let journal = GitCheckpoint::new(&journal.preview.workspace)
                    .apply_preview(
                        Arc::clone(&self.repositories),
                        self.store.objects.clone(),
                        journal.preview,
                        true,
                    )
                    .await?;
                return self.finish_restore(journal).await;
            }
            RecoveryAction::Undo => {
                let objects = self.store.objects.clone();
                let preview = tokio::task::spawn_blocking(move || {
                    let preview = journal.reverse(&objects)?;
                    preview.save(&objects)?;
                    Ok::<_, anyhow::Error>(preview)
                })
                .await
                .context("recovery preparation worker failed")??;
                Ok((preview.summary(0), Vec::new()))
            }
        }
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

    async fn resume_session(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        let mut session = self
            .store
            .load_session(&session_id)?
            .context("session not found")?;
        anyhow::ensure!(
            crate::workspace::same(&session.workspace, &self.session.workspace),
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
        session = self
            .store
            .acquire_session_lease(&session.id, &self.client_id)?;
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
            vec![self.event(
                "session_changed",
                serde_json::to_value(self.session_view()?)?,
            )?],
        ))
    }

    async fn pause_current_goal(&mut self) -> Result<()> {
        let Some(goal) = self.current_goal()? else {
            return Ok(());
        };
        if goal.state != GoalState::Active {
            return Ok(());
        }
        self.sync_native_goal(&goal, "paused").await?;
        let task = self
            .task_controller
            .current()?
            .context("task was cleared while pausing")?;
        self.task_controller.change(
            &mut self.store,
            Some(&task.revision),
            crate::task::TaskChange::Pause,
            self.clock.now_ms(),
        )?;
        Ok(())
    }

    async fn configuration_progress(&mut self, state: ConfigurationProgressState) -> Result<()> {
        let Some(operation_id) = self.active_operation_id.clone() else {
            return Ok(());
        };
        self.emit_session_update(
            SessionUpdate::ConfigurationProgress {
                operation_id: operation_id.to_string(),
                state,
            },
            &mut Vec::new(),
        )
        .await
    }

    async fn configure_session(&mut self, mut params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let mut resource_selection = None;
        if params.get("settings_revision").is_some() {
            let settings = crate::settings::SettingsStore::open(&self.data_root)?.get(
                &self.session.workspace,
                &self.session.id,
                &self.session.backend,
            )?;
            anyhow::ensure!(
                params["settings_revision"].as_u64() == Some(settings.revision),
                "settings revision was superseded"
            );
            params["access"] =
                serde_json::to_value(settings.desired.access.clone().unwrap_or_default())?;
            params["shell"] = serde_json::to_value(settings.desired.shell.unwrap_or_default())?;
            params["default_approval"] = serde_json::to_value(
                settings
                    .desired
                    .default_approval
                    .unwrap_or(ApprovalMode::Ask),
            )?;
            params["plan_approval"] = serde_json::to_value(settings.desired.plan_approval)?;
            params["plan_compact"] = json!(
                settings.desired.plan_compact.unwrap_or(false) && self.capability.native_compact
            );
            params["plan_auto_approve_revisions"] =
                json!(settings.desired.plan_auto_approve_revisions.unwrap_or(true));
            params["approval_timeout"] =
                serde_json::to_value(settings.desired.approval_timeout.unwrap_or_default())?;
            params["answer_timeout"] =
                serde_json::to_value(settings.desired.answer_timeout.unwrap_or_default())?;
            let desired = serde_json::to_value(&settings.desired)?;
            for (key, value) in desired.as_object().context("settings must be an object")? {
                if !value.is_null() && key != "skill" && key != "mcp" && key != "plan_compact" {
                    params[key] = value.clone();
                }
            }
            resource_selection = Some(settings.desired);
        }
        let mut configured = self.session.clone();
        let check_timing = params
            .get("check_timing")
            .map(|value| serde_json::from_value::<crate::checks::CheckTiming>(value.clone()))
            .transpose()?;
        let automatic_checks = params
            .get("automatic_checks")
            .map(|value| serde_json::from_value::<bool>(value.clone()))
            .transpose()?;
        if let Some(value) = params.get("auto_compaction") {
            let policy: crate::session::AutoCompaction = serde_json::from_value(value.clone())?;
            policy.validate(configured.context_usage.as_ref())?;
            anyhow::ensure!(
                !policy.enabled || self.capability.native_compact,
                "This backend does not support automatic compaction"
            );
            configured.auto_compaction = policy;
        }
        (configured.approval_timeout, configured.answer_timeout) = self
            .permission_coordinator
            .input_timeouts(&self.session.id)?;
        let access = params
            .get("access")
            .map(|value| serde_json::from_value::<crate::session::AccessPolicy>(value.clone()))
            .transpose()?
            .map(crate::session::AccessPolicy::validate)
            .transpose()?;
        if let Some(value) = params.get("shell") {
            configured.shell = serde_json::from_value(value.clone())?;
            configured.shell.executable()?;
        }
        if let Some(value) = params.get("approval_mode") {
            configured.approval_mode = serde_json::from_value(value.clone())?;
        }
        if let Some(value) = params.get("default_approval") {
            let permission: ApprovalMode = serde_json::from_value(value.clone())?;
            configured.default_approval = permission;
        }
        if let Some(value) = params.get("plan_approval") {
            configured.plan_approval = serde_json::from_value(value.clone())?;
        }
        if let Some(value) = params.get("approval_timeout") {
            configured.approval_timeout =
                serde_json::from_value::<crate::session::InputTimeout>(value.clone())?
                    .validate()?;
        }
        if let Some(value) = params.get("answer_timeout") {
            configured.answer_timeout =
                serde_json::from_value::<crate::session::InputTimeout>(value.clone())?
                    .validate()?;
        }
        let requested_service_tier = params
            .get("service_tier")
            .map(|value| serde_json::from_value::<crate::backend::ServiceTier>(value.clone()))
            .transpose()?;
        if let Some(tier) = requested_service_tier {
            tier.validate(&self.capability)?;
        }
        let requested_model = params.get("model").and_then(Value::as_str);
        let requested_effort = params.get("effort").and_then(Value::as_str);
        let requested_context_window = params.get("context_window").and_then(Value::as_str);
        let validate_selection = params.get("validate").and_then(Value::as_bool) == Some(true);
        let mut plan_executor = self.session.plan_executor.clone();
        let mut plan_compact = self.session.plan_compact;
        if let Some(enabled) = params.get("plan_compact").and_then(Value::as_bool) {
            if enabled {
                anyhow::ensure!(
                    self.capability.native_compact,
                    "current backend does not support context compaction"
                );
            }
            plan_compact = enabled;
        }
        if let Some(enabled) = params.get("plan_executor_enabled").and_then(Value::as_bool) {
            if enabled {
                anyhow::ensure!(
                    self.capability.model_selection,
                    "current backend does not support model selection"
                );
                if plan_executor.model.is_none() {
                    let active_model = self
                        .session
                        .resolved_model
                        .clone()
                        .unwrap_or_else(|| self.session.model.clone());
                    let selected_model = if active_model == "default" {
                        self.backend
                            .model_list(
                                self.backend_request(BackendInput::from_text(""), PromptMode::Chat),
                            )
                            .await?
                            .into_iter()
                            .find(|candidate| candidate.is_default)
                            .context("the backend has no default model")?
                            .id
                    } else {
                        active_model
                    };
                    plan_executor.model = Some(selected_model);
                    plan_executor.effort = Some(self.session.effort.clone());
                }
            }
            plan_executor.enabled = enabled;
        }
        if let Some(model) = params.get("plan_executor_model").and_then(Value::as_str) {
            anyhow::ensure!(
                !model.trim().is_empty(),
                "plan executor model cannot be empty"
            );
            plan_executor.model = Some(model.to_owned());
        }
        if let Some(effort) = params.get("plan_executor_effort").and_then(Value::as_str) {
            anyhow::ensure!(
                !effort.trim().is_empty(),
                "plan executor thinking cannot be empty"
            );
            plan_executor.effort = Some(effort.to_owned());
        }
        if params.get("plan_executor_model").is_some()
            || params.get("plan_executor_effort").is_some()
            || params.get("plan_executor_enabled").and_then(Value::as_bool) == Some(true)
        {
            self.validate_plan_executor(&plan_executor).await?;
        }
        let selected_tier = requested_service_tier.unwrap_or(self.session.service_tier);
        let validate_tier = selected_tier != crate::backend::ServiceTier::Standard
            && (requested_service_tier.is_some() || requested_model.is_some());
        if validate_tier
            || (validate_selection
                && (requested_model.is_some()
                    || requested_effort.is_some()
                    || requested_context_window.is_some()))
        {
            let model_list = self
                .backend
                .model_list(self.backend_request(BackendInput::from_text(""), PromptMode::Chat))
                .await?;
            let selected_model = requested_model.unwrap_or_else(|| {
                self.session
                    .resolved_model
                    .as_deref()
                    .unwrap_or(&self.session.model)
            });
            let target_model = model_list.iter().find(|candidate| {
                if selected_model == "default" {
                    candidate.is_default
                } else {
                    candidate.id == selected_model
                }
            });
            if let Some(model) = requested_model {
                anyhow::ensure!(
                    target_model
                        .is_some_and(|candidate| model == "default" || candidate.id == model),
                    "model {model} is unavailable for this backend"
                );
            }
            if validate_tier {
                selected_tier.validate_model(
                    target_model.context("the selected model is unavailable for this backend")?,
                )?;
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
            configured.model = model.to_owned();
            configured.resolved_model = (model != "default").then(|| model.to_owned());
        }
        if let Some(effort) = params.get("effort").and_then(Value::as_str) {
            anyhow::ensure!(!effort.trim().is_empty(), "effort cannot be empty");
            configured.effort = effort.to_owned();
        }
        if params.get("context_window").is_some() {
            configured.context_window = params
                .get("context_window")
                .and_then(Value::as_str)
                .map(str::to_owned);
        }
        if let Some(service_tier) = requested_service_tier {
            configured.service_tier = service_tier;
        }
        configured.plan_executor = plan_executor;
        configured.plan_compact = plan_compact;
        if let Some(enabled) = params
            .get("plan_auto_approve_revisions")
            .and_then(Value::as_bool)
        {
            configured.plan_auto_approve_revisions = enabled;
        }
        let subagent_tools = params.get("subagent_tools").and_then(Value::as_bool);
        let lsp_tools = params.get("lsp_tools").and_then(Value::as_bool);
        let session_tools = params
            .get("session_tools")
            .map(|value| serde_json::from_value::<bool>(value.clone()))
            .transpose()?;
        let status_thoughts = params
            .get("status_thoughts")
            .map(|value| serde_json::from_value::<bool>(value.clone()))
            .transpose()?;
        let delegation_registry_changed = subagent_tools
            .is_some_and(|enabled| enabled != self.agent_coordinator.enabled())
            || lsp_tools.is_some_and(|enabled| enabled != self.lsp.enabled(&self.session.id))
            || session_tools.is_some();
        if let Some(access) = access {
            configured.access = access;
        }
        configured.access = crate::backend::BackendKind::parse(&configured.backend)?
            .resolve_access(configured.access);
        if let Some(selection) = resource_selection {
            self.backend
                .reconcile_settings(
                    BackendCatalogRequest {
                        execution_owner: None,
                        harness_session_id: configured.id.clone(),
                        workspace: configured.workspace.clone(),
                        access: configured.access.clone(),
                        approval_mode: configured.approval_mode,
                        backend_session_id: configured.backend_session_id.clone(),
                    },
                    &selection,
                )
                .await?;
        }
        let previous = std::mem::replace(&mut self.session, configured);
        self.permission_coordinator.configure_input_timeouts(
            &self.session.id,
            self.session.approval_timeout,
            self.session.answer_timeout,
        )?;
        if let Err(error) = self.save_session() {
            self.permission_coordinator.configure_input_timeouts(
                &previous.id,
                previous.approval_timeout,
                previous.answer_timeout,
            )?;
            self.session = previous;
            return Err(error);
        }
        let update_model_preference = requested_model.is_some()
            || requested_effort.is_some()
            || params.get("context_window").is_some();
        self.save_preference(update_model_preference)?;
        if params.get("settings_revision").is_none() {
            let mut settings = crate::settings::SettingsStore::open(&self.data_root)?;
            let snapshot = settings.get(
                &self.session.workspace,
                &self.session.id,
                &self.session.backend,
            )?;
            let scope =
                if params.get("auto_compaction").is_some() || params["session_policy"] == true {
                    crate::settings::SettingScope::Session
                } else {
                    crate::settings::SettingScope::Global
                };
            let mut portable =
                settings.layer_settings(scope, &self.session.workspace, &self.session.id)?;
            let mut changed = false;
            if params.get("access").is_some() {
                portable.access = Some(self.session.access.clone());
                changed = true;
            }
            if params.get("shell").is_some() {
                portable.shell = Some(self.session.shell);
                changed = true;
            }
            if let Some(value) = check_timing {
                portable.check_timing = Some(value);
                changed = true;
            }
            if let Some(value) = automatic_checks {
                portable.automatic_checks = Some(value);
                changed = true;
            }
            if params.get("default_approval").is_some() {
                portable.default_approval = Some(self.session.default_approval);
                changed = true;
            }
            if params.get("plan_approval").is_some() {
                portable.plan_approval = self.session.plan_approval;
                changed = true;
            }
            if params.get("auto_compaction").is_some() {
                portable.auto_compaction = Some(self.session.auto_compaction.clone());
                changed = true;
            }
            if params.get("plan_compact").is_some() {
                portable.plan_compact = Some(self.session.plan_compact);
                changed = true;
            }
            if params.get("plan_auto_approve_revisions").is_some() {
                portable.plan_auto_approve_revisions =
                    Some(self.session.plan_auto_approve_revisions);
                changed = true;
            }
            if params.get("approval_timeout").is_some() {
                portable.approval_timeout = Some(self.session.approval_timeout);
                changed = true;
            }
            if params.get("answer_timeout").is_some() {
                portable.answer_timeout = Some(self.session.answer_timeout);
                changed = true;
            }
            if let Some(enabled) = subagent_tools {
                portable.subagent_tools = Some(enabled);
                changed = true;
            }
            if let Some(enabled) = lsp_tools {
                portable.lsp_tools = Some(enabled);
                changed = true;
            }
            if let Some(enabled) = session_tools {
                portable.session_tools = Some(enabled);
                changed = true;
            }
            if let Some(enabled) = status_thoughts {
                portable.status_thoughts = Some(enabled);
                changed = true;
            }
            let revision = if changed {
                settings.update(
                    snapshot.revision,
                    scope,
                    &self.session.workspace,
                    &self.session.id,
                    &portable,
                )?
            } else {
                snapshot.revision
            };
            if update_model_preference
                || params.get("service_tier").is_some()
                || params.get("plan_executor_enabled").is_some()
            {
                settings.save_provider(revision,&self.session.backend,&json!({"model":self.session.model,"effort":self.session.effort,
                    "context_window":self.session.context_window,"service_tier":self.session.service_tier,"plan_executor":self.session.plan_executor}))?;
            }
        }
        if let Some(enabled) = subagent_tools {
            self.agent_coordinator.set_enabled(enabled);
        }
        if let Some(enabled) = lsp_tools {
            self.lsp.set_enabled(&self.session.id, enabled);
        }
        if let Some(enabled) = status_thoughts {
            self.observer
                .status_thoughts
                .store(enabled, Ordering::Relaxed);
        }
        if delegation_registry_changed && params.get("settings_revision").is_none() {
            self.backend.cleanup_execution(&self.session.id).await?;
        }
        Ok((
            serde_json::to_value(self.session_view()?)?,
            vec![self.event(
                "session_configured",
                json!({
                    "session": self.session_view()?,
                    "access_abbreviation": self.session.access.abbreviation(),
                }),
            )?],
        ))
    }

    async fn validate_plan_executor(&mut self, executor: &PlanExecutor) -> Result<()> {
        anyhow::ensure!(
            self.capability.model_selection,
            "current backend does not support model selection"
        );
        let model = executor
            .model
            .as_deref()
            .context("plan executor model is not configured")?;
        let effort = executor
            .effort
            .as_deref()
            .context("plan executor thinking is not configured")?;
        let model_list = self
            .backend
            .model_list(self.backend_request(BackendInput::from_text(""), PromptMode::Chat))
            .await?;
        let selected = model_list
            .iter()
            .find(|candidate| candidate.id == model)
            .with_context(|| {
                format!("plan executor model {model} is unavailable for this backend")
            })?;
        anyhow::ensure!(
            selected.reasoning.is_empty()
                || selected
                    .reasoning
                    .iter()
                    .any(|candidate| candidate == effort),
            "plan executor thinking {effort} is unavailable for model {model}"
        );
        Ok(())
    }

    /// Resolve persisted ownership across primary and child-agent exchanges.
    fn compaction_exchange_owner(
        &self,
        compaction: &crate::backend::Compaction,
    ) -> Result<Option<Exchange>> {
        self.store
            .load_compaction_exchange(&self.session.id, compaction)
    }

    /// Merge active and background notifications into their persisted timeline owner.
    fn retain_external_compaction(
        &mut self,
        mut compaction: crate::backend::Compaction,
        now_ms: i64,
    ) -> Result<()> {
        if compaction.state == crate::backend::CompactionState::Running
            && self.session.backend_session_id.as_deref() == Some(compaction.thread_id.as_str())
        {
            compaction.result.context_tokens_before = compaction
                .result
                .context_tokens_before
                .or_else(|| self.session.context_usage.as_ref().map(|usage| usage.used));
        }
        self.save_session()?;
        if let Some(mut exchange) = self.compaction_exchange_owner(&compaction)? {
            exchange.observe_compaction(compaction, now_ms);
            self.store.save_exchange(&exchange)?;
        } else {
            let existing = self
                .store
                .list_session_event(&self.session.id)?
                .into_iter()
                .find(|record| {
                    matches!(&record.detail, SessionEventKind::Compaction { compaction: existing }
                    if existing.id == compaction.id && existing.thread_id == compaction.thread_id)
                });
            let record = if let Some(mut record) = existing {
                if let SessionEventKind::Compaction {
                    compaction: existing,
                } = &mut record.detail
                {
                    existing.observe(compaction, now_ms);
                }
                record
            } else {
                SessionEventRecord {
                    id: format!(
                        "{}:compaction:{}:{}",
                        self.session.id, compaction.thread_id, compaction.id
                    ),
                    session_id: self.session.id.clone(),
                    owner: crate::timeline::TimelineOwner::session(&self.session.id),
                    created_at_ms: now_ms,
                    detail: SessionEventKind::Compaction {
                        compaction: (compaction.observed(now_ms)).into(),
                    },
                }
            };
            self.store.save_session_event(&record)?;
        }
        Ok(())
    }

    /// Retain session notifications after provider execution has released the broker.
    pub(crate) fn observe_external_compaction(
        &mut self,
        event: BackendEvent,
    ) -> Result<Vec<SessionEvent>> {
        let now_ms = event.observed_at_ms(self.clock.now_ms());
        let BackendEvent::Compaction {
            scope,
            update: compaction,
        } = event
        else {
            return Ok(match SessionUpdate::from_observation(&event) {
                Some(update) => vec![self.event(
                    "backend_event",
                    publication::encode_session_update(&update)?,
                )?],
                None => Vec::new(),
            });
        };
        anyhow::ensure!(
            scope.owner().session_id == self.session.id,
            "compaction crosses session ownership"
        );
        self.retain_external_compaction(*compaction, now_ms)?;
        let patch = self.reconcile_timeline()?;
        Ok(if patch.is_empty() {
            Vec::new()
        } else {
            vec![self.event("document_changed", patch.notification())?]
        })
    }

    async fn compact_session(&mut self, manual: bool) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            self.capability.native_compact,
            "current backend does not support context compaction"
        );
        anyhow::ensure!(
            self.session.backend_session_id.is_some(),
            "current session has no provider conversation to compact"
        );
        let now_ms = self.clock.now_ms();
        let mut compaction = crate::backend::Compaction {
            id: Uuid::new_v4().to_string(),
            thread_id: self
                .session
                .backend_session_id
                .clone()
                .expect("validated provider conversation"),
            state: crate::backend::CompactionState::Running,
            manual,
            started_at_ms: Some(now_ms),
            completed_at_ms: None,
            result: crate::backend::CompactionResult {
                context_tokens_before: self.session.context_usage.as_ref().map(|usage| usage.used),
                ..Default::default()
            },
        };
        let mut record = SessionEventRecord {
            id: format!("{}:compaction:{}", self.session.id, compaction.id),
            session_id: self.session.id.clone(),
            owner: crate::timeline::TimelineOwner::session(&self.session.id),
            created_at_ms: now_ms,
            detail: SessionEventKind::Compaction {
                compaction: (compaction.clone()).into(),
            },
        };
        self.store.save_session_event(&record)?;
        let mut event = Vec::new();
        let patch = self.reconcile_timeline()?;
        self.emit_timeline_patch(patch, &mut event).await?;
        let result = self
            .backend
            .compact(crate::backend::BackendCompactionRequest {
                id: compaction.id.clone(),
                request: self.backend_request(BackendInput::from_text(""), PromptMode::Chat),
            })
            .await;
        compaction.completed_at_ms = Some(self.clock.now_ms());
        let output = match result {
            Ok(output) => output,
            Err(error) => {
                compaction.state = crate::backend::CompactionState::Failed;
                compaction.result.error = Some(format!("{error:#}"));
                record.detail = SessionEventKind::Compaction {
                    compaction: compaction.into(),
                };
                self.store.save_session_event(&record)?;
                let patch = self.reconcile_timeline()?;
                self.emit_timeline_patch(patch, &mut event).await?;
                return Err(error);
            }
        };
        let observed = output.event.iter().rev().find_map(|event| match event {
            BackendEvent::Compaction { update, .. } => Some(update.clone()),
            _ => None,
        });
        let output = output.completion;
        let context_tokens_before = compaction.result.context_tokens_before;
        if let Some(observed) = observed {
            compaction.state = observed.state;
            compaction.result = observed.result;
        } else {
            compaction.state = crate::backend::CompactionState::Completed;
        }
        compaction.result.context_tokens_before = context_tokens_before;
        if compaction.state == crate::backend::CompactionState::Completed {
            compaction.result.context_tokens_after = output
                .metrics
                .context_usage
                .as_ref()
                .map(|usage| usage.used);
        }
        let failed = compaction.state == crate::backend::CompactionState::Failed;
        let failure = compaction
            .result
            .error
            .clone()
            .unwrap_or_else(|| "Provider compaction failed".into());
        record.detail = SessionEventKind::Compaction {
            compaction: compaction.into(),
        };
        self.store.save_session_event(&record)?;
        self.capability = output.capability.clone();
        self.session.native_fork = output.capability.native_fork;
        self.session.native_compact = output.capability.native_compact;
        if let Some(backend_session_id) = output.backend_session_id {
            self.session.backend_session_id = Some(backend_session_id);
        }
        if let Some(context_usage) = output.metrics.context_usage {
            let size = self
                .session
                .context_usage
                .as_ref()
                .map_or(context_usage.size, |current| current.size);
            self.session.context_usage = ContextUsage::reported(context_usage.used, size);
        }
        if !output.runtime.provider.is_empty() {
            self.session.provider_label = output.runtime.provider;
        }
        if output.runtime.model.is_some() {
            self.session.resolved_model = output.runtime.model;
        }
        self.save_session()?;
        let snapshot = self.snapshot()?;
        let patch = self.reconcile_timeline()?;
        self.emit_timeline_patch(patch, &mut event).await?;
        anyhow::ensure!(!failed, "{failure}");
        event.push(self.event("context_compacted", serde_json::to_value(&snapshot)?)?);
        Ok((serde_json::to_value(&snapshot)?, event))
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
            execution_owner: None,
            harness_session_id: self.session.id.clone(),
            workspace: self.session.workspace.clone(),
            input: if self.submitted_attachments.is_empty() {
                input
            } else {
                input.with_attachments(self.submitted_attachments.clone())
            },
            mode: if self.session.background.is_some() {
                PromptMode::Background
            } else {
                mode
            },
            model: self.session.model.clone(),
            effort: self.session.effort.clone(),
            context_window: self.session.context_window.clone(),
            service_tier: self.session.service_tier,
            access: self.session.access.clone(),
            approval_mode: self.session.approval_mode,
            backend_session_id: self.session.backend_session_id.clone(),
            submission: None,
            recovered_input: Vec::new(),
            control_context: self.control_turn_context(if self.session.background.is_some() {
                PromptMode::Background
            } else {
                mode
            }),
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
        (self.session.approval_timeout, self.session.answer_timeout) = self
            .permission_coordinator
            .input_timeouts(&self.session.id)?;
        let now_ms = self.clock.now_ms();
        self.session.acquire_lease(&self.client_id)?;
        self.session.updated_at_ms = now_ms;
        self.store
            .save_owned_session(&self.session, &self.client_id)?;
        self.session.name = self
            .store
            .load_session(&self.session.id)?
            .context("owned session is missing")?
            .name;
        Ok(())
    }

    fn save_preference(&mut self, update_model_preference: bool) -> Result<()> {
        if self.session.background.is_some() {
            return Ok(());
        }
        let previous = self
            .store
            .load_preference(&self.session.workspace, &self.session.backend)?;
        let mut preference = preference_for_session(&self.session, previous.clone());
        if !update_model_preference && let Some(previous) = previous {
            preference.model = previous.model;
            preference.effort = previous.effort;
            preference.model_setting = previous.model_setting;
        }
        self.store
            .save_preference(&self.session.workspace, &self.session.backend, &preference)
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
                "active_plan_id": self.current_plan_id().ok().flatten(),
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
}

impl Drop for HarnessBroker {
    fn drop(&mut self) {
        let _ = self.release_lease();
    }
}

fn attribute_provider_tool(
    exchange: &mut Exchange,
    checklist: &mut ChecklistTracker,
    event: &BackendEvent,
) -> Result<()> {
    let BackendEvent::Tool { address, .. } = event else {
        return Ok(());
    };
    let Some(checklist_item_id) = checklist.attribution_target().map(str::to_owned) else {
        return Ok(());
    };
    let turn_id = exchange.turn_mut(&address.turn)?.id().to_owned();
    if exchange.attribute_tool(&turn_id, address.tool_id.as_str(), &checklist_item_id)? {
        checklist.mark_attributed(&checklist_item_id);
        exchange.checklist = Some(checklist.snapshot().clone());
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
        access: session.access.clone(),
        shell: session.shell,

        default_approval: session.default_approval,
        plan_approval: session.plan_approval,
        model: session.model.clone(),
        effort: session.effort.clone(),
        model_setting,
        service_tier: session.service_tier,
        plan_executor: session.plan_executor.clone(),
        plan_compact: session.plan_compact,
        plan_auto_approve_revisions: session.plan_auto_approve_revisions,
        approval_timeout: session.approval_timeout,
        answer_timeout: session.answer_timeout,
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
    fn completed_message(
        turn: &crate::backend::TurnAddress,
        id: impl Into<String>,
        role: crate::backend::MessageRole,
        text: impl Into<String>,
    ) -> Vec<BackendEvent> {
        crate::backend::test_support::message(turn, id, Some(role), text)
    }

    fn compaction_event(session_id: &str, compaction: crate::backend::Compaction) -> BackendEvent {
        BackendEvent::Compaction {
            scope: crate::backend::EventScope::SessionOperation {
                owner: crate::backend::ExecutionOwner::root(session_id),
                operation_id: compaction.id.clone(),
            },
            update: Box::new(compaction),
        }
    }

    fn active_execution(broker: &HarnessBroker) -> crate::plan::PlanExecutionRecord {
        match broker.task_controller.current().unwrap().unwrap().workflow {
            crate::task::TaskWorkflow::Execute { execution } => execution.record,
            _ => panic!("expected an active execution"),
        }
    }

    #[tokio::test]
    async fn live_status_and_agent_queries_do_not_reload_unrelated_history() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.create_task(crate::task::TaskKind::Goal, "active".into(), None, None)?;
        let task = broker.task_controller.current()?.unwrap();
        let connection = rusqlite::Connection::open(data.path().join("harness.sqlite3"))?;
        connection.execute("INSERT INTO exchange_record(id,session_id,agent_id,ordinal,payload) VALUES('old',?1,?2,1,?3)",rusqlite::params![broker.session.id,HarnessSession::primary_agent_id(&broker.session.id),serde_json::json!({"state":"complete","turn":"malformed history"}).to_string()])?;
        assert_eq!(
            broker.task_controller.current()?.unwrap().revision,
            task.revision
        );
        assert_eq!(
            broker.live_interaction_status(None)?,
            crate::session::state_machine::SessionPhase::Idle
        );
        assert!(broker.retry_finalization().await?.0["finalized_exchange_id"].is_null());
        connection.execute("DELETE FROM exchange_record WHERE id='old'", [])?;
        connection
            .execute_batch("ALTER TABLE prompt_history_record RENAME TO unavailable_history")?;
        let observer = broker.observer.clone();
        assert!(observer.snapshot().is_err());
        Ok(())
    }

    struct AutomaticCheckBackend {
        turns: Arc<std::sync::atomic::AtomicUsize>,
        repair: bool,
        explicit: bool,
    }

    #[async_trait::async_trait]
    impl Backend for AutomaticCheckBackend {
        fn checkpoint_tool_gate(&self) -> bool {
            true
        }
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Automatic check fixture".into(),
                capability: crate::backend::mock_capability(),
            }
        }
        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
        async fn has_active_turn(&self, _: &str) -> bool {
            false
        }
        async fn cleanup_execution(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn cancel_session(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn active_session_id_for(&self, _: &str) -> Option<String> {
            None
        }
        async fn rollback_cancelled_turn_for(&self, _: &str) -> Result<bool> {
            Ok(false)
        }
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(sink);
            let sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let sink = Some(sink);
                    if let Some(checkpoint) = request
                        .control_context
                        .as_ref()
                        .and_then(|context| context.checkpoint.as_ref())
                    {
                        checkpoint.wait().await?;
                    }
                    let turn = self.turns.fetch_add(1, Ordering::SeqCst);
                    if turn == 0 || self.repair {
                        std::fs::write(
                            Path::new(&request.workspace).join("seed.txt"),
                            if turn > 0 { "fixed" } else { "broken" },
                        )?;
                    }
                    if turn == 0 && self.explicit {
                        let context = request.control_context.as_ref().unwrap();
                        let advertised = crate::control_tools::ControlToolRegistry
                            .definition_list_for_context(Some(context), true);
                        assert!(advertised.iter().any(|tool| tool.name == "check_run"));
                        assert!(
                            !crate::control_tools::ControlToolRegistry
                                .definition_list_for_context(Some(context), false)
                                .iter()
                                .any(|tool| tool.name.starts_with("check_"))
                        );
                        let scope = request
                            .control_context
                            .as_ref()
                            .unwrap()
                            .checks
                            .as_ref()
                            .unwrap();
                        let started: Value = serde_json::from_str(
                            &scope.invoke("check_run", json!({"check":"verify"})).await?,
                        )?;
                        loop {
                            let run: Value = serde_json::from_str(
                                &scope
                                    .invoke(
                                        "check_status",
                                        json!({"run_id":started["run_id"],"wait_ms":30000}),
                                    )
                                    .await?,
                            )?;
                            if run["state"] != "running" {
                                break;
                            }
                        }
                    }
                    if let Some(sink) = sink {
                        let address = crate::backend::TurnAddress {
                            scope: fixture_scope.clone(),
                            thread_id: "automatic-check-stream".into(),
                            turn_id: (turn.to_string()).into(),
                        };
                        sink.send_wait(completed_message(
                            &address,
                            "repair-message",
                            crate::backend::MessageRole::Commentary,
                            "Check repair response",
                        ))
                        .await?;
                        sink.send_wait(BackendEvent::Turn {
                            address,
                            update: crate::backend::TurnUpdate::Finished {
                                outcome: crate::turn::TurnOutcome::Completed,
                                completed_at_ms: 100,
                            },
                        })
                        .await?;
                    }
                    Ok(crate::backend::BackendCompletion::default())
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
        }
    }

    #[tokio::test]
    async fn automatic_checks_default_off_until_explicitly_enabled() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.session.shell = crate::session::CheckShell::Nushell;
        broker.backend = Arc::new(AutomaticCheckBackend {
            turns: Arc::new(std::sync::atomic::AtomicUsize::new(0)),
            repair: true,
            explicit: false,
        });
        std::fs::write(repository.path().join(".forge.json"), json!({"checks":{"verify":{"trigger":{"auto":true,"paths":["seed.txt"]},"action":{"command":"print ok","label":"Verify","category":"test"}}}}).to_string())?;
        assert!(!broker.snapshot()?.automatic_checks);
        broker
            .run_interaction(
                "Default off".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Default off".into())),
            )
            .await?;
        assert!(broker.check_runner.snapshot()?.history.is_empty());
        broker
            .configure_session(json!({"automatic_checks":true}))
            .await?;
        assert!(broker.snapshot()?.automatic_checks);
        broker
            .run_interaction(
                "Explicitly enabled".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Explicitly enabled".into())),
            )
            .await?;
        assert_eq!(broker.check_runner.snapshot()?.history.len(), 1);
        broker
            .configure_session(json!({"automatic_checks":false}))
            .await?;
        broker
            .run_interaction(
                "Explicitly disabled".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Explicitly disabled".into())),
            )
            .await?;
        assert_eq!(broker.check_runner.snapshot()?.history.len(), 1);
        Ok(())
    }

    #[tokio::test]
    async fn automatic_checks_complete_when_remaining_queue_is_removed() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .configure_session(json!({"automatic_checks":true}))
            .await?;
        broker.session.approval_mode = ApprovalMode::Allow;
        broker.session.shell = crate::session::CheckShell::Nushell;
        let turns = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        broker.backend = Arc::new(AutomaticCheckBackend {
            turns: turns.clone(),
            repair: false,
            explicit: false,
        });
        std::fs::write(repository.path().join(".forge.json"), json!({"checks":{"verify":{"trigger":{"auto":true,"paths":["seed.txt"]},"action":{"command":"print queue-pass","label":"Verify","category":"test"}}}}).to_string())?;
        assert_eq!(
            broker.snapshot()?.check_timing,
            crate::checks::CheckTiming::AfterQueue
        );
        broker.observer.completion_queue.update(1, true)?;
        broker
            .run_interaction(
                "Queued edit".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Queued edit".into())),
            )
            .await?;
        assert!(broker.snapshot()?.checks_deferred);
        broker.observer.completion_queue.update(2, false)?;
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "session.check.complete".into(),
                params: json!({}),
            })
            .await;
        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
        assert_eq!(broker.check_runner.snapshot()?.history.len(), 1);
        assert!(!broker.snapshot()?.checks_deferred);
        assert_eq!(turns.load(Ordering::SeqCst), 1);
        Ok(())
    }

    #[tokio::test]
    async fn automatic_checks_repair_after_permission_resume_on_worker() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .configure_session(json!({"automatic_checks":true}))
            .await?;
        broker.session.shell = crate::session::CheckShell::Nushell;
        broker.backend = Arc::new(AutomaticCheckBackend {
            turns: Arc::new(std::sync::atomic::AtomicUsize::new(0)),
            repair: true,
            explicit: false,
        });
        std::fs::write(repository.path().join(".forge.json"), json!({"checks":{"verify":{"trigger":{"auto":true,"paths":["seed.txt"]},"action":{"command":"if ((open --raw seed.txt) | str contains fixed) { print ok } else { exit 7 }","label":"Verify","category":"test"}}}}).to_string())?;
        let (mut exchange, _, _) = broker
            .interaction_for_turn("Resumed edit", true, broker.clock.now_ms())
            .await?;
        broker
            .start_exchange_runtime(&mut exchange, broker.clock.now_ms())
            .await?;
        let mut runtime = broker.exchange_runtime.take().unwrap();
        broker
            .collect_initial_checkpoint(&mut runtime, &mut exchange)
            .await?;
        broker.exchange_runtime = Some(runtime);
        exchange.pause(broker.clock.now_ms());
        broker.store.save_exchange(&exchange)?;
        let worker = std::thread::spawn(move || -> Result<()> {
            // Reserve half the normal worker stack for the enclosing host request lifecycle.
            let runtime = tokio::runtime::Builder::new_multi_thread()
                .worker_threads(1)
                .thread_stack_size(1024 * 1024)
                .enable_all()
                .build()?;
            runtime.block_on(async move {
                tokio::spawn(async move {
                    let result = broker
                        .dispatch(Request {
                            id: 1,
                            method: "session.command.apply".into(),
                            params: json!({"action":"permission","change":{"kind":"set_approval","mode":"ask"},"resume":true}),
                        })
                        .await;
                    anyhow::ensure!(
                        result.response.error().is_none(),
                        "{:?}",
                        result.response.error()
                    );
                    anyhow::ensure!(
                        broker
                            .check_runner
                            .history()?
                            .iter()
                            .any(|run| run.state == crate::checks::CheckState::Passed),
                        "resumed repair did not pass"
                    );
                    Ok::<_, anyhow::Error>(())
                })
                .await?
            })
        });
        worker.join().expect("repair worker panicked")
    }

    #[tokio::test]
    async fn automatic_checks_wait_for_goal_completion_and_do_not_consume_questions() -> Result<()>
    {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .configure_session(json!({"automatic_checks":true}))
            .await?;
        std::fs::write(repository.path().join(".forge.json"), json!({"checks":{"verify":{"trigger":{"auto":true},"action":{"command":"print ok","label":"Verify","category":"test"}}}}).to_string())?;
        let checkpoint = broker
            .checkpoint
            .as_ref()
            .unwrap()
            .capture(
                &broker.store.objects,
                &broker.repositories,
                &broker.session.id,
                1,
            )
            .await?;
        broker
            .check_runner
            .begin_changes("goal-gate".into(), &checkpoint)?;
        std::fs::write(repository.path().join("seed.txt"), "changed")?;
        let goal = broker.create_goal("Verify completion".into(), false)?;
        broker
            .check_runner
            .begin_changes(goal.id.clone(), &checkpoint)?;
        let mut exchange = completed_interaction("goal-turn", &broker.session.id, Vec::new());
        exchange.check_owner = Some(goal.id.clone());
        broker.store.save_exchange(&exchange)?;
        broker.reconcile_timeline()?;
        broker
            .settle_automatic_checks((json!({}), Vec::new()), PromptMode::GoalContinuation)
            .await?;
        assert!(broker.check_runner.history()?.is_empty());
        broker.task_controller.change(
            &mut broker.store,
            None,
            crate::task::TaskChange::Pause,
            2,
        )?;
        broker
            .settle_automatic_checks((json!({}), Vec::new()), PromptMode::GoalContinuation)
            .await?;
        assert!(broker.check_runner.history()?.is_empty());
        let active = broker.task_controller.current()?.unwrap();
        broker
            .task_controller
            .update(&mut broker.store, &active.permit(), 3, |task| {
                if let crate::task::TaskWorkflow::Goal { goal } = &mut task.workflow {
                    goal.state = GoalState::Complete;
                }
                Ok(())
            })?;
        exchange.set_awaiting_input(true);
        broker.store.save_exchange(&exchange)?;
        broker
            .settle_automatic_checks((json!({}), Vec::new()), PromptMode::GoalContinuation)
            .await?;
        assert!(broker.check_runner.history()?.is_empty());
        exchange.set_awaiting_input(false);
        broker.store.save_exchange(&exchange)?;
        broker
            .settle_automatic_checks((json!({}), Vec::new()), PromptMode::GoalContinuation)
            .await?;
        assert_eq!(broker.check_runner.history()?.len(), 1);
        assert_eq!(
            broker.check_runner.history()?[0].state,
            crate::checks::CheckState::Passed
        );
        Ok(())
    }

    #[tokio::test]
    async fn interaction_future_has_bounded_inline_storage() {
        let repository = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let future = Box::pin(broker.run_interaction_input(
            "Plan a fixture".into(),
            PromptMode::Plan,
            None,
            BackendInput::from_text("Plan a fixture"),
        ));
        let bytes = std::mem::size_of_val(future.as_ref().get_ref());
        assert!(
            bytes <= 16 * 1024,
            "interaction retains {bytes} inline bytes"
        );
    }

    struct CheckpointBackend {
        preparation: Option<Arc<PreparationProbe>>,
        started: Arc<tokio::sync::Notify>,
        tool_ran: Arc<std::sync::atomic::AtomicBool>,
        active: Arc<std::sync::atomic::AtomicUsize>,
    }

    struct ActiveProviderProbe(Arc<std::sync::atomic::AtomicUsize>);

    impl Drop for ActiveProviderProbe {
        fn drop(&mut self) {
            self.0.fetch_sub(1, Ordering::SeqCst);
        }
    }

    #[async_trait::async_trait]
    impl Backend for CheckpointBackend {
        async fn prepare_turn(&self, _: &str) -> Result<()> {
            if let Some(preparation) = &self.preparation {
                preparation.started.notify_one();
                preparation.release.notified().await;
                anyhow::ensure!(!preparation.fail, "fixture preparation failed");
            }
            Ok(())
        }
        fn checkpoint_tool_gate(&self) -> bool {
            true
        }
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Checkpoint fixture".into(),
                capability: crate::backend::mock_capability(),
            }
        }
        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
        async fn has_active_turn(&self, _: &str) -> bool {
            false
        }
        async fn cleanup_execution(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn cancel_session(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn active_session_id_for(&self, _: &str) -> Option<String> {
            None
        }
        async fn rollback_cancelled_turn_for(&self, _: &str) -> Result<bool> {
            Ok(true)
        }
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = _sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(_sink);
            let _sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let _sink = Some(_sink);
                    self.active.fetch_add(1, Ordering::SeqCst);
                    let _active = ActiveProviderProbe(Arc::clone(&self.active));
                    self.started.notify_one();
                    request
                        .control_context
                        .as_ref()
                        .and_then(|context| context.checkpoint.as_ref())
                        .context("fixture requires a checkpoint barrier")?
                        .wait()
                        .await?;
                    self.tool_ran.store(true, Ordering::SeqCst);
                    std::fs::write(
                        Path::new(&request.workspace).join("seed.txt"),
                        "tool changed workspace\n",
                    )?;
                    Ok(crate::backend::BackendCompletion::default())
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
        }
    }

    #[tokio::test]
    async fn cancellation_rewinds_before_checkpoint_barrier_without_waiting_for_scan() -> Result<()>
    {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let started = Arc::new(Notify::new());
        let tool_ran = Arc::new(AtomicBool::new(false));
        let active = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        broker.backend = Arc::new(CheckpointBackend {
            preparation: None,
            started: Arc::clone(&started),
            tool_ran: Arc::clone(&tool_ran),
            active: Arc::clone(&active),
        });
        broker.capability.native_turn_rollback = true;
        let repositories = Arc::clone(&broker.repositories);
        let handle = repositories
            .open(repository.path().to_owned())
            .await?
            .context("fixture repository")?;
        let operation = repositories.writes.admit(
            vec![forge_git::mutation::MutationScope::WorktreeFiles(
                handle
                    .identity
                    .worktree
                    .clone()
                    .context("fixture worktree")?,
            )],
            0,
        )?;
        let held = repositories
            .writes
            .start(operation)?
            .context("fixture mutation admission")?;
        let cancellation = broker.turn_cancellation();
        for _ in 0..16 {
            cancellation.arm(true);
            let cancel = async {
                started.notified().await;
                cancellation.request(true);
            };
            let execution = broker.run_interaction(
                "Undo this new prompt".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Undo this new prompt".into())),
            );
            let result = tokio::time::timeout(std::time::Duration::from_millis(200), async {
                tokio::join!(execution, cancel).0
            })
            .await;
            let error = result
                .context("cancellation waited for the blocked checkpoint scan")?
                .unwrap_err();
            assert!(error.is::<TurnRetracted>(), "{error:#}");
            assert!(!tool_ran.load(Ordering::SeqCst));
            assert!(broker.store.list_exchange(&broker.session.id)?.is_empty());
            assert!(broker.exchange_runtime.is_none());
            assert_eq!(
                active.load(Ordering::SeqCst),
                0,
                "cancelled provider still owns its turn"
            );
            assert_eq!(
                Arc::strong_count(&active),
                2,
                "cancelled provider retained its lifetime probe"
            );
        }
        held.finish(forge_git::mutation::OperationCompletion::Completed)?;
        repositories.writes.take_receipt(operation);
        assert_eq!(
            std::fs::read_to_string(repository.path().join("seed.txt"))?,
            "seed\n"
        );
        Ok(())
    }

    #[tokio::test]
    async fn native_abort_completion_rewinds_before_pending_checkpoint_finalization() -> Result<()>
    {
        for (cleanup_before_completion, visible_output, restore_prompt) in [
            (true, false, true),
            (false, false, true),
            (true, true, true),
            (false, true, true),
            (true, false, false),
            (false, false, false),
        ] {
            let repository = repository();
            let data = tempfile::tempdir()?;
            let mut broker = planning_question_broker(repository.path(), data.path(), false);
            let backend = Arc::new(CleanupRetryBackend {
                started: Notify::new(),
                complete: Notify::new(),
                settled: Notify::new(),
                attempt: std::sync::atomic::AtomicUsize::new(1),
                visible_output,
            });
            broker.backend = backend.clone();
            broker.capability.native_turn_rollback = true;
            let repositories = Arc::clone(&broker.repositories);
            let handle = repositories
                .open(repository.path().to_owned())
                .await?
                .context("fixture repository")?;
            let operation = repositories.writes.admit(
                vec![forge_git::mutation::MutationScope::WorktreeFiles(
                    handle
                        .identity
                        .worktree
                        .clone()
                        .context("fixture worktree")?,
                )],
                0,
            )?;
            let held = repositories
                .writes
                .start(operation)?
                .context("fixture mutation admission")?;
            let cancellation = broker.turn_cancellation();
            cancellation.arm(true);
            let session_id = broker.session.id.clone();
            let cancel = async {
                backend.started.notified().await;
                if cleanup_before_completion {
                    cancellation.begin_cleanup(false, restore_prompt)?;
                    backend.cleanup_execution(&session_id).await?;
                } else {
                    backend.complete.notify_one();
                    backend.settled.notified().await;
                    tokio::task::yield_now().await;
                    cancellation.begin_cleanup(false, restore_prompt)?;
                }
                cancellation.request(restore_prompt);
                Ok::<_, anyhow::Error>(())
            };
            let execution = broker.run_interaction(
                "Cancel after native dispatch".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat(
                    "Cancel after native dispatch".into(),
                )),
            );
            let result = tokio::time::timeout(std::time::Duration::from_millis(200), async {
                tokio::join!(execution, cancel)
            })
            .await;
            held.finish(forge_git::mutation::OperationCompletion::Completed)?;
            repositories.writes.take_receipt(operation);
            let (result, cancelled) =
                result.context("native abort completion waited for checkpoint finalization")?;
            cancelled?;
            let error = result.unwrap_err();
            let exchange = broker.store.list_exchange(&broker.session.id)?;
            if restore_prompt && !visible_output {
                assert!(error.is::<TurnRetracted>(), "{error:#}");
                assert!(exchange.is_empty());
            } else {
                assert!(error.is::<TurnCancelled>(), "{error:#}");
                assert_eq!(exchange.len(), 1);
                assert_eq!(exchange[0].state(), ExchangeState::Cancelled);
                assert!(exchange[0].checkpoint_before.is_none());
                assert!(exchange[0].checkpoint_after.is_none());
                assert!(exchange[0].finalization_error.is_none());
                if visible_output {
                    assert!(exchange[0].turn.iter().any(|turn| {
                        turn.messages()
                            .iter()
                            .any(|message| message.text() == "Partial response")
                    }));
                }
            }
            assert!(broker.exchange_runtime.is_none());
        }
        Ok(())
    }

    #[tokio::test]
    async fn checkpoint_does_not_delay_prompt_admission_but_blocks_first_tool() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let started = Arc::new(tokio::sync::Notify::new());
        let tool_ran = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let document = forge_buffer::identity::DocumentId("checkpoint-transcript".into());
        {
            let mut presentation = broker.presentation.lock().unwrap();
            presentation.open(
                document.clone(),
                forge_buffer::identity::ViewId("checkpoint-view".into()),
                forge_buffer::width::WidthProfile::default(),
            )?;
            presentation.begin_submission(&document, 1, "Capture before my first tool")?;
        }
        broker.prompt_admission = Some((Some(document), 1));
        let (sink, mut stream) = publication::channel();
        broker.event_sink = Some(sink);
        broker.backend = Arc::new(CheckpointBackend {
            preparation: None,
            started: started.clone(),
            tool_ran: tool_ran.clone(),
            active: Default::default(),
        });
        let repositories = broker.repositories.clone();
        let handle = repositories
            .open(repository.path().to_owned())
            .await?
            .context("fixture repository")?;
        let operation = repositories.writes.admit(
            vec![forge_git::mutation::MutationScope::WorktreeFiles(
                handle
                    .identity
                    .worktree
                    .clone()
                    .context("fixture worktree")?,
            )],
            0,
        )?;
        let held = repositories
            .writes
            .start(operation)?
            .context("fixture mutation admission")?;
        let session_id = broker.session.id.clone();
        let observe = async {
            tokio::time::timeout(std::time::Duration::from_secs(2), started.notified()).await?;
            let mut accepted = false;
            let mut projected = false;
            while let Ok(event) = stream.try_recv() {
                accepted |= matches!(
                    &event,
                    SessionUpdate::Submission {
                        transition: publication::SubmissionUpdate {
                            state: publication::SubmissionState::Accepted,
                            ..
                        }
                    }
                );
                projected |= matches!(&event, SessionUpdate::DocumentChanged { .. });
            }
            assert!(
                accepted,
                "composer acceptance waited for checkpoint capture"
            );
            assert!(
                projected,
                "timeline did not project before checkpoint capture"
            );
            let store = SqliteStore::open(data.path())?;
            let exchange = store.list_exchange(&session_id)?;
            assert_eq!(exchange.len(), 1);
            assert_eq!(exchange[0].prompt, "Capture before my first tool");
            assert!(exchange[0].checkpoint_before.is_none());
            assert!(!tool_ran.load(Ordering::SeqCst));
            assert_eq!(
                std::fs::read_to_string(repository.path().join("seed.txt"))?,
                "seed\n"
            );
            held.finish(forge_git::mutation::OperationCompletion::Completed)?;
            repositories.writes.take_receipt(operation);
            Ok::<_, anyhow::Error>(())
        };
        let (result, observed) = tokio::time::timeout(std::time::Duration::from_secs(10), async {
            tokio::join!(
                broker.run_interaction(
                    "Capture before my first tool".into(),
                    PromptMode::Chat,
                    Some(ExchangeAdmission::chat(
                        "Capture before my first tool".into()
                    ))
                ),
                observe
            )
        })
        .await?;
        observed?;
        result?;
        assert!(tool_ran.load(Ordering::SeqCst));
        let exchange = broker
            .store
            .list_exchange(&session_id)?
            .pop()
            .context("completed fixture exchange")?;
        let before = broker
            .store
            .load_checkpoint(exchange.checkpoint_before.as_deref().context("before id")?)?
            .context("before checkpoint")?;
        assert!(!before.file.iter().any(|file| file.path == "seed.txt"));
        let after = broker
            .store
            .load_checkpoint(exchange.checkpoint_after.as_deref().context("after id")?)?
            .context("after checkpoint")?;
        assert!(after.file.iter().any(|file| file.path == "seed.txt"));
        Ok(())
    }

    #[tokio::test]
    async fn reopened_pause_finalizes_before_executing_the_admitted_prompt() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let session_id = broker.session.id.clone();
        let mut interrupted =
            completed_interaction("stopped-before-checkpoint", &session_id, Vec::new());
        interrupted.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Finalizing,
            interrupted.completed_at_ms(),
            interrupted.finalization_outcome(),
        );
        interrupted.phase = crate::exchange::ExchangePhase::fixture(
            interrupted.state(),
            None,
            interrupted.finalization_outcome(),
        );
        interrupted.phase =
            crate::exchange::ExchangePhase::Finalizing(crate::exchange::ExchangeOutcome::Cancelled);
        broker.store.save_exchange(&interrupted)?;
        broker
            .store
            .save_task_operation(&mut crate::task::TaskOperation {
            revision: 0,
            id: "pause-before-close".into(),
            session_id: session_id.clone(),
            action: json!({"action":"pause"}),
            permission_selection: None,
            state: "outcome_unknown".into(),
            error: Some(
                "Runtime stopped before the operation settled. Inspect task state before resuming"
                    .into(),
            ),
            exchange_id: Some(interrupted.id.clone()),
            created_at_ms: 3,
            resume_after_transition: false,
        })?;
        drop(broker);
        let mut reopened = planning_question_broker(repository.path(), data.path(), false);
        assert_eq!(reopened.session.id, session_id);
        let occupied: Vec<_> = (0..4)
            .map(|_| reopened.repositories.reads.submit(0, |_| Ok(())).unwrap())
            .collect();
        let error = reopened
            .run_interaction(
                "Wait for recovery".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Wait for recovery".into())),
            )
            .await
            .unwrap_err();
        assert!(format!("{error:#}").contains("capacity is full"));
        let pending = reopened.store.list_exchange(&session_id)?;
        assert_eq!(pending.len(), 2);
        assert_eq!(pending[0].state(), ExchangeState::Finalizing);
        assert!(pending[0].finalization_error.is_some());
        assert_eq!(pending[1].state(), ExchangeState::Failed);
        assert!(pending[1].checkpoint_before.is_none());
        for task in occupied {
            task.finish().await?;
        }
        reopened
            .run_interaction(
                "Continue after reopening".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Continue after reopening".into())),
            )
            .await?;
        let history = reopened.store.list_exchange(&session_id)?;
        assert_eq!(history.len(), 3);
        assert_eq!(history[0].state(), ExchangeState::Cancelled);
        assert!(history[0].checkpoint_after.is_some());
        assert!(history[0].finalization_error.is_none());
        assert_eq!(history[1].state(), ExchangeState::Failed);
        assert_eq!(history[2].state(), ExchangeState::Complete);
        let pause = reopened
            .store
            .load_task_operation(&session_id, "pause-before-close")?
            .unwrap();
        assert_eq!(pause.state, "cancelled");
        assert!(pause.error.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn initial_checkpoint_failure_can_finalize_without_replaying_provider_work() -> Result<()>
    {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let occupied: Vec<_> = (0..4)
            .map(|_| broker.repositories.reads.submit(0, |_| Ok(())).unwrap())
            .collect();
        let error = broker
            .run_interaction(
                "blocked acquisition".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("blocked acquisition".into())),
            )
            .await
            .unwrap_err();
        assert!(format!("{error:#}").contains("capacity is full"));
        let failed = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(failed.len(), 1);
        assert_eq!(failed[0].state(), ExchangeState::Finalizing);
        assert!(failed[0].checkpoint_before.is_none());
        for task in occupied {
            task.finish().await?;
        }
        broker.retry_finalization().await?;
        let settled = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(settled[0].state(), ExchangeState::Failed);
        assert_eq!(settled[0].checkpoint_before, settled[0].checkpoint_after);
        assert!(settled[0].turn.is_empty());
        Ok(())
    }

    use crate::agent::AgentExecutionState;
    use crate::session::state_machine::SessionPhase;
    include!("agent_test.rs");
    include!("input_test.rs");
    include!("admission_test.rs");
    async fn settle_managed_children(broker: &mut HarnessBroker) -> Result<Vec<SessionEvent>> {
        let mut revision = broker.agent_coordinator.subscribe();
        let mut event = Vec::new();
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            loop {
                event.extend(broker.drain_agent_commands(None).await?);
                if broker.agent_coordinator.active_count() == 0 {
                    return Ok::<_, anyhow::Error>(());
                }
                revision.changed().await.context("child lane closed")?;
            }
        })
        .await
        .context("managed child did not settle within five seconds")??;
        Ok(event)
    }

    #[tokio::test]
    async fn manual_child_admission_streams_and_followup_reuses_only_child_history() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.session.backend_session_id = Some("root-history".into());
        assert!(!broker.agent_coordinator.enabled());
        let (admitted, _) = broker
            .start_agent(json!({"profile":"worker","text":"CHILD_FIRST"}))
            .await?;
        let agent_id = admitted["agent"]["id"]
            .as_str()
            .context("admission has no child identity")?
            .to_owned();
        assert_eq!(admitted["status"], "admitted");
        let event = settle_managed_children(&mut broker).await?;
        assert!(
            event.iter().any(|event| event.event == "document_changed"
                || event.payload["kind"] == "document_changed"),
            "child events did not update the shared document: {:?}",
            event
                .iter()
                .map(|event| (&event.event, &event.payload["kind"]))
                .collect::<Vec<_>>()
        );
        let child = broker.agent_coordinator.resolve(&agent_id)?;
        assert!(child.provider_thread_id.is_some());
        assert_ne!(child.provider_thread_id.as_deref(), Some("root-history"));
        assert_eq!(
            broker.session.backend_session_id.as_deref(),
            Some("root-history")
        );
        let exchange = broker.store.list_agent_exchange(&agent_id)?;
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].state(), ExchangeState::Complete);
        assert!(
            exchange[0]
                .turn
                .iter()
                .flat_map(|turn| turn.messages())
                .any(|message| message.text().contains("CHILD_FIRST"))
        );
        let sender = broker.agent_coordinator.sender();
        let request = crate::agent::AgentRequest {
            operation_id: "followup".into(),
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::Followup {
                target: agent_id.clone(),
                message: "CHILD_SECOND".into(),
            },
        };
        let response = tokio::spawn(async move { sender.request(request).await });
        tokio::task::yield_now().await;
        broker.drain_agent_commands(None).await?;
        assert_eq!(response.await??["agent"]["id"], agent_id);
        settle_managed_children(&mut broker).await?;
        let exchange = broker.store.list_agent_exchange(&agent_id)?;
        assert_eq!(exchange.len(), 2);
        assert!(
            exchange
                .iter()
                .all(|exchange| exchange.state() == ExchangeState::Complete)
        );
        assert_eq!(broker.agent_coordinator.snapshot(None).len(), 1);
        assert_eq!(broker.store.list_agent_task(&broker.session.id)?.len(), 2);
        Ok(())
    }

    struct DelegatingRootBackend {
        turn: AtomicU64,
    }

    #[async_trait::async_trait]
    impl Backend for DelegatingRootBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock".into(),
                capability: crate::backend::mock_capability(),
            }
        }
        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
        async fn has_active_turn(&self, _session_id: &str) -> bool {
            true
        }
        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }
        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }
        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            Some("parent-history".into())
        }
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(sink);
            let sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {

            let output: anyhow::Result<crate::backend::BackendCompletion> = async {
let sink = Some(sink);
            let turn = self.turn.fetch_add(1, Ordering::AcqRel);
            if turn == 0 {
                let scope = request
                    .control_context
                    .as_ref()
                    .and_then(|context| context.agent.as_ref())
                    .context("root has no authenticated agent lane")?;
                let response = scope
                    .invoke(
                        "agent_spawn",
                        json!({"profile":"worker","text":"CHILD_RESULT"}),
                    )
                    .await?;
                assert!(response.contains("admitted"));
            } else {
                anyhow::ensure!(turn == 1, "parent continuation duplicated");
                anyhow::ensure!(
                    request.backend_session_id.as_deref() == Some("parent-history"),
                    "parent lost its conversation"
                );
                anyhow::ensure!(
                    matches!(&request.input,BackendInput::Text {text, ..} if text.contains("CHILD_RESULT") && text.contains("\"response\"")),
                    "parent continuation omitted child result"
                );
            }
            if let Some(sink) = sink {
                let response = if turn == 0 {
                    "Waiting for child"
                } else {
                    "SYNTHESIZED_RESULT"
                };
                sink.send_wait(completed_message(&crate::backend::test_support::address(&fixture_scope, "fixture", "turn"),
                    "root-response", crate::backend::MessageRole::Response, response,
                ))
                .await?;
            }
            Ok(crate::backend::BackendCompletion { backend_session_id: Some("parent-history".into()), capability: self.descriptor().capability, ..Default::default() })

        }.await;
            Ok(output?)

        }.await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
        }
    }

    #[tokio::test]
    async fn model_child_completes_through_the_active_root_lane_and_continues_the_same_exchange()
    -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let backend = Arc::new(DelegatingRootBackend {
            turn: AtomicU64::new(0),
        });
        broker.backend = backend.clone();
        broker.agent_coordinator.set_enabled(true);
        tokio::time::timeout(
            std::time::Duration::from_secs(5),
            broker.run_interaction(
                "Root request".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Root request".into())),
            ),
        )
        .await??;
        assert_eq!(backend.turn.load(Ordering::Acquire), 2);
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].state(), ExchangeState::Complete);
        assert!(
            exchange[0]
                .turn
                .iter()
                .flat_map(|turn| turn.messages())
                .any(|message| message.text().contains("SYNTHESIZED_RESULT"))
        );
        let task = broker.store.list_agent_task(&broker.session.id)?;
        assert_eq!(task.len(), 1);
        assert!(task[0].parent_reported);
        assert_eq!(task[0].parent_exchange_id, exchange[0].id);
        Ok(())
    }

    #[tokio::test]
    async fn yolo_toggles_sandbox_in_direct_and_admitted_transitions() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        for (backend, sandbox_supported) in [("codex", true), ("copilot", false)] {
            broker.session.backend = backend.into();
            for admitted in [false, true] {
                broker
                    .configure_session(json!({"access":crate::session::AccessPolicy {
                    sandbox: sandbox_supported, ..Default::default()
                }, "approval_mode":"ask"}))
                    .await
                    .unwrap();
                for (sandbox, approval) in [
                    (false, "allow"),
                    (sandbox_supported, "ask"),
                    (false, "allow"),
                ] {
                    let mut action = json!({"change":{"kind":"enable_yolo"}});
                    if admitted {
                        action["permission_selection"] = json!({
                            "sandbox":sandbox,"read_access":"full","write_access":"full","approval_mode":approval
                        });
                    }
                    broker
                        .apply_configuration(
                            action,
                            configuration::ConfigurationCommand::Permission,
                        )
                        .await
                        .unwrap();
                    let stored = broker
                        .store
                        .load_session(&broker.session.id)
                        .unwrap()
                        .unwrap();
                    assert_eq!(stored.access.sandbox, sandbox);
                    assert_eq!(stored.access.abbreviation(), "rxwx");
                    assert_eq!(stored.approval_mode.label().to_lowercase(), approval);
                    assert!(stored.active_task.is_none());
                }
            }
        }
    }

    #[tokio::test]
    async fn sandbox_preferences_resolve_on_load_and_configuration_for_each_provider() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let preferences = crate::settings::PortableSettings {
            access: Some(crate::session::AccessPolicy::default()),
            ..Default::default()
        };
        for provider in ["copilot", "claude", "antigravity", "mock", "codex"] {
            broker.session.backend = provider.into();
            broker.session.access.sandbox = true;
            preferences.apply(&mut broker.session, true).unwrap();
            assert_eq!(broker.session.access.sandbox, provider == "codex");
            broker
                .configure_session(json!({"access":crate::session::AccessPolicy::default()}))
                .await
                .unwrap();
            let stored = broker
                .store
                .load_session(&broker.session.id)
                .unwrap()
                .unwrap();
            assert_eq!(stored.access.sandbox, provider == "codex");
        }
        assert!(preferences.access.unwrap().sandbox);
    }

    #[tokio::test]
    async fn access_configuration_is_atomic_persistent_and_independent_of_approvals() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let writable = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let policy = crate::session::AccessPolicy {
            sandbox: false,
            writable_directory: vec![writable.path().to_string_lossy().into_owned()],
            ..Default::default()
        };
        broker
            .configure_session(json!({"access":policy}))
            .await
            .unwrap();
        let stored = broker
            .store
            .load_session(&broker.session.id)
            .unwrap()
            .unwrap();
        assert!(!stored.access.sandbox);
        assert_eq!(stored.access.writable_directory.len(), 1);
        assert_eq!(stored.approval_mode, ApprovalMode::Ask);
        let preference = broker
            .store
            .load_preference(&stored.workspace, &stored.backend)
            .unwrap()
            .unwrap();
        assert_eq!(preference.access, stored.access);
        broker
            .configure_session(json!({"approval_mode":"allow"}))
            .await
            .unwrap();
        assert_eq!(broker.session.access, stored.access);
        assert!(
            broker
                .configure_session(json!({"default_approval":"allow","effort":""}))
                .await
                .is_err()
        );
        assert_eq!(broker.session.default_approval, ApprovalMode::Ask);
        assert!(broker.configure_session(json!({"access":{"sandbox":true,"write_access":"workspace","writable_directory":["relative"],"windows_sandbox":"elevated"}})).await.is_err());
        assert_eq!(broker.session.access, stored.access);
    }

    #[tokio::test]
    async fn task_replacement_keeps_one_owner_and_clear_records_outcome() {
        use crate::task::TaskKind;
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .create_task(TaskKind::Goal, "First".into(), None, None)
            .unwrap();
        let first = broker.task_controller.current().unwrap().unwrap().revision;
        broker
            .create_task(TaskKind::Goal, "Second".into(), None, None)
            .unwrap();
        let second = broker.task_controller.current().unwrap().unwrap();
        assert_ne!(first, second.revision);
        assert_eq!(second.title(), "Second");
        broker.pause_task(true).await.unwrap();
        assert!(broker.task_controller.current().unwrap().is_none());
        let events = broker.store.list_session_event(&broker.session.id).unwrap();
        assert!(events.iter().any(|event| matches!(&event.detail,
            SessionEventKind::TaskFinished { outcome } if outcome.title == "Second")));
    }

    #[tokio::test]
    async fn paused_task_discussion_does_not_resume_task() -> Result<()> {
        use crate::task::{TaskKind, TaskStatus};
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let goal = broker.create_goal("Persist goal".into(), false)?;
        broker.create_task(TaskKind::Goal, goal.objective.clone(), None, Some(goal.id))?;
        broker.pause_task(false).await?;
        let paused = broker.task_controller.current()?.unwrap();
        let response = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({"text":"Explain the current approach"}),
            })
            .await;
        assert!(
            response.response.error().is_none(),
            "{:?}",
            response.response.error()
        );
        let current = broker.task_controller.current()?.unwrap();
        assert_eq!(current.title(), paused.title());
        assert_eq!(current.revision, paused.revision);
        assert_eq!(current.status(), TaskStatus::Paused);
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].prompt, "Explain the current approach");
        assert_eq!(exchange[0].origin, crate::exchange::ExchangeOrigin::User);
        Ok(())
    }

    #[tokio::test]
    async fn task_permission_change_while_paused_cannot_restart_the_provider() {
        use crate::task::{TaskKind, TaskStatus};
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let goal = broker.create_goal("Persist goal".into(), false).unwrap();
        broker
            .create_task(TaskKind::Goal, goal.objective.clone(), None, Some(goal.id))
            .unwrap();
        broker.pause_task(false).await.unwrap();
        broker
            .transition_task(
                json!({"action":"permission","change":{"kind":"set_approval","mode":"ask"}}),
            )
            .await
            .unwrap();
        let task = broker.task_controller.current().unwrap().unwrap();
        assert_eq!(task.status(), TaskStatus::Paused);
        assert_eq!(broker.session.approval_mode, ApprovalMode::Ask);
        assert!(
            broker
                .store
                .list_exchange(&broker.session.id)
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn completed_clear_receipt_survives_runtime_recovery() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .create_task(
                crate::task::TaskKind::Goal,
                "Interrupted".into(),
                None,
                None,
            )
            .unwrap();
        let receipt = crate::task::TaskOperation {
            revision: 0,
            id: "clear-before-shutdown".into(),
            session_id: broker.session.id.clone(),
            action: json!({"action":"clear"}),
            permission_selection: None,
            state: "completed".into(),
            error: None,
            exchange_id: None,
            created_at_ms: 1,
            resume_after_transition: false,
        };
        let task = broker.task_controller.current().unwrap().unwrap();
        broker
            .task_controller
            .command(
                &mut broker.store,
                &receipt,
                Some(&task.revision),
                crate::task::TaskChange::Clear,
                1,
            )
            .unwrap();
        assert!(broker.task_controller.current().unwrap().is_none());
        broker
            .store
            .interrupt_detached_execution(&broker.session.id)
            .unwrap();
        assert!(
            broker
                .store
                .load_session(&broker.session.id)
                .unwrap()
                .unwrap()
                .active_task
                .is_none()
        );
        let operation = broker
            .store
            .load_task_operation(&broker.session.id, "clear-before-shutdown")
            .unwrap()
            .unwrap();
        assert_eq!(operation.state, "completed");
        assert!(broker.store.list_session_event(&broker.session.id).unwrap().iter().any(|event|
            matches!(&event.detail, SessionEventKind::TaskFinished { outcome } if outcome.title == "Interrupted")));
    }

    #[test]
    fn completed_task_receipt_deduplicates_without_repeating_the_mutation() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .create_task(
                crate::task::TaskKind::Goal,
                "Retain task".into(),
                None,
                None,
            )
            .unwrap();
        let mut receipt = crate::task::TaskOperation {
            revision: 0,
            id: "original".into(),
            session_id: broker.session.id.clone(),
            action: json!({"action":"pause"}),
            permission_selection: None,
            state: "completed".into(),
            error: None,
            exchange_id: None,
            created_at_ms: 1,
            resume_after_transition: false,
        };
        let admitted = broker.task_controller.current().unwrap().unwrap().revision;
        broker
            .task_controller
            .command(
                &mut broker.store,
                &receipt,
                Some(&admitted),
                crate::task::TaskChange::Pause,
                1,
            )
            .unwrap();
        let revision = broker.task_controller.current().unwrap().unwrap().revision;
        broker
            .task_controller
            .command(
                &mut broker.store,
                &receipt,
                Some(&admitted),
                crate::task::TaskChange::Pause,
                2,
            )
            .unwrap();
        assert_eq!(
            broker.task_controller.current().unwrap().unwrap().revision,
            revision
        );
        receipt.action = json!({"action":"clear"});
        assert!(
            broker
                .task_controller
                .command(
                    &mut broker.store,
                    &receipt,
                    Some(&revision),
                    crate::task::TaskChange::Clear,
                    3
                )
                .is_err()
        );
        let receipt = broker
            .store
            .load_task_operation(&broker.session.id, "original")
            .unwrap()
            .unwrap();
        assert_eq!(receipt.state, "completed");
        assert!(
            broker
                .store
                .list_exchange(&broker.session.id)
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn task_failure_remains_inspectable_without_following_a_new_exchange() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"failed-exchange", "session_id":broker.session.id, "agent_id":broker.session.primary_agent_id, "ordinal":1,
            "prompt":"first", "state":"failed", "created_at_ms":1, "completed_at_ms":2,
            "attributed_matches_checkpoint":false, "node_list":[]
        })).unwrap();
        broker.store.save_exchange(&exchange).unwrap();
        let operation = crate::task::TaskOperation {
            revision: 0,
            id: "failed-operation".into(),
            session_id: broker.session.id.clone(),
            action: json!({"action":"plan"}),
            permission_selection: None,
            state: "failed".into(),
            error: Some("provider failed".into()),
            exchange_id: Some(exchange.id.clone()),
            created_at_ms: 1,
            resume_after_transition: false,
        };
        broker.store.admit_task_operation(&operation).unwrap();
        assert_eq!(
            broker.snapshot().unwrap().task_operation.unwrap()["error"],
            "provider failed"
        );
        exchange.id = "next-exchange".into();
        exchange.ordinal = 2;
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Complete,
            exchange.completed_at_ms(),
            exchange.finalization_outcome(),
        );
        exchange.created_at_ms = 3;
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            exchange.state(),
            Some(4),
            exchange.finalization_outcome(),
        );
        broker.store.save_exchange(&exchange).unwrap();
        assert!(broker.snapshot().unwrap().task_operation.is_none());
        assert_eq!(
            broker
                .store
                .load_task_operation(&broker.session.id, &operation.id)
                .unwrap()
                .unwrap()
                .error,
            Some("provider failed".into())
        );
    }

    use super::*;

    fn save_check_exchange(broker: &mut HarnessBroker, id: &str, execution: bool) -> Result<()> {
        let exchange: Exchange = serde_json::from_value(json!({
            "id": id, "session_id": broker.session.id,
            "agent_id": broker.session.primary_agent_id,
            "ordinal": broker.store.list_exchange(&broker.session.id)?.len() + 1,
            "prompt": "Check owner", "state": "complete",
            "kind": if execution { "plan_execution" } else { "chat" },
            "plan_id": execution.then_some("check-plan"),
            "execution_id": execution.then_some("check-execution"),
            "created_at_ms": 1, "completed_at_ms": 2,
            "attributed_matches_checkpoint": true, "node_list": []
        }))?;
        broker.store.save_exchange(&exchange)?;
        broker.reconcile_timeline()?;
        Ok(())
    }

    #[cfg(windows)]
    fn check_fixture(
        repository: &Path,
        data: &Path,
        commands: Vec<String>,
        manual: bool,
    ) -> HarnessBroker {
        let mut broker = planning_question_broker(repository, data, false);
        let mut document = crate::plan::test_fixture("check-plan", "Check fixture");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Check accepted commands".into();
        design.document.background = "A repository fixture".into();
        design.document.design = "Harness owns the command gate".into();
        let config = crate::checks::CheckConfig {
            version: 2,
            definitions: commands
                .into_iter()
                .enumerate()
                .map(|(index, command)| crate::checks::CheckDefinition {
                    auto: true,
                    id: format!("check-{index}"),
                    name: format!("Check {index}"),
                    category: crate::checks::CheckCategory::Check,
                    purpose: crate::checks::CheckPurpose::Verify,
                    enabled: true,
                    area: "Fixture".into(),
                    trigger: crate::checks::CheckTrigger::AnyChange,
                    scope: Default::default(),
                    directory: ".".into(),
                    all: crate::checks::CheckOperation {
                        input: false,
                        args: None,
                        inputs: None,
                        command,
                    },
                    files: None,
                    cases: None,
                    depends_on: Vec::new(),
                })
                .collect(),
        };
        std::fs::write(
            repository.join(".forge.json"),
            serde_json::to_vec(&json!({"checks":config})).unwrap(),
        )
        .unwrap();
        design.document.verification = if manual {
            vec!["Observe manual result".into()]
        } else {
            Vec::new()
        };
        design
            .proposed
            .insert(".gitignore".into(), "/target/\n".into());
        document.design = Some(design);
        let mut plan = broker
            .plan_file
            .prepare_entry(&broker.session.id, "Run checks".into(), Some(document), 1)
            .unwrap()
            .record;
        broker
            .plan_file
            .submit_document_revision(&broker.session.id, "check-plan", 1, 1)
            .unwrap();
        plan.state = PlanState::Accepted;
        plan.model_revision = 1;
        plan.accepted_revision = Some(1);
        plan.submitted_version = Some(plan.document_version);
        broker.store.save_plan(&plan).unwrap();
        std::fs::write(repository.join(".gitignore"), "/target/\n").unwrap();
        let goal = GoalRecord {
            id: "check-goal".into(),
            session_id: broker.session.id.clone(),
            objective: "Run checks".into(),
            state: GoalState::Active,
            continuation: Default::default(),
            native: false,
            created_at_ms: 1,
            updated_at_ms: 1,
        };
        broker.save_session().unwrap();
        let execution:PlanExecutionRecord=serde_json::from_value(json!({
            "id":"check-execution","session_id":broker.session.id,"plan_id":"check-plan","goal_id":goal.id,
            "state":"active","phase":"checking","original_revision":1,"revision":1,"generation":0,
            "lifecycle":[],"findings":[],"progress":crate::plan::execution::SemanticProgress::default(),
            "verification":[],"check_runs":[],"revision_history":[],"review_paused":false,
            "observed_source":{},"observed_generation":0,"observed_check":[],"created_at_ms":1
        })).unwrap();
        broker.store.save_plan_execution(&execution).unwrap();
        let task = crate::task::ActiveTask::execute(
            "Run checks".into(),
            execution,
            crate::session::continuation::ContinuationBudget::new(20, 2),
            false,
            1,
        );
        broker
            .task_controller
            .change(
                &mut broker.store,
                None,
                crate::task::TaskChange::Install(Box::new(task)),
                1,
            )
            .unwrap();
        save_check_exchange(&mut broker, "check-exchange", true).unwrap();
        broker
    }

    #[cfg(windows)]
    fn update_check_execution(broker: &mut HarnessBroker, record: PlanExecutionRecord) {
        let task = broker.task_controller.current().unwrap().unwrap();
        broker
            .task_controller
            .update(&mut broker.store, &task.permit(), 100, |task| {
                let crate::task::TaskWorkflow::Execute { execution } = &mut task.workflow else {
                    anyhow::bail!("check fixture has no selected execution");
                };
                anyhow::ensure!(execution.record.id == record.id, "check execution changed");
                execution.record = record;
                Ok(())
            })
            .unwrap();
    }

    #[cfg(windows)]
    #[tokio::test]
    async fn harness_checks_own_exit_codes_output_and_manual_gate_without_model_turns() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = check_fixture(
            repository.path(),
            data.path(),
            vec![
                "Write-Output 'accepted stdout'; [Console]::Error.WriteLine('accepted stderr')"
                    .into(),
                "exit 7".into(),
                "Write-Output 'last check'".into(),
            ],
            true,
        );
        broker.run_checks().await.unwrap();
        let execution = broker
            .store
            .load_plan_execution("check-execution")
            .unwrap()
            .unwrap();
        assert_eq!(execution.phase, crate::plan::PlanPhase::Resolve);
        let run = &execution.check_runs[0];
        assert_eq!(run.commands[1].exit_code, Some(7));
        assert_eq!(run.commands[2].state, crate::checks::CheckState::Passed);
        assert!(
            std::fs::read_to_string(&run.commands[0].stdout)
                .unwrap()
                .contains("accepted stdout")
        );
        assert!(
            std::fs::read_to_string(&run.commands[0].stderr)
                .unwrap()
                .contains("accepted stderr")
        );
        let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].id, "check-exchange");
        assert!(execution.verification.is_empty());
    }

    #[cfg(windows)]
    #[tokio::test]
    async fn harness_checks_reject_unplanned_public_api_without_manual_verification() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = check_fixture(repository.path(), data.path(), Vec::new(), false);
        let baseline = crate::checkpoint::GitCheckpoint::new(&broker.session.workspace)
            .capture(
                &broker.store.objects,
                &broker.repositories,
                &broker.session.id,
                1,
            )
            .await
            .unwrap();
        broker.store.save_checkpoint(&baseline).unwrap();
        let mut execution = broker
            .store
            .load_plan_execution("check-execution")
            .unwrap()
            .unwrap();
        execution.baseline_checkpoint = Some(baseline.id.clone());
        update_check_execution(&mut broker, execution);
        std::fs::write(
            repository.path().join("extra.rs"),
            "pub fn unplanned() {}\n",
        )
        .unwrap();
        broker.run_checks().await.unwrap();
        let execution = broker
            .store
            .load_plan_execution("check-execution")
            .unwrap()
            .unwrap();
        assert_eq!(execution.phase, crate::plan::PlanPhase::Resolve);
        assert!(!execution.check_runs[0].progress.different.is_empty());
        let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].id, "check-exchange");
    }

    #[cfg(windows)]
    #[tokio::test]
    async fn harness_checks_pass_before_manual_verification() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = check_fixture(
            repository.path(),
            data.path(),
            vec!["Write-Output 'passed'".into()],
            true,
        );
        broker.run_checks().await.unwrap();
        let execution = broker
            .store
            .load_plan_execution("check-execution")
            .unwrap()
            .unwrap();
        assert_eq!(execution.phase, crate::plan::PlanPhase::Verify);
        assert_eq!(
            execution.check_runs[0].state,
            crate::checks::CheckState::Passed
        );
        let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].id, "check-exchange");
    }

    #[cfg(windows)]
    #[tokio::test]
    async fn detached_check_recovery_retains_saved_output_and_pauses_the_gate() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = check_fixture(
            repository.path(),
            data.path(),
            vec!["Write-Output retained".into()],
            true,
        );
        broker.run_checks().await.unwrap();
        let mut execution = broker
            .store
            .load_plan_execution("check-execution")
            .unwrap()
            .unwrap();
        execution.phase = crate::plan::PlanPhase::Checking;
        let run = &mut execution.check_runs[0];
        run.state = crate::checks::CheckState::Running;
        run.commands[0].state = crate::checks::CheckState::Running;
        run.commands[0].output_bytes = 0;
        let output = run.commands[0].output.clone();
        update_check_execution(&mut broker, execution);
        broker
            .store
            .interrupt_detached_execution(&broker.session.id)
            .unwrap();
        let recovered = broker
            .store
            .load_plan_execution("check-execution")
            .unwrap()
            .unwrap();
        assert_eq!(recovered.state, PlanExecutionState::Paused);
        assert_eq!(recovered.phase, crate::plan::PlanPhase::Checking);
        assert_eq!(
            recovered.check_runs[0].state,
            crate::checks::CheckState::Interrupted
        );
        assert_eq!(
            recovered.check_runs[0].commands[0].state,
            crate::checks::CheckState::Interrupted
        );
        assert_eq!(
            recovered.check_runs[0].commands[0].output_bytes,
            std::fs::metadata(&output).unwrap().len()
        );
        assert!(
            std::fs::read_to_string(output)
                .unwrap()
                .contains("retained")
        );
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
    }

    #[cfg(windows)]
    #[tokio::test]
    async fn harness_checks_complete_without_a_manual_exchange_when_no_actions_apply() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = check_fixture(
            repository.path(),
            data.path(),
            vec!["Write-Output complete".into()],
            false,
        );
        broker.run_checks().await.unwrap();
        let execution = broker
            .store
            .load_plan_execution("check-execution")
            .unwrap()
            .unwrap();
        assert_eq!(execution.state, PlanExecutionState::Complete);
        assert!(broker.task_controller.current().unwrap().is_none());
        assert!(execution.check_runs[0].completed_plan.is_some());
        let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].id, "check-exchange");
    }

    #[cfg(windows)]
    #[tokio::test]
    async fn harness_check_cancellation_preserves_results_and_resumes_interrupted_command() {
        for changed in [false, true] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker=check_fixture(repository.path(),data.path(),vec!["New-Item target -ItemType Directory -Force | Out-Null; Add-Content target/completed.txt first".into(),
            "Set-Content target/started.txt started; if (-not (Test-Path target/resume.txt)) { Start-Sleep 30 }; Write-Output resumed".into()],true);
            let cancellation = broker.turn_cancellation();
            let marker = repository.path().join("target/started.txt");
            let cancel = tokio::spawn(async move {
                tokio::time::timeout(std::time::Duration::from_secs(10), async {
                    while !marker.is_file() {
                        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                    }
                })
                .await
                .unwrap();
                cancellation.request(false);
            });
            tokio::time::timeout(std::time::Duration::from_secs(15), broker.run_checks())
                .await
                .unwrap()
                .unwrap();
            cancel.await.unwrap();
            let mut execution = broker
                .store
                .load_plan_execution("check-execution")
                .unwrap()
                .unwrap();
            assert_eq!(execution.state, PlanExecutionState::Paused);
            assert_eq!(
                execution.check_runs[0].commands[1].state,
                crate::checks::CheckState::Interrupted
            );
            // Reuse is intentionally rejected when user changes workspace contents.
            std::fs::write(repository.path().join("target/resume.txt"), "resume").unwrap();
            if changed {
                std::fs::write(repository.path().join("extra.rs"), "fn helper() {}\n").unwrap();
            }
            broker.turn_cancellation.arm(false);
            execution.state = PlanExecutionState::Active;
            broker.store.save_plan_execution(&execution).unwrap();
            let task = broker.task_controller.current().unwrap().unwrap();
            broker
                .task_controller
                .update(&mut broker.store, &task.permit(), 100, |task| {
                    let crate::task::TaskWorkflow::Execute { execution: active } =
                        &mut task.workflow
                    else {
                        anyhow::bail!("expected an execution task");
                    };
                    active.record = execution.clone();
                    Ok(())
                })
                .unwrap();
            broker.run_checks().await.unwrap();
            let execution = broker
                .store
                .load_plan_execution("check-execution")
                .unwrap()
                .unwrap();
            assert_eq!(execution.phase, crate::plan::PlanPhase::Verify);
            assert_eq!(execution.check_runs.len(), if changed { 2 } else { 1 });
            assert_eq!(
                std::fs::read_to_string(repository.path().join("target/completed.txt"))
                    .unwrap()
                    .lines()
                    .count(),
                if changed { 2 } else { 1 }
            );
        }
    }

    #[tokio::test]
    async fn saved_checks_refresh_review_without_changing_design_metadata() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "session.prompt.submit".into(),
                params: json!({"text":"/plan add coverage"}),
            })
            .await;
        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
        run_plan_to_input_boundary(&mut broker).await.unwrap();
        let plan = broker.snapshot().unwrap().active_plan.unwrap();
        let before = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)
            .unwrap();
        std::fs::write(repository.path().join(".forge.json"),serde_json::to_vec(&json!({"checks":{"unit":{"trigger":{"auto":true},"action":{"command":"exit 0","label":"Unit tests","category":"test"}}}})).unwrap()).unwrap();
        assert!(broker.refresh_check_review(&plan.id).unwrap());
        let after = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)
            .unwrap();
        assert_eq!(
            before.design.as_ref().unwrap().document,
            after.design.as_ref().unwrap().document
        );
        assert_eq!(
            after
                .design
                .as_ref()
                .unwrap()
                .check_configuration
                .as_ref()
                .unwrap()
                .config
                .definitions[0]
                .id,
            "unit"
        );
        assert!(!broker.refresh_check_review(&plan.id).unwrap());
    }

    #[tokio::test]
    async fn deleting_selected_plan_tests_preserves_other_cases_and_rejects_stale_confirmation() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "session.prompt.submit".into(),
                params: json!({"text":"/plan add test coverage"}),
            })
            .await;
        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
        run_plan_to_input_boundary(&mut broker).await.unwrap();
        let mut plan = broker.snapshot().unwrap().active_plan.unwrap();
        let mut document = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)
            .unwrap();
        document.design.as_mut().unwrap().document.tests = serde_json::from_value(json!([
            {"file":"src/game.rs","cases":[
                {"name":"first","change":"new","description":"First expected result"},
                {"name":"second","change":"new","description":"Second expected result"}]},
            {"file":"tests/game.rs","cases":[
                {"name":"first","change":"new","description":"Separate file"},
                {"name":"fourth","change":"modified","description":"Fourth result"},
                {"name":"keep","change":"reused","description":"Existing coverage"}]}
        ]))
        .unwrap();
        let (_, _, checksum) = broker
            .plan_file
            .submit_validated_document_revision(
                &broker.session.id,
                &plan.id,
                plan.model_revision,
                document.version,
                document.clone(),
            )
            .unwrap();
        plan.review_digest = Some(checksum.clone());
        broker.publish_plan(&plan).unwrap();
        let source = broker
            .observer
            .capture_plan_review(&plan.id, &checksum)
            .unwrap();
        let review = crate::plan::review_document::ReadOnlyPlanReview::new(
            forge_buffer::identity::DocumentId("delete-tests".into()),
            forge_buffer::identity::ViewId("view".into()),
            source,
            Default::default(),
            None,
        )
        .unwrap();
        let store = crate::plan::review_document::PlanReviewStore::default();
        let admission = store
            .admit(forge_buffer::identity::DocumentId("delete-tests".into()))
            .unwrap();
        let opened = store.insert(review, admission).unwrap();
        assert!(
            opened["source_edit"]["upsert"]
                .as_array()
                .unwrap()
                .iter()
                .any(|row| row["test"]["name"] == "fourth")
        );
        let mut annotation = Vec::new();
        for name in ["keep", "fourth"] {
            let line = opened["source_edit"]["upsert"]
                .as_array()
                .unwrap()
                .iter()
                .find(|row| row["test"]["name"] == name)
                .unwrap()["source_line"]
                .as_u64()
                .unwrap() as u32;
            annotation.push(crate::plan::ReviewAnnotation {
                id: name.into(),
                parent_id: None,
                source: crate::plan::PlanAnnotationInput {
                    start_line: line,
                    end_line: line,
                    body: format!("Review {name}"),
                },
                anchor: None,
                kind: crate::plan::ReviewAnnotationKind::Comment,
                reply: None,
            });
        }
        store
            .save_annotations(
                &forge_buffer::identity::DocumentId("delete-tests".into()),
                opened["saved_source_digest"].as_str().unwrap(),
                annotation,
            )
            .unwrap();
        let selected = json!([
            {"file":"src/game.rs","name":"first"}, {"file":"src/game.rs","name":"second"},
            {"file":"tests/game.rs","name":"first"}, {"file":"tests/game.rs","name":"fourth"}
        ]);
        let params = json!({"plan_id":plan.id,"digest":checksum,"expected_version":document.version,"tests":selected});
        let mut invalid = params.clone();
        invalid["tests"][3]["name"] = json!("missing");
        assert!(broker.delete_plan_tests(invalid).is_err());
        assert_eq!(
            broker
                .plan_file
                .read_working_document(&broker.session.id, &plan.id)
                .unwrap(),
            document
        );
        let request = Request {
            id: 2,
            method: "session.plan.test.delete".into(),
            params: params.clone(),
        };
        let envelope = json!({"id":request.id,"method":request.method,"params":request.params,
            "session_id":broker.session.id});
        let dispatched = broker.dispatch(request).await;
        assert!(
            dispatched.response.error().is_none(),
            "{:?}",
            dispatched.response.error()
        );
        eprintln!(
            "API_CAPTURE {}",
            json!({"request":envelope,"response":dispatched.response})
        );
        let result = dispatched.response.result().unwrap().clone();
        let updated = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)
            .unwrap();
        let tests = &updated.design.as_ref().unwrap().document.tests;
        assert_eq!(tests.len(), 1);
        assert_eq!(tests[0].file, "tests/game.rs");
        assert_eq!(tests[0].cases.len(), 1);
        assert_eq!(tests[0].cases[0].name, "keep");
        assert_eq!(
            document.design.as_ref().unwrap().proposed,
            updated.design.as_ref().unwrap().proposed
        );
        assert_eq!(updated.version, document.version + 1);
        assert_eq!(result["plan"]["model_revision"], plan.model_revision);
        assert_eq!(result["plan"]["user_revision"], plan.user_revision);
        assert_eq!(
            result["plan"]["submitted_version"],
            serde_json::to_value(plan.submitted_version).unwrap()
        );
        assert!(result.get("lifecycle").is_none());
        assert!(
            broker
                .delete_plan_tests(params)
                .unwrap_err()
                .to_string()
                .contains("plan changed")
        );
        assert_eq!(
            broker
                .plan_file
                .read_working_document(&broker.session.id, &plan.id)
                .unwrap(),
            updated
        );
        let reopened: PlanRecord = serde_json::from_value(result["plan"].clone()).unwrap();
        let source = broker
            .observer
            .capture_plan_review(&plan.id, reopened.review_digest.as_deref().unwrap())
            .unwrap();
        assert!(source.rendered.markdown.contains("keep"));
        let review = crate::plan::review_document::ReadOnlyPlanReview::new(
            forge_buffer::identity::DocumentId("after-delete".into()),
            forge_buffer::identity::ViewId("after-view".into()),
            source,
            Default::default(),
            None,
        )
        .unwrap();
        let admission = store
            .admit(forge_buffer::identity::DocumentId("after-delete".into()))
            .unwrap();
        let opened = store.insert(review, admission).unwrap();
        assert_eq!(opened["annotation"].as_array().unwrap().len(), 1);
        assert_eq!(opened["annotation"][0]["id"], "keep");
        let line = opened["annotation"][0]["source"]["start_line"]
            .as_u64()
            .unwrap();
        assert!(
            opened["source_edit"]["upsert"]
                .as_array()
                .unwrap()
                .iter()
                .any(|row| row["source_line"] == line && row["test"]["name"] == "keep")
        );
        assert!(!repository.path().join("src/game.rs").exists());
        broker
            .delete_plan_tests(json!({"plan_id":plan.id,"digest":reopened.review_digest,
            "expected_version":updated.version,"tests":[{"file":"tests/game.rs","name":"keep"}]}))
            .unwrap();
        assert!(
            broker
                .plan_file
                .read_working_document(&broker.session.id, &plan.id)
                .unwrap()
                .design
                .unwrap()
                .document
                .tests
                .is_empty()
        );
        let draft = broker.store.load_plan(&plan.id).unwrap().unwrap();
        let saved_revision = broker
            .plan_file
            .read_submitted_document(&broker.session.id, &plan.id, plan.model_revision)
            .unwrap();
        assert_eq!(saved_revision, document);
        assert!(
            broker
                .plan_file
                .read_submitted_document(&broker.session.id, &plan.id, plan.model_revision + 1)
                .is_err()
        );
        broker.capability.approval_mode_list =
            vec![ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow];
        broker.snapshot().unwrap();
        broker
            .dispatch_session_command(
                "session.plan.acceptance.begin",
                json!({"plan_id":plan.id,"digest":draft.review_digest}),
            )
            .unwrap();
        broker
            .dispatch_session_command("session.plan.acceptance.cancel", json!({}))
            .unwrap();
        let selected_task =
            serde_json::to_value(broker.task_controller.current().unwrap()).unwrap();
        assert!(
            broker
                .plan_file
                .read_submitted_document(&broker.session.id, &plan.id, plan.model_revision + 1)
                .is_err()
        );
        let invalid = broker
            .dispatch(Request {
                id: 2,
                method: "session.plan.accept".into(),
                params: json!({"plan_id":plan.id,"digest":"stale","approval_mode":"ask"}),
            })
            .await;
        assert!(invalid.response.error().is_some());
        assert_eq!(
            serde_json::to_value(broker.task_controller.current().unwrap()).unwrap(),
            selected_task,
            "a rejected plan approval must preserve the active task"
        );
        assert!(
            broker
                .plan_file
                .read_submitted_document(&broker.session.id, &plan.id, plan.model_revision + 1)
                .is_err()
        );
        broker.backend = Arc::new(SemanticExecutionBackend {
            turn: Default::default(),
            revision: plan.model_revision + 1,
        });
        let accepted = broker.dispatch(Request { id: 3, method: "session.plan.accept".into(),
            params: json!({"plan_id":plan.id,"digest":draft.review_digest,"approval_mode":"ask"}) }).await;
        assert!(
            accepted.response.error().is_none(),
            "{:?}",
            accepted.response.error()
        );
        let finalized = broker.store.load_plan(&plan.id).unwrap().unwrap();
        assert_eq!(finalized.model_revision, plan.model_revision + 1);
        assert_eq!(finalized.user_revision, plan.user_revision + 1);
        assert_eq!(finalized.accepted_digest, draft.review_digest);
        assert_eq!(finalized.manual_changes, draft.manual_changes);
        assert_eq!(
            broker
                .plan_file
                .read_submitted_document(&broker.session.id, &plan.id, finalized.model_revision)
                .unwrap(),
            broker
                .plan_file
                .read_working_document(&broker.session.id, &plan.id)
                .unwrap()
        );
        assert_eq!(
            broker
                .store
                .list_plan_lifecycle(&broker.session.id)
                .unwrap()
                .iter()
                .filter(|record| record.kind == PlanLifecycleKind::RevisionCreated)
                .count(),
            1
        );
    }

    struct NoProgressExecutionBackend;

    struct StoppedExecutionBackend {
        cancelled: bool,
    }

    #[async_trait::async_trait]
    impl Backend for StoppedExecutionBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            _: BackendRequest,
            fixture_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = fixture_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(fixture_sink);
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    if self.cancelled {
                        return Err(anyhow::Error::new(TurnCancelled));
                    }
                    anyhow::bail!("provider connection lost")
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
        }
        async fn fork(&self, _: BackendForkRequest) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("test backend cannot fork")
        }
    }

    #[tokio::test]
    async fn task_migration_preserves_plan_and_execution() -> Result<()> {
        for executing in [false, true] {
            let repository = repository();
            let data = tempfile::tempdir()?;
            let mut broker = planning_question_broker(repository.path(), data.path(), false);
            broker.backend = Arc::from(crate::backend::build(
                BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                broker.permission_coordinator.clone(),
                broker.trace.clone(),
                broker.data_root.clone(),
            )?);
            broker
                .submit_prompt(json!({"text":"/plan add a reusable API"}))
                .await?;
            if executing {
                let plan = broker
                    .snapshot()?
                    .active_plan
                    .context("test plan missing")?;
                broker.backend = Arc::new(StoppedExecutionBackend { cancelled: true });
                broker.dispatch_session_command(
                    "session.plan.accept",
                    json!({"plan_id":plan.id,"approval_mode":"ask"}),
                )?;
            }
            let before = broker
                .store
                .load_active_task(&broker.session.id)?
                .context("task missing before migration")?;
            let plan_id = match &before.workflow {
                crate::task::TaskWorkflow::Plan { plan } => plan.artifact.id.clone(),
                crate::task::TaskWorkflow::Execute { execution } => {
                    execution.record.plan_id.clone()
                }
                crate::task::TaskWorkflow::Goal { .. } => unreachable!(),
            };
            let title = broker.store.load_plan(&plan_id)?.unwrap().title;
            let session_id = broker.session.id.clone();
            let path = broker.data_root.join("harness.sqlite3");
            let connection = rusqlite::Connection::open(&path)?;
            connection.execute("UPDATE session_record SET payload=json_set(payload,'$.format_version',51,'$.session.active_task',NULL) WHERE id=?1", [&session_id])?;
            connection.pragma_update(None, "user_version", 3)?;
            drop(connection);
            drop(broker);
            let migrated = SqliteStore::open(data.path())?;
            let after = migrated
                .load_active_task(&session_id)?
                .context("task missing after migration")?;
            assert_eq!(
                serde_json::to_value(&before.workflow)?,
                serde_json::to_value(&after.workflow)?
            );
            assert_eq!(migrated.load_plan(&plan_id)?.unwrap().title, title);
        }
        Ok(())
    }

    #[tokio::test]
    async fn execution_failure_and_cancellation_resume_the_phase_without_accepting_again() {
        for cancelled in [false, true] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = planning_question_broker(repository.path(), data.path(), false);
            broker.backend = Arc::from(
                crate::backend::build(
                    BackendLaunch {
                        kind: "mock".into(),
                        command: vec!["mock".into()],
                    },
                    Arc::clone(&broker.permission_coordinator),
                    Arc::clone(&broker.trace),
                    broker.data_root.clone(),
                )
                .unwrap(),
            );
            broker
                .submit_prompt(json!({"text":"/plan add a reusable API"}))
                .await
                .unwrap();
            run_plan_to_input_boundary(&mut broker).await.unwrap();
            let plan = broker.snapshot().unwrap().active_plan.unwrap();
            broker.backend = Arc::new(StoppedExecutionBackend { cancelled });
            broker
                .dispatch_session_command(
                    "session.plan.accept",
                    json!({"plan_id":plan.id,"approval_mode":"ask"}),
                )
                .unwrap();
            let result = dispatch_pending_task_input(&mut broker).await;
            assert!(result.is_err(), "stopped provider unexpectedly succeeded");
            let history = broker.store.list_exchange(&broker.session.id).unwrap();
            let stopped = history.last().unwrap();
            assert_eq!(
                stopped.state(),
                if cancelled {
                    ExchangeState::Cancelled
                } else {
                    ExchangeState::Failed
                }
            );
            assert!(
                stopped
                    .node_list
                    .iter()
                    .all(|node| !matches!(node, ExchangeNode::VerificationResult { .. }))
            );
            assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
            let text = timeline_text(&broker.snapshot().unwrap());
            assert_eq!(text.contains("Plan failed:"), !cancelled, "{text}");
            assert_eq!(text.matches("Plan accepted:").count(), 1);
            let stopped_id = stopped.id.clone();
            broker.backend = Arc::new(NoProgressExecutionBackend);
            broker
                .dispatch_session_command("session.goal.resume", json!({}))
                .unwrap();
            dispatch_pending_task_input(&mut broker).await.unwrap();
            let resumed = broker.store.list_exchange(&broker.session.id).unwrap();
            assert_ne!(resumed.last().unwrap().id, stopped_id);
            assert!(matches!(
                &resumed.last().unwrap().origin,
                crate::exchange::ExchangeOrigin::TaskStarted { start }
                    if start.kind == crate::task::TaskKind::Execute
            ));
            assert_eq!(
                timeline_text(&broker.snapshot().unwrap())
                    .matches("Plan accepted:")
                    .count(),
                1
            );
        }
    }

    #[async_trait::async_trait]
    impl Backend for NoProgressExecutionBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            fixture_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = fixture_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(fixture_sink);
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let context = request.control_context.unwrap();
                    let plan_id = context.plan_document.as_ref().unwrap().plan_id.clone();
                    let mut runtime = crate::control_tools::ControlToolRuntime::new(context);
                    runtime
                        .invoke(crate::control_tools::ControlToolInvocation {
                            task_permit: None,
                            name: "plan_read".into(),
                            arguments: json!({"plan_id":plan_id}),
                        })
                        .await?;
                    assert!(
                        runtime
                            .invoke(crate::control_tools::ControlToolInvocation {
                                task_permit: None,
                                name: "goal_complete".into(),
                                arguments: json!({"summary":"premature"})
                            })
                            .await
                            .is_err()
                    );
                    Ok(crate::backend::BackendCompletion {
                        evidence: crate::goal::TurnEvidence {
                            tool_called: true,
                            structured_complete: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    })
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
        }
        async fn fork(&self, _: BackendForkRequest) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("test backend cannot fork")
        }
    }

    #[tokio::test]
    async fn semantic_execution_stalls_after_two_read_only_turns_and_resumes_the_same_phase() {
        let repository = repository();
        crate::checks::configure_fixture(repository.path());
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "guard-test".into(),
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
        broker
            .submit_prompt(json!({"text":"/plan add a reusable API"}))
            .await
            .unwrap();
        let plan = broker.snapshot().unwrap().active_plan.unwrap();
        broker.backend = Arc::new(NoProgressExecutionBackend);
        let (_, first) = broker
            .dispatch_session_command(
                "session.plan.accept",
                json!({"plan_id":plan.id,"approval_mode":"ask"}),
            )
            .unwrap();
        assert!(
            first
                .iter()
                .any(|event| event.event == "goal_continue_requested")
        );
        let (_, second) = broker
            .dispatch_session_command("session.command.apply", json!({"action":"continue"}))
            .unwrap();
        assert!(
            !second
                .iter()
                .any(|event| event.event == "goal_continue_requested")
        );
        let stalled = active_execution(&broker);
        assert_eq!(stalled.state, PlanExecutionState::Stalled);
        assert_eq!(stalled.phase, crate::plan::PlanPhase::Implement);
        assert!(stalled.completed_at_ms.is_none());
        assert_eq!(broker.active_goal().unwrap().continuation.turn_count, 2);
        let attempts = broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .into_iter()
            .collect::<Vec<_>>();
        assert_eq!(attempts.len(), 2);
        assert_ne!(attempts[0].id, attempts[1].id);
        assert_eq!(attempts[0].state(), ExchangeState::Complete);
        assert!(
            !attempts[0]
                .node_list
                .iter()
                .any(|node| matches!(node, ExchangeNode::VerificationResult { .. }))
        );
        assert!(matches!(
            &attempts[1].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert_eq!(attempts[0].session_id, attempts[1].session_id);
        broker
            .dispatch_session_command("session.goal.resume", json!({}))
            .unwrap();
        let resumed = active_execution(&broker);
        assert_eq!(resumed.id, stalled.id);
        assert_eq!(resumed.state, PlanExecutionState::Active);
        assert_eq!(resumed.phase, stalled.phase);
        assert_eq!(broker.active_goal().unwrap().continuation.turn_count, 1);
    }

    struct SemanticExecutionBackend {
        turn: std::sync::atomic::AtomicUsize,
        revision: u32,
    }

    #[async_trait::async_trait]
    impl Backend for SemanticExecutionBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(sink);
            let sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {

            let output: anyhow::Result<crate::backend::BackendCompletion> = async {
let sink = Some(sink);
            use crate::control_tools::{ControlToolInvocation, ControlToolRuntime};
            let turn = self.turn.fetch_add(1, Ordering::SeqCst);
            let address = crate::backend::TurnAddress {
        scope: fixture_scope.clone(),
        thread_id: "semantic-execution".into(),
        turn_id: (format!("phase-{turn}")).into(),
    };
            sink.as_ref()
                .unwrap()
                .send_wait(BackendEvent::Turn { address: address.clone(), update: crate::backend::TurnUpdate::Started { started_at_ms: 0 } })
                .await?;
            let context = request
                .control_context
                .clone()
                .context("missing control context")?;
            let document = context.plan_document.clone().unwrap();
            let handoff: Value = serde_json::from_str(
                &document.execution_json(context.execution.as_ref().unwrap().phase)?,
            )
            .unwrap();
            if turn == 0 || turn == 2 {
                assert!(handoff["document"].get("verification").is_none());
                assert!(handoff["document"].get("tests").is_some());
            } else {
                assert!(handoff["document"].get("verification").is_some());
            }
            let mut runtime = ControlToolRuntime::new(context);
            let phase = match turn {
                0 => "implement",
                2 => "resolve",
                _ => "verify",
            };
            let mut arguments = json!({"phase":phase,"revision":self.revision,"summary":"Checked planned behavior"});
            if turn == 0 {
                let rejected = runtime.invoke(ControlToolInvocation { task_permit: None, name: "design_patch".into(), arguments: json!({
                    "plan_id":document.plan_id, "expected_version":document.version, "patch":"*** Begin Patch\n*** End Patch"}) }).await.unwrap_err();
                assert!(rejected.to_string().contains("Resolve"));
                for (path, overview) in &document.design.as_ref().unwrap().proposed {
                    let path = Path::new(&request.workspace).join(path);
                    std::fs::create_dir_all(path.parent().unwrap())?;
                    std::fs::write(path, overview.replace("();", "() {}"))?;
                }
                std::fs::write(
                    Path::new(&request.workspace).join("unexpected.rs"),
                    "fn unexpected() {}",
                )?;
            } else if phase == "verify" {
                let id = format!("check-{turn}");
                sink.as_ref()
                    .unwrap()
                    .send_wait(crate::backend::test_support::tool(
            &address.clone(), id.clone(),
            crate::backend::ToolInvocation { kind: crate::backend::ToolActivityKind::Command, name: "fixture".into(),
                title: "Observe fixture".into(), arguments: serde_json::Value::Null },
            crate::turn::test_support::tool_updates(crate::backend::ToolStatus::Succeeded, Some(
                                if turn == 1 {
                                    "round reset failed"
                                } else {
                                    "all checks passed"
                                }
                                .into(),
                            ), Default::default()),
        ))
                    .await?;
                let checks = document.design.as_ref().unwrap().document.verification.iter().map(String::as_str)
                    .map(str::trim).filter(|line| !line.is_empty()).map(|command| json!({
                        "category":"manual", "label":command,
                        "outcome":if turn == 1 { "failed" } else { "passed" },
                        "summary":if turn == 1 { "Round reset retains score" } else { "All checks passed" },
                        "evidence":[id]
                    })).collect::<Vec<_>>();
                arguments["verification"] = json!({"outcome":if turn == 1 { "failed" } else { "passed" },"checks":checks,"evidence":[id],"findings":if turn == 1 { vec!["Round reset retains score"] } else { Vec::<&str>::new() }});
            }
            if phase == "verify" {
                let read = runtime
                    .invoke(ControlToolInvocation {
                        task_permit: None,
                        name: "plan_read".into(),
                        arguments: json!({"plan_id":document.plan_id}),
                    })
                    .await
                    .unwrap();
                let inspection: Value = serde_json::from_str(&read.message).unwrap();
                let available = inspection["execution"]["verification_evidence"]
                    .as_array()
                    .unwrap();
                let reference = available
                    .iter()
                    .find(|evidence| evidence["id"] == format!("check-{turn}"))
                    .unwrap();
                assert_eq!(reference["title"], "Observe fixture");
                assert_eq!(inspection["execution"]["phase"], "verify");
                let mut invalid = arguments.clone();
                invalid["verification"]["evidence"] = json!(["unobserved-check"]);
                assert!(
                    runtime
                        .invoke(ControlToolInvocation {
                            task_permit: None,
                            name: "plan_phase_done".into(),
                            arguments: invalid
                        })
                        .await
                        .unwrap_err()
                        .to_string()
                        .contains("not a completed tool result")
                );
                if turn == 3 {
                    let mut reused = arguments.clone();
                    reused["verification"]["evidence"] = json!(["check-1"]);
                    assert!(
                        runtime
                            .invoke(ControlToolInvocation {
                                task_permit: None,
                                name: "plan_phase_done".into(),
                                arguments: reused
                            })
                            .await
                            .unwrap_err()
                            .to_string()
                            .contains("reused verification evidence")
                    );
                }
            }
            let invocation = ControlToolInvocation {
                task_permit: None,
                name: "plan_phase_done".into(),
                arguments,
            };
            let first = runtime.invoke(invocation.clone()).await?;
            let repeated = runtime.invoke(invocation).await?;
            assert_eq!(first.message, repeated.message);
            assert!(
                runtime
                    .invoke(ControlToolInvocation {
                        task_permit: None,
                        name: "goal_complete".into(),
                        arguments: json!({"summary":"premature"})
                    })
                    .await
                    .is_err()
            );
            sink.as_ref()
                .unwrap()
                .send_wait(BackendEvent::Usage { address: address.clone(), update: serde_json::from_value(json!({"id":format!("usage-{turn}"), "usage": {
                    "input":100 + turn, "cached_input":10, "reasoning":2, "output":20 + turn
                }, "cumulative": null, "cumulative_total": null}))? })
                .await?;
            sink.as_ref()
                .unwrap()
                .send_wait(BackendEvent::Turn { address: address, update: crate::backend::TurnUpdate::Finished { outcome: crate::turn::TurnOutcome::Completed, completed_at_ms: 100 } })
                .await?;
            Ok(crate::backend::BackendCompletion { backend_session_id: Some("semantic-execution".into()), capability: crate::backend::BackendCapability {
                    approval_mode_list: vec![ApprovalMode::Ask],
                    ..Default::default()
                }, ..Default::default() })

        }.await;
            Ok(output?)

        }.await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
        }
        async fn fork(&self, _: BackendForkRequest) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("test backend cannot fork")
        }
    }

    /// Render persisted timeline state through the native document projection.
    fn timeline_text(snapshot: &BrokerSnapshot) -> String {
        snapshot
            .timeline
            .iter()
            .flat_map(|entry| {
                let rendered = crate::buffer::projection::project(
                    entry,
                    &Default::default(),
                    false,
                    &Default::default(),
                    &Default::default(),
                    &mut crate::buffer::identity::DisplayRegistry::default(),
                )
                .unwrap();
                rendered
                    .entry
                    .block
                    .into_iter()
                    .flat_map(|block| {
                        (0..block.text.row_count())
                            .map(|row| block.text.row(row).unwrap().to_owned())
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>()
            .join("\n")
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

    pub(super) fn completed_interaction(
        id: &str,
        session_id: &str,
        node_list: Vec<ExchangeNode>,
    ) -> Exchange {
        Exchange {
            approval_mode: None,
            finalization_error: None,
            check_owner: None,
            output_store: None,
            archived_output: Default::default(),
            input_message_id: None,
            provider_retry: None,
            agent_id: HarnessSession::primary_agent_id(session_id),
            id: id.into(),
            session_id: session_id.into(),
            ordinal: 1,
            prompt: "test interaction".into(),
            origin: crate::exchange::ExchangeOrigin::User,

            phase: crate::exchange::ExchangePhase::fixture(ExchangeState::Complete, Some(2), None),
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            approval_resolution: Vec::new(),
            check_setup: None,
            turn: Vec::new(),
            created_at_ms: 1,
            entry: Vec::new(),
            tool_counter: Default::default(),
            node_list,
            metrics: crate::exchange::ExchangeMetrics::default(),
            comment: Vec::new(),
            checklist: None,

            question: crate::exchange::ExchangeQuestion::None,
            clock: crate::exchange::ExchangeClock::Paused { elapsed_ms: 1 },
        }
    }

    struct SaturatingBackend {
        completed: bool,
        wait_for_capacity: bool,
        cancelled: Arc<AtomicBool>,
    }

    #[async_trait::async_trait]
    impl Backend for SaturatingBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            _request: BackendRequest,
            event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(event_sink);
            let event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let event_sink = Some(event_sink);
                    let event_sink = event_sink.unwrap();
                    let address = crate::backend::MessageAddress {
                        turn: crate::backend::test_support::address(
                            &fixture_scope,
                            "fixture",
                            "turn",
                        ),
                        message_id: crate::backend::MessageId::Native("saturated-message".into()),
                    };
                    let mut text = String::new();
                    for sequence in 0..384 {
                        let delta = sequence.to_string();
                        text.push_str(&delta);
                        let update = if sequence == 0 {
                            crate::backend::MessageUpdate::Started {
                                classification: crate::backend::MessageClassification::Known(
                                    crate::backend::MessageRole::Response,
                                ),
                                text: delta,
                            }
                        } else {
                            crate::backend::MessageUpdate::Delta { text: delta }
                        };
                        let event = BackendEvent::Message {
                            address: address.clone(),
                            update,
                        };
                        if self.wait_for_capacity {
                            event_sink.send_wait(event).await?;
                        } else {
                            let _ = event_sink.send(event);
                        }
                    }
                    if self.wait_for_capacity {
                        event_sink
                            .send_wait(BackendEvent::Message {
                                address,
                                update: crate::backend::MessageUpdate::Completed { text },
                            })
                            .await?;
                    }
                    if self.completed {
                        Ok(crate::backend::BackendCompletion::default())
                    } else {
                        std::future::pending().await
                    }
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
                    background: None,
                    backend_launch_by_provider: Default::default(),
                    config_dir: None,
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
                    method: "session.goal.set".into(),
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
            assert_eq!(interaction[0].state(), ExchangeState::Failed);
        }
    }

    #[tokio::test]
    async fn burst_delivery_drains_both_queues_and_keeps_the_next_prompt_usable() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
            let session_id = broker.session.id.clone();
            let (sink, mut stream) = publication::channel();
            let dispatch = broker.dispatch_stream(
                Request {
                    id: request_id,
                    method: "session.prompt.submit".into(),
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
                let mut notifications = 0;
                while let Some(event) = stream.recv().await.unwrap() {
                    let channel = if matches!(event, SessionUpdate::DocumentChanged { .. }) {
                        notifications += 1;
                        "document_changed"
                    } else {
                        "backend_event"
                    };
                    let wire = forge_protocol::message::Message::Event(SessionEvent {
                        session_id: session_id.clone(),
                        event: channel.into(),
                        payload: publication::encode_session_update(&event).unwrap(),
                    });
                    serde_json::to_value(wire)
                        .expect("broker notifications must satisfy the editor wire contract");
                }
                assert!(notifications > 0);
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
                .all(|exchange| exchange.state() == ExchangeState::Complete)
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
        clarification: bool,
    }

    #[async_trait::async_trait]
    impl Backend for RestartBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(event_sink);
            let event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let event_sink = Some(event_sink);
                    let invocation = self.invocation.fetch_add(1, Ordering::SeqCst);
                    let mut event = BackendEvent::Turn {
                        address: crate::backend::TurnAddress {
                            scope: fixture_scope.clone(),
                            thread_id: "restart-thread".into(),
                            turn_id: (format!("turn-{invocation}")).into(),
                        },
                        update: crate::backend::TurnUpdate::Started { started_at_ms: 0 },
                    };
                    let sink = event_sink.context("restart test requires event delivery")?;
                    sink.send(event.clone())?;
                    if invocation == 0 {
                        self.cancellation.request_restart();
                        return std::future::pending().await;
                    }
                    assert_eq!(
                        request.backend_session_id.as_deref(),
                        Some("restart-thread")
                    );
                    assert_eq!(request.approval_mode, ApprovalMode::Allow);
                    if self.clarification {
                        assert!(
                            request
                                .input
                                .text()
                                .contains("Why choose staged migration?")
                        );
                        assert!(request.input.text().contains("Pending elicitation:"));
                        assert!(
                            request
                                .input
                                .text()
                                .contains("Do not continue the original request")
                                || request
                                    .input
                                    .text()
                                    .contains("Do not continue or submit the plan")
                        );
                        assert!(!request.input.text().contains("Original request:"));
                    }
                    if let BackendEvent::Turn { update, .. } = &mut event {
                        *update = crate::backend::TurnUpdate::Finished {
                            outcome: crate::turn::TurnOutcome::Completed,
                            completed_at_ms: 100,
                        };
                    }
                    sink.send(event)?;
                    Ok(crate::backend::BackendCompletion {
                        backend_session_id: Some("restart-thread".into()),
                        ..Default::default()
                    })
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
        broker.session.approval_mode = ApprovalMode::Ask;
        broker.backend = Arc::new(RestartBackend {
            cancellation: Arc::clone(&broker.turn_cancellation),
            invocation: std::sync::atomic::AtomicUsize::new(0),
            clarification: false,
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
        assert_eq!(before[0].state(), ExchangeState::Running);
        assert_eq!(before[0].turn.len(), 1);
        assert_eq!(
            before[0].turn[0].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Interrupted,
            }
        );
        assert!(before[0].checkpoint_after.is_none());
        assert_eq!(
            broker.session.backend_session_id.as_deref(),
            Some("restart-thread")
        );
        assert_eq!(
            broker
                .store
                .load_session(&broker.session.id)
                .unwrap()
                .unwrap()
                .backend_session_id
                .as_deref(),
            Some("restart-thread")
        );
        broker.turn_cancellation.arm(false);
        broker
            .transition_task(json!({"action": "permission", "change":{"kind":"set_approval","mode":"allow"}, "resume": true}))
            .await
            .unwrap();
        let after = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(after.len(), 1);
        assert_eq!(after[0].id, before[0].id);
        assert_eq!(after[0].prompt, "Original request");
        assert_eq!(after[0].checkpoint_before, before[0].checkpoint_before);
        assert_eq!(after[0].turn.len(), 2);
        assert_eq!(after[0].turn[0].state(), before[0].turn[0].state());
        assert_eq!(after[0].state(), ExchangeState::Complete);
    }

    #[tokio::test]
    async fn planning_restart_retains_the_plan_and_resumes_its_existing_exchange() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), true);
        broker.backend = Arc::new(RestartBackend {
            cancellation: broker.turn_cancellation(),
            invocation: std::sync::atomic::AtomicUsize::new(0),
            clarification: false,
        });
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "session.prompt.submit".into(),
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
        assert!(!matches!(
            &before.exchange[0].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        broker.backend = Arc::new(PlanningQuestionBackend::new(true));
        broker.turn_cancellation.arm(false);
        broker
            .resume_exchange(json!({"text":"Continue planning"}))
            .await
            .unwrap();
        let after = broker.snapshot().unwrap();
        assert_eq!(after.exchange.len(), 1);
        assert_eq!(after.exchange[0].id, before.exchange[0].id);
        assert_eq!(after.exchange[0].origin, before.exchange[0].origin);
        assert_eq!(
            after.exchange[0].checkpoint_before,
            before.exchange[0].checkpoint_before
        );
        assert_eq!(after.active_plan.unwrap().state, PlanState::AwaitingInput);
    }

    #[tokio::test]
    async fn permission_restart_preserves_pending_clarification_and_checkpoint() {
        for planning in [false, true] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = planning_question_broker(repository.path(), data.path(), true);
            broker.session.approval_mode = ApprovalMode::Ask;
            let prompted = broker
                .dispatch(Request {
                    id: 1,
                    method: "session.prompt.submit".into(),
                    params: json!({"text":if planning {
                        "/plan Design the migration"
                    } else {
                        "Design the migration"
                    }}),
                })
                .await;
            assert!(prompted.response.error().is_none());
            let pending = broker.snapshot().unwrap().active_elicitation.unwrap();
            let question_id = pending.elicitation.current_question().unwrap().id.clone();
            let restart = Arc::new(RestartBackend {
                cancellation: broker.turn_cancellation(),
                invocation: std::sync::atomic::AtomicUsize::new(0),
                clarification: true,
            });
            broker.backend = restart.clone();
            broker.capability = crate::backend::mock_capability();
            let interrupted = broker
                .ask_question(json!({
                    "question_id":question_id,
                    "text":"Why choose staged migration?"
                }))
                .await;
            assert!(interrupted.is_err());
            let before = broker.store.list_exchange(&broker.session.id).unwrap();
            assert_eq!(before.len(), 1);
            assert!(before[0].execution_started_at_ms().is_none());
            broker.turn_cancellation.arm(false);
            broker
                .transition_task(json!({"action":"permission","change":{"kind":"set_approval","mode":"allow"},"resume":true}))
                .await
                .unwrap();
            let snapshot = broker.snapshot().unwrap();
            let after = broker.store.list_exchange(&broker.session.id).unwrap();
            assert_eq!(after.len(), 1);
            assert_eq!(after[0].id, before[0].id);
            assert_eq!(after[0].checkpoint_before, before[0].checkpoint_before);
            assert!(after[0].checkpoint_after.is_none());
            assert_eq!(after[0].turn.len(), before[0].turn.len() + 1);
            assert_eq!(after[0].state(), ExchangeState::Running);
            assert_eq!(restart.invocation.load(Ordering::SeqCst), 2);
            let pending_after = snapshot.active_elicitation.unwrap();
            assert_eq!(pending_after.owner, pending.owner);
            assert_eq!(
                pending_after.elicitation.question_set.id,
                pending.elicitation.question_set.id
            );
            assert_eq!(
                pending_after.elicitation.current_question().unwrap().id,
                question_id
            );
            let clarification = after[0]
                .node_list
                .iter()
                .filter_map(|node| match node {
                    ExchangeNode::ExchangeInput { prompt }
                        if prompt.intent == crate::exchange::InputIntent::Clarification =>
                    {
                        Some(prompt)
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            assert_eq!(clarification.len(), 1);
            assert_eq!(clarification[0].text, "Why choose staged migration?");
            assert_eq!(
                clarification[0]
                    .question
                    .as_ref()
                    .unwrap()
                    .question_id
                    .as_deref(),
                Some(question_id.as_str())
            );
            if planning {
                assert_eq!(
                    snapshot.active_plan.unwrap().state,
                    PlanState::AwaitingInput
                );
                assert_eq!(
                    snapshot.session.active_task.as_ref().unwrap().status(),
                    crate::task::TaskStatus::Waiting
                );
            }
        }
    }

    #[tokio::test]
    async fn configuration_preserves_the_complete_active_task_in_every_lifecycle_state()
    -> Result<()> {
        for status in ["running", "waiting", "paused", "blocked"] {
            let repository = repository();
            let data = tempfile::tempdir()?;
            let mut broker = planning_question_broker(repository.path(), data.path(), true);
            let task = if status == "waiting" {
                let prepared = broker.plan_file.prepare_entry(
                    &broker.session.id,
                    "Preserve the review".into(),
                    None,
                    1,
                )?;
                let mut plan = prepared.record;
                plan.state = PlanState::AwaitingReview;
                broker.store.save_plan(&plan)?;
                crate::task::ActiveTask::plan(plan, 1)
            } else {
                let mut task = crate::task::ActiveTask::goal(
                    "Preserve the goal".into(),
                    crate::session::continuation::ContinuationBudget::new(12, 3),
                    true,
                    1,
                );
                if let crate::task::TaskWorkflow::Goal { goal } = &mut task.workflow {
                    goal.state = match status {
                        "running" => GoalState::Active,
                        "paused" => GoalState::Paused,
                        "blocked" => GoalState::Blocked,
                        _ => unreachable!(),
                    };
                    goal.continuation.turn_count = 2;
                }
                task
            };
            broker.task_controller.change(
                &mut broker.store,
                None,
                crate::task::TaskChange::Install(Box::new(task)),
                1,
            )?;
            let before = serde_json::to_value(broker.task_controller.current()?)?;
            let (configured, events) = broker
                .configure_session(json!({"effort":"default"}))
                .await?;
            assert_eq!(configured["active_task"], before);
            assert_eq!(events[0].payload["session"]["active_task"], before);
            let provider = Arc::new(RestartBackend {
                cancellation: broker.turn_cancellation(),
                invocation: std::sync::atomic::AtomicUsize::new(0),
                clarification: false,
            });
            broker.backend = provider.clone();
            broker.capability = crate::backend::mock_capability();
            for action in [
                json!({"action":"configure","config":{"effort":"high"}}),
                json!({"action":"permission","change":{"kind":"toggle_write_enabled"}}),
                json!({"action":"permission","change":{"kind":"set_approval","mode":"allow"}}),
                json!({"action":"configure","config":{"effort":"low"}}),
            ] {
                broker.transition_task(action.clone()).await?;
                assert_eq!(
                    serde_json::to_value(broker.task_controller.current()?)?,
                    before,
                    "configuration mutated {status} task: {action}"
                );
                assert_eq!(
                    serde_json::to_value(
                        broker
                            .store
                            .load_session(&broker.session.id)?
                            .unwrap()
                            .active_task
                    )?,
                    before,
                    "configuration changed stored {status} task: {action}"
                );
                assert_eq!(
                    serde_json::to_value(broker.snapshot()?.session.active_task)?,
                    before,
                    "configuration changed projected {status} task: {action}"
                );
                assert_eq!(
                    provider.invocation.load(Ordering::SeqCst),
                    0,
                    "configuration resumed an idle provider for {status}: {action}"
                );
                assert!(broker.store.list_exchange(&broker.session.id)?.is_empty());
            }
        }
        Ok(())
    }

    #[tokio::test]
    async fn automatic_resume_excludes_answer_forks() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let broker = planning_question_broker(repository.path(), data.path(), true);
        let source_id = broker.session.id.clone();
        let child = prepare_provider_fork(
            data.path(),
            "answer-client",
            "mock",
            &broker.capability,
            &source_id,
            &json!({"name":"Plan Review answers"}),
            crate::session::SessionPurpose::PlanReviewAnswer,
        )?;
        assert_eq!(
            child.child.purpose,
            crate::session::SessionPurpose::PlanReviewAnswer
        );
        let mut legacy = serde_json::to_value(&broker.session)?;
        legacy.as_object_mut().unwrap().remove("purpose");
        assert_eq!(
            serde_json::from_value::<HarnessSession>(legacy)?.purpose,
            crate::session::SessionPurpose::Conversation
        );
        drop(broker);
        let resumed = planning_question_broker(repository.path(), data.path(), true);
        assert_eq!(resumed.session.id, source_id);
        assert!(resumed.store.load_session(&child.child.id)?.is_some());
        Ok(())
    }

    #[tokio::test]
    async fn permission_change_keeps_idle_questions_waiting_without_provider_work() {
        for planning in [false, true] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = planning_question_broker(repository.path(), data.path(), true);
            let prompted = broker
                .dispatch(Request {
                    id: 1,
                    method: "session.prompt.submit".into(),
                    params: json!({"text":if planning {
                        "/plan Design the migration"
                    } else {
                        "Design the migration"
                    }}),
                })
                .await;
            assert!(prompted.response.error().is_none());
            if planning {
                let task = broker.task_controller.current().unwrap().unwrap();
                let entry = task.entry.as_ref().unwrap().clone();
                assert!(
                    broker
                        .task_controller
                        .acknowledge_entry(
                            &mut broker.store,
                            &task.revision,
                            &entry.id,
                            broker.clock.now_ms()
                        )
                        .unwrap()
                );
                broker.input_purpose = crate::input::InputPurpose::TaskEntry {
                    revision: task.revision.clone(),
                    id: entry.id,
                    start: crate::exchange::TaskStart {
                        kind: task.kind(),
                        title: task.title().to_owned(),
                    },
                };
                broker
                    .submit_prompt_inner(json!({"text":entry.instruction}))
                    .await
                    .unwrap();
                broker.input_purpose = crate::input::InputPurpose::User;
            }
            let before = broker.snapshot().unwrap();
            let restart = Arc::new(RestartBackend {
                cancellation: broker.turn_cancellation(),
                invocation: std::sync::atomic::AtomicUsize::new(0),
                clarification: true,
            });
            broker.backend = restart.clone();
            broker.capability = crate::backend::mock_capability();
            let (event_sink, mut event_stream) = publication::channel();
            broker.event_sink = Some(event_sink);
            let forwarded = tokio::spawn(async move {
                let mut event = Vec::new();
                while let Some(next) = event_stream.recv().await.unwrap() {
                    event.push(next);
                }
                event
            });
            broker
                .transition_task(json!({"action":"permission","change":{"kind":"toggle_write_enabled"},"resume":false}))
                .await
                .unwrap();
            broker.event_sink = None;
            let event = forwarded.await.unwrap();
            let after = broker.snapshot().unwrap();
            assert_eq!(
                after.session.access.write_access,
                crate::session::WriteScope::Off
            );
            assert!(event.iter().any(|event| {
                matches!(event, SessionUpdate::Configuration { access_abbreviation, .. }
                    if access_abbreviation == &after.access_abbreviation)
            }));
            assert_eq!(restart.invocation.load(Ordering::SeqCst), 0);
            assert_eq!(after.exchange.len(), before.exchange.len());
            assert_eq!(after.exchange[0].id, before.exchange[0].id);
            assert_eq!(
                after
                    .active_elicitation
                    .unwrap()
                    .elicitation
                    .question_set
                    .id,
                before
                    .active_elicitation
                    .unwrap()
                    .elicitation
                    .question_set
                    .id
            );
        }
    }

    struct CleanupRetryBackend {
        started: Notify,
        complete: Notify,
        settled: Notify,
        attempt: std::sync::atomic::AtomicUsize,
        visible_output: bool,
    }

    #[async_trait::async_trait]
    impl Backend for CleanupRetryBackend {
        fn checkpoint_tool_gate(&self) -> bool {
            true
        }
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn rollback_cancelled_turn_for(&self, _: &str) -> Result<bool> {
            Ok(true)
        }

        async fn prompt_stream(
            &self,
            _request: BackendRequest,
            sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(sink);
            let sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let sink = Some(sink);
                    let sink = sink.context("cleanup test requires event delivery")?;
                    let address = crate::backend::test_support::address(
                        &fixture_scope,
                        "cleanup-thread",
                        "cleanup-turn",
                    );
                    sink.send(BackendEvent::Turn {
                        address: address.clone(),
                        update: crate::backend::TurnUpdate::Started { started_at_ms: 0 },
                    })?;
                    if self.visible_output {
                        sink.send(completed_message(
                            &address,
                            "partial-response",
                            crate::backend::MessageRole::Response,
                            "Partial response",
                        ))?;
                    }
                    self.started.notify_one();
                    self.complete.notified().await;
                    sink.send(BackendEvent::Turn {
                        address,
                        update: crate::backend::TurnUpdate::Finished {
                            outcome: crate::turn::TurnOutcome::Interrupted,
                            completed_at_ms: 100,
                        },
                    })?;
                    self.settled.notify_one();
                    Ok(crate::backend::BackendCompletion::default())
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
    async fn cancelling_paused_restart_persists_goal_and_exchange_stop() {
        let repository = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .create_goal("Complete implementation".into(), false)
            .unwrap();
        let mut exchange = completed_interaction("paused-restart", &broker.session.id, Vec::new());
        exchange.phase = crate::exchange::ExchangePhase::Running;
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            exchange.state(),
            None,
            exchange.finalization_outcome(),
        );
        broker.store.save_exchange(&exchange).unwrap();
        broker.cancel_settled_execution().await.unwrap();
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        let stopped = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(stopped[0].state(), ExchangeState::Cancelled);
        broker.cancel_settled_execution().await.unwrap();
        assert_eq!(
            broker
                .store
                .list_exchange(&broker.session.id)
                .unwrap()
                .len(),
            1
        );
    }

    #[cfg(windows)]
    #[tokio::test]
    async fn goal_resume_retries_locked_checkpoint_before_admitting_work() {
        use std::os::windows::fs::OpenOptionsExt;

        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .create_goal("Finish implementation".into(), false)
            .unwrap();
        broker
            .task_controller
            .change(&mut broker.store, None, crate::task::TaskChange::Pause, 2)
            .unwrap();
        let before = GitCheckpoint::new(repository.path())
            .capture(
                &broker.store.objects,
                &broker.repositories,
                &broker.session.id,
                1,
            )
            .await
            .unwrap();
        let mut exchange =
            completed_interaction("locked-checkpoint", &broker.session.id, Vec::new());
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Finalizing,
            exchange.completed_at_ms(),
            exchange.finalization_outcome(),
        );
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            exchange.state(),
            None,
            exchange.finalization_outcome(),
        );
        exchange.phase =
            crate::exchange::ExchangePhase::Finalizing(crate::exchange::ExchangeOutcome::Cancelled);
        exchange.checkpoint_before = Some(before.id.clone());
        broker
            .store
            .save_checkpoint_exchange(&before, &exchange)
            .unwrap();
        std::fs::write(
            repository.path().join("seed.txt"),
            "changed after checkpoint\n",
        )
        .unwrap();
        let locked = std::fs::OpenOptions::new()
            .read(true)
            .share_mode(0)
            .open(repository.path().join("seed.txt"))
            .unwrap();
        let error = broker
            .dispatch_session_command("session.goal.resume", json!({}))
            .unwrap_err();
        assert!(format!("{error:#}").contains("seed.txt"));
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        let stored = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(stored.len(), 1);
        assert_eq!(stored[0].state(), ExchangeState::Finalizing);
        assert!(stored[0].finalization_error.is_some());
        drop(locked);

        broker.turn_cancellation.request(false);
        assert!(
            broker
                .dispatch_session_command("session.goal.resume", json!({}))
                .unwrap_err()
                .downcast_ref::<TurnCancelled>()
                .is_some()
        );
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        assert_eq!(
            broker
                .store
                .list_exchange(&broker.session.id)
                .unwrap()
                .len(),
            1
        );
        broker.turn_cancellation.arm(false);
        let (_, event) = broker
            .dispatch_session_command("session.goal.resume", json!({}))
            .unwrap();
        assert_eq!(event[0].event, "goal_changed");
        let stored = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(stored.len(), 2);
        assert_eq!(stored[0].state(), ExchangeState::Cancelled);
        assert!(stored[0].finalization_error.is_none());
        assert!(stored[0].checkpoint_after.is_some());
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
            visible_output: false,
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
                    if exchange[0].state() == ExchangeState::Finalizing
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
        assert_eq!(exchange[0].state(), ExchangeState::Cancelled);
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
        broker.session = broker
            .store
            .rename_owned_session(&broker.session.id, &broker.client_id, "", None, 100)
            .unwrap();
        broker
            .interaction_for_turn("  Explain\n this repository  ", true, 100)
            .await
            .unwrap();
        assert_eq!(broker.session.name, "Explain this repository");
        assert_eq!(
            broker
                .store
                .load_session(&broker.session.id)
                .unwrap()
                .unwrap()
                .name,
            "Explain this repository"
        );
        broker
            .interaction_for_turn("Another prompt", true, 101)
            .await
            .unwrap();
        assert_eq!(broker.session.name, "Explain this repository");
        broker.session.name = "My explicit name".into();
        broker
            .interaction_for_turn("Do not replace this name", true, 102)
            .await
            .unwrap();
        assert_eq!(broker.session.name, "My explicit name");
    }

    #[tokio::test]
    async fn first_slash_prompt_names_session_from_its_request() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.session = broker
            .store
            .rename_owned_session(&broker.session.id, &broker.client_id, "", None, 100)
            .unwrap();
        broker
            .interaction_for_turn("/plan convert to rust", true, 100)
            .await
            .unwrap();
        assert_eq!(broker.session.name, "convert to rust");
        assert_eq!(
            broker
                .store
                .load_session(&broker.session.id)
                .unwrap()
                .unwrap()
                .name,
            "convert to rust"
        );
    }

    #[tokio::test]
    async fn first_slash_prompt_caps_persisted_session_name_at_thirty_characters() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.session.name.clear();
        broker
            .interaction_for_turn(
                "/plan convert a rather long repository from TypeScript to Rust",
                true,
                100,
            )
            .await
            .unwrap();
        assert_eq!(broker.session.name, "convert a rather long reposito");
        assert_eq!(
            broker
                .store
                .load_session(&broker.session.id)
                .unwrap()
                .unwrap()
                .name,
            "convert a rather long reposito"
        );
    }

    #[tokio::test]
    async fn restart_collects_pending_initial_checkpoint_and_preserves_the_baseline_on_resume() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut exchange, _, _) = broker
            .interaction_for_turn("retain this request", true, 100)
            .await
            .unwrap();
        let baseline = broker
            .checkpoint
            .as_ref()
            .unwrap()
            .capture(
                &broker.store.objects,
                &broker.repositories,
                &broker.session.id,
                100,
            )
            .await
            .unwrap();
        let baseline_id = baseline.id.clone();
        let gate = crate::checkpoint::ToolGate::new().unwrap();
        gate.complete(Err(
            "provider turn ended before checkpoint admission completed".into(),
        ))
        .unwrap();
        let (complete, completed) = tokio::sync::oneshot::channel();
        broker.exchange_runtime = Some(ExchangeRuntime {
            task_permit: None,
            checkpoint: Some(crate::checkpoint::CheckpointTask(tokio::spawn(
                async move {
                    completed
                        .await
                        .context("initial checkpoint fixture owner disappeared")?
                },
            ))),
            tool_gate: Some(gate.clone()),
            exchange_id: exchange.id.clone(),

            checklist: ChecklistTracker::default(),
            retraction_eligible: true,
            active_wait: None,
        });
        {
            let collecting = broker.park_exchange_for_restart(&mut exchange);
            tokio::pin!(collecting);
            assert!(futures_util::poll!(&mut collecting).is_pending());
            assert!(!gate.ready());
            complete.send(Ok(baseline)).unwrap();
            collecting.await.unwrap();
        }
        assert!(!gate.ready());
        assert!(
            gate.wait()
                .await
                .unwrap_err()
                .to_string()
                .contains("provider turn ended")
        );
        assert_eq!(
            exchange.checkpoint_before.as_deref(),
            Some(baseline_id.as_str())
        );
        assert_eq!(
            broker.store.list_exchange(&broker.session.id).unwrap()[0].checkpoint_before,
            Some(baseline_id.clone())
        );
        assert!(broker.exchange_runtime.is_none());
        assert!(exchange.execution_started_at_ms().is_none());
        std::fs::write(
            repository.path().join("seed.txt"),
            "changed after baseline\n",
        )
        .unwrap();
        broker.resume_exchange_runtime(&exchange).unwrap();
        broker
            .collect_active_initial_checkpoint(&mut exchange)
            .await
            .unwrap();
        exchange.resume(102).unwrap();
        broker
            .capture_final_checkpoint(&mut exchange, crate::exchange::ExchangeOutcome::Complete)
            .await
            .unwrap();
        assert_eq!(
            exchange.checkpoint_before.as_deref(),
            Some(baseline_id.as_str())
        );
        assert_ne!(exchange.checkpoint_after, exchange.checkpoint_before);
        let persisted = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(persisted.len(), 1);
        assert_eq!(persisted[0].state(), ExchangeState::Complete);
        assert_eq!(
            persisted[0].checkpoint_before.as_deref(),
            Some(baseline_id.as_str())
        );
    }

    #[tokio::test]
    async fn restart_checkpoint_failure_propagates_without_releasing_tools() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut exchange, _, _) = broker
            .interaction_for_turn("retain the failed barrier", true, 100)
            .await
            .unwrap();
        let gate = crate::checkpoint::ToolGate::new().unwrap();
        broker.exchange_runtime = Some(ExchangeRuntime {
            task_permit: None,
            checkpoint: Some(crate::checkpoint::CheckpointTask(tokio::spawn(async {
                anyhow::bail!("initial checkpoint fixture failed")
            }))),
            tool_gate: Some(gate.clone()),
            exchange_id: exchange.id.clone(),

            checklist: ChecklistTracker::default(),
            retraction_eligible: true,
            active_wait: None,
        });
        let failure = broker
            .park_exchange_for_restart(&mut exchange)
            .await
            .unwrap_err();
        assert!(
            failure
                .to_string()
                .contains("initial checkpoint fixture failed")
        );
        assert!(
            gate.wait()
                .await
                .unwrap_err()
                .to_string()
                .contains("initial checkpoint fixture failed")
        );
        assert!(!gate.ready());
        assert!(exchange.checkpoint_before.is_none());
        assert_eq!(exchange.state(), ExchangeState::Finalizing);
        assert!(
            exchange
                .finalization_error
                .as_deref()
                .unwrap()
                .contains("initial checkpoint fixture failed")
        );
        let persisted = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(persisted[0].state(), ExchangeState::Finalizing);
        assert_eq!(persisted[0].finalization_error, exchange.finalization_error);
        assert!(
            broker
                .exchange_runtime
                .as_ref()
                .unwrap()
                .checkpoint
                .is_none()
        );
    }

    #[tokio::test]
    async fn conversation_rollback_preserves_files_and_restarts_with_retained_context() -> Result<()>
    {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut first = completed_interaction("retained", &broker.session.id, Vec::new());
        first.prompt = "KEEP_THIS_CONTEXT".into();
        let mut second = completed_interaction("removed", &broker.session.id, Vec::new());
        second.ordinal = 2;
        second.prompt = "DROP_THIS_CONTEXT".into();
        assert!(second.checkpoint_before.is_none());
        broker.store.save_exchange(&first)?;
        broker.store.save_exchange(&second)?;
        broker.session.backend_session_id = Some("old-provider-history".into());
        broker.save_session()?;
        std::fs::write(
            repository.path().join("seed.txt"),
            "Keep these file changes\n",
        )?;
        std::fs::write(
            repository.path().join("new-file.txt"),
            "Keep this new file\n",
        )?;
        let mut params =
            json!({"scope":"conversation","exchange_id":"removed","latest_exchange_id":"stale"});
        assert!(broker.apply_rollback(params.clone()).await.is_err());
        assert_eq!(
            broker.store.list_exchange(&broker.session.id)?[1].disposition(),
            crate::exchange::HistoryDisposition::Current
        );
        params["latest_exchange_id"] = json!("removed");
        let (result, events) = broker.apply_rollback(params).await?;
        assert_eq!(result["scope"], "conversation");
        assert!(
            events
                .iter()
                .any(|event| event.event == "interaction_rolled_back")
        );
        assert_eq!(
            std::fs::read_to_string(repository.path().join("seed.txt"))?,
            "Keep these file changes\n"
        );
        assert_eq!(
            std::fs::read_to_string(repository.path().join("new-file.txt"))?,
            "Keep this new file\n"
        );
        assert!(
            crate::checkpoint::RestoreJournal::progress(&broker.store.objects, &broker.session.id)?
                .is_none()
        );
        assert!(
            broker
                .store
                .load_session(&broker.session.id)?
                .unwrap()
                .backend_session_id
                .is_none()
        );
        assert!(
            !broker.snapshot()?.timeline.iter().any(
                |entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == "removed")
            )
        );
        let input =
            broker.rollback_context("new-request", BackendInput::from_text("CURRENT_REQUEST"))?;
        let BackendInput::Text { text, .. } = input else {
            unreachable!()
        };
        assert!(text.contains("KEEP_THIS_CONTEXT") && text.contains("CURRENT_REQUEST"));
        assert!(!text.contains("DROP_THIS_CONTEXT"));
        Ok(())
    }

    #[tokio::test]
    async fn consecutive_rollbacks_follow_restored_workspace_history() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut exchange_id = Vec::new();
        for content in ["first\n", "second\n"] {
            let (mut exchange, _, _) = broker
                .interaction_for_turn(content, true, 100)
                .await
                .unwrap();
            broker
                .start_exchange_runtime(&mut exchange, 100)
                .await
                .unwrap();
            let mut runtime = broker.exchange_runtime.take().unwrap();
            broker
                .collect_initial_checkpoint(&mut runtime, &mut exchange)
                .await
                .unwrap();
            broker.exchange_runtime = Some(runtime);
            std::fs::write(repository.path().join("seed.txt"), content).unwrap();
            broker
                .capture_final_checkpoint(&mut exchange, crate::exchange::ExchangeOutcome::Complete)
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
        let visible = broker.snapshot().unwrap().timeline;
        assert!(visible.iter().any(
            |entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == &exchange_id[0])
        ));
        assert!(
            !visible.iter().any(
                |entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == &exchange_id[1])
            ),
            "rollback must remove the reverted request instead of leaving a timeline tombstone"
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
        assert_eq!(history[0].state(), ExchangeState::Complete);
        assert_eq!(
            history[0].disposition(),
            crate::exchange::HistoryDisposition::RolledBack
        );
        assert_eq!(
            history[1].disposition(),
            crate::exchange::HistoryDisposition::Superseded
        );
        assert!(
            !broker
                .snapshot()
                .unwrap()
                .timeline
                .iter()
                .any(|entry| matches!(entry, TimelineEntry::Exchange { .. })),
            "rolled-back and superseded requests must stay out of the rebuilt timeline"
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
        parent.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Running,
            parent.completed_at_ms(),
            parent.finalization_outcome(),
        );
        parent.phase = crate::exchange::ExchangePhase::fixture(
            parent.state(),
            None,
            parent.finalization_outcome(),
        );
        parent
            .append_delegation(&child.id, "spawn", "Review", 1)
            .unwrap();
        broker.store.save_exchange(&parent).unwrap();
        let queued = broker.store.list_agent_exchange(&child.id).unwrap();
        assert_eq!(queued.len(), 1);
        assert_eq!(queued[0].state(), ExchangeState::Queued);
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
        assert_eq!(recovered[0].state(), ExchangeState::Interrupted);
        assert!(recovered[0].turn.is_empty());
        reopened
            .require_settled_exchange_tree(&broker.session.id, &[parent.id])
            .unwrap();
    }

    struct FailingBackend;

    #[async_trait::async_trait]
    impl Backend for FailingBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = _event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(_event_sink);
            let _event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let _event_sink = Some(_event_sink);
                    let nested = Path::new(&request.workspace).join("failed/deep/write.txt");
                    std::fs::create_dir_all(nested.parent().context("failed write parent")?)?;
                    std::fs::write(&nested, "written before provider failure\n")?;
                    anyhow::bail!("synthetic provider failure")
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            _request: BackendRequest,
            event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(event_sink);
            let event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let event_sink = Some(event_sink);
                    if let Some(event_sink) = event_sink {
                        let _ = event_sink.send(completed_message(
                            &crate::backend::test_support::address(
                                &fixture_scope,
                                "fixture",
                                "turn",
                            ),
                            "corrected-plan",
                            crate::backend::MessageRole::Response,
                            "Validated the corrected plan.",
                        ));
                    }
                    Ok(crate::backend::BackendCompletion {
                        plan_read: Some("rejected-plan".into()),
                        ..Default::default()
                    })
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(event_sink);
            let event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let event_sink = Some(event_sink);
                    let event_list = vec![
                        completed_message(
                            &crate::backend::test_support::address(
                                &fixture_scope,
                                "fixture",
                                "turn",
                            ),
                            "intermediate-answer",
                            crate::backend::MessageRole::Response,
                            "An intermediate answer.",
                        ),
                        vec![BackendEvent::ParentBoundary {
                            scope: fixture_scope.clone(),
                            update: crate::backend::ParentBoundaryUpdate::WaitStarted {
                                agent_count: 1,
                            },
                        }],
                        vec![BackendEvent::ParentBoundary {
                            scope: fixture_scope.clone(),
                            update: crate::backend::ParentBoundaryUpdate::WaitFinished,
                        }],
                        completed_message(
                            &crate::backend::test_support::address(
                                &fixture_scope,
                                "fixture",
                                "turn",
                            ),
                            "final-synthesis",
                            crate::backend::MessageRole::Response,
                            "A final synthesis.",
                        ),
                    ];
                    if let Some(event_sink) = event_sink {
                        for event in event_list {
                            let _ = event_sink.send(event);
                        }
                    }
                    Ok(crate::backend::BackendCompletion {
                        backend_session_id: Some("parent-boundary".into()),
                        runtime: crate::backend::BackendRuntime {
                            provider: "Parent boundary test".into(),
                            model: Some(request.model),
                        },
                        ..Default::default()
                    })
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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

    struct PlanDiscussionBackend {
        turn: std::sync::atomic::AtomicUsize,
    }

    #[async_trait::async_trait]
    impl Backend for PlanDiscussionBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            fixture_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = fixture_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(fixture_sink);
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {

            let output: anyhow::Result<crate::backend::BackendCompletion> = async {
            use crate::control_tools::{
                ControlToolInvocation, ControlToolRuntime, apply_invocation,
            };
            let turn = self.turn.fetch_add(1, Ordering::SeqCst);
            assert_eq!(
                request.mode,
                if turn == 4 {
                    PromptMode::Plan
                } else {
                    PromptMode::PlanDiscussion
                }
            );
            assert_eq!(request.approval_mode, ApprovalMode::Ask);
            if turn < 4 {
                assert!(
                    request
                        .input
                        .text()
                        .contains("Answer questions without editing or resubmitting")
                );
            }
            let context = request.control_context.clone().unwrap();
            let document = context.plan_document.as_ref().unwrap().clone();
            let mut runtime = ControlToolRuntime::new(context);
            let mut output = crate::backend::BackendCompletion::default();
            let read = runtime
                .invoke(ControlToolInvocation {
                    task_permit: None,
                    name: "plan_read".into(),
                    arguments: json!({"plan_id":document.plan_id}),
                })
                .await?;
            apply_invocation(&read.invocation.unwrap(), &mut output)?;
            match turn {
                0 | 2 => {}
                1 => {
                    output.plan_question = Some(PlanQuestionSet::freeform("Which controls?".into()))
                }
                3 => {
                    let edit = runtime.invoke(ControlToolInvocation { task_permit: None,
                        name: "design_patch".into(),
                        arguments: json!({"plan_id":document.plan_id, "expected_version":document.version,
                            "patch":"*** Begin Patch\n*** Add File: src/controls.rs\n+pub fn keyboard_movement();\n+Algorithm\n+  update movement from keyboard input\n*** End Patch"}),
                    }).await?;
                    apply_invocation(&edit.invocation.unwrap(), &mut output)?;
                }
                4 => {
                    let submit = runtime.invoke(ControlToolInvocation { task_permit: None,
                        name: "plan_submit".into(),
                        arguments: json!({"plan_id":document.plan_id, "expected_version":document.version}),
                    }).await?;
                    apply_invocation(&submit.invocation.unwrap(), &mut output)?;
                }
                _ => panic!("unexpected planning continuation {turn}"),
            }
            Ok(output)

        }.await;
            Ok(output?)

        }.await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
        }
    }

    #[tokio::test]
    async fn plan_queries_preserve_the_selected_task_and_pending_review() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .submit_prompt_inner(json!({"text":"/plan preserve review ownership"}))
            .await
            .unwrap();
        run_plan_to_input_boundary(&mut broker).await.unwrap();
        let current = broker.snapshot().unwrap().active_plan.unwrap();
        let mut older = current.clone();
        older.id = "older-review".into();
        broker.store.save_plan(&older).unwrap();
        let selected = serde_json::to_value(broker.task_controller.current().unwrap()).unwrap();
        for params in [
            json!({"plan_id":older.id}),
            json!({"plan_id":older.id,"revision":older.model_revision}),
        ] {
            let result = broker
                .dispatch(Request {
                    id: 1,
                    method: "session.plan.get".into(),
                    params,
                })
                .await;
            assert!(
                result.response.error().is_none(),
                "{:?}",
                result.response.error()
            );
            assert_eq!(result.response.result().unwrap()["id"], older.id);
            assert_eq!(
                broker.current_plan_id().unwrap().as_deref(),
                Some(current.id.as_str())
            );
            assert_eq!(
                serde_json::to_value(broker.task_controller.current().unwrap()).unwrap(),
                selected
            );
        }
        for (method, params) in [
            ("session.plan.get", json!({"plan_id":"missing"})),
            ("session.preview", json!({"session_id":"missing"})),
            (
                "session.plan.declaration.get",
                json!({"plan_id":"missing","path":"src/lib.rs","revision":1}),
            ),
        ] {
            let result = broker
                .dispatch(Request {
                    id: 2,
                    method: method.into(),
                    params,
                })
                .await;
            assert!(result.response.error().is_some(), "{method}");
            assert_eq!(
                serde_json::to_value(broker.task_controller.current().unwrap()).unwrap(),
                selected,
                "{method}"
            );
            assert_eq!(
                broker.snapshot().unwrap().active_plan.unwrap().state,
                PlanState::AwaitingReview
            );
        }
        assert!(broker.observer.plan(json!({"plan_id":older.id})).is_ok());
        assert_eq!(
            broker.current_plan_id().unwrap().as_deref(),
            Some(current.id.as_str())
        );
    }

    #[tokio::test]
    async fn failed_metadata_requests_preserve_an_unrelated_goal_task() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .create_task(
                crate::task::TaskKind::Goal,
                "Unrelated work".into(),
                None,
                None,
            )
            .unwrap();
        let selected = serde_json::to_value(broker.task_controller.current().unwrap()).unwrap();
        for (method, params) in [
            ("session.preview", json!({"session_id":"missing"})),
            ("session.plan.get", json!({"plan_id":"missing"})),
            ("session.rename", json!({})),
            ("session.exchange.comment.save", json!({})),
        ] {
            let result = broker
                .dispatch(Request {
                    id: 1,
                    method: method.into(),
                    params,
                })
                .await;
            assert!(result.response.error().is_some(), "{method}");
            assert_eq!(
                serde_json::to_value(broker.task_controller.current().unwrap()).unwrap(),
                selected,
                "{method}"
            );
        }
    }

    #[tokio::test]
    async fn submitted_plan_discussion_preserves_review_then_resumes_revision_after_questions() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .submit_prompt_inner(json!({"text":"/plan build a scene"}))
            .await
            .unwrap();
        let before = broker.snapshot().unwrap().active_plan.unwrap();
        let backend = Arc::new(PlanDiscussionBackend {
            turn: std::sync::atomic::AtomicUsize::new(0),
        });
        broker.backend = backend.clone();
        broker
            .submit_prompt_inner(json!({"text":"Why this design?"}))
            .await
            .unwrap();
        let snapshot = broker.snapshot().unwrap();
        let discussed = snapshot.active_plan.as_ref().unwrap();
        assert_eq!(discussed.state, PlanState::AwaitingReview);
        assert_eq!(discussed.model_revision, before.model_revision);
        assert_eq!(discussed.document_version, before.document_version);
        assert_eq!(discussed.review_digest, before.review_digest);
        assert_eq!(snapshot.active_plan.as_ref().unwrap().id, before.id);
        assert!(!matches!(
            &snapshot.exchange.last().unwrap().origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));

        broker
            .submit_prompt_inner(json!({"text":"Add interactive controls"}))
            .await
            .unwrap();
        let interaction = broker.active_interaction_elicitation().unwrap();
        let question_id = interaction.elicitation().unwrap().question_set.questions[0]
            .id
            .clone();
        broker
            .ask_question(json!({"question_id":question_id,"text":"What do you recommend?"}))
            .await
            .unwrap();
        assert_eq!(
            broker.snapshot().unwrap().active_plan.unwrap().state,
            PlanState::AwaitingReview
        );
        broker
            .answer_question(json!({"question_id":question_id,
            "response":{"kind":"other","text":"Keyboard controls"}}))
            .unwrap();
        broker.continue_question().await.unwrap();
        let snapshot = broker.snapshot().unwrap();
        let revised = snapshot.active_plan.unwrap();
        assert_eq!(revised.id, before.id);
        assert_eq!(revised.state, PlanState::AwaitingReview);
        assert_eq!(revised.model_revision, before.model_revision + 1);
        assert_eq!(revised.document_version, before.document_version + 1);
        let exchange = snapshot.exchange.last().unwrap();
        assert_ne!(exchange.id, interaction.id);
        assert!(matches!(
            &exchange.origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert!(!matches!(
            &exchange.origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert_eq!(exchange.state(), ExchangeState::Complete);
        assert!(!exchange.awaiting_input());
        assert_eq!(backend.turn.load(Ordering::SeqCst), 5);
    }

    #[tokio::test]
    async fn review_question_answers_capture_final_responses_without_revising_the_source() {
        use crate::plan::review_document::ReadOnlyPlanReview;
        use forge_buffer::identity::{DocumentId, InputSequence, ViewId};
        use forge_buffer::input::DocumentInput;
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.backend = crate::backend::build(
            broker.backend_launch.clone(),
            Arc::clone(&broker.permission_coordinator),
            Arc::clone(&broker.trace),
            broker.data_root.clone(),
        )
        .unwrap()
        .into();
        broker
            .submit_prompt_inner(json!({"text":"/plan revise the API"}))
            .await
            .unwrap();
        run_plan_to_input_boundary(&mut broker).await.unwrap();
        let before = broker.snapshot().unwrap().active_plan.unwrap();
        let thread = broker.session.backend_session_id.clone();
        let source = broker
            .plan_file
            .capture_review_source(
                &broker.session.id,
                &before.id,
                Some(before.model_revision),
                before.review_digest.as_deref().unwrap(),
            )
            .unwrap();
        let line = source
            .rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| {
                matches!(
                    anchor.target,
                    crate::plan::PlanReviewTarget::Declaration { .. }
                )
            })
            .unwrap()
            .line;
        let digest = source.saved_digest.clone();
        let id = DocumentId("review-question".into());
        let view = ViewId("view".into());
        let review = ReadOnlyPlanReview::new(
            id.clone(),
            view.clone(),
            source,
            forge_buffer::width::WidthProfile::default(),
            None,
        )
        .unwrap();
        let snapshot = review.snapshot();
        let target = snapshot.block[0].metadata.target[0].clone();
        let input = DocumentInput {
            document: id.clone(),
            revision: snapshot.revision,
            view,
            sequence: InputSequence(1),
            action: "session.plan.question.answer".into(),
            block: snapshot.block[0].id.clone(),
            position: target.range.start,
            target: Some(target.id),
        };
        let admission = broker.plan_review.admit(id.clone()).unwrap();
        broker.plan_review.insert(review, admission).unwrap();
        let params = json!({"review":input,"draft_source_digest":digest,"draft_annotation":[{
            "id":"question","kind":"question","source":{"start_line":line,"end_line":line,"body":"Why this API?"}
        }]});
        let mut other = before.clone();
        other.id = "different-current-plan".into();
        broker.store.save_plan(&other).unwrap();
        broker
            .create_task(
                crate::task::TaskKind::Plan,
                "Different task".into(),
                Some(other.id.clone()),
                None,
            )
            .unwrap();
        let selected_task =
            serde_json::to_value(broker.task_controller.current().unwrap()).unwrap();
        let count = broker.snapshot().unwrap().exchange.len();
        let (document, mut questions, _) = broker.prepare_review_questions(&params).unwrap();
        assert_eq!(questions.len(), 1);
        let mut reordered = params.clone();
        reordered["review"]["sequence"] = json!(3);
        assert_eq!(
            broker.prepare_review_questions(&reordered).unwrap().1.len(),
            1
        );
        reordered["review"]["sequence"] = json!(2);
        assert_eq!(
            broker.prepare_review_questions(&reordered).unwrap().1.len(),
            1
        );
        let scope = crate::plan::review_answer::ReviewAnswerScope::new(
            Arc::clone(&broker.plan_review),
            document.clone(),
            questions.remove(0),
        );
        let context = crate::control_tools::ControlTurnContext::inactive(PromptMode::PlanQuestion);
        let names = crate::control_tools::ControlToolRegistry
            .definition_list_for_context(Some(&context), true)
            .into_iter()
            .map(|tool| tool.name)
            .collect::<Vec<_>>();
        assert!(!names.contains(&"plan_review_answer"));
        assert!(!names.contains(&"design_patch"));
        assert!(!names.contains(&"question_ask"));
        assert!(
            broker.plan_review.annotations(&document).unwrap()[0]
                .reply
                .is_none()
        );
        assert!(scope.submit("   ".into()).is_err());
        scope
            .submit("The declaration preserves the caller contract.".into())
            .unwrap();
        let snapshot = broker.snapshot().unwrap();
        assert_eq!(snapshot.exchange.len(), count);
        assert_eq!(snapshot.active_plan.as_ref().unwrap().id, other.id);
        assert_eq!(
            serde_json::to_value(broker.task_controller.current().unwrap()).unwrap(),
            selected_task
        );
        assert_eq!(broker.session.backend_session_id, thread);
        assert_eq!(
            broker.plan_review.annotations(&document).unwrap()[0]
                .reply
                .as_ref()
                .unwrap()
                .body,
            "The declaration preserves the caller contract."
        );
        let mut params = params;
        params["review"]["sequence"] = json!(2);
        assert!(
            broker
                .prepare_review_questions(&params)
                .unwrap()
                .1
                .is_empty()
        );
        assert!(
            broker
                .configure_ask_model(json!({"model":"missing"}))
                .await
                .is_err()
        );
        let original_model = broker.session.model.clone();
        let mut resolved_source = broker.session.clone();
        resolved_source.model = "default".into();
        resolved_source.resolved_model = Some("resolved-planning-model".into());
        assert_eq!(
            crate::settings::AskModel::default()
                .resolve(&resolved_source, &[])
                .unwrap()
                .model
                .as_deref(),
            Some("resolved-planning-model")
        );
        broker
            .configure_ask_model(
                json!({"model":"mock-model","effort":"high","service_tier":"fast"}),
            )
            .await
            .unwrap();
        assert_eq!(broker.session.model, original_model);
        assert_eq!(
            broker.snapshot().unwrap().ask_model.effort.as_deref(),
            Some("high")
        );

        params["review"]["sequence"] = json!(3);
        params["draft_annotation"][0]["id"] = json!("retry-question");
        params["draft_annotation"][0]["source"]["body"] = json!("Explain the caller contract.");
        let (document, mut questions, selection) =
            broker.prepare_review_questions(&params).unwrap();
        assert!(scope.check().is_err());
        let scope = crate::plan::review_answer::ReviewAnswerScope::new(
            Arc::clone(&broker.plan_review),
            document.clone(),
            questions.remove(0),
        );
        let count = broker.snapshot().unwrap().exchange.len();
        let (progress_sink, mut progress_stream) = forge_protocol::outbound::channel();
        broker
            .run_review_question(Arc::clone(&scope), &selection, Some((&progress_sink, 53)))
            .await
            .unwrap();
        let progress: Value =
            serde_json::from_slice(progress_stream.try_recv().unwrap().bytes()).unwrap();
        assert_eq!(progress["request_id"], 53);
        assert_eq!(progress["event"], "session.plan.question.answer.progress");
        assert_eq!(progress["payload"]["question_id"], "retry-question");
        assert_eq!(progress["payload"]["model"], "mock-model");
        assert_eq!(progress["payload"]["effort"], "high");
        assert_eq!(broker.snapshot().unwrap().exchange.len(), count + 1);
        assert!(
            broker.plan_review.annotations(&document).unwrap()[0]
                .reply
                .is_some()
        );
    }

    fn submit_test_plan(
        request: &BackendRequest,
        output: &mut crate::backend::BackendCompletion,
        overview: &str,
    ) {
        let document = request
            .control_context
            .as_ref()
            .and_then(|context| context.plan_document.as_ref())
            .expect("planning context retains the design");
        let design = document.design.as_ref().expect("declaration design");
        let path = "src/migration.rs";
        let text = format!(
            "/// {}\npub fn migrate();\nAlgorithm\n  return\n",
            overview.replace('\n', " ")
        );
        let patch = if design.proposed.contains_key(path) {
            let document = design.read(Some(path), false).unwrap();
            let previous = document["text"].as_str().unwrap();
            format!(
                "*** Begin Patch\n*** Update File: {path}\n@@\n{}{}*** End Patch",
                previous
                    .lines()
                    .map(|line| format!("-{line}\n"))
                    .collect::<String>(),
                text.lines()
                    .map(|line| format!("+{line}\n"))
                    .collect::<String>()
            )
        } else {
            format!(
                "*** Begin Patch\n*** Add File: {path}\n{}*** End Patch",
                text.lines()
                    .map(|line| format!("+{line}\n"))
                    .collect::<String>()
            )
        };
        let previous_metadata = serde_json::to_string_pretty(&design.document)
            .unwrap()
            .lines()
            .map(|line| format!("-{line}\n"))
            .collect::<String>();
        let mut metadata = design.document.clone();
        metadata.objective = "Implement the migration strategy.".into();
        metadata.background = "The fixture contains the original implementation.".into();
        metadata.requirements = vec!["Preserve the migration contract.".into()];
        metadata.design = overview.into();
        let proposed_metadata = serde_json::to_string_pretty(&metadata)
            .unwrap()
            .lines()
            .map(|line| format!("+{line}\n"))
            .collect::<String>();
        let patch = patch.replace("*** End Patch", &format!("*** Update File: plan.json\n@@\n{previous_metadata}{proposed_metadata}*** End Patch"));
        let change = crate::plan::DesignPatchRequest {
            plan_id: document.plan_id.clone(),
            expected_version: document.version,
            patch,
            title: Some("Migration plan".into()),
            source_digests: Default::default(),
        };
        let changed = document
            .patch_design(Path::new(&request.workspace), change.clone())
            .unwrap();
        output.design_patch.push(change);
        output.plan_submit = Some(crate::backend::PlanSubmitRequest {
            plan_id: document.plan_id.clone(),
            expected_version: changed.version,
        });
    }

    #[async_trait::async_trait]
    impl Backend for MutableQuestionBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = _event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(_event_sink);
            let _event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let _event_sink = Some(_event_sink);
                    let question_set = || PlanQuestionSet {
                        id: "migration-set".into(),
                        questions: vec![crate::plan::PlanQuestion {
                            id: "migration".into(),
                            header: "Migration".into(),
                            question: "Which migration should the implementation use?".into(),
                            options: vec![
                                crate::plan::PlanQuestionOption {
                                    recommended: false,
                                    label: "Staged".into(),
                                    description: "Preserve compatibility temporarily.".into(),
                                },
                                crate::plan::PlanQuestionOption {
                                    recommended: false,
                                    label: "Immediate".into(),
                                    description: "Replace the old format now.".into(),
                                },
                            ],
                            allow_freeform: true,
                        }],
                    };
                    let request_text = request.input.text();
                    let mut output = crate::backend::BackendCompletion {
                        backend_session_id: Some("mutable-question".into()),
                        capability: BackendCapability::default(),
                        runtime: crate::backend::BackendRuntime {
                            provider: "Mutable question test".into(),
                            model: Some(request.model.clone()),
                        },
                        ..Default::default()
                    };
                    if request_text.contains("User follow-up:\nreplace the options") {
                        let mut replacement = question_set();
                        replacement.questions[0].options = vec![
                            crate::plan::PlanQuestionOption {
                                recommended: false,
                                label: "Safe".into(),
                                description: "Retain compatibility.".into(),
                            },
                            crate::plan::PlanQuestionOption {
                                recommended: false,
                                label: "Fast".into(),
                                description: "Prefer immediate cleanup.".into(),
                            },
                        ];
                        output.plan_question = Some(replacement);
                    } else if request_text.contains("User follow-up:\nuse staged") {
                        output.question_answer = Some(PlanQuestionAnswer {
                            source: crate::session::ResolutionSource::User,
                            question_id: "migration".into(),
                            response: PlanQuestionResponse::Selected {
                                option: "Staged".into(),
                                feedback: None,
                            },
                        });
                    } else if request_text
                        .contains("User follow-up:\nthis decision is not material")
                    {
                        output.question_withdrawal = Some(PlanQuestionWithdrawal {
                            reason: "Repository policy determines the migration.".into(),
                        });
                    } else if request_text.contains("Pending planning questions withdrawn")
                        || request_text.contains("Planning feedback:\n- Migration: Staged")
                        || request_text.contains("Harness resolved the pending questions")
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
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = _event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(_event_sink);
            let _event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let _event_sink = Some(_event_sink);
                    let turn = self.turn.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    let mut output = crate::backend::BackendCompletion {
                        backend_session_id: Some("repeated-question".into()),
                        capability: BackendCapability::default(),
                        runtime: crate::backend::BackendRuntime {
                            provider: "Repeated question test".into(),
                            model: Some(request.model.clone()),
                        },
                        ..Default::default()
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
                                        recommended: false,
                                        label: "Staged".into(),
                                        description: "Preserve compatibility temporarily.".into(),
                                    },
                                    crate::plan::PlanQuestionOption {
                                        recommended: false,
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
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = _event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(_event_sink);
            let _event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let _event_sink = Some(_event_sink);
                    let turn = self.turn.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    let mut output = crate::backend::BackendCompletion {
                        backend_session_id: Some("retry-plan".into()),
                        capability: crate::backend::mock_capability(),
                        runtime: crate::backend::BackendRuntime {
                            provider: "Retry plan test".into(),
                            model: Some(request.model.clone()),
                        },
                        ..Default::default()
                    };
                    if turn >= 2 {
                        submit_test_plan(
                            &request,
                            &mut output,
                            "Submitted after an explicit retry.",
                        );
                    }
                    Ok(output)
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            _event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = _event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(_event_sink);
            let _event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let _event_sink = Some(_event_sink);
                    let mut output = crate::backend::BackendCompletion {
                        backend_session_id: Some("repeated-plan-document".into()),
                        capability: BackendCapability::default(),
                        runtime: crate::backend::BackendRuntime {
                            provider: "Repeated plan document test".into(),
                            model: Some(request.model.clone()),
                        },
                        ..Default::default()
                    };
                    submit_test_plan(&request, &mut output, "The same canonical plan.");
                    output.plan_submit = None;
                    Ok(output)
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(event_sink);
            let event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let event_sink = Some(event_sink);
                    let turn = self.turn.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    let text = if turn == 0 {
                        "Which migration strategy should the plan use?"
                    } else {
                        "The plan is ready for review."
                    };
                    let event = completed_message(
                        &crate::backend::test_support::address(&fixture_scope, "fixture", "turn"),
                        "planning-response",
                        crate::backend::MessageRole::Response,
                        text,
                    );
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
                                    recommended: false,
                                    label: "Staged".into(),
                                    description: "Support both formats temporarily.".into(),
                                },
                                crate::plan::PlanQuestionOption {
                                    recommended: false,
                                    label: "Immediate".into(),
                                    description: "Remove the old format now.".into(),
                                },
                            ],
                            allow_freeform: true,
                        }],
                    });
                    let mut output = crate::backend::BackendCompletion {
                        backend_session_id: Some("planning-question".into()),
                        plan_question,
                        capability: BackendCapability::default(),
                        runtime: crate::backend::BackendRuntime {
                            provider: "Planning question test".into(),
                            model: Some(request.model.clone()),
                        },
                        ..Default::default()
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
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(event_sink);
            let event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let event_sink = Some(event_sink);
                    let commentary = completed_message(
                        &crate::backend::test_support::address(&fixture_scope, "fixture", "turn"),
                        "nested-commentary",
                        crate::backend::MessageRole::Commentary,
                        "Creating the nested module.",
                    );
                    let provider_change = crate::backend::ProviderChangeSet {
                        file: vec![crate::backend::ProviderFileChange {
                            path: "seed.txt".into(),
                            move_path: None,
                            kind: crate::backend::ProviderChangeKind::Update,
                            diff: "@@ -1 +1 @@\n-seed\n+provider edit".into(),
                        }],
                    };
                    let file_started = crate::backend::test_support::tool(
                        &crate::backend::test_support::address(&fixture_scope, "fixture", "turn"),
                        "provider-file-change".into(),
                        crate::backend::ToolInvocation {
                            kind: crate::backend::ToolActivityKind::FileChange,
                            name: "fixture".into(),
                            title: "file changes".into(),
                            arguments: serde_json::Value::Null,
                        },
                        crate::turn::test_support::tool_updates(
                            crate::backend::ToolStatus::Running,
                            None,
                            provider_change.clone(),
                        ),
                    );
                    let started = crate::backend::test_support::tool(
                        &crate::backend::test_support::address(&fixture_scope, "fixture", "turn"),
                        "nested-command".into(),
                        crate::backend::ToolInvocation {
                            kind: crate::backend::ToolActivityKind::Command,
                            name: "fixture".into(),
                            title: "create nested module".into(),
                            arguments: serde_json::Value::Null,
                        },
                        crate::turn::test_support::tool_updates(
                            crate::backend::ToolStatus::Running,
                            None,
                            crate::backend::ProviderChangeSet::default(),
                        ),
                    );
                    if let Some(event_sink) = event_sink.as_ref() {
                        let _ = event_sink.send(commentary.clone());
                        let _ = event_sink.send(file_started.clone());
                    }
                    std::fs::write(
                        Path::new(&request.workspace).join("seed.txt"),
                        "provider edit\n",
                    )?;
                    let file_completed = crate::backend::test_support::tool(
                        &crate::backend::test_support::address(&fixture_scope, "fixture", "turn"),
                        "provider-file-change".into(),
                        crate::backend::ToolInvocation {
                            kind: crate::backend::ToolActivityKind::FileChange,
                            name: "fixture".into(),
                            title: "file changes".into(),
                            arguments: serde_json::Value::Null,
                        },
                        crate::turn::test_support::tool_updates(
                            crate::backend::ToolStatus::Succeeded,
                            None,
                            provider_change,
                        ),
                    );
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
                    let completed = crate::backend::test_support::tool(
                        &crate::backend::test_support::address(&fixture_scope, "fixture", "turn"),
                        "nested-command".into(),
                        crate::backend::ToolInvocation {
                            kind: crate::backend::ToolActivityKind::Command,
                            name: "fixture".into(),
                            title: "create nested module".into(),
                            arguments: serde_json::Value::Null,
                        },
                        crate::turn::test_support::tool_updates(
                            crate::backend::ToolStatus::Succeeded,
                            Some("created nested module".into()),
                            crate::backend::ProviderChangeSet::default(),
                        ),
                    );
                    let response = completed_message(
                        &crate::backend::test_support::address(&fixture_scope, "fixture", "turn"),
                        "nested-response",
                        crate::backend::MessageRole::Response,
                        "The nested module is ready.",
                    );
                    if let Some(event_sink) = event_sink {
                        let _ = event_sink.send(completed.clone());
                        let _ = event_sink.send(response.clone());
                    }
                    Ok(crate::backend::BackendCompletion {
                        backend_session_id: Some("nested-write".into()),
                        capability: BackendCapability::default(),
                        runtime: crate::backend::BackendRuntime {
                            provider: "Nested test".into(),
                            model: Some(request.model),
                        },
                        ..Default::default()
                    })
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = event_sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(event_sink);
            let event_sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let event_sink = Some(event_sink);
                    let task_event = BackendEvent::Checklist {
                        address: crate::backend::test_support::address(
                            &fixture_scope,
                            "fixture",
                            "turn",
                        ),
                        update: crate::backend::ProviderChecklistUpdate {
                            scope_id: "ordinary-turn".into(),
                            name: None,
                            complete: false,
                            replace_entries: true,
                            entry_list: vec![crate::backend::ProviderChecklistEntry {
                                provider_id: None,
                                content: "Inspect ordinary chat".into(),
                                priority: None,
                                status: crate::backend::ChecklistStatus::InProgress,
                                provider_ordinal: 0,
                            }],
                        },
                    };
                    let response = completed_message(
                        &crate::backend::test_support::address(&fixture_scope, "fixture", "turn"),
                        "ordinary-response",
                        crate::backend::MessageRole::Response,
                        "Ordinary chat completed.",
                    );
                    if let Some(event_sink) = event_sink {
                        let _ = event_sink.send(task_event.clone());
                        let _ = event_sink.send(response.clone());
                    }
                    Ok(crate::backend::BackendCompletion {
                        backend_session_id: Some("task-update".into()),
                        capability: BackendCapability::default(),
                        runtime: crate::backend::BackendRuntime {
                            provider: "Task test".into(),
                            model: Some(request.model),
                        },
                        ..Default::default()
                    })
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
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

    #[tokio::test]
    async fn plan_settings_preserve_the_planning_model_and_reject_invalid_executor_choices() {
        let workspace = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: workspace.path().to_string_lossy().into_owned(),
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
        let rejected = broker
            .configure_session(json!({
                "plan_executor_model": "unknown-model", "plan_executor_effort": "high"
            }))
            .await;
        assert!(rejected.is_err());
        assert!(broker.session.plan_executor.model.is_none());
        broker
            .configure_session(json!({
                "plan_executor_model": "mock-model", "plan_executor_effort": "high",
                "plan_executor_enabled": true, "plan_compact": true
            }))
            .await
            .unwrap();
        assert_eq!(broker.session.model, "mock-model");
        assert_eq!(broker.session.effort, "low");
        broker.session.backend_session_id = Some("mock-session".into());
        let (_, events) = broker.compact_session(true).await.unwrap();
        assert!(
            events
                .iter()
                .any(|event| event.event == "context_compacted")
        );
        assert_eq!(broker.session.effort, "low");
        let preference = broker
            .store
            .load_preference(&broker.session.workspace, "mock")
            .unwrap()
            .unwrap();
        assert_eq!(preference.effort, "low");
        assert!(preference.plan_compact);
        assert!(preference.plan_executor.enabled);
        broker.session.effort = "high".into();
        broker.save_session().unwrap();
        drop(broker);
        let next = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: workspace.path().to_string_lossy().into_owned(),
                client_id: "next-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "medium".into(),
                session_id: None,
                new_session_name: Some("next".into()),
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(200)),
        )
        .unwrap();
        assert_eq!(next.session.effort, "low");
        assert!(next.session.plan_executor.enabled);
    }

    #[test]
    fn rejects_a_second_live_session_controller() {
        let mut session = HarnessSession {
            purpose: crate::session::SessionPurpose::Conversation,
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
            plan_executor: Default::default(),
            plan_compact: false,
            plan_auto_approve_revisions: true,
            approval_timeout: Default::default(),
            answer_timeout: Default::default(),
            context_window: None,
            service_tier: crate::backend::ServiceTier::Standard,
            access: Default::default(),
            shell: Default::default(),
            approval_mode: ApprovalMode::Ask,
            active_task: None,
            default_approval: ApprovalMode::Ask,
            plan_approval: None,
            created_at_ms: 0,
            updated_at_ms: 0,

            lease_owner: Some("one".into()),
            native_fork: false,
            native_compact: false,
            context_usage: None,
            background: None,
            auto_compaction: Default::default(),
            last_compaction_turn: None,
        };
        assert!(session.acquire_lease("two").is_err());
        assert!(session.acquire_lease("one").is_ok());
        session.lease_owner = None;
        assert!(session.acquire_lease("two").is_ok());
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
                access: Default::default(),
                shell: Default::default(),
                default_approval: ApprovalMode::Ask,
                plan_approval: None,
                model: "other-model".into(),
                effort: "high".into(),
                model_setting,
                service_tier: crate::backend::ServiceTier::Standard,
                plan_executor: Default::default(),
                plan_compact: false,
                plan_auto_approve_revisions: true,
                approval_timeout: Default::default(),
                answer_timeout: Default::default(),
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

    pub(super) fn repository() -> tempfile::TempDir {
        let temporary = tempfile::tempdir().unwrap();
        git(temporary.path(), &["init", "-q"]);
        git(temporary.path(), &["config", "core.autocrlf", "false"]);
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

    async fn dispatch_pending_task_input(
        broker: &mut HarnessBroker,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let task = broker
            .task_controller
            .current()?
            .context("task is missing")?;
        let (instruction, purpose) = if let Some(entry) = task.entry.as_ref()
            && entry.delivery == crate::task::TaskInputDelivery::Pending
        {
            anyhow::ensure!(
                broker.task_controller.acknowledge_entry(
                    &mut broker.store,
                    &task.revision,
                    &entry.id,
                    broker.clock.now_ms()
                )?,
                "task entry was replaced"
            );
            (
                entry.instruction.clone(),
                crate::input::InputPurpose::TaskEntry {
                    revision: task.revision.clone(),
                    id: entry.id.clone(),
                    start: crate::exchange::TaskStart {
                        kind: task.kind(),
                        title: task.title().to_owned(),
                    },
                },
            )
        } else {
            let turn = task.turn.as_ref().context("task turn is missing")?;
            let continuation = turn
                .continuation
                .as_ref()
                .context("task continuation is missing")?;
            anyhow::ensure!(
                continuation.delivery == crate::task::TaskInputDelivery::Pending,
                "task continuation was already delivered"
            );
            anyhow::ensure!(
                broker.task_controller.acknowledge_continuation(
                    &mut broker.store,
                    &task.revision,
                    &continuation.id,
                    broker.clock.now_ms()
                )?,
                "task continuation was replaced"
            );
            (
                continuation.instruction.clone(),
                crate::input::InputPurpose::TaskContinuation {
                    revision: task.revision.clone(),
                    id: continuation.id.clone(),
                    completed_turn_id: turn.id.clone(),
                    reason: continuation.reason.clone(),
                },
            )
        };
        broker.task_controller.admit(task.kind())?;
        broker.input_purpose = purpose;
        let result = broker
            .submit_prompt_inner(json!({"text": instruction}))
            .await;
        broker.input_purpose = crate::input::InputPurpose::User;
        result
    }

    async fn run_plan_to_input_boundary(broker: &mut HarnessBroker) -> Result<()> {
        let task = broker
            .task_controller
            .current()?
            .context("pending plan task is missing")?;
        let entry = task
            .entry
            .as_ref()
            .context("pending plan entry is missing")?
            .clone();
        anyhow::ensure!(
            broker.task_controller.acknowledge_entry(
                &mut broker.store,
                &task.revision,
                &entry.id,
                broker.clock.now_ms()
            )?,
            "plan entry was replaced before test dispatch"
        );
        broker.input_purpose = crate::input::InputPurpose::TaskEntry {
            revision: task.revision.clone(),
            id: entry.id,
            start: crate::exchange::TaskStart {
                kind: task.kind(),
                title: task.title().to_owned(),
            },
        };
        let result = broker
            .submit_prompt_inner(json!({"text":entry.instruction}))
            .await;
        broker.input_purpose = crate::input::InputPurpose::User;
        result?;
        loop {
            let task = broker
                .task_controller
                .current()?
                .context("planning owner disappeared")?;
            let crate::task::TaskWorkflow::Plan { plan } = &task.workflow else {
                anyhow::bail!("plan test owner changed")
            };
            if matches!(
                plan.artifact.state,
                PlanState::AwaitingInput | PlanState::AwaitingReview
            ) {
                return Ok(());
            }
            anyhow::ensure!(
                matches!(
                    plan.artifact.state,
                    PlanState::Generating | PlanState::Revising
                ),
                "planning stopped before input boundary: {:?}",
                plan.artifact.state
            );
            let turn = task
                .turn
                .as_ref()
                .context("planning turn did not record its completion")?;
            let continuation = turn
                .continuation
                .as_ref()
                .context("planning turn did not schedule its continuation")?
                .clone();
            anyhow::ensure!(
                continuation.delivery == crate::task::TaskInputDelivery::Pending,
                "planning continuation was not pending"
            );
            anyhow::ensure!(
                broker
                    .store
                    .continuation_source_complete(&broker.session.id, &turn.id)?,
                "planning continuation source was not finalized"
            );
            anyhow::ensure!(
                broker.task_controller.acknowledge_continuation(
                    &mut broker.store,
                    &task.revision,
                    &continuation.id,
                    broker.clock.now_ms()
                )?,
                "planning continuation authority changed"
            );
            broker.task_controller.admit(crate::task::TaskKind::Plan)?;
            broker.input_purpose = crate::input::InputPurpose::TaskContinuation {
                revision: task.revision,
                id: continuation.id,
                completed_turn_id: turn.id.clone(),
                reason: continuation.reason,
            };
            let result = broker
                .submit_prompt_inner(json!({"text":continuation.instruction}))
                .await;
            broker.input_purpose = crate::input::InputPurpose::User;
            result?;
        }
    }

    pub(super) fn planning_question_broker(
        repository: &Path,
        data_root: &Path,
        structured: bool,
    ) -> HarnessBroker {
        crate::checks::configure_fixture(repository);
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
    async fn session_mutations_enforce_target_ownership() -> Result<()> {
        use std::fs;

        let source_workspace = tempfile::tempdir()?;
        let target_workspace = tempfile::tempdir()?;
        let data = tempfile::tempdir()?;
        let mut source = mutable_question_broker(source_workspace.path(), data.path());
        let target = mutable_question_broker(target_workspace.path(), data.path());
        let target_id = target.session.id.clone();
        let target_name = target.session.name.clone();
        let plan_directory = data.path().join("plans").join(&target_id);
        fs::create_dir_all(&plan_directory)?;
        let plan = plan_directory.join("preserved.md");
        fs::write(&plan, "preserve active plan")?;
        for (id, method, params) in [
            (
                1,
                "session.rename",
                json!({"session_id":target_id,"name":"invalid"}),
            ),
            (2, "session.delete", json!({"session_id":target_id})),
        ] {
            let response = source
                .dispatch(Request {
                    id,
                    method: method.into(),
                    params,
                })
                .await;
            assert!(
                response.response.error().is_some(),
                "{method} accepted a foreign owner"
            );
            assert_eq!(
                source.store.load_session(&target_id)?.unwrap().name,
                target_name
            );
            assert_eq!(fs::read_to_string(&plan)?, "preserve active plan");
        }
        let source_id = source.session.id.clone();
        let rename = source
            .dispatch(Request {
                id: 3,
                method: "session.rename".into(),
                params: json!({"session_id":source_id,"name":"owned rename"}),
            })
            .await;
        assert!(rename.response.error().is_none());
        assert_eq!(source.session.name, "owned rename");
        drop(target);
        let deletion = source
            .dispatch(Request {
                id: 4,
                method: "session.delete".into(),
                params: json!({"session_id":target_id}),
            })
            .await;
        assert!(deletion.response.error().is_none());
        assert!(source.store.load_session(&target_id)?.is_none());
        assert!(!plan_directory.exists());
        Ok(())
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
            .apply_agent_lifecycle(&lifecycle, None, &mut event)
            .await
            .unwrap();

        assert!(broker.agent_registry.list().is_empty());
        assert!(event.is_empty());
    }

    #[tokio::test]
    async fn explicit_native_spawn_flags_cannot_create_durable_agent_identity() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let lifecycle = AgentLifecycleEvent {
            operation: "subagent_spawned".into(),
            starts_child: true,
            parent_thread_id: Some("root".into()),
            provider_thread_id: Some("native-child".into()),
            turn_id: Some("native-child-turn".into()),
            definition: Some("Reviewer".into()),
            nickname: Some("Review worker".into()),
            task: Some("Inspect the selected change".into()),
            status: AgentExecutionState::Running,
        };
        let mut events = Vec::new();
        let spawned = broker
            .apply_agent_lifecycle(&lifecycle, Some("parent-exchange"), &mut events)
            .await
            .unwrap();
        let saved = broker.store.list_agent_run(&broker.session.id).unwrap();
        let child = saved
            .iter()
            .filter(|agent| !agent.is_primary())
            .collect::<Vec<_>>();
        assert!(spawned.is_none());
        assert!(child.is_empty());
        assert!(events.is_empty());
    }

    #[tokio::test]
    async fn native_subagent_activity_cannot_bind_a_managed_identity() {
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
            .apply_agent_lifecycle(&lifecycle, Some("parent-interaction"), &mut event)
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
        assert!(run_list[0].provider_thread_id.is_none());
    }

    #[tokio::test]
    async fn publishes_and_persists_resolved_model_before_provider_turn_starts() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.session.model = "default".into();
        broker.session.resolved_model = None;
        let mut exchange =
            completed_interaction("runtime-identity", &broker.session.id, Vec::new());
        exchange.phase = crate::exchange::ExchangePhase::Running;
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            exchange.state(),
            None,
            exchange.finalization_outcome(),
        );
        let mut runtime = ExchangeRuntime {
            task_permit: None,
            exchange_id: exchange.id.clone(),
            checkpoint: None,
            tool_gate: None,

            checklist: ChecklistTracker::default(),
            retraction_eligible: true,
            active_wait: None,
        };
        let fixture_scope = crate::backend::InvocationScope::new(
            crate::backend::ExecutionOwner::root(&broker.session.id),
            exchange.id.clone(),
        );
        let mut events = Vec::new();
        broker
            .process_backend_event(
                &mut runtime,
                &mut exchange,
                crate::backend::BackendRuntime {
                    provider: "Codex CLI".into(),
                    model: Some("resolved-model".into()),
                }
                .resolved_event(fixture_scope),
                &mut events,
            )
            .await
            .unwrap();

        assert_eq!(
            broker.session.resolved_model.as_deref(),
            Some("resolved-model")
        );
        let persisted = broker
            .store
            .load_session(&broker.session.id)
            .unwrap()
            .unwrap();
        assert_eq!(persisted.resolved_model.as_deref(), Some("resolved-model"));
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].payload["kind"], "runtime_resolved");
        assert_eq!(events[0].payload["data"]["model"], "resolved-model");
        assert_eq!(events[0].payload["data"]["session_id"], broker.session.id);
        assert!(exchange.turn.is_empty());
        assert_eq!(runtime.exchange_id, exchange.id);
        assert!(runtime.retraction_eligible);
    }

    #[tokio::test]
    async fn native_provider_content_never_creates_presentation_segments() {
        use crate::backend::{
            TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome, ToolUpdate, TurnAddress,
            TurnUpdate,
        };
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut exchange = completed_interaction("native-runtime", &broker.session.id, Vec::new());
        exchange.phase = crate::exchange::ExchangePhase::Running;
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            exchange.state(),
            None,
            exchange.finalization_outcome(),
        );
        let mut runtime = ExchangeRuntime {
            task_permit: None,
            checkpoint: None,
            tool_gate: None,
            exchange_id: exchange.id.clone(),

            checklist: ChecklistTracker::default(),
            retraction_eligible: true,
            active_wait: None,
        };
        let fixture_scope = crate::backend::InvocationScope::new(
            crate::backend::ExecutionOwner::root(&broker.session.id),
            exchange.id.clone(),
        );
        let address = TurnAddress {
            scope: fixture_scope.clone(),
            thread_id: "parent".into(),
            turn_id: "native".into(),
        };
        let mut events = Vec::new();
        broker
            .process_backend_event(
                &mut runtime,
                &mut exchange,
                BackendEvent::Turn {
                    address: address.clone(),
                    update: TurnUpdate::Started { started_at_ms: 0 },
                },
                &mut events,
            )
            .await
            .unwrap();
        for update in completed_message(
            &address,
            "inspection",
            crate::backend::MessageRole::Commentary,
            "Inspecting",
        ) {
            broker
                .process_backend_event(&mut runtime, &mut exchange, update, &mut events)
                .await
                .unwrap();
        }
        for update in crate::backend::test_support::tool(
            &address,
            "tool".into(),
            ToolInvocation {
                kind: ToolActivityKind::Command,
                name: "inspect".into(),
                title: "inspect".into(),
                arguments: Value::Null,
            },
            [
                ToolUpdate::Output {
                    update: TextUpdate::Replace("result".into()),
                },
                ToolUpdate::Finished {
                    outcome: ToolOutcome::Succeeded,
                },
            ],
        ) {
            broker
                .process_backend_event(&mut runtime, &mut exchange, update, &mut events)
                .await
                .unwrap();
        }
        assert_eq!(exchange.turn[0].messages()[0].text(), "Inspecting");
        assert_eq!(exchange.tools_for_turn(exchange.turn[0].id()).count(), 1);
        assert!(
            exchange
                .node_list
                .iter()
                .all(|node| matches!(node, ExchangeNode::TurnContent { .. }))
        );
        assert_eq!(runtime.exchange_id, exchange.id);
        let persisted = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(persisted[0].node_list.len(), 2);
        assert!(!runtime.retraction_eligible);
        let compaction = crate::backend::Compaction {
            id: "native-compaction".into(),
            thread_id: "parent".into(),
            state: crate::backend::CompactionState::Running,
            manual: false,
            started_at_ms: Some(200),
            completed_at_ms: None,
            result: Default::default(),
        };
        let notification = BackendEvent::Compaction {
            scope: fixture_scope.clone().into(),
            update: Box::new(compaction.clone()),
        };
        broker
            .process_backend_event(&mut runtime, &mut exchange, notification, &mut events)
            .await
            .unwrap();
        let position = exchange.node_list.len() - 1;
        let original_id = exchange.node_list[position].id().to_owned();
        let mut unrelated = compaction.clone();
        unrelated.thread_id = "unrelated".into();
        broker
            .process_backend_event(
                &mut runtime,
                &mut exchange,
                BackendEvent::Compaction {
                    scope: fixture_scope.clone().into(),
                    update: Box::new(unrelated),
                },
                &mut events,
            )
            .await
            .unwrap();
        assert_eq!(exchange.node_list.len(), 3);
        broker
            .process_backend_event(
                &mut runtime,
                &mut exchange,
                BackendEvent::Turn {
                    address,
                    update: TurnUpdate::Finished {
                        outcome: crate::turn::TurnOutcome::Interrupted,
                        completed_at_ms: 300,
                    },
                },
                &mut events,
            )
            .await
            .unwrap();
        let ExchangeNode::Compaction {
            compaction: pending,
            ..
        } = &exchange.node_list[position]
        else {
            panic!("compaction row missing")
        };
        assert_eq!(pending.state, crate::backend::CompactionState::Unconfirmed);
        let mut completed = compaction;
        completed.state = crate::backend::CompactionState::Completed;
        completed.result.summary_content = Some("Retain the original task.".into());
        completed.completed_at_ms = Some(400);
        let notification = BackendEvent::Compaction {
            scope: fixture_scope.into(),
            update: Box::new(completed),
        };
        broker
            .process_backend_event(&mut runtime, &mut exchange, notification, &mut events)
            .await
            .unwrap();
        let ExchangeNode::Compaction {
            compaction: completed,
            ..
        } = &exchange.node_list[position]
        else {
            panic!("compaction position changed")
        };
        assert_eq!(completed.state, crate::backend::CompactionState::Completed);
        assert_eq!(completed.started_at_ms, Some(200));
        assert_eq!(completed.completed_at_ms, Some(400));
        assert_eq!(exchange.node_list[position].id(), original_id);
        assert_eq!(exchange.turn.len(), 1);
        assert_eq!(exchange.turn[0].messages().len(), 1);
        assert_eq!(exchange.tools_for_turn(exchange.turn[0].id()).count(), 1);
    }

    #[tokio::test]
    async fn failed_checkpoint_finalization_persists_failure_and_blocks_provider_execution() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut exchange = completed_interaction("unfinished", &broker.session.id, Vec::new());
        exchange.phase = crate::exchange::ExchangePhase::Running;
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            exchange.state(),
            None,
            exchange.finalization_outcome(),
        );
        exchange.checkpoint_before = Some("missing-checkpoint".into());
        broker.store.save_exchange(&exchange).unwrap();
        assert!(
            broker
                .capture_final_checkpoint(&mut exchange, crate::exchange::ExchangeOutcome::Failed)
                .await
                .is_err()
        );
        let persisted = broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .pop()
            .unwrap();
        assert_eq!(persisted.state(), ExchangeState::Finalizing);
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
                method: "session.prompt.submit".into(),
                params: json!({"text":"new request"}),
            })
            .await;
        assert!(rejected.response.error().is_some());
        let visible = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(visible.len(), 2);
        let rejected_exchange = visible
            .iter()
            .find(|candidate| candidate.id != exchange.id)
            .unwrap();
        assert_eq!(rejected_exchange.state(), ExchangeState::Failed);
        assert!(rejected_exchange.turn.is_empty());
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
                method: "session.turn.cancel".into(),
                params: json!({}),
            })
            .await;
        assert!(
            retried.response.error().is_none(),
            "{:?}",
            retried.response.error()
        );
        let settled = broker.store.load_exchange(&exchange.id).unwrap().unwrap();
        assert_eq!(settled.state(), ExchangeState::Failed);
        assert!(settled.finalization_error.is_none());
        assert_eq!(
            serde_json::to_value(&settled.node_list).unwrap(),
            nodes_before
        );
        assert_eq!(settled.turn.len(), exchange.turn.len());
        assert_eq!(settled.duration_ms(), exchange.duration_ms());
        let settled_json = serde_json::to_value(&settled).unwrap();
        let repeated = broker
            .dispatch(Request {
                id: 3,
                method: "session.turn.cancel".into(),
                params: json!({}),
            })
            .await;
        assert!(repeated.response.error().is_none());
        assert_eq!(
            serde_json::to_value(broker.store.load_exchange(&exchange.id).unwrap().unwrap())
                .unwrap(),
            settled_json
        );
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
                method: "session.prompt.history.record".into(),
                params: json!({ "text": "inspect the architecture" }),
            })
            .await;
        assert!(recorded.response.error().is_none());
        drop(first);

        let second = planning_question_broker(second_repository.path(), data.path(), false);
        assert_eq!(
            second.snapshot().unwrap().prompt_history,
            vec![crate::attachment::PromptDraft {
                text: "inspect the architecture".into(),
                attachments: Vec::new()
            }]
        );
    }

    #[tokio::test]
    async fn check_setup_followups_preserve_discovery_and_reviewed_state_after_restart() {
        for state in [
            crate::checks::CheckSetupState::Discovery,
            crate::checks::CheckSetupState::Reviewed,
        ] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = planning_question_broker(repository.path(), data.path(), false);
            let (mut setup, _, _) = broker
                .interaction_for_turn("Configure project checks", true, 100)
                .await
                .unwrap();
            setup.check_setup = Some(state);
            setup.phase = crate::exchange::ExchangePhase::fixture(
                ExchangeState::Interrupted,
                setup.completed_at_ms(),
                setup.finalization_outcome(),
            );
            broker.store.save_exchange(&setup).unwrap();
            let session_id = broker.session.id.clone();
            drop(broker);
            let mut broker = planning_question_broker(repository.path(), data.path(), false);
            assert_eq!(broker.session.id, session_id);

            broker
                .run_interaction(
                    "Continue the interrupted setup".into(),
                    PromptMode::Chat,
                    Some(ExchangeAdmission::chat("Continue setup".into())),
                )
                .await
                .unwrap();

            let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
            let continued = exchanges.last().unwrap();
            assert_ne!(continued.id, setup.id);
            assert_eq!(continued.check_setup, Some(state));
            assert_eq!(
                broker
                    .continued_check_setup(&ExchangeAdmission::chat("Another follow-up".into()))
                    .unwrap(),
                Some(state)
            );
        }
    }

    #[tokio::test]
    async fn check_tool_scope_observes_live_settings_and_editing_permission() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        save_check_exchange(&mut broker, "editing-test", false)?;
        let fixture_scope = crate::backend::InvocationScope::new(
            crate::backend::ExecutionOwner::root(&broker.session.id),
            "editing-test",
        );
        let (sink, _events) = crate::backend::events::channel(fixture_scope.clone());
        let (scope, _lease) = crate::checks::CheckToolScope::open(
            Arc::clone(&broker.check_runner),
            crate::checks::CheckAuthority {
                data_root: broker.data_root.clone(),
                backend: broker.session.backend.clone(),
                task_id: None,
                shell: broker.session.shell,
                mode: PromptMode::Chat,
                permission: ApprovalMode::Allow,
                permissions: Arc::clone(&broker.permission_coordinator),
                scope: fixture_scope.clone(),
                authorization_sink: sink,
                editing: Arc::clone(&broker.observer.check_editing),
                setup: false,
                can_run: true,
                agent: Arc::clone(&broker.agent_coordinator),
                cancelled: Arc::new(AtomicBool::new(false)),
                sink: None,
            },
        );
        let catalog: Value =
            serde_json::from_str(&scope.invoke("check_definition_list", json!({})).await?)?;
        assert_eq!(catalog["automatic_checks"], false);
        assert_eq!(catalog["check_timing"], "after_queue");
        let configured = catalog["configured"].clone();
        for (automatic_checks, check_timing) in [
            (false, crate::checks::CheckTiming::BetweenWork),
            (true, crate::checks::CheckTiming::AfterQueue),
        ] {
            broker
                .configure_session(json!({
                    "automatic_checks": automatic_checks, "check_timing": check_timing
                }))
                .await?;
            let catalog: Value =
                serde_json::from_str(&scope.invoke("check_definition_list", json!({})).await?)?;
            assert_eq!(catalog["automatic_checks"], json!(automatic_checks));
            assert_eq!(catalog["check_timing"], json!(check_timing));
            assert_eq!(catalog["configured"], configured);
            assert!(catalog.get("history").is_none());
        }
        let request = || -> Result<Value> {
            Ok(json!({
                "expected_revision": broker.check_runner.catalog(None)?["revision"],
                "config": {}
            }))
        };
        assert!(
            scope
                .invoke("check_definition_edit", request()?)
                .await
                .unwrap_err()
                .to_string()
                .contains("disabled")
        );
        broker.observer.set_check_editing(true)?;
        scope.invoke("check_definition_edit", request()?).await?;
        broker.observer.set_check_editing(false)?;
        assert!(
            scope
                .invoke("check_definition_edit", request()?)
                .await
                .unwrap_err()
                .to_string()
                .contains("disabled")
        );
        assert_eq!(broker.store.list_exchange(&broker.session.id)?.len(), 1);
        Ok(())
    }

    #[tokio::test]
    async fn configured_check_tools_run_without_command_approval() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        std::fs::write(
            repository.path().join(".forge.json"),
            serde_json::to_vec(
                &json!({"checks":{"configured":{"trigger":{"auto":false},"action":{"command":"echo configured-check","label":"Configured check","category":"test"}}}}),
            )?,
        )?;
        for permission in [ApprovalMode::Ask, ApprovalMode::Ask] {
            save_check_exchange(
                &mut broker,
                &format!("configured-check-{permission:?}"),
                false,
            )?;
            let fixture_scope = crate::backend::InvocationScope::new(
                crate::backend::ExecutionOwner::root(&broker.session.id),
                format!("configured-check-{permission:?}"),
            );
            let (sink, mut events) = crate::backend::events::channel(fixture_scope.clone());
            let (scope, lease) = crate::checks::CheckToolScope::open(
                Arc::clone(&broker.check_runner),
                crate::checks::CheckAuthority {
                    data_root: broker.data_root.clone(),
                    backend: broker.session.backend.clone(),
                    task_id: None,
                    shell: crate::session::CheckShell::System,
                    mode: PromptMode::Chat,
                    permission,
                    permissions: Arc::clone(&broker.permission_coordinator),
                    scope: fixture_scope.clone(),
                    authorization_sink: sink,
                    editing: Arc::clone(&broker.observer.check_editing),
                    setup: false,
                    can_run: true,
                    agent: Arc::clone(&broker.agent_coordinator),
                    cancelled: Arc::new(AtomicBool::new(false)),
                    sink: None,
                },
            );
            let response: Value = serde_json::from_str(
                &tokio::time::timeout(
                    std::time::Duration::from_secs(5),
                    scope.invoke("check_run", json!({"check":"configured"})),
                )
                .await??,
            )?;
            assert_eq!(response.as_object().unwrap().len(), 1);
            let run = broker
                .check_runner
                .status(response["run_id"].as_str().unwrap(), 5000, true)
                .await?;
            assert_eq!(run.state, crate::checks::CheckState::Passed);
            assert_eq!(run.commands.len(), 1);
            assert_eq!(run.commands[0].command, "echo configured-check");
            assert_eq!(run.commands[0].exit_code, Some(0));
            let summary: Value = serde_json::from_str(
                &scope
                    .invoke(
                        "check_status",
                        json!({"run_id":response["run_id"],"wait_ms":10}),
                    )
                    .await?,
            )?;
            assert_eq!(summary["state"], "passed");
            assert_eq!(summary["exit_code"], 0);
            assert_eq!(summary["latest_output"], "configured-check");
            assert!(summary.get("context").is_none());
            assert!(summary.get("commands").is_none());
            let details: Value = serde_json::from_str(
                &scope
                    .invoke("check_detail", json!({"run_id":response["run_id"]}))
                    .await?,
            )?;
            assert!(
                details["text"]
                    .as_str()
                    .unwrap()
                    .contains("configured-check")
            );
            assert!(details["command"]["invocation"]["args"].is_array());
            let running: Value =
                serde_json::from_str(&scope.invoke("check_list", json!({})).await?)?;
            assert_eq!(running, json!({"runs":[]}));
            assert!(events.try_recv().is_err(), "check requested approval");
            assert!(
                scope
                    .invoke(
                        "check_run",
                        json!({"check":"configured","command":"echo unconfigured"}),
                    )
                    .await
                    .is_err(),
                "check tools accepted a replacement command"
            );
            assert!(
                scope
                    .invoke(
                        "check_definition_edit",
                        json!({"expected_revision":"","config":{}}),
                    )
                    .await
                    .unwrap_err()
                    .to_string()
                    .contains("disabled")
            );
            drop(lease);
            assert!(
                scope
                    .invoke("check_run", json!({"check":"configured"}))
                    .await
                    .is_err(),
                "inactive check scope admitted execution"
            );
        }
        Ok(())
    }

    #[tokio::test]
    async fn check_setup_followups_stop_after_configuration_publication() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut setup, _, _) = broker
            .interaction_for_turn("Configure project checks", true, 100)
            .await
            .unwrap();
        setup.check_setup = Some(crate::checks::CheckSetupState::Reviewed);
        setup.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Complete,
            setup.completed_at_ms(),
            setup.finalization_outcome(),
        );
        broker.store.save_exchange(&setup).unwrap();
        let admission = ExchangeAdmission::chat("Continue setup".into());
        assert_eq!(
            broker.continued_check_setup(&admission).unwrap(),
            Some(crate::checks::CheckSetupState::Reviewed)
        );

        let catalog = broker.check_runner.catalog(None).unwrap();
        broker
            .check_runner
            .configure(
                broker.check_runner.session_owner(),
                serde_json::from_value(json!({
                    "expected_revision": catalog["revision"],
                    "config": {}
                }))
                .unwrap(),
                None,
                None,
            )
            .await
            .unwrap();

        assert_eq!(broker.continued_check_setup(&admission).unwrap(), None);
        assert!(
            broker
                .check_runner
                .history()
                .unwrap()
                .iter()
                .all(|run| run.commands.is_empty())
        );
    }

    #[tokio::test]
    async fn check_setup_followups_do_not_cross_task_or_rollback_boundaries() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut setup, _, _) = broker
            .interaction_for_turn("Configure project checks", true, 100)
            .await
            .unwrap();
        let admission = ExchangeAdmission::chat("Ordinary chat".into());
        broker.store.save_exchange(&setup).unwrap();
        assert_eq!(broker.continued_check_setup(&admission).unwrap(), None);

        setup.check_setup = Some(crate::checks::CheckSetupState::Reviewed);
        setup.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Complete,
            setup.completed_at_ms(),
            setup.finalization_outcome(),
        );
        broker.store.save_exchange(&setup).unwrap();

        setup.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Cancelled,
            setup.completed_at_ms(),
            setup.finalization_outcome(),
        );
        broker.store.save_exchange(&setup).unwrap();
        assert_eq!(broker.continued_check_setup(&admission).unwrap(), None);
        setup.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Complete,
            setup.completed_at_ms(),
            setup.finalization_outcome(),
        );
        setup
            .set_disposition(crate::exchange::HistoryDisposition::RolledBack)
            .unwrap();
        broker.store.save_exchange(&setup).unwrap();
        assert_eq!(broker.continued_check_setup(&admission).unwrap(), None);
    }

    #[tokio::test]
    async fn pauses_and_resumes_structured_questions_during_ordinary_chat() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), true);

        let prompted = broker
            .dispatch(Request {
                id: 1,
                method: "session.prompt.submit".into(),
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
        assert!(initial[0].completed_at_ms().is_none());
        let pending = broker.snapshot().unwrap().active_elicitation.unwrap();
        assert_eq!(pending.owner, ElicitationOwner::Interaction);
        assert_eq!(
            pending.elicitation.current_question().unwrap().header,
            "Migration"
        );

        let answered = broker
            .dispatch(Request {
                id: 2,
                method: "session.question.answer".into(),
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
                method: "session.question.continue".into(),
                params: json!({}),
            })
            .await;
        assert!(continued.response.error().is_none());
        assert!(
            continued
                .event
                .iter()
                .filter(|event| event.event == "document_changed")
                .any(|event| event.payload.to_string().contains("\"kind\":\"working\"")),
            "resuming a chat clarification must restore the Working footer in live patches"
        );
        assert!(broker.snapshot().unwrap().active_elicitation.is_none());
        let completed = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(completed.len(), 1);
        assert_eq!(completed[0].id, exchange_id);
        assert_eq!(completed[0].checkpoint_before, checkpoint_before);
        assert!(completed[0].checkpoint_after.is_some());
        assert_eq!(completed[0].state(), ExchangeState::Complete);
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
    async fn input_timeout_resumes_durable_questions_once_without_a_view() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), true);
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "session.prompt.submit".into(),
                params: json!({"text":"prompt me with a multiple-choice question"}),
            })
            .await;
        assert!(result.response.error().is_none());
        let mut interaction = broker
            .find_active_interaction_elicitation()
            .unwrap()
            .unwrap();
        interaction.elicitation_mut().unwrap().deadline =
            Some(serde_json::from_value(json!({"expires_at_ms":-1})).unwrap());
        broker.store.save_exchange(&interaction).unwrap();
        let mut deadline = None;
        let request = broker
            .expire_input(&mut deadline)
            .unwrap()
            .expect("overdue request");
        assert_eq!(request.method, "session.question.continue");
        let result = broker.dispatch(request).await;
        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
        assert!(
            broker
                .find_active_interaction_elicitation()
                .unwrap()
                .is_none()
        );
        assert!(broker.expire_input(&mut deadline).unwrap().is_none());
        let saved = broker.store.list_exchange(&broker.session.id).unwrap();
        assert!(
            saved
                .iter()
                .flat_map(|exchange| &exchange.node_list)
                .any(|node| matches!(node,
            ExchangeNode::ExchangeInput { prompt } if prompt.text.contains("[automatic timeout]")))
        );
    }

    #[tokio::test]
    async fn auto_compaction_requires_fresh_usage_and_runs_once_per_turn() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
            "data_root":data.path(),"workspace":repository.path(),"client_id":"automatic-compaction",
            "backend":{"kind":"mock","command":[]},"model":"mock-model","effort":"low"
        }))?)?;
        let result = broker
            .dispatch(Request {
                id: 1,
                method: "session.prompt.submit".into(),
                params: json!({"text":"First turn"}),
            })
            .await;
        assert!(result.response.error().is_none());
        broker.session.auto_compaction = crate::session::AutoCompaction {
            enabled: true,
            threshold_tokens: 40_000,
        };
        broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
        broker.context_usage_fresh = false;
        broker.auto_compact().await?;
        assert!(
            broker
                .store
                .list_session_event(&broker.session.id)?
                .is_empty()
        );
        broker.context_usage_fresh = true;
        broker.auto_compact().await?;
        let records = broker.store.list_session_event(&broker.session.id)?;
        assert_eq!(records.len(), 1);
        let SessionEventKind::Compaction { compaction } = &records[0].detail else {
            panic!("missing compaction")
        };
        assert!(!compaction.manual);
        broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
        broker.auto_compact().await?;
        assert_eq!(
            broker.store.list_session_event(&broker.session.id)?.len(),
            1
        );
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({"text":"Second turn"}),
            })
            .await;
        assert!(result.response.error().is_none());
        broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
        broker.context_usage_fresh = true;
        broker.auto_compact().await?;
        assert_eq!(
            broker.store.list_session_event(&broker.session.id)?.len(),
            2
        );
        let result = broker
            .dispatch(Request {
                id: 3,
                method: "session.prompt.submit".into(),
                params: json!({"text":"Third turn"}),
            })
            .await;
        assert!(result.response.error().is_none());
        broker.store.save_session_event(&SessionEventRecord {
            id: "native-running".into(),
            session_id: broker.session.id.clone(),
            owner: crate::timeline::TimelineOwner::session(&broker.session.id),
            created_at_ms: 1,
            detail: SessionEventKind::Compaction {
                compaction: (crate::backend::Compaction {
                    id: "native-running".into(),
                    thread_id: broker.session.backend_session_id.clone().unwrap(),
                    state: crate::backend::CompactionState::Running,
                    manual: false,
                    started_at_ms: Some(1),
                    completed_at_ms: None,
                    result: Default::default(),
                })
                .into(),
            },
        })?;
        broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
        broker.context_usage_fresh = true;
        broker.auto_compact().await?;
        assert_eq!(
            broker.store.list_session_event(&broker.session.id)?.len(),
            3,
            "native compaction must exclude another request"
        );
        Ok(())
    }

    #[tokio::test]
    async fn compaction_captures_context_usage_without_creating_an_interaction() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
                method: "session.prompt.submit".into(),
                params: json!({ "text": "establish provider context" }),
            })
            .await;
        assert!(prompted.response.error().is_none());
        let interaction_count = broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .len();

        broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
        let compacted = broker
            .dispatch(Request {
                id: 2,
                method: "session.compact".into(),
                params: json!({}),
            })
            .await;

        assert!(compacted.response.error().is_none());
        let statuses = compacted
            .event
            .iter()
            .filter(|event| event.event == "document_changed")
            .filter_map(|event| {
                event
                    .payload
                    .pointer("/status/kind")
                    .and_then(Value::as_str)
            })
            .collect::<Vec<_>>();
        assert_eq!(statuses, ["compacting", "idle"]);
        assert!(timeline_text(&broker.snapshot().unwrap()).contains("Context compacted"));
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
        assert!(
            compacted
                .event
                .iter()
                .any(|event| event.event == "context_compacted")
        );
        let records = broker.store.list_session_event(&broker.session.id).unwrap();
        assert_eq!(records.len(), 1);
        let SessionEventKind::Compaction { compaction } = &records[0].detail else {
            panic!("compaction record missing")
        };
        assert_eq!(compaction.state, crate::backend::CompactionState::Completed);
        assert!(compaction.manual);
        assert_eq!(compaction.result.context_tokens_before, Some(50_000));
        assert_eq!(compaction.result.context_tokens_after, Some(20_000));
        assert!(timeline_text(&broker.snapshot().unwrap()).contains("50.0k → 20.0k tokens"));
        for (state, heading) in [
            (crate::backend::CompactionState::Running, None),
            (
                crate::backend::CompactionState::Failed,
                Some("Context compaction failed"),
            ),
            (
                crate::backend::CompactionState::Unconfirmed,
                Some("Context compaction completion unconfirmed"),
            ),
            (
                crate::backend::CompactionState::Completed,
                Some("Context compacted"),
            ),
        ] {
            let mut record = records[0].clone();
            let SessionEventKind::Compaction { compaction } = &mut record.detail else {
                unreachable!()
            };
            compaction.state = state;
            broker.store.save_session_event(&record).unwrap();
            let snapshot = broker.snapshot().unwrap();
            let text = timeline_text(&snapshot);
            if let Some(heading) = heading {
                assert!(matches!(
                    snapshot.status,
                    crate::session::state_machine::SessionPhase::Idle
                ));
                assert!(text.contains(heading));
            } else {
                assert!(matches!(
                    snapshot.status,
                    crate::session::state_machine::SessionPhase::Compacting { started_at_ms: 100 }
                ));
                assert!(
                    !snapshot
                        .timeline
                        .iter()
                        .any(|entry| matches!(entry, TimelineEntry::SessionEvent { .. }))
                );
                assert!(text.contains("Compacting"));
            }
        }

        let mut exchange = broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .remove(0);
        let automatic = crate::backend::Compaction {
            id: "background".into(),
            thread_id: broker.session.backend_session_id.clone().unwrap(),
            state: crate::backend::CompactionState::Running,
            manual: false,
            started_at_ms: None,
            completed_at_ms: None,
            result: Default::default(),
        };
        exchange.observe_compaction(automatic.clone(), 200);
        let position = exchange.node_list.len() - 1;
        broker.store.save_exchange(&exchange).unwrap();
        let running = broker.snapshot().unwrap();
        assert!(matches!(
            running.status,
            crate::session::state_machine::SessionPhase::Compacting { started_at_ms: 200 }
        ));
        let running_text = timeline_text(&running);
        assert!(running_text.contains("Compacting"));
        assert!(!running_text.contains("Compacting context"));
        let mut completion = automatic;
        completion.state = crate::backend::CompactionState::Completed;
        completion.result.summary_content = Some("Retain the original constraints.".into());
        let update = compaction_event(&broker.session.id, completion);
        let mut current =
            completed_interaction("next-compaction-exchange", &broker.session.id, Vec::new());
        current.ordinal = interaction_count as u64 + 1;
        current.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Running,
            current.completed_at_ms(),
            current.finalization_outcome(),
        );
        current.phase = crate::exchange::ExchangePhase::fixture(
            current.state(),
            None,
            current.finalization_outcome(),
        );
        broker.store.save_exchange(&current).unwrap();
        let mut runtime = ExchangeRuntime {
            task_permit: None,
            exchange_id: current.id.clone(),

            checklist: ChecklistTracker::default(),
            checkpoint: None,
            tool_gate: None,
            retraction_eligible: true,
            active_wait: None,
        };
        let fixture_scope = crate::backend::InvocationScope::new(
            crate::backend::ExecutionOwner::root(&broker.session.id),
            current.id.clone(),
        );
        let (sink, mut stream) = publication::channel();
        broker.event_sink = Some(sink);
        let mut events = Vec::new();
        broker
            .process_backend_event(&mut runtime, &mut current, update.clone(), &mut events)
            .await
            .unwrap();
        assert!(
            current.node_list.is_empty(),
            "background completion entered the next exchange"
        );
        let notification = tokio::time::timeout(std::time::Duration::from_secs(1), stream.recv())
            .await
            .expect("cross-turn compaction patch was not delivered")
            .unwrap()
            .unwrap();
        assert!(matches!(
            notification,
            SessionUpdate::DocumentChanged { .. }
        ));
        assert!(events.is_empty());
        broker.event_sink = None;
        assert!(
            broker
                .observe_external_compaction(update.clone())
                .unwrap()
                .is_empty()
        );
        assert!(
            broker
                .observe_external_compaction(update)
                .unwrap()
                .is_empty()
        );
        let restored = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(restored.len(), interaction_count + 1);
        let ExchangeNode::Compaction { compaction, .. } = &restored[0].node_list[position] else {
            panic!("original compaction position lost")
        };
        assert_eq!(compaction.state, crate::backend::CompactionState::Completed);
        assert_eq!(
            compaction.result.summary_content.as_deref(),
            Some("Retain the original constraints.")
        );
        assert_eq!(
            broker
                .store
                .list_session_event(&broker.session.id)
                .unwrap()
                .len(),
            1
        );
        assert!(matches!(
            broker.snapshot().unwrap().status,
            crate::session::state_machine::SessionPhase::Working { .. }
        ));

        let mut record = broker
            .store
            .list_session_event(&broker.session.id)
            .unwrap()
            .remove(0);
        record.id = "late-manual-compaction".into();
        record.created_at_ms = 300;
        let SessionEventKind::Compaction { compaction } = &mut record.detail else {
            unreachable!()
        };
        compaction.id = "late-manual-compaction".into();
        compaction.started_at_ms = Some(300);
        compaction.completed_at_ms = Some(400);
        compaction.result.context_tokens_after = None;
        broker.store.save_session_event(&record).unwrap();
        for used in [32_000, 64_000] {
            let usage_event = BackendEvent::Runtime {
                scope: fixture_scope.clone(),
                update: crate::backend::RuntimeUpdate::ContextUsage {
                    usage: ContextUsage::reported(used, 100_000).unwrap(),
                },
            };
            broker
                .process_backend_event(&mut runtime, &mut current, usage_event, &mut events)
                .await
                .unwrap();
        }
        let record = broker
            .store
            .list_session_event(&broker.session.id)
            .unwrap()
            .pop()
            .unwrap();
        let SessionEventKind::Compaction { compaction } = &record.detail else {
            unreachable!()
        };
        assert_eq!(compaction.result.context_tokens_before, Some(50_000));
        assert_eq!(compaction.result.context_tokens_after, Some(32_000));
        assert!(timeline_text(&broker.snapshot().unwrap()).contains("50.0k → 32.0k tokens"));

        let automatic = crate::backend::Compaction {
            id: "current-compaction".into(),
            thread_id: broker.session.backend_session_id.clone().unwrap(),
            state: crate::backend::CompactionState::Running,
            manual: false,
            started_at_ms: None,
            completed_at_ms: None,
            result: Default::default(),
        };
        broker
            .process_backend_event(
                &mut runtime,
                &mut current,
                compaction_event(&broker.session.id, automatic.clone()),
                &mut events,
            )
            .await
            .unwrap();
        let mut completed = automatic;
        completed.state = crate::backend::CompactionState::Completed;
        broker
            .process_backend_event(
                &mut runtime,
                &mut current,
                compaction_event(&broker.session.id, completed),
                &mut events,
            )
            .await
            .unwrap();
        let usage_event = BackendEvent::Runtime {
            scope: fixture_scope.clone(),
            update: crate::backend::RuntimeUpdate::ContextUsage {
                usage: ContextUsage::reported(12_000, 100_000).unwrap(),
            },
        };
        broker
            .process_backend_event(&mut runtime, &mut current, usage_event, &mut events)
            .await
            .unwrap();
        let ExchangeNode::Compaction { compaction, .. } = current.node_list.last().unwrap() else {
            unreachable!()
        };
        assert_eq!(compaction.result.context_tokens_before, Some(64_000));
        assert_eq!(compaction.result.context_tokens_after, Some(12_000));
        assert!(timeline_text(&broker.snapshot().unwrap()).contains("64.0k → 12.0k tokens"));
    }

    #[tokio::test]
    async fn initializes_one_named_session_without_resuming_a_leased_session() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let initialize = |client_id: &str, name: &str| InitializeRequest {
            background: None,
            backend_launch_by_provider: Default::default(),
            config_dir: None,
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
        assert_eq!(first.session.approval_mode, ApprovalMode::Ask);
        assert_eq!(second.session.approval_mode, ApprovalMode::Ask);
        assert_eq!(second.store.list_session(None).unwrap().len(), 2);
    }

    #[test]
    fn explicit_session_initialization_validates_before_leasing_and_resumes_exact_id() {
        let workspace = repository();
        let other_workspace = repository();
        let data = tempfile::tempdir().unwrap();
        let mut request = InitializeRequest {
            background: None,
            backend_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: workspace.path().to_string_lossy().into_owned(),
            client_id: "source-client".into(),
            backend: BackendLaunch {
                kind: "mock".into(),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: Some("source".into()),
            goal_max_turns: 20,
            lease_conflict_action: None,
        };
        let mut source =
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(100)))
                .unwrap();
        source.release_lease().unwrap();
        request.client_id = "destination-client".into();
        request.new_session_name = Some(String::new());
        let newest =
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(110)))
                .unwrap();
        assert_ne!(source.session.id, newest.session.id);
        request.new_session_name = None;
        request.session_id = Some(source.session.id.clone());

        let mut incompatible = request.clone();
        incompatible.workspace = other_workspace.path().to_string_lossy().into_owned();
        let failure = HarnessBroker::initialize_with_clock(incompatible, Box::new(FixedClock(120)))
            .err()
            .unwrap();
        assert!(failure.to_string().contains("different workspace"));
        assert!(
            source
                .store
                .load_session(&source.session.id)
                .unwrap()
                .unwrap()
                .lease_owner
                .is_none()
        );

        let mut stored = source
            .store
            .load_session(&source.session.id)
            .unwrap()
            .unwrap();
        stored.backend = "copilot".into();
        source.store.save_session(&stored).unwrap();
        let failure =
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(130)))
                .err()
                .unwrap();
        assert!(failure.to_string().contains("different configured backend"));
        assert!(
            source
                .store
                .load_session(&source.session.id)
                .unwrap()
                .unwrap()
                .lease_owner
                .is_none()
        );
        stored.backend = "mock".into();
        stored.approval_mode = ApprovalMode::Ask;
        stored.workspace = format!("{}//./", stored.workspace);
        source.store.save_session(&stored).unwrap();

        let resumed =
            HarnessBroker::initialize_with_clock(request, Box::new(FixedClock(140))).unwrap();
        assert_eq!(resumed.session.id, source.session.id);
        assert_eq!(resumed.session.name, "source");
        assert_eq!(resumed.session.approval_mode, ApprovalMode::Ask);
        assert_eq!(resumed.store.list_session(None).unwrap().len(), 2);
    }

    #[tokio::test]
    async fn forks_a_live_session_without_taking_its_lease() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let initialize =
            |client_id: &str, lease_conflict_action: Option<String>| InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
                method: "session.permission.set".into(),
                params: json!({ "change":{"kind":"set_approval","mode":"ask"} }),
            })
            .await;
        let prompt = source
            .dispatch(Request {
                id: 1,
                method: "session.prompt.submit".into(),
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
            crate::session::SessionPurpose::Conversation,
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
        let inherited_source = source
            .snapshot()
            .unwrap()
            .timeline
            .into_iter()
            .filter(|entry| !matches!(entry, TimelineEntry::Status { .. }))
            .map(|entry| serde_json::to_value(entry).unwrap())
            .collect::<Vec<_>>();
        let backend_fork = fork_controller
            .backend
            .fork(preparation.backend_request)
            .await
            .unwrap();
        let mut child_initialize = initialize("fork-client", None);
        child_initialize.session_id = Some(child_session_id.clone());
        let mut child_controller = HarnessBroker::initialize_with_clock(
            child_initialize,
            Box::new(FixedClock(preparation.child.created_at_ms + 1)),
        )
        .unwrap();
        let ready_event = child_controller
            .complete_provider_fork(Ok(backend_fork))
            .unwrap();
        assert_eq!(ready_event.event, "session_fork_ready");
        let snapshot = serde_json::to_value(child_controller.snapshot().unwrap()).unwrap();
        assert_ne!(snapshot["session"]["id"], source_session_id);
        assert_eq!(snapshot["session"]["name"], "fork investigation");
        assert_eq!(snapshot["session"]["approval_mode"], "ask");
        assert_eq!(snapshot["session"]["provider_fork_state"]["state"], "ready");
        assert_eq!(snapshot["session"]["context_usage"]["used"], 50);
        assert_eq!(snapshot["session"]["context_usage"]["size"], 200);
        assert_eq!(
            snapshot["session"]["context_usage"]["remaining_percent"],
            75
        );
        assert!(snapshot.get("exchange").is_none() && snapshot.get("timeline").is_none());
        let timeline = child_controller.snapshot().unwrap().timeline;
        assert_eq!(timeline.len(), inherited_source.len() + 2);
        let inherited = timeline
            .iter()
            .filter_map(|entry| match entry {
                TimelineEntry::Inherited { entry, .. } => {
                    Some(serde_json::to_value(entry).unwrap())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(inherited, inherited_source);
        assert!(
            inherited
                .iter()
                .any(|entry| entry["exchange"]["prompt"] == "preserve this interaction")
        );
        assert!(
            child_controller
                .store
                .list_exchange(&child_session_id)
                .unwrap()
                .is_empty()
        );
        assert!(
            child_controller
                .task_controller
                .current()
                .unwrap()
                .is_none()
        );
        assert!(matches!(
            &timeline[inherited.len()],
            TimelineEntry::SessionEvent { .. }
        ));
        assert!(matches!(
            timeline.last().unwrap(),
            TimelineEntry::Status {
                status: SessionPhase::Idle,
                ..
            }
        ));
        let failed_preparation = prepare_provider_fork(
            data.path(),
            "fork-client",
            "mock",
            &fork_controller.capability,
            &fork_controller.session.id,
            &json!({ "session_id": source_session_id }),
            crate::session::SessionPurpose::Conversation,
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
        assert!(
            failed_controller
                .snapshot()
                .unwrap()
                .timeline
                .iter()
                .any(|entry| matches!(entry, TimelineEntry::Inherited { .. }))
        );

        let later_source = source
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({ "text": "source-only later message" }),
            })
            .await;
        assert!(later_source.response.error().is_none());
        let child_prompt = child_controller
            .dispatch(Request {
                id: 3,
                method: "session.prompt.submit".into(),
                params: json!({ "text": "child-only new message" }),
            })
            .await;
        assert!(child_prompt.response.error().is_none());
        let child_timeline = child_controller.snapshot().unwrap().timeline;
        let child_inherited = child_timeline
            .iter()
            .filter_map(|entry| match entry {
                TimelineEntry::Inherited { entry, .. } => {
                    Some(serde_json::to_value(entry).unwrap())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(child_inherited, inherited_source);
        assert!(child_timeline.iter().skip(inherited_source.len() + 1).any(
            |entry| matches!(entry, TimelineEntry::Exchange { exchange, .. }
                if exchange.prompt == "child-only new message")
        ));
        let preview = source
            .dispatch(Request {
                id: 4,
                method: "session.preview".into(),
                params: json!({ "session_id": child_session_id }),
            })
            .await;
        assert!(preview.response.error().is_none());
        assert!(
            serde_json::to_string(preview.response.result().unwrap())
                .unwrap()
                .contains("preserve this interaction")
        );

        let grandchild = prepare_provider_fork(
            data.path(),
            "fork-client",
            "mock",
            &child_controller.capability,
            &child_session_id,
            &json!({ "session_id": child_session_id }),
            crate::session::SessionPurpose::Conversation,
        )
        .unwrap();
        let grandchild_history = child_controller
            .store
            .with_fork_history(
                &grandchild.child.id,
                Vec::new(),
                &child_controller.plan_file,
            )
            .unwrap();
        assert_eq!(grandchild_history.len(), child_timeline.len() - 1);
        for (inherited, source) in grandchild_history.iter().zip(&child_timeline) {
            let TimelineEntry::Inherited { entry, .. } = inherited else {
                panic!("grandchild history must be presentation-only");
            };
            assert!(!matches!(entry.as_ref(), TimelineEntry::Inherited { .. }));
            let source = match source {
                TimelineEntry::Inherited { entry, .. } => entry.as_ref(),
                source => source,
            };
            assert_eq!(
                serde_json::to_value(entry).unwrap(),
                serde_json::to_value(source).unwrap()
            );
        }
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

    struct PreferenceBackend;

    #[async_trait::async_trait]
    impl Backend for PreferenceBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Codex,
                label: "Preference fixture".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }
        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }
        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }
        async fn prompt_stream(
            &self,
            _request: BackendRequest,
            _sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let _fixture_scope = _sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(_sink);
            let _sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let _sink = Some(_sink);
                    anyhow::bail!("preference persistence must not start a model turn")
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
        }

        async fn model_list(
            &self,
            _request: BackendRequest,
        ) -> Result<Vec<crate::backend::BackendModel>> {
            ["gpt-5.6", "gpt-5.4"].into_iter().map(|id| {
                Ok(serde_json::from_value(json!({
                    "id": id, "reasoning": ["low", "medium", "high"],
                    "default_reasoning": "medium", "is_default": id == "gpt-5.6", "vision": false,
                        "service_tiers": ["default", "fast", "ultrafast"],
                    "context_window": [{"id":"default"}, {"id":"long_context"}]
                }))?)
            }).collect()
        }
    }

    #[tokio::test]
    async fn carries_preferences_into_a_new_session_and_mode_into_a_resume() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
        broker.backend = Arc::new(PreferenceBackend);
        let selected = broker
            .dispatch(Request {
                id: 0,
                method: "session.permission.set".into(),
                params: json!({ "change":{"kind":"enable_yolo"} }),
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
                    "service_tier": "ultrafast"
                }),
            })
            .await;
        assert!(
            configured.response.error().is_none(),
            "{:?}",
            configured.response.error()
        );
        for invalid in [json!("warp"), json!(true), json!(null)] {
            assert!(
                broker
                    .configure_session(json!({"service_tier":invalid}))
                    .await
                    .is_err()
            );
            assert_eq!(
                broker.session.service_tier,
                crate::backend::ServiceTier::Ultrafast
            );
        }
        broker.capability.ultrafast_mode = false;
        assert!(
            broker
                .configure_session(json!({"service_tier":"ultrafast"}))
                .await
                .is_err()
        );
        assert_eq!(
            broker.session.service_tier,
            crate::backend::ServiceTier::Ultrafast
        );
        broker.capability.ultrafast_mode = true;

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
        assert_eq!(session["service_tier"], "ultrafast");
        assert_eq!(session["approval_mode"], "ask");
        drop(broker);
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
        broker.backend = Arc::new(PreferenceBackend);
        let selected = broker
            .dispatch(Request {
                id: 21,
                method: "session.permission.set".into(),
                params: json!({ "change":{"kind":"set_approval","mode":"ask"} }),
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
        let stale = broker
            .observer
            .rename_session(json!({ "session_id": session["id"],
            "name": "Generated name", "expected_name": "Previous name" }));
        assert!(
            stale
                .unwrap_err()
                .to_string()
                .contains("changed during generation")
        );
        assert_eq!(
            broker
                .store
                .load_session(session["id"].as_str().unwrap())
                .unwrap()
                .unwrap()
                .name,
            "Architecture review"
        );
        drop(broker);

        let resumed_session_id = session["id"].as_str().expect("new session id").to_owned();
        let restarted = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
        assert_eq!(
            restarted.session.service_tier,
            crate::backend::ServiceTier::Ultrafast
        );
        assert_eq!(restarted.session.approval_mode, ApprovalMode::Ask);
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
    async fn canonical_provider_defaults_apply_to_new_conversations_without_changing_resumes() {
        let other_repository = repository();
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let request = InitializeRequest {
            background: None,
            backend_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "provider-default-test".into(),
            backend: BackendLaunch {
                kind: "codex".into(),
                command: vec!["codex".into(), "app-server".into()],
            },
            model: "original-model".into(),
            effort: "medium".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        };
        let mut initial =
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(100)))
                .unwrap();
        assert_eq!(initial.session.approval_mode, ApprovalMode::Ask);
        initial.session.approval_mode = ApprovalMode::Ask;
        initial.save_session().unwrap();
        let original_id = initial.session.id.clone();
        let mut settings = crate::settings::SettingsStore::open(data.path()).unwrap();
        settings.save_provider(0, "codex", &json!({
            "model": "preferred-model", "effort": "high", "context_window": "long_context", "service_tier": "fast"
        })).unwrap();
        drop(initial);

        let mut resumed =
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(200)))
                .unwrap();
        assert_eq!(resumed.session.id, original_id);
        assert_eq!(resumed.session.model, "original-model");
        assert_eq!(resumed.session.approval_mode, ApprovalMode::Ask);
        let created = resumed
            .dispatch(Request {
                id: 1,
                method: "session.new".into(),
                params: json!({}),
            })
            .await;
        let child = &created.response.result().unwrap()["session"];
        assert_eq!(child["model"], "preferred-model");
        assert_eq!(child["effort"], "high");
        assert_eq!(child["context_window"], "long_context");
        assert_eq!(child["service_tier"], "fast");
        assert_eq!(child["approval_mode"], "ask");

        let fresh = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                workspace: other_repository.path().to_string_lossy().into_owned(),
                ..request
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        assert_eq!(fresh.session.model, "preferred-model");
        assert_eq!(fresh.session.approval_mode, ApprovalMode::Ask);
        assert_eq!(
            fresh.session.resolved_model.as_deref(),
            Some("preferred-model")
        );
        assert_eq!(fresh.session.effort, "high");
        assert_eq!(
            fresh.session.context_window.as_deref(),
            Some("long_context")
        );
        assert_eq!(
            fresh.session.service_tier,
            crate::backend::ServiceTier::Fast
        );
    }

    #[tokio::test]
    async fn rejects_inline_model_effort_and_mode_values_outside_backend_capabilities() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
                method: "session.permission.set".into(),
                params: json!({ "mode": "invalid" }),
            })
            .await;
        assert!(invalid_mode.response.error().is_some());
        assert_eq!(broker.session.approval_mode, ApprovalMode::Ask);
    }

    #[tokio::test]
    async fn previews_a_stored_session_without_acquiring_its_lease() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
                method: "session.prompt.submit".into(),
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
        assert!(result.get("exchange").is_none() && result.get("timeline").is_none());
        assert!(
            result["lines"]
                .as_array()
                .unwrap()
                .iter()
                .any(|line| line.as_str().unwrap().contains("preserve preview history"))
        );
        let persisted = broker
            .store
            .load_session(&source_session_id)
            .unwrap()
            .expect("persisted source session");
        assert_eq!(persisted.lease_owner.as_deref(), Some("foreign-client"));
    }

    #[tokio::test]
    async fn resumes_latest_repository_session_after_broker_restart() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
        broker.session = broker
            .store
            .rename_owned_session(
                &broker.session.id,
                &broker.client_id,
                "Persistent analysis",
                None,
                100,
            )
            .unwrap();
        let (mut exchange, _, _) = broker
            .interaction_for_turn("interrupted request", true, 100)
            .await
            .unwrap();
        exchange.resume(100).unwrap();
        exchange
            .start_turn(
                crate::backend::TurnAddress {
                    scope: crate::turn::test_support::address(&exchange, "unfinished-turn").scope,
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
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
        assert_eq!(recovered.state(), ExchangeState::Interrupted);
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
                method: "session.prompt.submit".into(),
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
        assert!(paused_snapshot.exchange.last().unwrap().awaiting_input());

        let original_exchange = paused_snapshot.exchange.last().unwrap().clone();
        assert!(original_exchange.execution_started_at_ms().is_none());
        assert!(
            broker
                .append_plan_feedback(
                    "unrelated-plan",
                    "wrong owner".into(),
                    crate::exchange::InputIntent::Clarification,
                    None
                )
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
                method: "session.question.ask".into(),
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
        assert!(clarified_exchange.execution_started_at_ms().is_none());
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
                method: "session.prompt.submit".into(),
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
                method: "session.question.answer".into(),
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
                method: "session.question.continue".into(),
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
        assert!(
            completed
                .event
                .iter()
                .filter(|event| event.event == "document_changed")
                .any(|event| {
                    let payload = event.payload.to_string();
                    payload.contains("\"kind\":\"working\"")
                        || payload.contains("\"kind\":\"retrying_plan_generation\"")
                }),
            "resuming plan feedback must restore an active footer in live patches"
        );
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
        let plan = completed_snapshot
            .active_plan
            .as_ref()
            .expect("submitted plan");
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
                .append_plan_feedback(
                    &plan.id,
                    "late feedback".into(),
                    crate::exchange::InputIntent::Clarification,
                    None
                )
                .is_err()
        );
        assert_eq!(completed_snapshot.active_plan.as_ref().unwrap().id, plan.id);
        assert!(completed_snapshot.timeline.iter().all(|entry| matches!(
            entry,
            TimelineEntry::Exchange { .. } | TimelineEntry::Status { .. }
        )));
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
                method: "session.prompt.submit".into(),
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
                method: "session.question.answer".into(),
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
                method: "session.question.continue".into(),
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
            2,
            "automatic retries must follow the answered exchange as a separate attempt"
        );
        assert!(matches!(
            &snapshot.exchange[1].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
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
                method: "session.prompt.submit".into(),
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
                method: "session.prompt.submit".into(),
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
                method: "session.prompt.submit".into(),
                params: json!({ "text": "/plan retry" }),
            })
            .await;
        assert!(retried.response.error().is_none());
        let submitted = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(submitted.id, plan_id);
        assert_eq!(submitted.state, PlanState::AwaitingReview);
        assert_eq!(submitted.generation.budget.turn_count, 1);
        let mut interrupted = submitted;
        PlanStateMachine::apply(
            &mut interrupted,
            PlanEvent::TransitionFailed,
            broker.clock.now_ms(),
        )
        .unwrap();
        broker.store.save_plan(&interrupted).unwrap();
        broker.pause_task(false).await.unwrap();
        let paused = broker.task_controller.current().unwrap().unwrap();
        let resumed = broker
            .dispatch(Request {
                id: 4,
                method: "session.prompt.submit".into(),
                params: json!({"text":"continue the existing design"}),
            })
            .await;
        assert!(
            resumed.response.error().is_none(),
            "{:?}",
            resumed.response.error()
        );
        let recovered = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(recovered.id, plan_id);
        assert_eq!(recovered.state, PlanState::Failed);
        assert_eq!(
            broker.task_controller.current().unwrap().unwrap().revision,
            paused.revision
        );
        let resumed = broker
            .dispatch(Request {
                id: 5,
                method: "session.prompt.submit".into(),
                params: json!({"text":"/plan retry"}),
            })
            .await;
        assert!(
            resumed.response.error().is_none(),
            "{:?}",
            resumed.response.error()
        );
        let recovered = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(recovered.id, plan_id);
        assert_eq!(recovered.state, PlanState::AwaitingReview);
        assert_eq!(recovered.generation.budget.turn_count, 1);
    }

    #[tokio::test]
    async fn abort_plan_clears_review_and_question_waits_and_restores_approval_mode() {
        for structured in [false, true] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = planning_question_broker(repository.path(), data.path(), structured);
            broker.session.approval_mode = ApprovalMode::Ask;
            let planned = broker
                .dispatch(Request {
                    id: 1,
                    method: "session.prompt.submit".into(),
                    params: json!({"text":"/plan migrate"}),
                })
                .await;
            assert!(planned.response.error().is_none());
            run_plan_to_input_boundary(&mut broker).await.unwrap();
            let plan = broker.snapshot().unwrap().active_plan.unwrap();
            assert!(matches!(
                plan.state,
                PlanState::AwaitingInput | PlanState::AwaitingReview
            ));
            let cancelled = broker
                .dispatch(Request {
                    id: 2,
                    method: "session.plan.cancel".into(),
                    params: json!({"plan_id":plan.id}),
                })
                .await;
            assert!(cancelled.response.error().is_none());
            let snapshot = broker.snapshot().unwrap();
            assert!(snapshot.active_plan.is_none());
            assert_eq!(broker.session.approval_mode, ApprovalMode::Ask);
            assert!(snapshot.active_elicitation.is_none());
            assert!(snapshot.timeline.iter().any(|entry| matches!(
                entry,
                TimelineEntry::Status {
                    status: SessionPhase::Idle,
                    ..
                }
            )));
            assert!(
                broker
                    .store
                    .list_exchange(&broker.session.id)
                    .unwrap()
                    .iter()
                    .all(|exchange| exchange.is_finished() && !exchange.awaiting_input())
            );
        }
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
                method: "session.prompt.submit".into(),
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
                method: "session.prompt.submit".into(),
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
                method: "session.prompt.submit".into(),
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
                method: "session.prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let initial = broker.snapshot().unwrap().active_plan.unwrap();
        let question = initial.elicitation.as_ref().unwrap().question_set.questions[0].clone();
        broker
            .dispatch(Request {
                id: 2,
                method: "session.question.answer".into(),
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
        for prompt in [
            "ask which migration to use",
            "/plan migrate the event format",
        ] {
            let repository = repository();
            let data = tempfile::tempdir().unwrap();
            let mut broker = mutable_question_broker(repository.path(), data.path());
            let result = broker
                .dispatch(Request {
                    id: 1,
                    method: "session.prompt.submit".into(),
                    params: json!({"text": prompt}),
                })
                .await;
            assert!(result.response.error().is_none());
            let question_id = broker
                .snapshot()
                .unwrap()
                .active_elicitation
                .unwrap()
                .elicitation
                .question_set
                .questions[0]
                .id
                .clone();
            let result = broker.dispatch(Request { id: 2, method: "session.question.answer".into(),
                params: json!({"question_id":question_id, "response":{"kind":"selected","option":"Staged"}}) }).await;
            assert!(result.response.error().is_none());
            assert!(
                broker
                    .snapshot()
                    .unwrap()
                    .active_elicitation
                    .unwrap()
                    .elicitation
                    .current_question()
                    .is_none()
            );
            let result = broker
                .dispatch(Request {
                    id: 3,
                    method: "session.question.ask".into(),
                    params: json!({"question_id":question_id,"text":"what do you mean?"}),
                })
                .await;
            assert!(
                result.response.error().is_none(),
                "{:?}",
                result.response.error()
            );
            let snapshot = broker.snapshot().unwrap();
            let elicitation = snapshot.active_elicitation.unwrap().elicitation;
            assert!(elicitation.answer.is_empty());
            assert_eq!(elicitation.current_question().unwrap().id, question_id);
            assert_eq!(snapshot.exchange.len(), 1);
            assert!(snapshot.exchange[0].completed_at_ms().is_none());
            let input = snapshot.exchange[0]
                .node_list
                .iter()
                .find_map(|node| match node {
                    ExchangeNode::ExchangeInput { prompt }
                        if prompt.intent == crate::exchange::InputIntent::Clarification =>
                    {
                        Some(prompt)
                    }
                    _ => None,
                })
                .unwrap();
            let target = input
                .question
                .as_ref()
                .expect("clarification must retain its question target");
            assert_eq!(target.question_id.as_deref(), Some(question_id.as_str()));
            assert_eq!(target.set_id, elicitation.question_set.id);
            assert!(
                !result
                    .event
                    .iter()
                    .any(|event| event.event == "question_answered")
            );
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
                method: "session.prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let previous_revision = broker
            .snapshot()
            .unwrap()
            .active_plan
            .unwrap()
            .elicitation
            .unwrap()
            .revision;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
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
        assert!(elicitation.revision > previous_revision);
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
                method: "session.prompt.submit".into(),
                params: json!({ "text": "ask which migration to use" }),
            })
            .await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
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
                method: "session.prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
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
                method: "session.prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
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
                method: "session.prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
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
                method: "session.prompt.submit".into(),
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
        assert_eq!(interaction.state(), ExchangeState::Failed);
        assert!(interaction.is_finished());
        assert!(interaction.duration_ms() > 0);
        assert!(
            interaction
                .turn
                .iter()
                .all(|turn| { !matches!(turn.state(), crate::turn::TurnState::Running) })
        );
    }

    #[tokio::test]
    async fn planning_retries_start_separate_labeled_exchanges() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);

        let paused = broker
            .dispatch(Request {
                id: 1,
                method: "session.prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert!(paused.response.error().is_none());
        let plan = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(plan.state, PlanState::AwaitingReview);
        assert!(plan.elicitation.is_none());
        let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchanges.len(), 2);
        assert_eq!(exchanges[0].prompt, "/plan migrate the event format");
        assert!(
            !exchanges[1].prompt.is_empty(),
            "internal continuation omitted its provider instruction"
        );
        assert!(matches!(
            &exchanges[1].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert_ne!(exchanges[0].id, exchanges[1].id);
        assert!(
            exchanges
                .iter()
                .all(|exchange| exchange.state() == ExchangeState::Complete
                    && exchange.is_finished()
                    && exchange.turn.len() == 1)
        );
        let restored: Vec<Exchange> =
            serde_json::from_value(serde_json::to_value(&exchanges).unwrap()).unwrap();
        assert!(matches!(
            &restored[1].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert_eq!(restored[0].turn.len(), 1);
        assert_eq!(restored[1].turn.len(), 1);
    }

    #[tokio::test]
    async fn rejected_agent_admissions_preserve_names_without_orphaned_identity() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        for params in [
            json!({"task_name":"bad name","message":"Inspect tests"}),
            json!({"task_name":"worker","message":"Inspect tests","provider":"unknown"}),
            json!({"task_name":"worker","message":"Inspect tests","model":"unavailable-model"}),
        ] {
            assert!(broker.start_agent(params).await.is_err());
            assert!(broker.agent_registry.list().is_empty());
            assert!(broker.store.list_agent_task(&broker.session.id)?.is_empty());
            assert_eq!(broker.agent_coordinator.active_count(), 0);
        }
        let params = json!({"profile":"code-explorer","text":"Inspect tests"});
        let (admitted, _) = broker.start_agent(params.clone()).await?;
        assert_eq!(admitted["agent"]["task_name"], "code-explorer-1");
        settle_managed_children(&mut broker).await?;
        assert!(broker.start_agent(params).await.is_ok());
        settle_managed_children(&mut broker).await?;
        assert_eq!(broker.agent_registry.list().len(), 2);
        let task = broker.store.list_agent_task(&broker.session.id)?;
        assert_eq!(task.len(), 2);
        assert_eq!(task[0].state, ExchangeState::Complete);
        assert_eq!(broker.agent_coordinator.active_count(), 0);
        Ok(())
    }
    struct NativeSettlementBackend {
        inner: Arc<dyn Backend>,
        state: GoalState,
    }

    struct NativeContinuationBackend {
        cancel_continuation: bool,
    }

    #[async_trait::async_trait]
    impl Backend for NativeContinuationBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            _request: BackendRequest,
            sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let fixture_scope = sink.invocation_scope()?.clone();
            let fixture = crate::backend::test_support::FixtureStream::new(sink);
            let sink = fixture.clone();
            let result: anyhow::Result<crate::backend::BackendCompletion> = async {
                let output: anyhow::Result<crate::backend::BackendCompletion> = async {
                    let sink = Some(sink);
                    use crate::backend::TurnUpdate;
                    use crate::backend::usage::{TokenUsage, UsageUpdate};
                    let sink = sink.unwrap();
                    for ordinal in 1..=2 {
                        let address = crate::backend::test_support::address(
                            &fixture_scope,
                            "native-goal-thread",
                            format!("turn-{ordinal}"),
                        );
                        let started = BackendEvent::Turn {
                            address: address.clone(),
                            update: TurnUpdate::Started { started_at_ms: 0 },
                        };
                        sink.send_wait(started.clone()).await?;
                        sink.send_wait(started).await?;
                        if ordinal == 2 && self.cancel_continuation {
                            return Err(TurnCancelled.into());
                        }
                        sink.send_wait(completed_message(
                            &address,
                            format!("attempt-{ordinal}"),
                            crate::backend::MessageRole::Response,
                            format!("Attempt {ordinal}"),
                        ))
                        .await?;
                        sink.send_wait(BackendEvent::Usage {
                            address: address.clone(),
                            update: UsageUpdate {
                                id: format!("request-{ordinal}"),
                                cumulative: None,
                                cumulative_total: None,
                                usage: TokenUsage {
                                    input: Some(100 * ordinal),
                                    cached_input: Some(0),
                                    reasoning: Some(10),
                                    output: Some(20),
                                },
                            },
                        })
                        .await?;
                        if ordinal == 2 {
                            let original = crate::backend::test_support::address(
                                &fixture_scope,
                                "native-goal-thread",
                                "turn-1",
                            );
                            let late = BackendEvent::Usage {
                                address: original.clone(),
                                update: UsageUpdate {
                                    id: "late-request".into(),
                                    cumulative: None,
                                    cumulative_total: None,
                                    usage: TokenUsage {
                                        input: Some(50),
                                        cached_input: Some(0),
                                        reasoning: Some(0),
                                        output: Some(5),
                                    },
                                },
                            };
                            sink.send_wait(late.clone()).await?;
                            sink.send_wait(late).await?;
                            sink.send_wait(BackendEvent::Turn {
                                address: original,
                                update: TurnUpdate::Started { started_at_ms: 0 },
                            })
                            .await?;
                        }
                        sink.send_wait(BackendEvent::Turn {
                            address,
                            update: TurnUpdate::Finished {
                                outcome: crate::turn::TurnOutcome::Completed,
                                completed_at_ms: 100,
                            },
                        })
                        .await?;
                    }
                    let mut output = crate::backend::BackendCompletion::default();
                    output.evidence.native_state = Some(GoalState::Complete);
                    output.evidence.tool_called = true;
                    Ok(output)
                }
                .await;
                Ok(output?)
            }
            .await;
            let result = result.map_err(crate::backend::BackendError::from);
            fixture.finish(&result).await?;
            result
        }

        async fn fork(
            &self,
            _request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("native continuation fixture does not fork")
        }
    }

    #[tokio::test]
    async fn native_goal_turns_start_separate_exchanges_and_route_late_usage() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.capability.native_goal = true;
        broker.backend = Arc::new(NativeContinuationBackend {
            cancel_continuation: false,
        });
        broker
            .dispatch_session_command(
                "session.goal.set",
                json!({"objective":"complete two native turns"}),
            )
            .unwrap();
        let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchanges.len(), 2);
        assert_eq!(exchanges[0].prompt, "/goal complete two native turns");
        assert!(!exchanges[1].prompt.is_empty());
        assert!(matches!(
            &exchanges[1].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert!(exchanges.iter().all(
            |exchange| exchange.state() == ExchangeState::Complete && exchange.turn.len() == 1
        ));
        assert_eq!(exchanges[0].usage().input, Some(150));
        assert_eq!(exchanges[1].usage().input, Some(200));
        assert_eq!(exchanges[0].metrics.request_count, 2);
        assert_eq!(exchanges[1].metrics.request_count, 1);
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Complete);
        let retained = serde_json::to_value(&exchanges).unwrap();
        let session_id = broker.session.id.clone();
        drop(broker);
        let store = SqliteStore::open(data.path()).unwrap();
        assert_eq!(
            serde_json::to_value(store.list_exchange(&session_id).unwrap()).unwrap(),
            retained
        );
    }

    #[tokio::test]
    async fn goal_continuations_start_separate_labeled_exchanges() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.backend = Arc::new(NativeSettlementBackend {
            inner: broker.backend.clone(),
            state: GoalState::Active,
        });
        broker
            .dispatch_session_command(
                "session.goal.set",
                json!({"objective":"complete a continued goal"}),
            )
            .unwrap();
        broker
            .dispatch_session_command("session.command.apply", json!({"action":"continue"}))
            .unwrap();
        broker.backend = Arc::new(NativeSettlementBackend {
            inner: broker.backend.clone(),
            state: GoalState::Complete,
        });
        broker
            .dispatch_session_command("session.command.apply", json!({"action":"continue"}))
            .unwrap();
        let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchanges.len(), 3);
        assert!(exchanges.iter().all(
            |exchange| exchange.state() == ExchangeState::Complete && exchange.turn.len() == 1
        ));
        assert!(
            exchanges[1..]
                .iter()
                .all(|exchange| !exchange.prompt.is_empty()
                    && matches!(
                        exchange.origin,
                        crate::exchange::ExchangeOrigin::TaskContinuation { .. }
                    ))
        );
    }

    #[tokio::test]
    async fn broker_snapshots_use_the_committed_task_owner() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        assert!(broker.session.active_task.is_none());
        let controller = broker.task_controller.clone();
        let installed = controller
            .change(
                &mut broker.store,
                None,
                crate::task::TaskChange::Install(Box::new(crate::task::ActiveTask::goal(
                    "Current task".into(),
                    Default::default(),
                    false,
                    1,
                ))),
                1,
            )
            .unwrap()
            .unwrap();
        assert_eq!(
            broker
                .snapshot()
                .unwrap()
                .session
                .active_task
                .unwrap()
                .revision,
            installed.revision
        );
        assert_eq!(
            broker
                .observer
                .snapshot()
                .unwrap()
                .session
                .active_task
                .unwrap()
                .revision,
            installed.revision
        );
        controller
            .change(
                &mut broker.store,
                Some(&installed.revision),
                crate::task::TaskChange::Clear,
                2,
            )
            .unwrap();
        assert!(broker.snapshot().unwrap().session.active_task.is_none());
        assert!(
            broker
                .observer
                .snapshot()
                .unwrap()
                .session
                .active_task
                .is_none()
        );
    }

    #[tokio::test]
    async fn cancelling_native_continuation_preserves_the_previous_attempt() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let backend = broker.backend.clone();
        broker.capability.native_goal = true;
        broker.backend = Arc::new(NativeContinuationBackend {
            cancel_continuation: true,
        });
        let error = broker
            .dispatch_session_command(
                "session.goal.set",
                json!({"objective":"interrupt the second native turn"}),
            )
            .unwrap_err();
        assert!(error.downcast_ref::<TurnCancelled>().is_some());
        let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(exchanges.len(), 2);
        assert_eq!(exchanges[0].state(), ExchangeState::Complete);
        assert_eq!(exchanges[1].state(), ExchangeState::Cancelled);
        assert!(matches!(
            &exchanges[1].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        broker.backend = Arc::new(NativeSettlementBackend {
            inner: backend,
            state: GoalState::Complete,
        });
        broker
            .dispatch_session_command("session.goal.resume", json!({}))
            .unwrap();
        let resumed = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(resumed.len(), 3);
        assert!(matches!(
            &resumed[2].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert_eq!(
            serde_json::to_value(&resumed[..2]).unwrap(),
            serde_json::to_value(exchanges).unwrap()
        );
    }

    #[async_trait::async_trait]
    impl Backend for NativeSettlementBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Mock CLI".into(),
                capability: crate::backend::mock_capability(),
            }
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
            None
        }

        async fn prompt_stream(
            &self,
            request: BackendRequest,
            event_sink: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
            let mut output = self.inner.prompt_stream(request, event_sink).await?;
            output.evidence.native_state = Some(self.state);
            output.evidence.tool_called = true;
            Ok(output)
        }

        async fn fork(
            &self,
            request: BackendForkRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            self.inner.fork(request).await
        }
    }

    #[tokio::test]
    async fn pauses_an_active_goal_after_a_backend_failure() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
                method: "session.goal.set".into(),
                params: json!({ "objective": "finish the work" }),
            })
            .await;
        assert!(result.response.error().is_some());
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(interaction.len(), 1);
        assert_eq!(interaction[0].state(), ExchangeState::Failed);
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
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
                method: "session.prompt.submit".into(),
                params: json!({ "text": "answer an ordinary question" }),
            })
            .await;
        assert!(result.response.error().is_none());
        let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        let checklist = interaction[0]
            .checklist
            .as_ref()
            .expect("ordinary checklist snapshot");
        assert_eq!(checklist.current[0].title, "Inspect ordinary chat");
        assert!(!matches!(
            &interaction[0].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
    }

    #[tokio::test]
    async fn removes_transient_wait_without_persisting_a_timeline_node() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
                method: "session.prompt.submit".into(),
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
    async fn non_git_exchanges_save_reported_changes_without_initializing_repository() {
        let workspace = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: workspace.path().to_string_lossy().into_owned(),
                client_id: "reported-edits-test".into(),
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
                method: "session.prompt.submit".into(),
                params: json!({"text":"create a module"}),
            })
            .await;
        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
        let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
        let exchange = &exchanges[0];
        assert_eq!(exchange.state(), ExchangeState::Complete);
        assert!(exchange.checkpoint_before.is_none() && exchange.checkpoint_after.is_none());
        assert!(exchange.checkpoint_diff_text.is_none());
        assert!(!exchange.attributed_matches_checkpoint);
        let diff = exchange
            .attributed_diff_text
            .as_deref()
            .expect("persisted reported edits");
        assert!(diff.contains("seed.txt") && diff.contains("+provider edit"));
        assert!(
            !diff.contains("apps/new/deep/module/lib.rs"),
            "command edits have no provider provenance"
        );
        assert!(!workspace.path().join(".git").exists());
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
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
                method: "session.prompt.submit".into(),
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
        assert_eq!(
            interaction[0]
                .tools_for_turn(interaction[0].turn[0].id())
                .count(),
            2
        );
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
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
        let (mut rejected, _, _) = broker
            .interaction_for_turn("not started", true, 300)
            .await
            .unwrap();
        broker
            .start_exchange_runtime(&mut rejected, 300)
            .await
            .unwrap();
        let mut runtime = broker.exchange_runtime.take().unwrap();
        let captured = (&mut runtime.checkpoint.as_mut().unwrap().0).await.unwrap();
        let error = broker
            .finish_initial_checkpoint(&mut runtime, &mut rejected, captured)
            .unwrap_err();
        assert!(error.to_string().contains("capacity is full"));
        assert!(runtime.tool_gate.as_ref().unwrap().wait().await.is_err());
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
                method: "session.prompt.submit".into(),
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

    #[tokio::test]
    async fn provider_change_index_includes_referenced_child_turns() {
        let repository = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
            let mut provider =
                crate::turn::test_support::address(&exchange, &format!("{id}:provider"));
            provider.thread_id = child_run_id.into();
            let mut turn =
                crate::turn::test_support::TestTurn::new(format!("{id}:turn:1"), provider, 1);
            let call_id = format!("{id}:change");
            for update in [
                crate::backend::ToolUpdate::Requested {
                    invocation: crate::backend::ToolInvocation {
                        kind: crate::backend::ToolActivityKind::FileChange,
                        name: "edit".into(),
                        title: "file changes".into(),
                        arguments: Value::Null,
                    },
                },
                crate::backend::ToolUpdate::Changes {
                    changes: crate::backend::ProviderChangeSet {
                        file: vec![crate::backend::ProviderFileChange {
                            path: path.into(),
                            move_path: None,
                            kind: crate::backend::ProviderChangeKind::Add,
                            diff: "child".into(),
                        }],
                    },
                },
                crate::backend::ToolUpdate::Finished {
                    outcome: crate::backend::ToolOutcome::Succeeded,
                },
            ] {
                turn.record_tool(&call_id, update, 1).unwrap();
            }
            turn.finish(crate::turn::TurnOutcome::Completed, 2).unwrap();
            let owned = turn.into_exchange();
            exchange.turn = owned.turn;
            exchange.node_list = owned.node_list;
            exchange.entry = owned.entry;
            exchange.rebuild_tool_counter();
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
        let mut parent_interaction = completed_interaction(
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
        parent_interaction.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Running,
            parent_interaction.completed_at_ms(),
            parent_interaction.finalization_outcome(),
        );
        parent_interaction.phase = crate::exchange::ExchangePhase::fixture(
            parent_interaction.state(),
            None,
            parent_interaction.finalization_outcome(),
        );
        broker
            .capture_final_checkpoint(
                &mut parent_interaction,
                crate::exchange::ExchangeOutcome::Complete,
            )
            .await
            .unwrap();
        let diff = parent_interaction
            .attributed_diff_text
            .as_deref()
            .expect("reported child edits");
        assert!(diff.contains("child.rs") && !diff.contains("unrelated.rs"));
    }

    #[tokio::test]
    async fn rejects_resuming_a_session_owned_by_another_backend() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                backend_launch_by_provider: Default::default(),
                config_dir: None,
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
