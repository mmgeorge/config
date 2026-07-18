use crate::agent::{
    AgentDefinition, AgentLifecycleEvent, AgentRegistry, AgentRun, AgentRunStatus, AgentTurnRecord,
    load_codex_agent_catalog,
};
use crate::backend::approval::{ApprovalRequestView, PermissionCoordinator};
use crate::backend::{
    Backend, BackendCapability, BackendEvent, BackendEventSink, BackendLaunch, BackendRequest,
    PromptMode,
};
use crate::checkpoint::{GitCheckpoint, checkpoint_diff, checkpoint_diff_for_paths};
use crate::goal::{ContinuationDecision, GoalRecord, GoalState};
use crate::interaction::{
    ActiveThoughtUpdate, ActiveWait, CompletedThought, InteractionComment, InteractionKind,
    InteractionNode, InteractionRecord, InteractionState, ProviderChangeIndex, TaskTracker,
    TimelineReducer,
};
use crate::permissions::store::PermissionStore;
use crate::plan::{
    ArtifactSummary, PlanAnnotation, PlanElicitation, PlanExecutionRecord, PlanExecutionState,
    PlanFileStore, PlanLifecycleKind, PlanLifecycleRecord, PlanPrompt, PlanQuestionAnswer,
    PlanQuestionResponse, PlanQuestionSet, PlanQuestionWithdrawal, PlanRecord, PlanState,
};
use crate::protocol::{BrokerEvent, BrokerRequest, BrokerResponse};
use crate::session::{
    ContextUsage, ExecutionMode, HarnessPreference, HarnessSession, ModelSetting, SessionStore,
};
use crate::storage::SqliteStore;
use crate::timeline::{SessionEventKind, SessionEventRecord, TimelineEntry, TimelineProjector};
use crate::workspace::WorkspaceKind;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{
    Arc, RwLock,
    atomic::{AtomicBool, Ordering},
};
use std::time::{Instant, SystemTime, UNIX_EPOCH};
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
    #[serde(default = "default_goal_max_turns")]
    pub goal_max_turns: u32,
    #[serde(default)]
    pub lease_conflict_action: Option<String>,
}

/// Represents the complete client-visible state for one active session.
#[derive(Clone, Debug, Serialize)]
pub struct BrokerSnapshot {
    pub session: HarnessSession,
    pub interaction: Vec<InteractionRecord>,
    pub capability: BackendCapability,
    pub no_checkpoint: bool,
    pub goal: Option<GoalRecord>,
    pub active_plan: Option<PlanRecord>,
    pub active_elicitation: Option<ActiveElicitation>,
    pub artifact: Vec<ArtifactSummary>,
    pub timeline: Vec<TimelineEntry>,
    pub active_execution: Option<PlanExecutionRecord>,
    pub active_wait: Option<ActiveWait>,
    pub prompt_history: Vec<String>,
    pub agent: AgentSnapshot,
    pub approval: Vec<ApprovalRequestView>,
}

/// Represents selectable definitions and durable child timelines for one session.
#[derive(Clone, Debug, Serialize)]
pub struct AgentSnapshot {
    pub definition: Vec<AgentDefinition>,
    pub run: Vec<AgentRun>,
    pub turn: Vec<AgentTurnRecord>,
}

/// Represents a stored session timeline projected without acquiring its lease.
#[derive(Clone, Debug, Serialize)]
pub struct SessionPreview {
    pub session: HarnessSession,
    pub interaction: Vec<InteractionRecord>,
    pub timeline: Vec<TimelineEntry>,
    pub agent: AgentSnapshot,
}

/// Represents the durable owner and question state presented by the Harness question UI.
#[derive(Clone, Debug, Serialize)]
pub struct ActiveElicitation {
    pub owner: String,
    pub plan_id: Option<String>,
    pub interaction_id: Option<String>,
    pub elicitation: PlanElicitation,
}

struct InteractionRuntime {
    interaction_id: String,
    timeline: TimelineReducer,
    task: TaskTracker,
    retraction_eligible: bool,
    active_wait: Option<ActiveWait>,
}

struct AgentRuntime {
    turn: AgentTurnRecord,
    timeline: TimelineReducer,
    task: TaskTracker,
}

/// Represents one request result plus asynchronous events emitted before its response.
pub struct DispatchResult {
    pub response: BrokerResponse,
    pub event: Vec<BrokerEvent>,
}

/// Coordinates an out-of-band cancellation request with the active backend turn.
pub struct TurnCancellation {
    requested: AtomicBool,
    restore_prompt: AtomicBool,
    retraction_allowed: AtomicBool,
    notify: Notify,
}

impl TurnCancellation {
    fn new() -> Self {
        Self {
            requested: AtomicBool::new(false),
            restore_prompt: AtomicBool::new(false),
            retraction_allowed: AtomicBool::new(false),
            notify: Notify::new(),
        }
    }

    /// Clear cancellation state before dispatching the next broker request.
    pub fn arm(&self, retraction_allowed: bool) {
        self.requested.store(false, Ordering::Release);
        self.restore_prompt.store(false, Ordering::Release);
        self.retraction_allowed
            .store(retraction_allowed, Ordering::Release);
    }

    /// Request cancellation without waiting for the broker's serialized request lane.
    pub fn request(&self, restore_prompt: bool) {
        self.restore_prompt.store(restore_prompt, Ordering::Release);
        self.requested.store(true, Ordering::Release);
        self.notify.notify_waiters();
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

struct InteractionAdmission {
    prompt: String,
    kind: InteractionKind,
    plan_id: Option<String>,
    execution_id: Option<String>,
    agent_run_id: Option<String>,
}

struct PromptControlSnapshot {
    session: HarnessSession,
    goal: Option<GoalRecord>,
    execution: Vec<PlanExecutionRecord>,
}

impl InteractionAdmission {
    fn chat(prompt: String) -> Self {
        Self {
            prompt,
            kind: InteractionKind::Chat,
            plan_id: None,
            execution_id: None,
            agent_run_id: None,
        }
    }

    fn agent(prompt: String, agent_run_id: String) -> Self {
        Self {
            prompt,
            kind: InteractionKind::Chat,
            plan_id: None,
            execution_id: None,
            agent_run_id: Some(agent_run_id),
        }
    }

    fn plan(prompt: String, plan_id: Option<String>, revision: bool) -> Self {
        Self {
            prompt,
            kind: if revision {
                InteractionKind::PlanRevision
            } else {
                InteractionKind::PlanDraft
            },
            plan_id,
            execution_id: None,
            agent_run_id: None,
        }
    }

    fn execution(prompt: String, plan_id: String, execution_id: String) -> Self {
        Self {
            prompt,
            kind: InteractionKind::PlanExecution,
            plan_id: Some(plan_id),
            execution_id: Some(execution_id),
            agent_run_id: None,
        }
    }
}

/// Coordinates backend turns, durable state, plan review, goals, and interaction checkpoints.
pub struct HarnessBroker {
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
    permission_store: Arc<RwLock<PermissionStore>>,
    permission_coordinator: Arc<PermissionCoordinator>,
    event_sink: Option<BackendEventSink>,
    clock: Box<dyn Clock>,
    interaction_runtime: Option<InteractionRuntime>,
    agent_registry: AgentRegistry,
    agent_runtime_by_run: HashMap<String, AgentRuntime>,
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
        let data_root = PathBuf::from(&request.data_root);
        let mut store = SqliteStore::open(&data_root)?;
        let workspace_kind = crate::workspace::resolve(Path::new(&request.workspace))?;
        let workspace = match &workspace_kind {
            WorkspaceKind::Git(path) | WorkspaceKind::Untracked(path) => {
                path.to_string_lossy().into_owned()
            }
        };
        let now_ms = clock.now_ms();
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
        let backend = Arc::<dyn Backend>::from(crate::backend::build(
            request.backend.clone(),
            Arc::clone(&permission_coordinator),
        )?);
        let backend_descriptor = backend.descriptor();
        let force_new_session = request.lease_conflict_action.as_deref() == Some("new");
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
                        let mut session = HarnessSession {
                            id: Uuid::new_v4().to_string(),
                            name: String::new(),
                            workspace,
                            backend: request.backend.kind.clone(),
                            backend_session_id: None,
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
        let previous_preference = store.load_preference(&session.workspace, &session.backend)?;
        store.save_preference(
            &session.workspace,
            &session.backend,
            &preference_for_session(&session, previous_preference),
        )?;
        let mut capability = backend_descriptor.capability;
        capability.native_fork = session.native_fork || capability.native_fork;
        capability.native_compact = session.native_compact || capability.native_compact;
        let agent_registry = load_agent_registry(&store, &session.id)?;
        Ok(Self {
            store,
            plan_file: PlanFileStore::new(&data_root),
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
            event_sink: None,
            clock,
            interaction_runtime: None,
            agent_registry,
            agent_runtime_by_run: HashMap::new(),
            turn_cancellation: Arc::new(TurnCancellation::new()),
        })
    }

    /// Share the out-of-band cancellation signal with the broker transport loop.
    pub fn turn_cancellation(&self) -> Arc<TurnCancellation> {
        Arc::clone(&self.turn_cancellation)
    }

    /// Clone the backend control boundary for out-of-band active-turn requests.
    pub fn backend_handle(&self) -> Arc<dyn Backend> {
        Arc::clone(&self.backend)
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
        let interaction = self.store.list_interaction(&self.session.id)?;
        let active_elicitation = active_plan
            .as_ref()
            .and_then(|plan| {
                plan.elicitation
                    .clone()
                    .map(|elicitation| ActiveElicitation {
                        owner: "plan".into(),
                        plan_id: Some(plan.id.clone()),
                        interaction_id: None,
                        elicitation,
                    })
            })
            .or_else(|| {
                interaction.iter().rev().find_map(|interaction| {
                    interaction
                        .elicitation
                        .clone()
                        .map(|elicitation| ActiveElicitation {
                            owner: "interaction".into(),
                            plan_id: interaction.plan_id.clone(),
                            interaction_id: Some(interaction.id.clone()),
                            elicitation,
                        })
                })
            });
        let plan_list = self.store.list_plan(&self.session.id)?;
        let lifecycle_list = self.store.list_plan_lifecycle(&self.session.id)?;
        let execution_list = self.store.list_plan_execution(&self.session.id)?;
        let active_execution = execution_list
            .iter()
            .rev()
            .find(|execution| execution.state == PlanExecutionState::Active)
            .cloned();
        let timeline = TimelineProjector::build(
            interaction.clone(),
            &plan_list,
            lifecycle_list,
            execution_list,
            self.agent_registry.list(),
            self.store.list_session_event(&self.session.id)?,
            &self.plan_file,
        )?;
        let agent_run_list = self.agent_registry.list();
        let mut agent_turn_list = Vec::new();
        for run in &agent_run_list {
            agent_turn_list.extend(self.store.list_agent_turn(&run.id)?);
        }
        Ok(BrokerSnapshot {
            session: self.session.clone(),
            interaction,
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
            active_execution,
            active_wait: self
                .interaction_runtime
                .as_ref()
                .and_then(|runtime| runtime.active_wait.clone()),
            prompt_history: self.store.list_prompt_history()?,
            agent: AgentSnapshot {
                definition: if self.capability.agent.catalog {
                    load_codex_agent_catalog(Path::new(&self.session.workspace))?
                } else {
                    Vec::new()
                },
                run: agent_run_list,
                turn: agent_turn_list,
            },
            approval: self.permission_coordinator.pending_list()?,
        })
    }

    /// Route one client request through the owning feature boundary.
    pub async fn dispatch(&mut self, request: BrokerRequest) -> DispatchResult {
        let id = request.id;
        let outcome = self.dispatch_inner(&request.method, request.params).await;
        match outcome {
            Ok((value, event)) => match BrokerResponse::success(id, value) {
                Ok(response) => DispatchResult { response, event },
                Err(error) => DispatchResult {
                    response: BrokerResponse::failure(id, "encode_response", error.to_string()),
                    event: Vec::new(),
                },
            },
            Err(error) => {
                if let Some(retracted) = error.downcast_ref::<TurnRetracted>() {
                    return DispatchResult {
                        response: BrokerResponse::failure_with_data(
                            id,
                            "turn_retracted",
                            retracted.to_string(),
                            json!({ "prompt": retracted.prompt }),
                        ),
                        event: Vec::new(),
                    };
                }
                let code = if error.downcast_ref::<TurnCancelled>().is_some() {
                    "turn_cancelled"
                } else {
                    "request_failed"
                };
                DispatchResult {
                    response: BrokerResponse::failure(id, code, format!("{error:#}")),
                    event: Vec::new(),
                }
            }
        }
    }

    /// Route one client request while forwarding provider updates as they arrive.
    pub async fn dispatch_stream(
        &mut self,
        request: BrokerRequest,
        event_sink: BackendEventSink,
    ) -> DispatchResult {
        self.event_sink = Some(event_sink);
        let result = self.dispatch(request).await;
        self.event_sink = None;
        result
    }

    async fn dispatch_inner(
        &mut self,
        method: &str,
        params: Value,
    ) -> Result<(Value, Vec<BrokerEvent>)> {
        self.refresh_lease()?;
        match method {
            "state.get" => Ok((serde_json::to_value(self.snapshot()?)?, Vec::new())),
            "backend.models" => self.list_backend_model().await,
            "agent.list" => Ok((serde_json::to_value(self.snapshot()?.agent)?, Vec::new())),
            "agent.start" => self.start_agent(params).await,
            "agent.submit" => self.submit_agent_prompt(params).await,
            "permissions.open" => self.open_permission_document(),
            "permissions.save" => self.save_permission_document(params),
            "session.execution_mode" => self.select_execution_mode(params),
            "interaction.resume" => self.resume_interaction(params).await,
            "prompt.submit" => self.submit_prompt(params).await,
            "history.record" => self.record_prompt_history(params),
            "queue.edit_last" => anyhow::bail!("prompt queue ownership lives in the Neovim client"),
            "plan.accept" => self.accept_plan(params).await,
            "plan.request_changes" => self.request_plan_changes(params).await,
            "plan.cancel" => self.cancel_plan(),
            "plan.activate" => self.activate_plan(params),
            "question.answer" => self.answer_question(params),
            "question.skip" => self.skip_question(params),
            "question.ask" => self.ask_question(params).await,
            "question.continue" => self.continue_question().await,
            "goal.set" => self.set_goal(params).await,
            "goal.pause" => self.pause_goal().await,
            "goal.resume" => self.resume_goal().await,
            "goal.clear" => self.clear_goal().await,
            "goal.continue" => self.continue_goal().await,
            "interaction.list" => Ok((
                serde_json::to_value(self.list_interaction_review()?)?,
                Vec::new(),
            )),
            "interaction.comment.save" => self.save_interaction_comment(params),
            "interaction.request_changes" => self.request_interaction_changes(params).await,
            "interaction.rollback" => self.rollback_interaction(params).await,
            "session.new" | "session.clear" => self.new_session().await,
            "session.list" => self.list_session(params),
            "session.preview" => self.preview_session(params),
            "session.resume" => self.resume_session(params).await,
            "session.rename" => self.rename_session(params),
            "session.configure" => self.configure_session(params).await,
            "session.compact" => self.compact_session().await,
            "session.delete" => self.delete_session(params),
            "session.fork" => self.fork_session(params).await,
            "shutdown" => {
                self.release_lease()?;
                Ok((json!({ "shutdown": true }), Vec::new()))
            }
            _ => anyhow::bail!("unknown Harness broker method: {method}"),
        }
    }

    fn open_permission_document(&self) -> Result<(Value, Vec<BrokerEvent>)> {
        let (path, source) = self
            .permission_store
            .read()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .open();
        Ok((json!({ "path": path, "source": source }), Vec::new()))
    }

    fn save_permission_document(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let source = required_text(&params, "source")?;
        self.permission_store
            .write()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .save(&source)?;
        self.open_permission_document()
    }

    fn select_execution_mode(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let mode = serde_json::from_value::<ExecutionMode>(
            params.get("mode").cloned().context("mode is required")?,
        )?;
        anyhow::ensure!(
            self.capability.execution_mode_list.contains(&mode),
            "execution mode {} is unavailable for this backend",
            mode.label()
        );
        self.session.execution_mode = mode;
        self.save_session()?;
        let session = serde_json::to_value(&self.session)?;
        Ok((
            session.clone(),
            vec![self.event("execution_mode_changed", session)?],
        ))
    }

    async fn resume_interaction(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let text = required_text(&params, "text")?;
        let interaction = self
            .store
            .list_interaction(&self.session.id)?
            .into_iter()
            .last()
            .context("no Harness interaction is available to resume")?;
        anyhow::ensure!(
            interaction.state == InteractionState::Cancelled,
            "the latest Harness interaction is not cancelled"
        );
        self.run_interaction(text, PromptMode::Chat, None).await
    }

    async fn start_agent(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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
        let run = AgentRun::pending(&self.session.id, &definition, &task, self.clock.now_ms());
        self.store.save_agent_run(&run)?;
        self.agent_registry.insert(run.clone());
        let provider_agent_type = crate::agent::codex_agent_type(&definition);
        let prompt = format!(
            "Call the subagent spawn tool exactly once from this parent turn. Set its agent type exactly to `{provider_agent_type}` for the Harness definition `{definition}` and give that child the task below. Do not spawn a default, intermediary, or coordinator agent. Wait for the selected child to finish, then synthesize its result for the user.\n\n{task}"
        );
        let (_, mut event) = self
            .run_interaction(
                prompt,
                PromptMode::Chat,
                Some(InteractionAdmission::agent(
                    format!("/agent {definition} {task}"),
                    run.id.clone(),
                )),
            )
            .await?;
        event.push(self.event(
            "agent_updated",
            serde_json::to_value(self.snapshot()?.agent)?,
        )?);
        Ok((serde_json::to_value(run)?, event))
    }

    async fn submit_agent_prompt(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let run_id = required_text(&params, "run_id")?;
        let text = required_text(&params, "text")?;
        let mut run = self
            .agent_registry
            .get(&run_id)
            .cloned()
            .context("selected child agent no longer exists")?;
        anyhow::ensure!(
            run.provider_thread_id.is_some(),
            "child agent has not reported a provider thread yet"
        );
        anyhow::ensure!(
            run.status.accepts_prompt(),
            "child agent is not accepting input"
        );
        let ordinal = self.store.list_agent_turn(&run.id)?.len() as u64 + 1;
        let now_ms = self.clock.now_ms();
        let mut interaction = InteractionRecord {
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            ordinal,
            prompt: text.clone(),
            kind: InteractionKind::Chat,
            plan_id: None,
            execution_id: None,
            state: InteractionState::Running,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            created_at_ms: now_ms,
            completed_at_ms: None,
            node_list: Vec::new(),
            awaiting_input: false,
            elicitation: None,
            duration_ms: 0,
            token_count: None,
            comment: Vec::new(),
            task: None,
        };
        if let WorkspaceKind::Git(workspace) = &self.workspace_kind {
            let checkpoint =
                GitCheckpoint::new(workspace).capture(&self.store, &self.session.id, now_ms)?;
            interaction.checkpoint_before = Some(checkpoint.id.clone());
            self.store.save_checkpoint(&checkpoint)?;
        }
        let mut timeline = TimelineReducer::new(&interaction.id);
        let mut event = Vec::new();
        self.emit_live(
            BackendEvent {
                kind: "agent_timeline_updated".into(),
                text: None,
                data: json!({ "run_id": run.id, "interaction": interaction, "active": null }),
                activity: None,
                summary: None,
                task_update: None,
            },
            &mut event,
        )?;
        let mut request = self.backend_request(text, PromptMode::Chat);
        request
            .backend_session_id
            .clone_from(&run.provider_thread_id);
        let (backend_event_sink, mut backend_event_stream) = tokio::sync::mpsc::unbounded_channel();
        let backend = Arc::clone(&self.backend);
        let prompt = async move {
            backend
                .prompt_stream(request, Some(backend_event_sink))
                .await
        };
        tokio::pin!(prompt);
        let output = loop {
            tokio::select! {
                Some(backend_event) = backend_event_stream.recv() => {
                    if backend_event.kind == "agent_lifecycle" {
                        self.apply_agent_lifecycle(&backend_event, None, &mut event)?;
                        continue;
                    }
                    let transition = timeline.apply(&backend_event, self.clock.now_ms());
                    if let Some(thought) = transition.completed {
                        interaction
                            .ensure_running_segment(thought.started_at_ms)
                            .thought
                            .push(thought);
                    }
                    if let Some(active) = transition.active.as_ref() {
                        interaction
                            .ensure_running_segment(self.clock.now_ms())
                            .active = Some(active.clone());
                    }
                    self.emit_live(
                        BackendEvent {
                            kind: "agent_timeline_updated".into(),
                            text: None,
                            data: json!({
                                "run_id": run.id,
                                "interaction": interaction,
                                "active": transition.active,
                            }),
                            activity: None,
                            summary: None,
                            task_update: None,
                        },
                        &mut event,
                    )?;
                }
                result = &mut prompt => break result?,
            }
        };
        while let Ok(backend_event) = backend_event_stream.try_recv() {
            let transition = timeline.apply(&backend_event, self.clock.now_ms());
            if let Some(thought) = transition.completed {
                interaction
                    .ensure_running_segment(thought.started_at_ms)
                    .thought
                    .push(thought);
            }
            if let Some(active) = transition.active {
                interaction
                    .ensure_running_segment(self.clock.now_ms())
                    .active = Some(active);
            }
        }
        let (final_thought, response) = timeline.finish_turn(self.clock.now_ms(), false);
        if let Some(thought) = final_thought {
            interaction
                .ensure_running_segment(thought.started_at_ms)
                .thought
                .push(thought);
        }
        if let Some(response) = response {
            interaction
                .ensure_running_segment(self.clock.now_ms())
                .response = Some(response);
        }
        interaction.complete_running_segment(self.clock.now_ms());
        interaction.duration_ms = self.clock.now_ms().saturating_sub(now_ms) as u64;
        interaction.token_count = output.metrics.token_count;
        if let WorkspaceKind::Git(workspace) = &self.workspace_kind {
            let checkpoint = GitCheckpoint::new(workspace).capture(
                &self.store,
                &self.session.id,
                self.clock.now_ms(),
            )?;
            let before = interaction
                .checkpoint_before
                .as_deref()
                .map(|checkpoint_id| self.store.load_checkpoint(checkpoint_id))
                .transpose()?
                .flatten()
                .context("child interaction before checkpoint is missing")?;
            self.populate_interaction_change_diffs(&mut interaction, &before, &checkpoint)?;
            interaction.checkpoint_after = Some(checkpoint.id.clone());
            self.store.save_checkpoint(&checkpoint)?;
        }
        interaction.state = InteractionState::Complete;
        interaction.completed_at_ms = Some(self.clock.now_ms());
        if let Some(thread_id) = output.backend_session_id {
            run.provider_thread_id = Some(thread_id);
        }
        run.status = AgentRunStatus::Completed;
        run.active_turn_id = None;
        run.updated_at_ms = self.clock.now_ms();
        self.store.save_agent_run(&run)?;
        self.agent_registry.insert(run.clone());
        let turn = AgentTurnRecord {
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            agent_run_id: run.id.clone(),
            ordinal,
            interaction,
        };
        self.store.save_agent_turn(&turn)?;
        event.push(self.event(
            "agent_updated",
            serde_json::to_value(self.snapshot()?.agent)?,
        )?);
        Ok((json!({ "run": run, "turn": turn }), event))
    }

    fn route_agent_backend_event(
        &mut self,
        backend_event: &BackendEvent,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<bool> {
        let thread_id = backend_event
            .data
            .pointer("/params/threadId")
            .or_else(|| backend_event.data.pointer("/params/thread_id"))
            .or_else(|| backend_event.data.pointer("/params/turn/threadId"))
            .or_else(|| backend_event.data.pointer("/params/turn/thread_id"))
            .and_then(Value::as_str);
        let Some(run_id) = thread_id
            .and_then(|thread_id| self.agent_registry.get_by_thread(thread_id))
            .map(|run| run.id.clone())
        else {
            return Ok(false);
        };
        let now_ms = self.clock.now_ms();
        if backend_event.kind == "turn_started" {
            let turn_id = backend_event
                .data
                .pointer("/params/turn/id")
                .and_then(Value::as_str)
                .map(str::to_owned);
            if let Some(run) = self.agent_registry.get_mut(&run_id) {
                run.active_turn_id = turn_id;
                run.status = AgentRunStatus::Running;
                run.updated_at_ms = now_ms;
                self.store.save_agent_run(run)?;
            }
            return Ok(true);
        }
        if backend_event.kind == "turn_completed" {
            if let Some(mut runtime) = self.agent_runtime_by_run.remove(&run_id) {
                let (final_thought, response) = runtime.timeline.finish_turn(now_ms, false);
                if let Some(thought) = final_thought {
                    runtime
                        .turn
                        .interaction
                        .ensure_running_segment(thought.started_at_ms)
                        .thought
                        .push(thought);
                }
                if let Some(response) = response {
                    runtime
                        .turn
                        .interaction
                        .ensure_running_segment(now_ms)
                        .response = Some(response);
                }
                runtime.turn.interaction.complete_running_segment(now_ms);
                runtime.turn.interaction.state = InteractionState::Complete;
                runtime.turn.interaction.completed_at_ms = Some(now_ms);
                runtime.turn.interaction.duration_ms =
                    now_ms.saturating_sub(runtime.turn.interaction.created_at_ms) as u64;
                self.store.save_agent_turn(&runtime.turn)?;
            }
            if let Some(run) = self.agent_registry.get_mut(&run_id) {
                run.active_turn_id = None;
                if run.status.is_active() {
                    run.status = AgentRunStatus::Completed;
                }
                run.updated_at_ms = now_ms;
                self.store.save_agent_run(run)?;
            }
            self.emit_live(
                BackendEvent {
                    kind: "agent_updated".into(),
                    text: None,
                    data: serde_json::to_value(self.snapshot()?.agent)?,
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                event,
            )?;
            return Ok(true);
        }
        let mut runtime = match self.agent_runtime_by_run.remove(&run_id) {
            Some(runtime) => runtime,
            None => {
                let run = self
                    .agent_registry
                    .get(&run_id)
                    .context("child event references an unknown agent run")?;
                let ordinal = self.store.list_agent_turn(&run_id)?.len() as u64 + 1;
                let interaction = InteractionRecord {
                    id: Uuid::new_v4().to_string(),
                    session_id: self.session.id.clone(),
                    ordinal,
                    prompt: run.task.clone(),
                    kind: InteractionKind::Chat,
                    plan_id: None,
                    execution_id: None,
                    state: InteractionState::Running,
                    checkpoint_before: None,
                    checkpoint_after: None,
                    attributed_diff_text: None,
                    checkpoint_diff_text: None,
                    attributed_matches_checkpoint: false,
                    created_at_ms: now_ms,
                    completed_at_ms: None,
                    node_list: Vec::new(),
                    awaiting_input: false,
                    elicitation: None,
                    duration_ms: 0,
                    token_count: None,
                    comment: Vec::new(),
                    task: None,
                };
                AgentRuntime {
                    timeline: TimelineReducer::new(&interaction.id),
                    task: TaskTracker::default(),
                    turn: AgentTurnRecord {
                        id: Uuid::new_v4().to_string(),
                        session_id: self.session.id.clone(),
                        agent_run_id: run_id.clone(),
                        ordinal,
                        interaction,
                    },
                }
            }
        };
        if let Some(update) = backend_event.task_update.as_ref() {
            runtime.task.replace(update);
            runtime.turn.interaction.task = Some(runtime.task.snapshot().clone());
        }
        let transition = runtime.timeline.apply(backend_event, now_ms);
        if let Some(thought) = transition.completed {
            runtime
                .turn
                .interaction
                .ensure_running_segment(thought.started_at_ms)
                .thought
                .push(thought);
        }
        if let Some(active) = transition.active.as_ref() {
            runtime
                .turn
                .interaction
                .ensure_running_segment(now_ms)
                .active = Some(active.clone());
        }
        self.store.save_agent_turn(&runtime.turn)?;
        self.emit_live(
            BackendEvent {
                kind: "agent_timeline_updated".into(),
                text: None,
                data: json!({
                    "run_id": run_id,
                    "interaction": runtime.turn.interaction,
                    "active": transition.active,
                }),
                activity: None,
                summary: None,
                task_update: None,
            },
            event,
        )?;
        self.agent_runtime_by_run.insert(run_id, runtime);
        Ok(true)
    }

    fn apply_agent_lifecycle(
        &mut self,
        backend_event: &BackendEvent,
        parent_interaction_id: Option<&str>,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<Option<AgentRun>> {
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
                        parent_interaction_id,
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
                AgentRun::pending(
                    &self.session.id,
                    lifecycle.definition.as_deref().unwrap_or("default"),
                    lifecycle.task.as_deref().unwrap_or_default(),
                    self.clock.now_ms(),
                )
            });
        if run.parent_interaction_id.is_none() {
            run.parent_interaction_id = parent_interaction_id.map(str::to_owned);
        }
        if run.parent_thread_id.is_none() {
            run.parent_thread_id = lifecycle.parent_thread_id;
        }
        run.provider_thread_id = lifecycle.provider_thread_id.or(run.provider_thread_id);
        run.active_turn_id = lifecycle.turn_id;
        if let Some(definition) = lifecycle.definition {
            run.definition = definition;
        }
        run.nickname = lifecycle.nickname.or(run.nickname);
        if let Some(task) = lifecycle.task.filter(|task| !task.is_empty()) {
            run.task = task;
        }
        run.status = lifecycle.status;
        run.updated_at_ms = self.clock.now_ms();
        self.store.save_agent_run(&run)?;
        self.agent_registry.insert(run.clone());
        self.emit_live(
            BackendEvent {
                kind: "agent_updated".into(),
                text: None,
                data: serde_json::to_value(self.snapshot()?.agent)?,
                activity: None,
                summary: None,
                task_update: None,
            },
            event,
        )?;
        Ok(Some(run))
    }

    fn record_prompt_history(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let text = required_text(&params, "text")?;
        self.store
            .record_prompt_history(&text, self.clock.now_ms())?;
        Ok((
            serde_json::to_value(self.store.list_prompt_history()?)?,
            Vec::new(),
        ))
    }

    async fn list_backend_model(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
        let mut model_list = self
            .backend
            .model_list(self.backend_request(String::new(), PromptMode::Chat))
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

    async fn submit_prompt(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let snapshot = PromptControlSnapshot {
            session: self.session.clone(),
            goal: self.current_goal()?,
            execution: self.store.list_plan_execution(&self.session.id)?,
        };
        let result = self.submit_prompt_inner(params).await;
        if result
            .as_ref()
            .is_err_and(|error| error.downcast_ref::<TurnRetracted>().is_some())
        {
            self.restore_retracted_control_state(snapshot).await?;
        }
        result
    }

    async fn submit_prompt_inner(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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
            return self.new_session().await;
        }
        if text == "/plan cancel" {
            return self.cancel_plan();
        }
        if let Some(request) = text.strip_prefix("/plan ") {
            let mut leading_event = Vec::new();
            if let Some(goal) = self.current_goal()?
                && goal.state == GoalState::Active
            {
                let (_, pause_event) = self.pause_goal().await?;
                leading_event.extend(pause_event);
            }
            let (result, mut event) = self
                .run_interaction(
                    PlanPrompt::draft(request),
                    PromptMode::Plan,
                    Some(InteractionAdmission::plan(text, None, false)),
                )
                .await?;
            leading_event.append(&mut event);
            return Ok((result, leading_event));
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
        let prompt = match self
            .session
            .active_plan_id
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
        {
            Some(plan) if !plan.working_path.is_empty() => {
                PlanPrompt::with_active_artifact(text.clone(), &plan.id, &plan.working_path)
            }
            _ => text.clone(),
        };
        self.run_interaction(
            prompt,
            PromptMode::Chat,
            Some(InteractionAdmission::chat(text)),
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
        self.session = snapshot.session;
        self.save_session()?;

        if goal_changed && self.capability.native_goal && self.session.backend_session_id.is_some()
        {
            let request = self.backend_request(String::new(), PromptMode::Chat);
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

    fn answer_plan_question_choice(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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

    fn answer_question(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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
        self.store.save_interaction(&interaction)?;
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
        event: &mut Vec<BrokerEvent>,
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
            self.store.save_interaction(&interaction)?;
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
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
        let reason = withdrawal.reason.trim();
        anyhow::ensure!(!reason.is_empty(), "question withdrawal reason is required");
        if self.active_plan_awaits_input()? {
            let mut plan = self.active_elicitation_plan()?;
            let elicitation = plan
                .elicitation
                .take()
                .context("active plan has no elicitation state")?;
            plan.state = if plan.model_revision > 0 {
                PlanState::Revising
            } else {
                PlanState::Generating
            };
            plan.updated_at_ms = self.clock.now_ms();
            self.store.save_plan(&plan)?;
            let lifecycle = PlanLifecycleRecord {
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
        self.store.save_interaction(&interaction)?;
        event.push(self.event(
            "question_withdrawn",
            json!({ "interaction_id": interaction.id, "reason": reason }),
        )?);
        Ok(())
    }

    /// Replace the pending owner question set when a clarification turn asks again.
    fn replace_active_elicitation(
        &mut self,
        question: PlanQuestionSet,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<bool> {
        if self.active_plan_awaits_input()? {
            let mut plan = self.active_elicitation_plan()?;
            plan.elicitation
                .as_mut()
                .context("active plan has no elicitation state")?
                .replace_question_set(question.clone());
            plan.updated_at_ms = self.clock.now_ms();
            self.store.save_plan(&plan)?;
            let lifecycle = PlanLifecycleRecord {
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
        self.store.save_interaction(&owner)?;
        event.push(self.event(
            "question",
            json!({
                "owner": "interaction",
                "plan_id": owner.plan_id,
                "interaction_id": owner.id,
                "elicitation": owner.elicitation,
            }),
        )?);
        Ok(true)
    }

    fn skip_question(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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

    fn active_interaction_elicitation(&self) -> Result<InteractionRecord> {
        self.find_active_interaction_elicitation()?
            .context("no interaction awaits user input")
    }

    fn find_active_interaction_elicitation(&self) -> Result<Option<InteractionRecord>> {
        Ok(self
            .store
            .list_interaction(&self.session.id)?
            .into_iter()
            .rev()
            .find(|interaction| interaction.awaiting_input && interaction.elicitation.is_some()))
    }

    async fn ask_plan_question(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let mut plan = self.active_elicitation_plan()?;
        let original_elicitation = plan
            .elicitation
            .clone()
            .context("active plan has no elicitation state")?;
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
        elicitation.clarification_active = true;
        let elicitation_json = serde_json::to_string_pretty(&elicitation)?;
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        let (mut value, mut event) = self
            .run_interaction(
                PlanPrompt::clarification(&plan.request, &elicitation_json, &text),
                PromptMode::Chat,
                Some(InteractionAdmission::chat(text)),
            )
            .await?;
        let mut current_plan = self
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
                .run_interaction(
                    PlanPrompt::feedback(&plan.request, &feedback),
                    PromptMode::Plan,
                    Some(InteractionAdmission::plan(
                        feedback,
                        Some(plan.id.clone()),
                        plan.model_revision > 0,
                    )),
                )
                .await;
            match continuation {
                Ok((next_value, mut next_event)) => {
                    value = next_value;
                    event.append(&mut next_event);
                }
                Err(error) => {
                    self.store.delete_plan_lifecycle(&withdrawal.id)?;
                    current_plan.state = PlanState::AwaitingInput;
                    current_plan.elicitation = Some(original_elicitation);
                    current_plan.updated_at_ms = self.clock.now_ms();
                    self.store.save_plan(&current_plan)?;
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

    async fn ask_question(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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
        elicitation.clarification_active = true;
        let elicitation_json = serde_json::to_string_pretty(elicitation)?;
        self.store.save_interaction(&interaction)?;
        let (mut value, mut event) = self
            .run_interaction(
                PlanPrompt::question_follow_up(&elicitation_json, &text),
                PromptMode::Chat,
                Some(InteractionAdmission::chat(text)),
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

    async fn continue_plan_question(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
        let mut plan = self.active_elicitation_plan()?;
        let elicitation = plan
            .elicitation
            .take()
            .context("active plan has no elicitation state")?;
        let answer = elicitation.feedback();
        let question = elicitation.question_set.clone();
        let revision = plan.model_revision > 0;
        plan.state = if revision {
            PlanState::Revising
        } else {
            PlanState::Generating
        };
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        let lifecycle = PlanLifecycleRecord {
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
            .run_interaction(
                PlanPrompt::feedback(&plan.request, &answer),
                PromptMode::Plan,
                Some(InteractionAdmission::plan(
                    answer.clone(),
                    Some(plan.id.clone()),
                    revision,
                )),
            )
            .await;
        match result {
            Ok((value, mut event)) => {
                leading_event.append(&mut event);
                Ok((value, leading_event))
            }
            Err(error) => {
                self.store.delete_plan_lifecycle(&lifecycle.id)?;
                plan.state = PlanState::AwaitingInput;
                plan.elicitation = Some(elicitation);
                plan.updated_at_ms = self.clock.now_ms();
                self.store.save_plan(&plan)?;
                Err(error).context("continue planning after user feedback")
            }
        }
    }

    async fn continue_question(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
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
        self.store.save_interaction(&interaction)?;
        let (value, mut event) = self
            .run_interaction(
                format!(
                    "The user answered the pending Harness questions. Continue the original request using these responses.\n\n{feedback}"
                ),
                PromptMode::Chat,
                Some(InteractionAdmission::chat(feedback)),
            )
            .await?;
        event.insert(
            0,
            self.event(
                "question_answered",
                json!({
                    "interaction_id": interaction.id,
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
        admission: Option<InteractionAdmission>,
    ) -> Result<(Value, Vec<BrokerEvent>)> {
        let now_ms = self.clock.now_ms();
        let admitted_prompt = admission
            .as_ref()
            .map(|value| value.prompt.as_str())
            .unwrap_or(&text);
        let (mut interaction, new_interaction) =
            self.interaction_for_turn(admitted_prompt, admission.is_some(), now_ms)?;
        if new_interaction && let Some(admission) = admission.as_ref() {
            interaction.kind = admission.kind;
            interaction.plan_id.clone_from(&admission.plan_id);
            interaction.execution_id.clone_from(&admission.execution_id);
            if let Some(run_id) = admission.agent_run_id.as_deref()
                && let Some(mut run) = self.agent_registry.get(run_id).cloned()
            {
                run.parent_interaction_id = Some(interaction.id.clone());
                self.store.save_agent_run(&run)?;
                self.agent_registry.insert(run);
            }
            if interaction.task.is_none()
                && let Some(execution_id) = interaction.execution_id.as_deref()
            {
                interaction.task = self
                    .store
                    .list_interaction(&self.session.id)?
                    .into_iter()
                    .rev()
                    .find(|previous| previous.execution_id.as_deref() == Some(execution_id))
                    .and_then(|previous| previous.task);
            }
        }
        let mut event = Vec::new();
        if new_interaction {
            self.start_interaction_runtime(&mut interaction, now_ms)?;
        } else if self
            .interaction_runtime
            .as_ref()
            .is_none_or(|runtime| runtime.interaction_id != interaction.id)
        {
            self.resume_interaction_runtime(&interaction)?;
        }
        self.store.save_interaction(&interaction)?;
        self.emit_live(
            BackendEvent {
                kind: "timeline_interaction_started".into(),
                text: None,
                data: serde_json::to_value(&interaction)?,
                activity: None,
                summary: None,
                task_update: None,
            },
            &mut event,
        )?;

        let turn_started_at_ms = self.clock.now_ms();
        let output = self
            .prompt_with_timeline(
                self.backend_request(text.clone(), mode),
                &mut interaction,
                &mut event,
            )
            .await;
        let mut output = match output {
            Ok(output) => output,
            Err(error) => {
                let cancelled = error.downcast_ref::<TurnCancelled>().is_some();
                if cancelled
                    && let Some(backend_session_id) = self.backend.active_session_id().await
                {
                    self.session.backend_session_id = Some(backend_session_id);
                }
                let final_checkpoint_id = self.capture_final_checkpoint(&mut interaction)?;
                let workspace_unchanged = interaction.checkpoint_before.is_some()
                    && interaction.checkpoint_before == final_checkpoint_id;
                let retraction_eligible = cancelled
                    && self.turn_cancellation.restores_prompt()
                    && self.capability.native_turn_rollback
                    && workspace_unchanged
                    && self
                        .interaction_runtime
                        .as_ref()
                        .is_some_and(|runtime| runtime.retraction_eligible);
                if retraction_eligible {
                    match self.backend.rollback_cancelled_turn().await {
                        Ok(true) => {
                            self.interaction_runtime.take();
                            self.store.delete_interaction(&interaction.id)?;
                            self.emit_live(
                                BackendEvent {
                                    kind: "timeline_interaction_retracted".into(),
                                    text: None,
                                    data: json!({
                                        "interaction_id": interaction.id,
                                        "prompt": interaction.prompt.clone(),
                                    }),
                                    activity: None,
                                    summary: None,
                                    task_update: None,
                                },
                                &mut event,
                            )?;
                            return Err(anyhow::Error::new(TurnRetracted {
                                prompt: interaction.prompt,
                            }));
                        }
                        Ok(false) => {}
                        Err(rollback_error) => {
                            self.emit_live(
                                BackendEvent {
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
                            )?;
                        }
                    }
                }
                if let Some(mut runtime) = self.interaction_runtime.take() {
                    let (final_thought, response) =
                        runtime.timeline.finish_turn(self.clock.now_ms(), false);
                    if let Some(thought) = final_thought {
                        self.complete_thought(&mut runtime, &mut interaction, thought, &mut event)?;
                    }
                    if let Some(response) = response {
                        interaction
                            .ensure_running_segment(self.clock.now_ms())
                            .response = Some(response);
                    }
                    interaction.complete_running_segment(self.clock.now_ms());
                }
                interaction.state = if cancelled {
                    InteractionState::Cancelled
                } else {
                    InteractionState::Failed
                };
                interaction.completed_at_ms = Some(self.clock.now_ms());
                self.store.save_interaction(&interaction)?;
                self.pause_goal_after_turn_failure().await?;
                self.save_session()?;
                if cancelled {
                    self.emit_live(
                        BackendEvent {
                            kind: "timeline_interaction_cancelled".into(),
                            text: None,
                            data: serde_json::to_value(&interaction)?,
                            activity: None,
                            summary: None,
                            task_update: None,
                        },
                        &mut event,
                    )?;
                    return Err(error);
                }
                return Err(error).context("backend turn failed");
            }
        };
        let token_count = output.metrics.token_count;
        interaction.duration_ms = interaction.duration_ms.saturating_add(
            self.clock
                .now_ms()
                .saturating_sub(turn_started_at_ms)
                .try_into()
                .unwrap_or_default(),
        );
        interaction.token_count = token_count.or(interaction.token_count);
        if let Some(segment) = interaction.running_segment_mut() {
            segment.token_count = token_count;
        }
        self.capability = output.capability.clone();
        self.session.native_fork = output.capability.native_fork;
        self.session.native_compact = output.capability.native_compact;
        if let Some(context_usage) = output.metrics.context_usage.clone() {
            self.session.context_usage = Some(context_usage);
        }
        self.session.backend_session_id = output.backend_session_id.clone();
        if !output.runtime.provider.is_empty() {
            self.session.provider_label = output.runtime.provider;
        }
        self.session.resolved_model = output.runtime.model;
        let fallback_question = output
            .event
            .iter()
            .rev()
            .find(|backend_event| backend_event.kind == "assistant_message")
            .and_then(|backend_event| backend_event.text.clone())
            .filter(|text| !text.trim().is_empty());
        output.event.clear();

        if let Some(control_error) = output.control_error.take() {
            anyhow::bail!(control_error);
        }

        let elicitation_control_count = usize::from(output.plan_question.is_some())
            + usize::from(output.question_answer.is_some())
            + usize::from(output.question_withdrawal.is_some());
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
            let markdown = output.plan_markdown.take();
            let question = output.plan_question.take().or_else(|| {
                markdown
                    .is_none()
                    .then(|| fallback_question.map(PlanQuestionSet::freeform))
                    .flatten()
            });
            if markdown.is_none() && question.is_none() {
                interaction.state = InteractionState::Failed;
                interaction.completed_at_ms = Some(self.clock.now_ms());
                self.store.save_interaction(&interaction)?;
                self.save_session()?;
                anyhow::bail!("planning turn ended without a submitted plan or feedback question");
            }
            let active_plan = self
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
                });
            let mut plan = match active_plan {
                Some(plan) => plan,
                None => PlanRecord {
                    id: Uuid::new_v4().to_string(),
                    session_id: self.session.id.clone(),
                    request: interaction
                        .prompt
                        .strip_prefix("/plan ")
                        .unwrap_or(&interaction.prompt)
                        .to_owned(),
                    title: markdown
                        .as_deref()
                        .map(|markdown| plan_title(markdown, &interaction.prompt))
                        .unwrap_or_else(|| interaction.prompt.chars().take(100).collect()),
                    state: PlanState::Generating,
                    working_path: String::new(),
                    model_revision: 0,
                    user_revision: 0,
                    review_digest: None,
                    accepted_digest: None,
                    elicitation: None,
                    created_at_ms: now_ms,
                    updated_at_ms: now_ms,
                },
            };
            self.session.active_plan_id = Some(plan.id.clone());
            if let Some(markdown) = markdown {
                let lifecycle_kind = if plan.model_revision == 0 {
                    PlanLifecycleKind::Created
                } else {
                    PlanLifecycleKind::RevisionCreated
                };
                plan.model_revision += 1;
                plan.title = plan_title(&markdown, &plan.request);
                let (path, digest) = self.plan_file.write_model_revision(
                    &self.session.id,
                    &plan.id,
                    plan.model_revision,
                    &markdown,
                )?;
                plan.working_path = path.to_string_lossy().into_owned();
                plan.review_digest = Some(digest);
                plan.elicitation = None;
                plan.state = PlanState::AwaitingReview;
                plan.updated_at_ms = self.clock.now_ms();
                self.store.save_plan(&plan)?;
                let lifecycle = PlanLifecycleRecord {
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
                    json!({ "plan": plan, "lifecycle": lifecycle, "content": markdown }),
                )?);
            } else if let Some(question) = question {
                let question = question.normalize()?;
                plan.state = PlanState::AwaitingInput;
                match plan.elicitation.as_mut() {
                    Some(elicitation) => elicitation.replace_question_set(question.clone()),
                    None => plan.elicitation = Some(PlanElicitation::new(question.clone())),
                }
                plan.updated_at_ms = self.clock.now_ms();
                self.store.save_plan(&plan)?;
                interaction.awaiting_input = true;
                let lifecycle = PlanLifecycleRecord {
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
            }
        } else {
            if let Some(question) = output.plan_question.take() {
                let question = question.normalize()?;
                if !self.replace_active_elicitation(question.clone(), &mut event)? {
                    interaction.awaiting_input = true;
                    interaction.elicitation = Some(PlanElicitation::new(question.clone()));
                    self.store.save_interaction(&interaction)?;
                    event.push(self.event(
                        "question",
                        json!({
                            "owner": "interaction",
                            "plan_id": interaction.plan_id,
                            "interaction_id": interaction.id,
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

        self.session.updated_at_ms = self.clock.now_ms();
        self.save_session()?;
        let continuing = self.apply_goal_evidence(output.evidence, &mut event)?;
        let mut runtime = self
            .interaction_runtime
            .take()
            .context("active interaction timeline is missing")?;
        let (final_thought, response) = runtime
            .timeline
            .finish_turn(self.clock.now_ms(), continuing);
        if let Some(thought) = final_thought {
            self.complete_thought(&mut runtime, &mut interaction, thought, &mut event)?;
        }
        if let Some(response) = response {
            interaction
                .ensure_running_segment(self.clock.now_ms())
                .response = Some(response);
        }
        interaction.complete_running_segment(self.clock.now_ms());
        if continuing {
            self.interaction_runtime = Some(runtime);
        } else {
            self.capture_final_checkpoint(&mut interaction)?;
        }
        interaction.state = if continuing {
            InteractionState::Running
        } else {
            InteractionState::Complete
        };
        interaction.completed_at_ms = (!continuing).then(|| self.clock.now_ms());
        self.store.save_interaction(&interaction)?;
        event.push(self.event(
            if continuing {
                "interaction_updated"
            } else {
                "interaction_complete"
            },
            serde_json::to_value(&interaction)?,
        )?);
        Ok((
            json!({ "interaction": interaction, "session": self.session, "capability": self.capability }),
            event,
        ))
    }

    fn start_interaction_runtime(
        &mut self,
        interaction: &mut InteractionRecord,
        now_ms: i64,
    ) -> Result<()> {
        if let WorkspaceKind::Git(workspace) = &self.workspace_kind {
            let checkpoint =
                GitCheckpoint::new(workspace).capture(&self.store, &self.session.id, now_ms)?;
            interaction.checkpoint_before = Some(checkpoint.id.clone());
            self.store.save_checkpoint(&checkpoint)?;
        }
        self.interaction_runtime = Some(InteractionRuntime {
            interaction_id: interaction.id.clone(),
            timeline: TimelineReducer::new(&interaction.id),
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

    fn resume_interaction_runtime(&mut self, interaction: &InteractionRecord) -> Result<()> {
        self.interaction_runtime = Some(InteractionRuntime {
            interaction_id: interaction.id.clone(),
            timeline: TimelineReducer::new(&interaction.id),
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
        interaction: &mut InteractionRecord,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<crate::backend::BackendOutput> {
        let mut runtime = self
            .interaction_runtime
            .take()
            .context("active interaction timeline is missing")?;
        let backend = Arc::clone(&self.backend);
        let cancellation = Arc::clone(&self.turn_cancellation);
        let (backend_event_sink, mut backend_event_stream) = tokio::sync::mpsc::unbounded_channel();
        let outcome = {
            let prompt = async move {
                backend
                    .prompt_stream(request, Some(backend_event_sink))
                    .await
            };
            tokio::pin!(prompt);
            loop {
                tokio::select! {
                    Some(backend_event) = backend_event_stream.recv() => {
                        if let Err(error) = self
                            .process_backend_event(&mut runtime, interaction, backend_event, event)
                            .await
                        {
                            break Err(error);
                        }
                    }
                    result = &mut prompt => break result,
                    () = cancellation.cancelled() => break Err(anyhow::Error::new(TurnCancelled)),
                }
            }
        };
        let mut outcome = outcome;
        if outcome.is_err()
            && let Err(error) = self
                .permission_coordinator
                .cancel_all(self.event_sink.as_ref())
        {
            outcome = Err(error).context("clear pending approval requests");
        }
        if outcome
            .as_ref()
            .is_err_and(|error| error.downcast_ref::<TurnCancelled>().is_some())
            && let Err(error) = self.backend.cancel().await
        {
            outcome = Err(error).context("stop cancelled backend transport");
        }
        while outcome.is_ok()
            && let Ok(backend_event) = backend_event_stream.try_recv()
        {
            if let Err(error) = self
                .process_backend_event(&mut runtime, interaction, backend_event, event)
                .await
            {
                outcome = Err(error);
            }
        }
        self.interaction_runtime = Some(runtime);
        outcome
    }

    fn capture_final_checkpoint(
        &mut self,
        interaction: &mut InteractionRecord,
    ) -> Result<Option<String>> {
        let WorkspaceKind::Git(workspace) = &self.workspace_kind else {
            return Ok(None);
        };
        let checkpoint = GitCheckpoint::new(workspace).capture(
            &self.store,
            &self.session.id,
            self.clock.now_ms(),
        )?;
        interaction.checkpoint_after = Some(checkpoint.id.clone());
        let before = interaction
            .checkpoint_before
            .as_deref()
            .map(|id| self.store.load_checkpoint(id))
            .transpose()?
            .flatten()
            .context("interaction before checkpoint is missing")?;
        self.populate_interaction_change_diffs(interaction, &before, &checkpoint)?;
        self.store.save_checkpoint(&checkpoint)?;
        Ok(Some(checkpoint.id))
    }

    fn populate_interaction_change_diffs(
        &self,
        interaction: &mut InteractionRecord,
        before: &crate::checkpoint::CheckpointRecord,
        after: &crate::checkpoint::CheckpointRecord,
    ) -> Result<()> {
        let change_index = self.interaction_provider_change_index(interaction)?;
        let checkpoint_diff_text = checkpoint_diff(&self.store, before, after)?;
        let attributed_diff_text = if change_index.is_empty() {
            None
        } else {
            Some(checkpoint_diff_for_paths(
                &self.store,
                before,
                after,
                change_index.paths(),
            )?)
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
        interaction: &InteractionRecord,
    ) -> Result<ProviderChangeIndex> {
        let mut index = ProviderChangeIndex::default();
        index.record(interaction);
        let mut pending_run_id = referenced_agent_run_id_list(interaction);
        let mut visited_run_id = HashSet::new();
        while let Some(run_id) = pending_run_id.pop() {
            if !visited_run_id.insert(run_id.clone()) {
                continue;
            }
            for turn in self.store.list_agent_turn(&run_id)? {
                index.record(&turn.interaction);
                pending_run_id.extend(referenced_agent_run_id_list(&turn.interaction));
            }
        }
        Ok(index)
    }

    async fn process_backend_event(
        &mut self,
        runtime: &mut InteractionRuntime,
        interaction: &mut InteractionRecord,
        backend_event: BackendEvent,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
        if backend_event.kind == "approval_requested" {
            self.emit_live(backend_event, event)?;
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
                    let (thought, response) = runtime.timeline.finish_turn(now_ms, false);
                    let segment_changed = thought.is_some() || response.is_some();
                    if let Some(thought) = thought {
                        self.complete_thought(runtime, interaction, thought, event)?;
                    }
                    if let Some(response) = response {
                        interaction.ensure_running_segment(now_ms).response = Some(response);
                    }
                    interaction.complete_running_segment(now_ms);
                    if segment_changed && let Some(node) = interaction.node_list.last().cloned() {
                        self.emit_live(
                            BackendEvent {
                                kind: "timeline_node_updated".into(),
                                text: None,
                                data: json!({
                                    "interaction_id": interaction.id,
                                    "node": node,
                                }),
                                activity: None,
                                summary: None,
                                task_update: None,
                            },
                            event,
                        )?;
                    }
                    runtime.active_wait = Some(ActiveWait {
                        interaction_id: interaction.id.clone(),
                        started_at_ms: now_ms,
                        agent_count: backend_event
                            .data
                            .get("agent_count")
                            .and_then(Value::as_u64)
                            .unwrap_or_default() as usize,
                    });
                    self.store.save_interaction(interaction)?;
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
            self.emit_active_wait(interaction, runtime, event)?;
            return Ok(());
        }
        if backend_event.kind == "agent_lifecycle" {
            runtime.retraction_eligible = false;
            let lifecycle: AgentLifecycleEvent =
                serde_json::from_value(backend_event.data.clone())?;
            if lifecycle.starts_child
                && let Some(thought) = runtime.timeline.complete_active(self.clock.now_ms())
            {
                self.complete_thought(runtime, interaction, thought, event)?;
            }
            if lifecycle.starts_child {
                let now_ms = self.clock.now_ms();
                interaction.complete_running_segment(now_ms);
                if let Some(run) =
                    self.apply_agent_lifecycle(&backend_event, Some(&interaction.id), event)?
                {
                    let appended = interaction.append_agent_reference(&run.id, now_ms);
                    let completed_segment = if appended {
                        interaction
                            .node_list
                            .iter_mut()
                            .rev()
                            .find_map(|node| match node {
                                crate::interaction::InteractionNode::MainSegment { segment } => {
                                    segment.spawned_agent_count += 1;
                                    Some(segment.clone())
                                }
                                _ => None,
                            })
                    } else {
                        None
                    };
                    if let Some(segment) = completed_segment {
                        self.emit_live(
                            BackendEvent {
                                kind: "timeline_node_updated".into(),
                                text: None,
                                data: json!({
                                    "interaction_id": interaction.id,
                                    "node": crate::interaction::InteractionNode::MainSegment {
                                        segment,
                                    },
                                }),
                                activity: None,
                                summary: None,
                                task_update: None,
                            },
                            event,
                        )?;
                    }
                    if appended {
                        self.store.save_interaction(interaction)?;
                        self.emit_live(
                            BackendEvent {
                                kind: "timeline_node_updated".into(),
                                text: None,
                                data: json!({
                                    "interaction_id": interaction.id,
                                    "node": interaction.node_list.last(),
                                }),
                                activity: None,
                                summary: None,
                                task_update: None,
                            },
                            event,
                        )?;
                    }
                }
            } else {
                self.apply_agent_lifecycle(&backend_event, Some(&interaction.id), event)?;
            }
            return Ok(());
        }
        if self.route_agent_backend_event(&backend_event, event)? {
            runtime.retraction_eligible = false;
            return Ok(());
        }
        if backend_event.kind == "steering_input" {
            runtime.retraction_eligible = false;
            let now_ms = self.clock.now_ms();
            if let Some(thought) = runtime.timeline.complete_active(now_ms) {
                self.complete_thought(runtime, interaction, thought, event)?;
            }
            interaction.complete_running_segment(now_ms);
            runtime.active_wait = None;
            interaction.append_steering_prompt(backend_event.text.unwrap_or_default(), now_ms);
            self.store.save_interaction(interaction)?;
            self.emit_live(
                BackendEvent {
                    kind: "timeline_node_updated".into(),
                    text: None,
                    data: json!({
                        "interaction_id": interaction.id,
                        "node": interaction.node_list.last(),
                    }),
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                event,
            )?;
            self.emit_active_wait(interaction, runtime, event)?;
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
            self.emit_live(backend_event, event)?;
            return Ok(());
        }
        if let Some(update) = backend_event.task_update.as_ref() {
            runtime.task.replace(update);
            interaction.task = Some(runtime.task.snapshot().clone());
            self.store.save_interaction(interaction)?;
            self.emit_live(
                BackendEvent {
                    kind: "timeline_task_updated".into(),
                    text: None,
                    data: serde_json::to_value(runtime.task.snapshot())?,
                    activity: None,
                    summary: None,
                    task_update: Some(update.clone()),
                },
                event,
            )?;
        }
        let transition = runtime.timeline.apply(&backend_event, self.clock.now_ms());
        if let Some(thought) = transition.completed {
            self.complete_thought(runtime, interaction, thought, event)?;
        }
        if let Some(active) = transition.active {
            self.emit_timeline_active(interaction, active, event)?;
        }
        Ok(())
    }

    fn complete_thought(
        &mut self,
        runtime: &mut InteractionRuntime,
        interaction: &mut InteractionRecord,
        mut thought: CompletedThought,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
        thought.task_id = runtime.task.attribution_target().map(str::to_owned);
        if let Some(task_id) = thought.task_id.as_deref() {
            runtime.task.mark_attributed(task_id);
            interaction.task = Some(runtime.task.snapshot().clone());
        }
        let segment = interaction.ensure_running_segment(thought.started_at_ms);
        segment.thought.push(thought.clone());
        segment.active = None;
        self.store.save_interaction(interaction)?;
        self.emit_live(
            BackendEvent {
                kind: "timeline_node_updated".into(),
                text: None,
                data: json!({
                    "interaction_id": interaction.id,
                    "node": interaction.node_list.last(),
                }),
                activity: None,
                summary: None,
                task_update: None,
            },
            event,
        )
    }

    fn emit_timeline_active(
        &mut self,
        interaction: &mut InteractionRecord,
        active: ActiveThoughtUpdate,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
        let segment = interaction.ensure_running_segment(self.clock.now_ms());
        segment.active = Some(active);
        self.store.save_interaction(interaction)?;
        self.emit_live(
            BackendEvent {
                kind: "timeline_node_updated".into(),
                text: None,
                data: json!({
                    "interaction_id": interaction.id,
                    "node": interaction.node_list.last(),
                }),
                activity: None,
                summary: None,
                task_update: None,
            },
            event,
        )
    }

    fn emit_active_wait(
        &self,
        interaction: &InteractionRecord,
        runtime: &InteractionRuntime,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
        self.emit_live(
            BackendEvent {
                kind: "timeline_wait_updated".into(),
                text: None,
                data: json!({
                    "interaction_id": interaction.id,
                    "wait": runtime.active_wait,
                }),
                activity: None,
                summary: None,
                task_update: None,
            },
            event,
        )
    }

    fn emit_live(&self, backend_event: BackendEvent, event: &mut Vec<BrokerEvent>) -> Result<()> {
        if let Some(event_sink) = self.event_sink.as_ref() {
            let _ = event_sink.send(backend_event);
        } else {
            event.push(self.event("backend_event", serde_json::to_value(backend_event)?)?);
        }
        Ok(())
    }

    fn interaction_for_turn(
        &mut self,
        text: &str,
        admit_user_action: bool,
        now_ms: i64,
    ) -> Result<(InteractionRecord, bool)> {
        let mut interaction_list = self.store.list_interaction(&self.session.id)?;
        if !admit_user_action {
            let mut interaction = interaction_list
                .pop()
                .context("goal continuation has no originating user interaction")?;
            interaction.state = InteractionState::Running;
            interaction.completed_at_ms = None;
            return Ok((interaction, false));
        }
        if let Some(previous) = interaction_list.last_mut()
            && previous.state == InteractionState::Running
        {
            if previous.checkpoint_before.is_some() {
                self.capture_final_checkpoint(previous)?;
            }
            previous.state = InteractionState::Complete;
            previous.completed_at_ms = Some(now_ms);
            self.store.save_interaction(previous)?;
        }
        Ok((
            InteractionRecord {
                id: Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                ordinal: self.store.next_interaction_ordinal(&self.session.id)?,
                prompt: text.to_owned(),
                kind: InteractionKind::Chat,
                plan_id: None,
                execution_id: None,
                state: InteractionState::Running,
                checkpoint_before: None,
                checkpoint_after: None,
                attributed_diff_text: None,
                checkpoint_diff_text: None,
                attributed_matches_checkpoint: false,
                created_at_ms: now_ms,
                completed_at_ms: None,
                node_list: Vec::new(),
                awaiting_input: false,
                elicitation: None,
                duration_ms: 0,
                token_count: None,
                comment: Vec::new(),
                task: None,
            },
            true,
        ))
    }

    async fn accept_plan(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        anyhow::ensure!(
            !self
                .store
                .list_plan_execution(&self.session.id)?
                .iter()
                .any(|execution| execution.state == PlanExecutionState::Active),
            "another plan execution is already active"
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
        let (_, digest) = self.plan_file.read_working(&self.session.id, &plan.id)?;
        if let Some(expected) = params.get("digest").and_then(Value::as_str) {
            anyhow::ensure!(
                expected == digest,
                "plan changed after the accept action began"
            );
        }
        plan.user_revision += 1;
        let (accepted_markdown, _) =
            self.plan_file
                .save_user_revision(&self.session.id, &plan.id, plan.user_revision)?;
        plan.accepted_digest = Some(digest);
        plan.state = PlanState::Accepted;
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        self.session.active_plan_id = Some(plan.id.clone());
        let objective = "Complete the plan".to_owned();
        let goal = self.create_goal(objective, self.capability.native_goal)?;
        let lifecycle = PlanLifecycleRecord {
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
        let mut execution_record = PlanExecutionRecord {
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            plan_id: plan.id.clone(),
            goal_id: goal.id.clone(),
            state: PlanExecutionState::Active,
            created_at_ms: self.clock.now_ms(),
            completed_at_ms: None,
        };
        self.store.save_plan_execution(&execution_record)?;
        let mut pre_execution_event = Vec::new();
        self.emit_live(
            BackendEvent {
                kind: "timeline_plan_lifecycle".into(),
                text: None,
                data: json!({
                    "kind": "plan_lifecycle",
                    "id": lifecycle.id,
                    "created_at_ms": lifecycle.created_at_ms,
                    "plan": &plan,
                    "lifecycle": &lifecycle,
                    "content": &accepted_markdown
                }),
                activity: None,
                summary: None,
                task_update: None,
            },
            &mut pre_execution_event,
        )?;
        let execution_prompt = format!(
            "Goal: Complete the plan. Execute the accepted plan exactly. Report completion with harness_goal_complete only when every task is finished.\n\n{accepted_markdown}"
        );
        let execution = self
            .run_interaction(
                execution_prompt,
                PromptMode::ExecutePlan,
                Some(InteractionAdmission::execution(
                    format!("Accept plan: {}", plan.request),
                    plan.id.clone(),
                    execution_record.id.clone(),
                )),
            )
            .await;
        let execution_succeeded = execution.is_ok();
        let (result, mut event) = match execution {
            Ok(result) => result,
            Err(error) => {
                let mut paused_goal = self.active_goal()?;
                paused_goal.state = GoalState::Paused;
                paused_goal.updated_at_ms = self.clock.now_ms();
                self.store.save_goal(&paused_goal)?;
                execution_record.state = PlanExecutionState::Paused;
                self.store.save_plan_execution(&execution_record)?;
                let backend_event = BackendEvent {
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
                    "content": accepted_markdown
                }),
            )?,
        );
        Ok((result, event))
    }

    async fn request_plan_changes(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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
        plan.user_revision += 1;
        let (edited, digest) =
            self.plan_file
                .save_user_revision(&self.session.id, &plan.id, plan.user_revision)?;
        let edit_diff = self.plan_file.user_edit_diff(
            &self.session.id,
            &plan.id,
            plan.model_revision,
            &edited,
        )?;
        let annotation: Vec<PlanAnnotation> = serde_json::from_value(
            params
                .get("annotations")
                .cloned()
                .unwrap_or_else(|| json!([])),
        )?;
        let overall_comment = params
            .get("comment")
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|comment| !comment.is_empty())
            .map(str::to_owned);
        plan.state = PlanState::Revising;
        plan.review_digest = Some(digest);
        self.store.save_plan(&plan)?;
        self.session.active_plan_id = Some(plan.id.clone());
        self.save_session()?;
        let lifecycle = PlanLifecycleRecord {
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
        let instruction = PlanPrompt::revision(
            &edited,
            &edit_diff,
            &serde_json::to_string_pretty(&annotation)?,
            overall_comment.as_deref(),
        );
        let leading_event = self.event(
            "plan_changes_requested",
            json!({ "plan": &plan, "lifecycle": &lifecycle }),
        )?;
        let mut pre_revision_event = Vec::new();
        self.emit_live(
            BackendEvent {
                kind: "timeline_plan_lifecycle".into(),
                text: None,
                data: json!({
                    "kind": "plan_lifecycle",
                    "id": lifecycle.id,
                    "created_at_ms": lifecycle.created_at_ms,
                    "plan": &plan,
                    "lifecycle": &lifecycle,
                    "content": &edited
                }),
                activity: None,
                summary: None,
                task_update: None,
            },
            &mut pre_revision_event,
        )?;
        match self
            .run_interaction(
                instruction,
                PromptMode::Plan,
                Some(InteractionAdmission::plan(
                    format!("Request plan changes: {}", plan.request),
                    Some(plan.id.clone()),
                    true,
                )),
            )
            .await
        {
            Ok((result, mut event)) => {
                event.splice(0..0, pre_revision_event);
                event.insert(0, leading_event);
                Ok((result, event))
            }
            Err(error) => {
                plan.state = PlanState::AwaitingReview;
                plan.updated_at_ms = self.clock.now_ms();
                self.store.save_plan(&plan)?;
                Err(error).context("revise reviewed plan")
            }
        }
    }

    fn cancel_plan(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
        let plan_id = self
            .session
            .active_plan_id
            .take()
            .context("no plan awaits review")?;
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("active plan record is missing")?;
        plan.state = PlanState::Cancelled;
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        let lifecycle = PlanLifecycleRecord {
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
        self.save_session()?;
        Ok((
            serde_json::to_value(&plan)?,
            vec![self.event(
                "plan_cancelled",
                json!({ "plan": plan, "lifecycle": lifecycle }),
            )?],
        ))
    }

    fn activate_plan(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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
        self.session.active_plan_id = Some(plan.id.clone());
        self.save_session()?;
        Ok((
            serde_json::to_value(&plan)?,
            vec![self.event("plan_activated", serde_json::to_value(plan)?)?],
        ))
    }

    async fn set_goal(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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
                Some(InteractionAdmission::chat(format!("/goal {objective}"))),
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
            continuation_count: 0,
            max_continuation: self.goal_max_turns,
            consecutive_no_progress: 0,
            native,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
        };
        self.store.save_goal(&goal)?;
        self.session.goal_id = Some(goal.id.clone());
        self.save_session()?;
        Ok(goal)
    }

    async fn pause_goal(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
        let mut goal = self.active_goal()?;
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

    async fn resume_goal(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
        let mut goal = self.active_goal()?;
        anyhow::ensure!(
            matches!(
                goal.state,
                GoalState::Paused | GoalState::Stalled | GoalState::Blocked
            ),
            "goal cannot resume from its current state"
        );
        self.sync_native_goal(&goal, "active").await?;
        goal.resume(self.clock.now_ms());
        self.store.save_goal(&goal)?;
        self.sync_plan_execution(&goal)?;
        let prompt = if goal.native {
            "/goal resume".to_owned()
        } else {
            format!(
                "/goal resume\nContinue working toward this goal: {}",
                goal.objective
            )
        };
        let admission = self
            .store
            .list_plan_execution(&self.session.id)?
            .into_iter()
            .find(|execution| execution.goal_id == goal.id)
            .map(|execution| {
                InteractionAdmission::execution(
                    "/goal resume".into(),
                    execution.plan_id,
                    execution.id,
                )
            })
            .unwrap_or_else(|| InteractionAdmission::chat("/goal resume".into()));
        match self
            .run_interaction(prompt, PromptMode::GoalContinuation, Some(admission))
            .await
        {
            Ok((result, mut event)) => {
                event.insert(0, self.event("goal_changed", serde_json::to_value(goal)?)?);
                Ok((result, event))
            }
            Err(error) => {
                goal.state = GoalState::Paused;
                goal.updated_at_ms = self.clock.now_ms();
                self.store.save_goal(&goal)?;
                self.sync_plan_execution(&goal)?;
                let _ = self.sync_native_goal(&goal, "paused").await;
                Err(error).context("resume Harness goal")
            }
        }
    }

    async fn clear_goal(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
        let mut goal = self.active_goal()?;
        self.sync_native_goal(&goal, "cleared").await?;
        goal.state = GoalState::Cleared;
        goal.updated_at_ms = self.clock.now_ms();
        self.store.save_goal(&goal)?;
        self.sync_plan_execution(&goal)?;
        self.session.goal_id = None;
        self.save_session()?;
        Ok((
            serde_json::to_value(&goal)?,
            vec![self.event("goal_changed", serde_json::to_value(goal)?)?],
        ))
    }

    async fn continue_goal(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
        let goal = self.active_goal()?;
        anyhow::ensure!(goal.state == GoalState::Active, "goal is not active");
        let prompt = if goal.native {
            "/goal resume".to_owned()
        } else {
            format!("Continue working toward this goal: {}", goal.objective)
        };
        self.run_interaction(prompt, PromptMode::GoalContinuation, None)
            .await
    }

    fn apply_goal_evidence(
        &mut self,
        evidence: crate::goal::TurnEvidence,
        event: &mut Vec<BrokerEvent>,
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
            | ContinuationDecision::Stalled => {
                event.push(self.event("goal_changed", serde_json::to_value(&goal)?)?);
                false
            }
            ContinuationDecision::Stop => false,
        };
        self.store.save_goal(&goal)?;
        self.sync_plan_execution(&goal)?;
        Ok(continuing)
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
        execution.state = match goal.state {
            GoalState::Active => PlanExecutionState::Active,
            GoalState::Paused | GoalState::Cleared => PlanExecutionState::Paused,
            GoalState::Complete => PlanExecutionState::Complete,
            GoalState::Blocked => PlanExecutionState::Blocked,
            GoalState::Stalled => PlanExecutionState::Stalled,
        };
        if execution.state != PlanExecutionState::Active {
            execution.completed_at_ms = Some(self.clock.now_ms());
        }
        self.store.save_plan_execution(&execution)
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

    async fn sync_native_goal(&self, goal: &GoalRecord, status: &str) -> Result<()> {
        if !goal.native || self.session.backend_session_id.is_none() {
            return Ok(());
        }
        self.backend
            .goal_status(
                self.backend_request(String::new(), PromptMode::GoalContinuation),
                Some(goal.objective.clone()),
                status,
            )
            .await
    }

    fn save_interaction_comment(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let mut comment: InteractionComment = serde_json::from_value(params)?;
        anyhow::ensure!(
            self.store
                .list_interaction(&self.session.id)?
                .iter()
                .any(|interaction| interaction.id == comment.interaction_id),
            "interaction comment does not belong to the active session"
        );
        comment.created_at_ms = self.clock.now_ms();
        self.store.save_interaction_comment(&comment)?;
        Ok((serde_json::to_value(comment)?, Vec::new()))
    }

    fn list_interaction_review(&self) -> Result<Vec<InteractionRecord>> {
        let mut interaction_list = self.store.list_interaction(&self.session.id)?;
        for interaction in &mut interaction_list {
            interaction.comment = self.store.list_interaction_comment(&interaction.id)?;
        }
        Ok(interaction_list)
    }

    async fn request_interaction_changes(
        &mut self,
        params: Value,
    ) -> Result<(Value, Vec<BrokerEvent>)> {
        let interaction_id = required_text(&params, "interaction_id")?;
        let interaction = self
            .store
            .list_interaction(&self.session.id)?
            .into_iter()
            .find(|item| item.id == interaction_id)
            .context("interaction not found")?;
        let comment = self.store.list_interaction_comment(&interaction.id)?;
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
            Some(InteractionAdmission::chat(format!(
                "Request changes for interaction {}",
                interaction.ordinal
            ))),
        )
        .await
    }

    async fn rollback_interaction(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let interaction_id = required_text(&params, "interaction_id")?;
        let mut interaction = self.store.list_interaction(&self.session.id)?;
        let target_index = interaction
            .iter()
            .position(|item| item.id == interaction_id)
            .context("interaction not found")?;
        let expected_id = interaction
            .last()
            .and_then(|item| item.checkpoint_after.as_deref())
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
        GitCheckpoint::new(workspace).restore(&self.store, &expected, &target)?;
        for (index, record) in interaction.iter_mut().enumerate().skip(target_index) {
            record.state = if index == target_index {
                InteractionState::RolledBack
            } else {
                InteractionState::Superseded
            };
            self.store.save_interaction(record)?;
        }
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

    async fn new_session(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
        self.pause_current_goal().await?;
        self.release_lease()?;
        self.interaction_runtime = None;
        self.agent_registry = AgentRegistry::default();
        self.agent_runtime_by_run.clear();
        let now_ms = self.clock.now_ms();
        self.session = HarnessSession {
            id: Uuid::new_v4().to_string(),
            name: String::new(),
            workspace: self.session.workspace.clone(),
            backend: self.session.backend.clone(),
            backend_session_id: None,
            model: self.session.model.clone(),
            provider_label: self.session.provider_label.clone(),
            resolved_model: self.session.resolved_model.clone(),
            effort: self.session.effort.clone(),
            context_window: self.session.context_window.clone(),
            fast_mode: self.session.fast_mode,
            execution_mode: ExecutionMode::Read,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
            active_plan_id: None,
            goal_id: None,
            lease_owner: Some(self.client_id.clone()),
            lease_expires_at_ms: Some(now_ms + 30_000),
            native_fork: false,
            native_compact: self.backend.descriptor().capability.native_compact,
            context_usage: None,
        };
        self.capability = self.backend.descriptor().capability;
        self.store.save_session(&self.session)?;
        Ok((
            serde_json::to_value(self.snapshot()?)?,
            vec![self.event("session_changed", serde_json::to_value(&self.session)?)?],
        ))
    }

    fn list_session(&self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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

    fn preview_session(&self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        let preview_session = self
            .store
            .load_session(&session_id)?
            .context("session not found")?;
        let interaction = self.store.list_interaction(&session_id)?;
        let plan_list = self.store.list_plan(&session_id)?;
        let agent_run_list = self.store.list_agent_run(&session_id)?;
        let timeline = TimelineProjector::build(
            interaction.clone(),
            &plan_list,
            self.store.list_plan_lifecycle(&session_id)?,
            self.store.list_plan_execution(&session_id)?,
            agent_run_list.clone(),
            self.store.list_session_event(&session_id)?,
            &self.plan_file,
        )?;
        let mut agent_turn_list = Vec::new();
        for run in &agent_run_list {
            agent_turn_list.extend(self.store.list_agent_turn(&run.id)?);
        }
        let preview = SessionPreview {
            session: preview_session,
            interaction,
            timeline,
            agent: AgentSnapshot {
                definition: Vec::new(),
                run: agent_run_list,
                turn: agent_turn_list,
            },
        };
        Ok((serde_json::to_value(preview)?, Vec::new()))
    }

    async fn resume_session(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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
        self.interaction_runtime = None;
        self.session = session;
        self.agent_registry = load_agent_registry(&self.store, &self.session.id)?;
        self.agent_runtime_by_run.clear();
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

    fn rename_session(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        let name = required_text(&params, "name")?.trim().to_owned();
        let mut session = self
            .store
            .load_session(&session_id)?
            .context("session not found")?;
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
        let entry = TimelineEntry::SessionEvent {
            id: timeline_event.id.clone(),
            created_at_ms,
            event: timeline_event,
        };
        Ok((
            serde_json::to_value(session)?,
            vec![self.event(
                "backend_event",
                json!({ "kind": "timeline_session_event", "data": entry }),
            )?],
        ))
    }

    async fn configure_session(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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
                .model_list(self.backend_request(String::new(), PromptMode::Chat))
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

    fn delete_session(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
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

    async fn compact_session(&mut self) -> Result<(Value, Vec<BrokerEvent>)> {
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
            .compact(self.backend_request(String::new(), PromptMode::Chat))
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

    async fn fork_session(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let fork_started = Instant::now();
        let mut timing = Vec::new();
        let source = match params.get("session_id").and_then(Value::as_str) {
            Some(session_id) => self
                .store
                .load_session(session_id)?
                .context("source session not found")?,
            None => self.session.clone(),
        };
        anyhow::ensure!(
            source.native_fork || (source.id == self.session.id && self.capability.native_fork),
            "native fork is unavailable for this backend session"
        );
        anyhow::ensure!(
            source.backend == self.backend_launch.kind,
            "source session uses a different configured backend"
        );
        let pause_started = Instant::now();
        self.pause_current_goal().await?;
        timing.push(crate::backend::BackendTimingRecord {
            phase: "broker.pause_goal".into(),
            duration_ms: pause_started.elapsed().as_secs_f64() * 1000.0,
        });
        let backend_started = Instant::now();
        let backend_fork = self
            .backend
            .fork(BackendRequest {
                workspace: source.workspace.clone(),
                text: String::new(),
                mode: PromptMode::Chat,
                model: source.model.clone(),
                effort: source.effort.clone(),
                context_window: source.context_window.clone(),
                fast_mode: source.fast_mode,
                execution_mode: ExecutionMode::Read,
                backend_session_id: source.backend_session_id.clone(),
            })
            .await?;
        let backend_duration_ms = backend_started.elapsed().as_secs_f64() * 1000.0;
        timing.extend(backend_fork.timing);
        timing.push(crate::backend::BackendTimingRecord {
            phase: "broker.backend_total".into(),
            duration_ms: backend_duration_ms,
        });
        let backend_session_id = backend_fork.backend_session_id;
        let persistence_started = Instant::now();
        let now_ms = self.clock.now_ms();
        let mut fork = source.clone();
        fork.id = Uuid::new_v4().to_string();
        fork.name = resolve_fork_name(&params, &source.name);
        fork.backend_session_id = Some(backend_session_id);
        fork.context_usage.clone_from(&source.context_usage);
        fork.execution_mode = ExecutionMode::Read;
        fork.active_plan_id = None;
        fork.goal_id = None;
        fork.created_at_ms = now_ms;
        fork.updated_at_ms = now_ms;
        fork.lease_owner = Some(self.client_id.clone());
        fork.lease_expires_at_ms = Some(now_ms + 30_000);
        self.store.save_session(&fork)?;
        self.store.save_session_event(&SessionEventRecord {
            id: Uuid::new_v4().to_string(),
            session_id: fork.id.clone(),
            created_at_ms: now_ms,
            detail: SessionEventKind::Forked {
                source_session_id: source.id,
                source_session_name: source.name,
            },
        })?;
        self.release_lease()?;
        self.workspace_kind = crate::workspace::resolve(Path::new(&fork.workspace))?;
        self.interaction_runtime = None;
        self.session = fork;
        self.agent_registry = load_agent_registry(&self.store, &self.session.id)?;
        self.agent_runtime_by_run.clear();
        timing.push(crate::backend::BackendTimingRecord {
            phase: "broker.persist_activate".into(),
            duration_ms: persistence_started.elapsed().as_secs_f64() * 1000.0,
        });
        let snapshot_started = Instant::now();
        let mut snapshot = serde_json::to_value(self.snapshot()?)?;
        timing.push(crate::backend::BackendTimingRecord {
            phase: "broker.snapshot".into(),
            duration_ms: snapshot_started.elapsed().as_secs_f64() * 1000.0,
        });
        snapshot["fork_performance"] = json!({
            "total_ms": fork_started.elapsed().as_secs_f64() * 1000.0,
            "timing": timing,
        });
        Ok((
            snapshot,
            vec![self.event("session_changed", serde_json::to_value(&self.session)?)?],
        ))
    }

    fn backend_request(&self, text: String, mode: PromptMode) -> BackendRequest {
        BackendRequest {
            workspace: self.session.workspace.clone(),
            text,
            mode,
            model: self.session.model.clone(),
            effort: self.session.effort.clone(),
            context_window: self.session.context_window.clone(),
            fast_mode: self.session.fast_mode,
            execution_mode: self.session.execution_mode,
            backend_session_id: self.session.backend_session_id.clone(),
        }
    }

    fn event(&self, name: &str, payload: Value) -> Result<BrokerEvent> {
        Ok(BrokerEvent {
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

    fn release_lease(&mut self) -> Result<()> {
        self.store
            .release_session_lease(&self.session.id, &self.client_id)
    }

    /// Resolve the durable data root for diagnostics and manual verification.
    pub fn data_root(&self) -> &Path {
        &self.data_root
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

/// Load durable child runs while removing placeholders created by legacy lifecycle parsing.
fn referenced_agent_run_id_list(interaction: &InteractionRecord) -> Vec<String> {
    interaction
        .node_list
        .iter()
        .filter_map(|node| match node {
            InteractionNode::AgentReference { agent } => Some(agent.agent_run_id.clone()),
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

fn resolve_fork_name(params: &Value, source_name: &str) -> String {
    params
        .get("name")
        .and_then(Value::as_str)
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(str::to_owned)
        .unwrap_or_else(|| {
            if source_name.is_empty() {
                "(fork)".into()
            } else {
                format!("{source_name} (fork)")
            }
        })
}

fn goal_state_name(state: GoalState) -> &'static str {
    match state {
        GoalState::Active => "active",
        GoalState::Paused => "paused",
        GoalState::Complete => "complete",
        GoalState::Blocked => "blocked",
        GoalState::Stalled => "stalled",
        GoalState::Cleared => "cleared",
    }
}

fn plan_title(markdown: &str, request: &str) -> String {
    markdown
        .lines()
        .find_map(|line| line.trim().strip_prefix("# ").map(str::trim))
        .filter(|title| !title.is_empty())
        .unwrap_or(request)
        .chars()
        .take(100)
        .collect()
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

    #[test]
    fn resolves_explicit_named_and_unnamed_fork_names() {
        assert_eq!(
            resolve_fork_name(&json!({ "name": "  investigation  " }), "review"),
            "investigation"
        );
        assert_eq!(resolve_fork_name(&json!({}), "review"), "review (fork)");
        assert_eq!(resolve_fork_name(&json!({}), ""), "(fork)");
    }

    fn completed_interaction(
        id: &str,
        session_id: &str,
        node_list: Vec<InteractionNode>,
    ) -> InteractionRecord {
        InteractionRecord {
            id: id.into(),
            session_id: session_id.into(),
            ordinal: 1,
            prompt: "test interaction".into(),
            kind: InteractionKind::Chat,
            plan_id: None,
            execution_id: None,
            state: InteractionState::Complete,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            created_at_ms: 1,
            completed_at_ms: Some(2),
            node_list,
            awaiting_input: false,
            elicitation: None,
            duration_ms: 1,
            token_count: None,
            comment: Vec::new(),
            task: None,
        }
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
            _request: BackendRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            anyhow::bail!("synthetic provider failure")
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
                    kind: "assistant_message".into(),
                    text: Some("An intermediate answer.".into()),
                    data: Value::Null,
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                BackendEvent {
                    kind: "parent_boundary".into(),
                    text: None,
                    data: json!({ "boundary": "wait_started", "agent_count": 1 }),
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                BackendEvent {
                    kind: "parent_boundary".into(),
                    text: None,
                    data: json!({ "boundary": "wait_ended", "agent_count": 0 }),
                    activity: None,
                    summary: None,
                    task_update: None,
                },
                BackendEvent {
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
            _request: BackendRequest,
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
            let mut output = crate::backend::BackendOutput {
                backend_session_id: Some("mutable-question".into()),
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Mutable question test".into(),
                    model: Some(request.model),
                },
                ..crate::backend::BackendOutput::default()
            };
            if request
                .text
                .contains("User follow-up:\nreplace the options")
            {
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
            } else if request.text.contains("User follow-up:\nuse staged") {
                output.question_answer = Some(PlanQuestionAnswer {
                    question_id: "migration".into(),
                    response: PlanQuestionResponse::Selected {
                        option: "Staged".into(),
                        feedback: None,
                    },
                });
            } else if request
                .text
                .contains("User follow-up:\nthis decision is not material")
            {
                output.question_withdrawal = Some(PlanQuestionWithdrawal {
                    reason: "Repository policy determines the migration.".into(),
                });
            } else if request
                .text
                .contains("Pending planning questions withdrawn")
                || request
                    .text
                    .contains("Planning feedback:\n- Migration: Staged")
                || request
                    .text
                    .contains("The user answered the pending Harness questions")
            {
                if self.fail_withdrawal_continuation
                    && request
                        .text
                        .contains("Pending planning questions withdrawn")
                {
                    anyhow::bail!("synthetic withdrawal continuation failure");
                }
                output.plan_markdown = (request.mode == PromptMode::Plan)
                    .then(|| "# Migration plan\n\n## Overview\n\nFollow repository policy.".into());
            } else {
                output.plan_question = Some(question_set());
            }
            Ok(output)
        }

        async fn fork(
            &self,
            _request: BackendRequest,
        ) -> Result<crate::backend::BackendForkResult> {
            Ok(crate::backend::BackendForkResult::unprofiled(
                "mutable-question-fork",
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
            Ok(crate::backend::BackendOutput {
                backend_session_id: Some("planning-question".into()),
                event: vec![event],
                plan_markdown: (turn > 0).then(|| {
                    "# Migration plan\n\n## Overview\n\nUse the selected migration strategy.".into()
                }),
                plan_question,
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Planning question test".into(),
                    model: Some(request.model),
                },
                ..crate::backend::BackendOutput::default()
            })
        }

        async fn fork(
            &self,
            _request: BackendRequest,
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
            _request: BackendRequest,
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
            _request: BackendRequest,
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
            name: "name".into(),
            workspace: "work".into(),
            backend: "mock".into(),
            backend_session_id: None,
            model: "model".into(),
            provider_label: "Mock backend".into(),
            resolved_model: Some("model".into()),
            effort: "low".into(),
            context_window: None,
            fast_mode: false,
            execution_mode: ExecutionMode::Read,
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

    #[test]
    fn unknown_non_spawn_lifecycle_does_not_create_an_agent_run() {
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
            status: AgentRunStatus::Completed,
        };

        broker
            .apply_agent_lifecycle(
                &BackendEvent {
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
            .unwrap();

        assert!(broker.agent_registry.list().is_empty());
        assert!(event.is_empty());
    }

    #[test]
    fn subagent_activity_binds_the_pending_explicit_run() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut pending = AgentRun::pending(&broker.session.id, "explorer", "inspect Bevy", 100);
        pending.parent_interaction_id = Some("parent-interaction".into());
        broker.store.save_agent_run(&pending).unwrap();
        broker.agent_registry.insert(pending.clone());
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
            status: AgentRunStatus::Running,
        };

        broker
            .apply_agent_lifecycle(
                &BackendEvent {
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
            .unwrap();

        let run_list = broker.agent_registry.list();
        assert_eq!(run_list.len(), 1);
        assert_eq!(run_list[0].id, pending.id);
        assert_eq!(run_list[0].definition, "explorer");
        assert_eq!(
            run_list[0].parent_interaction_id.as_deref(),
            Some("parent-interaction")
        );
        assert_eq!(
            run_list[0].provider_thread_id.as_deref(),
            Some("child-thread")
        );
    }

    #[test]
    fn child_turn_completion_moves_the_run_to_done_without_losing_resume() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut run = AgentRun::pending(&broker.session.id, "explorer", "inspect Bevy", 100);
        run.provider_thread_id = Some("child-thread".into());
        run.status = AgentRunStatus::Running;
        broker.store.save_agent_run(&run).unwrap();
        broker.agent_registry.insert(run.clone());
        let mut event = Vec::new();

        let routed = broker
            .route_agent_backend_event(
                &BackendEvent {
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
            .unwrap();

        let completed = broker.agent_registry.get(&run.id).unwrap();
        assert!(routed);
        assert_eq!(completed.status, AgentRunStatus::Completed);
        assert!(completed.status.accepts_prompt());
    }

    #[tokio::test]
    async fn shares_prompt_history_across_repository_sessions() {
        let first_repository = repository();
        let second_repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut first = planning_question_broker(first_repository.path(), data.path(), false);
        let recorded = first
            .dispatch(BrokerRequest {
                id: 1,
                method: "history.record".into(),
                params: json!({ "text": "inspect the architecture" }),
            })
            .await;
        assert!(recorded.response.error.is_none());
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
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "prompt me with a multiple-choice question" }),
            })
            .await;
        assert!(prompted.response.error.is_none());
        assert!(prompted.event.iter().any(|event| event.event == "question"));
        let pending = broker.snapshot().unwrap().active_elicitation.unwrap();
        assert_eq!(pending.owner, "interaction");
        assert_eq!(
            pending.elicitation.current_question().unwrap().header,
            "Migration"
        );

        let answered = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "question.answer".into(),
                params: json!({
                    "question_id": pending.elicitation.current_question().unwrap().id,
                    "response": { "kind": "selected", "option": "Staged" }
                }),
            })
            .await;
        assert!(answered.response.error.is_none());

        let continued = broker
            .dispatch(BrokerRequest {
                id: 3,
                method: "question.continue".into(),
                params: json!({}),
            })
            .await;
        assert!(continued.response.error.is_none());
        assert!(broker.snapshot().unwrap().active_elicitation.is_none());
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let prompted = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "establish provider context" }),
            })
            .await;
        assert!(prompted.response.error.is_none());
        let interaction_count = broker
            .store
            .list_interaction(&broker.session.id)
            .unwrap()
            .len();

        let compacted = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "session.compact".into(),
                params: json!({}),
            })
            .await;

        assert!(compacted.response.error.is_none());
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
                .list_interaction(&broker.session.id)
                .unwrap()
                .len(),
            interaction_count
        );
        assert_eq!(compacted.event[0].event, "context_compacted");
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
            .dispatch(BrokerRequest {
                id: 0,
                method: "session.execution_mode".into(),
                params: json!({ "mode": "full" }),
            })
            .await;
        let prompt = source
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "preserve this interaction" }),
            })
            .await;
        assert!(prompt.response.error.is_none());
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

        let mut fork_controller = HarnessBroker::initialize_with_clock(
            initialize("fork-client", Some("new".into())),
            Box::new(FixedClock(110)),
        )
        .unwrap();
        let forked = fork_controller
            .dispatch(BrokerRequest {
                id: 2,
                method: "session.fork".into(),
                params: json!({ "session_id": source_session_id, "name": "fork investigation" }),
            })
            .await;
        assert!(forked.response.error.is_none());
        let snapshot = forked.response.result.expect("fork snapshot");
        assert_ne!(snapshot["session"]["id"], source_session_id);
        assert_eq!(snapshot["session"]["name"], "fork investigation");
        assert_eq!(snapshot["session"]["execution_mode"], "read");
        assert_eq!(snapshot["session"]["context_usage"]["used"], 50);
        assert_eq!(snapshot["session"]["context_usage"]["size"], 200);
        assert_eq!(
            snapshot["session"]["context_usage"]["remaining_percent"],
            75
        );
        assert!(snapshot["fork_performance"]["total_ms"].as_f64().is_some());
        let timing = snapshot["fork_performance"]["timing"]
            .as_array()
            .expect("fork timing list");
        assert!(
            timing
                .iter()
                .any(|entry| entry["phase"] == "broker.backend_total")
        );
        assert!(
            timing
                .iter()
                .any(|entry| entry["phase"] == "broker.persist_activate")
        );
        assert!(
            timing
                .iter()
                .any(|entry| entry["phase"] == "broker.snapshot")
        );
        assert_eq!(snapshot["interaction"].as_array().map(Vec::len), Some(0));
        assert_eq!(snapshot["timeline"].as_array().map(Vec::len), Some(1));
        assert_eq!(snapshot["timeline"][0]["kind"], "session_event");
        assert_eq!(snapshot["timeline"][0]["event"]["kind"], "forked");
        assert_eq!(
            snapshot["timeline"][0]["event"]["source_session_id"],
            source_session_id
        );
        let persisted_source = fork_controller
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let selected = broker
            .dispatch(BrokerRequest {
                id: 0,
                method: "session.execution_mode".into(),
                params: json!({ "mode": "yolo" }),
            })
            .await;
        assert!(selected.response.error.is_none());
        let configured = broker
            .dispatch(BrokerRequest {
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
        assert!(configured.response.error.is_none());
        broker
            .dispatch(BrokerRequest {
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
            .dispatch(BrokerRequest {
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
            .dispatch(BrokerRequest {
                id: 2,
                method: "session.new".into(),
                params: json!({}),
            })
            .await;
        let session = created.response.result.unwrap()["session"].clone();
        assert_eq!(session["name"], "");
        assert_eq!(session["model"], "gpt-5.6");
        assert_eq!(session["effort"], "low");
        assert_eq!(session["context_window"], "default");
        assert_eq!(session["fast_mode"], true);
        assert_eq!(session["execution_mode"], "read");
        let selected = broker
            .dispatch(BrokerRequest {
                id: 21,
                method: "session.execution_mode".into(),
                params: json!({ "mode": "full" }),
            })
            .await;
        assert!(selected.response.error.is_none());
        let renamed = broker
            .dispatch(BrokerRequest {
                id: 3,
                method: "session.rename".into(),
                params: json!({ "session_id": session["id"], "name": "  Architecture review  " }),
            })
            .await;
        assert_eq!(
            renamed.response.result.as_ref().unwrap()["name"],
            "Architecture review"
        );
        assert!(renamed.event.iter().any(|event| {
            event.event == "backend_event"
                && event.payload["kind"] == "timeline_session_event"
                && event.payload["data"]["event"]["name"] == "Architecture review"
        }));
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
            restarted
                .snapshot()
                .unwrap()
                .timeline
                .iter()
                .any(|entry| matches!(
                    entry,
                    TimelineEntry::SessionEvent { event, .. }
                        if matches!(
                            &event.detail,
                            SessionEventKind::Renamed { name } if name == "Architecture review"
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();

        let invalid_model = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "session.configure".into(),
                params: json!({ "model": "sol", "validate": true }),
            })
            .await;
        assert!(invalid_model.response.error.is_some());
        assert_eq!(broker.session.model, "mock-model");

        let invalid_effort = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "session.configure".into(),
                params: json!({ "effort": "xhigh", "validate": true }),
            })
            .await;
        assert!(invalid_effort.response.error.is_some());
        assert_eq!(broker.session.effort, "medium");

        let invalid_mode = broker
            .dispatch(BrokerRequest {
                id: 3,
                method: "session.execution_mode".into(),
                params: json!({ "mode": "invalid" }),
            })
            .await;
        assert!(invalid_mode.response.error.is_some());
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let source_session_id = broker.session.id.clone();
        let prompt = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "preserve preview history" }),
            })
            .await;
        assert!(prompt.response.error.is_none());
        let created = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "session.new".into(),
                params: json!({}),
            })
            .await;
        assert!(created.response.error.is_none());
        let mut source_session = broker
            .store
            .load_session(&source_session_id)
            .unwrap()
            .expect("source session");
        source_session.lease_owner = Some("foreign-client".into());
        source_session.lease_expires_at_ms = Some(500);
        broker.store.save_session(&source_session).unwrap();

        let preview = broker
            .dispatch(BrokerRequest {
                id: 3,
                method: "session.preview".into(),
                params: json!({ "session_id": source_session_id }),
            })
            .await;
        assert!(preview.response.error.is_none());
        let result = preview.response.result.expect("session preview");
        assert_eq!(result["session"]["id"], source_session_id);
        assert_eq!(result["interaction"].as_array().map(Vec::len), Some(1));
        assert_eq!(
            result["interaction"][0]["prompt"],
            "preserve preview history"
        );
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let session_id = broker.session.id.clone();
        broker.session.name = "Persistent analysis".into();
        broker.store.save_session(&broker.session).unwrap();
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
        assert!(snapshot.interaction.is_empty());
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

        let paused = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert!(paused.response.error.is_none());
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
        assert!(paused_snapshot.artifact.is_empty());
        assert!(paused_snapshot.interaction.last().unwrap().awaiting_input);

        let question_id = elicitation.question_set.questions[0].id.clone();
        let clarified = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "question.ask".into(),
                params: json!({
                    "question_id": question_id,
                    "text": "What compatibility cost does staged migration add?"
                }),
            })
            .await;
        assert!(clarified.response.error.is_none());
        let clarified_plan = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(clarified_plan.state, PlanState::AwaitingInput);
        assert!(
            clarified_plan
                .elicitation
                .as_ref()
                .unwrap()
                .clarification_active
        );
        let follow_up = broker
            .dispatch(BrokerRequest {
                id: 3,
                method: "prompt.submit".into(),
                params: json!({ "text": "How long should compatibility remain?" }),
            })
            .await;
        assert!(follow_up.response.error.is_none());
        assert_eq!(
            broker.snapshot().unwrap().active_plan.unwrap().state,
            PlanState::AwaitingInput
        );
        let answered = broker
            .dispatch(BrokerRequest {
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
        assert!(answered.response.error.is_none());
        let completed = broker
            .dispatch(BrokerRequest {
                id: 5,
                method: "question.continue".into(),
                params: json!({}),
            })
            .await;
        assert!(completed.response.error.is_none());
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
                .any(|event| event.event == "plan_created")
        );
        let completed_snapshot = broker.snapshot().unwrap();
        let plan = completed_snapshot.active_plan.expect("submitted plan");
        assert_eq!(plan.state, PlanState::AwaitingReview);
        assert!(plan.elicitation.is_none());
        assert_eq!(completed_snapshot.artifact.len(), 1);
        assert!(Path::new(&plan.working_path).exists());
        assert_eq!(completed_snapshot.interaction.len(), 4);
        assert_eq!(completed_snapshot.interaction[3].plan_id, Some(plan.id));
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
    }

    #[tokio::test]
    async fn replaces_plan_questions_on_the_original_owner_and_preserves_valid_answers() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), true);
        broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let initial = broker.snapshot().unwrap().active_plan.unwrap();
        let question = initial.elicitation.as_ref().unwrap().question_set.questions[0].clone();
        broker
            .dispatch(BrokerRequest {
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
    async fn clarification_replaces_the_complete_pending_question_set() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = mutable_question_broker(repository.path(), data.path());
        broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "replace the options" }),
            })
            .await;
        assert!(result.response.error.is_none());
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
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "ask which migration to use" }),
            })
            .await;
        let result = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "use staged" }),
            })
            .await;
        assert!(result.response.error.is_none());
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
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "use staged" }),
            })
            .await;
        assert!(result.response.error.is_none());
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
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "this decision is not material" }),
            })
            .await;
        assert!(result.response.error.is_none());
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
    async fn failed_withdrawal_continuation_restores_the_pending_question() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = mutable_question_broker(repository.path(), data.path());
        broker.backend = Arc::new(MutableQuestionBackend {
            fail_withdrawal_continuation: true,
        });
        broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        let result = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "prompt.submit".into(),
                params: json!({ "text": "this decision is not material" }),
            })
            .await;
        assert!(result.response.error.is_some());
        let plan = broker.snapshot().unwrap().active_plan.unwrap();
        assert_eq!(plan.state, PlanState::AwaitingInput);
        assert!(plan.elicitation.is_some());
        assert!(
            !broker
                .store
                .list_plan_lifecycle(&broker.session.id)
                .unwrap()
                .iter()
                .any(|entry| entry.kind == PlanLifecycleKind::QuestionWithdrawn)
        );
    }

    #[tokio::test]
    async fn treats_an_unstructured_planning_question_as_freeform_feedback() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);

        let paused = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "/plan migrate the event format" }),
            })
            .await;
        assert!(paused.response.error.is_none());
        let question = broker
            .snapshot()
            .unwrap()
            .active_plan
            .unwrap()
            .elicitation
            .unwrap()
            .question_set;
        assert!(question.questions[0].options.is_empty());
        assert!(question.questions[0].allow_freeform);
        assert_eq!(
            question.questions[0].question,
            "Which migration strategy should the plan use?"
        );
    }

    #[tokio::test]
    async fn reviews_and_accepts_a_mock_plan_before_execution() {
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let (event_sink, mut event_stream) = tokio::sync::mpsc::unbounded_channel();
        let planned = broker
            .dispatch_stream(
                BrokerRequest {
                    id: 1,
                    method: "prompt.submit".into(),
                    params: json!({ "text": "/plan build the feature" }),
                },
                event_sink,
            )
            .await;
        assert!(planned.response.error.is_none());
        assert_eq!(
            event_stream.try_recv().unwrap().kind,
            "timeline_interaction_started",
            "the durable interaction must reach the live stream before provider progress"
        );
        assert_eq!(
            event_stream.try_recv().unwrap().kind,
            "timeline_node_updated",
            "provider progress must reach the live stream before the final response is rendered"
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
        assert!(planned_snapshot.timeline.iter().any(|entry| matches!(
            entry,
            TimelineEntry::PlanLifecycle { lifecycle, .. }
                if lifecycle.kind == PlanLifecycleKind::Created
        )));

        std::fs::write(path, "# Accepted plan\n\n1. Finish everything.\n").unwrap();
        let accepted = broker
            .dispatch(BrokerRequest {
                id: 2,
                method: "plan.accept".into(),
                params: json!({}),
            })
            .await;
        assert!(
            accepted.response.error.is_none(),
            "{:?}",
            accepted.response.error
        );
        assert_eq!(broker.session.execution_mode, ExecutionMode::Read);
        let goal = broker.active_goal().unwrap();
        assert_eq!(goal.objective, "Complete the plan");
        assert!(broker.snapshot().unwrap().active_execution.is_some());
        let duplicate_acceptance = broker
            .dispatch(BrokerRequest {
                id: 20,
                method: "plan.accept".into(),
                params: json!({}),
            })
            .await;
        assert!(duplicate_acceptance.response.error.is_some());
        let interaction_count = broker
            .store
            .list_interaction(&broker.session.id)
            .unwrap()
            .len();
        let paused = broker
            .dispatch(BrokerRequest {
                id: 3,
                method: "goal.pause".into(),
                params: json!({}),
            })
            .await;
        assert!(paused.response.error.is_none());
        let resumed = broker
            .dispatch(BrokerRequest {
                id: 4,
                method: "goal.resume".into(),
                params: json!({}),
            })
            .await;
        assert!(resumed.response.error.is_none());
        assert_eq!(
            broker
                .store
                .list_interaction(&broker.session.id)
                .unwrap()
                .len(),
            interaction_count + 1,
            "an explicit goal resume must start a new user interaction"
        );
        let plan = broker
            .store
            .load_plan(broker.session.active_plan_id.as_deref().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(plan.state, PlanState::Accepted);
        assert!(plan.accepted_digest.is_some());
        let interaction = broker.store.list_interaction(&broker.session.id).unwrap();
        assert_eq!(interaction[0].prompt, "/plan build the feature");
        assert_eq!(interaction[1].prompt, "Accept plan: build the feature");
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(200)),
        )
        .unwrap();
        broker.backend = Arc::new(FailingBackend);
        let result = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "goal.set".into(),
                params: json!({ "objective": "finish the work" }),
            })
            .await;
        assert!(result.response.error.is_some());
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        let interaction = broker.store.list_interaction(&broker.session.id).unwrap();
        assert_eq!(interaction.len(), 1);
        assert_eq!(interaction[0].state, InteractionState::Failed);
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(250)),
        )
        .unwrap();
        broker.backend = Arc::new(TaskUpdateBackend);
        let result = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "answer an ordinary question" }),
            })
            .await;
        assert!(result.response.error.is_none());
        let interaction = broker.store.list_interaction(&broker.session.id).unwrap();
        let task = interaction[0]
            .task
            .as_ref()
            .expect("ordinary task snapshot");
        assert_eq!(task.current[0].title, "Inspect ordinary chat");
        assert_eq!(interaction[0].kind, InteractionKind::Chat);
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(500)),
        )
        .unwrap();
        broker.backend = Arc::new(ParentBoundaryBackend);

        let result = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "wait for the explorer" }),
            })
            .await;
        assert!(result.response.error.is_none());
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
            snapshot.interaction[0]
                .node_list
                .iter()
                .all(|node| !node.id().ends_with(":wait"))
        );
        let segment_list = snapshot.interaction[0]
            .node_list
            .iter()
            .filter_map(|node| match node {
                crate::interaction::InteractionNode::MainSegment { segment } => Some(segment),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(segment_list.len(), 2);
        assert_eq!(
            segment_list[0].response.as_deref(),
            Some("An intermediate answer.")
        );
        assert_eq!(
            segment_list[1].response.as_deref(),
            Some("A final synthesis.")
        );
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        broker.backend = Arc::new(NestedWriteBackend);
        let result = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "create a nested module" }),
            })
            .await;
        assert!(
            result.response.error.is_none(),
            "{:?}",
            result.response.error
        );
        let interaction = broker.store.list_interaction(&broker.session.id).unwrap();
        assert_eq!(interaction.len(), 1);
        let segment_list = interaction[0]
            .node_list
            .iter()
            .filter_map(|node| match node {
                crate::interaction::InteractionNode::MainSegment { segment } => Some(segment),
                _ => None,
            })
            .collect::<Vec<_>>();
        let thought_list = segment_list
            .iter()
            .flat_map(|segment| segment.thought.iter())
            .collect::<Vec<_>>();
        assert_eq!(thought_list.len(), 1);
        let thought = thought_list[0];
        assert_eq!(thought.text, "Creating the nested module.");
        assert_eq!(thought.tool.len(), 2);
        let thought_diff = thought.diff_text.as_deref().expect("provider thought diff");
        assert!(thought_diff.contains("seed.txt"));
        assert!(!thought_diff.contains("apps/new/deep/module/lib.rs"));
        assert!(!thought_diff.contains("target/generated/deep/artifact.txt"));
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
            segment_list.iter().any(|segment| {
                segment.response.as_deref() == Some("The nested module is ready.")
            })
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        broker.backend = Arc::new(NestedWriteBackend);

        let result = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "change the seed" }),
            })
            .await;

        assert!(
            result.response.error.is_none(),
            "{:?}",
            result.response.error
        );
        let interaction = broker.store.list_interaction(&broker.session.id).unwrap();
        assert_eq!(interaction.len(), 1);
        assert!(interaction[0].attributed_matches_checkpoint);
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        let child_run_id = "child-run";
        let child_interaction = completed_interaction(
            "child-interaction",
            &broker.session.id,
            vec![InteractionNode::MainSegment {
                segment: Box::new(crate::interaction::MainSegment {
                    id: "child-segment".into(),
                    state: crate::interaction::SegmentState::Complete,
                    started_at_ms: 1,
                    completed_at_ms: Some(2),
                    duration_ms: 1,
                    token_count: None,
                    spawned_agent_count: 0,
                    thought: vec![CompletedThought {
                        id: "child-thought".into(),
                        text: "Editing the child file.".into(),
                        synthetic: false,
                        tool: vec![crate::interaction::CompletedTool {
                            id: "child-change".into(),
                            kind: "file_change".into(),
                            title: "file changes".into(),
                            output: String::new(),
                            status: "completed".into(),
                            failed: false,
                            change: crate::backend::ProviderChangeSet {
                                file: vec![crate::backend::ProviderFileChange {
                                    path: "child.rs".into(),
                                    move_path: None,
                                    kind: crate::backend::ProviderChangeKind::Add,
                                    diff: "child".into(),
                                }],
                            },
                        }],
                        started_at_ms: 1,
                        completed_at_ms: 2,
                        diff_text: None,
                        task_id: None,
                    }],
                    active: None,
                    response: None,
                }),
            }],
        );
        let mut child_run = AgentRun::pending(&broker.session.id, "explorer", "edit child", 1);
        child_run.id = child_run_id.into();
        child_run.parent_interaction_id = Some("parent-interaction".into());
        broker.store.save_agent_run(&child_run).unwrap();
        broker
            .store
            .save_agent_turn(&AgentTurnRecord {
                id: "child-turn".into(),
                session_id: broker.session.id.clone(),
                agent_run_id: child_run_id.into(),
                ordinal: 1,
                interaction: child_interaction,
            })
            .unwrap();
        let parent_interaction = completed_interaction(
            "parent-interaction",
            &broker.session.id,
            vec![InteractionNode::AgentReference {
                agent: crate::interaction::AgentReference {
                    id: "child-reference".into(),
                    agent_run_id: child_run_id.into(),
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
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        let active_id = broker.session.id.clone();
        let mut incompatible = broker.session.clone();
        incompatible.id = "copilot-session".into();
        incompatible.backend = "copilot".into();
        incompatible.lease_owner = None;
        incompatible.lease_expires_at_ms = None;
        broker.store.save_session(&incompatible).unwrap();
        let result = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "session.resume".into(),
                params: json!({ "session_id": incompatible.id }),
            })
            .await;
        assert!(result.response.error.is_some());
        assert_eq!(broker.session.id, active_id);
    }
}
