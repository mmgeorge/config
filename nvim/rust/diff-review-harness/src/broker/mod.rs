use crate::backend::{
    Backend, BackendCapability, BackendEvent, BackendEventSink, BackendLaunch, BackendRequest,
    PromptMode, TrustPolicy,
};
use crate::checkpoint::{GitCheckpoint, WorkspaceSnapshot, checkpoint_diff};
use crate::goal::{ContinuationDecision, GoalRecord, GoalState};
use crate::interaction::{
    ActiveThoughtUpdate, CompletedThought, InteractionComment, InteractionKind, InteractionRecord,
    InteractionState, TaskTracker, TimelineReducer,
};
use crate::plan::{
    ArtifactSummary, PlanAnnotation, PlanElicitation, PlanExecutionRecord, PlanExecutionState,
    PlanFileStore, PlanLifecycleKind, PlanLifecycleRecord, PlanPrompt, PlanQuestionResponse,
    PlanQuestionSet, PlanRecord, PlanState,
};
use crate::protocol::{BrokerEvent, BrokerRequest, BrokerResponse};
use crate::session::{ContextUsage, HarnessPreference, HarnessSession, SessionStore, WriteMode};
use crate::storage::SqliteStore;
use crate::timeline::{TimelineEntry, TimelineProjector};
use crate::workspace::WorkspaceKind;
use crate::workspace::watch::{NotifyWorkspaceWatch, WorkspaceWatch};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
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
    pub workspace: String,
    pub client_id: String,
    pub backend: BackendLaunch,
    #[serde(default = "default_model")]
    pub model: String,
    #[serde(default = "default_effort")]
    pub effort: String,
    #[serde(default = "default_trust_profile")]
    pub trust_profile: String,
    #[serde(default)]
    pub trust_policy: TrustPolicy,
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
    pub prompt_history: Vec<String>,
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
    watcher: Option<NotifyWorkspaceWatch>,
    thought_generation: u64,
    last_checkpoint_id: Option<String>,
    task: TaskTracker,
    retraction_eligible: bool,
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
        }
    }

    fn execution(prompt: String, plan_id: String, execution_id: String) -> Self {
        Self {
            prompt,
            kind: InteractionKind::PlanExecution,
            plan_id: Some(plan_id),
            execution_id: Some(execution_id),
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
    trust_policy: TrustPolicy,
    event_sink: Option<BackendEventSink>,
    clock: Box<dyn Clock>,
    interaction_runtime: Option<InteractionRuntime>,
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
        let backend = Arc::<dyn Backend>::from(crate::backend::build(request.backend.clone())?);
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
                            provider_label: backend_provider_label(&request.backend),
                            resolved_model: None,
                            effort: preference
                                .as_ref()
                                .map_or(request.effort, |value| value.effort.clone()),
                            fast_mode: preference.is_some_and(|value| value.fast_mode),
                            trust_profile: request.trust_profile,
                            write_mode: WriteMode::Read,
                            created_at_ms: now_ms,
                            updated_at_ms: now_ms,
                            active_plan_id: None,
                            goal_id: None,
                            lease_owner: None,
                            lease_expires_at_ms: None,
                            native_fork: false,
                            native_compact: request.backend.kind == "codex",
                            context_usage: None,
                        };
                        acquire_lease(&mut session, &request.client_id, now_ms)?;
                        session
                    }
                }
            }
        };
        if session.provider_label.is_empty() {
            session.provider_label = backend_provider_label(&request.backend);
        }
        if session.resolved_model.is_none() && session.model != "default" {
            session.resolved_model = Some(session.model.clone());
        }
        session.write_mode = WriteMode::Read;
        session.updated_at_ms = now_ms;
        store.save_session(&session)?;
        store.save_preference(
            &session.workspace,
            &session.backend,
            &HarnessPreference {
                model: session.model.clone(),
                effort: session.effort.clone(),
                fast_mode: session.fast_mode,
            },
        )?;
        let capability = BackendCapability {
            native_fork: session.native_fork,
            native_compact: session.native_compact,
            native_steer: request.backend.kind == "codex" || request.backend.kind == "mock",
            native_turn_rollback: request.backend.kind == "codex" || request.backend.kind == "mock",
            ..BackendCapability::default()
        };
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
            trust_policy: request.trust_policy,
            event_sink: None,
            clock,
            interaction_runtime: None,
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
            &self.plan_file,
        )?;
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
            prompt_history: self.store.list_prompt_history()?,
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
            "mode.set" => self.set_mode(params),
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
            "session.resume" => self.resume_session(params).await,
            "session.rename" => self.rename_session(params),
            "session.configure" => self.configure_session(params),
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

    fn set_mode(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let mode: WriteMode =
            serde_json::from_value(params.get("mode").cloned().context("mode is required")?)?;
        self.session.write_mode = mode;
        self.save_session()?;
        Ok((
            serde_json::to_value(&self.session)?,
            vec![self.event("mode_changed", serde_json::to_value(&self.session)?)?],
        ))
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
        let model_list = self
            .backend
            .model_list(self.backend_request(String::new(), PromptMode::Chat))
            .await?;
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
            return self.set_mode(json!({ "mode": "read" }));
        }
        if text == "/write" {
            return self.set_mode(json!({ "mode": "write" }));
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
            self.session.write_mode = WriteMode::Read;
            self.save_session()?;
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

        if goal_changed
            && self.session.backend == "codex"
            && self.session.backend_session_id.is_some()
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
        let (value, mut event) = self
            .run_interaction(
                PlanPrompt::clarification(&plan.request, &elicitation_json, &text),
                PromptMode::Chat,
                Some(InteractionAdmission::chat(text)),
            )
            .await?;
        event.insert(
            0,
            self.event("plan_question_updated", json!({ "plan": plan }))?,
        );
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
        let (value, event) = self
            .run_interaction(
                format!(
                    "The user asks for clarification about a pending Harness question. Answer the clarification without resolving or replacing the question set.\n\nPending questions:\n{elicitation_json}\n\nUser question:\n{text}"
                ),
                PromptMode::Chat,
                Some(InteractionAdmission::chat(text)),
            )
            .await?;
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
        self.session.write_mode = WriteMode::Read;
        self.save_session()?;
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
            self.start_interaction_runtime(&mut interaction, &mut event, now_ms)?;
        } else if self
            .interaction_runtime
            .as_ref()
            .is_none_or(|runtime| runtime.interaction_id != interaction.id)
        {
            self.resume_interaction_runtime(&interaction, &mut event)?;
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
                        self.complete_thought_with_checkpoint(
                            &mut runtime,
                            &mut interaction,
                            thought,
                            final_checkpoint_id.as_deref(),
                            &mut event,
                        )?;
                    }
                    if let Some(response) = response {
                        interaction.response = Some(response);
                    }
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
            self.session.write_mode = WriteMode::Read;
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
                plan.elicitation = Some(PlanElicitation::new(question.clone()));
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
        } else if let Some(question) = output.plan_question.take() {
            let question = question.normalize()?;
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

        let final_checkpoint_id = self.capture_final_checkpoint(&mut interaction)?;
        output.evidence.workspace_changed =
            final_checkpoint_id.as_deref() != interaction.checkpoint_before.as_deref();
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
        let has_final_thought = final_thought.is_some();
        if let Some(thought) = final_thought {
            self.complete_thought_with_checkpoint(
                &mut runtime,
                &mut interaction,
                thought,
                final_checkpoint_id.as_deref(),
                &mut event,
            )?;
        }
        if let Some(response) = response {
            interaction.response = Some(response);
        }
        let residual_change =
            final_checkpoint_id.as_deref() != runtime.last_checkpoint_id.as_deref();
        if residual_change && !has_final_thought {
            interaction.attribution_complete = false;
            let thought = CompletedThought {
                id: format!(
                    "{}:thought:{}",
                    interaction.id,
                    interaction.thought.len() + 1
                ),
                text: "Workspace changes detected at completion".into(),
                synthetic: true,
                tool: Vec::new(),
                started_at_ms: self.clock.now_ms(),
                completed_at_ms: self.clock.now_ms(),
                checkpoint_before: None,
                checkpoint_after: None,
                diff_text: None,
                task_id: None,
            };
            self.complete_thought_with_checkpoint(
                &mut runtime,
                &mut interaction,
                thought,
                final_checkpoint_id.as_deref(),
                &mut event,
            )?;
        }
        if continuing {
            if let Some(checkpoint_id) = final_checkpoint_id {
                runtime.last_checkpoint_id = Some(checkpoint_id);
            }
            runtime.thought_generation = runtime
                .watcher
                .as_ref()
                .map_or(runtime.thought_generation, WorkspaceWatch::generation);
            self.interaction_runtime = Some(runtime);
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
        event: &mut Vec<BrokerEvent>,
        now_ms: i64,
    ) -> Result<()> {
        let watcher = match &self.workspace_kind {
            WorkspaceKind::Git(workspace) => match NotifyWorkspaceWatch::start(workspace) {
                Ok(watcher) => Some(watcher),
                Err(error) => {
                    self.emit_live(
                        BackendEvent {
                            kind: "error".into(),
                            text: Some(format!(
                                "Workspace watcher unavailable; Harness will checkpoint every thought: {error:#}"
                            )),
                            data: Value::Null,
                            activity: None,
                            summary: None,
                            task_update: None,
                        },
                        event,
                    )?;
                    None
                }
            },
            WorkspaceKind::Untracked(_) => None,
        };
        let mut last_checkpoint_id = None;
        if let WorkspaceKind::Git(workspace) = &self.workspace_kind {
            let checkpoint =
                GitCheckpoint::new(workspace).capture(&self.store, &self.session.id, now_ms)?;
            interaction.checkpoint_before = Some(checkpoint.id.clone());
            last_checkpoint_id = Some(checkpoint.id.clone());
            self.store.save_checkpoint(&checkpoint)?;
        }
        let thought_generation = watcher.as_ref().map_or(0, WorkspaceWatch::generation);
        self.interaction_runtime = Some(InteractionRuntime {
            interaction_id: interaction.id.clone(),
            timeline: TimelineReducer::new(&interaction.id),
            watcher,
            thought_generation,
            last_checkpoint_id,
            task: interaction
                .task
                .clone()
                .map(TaskTracker::from_snapshot)
                .unwrap_or_default(),
            retraction_eligible: true,
        });
        Ok(())
    }

    fn resume_interaction_runtime(
        &mut self,
        interaction: &InteractionRecord,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
        let watcher = match &self.workspace_kind {
            WorkspaceKind::Git(workspace) => match NotifyWorkspaceWatch::start(workspace) {
                Ok(watcher) => Some(watcher),
                Err(error) => {
                    self.emit_live(
                        BackendEvent {
                            kind: "error".into(),
                            text: Some(format!(
                                "Workspace watcher unavailable; Harness will checkpoint every thought: {error:#}"
                            )),
                            data: Value::Null,
                            activity: None,
                            summary: None,
                            task_update: None,
                        },
                        event,
                    )?;
                    None
                }
            },
            WorkspaceKind::Untracked(_) => None,
        };
        let thought_generation = watcher.as_ref().map_or(0, WorkspaceWatch::generation);
        self.interaction_runtime = Some(InteractionRuntime {
            interaction_id: interaction.id.clone(),
            timeline: TimelineReducer::new(&interaction.id),
            watcher,
            thought_generation,
            last_checkpoint_id: interaction
                .checkpoint_after
                .clone()
                .or_else(|| interaction.checkpoint_before.clone()),
            task: interaction
                .task
                .clone()
                .map(TaskTracker::from_snapshot)
                .unwrap_or_default(),
            retraction_eligible: false,
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
        interaction.diff_text = Some(checkpoint_diff(&self.store, &before, &checkpoint)?);
        self.store.save_checkpoint(&checkpoint)?;
        Ok(Some(checkpoint.id))
    }

    async fn process_backend_event(
        &mut self,
        runtime: &mut InteractionRuntime,
        interaction: &mut InteractionRecord,
        backend_event: BackendEvent,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
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
        if backend_event
            .activity
            .as_ref()
            .is_some_and(|activity| activity.kind == crate::backend::ToolActivityKind::FileChange)
            && let Some(watcher) = runtime.watcher.as_ref()
        {
            watcher.mark_provider_change();
        }
        let transition = runtime.timeline.apply(&backend_event, self.clock.now_ms());
        if let Some(thought) = transition.completed {
            self.complete_thought(runtime, interaction, thought, event)
                .await?;
        }
        if let Some(active) = transition.active {
            self.emit_timeline_active(active, event)?;
        }
        Ok(())
    }

    async fn complete_thought(
        &mut self,
        runtime: &mut InteractionRuntime,
        interaction: &mut InteractionRecord,
        thought: CompletedThought,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
        let observation = match runtime.watcher.as_ref() {
            Some(watcher) => Some(
                watcher
                    .wait_for_quiet(Duration::from_millis(50), Duration::from_millis(250))
                    .await,
            ),
            None => None,
        };
        let dirty = observation.is_none_or(|value| {
            !value.healthy || !value.quiet || value.generation != runtime.thought_generation
        });
        let checkpoint_id = if dirty {
            match &self.workspace_kind {
                WorkspaceKind::Git(workspace) => {
                    let checkpoint = GitCheckpoint::new(workspace).capture(
                        &self.store,
                        &self.session.id,
                        self.clock.now_ms(),
                    )?;
                    let checkpoint_id = checkpoint.id.clone();
                    self.store.save_checkpoint(&checkpoint)?;
                    Some(checkpoint_id)
                }
                WorkspaceKind::Untracked(_) => None,
            }
        } else {
            runtime.last_checkpoint_id.clone()
        };
        self.complete_thought_with_checkpoint(
            runtime,
            interaction,
            thought,
            checkpoint_id.as_deref(),
            event,
        )?;
        runtime.thought_generation = observation
            .map(|value| value.generation)
            .or_else(|| runtime.watcher.as_ref().map(WorkspaceWatch::generation))
            .unwrap_or_default();
        Ok(())
    }

    fn complete_thought_with_checkpoint(
        &mut self,
        runtime: &mut InteractionRuntime,
        interaction: &mut InteractionRecord,
        mut thought: CompletedThought,
        checkpoint_id: Option<&str>,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
        thought.task_id = runtime.task.attribution_target().map(str::to_owned);
        if let Some(task_id) = thought.task_id.as_deref() {
            runtime.task.mark_attributed(task_id);
            interaction.task = Some(runtime.task.snapshot().clone());
        }
        thought.checkpoint_before = runtime.last_checkpoint_id.clone();
        thought.checkpoint_after = checkpoint_id.map(str::to_owned);
        if let (Some(before_id), Some(after_id)) = (
            thought.checkpoint_before.as_deref(),
            thought.checkpoint_after.as_deref(),
        ) {
            let before = self
                .store
                .load_checkpoint(before_id)?
                .context("thought before checkpoint is missing")?;
            let after = self
                .store
                .load_checkpoint(after_id)?
                .context("thought after checkpoint is missing")?;
            thought.diff_text = Some(checkpoint_diff(&self.store, &before, &after)?);
        }
        if let Some(checkpoint_id) = checkpoint_id {
            runtime.last_checkpoint_id = Some(checkpoint_id.to_owned());
        }
        interaction.thought.push(thought.clone());
        self.store.save_interaction(interaction)?;
        self.emit_live(
            BackendEvent {
                kind: "timeline_thought_completed".into(),
                text: None,
                data: serde_json::to_value(thought)?,
                activity: None,
                summary: None,
                task_update: None,
            },
            event,
        )
    }

    fn emit_timeline_active(
        &self,
        active: ActiveThoughtUpdate,
        event: &mut Vec<BrokerEvent>,
    ) -> Result<()> {
        self.emit_live(
            BackendEvent {
                kind: "timeline_active".into(),
                text: None,
                data: serde_json::to_value(active)?,
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
                diff_text: None,
                created_at_ms: now_ms,
                completed_at_ms: None,
                thought: Vec::new(),
                response: None,
                awaiting_input: false,
                elicitation: None,
                duration_ms: 0,
                token_count: None,
                attribution_complete: true,
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
        self.session.write_mode = WriteMode::Write;
        let objective = "Complete the plan".to_owned();
        let goal = self.create_goal(objective, self.session.backend == "codex")?;
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
        self.session.write_mode = WriteMode::Read;
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
        let goal = self.create_goal(objective.clone(), self.session.backend == "codex")?;
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
            interaction.diff_text.unwrap_or_default(),
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
        self.session.write_mode = WriteMode::Read;
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
            fast_mode: self.session.fast_mode,
            trust_profile: self.session.trust_profile.clone(),
            write_mode: WriteMode::Read,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
            active_plan_id: None,
            goal_id: None,
            lease_owner: Some(self.client_id.clone()),
            lease_expires_at_ms: Some(now_ms + 30_000),
            native_fork: false,
            native_compact: self.session.backend == "codex",
            context_usage: None,
        };
        self.capability = BackendCapability {
            native_compact: self.session.backend == "codex",
            native_steer: self.session.backend == "codex" || self.session.backend == "mock",
            native_turn_rollback: self.session.backend == "codex" || self.session.backend == "mock",
            ..BackendCapability::default()
        };
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
        session.write_mode = WriteMode::Read;
        if session.provider_label.is_empty() {
            session.provider_label = backend_provider_label(&self.backend_launch);
        }
        self.store.save_session(&session)?;
        self.interaction_runtime = None;
        self.session = session;
        self.capability.native_fork = self.session.native_fork;
        self.capability.native_compact = self.session.native_compact;
        self.capability.native_steer =
            self.session.backend == "codex" || self.session.backend == "mock";
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
        session.updated_at_ms = self.clock.now_ms();
        self.store.save_session(&session)?;
        if self.session.id == session.id {
            self.session = session.clone();
        }
        Ok((serde_json::to_value(session)?, Vec::new()))
    }

    fn configure_session(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        if let Some(model) = params.get("model").and_then(Value::as_str) {
            anyhow::ensure!(!model.trim().is_empty(), "model cannot be empty");
            self.session.model = model.to_owned();
            self.session.resolved_model = (model != "default").then(|| model.to_owned());
        }
        if let Some(effort) = params.get("effort").and_then(Value::as_str) {
            anyhow::ensure!(!effort.trim().is_empty(), "effort cannot be empty");
            self.session.effort = effort.to_owned();
        }
        if let Some(fast_mode) = params.get("fast_mode").and_then(Value::as_bool) {
            anyhow::ensure!(
                self.session.backend == "codex",
                "fast mode requires the Codex backend"
            );
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
        let source_interaction = self.store.list_interaction(&source.id)?;
        let source_plan = self.store.list_plan(&source.id)?;
        let source_lifecycle = self.store.list_plan_lifecycle(&source.id)?;
        let source_execution = self.store.list_plan_execution(&source.id)?;
        let mut source_comment = Vec::new();
        for interaction in &source_interaction {
            source_comment.push((
                interaction.id.clone(),
                self.store.list_interaction_comment(&interaction.id)?,
            ));
        }
        self.pause_current_goal().await?;
        let backend_session_id = self
            .backend
            .fork(BackendRequest {
                workspace: source.workspace.clone(),
                text: String::new(),
                mode: PromptMode::Chat,
                model: source.model.clone(),
                effort: source.effort.clone(),
                fast_mode: source.fast_mode,
                write_mode: WriteMode::Read,
                trust_profile: source.trust_profile.clone(),
                trust_policy: self.trust_policy.clone(),
                backend_session_id: source.backend_session_id.clone(),
            })
            .await?;
        let now_ms = self.clock.now_ms();
        let mut fork = source.clone();
        fork.id = Uuid::new_v4().to_string();
        fork.name = format!("{} (fork)", fork.name);
        fork.backend_session_id = Some(backend_session_id);
        fork.write_mode = WriteMode::Read;
        fork.active_plan_id = None;
        fork.goal_id = None;
        fork.created_at_ms = now_ms;
        fork.updated_at_ms = now_ms;
        fork.lease_owner = Some(self.client_id.clone());
        fork.lease_expires_at_ms = Some(now_ms + 30_000);
        self.store.save_session(&fork)?;
        let mut plan_id_map = HashMap::new();
        for source_plan in source_plan {
            let mut plan = source_plan.clone();
            plan.id = Uuid::new_v4().to_string();
            plan.session_id.clone_from(&fork.id);
            if !source_plan.working_path.is_empty() {
                plan.working_path = self
                    .plan_file
                    .copy_plan(&source.id, &source_plan.id, &fork.id, &plan.id)?
                    .to_string_lossy()
                    .into_owned();
            }
            plan_id_map.insert(source_plan.id, plan.id.clone());
            self.store.save_plan(&plan)?;
        }
        for mut lifecycle in source_lifecycle {
            let Some(plan_id) = plan_id_map.get(&lifecycle.plan_id) else {
                continue;
            };
            lifecycle.id = Uuid::new_v4().to_string();
            lifecycle.session_id.clone_from(&fork.id);
            lifecycle.plan_id.clone_from(plan_id);
            self.store.save_plan_lifecycle(&lifecycle)?;
        }
        let mut execution_id_map = HashMap::new();
        for mut execution in source_execution {
            let Some(plan_id) = plan_id_map.get(&execution.plan_id) else {
                continue;
            };
            let source_execution_id = execution.id.clone();
            execution.id = Uuid::new_v4().to_string();
            execution.session_id.clone_from(&fork.id);
            execution.plan_id.clone_from(plan_id);
            if execution.state == PlanExecutionState::Active {
                execution.state = PlanExecutionState::Paused;
                execution.completed_at_ms = Some(now_ms);
            }
            execution_id_map.insert(source_execution_id, execution.id.clone());
            self.store.save_plan_execution(&execution)?;
        }
        for mut interaction in source_interaction {
            let source_interaction_id = interaction.id;
            interaction.id = Uuid::new_v4().to_string();
            interaction.session_id.clone_from(&fork.id);
            interaction.plan_id = interaction
                .plan_id
                .as_ref()
                .and_then(|plan_id| plan_id_map.get(plan_id))
                .cloned();
            interaction.execution_id = interaction
                .execution_id
                .as_ref()
                .and_then(|execution_id| execution_id_map.get(execution_id))
                .cloned();
            interaction.checkpoint_before = None;
            interaction.checkpoint_after = None;
            self.store.save_interaction(&interaction)?;
            if let Some((_, comment_list)) = source_comment
                .iter()
                .find(|(interaction_id, _)| interaction_id == &source_interaction_id)
            {
                for source_comment in comment_list {
                    let mut comment = source_comment.clone();
                    comment.id = Uuid::new_v4().to_string();
                    comment.interaction_id.clone_from(&interaction.id);
                    self.store.save_interaction_comment(&comment)?;
                }
            }
        }
        self.release_lease()?;
        self.workspace_kind = crate::workspace::resolve(Path::new(&fork.workspace))?;
        self.interaction_runtime = None;
        self.session = fork;
        Ok((
            serde_json::to_value(self.snapshot()?)?,
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
            fast_mode: self.session.fast_mode,
            write_mode: self.session.write_mode,
            trust_profile: self.session.trust_profile.clone(),
            trust_policy: self.trust_policy.clone(),
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
        self.store.save_preference(
            &self.session.workspace,
            &self.session.backend,
            &HarnessPreference {
                model: self.session.model.clone(),
                effort: self.session.effort.clone(),
                fast_mode: self.session.fast_mode,
            },
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

fn required_text(value: &Value, field: &str) -> Result<String> {
    value
        .get(field)
        .and_then(Value::as_str)
        .map(str::to_owned)
        .with_context(|| format!("{field} is required"))
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
fn default_trust_profile() -> String {
    "workspace".into()
}
fn default_goal_max_turns() -> u32 {
    20
}

fn backend_provider_label(launch: &BackendLaunch) -> String {
    match launch.kind.as_str() {
        "codex" => "Codex CLI".into(),
        "mock" => "Mock backend".into(),
        "acp" => {
            let executable = launch
                .command
                .first()
                .map(String::as_str)
                .unwrap_or("ACP agent");
            let stem = Path::new(executable)
                .file_stem()
                .and_then(|value| value.to_str())
                .unwrap_or(executable);
            if stem.eq_ignore_ascii_case("copilot") {
                "Copilot CLI (ACP)".into()
            } else {
                format!("{stem} (ACP)")
            }
        }
        kind => format!("{kind} backend"),
    }
}

#[cfg(test)]
mod test {
    use super::*;

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

        async fn fork(&self, _request: BackendRequest) -> Result<String> {
            anyhow::bail!("synthetic provider failure")
        }
    }

    struct PlanningQuestionBackend {
        turn: std::sync::atomic::AtomicUsize,
        structured: bool,
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

        async fn fork(&self, _request: BackendRequest) -> Result<String> {
            Ok("planning-question-fork".into())
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
                    output_delta: false,
                }),
                summary: None,
                task_update: None,
            };
            if let Some(event_sink) = event_sink.as_ref() {
                let _ = event_sink.send(commentary.clone());
                let _ = event_sink.send(started.clone());
            }
            let nested = Path::new(&request.workspace).join("apps/new/deep/module");
            std::fs::create_dir_all(&nested)?;
            std::fs::write(nested.join("lib.rs"), "pub fn nested() {}\n")?;
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
                event: vec![commentary, started, completed, response],
                capability: BackendCapability::default(),
                runtime: crate::backend::BackendRuntime {
                    provider: "Nested test".into(),
                    model: Some(request.model),
                },
                ..crate::backend::BackendOutput::default()
            })
        }

        async fn fork(&self, _request: BackendRequest) -> Result<String> {
            Ok("nested-write-fork".into())
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

        async fn fork(&self, _request: BackendRequest) -> Result<String> {
            Ok("task-update-fork".into())
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
            fast_mode: false,
            trust_profile: "workspace".into(),
            write_mode: WriteMode::Read,
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
                workspace: repository.to_string_lossy().into_owned(),
                client_id: "planning-question-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        broker.backend = Arc::new(PlanningQuestionBackend::new(structured));
        broker
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
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "compact-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
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
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: client_id.into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                trust_profile: "workspace".into(),
                trust_policy: TrustPolicy::default(),
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
        let prompt = source
            .dispatch(BrokerRequest {
                id: 1,
                method: "prompt.submit".into(),
                params: json!({ "text": "preserve this interaction" }),
            })
            .await;
        assert!(prompt.response.error.is_none());
        assert!(source.session.native_fork);

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
                params: json!({ "session_id": source_session_id }),
            })
            .await;
        assert!(forked.response.error.is_none());
        let snapshot = forked.response.result.expect("fork snapshot");
        assert_ne!(snapshot["session"]["id"], source_session_id);
        assert_eq!(snapshot["interaction"].as_array().map(Vec::len), Some(1));
        assert_eq!(
            snapshot["interaction"][0]["prompt"],
            "preserve this interaction"
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
    async fn carries_model_effort_and_fast_mode_into_a_new_session() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "settings-client".into(),
                backend: BackendLaunch {
                    kind: "codex".into(),
                    command: vec!["codex".into(), "app-server".into()],
                },
                model: "default".into(),
                effort: "medium".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
                lease_conflict_action: None,
            },
            Box::new(FixedClock(100)),
        )
        .unwrap();
        let configured = broker
            .dispatch(BrokerRequest {
                id: 1,
                method: "session.configure".into(),
                params: json!({ "model": "gpt-5.6", "effort": "low", "fast_mode": true }),
            })
            .await;
        assert!(configured.response.error.is_none());
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
        assert_eq!(session["fast_mode"], true);
        let renamed = broker
            .dispatch(BrokerRequest {
                id: 3,
                method: "session.rename".into(),
                params: json!({ "session_id": session["id"], "name": "  Architecture review  " }),
            })
            .await;
        assert_eq!(
            renamed.response.result.unwrap()["name"],
            "Architecture review"
        );
        drop(broker);

        let restarted = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "restarted-settings-client".into(),
                backend: BackendLaunch {
                    kind: "codex".into(),
                    command: vec!["codex".into(), "app-server".into()],
                },
                model: "default".into(),
                effort: "medium".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
                lease_conflict_action: None,
            },
            Box::new(FixedClock(200)),
        )
        .unwrap();
        assert_eq!(restarted.session.model, "gpt-5.6");
        assert_eq!(restarted.session.effort, "low");
        assert!(restarted.session.fast_mode);
    }

    #[tokio::test]
    async fn resumes_latest_repository_session_after_broker_restart() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "first-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
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
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "second-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "different-model".into(),
                effort: "high".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
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
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "test-client".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
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
            "timeline_active",
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
        assert_eq!(broker.session.write_mode, WriteMode::Read);
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
        assert_eq!(broker.session.write_mode, WriteMode::Write);
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
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "failure-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
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
                .diff_text
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
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "task-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
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
    async fn watcher_attributes_shell_created_nested_directories_to_the_completed_thought() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "watcher-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
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
        assert_eq!(interaction[0].thought.len(), 1);
        let thought = &interaction[0].thought[0];
        assert_eq!(thought.text, "Creating the nested module.");
        assert_eq!(thought.tool.len(), 1);
        assert!(
            thought
                .diff_text
                .as_deref()
                .is_some_and(|diff| diff.contains("apps/new/deep/module/lib.rs")),
            "nested file must be captured by the watcher-gated thought checkpoint"
        );
        assert_eq!(
            interaction[0].response.as_deref(),
            Some("The nested module is ready.")
        );
    }

    #[tokio::test]
    async fn rejects_resuming_a_session_owned_by_another_backend() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                data_root: data.path().to_string_lossy().into_owned(),
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "backend-test".into(),
                backend: BackendLaunch {
                    kind: "mock".into(),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                trust_profile: "workspace".into(),
                session_id: None,
                goal_max_turns: 20,
                trust_policy: TrustPolicy::default(),
                lease_conflict_action: None,
            },
            Box::new(FixedClock(300)),
        )
        .unwrap();
        let active_id = broker.session.id.clone();
        let mut incompatible = broker.session.clone();
        incompatible.id = "acp-session".into();
        incompatible.backend = "acp".into();
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
