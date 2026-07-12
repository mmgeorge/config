use crate::backend::{
    Backend, BackendCapability, BackendEvent, BackendEventSink, BackendLaunch, BackendRequest,
    PromptMode, TrustPolicy,
};
use crate::checkpoint::{GitCheckpoint, WorkspaceSnapshot, checkpoint_diff};
use crate::goal::{ContinuationDecision, GoalRecord, GoalState};
use crate::interaction::{
    ActiveThoughtUpdate, CompletedThought, InteractionComment, InteractionRecord, InteractionState,
    TimelineReducer,
};
use crate::plan::{PlanAnnotation, PlanFileStore, PlanRecord, PlanState};
use crate::protocol::{BrokerEvent, BrokerRequest, BrokerResponse};
use crate::session::{HarnessPreference, HarnessSession, SessionStore, WriteMode};
use crate::storage::SqliteStore;
use crate::workspace::WorkspaceKind;
use crate::workspace::watch::{NotifyWorkspaceWatch, WorkspaceWatch};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
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
}

struct InteractionRuntime {
    interaction_id: String,
    timeline: TimelineReducer,
    watcher: Option<NotifyWorkspaceWatch>,
    thought_generation: u64,
    last_checkpoint_id: Option<String>,
}

/// Represents one request result plus asynchronous events emitted before its response.
pub struct DispatchResult {
    pub response: BrokerResponse,
    pub event: Vec<BrokerEvent>,
}

struct InteractionAdmission {
    prompt: String,
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
        Ok(Self {
            store,
            plan_file: PlanFileStore::new(&data_root),
            workspace_kind,
            data_root,
            client_id: request.client_id,
            backend_launch: request.backend,
            backend,
            session,
            capability: BackendCapability::default(),
            goal_max_turns: request.goal_max_turns,
            trust_policy: request.trust_policy,
            event_sink: None,
            clock,
            interaction_runtime: None,
        })
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
        Ok(BrokerSnapshot {
            session: self.session.clone(),
            interaction: self.store.list_interaction(&self.session.id)?,
            capability: self.capability.clone(),
            no_checkpoint: matches!(self.workspace_kind, WorkspaceKind::Untracked(_)),
            goal,
            active_plan,
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
            Err(error) => DispatchResult {
                response: BrokerResponse::failure(id, "request_failed", format!("{error:#}")),
                event: Vec::new(),
            },
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
            "queue.edit_last" => anyhow::bail!("prompt queue ownership lives in the Neovim client"),
            "plan.accept" => self.accept_plan(params).await,
            "plan.request_changes" => self.request_plan_changes(params).await,
            "plan.cancel" => self.cancel_plan(),
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
            if let Some(plan_id) = self.session.active_plan_id.as_deref()
                && let Some(plan) = self.store.load_plan(plan_id)?
            {
                anyhow::ensure!(
                    !matches!(
                        plan.state,
                        PlanState::Generating | PlanState::AwaitingReview | PlanState::Revising
                    ),
                    "a plan already awaits review; accept it, request changes, or use /plan cancel"
                );
            }
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
                    request.to_owned(),
                    PromptMode::Plan,
                    Some(InteractionAdmission { prompt: text }),
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
        self.run_interaction(
            text.clone(),
            PromptMode::Chat,
            Some(InteractionAdmission { prompt: text }),
        )
        .await
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
                let final_checkpoint_id = self.capture_final_checkpoint(&mut interaction)?;
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
                interaction.state = InteractionState::Failed;
                interaction.completed_at_ms = Some(self.clock.now_ms());
                self.store.save_interaction(&interaction)?;
                self.pause_goal_after_turn_failure().await?;
                self.save_session()?;
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
        self.session.backend_session_id = output.backend_session_id.clone();
        if !output.runtime.provider.is_empty() {
            self.session.provider_label = output.runtime.provider;
        }
        self.session.resolved_model = output.runtime.model;
        output.event.clear();

        if mode == PromptMode::Plan {
            let Some(markdown) = output.plan_markdown.take() else {
                interaction.state = InteractionState::Failed;
                interaction.completed_at_ms = Some(self.clock.now_ms());
                self.store.save_interaction(&interaction)?;
                self.save_session()?;
                anyhow::bail!(
                    "planning turn ended without harness_plan_submit or a complete native plan artifact"
                );
            };
            let active_plan = self
                .session
                .active_plan_id
                .as_deref()
                .map(|plan_id| self.store.load_plan(plan_id))
                .transpose()?
                .flatten()
                .filter(|plan| matches!(plan.state, PlanState::Generating | PlanState::Revising));
            let mut plan = match active_plan {
                Some(plan) => plan,
                None => PlanRecord {
                    id: Uuid::new_v4().to_string(),
                    session_id: self.session.id.clone(),
                    request: text.clone(),
                    state: PlanState::Generating,
                    working_path: String::new(),
                    model_revision: 0,
                    user_revision: 0,
                    review_digest: None,
                    accepted_digest: None,
                    created_at_ms: now_ms,
                    updated_at_ms: now_ms,
                },
            };
            plan.model_revision += 1;
            let (path, digest) = self.plan_file.write_model_revision(
                &self.session.id,
                &plan.id,
                plan.model_revision,
                &markdown,
            )?;
            plan.working_path = path.to_string_lossy().into_owned();
            plan.review_digest = Some(digest);
            plan.state = PlanState::AwaitingReview;
            plan.updated_at_ms = self.clock.now_ms();
            self.store.save_plan(&plan)?;
            self.session.active_plan_id = Some(plan.id.clone());
            self.session.write_mode = WriteMode::Read;
            event.push(self.event("plan_review", serde_json::to_value(&plan)?)?);
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
                            plan_progress: None,
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
                            plan_progress: None,
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
        let (backend_event_sink, mut backend_event_stream) = tokio::sync::mpsc::unbounded_channel();
        let prompt = async move {
            backend
                .prompt_stream(request, Some(backend_event_sink))
                .await
        };
        tokio::pin!(prompt);
        let outcome = loop {
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
            }
        };
        let mut outcome = outcome;
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
        if backend_event
            .activity
            .as_ref()
            .is_some_and(|activity| activity.kind == crate::backend::ToolActivityKind::FileChange)
            && let Some(watcher) = runtime.watcher.as_ref()
        {
            watcher.mark_provider_change();
        }
        if backend_event.kind == "plan" {
            self.emit_live(backend_event.clone(), event)?;
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
                plan_progress: None,
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
                plan_progress: None,
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
                state: InteractionState::Running,
                checkpoint_before: None,
                checkpoint_after: None,
                diff_text: None,
                created_at_ms: now_ms,
                completed_at_ms: None,
                thought: Vec::new(),
                response: None,
                duration_ms: 0,
                token_count: None,
                attribution_complete: true,
                comment: Vec::new(),
            },
            true,
        ))
    }

    async fn accept_plan(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let plan_id = self
            .session
            .active_plan_id
            .clone()
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
        self.plan_file
            .save_user_revision(&self.session.id, &plan.id, plan.user_revision)?;
        plan.accepted_digest = Some(digest);
        plan.state = PlanState::Accepted;
        plan.updated_at_ms = self.clock.now_ms();
        self.store.save_plan(&plan)?;
        self.session.write_mode = WriteMode::Write;
        let objective = "Complete the plan".to_owned();
        let goal = self.create_goal(objective, self.session.backend == "codex")?;
        let (markdown, _) = self.plan_file.read_working(&self.session.id, &plan.id)?;
        let execution_prompt = format!(
            "Goal: Complete the plan. Execute the accepted plan exactly. Report completion with harness_goal_complete only when every task is finished.\n\n{markdown}"
        );
        let execution = self
            .run_interaction(
                execution_prompt,
                PromptMode::ExecutePlan,
                Some(InteractionAdmission {
                    prompt: format!("Accept plan: {}", plan.request),
                }),
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
                let backend_event = BackendEvent {
                    kind: "error".into(),
                    text: Some(format!(
                        "The plan was accepted, but its first execution turn failed: {error:#}. Use /goal resume to retry."
                    )),
                    data: Value::Null,
                    activity: None,
                    summary: None,
                    plan_progress: None,
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
        if execution_succeeded {
            event.insert(0, self.event("goal_changed", serde_json::to_value(goal)?)?);
        }
        event.insert(0, self.event("plan_accepted", serde_json::to_value(plan)?)?);
        Ok((result, event))
    }

    async fn request_plan_changes(&mut self, params: Value) -> Result<(Value, Vec<BrokerEvent>)> {
        let plan_id = self
            .session
            .active_plan_id
            .clone()
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
        plan.state = PlanState::Revising;
        plan.review_digest = Some(digest);
        self.store.save_plan(&plan)?;
        let instruction = format!(
            "Revise the plan. Preserve intentional user edits in the complete current plan. Resolve every annotation. Return the full replacement through harness_plan_submit.\n\nCurrent edited plan:\n{edited}\n\nUser edit diff against the last model revision:\n{edit_diff}\n\nAnnotations:\n{}",
            serde_json::to_string_pretty(&annotation)?
        );
        match self
            .run_interaction(
                instruction,
                PromptMode::Plan,
                Some(InteractionAdmission {
                    prompt: format!("Request plan changes: {}", plan.request),
                }),
            )
            .await
        {
            Ok(result) => Ok(result),
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
        self.session.write_mode = WriteMode::Read;
        self.save_session()?;
        Ok((
            serde_json::to_value(&plan)?,
            vec![self.event("plan_cancelled", serde_json::to_value(plan)?)?],
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
                Some(InteractionAdmission {
                    prompt: format!("/goal {objective}"),
                }),
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
        let prompt = if goal.native {
            "/goal resume".to_owned()
        } else {
            format!(
                "/goal resume\nContinue working toward this goal: {}",
                goal.objective
            )
        };
        match self
            .run_interaction(
                prompt,
                PromptMode::GoalContinuation,
                Some(InteractionAdmission {
                    prompt: "/goal resume".into(),
                }),
            )
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
        Ok(continuing)
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
        self.store.save_goal(&goal)
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
            Some(InteractionAdmission {
                prompt: format!("Request changes for interaction {}", interaction.ordinal),
            }),
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
        };
        self.capability = BackendCapability::default();
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
        let mut fork = source;
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
        for mut interaction in source_interaction {
            let source_interaction_id = interaction.id;
            interaction.id = Uuid::new_v4().to_string();
            interaction.session_id.clone_from(&fork.id);
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
                plan_progress: None,
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
                plan_progress: None,
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
                plan_progress: None,
            };
            let response = BackendEvent {
                kind: "assistant_message".into(),
                text: Some("The nested module is ready.".into()),
                data: Value::Null,
                activity: None,
                summary: None,
                plan_progress: None,
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
            "timeline_active",
            "provider progress must reach the live stream before the final response is rendered"
        );
        let review = planned
            .event
            .iter()
            .find(|event| event.event == "plan_review")
            .unwrap();
        let path = review
            .payload
            .get("working_path")
            .and_then(Value::as_str)
            .unwrap();
        assert!(Path::new(path).exists());
        assert_eq!(broker.session.write_mode, WriteMode::Read);

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
