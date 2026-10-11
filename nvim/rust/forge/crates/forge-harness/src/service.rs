use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, ensure};
use forge_diff::engine::DiffEngine;
use forge_diff::syntax::{SyntaxEngine, SyntaxLanguage, SyntaxRequest};
use forge_git::store::RepositoryStore;
use forge_protocol::message::{Message, Request, Response, SessionEvent};
use forge_protocol::outbound::MessageSender;
use serde_json::{Value, json};
use tokio::sync::{Mutex, MutexGuard, Notify, OwnedSemaphorePermit, RwLock, RwLockReadGuard, Semaphore};
use tokio::task::JoinSet;

use crate::broker::{
    BrokerRuntime, HarnessBroker, InitializeRequest, TurnCancellation, prepare_new_session,
    prepare_provider_fork,
};
use crate::protocol::HarnessMethod;
use crate::session::{ProviderForkState, SessionLeaseConflict};
use crate::storage::SqliteStore;

/// Owns lazy Harness initialization and independently serialized session controllers.
pub struct HarnessService {
    repositories: Arc<RepositoryStore>,
    diff: Arc<DiffEngine>,
    syntax: Arc<SyntaxEngine>,
    registry: Mutex<Option<Arc<SessionControllerRegistry>>>,
    accepting: AtomicBool,
    activity: RwLock<()>,
}

impl HarnessService {
    pub async fn saved_diff(
        &self,
        session_id: Option<String>,
        input: forge_buffer::input::DocumentInput,
    ) -> Result<String> {
        let _activity = self.admit().await?;
        let registry = self.registry().await?;
        let session_id = session_id.unwrap_or_else(|| registry.initial_session_id.clone());
        let controller = registry.resolve(&session_id).await?;
        let action = controller
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .action(input)?;
        match action {
            crate::buffer::projection::TranscriptAction::Diff { text } => Ok(text),
            _ => anyhow::bail!("transcript target is not a saved diff"),
        }
    }

    pub async fn document(
        &self,
        session_id: Option<String>,
        request: crate::buffer::session::PresentationRequest,
    ) -> Result<Value> {
        let _activity = self.admit().await?;
        let registry = self.registry().await?;
        let session_id = session_id.unwrap_or_else(|| registry.initial_session_id.clone());
        let controller = registry.resolve(&session_id).await?;
        match &request {
            crate::buffer::session::PresentationRequest::Recap { model }
            | crate::buffer::session::PresentationRequest::SessionName { model } => {
                let purpose = if matches!(
                    &request,
                    crate::buffer::session::PresentationRequest::SessionName { .. }
                ) {
                    crate::backend::TextGeneration::SessionName
                } else {
                    crate::backend::TextGeneration::Recap
                };
                let history = controller
                    .presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .conversation_history()?;
                let request = controller.catalog_request.read().await.clone();
                let text = controller
                    .backend
                    .generate_text(request, purpose, model, &history)
                    .await?;
                return Ok(json!({"text": purpose.validate(&text)?}));
            }
            crate::buffer::session::PresentationRequest::TerminateTerminal { id } => {
                let request = controller.catalog_request.read().await.clone();
                tokio::time::timeout(
                    Duration::from_secs(8),
                    controller.backend.terminate_terminal(request, id),
                )
                .await
                .context("background terminal termination timed out")??;
                return Ok(json!({}));
            }
            crate::buffer::session::PresentationRequest::BackgroundTerminals => {
                if !controller.backend.descriptor().capability.background_terminals {
                    return Ok(json!({"supported":false,"reason":"Provider does not expose background terminal inventory","terminal":[]}));
                }
                let request = controller.catalog_request.read().await.clone();
                let snapshot = tokio::time::timeout(
                    Duration::from_secs(8),
                    controller.backend.background_terminals(request),
                )
                .await
                .context("background terminal query timed out")??;
                return Ok(serde_json::to_value(snapshot)?);
            }
            crate::buffer::session::PresentationRequest::Highlight { document } => {
                let job = controller
                    .presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .capture_syntax(document)?;
                if let Some(job) = job {
                    let highlighted = job.analyze(&self.syntax).await?;
                    controller
                        .presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                        .apply_syntax(document, &job, highlighted)?;
                }
                return Ok(json!({}));
            }
            crate::buffer::session::PresentationRequest::PlanOpen {
                document,
                view,
                plan_id,
                digest,
                revision,
                width,
                saved_source_digest,
                focused_annotation,
            } => {
                let capture_started = Instant::now();
                let admission = controller.plan_review.admit(document.clone())?;
                let mut source = if let Some(source) = controller.revision_review.capture(plan_id, digest, *revision).await? {
                    source
                } else {
                    let broker = controller.broker.lock().await;
                    if let Some(revision) = revision {
                        let source = broker.capture_plan_revision(plan_id, *revision)?;
                        ensure!(
                            crate::plan::digest(&serde_json::to_vec(&source.document)?) == *digest,
                            "plan revision changed before opening"
                        );
                        source
                    } else {
                        broker.capture_plan_review(plan_id, digest)?
                    }
                };
                if source.document.design.is_some() {
                    let trace = registry.runtime.trace();
                    if trace.status().enabled {
                        source.trace = Some(crate::plan::review_source::ReviewTrace {
                            store: trace,
                            session_id: session_id.clone(),
                        });
                    }
                }
                if let Some(trace) = &source.trace {
                    trace.record(
                        "plan.review.capture",
                        capture_started.elapsed(),
                        source
                            .document
                            .design
                            .as_ref()
                            .map_or(0, |design| design.proposed.len()),
                    );
                }
                admission.check()?;
                if let Some(expected) = saved_source_digest {
                    ensure!(
                        expected == &source.saved_digest,
                        "saved plan source changed before recovering annotations"
                    );
                }
                let syntax_started = Instant::now();
                if let Some(design) = &source.document.design {
                    for path in design.changed_paths() {
                        for (side, text) in [
                            (
                                "baseline",
                                design.baseline.get(&path).map(|file| &file.text),
                            ),
                            ("proposed", design.proposed.get(&path)),
                        ] {
                            let Some(text) = text else { continue };
                            let presentation =
                                forge_diff::syntax::DeclarationOverview::present(&path, text)
                                    .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                            admission.check()?;
                            let Some(language) =
                                forge_diff::syntax::DeclarationOverview::language(&path)
                            else {
                                continue;
                            };
                            let handle = self
                                .syntax
                                .analyze(SyntaxRequest {
                                    source: forge_diff::source::SourceVersion::new(
                                        presentation.text.as_bytes().to_vec(),
                                        forge_diff::source::Representation::DisplayOnly,
                                    )?,
                                    language,
                                    priority: forge_diff::workers::WorkPriority::Visible,
                                    deadline: Some(Instant::now() + Duration::from_secs(10)),
                                })
                                .await
                                .map_err(|error| {
                                    anyhow::anyhow!("Declaration syntax analysis failed: {error:?}")
                                })?;
                            source
                                .declaration_syntax
                                .insert((path.clone(), side.into()), handle);
                        }
                    }
                } else {
                    source.syntax = Some(
                        self.syntax
                            .analyze(SyntaxRequest {
                                source: forge_diff::source::SourceVersion::new(
                                    source.rendered.markdown.as_bytes().to_vec(),
                                    forge_diff::source::Representation::Raw,
                                )?,
                                language: SyntaxLanguage::Markdown,
                                priority: forge_diff::workers::WorkPriority::Visible,
                                deadline: Some(Instant::now() + Duration::from_secs(10)),
                            })
                            .await
                            .map_err(|error| {
                                anyhow::anyhow!("PlanReview syntax analysis failed: {error:?}")
                            })?,
                    );
                }
                if let Some(trace) = &source.trace {
                    trace.record(
                        "plan.review.syntax",
                        syntax_started.elapsed(),
                        source.declaration_syntax.len(),
                    );
                }
                admission.check()?;
                let document = crate::plan::review_document::PlanReviewDocument::new(
                    document.clone(),
                    view.clone(),
                    source,
                    width.clone(),
                    focused_annotation.clone(),
                )?;
                return controller.plan_review.insert(document, admission);
            }
            crate::buffer::session::PresentationRequest::PlanAction { input } => {
                let trace = if input.action == "jump_entity" {
                    crate::declaration::trace::DeclarationTrace::new(
                        registry.runtime.trace(),
                        &session_id,
                        input,
                    )
                } else {
                    None
                };
                let total = trace.as_ref().map(|trace| trace.stage("total", None));
                if input.action == "jump_entity" {
                    controller
                        .plan_review
                        .prepare_declaration_jump(input, trace.as_ref())
                        .await?;
                }
                let stage = trace.as_ref().map(|trace| trace.stage("action", None));
                let result = controller.plan_review.action(input.clone())?;
                if let Some(stage) = stage {
                    stage.complete(json!({}));
                }
                let result = serde_json::to_value(result)?;
                if let Some(stage) = total {
                    stage.complete(json!({}));
                }
                return Ok(result);
            }
            crate::buffer::session::PresentationRequest::PlanSaveAnnotations {
                document,
                saved_source_digest,
                annotation,
            } => {
                controller.plan_review.save_annotations(
                    document,
                    saved_source_digest,
                    annotation.clone(),
                )?;
                return Ok(json!({"saved":true}));
            }
            crate::buffer::session::PresentationRequest::PlanView {
                document,
                view,
                width,
            } => {
                return controller
                    .plan_review
                    .view(document, view.clone(), width.clone());
            }
            crate::buffer::session::PresentationRequest::PlanClose { document } => {
                controller.plan_review.close(document).await?;
                return Ok(json!({}));
            }
            _ => {}
        }
        if let crate::buffer::session::PresentationRequest::ToolExport { document } = request {
            let directory = PathBuf::from(&registry.initialize.data_root).join("tool-output");
            let path = controller
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .export_tool(&document, &directory)?;
            return Ok(json!({"path":path}));
        }
        controller
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .dispatch(request)
    }

    /// Construct the owner without discovering repositories, opening stores, or launching providers.
    pub fn new(
        repositories: Arc<RepositoryStore>,
        diff: Arc<DiffEngine>,
        syntax: Arc<SyntaxEngine>,
    ) -> Self {
        Self {
            repositories,
            diff,
            syntax,
            registry: Mutex::new(None),
            accepting: AtomicBool::new(true),
            activity: RwLock::new(()),
        }
    }

    /// Open the first session and preserve structured lease-conflict recovery in the response.
    pub async fn open_session(
        &self,
        request_id: u64,
        initialize: InitializeRequest,
    ) -> Result<Response> {
        let _activity = self.admit().await?;
        match self.initialize(initialize).await {
            Ok(snapshot) => Ok(Response::success(request_id, snapshot)?),
            Err(error) => Ok(initialize_failure(request_id, &error)),
        }
    }

    /// Arm provider work in input order before concurrent dispatch can admit a later cancellation.
    /// Keep the permit until dispatch and response delivery finish. Overlapping execution and
    /// pending cleanup reject admission without resetting the current cancellation state.
    pub async fn prepare(&self, session_id: Option<&str>, method: HarnessMethod) -> Result<Option<OwnedSemaphorePermit>> {
        let _activity = self.admit().await?;
        if method.requires_provider_fork() {
            let registry = self.registry().await?;
            let session_id = session_id.unwrap_or(&registry.initial_session_id);
            let controller = registry.resolve(session_id).await?;
            let _control = Arc::clone(&controller.control_admission)
                .try_acquire_owned()
                .context("Harness cleanup is still running")?;
            let execution = Arc::clone(&controller.execution_admission)
                .try_acquire_owned()
                .context("a Harness execution request is already running")?;
            controller.cancellation.arm(method == HarnessMethod::PromptSubmit);
            return Ok(Some(execution));
        }
        Ok(None)
    }

    /// Persist transition ordering on the host input lane before concurrent execution begins.
    pub async fn prepare_task(&self, session_id: Option<&str>, request: &Request) -> Result<()> {
        let _activity = self.admit().await?;
        let registry = self.registry().await?;
        let session_id = session_id.unwrap_or(&registry.initial_session_id);
        let controller = registry.resolve(session_id).await?;
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        admit_task_intent(&controller, session_id, request, &mut store)?;
        Ok(())
    }

    /// Route a validated session operation without holding the initialization mutex during work.
    ///
    /// The host must call `prepare` in input order before dispatching provider-bound operations.
    pub async fn dispatch(
        &self,
        session_id: Option<String>,
        mut request: Request,
        sink: &MessageSender,
    ) -> Result<()> {
        let method = HarnessMethod::decode(&request.method)?;
        if method == HarnessMethod::Shutdown {
            let initialized = self.registry.lock().await.is_some();
            self.shutdown(Duration::from_secs(2)).await?;
            let result = if initialized {
                json!({"shutdown":true})
            } else {
                json!({"stopped":true})
            };
            sink.send_terminal(Message::Response(Response::success(request.id, result)?))?;
            return Ok(());
        }
        let _activity = self.admit().await?;
        let registry = self.registry().await?;
        let session_id = session_id.unwrap_or_else(|| registry.initial_session_id.clone());
        let mut prompt_admission = None;
        if matches!(
            method,
            HarnessMethod::PlanAcceptanceBegin | HarnessMethod::PlanRequestChanges | HarnessMethod::PlanEntityRename | HarnessMethod::PlanTestsDelete
        ) {
            let params = request
                .params
                .as_object_mut()
                .context("plan review submission requires an object")?;
            if let Some(review) = params.remove("review") {
                let input = serde_json::from_value::<forge_buffer::input::DocumentInput>(review)?;
                let controller = registry.resolve(&session_id).await?;
                if let Some(annotation) = params.remove("draft_annotation") {
                    let digest = params
                        .remove("draft_source_digest")
                        .and_then(|value| value.as_str().map(str::to_owned))
                        .context("plan annotation capture requires its saved source digest")?;
                    controller.plan_review.save_annotations(
                        &input.document,
                        &digest,
                        serde_json::from_value(annotation)?,
                    )?;
                }
                let captured = controller.plan_review.submission(input)?;
                for (key, value) in captured
                    .as_object()
                    .context("plan review capture is not an object")?
                {
                    params.insert(key.clone(), value.clone());
                }
            }
        }
        if method == HarnessMethod::PromptSubmit {
            let params = request
                .params
                .as_object_mut()
                .context("prompt submission requires an object")?;
            params.remove("_prompt_admission");
            if let Some(submission) = params.remove("submission") {
                let document = serde_json::from_value::<forge_buffer::identity::DocumentId>(
                    submission
                        .get("document")
                        .cloned()
                        .context("submission document is required")?,
                )?;
                let token = submission
                    .get("token")
                    .and_then(Value::as_u64)
                    .context("submission token is required")?;
                let text = params
                    .get("text")
                    .and_then(Value::as_str)
                    .context("prompt text is required")?;
                let controller = registry.resolve(&session_id).await?;
                controller
                    .presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .begin_submission(&document, token, text)?;
                params.insert(
                    "_prompt_admission".into(),
                    json!({"document":document,"token":token}),
                );
                prompt_admission = Some(PromptAdmission {
                    presentation: Arc::clone(&controller.presentation),
                    document,
                    token,
                });
            }
        }
        let result = route_request(registry, session_id, request, method, sink).await;
        drop(prompt_admission);
        result
    }

    /// Close admission, cancel active turns, and release leases only after dispatch and forks finish.
    ///
    /// A timeout retains the registry and task ownership. A later call can finish shutdown.
    /// Provider subprocess cleanup still follows each backend's existing destruction contract.
    pub async fn shutdown(&self, deadline: Duration) -> Result<()> {
        self.accepting.store(false, Ordering::Release);
        tokio::time::timeout(deadline, async {
            let mut child_quiescence = Vec::new();
            if let Some(registry) = self.registry.lock().await.clone() {
                let controller_list: Vec<_> = registry
                    .controller_by_id
                    .read()
                    .await
                    .values()
                    .cloned()
                    .collect();
                for controller in controller_list {
                    controller.plan_review.close_all()?;
                    *controller.mcp_discovery.lock().await = Default::default();
                    controller.cancellation.request(false);
                    let approval_owner = crate::backend::ExecutionOwner::root(&controller.catalog_request.read().await.harness_session_id);
                controller.permission.cancel_owner(&approval_owner, None).await?;
                    child_quiescence.push(quiesce_children(&controller).await?);
                }
            }
            let _quiescence = self.activity.write().await;
            let mut owner = self.registry.lock().await;
            if let Some(registry) = owner.as_ref() {
                let mut task = registry.fork_task.lock().await;
                while let Some(result) = task.join_next().await {
                    result.context("Harness provider fork task failed")?;
                }
                registry
                    .runtime
                    .backend_handle()
                    .shutdown()
                    .await
                    .context("collect Harness provider runtime")?;
                let controller_list: Vec<_> = registry
                    .controller_by_id
                    .read()
                    .await
                    .values()
                    .cloned()
                    .collect();
                for controller in controller_list {
                    controller
                        .presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                        .close_all()?;
                    controller.broker.lock().await.release_lease()?;
                }
            }
            owner.take();
            Ok(())
        })
        .await
        .context("Harness shutdown deadline expired before session owners quiesced")?
    }

    async fn admit(&self) -> Result<RwLockReadGuard<'_, ()>> {
        ensure!(
            self.accepting.load(Ordering::Acquire),
            "Harness service is closed"
        );
        let activity = self.activity.read().await;
        ensure!(
            self.accepting.load(Ordering::Acquire),
            "Harness service is closed"
        );
        Ok(activity)
    }

    async fn registry(&self) -> Result<Arc<SessionControllerRegistry>> {
        self.registry
            .lock()
            .await
            .clone()
            .context("Harness is not initialized")
    }

    async fn initialize(&self, initialize: InitializeRequest) -> Result<Value> {
        let mut harness = self.registry.lock().await;
        ensure!(harness.is_none(), "Harness is already initialized");
        if let Some(root) = &initialize.config_dir {
            crate::settings::SettingsStore::open(PathBuf::from(&initialize.data_root).as_path())?
                .configure_catalog(PathBuf::from(root).as_path())?;
        }
        let repository = self
            .repositories
            .open(PathBuf::from(&initialize.workspace))
            .await?;
        let workspace_kind = repository
            .as_ref()
            .and_then(|repository| repository.identity.worktree_root.clone())
            .map(crate::workspace::WorkspaceKind::Git)
            .unwrap_or_else(|| {
                crate::workspace::WorkspaceKind::Untracked(PathBuf::from(&initialize.workspace))
            });
        let runtime = BrokerRuntime::initialize(
            &initialize,
            workspace_kind,
            Arc::clone(&self.repositories),
            Arc::clone(&self.diff),
        )?;
        let broker =
            HarnessBroker::initialize_with_runtime(initialize.clone(), Arc::clone(&runtime))?;
        let snapshot = broker.snapshot()?;
        let result = serde_json::to_value(&snapshot)?;
        *harness = Some(SessionControllerRegistry::new(
            snapshot.session.id.clone(),
            SessionController::new(broker),
            runtime,
            initialize,
        ));
        Ok(result)
    }
}

/// Owns one session's serialized state machine and out-of-band control lanes.
struct SessionController {
    agent_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    agent_coordinator: Arc<crate::agent::AgentCoordinator>,
    agent_backend_factory: crate::agent::AgentBackendFactory,
    parent_input: crate::backend::SteeringLane,
    compaction_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    execution_admission: Arc<Semaphore>,
    control_admission: Arc<Semaphore>,
    plan_review: Arc<crate::plan::review_document::PlanReviewStore>,
    revision_review: Arc<crate::plan::revision_review::RevisionReview>,
    broker: Mutex<HarnessBroker>,
    presentation: Arc<std::sync::Mutex<crate::buffer::session::SessionPresentation>>,
    cancellation: Arc<TurnCancellation>,
    backend: Arc<dyn crate::backend::Backend>,
    catalog_request: Arc<RwLock<crate::backend::BackendCatalogRequest>>,
    mcp_discovery: Mutex<crate::backend::mcp::McpDiscovery>,
    permission: Arc<crate::backend::approval::PermissionCoordinator>,
}

struct PromptAdmission {
    presentation: Arc<std::sync::Mutex<crate::buffer::session::SessionPresentation>>,
    document: forge_buffer::identity::DocumentId,
    token: u64,
}

impl Drop for PromptAdmission {
    fn drop(&mut self) {
        if let Ok(mut presentation) = self.presentation.lock() {
            let _ = presentation.complete_submission(&self.document, self.token, false);
        }
    }
}

impl SessionController {
    /// Build one independently serialized controller from a durable broker session.
    fn new(broker: HarnessBroker) -> Arc<Self> {
        let catalog_request = broker.backend_catalog_request();
        Arc::new(Self {
            agent_task: Mutex::new(None),
            agent_coordinator: broker.agent_coordinator(),
            agent_backend_factory: broker.agent_backend_factory.clone(),
            parent_input: broker.parent_input.clone(),
            compaction_task: Mutex::new(None),
            execution_admission: Arc::new(Semaphore::new(1)),
            control_admission: Arc::new(Semaphore::new(1)),
            plan_review: Arc::clone(&broker.plan_review),
            revision_review: Arc::clone(&broker.revision_review),
            presentation: broker.presentation(),
            cancellation: broker.turn_cancellation(),
            backend: broker.backend_handle(),
            catalog_request: Arc::new(RwLock::new(catalog_request)),
            mcp_discovery: Mutex::new(crate::backend::mcp::McpDiscovery::default()),
            permission: broker.permission_coordinator(),
            broker: Mutex::new(broker),
        })
    }

    async fn attach_compaction(self: &Arc<Self>, message_sink: &MessageSender) -> Result<()> {
        self.attach_agents(message_sink).await?;
        let mut task = self.compaction_task.lock().await;
        if task.is_some() { return Ok(()); }
        let session_id = self.catalog_request.read().await.harness_session_id.clone();
        let Some(mut stream) = self.backend.compaction_stream(&session_id).await? else { return Ok(()); };
        let controller = Arc::downgrade(self);
        let message_sink = message_sink.clone();
        *task = Some(tokio::spawn(async move {
            let result: Result<()> = async {
                while let Some(event) = stream.recv().await? {
                    let Some(controller) = controller.upgrade() else { return Ok(()); };
                    let events = controller.broker.lock().await.observe_external_compaction(event)?;
                    for event in events { message_sink.send_event(event).await?; }
                }
                Ok(())
            }.await;
            if let Err(error) = result {
                let _ = message_sink.send_event(SessionEvent {
                    session_id, event: "backend_event".into(),
                    payload: json!({"kind":"error", "text":format!("Compaction subscription failed: {error:#}")}),
                }).await;
            }
        }));
        Ok(())
    }

    async fn attach_agents(self: &Arc<Self>, message_sink: &MessageSender) -> Result<()> {
        let mut task = self.agent_task.lock().await;
        if task.is_some() { return Ok(()); }
        let mut revision = self.agent_coordinator.subscribe();
        let controller = Arc::downgrade(self);
        let message_sink = message_sink.clone();
        *task = Some(tokio::spawn(async move {
            loop {
                let Some(controller) = controller.upgrade() else { return; };
                let session_id = controller.catalog_request.read().await.harness_session_id.clone();
                let outcome = controller.broker.lock().await.drain_agent_commands(None).await;
                drop(controller);
                match outcome {
                    Ok(event) => for event in event { if message_sink.send_event(event).await.is_err() { return; } },
                    Err(error) => {
                        let _ = message_sink.send_event(SessionEvent {session_id,event:"backend_event".into(),
                            payload:json!({"kind":"error","text":format!("Agent coordinator failed: {error:#}")})}).await;
                    }
                }
                if revision.changed().await.is_err() { return; }
            }
        }));
        Ok(())
    }
}

impl Drop for SessionController {
    fn drop(&mut self) {
        if let Some(task) = self.agent_task.get_mut().take() { task.abort(); }
        if let Some(task) = self.compaction_task.get_mut().take() { task.abort(); }
    }
}

/// Stores live session controllers for one persistent broker process.
struct SessionControllerRegistry {
    controller_by_id: RwLock<HashMap<String, Arc<SessionController>>>,
    provider_fork_by_id: RwLock<HashMap<String, Arc<ProviderForkGate>>>,
    fork_task: Mutex<JoinSet<()>>,
    runtime: Arc<BrokerRuntime>,
    initialize: InitializeRequest,
    initial_session_id: String,
}

/// Coordinates queued child work with one asynchronous provider fork.
struct ProviderForkGate {
    outcome: Mutex<Option<std::result::Result<(), String>>>,
    notify: Notify,
}

impl ProviderForkGate {
    /// Build a pending gate before the child snapshot reaches the client.
    fn new() -> Arc<Self> {
        Arc::new(Self {
            outcome: Mutex::new(None),
            notify: Notify::new(),
        })
    }

    /// Publish provider preparation once durable child state reflects the outcome.
    async fn complete(&self, outcome: std::result::Result<(), String>) {
        *self.outcome.lock().await = Some(outcome);
        self.notify.notify_waiters();
    }

    /// Wait for provider preparation without blocking unrelated session controllers.
    async fn wait(&self) -> Result<()> {
        loop {
            let notified = self.notify.notified();
            if let Some(outcome) = self.outcome.lock().await.clone() {
                return outcome.map_err(anyhow::Error::msg);
            }
            notified.await;
        }
    }
}

impl SessionControllerRegistry {
    /// Build the registry with the session selected by broker initialization.
    fn new(
        initial_session_id: String,
        controller: Arc<SessionController>,
        runtime: Arc<BrokerRuntime>,
        initialize: InitializeRequest,
    ) -> Arc<Self> {
        Arc::new(Self {
            controller_by_id: RwLock::new(HashMap::from([(
                initial_session_id.clone(),
                controller,
            )])),
            provider_fork_by_id: RwLock::new(HashMap::new()),
            fork_task: Mutex::new(JoinSet::new()),
            runtime,
            initialize,
            initial_session_id,
        })
    }

    /// Resolve an existing controller or resume its durable session into this process.
    async fn resolve(&self, session_id: &str) -> Result<Arc<SessionController>> {
        if let Some(controller) = self.controller_by_id.read().await.get(session_id).cloned() {
            return Ok(controller);
        }
        let mut controller_by_id = self.controller_by_id.write().await;
        if let Some(controller) = controller_by_id.get(session_id).cloned() {
            return Ok(controller);
        }
        let mut initialize = self.initialize.clone();
        initialize.session_id = Some(session_id.to_owned());
        initialize.new_session_name = None;
        initialize.lease_conflict_action = None;
        let broker = HarnessBroker::initialize_with_runtime(initialize, Arc::clone(&self.runtime))?;
        anyhow::ensure!(
            broker.snapshot()?.session.id == session_id,
            "resolved Harness controller does not own requested session {session_id}"
        );
        let controller = SessionController::new(broker);
        controller_by_id.insert(session_id.to_owned(), Arc::clone(&controller));
        Ok(controller)
    }

    /// Queue provider-bound child work until its native fork reaches durable readiness.
    async fn await_provider_fork(
        &self,
        session_id: &str,
        controller: &SessionController,
    ) -> Result<()> {
        let provider_fork_state = controller.broker.lock().await.provider_fork_state();
        match provider_fork_state {
            ProviderForkState::Ready => Ok(()),
            ProviderForkState::Failed { message, .. } => anyhow::bail!(message),
            ProviderForkState::Preparing { .. } => {
                let gate = self
                    .provider_fork_by_id
                    .read()
                    .await
                    .get(session_id)
                    .cloned()
                    .context(
                        "provider fork preparation was interrupted before this broker resumed",
                    )?;
                gate.wait().await
            }
        }
    }
}

async fn route_request(
    registry: Arc<SessionControllerRegistry>,
    session_id: String,
    request: Request,
    method: HarnessMethod,
    message_sink: &MessageSender,
) -> Result<()> {
    if matches!(method, HarnessMethod::BackendModelPins | HarnessMethod::BackendModelPin) {
        let data_root = PathBuf::from(&registry.initialize.data_root);
        let parameter = request.params;
        let identifier_list = tokio::task::spawn_blocking(move || -> Result<Vec<String>> {
            let backend = parameter.get("backend").and_then(Value::as_str)
                .context("model pin request requires backend")?;
            let mut store = crate::storage::model_pin::ModelPinStore::open(&data_root)?;
            if method == HarnessMethod::BackendModelPins {
                store.load(backend)
            } else {
                let model = parameter.get("model").and_then(Value::as_str)
                    .context("backend.model_pin requires model")?;
                let pinned = parameter.get("pinned").and_then(Value::as_bool)
                    .context("backend.model_pin requires pinned")?;
                store.set(backend, model, pinned)
            }
        }).await.context("model pin storage task failed")??;
        message_sink.send_response(Response::success(request.id, identifier_list)?).await?;
        return Ok(());
    }
    if matches!(
        method,
        HarnessMethod::TraceStatus
            | HarnessMethod::TraceConfigure
            | HarnessMethod::TraceToggle
            | HarnessMethod::TraceClear
            | HarnessMethod::TraceSessionClear
    ) {
        let trace = registry.runtime.trace();
        match method {
            HarnessMethod::TraceConfigure => {
                let enabled = request
                    .params
                    .get("enabled")
                    .and_then(Value::as_bool)
                    .context("trace enabled is required")?;
                if request.params.get("default_only").and_then(Value::as_bool) == Some(true) {
                    trace.configure_default(enabled)?;
                } else {
                    trace.configure(enabled)?;
                }
            }
            HarnessMethod::TraceToggle => {
                trace.toggle()?;
            }
            HarnessMethod::TraceClear => {
                trace.clear()?;
            }
            HarnessMethod::TraceSessionClear => {
                trace.clear_session(&session_id)?;
            }
            _ => {}
        }
        message_sink
            .send_response(Response::success(
                request.id,
                serde_json::to_value(trace.session_status(&session_id))?,
            )?)
            .await?;
        return Ok(());
    }
    if method == HarnessMethod::SessionResume {
        return resume_session(registry, request, message_sink).await;
    }
    if method == HarnessMethod::SessionFork {
        return route_session_fork(registry, session_id, request, message_sink).await;
    }
    if method == HarnessMethod::SessionNew {
        return route_new_session(registry, session_id, request, message_sink).await;
    }
    let controller = registry.resolve(&session_id).await?;
    controller.attach_compaction(message_sink).await?;
    if matches!(method, HarnessMethod::SettingsGet | HarnessMethod::SettingsUpdate | HarnessMethod::SettingsReset
        | HarnessMethod::SettingsImportPreview | HarnessMethod::SettingsImportApply
        | HarnessMethod::BackendSkillsSetEnabled | HarnessMethod::BackendMcpSetEnabled) {
        let catalog = controller.catalog_request.read().await.clone();
        let mut settings = crate::settings::SettingsStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        if method == HarnessMethod::SettingsGet {
            let snapshot = settings.catalog(&catalog.workspace,&session_id,&registry.initialize.backend.kind)?;
            let mut value = serde_json::to_value(snapshot.settings)?;
            value["catalog"] = json!({"mcp":snapshot.server,"skill":snapshot.skill});
            value["layers"] = json!({
                "global":settings.layer_settings(crate::settings::SettingScope::Global,&catalog.workspace,&session_id)?,
                "workspace":settings.layer_settings(crate::settings::SettingScope::Workspace,&catalog.workspace,&session_id)?,
                "session":settings.layer_settings(crate::settings::SettingScope::Session,&catalog.workspace,&session_id)?,
            });
            value["application"] = serde_json::to_value(settings.application(&session_id)?)?;
            message_sink.send_response(Response::success(request.id,value)?).await?;
            return Ok(());
        }
        if method == HarnessMethod::SettingsImportPreview {
            let (_, preview) = preview_native_import(settings, &request.params, &catalog, &registry.initialize.backend.kind).await?;
            let result = serde_json::to_value(preview)?;
            message_sink.send_response(Response::success(request.id, result)?).await?;
            return Ok(());
        }
        let operation_id = request.params.get("operation_id").and_then(Value::as_str)
            .map(str::to_owned).unwrap_or_else(||uuid::Uuid::new_v4().to_string());
        let receipt = json!({"method":request.method,"params":request.params});
        let mutation = if let Some(mutation) = settings.operation(&session_id,&operation_id,&receipt)? {
            mutation
        } else {
        let resource_mutation = matches!(method, HarnessMethod::BackendSkillsSetEnabled | HarnessMethod::BackendMcpSetEnabled);
        let revision = match request.params["expected_revision"].as_u64() {
            Some(revision) => revision,
            None if resource_mutation => settings.get(&catalog.workspace,&session_id,&registry.initialize.backend.kind)?.revision,
            None => anyhow::bail!("settings require expected_revision"),
        };
        let default_scope = if method == HarnessMethod::SettingsImportApply { "workspace" } else { "global" };
        let scope = serde_json::from_value(request.params.get("scope").cloned().unwrap_or(json!(default_scope)))?;
        if method == HarnessMethod::SettingsImportApply {
            let fingerprint = request.params["fingerprint"].as_str().context("native import preview fingerprint is required")?;
            let selected: Vec<String> = serde_json::from_value(request.params["selected"].clone())?;
            let (restored, preview) = preview_native_import(settings, &request.params, &catalog, &registry.initialize.backend.kind).await?;
            settings = restored;
            let resource = preview.select(fingerprint, &selected)?;
            settings.import_operation(revision, scope, &catalog.workspace, &session_id, &resource, &operation_id, &receipt)?
        } else {
        let desired = if method == HarnessMethod::SettingsReset {
            crate::settings::PortableSettings::default()
        } else if resource_mutation {
            let name = request.params["name"].as_str().context("resource selection requires name")?;
            let requested_id = request.params["resource_id"].as_str();
            let enabled = request.params["enabled"].as_bool().context("resource selection requires enabled")?;
            let source = if method == HarnessMethod::BackendSkillsSetEnabled {
                settings.skills(&catalog.workspace)?
                    .into_iter().map(|entry|(entry.id,entry.name)).collect::<Vec<_>>()
            } else {
                settings.servers(&catalog.workspace)?
                    .into_iter().map(|entry|(entry.id,entry.name)).collect::<Vec<_>>()
            };
            let matching: Vec<_> = source.into_iter().filter(|(id, candidate)| {
                requested_id.map_or(candidate == name, |requested| requested == id)
            }).collect();
            ensure!(matching.len() == 1, "Resource selection requires one canonical source. Import native configuration explicitly or select a resource ID");
            let id = matching.into_iter().next().expect("one matching resource").0;
            let mut desired = settings.layer_settings(scope,&catalog.workspace,&session_id)?;
            if method == HarnessMethod::BackendSkillsSetEnabled { desired.skill.insert(id,enabled); }
            else { desired.mcp.insert(id,enabled); }
            desired
        } else {
            serde_json::from_value(request.params["desired"].clone())?
        };
        settings.update_operation(revision,scope,&catalog.workspace,&session_id,&desired,&operation_id,&receipt)?
        }
        };
        drop(settings);
        let request = Request { id:request.id, method:"task.transition".into(),
            params:json!({"operation_id":operation_id,"action":"configure","config":{"settings_revision":mutation.revision,"setting_fields":mutation.field}}) };
        return route_task_transition(registry,controller,session_id,request,message_sink).await;
    }
    let _setup_admission = if matches!(method, HarnessMethod::BackendSetup | HarnessMethod::BackendAuthenticate) {
        let control = Arc::clone(&controller.control_admission).try_acquire_owned()
            .context("Provider setup cannot run while cleanup is pending")?;
        let execution = Arc::clone(&controller.execution_admission).try_acquire_owned()
            .context("Finish active work before provider setup or authentication")?;
        Some((control, execution))
    } else { None };
    if method == HarnessMethod::BackendSetup {
        let install = request.params.get("install").and_then(Value::as_bool).unwrap_or(false);
        let result = controller.backend.setup(controller.catalog_request.read().await.clone(), install).await?;
        *controller.mcp_discovery.lock().await = Default::default();
        message_sink.send_response(Response::success(request.id, result)?).await?;
        return Ok(());
    }
    if method == HarnessMethod::BackendAuthenticate {
        let method = request.params["method"].as_str().context("authentication method is required")?.to_owned();
        controller.backend.authenticate(controller.catalog_request.read().await.clone(), method).await?;
        *controller.mcp_discovery.lock().await = Default::default();
        message_sink.send_response(Response::success(request.id, json!({"authenticated":true}))?).await?;
        return Ok(());
    }
    if method == HarnessMethod::BackendCapabilities {
        let manifest = controller.backend.capabilities(controller.catalog_request.read().await.clone()).await?;
        message_sink.send_response(Response::success(request.id, manifest)?).await?;
        return Ok(());
    }
    if matches!(method, HarnessMethod::AgentProfilesGet | HarnessMethod::AgentProfilesSave) {
        let profiles = &controller.agent_backend_factory.profiles;
        let snapshot = if method == HarnessMethod::AgentProfilesSave {
            let revision = request.params["expected_revision"].as_str().context("profile save requires expected_revision")?;
            profiles.save(revision, serde_json::from_value(request.params["document"].clone())?)?
        } else { profiles.snapshot()? };
        message_sink.send_response(Response::success(request.id, snapshot)?).await?;
        return Ok(());
    }
    if method == HarnessMethod::AgentModels {
        let provider = request.params["provider"].as_str().context("child model catalog requires provider")?;
        let provider = crate::backend::BackendKind::parse(provider)?;
        let catalog = controller.catalog_request.read().await.clone();
        let models = controller.agent_backend_factory.model_list(provider,crate::backend::BackendRequest {
            execution_owner:catalog.execution_owner,harness_session_id:catalog.harness_session_id,workspace:catalog.workspace,
            input:crate::backend::BackendInput::from_text(String::new()),mode:crate::backend::PromptMode::Chat,
            model:"default".into(),effort:"default".into(),context_window:None,service_tier:Default::default(),
            access:catalog.access,execution_mode:catalog.execution_mode,backend_session_id:None,control_context:None,
        }).await?;
        message_sink.send_response(Response::success(request.id,models)?).await?;
        return Ok(());
    }
    if method == HarnessMethod::Health {
        message_sink.send_control_wait(Message::Response(Response::success(request.id,
            json!({"session_id":session_id,"responsive":true}))?)).await?;
        return Ok(());
    }
    if method == HarnessMethod::TaskOperation {
        let id = request.params.get("operation_id").and_then(Value::as_str).context("operation_id is required")?;
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        message_sink.send_control_wait(Message::Response(Response::success(request.id,
            store.load_task_operation(&session_id, id)?.map(|operation| operation.view()))?)).await?;
        return Ok(());
    }
    if method == HarnessMethod::TaskTransition {
        return route_task_transition(registry, controller, session_id, request, message_sink).await;
    }
    if method.requires_provider_fork() {
        registry
            .await_provider_fork(&session_id, &controller)
            .await?;
    }
    if route_control_request(&controller, &request, method, message_sink).await? {
        return Ok(());
    }

    let (event_sink, mut event_stream) = crate::backend::events::channel();
    let _child_quiescence = if matches!(method, HarnessMethod::GoalPause | HarnessMethod::GoalClear) {
        Some(quiesce_children(&controller).await?)
    } else {None};
    let mut broker = acquire_request_broker(&controller, method).await?;
    let (data_root, lease_session_id, client_id) = broker.lease_identity();
    let (heartbeat_stop, heartbeat_stopped) = tokio::sync::oneshot::channel();
    let heartbeat = tokio::spawn(run_lease_heartbeat(
        data_root,
        lease_session_id,
        client_id,
        heartbeat_stopped,
    ));
    let message_sink_for_event = message_sink.clone();
    let routed_session_id = session_id.clone();
    let event_forwarder = tokio::spawn(async move {
        while let Some(event) = event_stream.recv().await? {
            let (event_name, payload) = if event.kind == "document_changed" {
                ("document_changed".to_owned(), event.data)
            } else {
                (
                    "backend_event".to_owned(),
                    serde_json::to_value(event).unwrap_or(Value::Null),
                )
            };
            message_sink_for_event
                .send_event(SessionEvent {
                    session_id: routed_session_id.clone(),
                    event: event_name,
                    payload,
                })
                .await?;
        }
        Ok::<(), anyhow::Error>(())
    });
    let shutdown = method == HarnessMethod::Shutdown;
    let continuation_store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let continuation_owner = continuation_store.latest_task_operation(&session_id)?.map(|operation| operation.id);
    let mut result = broker.dispatch_stream(request, event_sink).await;
    event_forwarder.await??;
    let mut continuing = result.response.error().is_none()
        && result.event.iter().any(|event| event.event == "goal_continue_requested");
    for event in result.event.drain(..) { message_sink.send_event(event).await?; }
    while continuing {
        if continuation_store.latest_task_operation(&session_id)?.map(|operation| operation.id) != continuation_owner { break; }
        match dispatch_task_attempt(&mut broker, &session_id,
            Request { id: 0, method: "goal.continue".into(), params: json!({}) }, message_sink, Arc::clone(&controller.catalog_request)).await {
            Ok(next) => continuing = next,
            Err(error) => {
                result.response = Response::failure(result.response.id, "continuation_failed", format!("{error:#}"));
                break;
            }
        }
    }
    let catalog_request = broker.backend_catalog_request();
    drop(broker);
    *controller.catalog_request.write().await = catalog_request;
    let _ = heartbeat_stop.send(());
    let _ = heartbeat.await;

    if let Some(child_session_id) = result
        .response
        .result()
        .and_then(|value| value.pointer("/session/id"))
        .and_then(Value::as_str)
        .filter(|child_session_id| *child_session_id != session_id)
    {
        registry.resolve(child_session_id).await?;
    }
    for event in result.event {
        message_sink.send_event(event).await?;
    }
    if shutdown {
        message_sink.send_terminal(Message::Response(result.response))?;
    } else {
        message_sink.send_response(result.response).await?;
    }
    Ok(())
}

async fn preview_native_import(settings: crate::settings::SettingsStore, params: &Value,
    catalog: &crate::backend::BackendCatalogRequest, backend: &str) -> Result<(crate::settings::SettingsStore, crate::settings::NativeImport)> {
    let kind = serde_json::from_value(params["kind"].clone())?;
    let source = PathBuf::from(params["source"].as_str().context("native import source is required")?);
    let scope = serde_json::from_value(params.get("scope").cloned().unwrap_or(json!("workspace")))?;
    let workspace = catalog.workspace.clone();
    let session_id = catalog.harness_session_id.clone();
    let backend = backend.to_owned();
    tokio::task::spawn_blocking(move || {
        let preview = settings.preview_import(kind, &source, scope, &workspace, &session_id, &backend)?;
        Ok((settings, preview))
    }).await.context("native import source inspection failed")?
}

fn admit_task_intent(controller: &SessionController, session_id: &str, request: &Request,
    store: &mut SqliteStore) -> Result<crate::task::TaskOperation> {
    let id = request.params.get("operation_id").and_then(Value::as_str)
        .filter(|id| !id.is_empty() && id.len() <= 128).context("task transition requires operation_id")?.to_owned();
    if let Some(previous) = store.load_task_operation(session_id, &id)? {
        ensure!(previous.action == request.params, "operation ID was reused for a different action");
        return Ok(previous);
    }
    let action = request.params.get("action").and_then(Value::as_str).context("task action is required")?;
    ensure!(["plan", "execute", "goal", "fork", "attach_plan", "resume", "pause", "clear", "permission", "configure", "compact"].contains(&action), "unknown task action");
    if action == "permission" {
        let permission: crate::session::PermissionMode = serde_json::from_value(request.params.get("mode").cloned().context("permission is required")?)?;
        ensure!(controller.backend.descriptor().capability.execution_mode_list.contains(&permission), "permission is unavailable for this backend");
    }
    if matches!(action, "plan" | "goal") {
        ensure!(request.params.get("text").and_then(Value::as_str).is_some_and(|text| !text.trim().is_empty()), "task objective is required");
    }
    let selected = crate::session::SessionStore::load_session(store, session_id)?.context("session is missing")?.current_task_id;
    let task = store.list_task(&session_id)?.into_iter().find(|task| Some(&task.id) == selected.as_ref());
    let prior = store.latest_task_operation(&session_id)?;
    let pending = prior.as_ref().filter(|operation| matches!(operation.state.as_str(),
        "admitted" | "accepted" | "stopping" | "running" | "applying" | "verifying" | "resuming"));
    if matches!(action, "permission" | "configure" | "compact")
        && let Some(pending) = pending.filter(|operation| matches!(operation.action["action"].as_str(), Some("plan" | "execute" | "goal" | "fork" | "attach_plan" | "resume"))) {
        ensure!(task.as_ref().is_some_and(|task| task.operation_id.as_deref() == Some(&pending.id)),
            "Task selection is still being admitted. Retry this setting after it starts, or pause the pending task");
    }
    let running_intent = pending
        .map(|operation| operation.resume_after_transition).unwrap_or_else(||
        task.as_ref().is_some_and(|task| task.status == crate::task::TaskStatus::Running)
            || (task.is_none() && controller.execution_admission.available_permits() == 0));
    let resume_after_transition = if matches!(action, "pause" | "clear") { false }
        else if matches!(action, "plan" | "execute" | "goal" | "fork" | "attach_plan" | "resume") { true }
        else { running_intent };
    let operation = crate::task::TaskOperation {
        id, session_id: session_id.to_owned(), action: request.params.clone(), state: "admitted".into(), error: None, exchange_id: None,
        created_at_ms: SystemTime::now().duration_since(UNIX_EPOCH)?.as_millis() as i64,
        resume_after_transition,
    };
    store.admit_task_operation(&operation)?;
    Ok(operation)
}

async fn route_task_transition(
    registry: Arc<SessionControllerRegistry>,
    controller: Arc<SessionController>,
    session_id: String,
    request: Request,
    sink: &MessageSender,
) -> Result<()> {
    let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let mut operation = admit_task_intent(&controller, &session_id, &request, &mut store)?;
    let admitted = store.claim_task_operation(&session_id, &operation.id)?;
    if !admitted {
        sink.send_control_wait(Message::Response(Response::success(request.id,
            store.load_task_operation(&session_id, &operation.id)?.map(|operation| operation.view()))?)).await?;
        return Ok(());
    }
    operation.state = "accepted".into();
    if operation.action["action"] == "configure" {
        *controller.mcp_discovery.lock().await = Default::default();
    }
    if let Some(revision) = operation.action.pointer("/config/settings_revision").and_then(Value::as_u64) {
        crate::settings::SettingsStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?
            .begin_application(&session_id, &operation.id, revision)?;
    }
    retain_configuration_event(&mut store, &operation)?;
    operation.exchange_id = store.list_exchange(&session_id)?.last().map(|exchange| exchange.id.clone());
    sink.send_control_wait(Message::Response(Response::success(request.id, json!({"id":operation.id,"state":operation.state}))?)).await?;
    let result = execute_task_transition(&registry, &controller, &mut operation, &mut store, sink).await;
    if operation.action["action"] == "configure" {
        *controller.mcp_discovery.lock().await = Default::default();
    }
    match result {
        Ok(()) => if operation.state != "superseded" { operation.state = "completed".into(); },
        Err(error) if error.is::<crate::broker::TurnCancelled>() => {
            operation.state = "cancelled".into();
            operation.error = None;
        }
        Err(error) => { operation.state = "failed".into(); operation.error = Some(format!("{error:#}")); }
    }
    if let Err(error) = store.save_task_operation(&operation) {
        operation.state = "outcome_unknown".into();
        operation.error = Some(format!("Task outcome could not be persisted: {error:#}. Effects may have occurred"));
    }
    retain_configuration_event(&mut store, &operation)?;
    if operation.action.pointer("/config/settings_revision").is_some() {
        crate::settings::SettingsStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?
            .finish_application(&session_id, &operation.id, &operation.state)?;
    }
    sink.send_control_wait(Message::Event(SessionEvent { session_id, event: "task_operation".into(), payload: operation.view() })).await?;
    Ok(())
}

fn retain_configuration_event(store: &mut SqliteStore, operation: &crate::task::TaskOperation) -> Result<()> {
    if operation.action["action"] != "configure" { return Ok(()); }
    let existing = store.list_session_event(&operation.session_id)?.into_iter()
        .find(|event| event.id == format!("configuration-{}",operation.id));
    let created_at_ms = existing.map(|event|event.created_at_ms).unwrap_or(operation.created_at_ms);
    store.save_session_event(&crate::timeline::SessionEventRecord {
        id:format!("configuration-{}",operation.id),session_id:operation.session_id.clone(),created_at_ms,
        detail:crate::timeline::SessionEventKind::Configuration { operation_id:operation.id.clone(),
            state:operation.state.clone(),field:operation.action["config"]["setting_fields"].as_array()
                .map(|field|field.iter().filter_map(|field|field.as_str().map(str::to_owned)).collect())
                .unwrap_or_else(||operation.action["config"].as_object()
                    .map(|config|config.keys().cloned().collect()).unwrap_or_default()),reason:operation.error.clone() },
    })
}

async fn execute_task_transition(
    registry: &SessionControllerRegistry,
    controller: &SessionController,
    operation: &mut crate::task::TaskOperation,
    store: &mut SqliteStore,
    sink: &MessageSender,
) -> Result<()> {
    let task_list = store.list_task(&operation.session_id)?;
    let session = crate::session::SessionStore::load_session(store, &operation.session_id)?.context("session is missing")?;
    let current_task = task_list.iter().find(|task| Some(&task.id) == session.current_task_id.as_ref());
    if operation.action["action"] == "resume" && current_task.is_some_and(|task|
        task.status == crate::task::TaskStatus::Running && operation.action.get("task_id").and_then(Value::as_str).is_none_or(|id| id == task.id)) {
        return Ok(());
    }
    let mut action = operation.action.clone();
    if matches!(action["action"].as_str(), Some("permission" | "configure" | "compact")) {
        action["resume"] = json!(operation.resume_after_transition);
    }
    let control = tokio::time::timeout(Duration::from_secs(10), Arc::clone(&controller.control_admission).acquire_owned())
        .await.context("task transition is waiting for previous cleanup. Execution state is unknown")??;
    if store.latest_task_operation(&operation.session_id)?.is_none_or(|latest| latest.id != operation.id) {
        operation.state = "superseded".into();
        return Ok(());
    }
    operation.state = "stopping".into();
    store.save_task_operation(operation)?;
    retain_configuration_event(store, operation)?;
    let changes_child_policy = action["action"] == "permission"
        || (action["action"] == "configure" && action["config"].as_object().is_some_and(|config|
            config.keys().any(|field|matches!(field.as_str(),"access" | "execution_mode" | "sandbox_enabled" | "backend" | "provider"))
                || config.get("setting_fields").and_then(Value::as_array).is_some_and(|fields|fields.iter().filter_map(Value::as_str)
                    .any(|field|matches!(field.split('.').next(),Some("access" | "execution_mode" | "sandbox_enabled" | "backend" | "provider"))))));
    let _child_quiescence = if changes_child_policy || matches!(action["action"].as_str(),Some("pause" | "clear" | "stop" | "cancel" | "undo" | "verify")) {
        Some(quiesce_children(controller).await?)
    } else {None};
    let execution = match Arc::clone(&controller.execution_admission).try_acquire_owned() {
        Ok(execution) => execution,
        Err(tokio::sync::TryAcquireError::Closed) => anyhow::bail!("session execution admission is closed"),
        Err(tokio::sync::TryAcquireError::NoPermits) => {
            let restart = operation.resume_after_transition
                && matches!(action["action"].as_str(), Some("permission" | "configure" | "compact"));
            controller.cancellation.begin_cleanup(restart, false)?;
            let approval_owner = crate::backend::ExecutionOwner::root(&controller.catalog_request.read().await.harness_session_id);
                controller.permission.cancel_owner(&approval_owner, None).await?;
            if let Err(error) = controller.backend.cleanup_execution(&operation.session_id).await {
                controller.cancellation.fail_cleanup(&error);
                return Err(error.context("collect current task before transition"));
            }
            controller.cancellation.request(false);
            // Execution ownership includes finalization and response delivery, independently of read-only broker locks.
            tokio::time::timeout(Duration::from_secs(10), Arc::clone(&controller.execution_admission).acquire_owned())
                .await.context("previous execution did not finish finalization within 10s. Inspect task state before resuming")??
        }
    };
    operation.exchange_id = store.list_exchange(&operation.session_id)?.last().map(|exchange| exchange.id.clone());
    if store.latest_task_operation(&operation.session_id)?.is_none_or(|latest| latest.id != operation.id) {
        operation.state = "superseded".into();
        return Ok(());
    }
    controller.cancellation.arm(false);
    drop(control);
    operation.state = "running".into();
    store.save_task_operation(operation)?;
    sink.send_event(SessionEvent {
        session_id: operation.session_id.clone(), event: "task_operation".into(),
        payload: json!({"id": operation.id, "state": "running"}),
    }).await?;
    let mut broker = controller.broker.lock().await;
    let (root, session_id, client_id) = broker.lease_identity();
    let (stop, stopped) = tokio::sync::oneshot::channel();
    let heartbeat = tokio::spawn(run_lease_heartbeat(root, session_id, client_id, stopped));
    let mut request = Request { id: 0, method: "task.transition".into(), params: action };
    let outcome = loop {
        let result = dispatch_task_attempt(&mut broker, &operation.session_id, request, sink, Arc::clone(&controller.catalog_request)).await;
        match result {
            Err(error) => break Err(error),
            Ok(continuing) => {
                if !continuing || store.latest_task_operation(&operation.session_id)?.is_none_or(|latest| latest.id != operation.id) { break Ok(()); }
                request = Request { id: 0, method: "goal.continue".into(), params: json!({}) };
            }
        }
    };
    *controller.catalog_request.write().await = broker.backend_catalog_request();
    drop(broker);
    let _ = stop.send(());
    heartbeat.await?;
    operation.exchange_id = store.list_exchange(&operation.session_id)?.last().map(|exchange| exchange.id.clone());
    outcome?;
    drop(execution);
    let _ = registry;
    Ok(())
}

async fn dispatch_task_attempt(broker: &mut HarnessBroker, session_id: &str, request: Request, sink: &MessageSender, catalog_request: Arc<RwLock<crate::backend::BackendCatalogRequest>>) -> Result<bool> {
    let (event_sink, mut event_stream) = crate::backend::events::channel();
    let output = sink.clone();
    let routed_session = session_id.to_owned();
    let data_root = broker.data_root().to_path_buf();
    let forwarder = tokio::spawn(async move {
        while let Some(event) = event_stream.recv().await? {
            if event.kind == "configuration_progress" {
                let operation_id = event.data["operation_id"].as_str().context("configuration operation is missing")?;
                let state = event.data["state"].as_str().context("configuration state is missing")?;
                anyhow::ensure!(matches!(state, "applying" | "verifying" | "resuming"), "invalid configuration state");
                let mut store = SqliteStore::open(&data_root)?;
                let mut operation = store.load_task_operation(&routed_session, operation_id)?
                    .context("configuration operation is missing")?;
                operation.state = state.into();
                store.save_task_operation(&operation)?;
                retain_configuration_event(&mut store, &operation)?;
                output.send_control_wait(Message::Event(SessionEvent { session_id:routed_session.clone(),
                    event:"task_operation".into(), payload:operation.view() })).await?;
                continue;
            }
            if event.kind == "configuration_applied" {
                *catalog_request.write().await = serde_json::from_value(event.data["catalog"].clone())?;
                output.send_event(SessionEvent { session_id:routed_session.clone(),
                    event:"session_configured".into(), payload:event.data }).await?;
                continue;
            }
            let (name, payload) = if event.kind == "document_changed" { ("document_changed", event.data) }
                else { ("backend_event", serde_json::to_value(event)?) };
            output.send_event(SessionEvent { session_id: routed_session.clone(), event: name.into(), payload }).await?;
        }
        Ok::<(), anyhow::Error>(())
    });
    let result = broker.dispatch_stream(request, event_sink).await;
    forwarder.await??;
    let continuing = result.event.iter().any(|event| event.event == "goal_continue_requested");
    for event in result.event { sink.send_event(event).await?; }
    if let Some(failure) = result.response.error() {
        if matches!(failure.code.as_str(), "turn_cancelled" | "turn_retracted") {
            return Err(anyhow::Error::new(crate::broker::TurnCancelled));
        }
        anyhow::bail!("{}", failure.message);
    }
    Ok(continuing)
}

async fn route_new_session(
    registry: Arc<SessionControllerRegistry>,
    session_id: String,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let child = prepare_new_session(
        PathBuf::from(&registry.initialize.data_root).as_path(),
        &registry.initialize.client_id,
        &registry.initialize.backend.kind,
        registry
            .runtime
            .backend_handle()
            .descriptor()
            .capability
            .native_compact,
        &session_id,
        &request.params,
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as i64,
    )?;
    let child_controller = registry.resolve(&child.id).await?;
    let snapshot = child_controller.broker.lock().await.snapshot()?;
    message_sink
        .send_response(Response::success(request.id, snapshot)?)
        .await?;
    message_sink.send(Message::Event(SessionEvent {
        session_id: child.id.clone(),
        event: "session_created".into(),
        payload: serde_json::to_value(child)?,
    }))?;
    Ok(())
}

async fn route_session_fork(
    registry: Arc<SessionControllerRegistry>,
    session_id: String,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let mut task = registry.fork_task.lock().await;
    while let Some(result) = task.try_join_next() {
        result.context("Harness provider fork task failed")?;
    }
    ensure!(
        task.len() < forge_protocol::MAX_ACTIVE_REQUESTS,
        "Harness provider fork admission is full"
    );
    let backend = registry.runtime.backend_handle();
    let descriptor = backend.descriptor();
    let preparation = prepare_provider_fork(
        PathBuf::from(&registry.initialize.data_root).as_path(),
        &registry.initialize.client_id,
        &registry.initialize.backend.kind,
        &descriptor.capability,
        &session_id,
        &request.params,
    )?;
    let child_session_id = preparation.child.id.clone();
    let child_controller = registry.resolve(&child_session_id).await?;
    let gate = ProviderForkGate::new();
    registry
        .provider_fork_by_id
        .write()
        .await
        .insert(child_session_id.clone(), Arc::clone(&gate));

    let mut snapshot = serde_json::to_value(child_controller.broker.lock().await.snapshot()?)?;
    snapshot["fork_performance"] = json!({
        "total_ms": preparation.total_duration_ms,
        "timing": preparation.timing,
        "provider_pending": true,
    });
    message_sink
        .send_response(Response::success(request.id, snapshot)?)
        .await?;
    message_sink.send(Message::Event(SessionEvent {
        session_id: child_session_id.clone(),
        event: "session_created".into(),
        payload: serde_json::to_value(&preparation.child)?,
    }))?;

    let message_sink = message_sink.clone();
    let registry_for_completion = Arc::clone(&registry);
    task.spawn(async move {
        let backend_started = Instant::now();
        let mut result = backend
            .fork(preparation.backend_request)
            .await
            .map_err(|error| format!("{error:#}"));
        if let Ok(result) = &mut result {
            result.timing.push(crate::backend::BackendTimingRecord {
                phase: "broker.backend_total".into(),
                duration_ms: backend_started.elapsed().as_secs_f64() * 1000.0,
            });
        }
        let mut readiness = result.as_ref().map(|_| ()).map_err(ToOwned::to_owned);
        let completion = child_controller
            .broker
            .lock()
            .await
            .complete_provider_fork(result);
        match completion {
            Ok(event) => {
                if let Err(error) = message_sink.send_event(event).await {
                    readiness = Err(format!("deliver provider fork outcome: {error:#}"));
                }
            }
            Err(error) => {
                readiness = Err(format!("persist provider fork outcome: {error:#}"));
            }
        }
        gate.complete(readiness).await;
        registry_for_completion
            .provider_fork_by_id
            .write()
            .await
            .remove(&child_session_id);
    });
    Ok(())
}

async fn resume_session(
    registry: Arc<SessionControllerRegistry>,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let target_session_id = request
        .params
        .get("session_id")
        .and_then(Value::as_str)
        .context("session.resume requires session_id")?;
    let controller = registry.resolve(target_session_id).await?;
    let snapshot = controller.broker.lock().await.snapshot()?;
    message_sink
        .send_response(Response::success(request.id, snapshot)?)
        .await?;
    Ok(())
}

/// Admit goal control during startup without releasing the acquired dispatch owner.
async fn acquire_request_broker(
    controller: &SessionController,
    method: HarnessMethod,
) -> Result<MutexGuard<'_, HarnessBroker>> {
    let broker = controller.broker.lock();
    tokio::pin!(broker);
    if matches!(method, HarnessMethod::GoalPause | HarnessMethod::GoalClear) {
        let session_id = controller
            .catalog_request
            .read()
            .await
            .harness_session_id
            .clone();
        tokio::select! {
            biased;
            broker = &mut broker => return Ok(broker),
            result = controller.backend.stop_goal_session(
                &session_id,
                method == HarnessMethod::GoalClear,
            ) => result?,
        }
    }
    Ok(broker.await)
}

async fn quiesce_children(controller: &SessionController) -> Result<crate::agent::AgentQuiescence> {
    let quiescence = controller.agent_coordinator.begin_quiescence();
    if controller.agent_coordinator.active_count() == 0 {return Ok(quiescence);}
    let mut activity = controller.agent_coordinator.activity();
    tokio::time::timeout(Duration::from_secs(35),async {
    controller.agent_coordinator.sender().request(crate::agent::AgentRequest {
        operation_id:uuid::Uuid::new_v4().to_string(),source:crate::agent::AgentSource::User,
        operation:crate::agent::AgentOperation::InterruptAll,
    }).await?;
        loop {
            if controller.agent_coordinator.active_count() == 0 {return Ok::<_,anyhow::Error>(());}
            for (agent,_) in controller.agent_coordinator.snapshot(None) {
                if let Some(task) = controller.agent_coordinator.cleanup_task(&agent.id) {
                    anyhow::bail!("child {} still requires cleanup: {}",agent.task_path,task.error.unwrap_or_default());
                }
            }
            activity.changed().await.context("child activity owner closed during cleanup")?;
        }
    }).await.context("children did not release execution ownership within 35 seconds")??;
    Ok(quiescence)
}

async fn route_control_request(
    controller: &SessionController,
    request: &Request,
    method: HarnessMethod,
    message_sink: &MessageSender,
) -> Result<bool> {
    if matches!(method,HarnessMethod::AgentStart | HarnessMethod::AgentMessage | HarnessMethod::AgentFollowup
        | HarnessMethod::AgentInterrupt | HarnessMethod::AgentInterruptAll) {
        use crate::agent::AgentOperation;
        let text = |field:&str|request.params[field].as_str().filter(|value|!value.trim().is_empty()).map(str::to_owned)
            .with_context(||format!("{} requires {field}",request.method));
        let operation = match method {
            HarnessMethod::AgentStart => {
                let mut arguments = request.params.clone();
                if let Some(arguments) = arguments.as_object_mut() {arguments.remove("operation_id");}
                AgentOperation::Spawn(serde_json::from_value(arguments)?)
            },
            HarnessMethod::AgentMessage => AgentOperation::Message {target:text("target")?,message:text("message")?},
            HarnessMethod::AgentFollowup => AgentOperation::Followup {target:text("target")?,message:text("message")?},
            HarnessMethod::AgentInterrupt => AgentOperation::Interrupt {target:text("target")?},
            HarnessMethod::AgentInterruptAll => AgentOperation::InterruptAll,
            _ => unreachable!(),
        };
        let result = controller.agent_coordinator.sender().request(crate::agent::AgentRequest {
            operation_id:request.params["operation_id"].as_str().map(str::to_owned).unwrap_or_else(||uuid::Uuid::new_v4().to_string()),
            source:crate::agent::AgentSource::User,operation}).await?;
        message_sink.send_response(Response::success(request.id,result)?).await?;
        return Ok(true);
    }
    if matches!(method, HarnessMethod::PlanAcceptanceBegin | HarnessMethod::PlanRequestChanges | HarnessMethod::PlanCancel
        | HarnessMethod::PlanEntityRename | HarnessMethod::PlanTestsDelete)
        && let Some((value, events)) = controller.revision_review.decide(method, request.params.clone()).await? {
        for event in events { message_sink.send_event(event).await?; }
        message_sink.send_response(Response::success(request.id, value)?).await?;
        return Ok(true);
    }
    let _control = if matches!(method, HarnessMethod::TurnCancel | HarnessMethod::TurnRestart | HarnessMethod::BackendMcpSetEnabled) {
        Some(Arc::clone(&controller.control_admission)
            .try_acquire_owned()
            .context("Harness cleanup is already running")?)
    } else {
        None
    };
    let catalog_request = controller.catalog_request.read().await.clone();
    let response = match method {
        HarnessMethod::TurnCancel => {
            validate_root_control(request.params.get("target"))?;
            {
                let _child_quiescence = quiesce_children(controller).await?;
                let restore_prompt = request
                    .params
                    .get("restore_prompt_if_no_output")
                    .and_then(Value::as_bool)
                    .unwrap_or(false);
                if let Ok(mut broker) = controller.broker.try_lock() {
                    controller.cancellation.request(restore_prompt);
                    let result = broker.dispatch(request.clone()).await;
                    for event in result.event {
                        message_sink.send_event(event).await?;
                    }
                    message_sink.send_control(Message::Response(result.response))?;
                    return Ok(true);
                }
                controller
                    .cancellation
                    .begin_cleanup(false, restore_prompt)?;
                let approval_owner = crate::backend::ExecutionOwner::root(&controller.catalog_request.read().await.harness_session_id);
                controller.permission.cancel_owner(&approval_owner, None).await?;
                let cleanup = controller
                    .backend
                    .cleanup_execution(&catalog_request.harness_session_id)
                    .await;
                if let Err(error) = cleanup {
                    controller.cancellation.fail_cleanup(&error);
                    return Err(error);
                }
                controller.cancellation.request(restore_prompt);
                let approval_owner = crate::backend::ExecutionOwner::root(&controller.catalog_request.read().await.harness_session_id);
                controller.permission.cancel_owner(&approval_owner, None).await?;
            }
            Some(Response::success(
                request.id,
                json!({ "cancel_requested": true }),
            )?)
        }
        HarnessMethod::TurnRestart => {
            let _child_quiescence = quiesce_children(controller).await?;
            controller.cancellation.begin_cleanup(true, false)?;
            let approval_owner = crate::backend::ExecutionOwner::root(&controller.catalog_request.read().await.harness_session_id);
                controller.permission.cancel_owner(&approval_owner, None).await?;
            if let Err(error) = controller
                .backend
                .cleanup_execution(&catalog_request.harness_session_id)
                .await
            {
                controller.cancellation.fail_cleanup(&error);
                return Err(error);
            }
            controller.cancellation.request_restart();
            let approval_owner = crate::backend::ExecutionOwner::root(&controller.catalog_request.read().await.harness_session_id);
                controller.permission.cancel_owner(&approval_owner, None).await?;
            Some(Response::success(
                request.id,
                json!({ "restart_requested": true, "mode": request.params.get("mode") }),
            )?)
        }
        HarnessMethod::TurnSteer => {
            let text = request
                .params
                .get("text")
                .and_then(Value::as_str)
                .map(str::trim)
                .filter(|text| !text.is_empty())
                .context("turn.steer requires non-empty text")?
                .to_owned();
            validate_root_control(request.params.get("target"))?;
            if controller.parent_input.is_active() {
                controller.parent_input.steer(text.into()).await?;
            } else {
                controller.backend.steer_session(&catalog_request.harness_session_id,text.into()).await?;
            }
            Some(Response::success(
                request.id,
                json!({ "steered": true, "delivery": "direct" }),
            )?)
        }
        HarnessMethod::ApprovalResolve => {
            let approval_id = request
                .params
                .get("approval_id")
                .and_then(Value::as_str)
                .context("approval.resolve requires approval_id")?;
            let answer_list = request
                .params
                .get("answer_list")
                .context("approval.resolve requires answer_list")?;
            let answer_list: Vec<crate::backend::approval::ApprovalAnswer> =
                serde_json::from_value(answer_list.clone())?;
            let approval = controller
                .permission
                .resolve(approval_id, &answer_list, None)
                .await?;
            if let (Some(owner), Some(task_id)) = (&approval.execution_owner, &approval.exchange_id)
                && !owner.is_root() {
                controller.agent_coordinator.sender().send(crate::agent::AgentCommand::Event {
                    owner:owner.clone(),task_id:task_id.clone(),event:crate::backend::BackendEvent {
                        received_at_ms:None,address:None,turn_boundary:None,kind:"approval_resolved".into(),
                        text:None,data:serde_json::to_value(&approval)?,activity:None,summary:None,task_update:None,
                    },
                }).await?;
            }
            Some(Response::success(
                request.id,
                json!({ "resolved": true, "approval": approval }),
            )?)
        }
        HarnessMethod::BackendSkills => Some(Response::success(
            request.id,
            controller
                .backend
                .skill_list(catalog_request.clone())
                .await?,
        )?),
        HarnessMethod::BackendMcp => {
            let server_list = controller
                .mcp_discovery
                .lock()
                .await
                .snapshot(controller.backend.clone(), catalog_request.clone())
                .await?;
            Some(Response::success(request.id, server_list)?)
        }
        _ => None,
    };
    if let Some(response) = response {
        if matches!(
            method,
            HarnessMethod::TurnCancel | HarnessMethod::TurnRestart
        ) {
            message_sink.send_control(Message::Response(response))?;
        } else {
            message_sink.send_response(response).await?;
        }
        return Ok(true);
    }
    Ok(false)
}

fn initialize_failure(request_id: u64, error: &anyhow::Error) -> Response {
    error.downcast_ref::<SessionLeaseConflict>().map_or_else(
        || Response::failure(request_id, "initialize_failed", format!("{error:#}")),
        |conflict| {
            Response::failure_with_data(
                request_id,
                "session_lease_conflict",
                conflict.to_string(),
                json!({
                    "session_id": conflict.session_id,
                    "native_fork": conflict.native_fork,
                }),
            )
        },
    )
}


fn validate_root_control(value: Option<&Value>) -> Result<()> {
    anyhow::ensure!(value.is_none_or(Value::is_null),
        "Root turn controls cannot address provider child threads. Use agent.message or agent.interrupt with a Harness child identity");
    Ok(())
}

async fn run_lease_heartbeat(
    data_root: PathBuf,
    session_id: String,
    client_id: String,
    mut stopped: tokio::sync::oneshot::Receiver<()>,
) {
    let mut interval = tokio::time::interval(Duration::from_secs(10));
    interval.tick().await;
    loop {
        tokio::select! {
            _ = &mut stopped => return,
            _ = interval.tick() => {
                let now_ms = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_millis() as i64;
                if let Ok(mut store) = SqliteStore::open(&data_root) {
                    let _ = store.renew_session_lease(&session_id, &client_id, now_ms);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_diff::cache::CacheLimits;

    #[tokio::test]
    async fn settings_application_orders_progress_and_retains_value_free_timeline() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await?;
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let observation = tokio::spawn(async move {
            let mut state = Vec::new();
            while let Some(frame) = output.recv().await? {
                let frame: Value = serde_json::from_slice(frame.bytes())?;
                if frame["event"] == "task_operation" {
                    let phase = frame["payload"]["state"].as_str().unwrap().to_owned();
                    let finished = phase == "completed" || phase == "failed";
                    state.push(phase);
                    if finished { break; }
                }
            }
            Ok::<_, anyhow::Error>(state)
        });
        let request = Request { id:2, method:"settings.update".into(),
            params:json!({"operation_id":"settings-retry","expected_revision":0,"scope":"global","desired":{"plan_compact":true}}) };
        service.dispatch(Some(session_id.clone()), request.clone(), &sink).await?;
        let state = tokio::time::timeout(Duration::from_secs(2), observation).await???;
        let application_state: Vec<_> = state.iter().filter(|phase| phase.as_str() != "running").map(String::as_str).collect();
        assert_eq!(application_state, ["applying", "verifying", "completed"]);
        let (retry_sink, mut retry_output) = forge_protocol::outbound::channel();
        service.dispatch(Some(session_id.clone()), request.clone(), &retry_sink).await?;
        let replay = retry_output.recv().await?.context("settings retry response is missing")?;
        let replay: Value = serde_json::from_slice(replay.bytes())?;
        assert_eq!(replay["result"]["id"], "settings-retry");
        assert_eq!(replay["result"]["state"], "completed");
        let mut conflicting = request;
        conflicting.params["desired"]["plan_compact"] = json!(false);
        assert!(service.dispatch(Some(session_id.clone()), conflicting, &retry_sink).await
            .unwrap_err().to_string().contains("reused with a different request"));
        let root = fixture.path().join("data");
        let settings = crate::settings::SettingsStore::open(&root)?;
        let application = settings.application(&session_id)?;
        assert_eq!(application.state, "applied");
        assert_eq!(application.generation, 1);
        assert_eq!(settings.get("workspace",&session_id,"mock")?.revision, 1);
        let store = SqliteStore::open(&root)?;
        let event = store.list_session_event(&session_id)?;
        assert_eq!(event.len(), 1);
        let crate::timeline::SessionEventKind::Configuration { state, field, .. } = &event[0].detail else {
            panic!("configuration event is missing");
        };
        assert_eq!(state, "completed");
        assert_eq!(field, &["plan_compact"]);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn active_chat_settings_ignore_completed_idle_operations_but_preserve_pending_pause() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await?;
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let request = |id: &str, action: &str| Request { id:2, method:"task.transition".into(),
            params:json!({"operation_id":id,"action":action,"config":{}}) };
        let mut idle = admit_task_intent(&controller, &session_id, &request("idle-config", "configure"), &mut store)?;
        assert!(!idle.resume_after_transition);
        idle.state = "completed".into();
        store.save_task_operation(&idle)?;
        let active = controller.execution_admission.acquire().await?;
        let mut changed = admit_task_intent(&controller, &session_id, &request("active-config", "configure"), &mut store)?;
        assert!(changed.resume_after_transition, "a completed idle configuration suppressed the new active chat");
        changed.state = "completed".into();
        store.save_task_operation(&changed)?;
        let pause = admit_task_intent(&controller, &session_id, &request("pending-pause", "pause"), &mut store)?;
        assert!(!pause.resume_after_transition);
        let stopped = admit_task_intent(&controller, &session_id, &request("after-pause", "configure"), &mut store)?;
        assert!(!stopped.resume_after_transition, "configuration resumed work despite a pending explicit pause");
        drop(active);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn provider_setup_rejects_active_execution_and_pending_cleanup() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await?;
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let (sink, _output) = forge_protocol::outbound::channel();
        for pending_cleanup in [false, true] {
            let admission = if pending_cleanup { &controller.control_admission } else { &controller.execution_admission };
            let occupied = admission.acquire().await?;
            for method in ["backend.setup", "backend.authenticate"] {
                let error = service.dispatch(Some(session_id.clone()), Request {
                    id:2, method:method.into(), params:json!({"method":"oauth-personal"}),
                }, &sink).await.unwrap_err();
                assert!(error.to_string().contains(if pending_cleanup { "cleanup is pending" } else { "Finish active work" }));
            }
            drop(occupied);
        }
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn model_pins_bypass_the_active_provider_broker_lock() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await.unwrap();
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        for (method, parameter) in [
            ("backend.model_pin", json!({"backend":"copilot", "model":"pinned-model", "pinned":true})),
            ("backend.model_pins", json!({"backend":"copilot"})),
        ] {
            tokio::time::timeout(Duration::from_secs(2), service.dispatch(Some(session_id.clone()),
                Request { id: 2, method: method.into(), params: parameter }, &sink))
                .await.expect("pins must not wait for provider execution").unwrap();
            let frame = output.recv().await.unwrap().unwrap();
            let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
            assert_eq!(value["result"], json!(["pinned-model"]));
        }
        drop(broker);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn child_model_catalog_bypasses_the_active_root_broker_lock() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await.unwrap();
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        tokio::time::timeout(Duration::from_secs(2), service.dispatch(Some(session_id.clone()),
            Request { id: 2, method: "agent.models".into(), params: json!({"provider":"mock"}) }, &sink))
            .await.expect("child model selection must remain available during root execution").unwrap();
        loop {
            let frame = output.recv().await.unwrap().unwrap();
            let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
            if value["id"] == 2 {
                assert!(value["error"].is_null(), "{value}");
                assert!(value["result"].as_array().is_some_and(|models| !models.is_empty()), "{value}");
                break;
            }
        }
        drop(broker);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn revision_review_controls_bypass_the_waiting_provider_broker_lock() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await.unwrap();
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let mut review = controller.revision_review.open().unwrap();
        let (sink, mut output) = forge_protocol::outbound::channel();
        for method in [HarnessMethod::PlanAcceptanceBegin, HarnessMethod::PlanRequestChanges,
            HarnessMethod::PlanCancel, HarnessMethod::PlanEntityRename, HarnessMethod::PlanTestsDelete] {
            let request = Request { id:2, method:"review".into(), params:json!({"plan_id":"plan"}) };
            let respond = async {
                let Some(crate::plan::revision_review::RevisionReviewRequest::Decide {
                    method: received, params, response,
                }) = review.receiver.recv().await else { panic!("missing review decision") };
                assert_eq!(received, method);
                assert_eq!(params, request.params);
                response.send(Ok((json!({"reviewed":true}), Vec::new()))).unwrap();
            };
            let (routed, ()) = tokio::time::timeout(Duration::from_secs(1), async {
                tokio::join!(route_control_request(&controller, &request, method, &sink), respond)
            }).await.expect("review controls must not acquire the provider's broker lock");
            assert!(routed.unwrap());
            let frame = output.recv().await.unwrap().unwrap();
            let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
            assert_eq!(value["result"]["reviewed"], true);
        }
        drop(review);
        drop(broker);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn task_stop_intent_and_health_remain_available_while_the_broker_is_owned() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await.unwrap();
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let stopping = service.dispatch(Some(session_id.clone()), Request { id: 2, method: "task.transition".into(), params: json!({"operation_id":"stop-once","action":"pause"}) }, &sink);
        tokio::pin!(stopping);
        tokio::select! {
            result = &mut stopping => panic!("stop completed before the old owner settled: {result:?}"),
            frame = output.recv() => {
                let frame = frame.unwrap().unwrap();
                let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
                assert_eq!(value["result"]["state"], "accepted");
            }
        }
        tokio::time::timeout(Duration::from_secs(1), service.dispatch(Some(session_id.clone()), Request { id: 3, method: "health.get".into(), params: json!({}) }, &sink)).await.unwrap().unwrap();
        let frame = output.recv().await.unwrap().unwrap();
        let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
        assert_eq!(value["result"]["responsive"], true);
        drop(frame);
        drop(broker);
        tokio::time::timeout(Duration::from_secs(2), &mut stopping).await.unwrap().unwrap();
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path()).unwrap();
        assert_eq!(store.load_task_operation(&session_id,"stop-once").unwrap().unwrap().state, "completed");
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    struct SteeringBackend {
        request: Mutex<Vec<(String, String)>>,
    }

    #[async_trait::async_trait]
    impl crate::backend::Backend for SteeringBackend {
        async fn shutdown(&self) -> Result<()> { Ok(()) }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("backend cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> { Ok(()) }

        async fn has_active_turn(&self, _session_id: &str) -> bool { false }

        async fn active_session_id_for(&self, _session_id: &str) -> Option<String> { None }


        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Steering test".into(),
                capability: Default::default(),
            }
        }

        async fn prompt_stream(
            &self,
            _: crate::backend::BackendRequest,
            _: Option<crate::backend::BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            anyhow::bail!("steering must not start another provider request")
        }

        async fn steer_session(&self, session_id: &str, input: crate::backend::SteeringInput) -> Result<()> {
            self.request.lock().await.push((session_id.into(), input.text().to_owned()));
            Ok(())
        }

        async fn stop_goal_session(&self, session_id: &str, clear: bool) -> Result<()> {
            self.request.lock().await.push((
                session_id.into(),
                if clear {
                    "clear".into()
                } else {
                    "pause".into()
                },
            ));
            Ok(())
        }

    }


    #[tokio::test]
    async fn child_approval_resolution_and_clear_use_the_owned_runtime() -> Result<()> {
        use crate::permissions::matcher::{PermissionRequest,PermissionTarget};
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service.open_session(1,initialize(&fixture,"visible-blocking")).await?;
        let session_id = opened.result().context("session did not open")?["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let (sink,mut output) = forge_protocol::outbound::channel();
        controller.attach_agents(&sink).await?;
        let admitted = controller.agent_coordinator.sender().request(crate::agent::AgentRequest {
            operation_id:"approval-child".into(),source:crate::agent::AgentSource::User,
            operation:crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest { profile: Some("worker".into()), text: "Wait".into() }),
        }).await?;
        let agent_id = admitted["agent"]["id"].as_str().unwrap().to_owned();
        let task_id = admitted["task"]["id"].as_str().unwrap().to_owned();
        let owner = crate::backend::ExecutionOwner {session_id:session_id.clone(),agent_id:agent_id.clone()};
        let permission = controller.permission.clone();
        let (event_sink,mut events) = crate::backend::events::channel();
        let approval_owner = owner.clone();
        let pending = tokio::spawn(async move {
            permission.authorize_callback(crate::session::PermissionMode::Read,true,PermissionRequest {
                id:"child-request".into(),provider:"Mock".into(),reason:None,
                target_list:vec![PermissionTarget::Command {command:"probe".into(),shell:None}],
            },Some(crate::backend::ProviderAddress {owner:Some(approval_owner),thread_id:"mock-session".into(),turn_id:"mock-turn".into()}),Some(&event_sink)).await
        });
        let requested = tokio::time::timeout(Duration::from_secs(2),events.recv()).await??.unwrap();
        controller.agent_coordinator.sender().send(crate::agent::AgentCommand::Event {
            owner,task_id:task_id.clone(),event:requested,
        }).await?;
        let requested_view = tokio::time::timeout(Duration::from_secs(2),async {
            loop {
                let frame = output.recv().await?.context("event stream ended")?;
                let value:Value = serde_json::from_slice(frame.bytes())?;
                if value["payload"]["kind"] == "approval_requested" {return Ok::<_,anyhow::Error>(value["payload"]["data"].clone());}
            }
        }).await??;
        assert_eq!(requested_view["exchange_id"],task_id);
        let approval_id = requested_view["id"].as_str().unwrap();
        let answers:Vec<_> = requested_view["item_list"].as_array().unwrap().iter()
            .map(|item|json!({"item_id":item["id"],"choice_id":"allow_once"})).collect();
        assert!(route_control_request(&controller,&Request {id:2,method:"approval.resolve".into(),
            params:json!({"approval_id":approval_id,"answer_list":answers})},HarnessMethod::ApprovalResolve,&sink).await?);
        assert_eq!(pending.await??,crate::backend::approval::ApprovalResolution::AllowOnce);
        tokio::time::timeout(Duration::from_secs(2),async {
            loop {
                let frame = output.recv().await?.context("event stream ended")?;
                let value:Value = serde_json::from_slice(frame.bytes())?;
                if value["payload"]["kind"] == "approval_resolved" {
                    assert_eq!(value["payload"]["data"]["execution_owner"]["agent_id"],agent_id);
                    return Ok::<_,anyhow::Error>(());
                }
            }
        }).await??;
        assert!(controller.permission.pending_list()?.is_empty());
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let mut operation = admit_task_intent(&controller,&session_id,&Request {id:3,method:"task.transition".into(),
            params:json!({"operation_id":"clear-child","action":"clear"})},&mut store)?;
        tokio::time::timeout(Duration::from_secs(3),execute_task_transition(&registry,&controller,&mut operation,&mut store,&sink)).await??;
        assert_eq!(controller.agent_coordinator.active_count(),0);
        assert_eq!(controller.agent_coordinator.task(&task_id).unwrap().state,crate::exchange::ExchangeState::Cancelled);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn idle_task_transition_waits_for_readers_without_provider_cleanup() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await.unwrap();
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await.unwrap();
        let mut controller = registry.controller_by_id.write().await.remove(&session_id).unwrap();
        Arc::get_mut(&mut controller).unwrap().backend = Arc::new(SteeringBackend {
            request: Mutex::new(Vec::new()),
        });
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path()).unwrap();
        let mut operation = admit_task_intent(&controller, &session_id, &Request {
            id: 2, method: "task.transition".into(),
            params: json!({"operation_id":"idle-pause","action":"pause"}),
        }, &mut store).unwrap();
        let reader = controller.broker.lock().await;
        let (sink, _output) = forge_protocol::outbound::channel();
        let mut transition = Box::pin(execute_task_transition(&registry, &controller, &mut operation, &mut store, &sink));
        assert!(futures_util::poll!(transition.as_mut()).is_pending(), "read-only ownership was treated as provider execution");
        drop(reader);
        tokio::time::timeout(Duration::from_secs(2), transition).await.unwrap().unwrap();
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn execution_admission_excludes_overlapping_work_and_pending_cleanup() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        assert!(service.open_session(1, initialize(&fixture, "mock")).await.unwrap().result().is_some());
        let execution = service.prepare(None, HarnessMethod::PromptSubmit).await.unwrap().unwrap();
        assert!(service.prepare(None, HarnessMethod::GoalResume).await.is_err());
        assert!(service.prepare(None, HarnessMethod::TurnCancel).await.unwrap().is_none());
        assert!(service.prepare(None, HarnessMethod::StateGet).await.unwrap().is_none());
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&registry.initial_session_id).await.unwrap();
        let cleanup = Arc::clone(&controller.control_admission).try_acquire_owned().unwrap();
        drop(execution);
        assert!(service.prepare(None, HarnessMethod::ExchangeResume).await.is_err());
        drop(cleanup);
        let resumed = service.prepare(None, HarnessMethod::ExchangeResume).await.unwrap().unwrap();
        drop(resumed);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn failed_initialization_can_retry_and_concurrent_open_has_one_owner() {
        let fixture = tempfile::tempdir().unwrap();
        let blocked = fixture.path().join("blocked");
        std::fs::write(&blocked, "file").unwrap();
        let service = service();
        let mut initialize = initialize(&fixture, "mock");
        initialize.data_root = blocked.to_string_lossy().into_owned();
        assert!(
            service
                .open_session(1, initialize.clone())
                .await
                .unwrap()
                .error()
                .is_some()
        );
        assert!(service.registry.lock().await.is_none());

        initialize.data_root = fixture.path().join("data").to_string_lossy().into_owned();
        let (first, second) = tokio::join!(
            service.open_session(2, initialize.clone()),
            service.open_session(3, initialize.clone()),
        );
        let first = first.unwrap();
        let second = second.unwrap();
        assert_ne!(first.result().is_some(), second.result().is_some());
        service.shutdown(Duration::from_secs(1)).await.unwrap();
        assert!(service.registry.lock().await.is_none());
        assert!(service.open_session(4, initialize).await.is_err());
    }

    #[tokio::test]
    async fn trace_controls_complete_while_a_turn_owns_the_broker() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await
            .unwrap();
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry.lock().await.clone().unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let owner = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        for (id, method, params) in [
            (2, "trace.configure", json!({"enabled":true})),
            (3, "trace.status", json!({})),
            (4, "trace.session.clear", json!({})),
            (5, "trace.configure", json!({"enabled":false})),
        ] {
            tokio::time::timeout(
                Duration::from_secs(1),
                service.dispatch(
                    Some(session_id.clone()),
                    Request {
                        id,
                        method: method.into(),
                        params,
                    },
                    &sink,
                ),
            )
            .await
            .expect("trace control waited for the broker")
            .unwrap();
            let frame = output.recv().await.unwrap().unwrap();
            let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
            assert_eq!(value["result"]["enabled"], id != 5);
            assert!(
                value["result"]["path"]
                    .as_str()
                    .unwrap()
                    .contains(&session_id)
            );
        }
        drop(owner);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn oversized_session_preview_uses_response_parts_and_keeps_broker_alive() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await
            .unwrap();
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry.lock().await.clone().unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let name = "preview history ".repeat(40000);
        let renamed = controller
            .broker
            .lock()
            .await
            .dispatch(Request {
                id: 2,
                method: "session.rename".into(),
                params: json!({"session_id":session_id, "name":name}),
            })
            .await;
        assert!(renamed.response.error().is_none());
        let (sink, mut output) = forge_protocol::outbound::channel();
        service
            .dispatch(
                None,
                Request {
                    id: 3,
                    method: "session.preview".into(),
                    params: json!({"session_id":session_id}),
                },
                &sink,
            )
            .await
            .unwrap();
        let mut encoded = String::new();
        loop {
            let frame = output.recv().await.unwrap().unwrap();
            assert!(frame.bytes().len() <= forge_protocol::MAX_FRAME_BYTES);
            let event: forge_protocol::message::RequestEvent =
                serde_json::from_slice(frame.bytes()).unwrap();
            assert_eq!(event.request_id, 3);
            if event.event == "result.complete" {
                break;
            }
            let part: forge_protocol::transfer::JsonPart =
                serde_json::from_value(event.payload).unwrap();
            encoded.push_str(&part.payload);
        }
        let response: Response = serde_json::from_str(&encoded).unwrap();
        assert_eq!(response.result().unwrap()["session"]["name"], name.trim());
        service
            .dispatch(
                None,
                Request {
                    id: 4,
                    method: "history.record".into(),
                    params: json!({"text":"still alive"}),
                },
                &sink,
            )
            .await
            .unwrap();
        output.check().unwrap();
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn shutdown_timeout_retains_fork_ownership_and_leases_until_completion() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let initialize = initialize(&fixture, "fork-blocking");
        let opened = service.open_session(1, initialize.clone()).await.unwrap();
        assert!(opened.error().is_none(), "{opened:?}");
        let (sink, mut output) = forge_protocol::outbound::channel();
        service
            .dispatch(
                None,
                Request {
                    id: 2,
                    method: "session.fork".into(),
                    params: json!({"name":"shutdown child"}),
                },
                &sink,
            )
            .await
            .unwrap();
        let frame = output.recv().await.unwrap().unwrap();
        let response: Response = serde_json::from_slice(frame.bytes()).unwrap();
        let child = response.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        drop(frame);

        assert!(service.shutdown(Duration::from_millis(10)).await.is_err());
        assert!(service.registry.lock().await.is_some());
        assert!(
            service
                .prepare(None, HarnessMethod::PromptSubmit)
                .await
                .is_err()
        );
        let contender_diff = DiffEngine::new(CacheLimits::default(), 1);
        let contender_syntax = SyntaxEngine::new(
            contender_diff.analysis_pool(),
        );
        let contender = HarnessService::new(
            Arc::new(RepositoryStore::default()),
            contender_diff,
            contender_syntax,
        );
        let mut reopen = initialize;
        reopen.client_id = "second-owner".into();
        reopen.session_id = Some(child);
        let conflict = contender.open_session(3, reopen.clone()).await.unwrap();
        assert_eq!(conflict.error().unwrap().code, "session_lease_conflict");

        service.shutdown(Duration::from_secs(5)).await.unwrap();
        assert!(service.registry.lock().await.is_none());
        let reopened = contender.open_session(4, reopen).await.unwrap();
        assert!(reopened.error().is_none(), "{reopened:?}");
        assert_eq!(
            reopened.result().unwrap()["session"]["provider_fork_state"]["state"],
            "ready"
        );
        contender.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn goal_control_reaches_provider_while_prompt_owns_broker() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let broker = HarnessBroker::initialize(initialize(&fixture, "mock"))?;
        let mut controller = SessionController::new(broker);
        let backend = Arc::new(SteeringBackend {
            request: Mutex::new(Vec::new()),
        });
        Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
        for method in [HarnessMethod::GoalPause, HarnessMethod::GoalClear] {
            let owner = controller.broker.lock().await;
            let mut admission = Box::pin(acquire_request_broker(&controller, method));
            assert!(futures_util::poll!(admission.as_mut()).is_pending());
            assert_eq!(
                backend.request.lock().await.len(),
                if method == HarnessMethod::GoalPause {
                    1
                } else {
                    2
                }
            );
            drop(owner);
            let admitted = tokio::time::timeout(Duration::from_secs(1), admission).await??;
            assert!(controller.broker.try_lock().is_err());
            drop(admitted);
        }
        assert_eq!(
            backend
                .request
                .lock()
                .await
                .iter()
                .map(|(_, command)| command.as_str())
                .collect::<Vec<_>>(),
            vec!["pause", "clear"]
        );
        Ok(())
    }

    #[tokio::test]
    async fn goal_control_acquires_broker_when_no_provider_reader_starts() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let broker = HarnessBroker::initialize(initialize(&fixture, "mock"))?;
        let mut controller = SessionController::new(broker);
        let backend = crate::backend::codex::CodexBackend::new_with_permission_coordinator(
            vec!["codex".into(), "app-server".into()],
            crate::backend::approval::PermissionCoordinator::transient(fixture.path())?,
            Arc::new(crate::trace::TraceStore::open(fixture.path())?),
                    None,
        )?;
        Arc::get_mut(&mut controller).unwrap().backend = Arc::new(backend);
        for method in [HarnessMethod::GoalPause, HarnessMethod::GoalClear] {
            let owner = controller.broker.lock().await;
            let mut admission = Box::pin(acquire_request_broker(&controller, method));
            assert!(futures_util::poll!(admission.as_mut()).is_pending());
            drop(owner);
            let admitted = tokio::time::timeout(Duration::from_secs(1), admission).await??;
            assert!(controller.broker.try_lock().is_err());
            drop(admitted);
        }
        Ok(())
    }

    fn service() -> HarnessService {
        let diff = DiffEngine::new(CacheLimits::default(), 1);
        let syntax = SyntaxEngine::new(
            diff.analysis_pool(),
        );
        HarnessService::new(Arc::new(RepositoryStore::default()), diff, syntax)
    }

    fn initialize(fixture: &tempfile::TempDir, command: &str) -> InitializeRequest {
        serde_json::from_value(json!({
            "workspace":fixture.path(),
            "data_root":fixture.path().join("data"),
            "client_id":"service-test",
            "backend":{"kind":"mock","command":[command]},
        }))
        .unwrap()
    }
}

#[cfg(test)]
mod target_test {
    use super::*;

    #[test]
    fn explicit_invalid_target_never_becomes_main_agent_input() {
        assert!(validate_root_control(None).is_ok());
        assert!(validate_root_control(Some(&Value::Null)).is_ok());
        for value in [
            json!({}),
            json!({"thread_id":"child"}),
            json!({"thread_id":"child","turn_id":" "}),
            json!({"thread_id":"child","turn_id":"turn"}),
        ] {
            assert!(validate_root_control(Some(&value)).is_err());
        }
    }
}
