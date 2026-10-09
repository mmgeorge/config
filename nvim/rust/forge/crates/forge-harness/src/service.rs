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
                let mut source = {
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
            HarnessMethod::PlanAcceptanceBegin | HarnessMethod::PlanRequestChanges | HarnessMethod::PlanEntityRename
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
                    controller.permission.cancel_all(None).await?;
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
    execution_admission: Arc<Semaphore>,
    control_admission: Arc<Semaphore>,
    plan_review: Arc<crate::plan::review_document::PlanReviewStore>,
    broker: Mutex<HarnessBroker>,
    presentation: Arc<std::sync::Mutex<crate::buffer::session::SessionPresentation>>,
    cancellation: Arc<TurnCancellation>,
    backend: Arc<dyn crate::backend::Backend>,
    catalog_request: RwLock<crate::backend::BackendCatalogRequest>,
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
            execution_admission: Arc::new(Semaphore::new(1)),
            control_admission: Arc::new(Semaphore::new(1)),
            plan_review: Arc::clone(&broker.plan_review),
            presentation: broker.presentation(),
            cancellation: broker.turn_cancellation(),
            backend: broker.backend_handle(),
            catalog_request: RwLock::new(catalog_request),
            mcp_discovery: Mutex::new(crate::backend::mcp::McpDiscovery::default()),
            permission: broker.permission_coordinator(),
            broker: Mutex::new(broker),
        })
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
            let (event_name, payload) = if event.kind == "timeline_patch" {
                ("timeline_patch".to_owned(), event.data)
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
            Request { id: 0, method: "goal.continue".into(), params: json!({}) }, message_sink).await {
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
    let pending = prior.as_ref().filter(|operation| matches!(operation.state.as_str(), "admitted" | "accepted" | "stopping" | "running"));
    if matches!(action, "permission" | "configure" | "compact")
        && let Some(pending) = pending.filter(|operation| matches!(operation.action["action"].as_str(), Some("plan" | "execute" | "goal" | "fork" | "attach_plan" | "resume"))) {
        ensure!(task.as_ref().is_some_and(|task| task.operation_id.as_deref() == Some(&pending.id)),
            "Task selection is still being admitted. Retry this setting after it starts, or pause the pending task");
    }
    let running_intent = pending.or_else(|| prior.as_ref().filter(|operation| !operation.resume_after_transition))
        .map(|operation| operation.resume_after_transition).unwrap_or_else(||
        task.as_ref().is_some_and(|task| task.status == crate::task::TaskStatus::Running)
            || (task.is_none() && controller.execution_admission.available_permits() == 0));
    let resume_after_transition = if matches!(action, "pause" | "clear") { false }
        else if action == "permission" && request.params["mode"] == "read"
            && task.as_ref().is_some_and(|task| task.kind != crate::task::TaskKind::Plan) { false }
        else if matches!(action, "plan" | "execute" | "goal" | "fork" | "attach_plan" | "resume") { true }
        else { running_intent };
    let operation = crate::task::TaskOperation {
        id, session_id: session_id.to_owned(), action: request.params.clone(), state: "admitted".into(), error: None,
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
    sink.send_control_wait(Message::Response(Response::success(request.id, json!({"id":operation.id,"state":operation.state}))?)).await?;
    let result = execute_task_transition(&registry, &controller, &mut operation, &mut store, sink).await;
    match result {
        Ok(()) => if operation.state != "superseded" { operation.state = "completed".into(); },
        Err(error) => { operation.state = "failed".into(); operation.error = Some(format!("{error:#}")); }
    }
    if let Err(error) = store.save_task_operation(&operation) {
        operation.state = "outcome_unknown".into();
        operation.error = Some(format!("Task outcome could not be persisted: {error:#}. Effects may have occurred"));
    }
    sink.send_control_wait(Message::Event(SessionEvent { session_id, event: "task_operation".into(), payload: operation.view() })).await?;
    Ok(())
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
    let execution = match Arc::clone(&controller.execution_admission).try_acquire_owned() {
        Ok(execution) => execution,
        Err(tokio::sync::TryAcquireError::Closed) => anyhow::bail!("session execution admission is closed"),
        Err(tokio::sync::TryAcquireError::NoPermits) => {
            controller.cancellation.begin_cleanup(false, false)?;
            controller.permission.cancel_all(None).await?;
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
        let result = dispatch_task_attempt(&mut broker, &operation.session_id, request, sink).await;
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
    outcome?;
    drop(execution);
    let _ = registry;
    Ok(())
}

async fn dispatch_task_attempt(broker: &mut HarnessBroker, session_id: &str, request: Request, sink: &MessageSender) -> Result<bool> {
    let (event_sink, mut event_stream) = crate::backend::events::channel();
    let output = sink.clone();
    let routed_session = session_id.to_owned();
    let forwarder = tokio::spawn(async move {
        while let Some(event) = event_stream.recv().await? {
            let (name, payload) = if event.kind == "timeline_patch" { ("timeline_patch", event.data) }
                else { ("backend_event", serde_json::to_value(event)?) };
            output.send_event(SessionEvent { session_id: routed_session.clone(), event: name.into(), payload }).await?;
        }
        Ok::<(), anyhow::Error>(())
    });
    let result = broker.dispatch_stream(request, event_sink).await;
    forwarder.await??;
    let continuing = result.event.iter().any(|event| event.event == "goal_continue_requested");
    for event in result.event { sink.send_event(event).await?; }
    if let Some(failure) = result.response.error() { anyhow::bail!("{}", failure.message); }
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

async fn route_control_request(
    controller: &SessionController,
    request: &Request,
    method: HarnessMethod,
    message_sink: &MessageSender,
) -> Result<bool> {
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
            if let Some(target) = parse_steer_target(request.params.get("target"))? {
                controller.backend.interrupt_target(target).await?;
            } else {
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
                controller.permission.cancel_all(None).await?;
                let cleanup = controller
                    .backend
                    .cleanup_execution(&catalog_request.harness_session_id)
                    .await;
                if let Err(error) = cleanup {
                    controller.cancellation.fail_cleanup(&error);
                    return Err(error);
                }
                controller.cancellation.request(restore_prompt);
                controller.permission.cancel_all(None).await?;
            }
            Some(Response::success(
                request.id,
                json!({ "cancel_requested": true }),
            )?)
        }
        HarnessMethod::TurnRestart => {
            controller.cancellation.begin_cleanup(true, false)?;
            controller.permission.cancel_all(None).await?;
            if let Err(error) = controller
                .backend
                .cleanup_execution(&catalog_request.harness_session_id)
                .await
            {
                controller.cancellation.fail_cleanup(&error);
                return Err(error);
            }
            controller.cancellation.request_restart();
            controller.permission.cancel_all(None).await?;
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
            let delivery = route_steering(
                controller.backend.as_ref(),
                &catalog_request.harness_session_id,
                text,
                parse_steer_target(request.params.get("target"))?,
            )
            .await?;
            Some(Response::success(
                request.id,
                json!({ "steered": true, "delivery": delivery }),
            )?)
        }
        HarnessMethod::ApprovalResolve => {
            let approval_id = request
                .params
                .get("approval_id")
                .and_then(Value::as_str)
                .context("approval.resolve requires approval_id")?;
            let choice_id = request
                .params
                .get("choice_id")
                .and_then(Value::as_str)
                .context("approval.resolve requires choice_id")?;
            let approval = controller
                .permission
                .resolve(approval_id, choice_id, None)
                .await?;
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
        HarnessMethod::BackendSkillsSetEnabled => {
            let name = request
                .params
                .get("name")
                .and_then(Value::as_str)
                .context("backend.skills.set_enabled requires name")?;
            let enabled = request
                .params
                .get("enabled")
                .and_then(Value::as_bool)
                .context("backend.skills.set_enabled requires enabled")?;
            Some(Response::success(
                request.id,
                controller
                    .backend
                    .set_skill_enabled(catalog_request.clone(), name, enabled)
                    .await?,
            )?)
        }
        HarnessMethod::BackendMcp => {
            let server_list = controller
                .mcp_discovery
                .lock()
                .await
                .snapshot(controller.backend.clone(), catalog_request.clone())
                .await?;
            Some(Response::success(request.id, server_list)?)
        }
        HarnessMethod::BackendMcpSetEnabled => {
            ensure!(controller.broker.try_lock().is_ok(), "Pause the current task before changing MCP configuration");
            let name = request
                .params
                .get("name")
                .and_then(Value::as_str)
                .context("backend.mcp.set_enabled requires name")?;
            let enabled = request
                .params
                .get("enabled")
                .and_then(Value::as_bool)
                .context("backend.mcp.set_enabled requires enabled")?;
            let capability = controller.backend.descriptor().capability.catalog;
            let interrupted = !capability.live_mcp_mutation
                && controller
                    .backend
                    .has_active_turn(&catalog_request.harness_session_id)
                    .await;
            if interrupted {
                controller.cancellation.begin_cleanup(true, false)?;
                controller.permission.cancel_all(None).await?;
                if let Err(error) = controller
                    .backend
                    .cleanup_execution(&catalog_request.harness_session_id)
                    .await
                {
                    controller.cancellation.fail_cleanup(&error);
                    return Err(error);
                }
                controller.cancellation.request_restart();
                controller.permission.cancel_all(None).await?;
            }
            let mut mutation = tokio::time::timeout(
                Duration::from_secs(30),
                controller
                    .backend
                    .set_mcp_enabled(catalog_request, name, enabled),
            )
            .await
            .context("MCP startup did not finish within 30 seconds")??;
            mutation.restart_required |= interrupted;
            *controller.mcp_discovery.lock().await = Default::default();
            Some(Response::success(request.id, mutation)?)
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

/// Reject malformed explicit targets instead of routing their input to the main agent.
/// Route user input through the provider's declared child-control boundary.
async fn route_steering(
    backend: &dyn crate::backend::Backend,
    session_id: &str,
    text: String,
    target: Option<crate::backend::SteerTarget>,
) -> Result<crate::agent::AgentControlMode> {
    use crate::agent::AgentControlMode;
    let Some(target) = target else {
        backend.steer_session(session_id, text).await?;
        return Ok(AgentControlMode::Direct);
    };
    let delivery = backend.descriptor().capability.agent.input;
    match delivery {
        AgentControlMode::Direct | AgentControlMode::ParentMediated => {
            backend.steer_target(session_id, text, target).await?;
        }
        AgentControlMode::Unsupported => {
            anyhow::bail!("the provider does not support child-agent input")
        }
    }
    Ok(delivery)
}

fn parse_steer_target(value: Option<&Value>) -> Result<Option<crate::backend::SteerTarget>> {
    let Some(value) = value.filter(|value| !value.is_null()) else {
        return Ok(None);
    };
    let field = |name| {
        value
            .get(name)
            .and_then(Value::as_str)
            .filter(|text| !text.trim().is_empty())
            .map(str::to_owned)
            .with_context(|| format!("target requires non-empty {name}"))
    };
    Ok(Some(crate::backend::SteerTarget {
        thread_id: field("thread_id")?,
        turn_id: field("turn_id")?,
    }))
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
        mode: crate::agent::AgentControlMode,
        request: Mutex<Vec<(String, String)>>,
    }

    #[async_trait::async_trait]
    impl crate::backend::Backend for SteeringBackend {
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Steering test".into(),
                capability: crate::backend::BackendCapability {
                    agent: crate::agent::AgentCapability {
                        input: self.mode.clone(),
                        ..Default::default()
                    },
                    ..Default::default()
                },
            }
        }

        async fn prompt_stream(
            &self,
            _: crate::backend::BackendRequest,
            _: Option<crate::backend::BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            anyhow::bail!("steering must not start another provider request")
        }

        async fn steer_session(&self, session_id: &str, text: String) -> Result<()> {
            self.request.lock().await.push((session_id.into(), text));
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

        async fn steer_target(
            &self,
            session_id: &str,
            text: String,
            target: crate::backend::SteerTarget,
        ) -> Result<()> {
            assert_eq!(target.thread_id, "child");
            assert_eq!(target.turn_id, "turn");
            self.request.lock().await.push((session_id.into(), text));
            Ok(())
        }
    }

    #[tokio::test]
    async fn child_input_uses_declared_delivery_and_the_owning_parent_session() {
        use crate::agent::AgentControlMode;
        let target = crate::backend::SteerTarget {
            thread_id: "child".into(),
            turn_id: "turn".into(),
        };
        let correction = "Keep \"quoted\" input\nand newlines";
        for mode in [
            AgentControlMode::ParentMediated,
            AgentControlMode::Direct,
            AgentControlMode::Unsupported,
        ] {
            let backend = SteeringBackend {
                mode: mode.clone(),
                request: Mutex::new(Vec::new()),
            };
            let result = route_steering(
                &backend,
                "parent-session",
                correction.into(),
                Some(target.clone()),
            )
            .await;
            let request = backend.request.lock().await;
            if mode == AgentControlMode::Unsupported {
                assert!(result.is_err());
                assert!(request.is_empty());
                continue;
            }
            assert_eq!(result.unwrap(), mode);
            assert_eq!(request.len(), 1);
            assert_eq!(request[0], ("parent-session".into(), correction.into()));
        }
        let backend = SteeringBackend {
            mode: AgentControlMode::Unsupported,
            request: Mutex::new(Vec::new()),
        };
        assert_eq!(
            route_steering(&backend, "parent-session", correction.into(), None)
                .await
                .unwrap(),
            AgentControlMode::Direct
        );
        assert_eq!(
            backend.request.lock().await[0],
            ("parent-session".into(), correction.into())
        );
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
            mode: crate::agent::AgentControlMode::Unsupported, request: Mutex::new(Vec::new()),
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
            forge_diff::syntax::SyntaxLimits::default(),
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
            mode: crate::agent::AgentControlMode::Unsupported,
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
            forge_diff::syntax::SyntaxLimits::default(),
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
        assert!(parse_steer_target(None).unwrap().is_none());
        assert!(parse_steer_target(Some(&Value::Null)).unwrap().is_none());
        for value in [
            json!({}),
            json!({"thread_id":"child"}),
            json!({"thread_id":"child","turn_id":" "}),
        ] {
            assert!(parse_steer_target(Some(&value)).is_err());
        }
        let target = parse_steer_target(Some(&json!({"thread_id":"child","turn_id":"turn"})))
            .unwrap()
            .unwrap();
        assert_eq!(target.thread_id, "child");
        assert_eq!(target.turn_id, "turn");
    }
}
