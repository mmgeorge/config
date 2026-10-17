use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
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
use crate::session::{ProviderForkState, SessionLeaseConflict, SessionStore};
use crate::storage::SqliteStore;

/// Defines provider settings for one-shot generation without a conversation lease.
#[derive(Clone, Debug, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TextGenerationConfig {
    pub backend: crate::backend::BackendLaunch,
    pub model: String,
    pub data_root: PathBuf,
}

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
    /// Generate isolated text from provider settings without resolving a conversation controller.
    pub async fn generate_text(
        &self,
        config: TextGenerationConfig,
        workspace: &str,
        purpose: crate::backend::TextGeneration,
        input: &str,
    ) -> Result<String> {
        let _activity = self.admit().await?;
        let mut model = config.model;
        if model == "default" && config.data_root.join("harness.sqlite3").is_file() {
            if let Some(preference) = SqliteStore::open(&config.data_root)?
                .load_preference(workspace, &config.backend.kind)? {
                model = preference.model;
            }
        }
        ensure!(!model.is_empty() && model.len() <= 256, "invalid generation model identity");
        let temporary = tempfile::tempdir()?;
        let permission_store = crate::permissions::store::PermissionStore::load(
            temporary.path().join("permissions.json"), workspace)?;
        let permission = Arc::new(crate::backend::approval::PermissionCoordinator::new(
            Arc::new(std::sync::RwLock::new(permission_store))));
        let trace = Arc::new(crate::trace::TraceStore::open(&config.data_root)?);
        let backend = crate::backend::build(config.backend, permission, trace, config.data_root)?;
        let request = crate::backend::BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: format!("generation-{}", uuid::Uuid::new_v4()),
            workspace: workspace.to_owned(),
            access: backend.descriptor().kind.resolve_access(crate::session::AccessPolicy::default()),
            execution_mode: crate::session::PermissionMode::Read,
            backend_session_id: None,
        };
        generate_isolated_text(backend.as_ref(), request, purpose, &model, input).await
    }

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
                    if let Some(revision) = revision {
                        let source = controller.observer.capture_plan_revision(plan_id, *revision)?;
                        ensure!(
                            crate::plan::digest(&serde_json::to_vec(&source.document)?) == *digest,
                            "plan revision changed before opening"
                        );
                        source
                    } else {
                        controller.observer.capture_plan_review(plan_id, digest)?
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
                analyze_plan_source(&mut source, &self.syntax, || Ok(admission.check()?), None).await?;
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
                let preview_trace = (input.action == "rename_entity").then(|| crate::plan::edit_trace::PlanEditTrace::new(
                    registry.runtime.trace(), &session_id, "plan.rename.preview",
                    &json!({"operation_id":format!("preview:{}:{}", input.document.0, input.sequence.0),
                        "document":input.document,"expected_version":input.revision})));
                let preview = preview_trace.as_ref().map(|trace| trace.stage("preview.total"));
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
                if let Some(stage) = preview { stage.complete(json!({"count":result.get("rename").and_then(|rename|rename.get("preview")).and_then(Value::as_array).map_or(0,Vec::len)})); }
                if let Some(stage) = stage {
                    stage.complete(json!({}));
                }
                let result = serde_json::to_value(result)?;
                if let Some(stage) = total {
                    stage.complete(json!({}));
                }
                return Ok(result);
            }
            crate::buffer::session::PresentationRequest::PlanDefinition { input, source, target } => {
                return controller.plan_review.definition(input, source, target);
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
        registry.runtime.lsp.attach(sink.clone());
        let session_id = session_id.unwrap_or_else(|| registry.initial_session_id.clone());
        let mut prompt_admission = None;
        let edit_trace = if matches!(method, HarnessMethod::PlanEntityRename | HarnessMethod::PlanTestsDelete | HarnessMethod::PlanChecksUpdate) {
            let params = request.params.as_object_mut().context("plan edit requires an object")?;
            params.entry("operation_id").or_insert_with(|| json!(format!("request:{}", request.id)));
            Some(crate::plan::edit_trace::PlanEditTrace::new(registry.runtime.trace(), &session_id, &request.method, &request.params))
        } else { None };
        let edit_total = edit_trace.as_ref().map(|trace| trace.stage("service.total"));
        let capture_stage = edit_trace.as_ref().map(|trace| trace.stage("service.capture_submission"));
        if matches!(
            method,
            HarnessMethod::PlanAcceptanceBegin | HarnessMethod::PlanRequestChanges | HarnessMethod::PlanCancel | HarnessMethod::PlanEntityRename | HarnessMethod::PlanTestsDelete | HarnessMethod::PlanChecksUpdate
        ) {
            let params = request
                .params
                .as_object_mut()
                .context("plan review submission requires an object")?;
            params.remove("review_document");
            if let Some(review) = params.remove("review") {
                let input = serde_json::from_value::<forge_buffer::input::DocumentInput>(review)?;
                if matches!(method, HarnessMethod::PlanTestsDelete | HarnessMethod::PlanChecksUpdate | HarnessMethod::PlanEntityRename) {
                    params.insert("review_document".into(), serde_json::to_value(&input.document)?);
                }
                let controller = registry.resolve(&session_id).await?;
                if let Some(annotation) = params.remove("draft_annotation") {
                    let digest = params
                        .remove("draft_source_digest")
                        .and_then(|value| value.as_str().map(str::to_owned))
                        .context("plan annotation capture requires its saved source digest")?;
                    let annotation_stage = edit_trace.as_ref().map(|trace| trace.stage("service.save_draft_annotations"));
                    controller.plan_review.save_annotations(
                        &input.document,
                        &digest,
                        serde_json::from_value(annotation)?,
                    )?;
                    if let Some(stage) = annotation_stage { stage.complete(json!({})); }
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
        if let Some(stage) = capture_stage { stage.complete(json!({})); }
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
        let result = Box::pin(route_request(registry, session_id, request, method, sink, &self.syntax)).await;
        if let Some(stage) = edit_total { stage.complete(json!({"status":if result.is_ok() { "completed" } else { "failed" }})); }
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
                registry.runtime.lsp.close();
                let controller_list: Vec<_> = registry
                    .controller_by_id
                    .read()
                    .await
                    .values()
                    .cloned()
                    .collect();
                for controller in controller_list {
                    if let Some(task) = controller.timeout_task.lock().await.take() { task.abort(); }
                    controller.plan_review.close_all()?;
                    *controller.mcp_discovery.lock().await = Default::default();
                    controller.cancellation.request(false);
                    if let Some(id) = controller.checks.active()? {
                        controller.checks.cancel(&id)?;
                        while controller.checks.active()?.is_some() {
                            controller.checks.status(&id,250).await?;
                        }
                    }
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
    observer: Arc<crate::broker::BrokerObserver>,
    timeout_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    agent_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    agent_coordinator: Arc<crate::agent::AgentCoordinator>,
    checks: Arc<crate::checks::CheckRunner>,
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
    mcp_revision: AtomicU64,
    mcp_discovery: Mutex<(u64, crate::backend::mcp::McpDiscovery)>,
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
            observer: broker.observer(),
            timeout_task: Mutex::new(None),
            agent_task: Mutex::new(None),
            agent_coordinator: broker.agent_coordinator(),
            checks: broker.checks(),
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
            mcp_revision: AtomicU64::new(0),
            mcp_discovery: Mutex::new((0, crate::backend::mcp::McpDiscovery::default())),
            permission: broker.permission_coordinator(),
            broker: Mutex::new(broker),
        })
    }

    async fn attach_compaction(self: &Arc<Self>, message_sink: &MessageSender) -> Result<()> {
        self.attach_timeout(message_sink).await;
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

    async fn attach_timeout(self: &Arc<Self>, message_sink: &MessageSender) {
        let mut task = self.timeout_task.lock().await;
        if task.is_some() { return; }
        let controller = Arc::downgrade(self);
        let message_sink = message_sink.clone();
        *task = Some(tokio::spawn(async move {
            let mut active_deadline = None;
            let mut interval = tokio::time::interval(Duration::from_millis(250));
            interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
            loop {
                interval.tick().await;
                let Some(controller) = controller.upgrade() else { return; };
                let Ok(_admission) = controller.execution_admission.try_acquire() else { continue; };
                let Ok(mut broker) = controller.broker.try_lock() else { continue; };
                let session_id = controller.catalog_request.read().await.harness_session_id.clone();
                let result = async {
                    if let Some(request) = broker.expire_input(&mut active_deadline)? {
                        if request.method == "question.continue" {
                            message_sink.send_event(SessionEvent { session_id: session_id.clone(), event: "question_answered".into(),
                                payload: json!({"resolution_source":"timeout"}) }).await?;
                        }
                        let (root, owner, client_id) = broker.lease_identity();
                        let (stop, stopped) = tokio::sync::oneshot::channel();
                        let heartbeat = tokio::spawn(run_lease_heartbeat(root, owner, client_id, stopped));
                        let dispatch = async {
                            let mut continuing = dispatch_task_attempt(&mut broker, &session_id, request, &message_sink, Arc::clone(&controller.catalog_request)).await?;
                            while continuing {
                                continuing = dispatch_task_attempt(&mut broker, &session_id,
                                    Request { id: 0, method: "goal.continue".into(), params: json!({}) },
                                    &message_sink, Arc::clone(&controller.catalog_request)).await?;
                            }
                            Ok::<(), anyhow::Error>(())
                        }.await;
                        let _ = stop.send(());
                        heartbeat.await.context("input timeout lease task failed")?;
                        dispatch?;
                    }
                    Ok::<(), anyhow::Error>(())
                }.await;
                drop(broker);
                match result {
                    Ok(()) => {},
                    Err(error) if error.is::<crate::broker::TurnCancelled>() => {},
                    Err(error) => {
                        let _ = message_sink.send_event(SessionEvent { session_id, event: "backend_event".into(),
                            payload: json!({"kind":"error","text":format!("Input timeout failed: {error:#}")}) }).await;
                    }
                }
            }
        }));
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
        if let Some(task) = self.timeout_task.get_mut().take() { task.abort(); }
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

async fn generate_isolated_text(
    backend: &dyn crate::backend::Backend,
    request: crate::backend::BackendCatalogRequest,
    purpose: crate::backend::TextGeneration,
    model: &str,
    input: &str,
) -> Result<String> {
    let provider = backend.descriptor().label;
    let result = backend.generate_text(request, purpose, model, input).await
        .with_context(|| format!("{provider} {} generation failed", purpose.name()));
    let cleanup = tokio::time::timeout(Duration::from_secs(8), backend.shutdown()).await
        .context("Isolated provider shutdown timed out").and_then(|result| result);
    match (result, cleanup) {
        (Err(failure), Err(cleanup)) => Err(failure.context(format!("Isolated provider cleanup failed: {cleanup:#}"))),
        (Err(failure), _) | (_, Err(failure)) => Err(failure),
        (Ok(text), Ok(())) => purpose.validate(&text),
    }
}

async fn analyze_plan_source(
    source: &mut crate::plan::review_source::PlanReviewSource,
    syntax: &Arc<SyntaxEngine>,
    check: impl Fn() -> Result<()> + Send + Sync,
    trace: Option<&crate::plan::edit_trace::PlanEditTrace>,
) -> Result<()> {
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
                let stage = trace.map(|trace| trace.stage(&format!("syntax.{side}.{path}")));
                if source.declaration_syntax.contains_key(&(path.clone(), side.into())) {
                    if let Some(stage) = stage { stage.complete(json!({"cached":true,"source_bytes":text.len()})); }
                    continue;
                }
                let calls = if side == "baseline" { &design.baseline_calls } else { &design.proposed_calls };
                let presentation = crate::plan::calls::present(&path, text,
                    calls.get(&path).map(Vec::as_slice).unwrap_or_default())?;
                check()?;
                let Some(language) =
                    forge_diff::syntax::DeclarationOverview::language(&path)
                else {
                    if let Some(stage) = stage { stage.complete(json!({"status":"unsupported"})); }
                    continue;
                };
                let handle = syntax
                    .analyze(SyntaxRequest {
                        source: forge_diff::source::SourceVersion::new(
                            presentation.plain.as_bytes().to_vec(),
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
                check()?;
                let algorithm = if let Some(algorithm) = presentation.algorithm_source() {
                    Some(syntax.analyze(SyntaxRequest {
                        source: forge_diff::source::SourceVersion::new(algorithm.into_bytes(), forge_diff::source::Representation::DisplayOnly)?,
                        language: SyntaxLanguage::Algorithm,
                        priority: forge_diff::workers::WorkPriority::Visible,
                        deadline: Some(Instant::now() + Duration::from_secs(10)),
                    }).await.map_err(|error| anyhow::anyhow!("Algorithm syntax analysis failed: {error:?}"))?)
                } else { None };
                check()?;
                source
                    .declaration_syntax
                    .insert((path.clone(), side.into()), crate::plan::review_source::ReviewSyntax { declaration: handle, algorithm });
                if let Some(stage) = stage { stage.complete(json!({"cached":false,"source_bytes":text.len()})); }
            }
        }
    } else {
        source.syntax = Some(
            syntax
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
    Ok(())
}

/// Forwards check lifecycle patches while retaining source-work exclusion until runner collection.
fn checks_event_sink(output: MessageSender, session_id: String, admission: Option<OwnedSemaphorePermit>) -> crate::backend::BackendEventSink {
    let (sink, mut stream) = crate::backend::events::channel();
    tokio::spawn(async move {
        let _admission = admission;
        while let Ok(Some(event)) = stream.recv().await {
            let (event_name,payload) = if event.kind == "document_changed" {("document_changed".into(),event.data)}
                else {("backend_event".into(),serde_json::to_value(event).unwrap_or(Value::Null))};
            if output.send_event(SessionEvent {session_id:session_id.clone(),event:event_name,payload}).await.is_err() {break;}
        }
    });
    sink
}

async fn route_request(
    registry: Arc<SessionControllerRegistry>,
    session_id: String,
    request: Request,
    method: HarnessMethod,
    message_sink: &MessageSender,
    syntax: &Arc<SyntaxEngine>,
) -> Result<()> {
    if matches!(
        method,
        HarnessMethod::LspAuthorize | HarnessMethod::LspRespond | HarnessMethod::LspAnalyze
    ) {
        let id = request.params["id"]
            .as_str()
            .context("LSP response requires id")?;
        let result = if method == HarnessMethod::LspAuthorize {
            registry
                .runtime
                .lsp
                .authorize(
                    &session_id,
                    id,
                    serde_json::from_value(request.params["paths"].clone())?,
                )
                .await?
        } else if method == HarnessMethod::LspAnalyze {
            registry
                .runtime
                .lsp
                  .analyze_part(
                    &session_id,
                    id,
                    request.params["path"]
                        .as_str()
                        .context("analysis requires path")?,
                    request.params["source"]
                        .as_str()
                        .context("analysis requires source")?
                        .to_owned(),
                      match (request.params.get("source_offset"), request.params.get("source_total")) {
                          (None, None) => None,
                          (Some(offset), Some(total)) => Some((
                              usize::try_from(offset.as_u64().context("invalid source offset")?)?,
                              usize::try_from(total.as_u64().context("invalid source size")?)?,
                          )),
                          _ => anyhow::bail!("source transfer requires offset and total"),
                      },
                      syntax,
                )
                .await?
        } else {
            registry.runtime.lsp.respond(&session_id, id, request.params["result"].clone(),
                request.params["failure"].as_str().map(str::to_owned))?;
            json!({"accepted":true})
        };
        message_sink.send_response(Response::success(request.id, result)?).await?;
        return Ok(());
    }
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
    if let Some(value) = controller.observer.query(method, request.params.clone())? {
        message_sink.send_response(Response::success(request.id, value)?).await?;
        return Ok(());
    }
    if method == HarnessMethod::BackendModels {
        let catalog = controller.catalog_request.read().await.clone();
        let selected = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?
            .load_session(&session_id)?.context("model catalog session is missing")?;
        let mut model_list = tokio::time::timeout(Duration::from_secs(30), controller.backend.model_list(crate::backend::BackendRequest {
            execution_owner: catalog.execution_owner,
            harness_session_id: catalog.harness_session_id,
            workspace: catalog.workspace,
            input: crate::backend::BackendInput::from_text(String::new()),
            mode: crate::backend::PromptMode::Chat,
            model: selected.model,
            effort: selected.effort,
            context_window: selected.context_window,
            service_tier: selected.service_tier,
            access: catalog.access,
            execution_mode: catalog.execution_mode,
            backend_session_id: catalog.backend_session_id,
            control_context: None,
        })).await.context("model catalog exceeded 30 seconds")??;
        let preference = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?
            .load_preference(&selected.workspace, &selected.backend)?;
        crate::backend::select_model_settings(&mut model_list, preference.as_ref());
        let event = if let Ok(mut broker) = controller.broker.try_lock() {
            broker.resolve_default_model(&model_list)?
        } else {
            Vec::new()
        };
        for event in event { message_sink.send_event(event).await?; }
        message_sink.send_response(Response::success(request.id, model_list)?).await?;
        return Ok(());
    }
    if method == HarnessMethod::InputTimeoutConfigure {
        let catalog = controller.catalog_request.read().await.clone();
        let mut settings = crate::settings::SettingsStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let snapshot = settings.get(&catalog.workspace, &session_id, &registry.initialize.backend.kind)?;
        let mut desired = settings.layer_settings(crate::settings::SettingScope::Global, &catalog.workspace, &session_id)?;
        ensure!(request.params.as_object().is_some_and(|fields| !fields.is_empty()
            && fields.keys().all(|field| matches!(field.as_str(), "approval_timeout" | "answer_timeout"))),
            "input timeout configuration accepts only approval_timeout and answer_timeout");
        if let Some(value) = request.params.get("approval_timeout") {
            desired.approval_timeout = Some(serde_json::from_value::<crate::session::InputTimeout>(value.clone())?.validate()?);
        }
        if let Some(value) = request.params.get("answer_timeout") {
            desired.answer_timeout = Some(serde_json::from_value::<crate::session::InputTimeout>(value.clone())?.validate()?);
        }
        settings.update(snapshot.revision, crate::settings::SettingScope::Global, &catalog.workspace, &session_id, &desired)?;
        let effective = settings.get(&catalog.workspace, &session_id, &registry.initialize.backend.kind)?.desired;
        let approval = effective.approval_timeout.unwrap_or_default();
        let answer = effective.answer_timeout.unwrap_or_default();
        controller.permission.configure_input_timeouts(&session_id, approval, answer)?;
        message_sink.send_response(Response::success(request.id, json!({"approval_timeout":approval,"answer_timeout":answer}))?).await?;
        return Ok(());
    }
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
        controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
        message_sink.send_response(Response::success(request.id, result)?).await?;
        return Ok(());
    }
    if method == HarnessMethod::BackendAuthenticate {
        let method = request.params["method"].as_str().context("authentication method is required")?.to_owned();
        controller.backend.authenticate(controller.catalog_request.read().await.clone(), method).await?;
        controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
        message_sink.send_response(Response::success(request.id, json!({"authenticated":true}))?).await?;
        return Ok(());
    }
    if method == HarnessMethod::BackendCapabilities {
        let manifest = tokio::time::timeout(Duration::from_secs(30), controller.backend.capabilities(controller.catalog_request.read().await.clone()))
            .await.context("capability discovery exceeded 30 seconds")??;
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
    if matches!(method, HarnessMethod::ChecksList | HarnessMethod::ChecksStatus | HarnessMethod::ChecksCancel | HarnessMethod::ChecksOverride | HarnessMethod::ChecksRun | HarnessMethod::ChecksSaveDefaults) {
        if matches!(method, HarnessMethod::ChecksList | HarnessMethod::ChecksRun | HarnessMethod::ChecksOverride) {
            controller.checks.refresh_changes().await?;
        }
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let session = store.load_session(&session_id)?.context("checks session is missing")?;
        let task = store.list_task(&session_id)?.into_iter().find(|task| Some(&task.id) == session.current_task_id.as_ref() && !task.status.terminal());
        let task_id = task.as_ref().map(|task|task.id.as_str());
        let result = match method {
            HarnessMethod::ChecksList => controller.checks.catalog_preview(task_id, session.shell).await?,
            HarnessMethod::ChecksStatus => serde_json::to_value(controller.checks.status(request.params["run_id"].as_str().context("run_id required")?,request.params["wait_ms"].as_u64().unwrap_or(0)).await?)?,
            HarnessMethod::ChecksCancel => {
                let id = request.params["run_id"].as_str().context("run_id required")?;
                controller.checks.cancel(id)?; json!({"run_id":id,"cancellation_requested":true})
            },
            HarnessMethod::ChecksOverride => controller.checks.set_overrides(task_id,serde_json::from_value(request.params.clone())?)?,
            HarnessMethod::ChecksRun => {
                ensure!(task.as_ref().is_none_or(|task|task.kind != crate::task::TaskKind::Plan)
                    && controller.agent_coordinator.active_count() == 0, "checks cannot race planning or writing agents");
                let admission = Arc::clone(&controller.execution_admission).try_acquire_owned().context("finish active source work before checks")?;
                let run = controller.checks.prepare_request(serde_json::from_value(request.params.clone())?,task_id.map(str::to_owned),session.shell,"User requested".into())?;
                let sink = checks_event_sink(message_sink.clone(),session_id.clone(),Some(admission));
                json!({"run_id":controller.checks.start(run,Some(sink),None).await?})
            },
            HarnessMethod::ChecksSaveDefaults => {
                let sink = checks_event_sink(message_sink.clone(),session_id.clone(),None);
                controller.checks.save_defaults(task_id,Some(&sink)).await?
            },
            _ => unreachable!(),
        };
        message_sink.send_response(Response::success(request.id,result)?).await?;
        return Ok(());
    }
    if method == HarnessMethod::AgentModels {
        let provider = request.params["provider"].as_str().context("child model catalog requires provider")?;
        let provider = crate::backend::BackendKind::parse(provider)?;
        let catalog = controller.catalog_request.read().await.clone();
        let models = tokio::time::timeout(Duration::from_secs(30), controller.agent_backend_factory.catalog(provider,crate::backend::BackendRequest {
            execution_owner:catalog.execution_owner,harness_session_id:catalog.harness_session_id,workspace:catalog.workspace,
            input:crate::backend::BackendInput::from_text(String::new()),mode:crate::backend::PromptMode::Chat,
            model:"default".into(),effort:"default".into(),context_window:None,service_tier:Default::default(),
            access:catalog.access,execution_mode:catalog.execution_mode,backend_session_id:None,control_context:None,
        })).await.context("child model catalog exceeded 30 seconds")??;
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
    let edit_trace = matches!(method, HarnessMethod::PlanTestsDelete | HarnessMethod::PlanChecksUpdate | HarnessMethod::PlanEntityRename)
        .then(|| crate::plan::edit_trace::PlanEditTrace::new(registry.runtime.trace(), &session_id, &request.method, &request.params));
    if route_control_request(&controller, &request, method, message_sink, syntax, edit_trace.as_ref()).await? {
        return Ok(());
    }

    let (event_sink, mut event_stream) = crate::backend::events::channel();
    let _child_quiescence = if matches!(method, HarnessMethod::GoalPause | HarnessMethod::GoalClear) {
        Some(quiesce_children(&controller).await?)
    } else {None};
    let (mut broker, _admission) = acquire_request_broker(&controller, method).await?;
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
    let mut result = Box::pin(broker.dispatch_stream(request, event_sink)).await;
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
        controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
    }
    if let Some(revision) = operation.action.pointer("/config/settings_revision").and_then(Value::as_u64) {
        crate::settings::SettingsStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?
            .begin_application(&session_id, &operation.id, revision)?;
    }
    retain_configuration_event(&mut store, &operation)?;
    operation.exchange_id = store.list_exchange(&session_id)?.last().map(|exchange| exchange.id.clone());
    sink.send_control_wait(Message::Response(Response::success(request.id, json!({"id":operation.id,"state":operation.state}))?)).await?;
    let result = Box::pin(execute_task_transition(&registry, &controller, &mut operation, &mut store, sink)).await;
    if operation.action["action"] == "configure" {
        controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
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
    let child_quiescence = if changes_child_policy || matches!(action["action"].as_str(),Some("pause" | "clear" | "stop" | "cancel" | "undo" | "verify")) {
        Some(quiesce_children(controller).await?)
    } else {None};
    let execution = match Arc::clone(&controller.execution_admission).try_acquire_owned() {
        Ok(execution) => execution,
        Err(tokio::sync::TryAcquireError::Closed) => anyhow::bail!("session execution admission is closed"),
        Err(tokio::sync::TryAcquireError::NoPermits) => {
            let restart = operation.resume_after_transition
                && matches!(action["action"].as_str(), Some("permission" | "configure" | "compact"));
            let restore_prompt = action["action"] == "pause"
                && action["restore_prompt_if_no_output"].as_bool() == Some(true);
            controller.cancellation.begin_cleanup(restart, restore_prompt)?;
            let approval_owner = crate::backend::ExecutionOwner::root(&controller.catalog_request.read().await.harness_session_id);
                controller.permission.cancel_owner(&approval_owner, None).await?;
            if let Err(error) = controller.backend.cleanup_execution(&operation.session_id).await {
                controller.cancellation.fail_cleanup(&error);
                return Err(error.context("collect current task before transition"));
            }
            controller.cancellation.request(restore_prompt);
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
    // Release child collection after acquiring exclusive execution, before the transition resumes work.
    drop(child_quiescence);
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
    let result = Box::pin(broker.dispatch_stream(request, event_sink)).await;
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
) -> Result<(MutexGuard<'_, HarnessBroker>, Option<OwnedSemaphorePermit>)> {
    // Provider requests and check setup retain admission from their owning request lane.
    let admitted = method.requires_provider_fork();
    let admission = if admitted {
        None
    } else {
        match Arc::clone(&controller.execution_admission).try_acquire_owned() {
            Ok(admission) => Some(admission),
            Err(_) if matches!(method, HarnessMethod::GoalPause | HarnessMethod::GoalClear) => None,
            Err(error) => return Err(error).context(
                "This operation requires a settled session. Use a task transition while execution is active"),
        }
    };
    if admitted || admission.is_some() {
        let broker = tokio::time::timeout(Duration::from_secs(10), controller.broker.lock())
            .await
            .context("Session maintenance did not release the broker within 10 seconds")?;
        return Ok((broker, admission));
    }
    tokio::time::timeout(Duration::from_secs(10), async {
        let broker = controller.broker.lock();
        tokio::pin!(broker);
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
        Ok(broker.await)
    })
    .await
    .context("Provider execution did not release the session within 10 seconds")?
    .map(|broker| (broker, None))
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
    syntax: &Arc<SyntaxEngine>,
    trace: Option<&crate::plan::edit_trace::PlanEditTrace>,
) -> Result<bool> {
    if matches!(method, HarnessMethod::PlanTestsDelete | HarnessMethod::PlanChecksUpdate | HarnessMethod::PlanEntityRename) {
        let dispatch = trace.map(|trace| trace.stage("service.dispatch_edit"));
        let revision_lane = trace.map(|trace| trace.stage("service.revision_lane"));
        let (mut response, events) = if let Some((value, events)) = controller.revision_review.decide(method, request.params.clone()).await? {
            if let Some(stage) = revision_lane { stage.complete(json!({"source":"revision_review"})); }
            (Response::success(request.id, value)?, events)
        } else {
            if let Some(stage) = revision_lane { stage.complete(json!({"source":"broker"})); }
            let wait = trace.map(|trace| trace.stage("service.broker_lock_wait"));
            let (mut broker, _admission) = acquire_request_broker(controller, method).await?;
            if let Some(stage) = wait { stage.complete(json!({})); }
            let result = broker.dispatch(request.clone()).await;
            (result.response, result.event)
        };
        if let Some(stage) = dispatch { stage.complete(json!({"status":if response.result().is_some() { "completed" } else { "failed" }})); }
        let event_stage = trace.map(|trace| trace.stage("service.send_events"));
        let event_count = events.len();
        for event in events { message_sink.send_event(event).await?; }
        if let Some(stage) = event_stage { stage.complete(json!({"count":event_count})); }
        if let (Some(document), Some(mut value)) = (request.params.get("review_document"), response.result().cloned()) {
            let document = serde_json::from_value(document.clone())?;
            let update = async {
                let plan_id = value["plan"]["id"].as_str().context("updated plan identity is missing")?;
                let digest = value["plan"]["review_digest"].as_str().context("updated plan digest is missing")?;
                let capture = trace.map(|trace| trace.stage("service.recapture_review"));
                let mut source = if let Some(source) = controller.revision_review.capture(plan_id, digest, None).await? {
                    source
                } else {
                    let wait = trace.map(|trace| trace.stage("service.recapture_lock_wait"));
                    if let Some(stage) = wait { stage.complete(json!({})); }
                    controller.observer.capture_plan_review(plan_id, digest)?
                };
                if let Some(stage) = capture { stage.complete(json!({"count":source.document.design.as_ref().map_or(0, |design|design.proposed.len())})); }
                let expected_version = crate::plan::edit_trace::measure(trace, "service.reuse_review_caches", || controller.plan_review.prepare_update(&document, &mut source))?;
                let syntax_stage = trace.map(|trace| trace.stage("service.syntax"));
                analyze_plan_source(&mut source, syntax, || Ok(()), trace).await?;
                if let Some(stage) = syntax_stage { stage.complete(json!({"count":source.declaration_syntax.len()})); }
                controller.plan_review.update(&document, source, expected_version, trace)
            }.await;
            match update {
                Ok(update) => value["review"] = update,
                Err(error) => value["review_error"] = Value::String(format!("Plan changed, but the open review could not update: {error:#}")),
            }
            response = crate::plan::edit_trace::measure(trace, "service.build_response", || Ok(Response::success(request.id, value)?))?;
        }
        let send = trace.map(|trace| trace.stage("service.send_response"));
        message_sink.send_response(response).await?;
        if let Some(stage) = send { stage.complete(json!({})); }
        return Ok(true);
    }
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
    if matches!(method, HarnessMethod::PlanAcceptanceBegin | HarnessMethod::PlanRequestChanges | HarnessMethod::PlanCancel)
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
            Some(Response::success(
                request.id,
                json!({ "resolved": true, "approval": approval }),
            )?)
        }
        HarnessMethod::BackendSkills => Some(Response::success(
            request.id,
            tokio::time::timeout(Duration::from_secs(30), controller.backend.skill_list(catalog_request.clone()))
                .await.context("skill discovery exceeded 30 seconds")??,
        )?),
        HarnessMethod::BackendMcp => {
            let revision = controller.mcp_revision.load(Ordering::Acquire);
            let mut discovery = controller.mcp_discovery.lock().await;
            if discovery.0 != revision { *discovery = (revision, Default::default()); }
            let server_list = discovery.1.snapshot(controller.backend.clone(), catalog_request.clone()).await?;
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
    use crate::backend::{
        Backend, BackendCatalogRequest, BackendDescriptor, BackendEventSink, BackendKind,
        BackendOutput, BackendRequest, TextGeneration,
    };
    use forge_diff::cache::CacheLimits;

    #[tokio::test]
    async fn isolated_generation_uses_configured_model_without_a_conversation() -> Result<()> {
        let backend = GenerationBackend {
            request: std::sync::Mutex::new(Vec::new()), shutdown: AtomicBool::new(false), fail: false,
        };
        let request = BackendCatalogRequest {
            execution_owner: None, harness_session_id: "generation-fixture".into(),
            workspace: "commit-workspace".into(), access: Default::default(),
            execution_mode: crate::session::PermissionMode::Read, backend_session_id: None,
        };
        let text = generate_isolated_text(&backend, request.clone(), TextGeneration::CommitMessage,
            "selected-model", "Staged diff:\n+change").await?;
        assert_eq!(text, "fix: preserve drafts\n\nKeep existing text.");
        assert!(backend.shutdown.load(Ordering::Acquire));
        let requests = backend.request.lock().unwrap();
        assert_eq!(requests.len(), 1);
        for (request, purpose, model, input) in requests.iter() {
            assert_eq!(request.harness_session_id, "generation-fixture");
            assert_eq!(request.workspace, "commit-workspace");
            assert!(request.backend_session_id.is_none());
            assert_eq!(*purpose, TextGeneration::CommitMessage);
            assert_eq!(model, "selected-model");
            assert_eq!(input, "Staged diff:\n+change");
        }
        drop(requests);
        let unsupported = SteeringBackend { request: Mutex::new(Vec::new()) };
        let error = generate_isolated_text(&unsupported, request,
            TextGeneration::CommitMessage, "selected-model", "Staged diff:\n+change").await.unwrap_err();
        let diagnostic = format!("{error:#}");
        assert!(diagnostic.contains("commit_message generation failed"), "{diagnostic}");
        assert!(diagnostic.contains("text_generation"), "{diagnostic}");
        let failing = GenerationBackend {
            request: std::sync::Mutex::new(Vec::new()), shutdown: AtomicBool::new(false), fail: true,
        };
        let request = BackendCatalogRequest {
            execution_owner: None, harness_session_id: "generation-failure".into(),
            workspace: "commit-workspace".into(), access: Default::default(),
            execution_mode: crate::session::PermissionMode::Read, backend_session_id: None,
        };
        let failure = generate_isolated_text(&failing, request, TextGeneration::CommitMessage,
            "selected-model", "Staged diff:\n+change").await.unwrap_err();
        assert!(format!("{failure:#}").contains("fixture generation failure"));
        assert!(failing.shutdown.load(Ordering::Acquire));
        Ok(())
    }

    #[tokio::test]
    async fn isolated_generation_never_acquires_another_editors_session_lease() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let owner = service();
        let opened = owner.open_session(1, initialize(&fixture, "mock")).await?;
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let data_root = fixture.path().join("data");
        let store = SqliteStore::open(&data_root)?;
        let before = store.load_session(&session_id)?.unwrap();
        let registry = owner.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let broker = controller.broker.lock().await;
        let generator = service();
        let text = tokio::time::timeout(Duration::from_secs(1), generator.generate_text(
            TextGenerationConfig {
                backend: crate::backend::BackendLaunch { kind: "mock".into(), command: vec![] },
                model: "default".into(), data_root,
            },
            &before.workspace, TextGeneration::CommitMessage, "Staged diff:\n+change",
        )).await??;
        assert_eq!(text, "feat: describe repository changes");
        assert!(generator.registry.lock().await.is_none());
        let after = store.load_session(&session_id)?.unwrap();
        assert_eq!(after.lease_owner, before.lease_owner);
        assert_eq!(after.model, before.model);
        assert_eq!(after.updated_at_ms, before.updated_at_ms);
        drop(broker);
        owner.shutdown(Duration::from_secs(1)).await?;
        generator.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    struct GenerationBackend {
        request: std::sync::Mutex<Vec<(BackendCatalogRequest, TextGeneration, String, String)>>,
        shutdown: AtomicBool,
        fail: bool,
    }

    #[async_trait::async_trait]
    impl Backend for GenerationBackend {
        async fn generate_text(&self, request: BackendCatalogRequest, purpose: TextGeneration,
            model: &str, input: &str) -> Result<String> {
            self.request.lock().unwrap().push((request, purpose, model.into(), input.into()));
            ensure!(!self.fail, "fixture generation failure");
            Ok("```text\nfix: preserve drafts\n\nKeep existing text.\n```".into())
        }
        fn descriptor(&self) -> BackendDescriptor {
            BackendDescriptor { kind: BackendKind::Mock, label: "Generation fixture".into(), capability: Default::default() }
        }
        async fn prompt_stream(&self, _: BackendRequest, _: Option<BackendEventSink>) -> Result<BackendOutput> {
            anyhow::bail!("isolated generation must not submit a conversational turn")
        }
        async fn shutdown(&self) -> Result<()> {
            self.shutdown.store(true, Ordering::Release);
            Ok(())
        }
        async fn cleanup_execution(&self, _: &str) -> Result<()> { Ok(()) }
        async fn cancel_session(&self, _: &str) -> Result<()> { Ok(()) }
        async fn has_active_turn(&self, _: &str) -> bool { false }
        async fn active_session_id_for(&self, _: &str) -> Option<String> { None }
    }

    #[tokio::test]
    async fn plan_check_edits_retain_review_annotations_and_project_defaults() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let config = json!({"checks":{"version":2,"definitions":[{
            "id":"format", "name":"Rust formatting", "category":"format", "area":"Rust", "enabled":true,
            "trigger":{"kind":"any_change"}, "scope":"all", "directory":".", "depends_on":[],
            "all":{"command":"nu", "args":["-c","print checked"]}
        }]}});
        std::fs::write(fixture.path().join(".forge.json"), serde_json::to_vec(&config)?)?;
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await?;
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let plan = {
            let mut broker = controller.broker.lock().await;
            let generated = broker.dispatch(Request { id:2, method:"prompt.submit".into(), params:json!({"text":"/plan add test coverage"}) }).await;
            assert!(generated.response.error().is_none(), "{:?}", generated.response.error());
            broker.snapshot()?.active_plan.unwrap()
        };
        let mut projection = service.document(Some(session_id.clone()), serde_json::from_value(json!({
            "operation":"plan_open", "document":"checks-review", "view":"checks-view", "plan_id":plan.id,
            "digest":plan.review_digest, "width":{"columns":80,"tabstop":4}
        }))?).await?;
        projection = crate::plan::reconstruct(None, projection);
        let annotation_line = projection["source_row"].as_array().unwrap().iter().find(|row| row["target"].is_string()).unwrap()["source_line"].clone();
        let (sink, mut output) = forge_protocol::outbound::channel();
        for (index, enabled) in [false, true].into_iter().enumerate() {
            assert_eq!(projection["checks"][0]["enabled"], !enabled);
            let row = projection["source_row"][0].clone();
            service.dispatch(Some(session_id.clone()), Request { id:3, method:"plan.checks.update".into(),
                params:json!({"enabled":{"format":enabled}, "expected_version":projection["version"],
                    "draft_source_digest":projection["saved_source_digest"],
                    "draft_annotation":[{"id":"retained", "source":{"start_line":annotation_line,"end_line":annotation_line,"body":"Keep this feedback."}}],
                    "review":{"document":"checks-review","view":"checks-view","sequence":index+1,
                        "revision":projection["snapshot"]["revision"],"block":row["block"],"position":row["position"],
                        "target":row["target"],"action":"checks"}}) }, &sink).await?;
            loop {
                let frame = output.recv().await?.context("check edit response is missing")?;
                let value: Value = serde_json::from_slice(frame.bytes())?;
                if value["id"] != 3 { continue; }
                assert!(value.get("error").is_none(), "{value}");
                assert!(value["result"].get("review_error").is_none(), "{value}");
                projection = crate::plan::reconstruct(Some(&projection), value["result"]["review"].clone());
                break;
            }
            assert_eq!(projection["checks"][0]["enabled"], enabled);
            assert_eq!(projection["source_row"].as_array().unwrap().iter().any(|row| row["check"] == "format"), enabled);
            assert!(projection["annotation"].as_array().unwrap().iter().any(|annotation| annotation["id"] == "retained"));
            assert_eq!(serde_json::from_slice::<Value>(&std::fs::read(fixture.path().join(".forge.json"))?)?, config);
        }
        let stale = controller.broker.lock().await.dispatch(Request { id:4, method:"plan.checks.update".into(),
            params:json!({"plan_id":plan.id,"digest":projection["saved_source_digest"], "expected_version":0, "enabled":{"format":false}}) }).await;
        assert!(stale.response.error().is_some());
        Ok(())
    }

    #[tokio::test]
    async fn manual_plan_edits_retain_projection_and_decision_history() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await?;
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let mut plan = {
            let mut broker = controller.broker.lock().await;
            let generated = broker.dispatch(Request { id: 2, method: "prompt.submit".into(),
                params: json!({"text":"/plan add test coverage"}) }).await;
            assert!(generated.response.error().is_none(), "{:?}", generated.response.error());
            broker.snapshot()?.active_plan.unwrap()
        };
        let data = fixture.path().join("data");
        let file = crate::plan::PlanFileStore::new(data.clone(), fixture.path());
        let mut canonical = file.read_working_document(&session_id, &plan.id)?;
        canonical.design.as_mut().unwrap().document.tests = serde_json::from_value(json!([
            {"file":"tests/config.rs","cases":[
                {"name":"first","change":"new","description":"Reject invalid input."},
                {"name":"keep","change":"new","description":"Accept valid input."}
            ]}
        ]))?;
        let (_, _, digest) = file.submit_validated_document_revision(&session_id, &plan.id,
            plan.model_revision, canonical.version, canonical)?;
        plan.review_digest = Some(digest.clone());
        SqliteStore::open(&data)?.save_plan(&plan)?;
        let submitted = file.read_submitted_document(&session_id, &plan.id, plan.model_revision)?;
        let lifecycle_count = SqliteStore::open(&data)?.list_plan_lifecycle(&session_id)?.len();
        let mut projection = service.document(Some(session_id.clone()), serde_json::from_value(json!({
            "operation":"plan_open", "document":"test-review", "view":"test-view",
            "plan_id":plan.id, "digest":digest,
            "width":{"columns":100,"tabstop":4}
        }))?).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        projection = crate::plan::reconstruct(None, projection);
        for (index, name) in ["first", "keep"].into_iter().enumerate() {
            let row = projection["source_row"].as_array().unwrap().iter()
                .find(|row| row["test"]["name"] == name).unwrap();
            let previous_revision = projection["snapshot"]["revision"].as_u64().unwrap();
            let previous_version = projection["version"].as_u64().unwrap();
            service.dispatch(Some(session_id.clone()), Request { id: 3, method: "plan.tests.delete".into(),
                params: json!({"tests":[{"file":"tests/config.rs","name":name}],
                    "expected_version":projection["version"], "draft_annotation":[],
                    "draft_source_digest":projection["saved_source_digest"],
                    "review":{"document":"test-review","view":"test-view","sequence":index + 1,
                        "revision":previous_revision,"block":row["block"],"position":row["position"],
                        "target":row["target"],"action":"delete_tests"}}) }, &sink).await?;
            loop {
                let frame = output.recv().await?.context("test deletion response is missing")?;
                let value: Value = serde_json::from_slice(frame.bytes())?;
                if value["id"] != 3 { continue; }
                assert!(value.get("error").is_none(), "{value}");
                assert!(value["result"].get("review_error").is_none(), "{value}");
                assert_eq!(value["result"]["plan"]["manual_changes"][index], format!("Removed planned tests: tests/config.rs::{name}"));
                assert_eq!(value["result"]["plan"]["model_revision"], plan.model_revision);
                assert_eq!(value["result"]["plan"]["user_revision"], plan.user_revision);
                projection = crate::plan::reconstruct(Some(&projection), value["result"]["review"].clone());
                break;
            }
            assert_eq!(projection["snapshot"]["document"], "test-review");
            assert!(projection["snapshot"]["revision"].as_u64().unwrap() > previous_revision);
            assert_eq!(projection["version"], previous_version + 1);
            assert!(!projection["source_row"].as_array().unwrap().iter().any(|row| row["test"]["name"] == name));
        }
        let row = projection["source_row"].as_array().unwrap().iter()
            .find(|row| row["text"].as_str().is_some_and(|text| text.trim_start().starts_with("pub fn "))
                && row["target"].is_string()).context("rename declaration is missing")?;
        let mut input = json!({"document":"test-review","view":"test-view","sequence":3,
            "revision":projection["snapshot"]["revision"],"block":row["block"],"position":row["position"],
            "target":row["target"],"action":"rename_entity"});
        input["position"]["column"] = json!(row["text"].as_str().unwrap().find("fn ").unwrap() + 3);
        let selected = service.document(Some(session_id.clone()), serde_json::from_value(json!({
            "operation":"plan_action","input":input
        }))?).await?;
        assert!(selected.get("rename").is_some(), "{selected}");
        let annotation = json!([{"id":"rename-feedback","source":{"start_line":row["source_line"],
            "end_line":row["source_line"],"body":"Keep this comment attached after rename."}}]);
        input["sequence"] = json!(4);
        service.dispatch(Some(session_id.clone()), Request { id:4, method:"plan.entity.rename".into(),
            params:json!({"review":input,"symbol":selected["rename"]["symbol"],"name":"reviewed_name",
                "expected_version":projection["version"],"draft_annotation":annotation,
                "draft_source_digest":projection["saved_source_digest"]}) }, &sink).await?;
        let renamed = loop {
            let frame = output.recv().await?.context("rename response is missing")?;
            let value: Value = serde_json::from_slice(frame.bytes())?;
            if value["id"] == 4 { break value; }
        };
        assert!(renamed.get("error").is_none(), "{renamed}");
        assert!(renamed["result"].get("review_error").is_none(), "{renamed}");
        let renamed_review = crate::plan::reconstruct(Some(&projection), renamed["result"]["review"].clone());
        assert_eq!(renamed_review["snapshot"]["document"], "test-review");
        assert_eq!(renamed_review["annotation"][0]["source"]["body"],
            "Keep this comment attached after rename.");
        assert!(renamed_review["source_row"].as_array().unwrap().iter()
            .any(|row| row["text"].as_str().is_some_and(|text| text.contains("reviewed_name"))));
        let changes = renamed["result"]["plan"]["manual_changes"].clone();
        assert_eq!(changes.as_array().unwrap().len(), 3);
        assert_eq!(renamed["result"]["plan"]["model_revision"], plan.model_revision);
        assert_eq!(file.read_submitted_document(&session_id, &plan.id, plan.model_revision)?, submitted);
        assert!(file.read_submitted_document(&session_id, &plan.id, plan.model_revision + 1).is_err());
        assert_eq!(SqliteStore::open(&data)?.list_plan_lifecycle(&session_id)?.len(), lifecycle_count);
        {
            let mut broker = controller.broker.lock().await;
            let rejected = broker.dispatch(Request { id: 99, method: "plan.request_changes".into(),
                params: json!({"plan_id":plan.id,"digest":"stale-review-digest","comment":"x".repeat(65537)}) }).await;
            assert!(rejected.response.error().unwrap().message.contains("plan review digest changed"));
            assert!(file.read_submitted_document(&session_id, &plan.id, plan.model_revision + 1).is_err());
        }
        service.dispatch(Some(session_id.clone()), Request { id:5, method:"plan.request_changes".into(),
            params:json!({"plan_id":plan.id,"digest":renamed["result"]["plan"]["review_digest"],
                "comment":"Retain the manual edits while revising the plan."}) }, &sink).await?;
        loop {
            let frame = output.recv().await?.context("review decision response is missing")?;
            let value: Value = serde_json::from_slice(frame.bytes())?;
            if value["id"] != 5 { continue; }
            assert!(value.get("error").is_none(), "{value}");
            assert_eq!(value["result"]["manual_changes"], changes);
            break;
        }
        assert_eq!(file.read_submitted_document(&session_id, &plan.id, plan.model_revision)?, submitted);
        let reviewed = file.read_submitted_document(&session_id, &plan.id, plan.model_revision + 1)?;
        assert!(reviewed.design.as_ref().unwrap().document.tests.is_empty());
        assert!(reviewed.design.as_ref().unwrap().proposed.values().any(|text| text.contains("reviewed_name")));
        assert_eq!(SqliteStore::open(&data)?.load_plan(&plan.id)?.unwrap().model_revision, plan.model_revision + 2);
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn input_timeout_settings_do_not_interrupt_a_busy_session() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await?;
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let execution = controller.execution_admission.acquire().await?;
        let broker = controller.broker.lock().await;
        let (sink, _output) = forge_protocol::outbound::channel();
        tokio::time::timeout(Duration::from_secs(2), service.dispatch(Some(session_id.clone()), Request {
            id:2, method:"session.input_timeouts".into(),
            params:json!({"approval_timeout":{"amount":2,"unit":"s"},"answer_timeout":{"amount":0,"unit":"m"}}),
        }, &sink)).await??;
        let (approval, answer) = controller.permission.input_timeouts(&session_id)?;
        assert_eq!(approval.amount, 2);
        assert_eq!(answer.amount, 0);
        assert!(controller.execution_admission.try_acquire().is_err());
        assert_eq!(broker.snapshot()?.session.approval_timeout, approval);
        drop(broker);
        drop(execution);
        let settings = crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?;
        assert_eq!(settings.get(&fixture.path().to_string_lossy(), &session_id, "mock")?.desired.answer_timeout, Some(answer));
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

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
    async fn session_queries_complete_while_execution_owns_the_broker() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await.unwrap();
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let _execution = Arc::clone(&controller.execution_admission).try_acquire_owned().unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        for (method, params) in [
            ("history.record", json!({"text":"prompt recorded during execution"})),
            ("state.get", json!({})),
            ("task.list", json!({})),
            ("plan.list", json!({})),
            ("agent.list", json!({})),
            ("exchange.list", json!({})),
            ("session.list", json!({"scope":"all"})),
            ("session.preview", json!({"session_id":session_id})),
            ("permissions.open", json!({})),
        ] {
            tokio::time::timeout(Duration::from_secs(2), service.dispatch(Some(session_id.clone()),
                Request { id: 2, method: method.into(), params }, &sink))
                .await.unwrap_or_else(|_| panic!("{method} waited for execution")).unwrap();
            let frame = output.recv().await.unwrap().unwrap();
            let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
            assert_eq!(value["id"], 2, "{method}: {value}");
            assert!(value["error"].is_null(), "{method}: {value}");
            if method == "state.get" {
                assert_eq!(value["result"]["prompt_history"][0], "prompt recorded during execution");
            }
        }
        let error = tokio::time::timeout(Duration::from_secs(2), service.dispatch(Some(session_id.clone()),
            Request { id: 3, method: "session.configure".into(), params: json!({"effort":"high"}) }, &sink))
            .await.expect("unsupported direct mutation must reject without waiting").unwrap_err();
        assert!(error.to_string().contains("requires a settled session"), "{error:#}");
        drop(broker);
        drop(_execution);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn configuration_acknowledgement_does_not_wait_for_mcp_discovery() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await.unwrap();
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let discovery = controller.mcp_discovery.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let configuring = service.dispatch(Some(session_id.clone()), Request {
            id: 2, method: "task.transition".into(),
            params: json!({"operation_id":"configure-responsive","action":"configure","config":{"effort":"low"}}),
        }, &sink);
        tokio::pin!(configuring);
        tokio::time::timeout(Duration::from_secs(2), async {
            tokio::select! {
                result = &mut configuring => panic!("configuration completed before its owner released: {result:?}"),
                frame = output.recv() => {
                    let frame = frame.unwrap().unwrap();
                    let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
                    assert_eq!(value["result"]["state"], "accepted", "{value}");
                }
            }
        }).await.expect("configuration acknowledgement waited for MCP discovery");
        assert!(controller.mcp_revision.load(Ordering::Acquire) > 0);
        drop(discovery);
        drop(broker);
        tokio::time::timeout(Duration::from_secs(2), &mut configuring).await.unwrap().unwrap();
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn root_model_catalog_bypasses_the_active_broker_lock() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await.unwrap();
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let mut store = SqliteStore::open(&fixture.path().join("data")).unwrap();
        let preference = serde_json::from_value(json!({
            "model":"default", "effort":"medium",
            "model_setting":{"mock-model":{"reasoning":"high","context_window":"default"}}
        })).unwrap();
        store.save_preference(fixture.path().to_str().unwrap(), "mock", &preference).unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        tokio::time::timeout(Duration::from_secs(2), service.dispatch(Some(session_id.clone()),
            Request { id: 2, method: "backend.models".into(), params: json!({}) }, &sink))
            .await.expect("root model selection must remain available during provider execution").unwrap();
        let frame = output.recv().await.unwrap().unwrap();
        let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
        assert_eq!(value["id"], 2, "{value}");
        assert!(value["error"].is_null(), "{value}");
        assert_eq!(value["result"][0]["id"], "mock-model");
        assert_eq!(value["result"][0]["selected_reasoning"], "high");
        assert_eq!(value["result"][0]["selected_context_window"], "default");
        drop(broker);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn idle_root_model_catalog_publishes_and_persists_default_resolution() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await.unwrap();
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let (sink, mut output) = forge_protocol::outbound::channel();
        service.dispatch(Some(session_id.clone()),
            Request { id: 2, method: "backend.models".into(), params: json!({}) }, &sink).await.unwrap();
        let mut configured = false;
        loop {
            let frame = output.recv().await.unwrap().unwrap();
            let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
            if value["event"] == "session_configured" {
                configured = value["payload"]["resolved_model"] == "mock-model";
            }
            if value["id"] == 2 {
                assert_eq!(value["result"][0]["selected_reasoning"], "medium");
                break;
            }
        }
        assert!(configured, "idle catalog did not publish default model resolution");
        let store = SqliteStore::open(&fixture.path().join("data")).unwrap();
        assert_eq!(store.load_session(&session_id).unwrap().unwrap().resolved_model.as_deref(), Some("mock-model"));
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
                assert!(value["result"]["models"].as_array().is_some_and(|models| !models.is_empty()), "{value}");
                assert_eq!(value["result"]["capability"]["fast_mode"], true, "{value}");
                assert_eq!(value["result"]["capability"]["ultrafast_mode"], true, "{value}");
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
            HarnessMethod::PlanCancel, HarnessMethod::PlanEntityRename, HarnessMethod::PlanTestsDelete, HarnessMethod::PlanChecksUpdate] {
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
                tokio::join!(route_control_request(&controller, &request, method, &sink, &service.syntax, None), respond)
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
            params:json!({"approval_id":approval_id,"answer_list":answers})},HarnessMethod::ApprovalResolve,&sink,&service.syntax,None).await?);
        assert_eq!(pending.await??,crate::backend::approval::ApprovalResolution::AllowOnce);
        let resolved = tokio::time::timeout(Duration::from_secs(2), events.recv()).await??.unwrap();
        assert_eq!(resolved.kind, "approval_resolved");
        controller.agent_coordinator.sender().send(crate::agent::AgentCommand::Event {
            owner: crate::backend::ExecutionOwner { session_id: session_id.clone(), agent_id: agent_id.clone() },
            task_id: task_id.clone(), event: resolved,
        }).await?;
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
    async fn permission_transition_releases_child_quiescence_before_resuming() -> Result<()> {
        for mode in ["write", "yolo"] {
            let fixture = tempfile::tempdir()?;
            let service = service();
            let opened = service.open_session(1, initialize(&fixture, "mock")).await?;
            let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
            let registry = service.registry().await?;
            let controller = registry.resolve(&session_id).await?;
            let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
            let session = crate::session::SessionStore::load_session(&store, &session_id)?.unwrap();
            let exchange: crate::exchange::Exchange = serde_json::from_value(json!({
                "id":"interrupted", "session_id":session_id, "agent_id":session.primary_agent_id, "ordinal":1,
                "prompt":"Explain the provider", "kind":"chat", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            store.save_exchange(&exchange)?;
            let previous_execution = controller.execution_admission.acquire().await?;
            let mut operation = admit_task_intent(&controller, &session_id, &Request {
                id: 2, method: "task.transition".into(),
                params: json!({"operation_id":"change-permission","action":"permission","mode":mode}),
            }, &mut store)?;
            assert!(operation.resume_after_transition);
            drop(previous_execution);
            let (sink, _output) = forge_protocol::outbound::channel();
            tokio::time::timeout(Duration::from_secs(2), execute_task_transition(
                &registry, &controller, &mut operation, &mut store, &sink,
            )).await.context("resumed provider output remained blocked by transition quiescence")??;
            assert!(!controller.agent_coordinator.is_quiescing());
            let exchanges = store.list_exchange(&session_id)?;
            assert!(exchanges.last().unwrap().completed_at_ms.is_some());
            let session = crate::session::SessionStore::load_session(&store, &session_id)?.unwrap();
            assert_eq!(session.execution_mode, serde_json::from_value::<crate::session::PermissionMode>(json!(mode))?);
            service.shutdown(Duration::from_secs(1)).await?;
        }
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
    async fn checks_setup_can_be_interrupted_while_provider_startup_is_pending() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service.open_session(1, initialize(&fixture, "mock")).await?;
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let execution = service.prepare(Some(&session_id), HarnessMethod::ChecksSetup).await?.unwrap();
        let maintenance = controller.broker.lock().await;
        let (sink, _output) = forge_protocol::outbound::channel();
        let setup = async {
            let _execution = execution;
            service.dispatch(Some(session_id.clone()), Request {
                id: 2, method: "checks.setup".into(), params: json!({}),
            }, &sink).await
        };
        tokio::pin!(setup);
        assert!(futures_util::poll!(setup.as_mut()).is_pending());
        let stopping = service.dispatch(Some(session_id.clone()), Request {
            id: 3, method: "task.transition".into(),
            params: json!({"operation_id":"interrupt-check-setup","action":"pause"}),
        }, &sink);
        tokio::pin!(stopping);
        assert!(futures_util::poll!(stopping.as_mut()).is_pending());
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        assert_eq!(store.load_task_operation(&session_id, "interrupt-check-setup")?.unwrap().state, "stopping",
            "check setup retained cleanup admission instead of accepting interruption");
        drop(maintenance);
        let (setup_result, stop_result) = tokio::time::timeout(Duration::from_secs(2), async {
            tokio::join!(setup, stopping)
        }).await.context("check setup interruption did not settle promptly")?;
        setup_result?;
        stop_result?;
        assert_eq!(store.load_task_operation(&session_id, "interrupt-check-setup")?.unwrap().state, "completed");
        assert_eq!(store.list_exchange(&session_id)?.last().unwrap().state, crate::exchange::ExchangeState::Cancelled);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn pause_intent_retracts_output_free_submission_when_requested() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        anyhow::ensure!(std::process::Command::new("git")
            .args(["init", "-q"]).current_dir(fixture.path()).status()?.success(),
            "failed to initialize retraction test repository");
        let data = tempfile::tempdir()?;
        let service = service();
        let mut initialization = initialize(&fixture, "blocking");
        initialization.data_root = data.path().to_string_lossy().into_owned();
        let opened = service.open_session(1, initialization).await?;
        let session_id = opened.result().unwrap()["session"]["id"].as_str().unwrap().to_owned();
        let registry = service.registry().await?;
        let execution = service.prepare(Some(&session_id), HarnessMethod::PromptSubmit).await?.unwrap();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let prompt = async {
            let _execution = execution;
            service.dispatch(Some(session_id.clone()), Request {
                id: 2, method: "prompt.submit".into(), params: json!({"text":"Restore this exact prompt"}),
            }, &sink).await
        };
        let stop = async {
            let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
            while store.list_exchange(&session_id)?.is_empty() { tokio::task::yield_now().await; }
            service.dispatch(Some(session_id.clone()), Request {
                id: 3, method: "task.transition".into(),
                params: json!({"operation_id":"retract-prompt","action":"pause","restore_prompt_if_no_output":true}),
            }, &sink).await
        };
        tokio::time::timeout(Duration::from_secs(2), async {
            tokio::try_join!(prompt, stop)
        }).await.context("output-free pause intent did not settle")??;
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        assert!(store.list_exchange(&session_id)?.is_empty(), "{:?}", store.list_exchange(&session_id)?);
        assert_eq!(store.load_task_operation(&session_id, "retract-prompt")?.unwrap().state, "completed");
        loop {
            let frame = tokio::time::timeout(Duration::from_secs(1), output.recv()).await??
                .context("prompt retraction response is missing")?;
            let response: Value = serde_json::from_slice(frame.bytes())?;
            if response["id"] == 2 {
                assert_eq!(response["error"]["code"], "turn_retracted");
                assert_eq!(response["error"]["data"]["prompt"], "Restore this exact prompt");
                break;
            }
        }
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
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
        let setup = service.prepare(None, HarnessMethod::ChecksSetup).await.unwrap().unwrap();
        assert!(service.prepare(None, HarnessMethod::PromptSubmit).await.is_err());
        assert!(service.prepare(None, HarnessMethod::ChecksSetup).await.is_err());
        let cleanup = Arc::clone(&controller.control_admission).try_acquire_owned()
            .expect("check setup must not retain cleanup admission while its model runs");
        assert!(service.prepare(None, HarnessMethod::TurnCancel).await.unwrap().is_none());
        drop(cleanup);
        drop(setup);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn admitted_execution_waits_for_broker_maintenance() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await
            .unwrap();
        let execution = service
            .prepare(None, HarnessMethod::PromptSubmit)
            .await
            .unwrap()
            .unwrap();
        let registry = service.registry().await.unwrap();
        let controller = registry
            .resolve(&registry.initial_session_id)
            .await
            .unwrap();
        let maintenance = controller.broker.lock().await;
        let mut acquisition = Box::pin(acquire_request_broker(
            &controller,
            HarnessMethod::PromptSubmit,
        ));
        assert!(
            futures_util::poll!(acquisition.as_mut()).is_pending(),
            "admitted execution must wait for transient broker ownership"
        );
        drop(maintenance);
        let broker = tokio::time::timeout(Duration::from_secs(2), acquisition)
            .await
            .unwrap()
            .unwrap();
        drop(broker);
        drop(execution);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn settled_mutations_wait_for_maintenance_without_provider_cleanup() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let broker = HarnessBroker::initialize(initialize(&fixture, "mock"))?;
        let mut controller = SessionController::new(broker);
        let backend = Arc::new(SteeringBackend {
            request: Mutex::new(Vec::new()),
        });
        Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
        for method in [
            HarnessMethod::SessionConfigure,
            HarnessMethod::PlanCancel,
            HarnessMethod::GoalPause,
            HarnessMethod::GoalClear,
        ] {
            let maintenance = controller.broker.lock().await;
            let mut acquisition = Box::pin(acquire_request_broker(&controller, method));
            assert!(futures_util::poll!(acquisition.as_mut()).is_pending());
            assert!(
                backend.request.lock().await.is_empty(),
                "maintenance cannot trigger provider cleanup"
            );
            assert!(controller.execution_admission.try_acquire().is_err());
            drop(maintenance);
            let admitted = tokio::time::timeout(Duration::from_secs(2), acquisition).await??;
            assert!(controller.execution_admission.try_acquire().is_err());
            drop(admitted);
            assert!(controller.execution_admission.try_acquire().is_ok());
        }
        Ok(())
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
        let _execution = Arc::clone(&controller.execution_admission).try_acquire_owned()?;
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
    async fn goal_control_timeout_includes_a_blocked_provider_stop() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let broker = HarnessBroker::initialize(initialize(&fixture, "mock"))?;
        let mut controller = SessionController::new(broker);
        let backend = Arc::new(SteeringBackend { request: Mutex::new(Vec::new()) });
        Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
        let _admission = Arc::clone(&controller.execution_admission).try_acquire_owned()?;
        let _execution = controller.broker.lock().await;
        let _provider_stop = backend.request.lock().await;
        let error = tokio::time::timeout(Duration::from_secs(11),
            acquire_request_broker(&controller, HarnessMethod::GoalPause))
            .await.context("blocked provider stop escaped the control deadline")?
            .err().context("blocked provider stop unexpectedly succeeded")?;
        assert!(error.to_string().contains("within 10 seconds"), "{error:#}");
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
        let _execution = Arc::clone(&controller.execution_admission).try_acquire_owned()?;
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
