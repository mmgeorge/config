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
use tokio::sync::{Mutex, Notify, RwLock, RwLockReadGuard};
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
            crate::buffer::session::PresentationRequest::PlanOpen {
                document,
                view,
                plan_id,
                digest,
                width,
                saved_source_digest,
            } => {
                let admission = controller.plan_review.admit(document.clone())?;
                let mut source = controller
                    .broker
                    .lock()
                    .await
                    .capture_plan_review(plan_id, digest)?;
                admission.check()?;
                if let Some(expected) = saved_source_digest {
                    ensure!(
                        expected == &source.saved_digest,
                        "saved plan source changed before recovering annotations"
                    );
                }
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
                admission.check()?;
                let document = crate::plan::review_document::PlanReviewDocument::new(
                    document.clone(),
                    view.clone(),
                    source,
                    width.clone(),
                )?;
                return controller.plan_review.insert(document, admission);
            }
            crate::buffer::session::PresentationRequest::PlanAction { input } => {
                return Ok(serde_json::to_value(
                    controller.plan_review.action(input.clone())?,
                )?);
            }
            crate::buffer::session::PresentationRequest::PlanAddAnnotation { input } => {
                return controller.plan_review.add_annotation(input.clone());
            }
            crate::buffer::session::PresentationRequest::PlanEdit { edit } => {
                return Ok(match controller.plan_review.edit(edit.clone())? {
                    forge_buffer::editable::LocalEditResult::Accepted {
                        acknowledgement,
                        patch,
                    } => {
                        json!({"accepted":true,"acknowledgement":acknowledgement,"patch":patch})
                    }
                    rejected => json!({"accepted":false,"reason":format!("{rejected:?}")}),
                });
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
    pub async fn prepare(&self, session_id: Option<&str>, method: HarnessMethod) -> Result<()> {
        let _activity = self.admit().await?;
        if method.requires_provider_fork() {
            let registry = self.registry().await?;
            let session_id = session_id.unwrap_or(&registry.initial_session_id);
            registry
                .resolve(session_id)
                .await?
                .cancellation
                .arm(method == HarnessMethod::PromptSubmit);
        }
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
        let mut composer_admission = None;
        if matches!(
            method,
            HarnessMethod::PlanAcceptanceBegin | HarnessMethod::PlanRequestChanges
        ) {
            let params = request
                .params
                .as_object_mut()
                .context("plan review submission requires an object")?;
            if let Some(review) = params.remove("review") {
                let input = serde_json::from_value::<forge_buffer::input::DocumentInput>(review)?;
                let controller = registry.resolve(&session_id).await?;
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
            params.remove("_composer_admission");
            if let Some(composer) = params.remove("composer") {
                let document = serde_json::from_value::<forge_buffer::identity::DocumentId>(
                    composer
                        .get("document")
                        .cloned()
                        .context("composer document is required")?,
                )?;
                let revision = serde_json::from_value::<forge_buffer::identity::RegionRevision>(
                    composer
                        .get("revision")
                        .cloned()
                        .context("composer revision is required")?,
                )?;
                let controller = registry.resolve(&session_id).await?;
                let submission = controller
                    .presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .begin_submission(&document, revision)?;
                params.insert("text".into(), Value::String(submission.text));
                params.insert(
                    "_composer_admission".into(),
                    json!({"document":document,"token":submission.token}),
                );
                composer_admission = Some(ComposerAdmission {
                    presentation: Arc::clone(&controller.presentation),
                    document,
                    token: submission.token,
                });
            }
        }
        let result = route_request(registry, session_id, request, method, sink).await;
        drop(composer_admission);
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
                    controller.cancellation.request(false);
                    controller.permission.cancel_all(None)?;
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
    plan_review: crate::plan::review_document::PlanReviewStore,
    broker: Mutex<HarnessBroker>,
    presentation: Arc<std::sync::Mutex<crate::buffer::session::SessionPresentation>>,
    cancellation: Arc<TurnCancellation>,
    backend: Arc<dyn crate::backend::Backend>,
    catalog_request: RwLock<crate::backend::BackendCatalogRequest>,
    permission: Arc<crate::backend::approval::PermissionCoordinator>,
}

struct ComposerAdmission {
    presentation: Arc<std::sync::Mutex<crate::buffer::session::SessionPresentation>>,
    document: forge_buffer::identity::DocumentId,
    token: u64,
}

impl Drop for ComposerAdmission {
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
            plan_review: crate::plan::review_document::PlanReviewStore::default(),
            presentation: broker.presentation(),
            cancellation: broker.turn_cancellation(),
            backend: broker.backend_handle(),
            catalog_request: RwLock::new(catalog_request),
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
    if method.requires_provider_fork() {
        registry
            .await_provider_fork(&session_id, &controller)
            .await?;
    }
    if route_control_request(&controller, &request, method, message_sink).await? {
        return Ok(());
    }

    if !method.requires_provider_fork() {
        controller.cancellation.arm(false);
    }
    let (event_sink, mut event_stream) = crate::backend::events::channel();
    let (data_root, lease_session_id, client_id) = controller.broker.lock().await.lease_identity();
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
            message_sink_for_event.send(Message::Event(SessionEvent {
                session_id: routed_session_id.clone(),
                event: event_name,
                payload,
            }))?;
        }
        Ok::<(), anyhow::Error>(())
    });
    let shutdown = method == HarnessMethod::Shutdown;
    let (result, catalog_request) = {
        let mut broker = controller.broker.lock().await;
        let result = broker.dispatch_stream(request, event_sink).await;
        (result, broker.backend_catalog_request())
    };
    *controller.catalog_request.write().await = catalog_request;
    let _ = heartbeat_stop.send(());
    let _ = heartbeat.await;
    event_forwarder.await??;

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
        message_sink.send(Message::Event(event))?;
    }
    if shutdown {
        message_sink.send_terminal(Message::Response(result.response))?;
    } else {
        message_sink.send(Message::Response(result.response))?;
    }
    Ok(())
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
    message_sink.send(Message::Response(Response::success(request.id, snapshot)?))?;
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
    message_sink.send(Message::Response(Response::success(request.id, snapshot)?))?;
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
                let _ = message_sink.send(Message::Event(event));
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
    message_sink.send(Message::Response(Response::success(request.id, snapshot)?))?;
    Ok(())
}

async fn route_control_request(
    controller: &SessionController,
    request: &Request,
    method: HarnessMethod,
    message_sink: &MessageSender,
) -> Result<bool> {
    let catalog_request = controller.catalog_request.read().await.clone();
    let response = match method {
        HarnessMethod::TurnCancel => {
            if let Some(target) = request.params.get("target").and_then(parse_steer_target) {
                controller.backend.interrupt_target(target).await?;
            } else {
                let restore_prompt = request
                    .params
                    .get("restore_prompt_if_no_output")
                    .and_then(Value::as_bool)
                    .unwrap_or(false);
                controller.cancellation.request(restore_prompt);
                controller.permission.cancel_all(None)?;
            }
            Some(Response::success(
                request.id,
                json!({ "cancel_requested": true }),
            )?)
        }
        HarnessMethod::TurnRestart => {
            controller.cancellation.request(false);
            controller.permission.cancel_all(None)?;
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
            match request.params.get("target").and_then(parse_steer_target) {
                Some(target) => controller.backend.steer_target(text, target).await?,
                None => {
                    controller
                        .backend
                        .steer_session(&catalog_request.harness_session_id, text)
                        .await?
                }
            }
            Some(Response::success(request.id, json!({ "steered": true }))?)
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
                .resolve(approval_id, choice_id, None)?;
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
            let server_list = tokio::time::timeout(
                Duration::from_secs(30),
                controller.backend.mcp_list(catalog_request.clone()),
            )
            .await
            .context("MCP status did not finish within 30 seconds")??;
            Some(Response::success(request.id, server_list)?)
        }
        HarnessMethod::BackendMcpSetEnabled => {
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
                controller.cancellation.request(false);
                controller.permission.cancel_all(None)?;
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
            message_sink.send(Message::Response(response))?;
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

fn parse_steer_target(value: &Value) -> Option<crate::backend::SteerTarget> {
    Some(crate::backend::SteerTarget {
        thread_id: value.get("thread_id")?.as_str()?.to_owned(),
        turn_id: value.get("turn_id")?.as_str()?.to_owned(),
    })
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
