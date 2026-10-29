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
use tokio::sync::{Mutex, MutexGuard, Notify, RwLock, RwLockReadGuard};
use tokio::task::JoinSet;

use crate::broker::{
    BrokerRuntime, HarnessBroker, InitializeRequest, TurnCancellation, prepare_new_session,
    prepare_provider_fork,
};
use crate::command::{CommandAdmission, CommandIdentity, ExecutionPermit, SessionCoordinator};
use crate::protocol::{RuntimeMethod, validate_root_control};
use crate::session::{ProviderForkState, SessionLeaseConflict, SessionStore};
use crate::storage::SqliteStore;
use crate::task::command as session_command;

#[path = "service/input.rs"]
mod input_delivery;
#[path = "service/review_questions.rs"]
mod review_questions;
#[path = "service/shared.rs"]
mod shared;
#[path = "service/task_state.rs"]
mod task_state;

#[derive(Clone, Copy)]
enum CheckOperation {
    List,
    Status,
    Cancel,
    Override,
    Run,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum InitializationMode {
    Services,
    Session,
}

impl CheckOperation {
    fn from_method(method: RuntimeMethod) -> Option<Self> {
        Some(match method {
            RuntimeMethod::SessionCheckList => Self::List,
            RuntimeMethod::SessionCheckStatus => Self::Status,
            RuntimeMethod::SessionCheckCancel => Self::Cancel,
            RuntimeMethod::SessionCheckOverride => Self::Override,
            RuntimeMethod::SessionCheckRun => Self::Run,
            _ => return None,
        })
    }
}

/// Defines provider settings for one-shot generation without a conversation lease.
#[derive(Clone, Debug, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TextGenerationConfig {
    pub backend: crate::backend::BackendLaunch,
    pub model: String,
    /// Explicit effort for the isolated request, independent of the active conversation.
    pub reasoning_effort: Option<String>,
    #[serde(default)]
    /// Provider speed for Background requests, independent of the active conversation.
    pub service_tier: crate::backend::ServiceTier,
    /// Background default, overridden by saved per-session policy.
    #[serde(default = "background_compaction")]
    pub auto_compaction: crate::session::AutoCompaction,
    pub data_root: PathBuf,
}

fn background_compaction() -> crate::session::AutoCompaction {
    crate::session::AutoCompaction {
        enabled: true,
        ..Default::default()
    }
}

/// Owns lazy Harness initialization and independently serialized session controllers.
pub struct HarnessService {
    repositories: Arc<RepositoryStore>,
    diff: Arc<DiffEngine>,
    syntax: Arc<SyntaxEngine>,
    registry: Mutex<Option<Arc<SessionControllerRegistry>>>,
    /// Independent owner for non-interactive persistent conversations.
    pub background: Arc<crate::background::BackgroundManager>,
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
        if model == "default"
            && config.data_root.join("harness.sqlite3").is_file()
            && let Some(preference) = SqliteStore::open(&config.data_root)?
                .load_preference(workspace, &config.backend.kind)?
        {
            model = preference.model;
        }
        ensure!(
            !model.is_empty() && model.len() <= 256,
            "invalid generation model identity"
        );
        if let Some(effort) = config.reasoning_effort.as_deref() {
            ensure!(
                ["none", "minimal", "low", "medium", "high", "xhigh", "max"].contains(&effort),
                "invalid generation reasoning effort: {effort}"
            );
        }
        let temporary = tempfile::tempdir()?;
        let permission_store = crate::permissions::store::PermissionStore::load(
            temporary.path().join("permissions.json"),
            workspace,
        )?;
        let permission = Arc::new(crate::backend::approval::PermissionCoordinator::new(
            Arc::new(std::sync::RwLock::new(permission_store)),
        ));
        let trace = Arc::new(crate::trace::TraceStore::open(&config.data_root)?);
        let backend = crate::backend::build(config.backend, permission, trace, config.data_root)?;
        let request = crate::backend::BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: format!("generation-{}", uuid::Uuid::new_v4()),
            workspace: workspace.to_owned(),
            access: backend
                .descriptor()
                .kind
                .resolve_access(crate::session::AccessPolicy {
                    write_access: crate::session::WriteScope::Off,
                    ..Default::default()
                }),
            approval_mode: crate::session::ApprovalMode::Ask,
            backend_session_id: None,
        };
        generate_isolated_text(
            backend.as_ref(),
            request,
            purpose,
            &model,
            config.reasoning_effort.as_deref(),
            input,
        )
        .await
    }

    pub async fn saved_diff(
        &self,
        session_id: Option<String>,
        input: forge_buffer::input::DocumentInput,
    ) -> Result<String> {
        let _activity = self.admit().await?;
        let registry = self.registry().await?;
        let session_id = session_id.context("an explicit session_id is required")?;
        let controller = registry.resolve(&session_id).await?;
        let action = controller
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .action(input)?;
        match action {
            crate::buffer::projection::TranscriptAction::Diff { text, .. } => Ok(text),
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
        let session_id = session_id.context("an explicit session_id is required")?;
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
                    .generate_text(request, purpose, model, None, &history)
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
                if !controller
                    .backend
                    .descriptor()
                    .capability
                    .background_terminals
                {
                    return Ok(
                        json!({"supported":false,"reason":"Provider does not expose background terminal inventory","terminal":[]}),
                    );
                }
                let snapshot = controller.background_terminals().await?;
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
                let mut source = if let Some(source) = controller
                    .revision_review
                    .capture(plan_id, digest, *revision)
                    .await?
                {
                    source
                } else {
                    if let Some(revision) = revision {
                        let source = controller
                            .observer
                            .capture_plan_revision(plan_id, *revision)?;
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
                analyze_plan_source(&mut source, &self.syntax, || Ok(admission.check()?), None)
                    .await?;
                if let Some(trace) = &source.trace {
                    trace.record(
                        "plan.review.syntax",
                        syntax_started.elapsed(),
                        source.declaration_syntax.len(),
                    );
                }
                admission.check()?;
                let document = crate::plan::review_document::ReadOnlyPlanReview::new(
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
                let preview = preview_trace
                    .as_ref()
                    .map(|trace| trace.stage("preview.total"));
                let trace = if matches!(input.action.as_str(), "jump_entity" | "open") {
                    crate::declaration::trace::DeclarationTrace::new(
                        registry.runtime.trace(),
                        &session_id,
                        input,
                    )
                } else {
                    None
                };
                let total = trace.as_ref().map(|trace| trace.stage("total", None));
                if matches!(input.action.as_str(), "jump_entity" | "open") {
                    controller
                        .plan_review
                        .prepare_declaration_jump(input, trace.as_ref())
                        .await?;
                }
                let stage = trace.as_ref().map(|trace| trace.stage("action", None));
                let result = controller.plan_review.action(input.clone())?;
                if let Some(stage) = preview {
                    stage.complete(json!({"count":result.get("rename").and_then(|rename|rename.get("preview")).and_then(Value::as_array).map_or(0,Vec::len)}));
                }
                if let Some(stage) = stage {
                    stage.complete(json!({}));
                }
                let result = serde_json::to_value(result)?;
                if let Some(stage) = total {
                    stage.complete(json!({}));
                }
                return Ok(result);
            }
            crate::buffer::session::PresentationRequest::PlanDefinition {
                input,
                source,
                target,
            } => {
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
                .export_tool(&document, &directory, controller.trace.clone(), &session_id)?;
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
            background: crate::background::BackgroundManager::new(
                Arc::clone(&repositories),
                Arc::clone(&diff),
                Arc::clone(&syntax),
            ),
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
        match self
            .initialize(initialize, InitializationMode::Session)
            .await
        {
            Ok(snapshot) => Ok(Response::success(request_id, snapshot)?),
            Err(error) => Ok(initialize_failure(request_id, &error)),
        }
    }

    /// Initialize shared runtime services without creating or selecting a session.
    pub async fn initialize_services(
        &self,
        request_id: u64,
        initialize: InitializeRequest,
    ) -> Result<Response> {
        let _activity = self.admit().await?;
        match self
            .initialize(initialize, InitializationMode::Services)
            .await
        {
            Ok(result) => Ok(Response::success(request_id, result)?),
            Err(error) => Ok(initialize_failure(request_id, &error)),
        }
    }

    /// Arm provider work in input order before concurrent dispatch can admit a later cancellation.
    /// Keep the permit until execution finalization, then release it before publishing completion. Overlapping execution and
    /// pending cleanup reject admission without resetting the current cancellation state. Direct prompts
    /// interrupt active checks and collect their execution before acquiring admission.
    pub async fn prepare(
        &self,
        session_id: Option<&str>,
        method: RuntimeMethod,
    ) -> Result<Option<ExecutionPermit>> {
        let _activity = self.admit().await?;
        if method.requires_provider_fork() {
            let registry = self.registry().await?;
            let session_id = session_id.context("an explicit session_id is required")?;
            let controller = registry.resolve(session_id).await?;
            let _control = Arc::clone(&controller.coordinator.control)
                .try_acquire_owned()
                .map_err(|_| {
                    crate::broker::RequestNotAdmitted("Harness cleanup is still running")
                })?;
            let interrupted_checks = if method == RuntimeMethod::SessionPromptSubmit {
                if let Some(run_id) = controller.checks.active()? {
                    controller.interrupt_checks(session_id, &run_id).await?;
                    true
                } else {
                    false
                }
            } else {
                false
            };
            let execution = if interrupted_checks {
                tokio::time::timeout(
                    Duration::from_secs(10),
                    Arc::clone(&controller.coordinator.execution).acquire_owned(),
                )
                .await
                .context("interrupted checks did not release execution within 10 seconds")??
            } else {
                Arc::clone(&controller.coordinator.execution)
                    .try_acquire_owned()
                    .map_err(|_| {
                        crate::broker::RequestNotAdmitted(
                            "a Harness execution request is already running",
                        )
                    })?
            };
            controller
                .cancellation
                .arm(method == RuntimeMethod::SessionPromptSubmit);
            return Ok(Some(execution));
        }
        Ok(None)
    }

    /// Resolve task commands before choosing provider execution admission.
    ///
    /// Preserves command identity across repeated normalization and resolves
    /// acceptance replay from its receipt before consulting the current task.
    pub async fn normalize_request(
        &self,
        session_id: Option<&str>,
        request: &mut Request,
    ) -> Result<()> {
        use session_command::SessionCommand;
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
        let command = if RuntimeMethod::decode(&candidate.method)?
            == RuntimeMethod::SessionQuestionContinue
        {
            let registry = self.registry().await?;
            let controller = registry
                .resolve(session_id.context("an explicit session_id is required")?)
                .await?;
            let prior = if let Some(id) = candidate.params["operation_id"].as_str() {
                SqliteStore::read_only(&controller.data_root)?
                    .load_task_operation(controller.observer.session_id(), id)?
            } else {
                None
            };
            let target = if prior.is_none() {
                controller
                    .task_controller
                    .current()?
                    .and_then(|task| match task.workflow {
                        crate::task::TaskWorkflow::Plan { plan } => plan
                            .artifact
                            .acceptance
                            .map(|acceptance| (plan.artifact.id, acceptance.review_digest)),
                        _ => None,
                    })
            } else {
                None
            };
            SessionCommand::question_continue(&candidate, prior.as_ref(), target)?
        } else {
            SessionCommand::parse_request(&candidate)?
        };
        if let Some(command) = command {
            let identity = request.params.get("_command").cloned();
            *request = command.to_request(request.id)?;
            if let Some(identity) = identity {
                request.params["_command"] = identity;
            }
        }
        Ok(())
    }

    /// Wait for queued prompt ownership without holding cleanup admission during active work.
    async fn prepare_queued_prompt(&self, session_id: &str) -> Result<ExecutionPermit> {
        let registry = self.registry().await?;
        let controller = registry.resolve(session_id).await?;
        let request_revision = controller
            .cancellation
            .request_revision
            .load(Ordering::Acquire);
        loop {
            let execution = Arc::clone(&controller.coordinator.execution)
                .acquire_owned()
                .await?;
            if let Ok(_control) = Arc::clone(&controller.coordinator.control).try_acquire_owned() {
                ensure!(
                    self.accepting.load(Ordering::Acquire),
                    "Harness service is closed"
                );
                ensure!(
                    controller
                        .cancellation
                        .request_revision
                        .load(Ordering::Acquire)
                        == request_revision,
                    "queued prompt was interrupted before admission; the message remains queued"
                );
                controller.cancellation.arm(true);
                return Ok(execution);
            }
            // Cleanup may itself need execution ownership. Release it before waiting for cleanup.
            drop(execution);
            drop(
                Arc::clone(&controller.coordinator.control)
                    .acquire_owned()
                    .await?,
            );
        }
    }

    /// Persist transition ordering before the admitted request begins execution.
    pub async fn prepare_task(&self, session_id: Option<&str>, request: &Request) -> Result<()> {
        let _activity = self.admit().await?;
        let registry = self.registry().await?;
        let session_id = session_id.context("an explicit session_id is required")?;
        let controller = registry.resolve(session_id).await?;
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let mut request = request.clone();
        if let Some(params) = request.params.as_object_mut() {
            params.remove("_command");
        }
        if task_state::handles(&controller, &request)? {
            return Ok(());
        }
        admit_task_intent(&controller, session_id, &request, &mut store)?;
        Ok(())
    }

    /// Route an operation without transferring a host-held execution permit.
    /// Provider-bound host requests use `dispatch_admitted` with the permit from `prepare`.
    pub async fn dispatch(
        &self,
        session_id: Option<String>,
        request: Request,
        sink: &MessageSender,
    ) -> Result<()> {
        self.dispatch_admitted(session_id, request, sink, None)
            .await
    }

    /// Publish completion only after the admitted execution and composer capture are released.
    pub async fn dispatch_admitted(
        &self,
        session_id: Option<String>,
        mut request: Request,
        sink: &MessageSender,
        admission: Option<ExecutionPermit>,
    ) -> Result<()> {
        self.normalize_request(session_id.as_deref(), &mut request)
            .await?;
        let identity = request
            .params
            .as_object_mut()
            .and_then(|params| params.remove("_command"));
        if request.method == "session.command.status.get" {
            let registry = self.registry().await?;
            let session_id = session_id
                .as_deref()
                .context("an explicit session_id is required")?;
            let controller = registry.resolve(session_id).await?;
            let id = request.params["id"]
                .as_str()
                .context("command identity is required")?;
            return Ok(sink
                .send_response(Response::success(
                    request.id,
                    controller.coordinator.status(id)?,
                )?)
                .await?);
        }
        let Some(identity) = identity else {
            return self
                .dispatch_request(session_id, request, sink, admission)
                .await;
        };
        let identity: CommandIdentity = serde_json::from_value(identity)?;
        let registry = self.registry().await?;
        let owner_id = session_id
            .as_deref()
            .context("an explicit session_id is required")?;
        let controller = registry.resolve(owner_id).await?;
        if let Some(epoch) = &identity.runtime_epoch {
            ensure!(
                epoch == controller.observer.runtime_epoch(),
                "command belongs to a previous host lifetime"
            );
        }
        let lease =
            match controller
                .coordinator
                .admit(&identity, &request.method, &request.params)?
            {
                CommandAdmission::Recorded(mut response) => {
                    response.id = request.id;
                    return Ok(sink.send_response(response).await?);
                }
                CommandAdmission::Pending => {
                    return Ok(sink
                        .send_response(Response::failure(
                            request.id,
                            "outcome_unknown",
                            "The original command is still being reconciled",
                        ))
                        .await?);
                }
                CommandAdmission::Execute(lease) => lease,
            };
        let coordinator = controller.coordinator.clone();
        let command_id = identity.id.clone();
        let attempt = lease.attempt;
        let request_id = request.id;
        let recorded = sink.observe_responses(move |response| {
            if response.id == request_id {
                coordinator
                    .complete(&command_id, attempt, response)
                    .map_err(std::io::Error::other)?;
            }
            Ok(())
        });
        let result = self
            .dispatch_request(session_id, request, &recorded, admission)
            .await;
        if let Err(error) = &result {
            let code = if error.is::<crate::broker::RequestNotAdmitted>() {
                "not_admitted"
            } else {
                "outcome_unknown"
            };
            controller.coordinator.complete(
                &identity.id,
                attempt,
                &Response::failure(request_id, code, crate::storage::error_message(error)),
            )?;
        }
        drop(lease);
        result
    }

    async fn dispatch_request(
        &self,
        session_id: Option<String>,
        mut request: Request,
        sink: &MessageSender,
        mut admission: Option<ExecutionPermit>,
    ) -> Result<()> {
        let method = RuntimeMethod::decode(&request.method)?;
        if method == RuntimeMethod::HostShutdown {
            let initialized = self.registry.lock().await.is_some();
            self.shutdown(Duration::from_secs(2)).await?;
            let result = if initialized {
                json!({"shutdown":true})
            } else {
                json!({"stopped":true})
            };
            sink.send_terminal_response(Response::success(request.id, result)?)?;
            return Ok(());
        }
        let _activity = self.admit().await?;
        let registry = self.registry().await?;
        registry.runtime.lsp.attach(sink.clone());
        if shared::handles(method) {
            return shared::dispatch(registry, session_id, request, sink).await;
        }
        let session_id = session_id.context("an explicit session_id is required")?;
        if method == RuntimeMethod::SessionPromptSubmit
            && request.params["queued"].as_bool() == Some(true)
        {
            ensure!(
                admission.is_none(),
                "queued prompt already owns execution admission"
            );
            admission = Some(self.prepare_queued_prompt(&session_id).await?);
        }
        if method == RuntimeMethod::SessionAttachmentImport {
            registry.resolve(&session_id).await?;
            let root = PathBuf::from(&registry.initialize.data_root);
            let path = PathBuf::from(
                request
                    .params
                    .get("path")
                    .and_then(Value::as_str)
                    .context("attachment path is required")?,
            );
            ensure!(path.is_absolute(), "attachment path must be absolute");
            let number = request
                .params
                .get("number")
                .and_then(Value::as_u64)
                .unwrap_or(1);
            let input_bytes = path.as_os_str().len() + root.as_os_str().len();
            let result = self
                .repositories
                .reads
                .submit_wait(input_bytes, move |cancellation| {
                    cancellation.check()?;
                    let attachment = crate::attachment::Attachment::import(&root, &path)?;
                    cancellation.check()?;
                    Ok(serde_json::to_value(attachment.selection(number)?)?)
                })
                .await?
                .finish()
                .await?;
            sink.send_response(Response::success(request.id, result)?)
                .await?;
            return Ok(());
        }
        let mut prompt_admission = None;
        let edit_trace = if matches!(
            method,
            RuntimeMethod::SessionPlanEntityRename
                | RuntimeMethod::SessionPlanTestDelete
                | RuntimeMethod::SessionPlanCheckUpdate
        ) {
            let params = request
                .params
                .as_object_mut()
                .context("plan edit requires an object")?;
            params
                .entry("operation_id")
                .or_insert_with(|| json!(format!("request:{}", request.id)));
            Some(crate::plan::edit_trace::PlanEditTrace::new(
                registry.runtime.trace(),
                &session_id,
                &request.method,
                &request.params,
            ))
        } else {
            None
        };
        let edit_total = edit_trace
            .as_ref()
            .map(|trace| trace.stage("service.total"));
        let capture_stage = edit_trace
            .as_ref()
            .map(|trace| trace.stage("service.capture_submission"));
        if matches!(
            method,
            RuntimeMethod::SessionPlanAcceptanceBegin
                | RuntimeMethod::SessionPlanChangesRequest
                | RuntimeMethod::SessionPlanCancel
                | RuntimeMethod::SessionPlanEntityRename
                | RuntimeMethod::SessionPlanTestDelete
                | RuntimeMethod::SessionPlanCheckUpdate
        ) {
            let params = request
                .params
                .as_object_mut()
                .context("plan review submission requires an object")?;
            params.remove("review_document");
            if let Some(review) = params.remove("review") {
                let input = serde_json::from_value::<forge_buffer::input::DocumentInput>(review)?;
                if matches!(
                    method,
                    RuntimeMethod::SessionPlanTestDelete
                        | RuntimeMethod::SessionPlanCheckUpdate
                        | RuntimeMethod::SessionPlanEntityRename
                ) {
                    params.insert(
                        "review_document".into(),
                        serde_json::to_value(&input.document)?,
                    );
                }
                let controller = registry.resolve(&session_id).await?;
                if let Some(annotation) = params.remove("draft_annotation") {
                    let digest = params
                        .remove("draft_source_digest")
                        .and_then(|value| value.as_str().map(str::to_owned))
                        .context("plan annotation capture requires its saved source digest")?;
                    let annotation_stage = edit_trace
                        .as_ref()
                        .map(|trace| trace.stage("service.save_draft_annotations"));
                    controller.plan_review.save_annotations(
                        &input.document,
                        &digest,
                        serde_json::from_value(annotation)?,
                    )?;
                    if let Some(stage) = annotation_stage {
                        stage.complete(json!({}));
                    }
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
        if let Some(stage) = capture_stage {
            stage.complete(json!({}));
        }
        if method == RuntimeMethod::SessionPromptSubmit {
            let params = request
                .params
                .as_object_mut()
                .context("prompt submission requires an object")?;
            params.remove("_prompt_admission");
            if let Some(submission) = params.remove("submission") {
                let document = submission
                    .get("document")
                    .cloned()
                    .map(serde_json::from_value::<forge_buffer::identity::DocumentId>)
                    .transpose()?;
                let token = submission
                    .get("token")
                    .and_then(Value::as_u64)
                    .context("submission token is required")?;
                let text = params
                    .get("text")
                    .and_then(Value::as_str)
                    .context("prompt text is required")?;
                if let Some(document) = &document {
                    let controller = registry.resolve(&session_id).await?;
                    controller
                        .presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                        .begin_submission(document, token, text)?;
                    prompt_admission = Some(PromptAdmission {
                        presentation: Arc::clone(&controller.presentation),
                        document: document.clone(),
                        token,
                    });
                }
                params.insert(
                    "_prompt_admission".into(),
                    json!({"document":document,"token":token}),
                );
            }
        }
        let result = Box::pin(route_request(
            registry,
            session_id,
            request,
            method,
            sink,
            &self.syntax,
            RequestAdmission {
                execution: &mut admission,
                prompt: &mut prompt_admission,
            },
        ))
        .await;
        if let Some(stage) = edit_total {
            stage.complete(json!({"status":if result.is_ok() { "completed" } else { "failed" }}));
        }
        drop(prompt_admission);
        result
    }

    /// Close admission, cancel active turns, and release leases only after dispatch and forks finish.
    ///
    /// A timeout retains the registry and task ownership. A later call can finish shutdown.
    /// Provider subprocess cleanup still follows each backend's existing destruction contract.
    pub async fn shutdown(&self, deadline: Duration) -> Result<()> {
        tokio::time::timeout(deadline, self.background.shutdown())
            .await
            .context("Background shutdown timed out")?;
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
                    controller.input_stopping.store(true, Ordering::Release);
                    controller.coordinator.submissions.changed.notify_one();
                    if let Some(task) = controller.timeout_task.lock().await.take() {
                        task.abort();
                    }
                    controller.plan_review.close_all()?;
                    *controller.mcp_discovery.lock().await = Default::default();
                    controller.cancellation.request(false);
                    if let Some(control) = controller.backend.mock_response_control() {
                        control.cancel_session(None);
                    }
                    if let Some(id) = controller.checks.active()? {
                        controller.checks.cancel(&id)?;
                        while controller.checks.active()?.is_some() {
                            controller.checks.status(&id, 250, false).await?;
                        }
                    }
                    let approval_owner = crate::backend::ExecutionOwner::root(
                        &controller.catalog_request.read().await.harness_session_id,
                    );
                    controller
                        .permission
                        .cancel_owner(&approval_owner, None)
                        .await?;
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
                    if let Some(task) = controller.input_task.lock().await.take() {
                        task.await.context("input worker shutdown failed")?;
                    }
                    for subscription in [
                        &controller.timeout_task,
                        &controller.agent_task,
                        &controller.compaction_task,
                    ] {
                        if let Some(task) = subscription.lock().await.take() {
                            task.abort();
                            if let Err(error) = task.await
                                && !error.is_cancelled()
                            {
                                return Err(error).context("session subscription shutdown failed");
                            }
                        }
                    }
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

    async fn initialize(
        &self,
        initialize: InitializeRequest,
        mode: InitializationMode,
    ) -> Result<Value> {
        let startup_started = std::time::Instant::now();
        let mut startup_timing = serde_json::Map::new();
        let mut harness = self.registry.lock().await;
        startup_timing.insert(
            "registry_ready_ms".into(),
            json!(startup_started.elapsed().as_secs_f64() * 1000.0),
        );
        if let Some(registry) = harness.as_ref() {
            ensure!(
                registry.initialize.data_root == initialize.data_root
                    && registry.initialize.workspace == initialize.workspace,
                "runtime is already initialized for a different workspace or data root"
            );
            if mode == InitializationMode::Services {
                return Ok(json!({"initialized":true}));
            }
            ensure!(
                registry.initial_session_id.is_none(),
                "Harness is already initialized"
            );
        }
        if let Some(root) = &initialize.config_dir {
            crate::settings::SettingsStore::open(PathBuf::from(&initialize.data_root).as_path())?
                .configure_catalog(PathBuf::from(root).as_path())?;
        }
        startup_timing.insert(
            "settings_catalog_ms".into(),
            json!(startup_started.elapsed().as_secs_f64() * 1000.0),
        );
        let repository = self
            .repositories
            .open(PathBuf::from(&initialize.workspace))
            .await?;
        startup_timing.insert(
            "repository_ready_ms".into(),
            json!(startup_started.elapsed().as_secs_f64() * 1000.0),
        );
        let workspace_kind = repository
            .as_ref()
            .and_then(|repository| repository.identity.worktree_root.clone())
            .map(crate::workspace::WorkspaceKind::Git)
            .unwrap_or_else(|| {
                crate::workspace::WorkspaceKind::Untracked(PathBuf::from(&initialize.workspace))
            });
        let runtime = if let Some(registry) = harness.as_ref() {
            registry.runtime.clone()
        } else {
            BrokerRuntime::initialize(
                &initialize,
                workspace_kind,
                Arc::clone(&self.repositories),
                Arc::clone(&self.diff),
            )?
        };
        startup_timing.insert(
            "runtime_ready_ms".into(),
            json!(startup_started.elapsed().as_secs_f64() * 1000.0),
        );
        if mode == InitializationMode::Services {
            SqliteStore::open(PathBuf::from(&initialize.data_root).as_path())?;
            *harness = Some(SessionControllerRegistry::new(None, runtime, initialize));
            return Ok(json!({"initialized":true,"startup_timing":{"phases":startup_timing}}));
        }
        let startup_request = initialize.clone();
        let startup_runtime = runtime.clone();
        let (mut broker, snapshot) = tokio::task::spawn_blocking(move || {
            let broker = HarnessBroker::initialize_with_runtime(startup_request, startup_runtime)?;
            let snapshot = broker.snapshot()?;
            Ok::<_, anyhow::Error>((broker, snapshot))
        })
        .await
        .context("Harness session startup worker failed")??;
        startup_timing.insert(
            "broker_ready_ms".into(),
            json!(startup_started.elapsed().as_secs_f64() * 1000.0),
        );
        let broker_timing = std::mem::take(&mut broker.startup_timing);
        let mut result = serde_json::to_value(&snapshot)?;
        startup_timing.insert(
            "snapshot_serialized_ms".into(),
            json!(startup_started.elapsed().as_secs_f64() * 1000.0),
        );
        *harness = Some(SessionControllerRegistry::new(
            Some((snapshot.session.id.clone(), SessionController::new(broker)?)),
            runtime,
            initialize,
        ));
        startup_timing.insert(
            "controller_ready_ms".into(),
            json!(startup_started.elapsed().as_secs_f64() * 1000.0),
        );
        result["startup_timing"] = json!({"phases":startup_timing,"broker":broker_timing});
        Ok(result)
    }
}

/// Owns one session's serialized state machine and out-of-band control lanes.
struct SessionController {
    goal_max_turns: u32,
    task_controller: Arc<crate::task::SessionTaskController>,
    coordinator: Arc<SessionCoordinator>,
    submission_store: Arc<crate::backend::submission::SubmissionStore>,
    input_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    input_stopping: AtomicBool,
    trace: Arc<crate::trace::TraceStore>,
    data_root: PathBuf,
    observer: Arc<crate::broker::BrokerObserver>,
    timeout_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    agent_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    agent_coordinator: Arc<crate::agent::AgentCoordinator>,
    checks: Arc<crate::checks::CheckSupervisor>,
    agent_backend_factory: crate::agent::AgentBackendFactory,
    parent_input: crate::backend::SteeringLane,
    compaction_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
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
    async fn background_terminals(&self) -> Result<crate::backend::terminal::TerminalSnapshot> {
        let request = self.catalog_request.read().await.clone();
        let revision = self.mcp_revision.load(Ordering::Acquire);
        let configuring = || -> Result<bool> { Ok(self.observer.admission.runtime()?.configuring) };
        let pending = || crate::backend::terminal::TerminalSnapshot {
            supported: true,
            starting: true,
            terminal: Vec::new(),
        };
        if configuring()? {
            return Ok(pending());
        }
        let result = tokio::time::timeout(
            Duration::from_secs(8),
            self.backend.background_terminals(request.clone()),
        )
        .await
        .context("background terminal query timed out");
        if revision != self.mcp_revision.load(Ordering::Acquire) || configuring()? {
            return Ok(pending());
        }
        result?
    }

    /// Collects the native execution before a replacement prompt can release its owner.
    async fn interrupt_checks(&self, session_id: &str, run_id: &str) -> Result<()> {
        let active = self.checks.active()?;
        ensure!(
            active.as_deref().is_none_or(|id| id == run_id),
            "check run is not owned by this active request"
        );
        self.cancellation.begin_cleanup(false, false)?;
        let cleanup = async {
            if active.is_some()
                && let Err(error) = self.checks.cancel(run_id)
                && self.checks.active()?.is_some()
            {
                return Err(error);
            }
            let owner = crate::backend::ExecutionOwner::root(session_id);
            self.permission.cancel_owner(&owner, None).await?;
            self.backend.cleanup_execution(session_id).await
        }
        .await;
        if let Err(error) = cleanup {
            self.cancellation.fail_cleanup(&error);
            return Err(
                error.context("collect provider execution before replacing a checked prompt")
            );
        }
        self.cancellation.request(false);
        tokio::time::timeout(Duration::from_secs(10), async {
            while self.checks.active()?.as_deref() == Some(run_id) {
                self.checks.status(run_id, 250, false).await?;
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("check processes did not stop within 10 seconds")??;
        Ok(())
    }

    /// Build one independently serialized controller from a durable broker session.
    fn new(mut broker: HarnessBroker) -> Result<Arc<Self>> {
        let catalog_request = broker.backend_catalog_request();
        let observer = broker.observer();
        let input = Arc::new(crate::input::SessionInputQueue::default());
        broker.input_wakeup = Some(input.changed.clone());
        Ok(Arc::new(Self {
            task_controller: broker.task_controller.clone(),
            goal_max_turns: broker.goal_max_turns,
            coordinator: SessionCoordinator::new(input),
            submission_store: broker.submission_store.clone(),
            input_task: Mutex::new(None),
            input_stopping: AtomicBool::new(false),
            trace: broker.trace.clone(),
            data_root: broker.data_root.clone(),
            observer,
            timeout_task: Mutex::new(None),
            agent_task: Mutex::new(None),
            agent_coordinator: broker.agent_coordinator(),
            checks: broker.checks(),
            agent_backend_factory: broker.agent_backend_factory.clone(),
            parent_input: broker.parent_input.clone(),
            compaction_task: Mutex::new(None),
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
        }))
    }

    async fn attach_compaction(self: &Arc<Self>, message_sink: &MessageSender) -> Result<()> {
        self.attach_timeout(message_sink).await?;
        self.attach_agents(message_sink).await?;
        let mut task = self.compaction_task.lock().await;
        if task.is_some() {
            return Ok(());
        }
        let session_id = self.catalog_request.read().await.harness_session_id.clone();
        let Some(mut stream) = self.backend.compaction_stream(&session_id).await? else {
            return Ok(());
        };
        let controller = Arc::downgrade(self);
        let message_sink = message_sink.clone();
        *task = Some(tokio::spawn(async move {
            let result: Result<()> = async {
                while let Some(event) = stream.recv().await? {
                    let Some(controller) = controller.upgrade() else {
                        return Ok(());
                    };
                    let events = controller
                        .broker
                        .lock()
                        .await
                        .observe_external_compaction(event)?;
                    for event in events {
                        message_sink.send_event(event).await?;
                    }
                }
                Ok(())
            }
            .await;
            if let Err(error) = result {
                let _ = message_sink.send_event(SessionEvent {
                    session_id, event: "backend_event".into(),
                    payload: json!({"kind":"error", "text":format!("Compaction subscription failed: {error:#}")}),
                }).await;
            }
        }));
        Ok(())
    }

    async fn attach_timeout(self: &Arc<Self>, message_sink: &MessageSender) -> Result<()> {
        let mut task = self.timeout_task.lock().await;
        if task.is_some() {
            return Ok(());
        }
        let mut schedule = self.observer.input_schedule.subscribe();
        let owner = crate::backend::ExecutionOwner::root(
            self.catalog_request.read().await.harness_session_id.clone(),
        );
        let mut feedback = self.permission.subscribe_timeout_feedback(&owner)?;
        let mut cancellation = self.cancellation.subscribe();
        let controller = Arc::downgrade(self);
        let message_sink = message_sink.clone();
        *task = Some(tokio::spawn(async move {
            let mut attempted = None;
            loop {
                let mut current = schedule.borrow_and_update().clone();
                let pending_feedback = *feedback.borrow_and_update();
                let cancelled = *cancellation.borrow_and_update();
                let identity = (current.revision, pending_feedback);
                let ready = async {
                    if cancelled || attempted == Some(identity) {
                        return std::future::pending::<()>().await;
                    }
                    if current.approval_allowed && pending_feedback {
                        return;
                    }
                    if let Some(deadline) = &mut current.deadline {
                        tokio::time::sleep_until(deadline.instant(0)).await;
                    } else {
                        std::future::pending::<()>().await;
                    }
                };
                tokio::select! {
                    biased;
                    changed = schedule.changed() => { if changed.is_err() { return; } continue; }
                    changed = feedback.changed() => { if changed.is_err() { return; } attempted = None; continue; }
                    changed = cancellation.changed() => { if changed.is_err() { return; } attempted = None; continue; }
                    () = ready => {}
                }
                let Some(controller) = controller.upgrade() else {
                    return;
                };
                let admission = async {
                    loop {
                        let execution = Arc::clone(&controller.coordinator.execution)
                            .acquire_owned()
                            .await?;
                        if let Ok(_control) = controller.coordinator.control.try_acquire() {
                            return Ok::<_, anyhow::Error>(execution);
                        }
                        drop(execution);
                        drop(controller.coordinator.control.acquire().await?);
                    }
                };
                let _admission = tokio::select! {
                    biased;
                    _ = schedule.changed() => continue,
                    _ = feedback.changed() => { attempted = None; continue; },
                    _ = cancellation.changed() => { attempted = None; continue; }
                    result = admission => match result { Ok(permit) => permit, Err(_) => return },
                };
                let mut broker = tokio::select! {
                    biased;
                    _ = schedule.changed() => continue,
                    _ = feedback.changed() => { attempted = None; continue; },
                    _ = cancellation.changed() => { attempted = None; continue; }
                    broker = controller.broker.lock() => broker,
                };
                attempted = Some(identity);
                let mut active_deadline = current.deadline.clone().map(|deadline| {
                    (
                        format!(
                            "{}:{}",
                            current.identity.as_deref().unwrap_or_default(),
                            deadline.expires_at_ms
                        ),
                        deadline,
                    )
                });
                let session_id = controller
                    .catalog_request
                    .read()
                    .await
                    .harness_session_id
                    .clone();
                let result = async {
                    broker.refresh_input_schedule()?;
                    if schedule.borrow().revision != current.revision || *cancellation.borrow() {
                        return Ok(());
                    }
                    if let Some(request) = broker.expire_input(&mut active_deadline)? {
                        if request.method == "session.question.continue" {
                            message_sink
                                .send_event(SessionEvent {
                                    session_id: session_id.clone(),
                                    event: "question_answered".into(),
                                    payload: json!({"resolution_source":"timeout"}),
                                })
                                .await?;
                        }
                        let dispatch = async {
                            let _ = dispatch_task_attempt(
                                &mut broker,
                                &session_id,
                                request,
                                &message_sink,
                                Arc::clone(&controller.catalog_request),
                            )
                            .await?;
                            Ok::<(), anyhow::Error>(())
                        }
                        .await;
                        dispatch?;
                    }
                    Ok::<(), anyhow::Error>(())
                }
                .await;
                let publication = broker.refresh_input_schedule();
                let result = result.and(publication);
                drop(broker);
                controller.coordinator.submissions.changed.notify_one();
                match result {
                    Ok(()) => {}
                    Err(error) if error.is::<crate::broker::TurnCancelled>() => {}
                    Err(error) => {
                        let _ = message_sink.send_event(SessionEvent { session_id, event: "backend_event".into(),
                            payload: json!({"kind":"error","text":format!("Input timeout failed: {error:#}")}) }).await;
                    }
                }
            }
        }));
        Ok(())
    }

    async fn attach_agents(self: &Arc<Self>, message_sink: &MessageSender) -> Result<()> {
        let mut task = self.agent_task.lock().await;
        if task.is_some() {
            return Ok(());
        }
        let mut revision = self.agent_coordinator.subscribe();
        let controller = Arc::downgrade(self);
        let message_sink = message_sink.clone();
        *task = Some(tokio::spawn(async move {
            loop {
                let Some(controller) = controller.upgrade() else {
                    return;
                };
                let session_id = controller
                    .catalog_request
                    .read()
                    .await
                    .harness_session_id
                    .clone();
                let outcome = controller
                    .broker
                    .lock()
                    .await
                    .drain_agent_commands(None)
                    .await;
                drop(controller);
                match outcome {
                    Ok(event) => {
                        for event in event {
                            if message_sink.send_event(event).await.is_err() {
                                return;
                            }
                        }
                    }
                    Err(error) => {
                        let _ = message_sink.send_event(SessionEvent {session_id,event:"backend_event".into(),
                            payload:json!({"kind":"error","text":format!("Agent coordinator failed: {error:#}")})}).await;
                    }
                }
                if revision.changed().await.is_err() {
                    return;
                }
            }
        }));
        Ok(())
    }
}

impl Drop for SessionController {
    fn drop(&mut self) {
        self.input_stopping.store(true, Ordering::Release);
        self.coordinator.submissions.changed.notify_one();
        if let Some(task) = self.timeout_task.get_mut().take() {
            task.abort();
        }
        if let Some(task) = self.agent_task.get_mut().take() {
            task.abort();
        }
        if let Some(task) = self.compaction_task.get_mut().take() {
            task.abort();
        }
    }
}

/// Stores live session controllers for one persistent broker process.
struct SessionControllerRegistry {
    controller_by_id: RwLock<HashMap<String, Arc<SessionController>>>,
    provider_fork_by_id: RwLock<HashMap<String, Arc<ProviderForkGate>>>,
    fork_task: Mutex<JoinSet<()>>,
    runtime: Arc<BrokerRuntime>,
    initialize: InitializeRequest,
    initial_session_id: Option<String>,
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
        initial: Option<(String, Arc<SessionController>)>,
        runtime: Arc<BrokerRuntime>,
        initialize: InitializeRequest,
    ) -> Arc<Self> {
        let initial_session_id = initial.as_ref().map(|(id, _)| id.clone());
        Arc::new(Self {
            controller_by_id: RwLock::new(initial.into_iter().collect()),
            provider_fork_by_id: RwLock::new(HashMap::new()),
            fork_task: Mutex::new(JoinSet::new()),
            runtime,
            initialize,
            initial_session_id,
        })
    }

    #[cfg(test)]
    fn initial_session_id(&self) -> &str {
        self.initial_session_id
            .as_deref()
            .expect("test requires an opened session")
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
        let runtime = self.runtime.clone();
        let broker = tokio::task::spawn_blocking(move || {
            HarnessBroker::initialize_with_runtime(initialize, runtime)
        })
        .await
        .context("Harness session resume worker failed")??;
        anyhow::ensure!(
            broker.snapshot()?.session.id == session_id,
            "resolved Harness controller does not own requested session {session_id}"
        );
        let controller = SessionController::new(broker)?;
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
    reasoning_effort: Option<&str>,
    input: &str,
) -> Result<String> {
    let provider = backend.descriptor().label;
    let result = backend
        .generate_text(request, purpose, model, reasoning_effort, input)
        .await
        .with_context(|| format!("{provider} {} generation failed", purpose.name()));
    let cleanup = tokio::time::timeout(Duration::from_secs(8), backend.shutdown())
        .await
        .context("Isolated provider shutdown timed out")
        .and_then(|result| result);
    match (result, cleanup) {
        (Err(failure), Err(cleanup)) => {
            Err(failure.context(format!("Isolated provider cleanup failed: {cleanup:#}")))
        }
        (Err(failure), _) | (_, Err(failure)) => Err(failure),
        (Ok(text), Ok(())) => purpose.validate(&text),
    }
}

async fn analyze_plan_source(
    source: &mut crate::plan::review_source::ReviewSourceData,
    syntax: &Arc<SyntaxEngine>,
    check: impl Fn() -> Result<()> + Send + Sync,
    trace: Option<&crate::plan::edit_trace::PlanEditTrace>,
) -> Result<()> {
    if let Some(design) = &source.revision.document.design {
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
                if source
                    .declaration_syntax
                    .contains_key(&(path.clone(), side.into()))
                {
                    if let Some(stage) = stage {
                        stage.complete(json!({"cached":true,"source_bytes":text.len()}));
                    }
                    continue;
                }
                let calls = if side == "baseline" {
                    &design.baseline_calls
                } else {
                    &design.proposed_calls
                };
                let presentation = crate::plan::calls::present(
                    &path,
                    text,
                    calls.get(&path).map(Vec::as_slice).unwrap_or_default(),
                )?;
                check()?;
                let Some(language) = forge_diff::syntax::DeclarationOverview::language(&path)
                else {
                    if let Some(stage) = stage {
                        stage.complete(json!({"status":"unsupported"}));
                    }
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
                    Some(
                        syntax
                            .analyze(SyntaxRequest {
                                source: forge_diff::source::SourceVersion::new(
                                    algorithm.into_bytes(),
                                    forge_diff::source::Representation::DisplayOnly,
                                )?,
                                language: SyntaxLanguage::Algorithm,
                                priority: forge_diff::workers::WorkPriority::Visible,
                                deadline: Some(Instant::now() + Duration::from_secs(10)),
                            })
                            .await
                            .map_err(|error| {
                                anyhow::anyhow!("Algorithm syntax analysis failed: {error:?}")
                            })?,
                    )
                } else {
                    None
                };
                check()?;
                source.declaration_syntax.insert(
                    (path.clone(), side.into()),
                    crate::plan::review_source::ReviewSyntax {
                        declaration: handle,
                        algorithm,
                    },
                );
                if let Some(stage) = stage {
                    stage.complete(json!({"cached":false,"source_bytes":text.len()}));
                }
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
                .map_err(|error| anyhow::anyhow!("PlanReview syntax analysis failed: {error:?}"))?,
        );
    }
    Ok(())
}

/// Forwards check observations without owning execution admission.
fn checks_event_sink(
    output: MessageSender,
    session_id: String,
) -> crate::backend::BackendEventSink {
    let (sink, mut stream) = crate::backend::events::channel();
    tokio::spawn(async move {
        while let Ok(Some(event)) = stream.recv().await {
            let (event_name, payload) = if event.kind == "document_changed" {
                ("document_changed".into(), event.data)
            } else {
                (
                    "backend_event".into(),
                    serde_json::to_value(event).unwrap_or(Value::Null),
                )
            };
            if output
                .send_event(SessionEvent {
                    session_id: session_id.clone(),
                    event: event_name,
                    payload,
                })
                .await
                .is_err()
            {
                break;
            }
        }
    });
    sink
}

struct RequestAdmission<'request> {
    execution: &'request mut Option<ExecutionPermit>,
    prompt: &'request mut Option<PromptAdmission>,
}

async fn route_request(
    registry: Arc<SessionControllerRegistry>,
    session_id: String,
    request: Request,
    method: RuntimeMethod,
    message_sink: &MessageSender,
    syntax: &Arc<SyntaxEngine>,
    permits: RequestAdmission<'_>,
) -> Result<()> {
    let RequestAdmission {
        execution: admission,
        prompt: prompt_admission,
    } = permits;
    if matches!(
        method,
        RuntimeMethod::SessionLspAuthorize
            | RuntimeMethod::SessionLspRespond
            | RuntimeMethod::SessionLspAnalyze
    ) {
        let id = request.params["id"]
            .as_str()
            .context("LSP response requires id")?;
        let result = if method == RuntimeMethod::SessionLspAuthorize {
            registry
                .runtime
                .lsp
                .authorize(
                    &session_id,
                    id,
                    serde_json::from_value(request.params["paths"].clone())?,
                )
                .await?
        } else if method == RuntimeMethod::SessionLspAnalyze {
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
                    match (
                        request.params.get("source_offset"),
                        request.params.get("source_total"),
                    ) {
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
            registry.runtime.lsp.respond(
                &session_id,
                id,
                request.params["result"].clone(),
                request.params["failure"].as_str().map(str::to_owned),
            )?;
            json!({"accepted":true})
        };
        message_sink
            .send_response(Response::success(request.id, result)?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionResume {
        return resume_session(registry, request, message_sink).await;
    }
    if method == RuntimeMethod::SessionDelete {
        return delete_session(registry, &session_id, request, message_sink).await;
    }
    if method == RuntimeMethod::SessionFork {
        start_session_fork(
            registry,
            session_id,
            request.params,
            Some(request.id),
            crate::session::SessionPurpose::Conversation,
            message_sink,
        )
        .await?;
        return Ok(());
    }
    if matches!(method, RuntimeMethod::SessionNew) {
        return route_new_session(registry, session_id, request, message_sink).await;
    }
    let controller = registry.resolve(&session_id).await?;
    ensure!(
        !controller.input_stopping.load(Ordering::Acquire),
        "Harness session is closing"
    );
    controller.attach_compaction(message_sink).await?;
    controller.attach_input(message_sink).await?;
    if matches!(
        method,
        RuntimeMethod::SessionInputApply
            | RuntimeMethod::SessionInputGet
            | RuntimeMethod::SessionInputOperationGet
    ) {
        return controller
            .input_request(request, method, message_sink)
            .await;
    }
    if method == RuntimeMethod::SessionPlanQuestionAnswer {
        return review_questions::answer(registry, controller, session_id, request, message_sink)
            .await;
    }
    if method == RuntimeMethod::SessionConfigure
        && request
            .params
            .as_object()
            .is_some_and(|fields| fields.len() == 1 && fields.contains_key("ask_model"))
    {
        let result = controller
            .broker
            .lock()
            .await
            .configure_ask_model(request.params["ask_model"].clone())
            .await?;
        message_sink
            .send_response(Response::success(request.id, result)?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionProviderMockResponse {
        let control = controller
            .backend
            .mock_response_control()
            .context("backend.mock_response requires the mock provider")?;
        let command = serde_json::from_value(request.params)?;
        let result = control.dispatch(&session_id, command).await?;
        message_sink
            .send_response(Response::success(request.id, result)?)
            .await?;
        return Ok(());
    }
    if let Some(value) = controller.observer.query(method, request.params.clone())? {
        message_sink
            .send_response(Response::success(request.id, value)?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionProviderModelList {
        let catalog = controller.catalog_request.read().await.clone();
        let selected = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?
            .load_session(&session_id)?
            .context("model catalog session is missing")?;
        let mut model_list = tokio::time::timeout(
            Duration::from_secs(30),
            controller
                .backend
                .model_list(crate::backend::BackendRequest {
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
                    approval_mode: catalog.approval_mode,
                    backend_session_id: catalog.backend_session_id,
                    submission: None,
                    recovered_input: Vec::new(),
                    control_context: None,
                }),
        )
        .await
        .context("model catalog exceeded 30 seconds")??;
        let preference =
            SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?
                .load_preference(&selected.workspace, &selected.backend)?;
        crate::backend::select_model_settings(&mut model_list, preference.as_ref());
        let event = if let Ok(mut broker) = controller.broker.try_lock() {
            broker.resolve_default_model(&model_list)?
        } else {
            Vec::new()
        };
        for event in event {
            message_sink.send_event(event).await?;
        }
        message_sink
            .send_response(Response::success(request.id, model_list)?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionConfigure
        && request.params.as_object().is_some_and(|fields| {
            !fields.is_empty()
                && fields
                    .keys()
                    .all(|field| matches!(field.as_str(), "check_timing" | "automatic_checks"))
        })
    {
        let timing = request
            .params
            .get("check_timing")
            .map(|value| serde_json::from_value::<crate::checks::CheckTiming>(value.clone()))
            .transpose()?;
        let enabled = request
            .params
            .get("automatic_checks")
            .map(|value| serde_json::from_value::<bool>(value.clone()))
            .transpose()?;
        let catalog = controller.catalog_request.read().await.clone();
        let mut settings = crate::settings::SettingsStore::open(
            PathBuf::from(&registry.initialize.data_root).as_path(),
        )?;
        let snapshot = settings.get(
            &catalog.workspace,
            &session_id,
            &registry.initialize.backend.kind,
        )?;
        let mut desired = settings.layer_settings(
            crate::settings::SettingScope::Global,
            &catalog.workspace,
            &session_id,
        )?;
        if let Some(timing) = timing {
            desired.check_timing = Some(timing);
        }
        if let Some(enabled) = enabled {
            desired.automatic_checks = Some(enabled);
        }
        settings.update(
            snapshot.revision,
            crate::settings::SettingScope::Global,
            &catalog.workspace,
            &session_id,
            &desired,
        )?;
        let desired = settings
            .get(
                &catalog.workspace,
                &session_id,
                &registry.initialize.backend.kind,
            )?
            .desired;
        if !desired.automatic_checks_enabled() {
            controller.observer.completion_queue.finish()?;
        }
        message_sink
            .send_response(Response::success(
                request.id,
                json!({"check_timing":desired.check_timing.unwrap_or_default(), "automatic_checks":desired.automatic_checks_enabled(), "checks_deferred":controller.observer.completion_queue.deferred()?}),
            )?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionConfigure
        && request
            .params
            .as_object()
            .is_some_and(|fields| fields.len() == 1 && fields.contains_key("status_thoughts"))
    {
        let enabled: bool = serde_json::from_value(request.params["status_thoughts"].clone())?;
        let catalog = controller.catalog_request.read().await.clone();
        let (enabled, patch) = {
            let mut presentation = controller
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?;
            let mut settings = crate::settings::SettingsStore::open(
                PathBuf::from(&registry.initialize.data_root).as_path(),
            )?;
            let snapshot = settings.get(
                &catalog.workspace,
                &session_id,
                &registry.initialize.backend.kind,
            )?;
            let mut desired = settings.layer_settings(
                crate::settings::SettingScope::Global,
                &catalog.workspace,
                &session_id,
            )?;
            desired.status_thoughts = Some(enabled);
            settings.update(
                snapshot.revision,
                crate::settings::SettingScope::Global,
                &catalog.workspace,
                &session_id,
                &desired,
            )?;
            let effective = settings
                .get(
                    &catalog.workspace,
                    &session_id,
                    &registry.initialize.backend.kind,
                )?
                .desired;
            let enabled = effective.status_thoughts.unwrap_or(true);
            let patch = presentation.set_status_thoughts(enabled)?;
            controller
                .observer
                .status_thoughts
                .store(enabled, Ordering::Relaxed);
            (enabled, patch)
        };
        if !patch.is_empty() {
            message_sink
                .send_event(SessionEvent {
                    session_id: session_id.clone(),
                    event: "document_changed".into(),
                    payload: patch.notification(),
                })
                .await?;
        }
        message_sink
            .send_response(Response::success(
                request.id,
                json!({"status_thoughts": enabled}),
            )?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionInputTimeoutConfigure {
        let catalog = controller.catalog_request.read().await.clone();
        let mut settings = crate::settings::SettingsStore::open(
            PathBuf::from(&registry.initialize.data_root).as_path(),
        )?;
        let snapshot = settings.get(
            &catalog.workspace,
            &session_id,
            &registry.initialize.backend.kind,
        )?;
        let mut desired = settings.layer_settings(
            crate::settings::SettingScope::Global,
            &catalog.workspace,
            &session_id,
        )?;
        ensure!(
            request
                .params
                .as_object()
                .is_some_and(|fields| !fields.is_empty()
                    && fields.keys().all(|field| matches!(
                        field.as_str(),
                        "approval_timeout" | "answer_timeout"
                    ))),
            "input timeout configuration accepts only approval_timeout and answer_timeout"
        );
        if let Some(value) = request.params.get("approval_timeout") {
            desired.approval_timeout = Some(
                serde_json::from_value::<crate::session::InputTimeout>(value.clone())?
                    .validate()?,
            );
        }
        if let Some(value) = request.params.get("answer_timeout") {
            desired.answer_timeout = Some(
                serde_json::from_value::<crate::session::InputTimeout>(value.clone())?
                    .validate()?,
            );
        }
        settings.update(
            snapshot.revision,
            crate::settings::SettingScope::Global,
            &catalog.workspace,
            &session_id,
            &desired,
        )?;
        let effective = settings
            .get(
                &catalog.workspace,
                &session_id,
                &registry.initialize.backend.kind,
            )?
            .desired;
        let approval = effective.approval_timeout.unwrap_or_default();
        let answer = effective.answer_timeout.unwrap_or_default();
        controller
            .permission
            .configure_input_timeouts(&session_id, approval, answer)?;
        message_sink
            .send_response(Response::success(
                request.id,
                json!({"approval_timeout":approval,"answer_timeout":answer}),
            )?)
            .await?;
        return Ok(());
    }
    let _setup_admission = if matches!(
        method,
        RuntimeMethod::SessionProviderSetup | RuntimeMethod::SessionProviderAuthenticate
    ) {
        let control = Arc::clone(&controller.coordinator.control)
            .try_acquire_owned()
            .context("Provider setup cannot run while cleanup is pending")?;
        let execution = Arc::clone(&controller.coordinator.execution)
            .try_acquire_owned()
            .context("Finish active work before provider setup or authentication")?;
        Some((control, execution))
    } else {
        None
    };
    if method == RuntimeMethod::SessionProviderSetup {
        let install = request
            .params
            .get("install")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        let result = controller
            .backend
            .setup(controller.catalog_request.read().await.clone(), install)
            .await?;
        controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
        message_sink
            .send_response(Response::success(request.id, result)?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionProviderAuthenticate {
        let method = request.params["method"]
            .as_str()
            .context("authentication method is required")?
            .to_owned();
        controller
            .backend
            .authenticate(controller.catalog_request.read().await.clone(), method)
            .await?;
        controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
        message_sink
            .send_response(Response::success(
                request.id,
                json!({"authenticated":true}),
            )?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionProviderCapabilitiesGet {
        let manifest = tokio::time::timeout(
            Duration::from_secs(30),
            controller
                .backend
                .capabilities(controller.catalog_request.read().await.clone()),
        )
        .await
        .context("capability discovery exceeded 30 seconds")??;
        message_sink
            .send_response(Response::success(request.id, manifest)?)
            .await?;
        return Ok(());
    }
    if let Some(operation) = CheckOperation::from_method(method) {
        if matches!(
            method,
            RuntimeMethod::SessionCheckList
                | RuntimeMethod::SessionCheckRun
                | RuntimeMethod::SessionCheckOverride
        ) {
            controller.checks.refresh_changes().await?;
        }
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let session = store
            .load_session(&session_id)?
            .context("checks session is missing")?;
        let task = controller.task_controller.current()?;
        let task_id = task
            .as_ref()
            .and_then(|task| task.entry.as_ref())
            .map(|entry| entry.id.as_str());
        let result = match operation {
            CheckOperation::List => {
                let settings = crate::settings::SettingsStore::open(
                    PathBuf::from(&registry.initialize.data_root).as_path(),
                )?
                .get(&session.workspace, &session.id, &session.backend)?
                .desired;
                controller
                    .checks
                    .catalog_preview(task_id, session.shell, &settings)
                    .await?
            }
            CheckOperation::Status => {
                let wait_for_completion = request.params["wait_for_completion"]
                    .as_bool()
                    .unwrap_or(false);
                let wait_ms = request.params["wait_ms"]
                    .as_u64()
                    .unwrap_or(if wait_for_completion { 60000 } else { 0 });
                serde_json::to_value(
                    controller
                        .checks
                        .status(
                            request.params["run_id"]
                                .as_str()
                                .context("run_id required")?,
                            wait_ms,
                            wait_for_completion,
                        )
                        .await?,
                )?
            }
            CheckOperation::Cancel => {
                let id = request.params["run_id"]
                    .as_str()
                    .context("run_id required")?;
                if request.params["interrupt_execution"].as_bool() == Some(true)
                    || controller.backend.has_active_turn(&session_id).await
                {
                    let _control = Arc::clone(&controller.coordinator.control)
                        .try_acquire_owned()
                        .context("Harness cleanup is still running")?;
                    controller.interrupt_checks(&session_id, id).await?;
                } else {
                    controller.checks.cancel(id)?;
                }
                json!({"run_id":id,"cancellation_requested":true})
            }
            CheckOperation::Override => controller
                .checks
                .set_overrides(task_id, serde_json::from_value(request.params.clone())?)?,
            CheckOperation::Run => {
                ensure!(
                    task.as_ref()
                        .is_none_or(|task| task.kind() != crate::task::TaskKind::Plan)
                        && controller.agent_coordinator.active_count() == 0,
                    "checks cannot race planning or writing agents"
                );
                let admission = Arc::clone(&controller.coordinator.execution)
                    .try_acquire_owned()
                    .context("finish active source work before checks")?;
                let run = controller.checks.prepare_request(
                    controller.checks.session_owner(),
                    crate::checks::RunRequest::try_from(serde_json::from_value::<
                        crate::checks::CheckRunRequest,
                    >(
                        request.params.clone()
                    )?)?,
                    task_id.map(str::to_owned),
                    session.shell,
                    "User requested".into(),
                )?;
                let sink = checks_event_sink(message_sink.clone(), session_id.clone());
                json!({"run_id":controller.checks.start(run,Some(sink),None,Some(admission)).await?})
            }
        };
        message_sink
            .send_response(Response::success(request.id, result)?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionAgentModelList {
        let provider = request.params["provider"]
            .as_str()
            .context("child model catalog requires provider")?;
        let provider = crate::backend::BackendKind::parse(provider)?;
        let catalog = controller.catalog_request.read().await.clone();
        let models = tokio::time::timeout(
            Duration::from_secs(30),
            controller.agent_backend_factory.catalog(
                provider,
                crate::backend::BackendRequest {
                    execution_owner: catalog.execution_owner,
                    harness_session_id: catalog.harness_session_id,
                    workspace: catalog.workspace,
                    input: crate::backend::BackendInput::from_text(String::new()),
                    mode: crate::backend::PromptMode::Chat,
                    model: "default".into(),
                    effort: "default".into(),
                    context_window: None,
                    service_tier: Default::default(),
                    access: catalog.access,
                    approval_mode: catalog.approval_mode,
                    backend_session_id: None,
                    submission: None,
                    recovered_input: Vec::new(),
                    control_context: None,
                },
            ),
        )
        .await
        .context("child model catalog exceeded 30 seconds")??;
        message_sink
            .send_response(Response::success(request.id, models)?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionHealthGet {
        message_sink
            .send_control_response(Response::success(
                request.id,
                json!({"session_id":session_id,"responsive":true}),
            )?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionCommandOperationGet {
        let id = request
            .params
            .get("operation_id")
            .and_then(Value::as_str)
            .context("operation_id is required")?;
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        message_sink
            .send_control_response(Response::success(
                request.id,
                store
                    .load_task_operation(&session_id, id)?
                    .map(|operation| operation.view()),
            )?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionPermissionSet {
        let mut request = request;
        request.params["action"] = json!("permission");
        if request.params.get("operation_id").is_none() {
            request.params["operation_id"] =
                json!(format!("permission:{session_id}:{}", request.id));
        }
        request.method = "session.command.apply".into();
        return route_task_transition(registry, controller, session_id, request, message_sink)
            .await;
    }
    if method == RuntimeMethod::SessionCommandApply {
        return route_task_transition(registry, controller, session_id, request, message_sink)
            .await;
    }
    if let Some(command) = session_command::SessionCommand::parse_request(&request)?
        && command.action.is_review()
    {
        if matches!(
            method,
            RuntimeMethod::SessionPlanAcceptanceBegin
                | RuntimeMethod::SessionPlanChangesRequest
                | RuntimeMethod::SessionPlanCancel
        ) && let Some((value, events)) = controller
            .revision_review
            .decide(method, request.params.clone())
            .await?
        {
            for event in events {
                message_sink.send_event(event).await?;
            }
            message_sink
                .send_response(Response::success(request.id, value)?)
                .await?;
            return Ok(());
        }
        return task_state::apply_command(
            &controller,
            &session_id,
            &command,
            Some(request.id),
            registry.initialize.goal_max_turns,
            message_sink,
        )
        .await;
    }
    if method.requires_provider_fork() {
        registry
            .await_provider_fork(&session_id, &controller)
            .await?;
    }
    let edit_trace = matches!(
        method,
        RuntimeMethod::SessionPlanTestDelete
            | RuntimeMethod::SessionPlanCheckUpdate
            | RuntimeMethod::SessionPlanEntityRename
    )
    .then(|| {
        crate::plan::edit_trace::PlanEditTrace::new(
            registry.runtime.trace(),
            &session_id,
            &request.method,
            &request.params,
        )
    });
    if route_control_request(
        &controller,
        &request,
        method,
        message_sink,
        syntax,
        edit_trace.as_ref(),
        admission,
    )
    .await?
    {
        return Ok(());
    }

    let (event_sink, mut event_stream) = crate::backend::events::channel();
    let (mut broker, _admission) = acquire_request_broker(&controller, method).await?;
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
    let shutdown = method == RuntimeMethod::HostShutdown;
    let mut result = Box::pin(broker.dispatch_stream(request, event_sink)).await;
    event_forwarder.await??;
    for event in result.event.drain(..) {
        message_sink.send_event(event).await?;
    }
    let catalog_request = broker.backend_catalog_request();
    drop(broker);
    *controller.catalog_request.write().await = catalog_request;

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
    controller.coordinator.submissions.changed.notify_one();
    publish_execution_completion(
        message_sink,
        result.response,
        admission,
        prompt_admission,
        shutdown,
    )
    .await
}

/// Release finalized execution ownership before its response can admit the next prompt.
async fn publish_execution_completion(
    sink: &MessageSender,
    response: Response,
    admission: &mut Option<ExecutionPermit>,
    prompt_admission: &mut Option<PromptAdmission>,
    terminal: bool,
) -> Result<()> {
    drop(prompt_admission.take());
    drop(admission.take());
    if terminal {
        sink.send_terminal_response(response)?;
    } else {
        sink.send_response(response).await?;
    }
    Ok(())
}

fn admit_task_intent(
    controller: &SessionController,
    session_id: &str,
    request: &Request,
    store: &mut SqliteStore,
) -> Result<crate::task::TaskOperation> {
    let id = request
        .params
        .get("operation_id")
        .and_then(Value::as_str)
        .filter(|id| !id.is_empty() && id.len() <= 128)
        .context("task transition requires operation_id")?
        .to_owned();
    if let Some(previous) = store.load_task_operation(session_id, &id)? {
        ensure!(
            previous.action == request.params,
            "operation ID was reused for a different action"
        );
        return Ok(previous);
    }
    let action = request
        .params
        .get("action")
        .and_then(Value::as_str)
        .context("task action is required")?;
    ensure!(
        [
            "plan",
            "execute",
            "goal",
            "fork",
            "attach_plan",
            "resume",
            "pause",
            "clear",
            "undo",
            "permission",
            "configure",
            "compact"
        ]
        .contains(&action),
        "unknown task action"
    );
    if action == "undo" {
        let rollback = &request.params["rollback"];
        ensure!(
            rollback["scope"] == "conversation",
            "undo requires conversation rollback scope"
        );
        let selected = rollback["exchange_id"]
            .as_str()
            .filter(|id| !id.is_empty())
            .context("undo requires exchange_id")?;
        let latest = rollback["latest_exchange_id"]
            .as_str()
            .context("undo requires latest_exchange_id")?;
        let exchanges = store.list_exchange(session_id)?;
        ensure!(
            exchanges
                .last()
                .is_some_and(|exchange| exchange.id == latest),
            "conversation changed; select the rollback interaction again"
        );
        ensure!(
            exchanges.iter().any(|exchange| exchange.id == selected
                && exchange.disposition() == crate::exchange::HistoryDisposition::Current),
            "selected interaction is unavailable for rollback"
        );
    }
    if action == "permission" {
        let _: crate::task::PermissionChange = serde_json::from_value(
            request
                .params
                .get("change")
                .cloned()
                .context("permission change is required")?,
        )?;
    }
    if matches!(action, "plan" | "goal") {
        ensure!(
            request
                .params
                .get("text")
                .and_then(Value::as_str)
                .is_some_and(|text| !text.trim().is_empty()),
            "task objective is required"
        );
    }
    let session = crate::session::SessionStore::load_session(store, session_id)?
        .context("session is missing")?;
    let prior = store.latest_task_operation(session_id)?;
    let pending = prior.as_ref().filter(|operation| {
        matches!(
            operation.state.as_str(),
            "admitted"
                | "accepted"
                | "stopping"
                | "running"
                | "applying"
                | "verifying"
                | "resuming"
        )
    });
    let running_intent = pending
        .map(|operation| operation.resume_after_transition)
        .unwrap_or_else(|| controller.coordinator.execution.available_permits() == 0);
    let resume_after_transition = if matches!(action, "pause" | "clear" | "undo") {
        false
    } else if matches!(
        action,
        "plan" | "execute" | "goal" | "fork" | "attach_plan" | "resume"
    ) {
        true
    } else {
        running_intent
    };
    let permission_selection = if action == "permission" {
        let mut access = session.access;
        let mut approval = session.approval_mode;
        if let Some(selection) = pending.and_then(|operation| operation.permission_selection) {
            access.sandbox = selection.sandbox;
            access.read_access = selection.read_access;
            access.write_access = selection.write_access;
            approval = selection.approval_mode;
        }
        let change: crate::task::PermissionChange =
            serde_json::from_value(request.params["change"].clone())?;
        change.apply(
            &mut access,
            &mut approval,
            crate::backend::BackendKind::parse(&session.backend)?.supports_sandbox(),
        );
        Some(crate::task::PermissionSelection {
            sandbox: access.sandbox,
            read_access: access.read_access,
            write_access: access.write_access,
            approval_mode: approval,
        })
    } else {
        None
    };
    let operation = crate::task::TaskOperation {
        revision: 0,
        id,
        session_id: session_id.to_owned(),
        action: request.params.clone(),
        permission_selection,
        state: "admitted".into(),
        error: None,
        exchange_id: None,
        created_at_ms: SystemTime::now().duration_since(UNIX_EPOCH)?.as_millis() as i64,
        resume_after_transition,
    };
    store.admit_task_operation(&operation)?;
    if matches!(action, "pause" | "clear" | "undo") {
        controller
            .coordinator
            .submissions
            .suspend(true, Some(&operation.id))?;
    }
    Ok(operation)
}

async fn route_task_transition(
    registry: Arc<SessionControllerRegistry>,
    controller: Arc<SessionController>,
    session_id: String,
    request: Request,
    sink: &MessageSender,
) -> Result<()> {
    if task_state::apply(
        &controller,
        &session_id,
        &request,
        registry.initialize.goal_max_turns,
        sink,
    )
    .await?
    {
        return Ok(());
    }
    let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let mut operation = admit_task_intent(&controller, &session_id, &request, &mut store)?;
    let admitted = store.claim_task_operation(&session_id, &operation.id)?;
    if !admitted {
        sink.send_control_response(Response::success(
            request.id,
            store
                .load_task_operation(&session_id, &operation.id)?
                .map(|operation| operation.view()),
        )?)
        .await?;
        return Ok(());
    }
    operation.state = "accepted".into();
    operation.revision += 1;
    let result = async {
        sink.send_control_response(Response::success(request.id, operation.view())?)
            .await?;
        if operation.action["action"] == "configure" {
            controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
        }
        if let Some(revision) = operation
            .action
            .pointer("/config/settings_revision")
            .and_then(Value::as_u64)
        {
            crate::settings::SettingsStore::open(
                PathBuf::from(&registry.initialize.data_root).as_path(),
            )?
            .begin_application(&session_id, &operation.id, revision)?;
        }
        let exchange_list = store.list_exchange(&session_id)?;
        let mut active = exchange_list.iter().filter(|exchange| {
            !exchange.is_finished()
                && exchange.state() != crate::exchange::ExchangeState::Queued
                && exchange.disposition() == crate::exchange::HistoryDisposition::Current
        });
        operation.exchange_id = active.next().map(|exchange| exchange.id.clone());
        ensure!(
            active.next().is_none(),
            "configuration has multiple active exchange owners"
        );
        if operation.exchange_id.is_none()
            && let Some(run_id) = controller.checks.active()?
        {
            operation.exchange_id = controller
                .checks
                .status(&run_id, 0, false)
                .await?
                .owner
                .exchange_id()
                .map(str::to_owned);
        }
        retain_configuration_event(&mut store, &operation)?;
        Box::pin(execute_task_transition(
            &registry,
            &controller,
            &mut operation,
            &mut store,
            sink,
        ))
        .await
    }
    .await;
    if operation.action["action"] == "configure" {
        controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
    }
    match result {
        Ok(()) => {
            if operation.state != "superseded" {
                operation.state = "completed".into();
            }
        }
        Err(error) if error.is::<crate::broker::TurnCancelled>() => {
            operation.state = "cancelled".into();
            operation.error = None;
        }
        Err(error) => {
            operation.state = "failed".into();
            operation.error = Some(format!("{error:#}"));
        }
    }
    if let Err(error) = store.save_task_operation(&mut operation) {
        operation.state = "outcome_unknown".into();
        operation.error = Some(format!(
            "Task outcome could not be persisted: {error:#}. Effects may have occurred"
        ));
    }
    retain_configuration_event(&mut store, &operation)?;
    if operation.state == "completed"
        && matches!(
            operation.action["action"].as_str(),
            Some("resume" | "plan" | "execute" | "goal" | "fork" | "attach_plan")
        )
    {
        let update = controller
            .coordinator
            .submissions
            .suspend(false, Some(&operation.id))?;
        controller.publish_input(update, sink).await?;
    }
    controller.coordinator.submissions.changed.notify_one();
    if operation
        .action
        .pointer("/config/settings_revision")
        .is_some()
    {
        crate::settings::SettingsStore::open(
            PathBuf::from(&registry.initialize.data_root).as_path(),
        )?
        .finish_application(&session_id, &operation.id, &operation.state)?;
    }
    sink.send_control_wait(Message::Event(SessionEvent {
        session_id,
        event: "task_operation".into(),
        payload: operation.view(),
    }))
    .await?;
    Ok(())
}

fn retain_configuration_event(
    store: &mut SqliteStore,
    operation: &crate::task::TaskOperation,
) -> Result<()> {
    if operation.action["action"] != "configure" {
        return Ok(());
    }
    let existing = store
        .list_session_event(&operation.session_id)?
        .into_iter()
        .find(|event| event.id == format!("configuration-{}", operation.id));
    let created_at_ms = existing
        .as_ref()
        .map(|event| event.created_at_ms)
        .unwrap_or(operation.created_at_ms);
    let owner = match existing {
        Some(event) => event.owner,
        None => match operation.exchange_id.as_deref() {
            Some(exchange_id) => {
                store.capture_timeline_owner(&operation.session_id, exchange_id)?
            }
            None => crate::timeline::TimelineOwner::session(&operation.session_id),
        },
    };
    store.save_session_event(&crate::timeline::SessionEventRecord {
        id: format!("configuration-{}", operation.id),
        session_id: operation.session_id.clone(),
        owner,
        created_at_ms,
        detail: crate::timeline::SessionEventKind::Configuration {
            operation_id: operation.id.clone(),
            state: operation.state.clone(),
            field: operation.action["config"]["setting_fields"]
                .as_array()
                .map(|field| {
                    field
                        .iter()
                        .filter_map(|field| field.as_str().map(str::to_owned))
                        .collect()
                })
                .unwrap_or_else(|| {
                    operation.action["config"]
                        .as_object()
                        .map(|config| config.keys().cloned().collect())
                        .unwrap_or_default()
                }),
            reason: operation.error.clone(),
        },
    })
}

async fn execute_task_transition(
    registry: &SessionControllerRegistry,
    controller: &SessionController,
    operation: &mut crate::task::TaskOperation,
    store: &mut SqliteStore,
    sink: &MessageSender,
) -> Result<()> {
    let mut action = operation.action.clone();
    if let Some(selection) = operation.permission_selection {
        action["permission_selection"] = serde_json::to_value(selection)?;
    }
    if matches!(
        action["action"].as_str(),
        Some("permission" | "configure" | "compact")
    ) {
        action["resume"] = json!(operation.resume_after_transition);
    }
    while store.pending_policy_predecessor(&operation.session_id, &operation.id)? {
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
    let policy_change = matches!(action["action"].as_str(), Some("configure" | "permission"));
    let control = Arc::clone(&controller.coordinator.control)
        .acquire_owned()
        .await?;
    if !policy_change
        && store
            .latest_task_operation(&operation.session_id)?
            .is_none_or(|latest| latest.id != operation.id)
    {
        operation.state = "superseded".into();
        return Ok(());
    }
    operation.state = "stopping".into();
    store.save_task_operation(operation)?;
    retain_configuration_event(store, operation)?;
    let changes_child_policy = action["action"] == "permission"
        || (action["action"] == "configure"
            && action["config"].as_object().is_some_and(|config| {
                config.keys().any(|field| {
                    matches!(
                        field.as_str(),
                        "access" | "approval_mode" | "sandbox_enabled" | "backend" | "provider"
                    )
                }) || config
                    .get("setting_fields")
                    .and_then(Value::as_array)
                    .is_some_and(|fields| {
                        fields.iter().filter_map(Value::as_str).any(|field| {
                            matches!(
                                field.split('.').next(),
                                Some(
                                    "access"
                                        | "approval_mode"
                                        | "sandbox_enabled"
                                        | "backend"
                                        | "provider"
                                )
                            )
                        })
                    })
            }));
    let child_quiescence = if changes_child_policy
        || matches!(
            action["action"].as_str(),
            Some("pause" | "clear" | "stop" | "cancel" | "undo" | "verify")
        ) {
        Some(quiesce_children(controller).await?)
    } else {
        None
    };
    let execution = match Arc::clone(&controller.coordinator.execution).try_acquire_owned() {
        Ok(execution) => execution,
        Err(tokio::sync::TryAcquireError::Closed) => {
            anyhow::bail!("session execution admission is closed")
        }
        Err(tokio::sync::TryAcquireError::NoPermits) => {
            let restart = operation.resume_after_transition
                && matches!(
                    action["action"].as_str(),
                    Some("permission" | "configure" | "compact")
                );
            let restore_prompt = action["action"] == "pause"
                && action["restore_prompt_if_no_output"].as_bool() == Some(true);
            controller
                .cancellation
                .begin_cleanup(restart, restore_prompt)?;
            if let Some(run_id) = controller.checks.active()? {
                controller.checks.cancel(&run_id)?;
            }
            let approval_owner = crate::backend::ExecutionOwner::root(
                &controller.catalog_request.read().await.harness_session_id,
            );
            controller
                .permission
                .cancel_owner(&approval_owner, None)
                .await?;
            if let Err(error) = controller
                .backend
                .cleanup_execution(&operation.session_id)
                .await
            {
                controller.cancellation.fail_cleanup(&error);
                return Err(error.context("collect current task before transition"));
            }
            controller.cancellation.request(restore_prompt);
            // Execution ownership includes finalization and response delivery, independently of read-only broker locks.
            Arc::clone(&controller.coordinator.execution)
                .acquire_owned()
                .await?
        }
    };
    operation.exchange_id = store
        .list_exchange(&operation.session_id)?
        .last()
        .map(|exchange| exchange.id.clone());
    if !policy_change
        && store
            .latest_task_operation(&operation.session_id)?
            .is_none_or(|latest| latest.id != operation.id)
    {
        operation.state = "superseded".into();
        return Ok(());
    }
    controller.cancellation.arm(false);
    if policy_change
        && store
            .latest_task_operation(&operation.session_id)?
            .is_some_and(|latest| latest.id != operation.id)
    {
        action["resume"] = json!(false);
    }
    // Release child collection after acquiring exclusive execution, before the transition resumes work.
    drop(child_quiescence);
    drop(control);
    operation.state = "running".into();
    store.save_task_operation(operation)?;
    sink.send_event(SessionEvent {
        session_id: operation.session_id.clone(),
        event: "task_operation".into(),
        payload: operation.view(),
    })
    .await?;
    let mut broker = controller.broker.lock().await;
    let request = Request {
        id: 0,
        method: "session.command.apply".into(),
        params: action,
    };
    let outcome = dispatch_task_attempt(
        &mut broker,
        &operation.session_id,
        request,
        sink,
        Arc::clone(&controller.catalog_request),
    )
    .await;
    *controller.catalog_request.write().await = broker.backend_catalog_request();
    drop(broker);
    operation.exchange_id = store
        .list_exchange(&operation.session_id)?
        .last()
        .map(|exchange| exchange.id.clone());
    outcome?;
    drop(execution);
    let _ = registry;
    Ok(())
}

async fn dispatch_task_attempt(
    broker: &mut HarnessBroker,
    session_id: &str,
    request: Request,
    sink: &MessageSender,
    catalog_request: Arc<RwLock<crate::backend::BackendCatalogRequest>>,
) -> Result<bool> {
    let (event_sink, mut event_stream) = crate::backend::events::channel();
    let output = sink.clone();
    let routed_session = session_id.to_owned();
    let data_root = broker.data_root().to_path_buf();
    let forwarder = tokio::spawn(async move {
        while let Some(event) = event_stream.recv().await? {
            if event.kind == "configuration_progress" {
                let operation_id = event.data["operation_id"]
                    .as_str()
                    .context("configuration operation is missing")?;
                let state = event.data["state"]
                    .as_str()
                    .context("configuration state is missing")?;
                anyhow::ensure!(
                    matches!(state, "applying" | "verifying" | "resuming"),
                    "invalid configuration state"
                );
                let mut store = SqliteStore::open(&data_root)?;
                let mut operation = store
                    .load_task_operation(&routed_session, operation_id)?
                    .context("configuration operation is missing")?;
                operation.state = state.into();
                store.save_task_operation(&mut operation)?;
                retain_configuration_event(&mut store, &operation)?;
                output
                    .send_control_wait(Message::Event(SessionEvent {
                        session_id: routed_session.clone(),
                        event: "task_operation".into(),
                        payload: operation.view(),
                    }))
                    .await?;
                continue;
            }
            if event.kind == "configuration_applied" {
                *catalog_request.write().await =
                    serde_json::from_value(event.data["catalog"].clone())?;
                output
                    .send_event(SessionEvent {
                        session_id: routed_session.clone(),
                        event: "session_configured".into(),
                        payload: event.data,
                    })
                    .await?;
                continue;
            }
            let (name, payload) = if event.kind == "document_changed" {
                ("document_changed", event.data)
            } else {
                ("backend_event", serde_json::to_value(event)?)
            };
            output
                .send_event(SessionEvent {
                    session_id: routed_session.clone(),
                    event: name.into(),
                    payload,
                })
                .await?;
        }
        Ok::<(), anyhow::Error>(())
    });
    let result = Box::pin(broker.dispatch_stream(request, event_sink)).await;
    forwarder.await??;
    let continuing = result
        .event
        .iter()
        .any(|event| event.event == "goal_continue_requested");
    for event in result.event {
        sink.send_event(event).await?;
    }
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

async fn start_session_fork(
    registry: Arc<SessionControllerRegistry>,
    session_id: String,
    params: Value,
    response_id: Option<u64>,
    purpose: crate::session::SessionPurpose,
    message_sink: &MessageSender,
) -> Result<(Arc<SessionController>, Arc<ProviderForkGate>)> {
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
        &params,
        purpose,
    )?;
    let child_session_id = preparation.child.id.clone();
    let child_controller = registry.resolve(&child_session_id).await?;
    let gate = ProviderForkGate::new();
    registry
        .provider_fork_by_id
        .write()
        .await
        .insert(child_session_id.clone(), Arc::clone(&gate));

    if let Some(response_id) = response_id {
        let mut snapshot = serde_json::to_value(child_controller.broker.lock().await.snapshot()?)?;
        snapshot["fork_performance"] = json!({
            "total_ms": preparation.total_duration_ms,
            "timing": preparation.timing,
            "provider_pending": true,
        });
        message_sink
            .send_response(Response::success(response_id, snapshot)?)
            .await?;
    }
    message_sink.send(Message::Event(SessionEvent {
        session_id: child_session_id.clone(),
        event: "session_created".into(),
        payload: serde_json::to_value(&preparation.child)?,
    }))?;

    let message_sink = message_sink.clone();
    let registry_for_completion = Arc::clone(&registry);
    let child_result = Arc::clone(&child_controller);
    let gate_result = Arc::clone(&gate);
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
    Ok((child_result, gate_result))
}

/// Retires only an idle local controller before acquiring durable deletion ownership.
async fn delete_session(
    registry: Arc<SessionControllerRegistry>,
    source_session_id: &str,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let target = request
        .params
        .get("session_id")
        .and_then(Value::as_str)
        .context("session.delete requires session_id")?
        .to_owned();
    ensure!(
        target != source_session_id,
        "cannot delete the active session"
    );
    let mut controllers = registry.controller_by_id.write().await;
    if let Some(controller) = controllers.get(&target).cloned() {
        let _control = controller
            .coordinator
            .control
            .clone()
            .try_acquire_owned()
            .context("cannot delete a session with active control work")?;
        let _execution = controller
            .coordinator
            .execution
            .clone()
            .try_acquire_owned()
            .context("cannot delete a session with active execution")?;
        ensure!(
            controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty(),
            "cannot delete a session with queued input"
        );
        ensure!(
            controller.agent_coordinator.active_count() == 0,
            "cannot delete a session with active agents"
        );
        ensure!(
            controller.checks.active()?.is_none(),
            "cannot delete a session with active checks"
        );
        let mut broker = controller
            .broker
            .try_lock()
            .context("cannot delete a session with active work")?;
        broker.release_lease()?;
        controller.input_stopping.store(true, Ordering::Release);
        controller.coordinator.submissions.changed.notify_one();
        controller.coordinator.execution.close();
        controller.coordinator.control.close();
        for task in [
            controller.input_task.lock().await.take(),
            controller.timeout_task.lock().await.take(),
            controller.agent_task.lock().await.take(),
            controller.compaction_task.lock().await.take(),
        ]
        .into_iter()
        .flatten()
        {
            task.abort();
            let _ = task.await;
        }
        controllers.remove(&target);
    }
    drop(controllers);
    let mut store = SqliteStore::open(std::path::Path::new(&registry.initialize.data_root))?;
    let session = store.load_session(&target)?.context("session not found")?;
    let plans = crate::plan::PlanFileStore::new(
        std::path::Path::new(&registry.initialize.data_root),
        &session.workspace,
    );
    store.delete_session_with(&target, || plans.delete_session(&target))?;
    message_sink
        .send_response(Response::success(
            request.id,
            json!({"deleted":target,"provider_session_preserved":true}),
        )?)
        .await?;
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
    let snapshot = controller.observer.snapshot()?;
    message_sink
        .send_response(Response::success(request.id, snapshot)?)
        .await?;
    Ok(())
}

async fn acquire_request_broker(
    controller: &SessionController,
    method: RuntimeMethod,
) -> Result<(MutexGuard<'_, HarnessBroker>, Option<ExecutionPermit>)> {
    let admission = if method.requires_provider_fork() {
        None
    } else {
        Some(Arc::clone(&controller.coordinator.execution).try_acquire_owned()
            .map_err(|_| crate::broker::RequestNotAdmitted(
                "This operation requires a settled session. Use a session command while execution is active"))?)
    };
    let broker = tokio::time::timeout(Duration::from_secs(10), controller.broker.lock())
        .await
        .context("Session maintenance did not release the broker within 10 seconds")?;
    Ok((broker, admission))
}

async fn quiesce_children(controller: &SessionController) -> Result<crate::agent::AgentQuiescence> {
    let quiescence = controller.agent_coordinator.begin_quiescence();
    if controller.agent_coordinator.active_count() == 0 {
        return Ok(quiescence);
    }
    let mut activity = controller.agent_coordinator.activity();
    tokio::time::timeout(Duration::from_secs(35), async {
        controller
            .agent_coordinator
            .sender()
            .request(crate::agent::AgentRequest {
                operation_id: uuid::Uuid::new_v4().to_string(),
                source: crate::agent::AgentSource::User,
                operation: crate::agent::AgentOperation::InterruptAll,
            })
            .await?;
        loop {
            if controller.agent_coordinator.active_count() == 0 {
                return Ok::<_, anyhow::Error>(());
            }
            for (agent, _) in controller.agent_coordinator.snapshot(None) {
                if let Some(task) = controller.agent_coordinator.cleanup_task(&agent.id) {
                    anyhow::bail!(
                        "child {} still requires cleanup: {}",
                        agent.task_path,
                        task.error.unwrap_or_default()
                    );
                }
            }
            activity
                .changed()
                .await
                .context("child activity owner closed during cleanup")?;
        }
    })
    .await
    .context("children did not release execution ownership within 35 seconds")??;
    Ok(quiescence)
}

async fn route_control_request(
    controller: &SessionController,
    request: &Request,
    method: RuntimeMethod,
    message_sink: &MessageSender,
    syntax: &Arc<SyntaxEngine>,
    trace: Option<&crate::plan::edit_trace::PlanEditTrace>,
    admission: &mut Option<ExecutionPermit>,
) -> Result<bool> {
    if matches!(
        method,
        RuntimeMethod::SessionQuestionAnswer | RuntimeMethod::SessionQuestionSkip
    ) && request.params.get("target").is_some()
    {
        let started = Instant::now();
        let mut params = request.params.clone();
        if method == RuntimeMethod::SessionQuestionSkip {
            params["response"] = json!({"kind":"skipped"});
        }
        let session_id = controller
            .catalog_request
            .read()
            .await
            .harness_session_id
            .clone();
        let root = controller.data_root.clone();
        let owner = session_id.clone();
        let result = tokio::task::spawn_blocking(move || {
            SqliteStore::open(&root)?.answer_question_command(&owner, &params)
        })
        .await
        .context("question answer worker failed")??;
        controller.trace.record(&session_id, "question.answer.committed", json!({"duration_ms":started.elapsed().as_millis(),"operation_id":result["operation_id"]}));
        message_sink
            .send_control_response(Response::success(request.id, &result)?)
            .await?;
        message_sink
            .send_event(SessionEvent {
                session_id,
                event: "question_updated".into(),
                payload: result["active_elicitation"].clone(),
            })
            .await?;
        return Ok(true);
    }
    if matches!(
        method,
        RuntimeMethod::SessionPlanTestDelete
            | RuntimeMethod::SessionPlanCheckUpdate
            | RuntimeMethod::SessionPlanEntityRename
    ) {
        let dispatch = trace.map(|trace| trace.stage("service.dispatch_edit"));
        let revision_lane = trace.map(|trace| trace.stage("service.revision_lane"));
        let (mut response, events) = if let Some((value, events)) = controller
            .revision_review
            .decide(method, request.params.clone())
            .await?
        {
            if let Some(stage) = revision_lane {
                stage.complete(json!({"source":"revision_review"}));
            }
            (Response::success(request.id, value)?, events)
        } else {
            if let Some(stage) = revision_lane {
                stage.complete(json!({"source":"broker"}));
            }
            let wait = trace.map(|trace| trace.stage("service.broker_lock_wait"));
            let (mut broker, _admission) = acquire_request_broker(controller, method).await?;
            if let Some(stage) = wait {
                stage.complete(json!({}));
            }
            let result = broker.dispatch(request.clone()).await;
            (result.response, result.event)
        };
        if let Some(stage) = dispatch {
            stage.complete(
                json!({"status":if response.result().is_some() { "completed" } else { "failed" }}),
            );
        }
        let event_stage = trace.map(|trace| trace.stage("service.send_events"));
        let event_count = events.len();
        for event in events {
            message_sink.send_event(event).await?;
        }
        if let Some(stage) = event_stage {
            stage.complete(json!({"count":event_count}));
        }
        if let (Some(document), Some(mut value)) = (
            request.params.get("review_document"),
            response.result().cloned(),
        ) {
            let document = serde_json::from_value(document.clone())?;
            let update = async {
                let plan_id = value["plan"]["id"].as_str().context("updated plan identity is missing")?;
                let digest = value["plan"]["review_digest"].as_str().context("updated plan digest is missing")?;
                let capture = trace.map(|trace| trace.stage("service.recapture_review"));
                let source = if let Some(source) = controller.revision_review.capture(plan_id, digest, None).await? {
                    source
                } else {
                    let wait = trace.map(|trace| trace.stage("service.recapture_lock_wait"));
                    if let Some(stage) = wait { stage.complete(json!({})); }
                    controller.observer.capture_plan_review(plan_id, digest)?
                };
                if let Some(stage) = capture { stage.complete(json!({"count":source.document.design.as_ref().map_or(0, |design|design.proposed.len())})); }
                let mut source = crate::plan::review_source::EditableReviewSource::try_from(source)?;
                let expected_version = crate::plan::edit_trace::measure(trace, "service.reuse_review_caches", || controller.plan_review.prepare_update(&document, &mut source))?;
                let syntax_stage = trace.map(|trace| trace.stage("service.syntax"));
                analyze_plan_source(&mut source, syntax, || Ok(()), trace).await?;
                if let Some(stage) = syntax_stage { stage.complete(json!({"count":source.declaration_syntax.len()})); }
                controller.plan_review.update(&document, source, expected_version, trace)
            }.await;
            match update {
                Ok(update) => value["review"] = update,
                Err(error) => {
                    value["review_error"] = Value::String(format!(
                        "Plan changed, but the open review could not update: {error:#}"
                    ))
                }
            }
            response = crate::plan::edit_trace::measure(trace, "service.build_response", || {
                Ok(Response::success(request.id, value)?)
            })?;
        }
        let send = trace.map(|trace| trace.stage("service.send_response"));
        message_sink.send_response(response).await?;
        if let Some(stage) = send {
            stage.complete(json!({}));
        }
        return Ok(true);
    }
    use crate::agent::AgentOperation;
    let text = |field: &str| {
        request.params[field]
            .as_str()
            .filter(|value| !value.trim().is_empty())
            .map(str::to_owned)
            .with_context(|| format!("{} requires {field}", request.method))
    };
    let operation = match method {
        RuntimeMethod::SessionAgentStart => {
            let mut arguments = request.params.clone();
            if let Some(arguments) = arguments.as_object_mut() {
                arguments.remove("operation_id");
            }
            Some(AgentOperation::Spawn(serde_json::from_value(arguments)?))
        }
        RuntimeMethod::SessionAgentMessageSend => Some(AgentOperation::Message {
            target: text("target")?,
            message: text("message")?,
        }),
        RuntimeMethod::SessionAgentFollowup => Some(AgentOperation::Followup {
            target: text("target")?,
            message: text("message")?,
        }),
        RuntimeMethod::SessionAgentInterrupt => Some(AgentOperation::Interrupt {
            target: text("target")?,
        }),
        RuntimeMethod::SessionAgentInterruptAll => Some(AgentOperation::InterruptAll),
        _ => None,
    };
    if let Some(operation) = operation {
        let result = controller
            .agent_coordinator
            .sender()
            .request(crate::agent::AgentRequest {
                operation_id: request.params["operation_id"]
                    .as_str()
                    .map(str::to_owned)
                    .unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
                source: crate::agent::AgentSource::User,
                operation,
            })
            .await?;
        drop(admission.take());
        message_sink
            .send_response(Response::success(request.id, result)?)
            .await?;
        return Ok(true);
    }
    if matches!(
        method,
        RuntimeMethod::SessionPlanAcceptanceBegin
            | RuntimeMethod::SessionPlanChangesRequest
            | RuntimeMethod::SessionPlanCancel
    ) && let Some((value, events)) = controller
        .revision_review
        .decide(method, request.params.clone())
        .await?
    {
        for event in events {
            message_sink.send_event(event).await?;
        }
        drop(admission.take());
        message_sink
            .send_response(Response::success(request.id, value)?)
            .await?;
        return Ok(true);
    }
    let _control = if matches!(
        method,
        RuntimeMethod::SessionTurnCancel
            | RuntimeMethod::SessionTurnRestart
            | RuntimeMethod::SettingsMcpSetEnabled
    ) {
        Some(
            Arc::clone(&controller.coordinator.control)
                .try_acquire_owned()
                .context("Harness cleanup is already running")?,
        )
    } else {
        None
    };
    let catalog_request = controller.catalog_request.read().await.clone();
    let response = match method {
        RuntimeMethod::SessionTurnCancel => {
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
                    message_sink.send_control_response(result.response).await?;
                    return Ok(true);
                }
                controller
                    .cancellation
                    .begin_cleanup(false, restore_prompt)?;
                let approval_owner = crate::backend::ExecutionOwner::root(
                    &controller.catalog_request.read().await.harness_session_id,
                );
                controller
                    .permission
                    .cancel_owner(&approval_owner, None)
                    .await?;
                let cleanup = controller
                    .backend
                    .cleanup_execution(&catalog_request.harness_session_id)
                    .await;
                if let Err(error) = cleanup {
                    controller.cancellation.fail_cleanup(&error);
                    return Err(error);
                }
                controller.cancellation.request(restore_prompt);
                let approval_owner = crate::backend::ExecutionOwner::root(
                    &controller.catalog_request.read().await.harness_session_id,
                );
                controller
                    .permission
                    .cancel_owner(&approval_owner, None)
                    .await?;
            }
            Some(Response::success(
                request.id,
                json!({ "cancel_requested": true }),
            )?)
        }
        RuntimeMethod::SessionTurnRestart => {
            let _child_quiescence = quiesce_children(controller).await?;
            controller.cancellation.begin_cleanup(true, false)?;
            let approval_owner = crate::backend::ExecutionOwner::root(
                &controller.catalog_request.read().await.harness_session_id,
            );
            controller
                .permission
                .cancel_owner(&approval_owner, None)
                .await?;
            if let Err(error) = controller
                .backend
                .cleanup_execution(&catalog_request.harness_session_id)
                .await
            {
                controller.cancellation.fail_cleanup(&error);
                return Err(error);
            }
            controller.cancellation.request_restart();
            let approval_owner = crate::backend::ExecutionOwner::root(
                &controller.catalog_request.read().await.harness_session_id,
            );
            controller
                .permission
                .cancel_owner(&approval_owner, None)
                .await?;
            Some(Response::success(
                request.id,
                json!({ "restart_requested": true, "mode": request.params.get("mode") }),
            )?)
        }
        RuntimeMethod::SessionTurnSteer => {
            let text = request
                .params
                .get("text")
                .and_then(Value::as_str)
                .filter(|text| !text.trim().is_empty())
                .context("turn.steer requires non-empty text")?
                .to_owned();
            validate_root_control(request.params.get("target"))?;
            let attachments =
                crate::attachment::resolve_input(&controller.data_root, &request.params).await?;
            let input = crate::backend::BackendInput::from_text(text.clone())
                .with_attachments(attachments.clone());
            let provider = controller.backend.descriptor().kind;
            let submission_catalog = catalog_request.clone();
            let submission = controller
                .submission_store
                .persist(move |journal| {
                    journal.prepare_steering(&submission_catalog, input, provider)
                })
                .await?;
            let input = crate::backend::SteeringInput::User {
                text,
                attachments,
                submission: Some(submission),
            };
            if controller.parent_input.is_active() {
                controller.parent_input.steer(input).await?;
            } else {
                controller
                    .backend
                    .steer_session(&catalog_request.harness_session_id, input)
                    .await?;
            }
            Some(Response::success(
                request.id,
                json!({ "steered": true, "delivery": "direct" }),
            )?)
        }
        RuntimeMethod::SessionApprovalResolve => {
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
        RuntimeMethod::SessionProviderSkillList => Some(Response::success(
            request.id,
            tokio::time::timeout(
                Duration::from_secs(30),
                controller.backend.skill_list(catalog_request.clone()),
            )
            .await
            .context("skill discovery exceeded 30 seconds")??,
        )?),
        RuntimeMethod::SessionProviderMcpList => {
            let revision = controller.mcp_revision.load(Ordering::Acquire);
            let mut discovery = controller.mcp_discovery.lock().await;
            if discovery.0 != revision {
                *discovery = (revision, Default::default());
            }
            let server_list = discovery
                .1
                .snapshot(controller.backend.clone(), catalog_request.clone())
                .await?;
            Some(Response::success(request.id, server_list)?)
        }
        _ => None,
    };
    if let Some(response) = response {
        if matches!(
            method,
            RuntimeMethod::SessionTurnCancel | RuntimeMethod::SessionTurnRestart
        ) {
            message_sink.send_control_response(response).await?;
        } else {
            message_sink.send_response(response).await?;
        }
        return Ok(true);
    }
    Ok(false)
}

fn initialize_failure(request_id: u64, error: &anyhow::Error) -> Response {
    error.downcast_ref::<SessionLeaseConflict>().map_or_else(
        || {
            Response::failure(
                request_id,
                "initialize_failed",
                crate::storage::error_message(error),
            )
        },
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

#[cfg(test)]
mod tests {
    include!("service/task_state_test.rs");
    use super::*;
    use crate::backend::{
        Backend, BackendCatalogRequest, BackendDescriptor, BackendEventSink, BackendKind,
        BackendOutput, BackendRequest, TextGeneration,
    };
    use forge_diff::cache::CacheLimits;

    #[tokio::test]
    async fn lsp_callbacks_enforce_session_and_request_ownership() -> Result<()> {
        async fn callback(
            service: &HarnessService,
            session_id: &str,
            request: Request,
            sink: &MessageSender,
            output: &mut forge_protocol::outbound::MessageReceiver,
        ) -> Result<Value> {
            let envelope = json!({"id":request.id,"method":request.method,
                "params":request.params,"session_id":session_id});
            service
                .dispatch(Some(session_id.into()), request, sink)
                .await?;
            let response: Value = serde_json::from_slice(
                output
                    .recv()
                    .await?
                    .context("LSP callback response missing")?
                    .bytes(),
            )?;
            assert_eq!(response["id"], envelope["id"]);
            assert!(response.get("error").is_none(), "{response}");
            eprintln!(
                "API_CAPTURE {}",
                json!({"request":envelope,"response":response})
            );
            Ok(response["result"].clone())
        }

        let fixture = tempfile::tempdir()?;
        let source = "pub fn callback_source() {}\n";
        let path = fixture.path().join("source.rs");
        std::fs::write(&path, source)?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        registry.runtime.lsp.attach(sink.clone());
        registry.runtime.lsp.set_enabled(&session_id, true);
        let policy = Arc::new(
            crate::permissions::matcher::CompiledPermissionDocument::compile(
                crate::permissions::document::default_permission_document(),
                fixture.path(),
            ),
        );
        let (scope, lease) = registry.runtime.lsp.scope(
            session_id.clone(),
            "callback-interaction".into(),
            fixture.path().into(),
            policy,
            Arc::new(AtomicBool::new(false)),
        );
        let work = tokio::spawn(async move {
            scope
                .invoke(
                    "lsp_context",
                    json!({"path":"source.rs","line":1,"column":1}),
                )
                .await
        });
        let event: Value = serde_json::from_slice(
            output
                .recv()
                .await?
                .context("LSP editor request missing")?
                .bytes(),
        )?;
        let editor_request_id = event["payload"]["id"]
            .as_str()
            .context("LSP editor identity missing")?;
        let authorization = Request {
            id: 2,
            method: "session.lsp.authorize".into(),
            params: json!({"id":editor_request_id,"paths":[path]}),
        };
        assert!(
            service
                .dispatch(
                    Some("unrelated-session".into()),
                    authorization.clone(),
                    &sink
                )
                .await
                .is_err()
        );
        let authorized = callback(&service, &session_id, authorization, &sink, &mut output).await?;
        assert_eq!(authorized[path.to_string_lossy().as_ref()], true);
        let analyzed = callback(
            &service,
            &session_id,
            Request {
                id: 3,
                method: "session.lsp.analyze".into(),
                params: json!({"id":editor_request_id,"path":path,"source":source}),
            },
            &sink,
            &mut output,
        )
        .await?;
        assert!(
            analyzed["declarations"]
                .as_array()
                .unwrap()
                .iter()
                .any(|declaration| declaration["name"] == "callback_source")
        );
        let response = Request {
            id: 4,
            method: "session.lsp.respond".into(),
            params: json!({"id":editor_request_id,"result":{"rows":[{"path":path,
                "line":1,"column":8,"text":"pub fn callback_source()","modified":false}],"source":"buffer"}}),
        };
        assert_eq!(
            callback(&service, &session_id, response.clone(), &sink, &mut output).await?["accepted"],
            true
        );
        assert!(work.await??.contains("callback_source"));
        assert!(
            service
                .dispatch(Some(session_id), response, &sink)
                .await
                .is_err()
        );
        drop(lease);
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn shared_requests_work_before_any_session_exists() -> Result<()> {
        async fn shared_response(
            service: &HarnessService,
            method: &str,
            params: Value,
        ) -> Result<Value> {
            let (sink, mut output) = forge_protocol::outbound::channel();
            service
                .dispatch(
                    None,
                    Request {
                        id: 42,
                        method: method.into(),
                        params,
                    },
                    &sink,
                )
                .await?;
            loop {
                let frame = output.recv().await?.context("shared response missing")?;
                let value: Value = serde_json::from_slice(frame.bytes())?;
                if value["id"] == 42 {
                    ensure!(
                        value.get("error").is_none(),
                        "shared request failed: {value}"
                    );
                    return Ok(value["result"].clone());
                }
            }
        }
        let fixture = tempfile::tempdir()?;
        let service = service();
        let response = service
            .initialize_services(1, initialize(&fixture, "mock"))
            .await?;
        assert!(response.error().is_none());
        assert_eq!(
            shared_response(&service, "session.list", json!({})).await?,
            json!([])
        );
        let settings = shared_response(&service, "settings.get", json!({})).await?;
        let saved = shared_response(&service, "settings.update", json!({
            "expected_revision":settings["revision"],"scope":"global","operation_id":"shared-setting",
            "desired":{"plan_compact":true}
        })).await?;
        assert_eq!(saved["state"], "saved");
        assert!(
            shared_response(
                &service,
                "settings.update",
                json!({
                    "expected_revision":saved["revision"],"scope":"session","desired":{}
                })
            )
            .await
            .is_err()
        );
        let profiles = shared_response(&service, "agent.profile.get", json!({})).await?;
        assert!(profiles["document"].is_object());
        let permissions = shared_response(&service, "permissions.document.open", json!({})).await?;
        assert!(permissions["source"].is_string());
        let tracing =
            shared_response(&service, "host.trace.configure", json!({"enabled":true})).await?;
        assert_eq!(tracing["enabled"], true);
        assert!(
            tracing["path"]
                .as_str()
                .unwrap()
                .ends_with("harness-trace.jsonl")
        );
        assert_eq!(
            shared_response(
                &service,
                "provider.model.pin.list",
                json!({"backend":"mock"})
            )
            .await?,
            json!([])
        );
        assert!(
            service
                .registry()
                .await?
                .controller_by_id
                .read()
                .await
                .is_empty()
        );
        let created =
            shared_response(&service, "session.new", json!({"name":"Explicit creation"})).await?;
        let id = created["session"]["id"]
            .as_str()
            .context("new session ID missing")?;
        let inherited = shared_response(
            &service,
            "session.new",
            json!({"source_session_id":id,"name":"Explicit inheritance"}),
        )
        .await?;
        assert_ne!(inherited["session"]["id"], created["session"]["id"]);
        assert_eq!(
            shared_response(&service, "session.list", json!({}))
                .await?
                .as_array()
                .unwrap()
                .len(),
            2
        );
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn shared_runtime_initialization_does_not_create_or_select_a_session() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let request = initialize(&fixture, "mock");
        let response = service.initialize_services(1, request.clone()).await?;
        assert!(response.error().is_none(), "{:?}", response.error());
        assert!(response.result().unwrap().get("session").is_none());
        let registry = service.registry().await?;
        assert!(registry.initial_session_id.is_none());
        assert!(registry.controller_by_id.read().await.is_empty());
        let database = rusqlite::Connection::open(fixture.path().join("data/harness.sqlite3"))?;
        let count: i64 =
            database.query_row("SELECT count(*) FROM session_record", [], |row| row.get(0))?;
        assert_eq!(count, 0);
        drop(registry);
        let opened = service.open_session(2, request.clone()).await?;
        assert!(opened.error().is_none(), "{:?}", opened.error());
        let id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let response = service.initialize_services(3, request).await?;
        assert!(response.error().is_none());
        assert_eq!(
            service.registry().await?.initial_session_id.as_deref(),
            Some(id.as_str())
        );
        let count: i64 =
            database.query_row("SELECT count(*) FROM session_record", [], |row| row.get(0))?;
        assert_eq!(count, 1);
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn plan_review_questions_fork_without_changing_the_main_timeline() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        std::fs::write(fixture.path().join(".forge.json"), r#"{"checks":{}}"#)?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        controller.attach_input(&sink).await?;
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.input.apply".into(),
                    params: json!({"id":"ask-plan","action":{"kind":"submit","message":{
                        "id":"ask-plan","text":"/plan explain the interface","intent":"auto","attachments":[]}}}),
                },
                &sink,
            )
            .await?;
        let plan = tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                if let Some(plan) = controller.broker.lock().await.snapshot()?.active_plan
                    && plan.review_digest.is_some()
                {
                    break Ok::<_, anyhow::Error>(plan);
                }
                tokio::time::sleep(Duration::from_millis(10)).await;
            }
        })
        .await??;
        let mut projection = service.document(Some(session_id.clone()), serde_json::from_value(json!({
            "operation":"plan_open", "document":"ask-review", "view":"ask-view", "plan_id":plan.id,
            "digest":plan.review_digest, "width":{"columns":100,"tabstop":4}
        }))?).await?;
        projection = crate::plan::reconstruct(None, projection);
        let row = projection["source_row"]
            .as_array()
            .unwrap()
            .iter()
            .find(|row| row["target"].is_string())
            .unwrap()
            .clone();
        let before = controller.broker.lock().await.snapshot()?;
        let source_execution = Arc::clone(&controller.coordinator.execution)
            .acquire_owned()
            .await?;
        assert!(
            service
                .prepare(Some(&session_id), RuntimeMethod::SessionPlanQuestionAnswer)
                .await?
                .is_none()
        );
        drop(source_execution);
        for (iteration, selection) in [
            json!({}),
            json!({"model":"mock-model","effort":"high","service_tier":"fast"}),
        ]
        .into_iter()
        .enumerate()
        {
            service
                .dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: 3,
                        method: "session.configure".into(),
                        params: json!({"ask_model":selection}),
                    },
                    &sink,
                )
                .await?;
            let configured: Response = loop {
                let value: Value =
                    serde_json::from_slice(output.recv().await.unwrap().unwrap().bytes())?;
                if value["id"] == 3 {
                    break serde_json::from_value(value)?;
                }
            };
            assert!(configured.error().is_none());
            let before_child = registry.controller_by_id.read().await.len();
            service.dispatch(Some(session_id.clone()), Request { id: 4, method: "session.plan.question.answer".into(),
                params: json!({"draft_source_digest":projection["saved_source_digest"],
                    "draft_annotation":[{"id":format!("question-{iteration}"),"kind":"question","source":{
                        "start_line":row["source_line"],"end_line":row["source_line"],"body":"Why this interface?"}},
                        {"id":format!("second-question-{iteration}"),"kind":"question","source":{
                            "start_line":row["source_line"],"end_line":row["source_line"],"body":"Which caller uses it?"}}],
                    "review":{"document":"ask-review","view":"ask-view","sequence":iteration+1,
                        "revision":projection["snapshot"]["revision"],"block":row["block"],"position":row["position"],
                        "target":row["target"],"action":"session.plan.question.answer"}}) }, &sink).await?;
            let response = loop {
                let value: Value =
                    serde_json::from_slice(output.recv().await.unwrap().unwrap().bytes())?;
                if value["id"] == 4 {
                    break value;
                }
            };
            assert!(response.get("error").is_none(), "{response}");
            let annotation = response["result"]["annotation"].as_array().unwrap();
            assert_eq!(annotation.len(), 2);
            for question in annotation {
                assert!(
                    question["reply"]["body"]
                        .as_str()
                        .unwrap()
                        .contains("This declaration")
                );
                assert_eq!(
                    question["reply"]["question_body"],
                    question["source"]["body"]
                );
            }
            assert_eq!(
                registry.controller_by_id.read().await.len(),
                before_child + 2
            );
            let after = controller.broker.lock().await.snapshot()?;
            assert_eq!(after.exchange.len(), before.exchange.len());
            assert_eq!(
                after.session.backend_session_id,
                before.session.backend_session_id
            );
            assert_eq!(after.session.model, before.session.model);
            let store = SqliteStore::open(&fixture.path().join("data"))?;
            let child = store
                .list_session(None)?
                .into_iter()
                .filter(|session| session.id != session_id)
                .max_by_key(|session| session.created_at_ms)
                .unwrap();
            let turns = store.list_exchange(&child.id)?;
            assert_eq!(turns.len(), 1);
            assert_eq!(
                child.model,
                if iteration == 0 {
                    before
                        .session
                        .resolved_model
                        .as_ref()
                        .unwrap_or(&before.session.model)
                } else {
                    "mock-model"
                }
            );
            assert_eq!(
                child.effort,
                if iteration == 0 {
                    before.session.effort.as_str()
                } else {
                    "high"
                }
            );
            assert_eq!(
                child.service_tier,
                if iteration == 0 {
                    before.session.service_tier
                } else {
                    crate::backend::ServiceTier::Fast
                }
            );
            assert!(
                child
                    .backend_session_id
                    .as_deref()
                    .unwrap()
                    .ends_with("-fork")
            );
        }
        service.shutdown(Duration::from_secs(3)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn isolated_generation_uses_configured_model_without_a_conversation() -> Result<()> {
        let backend = GenerationBackend {
            request: std::sync::Mutex::new(Vec::new()),
            shutdown: AtomicBool::new(false),
            fail: false,
        };
        let request = BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: "generation-fixture".into(),
            workspace: "commit-workspace".into(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            backend_session_id: None,
        };
        let text = generate_isolated_text(
            &backend,
            request.clone(),
            TextGeneration::CommitMessage,
            "selected-model",
            Some("low"),
            "Staged diff:\n+change",
        )
        .await?;
        assert_eq!(text, "fix: preserve drafts\n\nKeep existing text.");
        assert!(backend.shutdown.load(Ordering::Acquire));
        {
            let requests = backend.request.lock().unwrap();
            assert_eq!(requests.len(), 1);
            for GenerationCall {
                request,
                purpose,
                model,
                effort,
                input,
            } in requests.iter()
            {
                assert_eq!(request.harness_session_id, "generation-fixture");
                assert_eq!(request.workspace, "commit-workspace");
                assert!(request.backend_session_id.is_none());
                assert_eq!(*purpose, TextGeneration::CommitMessage);
                assert_eq!(model, "selected-model");
                assert_eq!(effort.as_deref(), Some("low"));
                assert_eq!(input, "Staged diff:\n+change");
            }
        }
        let unsupported = SteeringBackend {
            request: Mutex::new(Vec::new()),
        };
        let error = generate_isolated_text(
            &unsupported,
            request,
            TextGeneration::CommitMessage,
            "selected-model",
            Some("low"),
            "Staged diff:\n+change",
        )
        .await
        .unwrap_err();
        let diagnostic = format!("{error:#}");
        assert!(
            diagnostic.contains("commit_message generation failed"),
            "{diagnostic}"
        );
        assert!(diagnostic.contains("text_generation"), "{diagnostic}");
        let failing = GenerationBackend {
            request: std::sync::Mutex::new(Vec::new()),
            shutdown: AtomicBool::new(false),
            fail: true,
        };
        let request = BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: "generation-failure".into(),
            workspace: "commit-workspace".into(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            backend_session_id: None,
        };
        let failure = generate_isolated_text(
            &failing,
            request,
            TextGeneration::CommitMessage,
            "selected-model",
            Some("low"),
            "Staged diff:\n+change",
        )
        .await
        .unwrap_err();
        assert!(format!("{failure:#}").contains("fixture generation failure"));
        assert!(failing.shutdown.load(Ordering::Acquire));
        Ok(())
    }

    #[tokio::test]
    async fn isolated_generation_never_acquires_another_editors_session_lease() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let owner = service();
        let opened = owner.open_session(1, initialize(&fixture, "mock")).await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let data_root = fixture.path().join("data");
        let store = SqliteStore::open(&data_root)?;
        let before = store.load_session(&session_id)?.unwrap();
        let registry = owner.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let broker = controller.broker.lock().await;
        let generator = service();
        let text = tokio::time::timeout(
            Duration::from_secs(1),
            generator.generate_text(
                TextGenerationConfig {
                    auto_compaction: Default::default(),
                    backend: crate::backend::BackendLaunch {
                        kind: "mock".into(),
                        command: vec![],
                    },
                    model: "default".into(),
                    reasoning_effort: Some("low".into()),
                    service_tier: Default::default(),
                    data_root,
                },
                &before.workspace,
                TextGeneration::CommitMessage,
                "Staged diff:\n+change",
            ),
        )
        .await??;
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

    #[derive(Default)]
    struct TerminalInventoryBackend {
        started: Notify,
        release: Notify,
        calls: AtomicU64,
    }

    #[async_trait::async_trait]
    impl Backend for TerminalInventoryBackend {
        fn descriptor(&self) -> BackendDescriptor {
            BackendDescriptor {
                kind: BackendKind::Mock,
                label: "Terminal fixture".into(),
                capability: crate::backend::BackendCapability {
                    background_terminals: true,
                    ..Default::default()
                },
            }
        }
        async fn background_terminals(
            &self,
            _: BackendCatalogRequest,
        ) -> Result<crate::backend::terminal::TerminalSnapshot> {
            self.calls.fetch_add(1, Ordering::AcqRel);
            self.started.notify_one();
            self.release.notified().await;
            anyhow::bail!("connection closed")
        }
        async fn prompt_stream(
            &self,
            _: BackendRequest,
            _: Option<BackendEventSink>,
        ) -> Result<BackendOutput> {
            anyhow::bail!("terminal fixture cannot prompt")
        }
        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
        async fn cleanup_execution(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn cancel_session(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn has_active_turn(&self, _: &str) -> bool {
            false
        }
        async fn active_session_id_for(&self, _: &str) -> Option<String> {
            None
        }
    }

    #[tokio::test]
    async fn terminal_inventory_discards_configuration_races_but_reports_unrelated_errors()
    -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let mut controller = registry
            .controller_by_id
            .write()
            .await
            .remove(registry.initial_session_id())
            .unwrap();
        let backend = Arc::new(TerminalInventoryBackend::default());
        Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
        registry
            .controller_by_id
            .write()
            .await
            .insert(registry.initial_session_id().to_owned(), controller.clone());
        let mut store = SqliteStore::open(&controller.data_root)?;
        let mut operation = crate::task::TaskOperation {
            revision: 0,
            id: "terminal-config".into(),
            session_id: registry.initial_session_id().to_owned(),
            action: json!({"action":"configure"}),
            permission_selection: None,
            state: "applying".into(),
            error: None,
            exchange_id: None,
            created_at_ms: 1,
            resume_after_transition: false,
        };
        store.save_task_operation(&mut operation)?;
        assert!(controller.background_terminals().await?.starting);
        assert_eq!(backend.calls.load(Ordering::Acquire), 0);
        operation.state = "completed".into();
        store.save_task_operation(&mut operation)?;
        let query = controller.background_terminals();
        tokio::pin!(query);
        tokio::select! {
            result = &mut query => panic!("terminal query settled before release: {result:?}"),
            _ = backend.started.notified() => {}
        }
        controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
        backend.release.notify_one();
        assert!(query.await?.starting);
        operation.state = "resuming".into();
        store.save_task_operation(&mut operation)?;
        backend.release.notify_one();
        assert!(
            controller
                .background_terminals()
                .await
                .unwrap_err()
                .to_string()
                .contains("connection closed")
        );
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    struct GenerationCall {
        request: BackendCatalogRequest,
        purpose: TextGeneration,
        model: String,
        effort: Option<String>,
        input: String,
    }

    struct GenerationBackend {
        request: std::sync::Mutex<Vec<GenerationCall>>,
        shutdown: AtomicBool,
        fail: bool,
    }

    #[async_trait::async_trait]
    impl Backend for GenerationBackend {
        async fn generate_text(
            &self,
            request: BackendCatalogRequest,
            purpose: TextGeneration,
            model: &str,
            reasoning_effort: Option<&str>,
            input: &str,
        ) -> Result<String> {
            self.request.lock().unwrap().push(GenerationCall {
                request,
                purpose,
                model: model.into(),
                effort: reasoning_effort.map(str::to_owned),
                input: input.into(),
            });
            ensure!(!self.fail, "fixture generation failure");
            Ok("```text\nfix: preserve drafts\n\nKeep existing text.\n```".into())
        }
        fn descriptor(&self) -> BackendDescriptor {
            BackendDescriptor {
                kind: BackendKind::Mock,
                label: "Generation fixture".into(),
                capability: Default::default(),
            }
        }
        async fn prompt_stream(
            &self,
            _: BackendRequest,
            _: Option<BackendEventSink>,
        ) -> Result<BackendOutput> {
            anyhow::bail!("isolated generation must not submit a conversational turn")
        }
        async fn shutdown(&self) -> Result<()> {
            self.shutdown.store(true, Ordering::Release);
            Ok(())
        }
        async fn cleanup_execution(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn cancel_session(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn has_active_turn(&self, _: &str) -> bool {
            false
        }
        async fn active_session_id_for(&self, _: &str) -> Option<String> {
            None
        }
    }

    #[tokio::test]
    async fn plan_check_edits_retain_review_annotations_and_project_defaults() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let config = json!({"checks":{"format":{"trigger":{"auto":true},"action":{"command":["nu", "-c","print checked"],"label":"Rust formatting","category":"format","directory":".","after":[]}}}});
        std::fs::write(
            fixture.path().join(".forge.json"),
            serde_json::to_vec(&config)?,
        )?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let plan = {
            let mut broker = controller.broker.lock().await;
            let generated = broker
                .dispatch(Request {
                    id: 2,
                    method: "session.prompt.submit".into(),
                    params: json!({"text":"/plan add test coverage"}),
                })
                .await;
            assert!(
                generated.response.error().is_none(),
                "{:?}",
                generated.response.error()
            );
            broker.snapshot()?.active_plan.unwrap()
        };
        let mut projection = service.document(Some(session_id.clone()), serde_json::from_value(json!({
            "operation":"plan_open", "document":"checks-review", "view":"checks-view", "plan_id":plan.id,
            "digest":plan.review_digest, "width":{"columns":80,"tabstop":4}
        }))?).await?;
        projection = crate::plan::reconstruct(None, projection);
        let annotation_line = projection["source_row"]
            .as_array()
            .unwrap()
            .iter()
            .find(|row| row["target"].is_string())
            .unwrap()["source_line"]
            .clone();
        let (sink, mut output) = forge_protocol::outbound::channel();
        for (index, enabled) in [false, true].into_iter().enumerate() {
            assert_eq!(projection["checks"][0]["enabled"], !enabled);
            let row = projection["source_row"][0].clone();
            service.dispatch(Some(session_id.clone()), Request { id:3, method:"session.plan.check.update".into(),
                params:json!({"enabled":{"format":enabled}, "expected_version":projection["version"],
                    "draft_source_digest":projection["saved_source_digest"],
                    "draft_annotation":[{"id":"retained", "source":{"start_line":annotation_line,"end_line":annotation_line,"body":"Keep this feedback."}}],
                    "review":{"document":"checks-review","view":"checks-view","sequence":index+1,
                        "revision":projection["snapshot"]["revision"],"block":row["block"],"position":row["position"],
                        "target":row["target"],"action":"checks"}}) }, &sink).await?;
            loop {
                let frame = output
                    .recv()
                    .await?
                    .context("check edit response is missing")?;
                let value: Value = serde_json::from_slice(frame.bytes())?;
                if value["id"] != 3 {
                    continue;
                }
                assert!(value.get("error").is_none(), "{value}");
                assert!(value["result"].get("review_error").is_none(), "{value}");
                projection =
                    crate::plan::reconstruct(Some(&projection), value["result"]["review"].clone());
                break;
            }
            assert_eq!(projection["checks"][0]["enabled"], enabled);
            assert_eq!(
                projection["source_row"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .any(|row| row["check"] == "format"),
                enabled
            );
            assert!(
                projection["annotation"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .any(|annotation| annotation["id"] == "retained")
            );
            assert_eq!(
                serde_json::from_slice::<Value>(&std::fs::read(
                    fixture.path().join(".forge.json")
                )?)?,
                config
            );
        }
        let stale = controller.broker.lock().await.dispatch(Request { id:4, method:"session.plan.check.update".into(),
            params:json!({"plan_id":plan.id,"digest":projection["saved_source_digest"], "expected_version":0, "enabled":{"format":false}}) }).await;
        assert!(stale.response.error().is_some());
        Ok(())
    }

    #[tokio::test]
    async fn manual_plan_edits_retain_projection_and_decision_history() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let mut plan = {
            let mut broker = controller.broker.lock().await;
            let generated = broker
                .dispatch(Request {
                    id: 2,
                    method: "session.prompt.submit".into(),
                    params: json!({"text":"/plan add test coverage"}),
                })
                .await;
            assert!(
                generated.response.error().is_none(),
                "{:?}",
                generated.response.error()
            );
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
        let (_, _, digest) = file.submit_validated_document_revision(
            &session_id,
            &plan.id,
            plan.model_revision,
            canonical.version,
            canonical,
        )?;
        plan.review_digest = Some(digest.clone());
        SqliteStore::open(&data)?.save_plan(&plan)?;
        let submitted = file.read_submitted_document(&session_id, &plan.id, plan.model_revision)?;
        let lifecycle_count = SqliteStore::open(&data)?
            .list_plan_lifecycle(&session_id)?
            .len();
        let mut projection = service
            .document(
                Some(session_id.clone()),
                serde_json::from_value(json!({
                    "operation":"plan_open", "document":"test-review", "view":"test-view",
                    "plan_id":plan.id, "digest":digest,
                    "width":{"columns":100,"tabstop":4}
                }))?,
            )
            .await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        projection = crate::plan::reconstruct(None, projection);
        for (index, name) in ["first", "keep"].into_iter().enumerate() {
            let row = projection["source_row"]
                .as_array()
                .unwrap()
                .iter()
                .find(|row| row["test"]["name"] == name)
                .unwrap();
            let previous_revision = projection["snapshot"]["revision"].as_u64().unwrap();
            let previous_version = projection["version"].as_u64().unwrap();
            service.dispatch(Some(session_id.clone()), Request { id: 3, method: "session.plan.test.delete".into(),
                params: json!({"tests":[{"file":"tests/config.rs","name":name}],
                    "expected_version":projection["version"], "draft_annotation":[],
                    "draft_source_digest":projection["saved_source_digest"],
                    "review":{"document":"test-review","view":"test-view","sequence":index + 1,
                        "revision":previous_revision,"block":row["block"],"position":row["position"],
                        "target":row["target"],"action":"delete_tests"}}) }, &sink).await?;
            loop {
                let frame = output
                    .recv()
                    .await?
                    .context("test deletion response is missing")?;
                let value: Value = serde_json::from_slice(frame.bytes())?;
                if value["id"] != 3 {
                    continue;
                }
                assert!(value.get("error").is_none(), "{value}");
                assert!(value["result"].get("review_error").is_none(), "{value}");
                assert_eq!(
                    value["result"]["plan"]["manual_changes"][index],
                    format!("Removed planned tests: tests/config.rs::{name}")
                );
                assert_eq!(
                    value["result"]["plan"]["model_revision"],
                    plan.model_revision
                );
                assert_eq!(value["result"]["plan"]["user_revision"], plan.user_revision);
                projection =
                    crate::plan::reconstruct(Some(&projection), value["result"]["review"].clone());
                break;
            }
            assert_eq!(projection["snapshot"]["document"], "test-review");
            assert!(projection["snapshot"]["revision"].as_u64().unwrap() > previous_revision);
            assert_eq!(projection["version"], previous_version + 1);
            assert!(
                !projection["source_row"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .any(|row| row["test"]["name"] == name)
            );
        }
        let row = projection["source_row"]
            .as_array()
            .unwrap()
            .iter()
            .find(|row| {
                row["text"]
                    .as_str()
                    .is_some_and(|text| text.trim_start().starts_with("pub fn "))
                    && row["target"].is_string()
            })
            .context("rename declaration is missing")?;
        let mut input = json!({"document":"test-review","view":"test-view","sequence":3,
            "revision":projection["snapshot"]["revision"],"block":row["block"],"position":row["position"],
            "target":row["target"],"action":"rename_entity"});
        input["position"]["column"] = json!(row["text"].as_str().unwrap().find("fn ").unwrap() + 3);
        let selected = service
            .document(
                Some(session_id.clone()),
                serde_json::from_value(json!({
                    "operation":"plan_action","input":input
                }))?,
            )
            .await?;
        assert!(selected.get("rename").is_some(), "{selected}");
        let annotation = json!([{"id":"rename-feedback","source":{"start_line":row["source_line"],
            "end_line":row["source_line"],"body":"Keep this comment attached after rename."}}]);
        input["sequence"] = json!(4);
        service.dispatch(Some(session_id.clone()), Request { id:4, method:"session.plan.entity.rename".into(),
            params:json!({"review":input,"symbol":selected["rename"]["symbol"],"name":"reviewed_name",
                "expected_version":projection["version"],"draft_annotation":annotation,
                "draft_source_digest":projection["saved_source_digest"]}) }, &sink).await?;
        let renamed = loop {
            let frame = output.recv().await?.context("rename response is missing")?;
            let value: Value = serde_json::from_slice(frame.bytes())?;
            if value["id"] == 4 {
                break value;
            }
        };
        assert!(renamed.get("error").is_none(), "{renamed}");
        assert!(renamed["result"].get("review_error").is_none(), "{renamed}");
        let renamed_review =
            crate::plan::reconstruct(Some(&projection), renamed["result"]["review"].clone());
        assert_eq!(renamed_review["snapshot"]["document"], "test-review");
        assert_eq!(
            renamed_review["annotation"][0]["source"]["body"],
            "Keep this comment attached after rename."
        );
        assert!(
            renamed_review["source_row"]
                .as_array()
                .unwrap()
                .iter()
                .any(|row| row["text"]
                    .as_str()
                    .is_some_and(|text| text.contains("reviewed_name")))
        );
        let changes = renamed["result"]["plan"]["manual_changes"].clone();
        assert_eq!(changes.as_array().unwrap().len(), 3);
        assert_eq!(
            renamed["result"]["plan"]["model_revision"],
            plan.model_revision
        );
        assert_eq!(
            file.read_submitted_document(&session_id, &plan.id, plan.model_revision)?,
            submitted
        );
        assert!(
            file.read_submitted_document(&session_id, &plan.id, plan.model_revision + 1)
                .is_err()
        );
        assert_eq!(
            SqliteStore::open(&data)?
                .list_plan_lifecycle(&session_id)?
                .len(),
            lifecycle_count
        );
        {
            let mut broker = controller.broker.lock().await;
            let rejected = broker.dispatch(Request { id: 99, method: "session.plan.changes.request".into(),
                params: json!({"plan_id":plan.id,"digest":"stale-review-digest","comment":"x".repeat(65537)}) }).await;
            assert!(
                rejected
                    .response
                    .error()
                    .unwrap()
                    .message
                    .contains("plan review digest changed")
            );
            assert!(
                file.read_submitted_document(&session_id, &plan.id, plan.model_revision + 1)
                    .is_err()
            );
        }
        service.dispatch(Some(session_id.clone()), Request { id:5, method:"session.plan.changes.request".into(),
            params:json!({"plan_id":plan.id,"digest":renamed["result"]["plan"]["review_digest"],
                "comment":"Retain the manual edits while revising the plan."}) }, &sink).await?;
        loop {
            let frame = output
                .recv()
                .await?
                .context("review decision response is missing")?;
            let value: Value = serde_json::from_slice(frame.bytes())?;
            if value["id"] != 5 {
                continue;
            }
            assert!(value.get("error").is_none(), "{value}");
            assert_eq!(value["result"]["manual_changes"], changes);
            break;
        }
        assert_eq!(
            file.read_submitted_document(&session_id, &plan.id, plan.model_revision)?,
            submitted
        );
        let reviewed =
            file.read_submitted_document(&session_id, &plan.id, plan.model_revision + 1)?;
        assert!(reviewed.design.as_ref().unwrap().document.tests.is_empty());
        assert!(
            reviewed
                .design
                .as_ref()
                .unwrap()
                .proposed
                .values()
                .any(|text| text.contains("reviewed_name"))
        );
        assert_eq!(
            SqliteStore::open(&data)?
                .load_plan(&plan.id)?
                .unwrap()
                .model_revision,
            plan.model_revision + 2
        );
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn input_timeout_settings_do_not_interrupt_a_busy_session() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let execution = controller.coordinator.execution.acquire().await?;
        let broker = controller.broker.lock().await;
        let (sink, _output) = forge_protocol::outbound::channel();
        tokio::time::timeout(Duration::from_secs(2), service.dispatch(Some(session_id.clone()), Request {
            id:2, method:"session.input.timeout.configure".into(),
            params:json!({"approval_timeout":{"amount":2,"unit":"s"},"answer_timeout":{"amount":0,"unit":"m"}}),
        }, &sink)).await??;
        let (approval, answer) = controller.permission.input_timeouts(&session_id)?;
        assert_eq!(approval.amount, 2);
        assert_eq!(answer.amount, 0);
        assert!(controller.coordinator.execution.try_acquire().is_err());
        assert_eq!(broker.snapshot()?.session.approval_timeout, approval);
        drop(broker);
        drop(execution);
        let settings = crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?;
        assert_eq!(
            settings
                .get(&fixture.path().to_string_lossy(), &session_id, "mock")?
                .desired
                .answer_timeout,
            Some(answer)
        );
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn check_editing_toggles_without_prompt_or_execution_admission() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        assert_eq!(opened.result().unwrap()["check_editing"], false);
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let execution = controller.coordinator.execution.acquire().await?;
        let broker = controller.broker.lock().await;
        let (sink, _output) = forge_protocol::outbound::channel();
        for (index, enabled) in [true, false, true].into_iter().enumerate() {
            assert!(
                service
                    .prepare(Some(&session_id), RuntimeMethod::SessionCheckEditingSet)
                    .await?
                    .is_none()
            );
            tokio::time::timeout(
                Duration::from_secs(2),
                service.dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: index as u64 + 2,
                        method: "session.check.editing.set".into(),
                        params: json!({"enabled": enabled}),
                    },
                    &sink,
                ),
            )
            .await??;
            let snapshot = controller.observer.snapshot()?;
            assert_eq!(snapshot.check_editing, enabled);
            assert!(snapshot.exchange.is_empty());
            assert!(controller.coordinator.execution.try_acquire().is_err());
            let settings =
                crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?;
            assert_eq!(
                settings
                    .layer_settings(
                        crate::settings::SettingScope::Session,
                        &snapshot.session.workspace,
                        &session_id
                    )?
                    .check_editing,
                Some(enabled)
            );
            assert_eq!(
                settings
                    .get(&snapshot.session.workspace, "another-session", "mock")?
                    .desired
                    .check_editing,
                None
            );
        }
        drop(broker);
        drop(execution);
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn status_thought_settings_apply_and_persist_without_interrupting_a_busy_session()
    -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        assert_eq!(opened.result().unwrap()["status_thoughts"], true);
        assert_eq!(opened.result().unwrap()["automatic_checks"], false);
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let execution = controller.coordinator.execution.acquire().await?;
        let broker = controller.broker.lock().await;
        let (sink, _output) = forge_protocol::outbound::channel();
        for (index, enabled) in [true, false].into_iter().enumerate() {
            tokio::time::timeout(
                Duration::from_secs(2),
                service.dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: index as u64 + 2,
                        method: "session.configure".into(),
                        params: json!({"status_thoughts": enabled}),
                    },
                    &sink,
                ),
            )
            .await??;
            assert_eq!(controller.observer.snapshot()?.status_thoughts, enabled);
            assert!(controller.coordinator.execution.try_acquire().is_err());
            let settings =
                crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?;
            assert_eq!(
                settings
                    .get(&fixture.path().to_string_lossy(), &session_id, "mock")?
                    .desired
                    .status_thoughts,
                Some(enabled)
            );
        }
        drop(broker);
        drop(execution);
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn settings_application_orders_progress_and_retains_value_free_timeline() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let observation = tokio::spawn(async move {
            let mut state = Vec::new();
            while let Some(frame) = output.recv().await? {
                let frame: Value = serde_json::from_slice(frame.bytes())?;
                if frame["event"] == "task_operation" {
                    let phase = frame["payload"]["state"].as_str().unwrap().to_owned();
                    let finished = phase == "completed" || phase == "failed";
                    state.push(phase);
                    if finished {
                        break;
                    }
                }
            }
            Ok::<_, anyhow::Error>(state)
        });
        let request = Request {
            id: 2,
            method: "settings.update".into(),
            params: json!({"operation_id":"settings-retry","expected_revision":0,"scope":"global","desired":{"plan_compact":true}}),
        };
        service
            .dispatch(Some(session_id.clone()), request.clone(), &sink)
            .await?;
        let state = tokio::time::timeout(Duration::from_secs(2), observation).await???;
        let application_state: Vec<_> = state
            .iter()
            .filter(|phase| phase.as_str() != "running")
            .map(String::as_str)
            .collect();
        assert_eq!(application_state, ["applying", "verifying", "completed"]);
        let (retry_sink, mut retry_output) = forge_protocol::outbound::channel();
        service
            .dispatch(Some(session_id.clone()), request.clone(), &retry_sink)
            .await?;
        let replay = retry_output
            .recv()
            .await?
            .context("settings retry response is missing")?;
        let replay: Value = serde_json::from_slice(replay.bytes())?;
        assert_eq!(replay["result"]["id"], "settings-retry");
        assert_eq!(replay["result"]["state"], "completed");
        let mut conflicting = request;
        conflicting.params["desired"]["plan_compact"] = json!(false);
        assert!(
            service
                .dispatch(Some(session_id.clone()), conflicting, &retry_sink)
                .await
                .unwrap_err()
                .to_string()
                .contains("reused with a different request")
        );
        let root = fixture.path().join("data");
        let settings = crate::settings::SettingsStore::open(&root)?;
        let application = settings.application(&session_id)?;
        assert_eq!(application.state, "applied");
        assert_eq!(application.generation, 1);
        assert_eq!(settings.get("workspace", &session_id, "mock")?.revision, 1);
        let store = SqliteStore::open(&root)?;
        let event = store.list_session_event(&session_id)?;
        assert_eq!(event.len(), 1);
        let crate::timeline::SessionEventKind::Configuration { state, field, .. } =
            &event[0].detail
        else {
            panic!("configuration event is missing");
        };
        assert_eq!(state, "completed");
        assert_eq!(field, &["plan_compact"]);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn question_answers_commit_without_waiting_for_provider_execution() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let controller = service.registry().await?.resolve(&session_id).await?;
        let mut store = SqliteStore::open(&controller.data_root)?;
        let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
            "id":"question-exchange","session_id":session_id,"agent_id":format!("{session_id}:agent:primary"),
            "ordinal":1,"prompt":"question","kind":"chat","state":"complete","turn":[],"node_list":[],
            "created_at_ms":1,"attributed_matches_checkpoint":false,"awaiting_input":true
        }))?;
        exchange.set_elicitation(Some(crate::plan::PlanElicitation::new(serde_json::from_value(
            json!({
                "id":"set","questions":[{"id":"choice","header":"Choice","question":"Which?",
                    "options":[{"label":"Yes","description":"Proceed","recommended":true}],"allow_freeform":true}]
            }),
        )?)));
        store.save_exchange(&exchange)?;
        let _busy = controller.broker.lock().await;
        let _execution = controller.coordinator.execution.acquire().await?;
        let (sink, _output) = forge_protocol::outbound::channel();
        let answer = "retained answer ".repeat(forge_protocol::outbound::MAX_CONTROL_BYTES);
        tokio::time::timeout(Duration::from_secs(1), service.dispatch_admitted(
            Some(session_id.clone()), Request { id: 2, method: "session.question.answer".into(), params: json!({
                "_command":{"id":"answer"}, "operation_id":"answer",
                "target":{"owner":"interaction","id":"question-exchange","set_id":"set","revision":1},
                "question_id":"choice","response":{"kind":"other","text":answer}
            })}, &sink, None,
        )).await??;
        assert_eq!(
            store
                .load_exchange("question-exchange")?
                .unwrap()
                .elicitation()
                .unwrap()
                .answer
                .len(),
            1
        );
        assert_eq!(
            controller.coordinator.status("answer")?["state"],
            "completed"
        );
        Ok(())
    }

    #[tokio::test]
    async fn pending_configuration_patches_execute_without_losing_fields() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let mut store = SqliteStore::open(&controller.data_root)?;
        let mut operation = Vec::new();
        for (index, config) in [json!({"model":"model-b"}), json!({"effort":"high"})]
            .into_iter()
            .enumerate()
        {
            operation.push(admit_task_intent(&controller, &session_id, &Request {
                id: index as u64 + 2, method: "session.command.apply".into(),
                params: json!({"operation_id":format!("configure-{index}"),"action":"configure","config":config}),
            }, &mut store)?);
        }
        assert!(store.pending_policy_predecessor(&session_id, &operation[1].id)?);
        let (sink, _output) = forge_protocol::outbound::channel();
        for pending in &mut operation {
            execute_task_transition(&registry, &controller, pending, &mut store, &sink).await?;
            pending.state = "completed".into();
            store.save_task_operation(pending)?;
        }
        let session = store.load_session(&session_id)?.unwrap();
        assert_eq!(session.model, "model-b");
        assert_eq!(session.effort, "high");
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn pending_permission_intents_compose_and_execute_in_order() -> Result<()> {
        for (changes, expected_access, expected_approval, initial_sandbox) in [
            (
                vec![json!({"kind":"enable_yolo"}), json!({"kind":"enable_yolo"})],
                "rxwx",
                "ask",
                false,
            ),
            (
                vec![
                    json!({"kind":"enable_yolo"}),
                    json!({"kind":"enable_yolo"}),
                    json!({"kind":"enable_yolo"}),
                ],
                "rxwx",
                "allow",
                false,
            ),
            (
                vec![
                    json!({"kind":"toggle_write_scope"}),
                    json!({"kind":"set_approval","mode":"deny"}),
                ],
                "rxwx",
                "deny",
                false,
            ),
            (
                vec![
                    json!({"kind":"toggle_write_enabled"}),
                    json!({"kind":"toggle_write_enabled"}),
                ],
                "rxw",
                "ask",
                false,
            ),
            (
                vec![
                    json!({"kind":"enable_yolo"}),
                    json!({"kind":"toggle_write_enabled"}),
                    json!({"kind":"toggle_read_scope"}),
                    json!({"kind":"set_approval","mode":"ask"}),
                ],
                "r",
                "ask",
                true,
            ),
        ] {
            let fixture = tempfile::tempdir()?;
            let service = service();
            let opened = service
                .open_session(1, initialize(&fixture, "mock"))
                .await?;
            let session_id = opened.result().unwrap()["session"]["id"]
                .as_str()
                .unwrap()
                .to_owned();
            let registry = service.registry().await?;
            let controller = registry.resolve(&session_id).await?;
            let mut store = SqliteStore::open(&controller.data_root)?;
            let mut initial = store.load_session(&session_id)?.unwrap();
            initial.access.sandbox = initial_sandbox;
            store.save_session(&initial)?;
            let mut operations = Vec::new();
            for (index, change) in changes.into_iter().enumerate() {
                let request = Request {
                    id: index as u64 + 2,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":format!("permission-{index}"),"action":"permission","change":change}),
                };
                let operation = admit_task_intent(&controller, &session_id, &request, &mut store)?;
                let duplicate = admit_task_intent(&controller, &session_id, &request, &mut store)?;
                assert_eq!(
                    operation.permission_selection,
                    duplicate.permission_selection
                );
                assert_eq!(
                    store.load_session(&session_id)?.unwrap().access,
                    initial.access
                );
                operations.push(operation);
            }
            let mut latest = operations.pop().unwrap();
            assert!(!latest.permission_selection.unwrap().sandbox);
            let (sink, _output) = forge_protocol::outbound::channel();
            for mut preceding in operations {
                execute_task_transition(&registry, &controller, &mut preceding, &mut store, &sink)
                    .await?;
                assert_eq!(preceding.state, "running");
                preceding.state = "completed".into();
                store.save_task_operation(&mut preceding)?;
            }
            execute_task_transition(&registry, &controller, &mut latest, &mut store, &sink).await?;
            let confirmed = store.load_session(&session_id)?.unwrap();
            assert_eq!(confirmed.access.abbreviation(), expected_access);
            assert_eq!(
                confirmed.approval_mode.label().to_lowercase(),
                expected_approval
            );
            assert!(!confirmed.access.sandbox);
            assert_eq!(
                confirmed.access.readable_directory,
                initial.access.readable_directory
            );
            assert_eq!(
                confirmed.access.writable_directory,
                initial.access.writable_directory
            );
            service.shutdown(Duration::from_secs(1)).await?;
        }
        Ok(())
    }

    #[tokio::test]
    async fn active_chat_settings_ignore_completed_idle_operations_but_preserve_pending_pause()
    -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let request = |id: &str, action: &str| Request {
            id: 2,
            method: "session.command.apply".into(),
            params: json!({"operation_id":id,"action":action,"config":{}}),
        };
        let mut idle = admit_task_intent(
            &controller,
            &session_id,
            &request("idle-config", "configure"),
            &mut store,
        )?;
        assert!(!idle.resume_after_transition);
        idle.state = "completed".into();
        store.save_task_operation(&mut idle)?;
        let active = controller.coordinator.execution.acquire().await?;
        let mut changed = admit_task_intent(
            &controller,
            &session_id,
            &request("active-config", "configure"),
            &mut store,
        )?;
        assert!(
            changed.resume_after_transition,
            "a completed idle configuration suppressed the new active chat"
        );
        changed.state = "completed".into();
        store.save_task_operation(&mut changed)?;
        let pause = admit_task_intent(
            &controller,
            &session_id,
            &request("pending-pause", "pause"),
            &mut store,
        )?;
        assert!(!pause.resume_after_transition);
        let stopped = admit_task_intent(
            &controller,
            &session_id,
            &request("after-pause", "configure"),
            &mut store,
        )?;
        assert!(
            !stopped.resume_after_transition,
            "configuration resumed work despite a pending explicit pause"
        );
        drop(active);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn idle_execution_configuration_preserves_running_goal_without_resume() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let mut task = crate::task::ActiveTask::goal(
            "Retain the running objective".into(),
            crate::session::continuation::ContinuationBudget::new(20, 2),
            false,
            1,
        );
        if let crate::task::TaskWorkflow::Goal { goal } = &mut task.workflow {
            goal.continuation.turn_count = 4;
        }
        task.request_entry("Continue the retained objective".into(), 1)?;
        controller.task_controller.change(
            &mut store,
            None,
            crate::task::TaskChange::Install(Box::new(task)),
            1,
        )?;
        let task = controller.task_controller.current()?.unwrap();
        controller.task_controller.acknowledge_entry(
            &mut store,
            &task.revision,
            &task.entry.as_ref().unwrap().id,
            2,
        )?;
        let before = controller.task_controller.current()?.unwrap();
        assert_eq!(before.status(), crate::task::TaskStatus::Running);
        assert_eq!(controller.coordinator.execution.available_permits(), 1);
        let status_thoughts = !controller.observer.snapshot()?.status_thoughts;
        let request = Request {
            id: 2,
            method: "session.command.apply".into(),
            params: json!({"operation_id":"idle-running-goal-config","action":"configure",
                "config":{"status_thoughts":status_thoughts}}),
        };
        let mut operation = admit_task_intent(&controller, &session_id, &request, &mut store)?;
        assert!(!operation.resume_after_transition);
        assert_eq!(
            serde_json::to_value(controller.task_controller.current()?.unwrap())?,
            serde_json::to_value(&before)?,
        );
        let (sink, _output) = forge_protocol::outbound::channel();
        tokio::time::timeout(
            Duration::from_secs(2),
            execute_task_transition(&registry, &controller, &mut operation, &mut store, &sink),
        )
        .await??;
        assert_eq!(
            controller.observer.snapshot()?.status_thoughts,
            status_thoughts
        );
        assert_eq!(
            serde_json::to_value(controller.task_controller.current()?.unwrap())?,
            serde_json::to_value(&before)?,
        );
        assert!(!*controller.cancellation.subscribe().borrow());
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn provider_setup_rejects_active_execution_and_pending_cleanup() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let (sink, _output) = forge_protocol::outbound::channel();
        for pending_cleanup in [false, true] {
            let occupied = if pending_cleanup {
                (
                    Some(
                        controller
                            .coordinator
                            .control
                            .clone()
                            .acquire_owned()
                            .await?,
                    ),
                    None,
                )
            } else {
                (
                    None,
                    Some(controller.coordinator.execution.acquire().await?),
                )
            };
            for method in ["session.provider.setup", "session.provider.authenticate"] {
                let error = service
                    .dispatch(
                        Some(session_id.clone()),
                        Request {
                            id: 2,
                            method: method.into(),
                            params: json!({"method":"oauth-personal"}),
                        },
                        &sink,
                    )
                    .await
                    .unwrap_err();
                assert!(error.to_string().contains(if pending_cleanup {
                    "cleanup is pending"
                } else {
                    "Finish active work"
                }));
            }
            drop(occupied);
        }
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn mock_response_controls_bypass_active_execution_and_broker_lock() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "blocking"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let broker = controller.broker.lock().await;
        let execution = controller.coordinator.execution.acquire().await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        tokio::time::timeout(
            Duration::from_secs(2),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.provider.mock_response".into(),
                    params: json!({"operation":"hold","mode":"chat"}),
                },
                &sink,
            ),
        )
        .await??;
        let frame = output.recv().await?.context("missing hold response")?;
        let held: Value = serde_json::from_slice(frame.bytes())?;
        let hold_id = held["result"]["id"]
            .as_str()
            .context("missing hold ID")?
            .to_owned();
        let backend = controller.backend.clone();
        let prompt_session = session_id.clone();
        let prompt = tokio::spawn(async move {
            backend
                .prompt(crate::backend::BackendRequest {
                    execution_owner: None,
                    harness_session_id: prompt_session,
                    workspace: ".".into(),
                    input: crate::backend::BackendInput::from_text("held prompt"),
                    mode: crate::backend::PromptMode::Chat,
                    model: "mock-model".into(),
                    effort: "low".into(),
                    context_window: None,
                    service_tier: Default::default(),
                    access: Default::default(),
                    approval_mode: crate::session::ApprovalMode::Ask,
                    backend_session_id: None,
                    submission: None,
                    recovered_input: Vec::new(),
                    control_context: None,
                })
                .await
        });
        for (request_id, operation) in [(3, "wait"), (4, "release")] {
            tokio::time::timeout(
                Duration::from_secs(2),
                service.dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: request_id,
                        method: "session.provider.mock_response".into(),
                        params: json!({"operation":operation,"id":hold_id}),
                    },
                    &sink,
                ),
            )
            .await??;
            let frame = output
                .recv()
                .await?
                .context("missing mock control response")?;
            let result: Value = serde_json::from_slice(frame.bytes())?;
            assert_eq!(result["id"], request_id);
            assert!(result.get("error").is_none(), "{result}");
            if operation == "wait" {
                assert_eq!(result["result"]["prompt"], "held prompt");
                assert!(!prompt.is_finished(), "arrival released the response");
            } else {
                assert_eq!(result["result"]["released"], true);
            }
        }
        let completed = tokio::time::timeout(Duration::from_secs(2), prompt).await???;
        assert_eq!(
            completed.event[0].text.as_deref(),
            Some("Mock response: held prompt")
        );
        drop(execution);
        drop(broker);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn shutdown_cancels_an_unclaimed_mock_response_wait() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let control = controller.backend.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &session_id,
                crate::backend::mock_response::MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let (sink, _output) = forge_protocol::outbound::channel();
        let mut waiting = Box::pin(service.dispatch(
            Some(session_id),
            Request {
                id: 2,
                method: "session.provider.mock_response".into(),
                params: json!({"operation":"wait", "id":held["id"]}),
            },
            &sink,
        ));
        assert!(futures_util::poll!(&mut waiting).is_pending());
        assert!(
            service.activity.try_write().is_err(),
            "wait never entered dispatch"
        );
        let (waited, shutdown) = tokio::join!(waiting, service.shutdown(Duration::from_secs(2)));
        assert!(waited.unwrap_err().to_string().contains("cancelled"));
        shutdown?;
        Ok(())
    }

    #[tokio::test]
    async fn model_pins_bypass_the_active_provider_broker_lock() {
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
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        for (method, parameter) in [
            (
                "provider.model.pin.set",
                json!({"backend":"copilot", "model":"pinned-model", "pinned":true}),
            ),
            ("provider.model.pin.list", json!({"backend":"copilot"})),
        ] {
            tokio::time::timeout(
                Duration::from_secs(2),
                service.dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: 2,
                        method: method.into(),
                        params: parameter,
                    },
                    &sink,
                ),
            )
            .await
            .expect("pins must not wait for provider execution")
            .unwrap();
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
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await
            .unwrap();
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let inactive = prepare_new_session(
            std::path::Path::new(&registry.initialize.data_root),
            &registry.initialize.client_id,
            "mock",
            false,
            &session_id,
            &json!({"name":"inactive deletion target"}),
            0,
        )
        .unwrap();
        let _execution = Arc::clone(&controller.coordinator.execution)
            .try_acquire_owned()
            .unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        for (method, params) in [
            (
                "session.prompt.history.record",
                json!({"text":"prompt recorded during execution"}),
            ),
            (
                "session.rename",
                json!({"session_id":session_id,"name":"active rename"}),
            ),
            ("session.delete", json!({"session_id":inactive.id})),
            ("session.state.get", json!({})),
            ("session.task.get", json!({})),
            ("session.plan.list", json!({})),
            ("session.agent.list", json!({})),
            ("session.exchange.list", json!({})),
            ("session.list", json!({"scope":"all"})),
            ("session.preview", json!({"session_id":session_id})),
            ("permissions.document.open", json!({})),
        ] {
            tokio::time::timeout(
                Duration::from_secs(2),
                service.dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: 2,
                        method: method.into(),
                        params,
                    },
                    &sink,
                ),
            )
            .await
            .unwrap_or_else(|_| panic!("{method} waited for execution"))
            .unwrap();
            let frame = output.recv().await.unwrap().unwrap();
            let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
            assert_eq!(value["id"], 2, "{method}: {value}");
            assert!(value["error"].is_null(), "{method}: {value}");
            if method == "session.state.get" {
                assert_eq!(value["result"]["session"]["name"], "active rename");
                assert_eq!(
                    value["result"]["prompt_history"][0]["text"],
                    "prompt recorded during execution"
                );
                assert_eq!(
                    value["result"]["prompt_history"][0]["attachments"],
                    json!([])
                );
            }
        }
        let error = tokio::time::timeout(
            Duration::from_secs(2),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 3,
                    method: "session.configure".into(),
                    params: json!({"effort":"high"}),
                },
                &sink,
            ),
        )
        .await
        .expect("unsupported direct mutation must reject without waiting")
        .unwrap_err();
        assert!(
            error.to_string().contains("requires a settled session"),
            "{error:#}"
        );
        assert_eq!(broker.snapshot().unwrap().session.name, "active rename");
        for (offset, method) in ["session.new"].into_iter().enumerate() {
            let request_id = 10 + offset as u64;
            tokio::time::timeout(
                Duration::from_secs(2),
                service.dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: request_id,
                        method: method.into(),
                        params: json!({}),
                    },
                    &sink,
                ),
            )
            .await
            .expect("new sessions must not wait for source execution")
            .unwrap();
            loop {
                let frame = output.recv().await.unwrap().unwrap();
                let response: Value = serde_json::from_slice(frame.bytes()).unwrap();
                if response["id"] != request_id {
                    continue;
                }
                assert!(response["error"].is_null(), "{response}");
                assert_ne!(response["result"]["session"]["id"], session_id);
                break;
            }
        }
        drop(broker);
        drop(_execution);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn attachment_import_bypasses_active_execution_and_retains_source_bytes() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let source = fixture.path().join("screen shot.png");
        let bytes = b"\x89PNG\r\n\x1a\nretained image";
        std::fs::write(&source, bytes)?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        tokio::time::timeout(
            Duration::from_secs(2),
            service.dispatch(
                Some(session_id),
                Request {
                    id: 2,
                    method: "session.attachment.import".into(),
                    params: json!({"path":source, "number":3}),
                },
                &sink,
            ),
        )
        .await??;
        let frame = output
            .recv()
            .await?
            .context("attachment response channel closed")?;
        assert!(
            !frame.is_terminal(),
            "attachment reply must keep the host connection open"
        );
        let response: Value = serde_json::from_slice(frame.bytes())?;
        let selection = response["result"].clone();
        assert_eq!(selection["label"], "[Image #3]");
        std::fs::remove_file(source)?;
        let selected = crate::attachment::resolve_input(
            &fixture.path().join("data"),
            &json!({"attachments":[selection]}),
        )
        .await?;
        let input = crate::attachment::codex_input("Inspect [Image #3]".into(), &selected).await?;
        assert_eq!(input[1]["type"], "localImage");
        assert_eq!(std::fs::read(input[1]["path"].as_str().unwrap())?, bytes);
        assert!(controller.broker.try_lock().is_err());
        drop(broker);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn configuration_acknowledgement_does_not_wait_for_mcp_discovery() {
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
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let discovery = controller.mcp_discovery.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let configuring = service.dispatch(Some(session_id.clone()), Request {
            id: 2, method: "session.command.apply".into(),
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
        tokio::time::timeout(Duration::from_secs(2), &mut configuring)
            .await
            .unwrap()
            .unwrap();
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn root_model_catalog_bypasses_the_active_broker_lock() {
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
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let mut store = SqliteStore::open(&fixture.path().join("data")).unwrap();
        let preference = serde_json::from_value(json!({
            "model":"default", "effort":"medium",
            "model_setting":{"mock-model":{"reasoning":"high","context_window":"default"}}
        }))
        .unwrap();
        store
            .save_preference(fixture.path().to_str().unwrap(), "mock", &preference)
            .unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        tokio::time::timeout(
            Duration::from_secs(2),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.provider.model.list".into(),
                    params: json!({}),
                },
                &sink,
            ),
        )
        .await
        .expect("root model selection must remain available during provider execution")
        .unwrap();
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
        let seed_service = service();
        let seeded = seed_service
            .open_session(1, initialize(&fixture, "mock"))
            .await
            .unwrap();
        assert!(seeded.result().unwrap()["session"]["resolved_model"].is_null());
        let seed_id = seeded.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        seed_service.shutdown(Duration::from_secs(1)).await.unwrap();
        let mut store = SqliteStore::open(&fixture.path().join("data")).unwrap();
        let mut seed = store.load_session(&seed_id).unwrap().unwrap();
        seed.resolved_model = Some("previous-default".into());
        store.save_session(&seed).unwrap();
        drop(store);
        let service = service();
        let mut request = initialize(&fixture, "mock");
        request.new_session_name = Some("Cached default".into());
        let opened = service.open_session(1, request).await.unwrap();
        assert_eq!(
            opened.result().unwrap()["session"]["resolved_model"],
            "previous-default"
        );
        assert_eq!(opened.result().unwrap()["session"]["model"], "default");
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        assert_ne!(session_id, seed_id);
        let (sink, mut output) = forge_protocol::outbound::channel();
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.provider.model.list".into(),
                    params: json!({}),
                },
                &sink,
            )
            .await
            .unwrap();
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
        assert!(
            configured,
            "idle catalog did not publish default model resolution"
        );
        let store = SqliteStore::open(&fixture.path().join("data")).unwrap();
        assert_eq!(
            store
                .load_session(&session_id)
                .unwrap()
                .unwrap()
                .resolved_model
                .as_deref(),
            Some("mock-model")
        );
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn child_model_catalog_bypasses_the_active_root_broker_lock() {
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
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        tokio::time::timeout(
            Duration::from_secs(2),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.agent.model.list".into(),
                    params: json!({"provider":"mock"}),
                },
                &sink,
            ),
        )
        .await
        .expect("child model selection must remain available during root execution")
        .unwrap();
        loop {
            let frame = output.recv().await.unwrap().unwrap();
            let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
            if value["id"] == 2 {
                assert!(value["error"].is_null(), "{value}");
                assert!(
                    value["result"]["models"]
                        .as_array()
                        .is_some_and(|models| !models.is_empty()),
                    "{value}"
                );
                assert_eq!(value["result"]["capability"]["fast_mode"], true, "{value}");
                assert_eq!(
                    value["result"]["capability"]["ultrafast_mode"], true,
                    "{value}"
                );
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
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await
            .unwrap();
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let mut review = controller.revision_review.open().unwrap();
        let (sink, mut output) = forge_protocol::outbound::channel();
        for method in [
            RuntimeMethod::SessionPlanAcceptanceBegin,
            RuntimeMethod::SessionPlanChangesRequest,
            RuntimeMethod::SessionPlanCancel,
            RuntimeMethod::SessionPlanEntityRename,
            RuntimeMethod::SessionPlanTestDelete,
            RuntimeMethod::SessionPlanCheckUpdate,
        ] {
            let request = Request {
                id: 2,
                method: "review".into(),
                params: json!({"plan_id":"plan"}),
            };
            let respond = async {
                let Some(crate::plan::revision_review::RevisionReviewRequest::Decide {
                    method: received,
                    params,
                    response,
                }) = review.receiver.recv().await
                else {
                    panic!("missing review decision")
                };
                assert_eq!(received, method);
                assert_eq!(params, request.params);
                response
                    .send(Ok((json!({"reviewed":true}), Vec::new())))
                    .unwrap();
            };
            let mut control_state = None;
            let (routed, ()) = tokio::time::timeout(Duration::from_secs(1), async {
                tokio::join!(
                    route_control_request(
                        &controller,
                        &request,
                        method,
                        &sink,
                        &service.syntax,
                        None,
                        &mut control_state
                    ),
                    respond
                )
            })
            .await
            .expect("review controls must not acquire the provider's broker lock");
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
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await
            .unwrap();
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await.unwrap();
        let controller = registry.resolve(&session_id).await.unwrap();
        let broker = controller.broker.lock().await;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let stopping = service.dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"stop-once","action":"pause"}),
            },
            &sink,
        );
        tokio::pin!(stopping);
        tokio::select! {
            result = &mut stopping => panic!("stop completed before the old owner settled: {result:?}"),
            frame = output.recv() => {
                let frame = frame.unwrap().unwrap();
                let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
                assert_eq!(value["result"]["state"], "accepted");
            }
        }
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 3,
                    method: "session.health.get".into(),
                    params: json!({}),
                },
                &sink,
            ),
        )
        .await
        .unwrap()
        .unwrap();
        let frame = output.recv().await.unwrap().unwrap();
        let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
        assert_eq!(value["result"]["responsive"], true);
        drop(frame);
        drop(broker);
        tokio::time::timeout(Duration::from_secs(2), &mut stopping)
            .await
            .unwrap()
            .unwrap();
        let store =
            SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path()).unwrap();
        assert_eq!(
            store
                .load_task_operation(&session_id, "stop-once")
                .unwrap()
                .unwrap()
                .state,
            "completed"
        );
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    struct SteeringBackend {
        request: Mutex<Vec<(String, String)>>,
    }

    #[async_trait::async_trait]
    impl crate::backend::Backend for SteeringBackend {
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

        async fn steer_session(
            &self,
            session_id: &str,
            input: crate::backend::SteeringInput,
        ) -> Result<()> {
            self.request
                .lock()
                .await
                .push((session_id.into(), input.text().to_owned()));
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
        use crate::permissions::matcher::{PermissionRequest, PermissionTarget};
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "visible-blocking"))
            .await?;
        let session_id = opened.result().context("session did not open")?["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        controller.attach_agents(&sink).await?;
        let admitted = controller
            .agent_coordinator
            .sender()
            .request(crate::agent::AgentRequest {
                operation_id: "approval-child".into(),
                source: crate::agent::AgentSource::User,
                operation: crate::agent::AgentOperation::Spawn(crate::agent::SpawnAgentRequest {
                    profile: Some("worker".into()),
                    text: "Wait".into(),
                }),
            })
            .await?;
        let agent_id = admitted["agent"]["id"].as_str().unwrap().to_owned();
        let task_id = admitted["task"]["id"].as_str().unwrap().to_owned();
        let owner = crate::backend::ExecutionOwner {
            session_id: (session_id.clone()).into(),
            agent_id: (agent_id.clone()).into(),
        };
        let permission = controller.permission.clone();
        let (event_sink, mut events) = crate::backend::events::channel();
        let approval_owner = owner.clone();
        let pending = tokio::spawn(async move {
            permission
                .authorize(
                    crate::session::ApprovalMode::Ask,
                    PermissionRequest {
                        id: "child-request".into(),
                        provider: "Mock".into(),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "probe".into(),
                            shell: None,
                        }],
                    },
                    Some(crate::backend::ProviderAddress {
                        owner: Some(approval_owner),
                        thread_id: "mock-session".into(),
                        turn_id: "mock-turn".into(),
                    }),
                    Some(&event_sink),
                )
                .await
        });
        let requested = tokio::time::timeout(Duration::from_secs(2), events.recv())
            .await??
            .unwrap();
        controller
            .agent_coordinator
            .sender()
            .send(crate::agent::AgentCommand::Event {
                owner,
                task_id: task_id.clone(),
                event: requested,
            })
            .await?;
        let requested_view = tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                let frame = output.recv().await?.context("event stream ended")?;
                let value: Value = serde_json::from_slice(frame.bytes())?;
                if value["payload"]["kind"] == "approval_requested" {
                    return Ok::<_, anyhow::Error>(value["payload"]["data"].clone());
                }
            }
        })
        .await??;
        assert_eq!(requested_view["exchange_id"], task_id);
        let approval_id = requested_view["id"].as_str().unwrap();
        let answers: Vec<_> = requested_view["item_list"]
            .as_array()
            .unwrap()
            .iter()
            .map(|item| json!({"item_id":item["id"],"choice_id":"allow_once"}))
            .collect();
        assert!(
            route_control_request(
                &controller,
                &Request {
                    id: 2,
                    method: "session.approval.resolve".into(),
                    params: json!({"approval_id":approval_id,"answer_list":answers})
                },
                RuntimeMethod::SessionApprovalResolve,
                &sink,
                &service.syntax,
                None,
                &mut None
            )
            .await?
        );
        assert_eq!(
            pending.await??,
            crate::backend::approval::ApprovalResolution::AllowOnce
        );
        let resolved = tokio::time::timeout(Duration::from_secs(2), events.recv())
            .await??
            .unwrap();
        assert_eq!(resolved.kind, "approval_resolved");
        controller
            .agent_coordinator
            .sender()
            .send(crate::agent::AgentCommand::Event {
                owner: crate::backend::ExecutionOwner {
                    session_id: (session_id.clone()).into(),
                    agent_id: (agent_id.clone()).into(),
                },
                task_id: task_id.clone(),
                event: resolved,
            })
            .await?;
        tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                let frame = output.recv().await?.context("event stream ended")?;
                let value: Value = serde_json::from_slice(frame.bytes())?;
                if value["payload"]["kind"] == "approval_resolved" {
                    assert_eq!(
                        value["payload"]["data"]["execution_owner"]["agent_id"],
                        agent_id
                    );
                    return Ok::<_, anyhow::Error>(());
                }
            }
        })
        .await??;
        assert!(controller.permission.pending_list()?.is_empty());
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let mut operation = admit_task_intent(
            &controller,
            &session_id,
            &Request {
                id: 3,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"clear-child","action":"clear"}),
            },
            &mut store,
        )?;
        tokio::time::timeout(
            Duration::from_secs(3),
            execute_task_transition(&registry, &controller, &mut operation, &mut store, &sink),
        )
        .await??;
        assert_eq!(controller.agent_coordinator.active_count(), 0);
        assert_eq!(
            controller.agent_coordinator.task(&task_id).unwrap().state,
            crate::exchange::ExchangeState::Cancelled
        );
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn permission_transition_releases_child_quiescence_before_resuming() -> Result<()> {
        for mode in ["ask", "deny", "allow"] {
            let fixture = tempfile::tempdir()?;
            let service = service();
            let opened = service
                .open_session(1, initialize(&fixture, "mock"))
                .await?;
            let session_id = opened.result().unwrap()["session"]["id"]
                .as_str()
                .unwrap()
                .to_owned();
            let registry = service.registry().await?;
            let controller = registry.resolve(&session_id).await?;
            let mut store =
                SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
            let session = crate::session::SessionStore::load_session(&store, &session_id)?.unwrap();
            let exchange: crate::exchange::Exchange = serde_json::from_value(json!({
                "id":"interrupted", "session_id":session_id, "agent_id":session.primary_agent_id, "ordinal":1,
                "prompt":"Explain the provider", "kind":"chat", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            store.save_exchange(&exchange)?;
            let previous_execution = controller.coordinator.execution.acquire().await?;
            let mut operation = admit_task_intent(
                &controller,
                &session_id,
                &Request {
                    id: 2,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"change-permission","action":"permission","change":{"kind":"set_approval","mode":mode}}),
                },
                &mut store,
            )?;
            assert!(operation.resume_after_transition);
            drop(previous_execution);
            let (sink, _output) = forge_protocol::outbound::channel();
            tokio::time::timeout(
                Duration::from_secs(2),
                execute_task_transition(&registry, &controller, &mut operation, &mut store, &sink),
            )
            .await
            .context("resumed provider output remained blocked by transition quiescence")??;
            assert!(!controller.agent_coordinator.is_quiescing());
            let exchanges = store.list_exchange(&session_id)?;
            assert!(exchanges.last().unwrap().completed_at_ms().is_some());
            let session = crate::session::SessionStore::load_session(&store, &session_id)?.unwrap();
            assert_eq!(
                session.approval_mode,
                serde_json::from_value::<crate::session::ApprovalMode>(json!(mode))?
            );
            service.shutdown(Duration::from_secs(1)).await?;
        }
        Ok(())
    }

    #[tokio::test]
    async fn idle_task_transition_waits_for_readers_without_provider_cleanup() {
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
        let registry = service.registry().await.unwrap();
        let mut controller = registry
            .controller_by_id
            .write()
            .await
            .remove(&session_id)
            .unwrap();
        Arc::get_mut(&mut controller).unwrap().backend = Arc::new(SteeringBackend {
            request: Mutex::new(Vec::new()),
        });
        let mut store =
            SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path()).unwrap();
        let mut operation = admit_task_intent(
            &controller,
            &session_id,
            &Request {
                id: 2,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"idle-pause","action":"pause"}),
            },
            &mut store,
        )
        .unwrap();
        let reader = controller.broker.lock().await;
        let (sink, _output) = forge_protocol::outbound::channel();
        let mut transition = Box::pin(execute_task_transition(
            &registry,
            &controller,
            &mut operation,
            &mut store,
            &sink,
        ));
        assert!(
            futures_util::poll!(transition.as_mut()).is_pending(),
            "read-only ownership was treated as provider execution"
        );
        drop(reader);
        tokio::time::timeout(Duration::from_secs(2), transition)
            .await
            .unwrap()
            .unwrap();
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test]
    async fn conversation_undo_collects_execution_admission_before_rollback() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let session = crate::session::SessionStore::load_session(&store, &session_id)?.unwrap();
        let exchange: crate::exchange::Exchange = serde_json::from_value(json!({
            "id":"latest", "session_id":session_id, "agent_id":session.primary_agent_id, "ordinal":1,
            "prompt":"Remove this conversation entry", "kind":"chat", "state":"complete", "created_at_ms":0,
            "completed_at_ms":1, "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        store.save_exchange(&exchange)?;
        let file = fixture.path().join("keep.txt");
        std::fs::write(&file, "unchanged")?;
        let execution = controller.coordinator.execution.acquire().await?;
        let mut request = Request {
            id: 2,
            method: "session.command.apply".into(),
            params: json!({
                "operation_id":"undo-latest", "action":"undo", "rollback":{
                    "scope":"conversation", "exchange_id":"latest", "latest_exchange_id":"stale"
                }
            }),
        };
        assert!(admit_task_intent(&controller, &session_id, &request, &mut store).is_err());
        request.params["rollback"]["latest_exchange_id"] = json!("latest");
        let mut operation = admit_task_intent(&controller, &session_id, &request, &mut store)?;
        assert!(!operation.resume_after_transition);
        let (sink, _output) = forge_protocol::outbound::channel();
        let mut transition = Box::pin(execute_task_transition(
            &registry,
            &controller,
            &mut operation,
            &mut store,
            &sink,
        ));
        assert!(
            futures_util::poll!(transition.as_mut()).is_pending(),
            "rollback bypassed execution ownership"
        );
        drop(execution);
        tokio::time::timeout(Duration::from_secs(2), transition).await??;
        assert_eq!(
            store.list_exchange(&session_id)?[0].disposition(),
            crate::exchange::HistoryDisposition::RolledBack
        );
        assert_eq!(std::fs::read_to_string(file)?, "unchanged");
        assert_eq!(controller.coordinator.execution.available_permits(), 1);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn check_configuration_chat_startup_can_be_interrupted() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let execution = service
            .prepare(Some(&session_id), RuntimeMethod::SessionPromptSubmit)
            .await?
            .unwrap();
        let maintenance = controller.broker.lock().await;
        let (sink, _output) = forge_protocol::outbound::channel();
        let setup = async {
            let _execution = execution;
            service
                .dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: 2,
                        method: "session.prompt.submit".into(),
                        params: json!({"text":"Configure project checks."}),
                    },
                    &sink,
                )
                .await
        };
        tokio::pin!(setup);
        assert!(futures_util::poll!(setup.as_mut()).is_pending());
        let stopping = service.dispatch(
            Some(session_id.clone()),
            Request {
                id: 3,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"interrupt-check-setup","action":"pause"}),
            },
            &sink,
        );
        tokio::pin!(stopping);
        assert!(futures_util::poll!(stopping.as_mut()).is_pending());
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        assert_eq!(
            store
                .load_task_operation(&session_id, "interrupt-check-setup")?
                .unwrap()
                .state,
            "stopping",
            "check setup retained cleanup admission instead of accepting interruption"
        );
        drop(maintenance);
        let (setup_result, stop_result) = tokio::time::timeout(Duration::from_secs(2), async {
            tokio::join!(setup, stopping)
        })
        .await
        .context("check setup interruption did not settle promptly")?;
        setup_result?;
        stop_result?;
        assert_eq!(
            store
                .load_task_operation(&session_id, "interrupt-check-setup")?
                .unwrap()
                .state,
            "completed"
        );
        assert_eq!(
            store.list_exchange(&session_id)?.last().unwrap().state(),
            crate::exchange::ExchangeState::Cancelled
        );
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn pause_intent_retracts_output_free_submission_when_requested() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        anyhow::ensure!(
            std::process::Command::new("git")
                .args(["init", "-q"])
                .current_dir(fixture.path())
                .status()?
                .success(),
            "failed to initialize retraction test repository"
        );
        let data = tempfile::tempdir()?;
        let service = service();
        let mut initialization = initialize(&fixture, "blocking");
        initialization.data_root = data.path().to_string_lossy().into_owned();
        let opened = service.open_session(1, initialization).await?;
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let execution = service
            .prepare(Some(&session_id), RuntimeMethod::SessionPromptSubmit)
            .await?
            .unwrap();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let prompt = async {
            let _execution = execution;
            service
                .dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: 2,
                        method: "session.prompt.submit".into(),
                        params: json!({"text":"Restore this exact prompt"}),
                    },
                    &sink,
                )
                .await
        };
        let stop = async {
            let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
            while store.list_exchange(&session_id)?.is_empty() {
                tokio::task::yield_now().await;
            }
            service.dispatch(Some(session_id.clone()), Request {
                id: 3, method: "session.command.apply".into(),
                params: json!({"operation_id":"retract-prompt","action":"pause","restore_prompt_if_no_output":true}),
            }, &sink).await
        };
        tokio::time::timeout(Duration::from_secs(2), async {
            tokio::try_join!(prompt, stop)
        })
        .await
        .context("output-free pause intent did not settle")??;
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        assert!(
            store.list_exchange(&session_id)?.is_empty(),
            "{:?}",
            store.list_exchange(&session_id)?
        );
        assert_eq!(
            store
                .load_task_operation(&session_id, "retract-prompt")?
                .unwrap()
                .state,
            "completed"
        );
        loop {
            let frame = tokio::time::timeout(Duration::from_secs(1), output.recv())
                .await??
                .context("prompt retraction response is missing")?;
            let response: Value = serde_json::from_slice(frame.bytes())?;
            if response["id"] == 2 {
                assert_eq!(response["error"]["code"], "turn_retracted");
                assert_eq!(
                    response["error"]["data"]["prompt"],
                    "Restore this exact prompt"
                );
                break;
            }
        }
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn queued_prompt_waits_for_execution_and_acknowledges_admission() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let previous = service
            .prepare(Some(&session_id), RuntimeMethod::SessionPromptSubmit)
            .await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let mut queued = Box::pin(service.dispatch_admitted(
            Some(session_id.clone()),
            Request { id: 2, method: "session.prompt.submit".into(),
                params: json!({"text":"Queued after finalization","queued":true,"submission":{"token":42}}) },
            &sink, None,
        ));
        assert!(futures_util::poll!(queued.as_mut()).is_pending());
        assert!(
            service
                .prepare(Some(&session_id), RuntimeMethod::SessionStateGet)
                .await?
                .is_none()
        );
        let controller = registry.resolve(&session_id).await?;
        assert!(
            controller.coordinator.control.try_acquire().is_ok(),
            "queued work blocked cancellation"
        );
        drop(previous);
        let observe = async {
            let mut accepted = 0;
            loop {
                let frame = output.recv().await?.context("queued response missing")?;
                let message: Value = serde_json::from_slice(frame.bytes())?;
                if message["event"] == "backend_event"
                    && message["payload"]["kind"] == "prompt_submission"
                {
                    assert_eq!(message["payload"]["data"]["token"], 42);
                    assert_eq!(message["payload"]["data"]["state"], "accepted");
                    accepted += 1;
                }
                if message["id"] == 2 {
                    assert!(message.get("error").is_none(), "{message}");
                    assert_eq!(
                        accepted, 1,
                        "queued admission was not acknowledged exactly once"
                    );
                    return Ok::<(), anyhow::Error>(());
                }
            }
        };
        tokio::time::timeout(Duration::from_secs(5), async {
            tokio::try_join!(queued, observe)
        })
        .await??;
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn queued_prompt_waits_for_cleanup_without_retaining_execution() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let cleanup = controller.coordinator.control.acquire().await?;
        let mut queued = Box::pin(service.prepare_queued_prompt(registry.initial_session_id()));
        assert!(futures_util::poll!(queued.as_mut()).is_pending());
        let execution = controller
            .coordinator
            .execution
            .try_acquire()
            .expect("queued work blocked cleanup");
        drop(cleanup);
        assert!(futures_util::poll!(queued.as_mut()).is_pending());
        assert!(controller.coordinator.control.try_acquire().is_ok());
        drop(execution);
        drop(tokio::time::timeout(Duration::from_secs(1), queued).await??);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn queued_prompt_does_not_resume_after_cancellation() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let execution = controller.coordinator.execution.acquire().await?;
        let mut queued = Box::pin(service.prepare_queued_prompt(registry.initial_session_id()));
        assert!(futures_util::poll!(queued.as_mut()).is_pending());
        controller.cancellation.request(false);
        drop(execution);
        let failure = queued.await.unwrap_err();
        assert!(failure.to_string().contains("interrupted before admission"));
        assert!(controller.coordinator.execution.try_acquire().is_ok());
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn finalized_execution_releases_admission_before_response_delivery() -> Result<()> {
        let semaphore =
            SessionCoordinator::new(Arc::new(crate::input::SessionInputQueue::default()))
                .execution
                .clone();
        let mut admission = Some(Arc::clone(&semaphore).acquire_owned().await?);
        let mut prompt_admission = None;
        let (sink, mut output) = forge_protocol::outbound::channel();
        for sequence in
            0..forge_protocol::MAX_PENDING_FRAMES - forge_protocol::RESERVED_CONTROL_RECORDS
        {
            sink.send(sequence)?;
        }
        let mut completion = Box::pin(publish_execution_completion(
            &sink,
            Response::success(2, json!({"completed":true}))?,
            &mut admission,
            &mut prompt_admission,
            false,
        ));
        assert!(
            futures_util::poll!(completion.as_mut()).is_pending(),
            "fixture must block response delivery"
        );
        let next = Arc::clone(&semaphore).try_acquire_owned().expect(
            "finished execution still owns admission while its completion waits for delivery",
        );
        drop(output.recv().await?.context("expected queued frame")?);
        completion.await?;
        drop(next);
        assert!(admission.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn next_prompt_is_admitted_when_previous_completion_arrives() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let first = service
            .prepare(None, RuntimeMethod::SessionPromptSubmit)
            .await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let dispatch = service.dispatch_admitted(
            None,
            Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({"text":"First request"}),
            },
            &sink,
            first,
        );
        let next = async {
            loop {
                let frame = output
                    .recv()
                    .await?
                    .context("completion response is missing")?;
                let message: Value = serde_json::from_slice(frame.bytes())?;
                if message["id"] == 2 {
                    assert!(message.get("error").is_none(), "{message}");
                    return service
                        .prepare(None, RuntimeMethod::SessionPromptSubmit)
                        .await;
                }
            }
        };
        let (_, admission) = tokio::time::timeout(Duration::from_secs(2), async {
            tokio::try_join!(dispatch, next)
        })
        .await??;
        assert!(admission.is_some());
        drop(admission);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test(start_paused = true)]
    async fn input_timeout_waits_without_admission_and_releases_stale_waits() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let (sink, _output) = forge_protocol::outbound::channel();
        controller.attach_timeout(&sink).await?;
        let broker = controller.broker.lock().await;
        tokio::task::yield_now().await;
        tokio::time::advance(Duration::from_secs(60)).await;
        tokio::task::yield_now().await;
        assert_eq!(controller.coordinator.execution.available_permits(), 1);

        let schedule = crate::session::InputSchedule {
            revision: 1,
            identity: Some("exchange:question".into()),
            question_revision: Some(1),
            deadline: crate::session::InputTimeout {
                amount: 1,
                unit: crate::session::TimeoutUnit::S,
            }
            .deadline(0)?,
            approval_allowed: false,
        };
        controller
            .observer
            .input_schedule
            .send_replace(schedule.clone());
        tokio::task::yield_now().await;
        tokio::time::advance(Duration::from_millis(999)).await;
        tokio::task::yield_now().await;
        assert_eq!(controller.coordinator.execution.available_permits(), 1);
        tokio::time::advance(Duration::from_millis(1)).await;
        tokio::task::yield_now().await;
        assert_eq!(controller.coordinator.execution.available_permits(), 0);
        controller
            .observer
            .input_schedule
            .send_replace(crate::session::InputSchedule {
                revision: 2,
                ..Default::default()
            });
        tokio::task::yield_now().await;
        assert_eq!(controller.coordinator.execution.available_permits(), 1);

        let execution = controller.coordinator.execution.acquire().await?;
        controller
            .observer
            .input_schedule
            .send_replace(crate::session::InputSchedule {
                revision: 3,
                ..schedule
            });
        tokio::task::yield_now().await;
        controller.cancellation.request(false);
        tokio::task::yield_now().await;
        drop(execution);
        tokio::task::yield_now().await;
        assert_eq!(controller.coordinator.execution.available_permits(), 1);
        drop(broker);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn input_timeout_resumes_restored_questions_once_without_a_view() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let mut store = SqliteStore::open(&controller.data_root)?;
        let mut exchange = crate::exchange::Exchange::agent_admission(
            registry.initial_session_id(),
            1,
            "Await an answer".into(),
            0,
        );
        exchange.set_awaiting_input(true);
        exchange.pause(0);
        let mut elicitation = crate::plan::PlanElicitation::new(
            crate::plan::PlanQuestionSet::freeform("Which option?".into()).normalize()?,
        );
        elicitation.deadline = Some(serde_json::from_value(json!({"expires_at_ms":-1}))?);
        exchange.set_elicitation(Some(elicitation));
        store.save_exchange(&exchange)?;
        controller.broker.lock().await.refresh_input_schedule()?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        controller.attach_timeout(&sink).await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let frame = output
                    .recv()
                    .await?
                    .context("timeout worker output closed")?;
                let message: Value = serde_json::from_slice(frame.bytes())?;
                ensure!(
                    message["event"] != "backend_event" || message["payload"]["kind"] != "error",
                    "{message}"
                );
                if message["event"] == "question_answered"
                    && message["payload"]["resolution_source"] == "timeout"
                {
                    break;
                }
            }
            loop {
                tokio::task::yield_now().await;
                if controller.coordinator.execution.available_permits() == 1 {
                    break;
                }
            }
            Ok::<(), anyhow::Error>(())
        })
        .await??;
        let resolved = store.list_exchange(registry.initial_session_id())?;
        assert!(resolved.iter().all(|exchange| !exchange.awaiting_input()));
        let count = resolved.len();
        controller.broker.lock().await.refresh_input_schedule()?;
        tokio::task::yield_now().await;
        assert_eq!(
            store.list_exchange(registry.initial_session_id())?.len(),
            count
        );
        assert_eq!(controller.coordinator.execution.available_permits(), 1);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn execution_admission_excludes_overlapping_work_and_pending_cleanup() {
        let fixture = tempfile::tempdir().unwrap();
        let service = service();
        assert!(
            service
                .open_session(1, initialize(&fixture, "mock"))
                .await
                .unwrap()
                .result()
                .is_some()
        );
        let execution = service
            .prepare(None, RuntimeMethod::SessionPromptSubmit)
            .await
            .unwrap()
            .unwrap();
        assert!(
            service
                .prepare(None, RuntimeMethod::SessionGoalResume)
                .await
                .unwrap_err()
                .is::<crate::broker::RequestNotAdmitted>()
        );
        assert!(
            service
                .prepare(None, RuntimeMethod::SessionTurnCancel)
                .await
                .unwrap()
                .is_none()
        );
        assert!(
            service
                .prepare(None, RuntimeMethod::SessionStateGet)
                .await
                .unwrap()
                .is_none()
        );
        let registry = service.registry().await.unwrap();
        let controller = registry
            .resolve(registry.initial_session_id())
            .await
            .unwrap();
        let cleanup = Arc::clone(&controller.coordinator.control)
            .try_acquire_owned()
            .unwrap();
        drop(execution);
        assert!(
            service
                .prepare(None, RuntimeMethod::SessionCompact)
                .await
                .unwrap_err()
                .is::<crate::broker::RequestNotAdmitted>()
        );
        drop(cleanup);
        let resumed = service
            .prepare(None, RuntimeMethod::SessionCompact)
            .await
            .unwrap()
            .unwrap();
        drop(resumed);
        let prompt = service
            .prepare(None, RuntimeMethod::SessionPromptSubmit)
            .await
            .unwrap()
            .unwrap();
        assert!(
            service
                .prepare(None, RuntimeMethod::SessionPromptSubmit)
                .await
                .is_err()
        );
        assert!(
            service
                .prepare(None, RuntimeMethod::SessionCompact)
                .await
                .is_err()
        );
        let cleanup = Arc::clone(&controller.coordinator.control)
            .try_acquire_owned()
            .expect("chat must not retain cleanup admission while its model runs");
        assert!(
            service
                .prepare(None, RuntimeMethod::SessionCheckEditingSet)
                .await
                .unwrap()
                .is_none(),
            "check-editing toggles must not need provider execution admission"
        );
        assert!(
            service
                .prepare(None, RuntimeMethod::SessionTurnCancel)
                .await
                .unwrap()
                .is_none()
        );
        drop(cleanup);
        drop(prompt);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    struct CheckCleanupBackend {
        started: tokio::sync::Notify,
        release: tokio::sync::Notify,
        completed: AtomicBool,
    }

    #[async_trait::async_trait]
    impl crate::backend::Backend for CheckCleanupBackend {
        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
        async fn cleanup_execution(&self, _: &str) -> Result<()> {
            self.started.notify_one();
            self.release.notified().await;
            self.completed.store(true, Ordering::Release);
            Ok(())
        }
        async fn cancel_session(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn has_active_turn(&self, _: &str) -> bool {
            !self.completed.load(Ordering::Acquire)
        }
        async fn active_session_id_for(&self, _: &str) -> Option<String> {
            Some("old-provider-turn".into())
        }
        fn descriptor(&self) -> crate::backend::BackendDescriptor {
            crate::backend::BackendDescriptor {
                kind: crate::backend::BackendKind::Mock,
                label: "Cleanup test".into(),
                capability: Default::default(),
            }
        }
        async fn prompt_stream(
            &self,
            _: crate::backend::BackendRequest,
            _: Option<crate::backend::BackendEventSink>,
        ) -> Result<crate::backend::BackendOutput> {
            anyhow::bail!("cleanup must finish before another provider turn")
        }
    }

    #[tokio::test]
    async fn direct_prompt_collects_active_checks_before_admission() -> Result<()> {
        for interrupt_execution in [None, Some(false), Some(true)] {
            let fixture = tempfile::tempdir()?;
            std::fs::write(
                fixture.path().join(".forge.json"),
                serde_json::to_vec(&json!({
                    "checks":{"waiting":{"trigger":{"auto":false},"action":{"command":["nu", "--no-config-file","-c","sleep 30sec"],"label":"Waiting check","category":"test"}}}
                }))?,
            )?;
            let service = service();
            assert!(
                service
                    .open_session(1, initialize(&fixture, "mock"))
                    .await?
                    .error()
                    .is_none()
            );
            let registry = service.registry().await?;
            let mut controller = registry
                .controller_by_id
                .write()
                .await
                .remove(registry.initial_session_id())
                .unwrap();
            let backend = Arc::new(CheckCleanupBackend {
                started: tokio::sync::Notify::new(),
                release: tokio::sync::Notify::new(),
                completed: AtomicBool::new(false),
            });
            Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
            registry
                .controller_by_id
                .write()
                .await
                .insert(registry.initial_session_id().to_owned(), controller.clone());
            let (sink, mut output) = forge_protocol::outbound::channel();
            let drain =
                tokio::spawn(async move { while output.recv().await.ok().flatten().is_some() {} });
            service
                .dispatch(
                    None,
                    Request {
                        id: 2,
                        method: "session.check.run".into(),
                        params: json!({"check":"waiting"}),
                    },
                    &sink,
                )
                .await?;
            let run_id = controller.checks.active()?.context("check did not start")?;
            tokio::time::timeout(Duration::from_secs(5), async {
                loop {
                    let run = controller.checks.status(&run_id, 100, false).await?;
                    if run
                        .commands
                        .iter()
                        .any(|command| command.state == crate::checks::CheckState::Running)
                    {
                        break Ok::<(), anyhow::Error>(());
                    }
                    ensure!(
                        run.state == crate::checks::CheckState::Running,
                        "check failed before cancellation: {:?}",
                        run.state
                    );
                }
            })
            .await??;
            let snapshot = controller.observer.snapshot()?;
            assert!(snapshot.checks_running);
            assert_eq!(snapshot.active_check_run.as_deref(), Some(run_id.as_str()));
            assert!(
                service
                    .prepare(None, RuntimeMethod::SessionStateGet)
                    .await?
                    .is_none()
            );
            assert!(
                service
                    .prepare(None, RuntimeMethod::SessionGoalResume)
                    .await
                    .is_err()
            );
            assert_eq!(
                controller.checks.active()?.as_deref(),
                Some(run_id.as_str())
            );
            let mut preparation = Box::pin(async {
                if let Some(interrupt_execution) = interrupt_execution {
                    service.dispatch(None, Request {
                        id: 3, method: "session.check.cancel".into(),
                        params: json!({"run_id":run_id,"interrupt_execution":interrupt_execution}),
                    }, &sink).await?;
                }
                service
                    .prepare(None, RuntimeMethod::SessionPromptSubmit)
                    .await
            });
            tokio::select! {
                result = &mut preparation => panic!("prompt admitted before provider cleanup: {result:?}"),
                _ = backend.started.notified() => {}
                _ = tokio::time::sleep(Duration::from_secs(2)) => panic!("provider cleanup did not start"),
            }
            assert!(!backend.completed.load(Ordering::Acquire));
            assert!(futures_util::poll!(preparation.as_mut()).is_pending());
            backend.release.notify_one();
            let admission = tokio::time::timeout(Duration::from_secs(5), preparation)
                .await
                .context("checked prompt admission did not finish after provider cleanup")??;
            assert!(backend.completed.load(Ordering::Acquire));
            assert!(admission.is_some());
            assert!(controller.checks.active()?.is_none());
            assert_eq!(
                controller.checks.status(&run_id, 0, false).await?.state,
                crate::checks::CheckState::Interrupted
            );
            assert!(!controller.observer.snapshot()?.checks_running);
            drop(admission);
            service.shutdown(Duration::from_secs(1)).await?;
            drop(sink);
            drain.abort();
        }
        Ok(())
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
            .prepare(None, RuntimeMethod::SessionPromptSubmit)
            .await
            .unwrap()
            .unwrap();
        let registry = service.registry().await.unwrap();
        let controller = registry
            .resolve(registry.initial_session_id())
            .await
            .unwrap();
        let maintenance = controller.broker.lock().await;
        let mut acquisition = Box::pin(acquire_request_broker(
            &controller,
            RuntimeMethod::SessionPromptSubmit,
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
    async fn configuration_waits_for_maintenance_without_provider_cleanup() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let broker = HarnessBroker::initialize(initialize(&fixture, "mock"))?;
        let mut controller = SessionController::new(broker).unwrap();
        let backend = Arc::new(SteeringBackend {
            request: Mutex::new(Vec::new()),
        });
        Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
        for method in [RuntimeMethod::SessionConfigure] {
            let maintenance = controller.broker.lock().await;
            let mut acquisition = Box::pin(acquire_request_broker(&controller, method));
            assert!(futures_util::poll!(acquisition.as_mut()).is_pending());
            assert!(
                backend.request.lock().await.is_empty(),
                "maintenance cannot trigger provider cleanup"
            );
            assert!(controller.coordinator.execution.try_acquire().is_err());
            drop(maintenance);
            let admitted = tokio::time::timeout(Duration::from_secs(2), acquisition).await??;
            assert!(controller.coordinator.execution.try_acquire().is_err());
            drop(admitted);
            assert!(controller.coordinator.execution.try_acquire().is_ok());
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
            (2, "host.trace.configure", json!({"enabled":true})),
            (3, "host.trace.status", json!({})),
            (4, "session.trace.clear", json!({})),
            (5, "host.trace.configure", json!({"enabled":false})),
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
                    method: "session.prompt.history.record".into(),
                    params: json!({"text":"still alive"}),
                },
                &sink,
            )
            .await
            .unwrap();
        output.check().unwrap();
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    #[tokio::test(start_paused = true)]
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
                .prepare(None, RuntimeMethod::SessionPromptSubmit)
                .await
                .is_err()
        );
        let contender_diff = DiffEngine::new(CacheLimits::default(), 1);
        let contender_syntax = SyntaxEngine::new(contender_diff.analysis_pool());
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
    async fn task_state_controls_never_wait_for_provider_or_broker() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let broker = HarnessBroker::initialize(initialize(&fixture, "mock"))?;
        let mut controller = SessionController::new(broker)?;
        let backend = Arc::new(SteeringBackend {
            request: Mutex::new(Vec::new()),
        });
        Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
        let session_id = controller.observer.session_id().to_owned();
        let mut store = SqliteStore::open(&controller.data_root)?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        let execution = Arc::clone(&controller.coordinator.execution).try_acquire_owned()?;
        let broker_owner = controller.broker.lock().await;
        let provider_stop = backend.request.lock().await;
        for (index, action) in ["pause", "clear"].into_iter().enumerate() {
            let previous = controller.task_controller.current()?;
            controller.task_controller.change(
                &mut store,
                previous.as_ref().map(|task| &task.revision),
                crate::task::TaskChange::Install(Box::new(crate::task::ActiveTask::goal(
                    "Preserve provider ownership".into(),
                    Default::default(),
                    false,
                    1,
                ))),
                1,
            )?;
            let request = Request {
                id: index as u64 + 1,
                method: "session.command.apply".into(),
                params: json!({"operation_id":format!("locked-{action}"),"action":action}),
            };
            assert!(
                tokio::time::timeout(
                    Duration::from_secs(1),
                    task_state::apply(&controller, &session_id, &request, 20, &sink)
                )
                .await??
            );
            if action == "pause" {
                assert_eq!(
                    controller.task_controller.current()?.unwrap().status(),
                    crate::task::TaskStatus::Paused
                );
            } else {
                assert!(controller.task_controller.current()?.is_none());
            }
            assert!(!*controller.cancellation.subscribe().borrow());
            assert_eq!(controller.coordinator.execution.available_permits(), 0);
            assert!(
                provider_stop.is_empty(),
                "task state reached provider control"
            );
            assert_eq!(
                store
                    .load_task_operation(&session_id, &format!("locked-{action}"))?
                    .unwrap()
                    .state,
                "completed"
            );
        }
        drop(provider_stop);
        drop(broker_owner);
        drop(execution);
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
        Ok(())
    }

    #[tokio::test]
    async fn task_input_reads_the_current_session_revision() -> Result<()> {
        use crate::input::InputPurpose;
        use crate::task::{ActiveTask, TaskChange};
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let mut store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let task = controller
            .task_controller
            .change(
                &mut store,
                None,
                TaskChange::Install(Box::new(ActiveTask::goal(
                    "Finish".into(),
                    Default::default(),
                    false,
                    1,
                ))),
                1,
            )?
            .unwrap();
        let completed_turn = crate::task::TaskTurnId {
            exchange_ordinal: 1,
            turn_ordinal: 1,
        };
        controller.task_controller.observe_goal(
            &mut store,
            &task.permit(),
            completed_turn.clone(),
            crate::goal::TurnEvidence {
                tool_called: true,
                ..Default::default()
            },
            2,
        )?;
        let task = controller.task_controller.current()?.unwrap();
        let pending = InputPurpose::TaskContinuation {
            revision: task.revision.clone(),
            id: task
                .turn
                .as_ref()
                .unwrap()
                .continuation
                .as_ref()
                .unwrap()
                .id
                .clone(),
            completed_turn_id: completed_turn,
            reason: "Continue".into(),
        };
        assert!(controller.task_input_is_current(&pending)?);
        let paused = controller
            .task_controller
            .change(&mut store, Some(&task.revision), TaskChange::Pause, 2)?
            .unwrap();
        assert!(!controller.task_input_is_current(&pending)?);
        assert!(controller.task_input_is_current(&InputPurpose::User)?);
        let replacement = controller
            .task_controller
            .change(
                &mut store,
                Some(&paused.revision),
                TaskChange::Install(Box::new(ActiveTask::goal(
                    "Replacement".into(),
                    Default::default(),
                    false,
                    3,
                ))),
                3,
            )?
            .unwrap();
        assert!(!controller.task_input_is_current(&pending)?);
        assert!(controller.task_input_is_current(&InputPurpose::TaskEntry {
            revision: replacement.revision.clone(),
            id: crate::task::TaskInputId::fresh(),
            start: crate::exchange::TaskStart {
                kind: replacement.kind(),
                title: replacement.title().into(),
            },
        })?);
        controller.task_controller.change(
            &mut store,
            Some(&replacement.revision),
            TaskChange::Clear,
            4,
        )?;
        assert!(!controller.task_input_is_current(&pending)?);
        assert!(controller.task_input_is_current(&InputPurpose::User)?);
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn goal_state_commit_does_not_wait_for_or_cancel_the_running_owner() -> Result<()> {
        for action in ["goal", "plan"] {
            let fixture = tempfile::tempdir()?;
            let service = service();
            service
                .open_session(1, initialize(&fixture, "mock"))
                .await?;
            let registry = service.registry().await?;
            let session_id = registry.initial_session_id().to_owned();
            let controller = registry.resolve(&session_id).await?;
            let broker = controller.broker.lock().await;
            let running = controller
                .coordinator
                .execution
                .clone()
                .acquire_owned()
                .await?;
            let (sink, _output) = forge_protocol::outbound::channel();
            let request = Request {
                id: 2,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"promote","action":action,"text":"Finish the work"}),
            };
            service.prepare_task(Some(&session_id), &request).await?;
            assert!(
                SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?
                    .load_task_operation(&session_id, "promote")?
                    .is_none()
            );
            tokio::time::timeout(
                Duration::from_secs(1),
                task_state::apply(&controller, &session_id, &request, 20, &sink),
            )
            .await??;
            let task = controller.task_controller.current()?.unwrap();
            assert_eq!(task.title(), "Finish the work");
            assert!(!task.entry_accepted());
            assert!(!*controller.cancellation.subscribe().borrow());
            let clear = Request {
                id: 3,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"clear","action":"clear"}),
            };
            task_state::apply(&controller, &session_id, &clear, 20, &sink).await?;
            assert!(controller.task_controller.current()?.is_none());
            drop(running);
            drop(broker);
            service.shutdown(Duration::from_secs(1)).await?;
        }
        Ok(())
    }

    #[tokio::test]
    async fn user_input_cannot_impersonate_task_continuation() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let (sink, _output) = forge_protocol::outbound::channel();
        let result = controller.input_request(Request {
            id: 2,
            method: "session.input.apply".into(),
            params: json!({
                "id":"forged-continuation", "action":{"kind":"submit", "message":{
                    "id":"forged-message", "text":"Continue automatically",
                    "purpose":{"kind":"task_continuation", "revision":"untrusted", "completed_turn_id":"turn", "reason":"forged"}
                }}
            }),
        }, RuntimeMethod::SessionInputApply, &sink).await;
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("only be generated by Harness")
        );
        assert!(
            controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty()
        );
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn host_owned_input_steers_and_preserves_edit_and_pause_barriers() -> Result<()> {
        use crate::backend::mock_response::MockResponseCommand;
        use crate::input::{InputAction, InputEdit, InputIntent, InputMessage, InputOperation};
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let session_id = registry.initial_session_id().to_owned();
        let control = controller.backend.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = held["id"].as_str().unwrap().to_owned();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let message = |id: &str, intent: InputIntent| InputOperation {
            id: format!("operation-{id}"),
            control_revision: 0,
            after_control: None,
            action: InputAction::Submit {
                message: Box::new(InputMessage {
                    id: id.into(),
                    revision: 0,
                    text: format!("input {id}"),
                    purpose: crate::input::InputPurpose::User,
                    intent,
                    delivery: Default::default(),
                    route: None,
                    attachments: vec![],
                    config: None,
                    reservation: None,
                    submission_id: None,
                    error: None,
                }),
            },
        };
        controller
            .coordinator
            .submissions
            .apply(message("first", InputIntent::Auto))?;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await
        .with_context(|| {
            format!(
                "first input did not reach provider: queue={:?}, task={:?}",
                controller.coordinator.submissions.snapshot(),
                controller
                    .observer
                    .snapshot()
                    .map(|snapshot| snapshot.session.active_task)
            )
        })??;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .iter()
                    .any(|message| {
                        message.id == "first"
                            && message.delivery == crate::input::InputDelivery::Accepted
                    })
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("input delivery boundary 2")??;
        assert_eq!(
            controller.coordinator.execution.available_permits(),
            0,
            "delivery acknowledgement must not release active execution"
        );
        controller
            .coordinator
            .submissions
            .apply(message("queued", InputIntent::Queue))?;
        controller
            .coordinator
            .submissions
            .apply(message("steered", InputIntent::Auto))?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if !controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .iter()
                    .any(|message| message.id == "steered")
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("input delivery boundary 3")??;
        controller
            .coordinator
            .submissions
            .apply(message("queued", InputIntent::Queue))?;
        controller.coordinator.submissions.apply(InputOperation {
            id: "edit".into(),
            control_revision: 0,
            after_control: None,
            action: InputAction::Reserve {
                id: "queued".into(),
                revision: 1,
                reservation: "editor".into(),
            },
        })?;
        controller.coordinator.submissions.suspend(true, None)?;
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        let queued = controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .into_iter()
            .find(|message| message.id == "queued")
            .unwrap();
        assert_eq!(queued.reservation.as_deref(), Some("editor"));
        controller.coordinator.submissions.apply(InputOperation {
            id: "commit".into(),
            control_revision: 0,
            after_control: None,
            action: InputAction::Commit {
                reservation: "editor".into(),
                edits: vec![InputEdit {
                    id: "queued".into(),
                    revision: queued.revision,
                    text: "edited follow-up".into(),
                    attachments: vec![],
                    config: None,
                }],
            },
        })?;
        assert!(controller.coordinator.submissions.snapshot()?.suspended);
        controller.coordinator.submissions.suspend(false, None)?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .is_empty()
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("input delivery boundary 4")??;
        assert_eq!(controller.coordinator.execution.available_permits(), 1);
        let exchange = SqliteStore::open(&controller.data_root)?.list_exchange(&session_id)?;
        assert_eq!(
            exchange
                .iter()
                .filter(|exchange| exchange.input_message_id.as_deref() == Some("first"))
                .count(),
            1
        );
        assert_eq!(exchange.iter().flat_map(|exchange| &exchange.node_list).filter(|node|
            matches!(node, crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.id == "steered")
        ).count(), 1, "steering must be recorded once under its original message identity");
        service.shutdown(Duration::from_secs(5)).await?;
        drop(sink);
        reader.await.context("input delivery boundary 5")??;
        Ok(())
    }

    #[tokio::test]
    async fn input_retry_sends_once_and_never_replays_uncertain_delivery() -> Result<()> {
        use crate::backend::submission::{SubmissionStore, SubmissionTarget};
        use crate::input::{InputDelivery, InputOperation};
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let session_id = registry.initial_session_id().to_owned();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        let message = |id: &str| -> Result<InputOperation> {
            Ok(serde_json::from_value(
                json!({"id":format!("submit-{id}"), "action":{
                    "kind":"submit", "message":{"id":id,"text":"retry regression","intent":"auto"}
                }}),
            )?)
        };
        controller
            .coordinator
            .submissions
            .apply(message("retry-message")?)?;
        let failed = controller.coordinator.submissions.delivery(
            "retry-message",
            Some(InputDelivery::Failed),
            None,
            Some("failed before dispatch".into()),
        )?;
        let params = json!({"id":"retry-operation","action":{"kind":"retry", "id":"retry-message", "revision":failed.upsert[0].revision}});
        for id in [2, 3] {
            controller
                .input_request(
                    Request {
                        id,
                        method: "session.input.apply".into(),
                        params: params.clone(),
                    },
                    RuntimeMethod::SessionInputApply,
                    &sink,
                )
                .await?;
        }
        controller.attach_input(&sink).await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            while !controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty()
            {
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await??;
        let exchange = SqliteStore::open(&controller.data_root)?.list_exchange(&session_id)?;
        assert_eq!(
            exchange
                .iter()
                .filter(|exchange| exchange.input_message_id.as_deref() == Some("retry-message"))
                .count(),
            1
        );

        let control = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        controller
            .coordinator
            .submissions
            .apply(message("uncertain-message")?)?;
        let journal = SubmissionStore::open(&controller.data_root, controller.trace.clone())?;
        let submission = journal.prepare_input(
            &controller.catalog_request.read().await.clone(),
            crate::backend::BackendInput::from_text("uncertain"),
            crate::backend::BackendKind::Mock,
            Some("uncertain-message".into()),
        )?;
        submission.sending(SubmissionTarget::Untracked)?;
        let uncertain = controller.coordinator.submissions.delivery(
            "uncertain-message",
            Some(InputDelivery::Unknown),
            Some(submission.id().into()),
            Some("lost acknowledgement".into()),
        )?;
        drop(control);
        let failure = controller.input_request(Request { id:4, method:"session.input.apply".into(),
            params:json!({"id":"retry-uncertain","action":{"kind":"retry","id":"uncertain-message","revision":uncertain.upsert[0].revision}})
        }, RuntimeMethod::SessionInputApply, &sink).await.unwrap_err();
        assert!(format!("{failure:#}").contains("unconfirmed"));
        assert_eq!(
            controller.coordinator.submissions.snapshot()?.messages[0].delivery,
            InputDelivery::Unknown
        );
        assert_eq!(
            SqliteStore::open(&controller.data_root)?
                .list_exchange(&session_id)?
                .len(),
            exchange.len()
        );
        service.shutdown(Duration::from_secs(5)).await?;
        drop(sink);
        reader.await??;
        Ok(())
    }

    #[tokio::test]
    async fn reopening_active_and_deleting_idle_cached_sessions() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let first_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let registry = service.registry().await?;
        let first = registry.resolve(&first_id).await?;
        let second = prepare_new_session(
            std::path::Path::new(&registry.initialize.data_root),
            &registry.initialize.client_id,
            "mock",
            false,
            &first_id,
            &json!({"name":"second"}),
            1,
        )?;
        registry.resolve(&second.id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let execution = first.coordinator.execution.clone().acquire_owned().await?;
        let broker = first.broker.lock().await;
        tokio::time::timeout(
            Duration::from_secs(2),
            service.dispatch(
                Some(second.id.clone()),
                Request {
                    id: 2,
                    method: "session.resume".into(),
                    params: json!({"session_id":first_id}),
                },
                &sink,
            ),
        )
        .await??;
        let frame = output.recv().await?.context("resume response is missing")?;
        let reply: Value = serde_json::from_slice(frame.bytes())?;
        assert_eq!(reply["result"]["session"]["id"], first_id);
        assert!(
            service
                .dispatch(
                    Some(second.id.clone()),
                    Request {
                        id: 3,
                        method: "session.delete".into(),
                        params: json!({"session_id":first_id})
                    },
                    &sink
                )
                .await
                .is_err()
        );
        drop(broker);
        drop(execution);
        service
            .dispatch(
                Some(second.id.clone()),
                Request {
                    id: 4,
                    method: "session.delete".into(),
                    params: json!({"session_id":first_id}),
                },
                &sink,
            )
            .await?;
        assert!(
            !registry
                .controller_by_id
                .read()
                .await
                .contains_key(&first_id)
        );
        assert!(
            SqliteStore::open(std::path::Path::new(&registry.initialize.data_root))?
                .load_session(&first_id)?
                .is_none()
        );
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn sqlite_contention_fails_explicitly_and_leaves_host_usable() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let configuration = initialize(&fixture, "mock");
        let root = PathBuf::from(&configuration.data_root);
        let opened = service.open_session(1, configuration).await?;
        let session = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let mut external = rusqlite::Connection::open(root.join("harness.sqlite3"))?;
        let lock = external.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let failure = service
            .dispatch(
                Some(session.clone()),
                Request {
                    id: 2,
                    method: "session.prompt.history.record".into(),
                    params: json!({"text":"blocked write"}),
                },
                &sink,
            )
            .await
            .unwrap_err();
        let diagnostic = crate::storage::error_message(&failure);
        assert!(diagnostic.contains("SQLite contention"));
        assert!(diagnostic.contains("5 seconds"));
        lock.rollback()?;
        service
            .dispatch(
                Some(session),
                Request {
                    id: 3,
                    method: "session.prompt.history.record".into(),
                    params: json!({"text":"successful retry"}),
                },
                &sink,
            )
            .await?;
        let reply = output
            .recv()
            .await?
            .context("history retry response is missing")?;
        let reply: Value = serde_json::from_slice(reply.bytes())?;
        assert_eq!(reply["id"], 3);
        assert!(reply["error"].is_null());
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn startup_reports_session_loading_and_presentation_timings() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let timing = &opened.result().unwrap()["startup_timing"];
        for (scope, stages) in [
            (
                "phases",
                &[
                    "registry_ready_ms",
                    "settings_catalog_ms",
                    "repository_ready_ms",
                    "runtime_ready_ms",
                    "broker_ready_ms",
                    "snapshot_serialized_ms",
                    "controller_ready_ms",
                ][..],
            ),
            (
                "broker",
                &[
                    "database_open_ms",
                    "deletion_recovery_ms",
                    "session_selected_ms",
                    "settings_applied_ms",
                    "session_recovered_ms",
                    "services_created_ms",
                    "timeline_loaded_ms",
                    "presentation_initialized_ms",
                ][..],
            ),
        ] {
            let mut previous = 0.0;
            for stage in stages {
                let elapsed = timing[scope][stage]
                    .as_f64()
                    .expect("startup phase missing");
                assert!(elapsed.is_finite() && elapsed >= previous);
                previous = elapsed;
            }
        }
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    fn service() -> HarnessService {
        let diff = DiffEngine::new(CacheLimits::default(), 1);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
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
