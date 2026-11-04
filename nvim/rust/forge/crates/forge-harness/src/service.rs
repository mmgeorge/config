use std::collections::HashMap;
use std::io::Write;
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

#[path = "service/cancellation.rs"]
mod cancellation;
#[path = "service/check_completion.rs"]
mod check_completion;
#[path = "service/command.rs"]
mod command;
#[path = "service/compaction.rs"]
mod compaction;
#[path = "service/input.rs"]
mod input_delivery;
#[cfg(test)]
#[path = "service/input_durability_test.rs"]
mod input_durability_test;
#[path = "service/local_configuration.rs"]
mod local_configuration;
#[path = "service/native_permission.rs"]
mod native_permission;
#[path = "service/native_settings.rs"]
mod native_settings;
#[path = "service/owner_commands.rs"]
mod owner_commands;
#[cfg(test)]
#[path = "service/plan_question_event_test.rs"]
mod plan_question_event_test;
#[path = "service/prompt.rs"]
mod prompt;
#[path = "service/provider_lifecycle.rs"]
mod provider_lifecycle;
#[cfg(test)]
#[path = "service/provider_startup_test.rs"]
mod provider_startup_test;
#[cfg(test)]
#[path = "service/question_event_test.rs"]
mod question_event_test;
#[cfg(test)]
#[path = "service/question_wait_test.rs"]
mod question_wait_test;
#[path = "service/reconnect.rs"]
mod reconnect;
#[path = "service/review_questions.rs"]
mod review_questions;
#[cfg(test)]
#[path = "service/session_observation_test.rs"]
mod session_observation_test;
#[path = "service/shared.rs"]
mod shared;
#[path = "service/task_state.rs"]
mod task_state;

#[derive(Clone, Copy)]
enum CheckOperation {
    List,
    Status,
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
            _ => return None,
        })
    }
}

/// Defines provider settings for one-shot generation without a conversation lease.
#[derive(Clone, Debug, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TextGenerationConfig {
    #[serde(alias = "backend")]
    pub provider: crate::provider::ProviderLaunch,
    pub model: String,
    /// Explicit effort for the isolated request, independent of the active conversation.
    pub reasoning_effort: Option<String>,
    #[serde(default)]
    /// Provider speed for Background requests, independent of the active conversation.
    pub service_tier: crate::provider::ServiceTier,
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
        purpose: crate::provider::TextGeneration,
        input: &str,
    ) -> Result<String> {
        let _activity = self.admit().await?;
        let mut model = config.model;
        if model == "default"
            && config.data_root.join("harness.sqlite3").is_file()
            && let Some(preference) = SqliteStore::open(&config.data_root)?
                .load_preference(workspace, config.provider.kind.as_str())?
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
        let permission = Arc::new(crate::provider::approval::PermissionCoordinator::new(
            Arc::new(std::sync::RwLock::new(permission_store)),
        ));
        let trace = Arc::new(crate::trace::TraceStore::open(&config.data_root)?);
        let provider =
            crate::provider::build(config.provider, permission, trace.clone(), config.data_root)?;
        let request = crate::provider::ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: format!("generation-{}", uuid::Uuid::new_v4()),
            workspace: workspace.to_owned(),
            access: provider
                .descriptor()
                .capability
                .resolve_access(crate::session::AccessPolicy {
                    write_access: crate::session::WriteScope::Off,
                    ..Default::default()
                }),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
        };
        let generation = generate_isolated_text(
            provider.as_ref(),
            request,
            purpose,
            &model,
            config.reasoning_effort.as_deref(),
            input,
        )
        .await;
        drop(provider);
        let collected = tokio::task::spawn_blocking(move || trace.close()).await;
        // Diagnostic collection cannot change an already established model result.
        match collected {
            Ok(Ok(())) => {}
            Ok(Err(error)) => {
                let _ = writeln!(
                    std::io::stderr(),
                    "Forge Harness isolated trace collection failed: {error}"
                );
            }
            Err(error) => {
                let _ = writeln!(
                    std::io::stderr(),
                    "Forge Harness isolated trace collector failed: {error}"
                );
            }
        }
        generation
    }

    pub async fn saved_diff(
        &self,
        session_id: Option<String>,
        input: forge_buffer::input::DocumentInput,
    ) -> Result<forge_diff::saved_source::SavedDiffSource> {
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
            crate::buffer::projection::TranscriptAction::Diff {
                text, checkpoint, ..
            } => {
                let data_root = controller.data_root.clone();
                tokio::task::spawn_blocking(move || {
                    crate::buffer::saved_diff::capture(&data_root, checkpoint.as_ref(), text)
                })
                .await
                .context("saved diff source worker failed")
            }
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
        if let Some(result) = crate::buffer::session::diff_view::update(
            controller.presentation.clone(),
            &request,
            controller.data_root.clone(),
            self.diff.clone(),
            self.syntax.clone(),
        )
        .await?
        {
            return Ok(result);
        }
        match &request {
            crate::buffer::session::PresentationRequest::Recap { model }
            | crate::buffer::session::PresentationRequest::SessionName { model } => {
                let purpose = if matches!(
                    &request,
                    crate::buffer::session::PresentationRequest::SessionName { .. }
                ) {
                    crate::provider::TextGeneration::SessionName
                } else {
                    crate::provider::TextGeneration::Recap
                };
                let history = controller
                    .presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .conversation_history()?;
                let request = controller.catalog_request.read().await.clone();
                let text = controller
                    .provider_handle()
                    .generate_text(request, purpose, model, None, &history)
                    .await?;
                return Ok(json!({"text": purpose.validate(&text)?}));
            }
            crate::buffer::session::PresentationRequest::TerminateTerminal { id } => {
                let request = controller.catalog_request.read().await.clone();
                tokio::time::timeout(
                    Duration::from_secs(8),
                    controller.provider_handle().terminal_terminate(
                        request,
                        &crate::provider::terminal::TerminalId::new(id.clone())?,
                    ),
                )
                .await
                .context("background terminal termination timed out")??;
                return Ok(json!({}));
            }
            crate::buffer::session::PresentationRequest::BackgroundTerminals => {
                if !controller
                    .provider_handle()
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

    /// Start the opened conversation's provider without waiting for network initialization.
    pub async fn start_provider_session(
        &self,
        session_id: &str,
        sink: &MessageSender,
    ) -> Result<()> {
        let registry = self.registry().await?;
        let controller = registry.resolve(session_id).await?;
        controller.start_provider(sink).await;
        Ok(())
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

    /// Keep caller transport preparation free of lifecycle mutation and execution ownership.
    pub async fn prepare(
        &self,
        _session_id: Option<&str>,
        _method: RuntimeMethod,
    ) -> Result<Option<ExecutionPermit>> {
        let _activity = self.admit().await?;
        Ok(None)
    }

    /// Resolve task commands before choosing provider execution admission.
    ///
    /// Preserves command identity across repeated normalization and resolves
    /// acceptance replay from its receipt before consulting the current task.
    pub async fn normalize_request(
        &self,
        _session_id: Option<&str>,
        request: &mut Request,
    ) -> Result<()> {
        use session_command::SessionCommand;
        if matches!(
            RuntimeMethod::decode(&request.method)?,
            RuntimeMethod::SessionQuestionAnswer | RuntimeMethod::SessionQuestionSkip
        ) && request.params.get("command").is_some()
        {
            let _: crate::question::QuestionCommand =
                serde_json::from_value(request.params["command"].clone())?;
            request.method = "session.question.answer".into();
            return Ok(());
        }
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
        if RuntimeMethod::decode(&candidate.method)? == RuntimeMethod::SessionQuestionContinue {
            request.params = candidate.params;
            return Ok(());
        }
        let command = SessionCommand::parse_request(&candidate)?;
        if let Some(command) = command {
            let identity = request.params.get("_command").cloned();
            *request = command.to_request(request.id)?;
            if let Some(identity) = identity {
                request.params["_command"] = identity;
            }
        } else {
            request.params = candidate.params;
        }
        Ok(())
    }

    /// Persist transition ordering before the admitted request begins execution.
    pub async fn prepare_task(&self, session_id: Option<&str>, request: &Request) -> Result<()> {
        let _activity = self.admit().await?;
        let registry = self.registry().await?;
        let session_id = session_id.context("an explicit session_id is required")?;
        let controller = registry.resolve(session_id).await?;
        let mut request = request.clone();
        if let Some(params) = request.params.as_object_mut() {
            params.remove("_command");
        }
        if cancellation::handles(&request)
            || native_permission::handles(&request)
            || native_settings::handles(&request)
            || compaction::handles(&request)
            || local_configuration::handles(&request)
            || task_state::handles(&controller, &request)?
        {
            return Ok(());
        }
        anyhow::bail!("unsupported owner task action")
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
            return command::respond_status(&controller, &request, sink).await;
        }
        let Some(identity) = identity else {
            return Box::pin(self.dispatch_request(session_id, request, sink, admission)).await;
        };
        let identity: CommandIdentity = serde_json::from_value(identity)?;
        if cancellation::handles(&request)
            || native_permission::handles(&request)
            || native_settings::handles(&request)
            || compaction::handles(&request)
            || local_configuration::handles(&request)
        {
            request
                .params
                .as_object_mut()
                .context("command parameters must be an object")?
                .entry("operation_id")
                .or_insert_with(|| json!(identity.id));
        }
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
        if command::handles_owner_command(&controller, &request)? {
            request
                .params
                .as_object_mut()
                .context("command parameters must be an object")?
                .entry("operation_id")
                .or_insert_with(|| json!(identity.id));
            return Box::pin(self.dispatch_request(session_id, request, sink, admission)).await;
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
        let result =
            Box::pin(self.dispatch_request(session_id, request, &recorded, admission)).await;
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
        if method == RuntimeMethod::ProviderRegistryList {
            let result = json!({
                "default_provider": forge_providers::registry::default_id(),
                "providers": forge_providers::registry::descriptors(),
            });
            sink.send_response(Response::success(request.id, result)?)
                .await?;
            return Ok(());
        }
        let _activity = self.admit().await?;
        let registry = self.registry().await?;
        registry.runtime.lsp.attach(sink.clone());
        if shared::handles(method) {
            return shared::dispatch(registry, session_id, request, sink).await;
        }
        let session_id = session_id.context("an explicit session_id is required")?;
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
            RuntimeMethod::SessionPlanAccept
                | RuntimeMethod::SessionPlanChangesRequest
                | RuntimeMethod::SessionPlanCancel
                | RuntimeMethod::SessionPlanEntityRename
                | RuntimeMethod::SessionPlanTestDelete
                | RuntimeMethod::SessionPlanCheckUpdate
        ) {
            let controller = registry.resolve(&session_id).await?;
            if !task_state::normalize_review_retry(&controller, &mut request).await? {
                let params = request
                    .params
                    .as_object_mut()
                    .context("plan review submission requires an object")?;
                params.remove("review_document");
                if let Some(review) = params.remove("review") {
                    let input =
                        serde_json::from_value::<forge_buffer::input::DocumentInput>(review)?;
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
        }
        if let Some(stage) = capture_stage {
            stage.complete(json!({}));
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
            },
        ))
        .await;
        if let Some(stage) = edit_total {
            stage.complete(json!({"status":if result.is_ok() { "completed" } else { "failed" }}));
        }
        result
    }

    /// Close each session owner and retain all effect handles until collection and dispatch finish.
    /// A deadline leaves the registry and unfinished joins available for a later shutdown attempt.
    pub async fn shutdown(&self, deadline: Duration) -> Result<()> {
        self.accepting.store(false, Ordering::Release);
        tokio::time::timeout(deadline, self.background.shutdown())
            .await
            .context("Background shutdown timed out")??;
        tokio::time::timeout(deadline, async {
            let registry = self.registry.lock().await.clone();
            if let Some(registry) = &registry {
                registry.runtime.lsp.close();
                let controllers: Vec<_> = registry
                    .controller_by_id
                    .read()
                    .await
                    .values()
                    .cloned()
                    .collect();
                for controller in &controllers {
                    controller.input_stopping.store(true, Ordering::Release);
                    controller.coordinator.submissions.changed.notify_one();
                    controller.plan_review.close_all()?;
                    *controller.mcp_discovery.lock().await = Default::default();
                    controller.exchange_owner.close().await;
                }
                for controller in &controllers {
                    let session = controller.observer.session_id();
                    controller.trace.record(
                        session,
                        "service.shutdown.stage",
                        json!({"stage":"exchange_owner"}),
                    );
                    controller.finish_exchange_owner().await?;
                    controller.trace.record(
                        session,
                        "service.shutdown.stage",
                        json!({"stage":"prompt_settlement"}),
                    );
                    controller.finish_prompt_workers().await?;
                    controller.trace.record(
                        session,
                        "service.shutdown.stage",
                        json!({"stage":"provider_startup"}),
                    );
                    let mut startup = controller.provider_startup.lock().await;
                    if let Some(task) = startup.as_mut() {
                        task.await
                            .context("provider startup failed during shutdown")?;
                        startup.take();
                    }
                    controller.trace.record(
                        session,
                        "service.shutdown.stage",
                        json!({"stage":"input_worker"}),
                    );
                    let mut input = controller.input_task.lock().await;
                    if let Some(task) = input.as_mut() {
                        task.await.context("input worker shutdown failed")?;
                        input.take();
                    }
                    controller.trace.record(
                        session,
                        "service.shutdown.stage",
                        json!({"stage":"subscriptions"}),
                    );
                    for subscription in [&controller.timeout_task, &controller.agent_task] {
                        let mut subscription = subscription.lock().await;
                        if let Some(task) = subscription.as_mut() {
                            task.abort();
                            if let Err(error) = task.await
                                && !error.is_cancelled()
                            {
                                return Err(error).context("session subscription shutdown failed");
                            }
                            subscription.take();
                        }
                    }
                }
            }
            if let Some(registry) = &registry {
                for controller in registry.controller_by_id.read().await.values() {
                    controller.trace.record(
                        controller.observer.session_id(),
                        "service.shutdown.stage",
                        json!({"stage":"service_activity"}),
                    );
                }
            }
            let _quiescence = self.activity.write().await;
            if let Some(registry) = &registry {
                for controller in registry.controller_by_id.read().await.values() {
                    controller.trace.record(
                        controller.observer.session_id(),
                        "service.shutdown.stage",
                        json!({"stage":"service_activity_collected"}),
                    );
                }
            }
            let mut owner = self.registry.lock().await;
            if let Some(registry) = owner.as_ref() {
                let mut forks = registry.fork_task.lock().await;
                while let Some(result) = forks.join_next().await {
                    result.context("Harness provider fork task failed")?;
                }
                let controllers: Vec<_> = registry
                    .controller_by_id
                    .read()
                    .await
                    .values()
                    .cloned()
                    .collect();
                for controller in &controllers {
                    controller.input_stopping.store(true, Ordering::Release);
                    controller.coordinator.submissions.changed.notify_one();
                    controller.finish_exchange_owner().await?;
                    controller.finish_prompt_workers().await?;
                }
                for controller in &controllers {
                    controller.trace.record(
                        controller.observer.session_id(),
                        "service.shutdown.stage",
                        json!({"stage":"provider_runtime"}),
                    );
                }
                registry
                    .runtime
                    .shutdown()
                    .await
                    .context("collect Harness provider runtime")?;
                for controller in controllers {
                    controller.trace.record(
                        controller.observer.session_id(),
                        "service.shutdown.stage",
                        json!({"stage":"session_observation"}),
                    );
                    controller.finish_session_events().await?;
                    controller
                        .presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                        .close_all()?;
                    controller.broker.lock().await.release_lease()?;
                }
                let trace = registry.runtime.trace();
                tokio::task::spawn_blocking(move || trace.close())
                    .await
                    .context("collect Harness trace writer")??;
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
    publications: Arc<crate::exchange::SessionPublications>,
    prompt_workers: Mutex<prompt::PromptWorkers>,
    questions: Arc<crate::question::QuestionController>,
    provider_startup: Mutex<Option<tokio::task::JoinHandle<()>>>,
    provider_startup_error: std::sync::Mutex<Option<String>>,
    goal_max_turns: u32,
    task_controller: Arc<crate::task::SessionTaskController>,
    coordinator: Arc<SessionCoordinator>,
    submission_store: Arc<crate::provider::submission::SubmissionStore>,
    input_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    input_stopping: AtomicBool,
    trace: Arc<crate::trace::TraceStore>,
    data_root: PathBuf,
    observer: Arc<crate::broker::BrokerObserver>,
    timeout_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    agent_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    agent_coordinator: Arc<crate::agent::AgentCoordinator>,
    checks: Arc<crate::checks::CheckSupervisor>,
    agent_provider_factory: crate::agent::AgentProviderFactory,
    parent_input: crate::provider::SteeringLane,
    session_event_task: Mutex<Option<provider_lifecycle::SessionObservationTask>>,
    plan_review: Arc<crate::plan::review_document::PlanReviewStore>,
    revision_review: Arc<crate::plan::revision_review::RevisionReview>,
    broker: Arc<Mutex<HarnessBroker>>,
    exchange_owner: crate::exchange::ExchangeOwner,
    exchange_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
    presentation: Arc<std::sync::Mutex<crate::buffer::session::SessionPresentation>>,
    cancellation: Arc<TurnCancellation>,
    provider: Arc<std::sync::RwLock<Arc<crate::provider::Provider>>>,
    provider_lane: Arc<tokio::sync::RwLock<()>>,
    catalog_request: Arc<RwLock<crate::provider::ProviderCatalogRequest>>,
    mcp_revision: Arc<AtomicU64>,
    mcp_discovery: Mutex<(u64, crate::provider::mcp::McpDiscovery)>,
    permission: Arc<crate::provider::approval::PermissionCoordinator>,
}

impl SessionController {
    /// Snapshot the current provider without retaining the generation lock across I/O.
    fn provider_handle(&self) -> Arc<crate::provider::Provider> {
        self.provider
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
    }

    /// Gate dependent provider requests on completed, successful initialization.
    async fn provider_ready(&self) -> bool {
        self.provider_startup
            .lock()
            .await
            .as_ref()
            .is_some_and(|task| task.is_finished())
            && self
                .provider_startup_error
                .lock()
                .expect("provider startup state poisoned")
                .is_none()
    }

    async fn background_terminals(&self) -> Result<crate::provider::terminal::TerminalSnapshot> {
        let configuring = || -> Result<bool> { Ok(self.observer.admission.runtime()?.configuring) };
        let pending = || crate::provider::terminal::TerminalSnapshot {
            supported: true,
            starting: self
                .provider_startup_error
                .lock()
                .expect("provider startup state poisoned")
                .is_none(),
            terminal: Vec::new(),
        };
        if configuring()? || !self.provider_ready().await {
            return Ok(pending());
        }
        // Select the advertised generation only after retirement exclusion is acquired.
        let generation = self.provider_lane.clone().read_owned().await;
        if configuring()? || !self.provider_ready().await {
            return Ok(pending());
        }
        let request = self.catalog_request.read().await.clone();
        let revision = self.mcp_revision.load(Ordering::Acquire);
        let provider = self.provider_handle();
        let observed = provider.clone();
        let mut query = tokio::spawn(async move {
            // A caller timeout detaches this task; the generation remains leased until actual completion.
            let _generation = generation;
            observed.terminal_list(request).await
        });
        let result = tokio::time::timeout(Duration::from_secs(8), &mut query)
            .await
            .context("background terminal query timed out")
            .and_then(|result| result.context("background terminal query task failed"));
        if revision != self.mcp_revision.load(Ordering::Acquire)
            || !Arc::ptr_eq(&provider, &self.provider_handle())
            || configuring()?
            || !self.provider_ready().await
        {
            return Ok(pending());
        }
        Ok(result??)
    }

    /// Build one independently serialized controller from a durable broker session.
    fn new(mut broker: HarnessBroker) -> Result<Arc<Self>> {
        let catalog_request = broker.provider_catalog_request()?;
        let observer = broker.observer();
        let store = SqliteStore::read_only(&broker.data_root)?;
        for plan in store.list_plan(observer.session_id())? {
            observer.plan_file.reconcile_plan_artifacts(&plan)?;
        }

        let input = Arc::new(crate::input::SessionInputQueue::open(
            &broker.data_root,
            &catalog_request.harness_session_id,
            &broker.submission_store,
        )?);
        broker.input_wakeup = Some(input.changed.clone());
        let (exchange_owner, exchange_driver) = crate::exchange::ExchangeOwner::channel(
            &broker.data_root,
            &catalog_request.harness_session_id,
        )?;
        broker.question_events = Some(exchange_owner.question_events());
        let publications = Arc::new(crate::exchange::SessionPublications::open(
            &broker.data_root,
            &catalog_request.harness_session_id,
        )?);
        let controller = Arc::new(Self {
            publications,
            prompt_workers: Mutex::new(prompt::PromptWorkers::default()),
            questions: broker.questions.clone(),
            provider_startup: Mutex::new(None),
            provider_startup_error: std::sync::Mutex::new(None),
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
            agent_provider_factory: broker.agent_provider_factory.clone(),
            parent_input: broker.parent_input.clone(),
            session_event_task: Mutex::new(None),
            plan_review: Arc::clone(&broker.plan_review),
            revision_review: Arc::clone(&broker.revision_review),
            presentation: broker.presentation(),
            cancellation: broker.turn_cancellation(),
            provider: Arc::new(std::sync::RwLock::new(broker.provider_handle())),
            provider_lane: Arc::new(tokio::sync::RwLock::new(())),
            catalog_request: Arc::new(RwLock::new(catalog_request)),
            mcp_revision: Arc::new(AtomicU64::new(0)),
            mcp_discovery: Mutex::new((0, crate::provider::mcp::McpDiscovery::default())),
            permission: broker.permission_coordinator(),
            broker: Arc::new(Mutex::new(broker)),
            exchange_owner,
            exchange_task: Mutex::new(None),
        });
        let task = tokio::spawn(exchange_driver.run(
            controller.broker.clone(),
            controller.trace.clone(),
            controller.observer.session_id().to_owned(),
            crate::exchange::OwnerServices {
                provider_lifecycle: controller.provider_lifecycle(),
                publications: controller.publications.clone(),
                input: controller.coordinator.submissions.clone(),
                submissions: controller.submission_store.clone(),
                provider: controller.provider.clone(),
                provider_lane: controller.provider_lane.clone(),
                catalog: controller.catalog_request.clone(),
                resource_revision: controller.mcp_revision.clone(),
                root: controller.data_root.clone(),
                tasks: controller.task_controller.clone(),
                questions: controller.questions.clone(),
                observer: controller.observer.clone(),
                execution: controller.coordinator.execution.clone(),
                control: controller.coordinator.control.clone(),
                cancellation: controller.cancellation.clone(),
                permissions: controller.permission.clone(),
                children: controller.agent_coordinator.clone(),
                checks: controller.checks.clone(),
                goal_max_turns: controller.goal_max_turns,
            },
        ));
        *controller
            .exchange_task
            .try_lock()
            .expect("new controller has no exchange-task waiter") = Some(task);
        Ok(controller)
    }

    /// Drain the persistent transition owner before releasing its broker lease.
    async fn finish_exchange_owner(&self) -> Result<()> {
        let mut task = self.exchange_task.lock().await;
        if let Some(pending) = task.as_mut() {
            self.exchange_owner.close().await;
            pending
                .await
                .context("exchange owner failed during shutdown")?;
            task.take();
        }
        Ok(())
    }

    /// Await owner-controlled recovery, then retain the replacement observation listener.
    async fn recover_session_events(self: &Arc<Self>, message_sink: &MessageSender) -> Result<()> {
        let selected = self.observer.session_events();
        if !selected.is_closed() && !self.task_controller.configuration_is_uncertain() {
            return Ok(());
        }
        self.exchange_owner.recover_observation(selected).await?;
        self.attach_session_events(message_sink).await
    }

    async fn attach_timeout(self: &Arc<Self>, _message_sink: &MessageSender) -> Result<()> {
        let mut task = self.timeout_task.lock().await;
        if task.is_some() {
            return Ok(());
        }
        let mut schedule = self.observer.input_schedule.subscribe();
        let owner = crate::provider::ExecutionOwner::root(
            self.catalog_request.read().await.harness_session_id.clone(),
        );
        let mut feedback = self.permission.subscribe_timeout_feedback(&owner)?;
        let mut cancellation = self.cancellation.subscribe();
        let controller = Arc::downgrade(self);
        *task = Some(tokio::spawn(async move {
            let mut attempted = None;
            loop {
                let current = schedule.borrow_and_update().clone();
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
                    if let Some(mut deadline) = current.deadline.clone() {
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
                attempted = Some(identity);
                if controller
                    .exchange_owner
                    .approval_timeout(current.revision)
                    .await
                    .is_err()
                {
                    return;
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
                        let _ = message_sink.send_event(SessionEvent {session_id,event:"provider_event".into(),
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
        if let Some(task) = self.provider_startup.get_mut().take() {
            task.abort();
        }
        self.input_stopping.store(true, Ordering::Release);
        self.coordinator.submissions.changed.notify_one();
        if let Some(task) = self.timeout_task.get_mut().take() {
            task.abort();
        }
        if let Some(task) = self.agent_task.get_mut().take() {
            task.abort();
        }
        if let Some(task) = self.session_event_task.get_mut().take() {
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
    provider: &crate::provider::Provider,
    request: crate::provider::ProviderCatalogRequest,
    purpose: crate::provider::TextGeneration,
    model: &str,
    reasoning_effort: Option<&str>,
    input: &str,
) -> Result<String> {
    let provider_label = provider.descriptor().label;
    let result = provider
        .generate_text(request, purpose, model, reasoning_effort, input)
        .await;
    let cleanup = match tokio::time::timeout(Duration::from_secs(8), provider.shutdown()).await {
        Ok(result) => result,
        Err(source) => Err(crate::provider::lifecycle_error::classify_source(
            forge_provider_api::error::ProviderOperation::Shutdown,
            &source,
        )),
    };
    let result = match (result, cleanup) {
        (Err(primary), Err(cleanup)) => Err(crate::provider::ProviderError::Combined {
            primary: Box::new(primary),
            secondary: vec![cleanup],
        }),
        (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
        (Ok(text), Ok(())) => Ok(text),
    };
    result.with_context(|| format!("{provider_label} {} generation failed", purpose.name()))
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

struct RequestAdmission<'request> {
    execution: &'request mut Option<ExecutionPermit>,
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
    if !command::handles_owner_command(&controller, &request)? {
        crate::admission::complete();
    }
    ensure!(
        !controller.input_stopping.load(Ordering::Acquire),
        "Harness session is closing"
    );
    if method == RuntimeMethod::SessionStateGet {
        drop(admission.take());
        return reconnect::respond(&controller, request, message_sink).await;
    }
    let new_queued_submission = if method == RuntimeMethod::SessionInputApply {
        let operation: crate::input::InputOperation =
            crate::codec::from_value(request.params.clone())?;
        matches!(&operation.action, crate::input::InputAction::Submit { .. })
            && controller
                .coordinator
                .submissions
                .operation(&operation.id)?
                .is_none()
    } else {
        false
    };
    if method == RuntimeMethod::SessionPromptSubmit
        || new_queued_submission
        || (method == RuntimeMethod::SessionConfigure
            && controller.task_controller.configuration_is_uncertain())
    {
        controller.recover_session_events(message_sink).await?;
    }
    controller.attach_session_events(message_sink).await?;
    controller.attach_input(message_sink).await?;
    if method == RuntimeMethod::SessionPromptSubmit {
        drop(admission.take());
        return prompt::apply(&controller, request, message_sink).await;
    }
    if owner_commands::handles(method, &request) {
        drop(admission.take());
        controller.attach_agents(message_sink).await?;
        return owner_commands::apply(&controller, &request, method, message_sink).await;
    }

    if matches!(
        method,
        RuntimeMethod::SessionInputApply
            | RuntimeMethod::SessionInputGet
            | RuntimeMethod::SessionInputOperationGet
            | RuntimeMethod::SessionTurnSteer
    ) {
        return controller
            .input_request(request, method, message_sink)
            .await;
    }
    if method == RuntimeMethod::SessionPlanQuestionAnswer {
        return review_questions::answer(registry, controller, session_id, request, message_sink)
            .await;
    }
    if method == RuntimeMethod::SessionProviderMockResponse {
        let mock_provider = controller.provider_handle();
        let control = mock_provider
            .mock_response_control()
            .context("provider.mock_response requires the mock provider")?;
        let command = serde_json::from_value(request.params)?;
        let result = control.dispatch(&session_id, command).await?;
        message_sink
            .send_response(Response::success(request.id, result)?)
            .await?;
        return Ok(());
    }
    if native_permission::handles(&request) {
        return native_permission::apply(&controller, &request, message_sink).await;
    }
    if local_configuration::handles(&request) {
        return local_configuration::apply(&controller, &request, message_sink).await;
    }
    if native_settings::handles(&request) {
        return native_settings::apply(&controller, &request, message_sink).await;
    }
    if compaction::handles(&request) {
        drop(admission.take());
        return compaction::apply(&controller, &request, message_sink).await;
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
                .provider_handle()
                .model_list(controller.provider_handle().query_context(&catalog).await?),
        )
        .await
        .context("model catalog exceeded 30 seconds")??;
        let preference =
            SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?
                .load_preference(&selected.workspace, &selected.provider)?;
        crate::provider::select_model_settings(&mut model_list, preference.as_ref());
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
    let _setup_admission = if method == RuntimeMethod::SessionProviderAuthenticate {
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
        let result = controller
            .provider_handle()
            .inspect(forge_provider_api::catalog::WorkspaceContext {
                workspace: controller.catalog_request.read().await.workspace.clone(),
            })
            .await?;
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
            .provider_handle()
            .authenticate(
                forge_provider_api::catalog::WorkspaceContext {
                    workspace: controller.catalog_request.read().await.workspace.clone(),
                },
                method,
            )
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
                .provider_handle()
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
        if method == RuntimeMethod::SessionCheckList {
            controller.checks.refresh_changes().await?;
        }
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let session = store
            .load_session(&session_id)?
            .context("checks session is missing")?;
        let task = controller.task_controller.current()?;
        let task_id = task.as_ref().map(|task| task.id.as_str());
        let result = match operation {
            CheckOperation::List => {
                let settings = crate::settings::SettingsStore::open(
                    PathBuf::from(&registry.initialize.data_root).as_path(),
                )?
                .get(&session.workspace, &session.id, &session.provider)?
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
        let provider = forge_providers::registry::select(provider)?;
        let catalog = controller.catalog_request.read().await.clone();
        let models = tokio::time::timeout(
            Duration::from_secs(30),
            controller.agent_provider_factory.catalog(
                provider,
                crate::provider::ProviderRequest {
                    dispatch: forge_provider_api::submission::DispatchTracker::default(),
                    execution_owner: catalog.execution_owner,
                    harness_session_id: catalog.harness_session_id,
                    workspace: catalog.workspace,
                    input: crate::provider::ProviderInput::from_text(String::new()),
                    mode: crate::provider::PromptMode::Chat,
                    model: "default".into(),
                    effort: "default".into(),
                    context_window: None,
                    service_tier: Default::default(),
                    access: catalog.access,
                    approval_mode: catalog.approval_mode,
                    provider_session_id: None,
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
        let command_id = crate::exchange::command::CommandId::new(id)?;
        if let Some(record) = controller.exchange_owner.command_status(command_id).await? {
            let (state, outcome, error, error_code) = match &record.state {
                crate::exchange::journal::CommandState::Queued { .. } => {
                    ("queued", None, None, None)
                }
                crate::exchange::journal::CommandState::Active { .. } => {
                    ("active", None, None, None)
                }
                crate::exchange::journal::CommandState::Finished {
                    outcome: Ok(outcome),
                    ..
                } => ("completed", Some(outcome), None, None),
                crate::exchange::journal::CommandState::Finished {
                    outcome: Err(error),
                    ..
                } => ("failed", None, Some(error.to_string()), Some(error.code())),
            };
            message_sink
                .send_control_response(Response::success(
                    request.id,
                    json!({
                        "id":record.request.id.as_str(),"state":state,"sequence":record.sequence,
                        "revision":record.state.revision(),"outcome":outcome,"error":error,"error_code":error_code,
                        "progress":record.state,
                    }),
                )?)
                .await?;
            return Ok(());
        }
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
    // Release transport admission before the owner can collect the current exchange.
    if method == RuntimeMethod::SessionQuestionContinue
        || cancellation::handles(&request)
        || task_state::handles(&controller, &request)?
    {
        drop(admission.take());
        if task_state::apply_continuation(&controller, &request, message_sink).await? {
            return Ok(());
        }
        if cancellation::handles(&request) {
            return cancellation::apply(&controller, &request, message_sink).await;
        }
        if task_state::apply_plan_edit_review(&controller, &request, message_sink, syntax).await? {
            return Ok(());
        }
        if task_state::apply(
            &controller,
            &session_id,
            &request,
            registry.initialize.goal_max_turns,
            message_sink,
        )
        .await?
        {
            return Ok(());
        }
    }
    if method == RuntimeMethod::SessionCommandApply {
        message_sink
            .send_response(Response::failure(
                request.id,
                "unsupported_session_command",
                "The requested session command is not supported",
            ))
            .await?;
        return Ok(());
    }
    if method.requires_provider_fork() {
        registry
            .await_provider_fork(&session_id, &controller)
            .await?;
    }
    if route_control_request(&controller, &request, method, message_sink).await? {
        return Ok(());
    }

    let (event_sink, mut event_stream) = crate::broker::publication::channel();
    let (mut broker, _admission) = acquire_request_broker(&controller, method).await?;
    let message_sink_for_event = message_sink.clone();
    let routed_session_id = session_id.clone();
    let event_forwarder = tokio::spawn(async move {
        while let Some(event) = event_stream.recv().await? {
            message_sink_for_event
                .send_event(crate::broker::publication::encode_session_event(
                    &routed_session_id,
                    &event,
                )?)
                .await?;
        }
        Ok::<(), anyhow::Error>(())
    });
    let shutdown = method == RuntimeMethod::HostShutdown;
    let mut result = if method == RuntimeMethod::SessionCheckComplete {
        check_completion::drive(&controller, broker, request, event_sink, message_sink).await
    } else {
        let result = Box::pin(broker.dispatch_stream(request, event_sink)).await;
        drop(broker);
        result
    };
    event_forwarder.await??;
    for event in result.event.drain(..) {
        message_sink.send_event(event).await?;
    }
    let catalog_request = controller.broker.lock().await.provider_catalog_request()?;
    if catalog_request.provider_session_id.is_some() {
        *controller
            .provider_startup_error
            .lock()
            .expect("provider startup state poisoned") = None;
    }
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
    publish_execution_completion(message_sink, result.response, admission, shutdown).await
}

/// Release finalized execution ownership before its response can admit the next prompt.
async fn publish_execution_completion(
    sink: &MessageSender,
    response: Response,
    admission: &mut Option<ExecutionPermit>,
    terminal: bool,
) -> Result<()> {
    drop(admission.take());
    if terminal {
        sink.send_terminal_response(response)?;
    } else {
        sink.send_response(response).await?;
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
        registry.initialize.provider.kind.as_str(),
        registry
            .runtime
            .provider_handle()
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
    child_controller.start_provider(message_sink).await;
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
    let provider = registry.runtime.provider_handle();
    let descriptor = provider.descriptor();
    let preparation = prepare_provider_fork(
        PathBuf::from(&registry.initialize.data_root).as_path(),
        &registry.initialize.client_id,
        registry.initialize.provider.kind.as_str(),
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
        let provider_started = Instant::now();
        let mut result = provider
            .session_fork(preparation.provider_request)
            .await
            .map_err(|error| format!("{error:#}"));
        if let Ok(result) = &mut result {
            result.timing.push(crate::provider::ProviderTimingRecord {
                phase: "broker.provider_total".into(),
                duration_ms: provider_started.elapsed().as_secs_f64() * 1000.0,
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

/// Close the selected idle owner before deleting its durable session records.
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
    let selected = registry.controller_by_id.read().await.get(&target).cloned();
    if let Some(controller) = &selected {
        require_idle_deletion(controller)?;
        {
            let mut startup = controller.provider_startup.lock().await;
            if let Some(task) = startup.as_mut() {
                tokio::time::timeout(Duration::from_secs(60), &mut *task)
                    .await
                    .context("session startup did not settle before deletion")?
                    .context("session startup failed before deletion")?;
                startup.take();
            }
        }
        controller.input_stopping.store(true, Ordering::Release);
        controller.coordinator.submissions.changed.notify_one();
        // The owner retains every admitted command and collection effect when this wait is cancelled.
        tokio::time::timeout(Duration::from_secs(60), controller.finish_exchange_owner())
            .await
            .context("session owner did not settle before deletion")??;
        controller.finish_prompt_workers().await?;
        require_idle_deletion(controller)?;
        {
            let mut input = controller.input_task.lock().await;
            if let Some(task) = input.as_mut() {
                task.await.context("input worker failed before deletion")?;
                input.take();
            }
        }
        for subscription in [&controller.timeout_task, &controller.agent_task] {
            let mut slot = subscription.lock().await;
            if let Some(task) = slot.as_mut() {
                task.abort();
                if let Err(error) = task.await
                    && !error.is_cancelled()
                {
                    return Err(error).context("session subscription failed before deletion");
                }
                slot.take();
            }
        }
        {
            let mut observation = controller.session_event_task.lock().await;
            if let Some(task) = observation.as_mut() {
                task.abort();
                if let Err(error) = task.await
                    && !error.is_cancelled()
                {
                    return Err(error).context("session observation failed before deletion");
                }
                observation.take();
            }
        }
    }
    let mut controllers = registry.controller_by_id.write().await;
    ensure!(
        match (&selected, controllers.get(&target)) {
            (Some(selected), Some(current)) => Arc::ptr_eq(selected, current),
            (None, None) => true,
            _ => false,
        },
        "session controller changed while deletion waited for its owner"
    );
    if let Some(controller) = &selected {
        require_idle_deletion(controller)?;
        controller.broker.lock().await.release_lease()?;
    }
    let mut store = SqliteStore::open(std::path::Path::new(&registry.initialize.data_root))?;
    let session = store.load_session(&target)?.context("session not found")?;
    let plans = crate::plan::PlanFileStore::new(
        std::path::Path::new(&registry.initialize.data_root),
        &session.workspace,
    );
    store.delete_session_with(&target, || {
        plans.delete_session(&target)?;
        crate::input::SessionInputQueue::delete_persisted(
            std::path::Path::new(&registry.initialize.data_root),
            &target,
        )
    })?;
    if let Some(controller) = selected {
        controller.coordinator.execution.close();
        controller.coordinator.control.close();
        controllers.remove(&target);
    }
    drop(controllers);
    message_sink
        .send_response(Response::success(
            request.id,
            json!({"deleted":target,"provider_session_preserved":true}),
        )?)
        .await?;
    Ok(())
}

/// Reject deletion while any local execution, input, child, or check still owns session work.
fn require_idle_deletion(controller: &SessionController) -> Result<()> {
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
    controller.start_provider(message_sink).await;
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

async fn route_control_request(
    controller: &SessionController,
    request: &Request,
    method: RuntimeMethod,
    message_sink: &MessageSender,
) -> Result<bool> {
    if cancellation::handles(request) {
        cancellation::apply(controller, request, message_sink).await?;
        return Ok(true);
    }
    if matches!(
        method,
        RuntimeMethod::SessionQuestionAnswer | RuntimeMethod::SessionQuestionSkip
    ) && let Some(command) = request.params.get("command")
    {
        let command: crate::question::QuestionCommand = serde_json::from_value(command.clone())?;
        let result = match crate::exchange::QuestionChange::user(command) {
            Ok(change) => {
                controller
                    .exchange_owner
                    .question_events()
                    .change(change)
                    .await
            }
            Err(error) => Err(error),
        };
        let response = match result {
            Ok(snapshot) => Response::success(request.id, &snapshot)?,
            Err(error) => Response::failure(request.id, error.code(), error.to_string()),
        };
        message_sink.send_control_response(response).await?;
        return Ok(true);
    }
    if matches!(
        method,
        RuntimeMethod::SessionQuestionAnswer | RuntimeMethod::SessionQuestionSkip
    ) && request.params.get("target").is_some()
    {
        let mut params = request.params.clone();
        if method == RuntimeMethod::SessionQuestionSkip {
            params["response"] = json!({"kind":"skipped"});
        }
        let result = match crate::plan::PlanQuestionAnswerRequest::parse(params) {
            Ok(answer) => {
                controller
                    .exchange_owner
                    .question_events()
                    .answer_plan(answer)
                    .await
            }
            Err(error) => Err(crate::exchange::QuestionEventError::PlanAnswer(error)),
        };
        let response = match result {
            Ok(result) => Response::success(request.id, &result)?,
            Err(error) => Response::failure(request.id, error.code(), error.to_string()),
        };
        message_sink.send_control_response(response).await?;
        return Ok(true);
    }
    let catalog_request = controller.catalog_request.read().await.clone();
    let response = match method {
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
            let answer_list: Vec<crate::provider::approval::ApprovalAnswer> =
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
        RuntimeMethod::SessionProviderSkillList => {
            let provider = controller.provider_handle();
            let descriptor = provider.descriptor();
            let skills = match descriptor.capability.integration.resource_target.as_deref() {
                Some(target) => {
                    crate::settings::SessionResources::load(
                        &controller.data_root,
                        &catalog_request,
                        descriptor.kind.as_str(),
                        target,
                    )?
                    .skill
                }
                None => tokio::time::timeout(
                    Duration::from_secs(30),
                    provider.skill_list(provider.query_context(&catalog_request).await?),
                )
                .await
                .context("skill discovery exceeded 30 seconds")??,
            };
            Some(Response::success(request.id, skills)?)
        }
        RuntimeMethod::SessionProviderMcpList => {
            if let Some(error) = controller
                .provider_startup_error
                .lock()
                .expect("provider startup state poisoned")
                .clone()
            {
                anyhow::bail!(error);
            }
            let revision = controller.mcp_revision.load(Ordering::Acquire);
            let mut discovery = controller.mcp_discovery.lock().await;
            if discovery.0 != revision {
                *discovery = (revision, Default::default());
            }
            let provider = controller.provider_handle();
            let descriptor = provider.descriptor();
            let configured = descriptor
                .capability
                .integration
                .resource_target
                .as_deref()
                .map(|target| {
                    crate::settings::SessionResources::load(
                        &controller.data_root,
                        &catalog_request,
                        descriptor.kind.as_str(),
                        target,
                    )
                    .map(|resources| resources.mcp)
                })
                .transpose()?;
            let server_list = discovery
                .1
                .snapshot(
                    provider.clone(),
                    provider.query_context(&catalog_request).await?,
                    configured,
                )
                .await?;
            Some(Response::success(request.id, server_list)?)
        }
        _ => None,
    };
    if let Some(response) = response {
        message_sink.send_response(response).await?;
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
    include!("service/owner_transition_test.rs");
    include!("service/reconnect_overlap_test.rs");
    include!("service/live_goal_test.rs");
    include!("service/owner_storage_failure_test.rs");
    include!("service/task_presentation_test.rs");
    use super::*;
    use crate::provider::{ProviderCatalogRequest, TextGeneration};
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
                json!({"provider":"mock"})
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
                after.session.provider_session_id,
                before.session.provider_session_id
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
                    crate::provider::ServiceTier::Fast
                }
            );
            assert!(
                child
                    .provider_session_id
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
        let requests = Arc::new(Mutex::new(Vec::new()));
        let shutdown = Arc::new(AtomicBool::new(false));
        let native = generation_provider(requests.clone(), shutdown.clone(), false);
        let provider_fixture =
            crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let request = ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "generation-fixture".into(),
            workspace: "commit-workspace".into(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
        };
        let text = generate_isolated_text(
            provider_fixture.as_provider(),
            request.clone(),
            TextGeneration::CommitMessage,
            "selected-model",
            Some("low"),
            "Staged diff:\n+change",
        )
        .await?;
        assert_eq!(text, "fix: preserve drafts\n\nKeep existing text.");
        assert!(shutdown.load(Ordering::Acquire));
        {
            let requests = requests.lock().await;
            assert_eq!(requests.len(), 1);
            let request = &requests[0];
            assert_eq!(request.workspace, "commit-workspace");
            assert_eq!(request.model, "selected-model");
            assert!(
                matches!(request.reasoning, forge_provider_api::generation::ReasoningSelection::Level(ref level) if level == "low")
            );
            assert!(request.instructions.contains("Conventional Commit"));
            assert_eq!(request.input, "Staged diff:\n+change");
        }
        let descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
        )
        .expect("mock fixture descriptor");
        let unsupported_native =
            crate::provider::test_support::EventFixtureProvider::new(descriptor, |_, _, _| async {
                Ok(())
            });
        let unsupported =
            crate::provider::test_support::ProviderTestSession::from_native(unsupported_native)?;
        let error = generate_isolated_text(
            unsupported.as_provider(),
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
        assert!(diagnostic.contains("ModelSelection"), "{diagnostic}");
        let requests = Arc::new(Mutex::new(Vec::new()));
        let shutdown = Arc::new(AtomicBool::new(false));
        let failing_native = generation_provider(requests, shutdown.clone(), true);
        let failing =
            crate::provider::test_support::ProviderTestSession::from_native(failing_native)?;
        let request = ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "generation-failure".into(),
            workspace: "commit-workspace".into(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
        };
        let failure = generate_isolated_text(
            failing.as_provider(),
            request,
            TextGeneration::CommitMessage,
            "selected-model",
            Some("low"),
            "Staged diff:\n+change",
        )
        .await
        .unwrap_err();
        let diagnostic = format!("{failure:#}");
        assert!(diagnostic.contains("InputSubmit"));
        assert!(!diagnostic.contains("fixture generation failure"));
        assert!(shutdown.load(Ordering::Acquire));
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
                    provider: crate::provider::ProviderLaunch {
                        kind: forge_providers::registry::nonselectable_id()
                            .expect("registry supplies a nonselectable fixture identity"),
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

    struct TerminalInventoryProvider {
        inner: Arc<dyn forge_provider_api::provider::Provider>,
        started: Notify,
        release: Notify,
        calls: AtomicU64,
    }

    #[async_trait::async_trait]
    impl forge_provider_api::provider::Provider for TerminalInventoryProvider {
        async fn session_open(
            &self,
            context: forge_provider_api::OperationContext,
            request: forge_provider_api::SessionRequest,
        ) -> std::result::Result<forge_provider_api::OpenedSession, crate::provider::ProviderError>
        {
            self.inner.session_open(context, request).await
        }
        async fn session_configure(
            &self,
            context: forge_provider_api::OperationContext,
            binding: &forge_provider_api::BindingId,
            configuration: forge_provider_api::SessionConfiguration,
        ) -> std::result::Result<
            forge_provider_api::ConfiguredSession,
            crate::provider::ProviderError,
        > {
            self.inner
                .session_configure(context, binding, configuration)
                .await
        }
        async fn session_close(
            &self,
            context: forge_provider_api::OperationContext,
            binding: &forge_provider_api::BindingId,
        ) -> std::result::Result<(), crate::provider::ProviderError> {
            self.inner.session_close(context, binding).await
        }
        async fn cancel(
            &self,
            context: forge_provider_api::OperationContext,
            binding: &forge_provider_api::BindingId,
            request: forge_provider_api::CancelRequest,
        ) -> std::result::Result<
            forge_provider_api::InvocationSettlement,
            crate::provider::ProviderError,
        > {
            self.inner.cancel(context, binding, request).await
        }
        async fn input_submit(
            &self,
            context: forge_provider_api::OperationContext,
            binding: &forge_provider_api::BindingId,
            request: forge_provider_api::PromptRequest,
        ) -> std::result::Result<forge_provider_api::InputAcceptance, crate::provider::ProviderError>
        {
            self.inner.input_submit(context, binding, request).await
        }
        async fn inspect(
            &self,
            operation: forge_provider_api::OperationContext,
            context: forge_provider_api::catalog::WorkspaceContext,
        ) -> std::result::Result<forge_provider_api::ProviderSetup, crate::provider::ProviderError>
        {
            self.inner.inspect(operation, context).await
        }
        async fn shutdown(
            &self,
            context: forge_provider_api::OperationContext,
        ) -> std::result::Result<(), crate::provider::ProviderError> {
            self.inner.shutdown(context).await
        }
        async fn terminal_list(
            &self,
            context: forge_provider_api::OperationContext,
            _: &forge_provider_api::BindingId,
        ) -> std::result::Result<
            crate::provider::terminal::TerminalSnapshot,
            crate::provider::ProviderError,
        > {
            context.check_admission(forge_provider_api::ProviderOperation::TerminalList)?;
            self.calls.fetch_add(1, Ordering::AcqRel);
            self.started.notify_one();
            tokio::select! {
                _ = self.release.notified() => {},
                interruption = context.interrupted() => return Err(interruption.failure(forge_provider_api::ProviderOperation::TerminalList, forge_provider_api::ProviderPhase::Observation, forge_provider_api::FailureDisposition::Unchanged)),
            }
            Err(forge_provider_api::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TerminalList,
                    forge_provider_api::ProviderFailureKind::Transport,
                ),
            ))
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
        let controller = registry
            .controller_by_id
            .write()
            .await
            .remove(registry.initial_session_id())
            .unwrap();
        let inner = forge_providers::registry::create(
            crate::provider::ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
                command: Vec::new(),
            },
            controller.data_root.clone(),
        )?;
        let mut descriptor = inner.descriptor().clone();
        descriptor.capability.background_terminals = true;
        let provider = Arc::new(TerminalInventoryProvider {
            inner: inner.adapter().clone(),
            started: Notify::new(),
            release: Notify::new(),
            calls: AtomicU64::new(0),
        });
        let provider_fixture = crate::provider::test_support::ProviderTestSession::from_registered(
            forge_providers::registry::RegisteredProvider::new(descriptor, provider.clone(), None),
        )?;
        *controller.provider.write().unwrap() = provider_fixture.provider_handle();
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
        };
        store.save_task_operation(&mut operation)?;
        assert!(controller.background_terminals().await?.starting);
        assert_eq!(provider.calls.load(Ordering::Acquire), 0);
        operation.state = "completed".into();
        store.save_task_operation(&mut operation)?;
        let (sink, _output) = forge_protocol::outbound::channel();
        service
            .start_provider_session(registry.initial_session_id(), &sink)
            .await?;
        while !controller.provider_ready().await {
            tokio::task::yield_now().await;
        }
        let query = controller.background_terminals();
        tokio::pin!(query);
        tokio::select! {
            result = &mut query => panic!("terminal query settled before release: {result:?}"),
            _ = provider.started.notified() => {}
        }
        controller.mcp_revision.fetch_add(1, Ordering::AcqRel);
        provider.release.notify_one();
        assert!(query.await?.starting);
        operation.state = "resuming".into();
        store.save_task_operation(&mut operation)?;
        provider.release.notify_one();
        let error = controller
            .background_terminals()
            .await
            .unwrap_err()
            .to_string();
        assert!(error.to_lowercase().contains("transport"));
        assert!(!error.contains("connection closed"));
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    struct GenerationCall {
        workspace: String,
        model: String,
        reasoning: forge_provider_api::generation::ReasoningSelection,
        instructions: String,
        input: String,
    }

    fn generation_provider(
        requests: Arc<Mutex<Vec<GenerationCall>>>,
        shutdown: Arc<AtomicBool>,
        fail: bool,
    ) -> Arc<crate::provider::test_support::EventFixtureProvider> {
        let descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
        )
        .expect("mock fixture descriptor");
        let calls = requests.clone();
        let provider = crate::provider::test_support::EventFixtureProvider::new(descriptor, move |request, sink, _| {
            let calls = calls.clone();
            async move {
                let mut calls = calls.lock().await;
                let opened = calls.last_mut().expect("open precedes input");
                opened.input = match &request.input {
                    forge_provider_api::PromptInput::Text(text) => text.clone(),
                    forge_provider_api::PromptInput::Skill { .. } | forge_provider_api::PromptInput::QuestionReply(_) => panic!("utility input must preserve plain text"),
                };
                assert!(request.tools.is_empty());
                assert!(request.attachments.is_empty());
                assert!(request.task.is_none());
                assert!(request.recovered_inputs.is_empty());
                drop(calls);
                if fail {
                    return Err(forge_provider_api::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(forge_provider_api::ProviderOperation::InputSubmit, forge_provider_api::ProviderFailureKind::Runtime),
                    ));
                }
                let address = crate::provider::test_support::address(&request.scope, "temporary-thread", "temporary-turn");
                sink.send_wait(crate::provider::ProviderEvent::Execution {
                    observation: forge_provider_api::ExecutionObservation::new(request.scope.clone(), forge_provider_api::ExecutionUpdate::Started { address: address.clone() })?,
                }).await?;
                for event in crate::provider::test_support::message(&address, "thought", Some(crate::provider::MessageRole::Thought), "private reasoning") {
                    sink.send_wait(event).await?;
                }
                for event in crate::provider::test_support::message(&address, "answer", Some(crate::provider::MessageRole::Response), "```text\nfix: preserve drafts\n\nKeep existing text.\n```") {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(crate::provider::ProviderEvent::Execution {
                    observation: forge_provider_api::ExecutionObservation::new(request.scope.clone(), forge_provider_api::ExecutionUpdate::Finished {
                        address, covered_submissions: vec![request.submission_id.clone()], completed_at_ms: 1,
                        result: forge_provider_api::ExecutionResult::Completed { completion: forge_provider_api::completion::ProviderCompletion::default().into() },
                    })?,
                }).await?;
                Ok(())
            }
        }).with_models(vec![serde_json::from_value(json!({"id":"selected-model","reasoning":["low","high"],"context_window":[],"vision":false,"is_default":true})).expect("normalized fixture model")]);
        provider.with_session_open(move |request| {
            let requests = requests.clone();
            async move {
                assert!(matches!(
                    request.history,
                    forge_provider_api::SessionHistory::IsolatedText(_)
                ));
                assert!(request.tools.is_empty());
                assert!(request.tool_endpoint.is_none());
                assert!(request.configuration.resources.skills.is_empty());
                assert!(request.configuration.resources.mcp.is_empty());
                assert_eq!(
                    request.configuration.approval,
                    crate::session::ApprovalMode::Deny
                );
                requests.lock().await.push(GenerationCall {
                    workspace: request.workspace,
                    model: request
                        .configuration
                        .model
                        .model
                        .expect("catalog resolved explicit model"),
                    reasoning: request.configuration.model.reasoning,
                    instructions: request.system_instructions,
                    input: String::new(),
                });
                Ok(())
            }
        });
        let shutdown_called = shutdown.clone();
        provider.with_shutdown(move || {
            let shutdown_called = shutdown_called.clone();
            async move {
                shutdown_called.store(true, Ordering::Release);
                Ok(())
            }
        });
        provider
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
        let (setup_sink, _setup_output) = forge_protocol::outbound::channel();
        controller.attach_input(&setup_sink).await?;
        let reviewed =
            reviewed_execution_fixture(&service, &controller, &session_id, &setup_sink).await?;
        let plan = SqliteStore::open(&controller.data_root)?
            .load_plan(
                reviewed["plan_id"]
                    .as_str()
                    .context("reviewed plan has no identity")?,
            )?
            .context("reviewed plan is missing")?;
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
        let (setup_sink, _setup_output) = forge_protocol::outbound::channel();
        controller.attach_input(&setup_sink).await?;
        let reviewed =
            reviewed_execution_fixture(&service, &controller, &session_id, &setup_sink).await?;
        let mut plan = SqliteStore::open(&controller.data_root)?
            .load_plan(
                reviewed["plan_id"]
                    .as_str()
                    .context("reviewed plan has no identity")?,
            )?
            .context("reviewed plan is missing")?;
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
        let task = controller
            .task_controller
            .current()?
            .context("plan task is missing")?;
        controller.task_controller.apply(
            &mut SqliteStore::open(&data)?,
            &task.permit(),
            crate::task::TaskEvent::PlanArtifactUpdated(Box::new(plan.clone())),
            plan.updated_at_ms,
        )?;
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
        let rejected = service.dispatch(Some(session_id.clone()), Request { id: 99, method: "session.plan.changes.request".into(),
            params: json!({"plan_id":plan.id,"digest":"stale-review-digest","comment":"x".repeat(65537)}) }, &sink).await.unwrap_err();
        assert!(
            rejected
                .to_string()
                .contains("plan changed after the review action began"),
            "{rejected:#}"
        );
        assert!(
            file.read_submitted_document(&session_id, &plan.id, plan.model_revision + 1)
                .is_err()
        );
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
            break;
        }
        assert_eq!(
            file.read_submitted_document(&session_id, &plan.id, plan.model_revision)?,
            submitted
        );
        let revised = tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let revised = SqliteStore::open(&data)?
                    .load_plan(&plan.id)?
                    .context("revised plan is missing")?;
                if revised.model_revision == plan.model_revision + 2
                    && revised.state == crate::plan::PlanState::AwaitingReview
                    && controller.coordinator.execution.available_permits() > 0
                {
                    break Ok::<_, anyhow::Error>(revised);
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("revised plan did not reach review")??;
        assert_eq!(serde_json::to_value(&revised.manual_changes)?, changes);
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

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn concurrent_timeout_edits_preserve_latest_value_while_busy() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = Arc::new(service());
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
        let (sink, mut output) = forge_protocol::outbound::channel();
        let ready = Arc::new(tokio::sync::Barrier::new(3));
        let mut writer_list = Vec::new();
        for (offset, field) in [(10, "approval_timeout"), (30, "approval_timeout")] {
            let service = service.clone();
            let session_id = session_id.clone();
            let sink = sink.clone();
            let ready = ready.clone();
            writer_list.push(tokio::spawn(async move {
                ready.wait().await;
                for amount in 1..=16 {
                    service
                        .dispatch(
                            Some(session_id.clone()),
                            Request {
                                id: offset + amount,
                                method: "session.input.timeout.configure".into(),
                                params: json!({field:{"amount":amount,"unit":"m"}}),
                            },
                            &sink,
                        )
                        .await?;
                }
                Ok::<_, anyhow::Error>(())
            }));
        }
        ready.wait().await;
        for writer in writer_list {
            writer.await??;
        }
        for _ in 0..32 {
            let frame = output
                .recv()
                .await?
                .context("timeout response is missing")?;
            let response: Value = serde_json::from_slice(frame.bytes())?;
            assert!(response.get("error").is_none(), "{response}");
        }
        let approval = controller.permission.approval_timeout(&session_id)?;
        assert_eq!(approval.amount, 16);
        assert!(controller.coordinator.execution.try_acquire().is_err());
        let saved = crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?
            .get(&fixture.path().to_string_lossy(), &session_id, "mock")?
            .desired;
        assert_eq!(saved.approval_timeout, Some(approval));
        drop(broker);
        drop(execution);
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    /// Publish new questions once without turning state refreshes into an idle notification loop.
    #[tokio::test]
    async fn unchanged_question_relay_does_not_publish_refresh_notifications() -> Result<()> {
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
        let (sink, mut output) = forge_protocol::outbound::channel();
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        let initial = tokio::time::timeout(Duration::from_secs(2), output.recv())
            .await??
            .context("initial question notification is missing")?;
        assert_eq!(
            serde_json::from_slice::<Value>(initial.bytes())?["event"],
            "questions_changed"
        );
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        assert!(
            matches!(output.try_recv(), Err(error) if error.kind() == std::io::ErrorKind::WouldBlock)
        );

        controller.questions.ask(
            "new-question",
            "exchange",
            crate::question::QuestionOrigin::Chat,
            vec![crate::question::QuestionDraft {
                header: "Layout".into(),
                question: "Which layout?".into(),
                requirement: crate::question::QuestionRequirement::Optional,
                options: Vec::new(),
            }],
        )?;
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        let changed = tokio::time::timeout(Duration::from_secs(2), output.recv())
            .await??
            .context("changed question notification is missing")?;
        assert_eq!(
            serde_json::from_slice::<Value>(changed.bytes())?["payload"]["questions"]
                .as_array()
                .unwrap()
                .len(),
            1
        );
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        assert!(
            matches!(output.try_recv(), Err(error) if error.kind() == std::io::ErrorKind::WouldBlock)
        );
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn required_chat_questions_enqueue_one_continuation_after_answer_delivery() -> Result<()>
    {
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
        let execution = controller.coordinator.execution.acquire().await?;
        let mut broker = controller.broker.lock().await;
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({"text":"Original request awaiting a choice"}),
            })
            .await;
        assert!(result.response.result().is_some());
        drop(broker);
        let original = crate::storage::SqliteStore::read_only(&controller.data_root)?
            .latest_exchange(&session_id)?
            .unwrap();
        let ids = controller.questions.ask(
            "required",
            &original.id,
            crate::question::QuestionOrigin::Chat,
            vec![crate::question::QuestionDraft {
                header: "Layout".into(),
                question: "Which layout?".into(),
                requirement: crate::question::QuestionRequirement::Required,
                options: Vec::new(),
            }],
        )?;
        let (sink, _output) = forge_protocol::outbound::channel();
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        assert!(
            controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .is_empty()
        );
        controller
            .questions
            .apply(crate::question::QuestionCommand {
                operation_id: "answer-required".into(),
                question_id: ids[0].clone(),
                revision: 1,
                action: crate::question::QuestionAction::Answer {
                    answer: crate::question::QuestionAnswer::Other {
                        text: "Tabs".into(),
                    },
                },
            })?;
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        let queued = controller.coordinator.submissions.snapshot()?;
        assert_eq!(queued.messages.len(), 2);
        assert!(matches!(
            queued.messages[0].purpose,
            crate::input::InputPurpose::Question { .. }
        ));
        assert!(
            matches!(&queued.messages[1].purpose, crate::input::InputPurpose::QuestionContinuation { exchange_id } if exchange_id == &original.id)
        );
        assert_eq!(queued.messages[1].intent, crate::input::InputIntent::Queue);
        assert!(queued.messages[1].text.contains(&original.prompt));
        drop(execution);
        service.shutdown(Duration::from_secs(2)).await?;
        Ok(())
    }

    #[tokio::test]
    async fn persistent_question_answers_commit_and_relay_while_provider_owns_execution()
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
        let ids = controller.questions.ask(
            "ask",
            "exchange",
            crate::question::QuestionOrigin::Chat,
            vec![crate::question::QuestionDraft {
                header: "Layout".into(),
                question: "Which layout?".into(),
                requirement: crate::question::QuestionRequirement::Optional,
                options: Vec::new(),
            }],
        )?;
        let execution = controller.coordinator.execution.acquire().await?;
        let broker = controller.broker.lock().await;
        let (sink, _output) = forge_protocol::outbound::channel();
        let command = crate::question::QuestionCommand {
            operation_id: "answer".into(),
            question_id: ids[0].clone(),
            revision: 1,
            action: crate::question::QuestionAction::Answer {
                answer: crate::question::QuestionAnswer::Other {
                    text: "Tabs".into(),
                },
            },
        };
        let mut clarification = Request {
            id: 3,
            method: "session.input.apply".into(),
            params: json!({
                "id":"clarification",
                "target":{"owner":"question","question_id":ids[0],"revision":1},
                "action":{"kind":"submit","message":{
                    "id":"clarification-input","revision":0,"text":"Which scope?",
                    "purpose":{"kind":"user"},"intent":"auto","attachments":[]
                }}
            }),
        };
        let control = controller.coordinator.control.acquire().await?;
        service
            .normalize_request(Some(&session_id), &mut clarification)
            .await?;
        assert_eq!(clarification.method, "session.input.apply");
        assert!(
            service
                .prepare(
                    Some(&session_id),
                    RuntimeMethod::decode(&clarification.method)?
                )
                .await?
                .is_none()
        );
        drop(control);
        tokio::time::timeout(
            Duration::from_secs(2),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.question.answer".into(),
                    params: json!({"command": command}),
                },
                &sink,
            ),
        )
        .await??;
        assert!(!controller.questions.snapshot()?.questions[0].pending());
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        controller
            .exchange_owner
            .reconcile_questions(Some(sink.clone()))
            .await?;
        assert!(controller.questions.outbox()?.is_empty());
        let queued = controller.coordinator.submissions.snapshot()?;
        assert_eq!(queued.messages.len(), 1);
        assert!(matches!(
            queued.messages[0].purpose,
            crate::input::InputPurpose::Question { .. }
        ));
        assert!(controller.questions_block_work()?);
        assert!(controller.coordinator.execution.try_acquire().is_err());
        drop(broker);
        drop(execution);
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
        tokio::time::timeout(
            Duration::from_secs(2),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.input.timeout.configure".into(),
                    params: json!({"approval_timeout":{"amount":2,"unit":"s"},"tool_timeout":{"amount":3,"unit":"m"}}),
                },
                &sink,
            ),
        )
        .await??;
        let approval = controller.permission.approval_timeout(&session_id)?;
        assert_eq!(approval.amount, 2);
        assert!(controller.coordinator.execution.try_acquire().is_err());
        assert_eq!(broker.snapshot()?.session.approval_timeout, approval);
        assert_eq!(broker.snapshot()?.session.tool_timeout.amount, 3);
        assert_eq!(
            broker.snapshot()?.session.tool_timeout.unit,
            crate::session::TimeoutUnit::M
        );
        drop(broker);
        drop(execution);
        let settings = crate::settings::SettingsStore::open(fixture.path().join("data").as_path())?;
        assert_eq!(
            settings
                .get(&fixture.path().to_string_lossy(), &session_id, "mock")?
                .desired
                .approval_timeout,
            Some(approval)
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
    async fn configuration_dispatch_keeps_owned_phase_state_bounded() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        let opened = service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().context("session missing")?["session"]["id"]
            .as_str()
            .context("session identity missing")?
            .to_owned();
        let registry = service.registry().await?;
        let controller = registry.resolve(&session_id).await?;
        let (sink, _output) = forge_protocol::outbound::channel();
        let action = json!({"action":"configure","config":{"model":"fixture"}});
        let request = Request {
            id: 1,
            method: "session.command.apply".into(),
            params: action,
        };
        let dispatch = || native_settings::apply(&controller, &request, &sink);
        let bytes = crate::trace::future_bytes(&dispatch);
        assert!(
            bytes <= 32 * 1024,
            "configuration dispatcher retains {bytes} inline bytes"
        );
        service.shutdown(Duration::from_secs(5)).await?;
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
        let store = SqliteStore::open(&controller.data_root)?;
        let (sink, _output) = forge_protocol::outbound::channel();
        for (index, config) in [json!({"model":"model-b"}), json!({"effort":"high"})]
            .into_iter()
            .enumerate()
        {
            let request = Request {
                id: u64::try_from(index)? + 2,
                method: "session.command.apply".into(),
                params: json!({"operation_id":format!("configure-{index}"),"action":"configure","config":config}),
            };
            controller
                .exchange_owner
                .execute_identified(native_settings::decode_command(&request)?, sink.clone())
                .await?;
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
            let (sink, _output) = forge_protocol::outbound::channel();
            for (index, change) in changes.into_iter().enumerate() {
                let request = Request {
                    id: u64::try_from(index)? + 2,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":format!("permission-{index}"),"action":"permission","change":change}),
                };
                let command = native_permission::decode_command(&request)?;
                let first = controller
                    .exchange_owner
                    .execute_identified(command.clone(), sink.clone())
                    .await?;
                let duplicate = controller
                    .exchange_owner
                    .execute_identified(command, sink.clone())
                    .await?;
                assert_eq!(first.id, duplicate.id);
                assert_eq!(first.revision, duplicate.revision);
            }
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
        let mut task = crate::task::Task::goal(
            "Retain the running objective".into(),
            crate::session::continuation::ContinuationBudget::new(20, 2),
            1,
        );
        task.edit(|draft| {
            if let crate::task::WorkflowSnapshot::Goal { goal } = &mut draft.workflow {
                goal.continuation.turn_count = 4;
            }
            Ok(())
        })?;
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
        let (sink, _output) = forge_protocol::outbound::channel();
        controller
            .exchange_owner
            .execute_identified(native_settings::decode_command(&request)?, sink.clone())
            .await?;
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
        let registry = controller.broker.lock().await.execution_registry();
        let engine = registry
            .owner(crate::provider::ExecutionOwner::root(&session_id))
            .await?;
        let submissions = controller.submission_store.clone();
        let prompt_session = session_id.clone();
        let prompt = tokio::spawn(async move {
            let request = crate::provider::ProviderRequest {
                dispatch: forge_provider_api::submission::DispatchTracker::default(),
                execution_owner: None,
                harness_session_id: prompt_session,
                workspace: ".".into(),
                input: crate::provider::ProviderInput::from_text("held prompt"),
                mode: crate::provider::PromptMode::Chat,
                model: "mock-model".into(),
                effort: "low".into(),
                context_window: None,
                service_tier: Default::default(),
                access: Default::default(),
                approval_mode: crate::session::ApprovalMode::Ask,
                provider_session_id: None,
                submission: None,
                recovered_input: Vec::new(),
                control_context: None,
            };
            let scope =
                crate::provider::InvocationScope::new(request.owner()?, "mock-response-controls");
            let (sink, mut stream) = crate::provider::events::channel(scope);
            let execution = engine.run_once(request, sink, &submissions);
            tokio::pin!(execution);
            let mut event = Vec::new();
            let mut open = true;
            let completion = loop {
                tokio::select! {
                    result = &mut execution => break result?,
                    observed = stream.recv(), if open => match observed? {
                        Some(observed) => event.push(observed),
                        None => open = false,
                    },
                }
            };
            while let Some(observed) = stream.try_next()? {
                event.push(observed);
            }
            Ok::<_, anyhow::Error>(crate::provider::ProviderOutput { event, completion })
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
            let result: Value = loop {
                let frame = output
                    .recv()
                    .await?
                    .context("missing mock control response")?;
                let message: Value = serde_json::from_slice(frame.bytes())?;
                if message["id"] == request_id {
                    break message;
                }
            };
            assert!(result.get("error").is_none(), "{result}");
            if operation == "wait" {
                assert_eq!(result["result"]["prompt"], "held prompt");
                assert!(!prompt.is_finished(), "arrival released the response");
            } else {
                assert_eq!(result["result"]["released"], true);
            }
        }
        let completed = tokio::time::timeout(Duration::from_secs(2), prompt).await???;
        assert!(completed.event.iter().any(|event| matches!(
            event,
            crate::provider::ProviderEvent::Message {
                update: crate::provider::MessageUpdate::Started { text, .. },
                ..
            } if text == "Mock response: held prompt"
        )));
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
        let mock_provider = controller.provider_handle();
        let control = mock_provider.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &session_id,
                crate::provider::mock_response::MockResponseCommand::Hold {
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
                json!({"provider":forge_providers::registry::default_id().as_str(), "model":"pinned-model", "pinned":true}),
            ),
            (
                "provider.model.pin.list",
                json!({"provider":forge_providers::registry::default_id().as_str()}),
            ),
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
            ("session.plan.list", json!({})),
            ("session.task.get", json!({})),
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
        let input = selected
            .iter()
            .map(crate::attachment::InputAttachment::prepare)
            .collect::<std::result::Result<Vec<_>, _>>()?;
        assert_eq!(input.len(), 1);
        assert_eq!(input[0].label, "[Image #3]");
        assert_eq!(input[0].contents.as_ref(), bytes.as_slice());
        assert_eq!(std::fs::read(&input[0].path)?, bytes);
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
        let (sink, mut output) = forge_protocol::outbound::channel();
        // Bootstrap the session listener before holding the state lock that supplies its registry.
        controller.attach_session_events(&sink).await.unwrap();
        let broker = controller.broker.lock().await;
        let discovery = controller.mcp_discovery.lock().await;
        let configuring = service.dispatch(Some(session_id.clone()), Request {
            id: 2, method: "session.command.apply".into(),
            params: json!({"operation_id":"configure-responsive","action":"configure","config":{"effort":"low"}}),
        }, &sink);
        tokio::pin!(configuring);
        tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                tokio::select! {
                    result = &mut configuring => panic!("configuration completed before its owner released: {result:?}"),
                    frame = output.recv() => {
                        let frame = frame.unwrap().unwrap();
                        let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
                        if value["id"] == 2 {
                            assert_eq!(value["result"]["state"], "accepted", "{value}");
                            break;
                        }
                    }
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
            RuntimeMethod::SessionPlanAccept,
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
            let (routed, ()) = tokio::time::timeout(Duration::from_secs(1), async {
                tokio::join!(
                    route_control_request(&controller, &request, method, &sink,),
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

    fn steering_provider(
        requests: Arc<Mutex<Vec<(String, String)>>>,
    ) -> Arc<crate::provider::test_support::EventFixtureProvider> {
        let descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("mock fixture identity"),
        )
        .expect("mock fixture descriptor");
        let provider =
            crate::provider::test_support::EventFixtureProvider::new(descriptor, |_, _, _| async {
                Ok(())
            });
        let steering_requests = requests.clone();
        provider.with_steering(move |request| {
            let requests = steering_requests.clone();
            async move {
                requests
                    .lock()
                    .await
                    .push(("steering".into(), format!("{:?}", request.input)));
                Ok(())
            }
        });
        let cleanup_requests = requests.clone();
        provider.with_cleanup(move |_| {
            let requests = cleanup_requests.clone();
            async move {
                requests
                    .lock()
                    .await
                    .push(("cleanup".into(), String::new()));
                Err(forge_provider_api::ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::Cancel,
                        forge_provider_api::ProviderFailureKind::Transport,
                    ),
                ))
            }
        });
        provider
    }
    #[tokio::test]
    async fn child_approval_resolution_uses_the_owned_runtime() -> Result<()> {
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
        let owner = crate::provider::ExecutionOwner {
            session_id: (session_id.clone()).into(),
            agent_id: (agent_id.clone()).into(),
        };
        let permission = controller.permission.clone();
        let (event_sink, mut events) =
            crate::provider::events::channel(crate::provider::InvocationScope {
                owner: owner.clone(),
                exchange_id: task_id.clone().into(),
                invocation_id: "approval-invocation".into(),
            });
        let pending = tokio::spawn(async move {
            permission
                .authorize(
                    crate::session::ApprovalMode::Ask,
                    PermissionRequest {
                        id: "child-request".into(),
                        provider: forge_provider_api::ProviderId::new("fixture")
                            .expect("fixture identity is valid"),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "probe".into(),
                            shell: None,
                        }],
                    },
                    Some(crate::provider::TurnAddress {
                        scope: event_sink.invocation_scope()?.clone(),
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
                event: requested.into(),
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
            )
            .await?
        );
        assert_eq!(
            pending.await??,
            crate::provider::approval::ApprovalResolution::AllowOnce
        );
        let resolved = tokio::time::timeout(Duration::from_secs(2), events.recv())
            .await??
            .unwrap();
        assert!(matches!(
            &resolved,
            crate::provider::ProviderEvent::Approval {
                update: crate::provider::ApprovalUpdate::Resolved(_),
                ..
            }
        ));
        controller
            .agent_coordinator
            .sender()
            .send(crate::agent::AgentCommand::Event {
                owner: crate::provider::ExecutionOwner {
                    session_id: (session_id.clone()).into(),
                    agent_id: (agent_id.clone()).into(),
                },
                task_id: task_id.clone(),
                event: resolved.into(),
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
                if message["event"] == "provider_event"
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

    /// Transport preparation never reserves execution or changes cancellation policy.
    #[tokio::test]
    async fn prompt_preparation_leaves_busy_execution_owned() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let cleanup = controller.coordinator.control.acquire().await?;
        let execution = controller.coordinator.execution.acquire().await?;
        let cancelled = *controller.cancellation.subscribe().borrow();
        let prepared = service
            .prepare(
                Some(registry.initial_session_id()),
                RuntimeMethod::SessionPromptSubmit,
            )
            .await?;
        assert!(prepared.is_none());
        assert_eq!(*controller.cancellation.subscribe().borrow(), cancelled);
        drop(execution);
        drop(cleanup);
        assert!(controller.coordinator.execution.try_acquire().is_ok());
        service.shutdown(Duration::from_secs(1)).await?;
        Ok(())
    }

    /// A queued prompt reaches durable input acceptance while existing execution stays owned.
    #[tokio::test]
    async fn queued_prompt_commits_input_without_acquiring_execution() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let execution = controller.coordinator.execution.acquire().await?;
        let (sink, _output) = forge_protocol::outbound::channel();
        tokio::time::timeout(Duration::from_secs(1), prompt::apply(&controller, Request {
            id: 2, method: "session.prompt.submit".into(),
            params: json!({"operation_id":"queued-prompt", "input_id":"queued-message", "text":"Wait for current work", "queued":true}),
        }, &sink)).await??;
        let snapshot = controller.coordinator.submissions.snapshot()?;
        let message = snapshot
            .messages
            .iter()
            .find(|message| message.id == "queued-message")
            .context("queued prompt was not retained")?;
        assert_eq!(message.intent, crate::input::InputIntent::Queue);
        assert_eq!(message.delivery, crate::input::InputDelivery::Queued);
        assert!(matches!(message.purpose, crate::input::InputPurpose::User));
        assert_eq!(controller.coordinator.execution.available_permits(), 0);
        controller.finish_prompt_workers().await?;
        drop(execution);
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
            .prepare(
                Some(service.registry().await.unwrap().initial_session_id()),
                RuntimeMethod::SessionPromptSubmit,
            )
            .await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let dispatch = service.dispatch_admitted(
            Some(service.registry().await?.initial_session_id().to_owned()),
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
                        .prepare(
                            Some(service.registry().await.unwrap().initial_session_id()),
                            RuntimeMethod::SessionPromptSubmit,
                        )
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
            .prepare(
                Some(service.registry().await.unwrap().initial_session_id()),
                RuntimeMethod::SessionPromptSubmit,
            )
            .await
            .unwrap()
            .unwrap();
        assert!(
            service
                .prepare(
                    Some(service.registry().await.unwrap().initial_session_id()),
                    RuntimeMethod::SessionPromptSubmit
                )
                .await
                .unwrap_err()
                .is::<crate::broker::RequestNotAdmitted>()
        );
        assert!(
            service
                .prepare(
                    Some(service.registry().await.unwrap().initial_session_id()),
                    RuntimeMethod::SessionTurnCancel
                )
                .await
                .unwrap()
                .is_none()
        );
        assert!(
            service
                .prepare(
                    Some(service.registry().await.unwrap().initial_session_id()),
                    RuntimeMethod::SessionStateGet
                )
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
                .prepare(
                    Some(service.registry().await.unwrap().initial_session_id()),
                    RuntimeMethod::SessionCompact
                )
                .await
                .unwrap_err()
                .is::<crate::broker::RequestNotAdmitted>()
        );
        drop(cleanup);
        let resumed = service
            .prepare(
                Some(service.registry().await.unwrap().initial_session_id()),
                RuntimeMethod::SessionCompact,
            )
            .await
            .unwrap()
            .unwrap();
        drop(resumed);
        let prompt = service
            .prepare(
                Some(service.registry().await.unwrap().initial_session_id()),
                RuntimeMethod::SessionPromptSubmit,
            )
            .await
            .unwrap()
            .unwrap();
        assert!(
            service
                .prepare(
                    Some(service.registry().await.unwrap().initial_session_id()),
                    RuntimeMethod::SessionPromptSubmit
                )
                .await
                .is_err()
        );
        assert!(
            service
                .prepare(
                    Some(service.registry().await.unwrap().initial_session_id()),
                    RuntimeMethod::SessionCompact
                )
                .await
                .is_err()
        );
        let cleanup = Arc::clone(&controller.coordinator.control)
            .try_acquire_owned()
            .expect("chat must not retain cleanup admission while its model runs");
        assert!(
            service
                .prepare(
                    Some(service.registry().await.unwrap().initial_session_id()),
                    RuntimeMethod::SessionCheckEditingSet
                )
                .await
                .unwrap()
                .is_none(),
            "check-editing toggles must not need provider execution admission"
        );
        assert!(
            service
                .prepare(
                    Some(service.registry().await.unwrap().initial_session_id()),
                    RuntimeMethod::SessionTurnCancel
                )
                .await
                .unwrap()
                .is_none()
        );
        drop(cleanup);
        drop(prompt);
        service.shutdown(Duration::from_secs(1)).await.unwrap();
    }

    /// Collect an invocation-owned check through the same cancellation command as other work.
    #[tokio::test]
    async fn exchange_cancellation_collects_invocation_check() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        std::fs::write(
            fixture.path().join(".forge.json"),
            serde_json::to_vec(&json!({
                "checks":{"waiting":{"trigger":{"auto":false},"action":{
                    "command":["nu","--no-config-file","-c","sleep 30sec"],
                    "label":"Waiting check","category":"test"
                }}}
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
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let drain =
            tokio::spawn(async move { while output.recv().await.ok().flatten().is_some() {} });
        for (id, method, params) in [
            (2, "session.configure", json!({"approval_mode":"allow"})),
            (
                3,
                "session.provider.mock_response",
                json!({"operation":"script","script":{
                    "steps":[{"kind":"tool_call","name":"check_run","arguments":{"check":"waiting"}}],
                    "terminal":"completed"
                }}),
            ),
            (
                4,
                "session.prompt.submit",
                json!({"text":"Run the waiting check through its tool."}),
            ),
        ] {
            service
                .dispatch(
                    Some(session_id.clone()),
                    Request {
                        id,
                        method: method.into(),
                        params,
                    },
                    &sink,
                )
                .await?;
        }
        let run_id = tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if let Some(id) = controller.checks.active()? {
                    let run = controller.checks.status(&id, 100, false).await?;
                    if run
                        .commands
                        .iter()
                        .any(|command| command.state == crate::checks::CheckState::Running)
                    {
                        break Ok::<_, anyhow::Error>(id);
                    }
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("invocation check did not start")??;
        assert!(controller.observer.snapshot()?.checks_running);
        service
            .dispatch(
                Some(session_id),
                Request {
                    id: 5,
                    method: "session.turn.cancel".into(),
                    params: json!({"operation_id":"collect-invocation-check"}),
                },
                &sink,
            )
            .await?;
        assert!(controller.checks.active()?.is_none());
        assert_eq!(
            controller.checks.status(&run_id, 0, false).await?.state,
            crate::checks::CheckState::Interrupted
        );
        assert!(!controller.observer.snapshot()?.checks_running);
        service.shutdown(Duration::from_secs(1)).await?;
        drop(sink);
        drain.abort();
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
            .prepare(
                Some(service.registry().await.unwrap().initial_session_id()),
                RuntimeMethod::SessionPromptSubmit,
            )
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
        let controller = SessionController::new(broker).unwrap();
        let provider_requests = Arc::new(Mutex::new(Vec::new()));
        let provider = steering_provider(provider_requests.clone());
        let provider_fixture =
            crate::provider::test_support::ProviderTestSession::from_native(provider)?;
        *controller.provider.write().unwrap() = provider_fixture.provider_handle();
        for method in [RuntimeMethod::SessionConfigure] {
            let maintenance = controller.broker.lock().await;
            let mut acquisition = Box::pin(acquire_request_broker(&controller, method));
            assert!(futures_util::poll!(acquisition.as_mut()).is_pending());
            assert!(
                provider_requests.lock().await.is_empty(),
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
                Some(session_id.clone()),
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
            let value: Value = serde_json::from_slice(frame.bytes()).unwrap();
            if value.get("request_id").is_none() {
                continue;
            }
            let event: forge_protocol::message::RequestEvent =
                serde_json::from_value(value).unwrap();
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
                Some(registry.initial_session_id().to_owned()),
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
        let session_id = opened.result().unwrap()["session"]["id"]
            .as_str()
            .unwrap()
            .to_owned();
        let (sink, mut output) = forge_protocol::outbound::channel();
        service
            .dispatch(
                Some(session_id.clone()),
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
        let admission_error = service
            .prepare(Some(&session_id), RuntimeMethod::SessionPromptSubmit)
            .await
            .err()
            .unwrap();
        assert_eq!(admission_error.to_string(), "Harness service is closed");
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
        let controller = SessionController::new(broker)?;
        let provider_requests = Arc::new(Mutex::new(Vec::new()));
        let provider = steering_provider(provider_requests.clone());
        let provider_fixture =
            crate::provider::test_support::ProviderTestSession::from_native(provider)?;
        *controller.provider.write().unwrap() = provider_fixture.provider_handle();
        let session_id = controller.observer.session_id().to_owned();
        let mut store = SqliteStore::open(&controller.data_root)?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        let execution = Arc::clone(&controller.coordinator.execution).try_acquire_owned()?;
        let broker_owner = controller.broker.lock().await;
        let provider_stop = provider_requests.lock().await;
        for (index, action) in ["pause", "clear"].into_iter().enumerate() {
            let previous = controller.task_controller.current()?;
            controller.task_controller.change(
                &mut store,
                previous.as_ref().map(|task| &task.revision),
                crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                    "Preserve provider ownership".into(),
                    Default::default(),
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
        use crate::task::{Task, TaskChange};
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
                TaskChange::Install(Box::new(Task::goal("Finish".into(), Default::default(), 1))),
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
                TaskChange::Install(Box::new(Task::goal(
                    "Replacement".into(),
                    Default::default(),
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
            result.is_err(),
            "user-supplied continuation authority was accepted"
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
    async fn steering_burst_reaches_the_active_turn_before_any_acknowledgment() -> Result<()> {
        use crate::input::{InputDelivery, InputIntent, InputOperation};
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let execution = controller
            .coordinator
            .execution
            .clone()
            .acquire_owned()
            .await?;
        let mut active = controller.parent_input.activate(None)?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        for (id, intent) in [
            ("queued", InputIntent::Queue),
            ("first", InputIntent::Auto),
            ("second", InputIntent::Auto),
            ("third", InputIntent::Auto),
        ] {
            let operation: InputOperation = serde_json::from_value(json!({
                "id": format!("submit-{id}"), "action": {"kind":"submit", "message": {
                    "id":id, "text": format!("steering {id}"), "intent":intent
                }}
            }))?;
            controller.coordinator.submissions.apply(operation)?;
        }
        controller.attach_input(&sink).await?;
        let mut commands = Vec::new();
        for id in ["first", "second", "third"] {
            let command = tokio::time::timeout(Duration::from_secs(5), active.receive())
                .await?
                .context("steering lane closed")?;
            assert_eq!(command.text, format!("steering {id}"));
            let submission = command
                .submission
                .as_ref()
                .context("missing input identity")?;
            assert_eq!(submission.record()?.input_message_id.as_deref(), Some(id));
            submission.sending(None)?;
            commands.push(command);
        }
        let snapshot = controller.coordinator.submissions.snapshot()?;
        assert_eq!(
            snapshot
                .messages
                .iter()
                .filter(|message| message.delivery == InputDelivery::Delivering)
                .count(),
            3
        );
        assert!(
            snapshot
                .messages
                .iter()
                .any(|message| message.id == "queued" && message.delivery == InputDelivery::Queued)
        );
        // Acknowledgments may arrive in a different order from FIFO dispatch.
        for index in [1, 0, 0] {
            let command = commands.remove(index);
            let id = command
                .submission
                .as_ref()
                .unwrap()
                .record()?
                .input_message_id
                .unwrap();
            command.submission.as_ref().unwrap().accepted(Some(
                forge_provider_api::RecoveryToken::new(format!("accepted {id}").into_bytes())?,
            ))?;
            command.complete(Ok(())).await;
        }
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let snapshot = controller.coordinator.submissions.snapshot()?;
                if snapshot.messages.len() == 1 && snapshot.messages[0].id == "queued" {
                    break;
                }
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await??;
        controller.coordinator.submissions.suspend(true, None)?;
        drop(active);
        drop(execution);
        service.shutdown(Duration::from_secs(5)).await?;
        drop(sink);
        reader.await??;
        Ok(())
    }

    /// Preserve cancelled, undispatched followups for explicit recovery instead of replaying them.
    #[tokio::test]
    async fn cancelled_deferred_input_retains_text_and_attachment_without_replay() -> Result<()> {
        use crate::input::{InputDelivery, InputOperation};
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let execution = controller
            .coordinator
            .execution
            .clone()
            .acquire_owned()
            .await?;
        let mut active = controller.parent_input.activate(None)?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let source = fixture.path().join("retained.png");
        std::fs::write(&source, b"\x89PNG\r\n\x1a\nretained image")?;
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.attachment.import".into(),
                    params: json!({"path":source,"number":1}),
                },
                &sink,
            )
            .await?;
        let imported: Value = serde_json::from_slice(
            output
                .recv()
                .await?
                .context("missing imported attachment")?
                .bytes(),
        )?;
        let attachment = imported["result"].clone();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        let operation: InputOperation = serde_json::from_value(json!({
            "id":"deferred-op","action":{"kind":"submit","message":{
                "id":"deferred-input","text":"[Image #1] café 東京","intent":"auto","attachments":[attachment]
            }}
        }))?;
        controller.coordinator.submissions.apply(operation)?;
        controller.attach_input(&sink).await?;
        let command = tokio::time::timeout(Duration::from_secs(5), active.receive())
            .await?
            .context("missing deferred input")?;
        let submission = command
            .submission
            .as_ref()
            .context("missing deferred receipt")?
            .clone();
        assert_eq!(
            submission.record()?.delivery,
            crate::provider::submission::SubmissionState::Prepared
        );
        let before = controller.coordinator.submissions.snapshot()?.messages[0].clone();
        controller.coordinator.submissions.suspend(true, None)?;
        active.retire();
        command.cancel_undispatched().await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let snapshot = controller.coordinator.submissions.snapshot()?;
                if snapshot.messages[0].delivery == InputDelivery::Failed {
                    break Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        })
        .await??;
        let after = controller.coordinator.submissions.snapshot()?.messages[0].clone();
        assert_eq!(after.text, before.text);
        assert_eq!(after.attachments, before.attachments);
        assert_eq!(after.submission_id.as_deref(), Some(submission.id()));
        assert_eq!(
            submission.record()?.delivery,
            crate::provider::submission::SubmissionState::Cancelled
        );
        let fresh: InputOperation = serde_json::from_value(json!({
            "id":"fresh-op","control_revision":controller.coordinator.submissions.snapshot()?.control_revision,
            "action":{"kind":"submit","message":{"id":"fresh-input","text":"A distinct request","intent":"auto"}}
        }))?;
        controller.coordinator.submissions.apply(fresh)?;
        assert_eq!(
            controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .iter()
                .find(|message| message.id == "deferred-input")
                .unwrap()
                .delivery,
            InputDelivery::Failed
        );
        controller.coordinator.submissions.suspend(true, None)?;
        drop(active);
        drop(execution);
        service.shutdown(Duration::from_secs(5)).await?;
        drop(sink);
        reader.await??;
        Ok(())
    }

    #[tokio::test]
    async fn idle_reopen_applies_retained_question_effect() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let observed = Arc::new(std::sync::Mutex::new(Vec::new()));
        let observed_output = observed.clone();
        let reader = tokio::spawn(async move {
            while let Some(frame) = output.recv().await? {
                let message: Value = serde_json::from_slice(frame.bytes())?;
                observed_output.lock().unwrap().push(message);
            }
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_session_events(&sink).await?;
        let (scope, engine) = {
            let mut broker = controller.broker.lock().await;
            let result = broker
                .dispatch(Request {
                    id: 2,
                    method: "session.prompt.submit".into(),
                    params: json!({"text":"hello"}),
                })
                .await;
            anyhow::ensure!(
                result.response.error().is_none(),
                "initial prompt failed: {:?}",
                result.response.error()
            );
            broker.retained_idle_test_owner()?
        };
        engine
            .publish_idle_test_effect(
                scope.clone(),
                crate::provider::ProviderCompletion {
                    plan_question: Some(crate::plan::PlanQuestionSet {
                        id: "idle-question".into(),
                        questions: vec![crate::plan::PlanQuestion {
                            id: "idle-choice".into(),
                            header: "Choice".into(),
                            question: "Which route?".into(),
                            options: Vec::new(),
                            allow_freeform: true,
                        }],
                    }),
                    ..Default::default()
                },
            )
            .await?;
        engine
            .steer(
                scope.clone(),
                crate::provider::SteeringInput::AgentResult("continue".into()),
            )
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let broker = controller.broker.lock().await;
                let committed = broker.retained_idle_test_question(&scope, "idle-question")?;
                let published = observed
                    .lock()
                    .unwrap()
                    .iter()
                    .filter(|message| {
                        message["event"] == "question"
                            && message["payload"]["exchange_id"] == scope.exchange_id.as_str()
                            && message
                                .pointer("/payload/elicitation/question_set/id")
                                .and_then(Value::as_str)
                                == Some("idle-question")
                    })
                    .count();
                anyhow::ensure!(published <= 1, "idle question was published more than once");
                if committed && published == 1 {
                    broker.retained_idle_test_next_admission()?;
                    return Ok::<(), anyhow::Error>(());
                }
                drop(broker);
                tokio::task::yield_now().await;
            }
        })
        .await
        .with_context(|| {
            format!(
                "idle question effect was not applied; observations: {:?}",
                observed.lock().unwrap()
            )
        })??;
        service.shutdown(Duration::from_secs(5)).await?;
        drop(sink);
        reader.await??;
        Ok(())
    }

    /// An unlocked broker does not mean its foreground owner has settled.
    #[tokio::test]
    async fn active_cancel_collects_native_terminal_and_preserves_queued_input() -> Result<()> {
        use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
        use crate::provider::mock_response::MockResponseCommand;
        for (method, params) in [
            ("session.turn.cancel", json!({})),
            (
                "session.command.apply",
                json!({"operation_id":"pause-at-approval", "action":"pause"}),
            ),
        ] {
            let fixture = tempfile::tempdir()?;
            let service = service();
            service
                .open_session(1, initialize(&fixture, "mock"))
                .await?;
            let registry = service.registry().await?;
            let session_id = registry.initial_session_id().to_owned();
            let controller = registry.resolve(&session_id).await?;
            let provider = controller.provider_handle();
            let control = provider.mock_response_control().unwrap();
            let hold = control
                .dispatch(
                    &session_id,
                    MockResponseCommand::Hold {
                        mode: None,
                        any_session: false,
                    },
                )
                .await?;
            let (sink, mut output) = forge_protocol::outbound::channel();
            let responses = Arc::new(std::sync::Mutex::new(Vec::<Value>::new()));
            let seen = responses.clone();
            let reader = tokio::spawn(async move {
                while let Some(frame) = output.recv().await? {
                    seen.lock()
                        .unwrap()
                        .push(serde_json::from_slice(frame.bytes())?);
                }
                Ok::<(), anyhow::Error>(())
            });
            controller.attach_input(&sink).await?;
            let submit = |id: &str, intent: InputIntent| InputOperation {
                id: format!("op-{id}"),
                control_revision: 0,
                after_control: None,
                action: InputAction::Submit {
                    message: Box::new(InputMessage {
                        id: id.into(),
                        revision: 0,
                        text: id.into(),
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
                .apply(submit("active", InputIntent::Auto))?;
            tokio::time::timeout(
                Duration::from_secs(5),
                control.dispatch(
                    &session_id,
                    MockResponseCommand::Wait {
                        id: hold["id"].as_str().unwrap().into(),
                    },
                ),
            )
            .await??;
            controller
                .coordinator
                .submissions
                .apply(submit("queued", InputIntent::Queue))?;
            assert!(
                controller.broker.try_lock().is_ok(),
                "fixture must exercise an unlocked broker"
            );
            let question_ids = controller.questions.ask(
                "active-cancel-questions",
                &SqliteStore::read_only(&controller.data_root)?
                    .latest_exchange(&session_id)?
                    .context("active exchange is missing")?
                    .id,
                crate::question::QuestionOrigin::Tool(None),
                [
                    crate::question::QuestionRequirement::Required,
                    crate::question::QuestionRequirement::Optional,
                ]
                .into_iter()
                .map(|requirement| crate::question::QuestionDraft {
                    header: "Layout".into(),
                    question: "Which layout?".into(),
                    requirement,
                    options: Vec::new(),
                })
                .collect(),
            )?;
            tokio::time::timeout(
                Duration::from_secs(5),
                service.dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: 23,
                        method: method.into(),
                        params,
                    },
                    &sink,
                ),
            )
            .await??;
            tokio::time::timeout(Duration::from_secs(5), async {
                while controller.coordinator.execution.available_permits() == 0 {
                    tokio::task::yield_now().await;
                }
            })
            .await?;
            tokio::time::timeout(Duration::from_secs(2), async {
                loop {
                    if let Some(response) = responses
                        .lock()
                        .unwrap()
                        .iter()
                        .find(|value| value["id"] == 23)
                        .cloned()
                    {
                        anyhow::ensure!(
                            response.get("error").is_none(),
                            "cancel RPC failed: {response}"
                        );
                        break Ok::<(), anyhow::Error>(());
                    }
                    tokio::task::yield_now().await;
                }
            })
            .await??;
            let store = SqliteStore::open(&controller.data_root)?;
            let exchanges = store.list_exchange(&session_id)?;
            assert_eq!(exchanges.len(), 1);
            assert_eq!(
                exchanges[0].state(),
                crate::exchange::ExchangeState::Cancelled
            );
            assert!(matches!(
                controller.questions.snapshot()?.questions[0].resolution,
                crate::question::QuestionResolution::Retired { .. }
            ));
            assert!(
                controller
                    .questions
                    .apply(crate::question::QuestionCommand {
                        operation_id: "required-after-cancel".into(),
                        question_id: question_ids[0].clone(),
                        revision: 1,
                        action: crate::question::QuestionAction::Answer {
                            answer: crate::question::QuestionAnswer::Other {
                                text: "Tabs".into()
                            }
                        },
                    })
                    .is_err()
            );
            controller
                .questions
                .apply(crate::question::QuestionCommand {
                    operation_id: "optional-after-cancel".into(),
                    question_id: question_ids[1].clone(),
                    revision: 1,
                    action: crate::question::QuestionAction::Answer {
                        answer: crate::question::QuestionAnswer::Other {
                            text: "Tabs".into(),
                        },
                    },
                })?;
            controller
                .exchange_owner
                .reconcile_questions(Some(sink.clone()))
                .await?;
            assert!(controller.questions.outbox()?.is_empty());
            assert!(
                !controller.task_input_is_current(&crate::input::InputPurpose::Question {
                    content_revision: None,
                    question_id: question_ids[1].clone(),
                    task_id: None,
                    message_kind: crate::question::QuestionMessageKind::Answer,
                    reply: crate::provider::test_support::question_reply(
                        &question_ids[1],
                        forge_provider_api::QuestionReplyResponse::Answer {
                            answer: "Tabs".into(),
                            required_questions_remain: false
                        }
                    ),
                })?
            );
            assert_eq!(
                SqliteStore::read_only(&controller.data_root)?
                    .list_exchange(&session_id)?
                    .len(),
                1,
                "late answer dispatched a new exchange"
            );
            assert!(controller.coordinator.submissions.snapshot()?.suspended);
            assert!(
                controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .iter()
                    .any(|message| message.id == "queued")
            );
            assert!(!controller.observer.session_events().is_closed());
            service.shutdown(Duration::from_secs(5)).await?;
            drop(sink);
            reader.await??;
        }
        Ok(())
    }

    /// Pause and the following UI cancel share one native cancellation.
    #[tokio::test]
    async fn planning_pause_then_cancel_preserves_terminal_and_queued_input() -> Result<()> {
        use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
        use crate::provider::mock_response::MockResponseCommand;
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let provider = controller.provider_handle();
        let control = provider.mock_response_control().unwrap();
        let hold = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let responses = Arc::new(std::sync::Mutex::new(Vec::<Value>::new()));
        let seen = responses.clone();
        let reader = tokio::spawn(async move {
            while let Some(frame) = output.recv().await? {
                seen.lock()
                    .unwrap()
                    .push(serde_json::from_slice(frame.bytes())?);
            }
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let submit = |id: &str, intent: InputIntent| InputOperation {
            id: format!("op-{id}"),
            control_revision: 0,
            after_control: None,
            action: InputAction::Submit {
                message: Box::new(InputMessage {
                    id: id.into(),
                    revision: 0,
                    text: id.into(),
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
        service.dispatch(
            Some(session_id.clone()),
            Request {
                id: 22,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"plan", "action":"plan", "text":"Inspect the project"}),
            },
            &sink,
        ).await?;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold["id"].as_str().unwrap().into(),
                },
            ),
        )
        .await??;
        controller
            .coordinator
            .submissions
            .apply(submit("queued", InputIntent::Queue))?;
        assert!(
            controller.broker.try_lock().is_ok(),
            "fixture must exercise an unlocked broker"
        );
        tokio::time::timeout(
            Duration::from_secs(10),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 24,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"pause", "action":"pause"}),
                },
                &sink,
            ),
        )
        .await??;
        tokio::time::timeout(
            Duration::from_secs(5),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 23,
                    method: "session.turn.cancel".into(),
                    params: json!({}),
                },
                &sink,
            ),
        )
        .await??;
        tokio::time::timeout(Duration::from_secs(5), async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                if let Some(response) = responses
                    .lock()
                    .unwrap()
                    .iter()
                    .find(|value| value["id"] == 23)
                    .cloned()
                {
                    anyhow::ensure!(
                        response.get("error").is_none(),
                        "cancel RPC failed: {response}"
                    );
                    break Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        })
        .await??;
        for id in [22, 24] {
            let response = tokio::time::timeout(Duration::from_secs(2), async {
                loop {
                    let response = responses
                        .lock()
                        .unwrap()
                        .iter()
                        .find(|value| value["id"] == id)
                        .cloned();
                    if let Some(response) = response {
                        break response;
                    }
                    tokio::task::yield_now().await;
                }
            })
            .await
            .with_context(|| format!("missing command response {id}"))?;
            anyhow::ensure!(
                response.get("error").is_none(),
                "command RPC failed: {response}"
            );
        }
        let store = SqliteStore::open(&controller.data_root)?;
        let exchanges = store.list_exchange(&session_id)?;
        assert_eq!(exchanges.len(), 1);
        assert_eq!(
            exchanges[0].state(),
            crate::exchange::ExchangeState::Cancelled
        );
        assert!(controller.coordinator.submissions.snapshot()?.suspended);
        assert!(
            controller
                .coordinator
                .submissions
                .snapshot()?
                .messages
                .iter()
                .any(|message| message.id == "queued")
        );
        assert!(!controller.observer.session_events().is_closed());
        service.shutdown(Duration::from_secs(5)).await?;
        drop(sink);
        reader.await??;
        Ok(())
    }

    #[tokio::test]
    async fn host_owned_input_steers_and_preserves_edit_and_pause_barriers() -> Result<()> {
        use crate::input::{InputAction, InputEdit, InputIntent, InputMessage, InputOperation};
        use crate::provider::mock_response::MockResponseCommand;
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let controller = registry.resolve(registry.initial_session_id()).await?;
        let session_id = registry.initial_session_id().to_owned();
        let mock_provider = controller.provider_handle();
        let control = mock_provider.mock_response_control().unwrap();
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
        let execution_idle = Arc::new(AtomicBool::new(false));
        let observed_idle = execution_idle.clone();
        let reader = tokio::spawn(async move {
            while let Some(frame) = output.recv().await? {
                let event: Value = serde_json::from_slice(frame.bytes())?;
                if event["event"] == "input_changed" {
                    observed_idle.store(
                        event["payload"]["execution_busy"] == false,
                        Ordering::Release,
                    );
                }
            }
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
        tokio::time::timeout(Duration::from_secs(2), async {
            while !execution_idle.load(Ordering::Acquire) {
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("final input publication retained stale execution ownership")?;
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
        use crate::input::{InputDelivery, InputOperation};
        use crate::provider::submission::SubmissionStore;
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
            crate::provider::ProviderInput::from_text("uncertain"),
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            Some("uncertain-message".into()),
        )?;
        submission.sending(None)?;
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
        service.start_provider_session(&first_id, &sink).await?;
        while !first.provider_ready().await {
            tokio::task::yield_now().await;
        }
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
            "provider":{"kind":"mock","command":[command]},
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
