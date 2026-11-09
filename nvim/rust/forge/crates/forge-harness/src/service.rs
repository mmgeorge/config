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

mod cancellation;
mod check_completion;
mod command;
mod compaction;
mod input_delivery;
#[cfg(test)]
mod input_durability_test;
mod local_configuration;
mod native_permission;
mod native_settings;
mod owner_commands;
#[cfg(test)]
mod plan_question_event_test;
mod prompt;
mod provider_lifecycle;
#[cfg(test)]
mod provider_startup_test;
#[cfg(test)]
mod question_event_test;
#[cfg(test)]
mod question_wait_test;
mod reconnect;
mod review_questions;
#[cfg(test)]
mod session_observation_test;
mod shared;
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
            && let Some(preference) = SqliteStore::open(&config.data_root)?
                .load_preference(workspace, config.provider.kind.as_str())?
        {
            model = preference.model;
        }
        ensure!(
            !model.is_empty() && model.len() <= 256,
            "invalid generation model identity"
        );
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
        // Share the owner-command alias set with the transport coordinator, so an alias such as
        // conversation-only undo is never admitted by one boundary and rejected by the other.
        anyhow::ensure!(
            command::handles_owner_command(&controller, &request)?,
            "unsupported owner task action"
        );
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
        let publications = controller.publications.clone();
        let publication_session = owner_id.to_owned();
        let attempt = lease.attempt;
        let request_id = request.id;
        let recorded = sink.observe_responses(move |response| {
            if response.id == request_id {
                coordinator
                    .complete(&command_id, attempt, response)
                    .map_err(std::io::Error::other)?;
                publications
                    .publish(
                        SessionEvent {
                            session_id: publication_session.clone(),
                            event: "command_completed".into(),
                            payload: json!({"id":command_id,"revision":0}),
                        },
                        None,
                    )
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
            controller.publications.publish(
                SessionEvent {
                    session_id: controller.observer.session_id().into(),
                    event: "command_completed".into(),
                    payload: json!({"id":identity.id,"revision":0}),
                },
                None,
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
        // Normalization rewrites `session.plan.accept` into the `execute` owner command,
        // so acceptance from PlanReview is recognized by its review document. Without
        // this capture the command carries no reviewed digest and would accept
        // whichever revision is latest instead of the one the user reviewed.
        let reviewed_execute = method == RuntimeMethod::SessionCommandApply
            && request.params["action"] == "execute"
            && request.params.get("review").is_some();
        if reviewed_execute
            || matches!(
                method,
                RuntimeMethod::SessionPlanAccept
                    | RuntimeMethod::SessionPlanChangesRequest
                    | RuntimeMethod::SessionPlanCancel
                    | RuntimeMethod::SessionPlanEntityRename
                    | RuntimeMethod::SessionPlanTestDelete
                    | RuntimeMethod::SessionPlanCheckUpdate
            )
        {
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
                        // Acceptance needs only the reviewed identity. Its saved comments
                        // travel as draft annotations; passing them as execute
                        // annotations would turn the acceptance into a revision request.
                        if reviewed_execute && !matches!(key.as_str(), "plan_id" | "digest") {
                            continue;
                        }
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
                    if let Some(control) = controller.provider_handle().mock_response_control() {
                        control
                            .cancel_session(controller.observer.session_id())
                            .await?;
                    }
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
        let mut broker = tokio::task::spawn_blocking(move || {
            HarnessBroker::initialize_with_runtime(startup_request, startup_runtime)
        })
        .await
        .context("Harness session startup worker failed")??;
        startup_timing.insert(
            "broker_ready_ms".into(),
            json!(startup_started.elapsed().as_secs_f64() * 1000.0),
        );
        let broker_timing = std::mem::take(&mut broker.startup_timing);
        let controller = SessionController::new(broker).await?;
        let snapshot = controller.observer.snapshot()?;
        let mut result = serde_json::to_value(&snapshot)?;
        startup_timing.insert(
            "snapshot_serialized_ms".into(),
            json!(startup_started.elapsed().as_secs_f64() * 1000.0),
        );
        *harness = Some(SessionControllerRegistry::new(
            Some((snapshot.session.id.clone(), controller)),
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
    /// Fail every later provider initialization, so tests reach a restart that cannot finish.
    ///
    /// No provider boundary fails restart initialization, because it opens no native session.
    #[cfg(test)]
    initialization_fails: AtomicBool,
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

    async fn background_terminals(
        &self,
    ) -> Result<crate::provider::terminal_status::BackgroundTerminalStatus> {
        let pending = || {
            crate::provider::terminal_status::BackgroundTerminalStatus::pending_startup(
                self.provider_startup_error
                    .lock()
                    .expect("provider startup state poisoned")
                    .is_none(),
            )
        };
        if !self.provider_ready().await {
            return Ok(pending());
        }
        // Select the advertised generation only after retirement exclusion is acquired.
        let Ok(generation) = self.provider_lane.clone().try_read_owned() else {
            return Ok(pending());
        };
        if !self.provider_ready().await {
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
            || !self.provider_ready().await
        {
            return Ok(pending());
        }
        Ok(crate::provider::terminal_status::BackgroundTerminalStatus::from_inventory(result??))
    }

    /// Build one independently serialized controller from a durable broker session.
    async fn new(mut broker: HarnessBroker) -> Result<Arc<Self>> {
        broker.resolve_inherited_model_settings().await?;
        broker.snapshot()?;
        let catalog_request = broker.provider_catalog_request()?;
        let observer = broker.observer();
        let store = SqliteStore::read_only(&broker.data_root)?;
        for plan in store.list_plan(observer.session_id())? {
            observer.plan_file.reconcile_plan_artifacts(&plan)?;
        }

        let input = observer.input.clone();
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
            #[cfg(test)]
            initialization_fails: AtomicBool::new(false),
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
                presentation: controller.presentation.clone(),
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
        let owner = crate::provider::ThreadOwner::root(
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
        let controller = SessionController::new(broker).await?;
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
    // Provider-bound child work waits for its native fork, or it would open fresh history.
    if false && (method.requires_provider_fork() || new_queued_submission) {
        registry
            .await_provider_fork(&session_id, &controller)
            .await?;
    }
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
        let result = match command {
            crate::provider::mock_response::MockResponseCommand::ExpireApproval => {
                let owner = crate::provider::ThreadOwner::root(&session_id);
                let expired_count = controller.permission.expire_pending_for_owner(&owner)?;
                anyhow::ensure!(
                    expired_count > 0,
                    "no timed approval is pending for {session_id}"
                );
                serde_json::json!({ "expired": expired_count })
            }
            command => control.dispatch(&session_id, command).await?,
        };
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
        message_sink
            .send_response(Response::success(request.id, model_list)?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SessionProviderCapabilitiesGet {
        let provider = controller.provider_handle();
        let manifest = tokio::time::timeout(
            Duration::from_secs(30),
            provider.capabilities(controller.catalog_request.read().await.clone()),
        )
        .await
        .context("capability discovery exceeded 30 seconds")??;
        let menu = crate::provider::capability_menu::CapabilityMenu::compose(
            provider.descriptor().label,
            manifest,
        );
        message_sink
            .send_response(Response::success(request.id, menu)?)
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
                    acknowledged_child_reports: Vec::new(),
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
        message_sink
            .send_control_response(Response::success(request.id, Value::Null)?)
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
    // Native fork rejects a thread whose children still execute, and the optimistic child
    // session would otherwise inherit their live rows with no runtime left to settle them.
    if let Some(source) = registry.controller_by_id.read().await.get(&session_id) {
        anyhow::ensure!(
            source.agent_coordinator.active_count() == 0,
            "cannot fork while child agents are running; wait for them to finish or interrupt them"
        );
    }
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
            .thread_fork(preparation.provider_request)
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
    let session = crate::broker::SessionNotFound::require(store.load_session(&target)?, &target)?;
    let plans = crate::plan::PlanFileStore::new(
        std::path::Path::new(&registry.initialize.data_root),
        &session.workspace,
    )
    .unwrap();
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
    // A closed semaphore means a failed collection fenced the session, not that work is running.
    let blocked = |error: tokio::sync::TryAcquireError, work: &str| match error {
        tokio::sync::TryAcquireError::NoPermits => {
            anyhow::anyhow!("cannot delete a session with active {work}")
        }
        tokio::sync::TryAcquireError::Closed => anyhow::anyhow!(
            "cannot delete this session because its last execution could not be confirmed stopped; restart Harness and delete it again"
        ),
    };
    let _control = controller
        .coordinator
        .control
        .clone()
        .try_acquire_owned()
        .map_err(|error| blocked(error, "control work"))?;
    let _execution = controller
        .coordinator
        .execution
        .clone()
        .try_acquire_owned()
        .map_err(|error| blocked(error, "execution"))?;
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
mod tests;
