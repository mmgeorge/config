use super::{
    normalization::CodexNormalization,
    resources::CodexResources,
    rpc::{NativeRpc, RpcHandle},
    runtime::CodexRuntime,
    security::CodexSecurity,
};
use async_trait::async_trait;
use forge_provider_api::{
    OperationContext, ProviderError, ProviderEvent, ProviderFailure, ProviderModel,
    ProviderRuntime,
    catalog::{McpQuery, QueryContext, WorkspaceContext},
    completion::ProviderCompletion,
    error::{ProviderFailureKind, ProviderOperation},
    events::{self, ProviderEventSink, ProviderEventStream},
    generation::ReasoningSelection,
    input::PromptInput,
    lifecycle::{
        CompletionSnapshot, ExecutionFailure, ExecutionObservation, ExecutionResult,
        ExecutionUpdate,
    },
    permissions::{PermissionRequest, PermissionResponse, PermissionTarget},
    provider::Provider,
    session::{
        BindingId, InputAcceptance, ModelSelection, PromptRequest, SessionBinding, SessionRequest,
        SteeringAcceptance, SteeringRequest,
    },
};
use serde_json::{Value, json};
use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};
use tokio::sync::{Mutex, mpsc, watch};

/// Own Codex app-server conversations, execution correlation, and native cleanup.
#[derive(Clone)]
pub struct CodexProvider {
    command: Vec<String>,
    runtime_root: PathBuf,
    runtime: Arc<CodexRuntime>,
    bindings: Arc<Mutex<HashMap<BindingId, Arc<CodexBinding>>>>,
    owners: Arc<Mutex<HashMap<forge_provider_api::ExecutionOwner, BindingId>>>,
    catalog: Arc<Mutex<Option<CatalogConnection>>>,
    catalog_version: Arc<Mutex<forge_provider_api::VersionObservation>>,
    discovery: Arc<Mutex<Vec<Arc<NativeRpc>>>>,
    shutdown_failure: Arc<Mutex<Option<ProviderError>>>,
    shutdown_lock: Arc<Mutex<()>>,
    closed: Arc<AtomicBool>,
    closed_bindings: Arc<Mutex<std::collections::HashSet<BindingId>>>,
    opening: Arc<Mutex<()>>,
    operations: Arc<Mutex<Vec<Arc<crate::retained_task::RetainedTask>>>>,
    pending: Arc<Mutex<HashMap<BindingId, Arc<PendingOpen>>>>,
    preparing: Arc<Mutex<HashMap<forge_provider_api::OperationId, Arc<PreparedProfile>>>>,
    fork_targets: Arc<Mutex<HashMap<forge_provider_api::OperationId, Arc<PendingFork>>>>,
    pending_inputs: Arc<crate::pending_input::PendingInputs>,
    records: Arc<super::operations::Records>,
}

impl CodexProvider {
    /// Create the native owner with its configured command and private resource root.
    pub fn new(command: Vec<String>, runtime_root: PathBuf) -> Result<Self, ProviderError> {
        if command.is_empty() {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Rejected,
            ));
        }
        Ok(Self {
            runtime: Arc::new(CodexRuntime::new(command.clone())),
            command,
            runtime_root,
            bindings: Arc::new(Mutex::new(HashMap::new())),
            owners: Arc::new(Mutex::new(HashMap::new())),
            catalog: Arc::new(Mutex::new(None)),
            catalog_version: Arc::new(Mutex::new(
                forge_provider_api::VersionObservation::NotReported,
            )),
            discovery: Arc::new(Mutex::new(Vec::new())),
            shutdown_failure: Arc::new(Mutex::new(None)),
            shutdown_lock: Arc::new(Mutex::new(())),
            closed: Arc::new(AtomicBool::new(false)),
            closed_bindings: Arc::new(Mutex::new(std::collections::HashSet::new())),
            opening: Arc::new(Mutex::new(())),
            operations: Arc::new(Mutex::new(Vec::new())),
            pending: Arc::new(Mutex::new(HashMap::new())),
            preparing: Arc::new(Mutex::new(HashMap::new())),
            fork_targets: Arc::new(Mutex::new(HashMap::new())),
            pending_inputs: Arc::new(crate::pending_input::PendingInputs::new()),
            records: Arc::new(super::operations::Records::new()),
        })
    }

    /// Initialize catalog discovery independently of any conversation or prompt.
    async fn catalog_connection(&self) -> Result<Arc<NativeRpc>, ProviderError> {
        if self.closed.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::Inspect,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let mut catalog = self.catalog.lock().await;
        super::operations::check()?;
        if catalog.is_none() {
            let runtime = self.runtime.clone();
            let discovery = self.discovery.clone();
            let version = self.catalog_version.clone();
            let command = self.command.clone();
            let native = super::operations::current();
            *catalog = Some(CatalogConnection::Starting(tokio::spawn(async move {
                let connect = async move {
                    let connection = runtime.connect().await.map_err(|_| {
                        failure(ProviderOperation::Inspect, ProviderFailureKind::Transport)
                    })?;
                    let rpc = Arc::new(NativeRpc::new(connection));
                    discovery.lock().await.push(rpc.clone());
                    let initialized = async {
                    let initialized = rpc.request("initialize", json!({
                        "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
                        "capabilities":{"experimentalApi":true}
                    }), ProviderOperation::Inspect).await?;
                    let reported = initialized.get("userAgent").and_then(Value::as_str).and_then(|agent| agent.split_whitespace().next()).and_then(|product| product.split_once('/').map(|(_, version)| version));
                    let requirement = forge_provider_api::version::RuntimeRequirement::new(crate::registry::select("codex").expect("Codex is registered"), PathBuf::from(&command[0]), semver::Version::new(0, 164, 0));
                    requirement.validate(reported).map_err(ProviderError::from)?;
                    *version.lock().await = reported.map_or(forge_provider_api::VersionObservation::NotReported, |version| forge_provider_api::VersionObservation::Known { version: version.to_owned() });
                    rpc.notify("initialized", Value::Null).await
                }.await;
                    if let Err(primary) = initialized {
                        return Err(match rpc.close().await {
                            Ok(()) => primary,
                            Err(secondary) => ProviderError::Combined {
                                primary: Box::new(primary),
                                secondary: vec![secondary],
                            },
                        });
                    }
                    Ok(rpc)
                };
                match native {
                    Some(native) => super::operations::scope(native, connect).await,
                    None => connect.await,
                }
            })));
        }
        let result = match catalog.as_mut().expect("catalog initialization installed") {
            CatalogConnection::Ready(rpc) => return Ok(rpc.clone()),
            CatalogConnection::Starting(task) => task.await.unwrap_or_else(|_| {
                Err(failure(
                    ProviderOperation::Inspect,
                    ProviderFailureKind::Runtime,
                ))
            }),
        };
        match result {
            Ok(rpc) => {
                *catalog = Some(CatalogConnection::Ready(rpc.clone()));
                Ok(rpc)
            }
            Err(error) => {
                *catalog = None;
                Err(error)
            }
        }
    }

    async fn binding(&self, id: &BindingId) -> Result<Arc<CodexBinding>, ProviderError> {
        if let Some(binding) = self.bindings.lock().await.get(id).cloned() {
            return Ok(binding);
        }
        let kind = if self.closed_bindings.lock().await.contains(id) {
            ProviderFailureKind::BindingClosed
        } else {
            ProviderFailureKind::SessionMissing
        };
        Err(failure(ProviderOperation::InputSubmit, kind))
    }

    /// Exclude a second writable generation until every previous live or pending native owner is collected.
    /// The shared opening mutex serializes this check with pending reservation and binding installation.
    async fn ensure_history_available(
        &self,
        history: &forge_provider_api::ProviderSessionId,
    ) -> Result<(), ProviderError> {
        if self
            .fork_targets
            .lock()
            .await
            .values()
            .any(|fork| &fork.target == history)
        {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        if self
            .bindings
            .lock()
            .await
            .values()
            .any(|session| &session.history_id() == history)
        {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        let pending = self
            .pending
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        for pending in pending {
            if pending.history.lock().await.as_ref() == Some(history) {
                return Err(failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::ResourceConflict,
                ));
            }
        }
        Ok(())
    }

    async fn initialize(&self, request: SessionRequest) -> Result<SessionBinding, ProviderError> {
        super::operations::check()?;
        if self.closed.load(Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if request.workspace.trim().is_empty()
            || self.owners.lock().await.contains_key(&request.owner)
        {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Rejected,
            ));
        }
        if let Some(history) = request.history.resume_id() {
            self.ensure_history_available(history).await?;
        }
        let isolated = matches!(
            &request.history,
            forge_provider_api::session::SessionHistory::IsolatedText(_)
        );
        if isolated
            && (!request.tools.is_empty()
                || request.tool_endpoint.is_some()
                || !request.configuration.resources.skills.is_empty()
                || !request.configuration.resources.mcp.is_empty())
        {
            return Err(failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Policy,
            ));
        }
        validate_model_selection(&request.configuration.model)?;
        let security = CodexSecurity::new(&request.configuration.access)?;
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string())
            .map_err(|_| failure(ProviderOperation::SessionOpen, ProviderFailureKind::Unknown))?;
        let session_scope = forge_provider_api::EventScope::Binding {
            owner: request.owner.clone(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), session_scope);

        let operation_id = super::operations::current()
            .expect("open preparation has an operation owner")
            .context
            .id()
            .clone();
        let profile = crate::profile::ResourceProfile::create(&self.runtime_root, "codex")
            .map_err(|error| error.into_provider_error(ProviderOperation::SessionOpen))?;
        let prepared = Arc::new(PreparedProfile {
            profile: Mutex::new(Some(profile)),
        });
        self.preparing
            .lock()
            .await
            .insert(operation_id.clone(), prepared.clone());
        let mut prepared_profile = prepared.profile.lock().await;
        let profile = prepared_profile
            .as_mut()
            .expect("open retains its prepared profile until pending ownership transfers");
        profile
            .bind_skills(&request.configuration.resources.skills, "skills")
            .map_err(|error| error.into_provider_error(ProviderOperation::SessionOpen))?;
        let private_home = profile.path().to_path_buf();
        let resources = CodexResources::new(
            request.configuration.resources.clone(),
            private_home.clone(),
        );
        let (runtime, isolation_auth) = if isolated {
            let rpc = self.catalog_connection().await?;
            let catalog = rpc
                .request("model/list", json!({}), ProviderOperation::SessionOpen)
                .await?;
            let models = super::catalog::parse_model_catalog(&catalog);
            let selected = match request.configuration.model.model.as_deref() {
                Some(id) => models.iter().find(|model| model.id == id),
                None => models.iter().find(|model| model.is_default),
            }
            .ok_or_else(|| {
                failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::Rejected,
                )
            })?;
            let config = rpc
                .request(
                    "config/read",
                    json!({"includeLayers":false,"cwd":request.workspace}),
                    ProviderOperation::SessionOpen,
                )
                .await?;
            let config = config.get("config").ok_or_else(|| {
                failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::Protocol,
                )
            })?;
            let (command, home) =
                super::isolation::runtime_command(&self.command, profile.path(), selected, config)?;
            (
                Arc::new(CodexRuntime::isolated(command, home)),
                Some(Arc::new(super::isolation::IsolationAuth::new(rpc))),
            )
        } else {
            (Arc::new(CodexRuntime::new(self.command.clone())), None)
        };
        let pending = Arc::new(PendingOpen {
            operation_id: operation_id.clone(),
            runtime: runtime.clone(),
            rpc: Mutex::new(None),
            profile: Mutex::new(prepared_profile.take()),
            history: Mutex::new(request.history.resume_id().cloned()),
        });
        self.pending
            .lock()
            .await
            .insert(binding_id.clone(), pending.clone());
        drop(prepared_profile);
        self.preparing.lock().await.remove(&operation_id);
        super::operations::mark_changed();
        super::operations::check()?;
        let connection = runtime.connect().await.map_err(|_| {
            failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Transport,
            )
        })?;
        let rpc = Arc::new(NativeRpc::new(connection));
        *pending.rpc.lock().await = Some(rpc.clone());
        let initialized = rpc.request("initialize", json!({
            "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
            "capabilities":{"experimentalApi":true}
        }), ProviderOperation::SessionOpen).await?;
        let requirement = forge_provider_api::version::RuntimeRequirement::new(
            crate::registry::select("codex").expect("Codex is registered"),
            self.command
                .first()
                .map(PathBuf::from)
                .unwrap_or_else(|| "app-server".into()),
            semver::Version::new(0, 164, 0),
        );
        let reported = initialized
            .get("userAgent")
            .and_then(Value::as_str)
            .and_then(|agent| agent.split_whitespace().next())
            .and_then(|product| product.split_once('/').map(|(_, version)| version));
        requirement
            .validate(reported)
            .map_err(ProviderError::from)?;
        rpc.notify("initialized", Value::Null).await?;
        if isolated {
            let version = reported
                .and_then(|version| semver::Version::parse(version).ok())
                .ok_or_else(|| {
                    failure(
                        ProviderOperation::SessionOpen,
                        ProviderFailureKind::Protocol,
                    )
                })?;
            if version != semver::Version::new(0, 164, 0) {
                return Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::TextGeneration,
                )
                .into());
            }
            isolation_auth
                .as_ref()
                .expect("isolated runtime owns its credential bridge")
                .bootstrap(&rpc)
                .await?;
        }

        let resource_config = resources.configure(&rpc, &request.workspace, &[]).await?;
        let mut parameters = match request.history.resume_id() {
            Some(thread) => json!({
                "threadId":thread.as_str(), "cwd":request.workspace,
                "excludeTurns":true, "developerInstructions":request.system_instructions,
                "config":{}
            }),
            None => json!({
                "cwd":request.workspace, "ephemeral":isolated, "historyMode":"paginated",
                "experimentalRawEvents":false, "developerInstructions":request.system_instructions,
                "dynamicTools":request.tools.iter().map(|tool| json!({
                    "name":tool.name,"description":tool.description,"inputSchema":tool.input_schema
                })).collect::<Vec<_>>(),
                "serviceTier":request.configuration.model.service_tier
            }),
        };
        if !resource_config.is_null() {
            parameters["config"] = resource_config;
        }
        parameters["config"]["features.goals"] = json!(false);
        parameters["config"]["thread_unload_delay_secs"] = json!(0);
        if isolated {
            let config = parameters["config"]
                .as_object_mut()
                .expect("resource configuration is an object");
            config.extend(
                isolated_config()
                    .as_object()
                    .expect("isolation configuration is an object")
                    .clone(),
            );
            parameters["baseInstructions"] = json!(request.system_instructions);
            parameters["developerInstructions"] = Value::Null;
        }
        let _ = apply_model_selection(
            &rpc,
            &mut parameters,
            &request.configuration.model,
            ProviderOperation::SessionOpen,
        )
        .await?;
        security.apply_thread(&mut parameters, &request.workspace);
        if isolated {
            parameters["approvalPolicy"] = json!("never");
            parameters["sandbox"] = json!("read-only");
        }
        let method = if request.history.resume_id().is_some() {
            "thread/resume"
        } else {
            "thread/start"
        };
        let result = rpc
            .request(method, parameters, ProviderOperation::SessionOpen)
            .await?;
        let thread_id = result
            .pointer("/thread/id")
            .or_else(|| result.get("threadId"))
            .or_else(|| result.get("thread_id"))
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or_else(|| request.history.resume_id().map(|id| id.as_str().to_owned()))
            .ok_or_else(|| {
                failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let session_id =
            forge_provider_api::ProviderSessionId::try_from(thread_id).map_err(|_| {
                failure(
                    ProviderOperation::SessionOpen,
                    ProviderFailureKind::Protocol,
                )
            })?;
        *pending.history.lock().await = Some(session_id.clone());
        let binding = SessionBinding {
            id: binding_id.clone(),
            provider_session_id: Some(session_id.clone()),
        };
        let mut mcp_context = super::protocol::McpContext::default();
        mcp_context.set_bindings(resources.mcp_aliases());
        let auth_status = rpc
            .request(
                "getAuthStatus",
                json!({"includeToken":false,"refreshToken":false}),
                ProviderOperation::SessionOpen,
            )
            .await
            .ok();
        let capabilities = negotiated_capabilities(reported, auth_status.as_ref());
        let retained = Arc::new(CodexBinding {
            binding: binding.clone(),
            history: std::sync::Mutex::new(if request.history.resume_id().is_none() {
                NativeHistory::Virgin(session_id)
            } else {
                NativeHistory::Established(session_id)
            }),
            native_tools: request.tools,
            opened_by: super::operations::current()
                .expect("session open runs under a retained operation")
                .context
                .id()
                .clone(),
            owner: request.owner.clone(),
            workspace: request.workspace,
            model: Mutex::new(request.configuration.model.clone()),
            runtime: Mutex::new(runtime),
            rpc: RpcHandle::from_shared(rpc),
            sink,
            stream: Mutex::new(Some(stream)),
            normalization: Mutex::new(CodexNormalization::default()),
            resources: Mutex::new(resources),
            mcp_context: Mutex::new(mcp_context),
            profile: Mutex::new(pending.profile.lock().await.take()),
            _lease: request.resource_lease,
            system_instructions: request.system_instructions,
            access: Mutex::new(request.configuration.access.clone()),
            approval: Mutex::new(request.configuration.approval),
            approval_context: super::approval::ApprovalContext::default(),
            active: Mutex::new(None),
            admitting: Mutex::new(None),
            background_reader: Mutex::new(None),
            terminal_observer: Mutex::new(None),
            admission: Mutex::new(()),
            observations: Arc::new(tokio::sync::RwLock::new(false)),
            fenced: AtomicBool::new(false),
            invalidated: AtomicBool::new(false),
            closing: AtomicBool::new(false),
            provider_closed: self.closed.clone(),
            isolated,
            isolation_auth,
            configuration: Mutex::new(request.configuration.clone()),
            capabilities,
            settlements: Mutex::new(HashMap::new()),
            outcomes: Mutex::new(HashMap::new()),
            close_failure: Mutex::new(None),
            close_lock: Mutex::new(()),
            settlement_lock: Mutex::new(()),
            retired_rpcs: Mutex::new(Vec::new()),
            retired_profiles: Mutex::new(Vec::new()),
            rollback: Mutex::new(None),
        });
        self.bindings
            .lock()
            .await
            .insert(binding_id.clone(), retained.clone());
        *retained.terminal_observer.lock().await = Some(start_terminal_observer(&retained));
        self.pending.lock().await.remove(&binding_id);
        self.owners.lock().await.insert(request.owner, binding_id);
        Ok(binding)
    }

    async fn submit(
        &self,
        binding: &BindingId,
        request: PromptRequest,
        operation_id: String,
        pending: &crate::pending_input::PendingInput,
    ) -> Result<InputAcceptance, ProviderError> {
        let session = self.binding(binding).await?;
        session.check_input_admission(ProviderOperation::InputSubmit)?;
        pending.check_admission(ProviderOperation::InputSubmit)?;
        if session.stream.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        if request.scope.owner != session.owner
            || request.receipt.id() != &request.submission_id
            || request.receipt.attempt_id()? != request.attempt_id
            || !request.dispatch.matches(
                binding,
                &request.scope,
                &request.submission_id,
                &request.attempt_id,
            )
        {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        validate_model_selection(&request.configuration.model)?;
        let security = CodexSecurity::new(&request.configuration.access)?;
        if session.isolated
            && (!request.tools.is_empty()
                || request.tool_endpoint.is_some()
                || !matches!(request.input, PromptInput::Text(_))
                || !request.recovered_inputs.is_empty())
        {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Policy,
            ));
        }
        let _admission = tokio::select! {
            biased;
            () = pending.cancelled() => return Err(pending.check_admission(ProviderOperation::InputSubmit).expect_err("preparation cancellation is sticky")),
            admission = session.admission.lock() => admission,
        };
        pending.check_admission(ProviderOperation::InputSubmit)?;
        super::operations::check()?;
        session.check_input_admission(ProviderOperation::InputSubmit)?;
        collect_finished_execution(&session).await?;
        let mut active = session.active.lock().await;
        if active.is_some() {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let admitting = Arc::new(AdmittingInput {
            scope: request.scope.clone(),
            operation_id,
            native_may_exist: AtomicBool::new(false),
            cancel: AtomicBool::new(false),
            done: watch::channel(false).0,
        });
        *session.admitting.lock().await = Some(admitting.clone());
        pending.registered();
        stop_background_reader(&session).await?;
        tokio::select! {
            biased;
            () = pending.cancelled() => return Err(pending.check_admission(ProviderOperation::InputSubmit).expect_err("preparation cancellation is sticky")),
            result = request.turn_control.before_send(&request.scope) => result?,
        }
        session
            .normalization
            .lock()
            .await
            .bind(Some(request.scope.clone()));
        session.rollback.lock().await.take();
        let mut prompt = input_text(&request.input);
        let recovered_text = request
            .recovered_inputs
            .iter()
            .map(|recovered| {
                let text = input_text(&recovered.input);
                if recovered.instructions.is_empty() {
                    text
                } else if text.is_empty() {
                    recovered.instructions.clone()
                } else {
                    format!("{text}\n\n{}", recovered.instructions)
                }
            })
            .collect::<Vec<_>>();
        if !recovered_text.is_empty() {
            prompt = forge_provider_api::input::recovered_text(&prompt, &recovered_text);
        }
        if let Some(task) = &request.task
            && !task.title.is_empty()
        {
            prompt = format!("{}\n\n{prompt}", task.heading());
        }
        let mut attachments = request
            .recovered_inputs
            .iter()
            .flat_map(|recovered| recovered.attachments.iter().cloned())
            .collect::<Vec<_>>();
        attachments.extend(request.attachments.iter().cloned());
        let mut native_input = super::attachments::input(prompt, &attachments);
        for recovered in &request.recovered_inputs {
            if let PromptInput::Skill { name, .. } = &recovered.input {
                let rpc = session.rpc.lock().await;
                let path = session
                    .resources
                    .lock()
                    .await
                    .skill_path(&rpc, &session.workspace, name)
                    .await?;
                native_input.push(json!({"type":"skill","name":name,"path":path}));
            }
        }
        if let PromptInput::Skill { name, .. } = &request.input {
            let rpc = session.rpc.lock().await;
            let path = session
                .resources
                .lock()
                .await
                .skill_path(&rpc, &session.workspace, name)
                .await?;
            native_input.push(json!({"type":"skill","name":name,"path":path}));
        }
        let model = request.configuration.model.clone();
        let mut params = json!({
            "threadId":session.history_id().as_str(),
            "input":native_input.clone(),
            "clientUserMessageId":native_input_id(&request.submission_id, &request.attempt_id),
            "serviceTier":model.service_tier,
            "config":{}
        });
        let rpc = session.rpc.lock().await;
        let effort =
            apply_model_selection(&rpc, &mut params, &model, ProviderOperation::InputSubmit)
                .await?;
        drop(rpc);
        let enabled_tools = request
            .tools
            .iter()
            .map(|tool| tool.name)
            .collect::<Vec<_>>();
        let instructions = if enabled_tools.is_empty() {
            request.instructions.clone()
        } else {
            format!(
                "{}\n\nFor this invocation, the available host tools are: {}.",
                request.instructions,
                enabled_tools.join(", ")
            )
        };
        params["collaborationMode"] = json!({
            "mode":"default",
            "settings":{
                "model":params["model"].clone(),
                "reasoning_effort":effort.clone(),
                "developer_instructions":instructions
            }
        });
        security.apply_turn(&mut params, &session.workspace);
        params["config"]["features.goals"] = json!(false);
        if session.isolated {
            params["approvalPolicy"] = json!("never");
            params["sandboxPolicy"] = json!({"type":"readOnly","networkAccess":false});
        }
        let token = recovery_token(&session, &request.submission_id, &request.attempt_id)?;
        if let Err(error) = request.receipt.clone().record_sending(Some(token)).await {
            return Err(record_receipt_failure(request.receipt.clone(), error).await);
        }
        if let Err(error) = pending.check_admission(ProviderOperation::InputSubmit) {
            request
                .receipt
                .clone()
                .record_rejected(error.clone())
                .await?;
            return Err(error);
        }
        session
            .history
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .establish();
        admitting.native_may_exist.store(true, Ordering::Release);
        let id = match session
            .rpc
            .lock()
            .await
            .send_request("turn/start", params)
            .await
        {
            Ok(id) => id,
            Err(error) => {
                if matches!(&error, ProviderError::Failure(failure) if failure.phase() == forge_provider_api::ProviderPhase::Admission && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
                {
                    admitting.native_may_exist.store(false, Ordering::Release);
                    request
                        .receipt
                        .clone()
                        .record_rejected(error.clone())
                        .await?;
                    return Err(ProviderError::PreparationFailed(Box::new(error)));
                }
                let primary = combine_primary(ProviderError::DeliveryUncertain, error);
                return Err(record_receipt_failure(request.receipt.clone(), primary).await);
            }
        };
        request.dispatch.confirm();
        let (turn_id, initial_messages) = match await_admission(&session, &request, id).await {
            Ok(admitted) => admitted,
            Err(error) => {
                if matches!(&error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Rejected && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
                {
                    admitting.native_may_exist.store(false, Ordering::Release);
                    request
                        .receipt
                        .clone()
                        .record_rejected(error.clone())
                        .await?;
                    return Err(ProviderError::PreparationFailed(Box::new(error)));
                }
                return Err(record_receipt_failure(
                    request.receipt.clone(),
                    combine_primary(ProviderError::DeliveryUncertain, error),
                )
                .await);
            }
        };
        let accepted_token =
            match recovery_token(&session, &request.submission_id, &request.attempt_id) {
                Ok(token) => token,
                Err(error) => {
                    return Err(fence_unobserved_acceptance(
                        &session,
                        &request,
                        Some(turn_id.as_str()),
                        error,
                    )
                    .await);
                }
            };
        if let Err(error) = request
            .receipt
            .clone()
            .record_accepted(Some(accepted_token))
            .await
        {
            let error = record_receipt_failure(request.receipt.clone(), error).await;
            return Err(fence_unobserved_acceptance(
                &session,
                &request,
                Some(turn_id.as_str()),
                error,
            )
            .await);
        }
        let address = forge_provider_api::TurnAddress {
            scope: request.scope.clone(),
            thread_id: session.history_id().as_str().to_owned(),
            turn_id: turn_id.clone().into(),
        };
        let started = match ExecutionObservation::new(
            request.scope.clone(),
            ExecutionUpdate::Started {
                address: address.clone(),
            },
        ) {
            Ok(observation) => observation,
            Err(error) => {
                return Err(fence_unobserved_acceptance(
                    &session,
                    &request,
                    Some(&turn_id),
                    error.into(),
                )
                .await);
            }
        };
        if let Err(error) = publish(
            &session.sink,
            &request.scope,
            ProviderEvent::Execution {
                observation: started,
            },
        )
        .await
        {
            return Err(
                fence_unobserved_acceptance(&session, &request, Some(&turn_id), error).await,
            );
        }
        let (cancel, receiver) = mpsc::unbounded_channel();
        if admitting.cancel.load(Ordering::Acquire) || session.is_fenced() {
            let _ = cancel.send(());
        }
        let (turn_updates, _) = watch::channel(Some(turn_id.clone()));
        let submissions = Arc::new(Mutex::new(vec![request.submission_id.clone()]));
        let active_execution = ActiveExecution {
            scope: request.scope.clone(),
            turn_id,
            turn_updates,
            submissions: submissions.clone(),
            request: request.clone(),
            cancel,
            task: None,
        };
        *active = Some(active_execution);
        let accepted_submission_id = request.submission_id.clone();
        let dispatched = request.dispatch.clone();
        let session_task = session.clone();
        let task = Arc::new(crate::retained_task::RetainedTask::new(
            ProviderOperation::InputSubmit,
            tokio::spawn(async move {
                observe_execution(
                    session_task,
                    request,
                    address,
                    submissions,
                    receiver,
                    initial_messages,
                )
                .await
            }),
        ));
        active.as_mut().expect("active execution was inserted").task = Some(task.clone());
        let collector_session = session.clone();
        let collector_scope = active
            .as_ref()
            .expect("active execution was inserted")
            .scope
            .clone();
        let collector = tokio::spawn(async move {
            collect_invocation(collector_session, collector_scope, task).await
        });
        self.operations
            .lock()
            .await
            .push(Arc::new(crate::retained_task::RetainedTask::new(
                ProviderOperation::InputSubmit,
                collector,
            )));
        dispatched.confirm();
        Ok(InputAcceptance {
            submission_id: accepted_submission_id,
        })
    }

    async fn configure_binding(
        &self,
        binding: &BindingId,
        configuration: forge_provider_api::SessionConfiguration,
    ) -> Result<forge_provider_api::ConfiguredSession, ProviderError> {
        let session = self.binding(binding).await?;
        let resources = configuration.resources.clone();
        validate_model_selection(&configuration.model)?;
        let security = CodexSecurity::new(&configuration.access)?;
        if session.isolated {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::Policy,
            ));
        }
        let _admission = session.admission.lock().await;
        super::operations::check()?;
        session.check_input_admission(ProviderOperation::SessionConfigure)?;
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let history = session
            .history
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        let mut next_profile = crate::profile::ResourceProfile::create(&self.runtime_root, "codex")
            .map_err(|error| error.into_provider_error(ProviderOperation::SessionConfigure))?;
        if let Err(error) = next_profile.bind_skills(&resources.skills, "skills") {
            let primary = error.into_provider_error(ProviderOperation::SessionConfigure);
            return Err(match next_profile.close() {
                Ok(()) => primary,
                Err(error) => {
                    session.retired_profiles.lock().await.push(next_profile);
                    combine_primary(
                        primary,
                        error.into_provider_error(ProviderOperation::SessionConfigure),
                    )
                }
            });
        }
        let next_resources =
            CodexResources::new(resources.clone(), next_profile.path().to_path_buf());
        // Validate read-only native defaults and surviving resources while the existing reader remains authoritative.
        let mut model_parameters = json!({"config":{}});
        let current_rpc = session.rpc.lock().await;
        let prepared = async {
            let thread = session.history_id();
            if session
                .normalization
                .lock()
                .await
                .has_pending_background_tools()
                || !terminal_inventory(&session, thread.as_str())
                    .await?
                    .is_empty()
            {
                return Err(failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::ResourceConflict,
                ));
            }
            apply_model_selection(
                &current_rpc,
                &mut model_parameters,
                &configuration.model,
                ProviderOperation::SessionConfigure,
            )
            .await?;
            Ok::<_, ProviderError>(())
        }
        .await;
        if let Err(error) = prepared {
            let primary = configuration_preflight_error(error);
            return Err(match next_profile.close() {
                Ok(()) => primary,
                Err(error) => {
                    session.retired_profiles.lock().await.push(next_profile);
                    combine_primary(
                        primary,
                        configuration_preflight_error(
                            error.into_provider_error(ProviderOperation::SessionConfigure),
                        ),
                    )
                }
            });
        }
        session.retired_profiles.lock().await.push(next_profile);
        super::operations::mark_changed();
        super::operations::check()?;
        if let Err(error) = stop_background_reader(&session).await {
            return Err(invalidate_configuration(
                &session,
                error,
                forge_provider_api::ProviderPhase::Collection,
            ));
        }
        let retired_aliases = session
            .resources
            .lock()
            .await
            .mcp_aliases()
            .into_keys()
            .collect::<Vec<_>>();
        let next_runtime = session.runtime.lock().await.clone();
        let connection = match next_runtime.connect().await {
            Ok(connection) => connection,
            Err(_) => {
                return Err(invalidate_configuration(
                    &session,
                    ProviderFailure::new(
                        ProviderOperation::SessionConfigure,
                        ProviderFailureKind::Transport,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Preparation,
                        forge_provider_api::FailureDisposition::Invalidated,
                    )
                    .into(),
                    forge_provider_api::ProviderPhase::Preparation,
                ));
            }
        };
        let next_rpc = Arc::new(NativeRpc::new(connection));
        session.retired_rpcs.lock().await.push(next_rpc.clone());
        let resumed = async {
            let initialized = next_rpc.request("initialize", json!({
                "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
                "capabilities":{"experimentalApi":true}
            }), ProviderOperation::SessionConfigure).await?;
            let requirement = forge_provider_api::version::RuntimeRequirement::new(
                crate::registry::select("codex").expect("Codex is registered"),
                self.command
                    .first()
                    .map(PathBuf::from)
                    .unwrap_or_else(|| "app-server".into()),
                semver::Version::new(0, 164, 0),
            );
            let version = initialized
                .get("userAgent")
                .and_then(Value::as_str)
                .and_then(|agent| agent.split_whitespace().next())
                .and_then(|product| product.split_once('/').map(|(_, version)| version));
            requirement.validate(version).map_err(ProviderError::from)?;
            next_rpc.notify("initialized", Value::Null).await?;
            let config = next_resources
                .configure(&next_rpc, &session.workspace, &retired_aliases)
                .await?;
            let thread_id = session.history_id();
            let model = configuration.model.clone();
            let access = configuration.access.clone();
            let approval = configuration.approval;
            let mut parameters = match &history {
                NativeHistory::Virgin(_) => json!({"cwd":session.workspace,"ephemeral":false,"historyMode":"paginated","experimentalRawEvents":false,"developerInstructions":session.system_instructions,"dynamicTools":session.native_tools.iter().map(|tool| json!({"name":tool.name,"description":tool.description,"inputSchema":tool.input_schema})).collect::<Vec<_>>(),"config":{}}),
                NativeHistory::Established(_) => json!({"threadId":thread_id.as_str(),"cwd":session.workspace,"excludeTurns":true,"developerInstructions":session.system_instructions,"config":{}}),
            };
            if !config.is_null() {
                parameters["config"] = config;
            }
            parameters["config"]["features.goals"] = json!(false);
            parameters["config"]["thread_unload_delay_secs"] = json!(0);
            parameters["model"] = model_parameters["model"].clone();
            parameters["serviceTier"] = model_parameters["serviceTier"].clone();
            parameters["config"]["model_reasoning_effort"] =
                model_parameters["config"]["model_reasoning_effort"].clone();
            security.apply_thread(&mut parameters, &session.workspace);
            // Loaded native threads ignore resume overrides; virgin threads have no
            // rollout at all. Collect the old writer before applying complete settings.
            release_native_thread_writer(&current_rpc, &thread_id, ProviderOperation::SessionConfigure).await?;
            let method = match &history { NativeHistory::Virgin(_) => "thread/start", NativeHistory::Established(_) => "thread/resume" };
            let applied = next_rpc.request(
                    method,
                    parameters,
                    ProviderOperation::SessionConfigure,
                )
                .await?;
            let next_id = applied.pointer("/thread/id").and_then(Value::as_str).ok_or_else(|| failure(ProviderOperation::SessionConfigure, ProviderFailureKind::Protocol))?;
            let next_id = forge_provider_api::ProviderSessionId::try_from(next_id.to_owned()).map_err(|_| failure(ProviderOperation::SessionConfigure, ProviderFailureKind::Protocol))?;
            if matches!(&history, NativeHistory::Established(_)) && next_id != thread_id { return Err(failure(ProviderOperation::SessionConfigure, ProviderFailureKind::HistoryConflict)); }
            Ok::<_, ProviderError>((model, access, approval, next_id))
        }
        .await;
        let (model, access, approval, next_history) = match resumed {
            Ok(settings) => settings,
            Err(error) => {
                let primary = invalidate_configuration(
                    &session,
                    error,
                    forge_provider_api::ProviderPhase::Preparation,
                );
                return Err(match next_rpc.close().await {
                    Ok(()) => primary,
                    Err(cleanup) => combine_primary(primary, cleanup),
                });
            }
        };

        let old_rpc = session.rpc.replace_shared(next_rpc.clone()).await;
        session
            .retired_rpcs
            .lock()
            .await
            .retain(|rpc| !Arc::ptr_eq(rpc, &next_rpc));
        let next_profile = session
            .retired_profiles
            .lock()
            .await
            .pop()
            .expect("configuration retains its replacement profile");
        *session.runtime.lock().await = next_runtime.clone();
        {
            let mut context = session.mcp_context.lock().await;
            context.set_bindings(next_resources.mcp_aliases());
            context.clear_calls();
        }
        *session.resources.lock().await = next_resources;
        *session.model.lock().await = model;
        *session.access.lock().await = access;
        *session.approval.lock().await = approval;
        *session.configuration.lock().await = configuration.clone();
        if matches!(&history, NativeHistory::Virgin(_)) {
            *session
                .history
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner) =
                NativeHistory::Virgin(next_history.clone());
        }
        if let Some(old_profile) = session.profile.lock().await.replace(next_profile) {
            session.retired_profiles.lock().await.push(old_profile);
        }
        session.retired_rpcs.lock().await.push(old_rpc);
        if let Err(error) = collect_retired(&session).await {
            return Err(invalidate_configuration(
                &session,
                error,
                forge_provider_api::ProviderPhase::Collection,
            ));
        }
        if &next_history != history.id()
            && let Err(error) = session
                .sink
                .send_wait(ProviderEvent::SessionIdentity {
                    scope: forge_provider_api::EventScope::Binding {
                        owner: session.owner.clone(),
                    },
                    provider_session_id: next_history,
                })
                .await
        {
            return Err(invalidate_configuration(
                &session,
                error.into(),
                forge_provider_api::ProviderPhase::Observation,
            ));
        }
        match start_background_reader(&session).await {
            Ok(Some(start)) => {
                if start.send(()).is_err() {
                    return Err(invalidate_configuration(
                        &session,
                        failure(
                            ProviderOperation::SessionConfigure,
                            ProviderFailureKind::Runtime,
                        ),
                        forge_provider_api::ProviderPhase::Collection,
                    ));
                }
            }
            Ok(None) => {}
            Err(error) => {
                return Err(invalidate_configuration(
                    &session,
                    error,
                    forge_provider_api::ProviderPhase::Collection,
                ));
            }
        }
        Ok(forge_provider_api::ConfiguredSession {
            configuration,
            capabilities: session.capabilities.clone(),
            watermark: session.sink.watermark(),
        })
    }

    /// Retract the exact output-free cancelled input only while its native turn remains the newest boundary.
    async fn retract_input(
        &self,
        binding: &BindingId,
        request: forge_provider_api::InputRetractRequest,
    ) -> Result<forge_provider_api::InputRetractOutcome, ProviderError> {
        use forge_provider_api::{
            InputRetractOutcome as Outcome, InputRetractUnavailable as Unavailable,
            RetractionAuthorization,
        };
        let session = self.binding(binding).await?;
        let _admission = session.admission.lock().await;
        match request.authorization {
            RetractionAuthorization::OutputFreeCancellation => {}
        }
        session.check_input_admission(ProviderOperation::InputRetract)?;
        if session.active.lock().await.is_some() {
            return Ok(Outcome::Unavailable(Unavailable::HistoryBoundaryChanged));
        }
        let rollback = session.rollback.lock().await;
        let Some(authorization) = rollback.as_ref() else {
            return Ok(Outcome::Unavailable(Unavailable::NotCancelled));
        };
        if authorization.scope != request.scope
            || authorization.submission_id != request.submission_id
            || authorization.attempt_id != request.attempt_id
        {
            return Ok(Outcome::Unavailable(Unavailable::WrongInvocation));
        }
        if authorization.turn_id.as_str() != request.boundary.as_str() {
            return Ok(Outcome::Unavailable(Unavailable::HistoryBoundaryChanged));
        }
        if session.normalization.lock().await.replay_sensitive {
            return Ok(Outcome::Unavailable(Unavailable::NotCancelled));
        }
        if !session.settlements.lock().await.get(&request.scope).is_some_and(|settlement| matches!(settlement, Ok(settlement) if matches!(settlement.result, ExecutionResult::Cancelled { .. }))) {
            return Ok(Outcome::Unavailable(Unavailable::NotCancelled));
        }
        let rpc = session.rpc.lock().await;
        let history = rpc
            .request(
                "thread/read",
                json!({"threadId":authorization.thread_id,"includeTurns":true}),
                ProviderOperation::InputRetract,
            )
            .await?;
        let Some(turns) = history.pointer("/thread/turns").and_then(Value::as_array) else {
            return Ok(Outcome::Unknown);
        };
        let Some(turn) = turns.last() else {
            return Ok(Outcome::Unavailable(Unavailable::HistoryBoundaryChanged));
        };
        if !retractable_turn(turn, &request) {
            return Ok(Outcome::Unavailable(Unavailable::HistoryBoundaryChanged));
        }
        let previous = turns
            .iter()
            .rev()
            .nth(1)
            .and_then(|turn| turn.get("id"))
            .and_then(Value::as_str)
            .map(str::to_owned);
        // Record uncertainty before native mutation; a lost response never authorizes a second rollback.
        super::operations::mark_changed();
        super::operations::check()?;
        session.fenced.store(true, Ordering::Release);
        if let Err(error) = rpc
            .request(
                "thread/rollback",
                json!({"threadId":authorization.thread_id,"numTurns":1}),
                ProviderOperation::InputRetract,
            )
            .await
        {
            invalidate_retraction(&session, error).await;
            return Ok(Outcome::Unknown);
        }
        let history = match rpc
            .request(
                "thread/read",
                json!({"threadId":authorization.thread_id,"includeTurns":true}),
                ProviderOperation::InputRetract,
            )
            .await
        {
            Ok(history) => history,
            Err(error) => {
                invalidate_retraction(&session, error).await;
                return Ok(Outcome::Unknown);
            }
        };
        let checked = history
            .pointer("/thread/turns")
            .and_then(Value::as_array)
            .is_some_and(|turns| {
                !turns.iter().any(|turn| {
                    turn.get("id").and_then(Value::as_str) == Some(request.boundary.as_str())
                }) && turns
                    .last()
                    .and_then(|turn| turn.get("id"))
                    .and_then(Value::as_str)
                    == previous.as_deref()
            });
        if !checked {
            invalidate_retraction(
                &session,
                failure(
                    ProviderOperation::InputRetract,
                    ProviderFailureKind::HistoryConflict,
                ),
            )
            .await;
            return Ok(Outcome::Unknown);
        }
        let outcome = Outcome::Retracted {
            boundary: request.boundary.clone(),
        };
        drop(rollback);
        session.rollback.lock().await.take();
        if !session.closing.load(Ordering::Acquire) && !session.invalidated.load(Ordering::Acquire)
        {
            session.fenced.store(false, Ordering::Release);
        }
        Ok(outcome)
    }

    async fn steer_input(
        &self,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let session = self.binding(binding).await?;
        session.check_input_admission(ProviderOperation::InputSteer)?;
        if request.scope.owner != session.owner {
            return Err(failure(
                ProviderOperation::InputSteer,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if session.stream.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::InputSteer,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        let _admission = session.admission.lock().await;
        session.check_input_admission(ProviderOperation::InputSteer)?;
        let (initial_turn, submissions, prompt_request) = {
            let active = session.active.lock().await;
            let Some(execution) = active.as_ref() else {
                return Err(failure(
                    ProviderOperation::InputSteer,
                    ProviderFailureKind::WrongInvocation,
                ));
            };
            if execution.scope != request.scope
                || execution.turn_updates.borrow().is_none()
                || execution
                    .task
                    .as_ref()
                    .is_some_and(|task| task.is_finished())
            {
                return Err(failure(
                    ProviderOperation::InputSteer,
                    ProviderFailureKind::WrongInvocation,
                ));
            }
            (
                execution.turn_id.clone(),
                execution.submissions.clone(),
                execution.request.clone(),
            )
        };
        let submission_id = request.delivery.submission_id().cloned();
        let receipt = request.delivery.receipt().cloned();
        if let (Some(receipt), Some(submission_id)) = (&receipt, &submission_id) {
            let token = recovery_token(&session, submission_id, &receipt.attempt_id()?)?;
            if let Err(error) = receipt.clone().record_sending(Some(token)).await {
                return Err(record_receipt_failure(receipt.clone(), error).await);
            }
        }
        let mut prompt = input_text(&request.input);
        if let Some(task) = &request.task
            && !task.title.is_empty()
        {
            prompt = format!("{}\n\n{prompt}", task.heading());
        }
        let expected_turn_id = session
            .active
            .lock()
            .await
            .as_ref()
            .map(|execution| execution.turn_id.clone())
            .unwrap_or_else(|| initial_turn.clone());
        let mut params = json!({
            "threadId":session.history_id().as_str(),
            "expectedTurnId":expected_turn_id,
            "input":super::attachments::input(prompt, &request.attachments)
        });
        if let Some(submission_id) = &submission_id {
            params["clientUserMessageId"] = json!(native_input_id(
                submission_id,
                &receipt
                    .as_ref()
                    .expect("journaled delivery has a receipt")
                    .attempt_id()?
            ));
        }
        let result = session
            .rpc
            .lock()
            .await
            .request("turn/steer", params, ProviderOperation::InputSteer)
            .await;
        if let Err(error) = result {
            if matches!(&error, ProviderError::Failure(failure) if failure.disposition() == forge_provider_api::FailureDisposition::Unchanged && (failure.kind() == ProviderFailureKind::Rejected || failure.phase() == forge_provider_api::ProviderPhase::Admission))
            {
                if let Some(receipt) = receipt {
                    receipt.record_rejected(error.clone()).await?;
                }
                return Err(ProviderError::PreparationFailed(Box::new(error)));
            }
            let primary = combine_primary(ProviderError::DeliveryUncertain, error);
            let primary = if let Some(receipt) = receipt {
                record_receipt_failure(receipt, primary).await
            } else {
                primary
            };
            return Err(fence_unobserved_acceptance(
                &session,
                &prompt_request,
                Some(&expected_turn_id),
                primary,
            )
            .await);
        }
        if let (Some(receipt), Some(submission_id)) = (receipt, submission_id) {
            let accepted_token =
                match recovery_token(&session, &submission_id, &receipt.attempt_id()?) {
                    Ok(token) => token,
                    Err(error) => {
                        return Err(fence_unobserved_acceptance(
                            &session,
                            &prompt_request,
                            Some(&expected_turn_id),
                            error,
                        )
                        .await);
                    }
                };
            if let Err(error) = receipt.clone().record_accepted(Some(accepted_token)).await {
                let primary = record_receipt_failure(receipt, error).await;
                return Err(fence_unobserved_acceptance(
                    &session,
                    &prompt_request,
                    Some(expected_turn_id.as_str()),
                    primary,
                )
                .await);
            }
            submissions.lock().await.push(submission_id.clone());
            Ok(SteeringAcceptance::Journaled { submission_id })
        } else {
            Ok(SteeringAcceptance::Context)
        }
    }

    async fn reconcile_input(
        &self,
        request: forge_provider_api::InputReconcileRequest,
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        let submission_id = &request.submission_id;
        let Some(evidence) = parse_recovery_token(request.token.as_ref()) else {
            return Ok(forge_provider_api::SubmissionEvidence::Unknown);
        };
        if evidence.submission_id != submission_id.as_str()
            || evidence.attempt_id != request.attempt_id.as_str()
            || request
                .provider_session_id
                .as_ref()
                .is_some_and(|id| id.as_str() != evidence.thread_id)
        {
            return Ok(forge_provider_api::SubmissionEvidence::Unknown);
        }
        let connection = match self.runtime.connect().await {
            Ok(connection) => connection,
            Err(_) => {
                return Err(failure(
                    ProviderOperation::InputReconcile,
                    ProviderFailureKind::Transport,
                ));
            }
        };
        let rpc = Arc::new(NativeRpc::new(connection));
        self.discovery.lock().await.push(rpc.clone());
        let operation_result: Result<forge_provider_api::SubmissionEvidence, ProviderError> =
            async {
                let initialized = rpc.request("initialize", json!({
                "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
                "capabilities":{"experimentalApi":true}
            }), ProviderOperation::InputReconcile).await?;
                let requirement = forge_provider_api::version::RuntimeRequirement::new(
                    crate::registry::select("codex").expect("Codex is registered"),
                    self.command
                        .first()
                        .map(PathBuf::from)
                        .unwrap_or_else(|| "app-server".into()),
                    semver::Version::new(0, 164, 0),
                );
                let version = initialized
                    .get("userAgent")
                    .and_then(Value::as_str)
                    .and_then(|agent| agent.split_whitespace().next())
                    .and_then(|product| product.split_once('/').map(|(_, version)| version));
                requirement.validate(version).map_err(ProviderError::from)?;
                rpc.notify("initialized", Value::Null).await?;
                let mut cursor = None::<String>;
                let mut seen_cursors = std::collections::HashSet::new();
                loop {
                    let page = rpc.request("thread/items/list", json!({
                    "threadId":evidence.thread_id,"cursor":cursor,"sortDirection":"asc","limit":100
                }), ProviderOperation::InputReconcile).await?;
                    let rows = page.get("data").and_then(Value::as_array).ok_or_else(|| {
                        failure(
                            ProviderOperation::InputReconcile,
                            ProviderFailureKind::Protocol,
                        )
                    })?;
                    let accepted = history_page_evidence(
                        rows,
                        &evidence.thread_id,
                        submission_id,
                        &request.attempt_id,
                    )?;
                    if matches!(
                        accepted,
                        forge_provider_api::SubmissionEvidence::Accepted { .. }
                    ) {
                        return Ok(accepted);
                    }
                    let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
                        return Ok(forge_provider_api::SubmissionEvidence::Unknown);
                    };
                    if !seen_cursors.insert(next.to_owned()) {
                        return Ok(forge_provider_api::SubmissionEvidence::Unknown);
                    }
                    cursor = Some(next.to_owned());
                }
            }
            .await;
        let cleanup = rpc.close().await;
        if cleanup.is_ok() {
            self.discovery
                .lock()
                .await
                .retain(|owned| !Arc::ptr_eq(owned, &rpc));
        }
        match (operation_result, cleanup) {
            (Ok(evidence), Ok(())) => Ok(evidence),
            (Ok(_), Err(error)) | (Err(error), Ok(())) => Err(error),
            (Err(primary), Err(secondary)) => Err(combine_primary(primary, secondary)),
        }
    }

    async fn compact_binding(
        &self,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        let operation =
            super::operations::current().expect("compaction retains its operation owner");
        request.validate_context(&operation.context)?;
        let session = self.binding(binding).await?;
        let observation = session.observations.read().await;
        let admission = session.admission.lock().await;
        session.check_input_admission(ProviderOperation::SessionCompact)?;
        if *observation {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if request.scope.owner() != &session.owner {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        // A terminal event can arrive before its reader returns. Join only a
        // settled execution; compaction must never interrupt an active turn.
        collect_settled_execution(&session).await?;
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::SessionCompact,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        // Transfer exclusive notification consumption from the idle reader.
        super::operations::check()?;
        stop_background_reader(&session).await?;
        let result = compact_session(&session, request).await;
        if matches!(&result, Err(ProviderError::Failure(failure))
            if matches!(failure.kind(), ProviderFailureKind::Timeout | ProviderFailureKind::Transport | ProviderFailureKind::Protocol | ProviderFailureKind::Cancelled)
                && failure.disposition() != forge_provider_api::FailureDisposition::Unchanged)
        {
            // Unknown native completion cannot authorize another prompt on this binding.
            session.invalidated.store(true, Ordering::Release);
            session.fenced.store(true, Ordering::Release);
            if let Err(error) = &result {
                let mut cause = session.close_failure.lock().await;
                if cause.is_none() {
                    *cause = Some(error.clone());
                }
            }
            session.sink.fail(events::EventDeliveryError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable,
            ));
            // The native reader can return interruption before the retained supervisor
            // selects its cleanup hook. Collect this binding's exclusive runtime here too.
            drop(admission);
            drop(observation);
            let cleanup = super::operations::cleanup(self.close_binding(binding)).await;
            return match (result, cleanup) {
                (Err(primary), Err(secondary)) => Err(combine_primary(primary, secondary)),
                (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
                (Ok(compaction), Ok(())) => Ok(forge_provider_api::CompactionResult {
                    compaction,
                    watermark: session.sink.watermark(),
                }),
            };
        }
        let restarted = start_background_reader(&session).await;
        match (result, restarted) {
            (result, Ok(start)) => {
                if let Some(start) = start {
                    let _ = start.send(());
                }
                result.map(|compaction| forge_provider_api::CompactionResult {
                    compaction,
                    watermark: session.sink.watermark(),
                })
            }
            (Ok(_), Err(error)) => Err(error),
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
        }
    }

    async fn terminate_binding_terminal(
        &self,
        binding: &BindingId,
        terminal: &forge_provider_api::terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        let identity: Option<[String; 2]> = serde_json::from_str(terminal.as_str()).ok();
        let Some([terminal_binding, terminal_id]) = identity else {
            return Err(failure(
                ProviderOperation::TerminalTerminate,
                ProviderFailureKind::Rejected,
            ));
        };
        if terminal_binding != binding.as_str() {
            return Err(failure(
                ProviderOperation::TerminalTerminate,
                ProviderFailureKind::Rejected,
            ));
        }
        let session = self.binding(binding).await?;
        let _admission = session.admission.lock().await;
        super::operations::check()?;
        if session.is_fenced() {
            return Err(failure(
                ProviderOperation::TerminalTerminate,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let thread_id = session.history_id();
        let result = session
            .rpc
            .lock()
            .await
            .request(
                "thread/backgroundTerminals/terminate",
                json!(
                    {"threadId":thread_id,"processId":terminal_id}
                ),
                ProviderOperation::TerminalTerminate,
            )
            .await?;
        if result.get("terminated").and_then(Value::as_bool) != Some(true) {
            return Err(failure(
                ProviderOperation::TerminalTerminate,
                ProviderFailureKind::Rejected,
            ));
        }
        Ok(())
    }

    /// Open native history under serialized ownership; the retained operation transfers its stream once.
    async fn open_session(
        &self,
        request: SessionRequest,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        let _opening = self.opening.lock().await;
        let operation = super::operations::current()
            .expect("open retains its operation owner")
            .context
            .id()
            .clone();
        let binding = match self.initialize(request).await {
            Ok(binding) => binding,
            Err(primary) => {
                return Err(
                    match super::operations::cleanup(self.collect_open_operation(&operation)).await
                    {
                        Ok(()) => primary,
                        Err(secondary) => combine_primary(primary, secondary),
                    },
                );
            }
        };
        if let Err(primary) = super::operations::check() {
            return Err(
                match super::operations::cleanup(self.collect_open_operation(&operation)).await {
                    Ok(()) => primary,
                    Err(secondary) => combine_primary(primary, secondary),
                },
            );
        }
        let session = self.binding(&binding.id).await?;
        let events = session
            .stream
            .lock()
            .await
            .take()
            .expect("open operation owns its sole event stream");
        Ok(forge_provider_api::OpenedSession {
            binding,
            capabilities: session.capabilities.clone(),
            events,
        })
    }

    /// Complete pending-admission bookkeeping even if every observer has stopped awaiting input acceptance.
    async fn submit_input(
        &self,
        binding: &BindingId,
        request: PromptRequest,
        pending: crate::pending_input::PendingInput,
    ) -> Result<InputAcceptance, ProviderError> {
        let scope = request.scope.clone();
        let operation_id = super::operations::current()
            .expect("input submission has an operation owner")
            .context
            .id()
            .as_str()
            .to_owned();
        let mut result = self
            .submit(binding, request, operation_id.clone(), &pending)
            .await;
        drop(pending);
        if let Ok(session) = self.binding(binding).await {
            let mut admitting = session.admitting.lock().await;
            if admitting
                .as_ref()
                .is_some_and(|admitting| admitting.operation_id == operation_id)
            {
                if let Err(error) = &result {
                    let native_may_exist = admitting.as_ref().is_some_and(|admitting| {
                        admitting.native_may_exist.load(Ordering::Acquire)
                    });
                    if native_may_exist {
                        session.fenced.store(true, Ordering::Release);
                        session
                            .settlements
                            .lock()
                            .await
                            .entry(scope.clone())
                            .or_insert_with(|| Err(error.clone()));
                    } else if !session.is_fenced() {
                        // No native invocation was installed. Restore observation of prior
                        // background work before publishing no-future-registration to cancel.
                        let restored = match start_background_reader(&session).await {
                            Ok(Some(start)) => start.send(()).map_err(|_| {
                                failure(
                                    ProviderOperation::InputSubmit,
                                    ProviderFailureKind::Runtime,
                                )
                            }),
                            Ok(None) => Ok(()),
                            Err(error) => Err(error),
                        };
                        if let Err(restore_error) = restored {
                            let restore_error = invalidate_background_observer(
                                &session,
                                restore_error,
                                forge_provider_api::ObservationFailure::LifecycleUnavailable,
                            )
                            .await;
                            result = Err(combine_primary(error.clone(), restore_error));
                        }
                    }
                }
                if let Some(admitting) = admitting.take() {
                    admitting.done.send_replace(true);
                }
            }
        }
        // Native observation can notice interruption and finish before the retained
        // supervisor selects its interruption hook. This worker still owns collection
        // of a dispatched attempt whose native turn identity never became available.
        let interrupted = super::operations::current().is_some_and(|native| {
            native.changed()
                && native
                    .context
                    .check_admission(ProviderOperation::InputSubmit)
                    .is_err()
        });
        if result.is_err() && interrupted {
            let cleanup =
                super::operations::cleanup(self.collect_interrupted_input(binding, &scope)).await;
            return match (result, cleanup) {
                (Err(primary), Err(secondary)) => Err(combine_primary(primary, secondary)),
                (result, Ok(())) => result,
                (Ok(_), Err(error)) => Err(error),
            };
        }
        result
    }

    /// Collect only the interrupted attempt's binding, without selecting a later pending or active successor.
    async fn collect_interrupted_input(
        &self,
        binding: &BindingId,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        let session = match self.binding(binding).await {
            Ok(session) => session,
            Err(_) if self.closed_bindings.lock().await.contains(binding) => return Ok(()),
            Err(error) => return Err(error),
        };
        let admission = session.admission.lock().await;
        if session
            .admitting
            .lock()
            .await
            .as_ref()
            .is_some_and(|admitting| &admitting.scope != scope)
            || session
                .active
                .lock()
                .await
                .as_ref()
                .is_some_and(|execution| &execution.scope != scope)
            || session
                .settlements
                .lock()
                .await
                .get(scope)
                .is_some_and(Result::is_ok)
        {
            return Ok(());
        }
        session.fenced.store(true, Ordering::Release);
        drop(admission);
        self.close_binding(binding).await
    }

    /// Copy one stable native source and retain its target disposition in the typed operation store.
    async fn fork_history(
        &self,
        request: forge_provider_api::SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        validate_model_selection(&request.target.configuration.model)?;
        let security = CodexSecurity::new(&request.target.configuration.access)?;
        let (rpc, thread_id, checkpoint, session) = match &request.source {
            forge_provider_api::ForkSource::Binding(id) => {
                let session = self.binding(id).await?;
                let rpc = session.rpc.lock().await.shared();
                let thread = session.history_id();
                (rpc, thread, None, Some(session))
            }
            forge_provider_api::ForkSource::Checkpoint {
                provider_session_id,
                checkpoint,
            } => (
                self.catalog_connection().await?,
                provider_session_id.clone(),
                Some(checkpoint.clone()),
                None,
            ),
        };
        let admission = match session.as_ref() {
            Some(session) => Some(session.admission.lock().await),
            None => None,
        };
        if let Some(session) = &session {
            session.check_input_admission(ProviderOperation::SessionFork)?;
            collect_settled_execution(session).await?;
            if session.active.lock().await.is_some() {
                return Err(failure(
                    ProviderOperation::SessionFork,
                    ProviderFailureKind::ExecutionActive,
                ));
            }
        }
        let mut parameters = json!({"threadId":thread_id.as_str(),"cwd":request.target.workspace,"excludeTurns":true,"developerInstructions":request.target.system_instructions,"config":{"features.goals":false,"thread_unload_delay_secs":0}});
        if let Some(checkpoint) = checkpoint {
            parameters["lastTurnId"] = json!(checkpoint.as_str());
        }
        apply_model_selection(
            &rpc,
            &mut parameters,
            &request.target.configuration.model,
            ProviderOperation::SessionFork,
        )
        .await?;
        security.apply_thread(&mut parameters, &request.target.workspace);
        let result = async {
            let value = rpc
                .request("thread/fork", parameters, ProviderOperation::SessionFork)
                .await?;
            let id = value
                .pointer("/thread/id")
                .and_then(Value::as_str)
                .ok_or_else(|| {
                    failure(
                        ProviderOperation::SessionFork,
                        ProviderFailureKind::Protocol,
                    )
                })?;
            let id =
                forge_provider_api::ProviderSessionId::try_from(id.to_owned()).map_err(|_| {
                    failure(
                        ProviderOperation::SessionFork,
                        ProviderFailureKind::Protocol,
                    )
                })?;
            let operation = super::operations::current()
                .expect("fork retains its operation owner")
                .context
                .id()
                .clone();
            let target = Arc::new(PendingFork {
                operation: operation.clone(),
                target: id.clone(),
                rpc: rpc.clone(),
                source_binding: session.as_ref().map(|session| session.binding.id.clone()),
                cleanup: Mutex::new(()),
            });
            self.fork_targets
                .lock()
                .await
                .insert(operation, target.clone());
            // Fork creates a loaded writer in the source process. Complete its exact
            // release even if the host observation expires before exposing durable history.
            super::operations::cleanup(self.collect_fork_target(&target)).await?;
            Ok(forge_provider_api::ProviderForkResult::unprofiled(id))
        }
        .await;
        drop(admission);
        result
    }

    /// Release the same fork writer across cleanup retries without creating another target or touching its source.
    async fn collect_fork_target(&self, target: &Arc<PendingFork>) -> Result<(), ProviderError> {
        let _cleanup = target.cleanup.lock().await;
        if !self
            .fork_targets
            .lock()
            .await
            .get(&target.operation)
            .is_some_and(|retained| Arc::ptr_eq(retained, target))
        {
            return Ok(());
        }
        release_native_thread_writer(&target.rpc, &target.target, ProviderOperation::SessionFork)
            .await?;
        self.fork_targets.lock().await.remove(&target.operation);
        Ok(())
    }

    /// Collect all known fork targets while their source native owners are still available.
    async fn collect_fork_targets(&self) -> Result<(), ProviderError> {
        let targets = self
            .fork_targets
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        let mut errors = Vec::new();
        for target in targets {
            if let Err(error) = super::operations::cleanup(self.collect_fork_target(&target)).await
            {
                errors.push(error);
            }
        }
        combine_errors(errors)
    }

    /// Inspect installation and native discovery without starting any conversation or login.
    async fn inspect_runtime(
        &self,
        _workspace: WorkspaceContext,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        let executable = self
            .command
            .first()
            .and_then(|program| forge_shell::process::executable(program));
        let (runtime, version, capabilities) = if executable.is_none() {
            (
                forge_provider_api::RuntimeAvailability::Missing,
                forge_provider_api::VersionObservation::NotReported,
                codex_capabilities(),
            )
        } else {
            match self.catalog_connection().await {
                Ok(rpc) => {
                    let version = self.catalog_version.lock().await.clone();
                    let reported = match &version {
                        forge_provider_api::VersionObservation::Known { version } => {
                            Some(version.as_str())
                        }
                        forge_provider_api::VersionObservation::NotReported
                        | forge_provider_api::VersionObservation::Failed { .. } => None,
                    };
                    match rpc
                        .request(
                            "getAuthStatus",
                            json!({"includeToken":false,"refreshToken":false}),
                            ProviderOperation::Inspect,
                        )
                        .await
                    {
                        Ok(status) => (
                            forge_provider_api::RuntimeAvailability::Available,
                            version.clone(),
                            negotiated_capabilities(reported, Some(&status)),
                        ),
                        Err(error) => {
                            let (runtime, observed) = inspection_failure(&error);
                            (runtime, observed, negotiated_capabilities(reported, None))
                        }
                    }
                }
                Err(error) => {
                    let (runtime, version) = inspection_failure(&error);
                    (runtime, version, codex_capabilities())
                }
            }
        };
        Ok(forge_provider_api::ProviderSetup {
            runtime,
            origin: forge_provider_api::RuntimeOrigin::Configured,
            versions: forge_provider_api::RuntimeVersions {
                provider: version,
                adapter: forge_provider_api::VersionObservation::NotReported,
            },
            authentication: Vec::new(),
            capabilities,
        })
    }

    /// Revoke all binding callback admission before collecting jobs, exclusive runtimes, and shared discovery.
    async fn shutdown_native(&self) -> Result<(), ProviderError> {
        self.closed.store(true, Ordering::Release);
        self.records.registry.close_admission();
        super::operations::mark_changed();
        let _shutdown = self.shutdown_lock.lock().await;
        let mut errors = Vec::new();
        if let Err(error) = self.collect_fork_targets().await {
            errors.push(error);
        }
        let mut catalog = self.catalog.lock().await;
        let catalog_result = match catalog.as_mut() {
            Some(CatalogConnection::Starting(task)) => Some(task.await.unwrap_or_else(|_| {
                Err(failure(
                    ProviderOperation::Shutdown,
                    ProviderFailureKind::Runtime,
                ))
            })),
            Some(CatalogConnection::Ready(rpc)) => Some(Ok(rpc.clone())),
            None => None,
        };
        if let Some(Ok(rpc)) = &catalog_result {
            *catalog = Some(CatalogConnection::Ready(rpc.clone()));
        }
        drop(catalog);
        let bindings = self
            .bindings
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        if let Some(Err(error)) = catalog_result {
            errors.push(error);
        }
        for binding in bindings {
            if let Err(error) = self.close_binding(&binding).await {
                errors.push(error);
            }
        }
        let operations = self.operations.lock().await.clone();
        for operation in operations {
            if let Err(error) = operation.join().await {
                errors.push(error);
            }
        }
        // A pending open can complete after the first binding inventory.
        let bindings = self
            .bindings
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for binding in bindings {
            if let Err(error) = self.close_binding(&binding).await {
                errors.push(error);
            }
        }
        if let Err(error) = self.collect_pending(None).await {
            errors.push(error);
        }
        let operation = super::operations::current()
            .expect("shutdown retains its context")
            .context
            .id()
            .clone();
        if let Err(error) = self.records.collect_except(&operation).await {
            errors.push(error);
        }
        if let Err(error) = self.collect_fork_targets().await {
            errors.push(error);
        }
        // Admitted opens may finish while the first inventories are being collected.
        let bindings = self
            .bindings
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for binding in bindings {
            if let Err(error) = self.close_binding(&binding).await {
                errors.push(error);
            }
        }
        if let Err(error) = self.collect_pending(None).await {
            errors.push(error);
        }
        let mut discovery = self.discovery.lock().await;
        let mut retained = Vec::new();
        for rpc in discovery.drain(..) {
            if let Err(error) = rpc.close().await {
                errors.push(error);
                retained.push(rpc);
            }
        }
        *discovery = retained;
        if discovery.is_empty() {
            self.catalog.lock().await.take();
        }
        drop(discovery);
        if self.runtime.shutdown().await.is_err() {
            errors.push(failure(
                ProviderOperation::Shutdown,
                ProviderFailureKind::Transport,
            ));
        } else {
            self.fork_targets
                .lock()
                .await
                .retain(|_, target| target.source_binding.is_some());
        }
        if errors.is_empty() {
            return Ok(());
        }
        let error = combine_errors(errors).expect_err("nonempty shutdown failures were retained");
        let mut cause = self.shutdown_failure.lock().await;
        if cause.is_none() {
            *cause = Some(error);
        }
        Err(cause
            .clone()
            .expect("shutdown retains its first cleanup failure"))
    }

    /// Read model catalog state without configuring or opening a binding.
    async fn read_models(
        &self,
        context: QueryContext,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        let (rpc, _, _) = self.query_connection(context).await?;
        let catalog = rpc
            .request("model/list", json!({}), ProviderOperation::ModelList)
            .await?;
        Ok(crate::codex::catalog::parse_model_catalog(&catalog))
    }

    /// Read native skill definitions while preserving the existing reader and resource choices.
    async fn read_skills(
        &self,
        context: QueryContext,
    ) -> Result<Vec<forge_provider_api::catalog::SkillDefinition>, ProviderError> {
        let (rpc, workspace, _) = self.query_connection(context).await?;
        let catalog = rpc
            .request(
                "skills/list",
                json!({"cwd":workspace,"forceReload":false}),
                ProviderOperation::SkillList,
            )
            .await?;
        Ok(crate::codex::catalog::parse_skill_catalog(&catalog))
    }

    /// Read configured definitions or observe readiness without applying resource configuration.
    async fn read_mcp(
        &self,
        query: McpQuery,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let (observed, context) = match query {
            McpQuery::Configured(context) => (false, context),
            McpQuery::Observed(context) => (true, context),
        };
        let (rpc, workspace, session) = self.query_connection(context).await?;
        let config = rpc
            .request(
                "config/read",
                json!({"cwd":workspace,"includeLayers":false}),
                ProviderOperation::McpList,
            )
            .await?;
        let mut rows = Vec::new();
        if observed {
            let mut cursor = None::<String>;
            let mut seen = std::collections::HashSet::new();
            loop {
                let mut params = json!({"detail":"toolsAndAuthOnly","cursor":cursor});
                if let Some(session) = &session {
                    params["threadId"] = json!(session.history_id().as_str());
                }
                let page = rpc
                    .request("mcpServerStatus/list", params, ProviderOperation::McpList)
                    .await?;
                rows.extend(
                    page.get("data")
                        .and_then(Value::as_array)
                        .cloned()
                        .ok_or_else(|| {
                            failure(ProviderOperation::McpList, ProviderFailureKind::Protocol)
                        })?,
                );
                let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
                    break;
                };
                if !seen.insert(next.to_owned()) {
                    return Err(failure(
                        ProviderOperation::McpList,
                        ProviderFailureKind::Protocol,
                    ));
                }
                cursor = Some(next.to_owned());
            }
        }
        let parsed = crate::codex::catalog::parse_mcp_catalog(&json!({"data":rows}), &config);
        match session {
            Some(session) => Ok(session.resources.lock().await.normalize_mcp(parsed)),
            None => Ok(parsed),
        }
    }

    /// Observe the exact binding's shells under a retained read lease and bounded operation context.
    async fn read_terminals(
        &self,
        binding: &BindingId,
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
        let session = self.binding(binding).await?;
        read_binding_terminals(&session).await
    }

    /// Collect only an interrupted open's exclusive native resources and late installed binding.
    async fn collect_open_operation(
        &self,
        operation: &forge_provider_api::OperationId,
    ) -> Result<(), ProviderError> {
        let mut errors = Vec::new();
        if let Err(error) = self.collect_pending(Some(operation)).await {
            errors.push(error);
        }
        let bindings = self
            .bindings
            .lock()
            .await
            .values()
            .filter(|binding| &binding.opened_by == operation)
            .map(|binding| binding.binding.id.clone())
            .collect::<Vec<_>>();
        for binding in bindings {
            if let Err(error) = self.close_binding(&binding).await {
                errors.push(error);
            }
        }
        combine_errors(errors)
    }

    /// Retain cleanup after interruption and classify the actual predispatch or changed native boundary.
    fn operation_interruption(
        self,
        native: Arc<super::operations::NativeContext>,
        target: CleanupTarget,
        reason: forge_provider_api::OperationInterruption,
    ) -> forge_provider_api::retained_operation::Interruption<
        impl std::future::Future<Output = Result<(), ProviderError>> + Send + 'static,
    > {
        let changed = native.changed();
        let (phase, disposition) = if !changed {
            (
                forge_provider_api::ProviderPhase::Admission,
                forge_provider_api::FailureDisposition::Unchanged,
            )
        } else {
            match native.operation {
                ProviderOperation::InputSubmit
                | ProviderOperation::InputSteer
                | ProviderOperation::SessionFork
                | ProviderOperation::TerminalTerminate => (
                    forge_provider_api::ProviderPhase::Dispatch,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
                ProviderOperation::Cancel => (
                    forge_provider_api::ProviderPhase::Collection,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
                ProviderOperation::SessionClose | ProviderOperation::Shutdown => (
                    forge_provider_api::ProviderPhase::Close,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
                ProviderOperation::SessionOpen
                | ProviderOperation::SessionConfigure
                | ProviderOperation::InputRetract => (
                    forge_provider_api::ProviderPhase::Collection,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
                ProviderOperation::SessionCompact => (
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
                ProviderOperation::Inspect
                | ProviderOperation::Authenticate
                | ProviderOperation::InputReconcile
                | ProviderOperation::ModelList
                | ProviderOperation::SkillList
                | ProviderOperation::McpList
                | ProviderOperation::TerminalList => (
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Unchanged,
                ),
            }
        };
        let observation = reason.failure(native.operation, phase, disposition);
        let provider = self;
        forge_provider_api::retained_operation::Interruption::new(observation, async move {
            super::operations::cleanup(async move {
                match target {
                    CleanupTarget::None => Ok(()),
                    CleanupTarget::Open(operation) => {
                        provider.collect_open_operation(&operation).await
                    }
                    CleanupTarget::Binding {
                        binding,
                        invocation,
                    } if changed => {
                        let session = match provider.binding(&binding).await {
                            Ok(session) => session,
                            Err(_) if provider.closed_bindings.lock().await.contains(&binding) => {
                                return Ok(());
                            }
                            Err(error) => return Err(error),
                        };
                        if let Some(scope) = invocation {
                            let successor = session
                                .active
                                .lock()
                                .await
                                .as_ref()
                                .is_some_and(|execution| execution.scope != scope);
                            if successor
                                || session
                                    .settlements
                                    .lock()
                                    .await
                                    .get(&scope)
                                    .is_some_and(Result::is_ok)
                            {
                                return Ok(());
                            }
                        }
                        session.fenced.store(true, Ordering::Release);
                        provider.close_binding(&binding).await
                    }
                    CleanupTarget::Binding { .. } => Ok(()),
                }
            })
            .await
        })
    }

    /// Interrupt only the named invocation, sharing retained completion with automatic collection.
    async fn cancel_invocation(
        &self,
        binding: &BindingId,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<forge_provider_api::InvocationSettlement, ProviderError> {
        self.pending_inputs.cancel(binding, scope).await;
        let session = self.binding(binding).await?;
        let admitting = session
            .admitting
            .lock()
            .await
            .as_ref()
            .filter(|admitting| &admitting.scope == scope)
            .cloned();
        if let Some(admitting) = admitting {
            admitting.cancel.store(true, Ordering::Release);
            let mut done = admitting.done.subscribe();
            while !*done.borrow() {
                done.changed().await.map_err(|_| {
                    failure(ProviderOperation::Cancel, ProviderFailureKind::Transport)
                })?;
            }
        }
        if let Some(settlement) = session.settlements.lock().await.get(scope).cloned() {
            return settlement;
        }
        let task = {
            let _admission = session.admission.lock().await;
            let active = session.active.lock().await;
            let Some(execution) = active
                .as_ref()
                .filter(|execution| &execution.scope == scope)
            else {
                drop(active);
                if let Some(settlement) = session.settlements.lock().await.get(scope).cloned() {
                    return settlement;
                }
                return Err(failure(
                    ProviderOperation::Cancel,
                    ProviderFailureKind::WrongInvocation,
                ));
            };
            let task = execution
                .task
                .clone()
                .ok_or_else(|| failure(ProviderOperation::Cancel, ProviderFailureKind::Protocol))?;
            if !task.is_finished() && execution.turn_updates.borrow().is_some() {
                execution.cancel.send(()).map_err(|_| {
                    failure(ProviderOperation::Cancel, ProviderFailureKind::Transport)
                })?;
            }
            task
        };
        collect_invocation(session.clone(), scope.clone(), task).await?;
        session
            .settlements
            .lock()
            .await
            .get(scope)
            .cloned()
            .ok_or_else(|| failure(ProviderOperation::Cancel, ProviderFailureKind::Protocol))?
    }

    /// Revoke authority before collecting every retained native binding resource.
    async fn close_binding(&self, binding: &BindingId) -> Result<(), ProviderError> {
        if self.closed_bindings.lock().await.contains(binding) {
            return Ok(());
        }
        let session = self.binding(binding).await?;
        session.closing.store(true, Ordering::Release);
        session.fenced.store(true, Ordering::Release);
        self.pending_inputs.cancel_binding(binding).await;
        let _close = session.close_lock.lock().await;
        let mut errors = Vec::new();
        if let Some(observer) = session.terminal_observer.lock().await.take() {
            observer.abort();
            if let Err(error) = observer.await
                && !error.is_cancelled()
            {
                errors.push(failure(
                    ProviderOperation::SessionClose,
                    ProviderFailureKind::Runtime,
                ));
            }
        }

        let pending_scope = session
            .admitting
            .lock()
            .await
            .as_ref()
            .map(|admitting| admitting.scope.clone());
        let scope = match pending_scope {
            Some(scope) => Some(scope),
            None => session
                .active
                .lock()
                .await
                .as_ref()
                .map(|execution| execution.scope.clone()),
        };
        if let Some(scope) = scope
            && let Err(error) = self.cancel_invocation(binding, &scope).await
            && !matches!(&error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::WrongInvocation && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
        {
            errors.push(error);
        }
        let _admission = session.admission.lock().await;
        if let Err(error) = stop_background_reader(&session).await {
            errors.push(error);
        }
        if let Err(error) = collect_retired(&session).await {
            errors.push(error);
        }
        if let Err(error) = session.rpc.lock().await.close().await {
            errors.push(error);
        }
        *session.observations.write().await = true;
        if session.runtime.lock().await.shutdown().await.is_err() {
            errors.push(failure(
                ProviderOperation::Shutdown,
                ProviderFailureKind::Transport,
            ));
        }
        let worker = session
            .active
            .lock()
            .await
            .as_ref()
            .and_then(|execution| execution.task.clone());
        if let Some(worker) = worker
            && let Err(error) = worker.join().await
        {
            errors.push(error);
        }
        {
            let mut profile = session.profile.lock().await;
            if let Some(profile) = profile.as_mut()
                && let Err(error) = profile.close()
            {
                errors.push(error.into_provider_error(ProviderOperation::SessionClose));
            }
        }
        if let Err(error) = combine_errors(errors) {
            let mut cause = session.close_failure.lock().await;
            if cause.is_none() {
                *cause = Some(error.clone());
            }
            return Err(cause.clone().expect("failed close retains its first cause"));
        }
        session.sink.fail(events::EventDeliveryError::Closed);
        self.closed_bindings.lock().await.insert(binding.clone());
        self.bindings.lock().await.remove(binding);
        self.fork_targets
            .lock()
            .await
            .retain(|_, target| target.source_binding.as_ref() != Some(binding));
        let mut owners = self.owners.lock().await;
        if owners.get(&session.owner) == Some(binding) {
            owners.remove(&session.owner);
        }
        Ok(())
    }

    /// Select a read-only native connection without opening a live history binding.
    async fn query_connection(
        &self,
        context: QueryContext,
    ) -> Result<(Arc<NativeRpc>, String, Option<Arc<CodexBinding>>), ProviderError> {
        match context {
            QueryContext::Workspace(context) => {
                Ok((self.catalog_connection().await?, context.workspace, None))
            }
            QueryContext::Binding(id) => {
                let session = self.binding(&id).await?;
                if session.is_fenced() {
                    return Err(failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::SessionMissing,
                    ));
                }
                let rpc = session.rpc.lock().await.shared();
                Ok((rpc, session.workspace.clone(), Some(session)))
            }
        }
    }

    /// Collect native resources retained by an unsuccessful open before forgetting their identity.
    async fn collect_pending(
        &self,
        operation: Option<&forge_provider_api::OperationId>,
    ) -> Result<(), ProviderError> {
        let pending = self
            .pending
            .lock()
            .await
            .iter()
            .filter(|(_, pending)| {
                operation.is_none_or(|operation| &pending.operation_id == operation)
            })
            .map(|(id, pending)| (id.clone(), pending.clone()))
            .collect::<Vec<_>>();
        let mut errors = Vec::new();
        for (id, pending) in pending {
            let mut failed = false;
            if let Some(rpc) = pending.rpc.lock().await.as_ref()
                && let Err(error) = rpc.close().await
            {
                errors.push(error);
                failed = true;
            }
            if pending.runtime.shutdown().await.is_err() {
                errors.push(failure(
                    ProviderOperation::Shutdown,
                    ProviderFailureKind::Transport,
                ));
                failed = true;
            }
            let mut profile = pending.profile.lock().await;
            if let Some(profile) = profile.as_mut()
                && let Err(error) = profile.close()
            {
                errors.push(error.into_provider_error(ProviderOperation::SessionClose));
                failed = true;
            }
            if !failed {
                self.pending.lock().await.remove(&id);
            }
        }
        let preparing = self
            .preparing
            .lock()
            .await
            .iter()
            .filter(|(id, _)| operation.is_none_or(|operation| operation == *id))
            .map(|(id, profile)| (id.clone(), profile.clone()))
            .collect::<Vec<_>>();
        for (id, prepared) in preparing {
            let mut profile = prepared.profile.lock().await;
            match profile
                .as_mut()
                .map(crate::profile::ResourceProfile::close)
                .transpose()
            {
                Ok(_) => {
                    profile.take();
                    self.preparing.lock().await.remove(&id);
                }
                Err(error) => {
                    errors.push(error.into_provider_error(ProviderOperation::SessionClose))
                }
            }
        }
        combine_errors(errors)
    }
}

#[async_trait]
impl Provider for CodexProvider {
    async fn session_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::SessionOpen);
        let payload = request.identity();
        let target = CleanupTarget::Open(context.id().clone());
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .open
            .take(
                context,
                ProviderOperation::SessionOpen,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .open_session(request)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::SessionOpen))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn session_configure(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: forge_provider_api::SessionConfiguration,
    ) -> Result<forge_provider_api::ConfiguredSession, ProviderError> {
        let native = super::operations::NativeContext::new(
            context.clone(),
            ProviderOperation::SessionConfigure,
        );
        let payload = (binding.clone(), configuration.clone());
        let target = CleanupTarget::Binding {
            binding: binding.clone(),
            invocation: None,
        };
        let binding = binding.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .configure
            .observe(
                context,
                ProviderOperation::SessionConfigure,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .configure_binding(&binding, configuration)
                            .await
                            .map_err(|error| {
                                operation_error(error, ProviderOperation::SessionConfigure)
                            })
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn session_fork(
        &self,
        context: OperationContext,
        request: forge_provider_api::SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::SessionFork);
        let payload = request.clone();
        let target = CleanupTarget::None;
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .fork
            .observe(
                context,
                ProviderOperation::SessionFork,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .fork_history(request)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::SessionFork))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn session_compact(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        let native = super::operations::NativeContext::new(
            context.clone(),
            ProviderOperation::SessionCompact,
        );
        let payload = (binding.clone(), request.clone());
        let target = CleanupTarget::Binding {
            binding: binding.clone(),
            invocation: None,
        };
        let binding = binding.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .compact
            .observe(
                context,
                ProviderOperation::SessionCompact,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .compact_binding(&binding, request)
                            .await
                            .map_err(|error| {
                                operation_error(error, ProviderOperation::SessionCompact)
                            })
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn session_close(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::SessionClose);
        let payload = binding.clone();
        let target = CleanupTarget::None;
        let binding = binding.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .close
            .observe(
                context,
                ProviderOperation::SessionClose,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::mark_changed();
                        super::operations::check()?;
                        super::operations::cleanup(provider.close_binding(&binding))
                            .await
                            .map_err(|error| {
                                operation_error(error, ProviderOperation::SessionClose)
                            })
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn cancel(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CancelRequest,
    ) -> Result<forge_provider_api::InvocationSettlement, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::Cancel);
        let payload = (binding.clone(), request.clone());
        let target = CleanupTarget::None;
        let binding = binding.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .cancel
            .observe(
                context,
                ProviderOperation::Cancel,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::mark_changed();
                        super::operations::check()?;
                        super::operations::cleanup(
                            provider.cancel_invocation(&binding, &request.scope),
                        )
                        .await
                        .map_err(|error| operation_error(error, ProviderOperation::Cancel))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn input_submit(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        let pending = self
            .pending_inputs
            .register(binding, &request.scope, context.id());
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::InputSubmit);
        let payload = (binding.clone(), request.identity());
        let target = CleanupTarget::Binding {
            binding: binding.clone(),
            invocation: Some(request.scope.clone()),
        };
        let binding = binding.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .submit
            .observe(
                context,
                ProviderOperation::InputSubmit,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .submit_input(&binding, request, pending)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::InputSubmit))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn input_steer(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::InputSteer);
        let payload = (binding.clone(), request.identity());
        let target = CleanupTarget::Binding {
            binding: binding.clone(),
            invocation: Some(request.scope.clone()),
        };
        let binding = binding.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .steer
            .observe(
                context,
                ProviderOperation::InputSteer,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .steer_input(&binding, request)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::InputSteer))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn input_reconcile(
        &self,
        context: OperationContext,
        request: forge_provider_api::InputReconcileRequest,
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        let native = super::operations::NativeContext::new(
            context.clone(),
            ProviderOperation::InputReconcile,
        );
        let payload = request.clone();
        let target = CleanupTarget::None;
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .reconcile
            .observe(
                context,
                ProviderOperation::InputReconcile,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider.reconcile_input(request).await.map_err(|error| {
                            operation_error(error, ProviderOperation::InputReconcile)
                        })
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn input_retract(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::InputRetractRequest,
    ) -> Result<forge_provider_api::InputRetractOutcome, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::InputRetract);
        let payload = (binding.clone(), request.clone());
        let target = CleanupTarget::Binding {
            binding: binding.clone(),
            invocation: None,
        };
        let binding = binding.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .retract
            .observe(
                context,
                ProviderOperation::InputRetract,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .retract_input(&binding, request)
                            .await
                            .map_err(|error| {
                                operation_error(error, ProviderOperation::InputRetract)
                            })
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn inspect(
        &self,
        context: OperationContext,
        workspace: WorkspaceContext,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::Inspect);
        let payload = workspace.clone();
        let target = CleanupTarget::None;
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .inspect
            .observe(
                context,
                ProviderOperation::Inspect,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .inspect_runtime(workspace)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::Inspect))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn authenticate(
        &self,
        context: OperationContext,
        workspace: WorkspaceContext,
        method: forge_provider_api::AuthenticationMethodId,
    ) -> Result<(), ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::Authenticate);
        let payload = (workspace.clone(), method.clone());
        let target = CleanupTarget::None;
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .authenticate
            .observe(
                context,
                ProviderOperation::Authenticate,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        Err(forge_provider_api::capability::UnsupportedCapability(
                            forge_provider_api::capability::ProviderFeature::Authentication,
                        )
                        .into())
                        .map_err(|error| operation_error(error, ProviderOperation::Authenticate))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn shutdown(&self, context: OperationContext) -> Result<(), ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::Shutdown);
        let payload = ();
        let target = CleanupTarget::None;
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .shutdown
            .observe(
                context,
                ProviderOperation::Shutdown,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .shutdown_native()
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::Shutdown))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn model_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::ModelList);
        let payload = query.clone();
        let target = CleanupTarget::None;
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .models
            .observe(
                context,
                ProviderOperation::ModelList,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .read_models(query)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::ModelList))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn skill_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<forge_provider_api::catalog::SkillDefinition>, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::SkillList);
        let payload = query.clone();
        let target = CleanupTarget::None;
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .skills
            .observe(
                context,
                ProviderOperation::SkillList,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .read_skills(query)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::SkillList))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn mcp_list(
        &self,
        context: OperationContext,
        query: McpQuery,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::McpList);
        let payload = query.clone();
        let target = CleanupTarget::None;
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .mcp
            .observe(
                context,
                ProviderOperation::McpList,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .read_mcp(query)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::McpList))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn terminal_list(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::TerminalList);
        let payload = binding.clone();
        let target = CleanupTarget::None;
        let binding = binding.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .terminals
            .observe(
                context,
                ProviderOperation::TerminalList,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider.read_terminals(&binding).await.map_err(|error| {
                            operation_error(error, ProviderOperation::TerminalList)
                        })
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn terminal_terminate(
        &self,
        context: OperationContext,
        binding: &BindingId,
        terminal: &forge_provider_api::terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        let native = super::operations::NativeContext::new(
            context.clone(),
            ProviderOperation::TerminalTerminate,
        );
        let payload = (binding.clone(), terminal.clone());
        let target = CleanupTarget::None;
        let binding = binding.clone();
        let terminal = terminal.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .terminate
            .observe(
                context,
                ProviderOperation::TerminalTerminate,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .terminate_binding_terminal(&binding, &terminal)
                            .await
                            .map_err(|error| {
                                operation_error(error, ProviderOperation::TerminalTerminate)
                            })
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }
}

struct ActiveExecution {
    scope: forge_provider_api::InvocationScope,
    turn_id: String,
    turn_updates: watch::Sender<Option<String>>,
    submissions: Arc<Mutex<Vec<forge_provider_api::SubmissionId>>>,
    request: PromptRequest,
    cancel: mpsc::UnboundedSender<()>,
    task: Option<Arc<crate::retained_task::RetainedTask>>,
}

struct AdmittingInput {
    scope: forge_provider_api::InvocationScope,
    operation_id: String,
    native_may_exist: AtomicBool,
    cancel: AtomicBool,
    done: watch::Sender<bool>,
}

struct RollbackAuthorization {
    thread_id: String,
    scope: forge_provider_api::InvocationScope,
    turn_id: forge_provider_api::ProviderTurnId,
    submission_id: forge_provider_api::SubmissionId,
    attempt_id: forge_provider_api::DeliveryAttemptId,
}

struct BackgroundReader {
    cancel: mpsc::UnboundedSender<()>,
    task: crate::retained_task::RetainedTask,
}

struct CodexBinding {
    binding: SessionBinding,
    history: std::sync::Mutex<NativeHistory>,
    native_tools: Vec<forge_provider_api::control::ControlToolDefinition>,
    opened_by: forge_provider_api::OperationId,
    owner: forge_provider_api::ExecutionOwner,
    workspace: String,
    model: Mutex<ModelSelection>,
    runtime: Mutex<Arc<CodexRuntime>>,
    rpc: RpcHandle,
    sink: ProviderEventSink,
    stream: Mutex<Option<ProviderEventStream>>,
    normalization: Mutex<CodexNormalization>,
    mcp_context: Mutex<super::protocol::McpContext>,
    resources: Mutex<CodexResources>,
    profile: Mutex<Option<crate::profile::ResourceProfile>>,
    _lease: Option<Arc<dyn forge_provider_api::session::ResourceLease>>,
    system_instructions: String,
    access: Mutex<forge_provider_api::access::AccessPolicy>,
    approval: Mutex<forge_provider_api::approval::ApprovalMode>,
    approval_context: super::approval::ApprovalContext,
    active: Mutex<Option<ActiveExecution>>,
    admitting: Mutex<Option<Arc<AdmittingInput>>>,
    background_reader: Mutex<Option<BackgroundReader>>,
    terminal_observer: Mutex<Option<tokio::task::JoinHandle<()>>>,
    rollback: Mutex<Option<RollbackAuthorization>>,
    admission: Mutex<()>,
    observations: Arc<tokio::sync::RwLock<bool>>,
    fenced: AtomicBool,
    invalidated: AtomicBool,
    closing: AtomicBool,
    provider_closed: Arc<AtomicBool>,
    isolated: bool,
    isolation_auth: Option<Arc<super::isolation::IsolationAuth>>,
    configuration: Mutex<forge_provider_api::SessionConfiguration>,
    capabilities: forge_provider_api::capability::CapabilityManifest,
    settlements: Mutex<
        HashMap<
            forge_provider_api::InvocationScope,
            Result<forge_provider_api::InvocationSettlement, ProviderError>,
        >,
    >,
    outcomes: Mutex<HashMap<forge_provider_api::InvocationScope, ExecutionResult>>,
    close_failure: Mutex<Option<ProviderError>>,
    close_lock: Mutex<()>,
    settlement_lock: Mutex<()>,
    retired_rpcs: Mutex<Vec<Arc<NativeRpc>>>,
    retired_profiles: Mutex<Vec<crate::profile::ResourceProfile>>,
}

impl CodexBinding {
    /// Read current native history independently of the stable public binding generation.
    fn history_id(&self) -> forge_provider_api::ProviderSessionId {
        self.history
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .id()
            .clone()
    }
    /// Preserve the exact reason new input cannot use a revoked generation.
    fn check_input_admission(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        let kind = if self.provider_closed.load(Ordering::Acquire)
            || self.closing.load(Ordering::Acquire)
        {
            Some(ProviderFailureKind::BindingClosed)
        } else if self.invalidated.load(Ordering::Acquire) || self.fenced.load(Ordering::Acquire) {
            Some(ProviderFailureKind::BindingInvalidated)
        } else {
            None
        };
        match kind {
            Some(kind) => Err(ProviderFailure::new(operation, kind)
                .at_phase(
                    forge_provider_api::ProviderPhase::Admission,
                    forge_provider_api::FailureDisposition::Invalidated,
                )
                .into()),
            None => Ok(()),
        }
    }
    /// Reject admission after permanent close revocation or unresolved native mutation.
    fn is_fenced(&self) -> bool {
        self.provider_closed.load(Ordering::Acquire)
            || self.closing.load(Ordering::Acquire)
            || self.invalidated.load(Ordering::Acquire)
            || self.fenced.load(Ordering::Acquire)
    }
}

/// Only a new thread with no possibly dispatched input may replace its unpersisted native identity.
#[derive(Clone)]
enum NativeHistory {
    /// No input write may have occurred and this binding did not resume existing history.
    Virgin(forge_provider_api::ProviderSessionId),
    /// Existing history was resumed or input may have been attempted; acknowledgement or persistence is not implied.
    Established(forge_provider_api::ProviderSessionId),
}

impl NativeHistory {
    /// Get the exact native identity without inferring whether history was saved.
    fn id(&self) -> &forge_provider_api::ProviderSessionId {
        match self {
            Self::Virgin(id) | Self::Established(id) => id,
        }
    }
    /// Irreversibly forbid virgin replacement once an input write may have been attempted, even without acknowledgement.
    fn establish(&mut self) {
        if let Self::Virgin(id) = self {
            *self = Self::Established(id.clone());
        }
    }
}

enum CleanupTarget {
    None,
    Open(forge_provider_api::OperationId),
    Binding {
        binding: BindingId,
        invocation: Option<forge_provider_api::InvocationScope>,
    },
}

struct PendingOpen {
    operation_id: forge_provider_api::OperationId,
    history: Mutex<Option<forge_provider_api::ProviderSessionId>>,
    runtime: Arc<CodexRuntime>,
    rpc: Mutex<Option<Arc<NativeRpc>>>,
    profile: Mutex<Option<crate::profile::ResourceProfile>>,
}

/// Retain private profile cleanup before runtime selection or fallible native discovery begins.
struct PreparedProfile {
    profile: Mutex<Option<crate::profile::ResourceProfile>>,
}

/// Retain the exact native fork writer until positive collection or its source runtime's successful close.
struct PendingFork {
    operation: forge_provider_api::OperationId,
    target: forge_provider_api::ProviderSessionId,
    rpc: Arc<NativeRpc>,
    source_binding: Option<BindingId>,
    cleanup: Mutex<()>,
}

enum CatalogConnection {
    Starting(tokio::task::JoinHandle<Result<Arc<NativeRpc>, ProviderError>>),
    Ready(Arc<NativeRpc>),
}

/// Preserve normalized version and runtime availability when discovery cannot open its native connection.
fn inspection_failure(
    error: &ProviderError,
) -> (
    forge_provider_api::RuntimeAvailability,
    forge_provider_api::VersionObservation,
) {
    use forge_provider_api::version::{RuntimeProbeFailure, RuntimeVersionError};
    use forge_provider_api::{RuntimeAvailability as Availability, VersionObservation as Version};
    match error {
        ProviderError::RuntimeVersion(RuntimeVersionError::Unsupported { detected, .. }) => (
            Availability::Incompatible,
            Version::Known {
                version: detected.to_string(),
            },
        ),
        ProviderError::RuntimeVersion(RuntimeVersionError::Missing { .. }) => {
            (Availability::Incompatible, Version::NotReported)
        }
        ProviderError::RuntimeVersion(RuntimeVersionError::Malformed { .. }) => (
            Availability::Incompatible,
            Version::Failed {
                failure: ProviderFailureKind::Protocol,
            },
        ),
        ProviderError::RuntimeVersion(RuntimeVersionError::Probe { failure, .. }) => (
            Availability::Unavailable,
            Version::Failed {
                failure: match failure {
                    RuntimeProbeFailure::PermissionDenied => ProviderFailureKind::Authorization,
                    RuntimeProbeFailure::Unavailable => ProviderFailureKind::Transport,
                    RuntimeProbeFailure::TimedOut { .. } => ProviderFailureKind::Timeout,
                },
            },
        ),
        ProviderError::Combined { primary, .. } => inspection_failure(primary),
        ProviderError::Failure(failure) => (
            Availability::Unavailable,
            Version::Failed {
                failure: failure.kind(),
            },
        ),
        _ => (
            Availability::Unavailable,
            Version::Failed {
                failure: ProviderFailureKind::Unknown,
            },
        ),
    }
}

/// Retain the first typed destructive-mutation failure and revoke every new binding admission.
async fn invalidate_retraction(session: &CodexBinding, error: ProviderError) {
    session.invalidated.store(true, Ordering::Release);
    session.fenced.store(true, Ordering::Release);
    session.sink.fail(events::EventDeliveryError::Observation(
        forge_provider_api::ObservationFailure::LifecycleUnavailable,
    ));
    let mut retained = session.close_failure.lock().await;
    if retained.is_none() {
        *retained = Some(operation_error(error, ProviderOperation::InputRetract));
    }
}

/// Preserve the live observer and unchanged binding disposition after read-only configuration preflight fails.
fn configuration_preflight_error(error: ProviderError) -> ProviderError {
    match error {
        ProviderError::Failure(cause) => {
            ProviderFailure::new(ProviderOperation::SessionConfigure, cause.kind())
                .at_phase(
                    forge_provider_api::error::ProviderPhase::Admission,
                    forge_provider_api::error::FailureDisposition::Unchanged,
                )
                .into()
        }
        ProviderError::Combined { primary, secondary } => ProviderError::Combined {
            primary: Box::new(configuration_preflight_error(*primary)),
            secondary: secondary
                .into_iter()
                .map(configuration_preflight_error)
                .collect(),
        },
        other => other,
    }
}

/// Revoke authority after replacement or collection fails beyond the observer handoff boundary.
fn invalidate_configuration(
    session: &CodexBinding,
    error: ProviderError,
    fallback_phase: forge_provider_api::ProviderPhase,
) -> ProviderError {
    session.invalidated.store(true, Ordering::Release);
    session.fenced.store(true, Ordering::Release);
    // Replacement failure revokes lifecycle observation, even when the native transport
    // remains connected and the cause is rejection, authentication, or version validation.
    session.sink.fail(events::EventDeliveryError::Observation(
        forge_provider_api::ObservationFailure::LifecycleUnavailable,
    ));
    match error {
        ProviderError::Failure(cause) => {
            let phase = match cause.phase() {
                forge_provider_api::ProviderPhase::Admission => fallback_phase,
                phase => phase,
            };
            cause
                .for_operation(ProviderOperation::SessionConfigure)
                .at_phase(phase, forge_provider_api::FailureDisposition::Invalidated)
                .into()
        }
        ProviderError::Combined { primary, secondary } => ProviderError::Combined {
            primary: Box::new(invalidate_configuration(session, *primary, fallback_phase)),
            secondary,
        },
        specialized => ProviderError::Combined {
            primary: Box::new(configuration_failure(&specialized, fallback_phase).into()),
            secondary: vec![specialized],
        },
    }
}

/// Normalize specialized safe evidence without losing its separately retained typed detail.
fn configuration_failure(
    error: &ProviderError,
    fallback_phase: forge_provider_api::ProviderPhase,
) -> ProviderFailure {
    use forge_provider_api::version::{RuntimeProbeFailure, RuntimeVersionError};
    use forge_provider_api::{ObservationFailure as Observation, ProviderPhase as Phase};
    let (kind, phase) = match error {
        ProviderError::Failure(cause) => (
            cause.kind(),
            match cause.phase() {
                Phase::Admission => fallback_phase,
                phase => phase,
            },
        ),
        ProviderError::Combined { primary, .. } | ProviderError::PreparationFailed(primary) => {
            return configuration_failure(primary, fallback_phase);
        }
        ProviderError::PreparationCleanup { cause, .. } => {
            return configuration_failure(cause, fallback_phase);
        }
        ProviderError::Cleanup(report) => {
            return match report.origin() {
                Some(origin) => configuration_failure(origin, fallback_phase),
                None => ProviderFailure::new(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::Runtime,
                )
                .at_phase(
                    Phase::Collection,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
            };
        }
        ProviderError::RuntimeVersion(version) => (
            match version {
                RuntimeVersionError::Missing { .. } | RuntimeVersionError::Unsupported { .. } => {
                    ProviderFailureKind::Initialization
                }
                RuntimeVersionError::Malformed { .. } => ProviderFailureKind::Protocol,
                RuntimeVersionError::Probe { failure, .. } => match failure {
                    RuntimeProbeFailure::PermissionDenied => ProviderFailureKind::Authorization,
                    RuntimeProbeFailure::Unavailable => ProviderFailureKind::Transport,
                    RuntimeProbeFailure::TimedOut { .. } => ProviderFailureKind::Timeout,
                },
            },
            Phase::Preparation,
        ),
        ProviderError::Observation(observation) => (
            match observation {
                Observation::LifecycleUnavailable => ProviderFailureKind::Runtime,
                Observation::Overflow | Observation::Disconnected => ProviderFailureKind::Transport,
                Observation::DeliveryUncertain => ProviderFailureKind::Unknown,
                Observation::Protocol => ProviderFailureKind::Protocol,
                Observation::OwnershipConflict => ProviderFailureKind::ResourceConflict,
                Observation::UnexpectedExecution => ProviderFailureKind::WrongInvocation,
            },
            Phase::Observation,
        ),
        ProviderError::Delivery(delivery) => (
            match delivery {
                events::EventDeliveryError::Observation(observation) => {
                    return configuration_failure(
                        &ProviderError::Observation(observation.clone()),
                        Phase::Observation,
                    );
                }
                events::EventDeliveryError::Closed => ProviderFailureKind::Transport,
                events::EventDeliveryError::OwnershipMismatch { .. } => {
                    ProviderFailureKind::WrongInvocation
                }
                events::EventDeliveryError::InvalidTransportData
                | events::EventDeliveryError::IncompleteTransfer { .. } => {
                    ProviderFailureKind::Protocol
                }
                events::EventDeliveryError::Empty => ProviderFailureKind::Runtime,
            },
            Phase::Observation,
        ),
        ProviderError::Contract(_) => (ProviderFailureKind::Protocol, fallback_phase),
        ProviderError::Unsupported(_) => (ProviderFailureKind::Policy, fallback_phase),
        ProviderError::NotConsumed => (ProviderFailureKind::Rejected, fallback_phase),
        ProviderError::DeliveryUncertain => (ProviderFailureKind::Unknown, Phase::Dispatch),
        ProviderError::Cancelled => (ProviderFailureKind::Cancelled, fallback_phase),
        ProviderError::Retryable(_) => (ProviderFailureKind::Runtime, fallback_phase),
    };
    ProviderFailure::new(ProviderOperation::SessionConfigure, kind)
        .at_phase(phase, forge_provider_api::FailureDisposition::Invalidated)
}

/// Revoke local authority before publishing fatal idle-observer loss and retain its first native cause.
async fn invalidate_background_observer(
    session: &CodexBinding,
    error: ProviderError,
    observation: forge_provider_api::ObservationFailure,
) -> ProviderError {
    session.invalidated.store(true, Ordering::Release);
    session.fenced.store(true, Ordering::Release);
    let mut cause = session.close_failure.lock().await;
    if cause.is_none() {
        *cause = Some(error.clone());
    }
    drop(cause);
    session
        .sink
        .fail(events::EventDeliveryError::Observation(observation));
    error
}

/// Join the exact native worker and publish collection evidence once after its terminal observations.
async fn collect_invocation(
    session: Arc<CodexBinding>,
    scope: forge_provider_api::InvocationScope,
    task: Arc<crate::retained_task::RetainedTask>,
) -> Result<(), ProviderError> {
    let result = task.join().await;
    let _settlement = session.settlement_lock.lock().await;
    if let Some(retained) = session.settlements.lock().await.get(&scope) {
        return retained.as_ref().map(|_| ()).map_err(Clone::clone);
    }
    let result = match result {
        Ok(()) => match session.outcomes.lock().await.get(&scope).cloned() {
            Some(result) => Ok(forge_provider_api::InvocationSettlement {
                binding_id: session.binding.id.clone(),
                scope: scope.clone(),
                result,
                watermark: session.sink.watermark(),
            }),
            None => Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Protocol,
            )),
        },
        Err(error) => Err(error),
    };
    let result = match result {
        Ok(settlement) => {
            let scoped = session.sink.invocation_sink(&scope)?;
            scoped
                .send_settlement(settlement)
                .await
                .map_err(ProviderError::from)
        }
        Err(error) => {
            session.fenced.store(true, Ordering::Release);
            let failure = match &error {
                ProviderError::Failure(failure) => *failure,
                _ => ProviderFailure::new(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Runtime,
                )
                .at_phase(
                    forge_provider_api::error::ProviderPhase::Collection,
                    forge_provider_api::error::FailureDisposition::Invalidated,
                ),
            };
            let _ = publish(
                &session.sink,
                &scope,
                ProviderEvent::SettlementFailed {
                    scope: scope.clone(),
                    failure,
                },
            )
            .await;
            Err(error)
        }
    };
    if result.is_err() {
        session.fenced.store(true, Ordering::Release);
    }
    session
        .settlements
        .lock()
        .await
        .insert(scope.clone(), result.clone());
    let mut active = session.active.lock().await;
    if active
        .as_ref()
        .is_some_and(|execution| execution.scope == scope)
    {
        active.take();
    }
    result.map(|_| ())
}

/// Collect replaced transports and profiles without dropping resources whose cleanup failed.
async fn collect_retired(session: &CodexBinding) -> Result<(), ProviderError> {
    let mut errors = Vec::new();
    let mut rpcs = session.retired_rpcs.lock().await;
    let mut retained_rpcs = Vec::new();
    for rpc in rpcs.drain(..) {
        if let Err(error) = rpc.close().await {
            errors.push(error);
            retained_rpcs.push(rpc);
        }
    }
    *rpcs = retained_rpcs;
    drop(rpcs);
    let mut profiles = session.retired_profiles.lock().await;
    let mut retained_profiles = Vec::new();
    for mut profile in profiles.drain(..) {
        if let Err(error) = profile.close() {
            errors.push(error.into_provider_error(ProviderOperation::SessionConfigure));
            retained_profiles.push(profile);
        }
    }
    *profiles = retained_profiles;
    combine_errors(errors)
}

/// Interrupt the native foreground and descendants while retaining independent terminal identities.
async fn interrupt_native(
    session: &CodexBinding,
    thread_id: &str,
    turn_id: Option<&forge_provider_api::ProviderTurnId>,
    descendants: &super::descendants::DescendantTracker,
) -> Result<(), ProviderError> {
    let mut errors = Vec::new();
    if let Some(turn_id) = turn_id
        && let Err(error) = session
            .rpc
            .lock()
            .await
            .request(
                "turn/interrupt",
                json!({"threadId":thread_id,"turnId":turn_id}),
                ProviderOperation::Cancel,
            )
            .await
    {
        errors.push(error);
    }
    for (thread_id, turn_id) in descendants.active_turns() {
        if let Some(turn_id) = turn_id
            && let Err(error) = session
                .rpc
                .lock()
                .await
                .request(
                    "turn/interrupt",
                    json!({"threadId":thread_id,"turnId":turn_id}),
                    ProviderOperation::Cancel,
                )
                .await
        {
            errors.push(error);
        }
    }
    combine_errors(errors)
}

/// Get effective Codex support after validating the minimum native protocol version.
fn codex_capabilities() -> forge_provider_api::capability::CapabilityManifest {
    let mut capabilities = forge_provider_api::capability::CapabilityManifest::from_descriptor(
        crate::registry::descriptor(
            &crate::registry::select("codex").expect("Codex is registered"),
        )
        .expect("registered Codex descriptor"),
    );
    capabilities.task_steering = vec![
        forge_provider_api::input::TaskKind::Goal,
        forge_provider_api::input::TaskKind::Plan,
        forge_provider_api::input::TaskKind::Execute,
    ];
    if let Some(recap) = capabilities
        .capability
        .iter_mut()
        .find(|capability| capability.id == forge_provider_api::capability::CapabilityId::Recap)
    {
        recap.support = forge_provider_api::capability::Support::Unsupported;
        recap.reason =
            "Isolated text requires an audited runtime and native-exportable credentials.".into();
    }
    capabilities
}

/// Publish isolated support only for the audited runtime and a native-exportable authentication mode.
fn negotiated_capabilities(
    version: Option<&str>,
    auth_status: Option<&Value>,
) -> forge_provider_api::capability::CapabilityManifest {
    let mut capabilities = codex_capabilities();
    capabilities.provider_version = version.map(str::to_owned);
    let exportable = auth_status.is_some_and(|status| {
        status.get("requiresOpenaiAuth").and_then(Value::as_bool) == Some(false)
            || matches!(
                status.get("authMethod").and_then(Value::as_str),
                Some("apikey" | "chatgpt" | "chatgptAuthTokens")
            )
    });
    if version == Some("0.164.0")
        && exportable
        && let Some(recap) = capabilities
            .capability
            .iter_mut()
            .find(|capability| capability.id == forge_provider_api::capability::CapabilityId::Recap)
    {
        recap.support = forge_provider_api::capability::Support::Supported;
        recap.reason = "Isolated temporary history excludes inherited instructions and all native or host tools.".into();
    }
    capabilities
}

/// Check that a whole-turn rollback removes exactly one identified cancelled host input.
fn retractable_turn(turn: &Value, request: &forge_provider_api::InputRetractRequest) -> bool {
    if turn.get("id").and_then(Value::as_str) != Some(request.boundary.as_str())
        || !matches!(
            turn.get("status").and_then(Value::as_str),
            Some("interrupted" | "cancelled")
        )
    {
        return false;
    }
    let Some(items) = turn.get("items").and_then(Value::as_array) else {
        return false;
    };
    let inputs = items
        .iter()
        .filter(|item| item.get("type").and_then(Value::as_str) == Some("userMessage"))
        .collect::<Vec<_>>();
    inputs.len() == 1
        && inputs[0].get("clientId").and_then(Value::as_str)
            == Some(native_input_id(&request.submission_id, &request.attempt_id).as_str())
}

/// Collect terminal publication without cancelling a native turn that is still running.
async fn collect_settled_execution(session: &Arc<CodexBinding>) -> Result<(), ProviderError> {
    let retained = {
        let active = session.active.lock().await;
        active.as_ref().and_then(|execution| {
            execution
                .task
                .as_ref()
                .filter(|task| task.is_finished() || execution.turn_updates.borrow().is_none())
                .map(|task| (execution.scope.clone(), task.clone()))
        })
    };
    if let Some((scope, task)) = retained {
        collect_invocation(session.clone(), scope, task).await?;
    }
    Ok(())
}

/// Read one binding inventory under the same admission guard used by configuration changes.
async fn read_binding_terminals(
    session: &Arc<CodexBinding>,
) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
    // Rollback fences are provisional while the lifecycle owner holds admission.
    // Observe the resulting binding, never that intermediate fenced state.
    let _admission = session.admission.lock().await;
    super::operations::check()?;
    if session.is_fenced() {
        return Err(failure(
            ProviderOperation::TerminalList,
            ProviderFailureKind::BindingClosed,
        ));
    }
    let observation = session.observations.clone().read_owned().await;
    if *observation {
        return Err(failure(
            ProviderOperation::TerminalList,
            ProviderFailureKind::SessionMissing,
        ));
    }
    async {
        let _observation = observation;
        let thread_id = session.history_id();
        let mut cursor = None::<String>;
        let mut seen = std::collections::HashSet::new();
        let mut terminal = Vec::new();
        loop {
            let page = session
                .rpc
                .lock()
                .await
                .request(
                    "thread/backgroundTerminals/list",
                    json!(
                        {"threadId":thread_id,"limit":100,"cursor":cursor}
                    ),
                    ProviderOperation::TerminalList,
                )
                .await?;
            let parsed = crate::terminal::parse_snapshot(&page, false).map_err(|_| {
                failure(
                    ProviderOperation::TerminalList,
                    ProviderFailureKind::Protocol,
                )
            })?;
            for mut row in parsed.terminal {
                row.id = forge_provider_api::terminal::TerminalId::new(
                    serde_json::to_string(&[session.binding.id.as_str(), row.id.as_str()])
                        .map_err(|_| {
                            failure(
                                ProviderOperation::TerminalList,
                                ProviderFailureKind::Protocol,
                            )
                        })?,
                )?;
                terminal.push(row);
            }
            let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
                break;
            };
            if !seen.insert(next.to_owned()) {
                return Err(failure(
                    ProviderOperation::TerminalList,
                    ProviderFailureKind::Protocol,
                ));
            }
            cursor = Some(next.to_owned());
        }
        Ok(forge_provider_api::terminal::TerminalSnapshot {
            supported: true,
            starting: false,
            terminal,
        })
    }
    .await
}

/// Retain event-driven terminal observation independently of foreground consumption.
fn start_terminal_observer(session: &Arc<CodexBinding>) -> tokio::task::JoinHandle<()> {
    let weak = Arc::downgrade(session);
    let rpc = session.rpc.clone();
    tokio::spawn(async move {
        let mut replacements = rpc.replacements();
        let mut changes = rpc.lock().await.terminal_changes();
        let mut previous = None;
        loop {
            changes.borrow_and_update();
            let Some(session) = weak.upgrade() else {
                return;
            };
            if session.closing.load(Ordering::Acquire) {
                return;
            }
            match read_binding_terminals(&session).await {
                Ok(snapshot) => {
                    if previous.as_ref() != Some(&snapshot) {
                        if session
                            .sink
                            .send_wait(ProviderEvent::Terminals {
                                scope: forge_provider_api::EventScope::Binding {
                                    owner: session.owner.clone(),
                                },
                                snapshot: snapshot.clone(),
                            })
                            .await
                            .is_err()
                        {
                            return;
                        }
                        previous = Some(snapshot);
                    }
                }
                Err(_) => {
                    if session.closing.load(Ordering::Acquire) {
                        return;
                    }
                    let diagnostic = forge_provider_api::ProviderDiagnostic {
                        kind: ProviderFailureKind::Runtime,
                        message: forge_provider_api::diagnostic::SanitizedMessage::new(
                            "Background terminal inventory is unavailable.",
                        )
                        .expect("nonempty diagnostic"),
                        fatal: false,
                    };
                    if session
                        .sink
                        .send_wait(ProviderEvent::Diagnostic {
                            scope: forge_provider_api::EventScope::Binding {
                                owner: session.owner.clone(),
                            },
                            diagnostic,
                        })
                        .await
                        .is_err()
                    {
                        return;
                    }
                }
            }
            drop(session);
            tokio::select! {
                result = changes.changed() => if result.is_err() { return; },
                result = replacements.changed() => {
                    if result.is_err() { return; }
                    changes = rpc.lock().await.terminal_changes();
                }
            }
        }
    })
}

/// Unsubscribe the exact native writer and prove it finished before another runtime resumes its history.
/// The source's ordered notification reader remains untouched throughout this read-only verification.
async fn release_native_thread_writer(
    rpc: &NativeRpc,
    target: &forge_provider_api::ProviderSessionId,
    operation: ProviderOperation,
) -> Result<(), ProviderError> {
    let uncertain = |kind| {
        ProviderError::from(ProviderFailure::new(operation, kind).at_phase(
            forge_provider_api::ProviderPhase::Collection,
            forge_provider_api::FailureDisposition::Uncertain,
        ))
    };
    let release = async {
        let mut closed = rpc.closed_threads();
        let prior_close = closed.borrow_and_update().get(target).copied();
        let response = rpc
            .request(
                "thread/unsubscribe",
                json!({"threadId":target.as_str()}),
                operation,
            )
            .await?;
        if !matches!(
            response.get("status").and_then(Value::as_str),
            Some("unsubscribed" | "notSubscribed" | "notLoaded")
        ) {
            return Err(uncertain(ProviderFailureKind::Protocol));
        }
        let mut cursor = None::<String>;
        let mut seen = std::collections::HashSet::new();
        let loaded = loop {
            let page = rpc
                .request(
                    "thread/loaded/list",
                    json!({"limit":100,"cursor":cursor}),
                    operation,
                )
                .await?;
            let rows = page
                .get("data")
                .and_then(Value::as_array)
                .ok_or_else(|| uncertain(ProviderFailureKind::Protocol))?;
            if rows.iter().any(|row| row.as_str().is_none()) {
                return Err(uncertain(ProviderFailureKind::Protocol));
            }
            if rows.iter().any(|row| row.as_str() == Some(target.as_str())) {
                break true;
            }
            let next = match page.get("nextCursor") {
                None | Some(Value::Null) => break false,
                Some(Value::String(next)) if !next.is_empty() => next,
                Some(_) => return Err(uncertain(ProviderFailureKind::Protocol)),
            };
            if !seen.insert(next.to_owned()) {
                return Err(uncertain(ProviderFailureKind::Protocol));
            }
            cursor = Some(next.to_owned());
        };
        if !loaded {
            return Ok(());
        }
        loop {
            if closed.borrow_and_update().get(target).copied() != prior_close {
                return Ok(());
            }
            closed
                .changed()
                .await
                .map_err(|_| uncertain(ProviderFailureKind::Transport))?;
        }
    };
    match super::operations::current() {
        Some(context) => tokio::select! {
            result = release => result,
            reason = context.context.interrupted() => Err(reason.failure(
                operation,
                forge_provider_api::ProviderPhase::Collection,
                forge_provider_api::FailureDisposition::Uncertain,
            )),
        },
        None => release.await,
    }
}

/// Run native compaction while admission and notification consumption are exclusively owned.
async fn compact_session(
    session: &Arc<CodexBinding>,
    request: forge_provider_api::CompactionRequest,
) -> Result<forge_provider_api::Compaction, ProviderError> {
    let thread_id = session.history_id();
    session
        .normalization
        .lock()
        .await
        .bind(Some(request.scope.clone()));
    session
        .rpc
        .lock()
        .await
        .request(
            "thread/compact/start",
            json!({"threadId":thread_id}),
            ProviderOperation::SessionCompact,
        )
        .await?;
    loop {
        let mut message = session.rpc.lock().await.receive().await?;
        session.mcp_context.lock().await.normalize(&mut message);
        let mut output = forge_provider_api::completion::ProviderOutput::default();
        session
            .normalization
            .lock()
            .await
            .normalize(&message, &mut output, None, &session.workspace)
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::SessionCompact,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let mut completed = None;
        for event in output.event {
            if let ProviderEvent::Compaction { scope, update } = &event
                && scope == &request.scope
                && update.thread_id == thread_id.as_str()
                && update.state != forge_provider_api::compaction::CompactionState::Running
            {
                completed = Some((**update).clone());
            }
            publish_event(&session.sink, event).await?;
        }
        if let Some(compaction) = completed {
            return Ok(compaction);
        }
    }
}

async fn collect_finished_execution(session: &Arc<CodexBinding>) -> Result<(), ProviderError> {
    let retained = {
        let active = session.active.lock().await;
        active.as_ref().and_then(|execution| {
            execution
                .task
                .as_ref()
                .filter(|task| task.is_finished())
                .map(|task| (execution.scope.clone(), task.clone()))
        })
    };
    if let Some((scope, task)) = retained {
        collect_invocation(session.clone(), scope, task).await?;
    }
    Ok(())
}

async fn record_receipt_failure(
    receipt: Arc<dyn forge_provider_api::submission::InputReceipt>,
    primary: ProviderError,
) -> ProviderError {
    match receipt.record_failure(primary.clone()).await {
        Ok(()) => primary,
        Err(cleanup) => ProviderError::Combined {
            primary: Box::new(primary),
            secondary: vec![cleanup],
        },
    }
}

async fn fence_unobserved_acceptance(
    session: &CodexBinding,
    request: &PromptRequest,
    turn_id: Option<&str>,
    primary: ProviderError,
) -> ProviderError {
    let mut errors = vec![primary];
    if let Some(turn_id) = turn_id {
        let thread_id = session.history_id();
        if let Err(error) = session
            .rpc
            .lock()
            .await
            .request(
                "turn/interrupt",
                json!({
                    "threadId":thread_id,"turnId":turn_id
                }),
                ProviderOperation::Cancel,
            )
            .await
        {
            errors.push(error);
        }
    }
    let lost = ExecutionUpdate::ObservationLost {
        failure: forge_provider_api::lifecycle::ObservationFailure::Disconnected,
        completion: Some(completion(session, &request.configuration.model).await),
    };
    if let Err(error) = publish(
        &session.sink,
        &request.scope,
        ProviderEvent::Execution {
            observation: ExecutionObservation::new(request.scope.clone(), lost)
                .expect("observation loss belongs to the accepted invocation"),
        },
    )
    .await
    {
        errors.push(error);
    }
    session.fenced.store(true, Ordering::Release);
    if let Some(execution) = session.active.lock().await.as_mut() {
        execution.turn_updates.send_replace(None);
    }
    session.sink.fail(events::EventDeliveryError::Observation(
        forge_provider_api::lifecycle::ObservationFailure::Disconnected,
    ));
    let mut errors = errors.into_iter();
    let primary = errors
        .next()
        .unwrap_or_else(|| failure(ProviderOperation::InputSubmit, ProviderFailureKind::Unknown));
    let secondary = errors.collect::<Vec<_>>();
    if secondary.is_empty() {
        primary
    } else {
        ProviderError::Combined {
            primary: Box::new(primary),
            secondary,
        }
    }
}

async fn observe_execution(
    session: Arc<CodexBinding>,
    request: PromptRequest,
    address: forge_provider_api::TurnAddress,
    submissions: Arc<Mutex<Vec<forge_provider_api::SubmissionId>>>,
    mut cancel: mpsc::UnboundedReceiver<()>,
    initial_messages: Vec<Value>,
) -> Result<(), ProviderError> {
    let mut cancelled = false;
    let mut native_cleanup_failed = false;
    let mut cleanup_errors = Vec::new();
    let mut descendants = super::descendants::DescendantTracker::default();
    let mut parent_terminal = None::<Value>;
    let mut active_parent_turn = Some(address.turn_id.clone());
    let mut initial_messages = initial_messages.into_iter();
    let result = loop {
        let native = if let Some(message) = initial_messages.next() {
            Ok(message)
        } else {
            tokio::select! {
                signal = cancel.recv(), if !cancelled => {
                    if signal.is_some() {
                        cancelled = true;
                        if let Err(error) = interrupt_native(&session, &address.thread_id, active_parent_turn.as_ref(), &descendants).await { cleanup_errors.push(error); native_cleanup_failed = true; }
                    }
                    continue;
                }
                message = async { session.rpc.lock().await.receive().await } => message,
            }
        };
        let mut message = match native {
            Ok(message) => message,
            Err(error) => {
                return Err(lose_observation(
                    &session,
                    &request,
                    &address,
                    &descendants,
                    error,
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                )
                .await);
            }
        };
        session.mcp_context.lock().await.normalize(&mut message);
        let params = message.get("params").unwrap_or(&Value::Null);
        descendants.observe(&message, &address.thread_id);
        session.approval_context.observe(&message);
        if message.get("method").is_some() && message.get("id").is_some() {
            if cancelled || session.is_fenced() {
                session
                    .rpc
                    .lock()
                    .await
                    .reject_request(message.get("id").unwrap_or(&Value::Null))
                    .await?;
                continue;
            }
            let request_thread = params.get("threadId").and_then(Value::as_str);
            if request_thread
                .is_some_and(|thread| thread != address.thread_id && !descendants.owns(thread))
            {
                return Err(lose_observation(
                    &session,
                    &request,
                    &address,
                    &descendants,
                    failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Protocol,
                    ),
                    forge_provider_api::lifecycle::ObservationFailure::Protocol,
                )
                .await);
            }
            let handled = tokio::select! {
                biased;
                signal = cancel.recv(), if !cancelled => {
                    if signal.is_some() {
                        cancelled = true;
                        let mut errors = Vec::new();
                        if let Err(error) = session.rpc.lock().await.reject_request(message.get("id").unwrap_or(&Value::Null)).await { errors.push(error); }
                        if let Err(error) = interrupt_native(&session, &address.thread_id, active_parent_turn.as_ref(), &descendants).await { errors.push(error); }
                        if !errors.is_empty() { native_cleanup_failed = true; cleanup_errors.extend(errors); }
                    }
                    Ok(())
                }
                handled = handle_native_request(&session, &request, &message) => handled,
            };
            if let Err(error) = handled {
                return Err(lose_observation(
                    &session,
                    &request,
                    &address,
                    &descendants,
                    error,
                    forge_provider_api::lifecycle::ObservationFailure::Protocol,
                )
                .await);
            }
            continue;
        }
        let terminal = if params.get("threadId").and_then(Value::as_str)
            == Some(address.thread_id.as_str())
            && message.get("method").and_then(Value::as_str) == Some("turn/completed")
            && params
                .pointer("/turn/id")
                .or_else(|| params.get("turnId"))
                .and_then(Value::as_str)
                .map(forge_provider_api::ProviderTurnId::from)
                == active_parent_turn
        {
            Some(
                params
                    .pointer("/turn")
                    .cloned()
                    .unwrap_or_else(|| params.clone()),
            )
        } else {
            None
        };
        let Some(thread_id) = params.get("threadId").and_then(Value::as_str) else {
            continue;
        };
        if thread_id != address.thread_id {
            if !descendants.owns(thread_id) {
                continue;
            }
            let events = session
                .normalization
                .lock()
                .await
                .normalize_retained(&message, &session.workspace)
                .await;
            match events {
                Ok(Some(events)) => {
                    for event in events {
                        if let Err(error) = publish_event(&session.sink, event).await {
                            return Err(lose_observation(
                                &session,
                                &request,
                                &address,
                                &descendants,
                                error,
                                forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                            )
                            .await);
                        }
                    }
                }
                Ok(None) => {}
                Err(_) => {
                    return Err(lose_observation(
                        &session,
                        &request,
                        &address,
                        &descendants,
                        failure(
                            ProviderOperation::InputSubmit,
                            ProviderFailureKind::Protocol,
                        ),
                        forge_provider_api::lifecycle::ObservationFailure::Protocol,
                    )
                    .await);
                }
            }
            continue;
        }
        match message.get("method").and_then(Value::as_str) {
            Some("turn/started") => {
                if let Some(turn_id) = params
                    .pointer("/turn/id")
                    .or_else(|| params.get("turnId"))
                    .and_then(Value::as_str)
                {
                    if turn_id != address.turn_id.as_str() {
                        return Err(lose_observation(
                            &session,
                            &request,
                            &address,
                            &descendants,
                            failure(
                                ProviderOperation::InputSubmit,
                                ProviderFailureKind::Protocol,
                            ),
                            forge_provider_api::lifecycle::ObservationFailure::UnexpectedExecution,
                        )
                        .await);
                    }
                    active_parent_turn = Some(turn_id.to_owned().into());
                    if let Some(execution) = session.active.lock().await.as_mut() {
                        execution.turn_id = turn_id.to_owned();
                        execution
                            .turn_updates
                            .send_replace(Some(turn_id.to_owned()));
                    }
                }
            }
            Some("turn/completed") => {
                let completed_id = params
                    .pointer("/turn/id")
                    .or_else(|| params.get("turnId"))
                    .and_then(Value::as_str)
                    .map(forge_provider_api::ProviderTurnId::from);
                if active_parent_turn.is_some() && active_parent_turn == completed_id {
                    active_parent_turn = None;
                }
            }
            _ => {}
        }
        let mut output = forge_provider_api::completion::ProviderOutput::default();
        let mut normalization = session.normalization.lock().await;
        if let Err(_error) = normalization
            .normalize(&message, &mut output, None, &session.workspace)
            .await
        {
            drop(normalization);
            return Err(lose_observation(
                &session,
                &request,
                &address,
                &descendants,
                failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Protocol,
                ),
                forge_provider_api::lifecycle::ObservationFailure::Protocol,
            )
            .await);
        }
        drop(normalization);
        for event in output.event {
            if let Err(error) = publish(&session.sink, &request.scope, event).await {
                return Err(lose_observation(
                    &session,
                    &request,
                    &address,
                    &descendants,
                    error,
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                )
                .await);
            }
        }
        if let Some(terminal) = terminal {
            parent_terminal = Some(terminal);
        }
        if descendants.active_count() == 0
            && active_parent_turn.is_none()
            && let Some(terminal) = parent_terminal.take()
        {
            break terminal;
        }
    };

    if let Some(execution) = session.active.lock().await.as_mut() {
        execution.turn_updates.send_replace(None);
    }

    let covered_submissions = submissions.lock().await.clone();
    let mut completion: ProviderCompletion = completion(&session, &request.configuration.model)
        .await
        .into();
    completion.provider_checkpoint_id = Some(address.turn_id.as_str().to_owned());
    let completion = CompletionSnapshot::from(completion);
    let status = result.pointer("/status").and_then(Value::as_str);
    let (mut outcome, failure_detail) = match status {
        Some("completed") => (ExecutionResult::Completed { completion }, None),
        Some("interrupted" | "cancelled") => (ExecutionResult::Cancelled { completion }, None),
        Some("failed") => {
            let native_error = result.get("error");
            let kind =
                native_error.map_or(ProviderFailureKind::Unknown, super::retry::failure_kind);
            let retry_category = native_error.and_then(crate::codex::retry::transient_category);
            let replay_sensitive = session.normalization.lock().await.replay_sensitive;
            if let Some(category) = retry_category
                && !replay_sensitive
            {
                (
                    ExecutionResult::Failed {
                        completion,
                        failure: ExecutionFailure::Retryable {
                            address: address.clone(),
                            category,
                            retry_after: None,
                        },
                    },
                    Some(kind),
                )
            } else {
                (
                    ExecutionResult::Failed {
                        completion,
                        failure: ExecutionFailure::Terminal { kind },
                    },
                    Some(kind),
                )
            }
        }
        Some(_) => (
            ExecutionResult::Failed {
                completion,
                failure: ExecutionFailure::Terminal {
                    kind: ProviderFailureKind::Unknown,
                },
            },
            Some(ProviderFailureKind::Unknown),
        ),
        None => {
            return Err(lose_observation(
                &session,
                &request,
                &address,
                &descendants,
                failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Protocol,
                ),
                forge_provider_api::lifecycle::ObservationFailure::Protocol,
            )
            .await);
        }
    };
    if cancelled
        && matches!(status, Some("interrupted" | "cancelled"))
        && submissions.lock().await.as_slice() == [request.submission_id.clone()]
        && !native_cleanup_failed
    {
        *session.rollback.lock().await = Some(RollbackAuthorization {
            thread_id: address.thread_id.clone(),
            scope: request.scope.clone(),
            turn_id: address.turn_id.clone(),
            submission_id: request.submission_id.clone(),
            attempt_id: request.attempt_id.clone(),
        });
    }
    if native_cleanup_failed {
        let completion = match outcome {
            ExecutionResult::Completed { completion }
            | ExecutionResult::Cancelled { completion }
            | ExecutionResult::Failed { completion, .. } => completion,
        };
        outcome = ExecutionResult::Failed {
            completion,
            failure: ExecutionFailure::Terminal {
                kind: ProviderFailureKind::Runtime,
            },
        };
    }
    let _ = failure_detail;
    session
        .outcomes
        .lock()
        .await
        .insert(request.scope.clone(), outcome.clone());
    let completed_at_ms = now_ms();
    let update = ExecutionUpdate::Finished {
        address: address.clone(),
        covered_submissions,
        completed_at_ms,
        result: outcome,
    };
    let (background_start, background_error) = match start_background_reader(&session).await {
        Ok(start) => (start, None),
        Err(error) => (None, Some(error)),
    };
    let published = publish(
        &session.sink,
        &request.scope,
        ProviderEvent::Execution {
            observation: ExecutionObservation::new(request.scope.clone(), update)
                .expect("terminal coverage belongs to its invocation"),
        },
    )
    .await;
    if let Err(error) = published {
        return Err(lose_observation(
            &session,
            &request,
            &address,
            &descendants,
            error,
            forge_provider_api::lifecycle::ObservationFailure::Disconnected,
        )
        .await);
    }
    if let Some(start) = background_start {
        let _ = start.send(());
    }
    let mut errors = Vec::new();
    if let Some(error) = background_error {
        errors.push(error);
    }
    if native_cleanup_failed {
        errors.append(&mut cleanup_errors);
    }
    combine_errors(errors)
}

async fn lose_observation(
    session: &CodexBinding,
    request: &PromptRequest,
    address: &forge_provider_api::TurnAddress,
    descendants: &super::descendants::DescendantTracker,
    primary: ProviderError,
    failure_kind: forge_provider_api::lifecycle::ObservationFailure,
) -> ProviderError {
    let failure_kind = if let ProviderError::Observation(observed) = &primary {
        observed.clone()
    } else {
        failure_kind
    };
    let mut errors = vec![primary];
    let mut active_turns = descendants.active_turns();
    active_turns.push((
        address.thread_id.clone(),
        Some(address.turn_id.as_str().to_owned()),
    ));
    for (thread_id, turn_id) in active_turns {
        if let Some(turn_id) = turn_id
            && let Err(error) = session
                .rpc
                .lock()
                .await
                .request(
                    "turn/interrupt",
                    json!({
                        "threadId":thread_id,"turnId":turn_id
                    }),
                    ProviderOperation::Cancel,
                )
                .await
        {
            errors.push(error);
        }
    }
    if let Err(error) = stop_background_commands(session).await {
        errors.push(error);
    }

    let event = ProviderEvent::Execution {
        observation: ExecutionObservation::new(
            request.scope.clone(),
            ExecutionUpdate::ObservationLost {
                failure: failure_kind.clone(),
                completion: Some(completion(session, &request.configuration.model).await),
            },
        )
        .expect("observation loss belongs to the invocation scope"),
    };
    if let Err(error) = publish(&session.sink, &request.scope, event).await {
        errors.push(error);
    }
    session.fenced.store(true, Ordering::Release);
    if let Some(execution) = session.active.lock().await.as_mut() {
        execution.turn_updates.send_replace(None);
    }
    session
        .sink
        .fail(events::EventDeliveryError::Observation(failure_kind));
    combine_errors(errors).expect_err("observation loss always preserves a primary failure")
}

fn combine_errors(mut errors: Vec<ProviderError>) -> Result<(), ProviderError> {
    if errors.is_empty() {
        return Ok(());
    }
    let primary = errors.remove(0);
    if errors.is_empty() {
        Err(primary)
    } else {
        Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: errors,
        })
    }
}

fn combine_primary(primary: ProviderError, secondary: ProviderError) -> ProviderError {
    ProviderError::Combined {
        primary: Box::new(primary),
        secondary: vec![secondary],
    }
}

/// Continue admitted background tool observation after foreground settlement.
async fn start_background_reader(
    session: &Arc<CodexBinding>,
) -> Result<Option<tokio::sync::oneshot::Sender<()>>, ProviderError> {
    if !session
        .normalization
        .lock()
        .await
        .has_pending_background_tools()
    {
        return Ok(None);
    }
    let mut slot = session.background_reader.lock().await;
    if let Some(reader) = slot.as_ref()
        && !reader.task.is_finished()
    {
        return Ok(None);
    }
    if let Some(reader) = slot.as_ref() {
        let result = reader.task.join().await;
        slot.take();
        result?;
    }
    let (cancel, mut cancelled) = mpsc::unbounded_channel();
    let (start, started) = tokio::sync::oneshot::channel();
    let retained = session.clone();
    let task = tokio::spawn(async move {
        tokio::select! {
            _ = started => {},
            signal = cancelled.recv() => if signal.is_some() { return Ok(()); },
        }
        loop {
            if !retained
                .normalization
                .lock()
                .await
                .has_pending_background_tools()
            {
                break;
            }
            let received = tokio::select! {
                signal = cancelled.recv() => if signal.is_some() { break; } else { continue; },
                received = async { retained.rpc.lock().await.receive().await } => received,
            };
            let mut message = match received {
                Ok(message) => message,
                Err(error) => {
                    let observation = match &error {
                        ProviderError::Observation(observation)
                        | ProviderError::Delivery(events::EventDeliveryError::Observation(
                            observation,
                        )) => observation.clone(),
                        ProviderError::Failure(cause)
                            if cause.kind() == ProviderFailureKind::Transport =>
                        {
                            forge_provider_api::ObservationFailure::Disconnected
                        }
                        ProviderError::Failure(cause)
                            if cause.kind() == ProviderFailureKind::Protocol =>
                        {
                            forge_provider_api::ObservationFailure::Protocol
                        }
                        _ => forge_provider_api::ObservationFailure::LifecycleUnavailable,
                    };
                    return Err(
                        invalidate_background_observer(&retained, error, observation).await,
                    );
                }
            };
            retained.mcp_context.lock().await.normalize(&mut message);
            if message.get("method").is_some() && message.get("id").is_some() {
                if let Err(error) = retained
                    .rpc
                    .lock()
                    .await
                    .reject_request(message.get("id").unwrap_or(&Value::Null))
                    .await
                {
                    return Err(invalidate_background_observer(
                        &retained,
                        error,
                        forge_provider_api::ObservationFailure::LifecycleUnavailable,
                    )
                    .await);
                }
                continue;
            }
            let normalized = {
                let mut normalization = retained.normalization.lock().await;
                match normalization
                    .normalize_background(&message, &retained.workspace)
                    .await
                {
                    Ok(Some(events)) => Ok(Some(events)),
                    Ok(None) => {
                        normalization
                            .normalize_retained(&message, &retained.workspace)
                            .await
                    }
                    Err(error) => Err(error),
                }
            };
            match normalized {
                Ok(Some(events)) => {
                    for event in events {
                        if let Err(error) = publish_event(&retained.sink, event).await {
                            return Err(invalidate_background_observer(
                                &retained,
                                error,
                                forge_provider_api::ObservationFailure::LifecycleUnavailable,
                            )
                            .await);
                        }
                    }
                }
                Ok(None) => {
                    if message.get("method").and_then(Value::as_str) == Some("turn/started") {
                        return Err(invalidate_background_observer(
                            &retained,
                            failure(
                                ProviderOperation::InputSubmit,
                                ProviderFailureKind::WrongInvocation,
                            ),
                            forge_provider_api::ObservationFailure::OwnershipConflict,
                        )
                        .await);
                    }
                }
                Err(_error) => {
                    return Err(invalidate_background_observer(
                        &retained,
                        failure(
                            ProviderOperation::InputSubmit,
                            ProviderFailureKind::Protocol,
                        ),
                        forge_provider_api::ObservationFailure::Protocol,
                    )
                    .await);
                }
            }
        }
        Ok(())
    });
    *slot = Some(BackgroundReader {
        cancel,
        task: crate::retained_task::RetainedTask::new(ProviderOperation::InputSubmit, task),
    });
    Ok(Some(start))
}

/// Return the socket reader to the retained session before another operation takes it.
async fn stop_background_reader(session: &CodexBinding) -> Result<(), ProviderError> {
    let mut slot = session.background_reader.lock().await;
    if let Some(reader) = slot.as_ref() {
        let _ = reader.cancel.send(());
        let result = reader.task.join().await;
        slot.take();
        result?;
    }
    Ok(())
}

/// Terminate only the exact Codex background commands admitted by this session.
async fn stop_background_commands(session: &CodexBinding) -> Result<(), ProviderError> {
    let addresses = session
        .normalization
        .lock()
        .await
        .pending_background_addresses();
    let mut owned = HashMap::<String, std::collections::HashSet<String>>::new();
    for address in &addresses {
        owned
            .entry(address.turn.thread_id.clone())
            .or_default()
            .insert(address.tool_id.as_str().to_owned());
    }
    for (thread_id, item_ids) in &owned {
        let rows = terminal_inventory(session, thread_id).await?;
        for row in rows.into_iter().filter(|row| {
            row.get("itemId")
                .and_then(Value::as_str)
                .is_some_and(|id| item_ids.contains(id))
        }) {
            let process_id = row
                .get("processId")
                .and_then(Value::as_str)
                .ok_or_else(|| {
                    failure(
                        ProviderOperation::SessionClose,
                        ProviderFailureKind::Protocol,
                    )
                })?;
            let result = session
                .rpc
                .lock()
                .await
                .request(
                    "thread/backgroundTerminals/terminate",
                    json!({
                        "threadId":thread_id,"processId":process_id
                    }),
                    ProviderOperation::SessionClose,
                )
                .await?;
            if result.get("terminated").and_then(Value::as_bool).is_none() {
                return Err(failure(
                    ProviderOperation::SessionClose,
                    ProviderFailureKind::Protocol,
                ));
            }
        }
        let remaining = terminal_inventory(session, thread_id).await?;
        if remaining.iter().any(|row| {
            row.get("itemId")
                .and_then(Value::as_str)
                .is_some_and(|id| item_ids.contains(id))
        }) {
            return Err(failure(
                ProviderOperation::SessionClose,
                ProviderFailureKind::Runtime,
            ));
        }
    }
    for address in addresses {
        if let Some(event) = session
            .normalization
            .lock()
            .await
            .interrupt_background(&address)
        {
            publish_event(&session.sink, event).await?;
        }
    }
    Ok(())
}

async fn terminal_inventory(
    session: &CodexBinding,
    thread_id: &str,
) -> Result<Vec<Value>, ProviderError> {
    let mut cursor = None::<String>;
    let mut seen = std::collections::HashSet::new();
    let mut rows = Vec::new();
    loop {
        let page = session
            .rpc
            .lock()
            .await
            .request(
                "thread/backgroundTerminals/list",
                json!({
                    "threadId":thread_id,"limit":100,"cursor":cursor
                }),
                ProviderOperation::SessionClose,
            )
            .await?;
        let data = page.get("data").and_then(Value::as_array).ok_or_else(|| {
            failure(
                ProviderOperation::SessionClose,
                ProviderFailureKind::Protocol,
            )
        })?;
        rows.extend(data.iter().cloned());
        let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
            break;
        };
        if !seen.insert(next.to_owned()) {
            return Err(failure(
                ProviderOperation::SessionClose,
                ProviderFailureKind::Protocol,
            ));
        }
        cursor = Some(next.to_owned());
    }
    Ok(rows)
}

async fn handle_native_request(
    session: &CodexBinding,
    request: &PromptRequest,
    message: &Value,
) -> Result<(), ProviderError> {
    if session.isolated
        && message.get("method").and_then(Value::as_str)
            == Some("account/chatgptAuthTokens/refresh")
        && !session.is_fenced()
    {
        let id = message.get("id").ok_or_else(|| {
            failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Protocol,
            )
        })?;
        let auth = session.isolation_auth.as_ref().ok_or_else(|| {
            failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Authentication,
            )
        })?;
        let response = auth.refresh_response().await?;
        return session.rpc.lock().await.respond(id, response).await;
    }
    if session.is_fenced() || session.isolated {
        return session
            .rpc
            .lock()
            .await
            .reject_request(message.get("id").unwrap_or(&Value::Null))
            .await;
    }
    let id = message.get("id").ok_or_else(|| {
        failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Protocol,
        )
    })?;
    let method = message
        .get("method")
        .and_then(Value::as_str)
        .ok_or_else(|| {
            failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Protocol,
            )
        })?;
    let params = message.get("params").unwrap_or(&Value::Null);
    let response = match method {
        "item/tool/call" => {
            let call = super::decode::dynamic_tool_call(params).map_err(|_| {
                failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Protocol,
                )
            })?;
            let name = call.name.as_str();
            if !request.tools.iter().any(|tool| tool.name == name) {
                super::decode::dynamic_tool_response(
                    "The requested tool is not enabled for this invocation.".into(),
                    false,
                )
            } else {
                request.permissions.before_tool(&request.scope).await?;
                let authorization = request
                    .permissions
                    .authorize(
                        &request.scope,
                        PermissionRequest {
                            id: uuid::Uuid::new_v4().to_string(),
                            provider: crate::registry::select("codex")
                                .expect("Codex is registered"),
                            reason: None,
                            target_list: vec![PermissionTarget::HostTool {
                                name: name.to_owned(),
                            }],
                        },
                    )
                    .await;
                match authorization {
                    PermissionResponse::Allow => {
                        if session.is_fenced() {
                            return Err(ProviderError::Cancelled);
                        }
                        match request.provider_tools.call(&request.scope, call).await {
                            Ok(text) => super::decode::dynamic_tool_response(text, true),
                            Err(_) => super::decode::dynamic_tool_response(
                                "The requested tool could not be completed.".into(),
                                false,
                            ),
                        }
                    }
                    PermissionResponse::Deny { reason } => super::decode::dynamic_tool_response(
                        reason.unwrap_or_else(|| "The requested tool was denied.".into()),
                        false,
                    ),
                    PermissionResponse::Cancelled => super::decode::dynamic_tool_response(
                        "The requested tool was cancelled.".into(),
                        false,
                    ),
                }
            }
        }
        "item/commandExecution/requestApproval"
        | "item/fileChange/requestApproval"
        | "item/permissions/requestApproval"
        | "mcpServer/elicitation/request" => {
            let prepared = session.approval_context.prepare(message).map_err(|_| {
                failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Protocol,
                )
            })?;
            let decision = request
                .permissions
                .authorize(&request.scope, prepared.request.clone())
                .await;
            if session.is_fenced() {
                return Err(ProviderError::Cancelled);
            }
            prepared.validate().map_err(|_| {
                ProviderError::from(
                    ProviderFailure::new(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::ResourceConflict,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Observation,
                        forge_provider_api::FailureDisposition::Uncertain,
                    ),
                )
            })?;
            let resolution = match decision {
                PermissionResponse::Allow => {
                    forge_provider_api::approval::ApprovalResolution::AllowOnce
                }
                PermissionResponse::Deny { .. } => {
                    forge_provider_api::approval::ApprovalResolution::DenyOnce
                }
                PermissionResponse::Cancelled => {
                    forge_provider_api::approval::ApprovalResolution::Cancel
                }
            };
            super::approval::codex_response(message, resolution)
        }
        _ => {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Protocol,
            ));
        }
    };
    if session.is_fenced() {
        return session.rpc.lock().await.reject_request(id).await;
    }
    session.rpc.lock().await.respond(id, response).await
}

fn input_text(input: &PromptInput) -> String {
    match input {
        PromptInput::Text(text) => text.clone(),
        PromptInput::Skill { name, arguments } => {
            format!("Use skill {name} with arguments: {arguments}")
        }
        PromptInput::QuestionReply(reply) => super::question::render(reply),
    }
}

/// Reject model controls absent from the native protocol before acquiring resources.
fn validate_model_selection(selection: &ModelSelection) -> Result<(), ProviderError> {
    if selection.context_window.is_some() {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ContextWindowSelection,
        )
        .into());
    }
    Ok(())
}

/// Resolve native catalog defaults and encode validated model controls.
async fn apply_model_selection(
    rpc: &NativeRpc,
    parameters: &mut Value,
    selection: &ModelSelection,
    operation: ProviderOperation,
) -> Result<Option<String>, ProviderError> {
    validate_model_selection(selection)?;
    parameters["serviceTier"] = json!(selection.service_tier);
    let catalog = rpc.request("model/list", json!({}), operation).await?;
    let models = crate::codex::catalog::parse_model_catalog(&catalog);
    let selected = match selection.model.as_deref() {
        Some(id) => models.iter().find(|model| model.id == id),
        None => models.iter().find(|model| model.is_default),
    }
    .ok_or_else(|| failure(operation, ProviderFailureKind::Rejected))?;
    selection
        .service_tier
        .validate_model(selected)
        .map_err(|_| failure(operation, ProviderFailureKind::Policy))?;
    parameters["model"] = json!(selected.id);
    let reasoning = match &selection.reasoning {
        ReasoningSelection::Default => selected.default_reasoning.clone(),
        ReasoningSelection::Level(level) => {
            if !selected.reasoning.contains(level) {
                return Err(failure(operation, ProviderFailureKind::Policy));
            }
            Some(level.clone())
        }
        ReasoningSelection::Lowest => selected.reasoning.first().cloned(),
    };
    if !parameters["config"].is_object() {
        parameters["config"] = json!({});
    }
    parameters["config"]["model_reasoning_effort"] = json!(reasoning);
    Ok(reasoning)
}

async fn completion(session: &CodexBinding, model: &ModelSelection) -> CompletionSnapshot {
    let provider = crate::registry::descriptor(
        &crate::registry::select("codex").expect("Codex is registered"),
    )
    .expect("registered Codex descriptor");
    let evidence = forge_provider_api::completion::TurnEvidence::default();
    CompletionSnapshot::from(ProviderCompletion {
        provider_session_id: Some(session.history_id()),
        provider_checkpoint_id: None,
        evidence,
        capability: provider.capability.clone(),
        runtime: ProviderRuntime {
            provider: provider.label.clone(),
            model: model.model.clone(),
        },
        metrics: Default::default(),
    })
}

async fn publish(
    sink: &ProviderEventSink,
    _invocation: &forge_provider_api::InvocationScope,
    event: ProviderEvent,
) -> Result<(), ProviderError> {
    publish_event(sink, event).await
}

async fn publish_event(
    sink: &ProviderEventSink,
    event: ProviderEvent,
) -> Result<(), ProviderError> {
    let scoped = sink
        .bind_scope(event.scope())
        .map_err(ProviderError::from)?;
    scoped.send_wait(event).await.map_err(ProviderError::from)
}

/// Observe exact native admission without making cancellation depend on its RPC acknowledgement.
async fn await_admission(
    session: &CodexBinding,
    request: &PromptRequest,
    request_id: u64,
) -> Result<(String, Vec<Value>), ProviderError> {
    let rpc = session.rpc.lock().await.shared();
    let response = rpc.wait_response(request_id, ProviderOperation::InputSubmit);
    tokio::pin!(response);
    let thread_id = session.history_id();
    let mut initial_messages = Vec::new();
    loop {
        tokio::select! {
            result = &mut response => {
                let result = result?;
                let turn_id = result.get("turnId").or_else(|| result.pointer("/turn/id")).and_then(Value::as_str).filter(|id| !id.is_empty()).ok_or_else(|| failure(ProviderOperation::InputSubmit, ProviderFailureKind::Protocol))?;
                return Ok((turn_id.to_owned(), initial_messages));
            }
            message = rpc.receive() => {
                let message = message?;
                if message.get("method").and_then(Value::as_str) == Some("account/chatgptAuthTokens/refresh") && message.get("id").is_some() {
                    handle_native_request(session, request, &message).await?;
                    continue;
                }
                let params = message.get("params").unwrap_or(&Value::Null);
                let turn_id = if message.get("method").and_then(Value::as_str) == Some("turn/started")
                    && params.get("threadId").and_then(Value::as_str) == Some(thread_id.as_str()) {
                    params.pointer("/turn/id").or_else(|| params.get("turnId")).and_then(Value::as_str).filter(|id| !id.is_empty()).map(str::to_owned)
                } else { None };
                initial_messages.push(message);
                if let Some(turn_id) = turn_id {
                    // Admission owns a previously settled slot and its sole ordered reader. The preceding
                    // invocation's start precedes its consumed terminal; no successor or goal scheduler
                    // can create another start while this exact attempt owns the admission mutex.
                    rpc.abandon_response(request_id).await;
                    return Ok((turn_id, initial_messages));
                }
            }
        }
    }
}

/// Recognize positive matching native history evidence; absent, partial, or filtered pages stay unknown.
fn history_page_evidence(
    rows: &[Value],
    thread: &str,
    submission: &forge_provider_api::SubmissionId,
    attempt: &forge_provider_api::DeliveryAttemptId,
) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
    let client_id = native_input_id(submission, attempt);
    for row in rows {
        let item = row.get("item").unwrap_or(row);
        if item.get("type").and_then(Value::as_str) == Some("userMessage")
            && item.get("clientId").and_then(Value::as_str) == Some(client_id.as_str())
        {
            let receipt = recovery_token_parts(thread, submission.as_str(), attempt.as_str())?;
            let provider_session_id = forge_provider_api::ProviderSessionId::try_from(
                thread.to_owned(),
            )
            .map_err(|_| {
                failure(
                    ProviderOperation::InputReconcile,
                    ProviderFailureKind::Protocol,
                )
            })?;
            return Ok(forge_provider_api::SubmissionEvidence::Accepted {
                receipt: Some(receipt),
                provider_session_id: Some(provider_session_id),
            });
        }
    }
    Ok(forge_provider_api::SubmissionEvidence::Unknown)
}

/// Encode an attempt-specific native message identity without changing the host journal identity.
fn native_input_id(
    submission: &forge_provider_api::SubmissionId,
    attempt: &forge_provider_api::DeliveryAttemptId,
) -> String {
    serde_json::to_string(&["forge-input-v2", submission.as_str(), attempt.as_str()])
        .expect("string identity arrays always serialize")
}

/// Create opaque sending evidence scoped to durable history, logical input, and delivery attempt.
fn recovery_token(
    session: &CodexBinding,
    submission: &forge_provider_api::SubmissionId,
    attempt: &forge_provider_api::DeliveryAttemptId,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    let thread = session.history_id();
    recovery_token_parts(thread.as_str(), submission.as_str(), attempt.as_str())
}

/// Encode recovery identity without authorizing any history mutation or resend.
fn recovery_token_parts(
    thread: &str,
    submission: &str,
    attempt: &str,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    forge_provider_api::RecoveryToken::new(
        serde_json::to_vec(&["codex-input-v2", thread, submission, attempt]).map_err(|_| {
            failure(
                ProviderOperation::InputReconcile,
                ProviderFailureKind::Protocol,
            )
        })?,
    )
    .map_err(|_| {
        failure(
            ProviderOperation::InputReconcile,
            ProviderFailureKind::Protocol,
        )
    })
}

struct RecoveryEvidence {
    thread_id: String,
    submission_id: String,
    attempt_id: String,
}

/// Parse only the current opaque evidence format; malformed evidence cannot prove nonacceptance.
fn parse_recovery_token(
    token: Option<&forge_provider_api::RecoveryToken>,
) -> Option<RecoveryEvidence> {
    let values: [String; 4] = serde_json::from_slice(token?.as_bytes()).ok()?;
    let [version, thread_id, submission_id, attempt_id] = values;
    if version != "codex-input-v2"
        || thread_id.is_empty()
        || submission_id.is_empty()
        || attempt_id.is_empty()
    {
        return None;
    }
    Some(RecoveryEvidence {
        thread_id,
        submission_id,
        attempt_id,
    })
}

/// Preserve typed nested failure causes while assigning the operation requested by the caller.
fn operation_error(error: ProviderError, operation: ProviderOperation) -> ProviderError {
    match error {
        ProviderError::Failure(cause) => ProviderFailure::new(operation, cause.kind())
            .at_phase(cause.phase(), cause.disposition())
            .into(),
        ProviderError::Combined { primary, secondary } => ProviderError::Combined {
            primary: Box::new(operation_error(*primary, operation)),
            secondary: secondary
                .into_iter()
                .map(|error| operation_error(error, operation))
                .collect(),
        },
        other => other,
    }
}

/// Classify a native failure without leaking provider protocol text across the adapter boundary.
fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    use forge_provider_api::error::{FailureDisposition as Disposition, ProviderPhase as Phase};
    let (phase, disposition) = match (operation, kind) {
        (
            _,
            ProviderFailureKind::Rejected
            | ProviderFailureKind::ExecutionActive
            | ProviderFailureKind::WrongInvocation
            | ProviderFailureKind::SessionMissing
            | ProviderFailureKind::Policy
            | ProviderFailureKind::HistoryConflict
            | ProviderFailureKind::ResourceConflict,
        ) => (Phase::Admission, Disposition::Unchanged),
        (_, ProviderFailureKind::BindingClosed) => (Phase::Admission, Disposition::Closed),
        (ProviderOperation::SessionClose | ProviderOperation::Shutdown, _) => {
            (Phase::Close, Disposition::Invalidated)
        }
        (ProviderOperation::Cancel, _) => (Phase::Interruption, Disposition::Uncertain),
        (ProviderOperation::SessionConfigure, _) => (Phase::Preparation, Disposition::Invalidated),
        (ProviderOperation::SessionCompact, _) => (Phase::Observation, Disposition::Invalidated),
        (ProviderOperation::InputRetract | ProviderOperation::SessionFork, _) => {
            (Phase::Observation, Disposition::Uncertain)
        }
        (
            ProviderOperation::SessionOpen
            | ProviderOperation::Inspect
            | ProviderOperation::Authenticate
            | ProviderOperation::ModelList
            | ProviderOperation::SkillList
            | ProviderOperation::McpList
            | ProviderOperation::TerminalList
            | ProviderOperation::TerminalTerminate
            | ProviderOperation::InputSubmit
            | ProviderOperation::InputSteer
            | ProviderOperation::InputReconcile,
            _,
        ) => (Phase::Observation, Disposition::Uncertain),
    };
    ProviderFailure::new(operation, kind)
        .at_phase(phase, disposition)
        .into()
}

fn now_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| i64::try_from(duration.as_millis()).unwrap_or(i64::MAX))
        .unwrap_or_default()
}

/// Exclude inherited instructions and every native tool/automation surface in temporary history.
fn isolated_config() -> Value {
    let mut config = json!({
        "web_search":"disabled",
        "project_doc_max_bytes":0,
        "instructions":"",
        "developer_instructions":"",
        "include_permissions_instructions":false,
        "include_apps_instructions":false,
        "include_collaboration_mode_instructions":false,
        "include_environment_context":false,
        "include_environment_context_time":false,
        "cloud.skills.enabled":false,
        "orchestrator.mcp.enabled":false,
        "agents.enabled":false,
        "skills.include_instructions":false,
        "orchestrator.skills.enabled":false,
        "tools.update_plan.enabled":false,
        "tools.experimental_request_user_input.enabled":false
    });
    for feature in [
        "apps",
        "enable_mcp_apps",
        "codex_apps_mcp_2026_07_28",
        "recommended_plugins",
        "remote_plugin",
        "external_agent_memory_import",
        "skill_search",
        "in_app_browser",
        "in_app_local_automation",
        "code_mode",
        "code_mode_only",
        "code_mode_tool_search",
        "code_mode_host",
        "code_mode_prewarm",
        "default_mode_request_user_input",
        "sleep_tool",
        "send_message_to_user_async",
        "runtime_agent_instances",
        "use_agent_identity",
        "artifact",
        "async_compaction",
        "step_model_switching",
        "context_management",
        "current_time_reminder",
        "deferred_executor",
        "enable_fanout",
        "goals",
        "hooks",
        "image_generation",
        "memories",
        "multi_agent",
        "multi_agent_v2",
        "plugins",
        "request_permissions_tool",
        "shell_snapshot",
        "shell_tool",
        "standalone_web_search",
        "token_budget",
        "tool_suggest",
        "unified_exec",
        "view_image",
    ] {
        config[format!("features.{feature}")] = json!(false);
    }
    config
}

#[cfg(test)]
mod continuation_tests {
    use super::*;
    use async_trait::async_trait;
    use forge_provider_api::{
        HostEffectFailure,
        access::AccessPolicy,
        approval::ApprovalMode,
        control::{ProviderTools, ToolCall},
        permissions::{PermissionRequest, PermissionResponse, ProviderPermissions},
        session::InvocationConfiguration,
        submission::InputReceipt,
        turn_control::{HostTurnStatus, TurnControl},
    };

    /// Allocate one immutable host operation identity; dropping its source never requests cancellation.
    fn context() -> OperationContext {
        let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
        OperationContext::new(
            forge_provider_api::OperationId::new(uuid::Uuid::new_v4().to_string()).unwrap(),
            std::time::Duration::from_secs(10),
            cancellation,
        )
        .unwrap()
    }

    /// Build a real adapter and RPC dispatcher against an isolated native wire fixture.
    async fn compaction_fixture() -> (
        CodexProvider,
        Arc<CodexBinding>,
        ProviderEventStream,
        tokio_tungstenite::WebSocketStream<tokio::net::TcpStream>,
    ) {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let endpoint = format!("ws://{}", listener.local_addr().unwrap());
        let accept = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            tokio_tungstenite::accept_async(stream).await.unwrap()
        });
        let connection = super::super::connection::CodexConnection::connect(&endpoint)
            .await
            .unwrap();
        let socket = accept.await.unwrap();
        let provider =
            CodexProvider::new(vec!["unused-fixture-command".into()], PathBuf::new()).unwrap();
        let owner = request().scope.owner;
        let binding = SessionBinding {
            id: BindingId::new("compaction-binding").unwrap(),
            provider_session_id: Some("compaction-thread".try_into().unwrap()),
        };
        let (sink, stream) = events::channel(
            binding.id.clone(),
            forge_provider_api::EventScope::Binding {
                owner: owner.clone(),
            },
        );
        let session = Arc::new(CodexBinding {
            binding: binding.clone(),
            history: std::sync::Mutex::new(NativeHistory::Established(
                "compaction-thread".try_into().unwrap(),
            )),
            native_tools: Vec::new(),
            opened_by: forge_provider_api::OperationId::new("fixture-open").unwrap(),
            owner: owner.clone(),
            workspace: String::new(),
            model: Mutex::new(ModelSelection::default()),
            runtime: Mutex::new(Arc::new(CodexRuntime::new(vec![
                "unused-binding-fixture-command".into(),
            ]))),
            rpc: RpcHandle::new(NativeRpc::new(connection)),
            sink,
            stream: Mutex::new(None),
            normalization: Mutex::new(CodexNormalization::default()),
            mcp_context: Mutex::new(super::super::protocol::McpContext::default()),
            resources: Mutex::new(CodexResources::new(Default::default(), PathBuf::new())),
            profile: Mutex::new(None),
            _lease: None,
            system_instructions: String::new(),
            access: Mutex::new(AccessPolicy::default()),
            approval: Mutex::new(ApprovalMode::Ask),
            approval_context: super::super::approval::ApprovalContext::default(),
            active: Mutex::new(None),
            admitting: Mutex::new(None),
            background_reader: Mutex::new(None),
            terminal_observer: Mutex::new(None),
            rollback: Mutex::new(None),
            admission: Mutex::new(()),
            observations: Arc::new(tokio::sync::RwLock::new(false)),
            fenced: AtomicBool::new(false),
            invalidated: AtomicBool::new(false),
            closing: AtomicBool::new(false),
            provider_closed: provider.closed.clone(),
            isolated: false,
            isolation_auth: None,
            configuration: Mutex::new(forge_provider_api::SessionConfiguration {
                model: ModelSelection::default(),
                access: AccessPolicy::default(),
                approval: ApprovalMode::Ask,
                resources: Default::default(),
            }),
            capabilities: codex_capabilities(),
            settlements: Mutex::new(HashMap::new()),
            outcomes: Mutex::new(HashMap::new()),
            close_failure: Mutex::new(None),
            close_lock: Mutex::new(()),
            settlement_lock: Mutex::new(()),
            retired_rpcs: Mutex::new(Vec::new()),
            retired_profiles: Mutex::new(Vec::new()),
        });
        provider
            .bindings
            .lock()
            .await
            .insert(binding.id.clone(), session.clone());
        provider.owners.lock().await.insert(owner, binding.id);
        (provider, session, stream, socket)
    }

    struct ConfigureWireState {
        id: String,
        loaded: bool,
        virgin: bool,
        applied: usize,
    }

    /// Model the pinned native distinction between an unpersisted fresh thread and established history.
    async fn serve_configuration_wire(
        mut socket: tokio_tungstenite::WebSocketStream<tokio::net::TcpStream>,
        state: Arc<std::sync::Mutex<ConfigureWireState>>,
    ) {
        use futures_util::{SinkExt, StreamExt};
        while let Some(message) = socket.next().await {
            let message = message.unwrap();
            if matches!(message, tokio_tungstenite::tungstenite::Message::Close(_)) {
                break;
            }
            let request: Value = serde_json::from_str(message.to_text().unwrap()).unwrap();
            let Some(id) = request.get("id") else {
                assert_eq!(request["method"], "initialized");
                continue;
            };
            let result = {
                let mut state = state.lock().unwrap();
                match request["method"].as_str().unwrap() {
                    "initialize" => json!({"userAgent":"forge/0.164.0 fixture"}),
                    "thread/backgroundTerminals/list" => json!({"data":[]}),
                    "model/list" => {
                        json!({"data":[{"model":"native-model","isDefault":true,"supportedReasoningEfforts":[{"reasoningEffort":"low"},{"reasoningEffort":"medium"}],"defaultReasoningEffort":"medium"}]})
                    }
                    "config/read" => {
                        json!({"config":{"mcp_servers":{"inherited":{"enabled":true}},"plugins":{}}})
                    }
                    "skills/extraRoots/set" => json!({}),
                    "skills/list" => json!({"data":[]}),
                    "thread/unsubscribe" => {
                        assert_eq!(request["params"]["threadId"], state.id);
                        assert!(state.loaded);
                        state.loaded = false;
                        json!({"status":"unsubscribed"})
                    }
                    "thread/loaded/list" => {
                        assert!(!state.loaded);
                        json!({"data":[]})
                    }
                    "thread/start" | "thread/resume" => {
                        assert!(
                            !state.loaded,
                            "resume overrides on a loaded native writer would be ignored"
                        );
                        assert_eq!(
                            request["method"],
                            if state.virgin {
                                "thread/start"
                            } else {
                                "thread/resume"
                            }
                        );
                        if state.virgin {
                            assert_eq!(request["params"]["dynamicTools"], json!([]));
                            state.id = format!("virgin-replacement-{}", state.applied + 1);
                        } else {
                            assert_eq!(request["params"]["threadId"], state.id);
                        }
                        assert_eq!(
                            request["params"]["config"]["mcp_servers"]["inherited"]["enabled"],
                            false
                        );
                        assert_eq!(request["params"]["config"]["thread_unload_delay_secs"], 0);
                        assert_eq!(request["params"]["config"]["features.goals"], false);
                        assert_eq!(
                            request["params"]["config"]["model_reasoning_effort"],
                            if state.applied == 0 { "low" } else { "medium" }
                        );
                        state.applied += 1;
                        state.loaded = true;
                        json!({"thread":{"id":state.id}})
                    }
                    method => panic!("unexpected configuration operation {method}"),
                }
            };
            socket
                .send(tokio_tungstenite::tungstenite::Message::Text(
                    json!({"id":id,"result":result}).to_string().into(),
                ))
                .await
                .unwrap();
        }
    }

    /// Exercise complete configuration through real dispatcher replacement and stable public stream ownership.
    async fn configure_history_fixture(virgin: bool, cycles: usize) {
        let (mut provider, session, mut events, original_socket) = compaction_fixture().await;
        let directory = tempfile::tempdir().unwrap();
        provider.runtime_root = directory.path().to_path_buf();
        let original_id = session.history_id();
        if virgin {
            *session.history.lock().unwrap() = NativeHistory::Virgin(original_id.clone());
        }
        let state = Arc::new(std::sync::Mutex::new(ConfigureWireState {
            id: original_id.as_str().into(),
            loaded: true,
            virgin,
            applied: 0,
        }));
        let original = tokio::spawn(serve_configuration_wire(original_socket, state.clone()));
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        *session.runtime.lock().await = Arc::new(CodexRuntime::fixture(format!(
            "ws://{}",
            listener.local_addr().unwrap()
        )));
        let accepted_state = state.clone();
        let accepted = tokio::spawn(async move {
            let mut peers = Vec::new();
            for _ in 0..cycles {
                let (socket, _) = listener.accept().await.unwrap();
                let socket = tokio_tungstenite::accept_async(socket).await.unwrap();
                peers.push(tokio::spawn(serve_configuration_wire(
                    socket,
                    accepted_state.clone(),
                )));
            }
            for peer in peers {
                peer.await.unwrap();
            }
        });
        let public_binding = session.binding.id.clone();
        for cycle in 0..cycles {
            let mut configuration = session.configuration.lock().await.clone();
            configuration.model.reasoning =
                ReasoningSelection::Level(if cycle == 0 { "low" } else { "medium" }.into());
            let configured = provider
                .session_configure(context(), &public_binding, configuration.clone())
                .await
                .unwrap();
            assert_eq!(configured.configuration, configuration);
            assert_eq!(session.binding.id, public_binding);
            assert!(!session.is_fenced());
            if virgin {
                let expected = format!("virgin-replacement-{}", cycle + 1);
                assert_eq!(session.history_id().as_str(), expected);
                let identity = events.try_recv().unwrap();
                assert_eq!(identity.binding_id, public_binding);
                assert_eq!(identity.sequence, configured.watermark);
                assert!(
                    matches!(identity.event, ProviderEvent::SessionIdentity { provider_session_id, .. } if provider_session_id.as_str() == expected)
                );
                let token =
                    recovery_token(&session, &request().submission_id, &request().attempt_id)
                        .unwrap();
                assert_eq!(
                    parse_recovery_token(Some(&token)).unwrap().thread_id,
                    expected
                );
            } else {
                assert_eq!(session.history_id(), original_id);
                assert!(matches!(
                    events.try_recv(),
                    Err(events::EventDeliveryError::Empty)
                ));
            }
        }
        assert_eq!(state.lock().unwrap().applied, cycles);
        session.rpc.lock().await.close().await.unwrap();
        original.await.unwrap();
        accepted.await.unwrap();
    }

    /// Untouched history can be configured repeatedly without resuming a nonexistent rollout or changing its public generation.
    #[tokio::test]
    async fn configure_virgin_history_twice_preserves_binding_and_publishes_native_identity() {
        tokio::time::timeout(
            std::time::Duration::from_secs(5),
            configure_history_fixture(true, 2),
        )
        .await
        .unwrap();
    }

    /// Established history keeps its native identity and applies settings only after its former writer is collected.
    #[tokio::test]
    async fn configure_established_history_collects_writer_before_resuming_same_identity() {
        tokio::time::timeout(
            std::time::Duration::from_secs(5),
            configure_history_fixture(false, 1),
        )
        .await
        .unwrap();
    }

    /// Completed generation remains compactable even before its terminal publisher has returned.
    #[tokio::test]
    async fn compaction_collects_terminal_worker_and_preserves_native_failure_category() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let (release, released) = tokio::sync::oneshot::channel();
            let task = Arc::new(crate::retained_task::RetainedTask::new(ProviderOperation::InputSubmit, tokio::spawn(async move {
                released.await.unwrap(); Ok(())
            })));
            let (cancel, mut cancelled) = mpsc::unbounded_channel();
            let request = request();
            session.outcomes.lock().await.insert(request.scope.clone(), ExecutionResult::Completed { completion: completion(&session, &request.configuration.model).await });
            *session.active.lock().await = Some(ActiveExecution {
                scope: request.scope.clone(), turn_id: "finished-turn".into(),
                turn_updates: watch::channel(None).0, submissions: Arc::new(Mutex::new(Vec::new())),
                request, cancel, task: Some(task),
            });
            let compact = |operation: &OperationContext| forge_provider_api::CompactionRequest {
                scope: forge_provider_api::EventScope::SessionOperation {
                    owner: session.owner.clone(), operation_id: operation.id().clone(),
                },
            };
            let operation = context();
            let mut pending = Box::pin(provider.session_compact(operation.clone(), &session.binding.id, compact(&operation)));
            assert!(futures_util::poll!(pending.as_mut()).is_pending());
            assert!(cancelled.try_recv().is_err(), "compaction cancelled a finished generation");
            release.send(()).unwrap();
            let wire = tokio::spawn(async move {
                for rejected in [false, true] {
                    let received: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                    assert_eq!(received["method"], "thread/compact/start");
                    let reply = if rejected {
                        json!({"id":received["id"],"error":{"code":-32600,"message":"private native rejection"}})
                    } else { json!({"id":received["id"],"result":{}}) };
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(reply.to_string().into())).await.unwrap();
                    if !rejected {
                        socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({
                            "method":"item/completed","params":{"threadId":"compaction-thread",
                            "item":{"id":"native-compaction","type":"contextCompaction"}}
                        }).to_string().into())).await.unwrap();
                    }
                }
                // Keep transport live until the adapter explicitly closes it.
                while let Some(message) = socket.next().await {
                    if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; }
                }
            });
            assert_eq!(pending.await.unwrap().compaction.state, forge_provider_api::CompactionState::Completed);
            assert!(session.active.lock().await.is_none());
            let operation = context();
            let error = provider.session_compact(operation.clone(), &session.binding.id, compact(&operation)).await.unwrap_err();
            assert!(matches!(&error, ProviderError::Failure(failure)
                if failure.operation() == ProviderOperation::SessionCompact && failure.kind() == ProviderFailureKind::Rejected));
            assert!(!error.to_string().contains("private native rejection"));
            session.rpc.lock().await.close().await.unwrap();
            wire.await.unwrap();
        }).await.unwrap();
    }

    /// Compaction preserves the caller's complete budget and collects uncertain work at that deadline.
    #[tokio::test]
    async fn caller_compaction_timeout_preserves_uncertainty_and_collects_native_binding() {
        use futures_util::{SinkExt, StreamExt};
        let (provider, session, mut events, mut socket) = compaction_fixture().await;
        let native = tokio::spawn(async move {
            let request: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(request["method"], "thread/compact/start");
            socket
                .send(tokio_tungstenite::tungstenite::Message::Text(
                    json!({"id":request["id"],"result":{}}).to_string().into(),
                ))
                .await
                .unwrap();
            socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"item/started","params":{"threadId":"compaction-thread","item":{"id":"unfinished-compaction","type":"contextCompaction"}}}).to_string().into())).await.unwrap();
            while let Some(message) = socket.next().await {
                if matches!(
                    message,
                    Ok(tokio_tungstenite::tungstenite::Message::Close(_))
                ) {
                    break;
                }
            }
        });
        let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
        let operation = OperationContext::new(
            forge_provider_api::OperationId::new("caller-compaction-timeout").unwrap(),
            std::time::Duration::from_secs(150),
            cancellation,
        )
        .unwrap();
        let request = forge_provider_api::CompactionRequest {
            scope: forge_provider_api::EventScope::SessionOperation {
                owner: session.owner.clone(),
                operation_id: operation.id().clone(),
            },
        };
        let compacting_provider = provider.clone();
        let binding = session.binding.id.clone();
        let compacting_binding = binding.clone();
        let compacting_operation = operation.clone();
        let compacting = tokio::spawn(async move {
            compacting_provider
                .session_compact(compacting_operation, &compacting_binding, request)
                .await
        });
        // Wait for correlated native progress before pausing time, so socket readiness
        // cannot race an automatically advanced RPC acknowledgement deadline.
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            loop {
                let event = events.recv().await.unwrap().expect("compaction observation remains live");
                if matches!(event.event, ProviderEvent::Compaction { update, .. } if update.state == forge_provider_api::CompactionState::Running) { break; }
            }
        }).await.unwrap();
        tokio::time::pause();
        tokio::time::advance(std::time::Duration::from_secs(121)).await;
        tokio::task::yield_now().await;
        assert!(!compacting.is_finished());
        tokio::time::advance(std::time::Duration::from_secs(30)).await;
        tokio::time::resume();
        let error = tokio::time::timeout(std::time::Duration::from_secs(5), compacting)
            .await
            .unwrap()
            .unwrap()
            .unwrap_err();
        assert!(
            matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionCompact && failure.kind() == ProviderFailureKind::Timeout && failure.phase() == forge_provider_api::ProviderPhase::Observation && failure.disposition() == forge_provider_api::FailureDisposition::Uncertain)
        );
        provider.records.compact.join_all().await.unwrap();
        assert!(
            operation
                .check_admission(ProviderOperation::SessionCompact)
                .is_err()
        );
        assert!(session.invalidated.load(Ordering::Acquire));
        assert!(provider.closed_bindings.lock().await.contains(&binding));
        tokio::time::timeout(std::time::Duration::from_secs(5), native)
            .await
            .unwrap()
            .unwrap();
    }

    /// Explicit cancellation collects an unacknowledged compaction without touching sibling or discovery owners.
    #[tokio::test]
    async fn cancelled_compaction_without_acknowledgment_collects_only_its_binding() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut wire) = compaction_fixture().await;
            let (sibling_provider, mut sibling, _sibling_events, sibling_wire) = compaction_fixture().await;
            sibling_provider.bindings.lock().await.remove(&sibling.binding.id);
            let sibling_id = BindingId::new("sibling-binding").unwrap();
            let sibling_owner = forge_provider_api::ExecutionOwner::root("sibling-owner");
            let (sink, _sibling_output) = events::channel(sibling_id.clone(), forge_provider_api::EventScope::Binding { owner: sibling_owner.clone() });
            let sibling_mut = Arc::get_mut(&mut sibling).expect("unregistered fixture binding has one owner");
            sibling_mut.binding.id = sibling_id.clone();
            sibling_mut.binding.provider_session_id = Some("sibling-thread".try_into().unwrap());
            sibling_mut.owner = sibling_owner.clone(); sibling_mut.sink = sink; sibling_mut.provider_closed = provider.closed.clone();
            provider.bindings.lock().await.insert(sibling_id.clone(), sibling.clone());
            provider.owners.lock().await.insert(sibling_owner, sibling_id.clone());
            let (_discovery_provider, discovery_session, _discovery_events, discovery_wire) = compaction_fixture().await;
            let discovery = discovery_session.rpc.lock().await.shared();
            *provider.catalog.lock().await = Some(CatalogConnection::Ready(discovery.clone()));
            provider.discovery.lock().await.push(discovery.clone());
            let runtime = session.runtime.lock().await.clone();
            let sibling_runtime = sibling.runtime.lock().await.clone();
            assert!(!Arc::ptr_eq(&runtime, &provider.runtime));
            assert!(!Arc::ptr_eq(&runtime, &sibling_runtime));
            let peers = [sibling_wire, discovery_wire].into_iter().map(|mut peer| tokio::spawn(async move {
                let request: Value = serde_json::from_str(peer.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(request["method"], "model/list");
                peer.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":request["id"],"result":{"data":[]}}).to_string().into())).await.unwrap();
                while let Some(message) = peer.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            })).collect::<Vec<_>>();
            let (dispatched, started) = tokio::sync::oneshot::channel();
            let native = tokio::spawn(async move {
                let request: Value = serde_json::from_str(wire.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(request["method"], "thread/compact/start");
                dispatched.send(()).unwrap();
                while let Some(message) = wire.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            let (source, cancellation) = forge_provider_api::OperationCancellation::channel();
            let operation = OperationContext::new(forge_provider_api::OperationId::new("cancel-unacknowledged-compaction").unwrap(), std::time::Duration::from_secs(5), cancellation).unwrap();
            let request = forge_provider_api::CompactionRequest { scope: forge_provider_api::EventScope::SessionOperation { owner: session.owner.clone(), operation_id: operation.id().clone() } };
            let compacting_provider = provider.clone(); let binding = session.binding.id.clone(); let compacting_binding = binding.clone();
            let compacting = tokio::spawn(async move { compacting_provider.session_compact(operation, &compacting_binding, request).await });
            started.await.unwrap(); source.cancel();
            let error = compacting.await.unwrap().unwrap_err();
            let primary = match &error { ProviderError::Combined { primary, .. } => primary.as_ref(), error => error };
            assert!(matches!(primary, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Cancelled && failure.disposition() != forge_provider_api::FailureDisposition::Unchanged));
            provider.records.compact.join_all().await.unwrap();
            assert!(session.invalidated.load(Ordering::Acquire));
            assert!(provider.closed_bindings.lock().await.contains(&binding)); native.await.unwrap();
            assert!(provider.bindings.lock().await.contains_key(&sibling_id)); assert!(!sibling.is_fenced());
            sibling.rpc.lock().await.request("model/list", json!({}), ProviderOperation::ModelList).await.unwrap();
            discovery.request("model/list", json!({}), ProviderOperation::ModelList).await.unwrap();
            sibling.rpc.lock().await.close().await.unwrap(); discovery.close().await.unwrap();
            for peer in peers { peer.await.unwrap(); }
        }).await.unwrap();
    }

    /// A substituted event operation identity cannot hand off the reader or dispatch native compaction.
    #[tokio::test]
    async fn compaction_rejects_scope_context_identity_substitution() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        let error = provider
            .session_compact(
                context(),
                &session.binding.id,
                forge_provider_api::CompactionRequest {
                    scope: forge_provider_api::EventScope::SessionOperation {
                        owner: session.owner.clone(),
                        operation_id: forge_provider_api::OperationId::new("substituted-operation")
                            .unwrap(),
                    },
                },
            )
            .await
            .unwrap_err();
        assert!(
            matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionCompact && failure.kind() == ProviderFailureKind::OperationConflict)
        );
        assert_eq!(session.sink.watermark().get(), 0);
        assert!(!session.is_fenced());
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Compaction cannot collect, cancel, or replace a genuinely running generation.
    #[tokio::test]
    async fn compaction_rejects_active_turn_without_cancellation() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        let (release, released) = tokio::sync::oneshot::channel();
        let task = Arc::new(crate::retained_task::RetainedTask::new(
            ProviderOperation::InputSubmit,
            tokio::spawn(async move {
                released.await.unwrap();
                Ok(())
            }),
        ));
        let (cancel, mut cancelled) = mpsc::unbounded_channel();
        let request = request();
        *session.active.lock().await = Some(ActiveExecution {
            scope: request.scope.clone(),
            turn_id: "running-turn".into(),
            turn_updates: watch::channel(Some("running-turn".into())).0,
            submissions: Arc::new(Mutex::new(Vec::new())),
            request,
            cancel,
            task: Some(task.clone()),
        });
        let operation = context();
        let error = provider
            .session_compact(
                operation.clone(),
                &session.binding.id,
                forge_provider_api::CompactionRequest {
                    scope: forge_provider_api::EventScope::SessionOperation {
                        owner: session.owner.clone(),
                        operation_id: operation.id().clone(),
                    },
                },
            )
            .await
            .unwrap_err();
        assert!(matches!(error, ProviderError::Failure(failure)
            if failure.operation() == ProviderOperation::SessionCompact && failure.kind() == ProviderFailureKind::ExecutionActive));
        assert!(session.active.lock().await.is_some());
        assert!(cancelled.try_recv().is_err());
        release.send(()).unwrap();
        task.join().await.unwrap();
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Compaction and fork distinguish revoked binding authority without dispatching native reads.
    #[tokio::test]
    async fn history_mutations_preserve_revoked_binding_preconditions() {
        for closing in [false, true] {
            let (provider, session, _events, _socket) = compaction_fixture().await;
            session.invalidated.store(!closing, Ordering::Release);
            session.closing.store(closing, Ordering::Release);
            let expected = if closing {
                ProviderFailureKind::BindingClosed
            } else {
                ProviderFailureKind::BindingInvalidated
            };
            let operation = context();
            let compact = forge_provider_api::CompactionRequest {
                scope: forge_provider_api::EventScope::SessionOperation {
                    owner: session.owner.clone(),
                    operation_id: operation.id().clone(),
                },
            };
            let error = provider
                .session_compact(operation, &session.binding.id, compact)
                .await
                .unwrap_err();
            assert!(matches!(error, ProviderError::Failure(failure)
                if failure.kind() == expected
                    && failure.operation() == ProviderOperation::SessionCompact));
            let fork = forge_provider_api::SessionForkRequest {
                source: forge_provider_api::ForkSource::Binding(session.binding.id.clone()),
                target: forge_provider_api::ForkTarget {
                    owner: forge_provider_api::ExecutionOwner::root("revoked-fork-target"),
                    workspace: "workspace".into(),
                    configuration: session.configuration.lock().await.clone(),
                    system_instructions: String::new(),
                },
            };
            let error = provider.session_fork(context(), fork).await.unwrap_err();
            assert!(matches!(error, ProviderError::Failure(failure)
                if failure.kind() == expected
                    && failure.operation() == ProviderOperation::SessionFork));
            assert_eq!(session.sink.watermark().get(), 0);
            assert!(provider.fork_targets.lock().await.is_empty());
            session.rpc.lock().await.close().await.unwrap();
        }
    }

    /// Collect the shared runtime even when catalog initialization has already failed.
    #[tokio::test]
    async fn shutdown_closes_runtime_after_catalog_failure() {
        let root = tempfile::tempdir().unwrap();
        let provider = CodexProvider::new(
            vec![
                root.path()
                    .join("missing-provider")
                    .to_string_lossy()
                    .into_owned(),
            ],
            root.path().to_owned(),
        )
        .unwrap();
        *provider.catalog.lock().await = Some(CatalogConnection::Starting(tokio::spawn(async {
            Err(failure(
                ProviderOperation::Shutdown,
                ProviderFailureKind::Transport,
            ))
        })));

        assert!(provider.shutdown(context()).await.is_err());
        assert!(provider.catalog.lock().await.is_none());
        let error = match provider.runtime.connect().await {
            Ok(_) => panic!("shutdown runtime admitted another connection"),
            Err(error) => error,
        };
        assert_eq!(error.to_string(), "shared Codex runtime is shut down");
        provider.shutdown(context()).await.unwrap();
    }

    /// Concurrent resumes under different owners cannot acquire the same retained durable history.
    #[tokio::test]
    async fn duplicate_resume_is_excluded_until_invalidated_history_owner_is_collected() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        session.fenced.store(true, Ordering::Release);
        let request = |owner| SessionRequest {
            owner: forge_provider_api::ExecutionOwner::root(owner),
            mode: forge_provider_api::SessionMode::Interactive,
            workspace: "workspace".into(),
            history: forge_provider_api::SessionHistory::Durable {
                resume: session.binding.provider_session_id.clone(),
            },
            configuration: forge_provider_api::SessionConfiguration {
                model: ModelSelection::default(),
                access: AccessPolicy::default(),
                approval: ApprovalMode::Ask,
                resources: Default::default(),
            },
            resource_lease: None,
            tools: Vec::new(),
            tool_endpoint: None,
            system_instructions: String::new(),
            trace: Arc::new(FixtureAuthority),
        };
        let (first, second) = tokio::join!(
            provider.session_open(context(), request("first-owner")),
            provider.session_open(context(), request("second-owner"))
        );
        for opened in [first, second] {
            let error = match opened {
                Ok(_) => panic!("duplicate writable history binding admitted"),
                Err(error) => error,
            };
            assert!(
                matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionOpen && failure.kind() == ProviderFailureKind::ResourceConflict)
            );
        }
        assert_eq!(provider.bindings.lock().await.len(), 1);
        assert!(provider.pending.lock().await.is_empty());
        assert!(provider.runtime.process_id().await.is_none());
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Queue loss must retain its actual category instead of becoming a false disconnect.
    #[tokio::test]
    async fn observation_loss_preserves_overflow_in_events_and_return() {
        use futures_util::{SinkExt, StreamExt};
        let (provider, session, mut events, mut wire) = compaction_fixture().await;
        let peer = tokio::spawn(async move {
            let request: Value =
                serde_json::from_str(wire.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(request["method"], "turn/interrupt");
            wire.send(tokio_tungstenite::tungstenite::Message::Text(
                json!({"id":request["id"],"result":{}}).to_string().into(),
            ))
            .await
            .unwrap();
            while let Some(Ok(message)) = wire.next().await {
                if message.is_close() {
                    break;
                }
            }
        });
        let input = request();
        let address = forge_provider_api::TurnAddress {
            scope: input.scope.clone(),
            thread_id: session.history_id().to_string(),
            turn_id: "overflow-turn".into(),
        };
        let error = lose_observation(
            &session,
            &input,
            &address,
            &super::super::descendants::DescendantTracker::default(),
            ProviderError::Observation(forge_provider_api::ObservationFailure::Overflow),
            forge_provider_api::ObservationFailure::Disconnected,
        )
        .await;
        assert!(matches!(
            error,
            ProviderError::Observation(forge_provider_api::ObservationFailure::Overflow)
        ));
        let event = events.recv().await.unwrap().unwrap();
        assert!(
            matches!(event.event, ProviderEvent::Execution { observation }
            if matches!(observation.update(), ExecutionUpdate::ObservationLost { failure: forge_provider_api::ObservationFailure::Overflow, .. }))
        );
        assert!(matches!(
            events.recv().await,
            Err(events::EventDeliveryError::Observation(
                forge_provider_api::ObservationFailure::Overflow
            ))
        ));
        session.rpc.lock().await.close().await.unwrap();
        peer.await.unwrap();
        drop(provider);
    }

    /// Wait out provisional rollback fences before observing or terminating a shell.
    #[tokio::test]
    async fn terminal_operations_wait_for_lifecycle_admission_before_checking_fences() {
        use futures_util::{SinkExt, StreamExt};
        use std::time::Duration;
        let (provider, session, _events, mut wire) = compaction_fixture().await;
        let admission = session.admission.lock().await;
        session.fenced.store(true, Ordering::Release);
        let querying = provider.clone();
        let binding = session.binding.id.clone();
        let mut list = tokio::spawn(async move { querying.read_terminals(&binding).await });
        let stopping = provider.clone();
        let binding = session.binding.id.clone();
        let terminal = forge_provider_api::terminal::TerminalId::new(
            json!([binding.as_str(), "process"]).to_string(),
        )
        .unwrap();
        let mut terminate = tokio::spawn(async move {
            stopping
                .terminate_binding_terminal(&binding, &terminal)
                .await
        });
        assert!(
            tokio::time::timeout(Duration::from_millis(20), &mut list)
                .await
                .is_err()
        );
        assert!(
            tokio::time::timeout(Duration::from_millis(20), &mut terminate)
                .await
                .is_err()
        );
        assert!(
            tokio::time::timeout(Duration::from_millis(20), wire.next())
                .await
                .is_err()
        );
        let peer = tokio::spawn(async move {
            for _ in 0..2 {
                let request: Value =
                    serde_json::from_str(wire.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                let result = match request["method"].as_str().unwrap() {
                    "thread/backgroundTerminals/list" => json!({"data":[]}),
                    "thread/backgroundTerminals/terminate" => json!({"terminated":true}),
                    method => panic!("unexpected terminal method: {method}"),
                };
                wire.send(tokio_tungstenite::tungstenite::Message::Text(
                    json!({"id":request["id"],"result":result})
                        .to_string()
                        .into(),
                ))
                .await
                .unwrap();
            }
            while let Some(Ok(message)) = wire.next().await {
                if message.is_close() {
                    break;
                }
            }
        });
        session.fenced.store(false, Ordering::Release);
        drop(admission);
        assert!(
            tokio::time::timeout(Duration::from_secs(2), list)
                .await
                .unwrap()
                .unwrap()
                .unwrap()
                .terminal
                .is_empty()
        );
        tokio::time::timeout(Duration::from_secs(2), terminate)
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        session.fenced.store(true, Ordering::Release);
        assert!(matches!(provider.read_terminals(&session.binding.id).await,
            Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingClosed));
        session.rpc.lock().await.close().await.unwrap();
        peer.await.unwrap();
    }

    /// Reject unsupported settings at every ingress before touching native or profile ownership.
    #[tokio::test]
    async fn unsupported_options_reject_before_native_or_profile_changes() {
        use forge_provider_api::capability::{ProviderFeature, UnsupportedCapability};
        use futures_util::StreamExt;
        let (mut provider, session, _events, mut wire) = compaction_fixture().await;
        let directory = tempfile::tempdir().unwrap();
        provider.runtime_root = directory.path().to_owned();
        let mut cases = Vec::new();
        let mut context_window = session.configuration.lock().await.clone();
        context_window.model.context_window = Some("unavailable".into());
        cases.push((context_window, ProviderFeature::ContextWindowSelection));
        let mut isolation = session.configuration.lock().await.clone();
        isolation.access.isolation_mode = Some("unsupported".into());
        cases.push((isolation, ProviderFeature::IsolationMode));
        for (configuration, expected) in cases {
            let request = SessionRequest {
                owner: forge_provider_api::ExecutionOwner::root("invalid-open"),
                mode: forge_provider_api::SessionMode::Interactive,
                workspace: directory.path().to_string_lossy().into_owned(),
                history: forge_provider_api::SessionHistory::Durable { resume: None },
                configuration: configuration.clone(),
                resource_lease: None,
                tools: Vec::new(),
                tool_endpoint: None,
                system_instructions: String::new(),
                trace: Arc::new(FixtureAuthority),
            };
            assert!(matches!(provider.session_open(context(), request).await,
                Err(ProviderError::Unsupported(UnsupportedCapability(feature))) if feature == expected));
            assert!(
                matches!(provider.session_configure(context(), &session.binding.id, configuration.clone()).await,
                Err(ProviderError::Unsupported(UnsupportedCapability(feature))) if feature == expected)
            );
            assert!(
                matches!(provider.session_fork(context(), forge_provider_api::SessionForkRequest {
                source: forge_provider_api::ForkSource::Checkpoint { provider_session_id: "unknown".try_into().unwrap(), checkpoint: forge_provider_api::CheckpointId::new("checkpoint").unwrap() },
                target: forge_provider_api::ForkTarget { owner: forge_provider_api::ExecutionOwner::root("invalid-fork"),
                    workspace: directory.path().to_string_lossy().into_owned(), system_instructions: String::new(), configuration: configuration.clone() },
            }).await, Err(ProviderError::Unsupported(UnsupportedCapability(feature))) if feature == expected)
            );
            let mut prompt = self::request();
            prompt.configuration.model = configuration.model;
            prompt.configuration.access = configuration.access;
            prompt.dispatch = forge_provider_api::submission::DispatchReporter::channel(
                session.binding.id.clone(),
                prompt.scope.clone(),
                prompt.submission_id.clone(),
                prompt.attempt_id.clone(),
            )
            .0;
            assert!(
                matches!(provider.input_submit(context(), &session.binding.id, prompt).await,
                Err(ProviderError::Unsupported(UnsupportedCapability(feature))) if feature == expected)
            );
            assert!(!session.is_fenced());
            assert!(session.admitting.lock().await.is_none());
            assert!(session.active.lock().await.is_none());
            assert!(session.retired_profiles.lock().await.is_empty());
            assert!(provider.preparing.lock().await.is_empty());
            assert!(provider.pending.lock().await.is_empty());
            assert!(provider.catalog.lock().await.is_none());
            assert_eq!(std::fs::read_dir(directory.path()).unwrap().count(), 0);
        }
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(20), wire.next())
                .await
                .is_err(),
            "preflight sent native I/O"
        );
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Skill preflight failure collects the already-retained private profile without starting native discovery.
    #[tokio::test]
    async fn open_profile_is_retained_before_skill_preflight_and_collected_on_rejection() {
        let directory = tempfile::tempdir().unwrap();
        let provider = CodexProvider::new(
            vec!["unused-fixture-command".into()],
            directory.path().to_path_buf(),
        )
        .unwrap();
        let error = match provider
            .session_open(
                context(),
                SessionRequest {
                    owner: forge_provider_api::ExecutionOwner::root("profile-rejection"),
                    mode: forge_provider_api::SessionMode::Interactive,
                    workspace: "workspace".into(),
                    history: forge_provider_api::SessionHistory::Durable { resume: None },
                    configuration: forge_provider_api::SessionConfiguration {
                        model: ModelSelection::default(),
                        access: AccessPolicy::default(),
                        approval: ApprovalMode::Ask,
                        resources: forge_provider_api::ResourceSelection {
                            skills: vec![forge_provider_api::catalog::SkillDefinition {
                                resource_id: None,
                                scope: None,
                                name: "invalid/name".into(),
                                description: String::new(),
                                enabled: true,
                                user_invocable: true,
                                path: None,
                                source: None,
                                argument_hint: None,
                            }],
                            mcp: Vec::new(),
                        },
                    },
                    resource_lease: None,
                    tools: Vec::new(),
                    tool_endpoint: None,
                    system_instructions: String::new(),
                    trace: Arc::new(FixtureAuthority),
                },
            )
            .await
        {
            Ok(_) => panic!("invalid skill profile admitted"),
            Err(error) => error,
        };
        assert!(
            matches!(error, ProviderError::Failure(cause) if cause.operation() == ProviderOperation::SessionOpen && cause.kind() == ProviderFailureKind::Rejected)
        );
        assert!(provider.preparing.lock().await.is_empty());
        assert!(provider.pending.lock().await.is_empty());
        assert!(provider.catalog.lock().await.is_none());
        assert_eq!(
            std::fs::read_dir(directory.path().join("provider-runtime/codex"))
                .unwrap()
                .count(),
            0
        );
        provider.shutdown(context()).await.unwrap();
    }

    /// Interrupted isolated initialization collects only its exclusive runtime/profile, preserving persistent native RPC authority.
    #[tokio::test]
    async fn isolated_open_cleanup_preserves_persistent_binding_and_shared_discovery() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, persistent, _events, mut wire) = compaction_fixture().await;
            let directory = tempfile::tempdir().unwrap();
            let operation = context();
            let pending_id = BindingId::new("isolated-pending").unwrap();
            let profile =
                crate::profile::ResourceProfile::create(directory.path(), "codex").unwrap();
            provider.pending.lock().await.insert(
                pending_id.clone(),
                Arc::new(PendingOpen {
                    operation_id: operation.id().clone(),
                    history: Mutex::new(None),
                    runtime: Arc::new(CodexRuntime::new(vec!["unused-isolated".into()])),
                    rpc: Mutex::new(None),
                    profile: Mutex::new(Some(profile)),
                }),
            );
            provider
                .collect_open_operation(operation.id())
                .await
                .unwrap();
            assert!(!provider.pending.lock().await.contains_key(&pending_id));
            assert!(
                provider
                    .bindings
                    .lock()
                    .await
                    .contains_key(&persistent.binding.id)
            );
            assert!(!persistent.is_fenced());
            let peer = tokio::spawn(async move {
                let read: Value =
                    serde_json::from_str(wire.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(read["method"], "model/list");
                wire.send(tokio_tungstenite::tungstenite::Message::Text(
                    json!({"id":read["id"],"result":{"data":[]}})
                        .to_string()
                        .into(),
                ))
                .await
                .unwrap();
                while let Some(message) = wire.next().await {
                    if matches!(
                        message,
                        Ok(tokio_tungstenite::tungstenite::Message::Close(_))
                    ) {
                        break;
                    }
                }
            });
            persistent
                .rpc
                .lock()
                .await
                .request("model/list", json!({}), ProviderOperation::ModelList)
                .await
                .unwrap();
            persistent.rpc.lock().await.close().await.unwrap();
            peer.await.unwrap();
        })
        .await
        .unwrap();
    }

    /// Missing runtime inspection produces a normalized setup result without creating native resources.
    #[tokio::test]
    async fn inspection_missing_executable_is_normalized_without_native_allocation() {
        let directory = tempfile::tempdir().unwrap();
        let provider = CodexProvider::new(
            vec![
                directory
                    .path()
                    .join("missing-codex")
                    .to_string_lossy()
                    .into_owned(),
            ],
            directory.path().to_path_buf(),
        )
        .unwrap();
        let setup = provider
            .inspect(
                context(),
                WorkspaceContext {
                    workspace: directory.path().to_string_lossy().into_owned(),
                },
            )
            .await
            .unwrap();
        assert_eq!(
            setup.runtime,
            forge_provider_api::RuntimeAvailability::Missing
        );
        assert!(provider.catalog.lock().await.is_none());
        assert!(provider.pending.lock().await.is_empty());
    }

    /// Publish shell transitions and dispatcher replacement without periodic inventory requests.
    #[tokio::test]
    async fn terminal_inventory_streams_changes_and_rpc_replacements_without_polling() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (_provider, session, mut events, mut socket) = compaction_fixture().await;
            let observer = start_terminal_observer(&session);
            for (index, native) in [None, Some("native-shell"), None].into_iter().enumerate() {
                let request: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(request["method"], "thread/backgroundTerminals/list");
                let rows = native.map(|id| vec![json!({"processId":id,"command":"echo working"})]).unwrap_or_default();
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":request["id"],"result":{"data":rows}}).to_string().into())).await.unwrap();
                let event = events.recv().await.unwrap().unwrap();
                let ProviderEvent::Terminals { snapshot, .. } = event.event else { panic!("expected inventory"); };
                assert_eq!(snapshot.terminal.len(), usize::from(native.is_some()));
                if let Some(native) = native { assert_ne!(snapshot.terminal[0].id.as_str(), native); }
                if native == Some("native-shell") {
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"process/exited","params":{}}).to_string().into())).await.unwrap();
                } else if index == 0 {
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"item/completed","params":{}}).to_string().into())).await.unwrap();
                }
            }
            let (_other_provider, other, _other_events, mut replacement_socket) = compaction_fixture().await;
            let retired = session.rpc.replace_shared(other.rpc.lock().await.shared()).await;
            let request: Value = serde_json::from_str(replacement_socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
            assert_eq!(request["method"], "thread/backgroundTerminals/list");
            replacement_socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":request["id"],"result":{"data":[{"processId":"replacement-shell","command":"echo next"}]}}).to_string().into())).await.unwrap();
            let ProviderEvent::Terminals { snapshot, .. } = events.recv().await.unwrap().unwrap().event else { panic!("expected replacement inventory"); };
            assert_eq!(snapshot.terminal.len(), 1);
            let rpc = session.rpc.lock().await.shared();
            let querying = tokio::spawn(async move { rpc.request("model/list", json!({}), ProviderOperation::ModelList).await });
            let request: Value = serde_json::from_str(replacement_socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
            assert_eq!(request["method"], "model/list", "idle observer queried inventory again");
            replacement_socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":request["id"],"result":{"data":[]}}).to_string().into())).await.unwrap();
            querying.await.unwrap().unwrap();
            observer.abort();
            assert!(observer.await.unwrap_err().is_cancelled());
            retired.close().await.unwrap();
            session.rpc.lock().await.close().await.unwrap();
        }).await.unwrap();
    }

    /// Native fork acknowledgement cannot expose a target while its writer remains loaded on any listing page.
    #[tokio::test]
    async fn fork_collects_target_writer_before_returning_and_preserves_source() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let operation = context(); let operation_id = operation.id().clone();
            let request = forge_provider_api::SessionForkRequest { source: forge_provider_api::ForkSource::Binding(session.binding.id.clone()), target: forge_provider_api::ForkTarget { owner: forge_provider_api::ExecutionOwner::root("fork-owner"), workspace: "workspace".into(), configuration: session.configuration.lock().await.clone(), system_instructions: "fork instructions".into() } };
            let (waiting, waited) = tokio::sync::oneshot::channel(); let (release, released) = tokio::sync::oneshot::channel();
            let wire = tokio::spawn(async move {
                for (method, result) in [
                    ("model/list", json!({"data":[{"model":"native-model","isDefault":true,"supportedReasoningEfforts":[]}]})),
                    ("thread/fork", json!({"thread":{"id":"fork-target"}})),
                    ("thread/unsubscribe", json!({"status":"unsubscribed"})),
                    ("thread/loaded/list", json!({"data":["compaction-thread"],"nextCursor":"first-page"})),
                    ("thread/loaded/list", json!({"data":["fork-target"]})),
                ] {
                    let received: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap(); assert_eq!(received["method"], method);
                    if method == "thread/fork" { assert_eq!(received["params"]["config"]["thread_unload_delay_secs"], 0); }
                    if method == "thread/unsubscribe" { assert_eq!(received["params"]["threadId"], "fork-target"); }
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":received["id"],"result":result}).to_string().into())).await.unwrap();
                }
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"thread/closed","params":{"threadId":"unrelated"}}).to_string().into())).await.unwrap();
                waiting.send(()).unwrap(); released.await.unwrap();
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"thread/closed","params":{"threadId":"fork-target"}}).to_string().into())).await.unwrap();
                let source_read: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap(); assert_eq!(source_read["method"], "model/list");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":source_read["id"],"result":{"data":[]}}).to_string().into())).await.unwrap();
                while let Some(message) = socket.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            let forking_provider = provider.clone(); let mut forking = tokio::spawn(async move { forking_provider.session_fork(operation, request).await });
            waited.await.unwrap(); assert!(futures_util::poll!(&mut forking).is_pending());
            assert!(provider.fork_targets.lock().await.contains_key(&operation_id));
            let target = forge_provider_api::ProviderSessionId::try_from("fork-target".to_owned()).unwrap();
            assert!(matches!(provider.ensure_history_available(&target).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::ResourceConflict));
            release.send(()).unwrap(); assert_eq!(forking.await.unwrap().unwrap().provider_session_id, target);
            assert!(provider.fork_targets.lock().await.is_empty()); assert!(!session.is_fenced());
            session.rpc.lock().await.request("model/list", json!({}), ProviderOperation::ModelList).await.unwrap();
            session.rpc.lock().await.close().await.unwrap(); wire.await.unwrap();
        }).await.unwrap();
    }

    /// Uncertain release retains the same target and cleanup retries cannot issue a second native fork.
    #[tokio::test]
    async fn fork_release_failure_retains_exact_target_for_cleanup_retry() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let operation = context(); let operation_id = operation.id().clone();
            let request = forge_provider_api::SessionForkRequest { source: forge_provider_api::ForkSource::Binding(session.binding.id.clone()), target: forge_provider_api::ForkTarget { owner: forge_provider_api::ExecutionOwner::root("fork-owner"), workspace: "workspace".into(), configuration: session.configuration.lock().await.clone(), system_instructions: String::new() } };
            let wire = tokio::spawn(async move {
                for (method, reply) in [
                    ("model/list", json!({"result":{"data":[{"model":"native-model","isDefault":true,"supportedReasoningEfforts":[]}]}})),
                    ("thread/fork", json!({"result":{"thread":{"id":"retained-fork-target"}}})),
                    ("thread/unsubscribe", json!({"error":{"code":-32603,"message":"private native release failure"}})),
                    ("thread/unsubscribe", json!({"result":{"status":"notSubscribed"}})),
                    ("thread/loaded/list", json!({"result":{"data":["compaction-thread"]}})),
                ] {
                    let received: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap(); assert_eq!(received["method"], method);
                    let mut reply = reply; reply["id"] = received["id"].clone(); socket.send(tokio_tungstenite::tungstenite::Message::Text(reply.to_string().into())).await.unwrap();
                }
                while let Some(message) = socket.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            assert!(provider.session_fork(operation.clone(), request.clone()).await.is_err());
            assert_eq!(provider.fork_targets.lock().await.get(&operation_id).unwrap().target.as_str(), "retained-fork-target");
            assert!(provider.session_fork(operation, request).await.is_err());
            provider.collect_fork_targets().await.unwrap(); assert!(provider.fork_targets.lock().await.is_empty()); assert!(!session.is_fenced());
            session.rpc.lock().await.close().await.unwrap(); wire.await.unwrap();
        }).await.unwrap();
    }

    /// A malformed listing cursor never proves writer collection or exposes the new durable target.
    #[tokio::test]
    async fn malformed_fork_release_cursor_retains_target_without_exposing_result() {
        use futures_util::{SinkExt, StreamExt};
        for cursor in [json!(7), json!({"invalid":"cursor"}), json!("")] {
            tokio::time::timeout(std::time::Duration::from_secs(5), async {
                let (provider, session, _events, mut socket) = compaction_fixture().await;
                let operation = context(); let operation_id = operation.id().clone();
                let request = forge_provider_api::SessionForkRequest { source: forge_provider_api::ForkSource::Binding(session.binding.id.clone()), target: forge_provider_api::ForkTarget { owner: forge_provider_api::ExecutionOwner::root("fork-owner"), workspace: "workspace".into(), configuration: session.configuration.lock().await.clone(), system_instructions: String::new() } };
                let wire = tokio::spawn(async move {
                    for (method, result) in [
                        ("model/list", json!({"data":[{"model":"native-model","isDefault":true,"supportedReasoningEfforts":[]}]})),
                        ("thread/fork", json!({"thread":{"id":"malformed-cursor-target"}})),
                        ("thread/unsubscribe", json!({"status":"unsubscribed"})),
                        ("thread/loaded/list", json!({"data":["compaction-thread"],"nextCursor":cursor})),
                    ] {
                        let received: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap(); assert_eq!(received["method"], method);
                        socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":received["id"],"result":result}).to_string().into())).await.unwrap();
                    }
                    while let Some(message) = socket.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
                });
                let error = provider.session_fork(operation.clone(), request.clone()).await.unwrap_err();
                assert!(matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionFork && failure.kind() == ProviderFailureKind::Protocol && failure.phase() == forge_provider_api::ProviderPhase::Collection && failure.disposition() == forge_provider_api::FailureDisposition::Uncertain));
                assert_eq!(provider.fork_targets.lock().await.get(&operation_id).unwrap().target.as_str(), "malformed-cursor-target");
                assert!(provider.session_fork(operation, request).await.is_err());
                assert!(!session.is_fenced());
                session.rpc.lock().await.close().await.unwrap(); wire.await.unwrap();
            }).await.unwrap();
        }
    }

    /// A retained fork result cannot be reassigned to a substituted source, owner, or resource selection.
    #[tokio::test]
    async fn fork_retry_rejects_changed_payload_without_creating_a_second_target() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        let request = forge_provider_api::SessionForkRequest {
            source: forge_provider_api::ForkSource::Binding(session.binding.id.clone()),
            target: forge_provider_api::ForkTarget {
                owner: forge_provider_api::ExecutionOwner::root("target"),
                workspace: "workspace".into(),
                configuration: session.configuration.lock().await.clone(),
                system_instructions: "supplied instructions".into(),
            },
        };
        let operation = context();
        provider
            .records
            .fork
            .observe(
                operation.clone(),
                ProviderOperation::SessionFork,
                request.clone(),
                async {
                    Ok(forge_provider_api::ProviderForkResult::unprofiled(
                        "retained-target".try_into().unwrap(),
                    ))
                },
                |reason| {
                    forge_provider_api::retained_operation::Interruption::new(
                        reason.failure(
                            ProviderOperation::SessionFork,
                            forge_provider_api::ProviderPhase::Admission,
                            forge_provider_api::FailureDisposition::Unchanged,
                        ),
                        async { Ok(()) },
                    )
                },
            )
            .await
            .unwrap();
        assert_eq!(
            provider
                .session_fork(operation.clone(), request.clone())
                .await
                .unwrap()
                .provider_session_id
                .as_str(),
            "retained-target"
        );
        for changed in [0, 1, 2] {
            let mut changed_request = request.clone();
            match changed {
                0 => {
                    changed_request.target.owner =
                        forge_provider_api::ExecutionOwner::root("substituted")
                }
                1 => {
                    changed_request.source = forge_provider_api::ForkSource::Checkpoint {
                        provider_session_id: "different-history".try_into().unwrap(),
                        checkpoint: forge_provider_api::CheckpointId::new("different-boundary")
                            .unwrap(),
                    }
                }
                2 => changed_request.target.configuration.resources.mcp.push(
                    forge_provider_api::McpLaunch {
                        name: "different-resource".into(),
                        transport: forge_provider_api::McpTransport::Stdio {
                            command: "unused-command".into(),
                            arguments: Vec::new(),
                            environment: Default::default(),
                        },
                        timeout: None,
                        working_directory: None,
                        tools: None,
                    },
                ),
                _ => unreachable!(),
            }
            let error = provider
                .session_fork(operation.clone(), changed_request)
                .await
                .unwrap_err();
            assert!(
                matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionFork && failure.kind() == ProviderFailureKind::OperationConflict)
            );
        }
        assert_eq!(
            provider
                .session_fork(operation, request)
                .await
                .unwrap()
                .provider_session_id
                .as_str(),
            "retained-target"
        );
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Cancellation reaches the exact native turn and settles before its turn/start acknowledgment is released.
    #[tokio::test]
    async fn cancellation_interrupts_and_settles_while_start_acknowledgment_is_withheld() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let mut input = request();
            let (dispatch, mut observed) = forge_provider_api::submission::DispatchReporter::channel(session.binding.id.clone(), input.scope.clone(), input.submission_id.clone(), input.attempt_id.clone());
            input.dispatch = dispatch;
            let scope = input.scope.clone();
            let binding = session.binding.id.clone();
            let (release_ack, released_ack) = tokio::sync::oneshot::channel();
            let wire = tokio::spawn(async move {
                let models: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(models["method"], "model/list");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":models["id"],"result":{"data":[{"model":"native-model","isDefault":true,"supportedReasoningEfforts":[{"reasoningEffort":"low"}],"defaultReasoningEffort":"low"}]}}).to_string().into())).await.unwrap();
                let start: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(start["method"], "turn/start");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"turn/started","params":{"threadId":"compaction-thread","turn":{"id":"pending-ack-turn","status":"inProgress","items":[]}}}).to_string().into())).await.unwrap();
                let interrupt: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(interrupt["method"], "turn/interrupt");
                assert_eq!(interrupt["params"]["turnId"], "pending-ack-turn");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":interrupt["id"],"result":{}}).to_string().into())).await.unwrap();
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"turn/completed","params":{"threadId":"compaction-thread","turn":{"id":"pending-ack-turn","status":"interrupted"}}}).to_string().into())).await.unwrap();
                released_ack.await.unwrap();
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":start["id"],"result":{"turn":{"id":"pending-ack-turn"}}}).to_string().into())).await.unwrap();
                while let Some(message) = socket.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            let submitting_provider = provider.clone();
            let submitting_binding = binding.clone();
            let submitting = tokio::spawn(async move { submitting_provider.input_submit(context(), &submitting_binding, input).await });
            observed.wait().await.unwrap();
            let settled = provider.cancel(context(), &binding, forge_provider_api::CancelRequest { scope }).await.unwrap();
            assert!(matches!(settled.result, ExecutionResult::Cancelled { .. }));
            assert!(session.active.lock().await.is_none());
            submitting.await.unwrap().unwrap();
            release_ack.send(()).unwrap();
            session.rpc.lock().await.close().await.unwrap();
            wire.await.unwrap();
        }).await.unwrap();
    }

    /// Exact cancellation prevents both a queued retained worker and an input future which has not been polled yet.
    #[tokio::test]
    async fn exact_cancel_prevents_queued_preparation_and_input_before_first_poll() {
        use futures_util::StreamExt;
        for poll_input in [false, true] {
            tokio::time::timeout(std::time::Duration::from_secs(5), async {
                let (provider, session, _events, mut socket) = compaction_fixture().await;
                let mut input = request(); let scope = input.scope.clone(); let binding = session.binding.id.clone();
                input.dispatch = forge_provider_api::submission::DispatchReporter::channel(binding.clone(), scope.clone(), input.submission_id.clone(), input.attempt_id.clone()).0;
                let admission = if poll_input { Some(session.admission.lock().await) } else { None };
                let mut submitting = Box::pin(provider.input_submit(context(), &binding, input));
                if poll_input { assert!(futures_util::poll!(submitting.as_mut()).is_pending()); }
                let mut cancelling = Box::pin(provider.cancel(context(), &binding, forge_provider_api::CancelRequest { scope }));
                if poll_input { assert!(futures_util::poll!(cancelling.as_mut()).is_pending()); }
                else { assert!(matches!(cancelling.as_mut().await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation)); }
                let error = submitting.await.unwrap_err();
                assert!(matches!(error, ProviderError::PreparationFailed(cause) if matches!(cause.as_ref(), ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Cancelled && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)));
                drop(admission);
                if poll_input { assert!(matches!(cancelling.await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation)); }
                assert!(session.admitting.lock().await.is_none()); assert!(session.active.lock().await.is_none()); assert!(!session.is_fenced());
                let mut next = Box::pin(socket.next()); assert!(futures_util::poll!(next.as_mut()).is_pending()); drop(next);
                session.rpc.lock().await.close().await.unwrap();
            }).await.unwrap();
        }
    }

    /// Registered preparation cancellation proves no native invocation and restores the retained idle reader before reuse.
    #[tokio::test]
    async fn before_send_cancellation_collects_preparation_without_fabricating_native_settlement() {
        use futures_util::{SinkExt, StreamExt};
        for close_binding in [false, true] {
            tokio::time::timeout(std::time::Duration::from_secs(5), async {
                let (provider, session, _events, mut socket) = compaction_fixture().await;
                let mut normalization = session.normalization.lock().await; normalization.bind(Some(request().scope));
                let mut output = forge_provider_api::completion::ProviderOutput::default();
                normalization.normalize(&json!({"method":"item/started","params":{"threadId":"compaction-thread","turnId":"prior-turn","item":{"type":"commandExecution","id":"prior-command","command":"background","status":"inProgress"}}}), &mut output, None, "").await.unwrap();
                normalization.normalize(&json!({"method":"turn/completed","params":{"threadId":"compaction-thread","turn":{"id":"prior-turn","status":"completed"}}}), &mut output, None, "").await.unwrap(); drop(normalization);
                start_background_reader(&session).await.unwrap().unwrap().send(()).unwrap();
                let gate = Arc::new(BeforeSendGate { entered: tokio::sync::Notify::new(), release: tokio::sync::Semaphore::new(0) });
                let mut input = request(); input.turn_control = gate.clone(); let scope = input.scope.clone(); let binding = session.binding.id.clone();
                input.dispatch = forge_provider_api::submission::DispatchReporter::channel(binding.clone(), scope.clone(), input.submission_id.clone(), input.attempt_id.clone()).0;
                let submitting_provider = provider.clone(); let submitting_binding = binding.clone();
                let submitting = tokio::spawn(async move { submitting_provider.input_submit(context(), &submitting_binding, input).await });
                gate.entered.notified().await;
                assert!(session.admitting.lock().await.is_some()); assert!(session.background_reader.lock().await.is_none());
                let mut next = Box::pin(socket.next()); assert!(futures_util::poll!(next.as_mut()).is_pending()); drop(next);
                if close_binding {
                    provider.session_close(context(), &binding).await.unwrap();
                    assert!(matches!(submitting.await.unwrap(), Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingClosed && failure.disposition() == forge_provider_api::FailureDisposition::Invalidated));
                    assert!(provider.closed_bindings.lock().await.contains(&binding));
                    assert!(matches!(provider.input_submit(context(), &binding, request()).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingClosed && failure.disposition() == forge_provider_api::FailureDisposition::Closed));
                    gate.release.add_permits(1);
                    assert!(matches!(socket.next().await, Some(Ok(tokio_tungstenite::tungstenite::Message::Close(_))) | None));
                    return;
                }
                assert!(matches!(provider.cancel(context(), &binding, forge_provider_api::CancelRequest { scope: scope.clone() }).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation));
                assert!(matches!(submitting.await.unwrap(), Err(ProviderError::PreparationFailed(_))));
                assert!(!session.settlements.lock().await.contains_key(&scope)); assert!(!session.is_fenced()); assert!(session.background_reader.lock().await.is_some());
                gate.release.add_permits(1);
                let wire = tokio::spawn(async move {
                    let models: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap(); assert_eq!(models["method"], "model/list");
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":models["id"],"result":{"data":[{"model":"native-model","isDefault":true,"supportedReasoningEfforts":[]}]}}).to_string().into())).await.unwrap();
                    let start: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap(); assert_eq!(start["method"], "turn/start");
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":start["id"],"result":{"turn":{"id":"successor-turn"}}}).to_string().into())).await.unwrap();
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"turn/completed","params":{"threadId":"compaction-thread","turn":{"id":"successor-turn","status":"completed"}}}).to_string().into())).await.unwrap();
                    while let Some(message) = socket.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
                });
                let mut successor = request(); successor.scope = forge_provider_api::InvocationScope::new(session.owner.clone(), "successor-exchange"); let successor_scope = successor.scope.clone();
                successor.dispatch = forge_provider_api::submission::DispatchReporter::channel(binding.clone(), successor.scope.clone(), successor.submission_id.clone(), successor.attempt_id.clone()).0;
                provider.input_submit(context(), &binding, successor).await.unwrap();
                assert!(matches!(provider.cancel(context(), &binding, forge_provider_api::CancelRequest { scope: successor_scope }).await.unwrap().result, ExecutionResult::Completed { .. }));
                stop_background_reader(&session).await.unwrap(); session.rpc.lock().await.close().await.unwrap(); wire.await.unwrap();
            }).await.unwrap();
        }
    }

    /// Native late rejection proves the steering receipt was not consumed without replacing its typed cause.
    #[tokio::test]
    async fn native_late_steering_rejection_is_positive_preparation_failure() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let input = request(); let scope = input.scope.clone(); let (cancel, _) = mpsc::unbounded_channel();
            *session.active.lock().await = Some(ActiveExecution { scope: scope.clone(), turn_id: "finished-native-turn".into(), turn_updates: watch::channel(Some("finished-native-turn".into())).0, submissions: Arc::new(Mutex::new(Vec::new())), request: input.clone(), cancel, task: None });
            let wire = tokio::spawn(async move {
                let steer: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap(); assert_eq!(steer["method"], "turn/steer");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":steer["id"],"error":{"code":-32600,"message":"native turn no longer running"}}).to_string().into())).await.unwrap();
                while let Some(message) = socket.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            let request = SteeringRequest { scope, delivery: forge_provider_api::SteeringDelivery::Journaled { receipt: input.receipt.clone() }, input: PromptInput::Text("late steering".into()), attachments: Vec::new(), task: None };
            let error = provider.input_steer(context(), &session.binding.id, request).await.unwrap_err();
            assert!(matches!(error, ProviderError::PreparationFailed(cause) if matches!(cause.as_ref(), ProviderError::Failure(failure) if failure.operation() == ProviderOperation::InputSteer && failure.kind() == ProviderFailureKind::Rejected && failure.phase() == forge_provider_api::ProviderPhase::Observation && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)));
            assert!(!session.is_fenced()); session.active.lock().await.take(); session.rpc.lock().await.close().await.unwrap(); wire.await.unwrap();
        }).await.unwrap();
    }

    /// Explicit operation cancellation retains unknown delivery and collects an unanswered native admission.
    #[tokio::test]
    async fn operation_cancellation_collects_pending_native_admission_without_acknowledgment() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let mut input = request();
            let (dispatch, mut observed) = forge_provider_api::submission::DispatchReporter::channel(session.binding.id.clone(), input.scope.clone(), input.submission_id.clone(), input.attempt_id.clone()); input.dispatch = dispatch;
            let (source, cancellation) = forge_provider_api::OperationCancellation::channel();
            let operation = OperationContext::new(forge_provider_api::OperationId::new("cancel-pending-native-admission").unwrap(), std::time::Duration::from_secs(5), cancellation).unwrap();
            let wire = tokio::spawn(async move {
                let models: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(models["method"], "model/list");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":models["id"],"result":{"data":[{"model":"native-model","isDefault":true,"supportedReasoningEfforts":[]}]}}).to_string().into())).await.unwrap();
                let start: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap(); assert_eq!(start["method"], "turn/start");
                while let Some(message) = socket.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            let submitting_provider = provider.clone(); let binding = session.binding.id.clone();
            let submitting_operation = operation.clone(); let submitting_input = input.clone(); let submitting_binding = binding.clone();
            let submitting = tokio::spawn(async move { submitting_provider.input_submit(submitting_operation, &submitting_binding, submitting_input).await });
            observed.wait().await.unwrap(); source.cancel();
            assert!(submitting.await.unwrap().is_err());
            provider.records.submit.join_all().await.unwrap();
            assert!(session.is_fenced());
            assert!(provider.closed_bindings.lock().await.contains(&binding), "interrupted unknown-turn admission retained an uncollected native binding");
            assert!(matches!(&*session.history.lock().unwrap(), NativeHistory::Established(_)), "possibly dispatched input must permanently exclude virgin history replacement");
            let configuration = session.configuration.lock().await.clone();
            assert!(provider.session_configure(context(), &binding, configuration).await.is_err());
            wire.await.unwrap();
            provider.session_close(context(), &binding).await.unwrap();
            assert!(!provider.bindings.lock().await.contains_key(&binding));
            assert!(provider.input_submit(operation, &binding, input).await.is_err());
        }).await.unwrap();
    }

    /// Pre-admission observations have no total-message cap borrowed from transport queue capacity.
    #[tokio::test]
    async fn admission_preserves_more_than_4096_ordered_native_messages() {
        use futures_util::{SinkExt, StreamExt};
        let (provider, session, _events, mut socket) = compaction_fixture().await;
        let input = request();
        let count = 4097;
        let peer = tokio::spawn(async move {
            let sent: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(sent["method"], "turn/start");
            for sequence in 0..count {
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({"method":"thread/status/changed","params":{
                            "threadId":"compaction-thread","sequence":sequence
                        }})
                        .to_string()
                        .into(),
                    ))
                    .await
                    .unwrap();
            }
            socket
                .send(tokio_tungstenite::tungstenite::Message::Text(
                    json!({"method":"turn/started","params":{
                        "threadId":"compaction-thread","turnId":"admitted-after-observations"
                    }})
                    .to_string()
                    .into(),
                ))
                .await
                .unwrap();
            while let Some(Ok(message)) = socket.next().await {
                if message.is_close() {
                    break;
                }
            }
        });
        let id = session
            .rpc
            .lock()
            .await
            .send_request("turn/start", json!({}))
            .await
            .unwrap();
        let (turn, messages) = await_admission(&session, &input, id).await.unwrap();
        assert_eq!(turn, "admitted-after-observations");
        assert_eq!(messages.len(), count + 1);
        for (sequence, message) in messages[..count].iter().enumerate() {
            assert_eq!(message["params"]["sequence"], sequence);
        }
        assert_eq!(messages[count]["method"], "turn/started");
        assert!(!session.is_fenced());
        assert!(
            provider
                .bindings
                .lock()
                .await
                .contains_key(&session.binding.id)
        );
        session.rpc.lock().await.close().await.unwrap();
        peer.await.unwrap();
    }

    /// Normal native completion collects automatically; late cancellation cannot touch a newer invocation.
    #[tokio::test]
    async fn completed_invocation_settles_once_and_late_cancel_preserves_successor() {
        use futures_util::SinkExt;
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let request = request();
            let scope = request.scope.clone();
            session.normalization.lock().await.bind(Some(scope.clone()));
            let address = forge_provider_api::TurnAddress { scope: scope.clone(), thread_id: "compaction-thread".into(), turn_id: "completed-turn".into() };
            let (cancel, receiver) = mpsc::unbounded_channel();
            let submissions = Arc::new(Mutex::new(vec![request.submission_id.clone()]));
            let worker_session = session.clone();
            let worker_request = request.clone();
            let worker_submissions = submissions.clone();
            let worker = Arc::new(crate::retained_task::RetainedTask::new(ProviderOperation::InputSubmit, tokio::spawn(async move {
                observe_execution(worker_session, worker_request, address, worker_submissions, receiver, Vec::new()).await
            })));
            *session.active.lock().await = Some(ActiveExecution { scope: scope.clone(), turn_id: "completed-turn".into(), turn_updates: watch::channel(Some("completed-turn".into())).0, submissions, request: request.clone(), cancel, task: Some(worker.clone()) });
            let collected_session = session.clone();
            let collected_scope = scope.clone();
            let collector = tokio::spawn(async move { collect_invocation(collected_session, collected_scope, worker).await });
            socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"turn/completed","params":{"threadId":"compaction-thread","turn":{"id":"completed-turn","status":"completed"}}}).to_string().into())).await.unwrap();
            collector.await.unwrap().unwrap();
            assert!(session.active.lock().await.is_none());
            let retained = session.settlements.lock().await.get(&scope).cloned().unwrap().unwrap();
            assert!(matches!(retained.result, ExecutionResult::Completed { .. }));
            assert!(retained.watermark.get() > 0);
            let mut successor = request;
            successor.scope = forge_provider_api::InvocationScope::new(session.owner.clone(), "successor");
            let successor_scope = successor.scope.clone();
            let (cancel, mut cancelled) = mpsc::unbounded_channel();
            let (release, released) = tokio::sync::oneshot::channel();
            let worker = Arc::new(crate::retained_task::RetainedTask::new(ProviderOperation::InputSubmit, tokio::spawn(async move { released.await.unwrap(); Ok(()) })));
            *session.active.lock().await = Some(ActiveExecution { scope: successor_scope.clone(), turn_id: "successor-turn".into(), turn_updates: watch::channel(Some("successor-turn".into())).0, submissions: Arc::new(Mutex::new(Vec::new())), request: successor, cancel, task: Some(worker.clone()) });
            let settled = provider.cancel(context(), &session.binding.id, forge_provider_api::CancelRequest { scope: scope.clone() }).await.unwrap();
            assert_eq!(settled.watermark, retained.watermark);
            assert!(matches!(settled.result, ExecutionResult::Completed { .. }));
            assert!(cancelled.try_recv().is_err());
            assert_eq!(session.active.lock().await.as_ref().unwrap().scope, successor_scope);
            release.send(()).unwrap(); worker.join().await.unwrap();
            session.active.lock().await.take();
            session.rpc.lock().await.close().await.unwrap();
        }).await.unwrap();
    }

    /// A cancellation response deadline cannot discard the worker or its later authoritative settlement.
    #[tokio::test]
    async fn cancellation_timeout_retains_collection_until_late_settlement() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        let input = request();
        let scope = input.scope.clone();
        let (cancel, mut cancelled) = mpsc::unbounded_channel();
        let (release, released) = tokio::sync::oneshot::channel();
        let worker_session = session.clone();
        let worker_scope = scope.clone();
        let result = ExecutionResult::Cancelled {
            completion: completion(&session, &input.configuration.model).await,
        };
        let worker = Arc::new(crate::retained_task::RetainedTask::new(
            ProviderOperation::InputSubmit,
            tokio::spawn(async move {
                released.await.unwrap();
                worker_session
                    .outcomes
                    .lock()
                    .await
                    .insert(worker_scope, result);
                Ok(())
            }),
        ));
        *session.active.lock().await = Some(ActiveExecution {
            scope: scope.clone(),
            turn_id: "slow-cancellation".into(),
            turn_updates: watch::channel(Some("slow-cancellation".into())).0,
            submissions: Arc::new(Mutex::new(Vec::new())),
            request: input,
            cancel,
            task: Some(worker.clone()),
        });
        let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
        let operation = OperationContext::new(
            forge_provider_api::OperationId::new("slow-cancellation").unwrap(),
            std::time::Duration::from_secs(30),
            cancellation,
        )
        .unwrap();
        let cancelling_provider = provider.clone();
        let cancelling_binding = session.binding.id.clone();
        let cancelling_scope = scope.clone();
        let cancelling_operation = operation.clone();
        let cancelling = tokio::spawn(async move {
            cancelling_provider
                .cancel(
                    cancelling_operation,
                    &cancelling_binding,
                    forge_provider_api::CancelRequest {
                        scope: cancelling_scope,
                    },
                )
                .await
        });
        cancelled.recv().await.unwrap();
        tokio::time::pause();
        tokio::time::advance(std::time::Duration::from_secs(31)).await;
        tokio::time::resume();
        let error = cancelling.await.unwrap().unwrap_err();
        assert!(matches!(error, ProviderError::Failure(failure)
            if failure.kind() == ProviderFailureKind::Timeout
                && failure.phase() == forge_provider_api::ProviderPhase::Collection
                && failure.disposition() == forge_provider_api::FailureDisposition::Uncertain));
        assert!(!worker.is_finished());
        assert!(session.active.lock().await.is_some());
        assert!(!session.settlements.lock().await.contains_key(&scope));
        release.send(()).unwrap();
        provider.records.cancel.join_all().await.unwrap();
        let settled = provider
            .cancel(
                operation,
                &session.binding.id,
                forge_provider_api::CancelRequest { scope },
            )
            .await
            .unwrap();
        assert!(matches!(settled.result, ExecutionResult::Cancelled { .. }));
        assert!(session.active.lock().await.is_none());
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Failed filesystem preparation leaves the existing idle observer active and the binding reusable.
    #[tokio::test]
    async fn configure_preparation_failure_preserves_idle_reader() {
        let (mut provider, session, _events, _socket) = compaction_fixture().await;
        let directory = tempfile::tempdir().unwrap();
        let blocked = directory.path().join("blocked");
        std::fs::write(&blocked, "occupied").unwrap();
        provider.runtime_root = blocked;
        let cancelled = Arc::new(AtomicBool::new(false));
        let observed = cancelled.clone();
        let (cancel, mut receiver) = mpsc::unbounded_channel();
        let worker = tokio::spawn(async move {
            let _ = receiver.recv().await;
            observed.store(true, Ordering::Release);
            Ok(())
        });
        *session.background_reader.lock().await = Some(BackgroundReader {
            cancel,
            task: crate::retained_task::RetainedTask::new(ProviderOperation::InputSubmit, worker),
        });
        let configuration = session.configuration.lock().await.clone();
        assert!(
            provider
                .session_configure(context(), &session.binding.id, configuration)
                .await
                .is_err()
        );
        assert!(!cancelled.load(Ordering::Acquire));
        assert!(session.background_reader.lock().await.is_some());
        assert!(!session.is_fenced());
        stop_background_reader(&session).await.unwrap();
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Replacement rejection preserves its native category and observation phase while revoking admission.
    #[tokio::test]
    async fn configure_replacement_failure_preserves_cause_and_invalidated_disposition() {
        let (_provider, session, _events, _socket) = compaction_fixture().await;
        let cause = ProviderFailure::new(
            ProviderOperation::SessionConfigure,
            ProviderFailureKind::Rejected,
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Unchanged,
        );
        let error = invalidate_configuration(
            &session,
            cause.into(),
            forge_provider_api::ProviderPhase::Preparation,
        );
        assert!(
            matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Rejected && failure.phase() == forge_provider_api::ProviderPhase::Observation && failure.disposition() == forge_provider_api::FailureDisposition::Invalidated)
        );
        assert!(session.is_fenced());
        assert!(matches!(
            session.sink.failed().await,
            events::EventDeliveryError::Observation(
                forge_provider_api::ObservationFailure::LifecycleUnavailable
            )
        ));
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Specialized post-handoff evidence retains its detail behind an explicitly invalidated normalized primary.
    #[tokio::test]
    async fn configure_specialized_failures_preserve_invalidated_primary_and_typed_detail() {
        let requirement = forge_provider_api::version::RuntimeRequirement::new(
            crate::registry::select("codex").unwrap(),
            "fixture-codex".into(),
            semver::Version::new(0, 164, 0),
        );
        for (original, expected_kind, expected_phase) in [
            (
                ProviderError::RuntimeVersion(requirement.validate(Some("0.163.0")).unwrap_err()),
                ProviderFailureKind::Initialization,
                forge_provider_api::ProviderPhase::Preparation,
            ),
            (
                ProviderError::Observation(forge_provider_api::ObservationFailure::Overflow),
                ProviderFailureKind::Transport,
                forge_provider_api::ProviderPhase::Observation,
            ),
        ] {
            let (_provider, session, _events, _socket) = compaction_fixture().await;
            let original_is_version = matches!(&original, ProviderError::RuntimeVersion(_));
            let independent = ProviderFailure::new(
                ProviderOperation::SessionClose,
                ProviderFailureKind::Timeout,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Close,
                forge_provider_api::FailureDisposition::Uncertain,
            );
            let error = invalidate_configuration(
                &session,
                ProviderError::Combined {
                    primary: Box::new(original),
                    secondary: vec![independent.into()],
                },
                forge_provider_api::ProviderPhase::Collection,
            );
            let ProviderError::Combined { primary, secondary } = error else {
                panic!("independent cleanup evidence lost");
            };
            assert!(
                matches!(&secondary[..], [ProviderError::Failure(failure)] if *failure == independent)
            );
            let ProviderError::Combined { primary, secondary } = *primary else {
                panic!("specialized cause lost");
            };
            assert!(
                matches!(*primary, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionConfigure && failure.kind() == expected_kind && failure.phase() == expected_phase && failure.disposition() == forge_provider_api::FailureDisposition::Invalidated)
            );
            assert_eq!(secondary.len(), 1);
            assert_eq!(
                matches!(&secondary[0], ProviderError::RuntimeVersion(_)),
                original_is_version
            );
            assert!(session.invalidated.load(Ordering::Acquire));
            session.rpc.lock().await.close().await.unwrap();
        }
    }

    /// Native idle-observer loss revokes history before host cleanup and cannot be cleared by a provisional gate.
    #[tokio::test]
    async fn background_observer_failure_stays_invalidated_after_provisional_fence_release() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, socket) = compaction_fixture().await;
            let mut normalization = session.normalization.lock().await;
            normalization.bind(Some(request().scope));
            let mut output = forge_provider_api::completion::ProviderOutput::default();
            normalization.normalize(&json!({"method":"item/started","params":{"threadId":"compaction-thread","turnId":"old","item":{"type":"commandExecution","id":"command","command":"slow command","status":"inProgress"}}}), &mut output, None, "").await.unwrap();
            normalization.normalize(&json!({"method":"turn/completed","params":{"threadId":"compaction-thread","turn":{"id":"old","status":"completed"}}}), &mut output, None, "").await.unwrap();
            assert!(normalization.has_pending_background_tools()); drop(normalization);
            start_background_reader(&session).await.unwrap().unwrap().send(()).unwrap();
            drop(socket);
            let _ = session.sink.failed().await;
            assert!(session.invalidated.load(Ordering::Acquire));
            assert!(matches!(&*session.close_failure.lock().await, Some(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Transport));
            session.fenced.store(false, Ordering::Release);
            assert!(session.is_fenced());
            assert!(provider.session_fork(context(), forge_provider_api::SessionForkRequest { source: forge_provider_api::ForkSource::Binding(session.binding.id.clone()), target: forge_provider_api::ForkTarget { owner: forge_provider_api::ExecutionOwner::root("blocked-fork"), workspace: "workspace".into(), configuration: session.configuration.lock().await.clone(), system_instructions: String::new() } }).await.is_err());
            let _ = stop_background_reader(&session).await;
            let _ = session.rpc.lock().await.close().await;
        }).await.unwrap();
    }

    /// Surviving native terminals reject replacement before the binding's reader or settings change.
    #[tokio::test]
    async fn configure_retained_terminal_is_unchanged_resource_conflict() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (mut provider, session, _events, mut socket) = compaction_fixture().await;
            let directory = tempfile::tempdir().unwrap(); provider.runtime_root = directory.path().to_path_buf();
            let wire = tokio::spawn(async move {
                let read: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(read["method"], "thread/backgroundTerminals/list");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":read["id"],"result":{"data":[{"processId":"surviving-terminal","command":"running"}]}}).to_string().into())).await.unwrap();
                while let Some(message) = socket.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            let configuration = session.configuration.lock().await.clone();
            let error = provider.session_configure(context(), &session.binding.id, configuration).await.unwrap_err();
            assert!(matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionConfigure && failure.kind() == ProviderFailureKind::ResourceConflict && failure.disposition() == forge_provider_api::error::FailureDisposition::Unchanged));
            assert!(!session.is_fenced());
            assert!(session.retired_rpcs.lock().await.is_empty());
            session.rpc.lock().await.close().await.unwrap(); wire.await.unwrap();
        }).await.unwrap();
    }

    /// Rejected idle steering cannot stop the observer that still owns retained background activity.
    #[tokio::test]
    async fn rejected_idle_steering_preserves_background_observer() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        let cancelled = Arc::new(AtomicBool::new(false));
        let observed = cancelled.clone();
        let (cancel, mut receiver) = mpsc::unbounded_channel();
        let worker = tokio::spawn(async move {
            let _ = receiver.recv().await;
            observed.store(true, Ordering::Release);
            Ok(())
        });
        *session.background_reader.lock().await = Some(BackgroundReader {
            cancel,
            task: crate::retained_task::RetainedTask::new(ProviderOperation::InputSubmit, worker),
        });
        let request = SteeringRequest {
            scope: request().scope,
            delivery: forge_provider_api::SteeringDelivery::Context,
            input: PromptInput::Text("late context".into()),
            attachments: Vec::new(),
            task: None,
        };
        let error = provider
            .input_steer(context(), &session.binding.id, request)
            .await
            .unwrap_err();
        assert!(
            matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::InputSteer && failure.kind() == ProviderFailureKind::WrongInvocation && failure.phase() == forge_provider_api::ProviderPhase::Admission && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
        );
        assert!(!cancelled.load(Ordering::Acquire));
        assert!(session.background_reader.lock().await.is_some());
        stop_background_reader(&session).await.unwrap();
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Input preconditions preserve closed, invalidated, identity, and occupied-slot causes without native dispatch.
    #[tokio::test]
    async fn input_preconditions_preserve_exact_typed_rejection_reasons() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        let mut input = request();
        input.dispatch = forge_provider_api::submission::DispatchReporter::channel(
            session.binding.id.clone(),
            input.scope.clone(),
            input.submission_id.clone(),
            input.attempt_id.clone(),
        )
        .0;
        let mut wrong = input.clone();
        wrong.scope =
            forge_provider_api::InvocationScope::new(session.owner.clone(), "wrong-invocation");
        assert!(
            matches!(provider.input_submit(context(), &session.binding.id, wrong).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
        );
        session.invalidated.store(true, Ordering::Release);
        assert!(
            matches!(provider.input_submit(context(), &session.binding.id, input.clone()).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingInvalidated && failure.disposition() == forge_provider_api::FailureDisposition::Invalidated)
        );
        session.invalidated.store(false, Ordering::Release);
        session.closing.store(true, Ordering::Release);
        assert!(
            matches!(provider.input_submit(context(), &session.binding.id, input.clone()).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingClosed && failure.disposition() == forge_provider_api::FailureDisposition::Invalidated)
        );
        session.closing.store(false, Ordering::Release);
        let (cancel, _) = mpsc::unbounded_channel();
        *session.active.lock().await = Some(ActiveExecution {
            scope: input.scope.clone(),
            turn_id: "existing-turn".into(),
            turn_updates: watch::channel(Some("existing-turn".into())).0,
            submissions: Arc::new(Mutex::new(Vec::new())),
            request: input.clone(),
            cancel,
            task: None,
        });
        assert!(
            matches!(provider.input_submit(context(), &session.binding.id, input).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::ExecutionActive && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
        );
        session.active.lock().await.take();
        session.rpc.lock().await.close().await.unwrap();
    }

    /// New runtimes and nonexportable credentials cannot advertise the audited optional utility workflow.
    #[test]
    fn recap_capability_requires_audited_version_and_native_exportable_authentication() {
        let supported = json!({"requiresOpenaiAuth":true,"authMethod":"chatgpt"});
        let find = |manifest: forge_provider_api::capability::CapabilityManifest| {
            manifest
                .capability
                .into_iter()
                .find(|capability| {
                    capability.id == forge_provider_api::capability::CapabilityId::Recap
                })
                .unwrap()
                .support
        };
        assert!(matches!(
            find(negotiated_capabilities(Some("0.164.0"), Some(&supported))),
            forge_provider_api::capability::Support::Supported
        ));
        assert!(matches!(
            find(negotiated_capabilities(Some("0.165.0"), Some(&supported))),
            forge_provider_api::capability::Support::Unsupported
        ));
        assert!(matches!(
            find(negotiated_capabilities(
                Some("0.164.0"),
                Some(&json!({"requiresOpenaiAuth":true,"authMethod":"agentIdentity"}))
            )),
            forge_provider_api::capability::Support::Unsupported
        ));
        assert!(matches!(
            find(negotiated_capabilities(None, Some(&supported))),
            forge_provider_api::capability::Support::Unsupported
        ));
    }

    /// Applying defaults cannot interrupt the invocation that currently owns native work.
    #[tokio::test]
    async fn configure_rejects_active_invocation_without_sending_cancel() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        let request = request();
        let (cancel, mut cancelled) = mpsc::unbounded_channel();
        let (release, released) = tokio::sync::oneshot::channel();
        let worker = Arc::new(crate::retained_task::RetainedTask::new(
            ProviderOperation::InputSubmit,
            tokio::spawn(async move {
                released.await.unwrap();
                Ok(())
            }),
        ));
        *session.active.lock().await = Some(ActiveExecution {
            scope: request.scope.clone(),
            turn_id: "running".into(),
            turn_updates: watch::channel(Some("running".into())).0,
            submissions: Arc::new(Mutex::new(Vec::new())),
            request,
            cancel,
            task: Some(worker.clone()),
        });
        let configuration = session.configuration.lock().await.clone();
        let error = provider
            .session_configure(context(), &session.binding.id, configuration)
            .await
            .unwrap_err();
        assert!(
            matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::SessionConfigure && failure.kind() == ProviderFailureKind::ExecutionActive)
        );
        assert!(cancelled.try_recv().is_err());
        release.send(()).unwrap();
        worker.join().await.unwrap();
        session.active.lock().await.take();
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Lost rollback acknowledgment retains uncertainty and cannot remove a newer turn on retry.
    #[tokio::test]
    async fn uncertain_retraction_never_repeats_native_rollback() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let input = request();
            let operation = context();
            let request = forge_provider_api::InputRetractRequest { scope: input.scope.clone(), submission_id: input.submission_id.clone(), attempt_id: input.attempt_id.clone(), boundary: forge_provider_api::CheckpointId::new("cancelled-turn").unwrap(), authorization: forge_provider_api::RetractionAuthorization::OutputFreeCancellation };
            *session.rollback.lock().await = Some(RollbackAuthorization { thread_id: "compaction-thread".into(), scope: input.scope.clone(), turn_id: "cancelled-turn".into(), submission_id: input.submission_id.clone(), attempt_id: input.attempt_id.clone() });
            session.settlements.lock().await.insert(input.scope.clone(), Ok(forge_provider_api::InvocationSettlement { binding_id: session.binding.id.clone(), scope: input.scope.clone(), result: ExecutionResult::Cancelled { completion: completion(&session, &input.configuration.model).await }, watermark: session.sink.watermark() }));
            let client_id = native_input_id(&input.submission_id, &input.attempt_id);
            let wire = tokio::spawn(async move {
                let read: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(read["method"], "thread/read");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":read["id"],"result":{"thread":{"turns":[{"id":"cancelled-turn","status":"interrupted","items":[{"type":"userMessage","clientId":client_id}]}]}}}).to_string().into())).await.unwrap();
                let rollback: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(rollback["method"], "thread/rollback");
                socket.close(None).await.unwrap();
            });
            assert_eq!(provider.input_retract(operation.clone(), &session.binding.id, request.clone()).await.unwrap(), forge_provider_api::InputRetractOutcome::Unknown);
            assert!(session.is_fenced());
            assert!(session.close_failure.lock().await.is_some());
            wire.await.unwrap();
            assert_eq!(provider.input_retract(operation.clone(), &session.binding.id, request.clone()).await.unwrap(), forge_provider_api::InputRetractOutcome::Unknown);
            let mut substituted = request;
            substituted.submission_id = "newer-input".try_into().unwrap();
            let error = provider.input_retract(operation, &session.binding.id, substituted).await.unwrap_err();
            assert!(matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::OperationConflict));
            let _ = session.rpc.lock().await.close().await;
        }).await.unwrap();
    }

    /// Empty or mismatched native history cannot prove nonacceptance or authorize a resend.
    #[test]
    fn recovery_absence_and_other_attempt_remain_unknown() {
        let submission = forge_provider_api::SubmissionId::try_from("input").unwrap();
        let attempt = forge_provider_api::DeliveryAttemptId::new("attempt").unwrap();
        assert!(matches!(
            history_page_evidence(&[], "thread", &submission, &attempt).unwrap(),
            forge_provider_api::SubmissionEvidence::Unknown
        ));
        let other = forge_provider_api::DeliveryAttemptId::new("other-attempt").unwrap();
        let rows = [json!({"type":"userMessage","clientId":native_input_id(&submission, &other)})];
        assert!(matches!(
            history_page_evidence(&rows, "thread", &submission, &attempt).unwrap(),
            forge_provider_api::SubmissionEvidence::Unknown
        ));
        let rows = [
            json!({"item":{"type":"userMessage","clientId":native_input_id(&submission, &attempt)}}),
        ];
        assert!(matches!(
            history_page_evidence(&rows, "thread", &submission, &attempt).unwrap(),
            forge_provider_api::SubmissionEvidence::Accepted { .. }
        ));
        let token = recovery_token_parts("thread", submission.as_str(), attempt.as_str()).unwrap();
        let evidence = parse_recovery_token(Some(&token)).unwrap();
        assert_eq!(evidence.attempt_id, "attempt");
        assert_eq!(evidence.submission_id, "input");
    }

    /// Retraction requires exact attempt identity and cannot discard a second input in the same turn.
    #[test]
    fn retraction_boundary_rejects_substituted_attempt_and_steered_input() {
        let input = request();
        let request = forge_provider_api::InputRetractRequest {
            scope: input.scope,
            submission_id: input.submission_id,
            attempt_id: input.attempt_id,
            boundary: forge_provider_api::CheckpointId::new("cancelled").unwrap(),
            authorization: forge_provider_api::RetractionAuthorization::OutputFreeCancellation,
        };
        let item = json!({"type":"userMessage","clientId":native_input_id(&request.submission_id, &request.attempt_id)});
        let mut turn = json!({"id":"cancelled","status":"interrupted","items":[item.clone()]});
        assert!(retractable_turn(&turn, &request));
        turn["id"] = json!("successor");
        assert!(!retractable_turn(&turn, &request));
        turn["id"] = json!("cancelled");
        turn["items"] = json!([item.clone(), item]);
        assert!(!retractable_turn(&turn, &request));
        turn["items"] = json!([{"type":"userMessage","clientId":native_input_id(&request.submission_id, &forge_provider_api::DeliveryAttemptId::new("different").unwrap())}]);
        assert!(!retractable_turn(&turn, &request));
    }

    /// Hold the callback preparation after the adapter has registered exact cancellation ownership.
    struct BeforeSendGate {
        entered: tokio::sync::Notify,
        release: tokio::sync::Semaphore,
    }

    #[async_trait]
    impl TurnControl for BeforeSendGate {
        async fn before_send(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            self.entered.notify_one();
            self.release
                .acquire()
                .await
                .expect("fixture preparation gate remains owned")
                .forget();
            Ok(())
        }
        async fn receive(
            &self,
            scope: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            FixtureAuthority.receive(scope).await
        }
        async fn close(
            &self,
            scope: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            FixtureAuthority.close(scope).await
        }
        async fn status(
            &self,
            scope: &forge_provider_api::InvocationScope,
        ) -> Result<HostTurnStatus, ProviderError> {
            FixtureAuthority.status(scope).await
        }
        fn tool_finished(&self, _: &forge_provider_api::InvocationScope, _: Option<&str>) -> bool {
            false
        }
    }

    struct FixtureAuthority;

    impl forge_provider_api::ProviderTrace for FixtureAuthority {
        fn record(&self, _: forge_provider_api::ProviderTraceRecord) {}
    }

    #[async_trait]
    impl ProviderPermissions for FixtureAuthority {
        async fn authorize(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: PermissionRequest,
        ) -> PermissionResponse {
            PermissionResponse::Deny { reason: None }
        }

        async fn before_tool(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    #[async_trait]
    impl ProviderTools for FixtureAuthority {
        async fn call(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: ToolCall,
        ) -> Result<String, ProviderError> {
            Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Rejected,
            ))
        }
    }

    #[async_trait]
    impl TurnControl for FixtureAuthority {
        async fn before_send(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn receive(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn close(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn status(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<HostTurnStatus, ProviderError> {
            Ok(HostTurnStatus {
                replay_safe: false,
                control_handoff: false,
                cancelled: false,
                error: None,
            })
        }
        fn tool_finished(&self, _: &forge_provider_api::InvocationScope, _: Option<&str>) -> bool {
            false
        }
    }

    #[async_trait]
    impl forge_provider_api::ProviderHostEffects for FixtureAuthority {
        async fn settle_foreground(
            &self,
            _: &BindingId,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), HostEffectFailure> {
            Ok(())
        }
    }

    struct FixtureReceipt(forge_provider_api::SubmissionId);

    #[async_trait]
    impl InputReceipt for FixtureReceipt {
        fn id(&self) -> &forge_provider_api::SubmissionId {
            &self.0
        }
        fn attempt_id(&self) -> Result<forge_provider_api::DeliveryAttemptId, ProviderError> {
            Ok(forge_provider_api::DeliveryAttemptId::new("attempt")?)
        }
        async fn record_sending(
            self: Arc<Self>,
            _: Option<forge_provider_api::RecoveryToken>,
        ) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn record_accepted(
            self: Arc<Self>,
            _: Option<forge_provider_api::RecoveryToken>,
        ) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn record_rejected(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn record_failure(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    fn request() -> PromptRequest {
        let authority = Arc::new(FixtureAuthority);
        let (dispatch, _) = forge_provider_api::submission::DispatchReporter::channel(
            forge_provider_api::BindingId::new("binding").unwrap(),
            forge_provider_api::InvocationScope::new(
                forge_provider_api::ExecutionOwner::root("continuation-test"),
                "invocation",
            ),
            forge_provider_api::SubmissionId::try_from("submission").unwrap(),
            forge_provider_api::DeliveryAttemptId::new("attempt").unwrap(),
        );
        PromptRequest {
            dispatch,
            scope: forge_provider_api::InvocationScope::new(
                forge_provider_api::ExecutionOwner::root("continuation-test"),
                "invocation",
            ),
            mode: forge_provider_api::input::PromptMode::Chat,
            configuration: InvocationConfiguration {
                model: ModelSelection::default(),
                access: AccessPolicy::default(),
                approval: ApprovalMode::Ask,
            },
            task: None,
            tools: Vec::new(),
            tool_endpoint: None,
            resource_lease: None,
            attempt_id: forge_provider_api::DeliveryAttemptId::new("attempt").unwrap(),
            submission_id: forge_provider_api::SubmissionId::try_from("submission".to_owned())
                .expect("fixture identity is valid"),
            input: PromptInput::Text("original request".into()),
            recovered_inputs: Vec::new(),
            instructions: "instructions".into(),
            attachments: Vec::new(),
            receipt: Arc::new(FixtureReceipt(
                forge_provider_api::SubmissionId::try_from("submission".to_owned())
                    .expect("fixture identity is valid"),
            )),
            permissions: authority.clone(),
            provider_tools: authority.clone(),
            turn_control: authority.clone(),
            host_effects: authority,
        }
    }
}
