use super::{
    normalization::CodexNormalization,
    resources::CodexResources,
    rpc::{NativeRpc, RpcHandle},
    runtime::CodexRuntime,
    security::CodexSecurity,
};
use async_trait::async_trait;
use forge_provider_api::{
    AdapterActivity, OperationContext, ProviderError, ProviderFailure, ProviderModel,
    catalog::{McpQuery, QueryContext},
    error::{ProviderFailureKind, ProviderOperation},
    events::{self, ProviderEventSink, ProviderEventStream},
    generation::ReasoningSelection,
    input::PromptInput,
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
    owners: Arc<Mutex<HashMap<forge_provider_api::ThreadId, BindingId>>>,
    catalog: Arc<Mutex<Option<CatalogConnection>>>,
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
                ProviderOperation::ThreadOpen,
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
            let command = self.command.clone();
            let native = super::operations::current();
            *catalog = Some(CatalogConnection::Starting(tokio::spawn(async move {
                let connect = async move {
                    let connection = runtime.connect().await.map_err(|error| {
                        tracing::error!(target: "forge::provider", event_name = "provider.startup.failed",
                            provider = "codex", error = %format_args!("{error:#}"));
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
        Err(failure(ProviderOperation::TurnStart, kind))
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
                ProviderOperation::ThreadOpen,
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
                ProviderOperation::ThreadOpen,
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
                    ProviderOperation::ThreadOpen,
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
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if request.workspace.trim().is_empty()
            || self.owners.lock().await.contains_key(&request.thread_id)
        {
            return Err(failure(
                ProviderOperation::ThreadOpen,
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
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Policy,
            ));
        }
        validate_model_selection(&request.configuration.model)?;
        let security = CodexSecurity::new(&request.configuration.access)?;
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string())
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Unknown))?;
        let session_scope = forge_provider_api::EventScope::Binding {
            thread_id: request.thread_id.clone(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), session_scope);

        let operation_id = super::operations::current()
            .expect("open preparation has an operation owner")
            .context
            .id()
            .clone();
        let profile = crate::profile::ResourceProfile::create(&self.runtime_root, "codex")
            .map_err(|error| error.into_provider_error(ProviderOperation::ThreadOpen))?;
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
            .map_err(|error| error.into_provider_error(ProviderOperation::ThreadOpen))?;
        let private_home = profile.path().to_path_buf();
        let resources = CodexResources::new(
            request.configuration.resources.clone(),
            private_home.clone(),
        );
        let (runtime, isolation_auth) = if isolated {
            let rpc = self.catalog_connection().await?;
            let catalog = rpc
                .request("model/list", json!({}), ProviderOperation::ThreadOpen)
                .await?;
            let models = super::catalog::parse_model_catalog(&catalog);
            let selected = match request.configuration.model.model.as_deref() {
                Some(id) => models.iter().find(|model| model.id == id),
                None => models.iter().find(|model| model.is_default),
            }
            .ok_or_else(|| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Rejected))?;
            let config = rpc
                .request(
                    "config/read",
                    json!({"includeLayers":false,"cwd":request.workspace}),
                    ProviderOperation::ThreadOpen,
                )
                .await?;
            let config = config.get("config").ok_or_else(|| {
                failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol)
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
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Transport,
            )
        })?;
        let rpc = Arc::new(NativeRpc::new(connection));
        *pending.rpc.lock().await = Some(rpc.clone());
        let initialized = rpc.request("initialize", json!({
            "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
            "capabilities":{"experimentalApi":true}
        }), ProviderOperation::ThreadOpen).await?;
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
                    failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol)
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
                "experimentalRawEvents":true, "developerInstructions":request.system_instructions,
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
            ProviderOperation::ThreadOpen,
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
            .request(method, parameters, ProviderOperation::ThreadOpen)
            .await?;
        let thread_id = result
            .pointer("/thread/id")
            .or_else(|| result.get("threadId"))
            .or_else(|| result.get("thread_id"))
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or_else(|| request.history.resume_id().map(|id| id.as_str().to_owned()))
            .ok_or_else(|| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol))?;
        let session_id = forge_provider_api::ProviderSessionId::try_from(thread_id)
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol))?;
        *pending.history.lock().await = Some(session_id.clone());
        let binding = SessionBinding {
            thread_id: request.thread_id.clone(),
            id: binding_id.clone(),
            provider_session_id: Some(session_id.clone()),
        };
        let mut mcp_context = super::protocol::McpContext::default();
        mcp_context.set_bindings(resources.mcp_aliases());
        let auth_status = rpc
            .request(
                "getAuthStatus",
                json!({"includeToken":false,"refreshToken":false}),
                ProviderOperation::ThreadOpen,
            )
            .await
            .ok();
        let capabilities = negotiated_capabilities(reported, auth_status.as_ref());
        let mut normalization = CodexNormalization::default();
        normalization.admit_thread(session_id.as_str(), &request.thread_id)?;
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
            owner: request.thread_id.clone(),
            workspace: request.workspace,
            model: Mutex::new(request.configuration.model.clone()),
            native_confirmation: Mutex::new(native_model_confirmation(&result)?),
            runtime: Mutex::new(runtime),
            rpc: RpcHandle::from_shared(rpc),
            sink,
            stream: Mutex::new(Some(stream)),
            child_observations: Mutex::new(HashMap::new()),
            child_runners: Arc::default(),
            callback_requests: Mutex::new(HashMap::new()),
            pending_messages: Mutex::new(Default::default()),
            normalization: Mutex::new(normalization),
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
            background: request.mode == forge_provider_api::session::SessionMode::Background,
            isolation_auth,
            configuration: Mutex::new(request.configuration.clone()),
            capabilities,
            settlements: Mutex::new(HashMap::new()),
            native_results: Mutex::new(HashMap::new()),
            close_failure: Mutex::new(None),
            close_lock: Mutex::new(()),
            settlement_lock: Mutex::new(()),
            retired_rpcs: Mutex::new(Vec::new()),
            retired_profiles: Mutex::new(Vec::new()),
            rollback: Mutex::new(None),
            turn_context: Mutex::new(Default::default()),
        });
        self.bindings
            .lock()
            .await
            .insert(binding_id.clone(), retained.clone());
        *retained.terminal_observer.lock().await = Some(start_terminal_observer(&retained));
        if let Some(start) = start_background_reader(&retained).await? {
            let _ = start.send(());
        }
        self.pending.lock().await.remove(&binding_id);
        self.owners
            .lock()
            .await
            .insert(request.thread_id, binding_id);
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
        session.check_input_admission(ProviderOperation::TurnStart)?;
        pending.check_admission(ProviderOperation::TurnStart)?;
        if session.stream.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        if request.scope.thread_id != session.owner
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
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        validate_model_selection(&request.configuration.model)?;
        let security = CodexSecurity::new(&request.configuration.access)?;
        if session.isolated
            && (!request.tools.is_empty()
                || request.tool_endpoint.is_some()
                || !matches!(
                    request.input.kind(),
                    forge_provider_api::forge::TurnKind::UserMessage {
                        input: PromptInput::Text(_)
                    }
                )
                || !request.recovered_inputs.is_empty())
        {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Policy,
            ));
        }
        let _admission = tokio::select! {
            biased;
            () = pending.cancelled() => return Err(pending.check_admission(ProviderOperation::TurnStart).expect_err("preparation cancellation is sticky")),
            admission = session.admission.lock() => admission,
        };
        pending.check_admission(ProviderOperation::TurnStart)?;
        super::operations::check()?;
        session.check_input_admission(ProviderOperation::TurnStart)?;
        collect_finished_execution(&session).await?;
        let mut active = session.active.lock().await;
        if active.is_some() {
            return Err(failure(
                ProviderOperation::TurnStart,
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
            () = pending.cancelled() => return Err(pending.check_admission(ProviderOperation::TurnStart).expect_err("preparation cancellation is sticky")),
            result = request.turn_control.before_send(&request.scope) => result?,
        }
        session
            .normalization
            .lock()
            .await
            .bind(Some(request.scope.clone()));
        session.rollback.lock().await.take();
        // Codex carries `<permissions>`, `<instructions>`, and `<task>` in one developer message;
        // the turn's data elements start the user input, followed by the user's own words.
        let blocks = request.blocks(forge_provider_api::forge::ContextChannel::DeveloperMessage);
        let current = joined([blocks.user.clone(), user_text(&request.input)]);
        let recovered = request
            .recovered_inputs
            .iter()
            .map(|recovered| (recovered.input.clone(), user_text(&recovered.input)))
            .collect::<Vec<_>>();
        let prompt = forge_provider_api::input::recovered_text(&current, &recovered);
        let mut attachments = request
            .recovered_inputs
            .iter()
            .flat_map(|recovered| recovered.attachments.iter().cloned())
            .collect::<Vec<_>>();
        attachments.extend(request.attachments.iter().cloned());
        let mut native_input = super::attachments::input(prompt, &attachments);
        for recovered in &request.recovered_inputs {
            if let Some(PromptInput::Skill { name, .. }) = recovered.input.user_input() {
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
        if let Some(PromptInput::Skill { name, .. }) = request.input.user_input() {
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
            apply_model_selection(&rpc, &mut params, &model, ProviderOperation::TurnStart).await?;
        drop(rpc);
        // Harness tracks which standing sections the conversation already accepted, so the
        // developer message is empty when nothing changed and the input has no instructions.
        let developer = Some(blocks.lead)
            .filter(|block| !block.is_empty())
            .into_iter()
            .collect::<Vec<_>>();
        // Codex prefers its catalog's mode text over per-turn mode instructions, so this slot
        // carries only the static open-time text and per-turn context travels as history items.
        params["collaborationMode"] = json!({
            "mode":"default",
            "settings":{
                "model":params["model"].clone(),
                "reasoning_effort":effort.clone(),
                "developer_instructions":session.system_instructions.clone()
            }
        });
        security.apply_turn(&mut params, &session.workspace);
        params["config"]["features.goals"] = json!(false);
        if session.isolated {
            params["approvalPolicy"] = json!("never");
            params["sandboxPolicy"] = json!({"type":"readOnly","networkAccess":false});
        } else if session.background {
            // Harness rejects every Background approval request; the read-only sandbox without
            // network keeps commands Codex runs unprompted from changing anything.
            params["sandboxPolicy"] = json!({"type":"readOnly","networkAccess":false});
        }
        let thread_id = session.history_id();
        let rpc = session.rpc.lock().await;
        let injected = session
            .turn_context
            .lock()
            .await
            .deliver(&rpc, &thread_id, &developer, || {
                // An injection persists native history, so the thread can no longer be
                // replaced as unused even if the turn never starts.
                session
                    .history
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .establish();
            })
            .await?;
        drop(rpc);
        if !injected {
            // Without developer injection, the instruction block starts the user input instead.
            if let Some(input) = params["input"].as_array_mut() {
                input.insert(0, json!({"type":"text","text":developer.concat()}));
            }
        }
        let native_confirmation = session
            .native_confirmation
            .lock()
            .await
            .as_ref()
            .filter(|model| {
                Some(model.model_id.as_str()) == params.get("model").and_then(Value::as_str)
            })
            .cloned();
        let token = recovery_token(&session, &request.submission_id, &request.attempt_id)?;
        if let Err(error) = request.receipt.clone().record_sending(Some(token)).await {
            return Err(record_receipt_failure(request.receipt.clone(), error).await);
        }
        if let Err(error) = pending.check_admission(ProviderOperation::TurnStart) {
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
        let NativeAdmission {
            turn_id,
            initial_messages,
            model: admission_model,
            started_at_ms: native_started_at_ms,
        } = match await_admission(&session, &request, id).await {
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
        let native_address = NativeTurnAddress {
            thread_id: session.history_id().as_str().to_owned(),
            turn_id: NativeTurnId::new(turn_id.clone())?,
        };
        let address = match session.normalization.lock().await.admit_turn(
            &native_address.thread_id,
            native_address.turn_id.as_str(),
            &request.scope,
        ) {
            Ok(address) => address,
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
        let (cancel, receiver) = mpsc::unbounded_channel();
        if admitting.cancel.load(Ordering::Acquire) || session.is_fenced() {
            let _ = cancel.send(());
        }
        let (turn_updates, _) = watch::channel(Some(turn_id.clone()));
        let submissions = Arc::new(Mutex::new(request.input_coverage()));
        let active_execution = ActiveExecution {
            scope: request.scope.clone(),
            native_turn_id: turn_id,
            turn_updates,
            submissions: submissions.clone(),
            request: request.clone(),
            cancel,
            task: None,
        };
        *active = Some(active_execution);
        let accepted_submission_id = request.submission_id.clone();
        let dispatched = request.dispatch.clone();
        let task = Arc::new(crate::turn_runner::RetainedTurn::spawn(
            Arc::new(CodexTurnDriver {
                session: session.clone(),
                started_at_ms: native_started_at_ms,
                native_confirmation,
                admission_model,
                request,
                address: address.clone(),
                native_address,
                submissions,
                cancel: Mutex::new(Some(receiver)),
                initial_messages: Mutex::new(Some(initial_messages)),
                cleanup_errors: Arc::new(Mutex::new(Vec::new())),
            }),
            session.sink.clone(),
            address,
        )?);
        active.as_mut().expect("active execution was inserted").task = Some(task.clone());
        let collector_session = session.clone();
        let collector_scope = active
            .as_ref()
            .expect("active execution was inserted")
            .scope
            .clone();
        let started = task.clone();
        drop(active);
        let collector = tokio::spawn(async move {
            collect_invocation(collector_session, collector_scope, task).await
        });
        self.operations
            .lock()
            .await
            .push(Arc::new(crate::retained_task::RetainedTask::new(
                ProviderOperation::TurnStart,
                collector,
            )));
        started.started().await?;
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
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::Policy,
            ));
        }
        let _admission = session.admission.lock().await;
        super::operations::check()?;
        session.check_input_admission(ProviderOperation::ThreadConfigure)?;
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        if session
            .child_observations
            .lock()
            .await
            .values()
            .any(|child| child.result.borrow().is_none())
        {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        let history = session
            .history
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        let mut next_profile = crate::profile::ResourceProfile::create(&self.runtime_root, "codex")
            .map_err(|error| error.into_provider_error(ProviderOperation::ThreadConfigure))?;
        if let Err(error) = next_profile.bind_skills(&resources.skills, "skills") {
            let primary = error.into_provider_error(ProviderOperation::ThreadConfigure);
            return Err(match next_profile.close() {
                Ok(()) => primary,
                Err(error) => {
                    session.retired_profiles.lock().await.push(next_profile);
                    combine_primary(
                        primary,
                        error.into_provider_error(ProviderOperation::ThreadConfigure),
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
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::ResourceConflict,
                ));
            }
            apply_model_selection(
                &current_rpc,
                &mut model_parameters,
                &configuration.model,
                ProviderOperation::ThreadConfigure,
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
                            error.into_provider_error(ProviderOperation::ThreadConfigure),
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
                        ProviderOperation::ThreadConfigure,
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
            }), ProviderOperation::ThreadConfigure).await?;
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
                NativeHistory::Virgin(_) => json!({"cwd":session.workspace,"ephemeral":false,"historyMode":"paginated","experimentalRawEvents":true,"developerInstructions":session.system_instructions,"dynamicTools":session.native_tools.iter().map(|tool| json!({"name":tool.name,"description":tool.description,"inputSchema":tool.input_schema})).collect::<Vec<_>>(),"config":{}}),
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
            release_native_thread_writer(&current_rpc, &thread_id, ProviderOperation::ThreadConfigure).await?;
            let method = match &history { NativeHistory::Virgin(_) => "thread/start", NativeHistory::Established(_) => "thread/resume" };
            let applied = next_rpc.request(
                    method,
                    parameters,
                    ProviderOperation::ThreadConfigure,
                )
                .await?;
            let next_id = applied.pointer("/thread/id").and_then(Value::as_str).ok_or_else(|| failure(ProviderOperation::ThreadConfigure, ProviderFailureKind::Protocol))?;
            let next_id = forge_provider_api::ProviderSessionId::try_from(next_id.to_owned()).map_err(|_| failure(ProviderOperation::ThreadConfigure, ProviderFailureKind::Protocol))?;
            if matches!(&history, NativeHistory::Established(_)) && next_id != thread_id { return Err(failure(ProviderOperation::ThreadConfigure, ProviderFailureKind::HistoryConflict)); }
            Ok::<_, ProviderError>((model, access, approval, next_id, native_model_confirmation(&applied)?))
        }
        .await;
        let (model, access, approval, next_history, native_confirmation) = match resumed {
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
        *session.native_confirmation.lock().await = native_confirmation;
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
                .activity_writer()
                .send_wait(AdapterActivity::Thread {
                    thread_id: session.owner.clone(),
                    event: forge_provider_api::ThreadEvent::IdentityResolved(next_history),
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
                            ProviderOperation::ThreadConfigure,
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
    /// Revert native history to end at the requested turn checkpoint.
    ///
    /// Codex checkpoints name the completed turn itself, so the turn after the checkpoint is the
    /// first one `thread/revert` excludes. An absent successor means history already ends at the
    /// checkpoint, which keeps a repeated request a successful no-op.
    async fn rewind_binding(
        &self,
        binding: &BindingId,
        request: forge_provider_api::ThreadRewindRequest,
    ) -> Result<(), ProviderError> {
        const OPERATION: ProviderOperation = ProviderOperation::ThreadRewind;
        let session = self.binding(binding).await?;
        let _admission = session.admission.lock().await;
        session.check_input_admission(OPERATION)?;
        collect_settled_execution(&session).await?;
        if session.active.lock().await.is_some() {
            return Err(failure(OPERATION, ProviderFailureKind::ExecutionActive));
        }
        let thread_id = session.history_id().as_str().to_owned();
        let rpc = session.rpc.lock().await;
        let turn_ids = |history: &Value| {
            history
                .pointer("/thread/turns")
                .and_then(Value::as_array)
                .map(|turns| {
                    turns
                        .iter()
                        .filter_map(|turn| turn.get("id").and_then(Value::as_str))
                        .map(str::to_owned)
                        .collect::<Vec<_>>()
                })
        };
        let history = rpc
            .request(
                "thread/read",
                json!({"threadId":thread_id,"includeTurns":true}),
                OPERATION,
            )
            .await?;
        let Some(turns) = turn_ids(&history) else {
            return Err(failure(OPERATION, ProviderFailureKind::Protocol));
        };
        let retained = match &request.after {
            Some(checkpoint) => match turns.iter().position(|id| id == checkpoint.as_str()) {
                Some(index) => index + 1,
                None => return Err(failure(OPERATION, ProviderFailureKind::Rejected)),
            },
            None => 0,
        };
        let Some(first_removed) = turns.get(retained) else {
            return Ok(());
        };
        super::operations::mark_changed();
        super::operations::check()?;
        *session.rollback.lock().await = None;
        rpc.request(
            "thread/revert",
            json!({"threadId":thread_id,"beforeTurnId":first_removed}),
            OPERATION,
        )
        .await?;
        let history = rpc
            .request(
                "thread/read",
                json!({"threadId":thread_id,"includeTurns":true}),
                OPERATION,
            )
            .await?;
        if turn_ids(&history).as_deref() != Some(&turns[..retained]) {
            return Err(
                ProviderFailure::new(OPERATION, ProviderFailureKind::Protocol)
                    .at_phase(
                        forge_provider_api::ProviderPhase::Observation,
                        forge_provider_api::FailureDisposition::Uncertain,
                    )
                    .into(),
            );
        }
        Ok(())
    }

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
        if !session.settlements.lock().await.get(&request.scope).is_some_and(|settlement| matches!(settlement, Ok(settlement) if matches!(settlement.completed.result().outcome(), forge_provider_api::turn::TurnOutcome::Interrupted))) {
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
                "thread/revert",
                json!({"threadId":authorization.thread_id,"beforeTurnId":authorization.turn_id.as_str()}),
                ProviderOperation::InputRetract,
            )
            .await
        {
            // A runtime without `thread/revert` rejects it through the generic invalid-request
            // path before touching history. A positive rejection proves the cancelled turn
            // remains, so the binding stays live and the turn stays cancelled.
            if matches!(&error, ProviderError::Failure(failure) if failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
            {
                if !session.closing.load(Ordering::Acquire)
                    && !session.invalidated.load(Ordering::Acquire)
                {
                    session.fenced.store(false, Ordering::Release);
                }
                return Ok(Outcome::Unavailable(Unavailable::Unsupported));
            }
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
        // Harness clears its delivery record on retraction, so the next turn re-sends any
        // context the revert removed.
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
        session.check_input_admission(ProviderOperation::TurnSteer)?;
        if request.scope.thread_id != session.owner {
            return Err(failure(
                ProviderOperation::TurnSteer,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if session.stream.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::TurnSteer,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        let _admission = session.admission.lock().await;
        session.check_input_admission(ProviderOperation::TurnSteer)?;
        let (initial_turn, submissions, prompt_request) = {
            let active = session.active.lock().await;
            let Some(execution) = active.as_ref() else {
                return Err(failure(
                    ProviderOperation::TurnSteer,
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
                    ProviderOperation::TurnSteer,
                    ProviderFailureKind::WrongInvocation,
                ));
            }
            (
                execution.native_turn_id.clone(),
                execution.submissions.clone(),
                execution.request.clone(),
            )
        };
        let submission_id = request.delivery.submission_id().cloned();
        let receipt = request.delivery.receipt().cloned();
        let covered_input = receipt
            .as_ref()
            .map(|receipt| {
                receipt.attempt_id().map(|attempt| {
                    forge_provider_api::CoveredInput::new(receipt.id().clone(), attempt)
                })
            })
            .transpose()?;
        if let (Some(receipt), Some(submission_id)) = (&receipt, &submission_id) {
            let token = recovery_token(
                &session,
                submission_id,
                covered_input
                    .as_ref()
                    .expect("journaled steering retained its checked attempt")
                    .attempt_id(),
            )?;
            if let Err(error) = receipt.clone().record_sending(Some(token)).await {
                return Err(record_receipt_failure(receipt.clone(), error).await);
            }
        }
        // A steered input carries its own elements, such as a question response, and no
        // standing context; they precede the user's words in the same user message.
        let prompt = joined([
            forge_provider_api::forge::input_block(&request.input),
            user_text(&request.input),
        ]);
        let expected_turn_id = session
            .active
            .lock()
            .await
            .as_ref()
            .map(|execution| execution.native_turn_id.clone())
            .unwrap_or_else(|| initial_turn.clone());
        let mut params = json!({
            "threadId":session.history_id().as_str(),
            "expectedTurnId":expected_turn_id,
            "input":super::attachments::input(prompt, &request.attachments)
        });
        if let Some(submission_id) = &submission_id {
            params["clientUserMessageId"] = json!(native_input_id(
                submission_id,
                covered_input
                    .as_ref()
                    .expect("journaled steering retained its checked attempt")
                    .attempt_id()
            ));
        }
        let result = session
            .rpc
            .lock()
            .await
            .request("turn/steer", params, ProviderOperation::TurnSteer)
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
            let accepted_token = match recovery_token(
                &session,
                &submission_id,
                covered_input
                    .as_ref()
                    .expect("journaled steering retained its checked attempt")
                    .attempt_id(),
            ) {
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
            let input = covered_input
                .expect("journaled steering retained its checked receipt identity before dispatch");
            submissions.lock().await.push(input);
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
        session.check_input_admission(ProviderOperation::ThreadCompact)?;
        if *observation {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if request.scope.thread_id() != &session.owner {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        // A terminal event can arrive before its reader returns. Join only a
        // settled execution; compaction must never interrupt an active turn.
        collect_settled_execution(&session).await?;
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        if session
            .child_observations
            .lock()
            .await
            .values()
            .any(|child| child.result.borrow().is_none())
        {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        // Transfer exclusive notification consumption from the idle reader.
        super::operations::check()?;
        stop_background_reader(&session).await?;
        let result = compact_session(&session, operation.context.id(), request).await;
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
                                failure(ProviderOperation::TurnStart, ProviderFailureKind::Runtime)
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
                    .check_admission(ProviderOperation::TurnStart)
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
            session.check_input_admission(ProviderOperation::ThreadFork)?;
            collect_settled_execution(session).await?;
            if session.active.lock().await.is_some() {
                return Err(failure(
                    ProviderOperation::ThreadFork,
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
            ProviderOperation::ThreadFork,
        )
        .await?;
        security.apply_thread(&mut parameters, &request.target.workspace);
        let result = async {
            let value = rpc
                .request("thread/fork", parameters, ProviderOperation::ThreadFork)
                .await?;
            let id = value
                .pointer("/thread/id")
                .and_then(Value::as_str)
                .ok_or_else(|| {
                    failure(ProviderOperation::ThreadFork, ProviderFailureKind::Protocol)
                })?;
            let id =
                forge_provider_api::ProviderSessionId::try_from(id.to_owned()).map_err(|_| {
                    failure(ProviderOperation::ThreadFork, ProviderFailureKind::Protocol)
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
        release_native_thread_writer(&target.rpc, &target.target, ProviderOperation::ThreadFork)
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
    ) -> Result<forge_provider_api::terminal::TerminalInventory, ProviderError> {
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
                ProviderOperation::TurnStart
                | ProviderOperation::TurnSteer
                | ProviderOperation::ThreadFork
                | ProviderOperation::ThreadRewind
                | ProviderOperation::TerminalTerminate => (
                    forge_provider_api::ProviderPhase::Dispatch,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
                ProviderOperation::TurnInterrupt => (
                    forge_provider_api::ProviderPhase::Collection,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
                ProviderOperation::ThreadClose | ProviderOperation::Shutdown => (
                    forge_provider_api::ProviderPhase::Close,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
                ProviderOperation::ThreadOpen
                | ProviderOperation::ThreadConfigure
                | ProviderOperation::InputRetract => (
                    forge_provider_api::ProviderPhase::Collection,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
                ProviderOperation::ThreadCompact => (
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
                ProviderOperation::Inspect
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
    ) -> Result<CollectedInvocation, ProviderError> {
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
                    failure(
                        ProviderOperation::TurnInterrupt,
                        ProviderFailureKind::Transport,
                    )
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
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::WrongInvocation,
                ));
            };
            let task = execution.task.clone().ok_or_else(|| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::Protocol,
                )
            })?;
            if !task.is_finished() && execution.turn_updates.borrow().is_some() {
                execution.cancel.send(()).map_err(|_| {
                    failure(
                        ProviderOperation::TurnInterrupt,
                        ProviderFailureKind::Transport,
                    )
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
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::Protocol,
                )
            })?
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
                    ProviderOperation::ThreadClose,
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
        let children = session.child_runners.lock().await.clone();
        if !children.is_empty()
            && let Some(start) = start_background_reader(&session).await?
        {
            let _ = start.send(());
        }
        for child in &children {
            if child.native_result().await.is_none()
                && let Err(error) = child.interrupt().await
            {
                errors.push(error);
            }
        }
        for child in children {
            if let Err(error) = child.join().await {
                errors.push(error);
            }
        }
        session.child_runners.lock().await.clear();
        session.child_observations.lock().await.clear();
        session.callback_requests.lock().await.clear();
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
                errors.push(error.into_provider_error(ProviderOperation::ThreadClose));
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
                        ProviderOperation::TurnStart,
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
                errors.push(error.into_provider_error(ProviderOperation::ThreadClose));
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
                    errors.push(error.into_provider_error(ProviderOperation::ThreadClose))
                }
            }
        }
        combine_errors(errors)
    }
}

#[async_trait]
impl Provider for CodexProvider {
    async fn thread_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        request.validate_thread_owner()?;
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::ThreadOpen);
        let payload = request.identity();
        let target = CleanupTarget::Open(context.id().clone());
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .open
            .take(
                context,
                ProviderOperation::ThreadOpen,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .open_session(request)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::ThreadOpen))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn thread_configure(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: forge_provider_api::SessionConfiguration,
    ) -> Result<forge_provider_api::ConfiguredSession, ProviderError> {
        let native = super::operations::NativeContext::new(
            context.clone(),
            ProviderOperation::ThreadConfigure,
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
                ProviderOperation::ThreadConfigure,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .configure_binding(&binding, configuration)
                            .await
                            .map_err(|error| {
                                operation_error(error, ProviderOperation::ThreadConfigure)
                            })
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn thread_fork(
        &self,
        context: OperationContext,
        request: forge_provider_api::SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::ThreadFork);
        let payload = request.clone();
        let target = CleanupTarget::None;
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let worker_native = native.clone();
        self.records
            .fork
            .observe(
                context,
                ProviderOperation::ThreadFork,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .fork_history(request)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::ThreadFork))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn thread_compact(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        let native = super::operations::NativeContext::new(
            context.clone(),
            ProviderOperation::ThreadCompact,
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
                ProviderOperation::ThreadCompact,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .compact_binding(&binding, request)
                            .await
                            .map_err(|error| {
                                operation_error(error, ProviderOperation::ThreadCompact)
                            })
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn thread_close(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::ThreadClose);
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
                ProviderOperation::ThreadClose,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::mark_changed();
                        super::operations::check()?;
                        super::operations::cleanup(provider.close_binding(&binding))
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::ThreadClose))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn turn_interrupt(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CancelRequest,
    ) -> Result<forge_provider_api::TurnInterruptResult, ProviderError> {
        let native = super::operations::NativeContext::new(
            context.clone(),
            ProviderOperation::TurnInterrupt,
        );
        let payload = (binding.clone(), request.clone());
        let target = CleanupTarget::None;
        let binding = binding.clone();
        let provider = self.clone();
        let cleanup_provider = self.clone();
        let operation_id = context.id().clone();
        let worker_native = native.clone();
        self.records
            .cancel
            .observe(
                context,
                ProviderOperation::TurnInterrupt,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::mark_changed();
                        super::operations::check()?;
                        super::operations::cleanup(
                            provider.cancel_invocation(&binding, &request.scope),
                        )
                        .await
                        .map(|collected| forge_provider_api::TurnInterruptResult {
                            operation_id,
                            thread_id: collected.address.thread_id,
                            turn_id: collected.address.turn_id,
                            watermark: collected.watermark,
                        })
                        .map_err(|error| operation_error(error, ProviderOperation::TurnInterrupt))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn turn_start(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        request.validate_turn_identity()?;
        let pending = self
            .pending_inputs
            .register(binding, &request.scope, context.id());
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::TurnStart);
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
                ProviderOperation::TurnStart,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .submit_input(&binding, request, pending)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::TurnStart))
                    })
                    .await
                },
                move |reason| cleanup_provider.operation_interruption(native, target, reason),
            )
            .await
    }

    async fn turn_steer(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::TurnSteer);
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
                ProviderOperation::TurnSteer,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .steer_input(&binding, request)
                            .await
                            .map_err(|error| operation_error(error, ProviderOperation::TurnSteer))
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

    async fn thread_rewind(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: forge_provider_api::ThreadRewindRequest,
    ) -> Result<(), ProviderError> {
        let native =
            super::operations::NativeContext::new(context.clone(), ProviderOperation::ThreadRewind);
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
            .rewind
            .observe(
                context,
                ProviderOperation::ThreadRewind,
                payload,
                async move {
                    super::operations::scope(worker_native, async move {
                        super::operations::check()?;
                        provider
                            .rewind_binding(&binding, request)
                            .await
                            .map_err(|error| {
                                operation_error(error, ProviderOperation::ThreadRewind)
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
    ) -> Result<forge_provider_api::terminal::TerminalInventory, ProviderError> {
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

/// Preserve a model positively reported by native startup rather than inferring a configured default.
fn native_model_confirmation(
    snapshot: &Value,
) -> Result<Option<forge_provider_api::ResolvedModel>, ProviderError> {
    let model = snapshot
        .get("model")
        .or_else(|| snapshot.pointer("/thread/model"));
    match model {
        None | Some(Value::Null) => Ok(None),
        Some(Value::String(model)) => Ok(Some(forge_provider_api::ResolvedModel {
            model_id: forge_provider_api::ModelId::new(model).map_err(|_| {
                failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol)
            })?,
            observed_at_ms: now_ms(),
        })),
        Some(_) => Err(failure(
            ProviderOperation::ThreadOpen,
            ProviderFailureKind::Protocol,
        )),
    }
}

/// Keep a validated native turn identifier private to the Codex transport boundary.
#[derive(Clone, Debug, Eq, PartialEq)]
struct NativeTurnId(String);

impl NativeTurnId {
    /// Create a native identity without allowing an empty protocol correlation target.
    fn new(value: impl Into<String>) -> Result<Self, ProviderError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Protocol,
            ));
        }
        Ok(Self(value))
    }
    /// Get the native spelling only when addressing its owning protocol transport.
    fn as_str(&self) -> &str {
        &self.0
    }
}

/// Retain native protocol correlation independently of published host identities.
struct NativeTurnAddress {
    thread_id: String,
    turn_id: NativeTurnId,
}

/// Preserve collected canonical evidence for repeated exact-scope interruption.
#[derive(Clone)]
struct CollectedInvocation {
    address: forge_provider_api::TurnAddress,
    completed: forge_provider_api::CompletedTurn,
    watermark: forge_provider_api::EventSequence,
}

/// Keep one native response and its cleanup behind the common retained turn runner.
struct CodexTurnDriver {
    session: Arc<CodexBinding>,
    started_at_ms: i64,
    native_confirmation: Option<forge_provider_api::ResolvedModel>,
    admission_model: Option<forge_provider_api::ResolvedModel>,
    request: PromptRequest,
    address: forge_provider_api::TurnAddress,
    native_address: NativeTurnAddress,
    submissions: Arc<Mutex<Vec<forge_provider_api::CoveredInput>>>,
    cancel: Mutex<Option<mpsc::UnboundedReceiver<()>>>,
    initial_messages: Mutex<Option<Vec<Value>>>,
    cleanup_errors: Arc<Mutex<Vec<ProviderError>>>,
}

#[async_trait]
impl crate::turn_runner::NativeTurnDriver for CodexTurnDriver {
    /// Observe the acknowledged native turn and retain its unique terminal outcome.
    async fn run(
        &self,
        start: forge_provider_api::NativeTurnStart,
    ) -> Result<forge_provider_api::TurnResult, ProviderError> {
        start.observed(self.started_at_ms).await?;
        if let Some(model) = &self.native_confirmation {
            let event = self
                .session
                .normalization
                .lock()
                .await
                .resolve_model(&self.address, model.clone());
            if let Some(event) = event {
                publish_event(&self.session, event).await?;
            }
        }
        if let Some(model) = &self.admission_model {
            let event = self
                .session
                .normalization
                .lock()
                .await
                .resolve_model(&self.address, model.clone());
            if let Some(event) = event {
                publish_event(&self.session, event).await?;
            }
        }
        let cancel =
            self.cancel.lock().await.take().ok_or_else(|| {
                failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
            })?;
        let initial =
            self.initial_messages.lock().await.take().ok_or_else(|| {
                failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
            })?;
        observe_execution(NativeTurnObservation {
            session: self.session.clone(),
            request: self.request.clone(),
            address: self.address.clone(),
            native_address: NativeTurnAddress {
                thread_id: self.native_address.thread_id.clone(),
                turn_id: self.native_address.turn_id.clone(),
            },
            submissions: self.submissions.clone(),
            cancel,
            initial_messages: initial,
            retained_cleanup: self.cleanup_errors.clone(),
        })
        .await
    }
    /// Signal the original invocation without touching a successor's native resources.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        let active = self.session.active.lock().await;
        if let Some(active) = active
            .as_ref()
            .filter(|active| active.scope == self.address.scope)
            && active.turn_updates.borrow().is_some()
        {
            active.cancel.send(()).map_err(|_| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::Transport,
                )
            })?;
        }
        Ok(())
    }
    /// Transfer certified background observation after foreground callbacks have joined.
    async fn collect(&self) -> Result<(), ProviderError> {
        if self
            .session
            .normalization
            .lock()
            .await
            .has_unresolved_sessions(&self.address)
        {
            self.session.fenced.store(true, Ordering::Release);
            let error = ProviderError::from(
                ProviderFailure::new(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::Runtime,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Collection,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
            );
            let mut retained = self.session.close_failure.lock().await;
            if retained.is_none() {
                *retained = Some(error.clone());
            }
            return Err(error);
        }
        combine_errors(std::mem::take(&mut *self.cleanup_errors.lock().await))?;
        if let Some(start) = start_background_reader(&self.session).await? {
            let _ = start.send(());
        }
        Ok(())
    }
}

/// Retain the decoder's ordered terminal and callback-collection proof for one native child.
struct ChildObservation {
    address: forge_provider_api::TurnAddress,
    callbacks: Arc<Mutex<Vec<Arc<crate::retained_task::RetainedTask>>>>,
    result: watch::Sender<Option<Result<forge_provider_api::TurnResult, ProviderError>>>,
    collected: watch::Sender<Option<Result<(), ProviderError>>>,
}

impl Drop for ChildObservation {
    /// Fail pending child observation instead of inventing completion when its dispatcher is lost.
    fn drop(&mut self) {
        let error = failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport);
        if self.result.borrow().is_none() {
            self.result.send_replace(Some(Err(error.clone())));
        }
        if self.collected.borrow().is_none() {
            self.collected.send_replace(Some(Err(error)));
        }
    }
}

/// Share the binding's transport while owning one child's exact native end and callback boundary.
struct ObservedChildDriver {
    session: Arc<CodexBinding>,
    callbacks: Arc<Mutex<Vec<Arc<crate::retained_task::RetainedTask>>>>,
    native_thread: String,
    native_turn: String,
    started_at_ms: i64,
    started: Mutex<Option<tokio::sync::oneshot::Sender<Result<(), ProviderError>>>>,
    result: watch::Receiver<Option<Result<forge_provider_api::TurnResult, ProviderError>>>,
    collected: watch::Receiver<Option<Result<(), ProviderError>>>,
}

#[async_trait]
impl crate::turn_runner::NativeTurnDriver for ObservedChildDriver {
    /// Publish positive child start before exposing its first metadata or item activity.
    async fn run(
        &self,
        start: forge_provider_api::NativeTurnStart,
    ) -> Result<forge_provider_api::TurnResult, ProviderError> {
        let started = start.observed(self.started_at_ms).await;
        if let Some(observer) = self.started.lock().await.take() {
            let _ = observer.send(started.clone());
        }
        started?;
        let mut result = self.result.clone();
        loop {
            if let Some(result) = result.borrow_and_update().clone() {
                return result;
            }
            result.changed().await.map_err(|_| {
                failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport)
            })?;
        }
    }
    /// Interrupt exactly this native child turn without redirecting the parent or a successor.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        self.session
            .rpc
            .lock()
            .await
            .request(
                "turn/interrupt",
                json!({"threadId":self.native_thread,"turnId":self.native_turn}),
                ProviderOperation::TurnInterrupt,
            )
            .await
            .map(|_| ())
    }
    /// Await the dispatcher's proof that every preceding child callback was fully joined.
    async fn collect(&self) -> Result<(), ProviderError> {
        let mut collected = self.collected.clone();
        loop {
            let observed = collected.borrow_and_update().clone();
            if let Some(collected) = observed {
                collected?;
                for callback in self.callbacks.lock().await.clone() {
                    callback.join().await?;
                }
                return Ok(());
            }
            collected.changed().await.map_err(|_| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::Transport,
                )
            })?;
        }
    }
}

/// Install child runners only from positively trusted and independently observed native starts.
async fn begin_child_turns(
    session: &Arc<CodexBinding>,
    observations: &mut HashMap<forge_provider_api::TurnId, ChildObservation>,
    retained: &Arc<Mutex<Vec<Arc<crate::turn_runner::RetainedTurn>>>>,
) -> Result<(), ProviderError> {
    let starts = session.normalization.lock().await.take_observed_starts();
    for (address, native_thread, native_turn, started_at_ms) in starts {
        let (result, observed) = watch::channel(None);
        let (collected, collection) = watch::channel(None);
        let (started, start) = tokio::sync::oneshot::channel();
        let callbacks = Arc::new(Mutex::new(Vec::new()));
        let driver = Arc::new(ObservedChildDriver {
            session: session.clone(),
            callbacks: callbacks.clone(),
            native_thread,
            native_turn,
            started_at_ms,
            started: Mutex::new(Some(started)),
            result: observed,
            collected: collection,
        });
        let runner = Arc::new(crate::turn_runner::RetainedTurn::spawn_observed(
            driver,
            session.sink.clone(),
            address.clone(),
        )?);
        observations.insert(
            address.turn_id.clone(),
            ChildObservation {
                address,
                callbacks,
                result,
                collected,
            },
        );
        retained.lock().await.push(runner);
        start
            .await
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport))??;
    }
    Ok(())
}

/// Transfer exact child terminal evidence only after ordered native callbacks and activity delivery finish.
async fn finish_child_turns(
    session: &CodexBinding,
    observations: &mut HashMap<forge_provider_api::TurnId, ChildObservation>,
) -> Result<(), ProviderError> {
    let normalization = session.normalization.lock().await;
    for child in observations.values_mut() {
        if child.result.borrow().is_some() {
            continue;
        }
        if let Some((outcome, completed_at_ms)) = normalization.terminal_evidence(&child.address) {
            let result = forge_provider_api::TurnResult::new(
                outcome,
                completed_at_ms,
                None,
                Vec::new(),
                normalization.final_usage(&child.address),
                None,
            )
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
            child.result.send_replace(Some(Ok(result)));
            // The binding dispatcher awaits each native request handler before consuming the next frame.
            let collection = if normalization.has_unresolved_sessions(&child.address) {
                Err(ProviderFailure::new(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::Runtime,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Collection,
                    forge_provider_api::FailureDisposition::Invalidated,
                )
                .into())
            } else {
                Ok(())
            };
            child.collected.send_replace(Some(collection));
        }
    }
    Ok(())
}

struct ActiveExecution {
    scope: forge_provider_api::InvocationScope,
    native_turn_id: String,
    turn_updates: watch::Sender<Option<String>>,
    submissions: Arc<Mutex<Vec<forge_provider_api::CoveredInput>>>,
    request: PromptRequest,
    cancel: mpsc::UnboundedSender<()>,
    task: Option<Arc<crate::turn_runner::RetainedTurn>>,
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
    turn_id: NativeTurnId,
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
    owner: forge_provider_api::ThreadId,
    workspace: String,
    model: Mutex<ModelSelection>,
    native_confirmation: Mutex<Option<forge_provider_api::ResolvedModel>>,
    runtime: Mutex<Arc<CodexRuntime>>,
    rpc: RpcHandle,
    sink: ProviderEventSink,
    stream: Mutex<Option<ProviderEventStream>>,
    normalization: Mutex<CodexNormalization>,
    child_observations: Mutex<HashMap<forge_provider_api::TurnId, ChildObservation>>,
    child_runners: Arc<Mutex<Vec<Arc<crate::turn_runner::RetainedTurn>>>>,
    callback_requests: Mutex<HashMap<forge_provider_api::InvocationScope, PromptRequest>>,
    pending_messages: Mutex<std::collections::VecDeque<Value>>,
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
    turn_context: Mutex<super::turn_context::TurnContextDelivery>,
    admission: Mutex<()>,
    observations: Arc<tokio::sync::RwLock<bool>>,
    fenced: AtomicBool,
    invalidated: AtomicBool,
    closing: AtomicBool,
    provider_closed: Arc<AtomicBool>,
    isolated: bool,
    /// Retained Background generation, which must run no native tool that changes anything.
    background: bool,
    isolation_auth: Option<Arc<super::isolation::IsolationAuth>>,
    configuration: Mutex<forge_provider_api::SessionConfiguration>,
    capabilities: forge_provider_api::capability::CapabilityManifest,
    settlements: Mutex<
        HashMap<forge_provider_api::InvocationScope, Result<CollectedInvocation, ProviderError>>,
    >,
    native_results:
        Mutex<HashMap<forge_provider_api::InvocationScope, forge_provider_api::TurnResult>>,
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
            ProviderFailure::new(ProviderOperation::ThreadConfigure, cause.kind())
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
                .for_operation(ProviderOperation::ThreadConfigure)
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
                    ProviderOperation::ThreadConfigure,
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
    ProviderFailure::new(ProviderOperation::ThreadConfigure, kind)
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
    let failure = match &error {
        ProviderError::Failure(failure) => *failure,
        _ => ProviderFailure::new(
            ProviderOperation::TurnStart,
            match observation {
                forge_provider_api::ObservationFailure::Protocol => ProviderFailureKind::Protocol,
                forge_provider_api::ObservationFailure::OwnershipConflict => {
                    ProviderFailureKind::ResourceConflict
                }
                forge_provider_api::ObservationFailure::UnexpectedExecution => {
                    ProviderFailureKind::WrongInvocation
                }
                forge_provider_api::ObservationFailure::Overflow
                | forge_provider_api::ObservationFailure::Disconnected => {
                    ProviderFailureKind::Transport
                }
                forge_provider_api::ObservationFailure::LifecycleUnavailable
                | forge_provider_api::ObservationFailure::DeliveryUncertain => {
                    ProviderFailureKind::Runtime
                }
            },
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Invalidated,
        ),
    };
    session.sink.unavailable_because(failure, observation).await;
    error
}

/// Join the exact native worker and publish collection evidence once after its terminal observations.
async fn collect_invocation(
    session: Arc<CodexBinding>,
    scope: forge_provider_api::InvocationScope,
    task: Arc<crate::turn_runner::RetainedTurn>,
) -> Result<(), ProviderError> {
    let result = task.join().await;
    if let Some(Ok(native)) = task.native_result().await {
        session
            .native_results
            .lock()
            .await
            .entry(scope.clone())
            .or_insert(native);
    }
    let _settlement = session.settlement_lock.lock().await;
    if let Some(retained) = session.settlements.lock().await.get(&scope) {
        return retained.as_ref().map(|_| ()).map_err(Clone::clone);
    }
    let result = result.map(|completed| CollectedInvocation {
        address: forge_provider_api::TurnAddress {
            scope: scope.clone(),
            thread_id: scope.thread_id.clone(),
            turn_id: scope
                .turn_id()
                .expect("admitted invocation has checked turn identity"),
        },
        completed,
        watermark: session.sink.watermark(),
    });
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
            errors.push(error.into_provider_error(ProviderOperation::ThreadConfigure));
            retained_profiles.push(profile);
        }
    }
    *profiles = retained_profiles;
    combine_errors(errors)
}

/// Report whether Codex registered the root turn named by one `turn/interrupt` request.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NativeInterrupt {
    /// Codex accepted the interrupt, or no root turn was named.
    Delivered,
    /// Codex positively rejected the interrupt because it had no matching active turn.
    NotActive,
}

/// Interrupt the native foreground and descendants while retaining independent terminal identities.
async fn interrupt_native(
    session: &CodexBinding,
    thread_id: &str,
    turn_id: Option<&NativeTurnId>,
    descendants: &super::descendants::DescendantTracker,
) -> Result<NativeInterrupt, ProviderError> {
    let mut errors = Vec::new();
    let mut root = NativeInterrupt::Delivered;
    if let Some(turn_id) = turn_id
        && let Err(error) = session
            .rpc
            .lock()
            .await
            .request(
                "turn/interrupt",
                json!({"threadId":thread_id,"turnId":turn_id.as_str()}),
                ProviderOperation::TurnInterrupt,
            )
            .await
    {
        if interrupt_found_no_active_turn(&error) {
            root = NativeInterrupt::NotActive;
        } else {
            errors.push(error);
        }
    }
    for (thread_id, turn_id) in descendants.active_turns() {
        if let Some(turn_id) = turn_id
            && let Err(error) = session
                .rpc
                .lock()
                .await
                .request(
                    "turn/interrupt",
                    json!({"threadId":thread_id,"turnId":turn_id.as_str()}),
                    ProviderOperation::TurnInterrupt,
                )
                .await
            && !interrupt_found_no_active_turn(&error)
        {
            errors.push(error);
        }
    }
    if let Some(turn_id) = turn_id
        && let Err(error) = stop_foreground_commands(session, thread_id, turn_id.as_str()).await
    {
        errors.push(error);
    }
    combine_errors(errors).map(|()| root)
}

/// Get whether Codex positively rejected `turn/interrupt` because it had no matching active turn.
///
/// Codex answers an interrupt with an invalid-request error, leaving the thread unchanged, in
/// two windows: after acknowledging `turn/start` but before registering the turn, and after the
/// turn finished natively. Neither window leaves native work for the interrupt to stop.
fn interrupt_found_no_active_turn(error: &ProviderError) -> bool {
    matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Rejected && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
}

/// Record one root interrupt attempt for the turn driver.
///
/// Codex rejects an interrupt while the turn is not yet interruptible: before `turn/started`,
/// and briefly after it. The driver therefore retries on each later root notification. If the
/// turn instead ended natively, its own `turn/completed` notification ends the retries.
fn record_interrupt_attempt(
    attempt: Result<NativeInterrupt, ProviderError>,
    retry_pending: &mut bool,
    errors: &mut Vec<ProviderError>,
) {
    match attempt {
        Ok(NativeInterrupt::Delivered) => {}
        Ok(NativeInterrupt::NotActive) => *retry_pending = true,
        Err(error) => errors.push(error),
    }
}

/// Terminate processes of the interrupted turn's foreground commands. Codex aborts the
/// turn on `turn/interrupt` but leaves its exec processes running, so a cancelled
/// command would otherwise keep running and report output after the turn settled.
async fn stop_foreground_commands(
    session: &CodexBinding,
    thread_id: &str,
    native_turn: &str,
) -> Result<(), ProviderError> {
    let item_ids = session
        .normalization
        .lock()
        .await
        .foreground_command_items(thread_id, native_turn);
    if item_ids.is_empty() {
        return Ok(());
    }
    for row in terminal_inventory(session, thread_id).await? {
        if !row
            .get("itemId")
            .and_then(Value::as_str)
            .is_some_and(|id| item_ids.contains(id))
        {
            continue;
        }
        let process_id = row
            .get("processId")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::Protocol,
                )
            })?;
        session
            .rpc
            .lock()
            .await
            .request(
                "thread/backgroundTerminals/terminate",
                json!({"threadId":thread_id,"processId":process_id}),
                ProviderOperation::TurnInterrupt,
            )
            .await?;
    }
    Ok(())
}

/// Get effective Codex support after validating the minimum native protocol version.
fn codex_capabilities() -> forge_provider_api::capability::CapabilityManifest {
    let mut capabilities = forge_provider_api::capability::CapabilityManifest::from_descriptor(
        crate::registry::descriptor(
            &crate::registry::select("codex").expect("Codex is registered"),
        )
        .expect("registered Codex descriptor"),
    );
    // Isolated text requires an audited runtime and native-exportable credentials, which
    // only negotiation can prove.
    capabilities.set_support(
        forge_provider_api::capability::CapabilityId::IsolatedGeneration,
        forge_provider_api::capability::Support::Unsupported,
    );
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
    if version == Some("0.164.0") && exportable {
        capabilities.set_support(
            forge_provider_api::capability::CapabilityId::IsolatedGeneration,
            forge_provider_api::capability::Support::Supported,
        );
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
) -> Result<forge_provider_api::terminal::TerminalInventory, ProviderError> {
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
            let parsed = crate::terminal::parse_snapshot(&page, false).map_err(|error| {
                forge_provider_api::ProviderFailure::terminal_inventory(
                    ProviderOperation::TerminalList,
                    error,
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
        Ok(forge_provider_api::terminal::TerminalInventory::observed(
            terminal,
        ))
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
                            .activity_writer()
                            .send_wait(AdapterActivity::Thread {
                                thread_id: session.owner.clone(),
                                event: forge_provider_api::ThreadEvent::TerminalsUpdated(Ok(
                                    snapshot.clone(),
                                )),
                            })
                            .await
                            .is_err()
                        {
                            return;
                        }
                        previous = Some(snapshot);
                    }
                }
                Err(error) => {
                    if session.closing.load(Ordering::Acquire) {
                        return;
                    }
                    previous = None;
                    let failure = match error {
                        ProviderError::Failure(failure) => failure,
                        _ => ProviderFailure::new(
                            ProviderOperation::TerminalList,
                            ProviderFailureKind::Runtime,
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Observation,
                            forge_provider_api::FailureDisposition::Unchanged,
                        ),
                    };
                    if session
                        .sink
                        .activity_writer()
                        .send_wait(AdapterActivity::Thread {
                            thread_id: session.owner.clone(),
                            event: forge_provider_api::ThreadEvent::TerminalsUpdated(Err(failure)),
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
    operation_id: &forge_provider_api::OperationId,
    request: forge_provider_api::CompactionRequest,
) -> Result<forge_provider_api::Compaction, ProviderError> {
    session
        .normalization
        .lock()
        .await
        .bind(Some(request.scope.clone()));
    let result = observe_compaction(session, operation_id).await;
    // Release the operation scope so later provider-initiated compaction keeps its native
    // identity and automatic provenance instead of inheriting this host attempt.
    session
        .normalization
        .lock()
        .await
        .bind(None::<forge_provider_api::EventScope>);
    result
}

/// Dispatch native compaction and read its notifications until the thread reports a terminal state.
async fn observe_compaction(
    session: &Arc<CodexBinding>,
    operation_id: &forge_provider_api::OperationId,
) -> Result<forge_provider_api::Compaction, ProviderError> {
    let thread_id = session.history_id();
    session
        .rpc
        .lock()
        .await
        .request(
            "thread/compact/start",
            json!({"threadId":thread_id}),
            ProviderOperation::ThreadCompact,
        )
        .await?;
    loop {
        let mut message = session.rpc.lock().await.receive().await?;
        session.mcp_context.lock().await.normalize(&mut message);
        let mut output = super::normalization::CodexOutput::default();
        session
            .normalization
            .lock()
            .await
            .normalize(&message, &mut output, None, &session.workspace)
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::ThreadCompact,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let mut completed = None;
        for event in output.event {
            match &event {
                AdapterActivity::Thread {
                    event: forge_provider_api::ThreadEvent::CompactionUpdated(update),
                    ..
                } if update.thread_id == session.owner.clone().as_str()
                    && update.state != forge_provider_api::compaction::CompactionState::Running =>
                {
                    completed = Some(update.as_ref().clone());
                }
                // Codex reports a failed compaction turn as an error on that turn, which
                // the normalizer lifts to the thread because the operation owns no turn.
                AdapterActivity::Thread {
                    thread_id,
                    event: forge_provider_api::ThreadEvent::Error { error },
                } if thread_id == &session.owner.clone() => {
                    completed = Some(failed_compaction(session, operation_id, error));
                }
                _ => {}
            }
            publish_event(session, event).await?;
        }
        if let Some(compaction) = completed {
            return Ok(compaction);
        }
    }
}

/// Create the terminal failure record for a host compaction attempt from a native thread error.
fn failed_compaction(
    session: &CodexBinding,
    operation_id: &forge_provider_api::OperationId,
    error: &ProviderFailure,
) -> forge_provider_api::Compaction {
    forge_provider_api::Compaction {
        id: operation_id.as_str().to_owned(),
        thread_id: session.owner.clone().as_str().to_owned(),
        state: forge_provider_api::compaction::CompactionState::Failed,
        manual: true,
        started_at_ms: None,
        completed_at_ms: None,
        result: forge_provider_api::compaction::CompactionResult {
            error: Some(ProviderFailure::new(
                ProviderOperation::ThreadCompact,
                error.kind(),
            )),
            ..Default::default()
        },
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
    _request: &PromptRequest,
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
                ProviderOperation::TurnInterrupt,
            )
            .await
        {
            errors.push(error);
        }
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
        .unwrap_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Unknown));
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

/// Own the native reader, cancellation input, and retained collection proof for one turn.
struct NativeTurnObservation {
    session: Arc<CodexBinding>,
    request: PromptRequest,
    address: forge_provider_api::TurnAddress,
    native_address: NativeTurnAddress,
    submissions: Arc<Mutex<Vec<forge_provider_api::CoveredInput>>>,
    cancel: mpsc::UnboundedReceiver<()>,
    initial_messages: Vec<Value>,
    retained_cleanup: Arc<Mutex<Vec<ProviderError>>>,
}

/// Observe the exact native turn through its retained terminal and collection boundaries.
async fn observe_execution(
    observation: NativeTurnObservation,
) -> Result<forge_provider_api::TurnResult, ProviderError> {
    let NativeTurnObservation {
        session,
        request,
        address,
        native_address,
        submissions,
        mut cancel,
        initial_messages,
        retained_cleanup,
    } = observation;
    let mut cancelled = false;
    // Codex acknowledges `turn/start` before its thread state registers the active turn and
    // rejects an interrupt in that window with "no active turn to interrupt". The driver
    // retries such a rejected interrupt once the root `turn/started` notification arrives.
    let mut parent_started = false;
    let mut retry_interrupt_on_start = false;
    let mut native_cleanup_failed = false;
    let mut cleanup_errors = Vec::new();
    session
        .callback_requests
        .lock()
        .await
        .insert(request.scope.clone(), request.clone());
    let mut child_observations = session.child_observations.lock().await;
    let mut descendants = super::descendants::DescendantTracker::default();
    let mut parent_terminal = None::<Value>;
    let mut root_completion = None;
    let mut active_parent_turn = Some(native_address.turn_id.clone());
    let mut queued = std::mem::take(&mut *session.pending_messages.lock().await);
    queued.extend(initial_messages);
    let mut initial_messages = queued.into_iter();
    let result = loop {
        let native = if let Some(message) = initial_messages.next() {
            Ok(message)
        } else {
            tokio::select! {
                signal = cancel.recv(), if !cancelled => {
                    if signal.is_some() {
                        cancelled = true;
                        let mut errors = Vec::new();
                        record_interrupt_attempt(interrupt_native(&session, &native_address.thread_id, active_parent_turn.as_ref(), &descendants).await, &mut retry_interrupt_on_start, &mut errors);
                        if !errors.is_empty() { native_cleanup_failed = true; cleanup_errors.extend(errors); }
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
                    &native_address,
                    &descendants,
                    error,
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                )
                .await);
            }
        };
        session.mcp_context.lock().await.normalize(&mut message);
        let params = message.get("params").unwrap_or(&Value::Null);
        if message["method"]
            .as_str()
            .is_some_and(|method| method.eq_ignore_ascii_case("error"))
        {
            let result = async {
                let events = session
                    .normalization
                    .lock()
                    .await
                    .normalize_retained(&message, &session.workspace)
                    .await
                    .map_err(|_| {
                        failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
                    })?
                    .ok_or_else(|| {
                        failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
                    })?;
                for event in events {
                    publish_event(&session, event).await?;
                }
                Ok::<(), ProviderError>(())
            }
            .await;
            if let Err(error) = result {
                return Err(lose_observation(
                    &session,
                    &request,
                    &native_address,
                    &descendants,
                    error,
                    forge_provider_api::ObservationFailure::Protocol,
                )
                .await);
            }
            continue;
        }
        descendants.observe(&message, &native_address.thread_id);
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
            if let Some(thread) = request_thread
                && thread != native_address.thread_id
                && !session.normalization.lock().await.owns_child_thread(thread)
            {
                return Err(lose_observation(
                    &session,
                    &request,
                    &native_address,
                    &descendants,
                    failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol),
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
                        record_interrupt_attempt(interrupt_native(&session, &native_address.thread_id, active_parent_turn.as_ref(), &descendants).await, &mut retry_interrupt_on_start, &mut errors);
                        if !errors.is_empty() { native_cleanup_failed = true; cleanup_errors.extend(errors); }
                    }
                    Ok(())
                }
                handled = dispatch_native_request(&session, &request, &message, &mut child_observations) => handled,
            };
            if let Err(error) = handled {
                return Err(lose_observation(
                    &session,
                    &request,
                    &native_address,
                    &descendants,
                    error,
                    forge_provider_api::lifecycle::ObservationFailure::Protocol,
                )
                .await);
            }
            continue;
        }
        let terminal = if params.get("threadId").and_then(Value::as_str)
            == Some(native_address.thread_id.as_str())
            && message.get("method").and_then(Value::as_str) == Some("turn/completed")
            && params
                .pointer("/turn/id")
                .or_else(|| params.get("turnId"))
                .and_then(Value::as_str)
                .map(NativeTurnId::new)
                .transpose()?
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
        if thread_id != native_address.thread_id {
            if !session
                .normalization
                .lock()
                .await
                .owns_child_thread(thread_id)
            {
                session.normalization.lock().await.defer_child(&message);
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
                    let (agents, events): (Vec<_>, Vec<_>) = events
                        .into_iter()
                        .partition(|event| matches!(event, AdapterActivity::Agent { .. }));
                    for event in agents {
                        publish_event(&session, event).await?;
                    }
                    begin_child_turns(&session, &mut child_observations, &session.child_runners)
                        .await?;
                    for event in events {
                        if let Err(error) = publish_event(&session, event).await {
                            return Err(lose_observation(
                                &session,
                                &request,
                                &native_address,
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
                        &native_address,
                        &descendants,
                        failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol),
                        forge_provider_api::lifecycle::ObservationFailure::Protocol,
                    )
                    .await);
                }
            }
            finish_child_turns(&session, &mut child_observations).await?;
            if active_parent_turn.is_none()
                && let Some(terminal) = parent_terminal.take()
            {
                break terminal;
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
                    if turn_id != native_address.turn_id.as_str() {
                        return Err(lose_observation(
                            &session,
                            &request,
                            &native_address,
                            &descendants,
                            failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol),
                            forge_provider_api::lifecycle::ObservationFailure::UnexpectedExecution,
                        )
                        .await);
                    }
                    active_parent_turn = Some(NativeTurnId::new(turn_id)?);
                    if let Some(execution) = session.active.lock().await.as_mut() {
                        execution.native_turn_id = turn_id.to_owned();
                        execution
                            .turn_updates
                            .send_replace(Some(turn_id.to_owned()));
                    }
                    parent_started = true;
                }
            }
            Some("turn/completed") => {
                let completed_id = params
                    .pointer("/turn/id")
                    .or_else(|| params.get("turnId"))
                    .and_then(Value::as_str)
                    .map(NativeTurnId::new)
                    .transpose()?;
                if active_parent_turn.is_some() && active_parent_turn == completed_id {
                    active_parent_turn = None;
                }
            }
            _ => {}
        }
        // Codex announces `turn/started` slightly before the turn accepts interrupts, so a
        // rejected interrupt is retried on each later root notification until it is delivered
        // or the turn completes on its own.
        if parent_started
            && active_parent_turn.is_some()
            && std::mem::take(&mut retry_interrupt_on_start)
        {
            let mut errors = Vec::new();
            record_interrupt_attempt(
                interrupt_native(
                    &session,
                    &native_address.thread_id,
                    active_parent_turn.as_ref(),
                    &descendants,
                )
                .await,
                &mut retry_interrupt_on_start,
                &mut errors,
            );
            if !errors.is_empty() {
                native_cleanup_failed = true;
                cleanup_errors.extend(errors);
            }
        }
        let mut output = super::normalization::CodexOutput::default();
        let mut normalization = session.normalization.lock().await;
        if let Err(_error) = normalization
            .normalize(&message, &mut output, None, &session.workspace)
            .await
        {
            drop(normalization);
            return Err(lose_observation(
                &session,
                &request,
                &native_address,
                &descendants,
                failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol),
                forge_provider_api::lifecycle::ObservationFailure::Protocol,
            )
            .await);
        }
        if let Some(observed) = normalization.terminal_evidence(&address) {
            root_completion = Some(observed);
        }
        drop(normalization);
        let (agents, events): (Vec<_>, Vec<_>) = output
            .event
            .into_iter()
            .partition(|event| matches!(event, AdapterActivity::Agent { .. }));
        for event in agents {
            publish_event(&session, event).await?;
        }
        begin_child_turns(&session, &mut child_observations, &session.child_runners).await?;
        for event in events {
            if let Err(error) = publish_event(&session, event).await {
                return Err(lose_observation(
                    &session,
                    &request,
                    &native_address,
                    &descendants,
                    error,
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                )
                .await);
            }
        }
        finish_child_turns(&session, &mut child_observations).await?;
        if let Some(terminal) = terminal {
            parent_terminal.get_or_insert(terminal);
        }
        if active_parent_turn.is_none()
            && let Some(terminal) = parent_terminal.take()
        {
            break terminal;
        }
    };

    session
        .pending_messages
        .lock()
        .await
        .extend(initial_messages);
    drop(child_observations);
    if let Some(execution) = session.active.lock().await.as_mut() {
        execution.turn_updates.send_replace(None);
    }

    let covered_inputs = submissions.lock().await.clone();
    let (native_outcome, completed_at_ms) = match root_completion {
        Some(outcome) => outcome,
        None => {
            return Err(lose_observation(
                &session,
                &request,
                &native_address,
                &descendants,
                failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol),
                forge_provider_api::lifecycle::ObservationFailure::Protocol,
            )
            .await);
        }
    };
    let native_outcome = match native_outcome {
        forge_provider_api::turn::TurnOutcome::Failed { error, retry: _ } => {
            let category = result
                .get("error")
                .and_then(crate::codex::retry::transient_category);
            let replay_sensitive = session.normalization.lock().await.replay_sensitive;
            let permitted = category.filter(|_| !replay_sensitive);
            let retry = match permitted {
                Some(_) => forge_provider_api::turn::RetryDecision::Allowed { retry_after: None },
                None => forge_provider_api::turn::RetryDecision::NotAllowed,
            };
            forge_provider_api::turn::TurnOutcome::Failed { error, retry }
        }
        outcome @ (forge_provider_api::turn::TurnOutcome::Completed { .. }
        | forge_provider_api::turn::TurnOutcome::Interrupted) => outcome,
    };
    if cancelled
        && matches!(
            native_outcome,
            forge_provider_api::turn::TurnOutcome::Interrupted
        )
        && covered_inputs.as_slice()
            == [forge_provider_api::CoveredInput::new(
                request.submission_id.clone(),
                request.attempt_id.clone(),
            )]
        && !native_cleanup_failed
    {
        *session.rollback.lock().await = Some(RollbackAuthorization {
            thread_id: native_address.thread_id.clone(),
            scope: request.scope.clone(),
            turn_id: native_address.turn_id.clone(),
            submission_id: request.submission_id.clone(),
            attempt_id: request.attempt_id.clone(),
        });
    }
    if native_cleanup_failed {
        retained_cleanup.lock().await.extend(cleanup_errors);
    }
    forge_provider_api::TurnResult::new(
        native_outcome,
        completed_at_ms,
        None,
        covered_inputs,
        session.normalization.lock().await.final_usage(&address),
        Some(
            forge_provider_api::CheckpointId::new(native_address.turn_id.as_str()).map_err(
                |_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol),
            )?,
        ),
    )
    .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))
}

async fn lose_observation(
    session: &CodexBinding,
    _request: &PromptRequest,
    native_address: &NativeTurnAddress,
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
        native_address.thread_id.clone(),
        Some(native_address.turn_id.as_str().to_owned()),
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
                    ProviderOperation::TurnInterrupt,
                )
                .await
        {
            errors.push(error);
        }
    }
    if let Err(error) = stop_background_commands(session).await {
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

/// Observe delayed accounting, thread errors, and identified background work while the binding is idle.
async fn start_background_reader(
    session: &Arc<CodexBinding>,
) -> Result<Option<tokio::sync::oneshot::Sender<()>>, ProviderError> {
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
        let observed = async {
        tokio::select! {
            result = started => if result.is_err() { return Ok(()); },
            signal = cancelled.recv() => if signal.is_some() { return Ok(()); },
        }
        {
            let mut children = retained.child_observations.lock().await;
            begin_child_turns(&retained, &mut children, &retained.child_runners).await?;
            finish_child_turns(&retained, &mut children).await?;
        }
        loop {
            let pending = retained.pending_messages.lock().await.pop_front();
            let received = match pending {
                Some(message) => Ok(message),
                None => tokio::select! {
                    signal = cancelled.recv() => if signal.is_some() { break; } else { continue; },
                    received = async { retained.rpc.lock().await.receive().await } => received,
                },
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
                let original = retained
                    .callback_requests
                    .lock()
                    .await
                    .values()
                    .next()
                    .cloned();
                if let Some(original) = original {
                    let mut children = retained.child_observations.lock().await;
                    dispatch_native_request(&retained, &original, &message, &mut children).await?;
                } else {
                    retained
                        .rpc
                        .lock()
                        .await
                        .reject_request(message.get("id").unwrap_or(&Value::Null))
                        .await?;
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
                    let (agents, events): (Vec<_>, Vec<_>) = events
                        .into_iter()
                        .partition(|event| matches!(event, AdapterActivity::Agent { .. }));
                    for event in agents {
                        publish_event(&retained, event).await?;
                    }
                    let mut children = retained.child_observations.lock().await;
                    begin_child_turns(&retained, &mut children, &retained.child_runners).await?;
                    for event in events {
                        if let Err(error) = publish_event(&retained, event).await {
                            return Err(invalidate_background_observer(
                                &retained,
                                error,
                                forge_provider_api::ObservationFailure::LifecycleUnavailable,
                            )
                            .await);
                        }
                    }
                    finish_child_turns(&retained, &mut children).await?;
                }
                Ok(None) => {
                    if message.get("method").and_then(Value::as_str) == Some("turn/started") {
                        return Err(invalidate_background_observer(
                            &retained,
                            failure(
                                ProviderOperation::TurnStart,
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
                        failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol),
                        forge_provider_api::ObservationFailure::Protocol,
                    )
                    .await);
                }
            }
        }
        Ok(())
        }.await;
        if observed.is_err() {
            retained.child_observations.lock().await.clear();
        }
        observed
    });
    *slot = Some(BackgroundReader {
        cancel,
        task: crate::retained_task::RetainedTask::new(ProviderOperation::TurnStart, task),
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
        let (thread, item) = session
            .normalization
            .lock()
            .await
            .native_tool(address)
            .ok_or_else(|| {
                failure(
                    ProviderOperation::ThreadClose,
                    ProviderFailureKind::WrongInvocation,
                )
            })?;
        owned.entry(thread).or_default().insert(item);
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
                        ProviderOperation::ThreadClose,
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
                    ProviderOperation::ThreadClose,
                )
                .await?;
            if result.get("terminated").and_then(Value::as_bool).is_none() {
                return Err(failure(
                    ProviderOperation::ThreadClose,
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
                ProviderOperation::ThreadClose,
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
            publish_event(session, event).await?;
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
                ProviderOperation::ThreadClose,
            )
            .await?;
        let data = page.get("data").and_then(Value::as_array).ok_or_else(|| {
            failure(
                ProviderOperation::ThreadClose,
                ProviderFailureKind::Protocol,
            )
        })?;
        rows.extend(data.iter().cloned());
        let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
            break;
        };
        if !seen.insert(next.to_owned()) {
            return Err(failure(
                ProviderOperation::ThreadClose,
                ProviderFailureKind::Protocol,
            ));
        }
        cursor = Some(next.to_owned());
    }
    Ok(rows)
}

/// Resolve a live root or retained child callback to its original request authority.
async fn callback_request(
    session: &CodexBinding,
    request: &PromptRequest,
    params: &Value,
) -> Option<(PromptRequest, super::normalization::NativeCallbackOwner)> {
    let normalization = session.normalization.lock().await;
    if let Ok(owner) = normalization.callback_owner(params, &request.scope) {
        return Some((request.clone(), owner));
    }
    for original in session.callback_requests.lock().await.values() {
        if let Ok(owner @ super::normalization::NativeCallbackOwner::ObservedChild(_)) =
            normalization.callback_owner(params, &original.scope)
        {
            return Some((original.clone(), owner));
        }
    }
    None
}

/// Transfer independently owned child callbacks before allowing root native-end observation to proceed.
async fn dispatch_native_request(
    session: &Arc<CodexBinding>,
    request: &PromptRequest,
    message: &Value,
    children: &mut HashMap<forge_provider_api::TurnId, ChildObservation>,
) -> Result<(), ProviderError> {
    if let Some((original, super::normalization::NativeCallbackOwner::ObservedChild(address))) =
        callback_request(session, request, &message["params"]).await
    {
        let Some(child) = children.get(&address.turn_id) else {
            return session
                .rpc
                .lock()
                .await
                .reject_request(&message["id"])
                .await;
        };
        let binding = session.clone();
        let message = message.clone();
        let task = tokio::spawn(async move {
            let result = handle_native_request(&binding, &original, &message).await;
            if result.is_err() {
                let _ = binding
                    .rpc
                    .lock()
                    .await
                    .reject_request(&message["id"])
                    .await;
            }
            result
        });
        child
            .callbacks
            .lock()
            .await
            .push(Arc::new(crate::retained_task::RetainedTask::new(
                ProviderOperation::TurnStart,
                task,
            )));
        return Ok(());
    }
    handle_native_request(session, request, message).await
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
        let id = message
            .get("id")
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        let auth = session.isolation_auth.as_ref().ok_or_else(|| {
            failure(
                ProviderOperation::TurnStart,
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
    let id = message
        .get("id")
        .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
    let method = message
        .get("method")
        .and_then(Value::as_str)
        .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
    let params = message.get("params").unwrap_or(&Value::Null);
    let Some((request, owner)) = callback_request(session, request, params).await else {
        return session.rpc.lock().await.reject_request(id).await;
    };
    let response = match method {
        "item/tool/call" => {
            let call = super::decode::dynamic_tool_call(params).map_err(|_| {
                failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
            })?;
            let name = call.name.as_str();
            if !request.tools.iter().any(|tool| tool.name == name) {
                // Codex registers its catalog once per thread, so the model can see tools this
                // invocation rejects; the host explains which toolset or task enables them.
                super::decode::dynamic_tool_response(
                    request.provider_tools.unavailable_reason(name),
                    false,
                )
            } else {
                match &owner {
                    super::normalization::NativeCallbackOwner::Root => {
                        request.permissions.before_tool(&request.scope).await?
                    }
                    super::normalization::NativeCallbackOwner::ObservedChild(child) => {
                        request
                            .permissions
                            .before_observed_child(&request.scope, child)
                            .await?
                    }
                }
                let permission = PermissionRequest {
                    id: uuid::Uuid::new_v4().to_string(),
                    provider: crate::registry::select("codex").expect("Codex is registered"),
                    reason: None,
                    target_list: vec![PermissionTarget::HostTool {
                        name: name.to_owned(),
                    }],
                };
                let authorization = match &owner {
                    super::normalization::NativeCallbackOwner::Root => {
                        request
                            .permissions
                            .authorize(&request.scope, permission)
                            .await
                    }
                    super::normalization::NativeCallbackOwner::ObservedChild(child) => {
                        request
                            .permissions
                            .authorize_observed_child(&request.scope, child, permission)
                            .await
                    }
                };
                match authorization {
                    PermissionResponse::Allow => {
                        if session.is_fenced() {
                            return Err(ProviderError::Cancelled);
                        }
                        session.normalization.lock().await.replay_sensitive = true;
                        let result = match &owner {
                            super::normalization::NativeCallbackOwner::Root => {
                                request.provider_tools.call(&request.scope, call).await
                            }
                            super::normalization::NativeCallbackOwner::ObservedChild(child) => {
                                request
                                    .provider_tools
                                    .call_observed_child(&request.scope, child, call)
                                    .await
                            }
                        };
                        match result {
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
                failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
            })?;
            let decision = match &owner {
                super::normalization::NativeCallbackOwner::Root => {
                    request
                        .permissions
                        .authorize(&request.scope, prepared.request.clone())
                        .await
                }
                super::normalization::NativeCallbackOwner::ObservedChild(child) => {
                    request
                        .permissions
                        .authorize_observed_child(&request.scope, child, prepared.request.clone())
                        .await
                }
            };
            if session.is_fenced() {
                return Err(ProviderError::Cancelled);
            }
            prepared.validate().map_err(|_| {
                ProviderError::from(
                    ProviderFailure::new(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::ResourceConflict,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Observation,
                        forge_provider_api::FailureDisposition::Uncertain,
                    ),
                )
            })?;
            super::approval::codex_response(message, &decision)
        }
        _ => {
            return Err(failure(
                ProviderOperation::TurnStart,
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
    }
}

/// Render the user's own words of one turn input, or nothing for a Harness-started turn.
fn user_text(input: &forge_provider_api::forge::TurnInput) -> String {
    input.user_input().map(input_text).unwrap_or_default()
}

/// Join non-empty prompt parts with one line break, keeping a block directly before its text.
fn joined(parts: impl IntoIterator<Item = String>) -> String {
    parts
        .into_iter()
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
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

/// Deliver restricted activity through the writer retained for the exact original object owner.
async fn publish_event(
    session: &CodexBinding,
    event: AdapterActivity,
) -> Result<(), ProviderError> {
    let writer = session.sink.activity_writer();
    let address = session.normalization.lock().await.activity_address(&event);
    let writer = match address {
        Some(address) => writer.register_observed_turn(address)?,
        None => writer,
    };
    writer.send_wait(event).await.map_err(ProviderError::from)
}

/// Preserve positively observed start identity and model metadata independently of receipt persistence.
struct NativeAdmission {
    turn_id: String,
    initial_messages: Vec<Value>,
    model: Option<forge_provider_api::ResolvedModel>,
    started_at_ms: i64,
}

/// Observe exact native admission without making cancellation depend on its RPC acknowledgement.
async fn await_admission(
    session: &CodexBinding,
    request: &PromptRequest,
    request_id: u64,
) -> Result<NativeAdmission, ProviderError> {
    let rpc = session.rpc.lock().await.shared();
    let response = rpc.wait_response(request_id, ProviderOperation::TurnStart);
    tokio::pin!(response);
    let thread_id = session.history_id();
    let mut initial_messages = Vec::new();
    loop {
        tokio::select! {
            result = &mut response => {
                let result = result?;
                let turn_id = result.get("turnId").or_else(|| result.pointer("/turn/id")).and_then(Value::as_str).filter(|id| !id.is_empty()).ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
                let turn_id = turn_id.to_owned();
                let model = match result.pointer("/turn/model").or_else(|| result.get("model")) {
                    Some(Value::String(model)) => Some(forge_provider_api::ResolvedModel { model_id: forge_provider_api::ModelId::new(model).map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?, observed_at_ms: now_ms() }),
                    None | Some(Value::Null) => None,
                    Some(_) => return Err(failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)),
                };
                return Ok(NativeAdmission { turn_id, initial_messages, model, started_at_ms: now_ms() });
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
                    return Ok(NativeAdmission { turn_id, initial_messages, model: None, started_at_ms: now_ms() });
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

/// Recover the Forge submission that a native `userMessage.clientId` echoes.
///
/// Returns `None` for client IDs that `native_input_id` did not produce, such as input another
/// client added to the shared thread.
pub(super) fn submission_from_client_id(
    client_id: &str,
) -> Option<forge_provider_api::SubmissionId> {
    let [scheme, submission, _attempt]: [String; 3] = serde_json::from_str(client_id).ok()?;
    (scheme == "forge-input-v2")
        .then(|| forge_provider_api::SubmissionId::try_from(submission).ok())
        .flatten()
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
        (ProviderOperation::ThreadClose | ProviderOperation::Shutdown, _) => {
            (Phase::Close, Disposition::Invalidated)
        }
        (ProviderOperation::TurnInterrupt, _) => (Phase::Interruption, Disposition::Uncertain),
        (ProviderOperation::ThreadConfigure, _) => (Phase::Preparation, Disposition::Invalidated),
        (ProviderOperation::ThreadCompact, _) => (Phase::Observation, Disposition::Invalidated),
        (
            ProviderOperation::InputRetract
            | ProviderOperation::ThreadFork
            | ProviderOperation::ThreadRewind,
            _,
        ) => (Phase::Observation, Disposition::Uncertain),
        (
            ProviderOperation::ThreadOpen
            | ProviderOperation::Inspect
            | ProviderOperation::ModelList
            | ProviderOperation::SkillList
            | ProviderOperation::McpList
            | ProviderOperation::TerminalList
            | ProviderOperation::TerminalTerminate
            | ProviderOperation::TurnStart
            | ProviderOperation::TurnSteer
            | ProviderOperation::InputReconcile,
            _,
        ) => (Phase::Observation, Disposition::Uncertain),
    };
    ProviderFailure::new(operation, kind)
        .at_phase(phase, disposition)
        .into()
}

/// Get a wall-clock timestamp for provider collection and retained-worker fixtures.
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
    use forge_provider_api::{ProviderEvent, turn::TurnOutcome};

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
        let owner = request().scope.thread_id;
        let binding = SessionBinding {
            thread_id: owner.clone(),
            id: BindingId::new("compaction-binding").unwrap(),
            provider_session_id: Some("compaction-thread".try_into().unwrap()),
        };
        let (sink, stream) = events::channel(
            binding.id.clone(),
            forge_provider_api::EventScope::Binding {
                thread_id: owner.clone(),
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
            native_confirmation: Mutex::new(None),
            runtime: Mutex::new(Arc::new(CodexRuntime::new(vec![
                "unused-binding-fixture-command".into(),
            ]))),
            rpc: RpcHandle::new(NativeRpc::new(connection)),
            sink,
            stream: Mutex::new(None),
            child_observations: Mutex::new(HashMap::new()),
            child_runners: Arc::default(),
            callback_requests: Mutex::new(HashMap::new()),
            pending_messages: Mutex::new(Default::default()),
            normalization: Mutex::new({
                let mut normalization = CodexNormalization::default();
                normalization
                    .admit_thread("compaction-thread", &owner)
                    .unwrap();
                normalization
            }),
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
            turn_context: Mutex::new(Default::default()),
            admission: Mutex::new(()),
            observations: Arc::new(tokio::sync::RwLock::new(false)),
            fenced: AtomicBool::new(false),
            invalidated: AtomicBool::new(false),
            closing: AtomicBool::new(false),
            provider_closed: provider.closed.clone(),
            isolated: false,
            background: false,
            isolation_auth: None,
            configuration: Mutex::new(forge_provider_api::SessionConfiguration {
                model: ModelSelection::default(),
                access: AccessPolicy::default(),
                approval: ApprovalMode::Ask,
                resources: Default::default(),
            }),
            capabilities: codex_capabilities(),
            settlements: Mutex::new(HashMap::new()),
            native_results: Mutex::new(HashMap::new()),
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

    /// Acknowledge the per-turn context injection that precedes a binding's first native turn.
    async fn acknowledge_context_injection(
        socket: &mut tokio_tungstenite::WebSocketStream<tokio::net::TcpStream>,
    ) {
        use futures_util::{SinkExt, StreamExt};
        let inject: Value =
            serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
        assert_eq!(inject["method"], "thread/inject_items");
        socket
            .send(tokio_tungstenite::tungstenite::Message::Text(
                json!({"id":inject["id"],"result":{}}).to_string().into(),
            ))
            .await
            .unwrap();
    }

    /// Create canonical native evidence for retained-runner lifecycle fixtures.
    fn collected_result(
        request: &PromptRequest,
        outcome: forge_provider_api::turn::TurnOutcome,
    ) -> forge_provider_api::TurnResult {
        forge_provider_api::TurnResult::new(
            outcome,
            now_ms(),
            None,
            vec![forge_provider_api::CoveredInput::new(
                request.submission_id.clone(),
                request.attempt_id.clone(),
            )],
            Vec::new(),
            None,
        )
        .unwrap()
    }

    struct FixtureDriver {
        worker: Arc<crate::retained_task::RetainedTask<forge_provider_api::TurnResult>>,
    }

    #[async_trait]
    impl crate::turn_runner::NativeTurnDriver for FixtureDriver {
        /// Exercise the actual runner's native-start and canonical result contract.
        async fn run(
            &self,
            start: forge_provider_api::NativeTurnStart,
        ) -> Result<forge_provider_api::TurnResult, ProviderError> {
            let result = self.worker.join().await?;
            start.observed(result.completed_at_ms()).await?;
            Ok(result)
        }
        /// Leave explicit native cancellation channels under each fixture's asserted ownership.
        async fn interrupt(&self) -> Result<(), ProviderError> {
            Ok(())
        }
        /// Collect fixtures that own no separate transport or callback resource.
        async fn collect(&self) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    /// Route injected native workers through the production retained runner and its restricted publisher.
    fn fixture_worker(
        session: &Arc<CodexBinding>,
        scope: &forge_provider_api::InvocationScope,
        worker: Arc<crate::retained_task::RetainedTask<forge_provider_api::TurnResult>>,
    ) -> Arc<crate::turn_runner::RetainedTurn> {
        Arc::new(
            crate::turn_runner::RetainedTurn::spawn(
                Arc::new(FixtureDriver { worker }),
                session.sink.clone(),
                forge_provider_api::TurnAddress {
                    scope: scope.clone(),
                    thread_id: scope.thread_id.clone(),
                    turn_id: scope.turn_id().unwrap(),
                },
            )
            .unwrap(),
        )
    }

    /// Concurrent resumes under different owners cannot acquire the same retained durable history.
    #[tokio::test]
    async fn duplicate_resume_is_excluded_until_invalidated_history_owner_is_collected() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        session.fenced.store(true, Ordering::Release);
        let request = |owner| SessionRequest {
            thread_id: (forge_provider_api::testing::root_thread(owner)).clone(),
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
            provider.thread_open(context(), request("first-owner")),
            provider.thread_open(context(), request("second-owner"))
        );
        for opened in [first, second] {
            let error = match opened {
                Ok(_) => panic!("duplicate writable history binding admitted"),
                Err(error) => error,
            };
            assert!(
                matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::ThreadOpen && failure.kind() == ProviderFailureKind::ResourceConflict)
            );
        }
        assert_eq!(provider.bindings.lock().await.len(), 1);
        assert!(provider.pending.lock().await.is_empty());
        assert!(provider.runtime.process_id().await.is_none());
        session.rpc.lock().await.close().await.unwrap();
    }

    /// Cancellation registered before any turn/start acknowledgment or turn/started waits for the
    /// acknowledgment, then interrupts and collects that exact native turn.
    ///
    /// The wire models Codex, which acknowledges `turn/start` before registering the active turn
    /// and rejects an interrupt in that window as an invalid request. The pre-queued cancellation
    /// reaches the driver before any notification, so the first interrupt always meets that
    /// rejection, and only a retry after `turn/started` stops the turn.
    #[tokio::test]
    async fn cancellation_before_start_acknowledgment_interrupts_after_admission() {
        use futures_util::{SinkExt, StreamExt};
        async {
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
                acknowledge_context_injection(&mut socket).await;
                let start: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(start["method"], "turn/start");
                released_ack.await.unwrap();
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":start["id"],"result":{"turn":{"id":"late-ack-turn"}}}).to_string().into())).await.unwrap();
                let early: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(early["method"], "turn/interrupt");
                assert_eq!(early["params"]["turnId"], "late-ack-turn");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":early["id"],"error":{"code":-32600,"message":"no active turn to interrupt"}}).to_string().into())).await.unwrap();
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"turn/started","params":{"threadId":"compaction-thread","turn":{"id":"late-ack-turn","status":"inProgress"}}}).to_string().into())).await.unwrap();
                // Codex announces the turn slightly before it accepts interrupts.
                let started: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(started["method"], "turn/interrupt");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":started["id"],"error":{"code":-32600,"message":"no active turn to interrupt"}}).to_string().into())).await.unwrap();
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"turn/diff/updated","params":{"threadId":"compaction-thread","turnId":"late-ack-turn","diff":""}}).to_string().into())).await.unwrap();
                let interrupt: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(interrupt["method"], "turn/interrupt");
                assert_eq!(interrupt["params"]["turnId"], "late-ack-turn");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":interrupt["id"],"result":{}}).to_string().into())).await.unwrap();
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"turn/completed","params":{"threadId":"compaction-thread","turn":{"id":"late-ack-turn","status":"interrupted"}}}).to_string().into())).await.unwrap();
                while let Some(message) = socket.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            let submitting_provider = provider.clone();
            let submitting_binding = binding.clone();
            let submitting = tokio::spawn(async move { submitting_provider.turn_start(context(), &submitting_binding, input).await });
            observed.wait().await.unwrap();
            let interrupting_provider = provider.clone();
            let interrupting_binding = binding.clone();
            let interrupting_scope = scope.clone();
            let interrupting = tokio::spawn(async move { interrupting_provider.turn_interrupt(context(), &interrupting_binding, forge_provider_api::CancelRequest { scope: interrupting_scope }).await });
            // Release the acknowledgment only after the cancellation is recorded on the admission.
            loop {
                let requested = session.admitting.lock().await.as_ref().is_some_and(|admitting| admitting.scope == scope && admitting.cancel.load(Ordering::Acquire));
                if requested { break; }
                tokio::task::yield_now().await;
            }
            release_ack.send(()).unwrap();
            let settled = interrupting.await.unwrap().unwrap();
            assert_eq!(settled.thread_id, session.owner.clone());
            assert!(session.settlements.lock().await.get(&scope).is_some_and(|settlement| matches!(settlement, Ok(settlement) if matches!(settlement.completed.result().outcome(), TurnOutcome::Interrupted))));
            assert!(session.active.lock().await.is_none());
            assert!(!session.is_fenced());
            submitting.await.unwrap().unwrap();
            session.rpc.lock().await.close().await.unwrap();
            wire.await.unwrap();
        }.await;
    }

    /// Normal native completion collects automatically; late cancellation cannot touch a newer invocation.
    #[tokio::test]
    async fn completed_invocation_settles_once_and_late_cancel_preserves_successor() {
        use futures_util::SinkExt;
        async {
            let (provider, session, mut events, mut socket) = compaction_fixture().await;
            let request = request();
            let scope = request.scope.clone();
            session.normalization.lock().await.bind(Some(scope.clone()));
            let address = forge_provider_api::TurnAddress { scope: scope.clone(), thread_id: request.thread_id.clone(), turn_id: request.turn_id.clone() };
            session.normalization.lock().await.admit_turn("compaction-thread", "completed-turn", &scope).unwrap();
            let expected_address = address.clone();
            let (cancel, receiver) = mpsc::unbounded_channel();
            let submissions = Arc::new(Mutex::new(vec![forge_provider_api::CoveredInput::new(request.submission_id.clone(), request.attempt_id.clone())]));
            let worker_session = session.clone();
            let worker_request = request.clone();
            let worker_submissions = submissions.clone();
            let worker = Arc::new(crate::retained_task::RetainedTask::new(ProviderOperation::TurnStart, tokio::spawn(async move {
                observe_execution(NativeTurnObservation {
session: worker_session,
request: worker_request,
address: address.clone(),
native_address: NativeTurnAddress { thread_id: "compaction-thread".into(), turn_id: NativeTurnId::new("completed-turn").unwrap() },
submissions: worker_submissions,
cancel: receiver,
initial_messages: Vec::new(),
retained_cleanup: Arc::new(Mutex::new(Vec::new())),

}).await
            })));
        let worker = fixture_worker(&session, &request.scope, worker);
            *session.active.lock().await = Some(ActiveExecution { scope: scope.clone(), native_turn_id: "completed-turn".into(), turn_updates: watch::channel(Some("completed-turn".into())).0, submissions, request: request.clone(), cancel, task: Some(worker.clone()) });
            let retained_worker = worker.clone();
            let collected_session = session.clone();
            let collected_scope = scope.clone();
            let collector = tokio::spawn(async move { collect_invocation(collected_session, collected_scope, worker).await });
            socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"method":"turn/completed","params":{"threadId":"compaction-thread","turn":{"id":"completed-turn","status":"completed"}}}).to_string().into())).await.unwrap();
            collector.await.unwrap().unwrap();
            assert!(session.active.lock().await.is_none());
            let native = retained_worker.join().await.unwrap().result().clone();
            assert_eq!(native.covered_inputs(), &[forge_provider_api::CoveredInput::new(request.submission_id.clone(), request.attempt_id.clone())]);
            assert_eq!(native.checkpoint_id().unwrap().as_str(), "completed-turn");
            let retained = session.settlements.lock().await.get(&scope).cloned().unwrap().unwrap();
            assert!(matches!(retained.completed.result().outcome(), TurnOutcome::Completed { .. }));
            assert!(retained.watermark.get() > 0);
            let completed = loop {
                let event = events.recv().await.unwrap().unwrap();
                if let ProviderEvent::Turn { thread_id, turn_id, event: forge_provider_api::TurnEvent::Completed(completed) } = event.event {
                    assert_eq!(thread_id, expected_address.thread_id);
                    assert_eq!(turn_id, expected_address.turn_id);
                    break completed;
                }
            };
            assert!(matches!(completed.result().outcome(), TurnOutcome::Completed { .. }));
            assert_eq!(completed.result().checkpoint_id().unwrap().as_str(), "completed-turn");

            let mut successor = request;
            successor.scope = forge_provider_api::InvocationScope::new(session.owner.clone(), "successor"); successor.turn_id = successor.scope.turn_id().unwrap();
            let successor_scope = successor.scope.clone();
            let (cancel, mut cancelled) = mpsc::unbounded_channel();
            let (release, released) = tokio::sync::oneshot::channel();
            let completed = collected_result(&successor, retained.completed.result().outcome());
            let worker = Arc::new(crate::retained_task::RetainedTask::new(ProviderOperation::TurnStart, tokio::spawn(async move { released.await.unwrap(); Ok(completed) })));
        let worker = fixture_worker(&session, &successor_scope, worker);
            *session.active.lock().await = Some(ActiveExecution { scope: successor_scope.clone(), native_turn_id: "successor-turn".into(), turn_updates: watch::channel(Some("successor-turn".into())).0, submissions: Arc::new(Mutex::new(Vec::new())), request: successor, cancel, task: Some(worker.clone()) });
            let settled = provider.turn_interrupt(context(), &session.binding.id, forge_provider_api::CancelRequest { scope: scope.clone() }).await.unwrap();
            assert_eq!(settled.watermark, retained.watermark);
            assert_eq!(settled.thread_id, session.owner.clone());
            assert!(cancelled.try_recv().is_err());
            assert_eq!(session.active.lock().await.as_ref().unwrap().scope, successor_scope);
            release.send(()).unwrap(); worker.join().await.unwrap();
            session.active.lock().await.take();
            session.rpc.lock().await.close().await.unwrap();
        }.await;
    }

    /// Canonical failure and legacy publication share the same retained retry permission.
    #[tokio::test]
    async fn canonical_native_failure_preserves_coverage_and_retry_decision() {
        for (code, sensitive, allowed, kind) in [
            (
                "serverOverloaded",
                false,
                true,
                ProviderFailureKind::Transport,
            ),
            (
                "serverOverloaded",
                true,
                false,
                ProviderFailureKind::Transport,
            ),
            (
                "unauthorized",
                false,
                false,
                ProviderFailureKind::Authentication,
            ),
        ] {
            let (_provider, session, mut events, _socket) = compaction_fixture().await;
            let request = request();
            {
                let mut normalization = session.normalization.lock().await;
                normalization.bind(Some(request.scope.clone()));
                normalization.replay_sensitive = sensitive;
            }
            let original = forge_provider_api::CoveredInput::new(
                request.submission_id.clone(),
                request.attempt_id.clone(),
            );
            let steering = forge_provider_api::CoveredInput::new(
                "steering".try_into().unwrap(),
                forge_provider_api::DeliveryAttemptId::new("steering-attempt").unwrap(),
            );
            let (_cancel, receiver) = mpsc::unbounded_channel();
            let address = forge_provider_api::TurnAddress {
                scope: request.scope.clone(),
                thread_id: request.thread_id.clone(),
                turn_id: request.turn_id.clone(),
            };
            session
                .normalization
                .lock()
                .await
                .admit_turn("compaction-thread", "failed-turn", &request.scope)
                .unwrap();
            let collected = observe_execution(NativeTurnObservation {
session: session.clone(),
request: request.clone(),
address,
native_address: NativeTurnAddress { thread_id: "compaction-thread".into(), turn_id: NativeTurnId::new("failed-turn").unwrap() },
submissions: Arc::new(Mutex::new(vec![original.clone(), steering.clone(), original.clone()])),
cancel: receiver,
initial_messages: vec![json!({"method":"turn/completed", "params":{"threadId":"compaction-thread", "turn":{"id":"failed-turn", "status":"failed", "error":{"codexErrorInfo":code,"message":"private native error"}}}})],
retained_cleanup: Arc::new(Mutex::new(Vec::new())),

}).await.unwrap();
            assert_eq!(collected.covered_inputs(), &[original, steering]);
            assert!(
                matches!(collected.outcome(), forge_provider_api::turn::TurnOutcome::Failed { error, retry }
                if error.kind() == kind && matches!(retry, forge_provider_api::turn::RetryDecision::Allowed { .. }) == allowed)
            );
            let native = collected.clone();
            let task = Arc::new(crate::retained_task::RetainedTask::new(
                ProviderOperation::TurnStart,
                tokio::spawn(async move { Ok(collected) }),
            ));
            let task = fixture_worker(&session, &request.scope, task);
            collect_invocation(session.clone(), request.scope.clone(), task)
                .await
                .unwrap();
            let event = loop {
                let event = events.recv().await.unwrap().unwrap().event;
                if matches!(
                    event,
                    ProviderEvent::Turn {
                        event: forge_provider_api::TurnEvent::Completed(_),
                        ..
                    }
                ) {
                    break event;
                }
            };
            assert!(
                matches!(event, ProviderEvent::Turn { event: forge_provider_api::TurnEvent::Completed(completed), .. }
                if completed.result() == &native)
            );
            let settlement = session
                .settlements
                .lock()
                .await
                .get(&request.scope)
                .cloned()
                .unwrap()
                .unwrap();
            assert!(
                matches!(
                    settlement.completed.result().outcome(),
                    TurnOutcome::Failed {
                        retry: forge_provider_api::turn::RetryDecision::Allowed { .. },
                        ..
                    }
                ) == allowed
            );
            session.rpc.lock().await.close().await.unwrap();
        }
    }

    /// Lost rollback acknowledgment retains uncertainty and cannot remove a newer turn on retry.
    #[tokio::test]
    async fn uncertain_retraction_never_repeats_native_rollback() {
        use futures_util::{SinkExt, StreamExt};
        async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let input = request();
            let operation = context();
            let request = forge_provider_api::InputRetractRequest { scope: input.scope.clone(), submission_id: input.submission_id.clone(), attempt_id: input.attempt_id.clone(), boundary: forge_provider_api::CheckpointId::new("cancelled-turn").unwrap(), authorization: forge_provider_api::RetractionAuthorization::OutputFreeCancellation };
            *session.rollback.lock().await = Some(RollbackAuthorization { thread_id: "compaction-thread".into(), scope: input.scope.clone(), turn_id: NativeTurnId::new("cancelled-turn").unwrap(), submission_id: input.submission_id.clone(), attempt_id: input.attempt_id.clone() });
            let native = collected_result(&input, TurnOutcome::Interrupted);
            let address = forge_provider_api::TurnAddress { scope: input.scope.clone(), thread_id: input.thread_id.clone(), turn_id: input.turn_id.clone() };
            let worker = Arc::new(crate::retained_task::RetainedTask::new(ProviderOperation::TurnStart, tokio::spawn(async move { Ok(native) })));
            let worker = fixture_worker(&session, &input.scope, worker);
            let completed = worker.join().await.unwrap();
            session.settlements.lock().await.insert(input.scope.clone(), Ok(CollectedInvocation { address, completed, watermark: session.sink.watermark() }));
            let client_id = native_input_id(&input.submission_id, &input.attempt_id);
            let wire = tokio::spawn(async move {
                let read: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(read["method"], "thread/read");
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":read["id"],"result":{"thread":{"turns":[{"id":"cancelled-turn","status":"interrupted","items":[{"type":"userMessage","clientId":client_id}]}]}}}).to_string().into())).await.unwrap();
                let rollback: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(rollback["method"], "thread/revert");
                assert_eq!(rollback["params"]["beforeTurnId"], "cancelled-turn");
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
        }.await;
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

    /// Record dedicated child authority calls and fail if the root capability is substituted.
    struct ChildCallbackAuthority {
        parent: forge_provider_api::InvocationScope,
        child: forge_provider_api::TurnAddress,
        calls: std::sync::Mutex<Vec<&'static str>>,
    }

    #[async_trait]
    impl ProviderPermissions for ChildCallbackAuthority {
        async fn authorize(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: PermissionRequest,
        ) -> PermissionResponse {
            panic!("native child callback used root authorization")
        }
        async fn before_tool(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            panic!("native child callback used root checkpoint barrier")
        }
        async fn before_observed_child(
            &self,
            parent: &forge_provider_api::InvocationScope,
            child: &forge_provider_api::TurnAddress,
        ) -> Result<(), ProviderError> {
            assert_eq!(parent, &self.parent);
            assert_eq!(child, &self.child);
            self.calls.lock().unwrap().push("before");
            Ok(())
        }
        async fn authorize_observed_child(
            &self,
            parent: &forge_provider_api::InvocationScope,
            child: &forge_provider_api::TurnAddress,
            _: PermissionRequest,
        ) -> PermissionResponse {
            assert_eq!(parent, &self.parent);
            assert_eq!(child, &self.child);
            self.calls.lock().unwrap().push("authorize");
            PermissionResponse::Allow
        }
    }

    #[async_trait]
    impl ProviderTools for ChildCallbackAuthority {
        async fn call(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: ToolCall,
        ) -> Result<String, ProviderError> {
            panic!("native child callback used root tool capability")
        }
        async fn call_observed_child(
            &self,
            parent: &forge_provider_api::InvocationScope,
            child: &forge_provider_api::TurnAddress,
            call: ToolCall,
        ) -> Result<String, ProviderError> {
            assert_eq!(parent, &self.parent);
            assert_eq!(child, &self.child);
            assert_eq!(call.id.as_str(), "original-child-call");
            self.calls.lock().unwrap().push("call");
            Ok("child effect".into())
        }
    }

    /// Route an actual native callback to the original child and deny absent, retired, or successor scope.
    #[tokio::test]
    async fn native_child_host_callback_never_borrows_root_or_successor_authority() {
        use futures_util::StreamExt;
        async {
            let (_, session, _events, mut socket) = compaction_fixture().await;
            let mut request = request();
            let child = {
                let mut normalization = session.normalization.lock().await;
                normalization.admit_turn("compaction-thread", "parent", &request.scope).unwrap();
                for message in [
                    json!({"method":"item/completed","params":{"threadId":"compaction-thread","turnId":"parent","item":{"type":"subAgentActivity","id":"delegation","kind":"started","agentThreadId":"native-child"}}}),
                    json!({"method":"turn/started","params":{"threadId":"native-child","turn":{"id":"child-turn"}}}),
                    json!({"method":"turn/completed","params":{"threadId":"compaction-thread","turn":{"id":"parent","status":"completed"}}}),
                ] {
                    normalization.normalize(&message, &mut super::super::normalization::CodexOutput::default(), None, "").await.unwrap();
                }
                let starts = normalization.take_observed_starts();
                starts.into_iter().find(|(_, thread, _, _)| thread == "native-child").unwrap().0
            };
            let authority = Arc::new(ChildCallbackAuthority { parent: request.scope.clone(), child, calls: std::sync::Mutex::new(Vec::new()) });
            request.permissions = authority.clone(); request.provider_tools = authority.clone();
            request.tools.push(forge_provider_api::control::ControlToolDefinition { name:"lsp_rename", description:"fixture", input_schema:json!({}) });
            let message = json!({"id":10,"method":"item/tool/call","params":{"threadId":"native-child","turnId":"child-turn","callId":"original-child-call","tool":"lsp_rename","arguments":{}}});
            handle_native_request(&session, &request, &message).await.unwrap();
            let response: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
            assert_eq!(response["result"]["success"], true);
            assert_eq!(*authority.calls.lock().unwrap(), ["before","authorize","call"]);
            let mut successor = request.clone();
            successor.scope = forge_provider_api::InvocationScope::new(request.scope.thread_id.clone(), "successor");
            for (active, envelope) in [(&successor, message.clone()), (&request, json!({"id":11,"method":"item/tool/call","params":{"callId":"original-child-call","tool":"lsp_rename","arguments":{}}}))] {
                handle_native_request(&session, active, &envelope).await.unwrap();
                let denied: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert!(denied.get("error").is_some());
            }
            session.callback_requests.lock().await.insert(request.scope.clone(), request.clone());
            handle_native_request(&session, &successor, &message).await.unwrap();
            let original: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
            assert_eq!(original["result"]["success"], true, "retained child must use its original request during a successor");
            session.normalization.lock().await.normalize(&json!({"method":"turn/completed","params":{"threadId":"native-child","turn":{"id":"child-turn","status":"completed"}}}), &mut super::super::normalization::CodexOutput::default(), None, "").await.unwrap();
            handle_native_request(&session, &request, &message).await.unwrap();
            let denied: Value = serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
            assert!(denied.get("error").is_some());
            assert_eq!(*authority.calls.lock().unwrap(), ["before","authorize","call","before","authorize","call"]);
            session.rpc.lock().await.close().await.unwrap();
        }.await;
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
                ProviderOperation::TurnStart,
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
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::testing::root_thread("continuation-test"),
            "invocation",
        );
        let (dispatch, _) = forge_provider_api::submission::DispatchReporter::channel(
            forge_provider_api::BindingId::new("binding").unwrap(),
            scope.clone(),
            forge_provider_api::SubmissionId::try_from("submission").unwrap(),
            forge_provider_api::DeliveryAttemptId::new("attempt").unwrap(),
        );
        PromptRequest {
            dispatch,
            thread_id: scope.thread_id.clone(),
            turn_id: scope.turn_id().unwrap(),
            scope,
            execution: forge_provider_api::ExecutionMode::Default,
            configuration: InvocationConfiguration {
                model: ModelSelection::default(),
                access: AccessPolicy::default(),
                approval: ApprovalMode::Ask,
            },
            tools: Vec::new(),
            tool_endpoint: None,
            resource_lease: None,
            attempt_id: forge_provider_api::DeliveryAttemptId::new("attempt").unwrap(),
            submission_id: forge_provider_api::SubmissionId::try_from("submission".to_owned())
                .expect("fixture identity is valid"),
            // A kind with instructions makes the turn inject one developer message first.
            input: forge_provider_api::forge::TurnInput::new(
                forge_provider_api::forge::TurnKind::PlanDiscussion {
                    input: PromptInput::Text("original request".into()),
                },
            ),
            recovered_inputs: Vec::new(),
            context: Default::default(),
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
