use super::{
    normalization::CodexNormalization,
    resources::CodexResources,
    rpc::{NativeRpc, RpcHandle},
    runtime::CodexRuntime,
    security::CodexSecurity,
};
use async_trait::async_trait;
use forge_provider_api::{
    GoalStopMode, ProviderError, ProviderEvent, ProviderFailure, ProviderModel, ProviderRuntime,
    catalog::ProviderCatalogRequest,
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
        BindingId, ContinuationOutcome, ContinuationRequest, InputAcceptance, ModelSelection,
        PromptRequest, SessionBinding, SessionRequest, SteeringAcceptance, SteeringRequest,
    },
};
use serde_json::{Value, json};
use std::{collections::HashMap, path::PathBuf, sync::Arc};
use tokio::sync::{Mutex, mpsc, watch};

struct ActiveExecution {
    scope: forge_provider_api::InvocationScope,
    turn_id: String,
    turn_updates: watch::Sender<Option<String>>,
    submissions: Arc<Mutex<Vec<forge_provider_api::SubmissionId>>>,
    request: PromptRequest,
    cancel: mpsc::UnboundedSender<()>,
    task: Option<Arc<crate::retained_task::RetainedTask>>,
}

struct RetryAuthorization {
    token: forge_provider_api::ContinuationToken,
    request: PromptRequest,
}

struct RollbackAuthorization {
    thread_id: String,
}

struct BackgroundReader {
    cancel: mpsc::UnboundedSender<()>,
    task: crate::retained_task::RetainedTask,
}

struct CodexBinding {
    binding: SessionBinding,
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
    native_goal: Mutex<Option<String>>,
    native_goal_status: Mutex<Option<forge_provider_api::completion::GoalState>>,
    access: Mutex<forge_provider_api::access::AccessPolicy>,
    approval: Mutex<forge_provider_api::approval::ApprovalMode>,
    approval_context: super::approval::ApprovalContext,
    active: Mutex<Option<ActiveExecution>>,
    background_reader: Mutex<Option<BackgroundReader>>,
    retry: Mutex<Option<RetryAuthorization>>,
    rollback: Mutex<Option<RollbackAuthorization>>,
    admission: Mutex<()>,
    observations: Arc<tokio::sync::RwLock<bool>>,
    fenced: Mutex<bool>,
}

/// Own Codex app-server conversations, execution correlation, and native cleanup.
pub struct CodexProvider {
    command: Vec<String>,
    runtime_root: PathBuf,
    runtime: Arc<CodexRuntime>,
    bindings: Mutex<HashMap<BindingId, Arc<CodexBinding>>>,
    owners: Mutex<HashMap<forge_provider_api::ExecutionOwner, BindingId>>,
    catalog: Mutex<Option<CatalogConnection>>,
}

enum CatalogConnection {
    Starting(tokio::task::JoinHandle<Result<Arc<NativeRpc>, ProviderError>>),
    Ready(Arc<NativeRpc>),
}

impl CodexProvider {
    /// Create the native owner with its configured command and private resource root.
    pub fn new(command: Vec<String>, runtime_root: PathBuf) -> Result<Self, ProviderError> {
        if command.is_empty() {
            return Err(failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            ));
        }
        Ok(Self {
            runtime: Arc::new(CodexRuntime::new(command.clone())),
            command,
            runtime_root,
            bindings: Mutex::new(HashMap::new()),
            owners: Mutex::new(HashMap::new()),
            catalog: Mutex::new(None),
        })
    }

    /// Initialize catalog discovery independently of any conversation or prompt.
    async fn catalog_connection(&self) -> Result<Arc<NativeRpc>, ProviderError> {
        let mut catalog = self.catalog.lock().await;
        if catalog.is_none() {
            let runtime = self.runtime.clone();
            *catalog = Some(CatalogConnection::Starting(tokio::spawn(async move {
                let connection = runtime.connect().await.map_err(|_| {
                    failure(
                        ProviderOperation::InspectRuntime,
                        ProviderFailureKind::Transport,
                    )
                })?;
                let rpc = Arc::new(NativeRpc::new(connection));
                let initialized = async {
                    rpc.request("initialize", json!({
                        "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
                        "capabilities":{"experimentalApi":true}
                    }), ProviderOperation::InspectRuntime).await?;
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
            })));
        }
        let result = match catalog.as_mut().expect("catalog initialization installed") {
            CatalogConnection::Ready(rpc) => return Ok(rpc.clone()),
            CatalogConnection::Starting(task) => task.await.unwrap_or_else(|_| {
                Err(failure(
                    ProviderOperation::InspectRuntime,
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
        self.bindings.lock().await.get(id).cloned().ok_or_else(|| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::SessionMissing,
            )
        })
    }

    async fn initialize(&self, request: SessionRequest) -> Result<SessionBinding, ProviderError> {
        if request.workspace.trim().is_empty()
            || self.owners.lock().await.contains_key(&request.owner)
        {
            return Err(failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            ));
        }
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Unknown,
            )
        })?;
        let session_scope = forge_provider_api::EventScope::SessionOperation {
            owner: request.owner.clone(),
            operation_id: binding_id.as_str().to_owned(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), session_scope);

        let mut profile = crate::profile::ResourceProfile::create(&self.runtime_root, "codex")
            .map_err(|_| {
                failure(
                    ProviderOperation::InitializeSession,
                    ProviderFailureKind::Initialization,
                )
            })?;
        profile
            .bind_skills(&request.resources.skills, "skills")
            .map_err(|_| {
                failure(
                    ProviderOperation::InitializeSession,
                    ProviderFailureKind::Rejected,
                )
            })?;
        let private_home = profile.path().to_path_buf();
        let resources = CodexResources::new(request.resources.clone(), private_home.clone());
        let runtime = Arc::new(CodexRuntime::new(self.command.clone()));
        let connection = runtime.connect().await.map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Transport,
            )
        })?;
        let rpc = NativeRpc::new(connection);
        let initialized = rpc.request("initialize", json!({
            "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
            "capabilities":{"experimentalApi":true}
        }), ProviderOperation::InitializeSession).await?;
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

        let resource_config = resources.configure(&rpc, &request.workspace, &[]).await?;
        let mut parameters = match request.provider_session_id.as_ref() {
            Some(thread) => json!({
                "threadId":thread.as_str(), "cwd":request.workspace,
                "excludeTurns":true, "developerInstructions":request.system_instructions,
                "config":{}
            }),
            None => json!({
                "cwd":request.workspace, "ephemeral":false, "historyMode":"paginated",
                "experimentalRawEvents":false, "developerInstructions":request.system_instructions,
                "dynamicTools":request.tools.iter().map(|tool| json!({
                    "name":tool.name,"description":tool.description,"inputSchema":tool.input_schema
                })).collect::<Vec<_>>(),
                "serviceTier":request.model.service_tier
            }),
        };
        if !resource_config.is_null() {
            parameters["config"] = resource_config;
        }
        let _ = apply_model_selection(
            &rpc,
            &mut parameters,
            &request.model,
            ProviderOperation::InitializeSession,
        )
        .await?;
        CodexSecurity::new(request.approval, &request.access)
            .apply_thread(&mut parameters, &request.workspace);
        let method = if request.provider_session_id.is_some() {
            "thread/resume"
        } else {
            "thread/start"
        };
        let result = rpc
            .request(method, parameters, ProviderOperation::InitializeSession)
            .await?;
        let thread_id = result
            .pointer("/thread/id")
            .or_else(|| result.get("threadId"))
            .or_else(|| result.get("thread_id"))
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or_else(|| {
                request
                    .provider_session_id
                    .as_ref()
                    .map(|id| id.as_str().to_owned())
            })
            .ok_or_else(|| {
                failure(
                    ProviderOperation::InitializeSession,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let session_id =
            forge_provider_api::ProviderSessionId::try_from(thread_id).map_err(|_| {
                failure(
                    ProviderOperation::InitializeSession,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let binding = SessionBinding {
            id: binding_id.clone(),
            provider_session_id: Some(session_id),
        };
        let mut mcp_context = super::protocol::McpContext::default();
        mcp_context.set_bindings(resources.mcp_aliases());
        let retained = Arc::new(CodexBinding {
            binding: binding.clone(),
            owner: request.owner.clone(),
            workspace: request.workspace,
            model: Mutex::new(request.model),
            runtime: Mutex::new(runtime),
            rpc: RpcHandle::new(rpc),
            sink,
            stream: Mutex::new(Some(stream)),
            normalization: Mutex::new(CodexNormalization::default()),
            resources: Mutex::new(resources),
            mcp_context: Mutex::new(mcp_context),
            profile: Mutex::new(Some(profile)),
            _lease: request.resource_lease,
            system_instructions: request.system_instructions,
            access: Mutex::new(request.access),
            native_goal: Mutex::new(None),
            native_goal_status: Mutex::new(None),
            approval: Mutex::new(request.approval),
            approval_context: super::approval::ApprovalContext::default(),
            active: Mutex::new(None),
            background_reader: Mutex::new(None),
            admission: Mutex::new(()),
            observations: Arc::new(tokio::sync::RwLock::new(false)),
            fenced: Mutex::new(false),
            retry: Mutex::new(None),
            rollback: Mutex::new(None),
        });
        self.bindings
            .lock()
            .await
            .insert(binding_id.clone(), retained);
        self.owners.lock().await.insert(request.owner, binding_id);
        Ok(binding)
    }

    async fn submit(
        &self,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        let session = self.binding(binding).await?;
        if *session.fenced.lock().await
            || session.stream.lock().await.is_some()
            || request.scope.owner != session.owner
            || request.receipt.id() != &request.submission_id
            || !request
                .dispatch
                .matches(binding, &request.scope, &request.submission_id)
        {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        let _admission = session.admission.lock().await;
        collect_finished_execution(&session).await?;
        let mut active = session.active.lock().await;
        if active.is_some() {
            let continuation = not_consumed_token(
                &session.binding.id,
                &request.scope,
                request.submission_id.as_str(),
            )?;
            *session.retry.lock().await = Some(RetryAuthorization {
                token: continuation.clone(),
                request,
            });
            return Err(ProviderError::NotConsumed { continuation });
        }
        stop_background_reader(&session).await?;
        session.retry.lock().await.take();
        request.turn_control.before_send(&request.scope).await?;
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
        let resume_prompt = prompt.clone();
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
        let native_resume = matches!(
            request.goal.as_ref(),
            Some(forge_provider_api::native_goal::NativeGoalCommand::Resume { .. })
        );
        let model = request.configuration.model.clone();
        let approval = request.configuration.approval;
        let access = request.configuration.access.clone();
        let mut params = json!({
            "threadId":session.binding.provider_session_id.as_ref().map(|id| id.as_str().to_owned()),
            "input":native_input.clone(),
            "clientUserMessageId":request.submission_id.as_str(),
            "serviceTier":model.service_tier,
            "config":{}
        });
        let rpc = session.rpc.lock().await;
        let effort =
            apply_model_selection(&rpc, &mut params, &model, ProviderOperation::AdmitInput).await?;
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
                "model":model.model.clone(),
                "reasoning_effort":effort.clone(),
                "developer_instructions":instructions
            }
        });
        CodexSecurity::new(approval, &access).apply_turn(&mut params, &session.workspace);
        let token = recovery_token(&session, &request.submission_id)?;
        if let Err(error) = request.receipt.clone().record_sending(Some(token)).await {
            return Err(record_receipt_failure(request.receipt.clone(), error).await);
        }
        if let Some(forge_provider_api::native_goal::NativeGoalCommand::Set { objective }) =
            &request.goal
        {
            if let Err(error) = session.rpc.lock().await.request("thread/goal/set", json!({
                "threadId":session.binding.provider_session_id.as_ref().map(|id| id.as_str().to_owned()),
                "objective":objective,"status":"active"
            }), ProviderOperation::AdmitInput).await {
                let primary = if matches!(&error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Rejected) {
                    error
                } else {
                        combine_primary(ProviderError::DeliveryUncertain, error)
                };
                return Err(record_receipt_failure(request.receipt.clone(), primary).await);
            }
            *session.native_goal.lock().await = Some(objective.clone());
            *session.native_goal_status.lock().await =
                Some(forge_provider_api::completion::GoalState::Active);
        }
        let (turn_id, initial_messages, goal_resolved) = if native_resume {
            let objective = match request.goal.as_ref() {
                Some(forge_provider_api::native_goal::NativeGoalCommand::Resume { objective }) => {
                    objective
                }
                _ => {
                    return Err(record_receipt_failure(
                        request.receipt.clone(),
                        failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected),
                    )
                    .await);
                }
            };
            session
                .normalization
                .lock()
                .await
                .bind(Some(request.scope.clone()));
            let (turn_id, messages, state) = match resume_native_goal(&session, &request, objective)
                .await
            {
                Ok(admission) => admission,
                Err(error) => {
                    let primary = if matches!(&error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Rejected)
                    {
                        error
                    } else {
                        combine_primary(ProviderError::DeliveryUncertain, error)
                    };
                    return Err(record_receipt_failure(request.receipt.clone(), primary).await);
                }
            };
            (turn_id, messages, state)
        } else {
            let id = match session
                .rpc
                .lock()
                .await
                .send_request("turn/start", params)
                .await
            {
                Ok(id) => id,
                Err(error) => {
                    let primary = combine_primary(ProviderError::DeliveryUncertain, error);
                    return Err(record_receipt_failure(request.receipt.clone(), primary).await);
                }
            };
            let response = await_response(&session, id, ProviderOperation::AdmitInput).await;
            let result = match response {
                Ok(result) => result,
                Err(error) => {
                    if matches!(&error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Rejected)
                    {
                        return Err(record_receipt_failure(request.receipt.clone(), error).await);
                    }
                    let primary = combine_primary(ProviderError::DeliveryUncertain, error);
                    return Err(record_receipt_failure(request.receipt.clone(), primary).await);
                }
            };
            let turn_id = result
                .get("turnId")
                .or_else(|| result.pointer("/turn/id"))
                .and_then(Value::as_str)
                .map(str::to_owned)
                .ok_or_else(|| {
                    failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol)
                });
            let turn_id = match turn_id {
                Ok(turn_id) => turn_id,
                Err(error) => {
                    return Err(record_receipt_failure(request.receipt.clone(), error).await);
                }
            };
            (Some(turn_id), Vec::new(), None)
        };
        let accepted_token = match recovery_token(&session, &request.submission_id) {
            Ok(token) => token,
            Err(error) => {
                return Err(fence_unobserved_acceptance(
                    &session,
                    &request,
                    turn_id.as_deref(),
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
            return Err(
                fence_unobserved_acceptance(&session, &request, turn_id.as_deref(), error).await,
            );
        }
        if let Some(state) = goal_resolved {
            for message in initial_messages {
                let mut output = forge_provider_api::completion::ProviderOutput::default();
                if let Err(_error) = session
                    .normalization
                    .lock()
                    .await
                    .normalize(&message, &mut output, None, &session.workspace)
                    .await
                {
                    return Err(fence_unobserved_acceptance(
                        &session,
                        &request,
                        None,
                        failure(
                            ProviderOperation::CollectEventReader,
                            ProviderFailureKind::Protocol,
                        ),
                    )
                    .await);
                }
                for event in output.event {
                    if let Err(error) = publish_event(&session.sink, event).await {
                        return Err(
                            fence_unobserved_acceptance(&session, &request, None, error).await
                        );
                    }
                }
                if let Some(state) = super::normalization::native_goal_state(
                    &message,
                    &session
                        .binding
                        .provider_session_id
                        .as_ref()
                        .map(|id| id.as_str().to_owned())
                        .unwrap_or_default(),
                ) {
                    *session.native_goal_status.lock().await = Some(state);
                }
            }
            *session.native_goal_status.lock().await = Some(state);
            let control = request.turn_control.close(&request.scope).await;
            let effects = request
                .host_effects
                .settle_foreground(&session.binding.id, &request.scope)
                .await;
            let completion = completion(&session, &model).await;
            let result = ExecutionResult::Completed { completion };
            let host_failure = if control.is_err() {
                Some(forge_provider_api::session::HostEffectFailure::Uncertain)
            } else {
                effects.as_ref().err().cloned()
            };
            let update = match host_failure {
                Some(failure) => ExecutionUpdate::ResolutionPublicationFailed {
                    covered_submissions: vec![request.submission_id.clone()],
                    completed_at_ms: now_ms(),
                    result,
                    failure,
                },
                None => ExecutionUpdate::ResolvedWithoutExecution {
                    covered_submissions: vec![request.submission_id.clone()],
                    completed_at_ms: now_ms(),
                    result,
                },
            };
            let observation = match ExecutionObservation::new(request.scope.clone(), update) {
                Ok(observation) => observation,
                Err(error) => {
                    return Err(fence_unobserved_acceptance(
                        &session,
                        &request,
                        None,
                        error.into(),
                    )
                    .await);
                }
            };
            if let Err(error) = publish(
                &session.sink,
                &request.scope,
                ProviderEvent::Execution { observation },
            )
            .await
            {
                return Err(fence_unobserved_acceptance(&session, &request, None, error).await);
            }
            let mut errors = Vec::new();
            if let Err(error) = control {
                errors.push(error);
            }
            if let Err(error) = effects {
                let kind = match error {
                    forge_provider_api::session::HostEffectFailure::Rejected => {
                        ProviderFailureKind::Rejected
                    }
                    forge_provider_api::session::HostEffectFailure::Uncertain
                    | forge_provider_api::session::HostEffectFailure::Retired => {
                        ProviderFailureKind::Unknown
                    }
                };
                errors.push(failure(ProviderOperation::HostEffectPublication, kind));
            }
            combine_errors(errors)?;
            request.dispatch.confirm();
            return Ok(InputAcceptance {
                submission_id: request.submission_id,
            });
        }
        let turn_id = match turn_id {
            Some(turn_id) => turn_id,
            None => {
                return Err(fence_unobserved_acceptance(
                    &session,
                    &request,
                    None,
                    failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol),
                )
                .await);
            }
        };
        if native_resume && !resume_prompt.trim().is_empty() {
            let enabled_tools = request
                .tools
                .iter()
                .map(|tool| tool.name)
                .collect::<Vec<_>>();
            let tool_context = if enabled_tools.is_empty() {
                String::new()
            } else {
                format!(
                    "\n\nFor this invocation, the available host tools are: {}.",
                    enabled_tools.join(", ")
                )
            };
            let mut resume_input = native_input;
            resume_input.push(json!({
                "type":"text",
                "text":format!("{}{}", request.instructions, tool_context)
            }));
            let params = json!({
                "threadId":session.binding.provider_session_id.as_ref().map(|id| id.as_str().to_owned()),
                "expectedTurnId":turn_id,
                "input":resume_input,
            });
            if let Err(error) = session
                .rpc
                .lock()
                .await
                .request("turn/steer", params, ProviderOperation::AdmitInput)
                .await
            {
                return Err(
                    fence_unobserved_acceptance(&session, &request, Some(&turn_id), error).await,
                );
            }
        }
        let address = forge_provider_api::TurnAddress {
            scope: request.scope.clone(),
            thread_id: session
                .binding
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned())
                .unwrap_or_default(),
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
        let task = tokio::spawn(async move {
            observe_execution(
                session_task,
                request,
                address,
                submissions,
                receiver,
                initial_messages,
            )
            .await
        });
        active.as_mut().expect("active execution was inserted").task =
            Some(Arc::new(crate::retained_task::RetainedTask::new(task)));
        dispatched.confirm();
        Ok(InputAcceptance {
            submission_id: accepted_submission_id,
        })
    }

    async fn request_binding(
        &self,
        request: &ProviderCatalogRequest,
    ) -> Result<Arc<CodexBinding>, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            )
        })?;
        let id = self
            .owners
            .lock()
            .await
            .get(&owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let session = self.binding(&id).await?;
        if request.workspace != session.workspace
            || request
                .provider_session_id
                .as_ref()
                .is_some_and(|requested| {
                    session.binding.provider_session_id.as_ref() != Some(requested)
                })
        {
            return Err(failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            ));
        }
        Ok(session)
    }
}

#[async_trait]
impl Provider for CodexProvider {
    async fn initialize_session(
        &self,
        request: SessionRequest,
    ) -> Result<SessionBinding, ProviderError> {
        self.initialize(request).await
    }

    async fn take_session_events(
        &self,
        binding: &BindingId,
    ) -> Result<ProviderEventStream, ProviderError> {
        self.binding(binding)
            .await?
            .stream
            .lock()
            .await
            .take()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::CollectEventReader,
                    ProviderFailureKind::Rejected,
                )
            })
    }

    async fn session_prompt(
        &self,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        self.submit(binding, request).await
    }

    async fn session_steer(
        &self,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let session = self.binding(binding).await?;
        if request.scope.owner != session.owner
            || *session.fenced.lock().await
            || session.stream.lock().await.is_some()
        {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        stop_background_reader(&session).await?;
        let (initial_turn, turn_updates, submissions, prompt_request) = {
            let active = session.active.lock().await;
            let Some(execution) = active.as_ref() else {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ));
            };
            if execution.scope != request.scope {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ));
            }
            (
                execution.turn_id.clone(),
                execution.turn_updates.clone(),
                execution.submissions.clone(),
                execution.request.clone(),
            )
        };
        let submission_id = request.delivery.submission_id().cloned();
        let receipt = request.delivery.receipt().cloned();
        if let (Some(receipt), Some(submission_id)) = (&receipt, &submission_id) {
            let token = recovery_token(&session, submission_id)?;
            if let Err(error) = receipt.clone().record_sending(Some(token)).await {
                return Err(record_receipt_failure(receipt.clone(), error).await);
            }
        }
        if let Some(goal) = &request.goal {
            let (objective, is_resume) = match goal {
                forge_provider_api::NativeGoalCommand::Set { objective } => (objective, false),
                forge_provider_api::NativeGoalCommand::Resume { objective } => (objective, true),
            };
            let thread_id = session
                .binding
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned());
            if let Err(error) = session
                .rpc
                .lock()
                .await
                .request(
                    "thread/goal/set",
                    json!({
                        "threadId":thread_id,"objective":objective,"status":"active"
                    }),
                    ProviderOperation::AdmitInput,
                )
                .await
            {
                let primary = if matches!(&error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Rejected)
                {
                    error
                } else {
                    combine_primary(ProviderError::DeliveryUncertain, error)
                };
                if let Some(receipt) = receipt.clone() {
                    let primary = record_receipt_failure(receipt, primary).await;
                    return Err(fence_unobserved_acceptance(
                        &session,
                        &prompt_request,
                        Some(&initial_turn),
                        primary,
                    )
                    .await);
                }
                return Err(fence_unobserved_acceptance(
                    &session,
                    &prompt_request,
                    Some(&initial_turn),
                    primary,
                )
                .await);
            }
            *session.native_goal.lock().await = Some(objective.clone());
            *session.native_goal_status.lock().await =
                Some(forge_provider_api::completion::GoalState::Active);
            if is_resume {
                let mut updates = turn_updates.subscribe();
                let current = tokio::time::timeout(std::time::Duration::from_secs(30), async {
                    loop {
                        if let Some(turn_id) = updates.borrow().clone() {
                            if turn_id != initial_turn {
                                return Ok(Some(turn_id));
                            }
                        } else {
                            return Ok(None);
                        }
                        updates.changed().await.map_err(|_| {
                            failure(
                                ProviderOperation::AdmitInput,
                                ProviderFailureKind::Transport,
                            )
                        })?;
                    }
                })
                .await;
                let current = match current {
                    Ok(current) => current,
                    Err(_) => {
                        let error = failure(
                            ProviderOperation::AdmitInput,
                            ProviderFailureKind::Transport,
                        );
                        if let Some(receipt) = receipt.clone() {
                            let error = record_receipt_failure(receipt, error).await;
                            return Err(fence_unobserved_acceptance(
                                &session,
                                &prompt_request,
                                Some(&initial_turn),
                                error,
                            )
                            .await);
                        }
                        return Err(fence_unobserved_acceptance(
                            &session,
                            &prompt_request,
                            Some(&initial_turn),
                            error,
                        )
                        .await);
                    }
                };
                match current {
                    Ok(Some(turn_id)) => {
                        // The observer is the only reader and publishes the authoritative turn ID.
                        if let Some(execution) = session.active.lock().await.as_mut() {
                            execution.turn_id = turn_id;
                        }
                    }
                    Ok(None) => {
                        let error =
                            failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected);
                        if let Some(receipt) = receipt.clone() {
                            return Err(record_receipt_failure(receipt, error).await);
                        }
                        return Err(error);
                    }
                    Err(error) => {
                        if let Some(receipt) = receipt.clone() {
                            let error = record_receipt_failure(receipt, error).await;
                            return Err(fence_unobserved_acceptance(
                                &session,
                                &prompt_request,
                                Some(&initial_turn),
                                error,
                            )
                            .await);
                        }
                        return Err(fence_unobserved_acceptance(
                            &session,
                            &prompt_request,
                            Some(&initial_turn),
                            error,
                        )
                        .await);
                    }
                }
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
            "threadId":session.binding.provider_session_id.as_ref().map(|id| id.as_str().to_owned()),
            "expectedTurnId":expected_turn_id,
            "input":super::attachments::input(prompt, &request.attachments)
        });
        if let Some(submission_id) = &submission_id {
            params["clientUserMessageId"] = json!(submission_id.as_str());
        }
        let result = session
            .rpc
            .lock()
            .await
            .request("turn/steer", params, ProviderOperation::AdmitInput)
            .await;
        if let Err(error) = result {
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
            let accepted_token = match recovery_token(&session, &submission_id) {
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
                let turn_id = session
                    .active
                    .lock()
                    .await
                    .as_ref()
                    .map(|execution| execution.turn_id.clone());
                return Err(fence_unobserved_acceptance(
                    &session,
                    &prompt_request,
                    turn_id.as_deref(),
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

    async fn retire_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.binding(binding).await?;
        let mut observations = session.observations.write().await;
        *observations = true;
        let cancel_result = self.cancel_session(binding).await;
        let profile_result = if let Some(mut profile) = session.profile.lock().await.take() {
            profile.close().map_err(|_| {
                failure(
                    ProviderOperation::RetireSession,
                    ProviderFailureKind::Runtime,
                )
            })
        } else {
            Ok(())
        };
        let close_result = session.rpc.lock().await.close().await;
        let runtime_result = session.runtime.lock().await.shutdown().await.map_err(|_| {
            failure(
                ProviderOperation::CollectNativeRuntime,
                ProviderFailureKind::Transport,
            )
        });
        self.bindings.lock().await.remove(binding);
        self.owners.lock().await.remove(&session.owner);
        let cleanup = [profile_result, close_result, runtime_result]
            .into_iter()
            .find_map(Result::err);
        match (cancel_result, cleanup) {
            (Ok(()), None) => Ok(()),
            (Err(error), None) | (Ok(()), Some(error)) => Err(error),
            (Err(primary), Some(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
        }
    }

    async fn fence_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.binding(binding).await?;
        *session.fenced.lock().await = true;
        session.sink.fail(events::EventDeliveryError::Observation(
            forge_provider_api::lifecycle::ObservationFailure::Disconnected,
        ));
        self.cancel_session(binding).await
    }

    async fn recover_session(
        &self,
        request: SessionRequest,
        _token: Option<forge_provider_api::RecoveryToken>,
    ) -> Result<SessionBinding, ProviderError> {
        self.initialize(request).await
    }

    async fn reconcile_settings(
        &self,
        request: ProviderCatalogRequest,
        settings: forge_provider_api::SessionSettings,
        resources: forge_provider_api::session::ResourceSelection,
    ) -> Result<(), ProviderError> {
        let session = self.request_binding(&request).await?;
        if session.active.lock().await.is_some() {
            self.cancel_session(&session.binding.id).await?;
        }
        let _admission = session.admission.lock().await;
        if session.active.lock().await.is_some() || *session.fenced.lock().await {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
        stop_background_reader(&session).await?;

        let mut next_profile = crate::profile::ResourceProfile::create(&self.runtime_root, "codex")
            .map_err(|error| error.into_provider_error(ProviderOperation::ReconcileSettings))?;
        next_profile
            .bind_skills(&resources.skills, "skills")
            .map_err(|error| error.into_provider_error(ProviderOperation::ReconcileSettings))?;
        let next_resources =
            CodexResources::new(resources.clone(), next_profile.path().to_path_buf());
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
                *session.fenced.lock().await = true;
                session.sink.fail(events::EventDeliveryError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                ));
                return Err(failure(
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::Transport,
                ));
            }
        };
        let next_rpc = NativeRpc::new(connection);
        let current_model = session.model.lock().await.clone();
        let current_access = session.access.lock().await.clone();
        let current_approval = *session.approval.lock().await;
        let resumed = async {
            let initialized = next_rpc.request("initialize", json!({
                "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
                "capabilities":{"experimentalApi":true}
            }), ProviderOperation::ReconcileSettings).await?;
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
            let thread_id = session
                .binding
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned())
                .ok_or_else(|| {
                    failure(
                        ProviderOperation::ReconcileSettings,
                        ProviderFailureKind::SessionMissing,
                    )
                })?;
            let model = settings.model.clone().unwrap_or(current_model);
            let access = settings.access.clone().unwrap_or(current_access);
            let approval = settings.approval.unwrap_or(current_approval);
            let mut parameters = json!({
                "threadId":thread_id,
                "cwd":session.workspace,
                "excludeTurns":true,
                "developerInstructions":session.system_instructions,
                "config":{}
            });
            if !config.is_null() {
                parameters["config"] = config;
            }
            let _ = apply_model_selection(
                &next_rpc,
                &mut parameters,
                &model,
                ProviderOperation::ReconcileSettings,
            )
            .await?;
            CodexSecurity::new(approval, &access).apply_thread(&mut parameters, &session.workspace);
            next_rpc
                .request(
                    "thread/resume",
                    parameters,
                    ProviderOperation::ReconcileSettings,
                )
                .await?;
            Ok::<_, ProviderError>((model, access, approval))
        }
        .await;
        let (model, access, approval) = match resumed {
            Ok(settings) => settings,
            Err(error) => {
                let _ = next_rpc.close().await;
                *session.fenced.lock().await = true;
                session.sink.fail(events::EventDeliveryError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                ));
                return Err(error);
            }
        };

        let old_rpc = session.rpc.replace(next_rpc).await;
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
        let profile_cleanup =
            if let Some(mut old_profile) = session.profile.lock().await.replace(next_profile) {
                old_profile.close().map_err(|error| {
                    error.into_provider_error(ProviderOperation::ReconcileSettings)
                })
            } else {
                Ok(())
            };
        let rpc_cleanup = old_rpc.close().await;
        match (profile_cleanup, rpc_cleanup) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(error), Ok(())) | (Ok(()), Err(error)) => {
                *session.fenced.lock().await = true;
                session.sink.fail(events::EventDeliveryError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                ));
                Err(error)
            }
            (Err(primary), Err(secondary)) => {
                *session.fenced.lock().await = true;
                session.sink.fail(events::EventDeliveryError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                ));
                Err(ProviderError::Combined {
                    primary: Box::new(primary),
                    secondary: vec![secondary],
                })
            }
        }
    }

    async fn reconcile_submission(
        &self,
        submission_id: &forge_provider_api::SubmissionId,
        token: Option<&forge_provider_api::RecoveryToken>,
        _accepted_receipts: &[forge_provider_api::RecoveryToken],
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        let Some(evidence) = parse_recovery_token(token) else {
            return Ok(forge_provider_api::SubmissionEvidence::Unknown);
        };
        if evidence.submission_id != submission_id.as_str() {
            return Ok(forge_provider_api::SubmissionEvidence::Unknown);
        }
        let connection = match self.runtime.connect().await {
            Ok(connection) => connection,
            Err(_) => return Ok(forge_provider_api::SubmissionEvidence::Unknown),
        };
        let rpc = NativeRpc::new(connection);
        let operation_result: Result<forge_provider_api::SubmissionEvidence, ProviderError> =
            async {
                let initialized = rpc.request("initialize", json!({
                "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
                "capabilities":{"experimentalApi":true}
            }), ProviderOperation::RecoverSession).await?;
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
                }), ProviderOperation::RecoverSession).await?;
                    let rows = page.get("data").and_then(Value::as_array).ok_or_else(|| {
                        failure(
                            ProviderOperation::RecoverSession,
                            ProviderFailureKind::Protocol,
                        )
                    })?;
                    for row in rows {
                        let item = row.get("item").unwrap_or(row);
                        if item.get("type").and_then(Value::as_str) == Some("userMessage")
                            && item.get("clientId").and_then(Value::as_str)
                                == Some(submission_id.as_str())
                        {
                            let receipt =
                                recovery_token_parts(&evidence.thread_id, submission_id.as_str())?;
                            let session_id = forge_provider_api::ProviderSessionId::try_from(
                                evidence.thread_id.clone(),
                            )
                            .map_err(|_| {
                                failure(
                                    ProviderOperation::RecoverSession,
                                    ProviderFailureKind::Protocol,
                                )
                            })?;
                            return Ok(forge_provider_api::SubmissionEvidence::Accepted {
                                receipt: Some(receipt),
                                provider_session_id: Some(session_id),
                            });
                        }
                    }
                    let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
                        return Ok(forge_provider_api::SubmissionEvidence::NotAccepted);
                    };
                    if !seen_cursors.insert(next.to_owned()) {
                        return Ok(forge_provider_api::SubmissionEvidence::Unknown);
                    }
                    cursor = Some(next.to_owned());
                }
            }
            .await;
        let cleanup = rpc.close().await;
        match (operation_result, cleanup) {
            (Ok(evidence), Ok(())) => Ok(evidence),
            (Ok(_), Err(_)) | (Err(_), _) => Ok(forge_provider_api::SubmissionEvidence::Unknown),
        }
    }

    async fn shutdown(&self) -> Result<(), ProviderError> {
        let mut errors = Vec::new();
        let mut catalog = self.catalog.lock().await;
        let catalog_result = match catalog.as_mut() {
            Some(CatalogConnection::Starting(task)) => Some(task.await.unwrap_or_else(|_| {
                Err(failure(
                    ProviderOperation::CollectNativeRuntime,
                    ProviderFailureKind::Runtime,
                ))
            })),
            Some(CatalogConnection::Ready(rpc)) => Some(Ok(rpc.clone())),
            None => None,
        };
        if let Some(Ok(rpc)) = &catalog_result {
            *catalog = Some(CatalogConnection::Ready(rpc.clone()));
            if let Err(error) = rpc.close().await {
                errors.push(error);
            }
        }
        catalog.take();
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
            if let Err(error) = self.retire_session(&binding).await {
                errors.push(error);
            }
        }
        if self.runtime.shutdown().await.is_err() {
            errors.push(failure(
                ProviderOperation::CollectNativeRuntime,
                ProviderFailureKind::Transport,
            ));
        }
        if errors.is_empty() {
            return Ok(());
        }
        Err(ProviderError::Combined {
            primary: Box::new(errors.remove(0)),
            secondary: errors,
        })
    }

    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        let id = crate::registry::select("codex").expect("Codex is registered");
        crate::registry::descriptor(&id).expect("registered Codex descriptor")
    }

    fn supports_task_steering(&self, kind: forge_provider_api::input::TaskKind) -> bool {
        kind == forge_provider_api::input::TaskKind::Goal
    }

    async fn has_active_turn(&self, owner: &forge_provider_api::ExecutionOwner) -> bool {
        let Some(binding) = self.owners.lock().await.get(owner).cloned() else {
            return false;
        };
        let Ok(session) = self.binding(&binding).await else {
            return false;
        };
        let active = session.active.lock().await;
        active.as_ref().is_some_and(|execution| {
            execution
                .task
                .as_ref()
                .is_none_or(|task| !task.is_finished())
        })
    }

    async fn cancel_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.binding(binding).await?;
        let retained = {
            let active = session.active.lock().await;
            let Some(execution) = active.as_ref() else {
                return Ok(());
            };
            if execution
                .task
                .as_ref()
                .is_some_and(|task| !task.is_finished())
            {
                execution.cancel.send(()).map_err(|_| {
                    failure(
                        ProviderOperation::CancelSession,
                        ProviderFailureKind::Transport,
                    )
                })?;
            }
            execution
                .task
                .as_ref()
                .map(|task| (execution.scope.clone(), task.clone()))
        };
        if let Some((scope, task)) = retained {
            let result = tokio::time::timeout(std::time::Duration::from_secs(30), task.join())
                .await
                .map_err(|_| {
                    failure(
                        ProviderOperation::CancelSession,
                        ProviderFailureKind::Timeout,
                    )
                })?;
            let mut active = session.active.lock().await;
            if active
                .as_ref()
                .is_some_and(|execution| execution.scope == scope)
            {
                active.take();
            }
            drop(active);
            result?;
        }
        Ok(())
    }

    async fn cleanup_execution(&self, binding: &BindingId) -> Result<(), ProviderError> {
        self.cancel_session(binding).await?;
        let session = self.binding(binding).await?;
        stop_background_reader(&session).await?;
        stop_background_commands(&session).await
    }

    async fn rollback_cancelled_turn(&self, binding: &BindingId) -> Result<bool, ProviderError> {
        let session = self.binding(binding).await?;
        let mut rollback = session.rollback.lock().await;
        let Some(authorization) = rollback.take() else {
            return Ok(false);
        };
        match session
            .rpc
            .lock()
            .await
            .request(
                "thread/rollback",
                json!({"threadId":authorization.thread_id,"numTurns":1}),
                ProviderOperation::CancelSession,
            )
            .await
        {
            Ok(_) => Ok(true),
            Err(error) => {
                *rollback = Some(authorization);
                Err(error)
            }
        }
    }

    async fn stop_goal_session(
        &self,
        owner: &forge_provider_api::ExecutionOwner,
        mode: GoalStopMode,
    ) -> Result<(), ProviderError> {
        let binding = self
            .owners
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let session = self.binding(&binding).await?;
        let active = session.active.lock().await;
        let Some(execution) = active.as_ref() else {
            return Ok(());
        };
        let thread_id = session
            .binding
            .provider_session_id
            .as_ref()
            .map(|id| id.as_str().to_owned())
            .ok_or_else(|| {
                failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let rpc = session.rpc.lock().await;
        match mode {
            GoalStopMode::RetainConversation => {
                let objective = session.native_goal.lock().await.clone().ok_or_else(|| {
                    failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected)
                })?;
                rpc.request(
                    "thread/goal/set",
                    json!({"threadId":thread_id,"objective":objective,"status":"paused"}),
                    ProviderOperation::AdmitInput,
                )
                .await?;
                *session.native_goal_status.lock().await =
                    Some(forge_provider_api::completion::GoalState::Paused);
            }
            GoalStopMode::ClearConversation => {
                rpc.request(
                    "thread/goal/clear",
                    json!({"threadId":thread_id}),
                    ProviderOperation::AdmitInput,
                )
                .await?;
                *session.native_goal.lock().await = None;
                *session.native_goal_status.lock().await =
                    Some(forge_provider_api::completion::GoalState::Cleared);
            }
        }
        rpc.request(
            "turn/interrupt",
            json!({"threadId":thread_id,"turnId":execution.turn_id}),
            ProviderOperation::CancelSession,
        )
        .await?;
        Ok(())
    }

    async fn goal_status(
        &self,
        owner: &forge_provider_api::ExecutionOwner,
        update: forge_provider_api::native_goal::NativeGoalUpdate,
    ) -> Result<(), ProviderError> {
        let binding = self
            .owners
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let session = self.binding(&binding).await?;
        let thread_id = session
            .binding
            .provider_session_id
            .as_ref()
            .map(|id| id.as_str().to_owned())
            .ok_or_else(|| {
                failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let (method, params) = match update {
            forge_provider_api::native_goal::NativeGoalUpdate::Absent => {
                ("thread/goal/clear", json!({"threadId":thread_id}))
            }
            forge_provider_api::native_goal::NativeGoalUpdate::Present { objective, status } => {
                *session.native_goal.lock().await = Some(objective.clone());
                *session.native_goal_status.lock().await = Some(match status {
                    forge_provider_api::native_goal::NativeGoalStatus::Active => {
                        forge_provider_api::completion::GoalState::Active
                    }
                    forge_provider_api::native_goal::NativeGoalStatus::Paused => {
                        forge_provider_api::completion::GoalState::Paused
                    }
                    forge_provider_api::native_goal::NativeGoalStatus::Complete => {
                        forge_provider_api::completion::GoalState::Complete
                    }
                    forge_provider_api::native_goal::NativeGoalStatus::Blocked => {
                        forge_provider_api::completion::GoalState::Blocked
                    }
                    forge_provider_api::native_goal::NativeGoalStatus::UsageLimited => {
                        forge_provider_api::completion::GoalState::UsageLimited
                    }
                    forge_provider_api::native_goal::NativeGoalStatus::BudgetLimited => {
                        forge_provider_api::completion::GoalState::BudgetLimited
                    }
                });
                let status = match status {
                    forge_provider_api::native_goal::NativeGoalStatus::Active => "active",
                    forge_provider_api::native_goal::NativeGoalStatus::Paused => "paused",
                    forge_provider_api::native_goal::NativeGoalStatus::Complete => "complete",
                    forge_provider_api::native_goal::NativeGoalStatus::Blocked => "blocked",
                    forge_provider_api::native_goal::NativeGoalStatus::UsageLimited => {
                        "usageLimited"
                    }
                    forge_provider_api::native_goal::NativeGoalStatus::BudgetLimited => {
                        "budgetLimited"
                    }
                };
                (
                    "thread/goal/set",
                    json!({"threadId":thread_id,"objective":objective,"status":status}),
                )
            }
        };
        session
            .rpc
            .lock()
            .await
            .request(method, params, ProviderOperation::AdmitInput)
            .await?;
        if method == "thread/goal/clear" {
            *session.native_goal.lock().await = None;
            *session.native_goal_status.lock().await =
                Some(forge_provider_api::completion::GoalState::Cleared);
        }
        Ok(())
    }

    async fn active_session_id_for(
        &self,
        owner: &forge_provider_api::ExecutionOwner,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
        let Some(binding) = self.owners.lock().await.get(owner).cloned() else {
            return Ok(None);
        };
        Ok(self
            .binding(&binding)
            .await?
            .binding
            .provider_session_id
            .clone())
    }

    async fn generate_text(
        &self,
        request: forge_provider_api::TextGenerationRequest,
    ) -> Result<String, ProviderError> {
        super::text_generation::generate(&self.runtime, &self.command, request).await
    }

    async fn model_list(
        &self,
        _request: ProviderCatalogRequest,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        let rpc = self.catalog_connection().await?;
        let catalog = rpc
            .request(
                "model/list",
                json!({}),
                ProviderOperation::ReconcileSettings,
            )
            .await?;
        Ok(crate::codex::catalog::parse_model_catalog(&catalog))
    }

    async fn skill_list(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::catalog::SkillDefinition>, ProviderError> {
        let session = self.request_binding(&request).await?;
        let catalog = session
            .rpc
            .lock()
            .await
            .request(
                "skills/list",
                json!({
                    "cwd":session.workspace,"forceReload":true
                }),
                ProviderOperation::ReconcileSettings,
            )
            .await?;
        Ok(crate::codex::catalog::parse_skill_catalog(&catalog))
    }

    async fn mcp_configuration(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let session = self.request_binding(&request).await?;
        let config = session
            .rpc
            .lock()
            .await
            .request(
                "config/read",
                json!({
                    "cwd":session.workspace,"includeLayers":false
                }),
                ProviderOperation::ReconcileSettings,
            )
            .await?;
        let parsed = crate::codex::catalog::parse_mcp_catalog(&Value::Null, &config);
        Ok(session.resources.lock().await.normalize_mcp(parsed))
    }

    async fn mcp_list(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let session = self.request_binding(&request).await?;
        let thread_id = session
            .binding
            .provider_session_id
            .as_ref()
            .map(|id| id.as_str().to_owned())
            .ok_or_else(|| {
                failure(
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let mut cursor = None::<String>;
        let mut seen = std::collections::HashSet::new();
        let mut rows = Vec::new();
        loop {
            let page = session
                .rpc
                .lock()
                .await
                .request(
                    "mcpServerStatus/list",
                    json!({
                        "threadId":thread_id,"detail":"toolsAndAuthOnly","cursor":cursor
                    }),
                    ProviderOperation::ReconcileSettings,
                )
                .await?;
            rows.extend(
                page.get("data")
                    .and_then(Value::as_array)
                    .cloned()
                    .unwrap_or_default(),
            );
            let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
                break;
            };
            if !seen.insert(next.to_owned()) {
                return Err(failure(
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::Protocol,
                ));
            }
            cursor = Some(next.to_owned());
        }
        let config = session
            .rpc
            .lock()
            .await
            .request(
                "config/read",
                json!({
                    "cwd":session.workspace,"includeLayers":false
                }),
                ProviderOperation::ReconcileSettings,
            )
            .await?;
        let parsed = crate::codex::catalog::parse_mcp_catalog(&json!({"data":rows}), &config);
        Ok(session.resources.lock().await.normalize_mcp(parsed))
    }

    async fn fork(
        &self,
        request: forge_provider_api::session::ConversationForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        let session = self.request_binding(&request.source).await?;
        if session.active.lock().await.is_some()
            || self.owners.lock().await.contains_key(&request.target.owner)
        {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
        let mut parameters = if let (Some(thread), Some(checkpoint)) = (
            session.binding.provider_session_id.as_ref(),
            request.checkpoint.as_ref(),
        ) {
            json!({
                "threadId":thread.as_str(), "cwd":request.target.workspace,
                "lastTurnId":checkpoint.as_str(), "excludeTurns":true,
            })
        } else {
            json!({
                "cwd":request.target.workspace,"historyMode":"paginated",
                "developerInstructions":request.target.system_instructions,
            })
        };
        let rpc = session.rpc.lock().await;
        let _ = apply_model_selection(
            &rpc,
            &mut parameters,
            &request.target.configuration.model,
            ProviderOperation::ReconcileSettings,
        )
        .await?;
        drop(rpc);
        CodexSecurity::new(
            request.target.configuration.approval,
            &request.target.configuration.access,
        )
        .apply_thread(&mut parameters, &request.target.workspace);
        let method = if request.checkpoint.is_some() {
            "thread/fork"
        } else {
            "thread/start"
        };
        let result = session
            .rpc
            .lock()
            .await
            .request(method, parameters, ProviderOperation::ReconcileSettings)
            .await?;
        let thread_id = result
            .pointer("/thread/id")
            .or_else(|| result.get("threadId"))
            .or_else(|| result.get("thread_id"))
            .and_then(Value::as_str)
            .ok_or_else(|| {
                failure(
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let provider_session_id = forge_provider_api::ProviderSessionId::try_from(
            thread_id.to_owned(),
        )
        .map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Protocol,
            )
        })?;
        Ok(forge_provider_api::ProviderForkResult::unprofiled(
            provider_session_id,
        ))
    }

    async fn compact(
        &self,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::Compaction, ProviderError> {
        let session = self.binding(binding).await?;
        let observation = session.observations.read().await;
        let _admission = session.admission.lock().await;
        if *observation || *session.fenced.lock().await || request.scope.owner() != &session.owner {
            return Err(failure(
                ProviderOperation::CompactSession,
                ProviderFailureKind::Rejected,
            ));
        }
        // A terminal event can arrive before its reader returns. Join only a
        // settled execution; compaction must never interrupt an active turn.
        collect_settled_execution(&session).await?;
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::CompactSession,
                ProviderFailureKind::Rejected,
            ));
        }
        // Transfer exclusive notification consumption from the idle reader.
        stop_background_reader(&session).await?;
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(120),
            compact_session(&session, request),
        )
        .await
        .unwrap_or_else(|_| {
            Err(failure(
                ProviderOperation::CompactSession,
                ProviderFailureKind::Timeout,
            ))
        });
        if matches!(&result, Err(ProviderError::Failure(failure))
            if matches!(failure.kind(), ProviderFailureKind::Timeout | ProviderFailureKind::Transport | ProviderFailureKind::Protocol))
        {
            // Unknown native completion cannot authorize another prompt on this binding.
            *session.fenced.lock().await = true;
            session.sink.fail(events::EventDeliveryError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable,
            ));
            return result;
        }
        let restarted = start_background_reader(&session).await;
        match (result, restarted) {
            (result, Ok(start)) => {
                if let Some(start) = start {
                    let _ = start.send(());
                }
                result
            }
            (Ok(_), Err(error)) => Err(error),
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
        }
    }

    async fn background_terminals(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
        let session = self.request_binding(&request).await?;
        let observation = session.observations.clone().read_owned().await;
        if *observation {
            return Err(failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::SessionMissing,
            ));
        }
        tokio::spawn(async move {
            let _observation = observation;
            let Some(thread_id) = session
                .binding
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned())
            else {
                return Ok(forge_provider_api::terminal::TerminalSnapshot {
                    supported: true,
                    starting: false,
                    terminal: Vec::new(),
                });
            };
            let mut cursor = None::<String>;
            let mut seen = std::collections::HashSet::new();
            let mut terminal = Vec::new();
            loop {
                let page = session
                    .rpc
                    .lock()
                    .await
                    .request_observation(
                        "thread/backgroundTerminals/list",
                        json!(
                            {"threadId":thread_id,"limit":100,"cursor":cursor}
                        ),
                        ProviderOperation::ReconcileSettings,
                    )
                    .await?;
                let parsed = crate::terminal::parse_snapshot(&page, false).map_err(|_| {
                    failure(
                        ProviderOperation::ReconcileSettings,
                        ProviderFailureKind::Protocol,
                    )
                })?;
                terminal.extend(parsed.terminal);
                let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
                    break;
                };
                if !seen.insert(next.to_owned()) {
                    return Err(failure(
                        ProviderOperation::ReconcileSettings,
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
        })
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Runtime,
            )
        })?
    }

    async fn terminate_terminal(
        &self,
        request: ProviderCatalogRequest,
        terminal_id: &str,
    ) -> Result<(), ProviderError> {
        if terminal_id.is_empty() {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
        let session = self.request_binding(&request).await?;
        let thread_id = session
            .binding
            .provider_session_id
            .as_ref()
            .map(|id| id.as_str().to_owned())
            .ok_or_else(|| {
                failure(
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let result = session
            .rpc
            .lock()
            .await
            .request(
                "thread/backgroundTerminals/terminate",
                json!(
                    {"threadId":thread_id,"processId":terminal_id}
                ),
                ProviderOperation::ReconcileSettings,
            )
            .await?;
        if result.get("terminated").and_then(Value::as_bool) != Some(true) {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
        Ok(())
    }

    async fn continue_session(
        &self,
        binding: &BindingId,
        request: ContinuationRequest,
    ) -> Result<ContinuationOutcome, ProviderError> {
        match request {
            ContinuationRequest::NotConsumed {
                request,
                authorization,
            } => {
                let session = self.binding(binding).await?;
                if request.scope.owner != session.owner {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                }
                let mut retry = session.retry.lock().await;
                let authorized_request =
                    consume_not_consumed(&mut retry, &authorization, &request)?;
                drop(retry);
                self.submit(binding, authorized_request)
                    .await
                    .map(ContinuationOutcome::InputAccepted)
            }
            ContinuationRequest::RetryableExecution {
                scope,
                authorization,
                callbacks,
                dispatch,
            } => {
                if !dispatch.matches_continuation(binding, &scope, &authorization) {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Protocol,
                    ));
                }

                let callbacks = *callbacks;
                let session = self.binding(binding).await?;
                if scope.owner != session.owner {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                }
                let _admission = session.admission.lock().await;
                collect_finished_execution(&session).await?;
                if session.active.lock().await.is_some() || *session.fenced.lock().await {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                }
                let mut retry = session.retry.lock().await;
                let Some(authorized) = retry.take() else {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                };
                if authorized.token != authorization
                    || authorized.request.scope.owner != scope.owner
                    || authorized.request.scope.invocation_id != scope.invocation_id
                {
                    *retry = Some(authorized);
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                }
                let mut request = authorized.request;
                request.scope = scope.clone();
                request.input = PromptInput::Text(
                    "Continue the previous request after the temporary provider failure. Preserve the original instructions and complete only unfinished work.".into(),
                );
                request.recovered_inputs.clear();
                request.attachments.clear();
                request.permissions = callbacks.permissions;
                request.provider_tools = callbacks.provider_tools;
                request.turn_control = callbacks.turn_control;
                request.host_effects = callbacks.host_effects;
                request.tool_endpoint = callbacks.tool_endpoint;
                request.resource_lease = callbacks.resource_lease;
                request.turn_control.before_send(&scope).await?;
                let model = request.configuration.model.clone();
                let mut params = json!({
                    "threadId":session.binding.provider_session_id.as_ref().map(|id| id.as_str().to_owned()),
                    "input":super::attachments::input(input_text(&request.input), &[]),
                    "serviceTier":model.service_tier,
                    "config":{}
                });
                let rpc = session.rpc.lock().await;
                let effort =
                    apply_model_selection(&rpc, &mut params, &model, ProviderOperation::AdmitInput)
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
                        "model":model.model.clone(),
                        "reasoning_effort":effort.clone(),
                        "developer_instructions":instructions
                    }
                });
                let id = session
                    .rpc
                    .lock()
                    .await
                    .send_request("turn/start", params)
                    .await?;
                let result = await_response(&session, id, ProviderOperation::AdmitInput).await?;
                let turn_id = result
                    .get("turnId")
                    .or_else(|| result.pointer("/turn/id"))
                    .and_then(Value::as_str)
                    .map(str::to_owned)
                    .ok_or_else(|| {
                        failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol)
                    })?;
                let address = forge_provider_api::TurnAddress {
                    scope: scope.clone(),
                    thread_id: session
                        .binding
                        .provider_session_id
                        .as_ref()
                        .map(|id| id.as_str().to_owned())
                        .unwrap_or_default(),
                    turn_id: turn_id.clone().into(),
                };
                publish(
                    &session.sink,
                    &scope,
                    ProviderEvent::Execution {
                        observation: ExecutionObservation::new(
                            scope.clone(),
                            ExecutionUpdate::Started {
                                address: address.clone(),
                            },
                        )?,
                    },
                )
                .await?;
                let submissions = Arc::new(Mutex::new(Vec::new()));
                let (cancel, receiver) = mpsc::unbounded_channel();
                let (turn_updates, _) = watch::channel(Some(turn_id.clone()));
                let mut active = session.active.lock().await;
                *active = Some(ActiveExecution {
                    scope: scope.clone(),
                    turn_id,
                    turn_updates,
                    submissions: submissions.clone(),
                    request: request.clone(),
                    cancel,
                    task: None,
                });
                let session_task = session.clone();
                let task = tokio::spawn(async move {
                    observe_execution(
                        session_task,
                        request,
                        address,
                        submissions,
                        receiver,
                        Vec::new(),
                    )
                    .await
                });
                active
                    .as_mut()
                    .expect("continuation execution was inserted")
                    .task = Some(Arc::new(crate::retained_task::RetainedTask::new(task)));
                dispatch.confirm();
                Ok(ContinuationOutcome::ExecutionResumed)
            }
        }
    }
}

fn consume_not_consumed(
    retry: &mut Option<RetryAuthorization>,
    authorization: &forge_provider_api::ContinuationToken,
    supplied: &PromptRequest,
) -> Result<PromptRequest, ProviderError> {
    let Some(authorized) = retry.take() else {
        return Err(failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ));
    };
    if &authorized.token != authorization
        || !same_not_consumed_request(&authorized.request, supplied)
    {
        *retry = Some(authorized);
        return Err(failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ));
    }
    let mut request = authorized.request;
    request.dispatch = supplied.dispatch.clone();
    Ok(request)
}

fn same_not_consumed_request(authorized: &PromptRequest, supplied: &PromptRequest) -> bool {
    let same_input = match (&authorized.input, &supplied.input) {
        (PromptInput::Text(left), PromptInput::Text(right)) => left == right,
        (
            PromptInput::Skill {
                name: left_name,
                arguments: left_arguments,
            },
            PromptInput::Skill {
                name: right_name,
                arguments: right_arguments,
            },
        ) => left_name == right_name && left_arguments == right_arguments,
        (PromptInput::QuestionReply(left), PromptInput::QuestionReply(right)) => left == right,
        _ => false,
    };
    let same_attachments =
        |left: &[forge_provider_api::input::FileAttachment],
         right: &[forge_provider_api::input::FileAttachment]| {
            left.len() == right.len()
                && left.iter().zip(right).all(|(left, right)| {
                    left.path == right.path
                        && left.label == right.label
                        && left.media_type == right.media_type
                        && left.contents.as_ref() == right.contents.as_ref()
                        && left.context == right.context
                })
        };
    let same_recovered = authorized.recovered_inputs.len() == supplied.recovered_inputs.len()
        && authorized
            .recovered_inputs
            .iter()
            .zip(&supplied.recovered_inputs)
            .all(|(left, right)| {
                let same_input = match (&left.input, &right.input) {
                    (PromptInput::Text(left), PromptInput::Text(right)) => left == right,
                    (
                        PromptInput::Skill {
                            name: left_name,
                            arguments: left_arguments,
                        },
                        PromptInput::Skill {
                            name: right_name,
                            arguments: right_arguments,
                        },
                    ) => left_name == right_name && left_arguments == right_arguments,
                    (PromptInput::QuestionReply(left), PromptInput::QuestionReply(right)) => {
                        left == right
                    }
                    _ => false,
                };
                same_input
                    && left.instructions == right.instructions
                    && same_attachments(&left.attachments, &right.attachments)
            });
    let same_tools = authorized.tools.len() == supplied.tools.len()
        && authorized
            .tools
            .iter()
            .zip(&supplied.tools)
            .all(|(left, right)| {
                left.name == right.name
                    && left.description == right.description
                    && left.input_schema == right.input_schema
            });
    authorized.scope == supplied.scope
        && authorized.mode == supplied.mode
        && authorized.configuration.model == supplied.configuration.model
        && authorized.configuration.access == supplied.configuration.access
        && authorized.configuration.approval == supplied.configuration.approval
        && authorized.tool_endpoint == supplied.tool_endpoint
        && match (&authorized.resource_lease, &supplied.resource_lease) {
            (None, None) => true,
            (Some(left), Some(right)) => Arc::ptr_eq(left, right),
            _ => false,
        }
        && authorized.task == supplied.task
        && same_tools
        && authorized.goal == supplied.goal
        && authorized.submission_id == supplied.submission_id
        && same_input
        && same_recovered
        && authorized.instructions == supplied.instructions
        && same_attachments(&authorized.attachments, &supplied.attachments)
        && Arc::ptr_eq(&authorized.receipt, &supplied.receipt)
        && Arc::ptr_eq(&authorized.permissions, &supplied.permissions)
        && Arc::ptr_eq(&authorized.provider_tools, &supplied.provider_tools)
        && Arc::ptr_eq(&authorized.turn_control, &supplied.turn_control)
        && Arc::ptr_eq(&authorized.host_effects, &supplied.host_effects)
}

/// Collect terminal publication without cancelling a native turn that is still running.
async fn collect_settled_execution(session: &CodexBinding) -> Result<(), ProviderError> {
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
        let result = tokio::time::timeout(std::time::Duration::from_secs(30), task.join())
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::CompactSession,
                    ProviderFailureKind::Timeout,
                )
            })?;
        let mut active = session.active.lock().await;
        if active
            .as_ref()
            .is_some_and(|execution| execution.scope == scope)
        {
            active.take();
        }
        drop(active);
        result?;
    }
    Ok(())
}

/// Run native compaction while admission and notification consumption are exclusively owned.
async fn compact_session(
    session: &Arc<CodexBinding>,
    request: forge_provider_api::CompactionRequest,
) -> Result<forge_provider_api::Compaction, ProviderError> {
    let thread_id = session
        .binding
        .provider_session_id
        .as_ref()
        .map(|id| id.as_str().to_owned())
        .ok_or_else(|| {
            failure(
                ProviderOperation::CompactSession,
                ProviderFailureKind::SessionMissing,
            )
        })?;
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
            ProviderOperation::CompactSession,
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
                    ProviderOperation::CompactSession,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let mut completed = None;
        for event in output.event {
            if let ProviderEvent::Compaction { scope, update } = &event
                && scope == &request.scope
                && update.thread_id == thread_id
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

async fn collect_finished_execution(session: &CodexBinding) -> Result<(), ProviderError> {
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
        let result = task.join().await;
        let mut active = session.active.lock().await;
        if active
            .as_ref()
            .is_some_and(|execution| execution.scope == scope)
        {
            active.take();
        }
        drop(active);
        result?;
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
        let thread_id = session
            .binding
            .provider_session_id
            .as_ref()
            .map(|id| id.as_str().to_owned())
            .unwrap_or_default();
        if let Err(error) = session
            .rpc
            .lock()
            .await
            .request(
                "turn/interrupt",
                json!({
                    "threadId":thread_id,"turnId":turn_id
                }),
                ProviderOperation::CancelSession,
            )
            .await
        {
            errors.push(error);
        }
    }
    if let Err(error) = request.turn_control.close(&request.scope).await {
        errors.push(error);
    }
    if let Err(error) = request
        .host_effects
        .settle_foreground(&session.binding.id, &request.scope)
        .await
    {
        errors.push(host_effect_error(error));
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
    *session.fenced.lock().await = true;
    if let Some(execution) = session.active.lock().await.as_mut() {
        execution.turn_updates.send_replace(None);
    }
    session.sink.fail(events::EventDeliveryError::Observation(
        forge_provider_api::lifecycle::ObservationFailure::Disconnected,
    ));
    let mut errors = errors.into_iter();
    let primary = errors
        .next()
        .unwrap_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown));
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
        if *session.fenced.lock().await {
            let mut errors = vec![failure(
                ProviderOperation::CollectEventReader,
                ProviderFailureKind::Rejected,
            )];
            if let Err(error) = request.turn_control.close(&request.scope).await {
                errors.push(error);
            }
            if let Err(error) = request
                .host_effects
                .settle_foreground(&session.binding.id, &request.scope)
                .await
            {
                errors.push(host_effect_error(error));
            }
            return combine_errors(errors);
        }
        let native = if let Some(message) = initial_messages.next() {
            Ok(message)
        } else {
            tokio::select! {
                signal = cancel.recv(), if !cancelled => {
                    if signal.is_some() {
                        cancelled = true;
                        let mut cancel_errors = Vec::new();
                        if let Some(turn_id) = &active_parent_turn {
                            let params = json!({"threadId":address.thread_id,"turnId":turn_id});
                            if let Err(error) = session.rpc.lock().await.request("turn/interrupt", params, ProviderOperation::CancelSession).await {
                                cancel_errors.push(error);
                            }
                        }
                        for (thread_id, turn_id) in descendants.active_turns() {
                            if let Some(turn_id) = turn_id {
                                let params = json!({"threadId":thread_id,"turnId":turn_id});
                                if let Err(error) = session.rpc.lock().await.request("turn/interrupt", params, ProviderOperation::CancelSession).await {
                                    cancel_errors.push(error);
                                }
                            }
                        }
                        if let Err(error) = stop_background_commands(&session).await {
                            native_cleanup_failed = true;
                            cancel_errors.push(error);
                        }
                        if !cancel_errors.is_empty() {
                            native_cleanup_failed = true;
                            cleanup_errors.extend(cancel_errors);
                        }
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
        if let Some(state) = super::normalization::native_goal_state(&message, &address.thread_id) {
            *session.native_goal_status.lock().await = Some(state);
        }
        if message.get("method").is_some() && message.get("id").is_some() {
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
                        ProviderOperation::CollectEventReader,
                        ProviderFailureKind::Protocol,
                    ),
                    forge_provider_api::lifecycle::ObservationFailure::Protocol,
                )
                .await);
            }
            if let Err(error) = handle_native_request(&session, &request, &message).await {
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
                            ProviderOperation::CollectEventReader,
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
        normalization.bind(Some(request.scope.clone()));
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
                    ProviderOperation::CollectEventReader,
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
        let goal_active = *session.native_goal_status.lock().await
            == Some(forge_provider_api::completion::GoalState::Active);
        if descendants.active_count() == 0
            && !goal_active
            && active_parent_turn.is_none()
            && let Some(terminal) = parent_terminal.take()
        {
            break terminal;
        }
    };

    if let Some(execution) = session.active.lock().await.as_mut() {
        execution.turn_updates.send_replace(None);
    }

    let control_result = request.turn_control.close(&request.scope).await;
    let effects = request
        .host_effects
        .settle_foreground(&session.binding.id, &request.scope)
        .await;
    let covered_submissions = submissions.lock().await.clone();
    let completion = completion(&session, &request.configuration.model).await;
    let status = result.pointer("/status").and_then(Value::as_str);
    let mut status_failure = None;
    let (mut outcome, failure_detail) = match status {
        Some("completed") if !cancelled => (ExecutionResult::Completed { completion }, None),
        Some("interrupted" | "cancelled") | Some("completed") if cancelled => {
            (ExecutionResult::Cancelled { completion }, None)
        }
        Some("failed") if control_result.is_ok() && effects.is_ok() => {
            let native_error = result.get("error");
            let kind =
                native_error.map_or(ProviderFailureKind::Unknown, super::retry::failure_kind);
            let retry_category = native_error.and_then(crate::codex::retry::transient_category);
            let replay_sensitive = session.normalization.lock().await.replay_sensitive;
            let host_status = match request.turn_control.status(&request.scope).await {
                Ok(status) => Some(status),
                Err(error) => {
                    status_failure = Some(error);
                    None
                }
            };
            if let (Some(category), false, Some(host)) =
                (retry_category, replay_sensitive, host_status)
                && host.replay_safe
                && !host.control_handoff
                && !host.cancelled
            {
                let continuation =
                    continuation_token(&session.binding.id, &request.scope, &address.turn_id)
                        .expect(
                            "validated invocation identity produces a nonempty continuation token",
                        );
                *session.retry.lock().await = Some(RetryAuthorization {
                    token: continuation.clone(),
                    request: request.clone(),
                });
                (
                    ExecutionResult::Failed {
                        completion,
                        failure: ExecutionFailure::Retryable {
                            address: address.clone(),
                            category,
                            continuation,
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
                    ProviderOperation::CollectEventReader,
                    ProviderFailureKind::Protocol,
                ),
                forge_provider_api::lifecycle::ObservationFailure::Protocol,
            )
            .await);
        }
    };
    if cancelled
        && matches!(status, Some("interrupted" | "cancelled"))
        && !submissions.lock().await.is_empty()
        && !native_cleanup_failed
    {
        *session.rollback.lock().await = Some(RollbackAuthorization {
            thread_id: address.thread_id.clone(),
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
    let completed_at_ms = now_ms();
    let host_failure = if control_result.is_err() || status_failure.is_some() {
        Some(forge_provider_api::session::HostEffectFailure::Uncertain)
    } else {
        effects.err()
    };
    let update = match host_failure {
        Some(failure) => ExecutionUpdate::PublicationFailed {
            address: address.clone(),
            covered_submissions,
            completed_at_ms,
            result: outcome,
            failure,
        },
        None => ExecutionUpdate::Finished {
            address: address.clone(),
            covered_submissions,
            completed_at_ms,
            result: outcome,
        },
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
    if let Err(error) = control_result {
        errors.push(error);
    }
    if let Some(error) = status_failure {
        errors.push(error);
    }
    if let Err(error) = effects {
        errors.push(host_effect_error(error));
    }
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
                    ProviderOperation::CancelSession,
                )
                .await
        {
            errors.push(error);
        }
    }
    if let Err(error) = stop_background_commands(session).await {
        errors.push(error);
    }
    if let Err(error) = request.turn_control.close(&request.scope).await {
        errors.push(error);
    }
    if let Err(error) = request
        .host_effects
        .settle_foreground(&session.binding.id, &request.scope)
        .await
    {
        errors.push(host_effect_error(error));
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
    *session.fenced.lock().await = true;
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

fn host_effect_error(error: forge_provider_api::session::HostEffectFailure) -> ProviderError {
    let kind = match error {
        forge_provider_api::session::HostEffectFailure::Rejected => ProviderFailureKind::Rejected,
        forge_provider_api::session::HostEffectFailure::Uncertain
        | forge_provider_api::session::HostEffectFailure::Retired => ProviderFailureKind::Unknown,
    };
    failure(ProviderOperation::HostEffectPublication, kind)
}

async fn resume_native_goal(
    session: &CodexBinding,
    request: &PromptRequest,
    objective: &str,
) -> Result<
    (
        Option<String>,
        Vec<Value>,
        Option<forge_provider_api::completion::GoalState>,
    ),
    ProviderError,
> {
    let thread_id = session
        .binding
        .provider_session_id
        .as_ref()
        .map(|id| id.as_str().to_owned())
        .ok_or_else(|| {
            failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::SessionMissing,
            )
        })?;
    let rpc = session.rpc.lock().await;
    let request_id = rpc
        .send_request(
            "thread/goal/set",
            json!(
                {"threadId":thread_id,"objective":objective,"status":"active"}
            ),
        )
        .await?;
    let result = rpc
        .wait_response(request_id, ProviderOperation::AdmitInput)
        .await?;
    drop(rpc);
    let mut messages = Vec::new();
    let result_message = json!({
        "method":"thread/goal/updated",
        "params":{"threadId":thread_id,"goal":result.get("goal")}
    });
    if let Some(state) = super::normalization::native_goal_state(&result_message, &thread_id) {
        *session.native_goal_status.lock().await = Some(state);
        if state != forge_provider_api::completion::GoalState::Active {
            messages.push(result_message);
            return Ok((None, messages, Some(state)));
        }
    }
    *session.native_goal.lock().await = Some(objective.to_owned());
    *session.native_goal_status.lock().await =
        Some(forge_provider_api::completion::GoalState::Active);
    loop {
        let mut message = session.rpc.lock().await.receive().await?;
        session.mcp_context.lock().await.normalize(&mut message);
        let params = message.get("params").unwrap_or(&Value::Null);
        if message.get("method").is_some() && message.get("id").is_some() {
            if params.get("threadId").and_then(Value::as_str) == Some(thread_id.as_str()) {
                handle_native_request(session, request, &message).await?;
            }
            messages.push(message);
            continue;
        }
        if params.get("threadId").and_then(Value::as_str) == Some(thread_id.as_str())
            && message.get("method").and_then(Value::as_str) == Some("turn/started")
        {
            let turn_id = params
                .pointer("/turn/id")
                .and_then(Value::as_str)
                .or_else(|| params.get("turnId").and_then(Value::as_str))
                .map(str::to_owned)
                .ok_or_else(|| {
                    failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol)
                })?;
            messages.push(message);
            return Ok((Some(turn_id), messages, None));
        }
        if let Some(state) = super::normalization::native_goal_state(&message, &thread_id) {
            *session.native_goal_status.lock().await = Some(state);
            messages.push(message);
            if state != forge_provider_api::completion::GoalState::Active {
                return Ok((None, messages, Some(state)));
            }
            continue;
        }
        messages.push(message);
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
                Err(_error) => {
                    retained.sink.fail(events::EventDeliveryError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                    ));
                    return Err(failure(
                        ProviderOperation::CollectEventReader,
                        ProviderFailureKind::Protocol,
                    ));
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
                    retained.sink.fail(events::EventDeliveryError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::Protocol,
                    ));
                    return Err(error);
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
                            retained.sink.fail(events::EventDeliveryError::Observation(
                                forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                            ));
                            return Err(error);
                        }
                    }
                }
                Ok(None) => {
                    if message.get("method").and_then(Value::as_str) == Some("turn/started") {
                        retained.sink.fail(events::EventDeliveryError::Observation(
                            forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                        ));
                        return Err(failure(
                            ProviderOperation::CollectEventReader,
                            ProviderFailureKind::Protocol,
                        ));
                    }
                }
                Err(_error) => {
                    retained.sink.fail(events::EventDeliveryError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::Protocol,
                    ));
                    return Err(failure(
                        ProviderOperation::CollectEventReader,
                        ProviderFailureKind::Protocol,
                    ));
                }
            }
        }
        Ok(())
    });
    *slot = Some(BackgroundReader {
        cancel,
        task: crate::retained_task::RetainedTask::new(task),
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
                        ProviderOperation::CleanupExecution,
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
                    ProviderOperation::CleanupExecution,
                )
                .await?;
            if result.get("terminated").and_then(Value::as_bool).is_none() {
                return Err(failure(
                    ProviderOperation::CleanupExecution,
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
                ProviderOperation::CleanupExecution,
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
                ProviderOperation::CleanupExecution,
            )
            .await?;
        let data = page.get("data").and_then(Value::as_array).ok_or_else(|| {
            failure(
                ProviderOperation::CleanupExecution,
                ProviderFailureKind::Protocol,
            )
        })?;
        rows.extend(data.iter().cloned());
        let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
            break;
        };
        if !seen.insert(next.to_owned()) {
            return Err(failure(
                ProviderOperation::CleanupExecution,
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
    let id = message.get("id").ok_or_else(|| {
        failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Protocol,
        )
    })?;
    let method = message
        .get("method")
        .and_then(Value::as_str)
        .ok_or_else(|| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            )
        })?;
    let params = message.get("params").unwrap_or(&Value::Null);
    let response = match method {
        "item/tool/call" => {
            let call = super::decode::dynamic_tool_call(params).map_err(|_| {
                failure(
                    ProviderOperation::NativeTransport,
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
                failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol)
            })?;
            let decision = request
                .permissions
                .authorize(&request.scope, prepared.request.clone())
                .await;
            prepared.validate().map_err(|_| {
                failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected)
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
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            ));
        }
    };
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

async fn apply_model_selection(
    rpc: &NativeRpc,
    parameters: &mut Value,
    selection: &ModelSelection,
    operation: ProviderOperation,
) -> Result<Option<String>, ProviderError> {
    if selection.context_window.is_some() {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ContextWindowSelection,
        )
        .into());
    }
    parameters["serviceTier"] = json!(selection.service_tier);
    if let Some(model) = &selection.model {
        parameters["model"] = json!(model);
    }
    let reasoning = match &selection.reasoning {
        ReasoningSelection::Default => None,
        ReasoningSelection::Level(level) => Some(level.clone()),
        ReasoningSelection::Lowest => {
            let catalog = rpc.request("model/list", json!({}), operation).await?;
            let models = crate::codex::catalog::parse_model_catalog(&catalog);
            let model = selection
                .model
                .as_deref()
                .and_then(|id| models.iter().find(|model| model.id == id))
                .or_else(|| models.iter().find(|model| model.is_default));
            model.and_then(|model| model.reasoning.first().cloned())
        }
    };
    if let Some(reasoning) = &reasoning {
        if !parameters["config"].is_object() {
            parameters["config"] = json!({});
        }
        parameters["config"]["model_reasoning_effort"] = json!(reasoning);
    }
    Ok(reasoning)
}

async fn completion(session: &CodexBinding, model: &ModelSelection) -> CompletionSnapshot {
    let provider = crate::registry::descriptor(
        &crate::registry::select("codex").expect("Codex is registered"),
    )
    .expect("registered Codex descriptor");
    let evidence = forge_provider_api::completion::TurnEvidence {
        native_state: *session.native_goal_status.lock().await,
        ..Default::default()
    };
    CompletionSnapshot::from(ProviderCompletion {
        provider_session_id: session.binding.provider_session_id.clone(),
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
    let scoped = sink.bind_scope(event.scope()).map_err(|_| {
        failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Rejected,
        )
    })?;
    scoped.send_wait(event).await.map_err(ProviderError::from)
}

fn recovery_token(
    session: &CodexBinding,
    submission: &forge_provider_api::SubmissionId,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    let thread = session
        .binding
        .provider_session_id
        .as_ref()
        .map(|id| id.as_str().to_owned())
        .unwrap_or_default();
    recovery_token_parts(&thread, submission.as_str())
}

fn recovery_token_parts(
    thread: &str,
    submission_id: &str,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    let bytes = format!("codex-v1\0{thread}\0{submission_id}").into_bytes();
    forge_provider_api::RecoveryToken::new(bytes)
        .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown))
}

struct RecoveryEvidence {
    thread_id: String,
    submission_id: String,
}

fn parse_recovery_token(
    token: Option<&forge_provider_api::RecoveryToken>,
) -> Option<RecoveryEvidence> {
    let text = std::str::from_utf8(token?.as_bytes()).ok()?;
    let mut fields = text.split('\0');
    if fields.next()? != "codex-v1" {
        return None;
    }
    let thread_id = fields.next()?.to_owned();
    let submission_id = fields.next()?.to_owned();
    if thread_id.is_empty() || submission_id.is_empty() || fields.next().is_some() {
        return None;
    }
    Some(RecoveryEvidence {
        thread_id,
        submission_id,
    })
}

fn continuation_token(
    binding: &BindingId,
    scope: &forge_provider_api::InvocationScope,
    turn_id: &forge_provider_api::ProviderTurnId,
) -> Result<forge_provider_api::ContinuationToken, ProviderError> {
    let bytes = format!(
        "codex-retry-v1\0{}\0{}\0{}\0{}",
        binding.as_str(),
        scope.owner.runtime_key(),
        scope.invocation_id,
        turn_id,
    )
    .into_bytes();
    forge_provider_api::ContinuationToken::new(bytes)
        .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown))
}

fn not_consumed_token(
    binding: &BindingId,
    scope: &forge_provider_api::InvocationScope,
    submission_id: &str,
) -> Result<forge_provider_api::ContinuationToken, ProviderError> {
    let bytes = format!(
        "codex-not-consumed-v1\0{}\0{}\0{}\0{}",
        binding.as_str(),
        scope.owner.runtime_key(),
        scope.invocation_id,
        submission_id,
    )
    .into_bytes();
    forge_provider_api::ContinuationToken::new(bytes)
        .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown))
}

async fn await_response(
    session: &CodexBinding,
    request_id: u64,
    operation: ProviderOperation,
) -> Result<Value, ProviderError> {
    session
        .rpc
        .lock()
        .await
        .wait_response(request_id, operation)
        .await
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

fn now_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| i64::try_from(duration.as_millis()).unwrap_or(i64::MAX))
        .unwrap_or_default()
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
            forge_provider_api::EventScope::SessionOperation {
                owner: owner.clone(),
                operation_id: "binding".into(),
            },
        );
        let session = Arc::new(CodexBinding {
            binding: binding.clone(),
            owner: owner.clone(),
            workspace: String::new(),
            model: Mutex::new(ModelSelection::default()),
            runtime: Mutex::new(provider.runtime.clone()),
            rpc: RpcHandle::new(NativeRpc::new(connection)),
            sink,
            stream: Mutex::new(None),
            normalization: Mutex::new(CodexNormalization::default()),
            mcp_context: Mutex::new(super::super::protocol::McpContext::default()),
            resources: Mutex::new(CodexResources::new(Default::default(), PathBuf::new())),
            profile: Mutex::new(None),
            _lease: None,
            system_instructions: String::new(),
            native_goal: Mutex::new(None),
            native_goal_status: Mutex::new(None),
            access: Mutex::new(AccessPolicy::default()),
            approval: Mutex::new(ApprovalMode::Ask),
            approval_context: super::super::approval::ApprovalContext::default(),
            active: Mutex::new(None),
            background_reader: Mutex::new(None),
            retry: Mutex::new(None),
            rollback: Mutex::new(None),
            admission: Mutex::new(()),
            observations: Arc::new(tokio::sync::RwLock::new(false)),
            fenced: Mutex::new(false),
        });
        provider
            .bindings
            .lock()
            .await
            .insert(binding.id.clone(), session.clone());
        provider.owners.lock().await.insert(owner, binding.id);
        (provider, session, stream, socket)
    }

    /// Completed generation remains compactable even before its terminal publisher has returned.
    #[tokio::test]
    async fn compaction_collects_terminal_worker_and_preserves_native_failure_category() {
        use futures_util::{SinkExt, StreamExt};
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (provider, session, _events, mut socket) = compaction_fixture().await;
            let (release, released) = tokio::sync::oneshot::channel();
            let task = Arc::new(crate::retained_task::RetainedTask::new(tokio::spawn(async move {
                released.await.unwrap(); Ok(())
            })));
            let (cancel, mut cancelled) = mpsc::unbounded_channel();
            let request = request();
            *session.active.lock().await = Some(ActiveExecution {
                scope: request.scope.clone(), turn_id: "finished-turn".into(),
                turn_updates: watch::channel(None).0, submissions: Arc::new(Mutex::new(Vec::new())),
                request, cancel, task: Some(task),
            });
            let compact = || forge_provider_api::CompactionRequest {
                operation_id: "compact".into(), scope: forge_provider_api::EventScope::SessionOperation {
                    owner: session.owner.clone(), operation_id: "compact".into(),
                },
            };
            let mut pending = Box::pin(provider.compact(&session.binding.id, compact()));
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
            assert_eq!(pending.await.unwrap().state, forge_provider_api::CompactionState::Completed);
            assert!(session.active.lock().await.is_none());
            let error = provider.compact(&session.binding.id, compact()).await.unwrap_err();
            assert!(matches!(&error, ProviderError::Failure(failure)
                if failure.operation() == ProviderOperation::CompactSession && failure.kind() == ProviderFailureKind::Rejected));
            assert!(!error.to_string().contains("private native rejection"));
            session.rpc.lock().await.close().await.unwrap();
            wire.await.unwrap();
        }).await.unwrap();
    }

    /// Compaction cannot collect, cancel, or replace a genuinely running generation.
    #[tokio::test]
    async fn compaction_rejects_active_turn_without_cancellation() {
        let (provider, session, _events, _socket) = compaction_fixture().await;
        let (release, released) = tokio::sync::oneshot::channel();
        let task = Arc::new(crate::retained_task::RetainedTask::new(tokio::spawn(
            async move {
                released.await.unwrap();
                Ok(())
            },
        )));
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
        let error = provider
            .compact(
                &session.binding.id,
                forge_provider_api::CompactionRequest {
                    operation_id: "compact".into(),
                    scope: forge_provider_api::EventScope::SessionOperation {
                        owner: session.owner.clone(),
                        operation_id: "compact".into(),
                    },
                },
            )
            .await
            .unwrap_err();
        assert!(matches!(error, ProviderError::Failure(failure)
            if failure.operation() == ProviderOperation::CompactSession && failure.kind() == ProviderFailureKind::Rejected));
        assert!(session.active.lock().await.is_some());
        assert!(cancelled.try_recv().is_err());
        release.send(()).unwrap();
        task.join().await.unwrap();
        session.rpc.lock().await.close().await.unwrap();
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
                ProviderOperation::CollectNativeRuntime,
                ProviderFailureKind::Transport,
            ))
        })));

        assert!(provider.shutdown().await.is_err());
        assert!(provider.catalog.lock().await.is_none());
        let error = match provider.runtime.connect().await {
            Ok(_) => panic!("shutdown runtime admitted another connection"),
            Err(error) => error,
        };
        assert_eq!(error.to_string(), "shared Codex runtime is shut down");
        provider.shutdown().await.unwrap();
    }

    struct FixtureAuthority;

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
                ProviderOperation::AdmitInput,
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
            goal: None,
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

    #[test]
    fn changed_not_consumed_payload_is_rejected_without_consuming_authorization() {
        let original = request();
        let token = forge_provider_api::ContinuationToken::new(vec![7, 3, 1]).unwrap();
        let mut retry = Some(RetryAuthorization {
            token: token.clone(),
            request: original.clone(),
        });
        let mut changed = original.clone();
        changed.input = PromptInput::Text("substituted request".into());
        assert!(consume_not_consumed(&mut retry, &token, &changed).is_err());
        assert!(
            retry.is_some(),
            "invalid payload consumed the valid continuation authorization"
        );
        changed = original.clone();
        changed.tool_endpoint = Some(forge_provider_api::session::HostToolEndpoint {
            url: "http://replacement.invalid".into(),
            headers: Default::default(),
        });
        assert!(consume_not_consumed(&mut retry, &token, &changed).is_err());
        assert!(
            retry.is_some(),
            "endpoint mismatch consumed valid authorization"
        );
        let recovered = consume_not_consumed(&mut retry, &token, &original).unwrap();
        assert!(same_not_consumed_request(&original, &recovered));
        assert!(
            retry.is_none(),
            "valid continuation authorization was reusable"
        );
    }
}
