use super::handle::CopilotSession;
use super::runtime::CopilotRuntime;
use super::sessions::{CopilotSessions, PreparedSession, SessionReuse, SessionSelection};
use super::turn::CopilotTurnEvent;
use async_trait::async_trait;
use forge_provider_api::control::{ProviderTools, ToolCall};
use forge_provider_api::events::{self, ProviderEventSink, ProviderEventStream};
use forge_provider_api::generation::ReasoningSelection;
use forge_provider_api::permissions::{PermissionRequest, PermissionResponse, ProviderPermissions};
use forge_provider_api::provider::Provider as _;
use forge_provider_api::session::{
    BindingId, ContinuationOutcome, ContinuationRequest, InputAcceptance, ModelSelection,
    PromptRequest, RecoveryToken, ResourceLease, SessionBinding, SessionMode, SessionRequest,
    SteeringAcceptance, SteeringRequest,
};
use forge_provider_api::turn_control::{HostTurnStatus, TurnControl};
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation, SubmissionEvidence,
};
use futures_util::{StreamExt, stream};
use serde_json::{Map, Value, json};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{
    Arc,
    atomic::{AtomicU64, Ordering},
};
use tokio::sync::{Mutex, RwLock};

use super::prompt::NativeRecoveryEvidence;

struct RetainedResources {
    _profile: crate::profile::ResourceProfile,
    _lease: Option<Arc<dyn ResourceLease>>,
}

#[derive(Clone)]
struct InvocationCallbacks {
    scope: forge_provider_api::InvocationScope,
    generation: u64,
    permissions: Arc<dyn ProviderPermissions>,
    tools: Arc<dyn ProviderTools>,
    control: Arc<dyn TurnControl>,
    effects: Arc<dyn forge_provider_api::ProviderHostEffects>,
    allowed_tools: HashSet<String>,
    session_directory: Option<super::session_directory::SessionDirectory>,
}

impl InvocationCallbacks {
    fn authorize_tool(&self, call: &ToolCall) -> Result<(), ProviderError> {
        if self.allowed_tools.contains(&call.name) {
            return Ok(());
        }
        Err(failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Authorization,
        ))
    }
}

enum CallbackPhase {
    Foreground,
    Settled,
}

struct CallbackRegistration {
    callbacks: InvocationCallbacks,
    phase: CallbackPhase,
}

#[derive(Default)]
struct CallbackRouter {
    active: RwLock<HashMap<forge_provider_api::InvocationScope, CallbackRegistration>>,
    native_calls: RwLock<HashMap<String, InvocationCallbacks>>,
    mcp_tools: RwLock<HashMap<String, HashSet<String>>>,
    next_generation: AtomicU64,
}

impl CallbackRouter {
    /// Retain canonical tool names independently of the SDK's generated callback aliases.
    async fn register_mcp_tools(
        &self,
        session: &CopilotSession,
        resources: &forge_provider_api::ResourceSelection,
    ) {
        let servers = resources
            .mcp
            .iter()
            .map(|server| server.name.clone())
            .collect::<Vec<_>>();
        let native = session.inner.clone();
        let catalog = stream::iter(servers)
            .map(move |server| {
                let native = native.clone();
                async move {
                    let tools = tokio::time::timeout(
                        std::time::Duration::from_secs(5),
                        native.rpc().mcp().list_tools(
                            github_copilot_sdk::rpc::McpListToolsRequest {
                                server_name: server.clone(),
                            },
                        ),
                    )
                    .await
                    .ok()?
                    .ok()?;
                    Some((
                        server,
                        tools.tools.into_iter().map(|tool| tool.name).collect(),
                    ))
                }
            })
            .buffer_unordered(4)
            .filter_map(futures_util::future::ready)
            .collect()
            .await;
        *self.mcp_tools.write().await = catalog;
    }

    /// Resolve only one catalog-backed canonical identity; reject unknown or ambiguous aliases.
    async fn canonical_mcp_target(&self, target: &str) -> Option<String> {
        let (server, requested) = target.split_once('/')?;
        let catalog = self.mcp_tools.read().await;
        let mut matching = catalog
            .get(server)?
            .iter()
            .filter(|tool| requested == tool.as_str() || requested == format!("{server}-{tool}"));
        let canonical = matching.next()?;
        if matching.next().is_some() {
            return None;
        }
        Some(format!("{server}/{canonical}"))
    }

    async fn install(&self, mut callbacks: InvocationCallbacks) -> Result<(), ProviderError> {
        let mut active = self.active.write().await;
        callbacks.generation = self.next_generation.fetch_add(1, Ordering::Relaxed);
        active.insert(
            callbacks.scope.clone(),
            CallbackRegistration {
                callbacks,
                phase: CallbackPhase::Foreground,
            },
        );
        Ok(())
    }

    async fn callbacks(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<InvocationCallbacks, ProviderError> {
        self.active
            .read()
            .await
            .get(scope)
            .map(|registration| registration.callbacks.clone())
            .ok_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))
    }

    async fn retire_all(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let _ = binding;
        self.active.write().await.clear();
        self.native_calls.write().await.clear();
        Ok(())
    }

    async fn bind_native_call(&self, call_id: &str, scope: &forge_provider_api::InvocationScope) {
        if let Ok(callbacks) = self.callbacks(scope).await {
            self.native_calls
                .write()
                .await
                .entry(call_id.to_owned())
                .or_insert(callbacks);
        }
    }

    async fn prune_settled_scope(
        &self,
        scope: &forge_provider_api::InvocationScope,
        generation: u64,
    ) {
        let still_owned = self
            .native_calls
            .read()
            .await
            .values()
            .any(|active| active.scope == *scope && active.generation == generation);
        if !still_owned {
            let mut active = self.active.write().await;
            if active.get(scope).is_some_and(|registration| {
                registration.callbacks.generation == generation
                    && matches!(registration.phase, CallbackPhase::Settled)
            }) {
                active.remove(scope);
            }
        }
    }

    /// Release foreground authority only after the prompt or its host settlement finishes.
    async fn finish_foreground_scope(
        &self,
        scope: &forge_provider_api::InvocationScope,
        generation: u64,
    ) {
        {
            let mut active = self.active.write().await;
            if let Some(registration) = active.get_mut(scope)
                && registration.callbacks.generation == generation
            {
                registration.phase = CallbackPhase::Settled;
            }
        }
        self.prune_settled_scope(scope, generation).await;
    }

    async fn callbacks_for_native_call(
        &self,
        call_id: &str,
    ) -> Result<InvocationCallbacks, ProviderError> {
        self.native_calls
            .read()
            .await
            .get(call_id)
            .cloned()
            .ok_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))
    }
}

#[async_trait]
impl ProviderPermissions for CallbackRouter {
    async fn authorize(
        &self,
        scope: &forge_provider_api::InvocationScope,
        request: PermissionRequest,
    ) -> PermissionResponse {
        match self.callbacks(scope).await {
            Ok(callbacks)
                if callbacks
                    .session_directory
                    .as_ref()
                    .is_some_and(|directory| directory.approves(&request)) =>
            {
                callbacks
                    .permissions
                    .authorize_session_storage(scope, request)
                    .await
            }
            Ok(callbacks) => callbacks.permissions.authorize(scope, request).await,
            Err(_) => PermissionResponse::Cancelled,
        }
    }

    async fn before_tool(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        self.callbacks(scope)
            .await?
            .permissions
            .before_tool(scope)
            .await
    }
}

#[async_trait]
impl ProviderTools for CallbackRouter {
    async fn call(
        &self,
        scope: &forge_provider_api::InvocationScope,
        call: ToolCall,
    ) -> Result<String, ProviderError> {
        let callbacks = self.callbacks(scope).await?;
        callbacks.authorize_tool(&call)?;
        callbacks.permissions.before_tool(scope).await?;
        callbacks.tools.call(scope, call).await
    }
}

#[async_trait]
impl TurnControl for CallbackRouter {
    async fn before_send(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        self.callbacks(scope)
            .await?
            .control
            .before_send(scope)
            .await
    }
    async fn receive(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        self.callbacks(scope).await?.control.receive(scope).await
    }
    async fn close(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        self.callbacks(scope).await?.control.close(scope).await
    }
    async fn status(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<HostTurnStatus, ProviderError> {
        self.callbacks(scope).await?.control.status(scope).await
    }
    fn tool_finished(
        &self,
        scope: &forge_provider_api::InvocationScope,
        name: Option<&str>,
    ) -> bool {
        self.active
            .try_read()
            .ok()
            .and_then(|active| {
                active
                    .get(scope)
                    .map(|registration| registration.callbacks.control.tool_finished(scope, name))
            })
            .unwrap_or(false)
    }
}

#[async_trait]
impl forge_provider_api::ProviderHostEffects for CallbackRouter {
    async fn settle_foreground(
        &self,
        binding: &BindingId,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), forge_provider_api::session::HostEffectFailure> {
        let callbacks = self
            .callbacks(scope)
            .await
            .map_err(|_| forge_provider_api::session::HostEffectFailure::Retired)?;
        callbacks.effects.settle_foreground(binding, scope).await?;
        self.finish_foreground_scope(scope, callbacks.generation)
            .await;
        Ok(())
    }
}

#[async_trait]
impl NativeCallbackRouter for CallbackRouter {
    async fn call_native(&self, call_id: &str, call: ToolCall) -> Result<String, ProviderError> {
        let callbacks = self.callbacks_for_native_call(call_id).await?;
        callbacks.authorize_tool(&call)?;
        callbacks.permissions.before_tool(&callbacks.scope).await?;
        callbacks.tools.call(&callbacks.scope, call).await
    }

    async fn authorize_native(
        &self,
        call_id: &str,
        mut request: PermissionRequest,
    ) -> PermissionResponse {
        for target in &mut request.target_list {
            if let forge_provider_api::permissions::PermissionTarget::Mcp { target } = target {
                let Some(canonical) = self.canonical_mcp_target(target).await else {
                    return PermissionResponse::Deny {
                        reason: Some("MCP tool identity is unavailable or ambiguous.".into()),
                    };
                };
                *target = canonical;
            }
        }
        match self.callbacks_for_native_call(call_id).await {
            Ok(callbacks)
                if callbacks
                    .session_directory
                    .as_ref()
                    .is_some_and(|directory| directory.approves(&request)) =>
            {
                callbacks
                    .permissions
                    .authorize_session_storage(&callbacks.scope, request)
                    .await
            }
            Ok(callbacks) => {
                callbacks
                    .permissions
                    .authorize(&callbacks.scope, request)
                    .await
            }
            Err(_) => PermissionResponse::Cancelled,
        }
    }

    async fn before_tool_native(&self, call_id: Option<&str>) -> Result<(), ProviderError> {
        let callbacks = match call_id {
            Some(call_id) => vec![self.callbacks_for_native_call(call_id).await?],
            // The SDK's pre-tool hook has no call identity. Fence all retained invocation
            // barriers for this binding instead of attributing the tool to a guessed owner.
            None => self
                .active
                .read()
                .await
                .values()
                .map(|registration| registration.callbacks.clone())
                .collect(),
        };
        if callbacks.is_empty() {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        for callbacks in callbacks {
            callbacks.permissions.before_tool(&callbacks.scope).await?;
        }
        Ok(())
    }

    async fn bind_native_call(&self, call_id: &str, scope: &forge_provider_api::InvocationScope) {
        CallbackRouter::bind_native_call(self, call_id, scope).await;
    }

    async fn release_native_call(&self, call_id: &str) {
        let callbacks = self.native_calls.write().await.remove(call_id);
        if let Some(callbacks) = callbacks {
            self.prune_settled_scope(&callbacks.scope, callbacks.generation)
                .await;
        }
    }
}

struct BindingState {
    binding: SessionBinding,
    owner: forge_provider_api::ExecutionOwner,
    runtime_key: String,
    session: RwLock<Arc<CopilotSession>>,
    request: Mutex<SessionRequest>,
    sink: ProviderEventSink,
    stream: Mutex<Option<ProviderEventStream>>,
    turn: Arc<CopilotTurnEvent>,
    callbacks: Arc<CallbackRouter>,
    admission: Mutex<()>,
    active: Mutex<Option<tokio::task::JoinHandle<Result<(), ProviderError>>>>,
    continuations: Mutex<HashMap<Vec<u8>, ContinuationGrant>>,
}

impl BindingState {
    /// Join native execution while retaining ownership if the waiter is cancelled or times out.
    async fn collect_execution(&self) -> Result<Option<Result<(), ProviderError>>, ProviderError> {
        let mut active = self.active.lock().await;
        let Some(task) = active.as_mut() else {
            return Ok(None);
        };
        let result = tokio::time::timeout(std::time::Duration::from_secs(30), task)
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::CleanupExecution,
                    ProviderFailureKind::Timeout,
                )
            })?;
        active.take();
        result.map(Some).map_err(|_| {
            failure(
                ProviderOperation::CleanupExecution,
                ProviderFailureKind::Runtime,
            )
        })
    }
}

#[derive(Clone)]
enum ContinuationGrant {
    NotConsumed {
        request: Box<PromptRequest>,
    },
    Retryable {
        scope: forge_provider_api::InvocationScope,
        mode: forge_provider_api::PromptMode,
        allowed_tools: HashSet<String>,
    },
}

#[async_trait]
pub(super) trait NativeCallbackRouter: Send + Sync {
    async fn call_native(&self, call_id: &str, call: ToolCall) -> Result<String, ProviderError>;
    async fn authorize_native(
        &self,
        call_id: &str,
        request: PermissionRequest,
    ) -> PermissionResponse;
    async fn before_tool_native(&self, call_id: Option<&str>) -> Result<(), ProviderError>;
    async fn bind_native_call(&self, call_id: &str, scope: &forge_provider_api::InvocationScope);
    async fn release_native_call(&self, call_id: &str);
}

pub(super) struct NoopNativeCallbacks;

#[async_trait]
impl NativeCallbackRouter for NoopNativeCallbacks {
    async fn call_native(&self, _: &str, _: ToolCall) -> Result<String, ProviderError> {
        Err(failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn authorize_native(&self, _: &str, _: PermissionRequest) -> PermissionResponse {
        PermissionResponse::Cancelled
    }
    async fn before_tool_native(&self, _: Option<&str>) -> Result<(), ProviderError> {
        Err(failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ))
    }
    async fn bind_native_call(&self, _: &str, _: &forge_provider_api::InvocationScope) {}
    async fn release_native_call(&self, _: &str) {}
}

/// Own Copilot SDK sessions, callback routers, and normalized event streams.
pub struct CopilotProvider {
    runtime: Arc<CopilotRuntime>,
    sessions: Arc<CopilotSessions<RetainedResources>>,
    runtime_root: PathBuf,
    bindings: Mutex<HashMap<BindingId, Arc<BindingState>>>,
    owners: Mutex<HashMap<forge_provider_api::ExecutionOwner, BindingId>>,
    initialization_lock: Mutex<()>,
}

impl CopilotProvider {
    /// Create the native owner with its private runtime resource root.
    pub fn new(command: Vec<String>, runtime_root: PathBuf) -> Result<Self, ProviderError> {
        let trace: Arc<dyn forge_provider_api::trace::ProviderTrace> = Arc::new(NullTrace);
        Ok(Self {
            runtime: Arc::new(CopilotRuntime::new(command)),
            sessions: Arc::new(CopilotSessions::new(trace)),
            runtime_root,
            bindings: Mutex::new(HashMap::new()),
            owners: Mutex::new(HashMap::new()),
            initialization_lock: Mutex::new(()),
        })
    }

    async fn binding(&self, id: &BindingId) -> Result<Arc<BindingState>, ProviderError> {
        self.bindings.lock().await.get(id).cloned().ok_or_else(|| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::SessionMissing,
            )
        })
    }

    async fn request_binding(
        &self,
        request: &forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<Arc<BindingState>, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            )
        })?;
        let binding = self
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
        self.binding(&binding).await
    }

    /// Collect a failed shared process before permitting replacement sessions.
    async fn collect_failed_runtime(&self) -> Result<(), ProviderError> {
        let Some(failed) = self.runtime.failed_generation().await else {
            return Ok(());
        };
        self.sessions.fail_generation(&failed).await?;
        let retained = self
            .bindings
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        let mut bindings = Vec::new();
        for state in retained {
            if state.session.read().await.belongs_to(&failed) {
                state
                    .sink
                    .fail(events::EventDeliveryError::Observation(failed.failure()));
                state.callbacks.retire_all(&state.binding.id).await?;
                bindings.push(state);
            }
        }
        let client = self.runtime.collect_failed_client(&failed).await?;
        let sessions = self.sessions.collect_failed_generation(&client).await?;
        for state in &bindings {
            // The observer already published terminal failure; only collection itself can block recovery.
            let _terminal = state.collect_execution().await?;
        }
        self.runtime.finish_recovery(sessions).await?;
        for state in bindings {
            self.bindings.lock().await.remove(&state.binding.id);
            self.owners.lock().await.remove(&state.owner);
        }
        Ok(())
    }
}

struct NullTrace;
impl forge_provider_api::trace::ProviderTrace for NullTrace {
    fn record(&self, _: forge_provider_api::trace::ProviderTraceRecord) {}
}

#[async_trait]
impl forge_provider_api::provider::Provider for CopilotProvider {
    async fn initialize_session(
        &self,
        request: SessionRequest,
    ) -> Result<SessionBinding, ProviderError> {
        let _initialization = self.initialization_lock.lock().await;
        if request.owner.session_id.as_str().is_empty() || request.workspace.trim().is_empty() {
            return Err(failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            ));
        }
        self.descriptor()
            .capability
            .validate_access(&request.access)
            .map_err(|_| {
                failure(
                    ProviderOperation::InitializeSession,
                    ProviderFailureKind::Rejected,
                )
            })?;
        self.collect_failed_runtime().await?;
        if self.owners.lock().await.contains_key(&request.owner) {
            return Err(failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            ));
        }
        let runtime_key = request.owner.runtime_key();
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
        self.sessions
            .subscribe(runtime_key.clone(), sink.clone())
            .await;
        let callbacks = Arc::new(CallbackRouter::default());
        let client_runtime = self.runtime.clone();
        let workspace = request.workspace.clone();
        let model_selection = request.model.clone();
        let capability = self.descriptor().capability;
        let client = async move {
            let client = client_runtime.client(&workspace).await?;
            let models = client.models().await?;
            let (_, reasoning) = resolve_model_selection(&model_selection, &models, &capability)?;
            Ok((client, reasoning))
        };
        let profile = Arc::new(Mutex::new(Some(make_profile(
            &self.runtime_root,
            &request,
        )?)));
        let worker_profile = profile.clone();
        let skills = request.resources.skills.clone();
        let mcp = request
            .resources
            .mcp
            .iter()
            .map(mcp_config)
            .collect::<Vec<_>>();
        let profile_path = profile
            .lock()
            .await
            .as_ref()
            .expect("profile initialized")
            ._profile
            .path()
            .to_owned();
        let selected_mcp = request
            .resources
            .mcp
            .iter()
            .map(|resource| resource.name.clone())
            .collect::<Vec<_>>();
        let tools = request.tools.clone();
        let mode = session_mode(request.mode);
        let options = super::session::SessionOptions {
            session_id: request
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned()),
            model: request.model.model.clone(),
            reasoning_effort: None,
            context_tier: request.model.context_window.clone(),
            workspace: PathBuf::from(&request.workspace),
            mode,
            system_message: request.system_instructions.clone(),
            resources: match request.mode {
                SessionMode::Background => super::session::ResourceDiscovery::Native,
                SessionMode::Interactive | SessionMode::Child => {
                    super::session::ResourceDiscovery::Managed {
                        skill_directories: vec![profile_path.join("skills")],
                        selected_mcp,
                    }
                }
            },
            excluded_tools: Vec::new(),
            tools: tools.clone(),
            native_callbacks: Some(callbacks.clone()),
        };
        let permissions: Arc<dyn ProviderPermissions> = callbacks.clone();
        let provider_tools: Arc<dyn ProviderTools> = callbacks.clone();
        let selection = SessionSelection {
            execution_owner: request.owner.clone(),
            runtime_key: runtime_key.clone(),
            correlation_id: request.owner.session_id.to_string(),
            session_id: request
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned()),
            model: request.model.model.clone(),
            context_tier: request.model.context_window.clone(),
            tools: Some(tools),
            reuse: SessionReuse::Compatible,
        };
        let session = self
            .sessions
            .acquire(
                client,
                selection,
                permissions,
                provider_tools,
                move || async move {
                    let retained = worker_profile
                        .lock()
                        .await
                        .take()
                        .expect("resource profile prepared once");
                    Ok(PreparedSession {
                        options,
                        skills: Some(
                            skills
                                .iter()
                                .map(|skill| super::handle::SkillSelection {
                                    name: skill.name.clone(),
                                    enabled: skill.enabled,
                                })
                                .collect(),
                        ),
                        mcp: Some(mcp),
                        profile: Some(retained),
                    })
                },
                |_| Ok(()),
            )
            .await
            .map_err(|error| {
                error.downcast::<ProviderError>().unwrap_or_else(|_| {
                    failure(
                        ProviderOperation::InitializeSession,
                        ProviderFailureKind::Initialization,
                    )
                })
            })?;
        callbacks
            .register_mcp_tools(&session, &request.resources)
            .await;
        let turn = self.sessions.turn(&runtime_key).await;
        let binding = SessionBinding {
            id: binding_id.clone(),
            provider_session_id: Some(session.id().to_string().try_into().map_err(|_| {
                failure(
                    ProviderOperation::InitializeSession,
                    ProviderFailureKind::Protocol,
                )
            })?),
        };
        let state = Arc::new(BindingState {
            binding: binding.clone(),
            owner: request.owner.clone(),
            runtime_key: runtime_key.clone(),
            session: RwLock::new(session),
            request: Mutex::new(request.clone()),
            sink,
            stream: Mutex::new(Some(stream)),
            turn,
            callbacks,
            admission: Mutex::new(()),
            active: Mutex::new(None),
            continuations: Mutex::new(HashMap::new()),
        });
        self.bindings.lock().await.insert(binding_id.clone(), state);
        self.owners.lock().await.insert(request.owner, binding_id);
        Ok(binding)
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
        self.submit(binding, request, false).await
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
                let state = self.binding(binding).await?;
                let authorized = {
                    let mut grants = state.continuations.lock().await;
                    match grants.get(authorization.as_bytes()) {
                        Some(ContinuationGrant::NotConsumed {
                            request: authorized,
                        }) if same_prompt_request(authorized, &request) => {}
                        _ => {
                            return Err(failure(
                                ProviderOperation::AdmitInput,
                                ProviderFailureKind::Rejected,
                            ));
                        }
                    }
                    match grants.remove(authorization.as_bytes()) {
                        Some(ContinuationGrant::NotConsumed { request }) => request,
                        _ => unreachable!("validated continuation stays locked until consumption"),
                    }
                };
                Ok(ContinuationOutcome::InputAccepted(
                    self.submit(binding, *authorized, true).await?,
                ))
            }
            ContinuationRequest::RetryableExecution {
                scope,
                authorization,
                callbacks,
            } => {
                let callbacks = *callbacks;
                let state = self.binding(binding).await?;
                let grant = {
                    let mut grants = state.continuations.lock().await;
                    match grants.get(authorization.as_bytes()) {
                        Some(ContinuationGrant::Retryable {
                            scope: previous_scope,
                            ..
                        }) if scope == *previous_scope && scope.owner == state.owner => {}
                        _ => {
                            return Err(failure(
                                ProviderOperation::AdmitInput,
                                ProviderFailureKind::Rejected,
                            ));
                        }
                    }
                    grants.remove(authorization.as_bytes())
                };
                match grant {
                    Some(ContinuationGrant::Retryable {
                        scope: previous_scope,
                        mode,
                        allowed_tools,
                    }) => {
                        if scope != previous_scope || scope.owner != state.owner {
                            return Err(failure(
                                ProviderOperation::AdmitInput,
                                ProviderFailureKind::Rejected,
                            ));
                        }
                        self.resume_retained_execution(
                            binding,
                            &state,
                            scope,
                            callbacks,
                            mode,
                            allowed_tools,
                        )
                        .await?;
                        Ok(ContinuationOutcome::ExecutionResumed)
                    }
                    _ => Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    )),
                }
            }
        }
    }

    async fn session_steer(
        &self,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let state = self.binding(binding).await?;
        let submission_id = request.delivery.submission_id().cloned();
        let receipt = request.delivery.receipt().cloned();
        if request.scope.owner != state.owner
            || receipt
                .as_ref()
                .is_some_and(|receipt| submission_id.as_ref().is_none_or(|id| receipt.id() != id))
        {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        if request.goal.is_some() {
            return Err(unsupported_goal());
        }
        let callbacks = state.callbacks.callbacks(&request.scope).await?;
        let input = forge_provider_api::input::MessageInput {
            prompt: {
                let prompt = match request.input {
                    forge_provider_api::PromptInput::Text(text) => text,
                    forge_provider_api::PromptInput::Skill { name, arguments } => {
                        format!("Use skill {name} with arguments: {arguments}")
                    }
                    forge_provider_api::PromptInput::QuestionReply(reply) => {
                        let answer = match reply.response {
                            forge_provider_api::input::QuestionReplyResponse::Answer {
                                answer,
                                ..
                            }
                            | forge_provider_api::input::QuestionReplyResponse::Correction {
                                answer,
                                ..
                            } => answer,
                            forge_provider_api::input::QuestionReplyResponse::Clarification {
                                text,
                                ..
                            } => text,
                        };
                        format!("Answer to the question {:?}: {answer}", reply.question)
                    }
                };
                match request.task {
                    Some(task) => format!("{}\n\n{prompt}", task.heading()),
                    None => prompt,
                }
            },
            attachments: request.attachments,
        };
        callbacks.control.before_send(&request.scope).await?;
        let session = state.session.read().await.clone();
        let workspace = session
            .workspace_path()
            .unwrap_or(Path::new("."))
            .to_string_lossy()
            .into_owned();
        state
            .turn
            .steer(submission_id.clone(), async {
                session
                    .steer(input, receipt, &request.scope, &workspace)
                    .await
                    .map_err(anyhow::Error::new)
            })
            .await
            .map_err(|_| ProviderError::DeliveryUncertain)?;
        Ok(match submission_id {
            Some(submission_id) => SteeringAcceptance::Journaled { submission_id },
            None => SteeringAcceptance::Context,
        })
    }

    async fn retire_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let state = self.binding(binding).await?;
        let cancellation = self.cancel_session(binding).await.err();
        let callback_error = state.callbacks.retire_all(binding).await.err();
        let collection = self.sessions.retire(&state.runtime_key).await.map_err(|_| {
            failure(
                ProviderOperation::RetireSession,
                ProviderFailureKind::Transport,
            )
        });
        if collection.is_ok() {
            self.bindings.lock().await.remove(binding);
            self.owners.lock().await.remove(&state.owner);
        }
        let mut failures = Vec::new();
        if let Some(error) = callback_error {
            failures.push(error);
        }
        if let Some(error) = cancellation {
            failures.push(error);
        }
        if let Err(error) = collection {
            failures.push(error);
        }
        match failures.len() {
            0 => Ok(()),
            1 => Err(failures.remove(0)),
            _ => {
                let primary = failures.remove(0);
                Err(ProviderError::Combined {
                    primary: Box::new(primary),
                    secondary: failures,
                })
            }
        }
    }

    async fn fence_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let state = self.binding(binding).await?;
        state.callbacks.retire_all(binding).await?;
        state.sink.fail(events::EventDeliveryError::Observation(
            forge_provider_api::lifecycle::ObservationFailure::Disconnected,
        ));
        Ok(())
    }

    async fn recover_session(
        &self,
        request: SessionRequest,
        token: Option<RecoveryToken>,
    ) -> Result<SessionBinding, ProviderError> {
        let session_id = request.provider_session_id.as_ref().ok_or_else(|| {
            failure(
                ProviderOperation::RecoverSession,
                ProviderFailureKind::Rejected,
            )
        })?;
        let token = token.ok_or_else(|| {
            failure(
                ProviderOperation::RecoverSession,
                ProviderFailureKind::Rejected,
            )
        })?;
        let evidence = super::prompt::decode_recovery_token(&token)?;
        let (token_owner, token_session) = match evidence {
            super::prompt::NativeRecoveryEvidence::Sending {
                version: 1,
                owner,
                session_id,
                ..
            }
            | super::prompt::NativeRecoveryEvidence::Accepted {
                version: 1,
                owner,
                session_id,
                ..
            } => (owner, session_id),
            _ => {
                return Err(failure(
                    ProviderOperation::RecoverSession,
                    ProviderFailureKind::Rejected,
                ));
            }
        };
        if token_owner != request.owner || token_session != session_id.as_str() {
            return Err(failure(
                ProviderOperation::RecoverSession,
                ProviderFailureKind::Rejected,
            ));
        }
        {
            let _initialization = self.initialization_lock.lock().await;
            self.collect_failed_runtime().await?;
        }
        let existing = self.owners.lock().await.get(&request.owner).cloned();
        if let Some(existing) = existing {
            self.retire_session(&existing).await?;
        }
        self.initialize_session(request).await
    }

    async fn reconcile_settings(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
        settings: forge_provider_api::session::SessionSettings,
        resources: forge_provider_api::session::ResourceSelection,
    ) -> Result<(), ProviderError> {
        let state = self.request_binding(&request).await?;
        if state.turn.is_active() {
            self.cancel_session(&state.binding.id).await?;
        }
        let _admission = state.admission.lock().await;
        if state.turn.is_active() {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
        state.turn.reset().map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Protocol,
            )
        })?;

        let mut next_request = state.request.lock().await.clone();
        next_request.workspace = request.workspace.clone();
        next_request.provider_session_id = state.binding.provider_session_id.clone();
        next_request.model = settings.model.unwrap_or(next_request.model);
        next_request.access = settings.access.unwrap_or(request.access);
        next_request.approval = settings.approval.unwrap_or(request.approval_mode);
        next_request.resources = resources;
        self.descriptor()
            .capability
            .validate_access(&next_request.access)
            .map_err(|_| {
                failure(
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::Rejected,
                )
            })?;

        let profile = Arc::new(Mutex::new(Some(make_profile(
            &self.runtime_root,
            &next_request,
        )?)));
        let profile_path = profile
            .lock()
            .await
            .as_ref()
            .expect("profile initialized")
            ._profile
            .path()
            .to_owned();
        let worker_profile = profile.clone();
        let client_runtime = self.runtime.clone();
        let workspace = next_request.workspace.clone();
        let model_selection = next_request.model.clone();
        let capability = self.descriptor().capability;
        let client = async move {
            let client = client_runtime.client(&workspace).await?;
            let models = client.models().await?;
            let (_, reasoning) = resolve_model_selection(&model_selection, &models, &capability)?;
            Ok((client, reasoning))
        };
        let skills = next_request.resources.skills.clone();
        let mcp = next_request
            .resources
            .mcp
            .iter()
            .map(mcp_config)
            .collect::<Vec<_>>();
        let selected_mcp = next_request
            .resources
            .mcp
            .iter()
            .map(|resource| resource.name.clone())
            .collect::<Vec<_>>();
        let tools = next_request.tools.clone();
        let mode = session_mode(next_request.mode);
        let options = super::session::SessionOptions {
            session_id: next_request
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned()),
            model: next_request.model.model.clone(),
            reasoning_effort: None,
            context_tier: next_request.model.context_window.clone(),
            workspace: PathBuf::from(&next_request.workspace),
            mode,
            system_message: next_request.system_instructions.clone(),
            resources: match next_request.mode {
                SessionMode::Background => super::session::ResourceDiscovery::Native,
                SessionMode::Interactive | SessionMode::Child => {
                    super::session::ResourceDiscovery::Managed {
                        skill_directories: vec![profile_path.join("skills")],
                        selected_mcp,
                    }
                }
            },
            excluded_tools: Vec::new(),
            tools: tools.clone(),
            native_callbacks: Some(state.callbacks.clone()),
        };
        let permissions: Arc<dyn ProviderPermissions> = state.callbacks.clone();
        let provider_tools: Arc<dyn ProviderTools> = state.callbacks.clone();
        let selection = SessionSelection {
            execution_owner: next_request.owner.clone(),
            runtime_key: state.runtime_key.clone(),
            correlation_id: next_request.owner.session_id.to_string(),
            session_id: next_request
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned()),
            model: next_request.model.model.clone(),
            context_tier: next_request.model.context_window.clone(),
            tools: Some(tools),
            reuse: SessionReuse::Refresh,
        };
        let acquired = self
            .sessions
            .acquire(
                client,
                selection,
                permissions,
                provider_tools,
                move || async move {
                    let retained = worker_profile
                        .lock()
                        .await
                        .take()
                        .expect("resource profile prepared once");
                    Ok(PreparedSession {
                        options,
                        skills: Some(
                            skills
                                .iter()
                                .map(|skill| super::handle::SkillSelection {
                                    name: skill.name.clone(),
                                    enabled: skill.enabled,
                                })
                                .collect(),
                        ),
                        mcp: Some(mcp),
                        profile: Some(retained),
                    })
                },
                |_| Ok(()),
            )
            .await;
        let session = match acquired {
            Ok(session) => session,
            Err(error) => {
                state.callbacks.retire_all(&state.binding.id).await?;
                state.sink.fail(events::EventDeliveryError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                ));
                return Err(error.downcast::<ProviderError>().unwrap_or_else(|_| {
                    failure(
                        ProviderOperation::ReconcileSettings,
                        ProviderFailureKind::Runtime,
                    )
                }));
            }
        };
        state.callbacks.retire_all(&state.binding.id).await?;
        state
            .callbacks
            .register_mcp_tools(&session, &next_request.resources)
            .await;
        *state.session.write().await = session;
        *state.request.lock().await = next_request;
        Ok(())
    }

    async fn shutdown(&self) -> Result<(), ProviderError> {
        self.runtime.close_admission();
        let bindings = self
            .bindings
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        let mut failures = Vec::new();
        for binding in bindings {
            if let Err(error) = self.retire_session(&binding).await {
                failures.push(error);
            }
        }
        let runtime_result = self.runtime.shutdown().await;
        let collection = if runtime_result.is_ok() {
            super::sessions::ClientShutdown::Collected
        } else {
            super::sessions::ClientShutdown::Uncertain
        };
        if let Err(error) = runtime_result {
            failures.push(error);
        }
        if let Err(error) = self.sessions.clear(collection).await {
            failures.push(error);
        }
        match failures.len() {
            0 => Ok(()),
            1 => Err(failures.remove(0)),
            _ => {
                let primary = failures.remove(0);
                Err(ProviderError::Combined {
                    primary: Box::new(primary),
                    secondary: failures,
                })
            }
        }
    }

    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        let id = crate::registry::select("copilot").expect("Copilot is registered");
        crate::registry::descriptor(&id).expect("registered Copilot descriptor")
    }

    async fn has_active_turn(&self, owner: &forge_provider_api::ExecutionOwner) -> bool {
        let Some(binding) = self.owners.lock().await.get(owner).cloned() else {
            return false;
        };
        self.binding(&binding)
            .await
            .ok()
            .is_some_and(|state| state.turn.is_active())
    }

    async fn cancel_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let state = self.binding(binding).await?;
        let trace = state.request.lock().await.trace.clone();
        trace.record(forge_provider_api::trace::ProviderTraceRecord {
            binding_id: binding.clone(),
            event: forge_provider_api::trace::ProviderTraceEvent::CancellationStarted,
            scope: None,
            submission_id: None,
            failure: None,
        });
        state.turn.request_stop().map_err(|_| {
            failure(
                ProviderOperation::CancelSession,
                ProviderFailureKind::Unknown,
            )
        })?;
        let mut abort_error = None;
        if state.turn.is_active() {
            let session = state.session.read().await.clone();
            let stop = async {
                let sends = state.turn.finish_pending_sends().await;
                let aborted = session.abort().await;
                match (sends, aborted) {
                    (Ok(()), result) | (result, Ok(())) => result,
                    (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                        primary: Box::new(primary),
                        secondary: vec![secondary],
                    }),
                }
            };
            let abort = tokio::time::timeout(std::time::Duration::from_secs(10), stop)
                .await
                .unwrap_or_else(|_| {
                    Err(failure(
                        ProviderOperation::CancelSession,
                        ProviderFailureKind::Timeout,
                    ))
                });
            let failed = abort.as_ref().err().map(super::error::trace_failure_kind);
            trace.record(forge_provider_api::trace::ProviderTraceRecord {
                binding_id: binding.clone(),
                event: forge_provider_api::trace::ProviderTraceEvent::CancellationAcknowledged,
                scope: None,
                submission_id: None,
                failure: failed,
            });
            abort?;
            if tokio::time::timeout(
                std::time::Duration::from_secs(30),
                state.turn.wait_finished(),
            )
            .await
            .is_err()
            {
                abort_error.get_or_insert_with(|| {
                    failure(
                        ProviderOperation::CancelSession,
                        ProviderFailureKind::Timeout,
                    )
                });
            }
        }
        trace.record(forge_provider_api::trace::ProviderTraceRecord {
            binding_id: binding.clone(),
            event: forge_provider_api::trace::ProviderTraceEvent::ExecutionCollectionStarted,
            scope: None,
            submission_id: None,
            failure: None,
        });
        let mut join_error = None;
        match state.collect_execution().await {
            Ok(None | Some(Ok(()))) => {}
            Ok(Some(Err(error))) | Err(error) => join_error = Some(error),
        }
        trace.record(forge_provider_api::trace::ProviderTraceRecord {
            binding_id: binding.clone(),
            event: forge_provider_api::trace::ProviderTraceEvent::ExecutionCollectionFinished,
            scope: None,
            submission_id: None,
            failure: join_error.as_ref().map(super::error::trace_failure_kind),
        });
        match (abort_error, join_error) {
            (Some(primary), Some(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
            (Some(error), None) | (None, Some(error)) => Err(error),
            (None, None) => {
                // An aborted tool may never emit execution_complete. Revoke its routing before
                // a later identity-free SDK hook evaluates barriers for this binding.
                state.callbacks.retire_all(binding).await
            }
        }
    }

    async fn cleanup_execution(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let state = self.binding(binding).await?;
        if state.turn.is_active() {
            self.cancel_session(binding).await?;
        }
        if let Some(result) = state.collect_execution().await? {
            result?;
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
            .await
            .ok()
            .and_then(|state| state.binding.provider_session_id.clone()))
    }

    async fn reconcile_submission(
        &self,
        submission_id: &forge_provider_api::SubmissionId,
        token: Option<&RecoveryToken>,
        accepted_receipts: &[RecoveryToken],
    ) -> Result<SubmissionEvidence, ProviderError> {
        let Some(token) = token else {
            return Ok(SubmissionEvidence::Unknown);
        };
        let NativeRecoveryEvidence::Sending {
            version,
            submission_id: token_submission,
            owner,
            workspace,
            session_id,
            cursor,
            content_digest,
        } = super::prompt::decode_recovery_token(token)?
        else {
            return Ok(SubmissionEvidence::Unknown);
        };
        if version != 1 || token_submission != *submission_id {
            return Ok(SubmissionEvidence::Unknown);
        }
        let known_receipts =
            accepted_message_ids(submission_id, &owner, &session_id, accepted_receipts)?;
        let client = self.runtime.client(&workspace).await?;
        let temporary =
            super::session::open_recovery_session(&client, &session_id, Path::new(&workspace))
                .await?;
        let receipt = CopilotSession::find_unclaimed_submission_in(
            &temporary,
            &cursor,
            &known_receipts,
            Some(&content_digest),
        )
        .await;
        let cleanup = temporary.disconnect().await;
        let receipt = match (receipt, cleanup) {
            (Ok(receipt), Ok(())) => receipt,
            (Err(error), Ok(())) => return Err(error),
            (Ok(_), Err(_)) => {
                return Err(failure(
                    ProviderOperation::RecoverSession,
                    ProviderFailureKind::Runtime,
                ));
            }
            (Err(error), Err(_)) => {
                return Err(ProviderError::Combined {
                    primary: Box::new(error),
                    secondary: vec![failure(
                        ProviderOperation::CleanupExecution,
                        ProviderFailureKind::Runtime,
                    )],
                });
            }
        };
        let message_id = match receipt {
            super::handle::SubmissionReconciliation::Found(message_id) => message_id,
            super::handle::SubmissionReconciliation::NotFound => {
                return Ok(SubmissionEvidence::NotAccepted);
            }
            super::handle::SubmissionReconciliation::Ambiguous
            | super::handle::SubmissionReconciliation::ContentMismatch => {
                return Ok(SubmissionEvidence::Unknown);
            }
        };
        let recovered_receipt = super::prompt::accepted_recovery_token(
            submission_id.clone(),
            owner,
            session_id.clone(),
            message_id,
        )?;
        let provider_session_id = session_id.try_into().map_err(|_| {
            failure(
                ProviderOperation::RecoverSession,
                ProviderFailureKind::Protocol,
            )
        })?;
        Ok(SubmissionEvidence::Accepted {
            receipt: Some(recovered_receipt),
            provider_session_id: Some(provider_session_id),
        })
    }

    async fn generate_text(
        &self,
        request: forge_provider_api::TextGenerationRequest,
    ) -> Result<String, ProviderError> {
        let command = self.runtime.command().to_vec();
        super::text_generation::generate(
            &command,
            &request.workspace,
            &request,
            request.validator.as_ref(),
        )
        .await
    }

    async fn model_list(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        let client = self.runtime.client(&request.workspace).await?;
        client.models().await
    }

    async fn skill_list(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::catalog::SkillDefinition>, ProviderError> {
        let state = self.request_binding(&request).await?;
        state.session.read().await.clone().skills().await
    }

    async fn set_skill_enabled(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<forge_provider_api::catalog::CatalogMutation, ProviderError> {
        let state = self.request_binding(&request).await?;
        state
            .session
            .read()
            .await
            .clone()
            .set_skill_enabled(name, enabled)
            .await
    }

    async fn mcp_configuration(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        self.runtime
            .client(&request.workspace)
            .await?
            .discover_mcp(&request.workspace)
            .await
    }

    async fn mcp_list(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let client = self.runtime.client(&request.workspace).await?;
        let mut definitions = client.discover_mcp(&request.workspace).await?;
        let state = self.request_binding(&request).await?;
        let resources = state.request.lock().await.resources.clone();
        for launch in resources.mcp {
            let transport = match launch.transport {
                forge_provider_api::McpTransport::Stdio { .. } => {
                    forge_provider_api::catalog::McpTransportKind::Stdio
                }
                forge_provider_api::McpTransport::Http { .. } => {
                    forge_provider_api::catalog::McpTransportKind::Http
                }
                forge_provider_api::McpTransport::Sse { .. } => {
                    forge_provider_api::catalog::McpTransportKind::Sse
                }
            };
            definitions.retain(|row| row.name != launch.name);
            definitions.push(forge_provider_api::catalog::McpDefinition {
                resource_id: None,
                scope: None,
                source_path: None,
                name: launch.name,
                transport,
                enabled: true,
                status: forge_provider_api::catalog::McpStatus::Loading,
                status_detail: None,
                token_count: None,
                token_estimated: false,
                tools: Vec::new(),
                tool_error: None,
            });
        }
        let session = state.session.read().await.clone();
        let startup_failures = self.sessions.mcp_failures(&state.runtime_key).await;
        session.mcp_list(definitions, &startup_failures).await
    }

    async fn set_mcp_enabled(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<forge_provider_api::catalog::CatalogMutation, ProviderError> {
        let state = self.request_binding(&request).await?;
        state
            .session
            .read()
            .await
            .clone()
            .set_mcp_enabled(name, enabled)
            .await
    }

    async fn fork(
        &self,
        request: forge_provider_api::session::ConversationForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        let provider_session_id = if let Some(session_id) = request.source.provider_session_id {
            let client = self
                .runtime
                .client(&request.source.workspace)
                .await
                .map_err(|_| {
                    failure(
                        ProviderOperation::NativeTransport,
                        ProviderFailureKind::Runtime,
                    )
                })?;
            client
                .fork(
                    session_id.as_str().to_owned(),
                    request
                        .checkpoint
                        .map(|checkpoint| checkpoint.as_str().to_owned()),
                )
                .await?
                .try_into()
                .map_err(|_| {
                    failure(
                        ProviderOperation::NativeTransport,
                        ProviderFailureKind::Protocol,
                    )
                })?
        } else {
            let target = request.target;
            let initialized = self
                .initialize_session(SessionRequest {
                    owner: target.owner,
                    mode: SessionMode::Interactive,
                    workspace: target.workspace,
                    provider_session_id: None,
                    model: target.configuration.model,
                    access: target.configuration.access,
                    approval: target.configuration.approval,
                    resources: target.resources,
                    resource_lease: None,
                    tools: Vec::new(),
                    tool_endpoint: None,
                    system_instructions: target.system_instructions,
                    trace: Arc::new(NullTrace),
                })
                .await?;
            let provider_session_id = initialized.provider_session_id.ok_or_else(|| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::Protocol,
                )
            })?;
            self.retire_session(&initialized.id).await?;
            provider_session_id
        };
        Ok(forge_provider_api::ProviderForkResult::unprofiled(
            provider_session_id,
        ))
    }

    async fn background_terminals(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            )
        })?;
        self.sessions
            .terminals(&owner.runtime_key(), &request.harness_session_id)
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::Unknown,
                )
            })
    }

    async fn terminate_terminal(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
        terminal_id: &str,
    ) -> Result<(), ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            )
        })?;
        self.sessions
            .terminate(&owner.runtime_key(), terminal_id)
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::Unknown,
                )
            })
    }

    async fn compact(
        &self,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::Compaction, ProviderError> {
        let state = self.binding(binding).await?;
        self.sessions
            .compact(&state.runtime_key, request.operation_id)
            .await
    }

    async fn rollback_cancelled_turn(&self, binding: &BindingId) -> Result<bool, ProviderError> {
        let state = self.binding(binding).await?;
        let target = state.turn.retraction_target().map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            )
        })?;
        if matches!(target, super::turn::CopilotRetraction::Unsent) {
            return Ok(false);
        }
        state.session.read().await.clone().rewind(target).await?;
        state.turn.finish_retraction().map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            )
        })?;
        Ok(true)
    }
}

impl CopilotProvider {
    async fn submit(
        &self,
        binding: &BindingId,
        request: PromptRequest,
        authorized_not_consumed: bool,
    ) -> Result<InputAcceptance, ProviderError> {
        let state = self.binding(binding).await?;
        let _admission = state.admission.lock().await;
        let submission_id = request.submission_id.clone();
        let mode = request.mode;
        let allowed_tools = request
            .tools
            .iter()
            .map(|tool| tool.name.to_owned())
            .collect::<HashSet<_>>();
        let grant_request = request.clone();
        if request.scope.owner != state.owner
            || request.receipt.id().as_str() != request.submission_id.as_str()
        {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        if request.goal.is_some() {
            return Err(unsupported_goal());
        }
        self.descriptor()
            .capability
            .validate_access(&request.configuration.access)
            .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))?;
        if state.stream.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::CollectEventReader,
                ProviderFailureKind::Rejected,
            ));
        }
        let previous = {
            let mut active = state.active.lock().await;
            if active.as_ref().is_some_and(|task| !task.is_finished()) {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ));
            }
            active.take()
        };
        if let Some(previous) = previous {
            match previous.await {
                Ok(Ok(())) => {}
                Ok(Err(ProviderError::NotConsumed { .. })) if authorized_not_consumed => {}
                Ok(Err(error)) => return Err(error),
                Err(_) => {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Runtime,
                    ));
                }
            }
        }
        let event_sink = state.sink.invocation_sink(&request.scope)?;
        let session = state.session.read().await.clone();
        let workspace = session
            .workspace_path()
            .unwrap_or(Path::new("."))
            .to_string_lossy()
            .into_owned();
        let client = self.runtime.client(&workspace).await?;
        let models = client.models().await?;
        let (selected_model, reasoning) = resolve_model_selection(
            &request.configuration.model,
            &models,
            &self.descriptor().capability,
        )
        .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))?;
        session
            .set_model(
                selected_model.as_deref().unwrap_or("auto"),
                reasoning,
                request.configuration.model.context_window.as_deref(),
            )
            .await?;
        let callbacks = InvocationCallbacks {
            scope: request.scope.clone(),
            generation: 0,
            permissions: request.permissions.clone(),
            tools: request.provider_tools.clone(),
            control: request.turn_control.clone(),
            effects: request.host_effects.clone(),
            allowed_tools: allowed_tools.clone(),
            session_directory: super::session_directory::SessionDirectory::from_session(&session),
        };
        state.callbacks.install(callbacks).await?;
        let callback_generation = state.callbacks.callbacks(&request.scope).await?.generation;
        // Preserve this conversation on refresh once native dispatch becomes possible.
        self.sessions
            .mark_submitted(&state.runtime_key, &session)
            .await?;
        let repair_caution = self
            .sessions
            .repair(&state.runtime_key)
            .await
            .map(|repair| repair.caution().to_owned());
        let (accepted, acceptance) = tokio::sync::oneshot::channel();
        let turn = state.turn.clone();
        let task_state = state.clone();
        let grant_scope = request.scope.clone();
        let options = super::prompt::TurnOptions {
            workspace: session
                .workspace_path()
                .map(|path| path.to_string_lossy().into_owned())
                .unwrap_or_default(),
            model: selected_model,
            capability: self.descriptor().capability,
            input: request.input,
            task: request.task,
            goal: request.goal,
            recovered: request.recovered_inputs,
            instructions: request.instructions,
            repair_caution,
            attachments: forge_provider_api::input::MessageInput {
                prompt: String::new(),
                attachments: request.attachments,
            },
            receipt: Some(request.receipt),
            submission_id: Some(submission_id.clone()),
            background_compaction_observed: true,
            scope: request.scope.clone(),
            binding_id: binding.clone(),
            host_effects: state.callbacks.clone(),
            accepted: Some(accepted),
        };
        let control: Arc<CallbackRouter> = state.callbacks.clone();
        let (activated, activation) = tokio::sync::oneshot::channel();
        let task = tokio::spawn(async move {
            let guard = match turn.activate(Some(event_sink.clone()), None) {
                Ok(guard) => {
                    let _ = activated.send(Ok(()));
                    guard
                }
                Err(_) => {
                    let error =
                        failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected);
                    let _ = activated.send(Err(()));
                    return Err(error);
                }
            };
            let result = super::prompt::run(
                &session,
                &turn,
                &guard,
                options,
                &event_sink,
                control.as_ref(),
                task_state.callbacks.as_ref(),
            )
            .await;
            match &result {
                Err(ProviderError::Retryable(retryable)) => {
                    task_state.continuations.lock().await.insert(
                        retryable.continuation.as_bytes().to_vec(),
                        ContinuationGrant::Retryable {
                            scope: grant_scope.clone(),
                            mode,
                            allowed_tools,
                        },
                    );
                }
                Err(ProviderError::NotConsumed { continuation }) => {
                    task_state.continuations.lock().await.insert(
                        continuation.as_bytes().to_vec(),
                        ContinuationGrant::NotConsumed {
                            request: Box::new(grant_request),
                        },
                    );
                }
                _ => {}
            }
            task_state
                .callbacks
                .finish_foreground_scope(&grant_scope, callback_generation)
                .await;
            drop(guard);
            match result {
                Ok(_) | Err(ProviderError::Cancelled) => Ok(()),
                Err(error) => Err(error),
            }
        });
        *state.active.lock().await = Some(task);
        match activation.await {
            Ok(Ok(())) => {}
            Ok(Err(())) => {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ));
            }
            Err(_) => {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Runtime,
                ));
            }
        }
        match tokio::time::timeout(std::time::Duration::from_secs(30), acceptance).await {
            Ok(Ok(Ok(()))) => {}
            Ok(Ok(Err(error))) => return Err(error),
            Ok(Err(_)) | Err(_) => return Err(ProviderError::DeliveryUncertain),
        }
        Ok(InputAcceptance { submission_id })
    }

    async fn resume_retained_execution(
        &self,
        binding: &BindingId,
        state: &Arc<BindingState>,
        scope: forge_provider_api::InvocationScope,
        callbacks: forge_provider_api::session::InvocationCallbacks,
        mode: forge_provider_api::PromptMode,
        allowed_tools: HashSet<String>,
    ) -> Result<(), ProviderError> {
        let _admission = state.admission.lock().await;
        let previous = {
            let mut active = state.active.lock().await;
            if active.as_ref().is_some_and(|task| !task.is_finished()) {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ));
            }
            active.take()
        };
        if let Some(previous) = previous {
            match previous.await {
                Ok(Ok(())) => {}
                Ok(Err(ProviderError::Retryable(_)))
                | Ok(Err(ProviderError::NotConsumed { .. })) => {}
                Ok(Err(error)) => return Err(error),
                Err(_) => {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Runtime,
                    ));
                }
            }
        }
        let event_sink = state.sink.invocation_sink(&scope)?;
        let session = state.session.read().await.clone();
        state
            .callbacks
            .install(InvocationCallbacks {
                scope: scope.clone(),
                generation: 0,
                permissions: callbacks.permissions,
                tools: callbacks.provider_tools,
                control: callbacks.turn_control,
                effects: callbacks.host_effects,
                allowed_tools: allowed_tools.clone(),
                session_directory: super::session_directory::SessionDirectory::from_session(
                    &session,
                ),
            })
            .await?;
        let callback_generation = state.callbacks.callbacks(&scope).await?.generation;
        let repair_caution = self
            .sessions
            .repair(&state.runtime_key)
            .await
            .map(|repair| repair.caution().to_owned());
        let (accepted, acceptance) = tokio::sync::oneshot::channel();
        let options = super::prompt::TurnOptions {
            workspace: session.workspace_path().map(|path| path.to_string_lossy().into_owned()).unwrap_or_default(),
            model: None,
            capability: self.descriptor().capability,
            input: forge_provider_api::PromptInput::Text(
                "Continue the previous response from accepted conversation history. Do not repeat the user's request.".into(),
            ),
            task: None,
            goal: None,
            recovered: Vec::new(),
            instructions: String::new(),
            repair_caution,
            attachments: forge_provider_api::input::MessageInput { prompt: String::new(), attachments: Vec::new() },
            receipt: None,
            submission_id: None,
            background_compaction_observed: true,
            scope: scope.clone(),
            binding_id: binding.clone(),
            host_effects: state.callbacks.clone(),
            accepted: Some(accepted),
        };
        let turn = state.turn.clone();
        let sink = event_sink.clone();
        let control = state.callbacks.clone();
        let task_state = state.clone();
        let task_scope = scope.clone();
        let task_allowed_tools = allowed_tools;
        let (activated, activation) = tokio::sync::oneshot::channel();
        let task = tokio::spawn(async move {
            let guard = match turn.activate(Some(sink.clone()), None) {
                Ok(guard) => {
                    let _ = activated.send(Ok(()));
                    guard
                }
                Err(_) => {
                    let error =
                        failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected);
                    let _ = activated.send(Err(()));
                    return Err(error);
                }
            };
            let result = super::prompt::run(
                &session,
                &turn,
                &guard,
                options,
                &sink,
                control.as_ref(),
                control.as_ref(),
            )
            .await;
            if let Err(ProviderError::Retryable(retryable)) = &result {
                task_state.continuations.lock().await.insert(
                    retryable.continuation.as_bytes().to_vec(),
                    ContinuationGrant::Retryable {
                        scope: task_scope.clone(),
                        mode,
                        allowed_tools: task_allowed_tools,
                    },
                );
            }
            task_state
                .callbacks
                .finish_foreground_scope(&task_scope, callback_generation)
                .await;
            drop(guard);
            match result {
                Ok(_) | Err(ProviderError::Cancelled) => Ok(()),
                Err(error) => Err(error),
            }
        });
        *state.active.lock().await = Some(task);
        match activation.await {
            Ok(Ok(())) => {}
            Ok(Err(())) => {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ));
            }
            Err(_) => {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Runtime,
                ));
            }
        }
        match tokio::time::timeout(std::time::Duration::from_secs(30), acceptance).await {
            Ok(Ok(Ok(()))) => Ok(()),
            Ok(Ok(Err(error))) => Err(error),
            Ok(Err(_)) | Err(_) => Err(ProviderError::DeliveryUncertain),
        }
    }
}

fn make_profile(root: &Path, request: &SessionRequest) -> Result<RetainedResources, ProviderError> {
    let mut profile = crate::profile::ResourceProfile::create(root, "copilot")
        .map_err(|error| error.into_provider_error(ProviderOperation::InitializeSession))?;
    profile
        .bind_skills(&request.resources.skills, "skills")
        .map_err(|error| error.into_provider_error(ProviderOperation::InitializeSession))?;
    Ok(RetainedResources {
        _profile: profile,
        _lease: request.resource_lease.clone(),
    })
}

fn session_mode(mode: SessionMode) -> forge_provider_api::PromptMode {
    match mode {
        SessionMode::Background => forge_provider_api::PromptMode::Background,
        SessionMode::Interactive | SessionMode::Child => forge_provider_api::PromptMode::Chat,
    }
}

fn resolve_model_selection(
    selection: &ModelSelection,
    models: &[forge_provider_api::ProviderModel],
    capability: &forge_provider_api::ProviderCapability,
) -> Result<(Option<String>, Option<String>), ProviderError> {
    selection.service_tier.validate(capability).map_err(|_| {
        failure(
            ProviderOperation::ReconcileSettings,
            ProviderFailureKind::Rejected,
        )
    })?;
    let selected_id = selection.model.as_deref().unwrap_or("auto");
    let model = models
        .iter()
        .find(|model| model.id == selected_id)
        .ok_or_else(|| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            )
        })?;
    if selection.service_tier != forge_provider_api::ServiceTier::Standard
        && !model.service_tiers.contains(&selection.service_tier)
    {
        return Err(failure(
            ProviderOperation::ReconcileSettings,
            ProviderFailureKind::Rejected,
        ));
    }
    let reasoning = match &selection.reasoning {
        ReasoningSelection::Default => None,
        ReasoningSelection::Lowest => model.reasoning.first().cloned(),
        ReasoningSelection::Level(level)
            if model.reasoning.iter().any(|choice| choice == level) =>
        {
            Some(level.clone())
        }
        ReasoningSelection::Level(_) => {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
    };
    Ok((selection.model.clone(), reasoning))
}

fn mcp_config(launch: &forge_provider_api::session::McpLaunch) -> (String, Map<String, Value>) {
    let mut payload = match &launch.transport {
        forge_provider_api::session::McpTransport::Stdio {
            command,
            arguments,
            environment,
        } => json!({
            "type":"stdio", "command":command, "args":arguments, "env":environment,
        }),
        forge_provider_api::session::McpTransport::Http { url, headers } => {
            json!({"type":"http","url":url,"headers":headers})
        }
        forge_provider_api::session::McpTransport::Sse { url, headers } => {
            json!({"type":"sse","url":url,"headers":headers})
        }
    };
    let object = payload
        .as_object_mut()
        .expect("MCP transport encodes as an object");
    if let Some(timeout) = launch.timeout {
        object.insert("timeout".into(), json!(timeout.as_millis()));
    }
    if let Some(directory) = &launch.working_directory {
        object.insert("cwd".into(), json!(directory));
    }
    if let Some(tools) = &launch.tools {
        let tools = match tools {
            forge_provider_api::session::McpToolSelection::All => vec!["*"],
            forge_provider_api::session::McpToolSelection::Only(names) => {
                names.iter().map(|name| name.as_str()).collect()
            }
        };
        object.insert("tools".into(), json!(tools));
    }
    (
        launch.name.clone(),
        payload.as_object().cloned().unwrap_or_default(),
    )
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

fn unsupported_goal() -> ProviderError {
    forge_provider_api::capability::UnsupportedCapability(
        forge_provider_api::capability::ProviderFeature::NativeGoal,
    )
    .into()
}

fn same_prompt_request(left: &PromptRequest, right: &PromptRequest) -> bool {
    left.scope == right.scope
        && left.mode == right.mode
        && left.configuration.model == right.configuration.model
        && left.configuration.access == right.configuration.access
        && left.configuration.approval == right.configuration.approval
        && left.tool_endpoint == right.tool_endpoint
        && match (&left.resource_lease, &right.resource_lease) {
            (None, None) => true,
            (Some(left), Some(right)) => Arc::ptr_eq(left, right),
            _ => false,
        }
        && left.task == right.task
        && left.tools.len() == right.tools.len()
        && left.tools.iter().zip(&right.tools).all(|(left, right)| {
            left.name == right.name
                && left.description == right.description
                && left.input_schema == right.input_schema
        })
        && left.goal == right.goal
        && left.submission_id == right.submission_id
        && same_input(&left.input, &right.input)
        && left.recovered_inputs.len() == right.recovered_inputs.len()
        && left
            .recovered_inputs
            .iter()
            .zip(&right.recovered_inputs)
            .all(|(left, right)| {
                same_input(&left.input, &right.input)
                    && left.instructions == right.instructions
                    && same_attachments(&left.attachments, &right.attachments)
            })
        && left.instructions == right.instructions
        && same_attachments(&left.attachments, &right.attachments)
        && left.receipt.id() == right.receipt.id()
}

fn same_input(
    left: &forge_provider_api::PromptInput,
    right: &forge_provider_api::PromptInput,
) -> bool {
    match (left, right) {
        (
            forge_provider_api::PromptInput::Text(left),
            forge_provider_api::PromptInput::Text(right),
        ) => left == right,
        (
            forge_provider_api::PromptInput::Skill {
                name: left_name,
                arguments: left_arguments,
            },
            forge_provider_api::PromptInput::Skill {
                name: right_name,
                arguments: right_arguments,
            },
        ) => left_name == right_name && left_arguments == right_arguments,
        (
            forge_provider_api::PromptInput::QuestionReply(left),
            forge_provider_api::PromptInput::QuestionReply(right),
        ) => left == right,
        _ => false,
    }
}

fn same_attachments(
    left: &[forge_provider_api::input::FileAttachment],
    right: &[forge_provider_api::input::FileAttachment],
) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.path == right.path
                && left.label == right.label
                && left.media_type == right.media_type
                && left.contents.as_ref() == right.contents.as_ref()
                && left.context == right.context
        })
}

/// Collect prior native message receipts without consuming the currently reconciled submission.
fn accepted_message_ids(
    submission_id: &forge_provider_api::SubmissionId,
    owner: &forge_provider_api::ExecutionOwner,
    session_id: &str,
    accepted_receipts: &[RecoveryToken],
) -> Result<std::collections::HashSet<String>, ProviderError> {
    let mut known_receipts = std::collections::HashSet::new();
    for accepted in accepted_receipts {
        if let super::prompt::NativeRecoveryEvidence::Accepted {
            version,
            submission_id: accepted_submission,
            owner: accepted_owner,
            session_id: accepted_session,
            message_id,
        } = super::prompt::decode_recovery_token(accepted)?
            && version == 1
            && accepted_submission != *submission_id
            && accepted_owner == *owner
            && accepted_session == session_id
        {
            known_receipts.insert(message_id);
        }
    }
    Ok(known_receipts)
}

#[cfg(test)]
mod callback_tests {
    use super::*;
    use forge_provider_api::ProviderHostEffects;

    struct Authority;

    struct CancelledAuthority;

    #[async_trait]
    impl ProviderPermissions for CancelledAuthority {
        async fn authorize(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: PermissionRequest,
        ) -> PermissionResponse {
            PermissionResponse::Cancelled
        }

        async fn before_tool(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            Err(ProviderError::Cancelled)
        }
    }

    #[async_trait]
    impl ProviderPermissions for Authority {
        async fn authorize(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: PermissionRequest,
        ) -> PermissionResponse {
            PermissionResponse::Allow
        }
        async fn before_tool(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    #[async_trait]
    impl ProviderTools for Authority {
        async fn call(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: ToolCall,
        ) -> Result<String, ProviderError> {
            Ok("tool completed".into())
        }
    }

    #[async_trait]
    impl TurnControl for Authority {
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
            std::future::pending().await
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
                replay_safe: true,
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
    impl ProviderHostEffects for Authority {
        async fn settle_foreground(
            &self,
            _: &BindingId,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), forge_provider_api::HostEffectFailure> {
            Ok(())
        }
    }

    /// Install authority shared by successive native tool callbacks.
    async fn install(router: &CallbackRouter, scope: &forge_provider_api::InvocationScope) -> u64 {
        let authority = Arc::new(Authority);
        router
            .install(InvocationCallbacks {
                scope: scope.clone(),
                generation: 0,
                permissions: authority.clone(),
                tools: authority.clone(),
                control: authority.clone(),
                effects: authority,
                allowed_tools: HashSet::from(["fixture_tool".into()]),
                session_directory: None,
            })
            .await
            .unwrap();
        router.callbacks(scope).await.unwrap().generation
    }

    /// Exercise callback dispatch rather than only checking registry contents.
    async fn call(router: &CallbackRouter, id: &str) {
        assert_eq!(
            router
                .call_native(
                    id,
                    ToolCall {
                        name: "fixture_tool".into(),
                        arguments: json!({})
                    }
                )
                .await
                .unwrap(),
            "tool completed"
        );
    }

    /// Completing one tool cannot remove foreground authority needed by the next tool.
    #[tokio::test]
    async fn tool_completion_retains_live_foreground_callbacks() {
        let router = CallbackRouter::default();
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("callbacks"),
            "exchange",
        );
        install(&router, &scope).await;
        for id in ["first", "second"] {
            router.bind_native_call(id, &scope).await;
            call(&router, id).await;
            router.release_native_call(id).await;
            router.before_send(&scope).await.unwrap();
            router.status(&scope).await.unwrap();
        }
        router
            .settle_foreground(&BindingId::new("binding").unwrap(), &scope)
            .await
            .unwrap();
        assert!(router.callbacks(&scope).await.is_err());
    }

    /// Settled foreground retains callbacks still held by an admitted native call.
    #[tokio::test]
    async fn settled_callbacks_live_until_the_last_native_call_finishes() {
        let router = CallbackRouter::default();
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("callbacks"),
            "exchange",
        );
        install(&router, &scope).await;
        router.bind_native_call("late", &scope).await;
        router
            .settle_foreground(&BindingId::new("binding").unwrap(), &scope)
            .await
            .unwrap();
        call(&router, "late").await;
        router.release_native_call("late").await;
        assert!(router.callbacks(&scope).await.is_err());
        assert!(router.callbacks_for_native_call("late").await.is_err());
    }

    /// Completing an old native call cannot revoke replacement foreground authority.
    #[tokio::test]
    async fn old_callback_completion_does_not_retire_a_new_generation() {
        let router = CallbackRouter::default();
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("callbacks"),
            "exchange",
        );
        let generation = install(&router, &scope).await;
        router.bind_native_call("old", &scope).await;
        router.finish_foreground_scope(&scope, generation).await;
        let next = install(&router, &scope).await;
        router.release_native_call("old").await;
        assert_eq!(router.callbacks(&scope).await.unwrap().generation, next);
        router.before_send(&scope).await.unwrap();
    }

    /// An aborted tool without a completion event cannot poison later identity-free hooks.
    #[tokio::test]
    async fn cancelled_native_calls_do_not_block_the_next_invocation() {
        let router = CallbackRouter::default();
        let owner = forge_provider_api::ExecutionOwner::root("callbacks");
        let scope = forge_provider_api::InvocationScope::new(owner.clone(), "cancelled");
        install(&router, &scope).await;
        let mut callbacks = router.callbacks(&scope).await.unwrap();
        callbacks.permissions = Arc::new(CancelledAuthority);
        router.install(callbacks).await.unwrap();
        let generation = router.callbacks(&scope).await.unwrap().generation;
        router.bind_native_call("aborted-tool", &scope).await;
        router.finish_foreground_scope(&scope, generation).await;
        assert!(matches!(
            router.before_tool_native(None).await,
            Err(ProviderError::Cancelled)
        ));

        router
            .retire_all(&BindingId::new("binding").unwrap())
            .await
            .unwrap();
        let next = forge_provider_api::InvocationScope::new(owner, "next");
        install(&router, &next).await;
        router.before_tool_native(None).await.unwrap();
        assert!(
            router
                .before_tool_native(Some("aborted-tool"))
                .await
                .is_err()
        );
        router.bind_native_call("new-tool", &next).await;
        call(&router, "new-tool").await;
    }

    /// Hide generated MCP prefixes without confusing them with real prefixed tool names.
    #[tokio::test]
    async fn mcp_permission_aliases_require_a_unique_catalog_identity() {
        let router = CallbackRouter::default();
        router
            .mcp_tools
            .write()
            .await
            .insert("fixture".into(), HashSet::from(["read_marker".into()]));
        assert_eq!(
            router
                .canonical_mcp_target("fixture/fixture-read_marker")
                .await
                .as_deref(),
            Some("fixture/read_marker")
        );
        assert_eq!(
            router
                .canonical_mcp_target("fixture/read_marker")
                .await
                .as_deref(),
            Some("fixture/read_marker")
        );
        assert!(
            router
                .canonical_mcp_target("fixture/unknown")
                .await
                .is_none()
        );
        router
            .mcp_tools
            .write()
            .await
            .get_mut("fixture")
            .unwrap()
            .insert("fixture-read_marker".into());
        assert!(
            router
                .canonical_mcp_target("fixture/fixture-read_marker")
                .await
                .is_none()
        );
        assert_eq!(
            router
                .canonical_mcp_target("fixture/fixture-fixture-read_marker")
                .await
                .as_deref(),
            Some("fixture/fixture-read_marker")
        );
    }
}

#[cfg(test)]
mod recovery_receipt_tests {
    use super::*;
    use sha2::Digest;

    #[test]
    fn prior_receipts_reserve_only_other_submissions_in_the_same_session() {
        let owner = forge_provider_api::ExecutionOwner::root("recovery-owner");
        let current =
            forge_provider_api::SubmissionId::try_from("current-submission".to_owned()).unwrap();
        let prior =
            forge_provider_api::SubmissionId::try_from("prior-submission".to_owned()).unwrap();
        let other_owner = forge_provider_api::ExecutionOwner::root("other-owner");
        let receipt = |submission_id: forge_provider_api::SubmissionId,
                       owner: forge_provider_api::ExecutionOwner,
                       session: &str,
                       message: &str| {
            super::super::prompt::accepted_recovery_token(
                submission_id,
                owner,
                session.to_owned(),
                message.to_owned(),
            )
            .unwrap()
        };
        let receipts = [
            receipt(
                prior.clone(),
                owner.clone(),
                "native-session",
                "claimed-message",
            ),
            receipt(
                prior.clone(),
                owner.clone(),
                "native-session",
                "claimed-message",
            ),
            receipt(
                current.clone(),
                owner.clone(),
                "native-session",
                "current-message",
            ),
            receipt(
                prior.clone(),
                other_owner,
                "native-session",
                "other-owner-message",
            ),
            receipt(
                prior,
                owner.clone(),
                "different-session",
                "other-session-message",
            ),
        ];
        let known = accepted_message_ids(&current, &owner, "native-session", &receipts).unwrap();
        assert_eq!(
            known,
            [String::from("claimed-message")].into_iter().collect()
        );

        let target = super::super::prompt::sending_recovery_token(
            owner,
            current,
            "workspace".into(),
            "native-session".into(),
            "cursor".into(),
            hex::encode(sha2::Sha256::digest(b"target input")),
        )
        .unwrap();
        let super::super::prompt::NativeRecoveryEvidence::Sending {
            submission_id: target_submission,
            content_digest,
            ..
        } = super::super::prompt::decode_recovery_token(&target).unwrap()
        else {
            panic!("target recovery token was not a Sending checkpoint");
        };
        assert_eq!(target_submission.as_str(), "current-submission");
        assert!(!known.contains("current-message"));
        assert!(super::super::handle::content_matches(
            Some(&content_digest),
            "target input"
        ));
        assert_eq!(
            super::super::handle::resolve_unclaimed_submissions(
                &[
                    (
                        String::from("claimed-message"),
                        Some(String::from("target input"))
                    ),
                    (
                        String::from("current-message"),
                        Some(String::from("target input"))
                    ),
                ],
                &known,
                Some(&content_digest),
            ),
            super::super::handle::SubmissionReconciliation::Found(String::from("current-message")),
        );
    }
}
